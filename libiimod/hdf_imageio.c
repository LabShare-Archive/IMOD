/******************************************************************************
 *  hdf_imageio.c - HDF file dataset reading/writing 
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 2014 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *****************************************************************************/
 
#include "hdfP.h"

/* 
 * Local functions 
 */
static void cleanupTmp(unsigned char *tmpData, int freeMap, unsigned char *map,
                       hid_t dspaceID, hid_t memSpaceID, const char *mess);
static hid_t createGroupAndDataset(ImodImageFile *inFile, int zValue, char *buf);
static int getFileXscale(int format);
static hid_t getDatasetForZ(ImodImageFile *inFile, int cz, int *noData);
static hid_t lookupNativeDatatype(ImodImageFile *inFile);

/*
 * Generic routine for reading the data in lines or chunks and processing it line-by-line
 * with proper scaling.  This mirrors code in mrcsec.c and uses the functions there for
 * much of the setup and for processing each line.
 */
int hdfReadSectionAny(ImodImageFile *inFile, unsigned char *buf, int cz, int type)
{
  int pixSizeBuf[4] = {0, 1, 4, 2};
  LineProcData d;
  IloadInfo loadInfo;
  IloadInfo *li = &loadInfo;
  MrcHeader *hdata = (MrcHeader *)inFile->header;
  int padLeft, padRight;
  float targetMemory = 65536.;
  int err, chunkLines = 1;
  int freeMap = 0;
  unsigned char *tmpData = NULL;
  unsigned char *bdataRead;
  int needRead, lineEnd, noDataAtZ;
  int ind, yEnd, readStackInY, maxLines, numLines, fileXscale, rank;
  hid_t dspaceID = 0, memSpaceID, dsetID, nativeType;
  hsize_t memDim[2], fileCount[3], memCount[2], fileOffset[3], memOffset[2];

  /* Translate the information to a loadInfo to call common routines and set some 
     type-dependent settings as in iimrc */
  iiMRCsetLoadInfo(inFile, li);
  padLeft = B3DMAX(0, li->padLeft);
  padRight = B3DMAX(0, li->padRight);
  if (type == MRSA_FLOAT || !type) {
    li->outmin = inFile->smin;
    li->outmax = inFile->smax;
    li->mirrorFFT = 0;
  } else {
    li->outmin = 0;
    li->outmax = (type == MRSA_USHORT) ? 65535 : 255;
    li->mirrorFFT = inFile->mirrorFFT;
  }

  /* Initialize members of data structure */
  d.type = type;
  d.readY = li->axis == 2 ? 1 : 0;
  yEnd = d.readY ? li->zmax : li->ymax;
  readStackInY = d.readY && inFile->stackSetList ? 1 : 0;
  d.cz = cz;
  d.swapped = 0;
  err = iiInitReadSectionAny(hdata, li, buf, &d, &freeMap, &yEnd, "hdfReadSectionAny");
  if (err)
    return err;
  
  d.xDimension = d.xsize + padLeft + padRight;
  pixSizeBuf[0] = d.pixSize;
  
  /* Advance output pointers by left padding before starting */
  d.bufp += pixSizeBuf[type] * padLeft;
  d.usbufp += padLeft;
  d.fbufp += padLeft;
  d.pixIndex += padLeft;
  
  /* Determine if doing multi-line reads: not if reading in Y from single-image stack
     or mirroring FFT, only
     do it if it will involve more than one line of buffer, or if there is no buffer */
  maxLines = B3DNINT(targetMemory / (d.xsize * d.pixSize));
  if (!readStackInY && !(d.convert && li->mirrorFFT) && (maxLines > 1 || !d.needData)) {
    
    /* limit the chunk lines by memory usage for a buffer but do whole read otherwise */
    chunkLines = yEnd + 1 - d.yStart;
    if (d.needData)
      chunkLines = B3DMAX(1, B3DMIN(chunkLines, maxLines));
  }
  /*printf("ps %d cl %d nd %d nx %d xsize %d pl %d pr %d\n", d.pixSize, chunkLines,
    d.needData, inFile->nx, d.xsize, padLeft, padRight);*/

  /* Get the supplemental data array, set all pointers to it */
  if (d.needData) {
    tmpData = (unsigned char *)malloc(d.pixSize * d.xsize * chunkLines);
    if (!tmpData) {
      cleanupTmp(tmpData, freeMap, d.map, 0, 0, "getting memory for temporary array");
      return 2;
    }
  }

  /* Loop on chunks in Y */
  /*printf("chunkLines %d  needData %d mode %d byte %d toShort %d convert %d doScale %d"
    "\n", chunkLines, d.needData, hdata->mode, d.byte, d.toShort, d.convert, d.doScale);*/

  /* Get the multiplier to X dimensions and offsets and make default memory dataspace */
  fileXscale = getFileXscale(inFile->format);
  memCount[0] = memDim[0] = chunkLines;
  memCount[1] = d.xsize * fileXscale;
  memOffset[0] = 0;
  if (d.needData) {
    memDim[1] = d.xsize * fileXscale;
    memOffset[1] = 0;
  } else {
    memDim[1] = d.xDimension * fileXscale;
    memOffset[1] = padLeft * fileXscale;
  }
  memSpaceID = H5Screate_simple(2, memDim, NULL);
  err = H5Sselect_hyperslab(memSpaceID, H5S_SELECT_SET, memOffset, NULL, memCount, NULL);
  if (err < 0) {
    cleanupTmp(tmpData, freeMap, d.map, 0, memSpaceID, "selecting memory area to use");
    return 3;
  }
  
  /* Open the dataset here unless reading Y from a stack */
  if (!readStackInY) {
    dsetID = getDatasetForZ(inFile, cz, &noDataAtZ);
    if (noDataAtZ) {

      /* If there are no data, fill array with zero as an MRC file would do */
      for (ind = d.yStart; ind <= yEnd; ind++) {
        memset(d.bufp, 0, pixSizeBuf[type] * d.xsize);
        d.bufp += pixSizeBuf[type] * (padLeft + padRight);
      }
      return 0;
    }
    dspaceID = H5Dget_space(dsetID);
    rank = H5Sget_simple_extent_ndims(dspaceID);
    fileCount[rank - 1] = d.xsize * fileXscale;
    fileOffset[rank - 1] = d.xStart * fileXscale;
  }

  nativeType = lookupNativeDatatype(inFile);

  d.line = d.yStart;
  while (d.line <= yEnd) {

    /* Set up the ending line and amount to read */
    lineEnd = B3DMIN(yEnd, d.line + chunkLines - 1);
    numLines = (lineEnd + 1 - d.line);
    needRead = 1;

    /* Revise the memory selection for ending lines (will not happen with Y from stack) */
    if (numLines != memCount[0]) {
      memCount[0] = numLines;
      err = H5Sselect_hyperslab(memSpaceID, H5S_SELECT_SET, memOffset, NULL, memCount,
                                NULL);
      if (err < 0) {
        cleanupTmp(tmpData, freeMap, d.map, dspaceID, memSpaceID, 
                   "selecting memory area to use");
        return 3;
      }
    }

    /* Handle the four cases: first Z plane from stack */
    if (!d.readY && inFile->stackSetList) {
      fileCount[0] = 1;
      fileCount[rank - 2] = numLines;
      fileOffset[0] = 0;
      fileOffset[rank - 2] = d.line;
    } else if (readStackInY) {

      /* Y plane from stack */
      dsetID = getDatasetForZ(inFile, d.line, &noDataAtZ);
      if (!noDataAtZ) {
        dspaceID = H5Dget_space(dsetID);
        rank = H5Sget_simple_extent_ndims(dspaceID);
        fileCount[0] = 1;
        fileCount[rank - 2] = 1;
        fileCount[rank - 1] = d.xsize * fileXscale;
        fileOffset[0] = 0;
        fileOffset[rank - 2] = cz;
        fileOffset[rank - 1] = d.xStart * fileXscale;
      }
    } else if (!d.readY) {
      
      /* Z plane from volume */
      fileCount[0] = 1;
      fileCount[rank - 2] = numLines;
      fileOffset[0] = cz;
      fileOffset[rank - 2] = d.line;
    } else {

      /* Y plane from volume */
      fileCount[0] = numLines;
      fileCount[rank - 2] = 1;
      fileOffset[0] = d.line;
      fileOffset[rank - 2] = cz;
    }
    if (!noDataAtZ)
      err = H5Sselect_hyperslab(dspaceID, H5S_SELECT_SET, fileOffset, NULL, fileCount,
                              NULL);
    if (!noDataAtZ && err < 0) {
      cleanupTmp(tmpData, freeMap, d.map, dspaceID, memSpaceID, 
                 "selecting area to read from file");
      return 3;
    }

    /* Reset the bdata pointer to temp data for each line/chunk, or to the buffer pointer
       if reading directly into buffer.  Back off the pointer for reading into memory */
    if (d.needData)
      d.bdata = tmpData;
    else
      d.bdata = d.bufp;
    bdataRead = d.bdata - padLeft * d.pixSize;
    
    /* Start loop on Y and read a line or chunk of data first time through */
    for (; d.line <= lineEnd; d.line++) {
      if (noDataAtZ) {
        memset(d.bdata, 0, d.pixSize * d.xsize);
      } else if (needRead) {
        if (H5Dread(dsetID, nativeType, memSpaceID, dspaceID, H5P_DEFAULT,    
                    bdataRead) < 0) {
          cleanupTmp(tmpData, freeMap, d.map, dspaceID, memSpaceID, 
                     "reading data from file");
          return 3;
        }
        if (readStackInY || d.line == yEnd) {
          H5Sclose(dspaceID);
        }
      }
      needRead = 0;

      /* Process the next line */
      if (iiProcessReadLine(hdata, li, &d))
        return 1;
      
      /* End loop on Y - advance data pointer for continuing with a chunk */
      d.bdata += d.xsize * d.pixSize;
    }
  }
  fflush(stdout);
  cleanupTmp(tmpData, freeMap, d.map, 0, memSpaceID, NULL);
  return 0;
}

/*
 * Common writing function, with data conversion if data is floats and file mode is not.
 */
int hdfWriteSectionAny(ImodImageFile *inFile, unsigned char *buf, int cz,
                              int fromFloat)
{
  IloadInfo loadInfo;
  IloadInfo *li = &loadInfo;
  StackSetData stackSet;
  MrcHeader *hdata = (MrcHeader *)inFile->header;
  int  nx = hdata->nx;
  int bytesSigned = (!hdata->mode && hdata->bytesSigned) ? 1 : 0;
  int convert = 0, bufMode = hdata->mode;
  int padLeft, padRight, yStart, yEnd;
  int line, xDimension, noDataAtZ, err;
  int pixSizeOut, pixSizeBuf, bytesPerChanOut, numChanOut, bytesPerChanBuf, numChanBuf;
  int needData = 0;
  int chunkLines = 1;
  float targetMemory = 65536.;
  char nameBuf[36];

  /* Buffer to write lines from; may be replaced by temporary buffer */
  unsigned char *bdata = buf;
  b3dInt16 *sdata;
  char *sbdata;

  /* Copies of buffer pointer that can be advanced after each line */
  unsigned char *bufp = buf;
  b3dFloat *fbufp = (b3dFloat *)buf;
  unsigned char *tmpData = NULL;
  unsigned char *writePtr;
  int lineEnd;
  int ind, numLines, fileXscale, rank;
  hid_t dspaceID = 0, memSpaceID, dsetID, nativeType;
  hsize_t memDim[3], fileCount[3], memCount[3], fileOffset[3], memOffset[3];

  /* Set the mode of the buffer if writing floats and they are not complex */
  if (fromFloat && hdata->mode != MRC_MODE_COMPLEX_FLOAT && 
      hdata->mode != MRC_MODE_FLOAT) {
    bufMode = MRC_MODE_FLOAT;
    convert = 1;
  }

  /* Translate load info to an li to keep code similar to mrc write */
  iiMRCsetLoadInfo(inFile, li);
  padLeft = B3DMAX(0, li->padLeft);
  padRight = B3DMAX(0, li->padRight);
  yStart = li->ymin;
  yEnd = li->ymax;
 
  if (li->xmin || li->xmax != nx - 1) {
    b3dError(stderr, "ERROR: hdfWriteSectionAny - only full lines can be written\n", 
             li->xmin, li->xmax, nx - 1);
    return 1;
  }
  
  if (mrc_getdcsize(hdata->mode, &bytesPerChanOut, &numChanOut) || 
      hdata->mode == MRC_MODE_COMPLEX_SHORT){
    b3dError(stderr, "ERROR: hdfWriteSectionAny - mode not known or not allowed.\n");
    return(-1);
  }
  pixSizeOut = bytesPerChanOut * numChanOut;
  mrc_getdcsize(bufMode, &bytesPerChanBuf, &numChanBuf);
  pixSizeBuf = bytesPerChanBuf * numChanBuf;

  if (convert && sliceModeIfReal(hdata->mode) < 0) {
    b3dError(stderr, "ERROR: hdfWriteSectionAny - floating point data can only be "
             "converted to byte/integer modes\n");
    return 1;
  }

  xDimension = nx + padLeft + padRight;
  bufp += pixSizeBuf * padLeft;
  fbufp += padLeft;

  needData = convert || bytesSigned;
  chunkLines = yEnd + 1 - yStart;
  if (needData) {
    chunkLines = B3DNINT(targetMemory / (nx * pixSizeOut));
    B3DCLAMP(chunkLines, 1, yEnd + 1 - yStart);
    tmpData = B3DMALLOC(unsigned char, pixSizeOut * nx * chunkLines);
    if (!tmpData) {
      b3dError(stderr, "ERROR: hdfWriteSectionAny - getting memory for temporary "
               "array.\n");
      return 2;
    }
  }
  /* printf("ystart %d yend %d needData %d convert %d chunkLines %d bufmode %d filemode"
     " %d\n", yStart, yEnd, needData, convert, chunkLines, bufMode, hdata->mode); */

  /* If neither stack nor dataset are defined, this is the first write, and the file needs
     to be initialized further */
  if (!inFile->stackSetList && !inFile->datasetName && initNewHDFfile(inFile)) {
    cleanupTmp(tmpData, -1, NULL, 0, 0, "Initializing new HDF for writing");
    return 2;
  }

  /* Get the dataset ID, creating stack image if needed */
  if (inFile->stackSetList) {

    /* First make Z map bigger if needed for this section */
    if (cz >= inFile->zMapSize) {
      B3DREALLOC(inFile->zToDataSetMap, int, inFile->zMapSize + 8);
      if (!inFile->zToDataSetMap) {
        inFile->zMapSize = 0;
        cleanupTmp(tmpData, -1, NULL, 0, 0, "Reallocating section map");
        return 1;
      }
      for (ind = inFile->zMapSize; ind < inFile->zMapSize + 8; ind++)
        inFile->zToDataSetMap[ind] = -1;
      inFile->zMapSize += 8;
    }

    /* If it is a new section, create a dataset and add to list/map; otherwise get
       existing dataset */
    if (inFile->zToDataSetMap[cz] < 0) {
      dsetID = createGroupAndDataset(inFile, cz, nameBuf);
      if (dsetID < 0) {
        cleanupTmp(tmpData, -1, NULL, 0, 0, "Creating dataset for new section");
        return 1;
      }
      stackSet.name = strdup(nameBuf);
      stackSet.dsetID = dsetID;
      stackSet.isOpen = 1;
      if (ilistAppend(inFile->stackSetList, &stackSet)) {
        cleanupTmp(tmpData, -1, NULL, 0, 0, "Memory error adding new section to list");
        return 1;
      }
      inFile->zToDataSetMap[cz] = ilistSize(inFile->stackSetList) - 1;
    } else {
      dsetID = getDatasetForZ(inFile, cz, &noDataAtZ);
    }
  } else {
    dsetID = getDatasetForZ(inFile, cz, &noDataAtZ);
  }
  if (dsetID < 0) {
    cleanupTmp(tmpData, -1, NULL, 0, 0, "Opening existing dataset for writing a "
               "section");
    return 1;
  }

  /* Get the multiplier to X dimensions and offsets and make default memory dataspace */
  fileXscale = getFileXscale(inFile->format);
  dspaceID = H5Dget_space(dsetID);
  rank = H5Sget_simple_extent_ndims(dspaceID);
  memCount[0] = memDim[0] = 1;
  memOffset[0] = 0;
  memCount[rank - 2] = memDim[rank - 2] = chunkLines;
  memCount[rank - 1] = nx * fileXscale;
  memOffset[rank - 2] = 0;
  if (needData) {
    memDim[rank - 1] = nx * fileXscale;
    memOffset[rank - 1] = 0;
  } else {
    memDim[rank - 1] = xDimension * fileXscale;
    memOffset[rank - 1] = padLeft * fileXscale;
  }
  memSpaceID = H5Screate_simple(rank, memDim, NULL);
  if (memDim[rank - 1] != memCount[rank - 1] && H5Sselect_hyperslab
      (memSpaceID, H5S_SELECT_SET, memOffset, NULL, memCount, NULL) < 0) {
    cleanupTmp(tmpData, -1, NULL, dspaceID, memSpaceID, "selecting memory area to use");
    return 3;
  }

  fileCount[rank - 1] = nx * fileXscale;
  fileOffset[rank - 1] = 0;
  fileCount[0] = 1;
  fileOffset[0] = cz;

  nativeType = lookupNativeDatatype(inFile);
  line = yStart;
  while (line <= yEnd) {

    /* Set up the ending line and amount to write */
    lineEnd = B3DMIN(yEnd, line + chunkLines - 1);
    numLines = lineEnd + 1 - line;

    /* Revise the memory selection for ending lines */
    if (numLines != memCount[rank - 2]) {
      memCount[rank - 2] = numLines;
      err = H5Sselect_hyperslab(memSpaceID, H5S_SELECT_SET, memOffset, NULL, memCount,
                                NULL);
      if (err < 0) {
        cleanupTmp(tmpData, -1, NULL, dspaceID, memSpaceID,
                   "selecting memory area to use");
        return 3;
      }
    }

    /* Z plane from volume or stack is addressed the same way */
    fileCount[rank - 2] = numLines;
    fileOffset[rank - 2] = line;
    if (H5Sselect_hyperslab(dspaceID, H5S_SELECT_SET, fileOffset, NULL, fileCount,
                              NULL) < 0) {
      cleanupTmp(tmpData, -1, NULL, dspaceID, memSpaceID, 
                 "selecting area to write to file");
      return 3;
    }

    /* Reset the bdata pointer to temp data for each line/chunk, or to the buffer pointer
       if reading directly into buffer.  bufp needs to be maintained for all modes when
       a pointer for another mode is advanced */
    if (needData)
      bdata = tmpData;
    else
      bdata = bufp;
    writePtr = bdata - padLeft * fileXscale;
    
    /* Start loop on Y */
    for (; line <= lineEnd; line++) {
      sdata = (b3dInt16 *)bdata;
      sbdata = (char *)bdata;

      if (convert) {

        /* Convert floats */
        iiConvertLineOfFloats(fbufp, bdata, nx, hdata->mode, bytesSigned);
        fbufp += xDimension;
        bufp = (unsigned char *)fbufp;

      } else {

        /* Unconverted output, shift bytes if needed */
        if (bytesSigned)
          b3dShiftBytes(bufp, sbdata, nx, 1, 1, 1);
        bufp += pixSizeBuf * xDimension;
      }

      /* Advance output data pointer */
      bdata += pixSizeOut * nx;
    }
      
    /* Write the chunk of data */
    if (H5Dwrite(dsetID, nativeType, memSpaceID, dspaceID, H5P_DEFAULT, writePtr) < 0) {
      cleanupTmp(tmpData, -1, NULL, dspaceID, memSpaceID, "Writing data to file");
      return 3;
    }
  }
  cleanupTmp(tmpData, -1, NULL, dspaceID, memSpaceID, NULL);
  return 0;
}


/* 
 * Once the file properties have been set externally; this function gets all the HDF 
 * aspects finalized accordingly and creates a dataset for a 3D volume
*/
int initNewHDFfile(ImodImageFile *inFile)
{
  char buf[36];
  hid_t dsetID;
  int ind, size = B3DMAX(4, inFile->nz);
  if (inFile->zChunkSize) {

    /* 3D volume: create the dataset now with the volume number */
    dsetID = createGroupAndDataset(inFile, inFile->numVolumes - 1, buf);
    if (dsetID < 0)
      return 1;
    inFile->datasetName = strdup(buf);
    inFile->datasetID = dsetID;
    inFile->datasetIsOpen = 1;

    /* Set up a global Adoc if there is not one yet */
    if (inFile->globalAdocIndex < 0) {
      inFile->globalAdocIndex = AdocNew();
      if (inFile->globalAdocIndex < 0)
        return 1;
      for (ind = 0; ind < inFile->numVolumes; ind++)
        inFile->iiVolumes[ind]->globalAdocIndex = inFile->globalAdocIndex;
    }

  } else {

    /* If no Z chunk size, it is a stack, so set up the list and map */
    inFile->stackSetList = ilistNew(sizeof(StackSetData), size);
    inFile->zToDataSetMap = B3DMALLOC(int, size);
    if (!inFile->stackSetList || !inFile->zToDataSetMap)
      return 1;
    inFile->zMapSize = size;
    for (ind = 0; ind < size; ind++)
      inFile->zToDataSetMap[ind] = -1;
  }
  return 0;
}

/*
 * Creates a container group with the given Z value and a dataset inside it, set up
 * appropriately as 2D or 3D
 */
static hid_t createGroupAndDataset(ImodImageFile *inFile, int zValue, char *buf)
{
  hid_t groupID, dsetID, dspaceID, cparms = H5P_DEFAULT, nativeType, useType;
  hid_t accParms = H5P_DEFAULT;
  hsize_t chunkDims[3], fileDims[3], maxDims[3];
  int rank = inFile->zChunkSize > 0 ? 3 : 2;
  int fileXscale = getFileXscale(inFile->format);
  int bytesPerChanBuf, numChanBuf, numChunks;
  size_t nslots, nbytes, nbytesNew;
  double w0;
  sprintf(buf, "/MDF/images/%d", zValue);
  groupID = H5Gcreate((hid_t)inFile->hdfFileID, buf, H5P_DEFAULT, H5P_DEFAULT, 
                      H5P_DEFAULT);
  maxDims[rank - 1] = fileDims[rank - 1] = inFile->nx * fileXscale;
  maxDims[rank - 2] = fileDims[rank - 2] = inFile->ny;
  if (rank > 2) {

    /* Set the chunk sizes for 3D data set */
    cparms = H5Pcreate(H5P_DATASET_CREATE);
    maxDims[0] = H5S_UNLIMITED;
    fileDims[0] = inFile->nz;
    chunkDims[0] = inFile->zChunkSize;
    if (inFile->tileSizeY <= 0 || inFile->tileSizeY > inFile->ny)
      chunkDims[1] = inFile->ny;
    else
      chunkDims[1] = inFile->tileSizeY;
    if (inFile->tileSizeX <= 0 || inFile->tileSizeX > inFile->nx)
      chunkDims[2] = inFile->nx * fileXscale;
    else
      chunkDims[2] = inFile->tileSizeX * fileXscale;
    if (H5Pset_chunk(cparms, rank, chunkDims) < 0)
      return -1;

    /* Set the chunk cache big enough for a full row of actual chunks if there are 
     multiple chunks in X.  Its seems no increase in cache is needed if there are no
    chunks in X, even when there are in Y */
    accParms = H5Pcreate(H5P_DATASET_ACCESS);
    if (H5Pget_chunk_cache(accParms, &nslots, &nbytes, &w0) < 0)
      return -1;
    mrc_getdcsize(inFile->mode, &bytesPerChanBuf, &numChanBuf);
    numChunks = (inFile->nx + chunkDims[2] - 1) / chunkDims[2];
    nbytesNew = (bytesPerChanBuf * numChunks * chunkDims[2] * fileXscale * chunkDims[1] *
                 11) / 10;
    if (numChunks > 1 && nbytesNew > nbytes && 
        H5Pset_chunk_cache(accParms, nslots, nbytesNew, w0) < 0)
      return -1;
  }
  nativeType = lookupNativeDatatype(inFile);
  useType = H5Tcopy(nativeType);
  if (H5Tget_precision(useType) > 8 && ((MrcHeader *)inFile->header)->swapped) {
    if (H5Tget_order(nativeType) == H5T_ORDER_LE)
      H5Tset_order(useType, H5T_ORDER_BE);
    else
      H5Tset_order(useType, H5T_ORDER_LE);
  }
  dspaceID = H5Screate_simple(rank, fileDims, maxDims); 
  dsetID = H5Dcreate(groupID, "image", useType, dspaceID, H5P_DEFAULT, cparms, 
                     accParms);
  if (dspaceID < 0 || dsetID < 0)
    return -1;
  if (rank > 2) {
    H5Pclose(cparms);
    H5Pclose(accParms);
  }
  H5Tclose(useType);
  H5Gclose(groupID);
  H5Sclose(dspaceID);
  sprintf(buf, "/MDF/images/%d/image", zValue);
  return dsetID;
}

/*
 * Get the scaling to apply to X dimensions and coordinates in the file for RGB or complex
 * data
 */
static int getFileXscale(int format)
{
  int scale = 1;
  if (format == IIFORMAT_RGB)
    scale = 3;
  if (format == IIFORMAT_COMPLEX)
    scale = 2;
  return scale;
}

/*
 * Look up the native data type  of the data in file 
 */
static hid_t lookupNativeDatatype(ImodImageFile *inFile)
{
  int ind, nativeInd = 0;
  hid_t nativeType[5];
  int iiTypeLookup[5] = {IITYPE_BYTE, IITYPE_UBYTE, IITYPE_SHORT, IITYPE_USHORT,
                         IITYPE_FLOAT};
  nativeType[0] = H5T_NATIVE_CHAR;
  nativeType[1] = H5T_NATIVE_UCHAR;
  nativeType[2] = H5T_NATIVE_SHORT;
  nativeType[3] = H5T_NATIVE_USHORT;
  nativeType[4] = H5T_NATIVE_FLOAT;
  
  for (ind = 0; ind < 5; ind++)
    if (iiTypeLookup[ind] == inFile->type)
      nativeInd = ind;
  return nativeType[nativeInd];
}

/*
 * Returns a dataset ID for reading from a Z slice, opening the set if needed; sets noData
 * if there is no such Z slice.
 */
static hid_t getDatasetForZ(ImodImageFile *inFile, int cz, int *noData)
{
  int ind;
  hid_t dsetID, fileID;
  StackSetData *stackp;
  fileID = (hid_t)inFile->hdfFileID;
  *noData = 0;
  if (inFile->stackSetList) {

    /* For a stack, see if section actually exists, and get the ID or open the set */
    ind = inFile->zToDataSetMap[cz];
    if (ind < 0) {
      *noData = 1;
      return 0;
    }
    stackp = (StackSetData *)ilistItem(inFile->stackSetList, ind);
    if (stackp->isOpen)
      dsetID = stackp->dsetID;
    else {
      dsetID = H5Dopen(fileID, stackp->name, H5P_DEFAULT);
      stackp->isOpen = 1;
    }
  } else {

    /* Similarly for volume dataset get the ID or open the set */
    if (inFile->datasetIsOpen)
      dsetID = inFile->datasetID;
    else {
      dsetID = H5Dopen(fileID, inFile->datasetName, H5P_DEFAULT);
      inFile->datasetID = dsetID;
      inFile->datasetIsOpen = 1;
    }
  }
  return dsetID;
}

/*
 * Close opened items and free temp data, setting up an error message if mess non-Null.
 */
static void cleanupTmp(unsigned char *tmpData, int freeMap, unsigned char *map,
                      hid_t dspaceID, hid_t memSpaceID, const char *mess)
{
  fflush(stdout);
  if (memSpaceID)
    H5Sclose(memSpaceID);
  if (dspaceID)
    H5Sclose(dspaceID);
  if (tmpData)
    free(tmpData);
  if (freeMap > 0)
    free(map);
  if (mess)
    b3dError(stderr, "ERROR: hdf%sSectionAny - %s.\n", freeMap >= 0 ? "Read" : "Write", 
             mess);
}
