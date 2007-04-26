/*  
 *  imodmop.c --  Create an mrc image file from a model
 *                and an mrc image file.
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *  Based on imodmop and imodcmopp by James Kremer
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 * Log at end of file
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "imodel.h"
#include "mrcfiles.h"
#include "mrcslice.h"

static int paintContours(Iobj *obj, Islice *islice, Islice *pslice[3], 
                         int numChan, int inside, IloadInfo *li, int iz);
static void scaleAndCombineSlices(Islice *pslice[3], Islice *oslice, 
                                  float smin, float smax, int numFiles);
static void paintScatPoints(Iobj *obj, Islice *islice, Islice *pslice[3], 
                            int numChan, IloadInfo *li, int scat3D, int iz);
static void paintScanContour(Iobj *obj, Icont *cont, Islice *islice,
                             Islice *pslice[3], int numChan, IloadInfo *li,
                             int fill);
static void paintTubularLines(Iobj *obj, Islice *islice, Islice *pslice[3], 
                              int numChan, IloadInfo *li, float rad, int iz);
static void putSliceValue(Iobj *obj,  Islice *islice, Islice *pslice[3],
                          int numChan, IloadInfo *li, int x, int y, 
                          Ival inval);
static void scaleAndCombineSlices(Islice *pslice[3], Islice *oslice,
                                  float smin, float smax, int numFiles);

#define FILE_STR_SIZE 4096
#define MAX_TUBE_DIAMS 1024

int main( int argc, char *argv[])
{
  char *infile;
  char *modfile;
  char *outfile;
  FILE *gfin  = NULL;
  FILE *gfout = NULL;
  FILE *files[3];
  char *Yaxis = "Y";
  char *axis = Yaxis;
  char *dot = ".";
  char *tempDir = dot;
  int numObj = 0, scat2D = 0, scat3D = 0, retain = 0, constScale = 0;
  int numDiams, numTubes = 0;
  int black = 0, white = 255;
  char *listString;
  int *objList;
  int *tubeList;
  float tubeDiams[MAX_TUBE_DIAMS];
  Imod *imod;
  Iobj *obj;
  Ipoint *pts;
  int ob, co, pt;
  float xload, yload, zload;
  IrefImage *ref;
  MrcHeader hdata, hdbyte;
  IloadInfo li;
  MrcHeader hdout[3];
  char recnames[3][FILE_STR_SIZE];
  char xyznames[3][FILE_STR_SIZE];
  char comStr[3 * FILE_STR_SIZE];
  Islice *islice, *oslice, *rgbslice;
  Islice *pslice[3];
  int i, k, ix, iy, ifXin, ifYin, ifZin, dsize, csize, outMode, pid;
  int invert = 0, reverse = 0, ifThresh = 0, ifProj = 0, rgbOut = 0;
  int xmin, ymin, zmin, xmax, ymax, zmax, numChan, numFiles, objnum;
  int nxout, nyout, nzout, inside, numOptArgs,numNonOptArgs;
  float start_tilt, end_tilt, inc_tilt;
  float thresh, smin, smax, revMax;
  Ival val, pval;

  /* Fallbacks from    ../manpages/autodoc2man 2 1 imodmop  */
  int numOptions = 19;
  char *options[] = {
    "xminmax:XMinAndMax:IP:", "yminmax:YMinAndMax:IP:", 
    "zminmax:ZMinAndMax:IP:", "scale:ScalingMinMax:FP:", 
    "invert:InvertPaintedArea:B:", "color:ColorOutput:B:", 
    "project:ProjectTiltSeries:FT:", "axis:AxisToTiltAround:CH:", 
    "objects:ObjectsToDo:LI:", "2dscat:2DScatteredPoints:B:", 
    "3dscat:3DScatteredPoints:B:", "tube:TubeObjects:LI:", 
    "diam:DiameterForTubes:F:", "constant:ConstantScaling:B:", 
    "bw:BlackAndWhite:IP:", "reverse:ReverseContrast:B:",
    "thresh:Threshold:F:", 
    "tempdir:TemporaryDirectory:CH:", "keep:KeepTempFiles:B:"};

  char *progname = imodProgName(argv[0]);

  /* Startup with fallback */
  PipReadOrParseOptions(argc, argv, options, numOptions, progname, 
                        0, 2, 1, &numOptArgs, &numNonOptArgs, imodUsageHeader);

  /* Need special usage statement because of 3 filename arguments */
  if (numNonOptArgs < 3){
    imodVersion(progname);
    imodCopyright();
    printf("Usage: %s [options] model_file input_image output_image\n",
           progname);
    if (!numNonOptArgs)
      PipPrintHelp(progname, 0, 2, 1);
    exit(3);
  }

  PipGetNonOptionArg(0, &modfile);
  PipGetNonOptionArg(1, &infile);
  PipGetNonOptionArg(2, &outfile);

  /* Get input model */
  imod = imodRead(modfile);
  if (!imod)
    exitError("Reading model file %s\n", modfile);
  free(modfile);
  
  /* Open input image, read header */
  gfin = fopen(infile, "rb");
  if (gfin == NULL)
    exitError("Could not open %s\n", infile);
  if (mrc_head_read(gfin, &hdata))
    exitError("Reading header of %s\n", infile);
  free(infile);

  /* Get more options after setting defaults */
  li.xmin = li.ymin = li.zmin = 0;
  li.xmax = hdata.nx - 1;
  li.ymax = hdata.ny - 1;
  li.zmax = hdata.nz - 1;
  ifXin = 1 - PipGetTwoIntegers("XMinAndMax", &li.xmin, &li.xmax);
  ifYin = 1 - PipGetTwoIntegers("YMinAndMax", &li.ymin, &li.ymax);
  ifZin = 1 - PipGetTwoIntegers("ZMinAndMax", &li.zmin, &li.zmax);
  li.xmin = B3DMAX(0, li.xmin);
  li.ymin = B3DMAX(0, li.ymin);
  li.zmin = B3DMAX(0, li.zmin);
  li.xmax = B3DMIN(hdata.nx - 1, li.xmax);
  li.ymax = B3DMIN(hdata.ny - 1, li.ymax);
  li.zmax = B3DMIN(hdata.nz - 1, li.zmax);
  if (li.xmax <= li.xmin || li.ymax <= li.ymin || li.zmax < li.zmin)
    exitError("Coordinate limits are out of order\n");

  PipGetBoolean("InvertPaintedArea", &invert);
  PipGetBoolean("ReverseContrast", &reverse);
  PipGetBoolean("ColorOutput", &rgbOut);
  ifThresh = 1 - PipGetFloat("Threshold", &thresh);

  /* Projection option */
  ifProj = 1 - PipGetThreeFloats("p", &start_tilt, &end_tilt, &inc_tilt);
  if (ifProj) {
    PipGetString("TemporaryDirectory", &tempDir);
    PipGetBoolean("ConstantScaling", &constScale);
    PipGetTwoIntegers("BlackAndWhite", &black, &white);
    PipGetString("AxisToTiltAround", &axis);
    if (!strcmp(axis, "X") && !strcmp(axis, "Y") && !strcmp(axis, "Z"))
      exitError("Axis extry must be X, Y, or Z\n");

    if ((start_tilt > end_tilt) && (inc_tilt > 0))
      exitError("Ending tilt must be > starting tilt with a positive "
                "increment\n");
    if ((end_tilt > start_tilt) && (inc_tilt < 0))
      exitError("Ending tilt must be < starting tilt with a negative "
                "increment\n");
    if (!inc_tilt && end_tilt == start_tilt)
      exitError("Tilt increment must be non-zero unless start equals end\n");

    if (black < 0 || black > 255 || white < 0 || white > 255 || black == white)
      exitError("Black/white values must be in range 0-255 and nonequal\n");
  }

  PipGetBoolean("KeepTempFiles", &retain);
  PipGetBoolean("2DScatteredPoints", &scat2D);
  PipGetBoolean("3DScatteredPoints", &scat3D);
  if (scat2D && scat3D)
    exitError("You can not enter both -2 and -3\n");

  /* Get object and tube lists */
  if (!PipGetString("ObjectsToDo", &listString)) {
    objList = parselist(listString, &numObj);
    free(listString);
  }

  if (!PipGetString("TubeObjects", &listString)) {
    tubeList = parselist(listString, &numTubes);
    free(listString);

    numDiams = 0;
    if (PipGetFloatArray("DiameterForTubes", tubeDiams, &numDiams, 
                         MAX_TUBE_DIAMS)) {
      numDiams = 1;
      tubeDiams[0] = 25.;
    }
    if (numDiams != 1 && numDiams != numTubes)
      exitError("You must enter either 1 diameter or 1 per selected "
                "tube object");
    for (i = numDiams; i < numTubes; i++)
      tubeDiams[i] = tubeDiams[0];
  }

  /* Check for incompatible options */
  if (hdata.mode == MRC_MODE_COMPLEX_FLOAT || 
      hdata.mode == MRC_MODE_COMPLEX_SHORT) {
    if (reverse || rgbOut || ifProj)
      exitError("You can not use -reverse, -color, or -project"
                " with FFT data\n");
    if (ifXin || ifYin)
      exitError("You can not limit the X or Y range with FFT data\n");
    if (ifZin && scat3D)
      exitError("You can not limit the Z range of 3D FFT data\n");
  }

  if (hdata.mode == MRC_MODE_RGB && (rgbOut || ifProj))
    exitError("You can not use -color or -project with RGB input data\n");

  if (rgbOut && invert)
    exitError("You can not use -invert when making colored data\n");

  if (mrc_getdcsize(hdata.mode, &dsize, &csize))
    exitError("Unsupported input data mode %d\n", hdata.mode);

  /* Set up scaling for rgb volume output; default for byte input is no
     scaling */
  if (rgbOut && !ifProj) {
    smin = hdata.amin;
    smax = hdata.amax;
    if (hdata.mode == MRC_MODE_BYTE) {
      smin = 0;
      smax = 255.;
    }
    if (!PipGetTwoFloats("ScalingMinMax", &smin, &smax) || 
        (hdata.mode != MRC_MODE_BYTE)) {
      
      if (reverse) {
        val[0] = hdata.amax - smin;
        smin = hdata.amax - smax;
        smax = val[0];
      }
      if (smin >= smax)
        exitError("Minimum density for scaling should be less than maximum\n");
    }
  }

  /* Reverse threshold so user can enter in terms of original units */
  if (reverse && ifThresh)
    thresh = hdata.amax - thresh;
  PipDone();

  /* Shift the model if it was loaded on a subset (takes care of 
     mirrored/nonmirrored FFT problem too) */
  ref = imod->refImage;
  if (ref && ref->cscale.x && ref->cscale.y && ref->cscale.z) {
    xload = (hdata.xorg - ref->ctrans.x) / ref->cscale.x;
    yload = (hdata.yorg - ref->ctrans.y) / ref->cscale.y;
    zload = (hdata.zorg - ref->ctrans.z) / ref->cscale.z;
    if (xload || yload || zload) {
      for (ob = 0; ob < imod->objsize; ob++) {
        for (co = 0; co < imod->obj[ob].contsize; co++) {
          pts = imod->obj[ob].cont[co].pts;
          for (pt = 0; pt < imod->obj[ob].cont[co].psize; pt++) {
            pts[pt].x += xload;
            pts[pt].y += yload;
            pts[pt].z += zload;
          }
        }
      }
    }
  }
 
  /* Open final output file now */
  imodBackupFile(outfile);
  gfout = fopen(outfile, "wb");
  if (gfout == NULL)
    exitError("Could not open %s\n", outfile);
  free(outfile);
     
  /* Open temp files for projections */
  numChan = rgbOut ? 3 : 1;
  numFiles = 1;
  files[0] = gfout;
  if (ifProj) {
    numFiles = numChan;
    pid = getpid();
    for (i = 0; i < numChan; i++) {
      sprintf(recnames[i], "%s/%s.rec%d.%d", tempDir, progname, i, pid);
      sprintf(xyznames[i], "%s/%s.xyz%d.%d", tempDir, progname, i, pid);
      files[i] = fopen(recnames[i], "wb");
      if (!files[i])
        exitError("Could not open %s\n", recnames[i]);
    }
  }

  /* Set up output file(s) in correct mode and size */
  outMode = hdata.mode;
  nxout = li.xmax + 1 - li.xmin;
  nyout = li.ymax + 1 - li.ymin;
  nzout = li.zmax + 1 - li.zmin;
  if (rgbOut && !ifProj)
    outMode = MRC_MODE_RGB;
  for (i = 0; i < numFiles; i++) {
    mrc_head_new(&hdout[i], nxout, nyout, nzout, outMode);
    mrc_head_label_cp(&hdata, &hdout[i]);
    sprintf(comStr, "%s: Painted image from model", progname);
    mrc_head_label(&hdout[i], comStr);
    hdout[i].amin = 1.e30;
    hdout[i].amax = -1.e30;
    hdout[i].amean = 0.;
  }
     
  /* Get slices for input and output data */
  islice = sliceCreate(nxout, nyout, hdata.mode);
  if (!islice)
    exitError("Memory error creating slice for input\n");
  for (i = 0; i < numChan; i++) {
    pslice[i] = sliceCreate(nxout, nyout, hdata.mode);
    if (!pslice[i])
      exitError("Memory error creating slice for painted data\n");
  }
  if (rgbOut && !ifProj) {
    rgbslice = sliceCreate(nxout, nyout, MRC_MODE_RGB);
    if (!rgbslice)
      exitError("Memory error creating slice for RGB painted data\n");
  }
     
  /* Loop on slices */
  revMax = (hdata.mode == MRC_MODE_RGB ? 255 : hdata.amax);
  for (k = li.zmin; k <= li.zmax; k++) {
    printf("Painting section %d ", k);
    fflush(stdout);
    if (mrcReadZ(&hdata, &li, islice->data.b, k))
      exitError("Reading data from file for slice %d", k);
    
    /* Apply contrast reversal and/or thresholding */
    if (reverse || ifThresh) {
      val[1] = val[2] = 0.;
      for (iy = 0; iy < nyout; iy++) {
        for (ix = 0; ix < nxout; ix++) {
          sliceGetVal(islice, ix, iy, val);
          if (reverse)
            for (i = 0; i < csize; i++)
              val[i] = revMax - val[i];

          if (ifThresh && sliceGetValMagnitude(val, hdata.mode) < thresh)
            for (i = 0; i < csize; i++)
              val[i] = 0.;
          slicePutVal(islice, ix, iy, val);
        }
      }
    }

    val[0] = val[1] = val[2] = 0.;
    for (i = 0; i < numChan; i++)
      sliceClear(pslice[i], val);

    /* Do two passes through objects, doing inside-out ones on second pass */
    for (inside = 0; inside < 2; inside++) {
      for (objnum = 0; objnum < imod->objsize; objnum++) {
        //printf("iz %d pass %d object %d\n", k, inside, objnum);

        /* Check if object on list */
        if (numObj) {
          i = 0;
          for (ix = 0; ix < numObj; ix++)
            if (objList[ix] == objnum + 1)
              i = 1;
          if (!i)
            continue;
        }
        obj = &(imod->obj[objnum]);

        /* Do open objects if on tube list */
        if (iobjOpen(obj->flags)) {
          i = -1;
          for (ix = 0; ix < numTubes; ix++)
            if (tubeList[ix] == objnum + 1)
              i = ix;
          if (i >= 0 && !inside)
            paintTubularLines(obj, islice, pslice, numChan, &li,
                              tubeDiams[i] /2., k);
          
        /* Do scattered objects if specified and on first pass only */
        } else if (iobjScat(obj->flags)) {
          if (!inside && (scat2D || scat3D))
            paintScatPoints(obj, islice, pslice, numChan, &li, scat3D, k);
        } else {

          /* Do closed objects on appropriate pass */
          if ((inside && (obj->flags & IMOD_OBJFLAG_OUT)) ||
              (!inside && !(obj->flags & IMOD_OBJFLAG_OUT))) {
            if (paintContours(obj, islice, pslice, numChan, inside, &li, k))
              exitError("Analyzing contours for object %d\n", objnum + 1);
          }
        }
      }
    }

    /* To invert, subtract painted data from the original data */
    if (invert) {
      for (iy = 0; iy < nyout; iy++) {
        for (ix = 0; ix < nxout; ix++) {
          sliceGetVal(islice, ix, iy, val);
          sliceGetVal(pslice[0], ix, iy, pval);
          for (i = 0; i < csize; i++)
            pval[i] = val[i] - pval[i];
          slicePutVal(pslice[0], ix, iy, pval);
        }
      }
    }

    /* write data and maintain min/max/mean */
    for (i = 0; i < numFiles; i++) {
      oslice = pslice[i];
      if (rgbOut && !ifProj) {
        scaleAndCombineSlices(pslice, rgbslice, smin, smax, 3);
          oslice = rgbslice;
      }
      sliceMMM(oslice);
      hdout[i].amin = B3DMIN(hdout[i].amin, oslice->min);
      hdout[i].amax = B3DMAX(hdout[i].amax, oslice->max);
      hdout[i].amean += oslice->mean / nzout;
      if (mrc_write_slice(oslice->data.b, files[i], &hdout[i], k - li.zmin,
                          'Z'))
        exitError("Writing slice at Z = %d to file # %d\n", k - li.zmin, i +1);
    }
    printf("\r");
  }
  printf("\n");

  /* Write headers, close files, delete slices */
  for (i = 0; i < numFiles; i++) {
    if (mrc_head_write(files[i], &hdout[i]))
      exitError("Writing header to file # %d\n", i + 1);
    fclose(files[i]);
  }
  fclose(gfin);
  sliceFree(islice);
  for (i = 0; i < numChan; i++)
    sliceFree(pslice[i]);
  if (rgbOut && !ifProj)
    sliceFree(rgbslice);

  if (!ifProj)
    exit(0);

  /* PROJECTING: Run xyzproj on the files */
  for (i = 0; i < numFiles; i++) {
    printf("Projecting file # %d...\n", i + 1);
    fflush(stdout);
    sprintf(comStr, "xyzproj -mode 2 -axis %s -angles %f,%f,%f %s %s %s %s",
            axis, start_tilt, end_tilt, inc_tilt, constScale ? "-const" : " ",
            invert ? " " : "-fill 0", recnames[i], xyznames[i]);
    ix = system(comStr);
    if (ix)
      exitError("Running xyzproj on file %d (return value %d)\n",
                recnames[i], ix);
    if (!retain)
      remove(recnames[i]);
  }

  /* Open the files, determine size, set up output file */
  smin = 1.e30;
  smax = -1.e30;
  printf("Scaling data into final output file...\n");
  for (i = 0; i < numFiles; i++) {
    files[i] = fopen(xyznames[i], "rb");
    if (files[i] == NULL)
      exitError("Could not open %s\n", xyznames[i]);
    if (mrc_head_read(files[i], &hdout[i]))
      exitError("Reading header of %s\n", xyznames[i]);
    smin = B3DMIN(smin, hdout[i].amin);
    smax = B3DMAX(smax, hdout[i].amax);
  }

  outMode =  rgbOut ? MRC_MODE_RGB : MRC_MODE_BYTE;
  mrc_head_new(&hdbyte, hdout[0].nx, hdout[0].ny, hdout[0].nz, outMode);
  mrc_head_label_cp(&hdata, &hdbyte);
  sprintf(comStr, "%s: Projected image painted from model", progname);
  mrc_head_label(&hdbyte, comStr);
  hdbyte.amin = 1.e30;
  hdbyte.amax = -1.e30;
  hdbyte.amean = 0.;

  /* Adjust scaling by finding the values that would map to black and white
     after mapping smin/smax to 0/255 */
  val[0] = smin + black * (smax - smin) / 255.;
  smax = smin + white * (smax - smin) / 255.;
  smin = val[0];

  /* Get slices for new size of data */
  oslice = sliceCreate(hdout[0].nx, hdout[0].ny, outMode);
  if (!oslice)
    exitError("Memory error creating slice for output\n");
  for (i = 0; i < numFiles; i++) {
    pslice[i] = sliceCreate(hdout[0].nx, hdout[0].ny, hdout[0].mode);
    if (!pslice[i])
      exitError("Memory error creating slice for reading projections\n");
  }

  /* Read in the data and scale it into output, write it */
  for (k = 0; k < hdout[0].nz; k++) {
    for (i = 0; i < numFiles; i++) {
      if (mrc_read_slice(pslice[i]->data.b, files[i], &hdout[i], k, 'Z'))
        exitError("Reading slice %d from file # %d\n", k, i + 1);
    }
    scaleAndCombineSlices(pslice, oslice, smin, smax, numFiles);
    sliceMMM(oslice);
    hdbyte.amin = B3DMIN(hdbyte.amin, oslice->min);
    hdbyte.amax = B3DMAX(hdbyte.amax, oslice->max);
    hdbyte.amean += oslice->mean / hdbyte.nz;
    if (mrc_write_slice(oslice->data.b, gfout, &hdbyte, k, 'Z'))
        exitError("Writing slice at Z = %d to final output file\n", k);
  }

  /* Finalize header and clean up */
  if (mrc_head_write(gfout, &hdbyte))
    exitError("Writing header to final output file\n");
  fclose(gfout);
  sliceFree(oslice);
  for (i = 0; i < numFiles; i++) {
    fclose(files[i]);
    sliceFree(pslice[i]);
    if (!retain)
      remove(xyznames[i]);
  }

  return(0);
}     


int paintContours(Iobj *obj, Islice *islice, Islice *pslice[3], int numChan,
                  int inside, IloadInfo *li, int iz)
{
  int co;
  Icont *cont;
  Icont **scancont;
  int zmin,zmax;
  int indz;
  int nummax, inbox;

  int *contz;
  Ipoint *pmin, *pmax;
  int *numatz;
  int **contatz;
  int *zlist;
  int  kis, zlsize;
  int numnests;
  int eco;
  Nesting *nests;
  int *nestind;
  int numwarn = -1;
  int level, doLevel, found;

  if (!obj->contsize)
    return 0;
     
  if (imodContourMakeZTables(obj, 1, 0, &contz, &zlist, &numatz, &contatz, 
                             &zmin, &zmax, &zlsize, &nummax))
    return -1;

  if (iz < zmin || iz > zmax || !numatz[iz - zmin]) {
    imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);
    return 0;
  }

  indz = iz - zmin;
  nummax = numatz[indz];

  /* Allocate space for lists of min's max's, and scan contours */
  pmin = (Ipoint *)malloc(nummax * sizeof(Ipoint));
  pmax = (Ipoint *)malloc(nummax * sizeof(Ipoint));
  scancont = (Icont **)malloc(nummax * sizeof (Icont *)); 

  /* Get array for index to inside/outside information */
  nestind = (int *)malloc(nummax * sizeof(int));
  if (!pmin || !pmax || !scancont || !nestind)
    return -1;

  /* Make scan contours and get mins/maxes for ones with non-empty scans */
  inbox = 0;
  numnests = 0;
  for (kis = 0; kis < numatz[indz]; kis++) {
    co = contatz[indz][kis];
    cont = &(obj->cont[co]);
    scancont[inbox] = imodel_contour_scan(cont);
    if (scancont[inbox]->psize) {
      imodContourGetBBox(cont, &pmin[inbox], &pmax[inbox]);
      nestind[inbox++] = -1;
    } else
      imodContourDelete(scancont[inbox]);
  }

  /* Look for overlapping contours as in imodmesh */
  for (co = 0; co < inbox - 1; co++) {
    for (eco = co + 1; eco < inbox; eco++) {
      if (imodContourCheckNesting(co, eco, scancont, pmin, pmax, &nests,
                                    nestind, &numnests, &numwarn))
        return -1;
    }
  }

  /* Analyze inside and outside contours to determine level, then go through
     contours from lowest level inward */
  imodContourNestLevels(nests, nestind, numnests);
  doLevel = 1;
  do {
    found = 0;
    for (co = 0; co < inbox; co++) {
      level = 1;
      if (nestind[co] >= 0)
        level = nests[nestind[co]].level;
      if (level != doLevel)
        continue;
      found = 1;
      paintScanContour(obj, scancont[co], islice, pslice, numChan, li, 
                       (level + inside) % 2);
    }
    doLevel++;
  } while (found);

  /* clean up inside the nests */
  imodContourFreeNests(nests, numnests);
  
  /* clean up scan conversions */
  for (co = 0; co < inbox; co++)
    imodContourDelete(scancont[co]);
  
  /* clean up everything else */
  free(nestind);

  imodContourFreeZTables(numatz, contatz, contz, zlist, zmin, zmax);
  free(scancont);
  free(pmin);
  free(pmax);
  return 0;
}

void paintScanContour(Iobj *obj, Icont *cont, Islice *islice,
                      Islice *pslice[3], int numChan, IloadInfo *li, int fill)
{
  Ival inval;
  int j, x, y, xst, xnd;

  /* Initialize value to 0 if not filling */
  if (!fill)
    inval[0] = inval[1] = inval[2] = 0.;
  for (j = 0; j < cont->psize; j += 2) {

    /* Move Y down by 1 because otherwise bumps in X occur at the wrong Y
       height.  This is not perfect */
    y = cont->pts[j].y - 1;

    /* Check if line is in the volume and get X limits */
    if (y < li->ymin || y > li->ymax)
      continue;

    /* And here truncation matches the positions better than taking the nint,
       and running to < xnd instead of to <= xnd.  */
    xst = cont->pts[j].x;
    xst = B3DMAX(xst, li->xmin);
    xnd = cont->pts[j + 1].x;
    xnd = B3DMIN(xnd, li->xmax);
    for (x = xst; x < xnd; x++) {

      /* Get value if filling */
      if (fill)
        sliceGetVal(islice, x - li->xmin, y - li->ymin, inval);
      putSliceValue(obj, islice, pslice, numChan, li, x, y, inval);
    }
  }
}

void paintScatPoints(Iobj *obj, Islice *islice, Islice *pslice[3], int numChan,
                    IloadInfo *li, int scat3D, int iz)
{
  Ival inval;
  Icont *cont;
  Ipoint *pnt;
  int co, pt, x, y, xst, xnd, yst, ynd;
  float rad, size;
  double dx, dy, dz;

  /* Loop on all points in all contours */
  for (co = 0; co < obj->contsize; co++) {
    cont = & obj->cont[co];
    for (pt = 0; pt < cont->psize; pt++) {
      pnt = &cont->pts[pt];

      /* Get size, check if point is within bounds in X */
      size = imodPointGetSize(obj, cont, pt);
      if (pnt->x + size < li->xmin || pnt->x - size > li->xmax || 
          pnt->y + size < li->ymin || pnt->y - size > li->ymax)
        continue;

      /* Check if Z is within range (3D) or right on (2D), get radius on
         current Z  for 3D */
      if (scat3D) {
        dz = fabs((double)(pnt->z - iz));
        if (dz >= size)
          continue;
        rad = (float)sqrt(size * size - dz * dz);

      } else {
        if (B3DNINT(cont->pts[pt].z) != iz)
          continue;
        rad = size;
      }

      /* Should this limit be here ? */
      if (rad < 0.25)
        continue;

      /* Loop on each line in circle, get X limits. Here, subtract 0.5 from 
         Y coordinates since the middle of a pixel is at 0.5, 0.5 */
      yst = B3DNINT(pnt->y - 0.5 - rad);
      ynd = B3DNINT(pnt->y - 0.5 + rad);
      for (y = yst; y <= ynd; y++) {
        dy = fabs((double)(pnt->y - 0.5 - y));
        if (dy > rad)
          continue;
        dx = sqrt(rad * rad - dy * dy);

        /* But subtracting 0.5 from both xst and xnd, then running from xst to
           xnd inclusive made it ~0.5 pixel too wide.  So don't subtract 0.5,
           and run to < xnd instead of <= xnd, and it's pretty good */
        xst = B3DNINT(pnt->x - dx);
        xst = B3DMAX(xst, li->xmin);
        xnd = B3DNINT(pnt->x + dx);
        xnd = B3DMIN(xnd, li->xmax);
        for (x = xst; x < xnd; x++) {
          sliceGetVal(islice, x - li->xmin, y - li->ymin, inval);
          putSliceValue(obj, islice, pslice, numChan, li, x, y, inval);
        }
      }
    }
  }
}

static void paintTubularLines(Iobj *obj, Islice *islice, Islice *pslice[3], 
                              int numChan, IloadInfo *li, float rad, int iz)
{
  Ival inval;
  Icont *cont;
  Ipoint *ptcur, *ptlas;
  Ipoint pix;
  int co, pt, x, y, xst, xnd, yst, ynd;
  float radsq, tval, radinc;
  double dz, dzlas;

  /* Loop on all line segments in all contours */
  radsq = rad * rad;
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    ptlas = cont->pts;
    dzlas = fabs((double)ptlas->z - iz);
    for (pt = 1; pt < cont->psize; pt++) {
      ptcur = &cont->pts[pt];
      dz = fabs((double)ptcur->z - iz);
      if (dz <= rad || dzlas <= rad) {

        /* If either endpoint is within radius in Z, make a box around the
           segment */
        radinc = rad + 2.;
        xst = (int)B3DMIN(ptcur->x - radinc, (int)ptlas->x - radinc);
        xnd = (int)B3DMAX(ptcur->x + radinc, (int)ptlas->x + radinc);
        yst = (int)B3DMIN(ptcur->y - radinc, (int)ptlas->y - radinc);
        ynd = (int)B3DMAX(ptcur->y + radinc, (int)ptlas->y + radinc);
        xst = B3DMAX(xst, li->xmin);
        xnd = B3DMIN(xnd, li->xmax);
        yst = B3DMAX(yst, li->ymin);
        ynd = B3DMIN(ynd, li->ymax);
        for (y = yst; y <= ynd; y++) {
          for (x = xst; x <= xnd; x++) {
            float dist;
            pix.x = x + 0.5f;
            pix.y = y + 0.5f;
            pix.z = iz;
            dist = imodPointLineSegDistance(ptlas, ptcur, &pix, &tval);
            if (dist <= radsq) {
              sliceGetVal(islice, x - li->xmin, y - li->ymin, inval);
              putSliceValue(obj, islice, pslice, numChan, li, x, y, inval);
            }
          }
        }
      }
      dzlas = dz;
      ptlas = ptcur;
    }
  }    
}

static void putSliceValue(Iobj *obj,  Islice *islice, Islice *pslice[3],
                          int numChan, IloadInfo *li, int x, int y, Ival inval)
{
  Ival outval;
  if (numChan < 3)
    slicePutVal(pslice[0],  x - li->xmin, y - li->ymin, inval);
  else {
    
    /* If doing 3 channels, only first element is needed */
    outval[0] = obj->red * inval[0];
    slicePutVal(pslice[0],  x - li->xmin, y - li->ymin, outval);
    outval[0] = obj->green * inval[0];
    slicePutVal(pslice[1],  x - li->xmin, y - li->ymin, outval);
    outval[0] = obj->blue * inval[0];
    slicePutVal(pslice[2],  x - li->xmin, y - li->ymin, outval);
  }
}

static void scaleAndCombineSlices(Islice *pslice[3], Islice *oslice,
                                  float smin, float smax, int numFiles)
{
  Ival inval, outval;
  int x, y, i;
  for (y = 0; y < oslice->ysize; y++) {
    for (x = 0; x < oslice->xsize; x++) {
      for (i = 0; i < numFiles; i++) {
        sliceGetVal(pslice[i], x, y, inval);
        outval[i] = 255. * (inval[0] - smin) / (smax - smin);
        outval[i] = B3DMAX(0., B3DMIN(255., outval[i]));
      }
      slicePutVal(oslice, x, y, outval);
    }
  }
}


/*
$Log$

*/
