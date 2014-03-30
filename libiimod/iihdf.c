/******************************************************************************
 *  iihdf.c - HDF file opening, management, and header/attribute access
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
 * Structure to hold information about all groups as file is scanned
 */
typedef struct {
  hid_t groupID;
  char *name;
  unsigned char hasValidDatasets;   /* Flag for whether there are valid datasets */
  unsigned char hasAnyDatasets;     /* Flag for whether there are any datasets */
  unsigned char hasGroups;          /* Flag for whether there are subgroups */
  unsigned char nonGlobalAttrib;    /* Flag for whether there are non-global attributes */
  short addedGlobal;           /* Flag for whether global attributes were added to adoc */
  short adocCollection;    /* 3-state flag for whether it can or cannot be a collection */
  int numAttributes;                /* Number of attributes */
} GroupData;

/*
 * Structure to hold information about valid datasets during scanning
 */
typedef struct {
  hid_t dsetID;
  char *name;
  short  dataType;          /* IITYPE type of data in file */
  short swapped;            /* Flag for whether data are byte-swapped */
  int  nx, ny, nz;          /* Dimensions; nx is full number of type elements */
  int chunkX, chunkY, chunkZ;  /* Chunk sizes if any */
  int numAttributes;        /* Number of attributes */
  int groupNum;             /* Group name converted to a number if possible */
} DatasetData;

/*
 * Local functions
 */
static void setIOFuncsPlus(ImodImageFile *inFile, int hdfSource, int writable, 
                           hid_t fileID);
static int hdfReopen(ImodImageFile *inFile);
static void hdfClose(ImodImageFile *inFile);
static void hdfDelete(ImodImageFile *inFile);
static int hdfReadSection(ImodImageFile *inFile, char *buf, int cz);
static int hdfReadSectionByte(ImodImageFile *inFile, char *buf, int cz);
static int hdfReadSectionUShort(ImodImageFile *inFile, char *buf, int cz);
static int hdfReadSectionFloat(ImodImageFile *inFile, char *buf, int cz);
static int hdfWriteSection(ImodImageFile *inFile, char *buf, int cz);
static int hdfWriteSectionFloat(ImodImageFile *inFile, char *buf, int cz);
static int hdfWriteHeader(ImodImageFile *inFile);
static int hdfSyncFromMrcHeader(ImodImageFile *inFile, MrcHeader *hdata);
static int scanGroup(hid_t groupID, char *groupName, int *adocSection);
static hid_t openDatasetGroup(ImodImageFile *inFile, const char *dsName);
static int attributesToAdoc(hid_t objID, int numAttrib, const char *typeName, 
                            int sectInd);
static int adocToAttributes(hid_t parentID, const char *typeName, int sectInd,
                            const char *prefix);
static int addIntegerAttribute(hid_t parentID, const char *key, int *ivals, int numVals);
static int addFloatAttribute(hid_t parentID, const char *key, float *vals, int numVals);
static int addStringAttribute(hid_t parentID, const char *key, const char *valStr);
static int getDelPrefixedFloat(const char *key, float *value, int *errSum);
static int deletePrefixedKeyValue(const char *key);
static int getDelPrefixedInteger(const char *key, int *value, int *errSum);
static int getPrefixedInteger(const char *key, int *value, int *errSum);
static int addOnePrefixedInteger(hid_t parentID, const char *key, int ival, int *errSum);
static int addOnePrefixedFloat(hid_t parentID, const char *key, float val, int *errSum);
static int manageMallocBuf(void **buffer, int *size, int needed, int dsize);
static void cleanupFromOpen(hid_t fileID, int closeFile, int numVol, 
                            ImodImageFile *inFile);
static void cleanupMallocBufs();
static int setupZtoSetMap(hid_t fileID, ImodImageFile *inFile, int size, int sequence);
static int startsWith(const char *fullStr, const char *subStr);
static int endsWith(const char *fullStr, const char *subStr);
static const char *prefixedKey(const char *prefix, const char *key);
  
/* 
 * Transient lists of groups and datasets and other static variables used in scan
 */
static Ilist *sGroups = NULL;
static Ilist *sDatasets = NULL;

static int sNumberedGroups;
static int sMaxGroupNum;
static int sMinGroupNum;

static const char *sPreData = ADOC_GLOBAL_NAME;
static const char *sZvalue = ADOC_ZVALUE_NAME;

/* 
 * Allocated buffers and their sizes 
 */
static char *sAttribName = NULL;
static int sAttrNameSize = 0;
static b3dFloat *sFloatBuf = NULL;
static int sFloatBufSize = 0;
static b3dInt16 *sShortBuf = NULL;
static int sShortBufSize = 0;
static b3dUInt16 *sUShortBuf = NULL;
static int sUShortBufSize = 0;
static b3dInt32 *sIntBuf = NULL;
static int sIntBufSize = 0;
static char *sStringBuf = NULL;
static int sStrBufSize = 0;

static char *sMrcPrefix = NULL;

/* 
 * Group name or attributes by which file types will be recognized
 */
#define CHIMERA_TAG "/Chimera"
#define EMAN_TAG    "EMAN.nx"

/*
 * Checks for and opens an HDF file
 */
int iiHDFCheck(ImodImageFile *inFile)
{
  hid_t fileID, groupID, dsetID;
  char *slash;
  int numMrcTags = 6;
  const char *emanSectType = NULL;
  
  /* Attributes that have to be present for an IMOD-like MRC-like file */
  char *mrcTags[] = {"MRC.mx", "MRC.my", "MRC.mz", "MRC.xlen", "MRC.ylen", "MRC.zlen"};
  int retval, nsum, hdfSource, ind, sub, singleImStack, set, nxStack, nyStack, stackType;
  float tmp, xscale, yscale, zscale, sectXorig, sectYorig;
  int volInd, adocIndex, addedSectInd, sectInd, grp, numKeys, numMatch, mode, keyInd;
  int origMatch;
  char sectText[10], labelKey[14];
  ImodImageFile *iiVol;
  const char *collName;
  char *datasetName, *sectName, *key, *label;
  GroupData *group, *subGroup;
  DatasetData *dataset;
  StackSetData *stackSetPtr;
  StackSetData stackSet;
  MrcHeader *hdata;
  int err = H5Fis_hdf5(inFile->filename);
  if (err < 0)
    return IIERR_IO_ERROR;
  if (err == 0)
    return IIERR_NOT_FORMAT;
  fclose(inFile->fp);
  sGroups = ilistNew(sizeof(GroupData), 4);
  sDatasets = ilistNew(sizeof(DatasetData), 4);
  sNumberedGroups = 1;
  sMaxGroupNum = -1000000000;
  sMinGroupNum = 1000000000;
  slash = strdup("/");
  if (!sGroups || !sDatasets || !slash) {
    cleanupFromOpen(0, 0, 0, inFile);
    return IIERR_MEMORY_ERR;
  }

  fileID = H5Fopen(inFile->filename, 
                   strstr(inFile->fmode, "+") != NULL ? H5F_ACC_RDWR : H5F_ACC_RDONLY,
                   H5P_DEFAULT);

  /* Open the root group and recursively scan for groups and datasets */
  /* TODO: be able to handle an empty file with backup nx, ny, nz values */
  groupID = H5Gopen(fileID, slash, H5P_DEFAULT);
  /* printf("fileID %d groupID %d\n", fileID, groupID); */
  err = scanGroup(groupID, slash, &retval);
  if (err || !ilistSize(sDatasets)) {
    if (!err)
      err = IIERR_NOT_FORMAT;
    cleanupFromOpen(fileID, 1, 0, inFile);
    return err;
  }

  /* See if all datasets are single-image, same size, same type */
  singleImStack = 1;
  inFile->numVolumes = 1;
  for (set = 0; set < ilistSize(sDatasets); set++) {
    dataset = (DatasetData *)ilistItem(sDatasets, set);
    if (!set) {
      nxStack = dataset->nx;
      nyStack = dataset->ny;
      stackType = dataset->dataType;
    } 
    if ((set && (nxStack != dataset->nx || nyStack != dataset->ny ||
                 stackType != dataset->dataType)) || dataset->nz > 1) {
      singleImStack = 0;
      inFile->numVolumes = ilistSize(sDatasets);
      break;
    }
  }

  /* Get the array of iifile pointers and set it up */
  inFile->iiVolumes = B3DMALLOC(ImodImageFile *, inFile->numVolumes);
  if (!inFile->iiVolumes) {
    cleanupFromOpen(fileID, 1, 0, inFile);
    return IIERR_MEMORY_ERR;
  }
  inFile->iiVolumes[0] = inFile;
  for (ind = 1; ind < inFile->numVolumes; ind++) {
    inFile->iiVolumes[ind] = iiNew();
    if (!inFile->iiVolumes[ind]) {
      cleanupFromOpen(fileID, 1, ind, inFile);
      return IIERR_MEMORY_ERR;
    }
  }
  
  /* Loop on the groups with attributes and determine which ones are not global:
     i.e., see if any section is not a descendant of the group */
  hdfSource = IIHDF_UNKNOWN;
  for (ind = 0; ind < ilistSize(sGroups); ind++) {
    group = (GroupData *)ilistItem(sGroups, ind);
    if (!strcmp(group->name, CHIMERA_TAG))
      hdfSource = IIHDF_CHIMERA;
    if (!group->numAttributes || group->adocCollection > 0)
      continue;
    for (set = 0; set <  ilistSize(sDatasets); set++) {
      dataset = (DatasetData *)ilistItem(sDatasets, set);
      if (!startsWith(dataset->name, group->name)) {
        group->nonGlobalAttrib = 1;
        break;
      }
    }
  }

  /* Process things for a single-image stack */
  if (singleImStack) {
    /* printf("SingleImStack %d %d %d\n", nxStack, nyStack, stackType); */
    inFile->nx = nxStack;
    inFile->ny = nyStack;
    inFile->nz = ilistSize(sDatasets);
    inFile->type = stackType;

    /* Take over the dataset names and set ID's into new data list */
    inFile->stackSetList = ilistNew(sizeof(StackSetData), inFile->nz);
    if (!inFile->stackSetList) {
      cleanupFromOpen(fileID, 1, 1, inFile);
      return IIERR_MEMORY_ERR;
    }
    for (set = 0; set < inFile->nz; set++) {
      dataset = (DatasetData *)ilistItem(sDatasets, set);
      stackSet.name = dataset->name;
      stackSet.dsetID = dataset->dsetID;
      stackSet.isOpen = 1;
      ilistAppend(inFile->stackSetList, &stackSet);
      dataset->name = NULL;
    }

  } else {

    /* For a multi-volume file, set sizes, types, take over names */
    for (set = 0; set < ilistSize(sDatasets); set++) {
      dataset = (DatasetData *)ilistItem(sDatasets, set);
      inFile->iiVolumes[set]->nx = dataset->nx;
      inFile->iiVolumes[set]->ny = dataset->ny;
      inFile->iiVolumes[set]->nz = dataset->nz;
      inFile->iiVolumes[set]->type = dataset->dataType;
      inFile->iiVolumes[set]->datasetName = dataset->name;
      dataset->name = NULL;
      inFile->iiVolumes[set]->datasetID = dataset->dsetID;
      inFile->iiVolumes[set]->datasetIsOpen = 1;
      inFile->iiVolumes[set]->numVolumes = inFile->numVolumes;
    }
  }

  /* Get the new autodocs and header structures */
  retval = 0;
  for (ind = 0; ind < inFile->numVolumes; ind++) {
    if (!singleImStack && !ind) {
      inFile->globalAdocIndex = AdocNew();
      if (inFile->globalAdocIndex < 0)
        retval = IIERR_MEMORY_ERR;
    }
    if (!retval) {
      inFile->iiVolumes[ind]->adocIndex = AdocNew();
      if (inFile->iiVolumes[ind]->adocIndex < 0)
        retval = IIERR_MEMORY_ERR;
    }        
    if (!retval)
      inFile->iiVolumes[ind]->header = (char *)malloc(sizeof(MrcHeader));
    if (retval || !inFile->iiVolumes[ind]->header) {
      cleanupFromOpen(fileID, 1, inFile->numVolumes, inFile);
      return IIERR_MEMORY_ERR;
    }
  }

  /* See if it is a valid stack of single images with Z values */
  if (singleImStack && sNumberedGroups && sMinGroupNum >= 0 && 
      sMaxGroupNum + 1 - sMinGroupNum >= inFile->nz) { 

    /* The minimum must be non-negative, the total range must not be smaller than NZ,
       there must be an EMAN/IMOD type imageid_max. Originally required it to be
       be big enough, but this could lead to garbled results.  Take the existence of
       this alone as sign that they are really to be considered ordered.  */
    if (H5Aexists_by_name(fileID, "/MDF/images", "imageid_max", H5P_DEFAULT) <= 0)
      sNumberedGroups = 0;
    /* This was the test for it being big enough with exists returning > 0 */
      /*attribID = H5Aopen_by_name(fileID, "/MDF/images", "imageid_max", H5P_DEFAULT, 
                                 H5P_DEFAULT);
      if (H5Aread(attribID, H5T_NATIVE_INT, &ind) < 0 || ind < sMaxGroupNum)
        sNumberedGroups = 0;
        H5Aclose(attribID);
    } else */
    if (sNumberedGroups) {

      /* Make the map from Z values to set numbers and make sure of no duplicates */
      if (setupZtoSetMap(fileID, inFile, sMaxGroupNum + 1, 0))
        return IIERR_MEMORY_ERR;
      for (set = 0; set < inFile->nz && sNumberedGroups; set++) {
        dataset = (DatasetData *)ilistItem(sDatasets, set);
        if (inFile->zToDataSetMap[dataset->groupNum] >= 0)
          sNumberedGroups = 0;
        else
          inFile->zToDataSetMap[dataset->groupNum] = set;
      }
    }
  }

  /* If this failed, make a default map of numbers in sequence */
  if (singleImStack && !sNumberedGroups) {
    B3DFREE(inFile->zToDataSetMap);
    if (setupZtoSetMap(fileID, inFile, inFile->nz, 1))
      return IIERR_MEMORY_ERR;
  }    

  /* Loop on the datasets and then on the groups to add attributes to the autodoc */
  for (set = 0; set < ilistSize(sDatasets); set++) {
    volInd = singleImStack ? 0 : set;
    adocIndex = inFile->iiVolumes[volInd]->adocIndex;
    AdocSetCurrent(adocIndex);
    dataset = (DatasetData *)ilistItem(sDatasets, set);
    sprintf(sectText, "%d", sNumberedGroups ? dataset->groupNum : set);
    addedSectInd = -1;
    sectInd = 0;
    collName = sPreData;
    if (singleImStack) {
      stackSetPtr = (StackSetData *)ilistItem(inFile->stackSetList, set);
      datasetName = stackSetPtr->name;
    } else
      datasetName = inFile->iiVolumes[set]->datasetName;

    /* Process dataset attributes as a section for a stack section, global otherwise */
    if (dataset->numAttributes) {
      dsetID = H5Dopen(fileID, datasetName, H5P_DEFAULT);
      if (singleImStack) {
        sectInd = AdocAddSection(sZvalue, sectText);
        if (sectInd < 0)
          retval = IIERR_MEMORY_ERR;
        addedSectInd = sectInd;
        collName = sZvalue;
      }
      if (!retval) 
        retval = attributesToAdoc(dsetID, dataset->numAttributes, collName, sectInd);
      H5Dclose(dsetID);
    }

    /* Then loop on groups, find ones containing this dataset and process them */
    for (ind = 0; ind < ilistSize(sGroups) && !retval; ind++) {
      group = (GroupData *)ilistItem(sGroups, ind);
      if (!group->numAttributes || group->adocCollection > 0 || group->addedGlobal)
        continue;
      groupID = H5Gopen(fileID, group->name, H5P_DEFAULT);
      if (startsWith(datasetName, group->name)) {
        /* printf("grp %s  numattr %d  adoccol %d addedg %d\n", group->name,
           group->numAttributes, group->adocCollection, group->addedGlobal); */
        adocIndex = inFile->iiVolumes[volInd]->adocIndex;
        sectInd = 0;
        collName = sPreData;

        /* If it is group with non-global attributes, add a section if necessary and
           add attributes to the section.  If there is just one image, also put it in a
           section so that it will be preserved when stacking. */
        if (singleImStack && (group->nonGlobalAttrib || inFile->nz == 1)) {
          if (addedSectInd < 0) {
            addedSectInd = AdocAddSection(sZvalue, sectText);
            if (addedSectInd < 0)
              retval = IIERR_MEMORY_ERR;
          }
          sectInd = addedSectInd;
          collName = sZvalue;
        } else if (!group->nonGlobalAttrib && (singleImStack || inFile->numVolumes > 1)) {
          group->addedGlobal = 1;
          if (!singleImStack)
            adocIndex = inFile->globalAdocIndex;
        }
        AdocSetCurrent(adocIndex);
        if (!retval) 
          retval = attributesToAdoc(groupID, group->numAttributes, collName, sectInd);
      }
      H5Gclose(groupID);
    }
    if (retval) {
      cleanupFromOpen(fileID, 1, inFile->numVolumes, inFile);
      return retval;
    }
  }

  /* For sake of completeness, now add the sections for groups with attributes only */
  retval = 0;
  for (grp = 0; grp < ilistSize(sGroups); grp++) {
    group = (GroupData *)ilistItem(sGroups, grp);
    if (group->adocCollection <= 0)
      continue;
    collName = strrchr(group->name, '/') + 1;

    /* Find subgroups with attributes */
    for (sub = 0; sub < ilistSize(sGroups) && !retval; sub++) {
      subGroup = (GroupData *)ilistItem(sGroups, sub);
      if (sub == grp || !subGroup->numAttributes || 
          !startsWith(subGroup->name, group->name))
        continue;
      sectName = strrchr(subGroup->name, '/') + 1;
      groupID = H5Gopen(fileID, subGroup->name, H5P_DEFAULT);

      /* Add a section to the global or only autodoc */
      AdocSetCurrent(singleImStack ? inFile->adocIndex : inFile->globalAdocIndex);
      sectInd = AdocAddSection(collName, sectName);
      if (sectInd < 0)
        retval = IIERR_MEMORY_ERR;
      if (!retval)
        retval = attributesToAdoc(groupID, subGroup->numAttributes, collName, sectInd);
      H5Gclose(groupID);
    }
    if (retval) {
      cleanupFromOpen(fileID, 1, inFile->numVolumes, inFile);
      return retval;
    }
  }    

  /* Try to define the file source and determine an MRC prefix */
  if (hdfSource != IIHDF_CHIMERA) {
    AdocSetCurrent(inFile->adocIndex);
    numKeys = AdocGetNumberOfKeys(sPreData, 0);
    numMatch = 0;
    retval = 0;
    for (keyInd = 0; keyInd < numKeys && !retval; keyInd++) {
      key = NULL;
      retval = AdocGetKeyByIndex(sPreData, 0, keyInd, &key);
      if (!retval && !startsWith(key, "EMAN.")) {
        for (ind = 0; ind < numMrcTags; ind++) {
          sub = endsWith(key, mrcTags[ind]);
          if (sub >= 0) {
            retval = manageMallocBuf((void **)&sStringBuf, &sStrBufSize, sub + 1, 1);
            if (retval)
              break;
            if (sub)
              strncpy(sStringBuf, key, sub);
            sStringBuf[sub] = 0x00;
            if (!numMatch) {
              sMrcPrefix = strdup(sStringBuf);
              if (!sMrcPrefix)
                retval = 1;
              numMatch = 1;
            } else if (!strcmp(sMrcPrefix, sStringBuf))
              numMatch++;
            break;
          }
        }
      }
      B3DFREE(key);
    }
    if (retval) {
      cleanupFromOpen(fileID, 1, inFile->numVolumes, inFile);
      return IIERR_MEMORY_ERR;
    }
    /* printf("prefix %s\n", sMrcPrefix); */

    /* If all tags found a match, it is an MRC-like file with the expected tags */
    if (numMatch == numMrcTags) {
      hdfSource = IIHDF_OTHER_MRC;
      if (!strcmp(sMrcPrefix, "IMOD."))
        hdfSource = IIHDF_IMOD;

    } else {
      if (!AdocGetInteger(sPreData, 0, EMAN_TAG, &ind)) 
        emanSectType = sPreData;
      else if (!AdocGetInteger(sZvalue, 0, EMAN_TAG, &ind))
        emanSectType = sZvalue;
      if (emanSectType)
        hdfSource = IIHDF_EMAN;
    }
  }

  /* Now that autodocs are complete, we can finish populating the iiFile(s) */
  retval = 0;
  for (volInd = 0; volInd < inFile->numVolumes; volInd++) {
    iiVol = inFile->iiVolumes[volInd];
    iiVol->iiVolumes = inFile->iiVolumes;
    iiVol->format = IIFORMAT_LUMINANCE;
    if (iiVol->type == IITYPE_BYTE || iiVol->type == IITYPE_UBYTE) {
      mode = MRC_MODE_BYTE;
      if (iiVol->type == IITYPE_UBYTE) {
        if (!getPrefixedInteger("is_rgb", &ind, &retval) && ind > 0) {
          iiVol->format = IIFORMAT_RGB;
          mode = MRC_MODE_RGB;
          iiVol->nx /= 3;
        }
      }
    } else if (iiVol->type == IITYPE_SHORT) {
      mode = MRC_MODE_SHORT;
    } else if (iiVol->type == IITYPE_USHORT) {
      mode = MRC_MODE_USHORT;
    } else {
      mode = MRC_MODE_FLOAT;
      if ((hdfSource == IIHDF_IMOD && 
           !getPrefixedInteger("is_complex", &ind, &retval) && ind > 0) ||
          (hdfSource == IIHDF_EMAN && 
           !AdocGetInteger(emanSectType, 0, "EMAN.is_complex", &ind) &&
           ind > 0)) {
        mode = MRC_MODE_COMPLEX_FLOAT;
        iiVol->nx /= 2;
        iiVol->format = IIFORMAT_COMPLEX;
      }
    }
    iiVol->mode = mode;
    hdata = (MrcHeader *)iiVol->header;

    /* Initialize an MRC header */
    mrc_head_new(hdata, iiVol->nx, iiVol->ny, iiVol->nz, iiVol->mode);
    hdata->bytesSigned = iiVol->type == IITYPE_BYTE ? 1 : 0;
    dataset = (DatasetData *)ilistItem(sDatasets, volInd);
    hdata->swapped = dataset->swapped;

    /* Set default min/max in case there is none in the file */
    iiDefaultMinMaxMean(iiVol->type, &hdata->amin, &hdata->amax, &hdata->amean);

    if (hdfSource == IIHDF_IMOD || hdfSource == IIHDF_OTHER_MRC) {
      AdocSetCurrent(iiVol->adocIndex);

      /* Get all the standard MRC fields for IMOD-like */
      getDelPrefixedInteger("MRC.nxstart", &hdata->nxstart, &retval);
      getDelPrefixedInteger("MRC.nystart", &hdata->nystart, &retval);
      getDelPrefixedInteger("MRC.nzstart", &hdata->nzstart, &retval);
      getDelPrefixedInteger("MRC.mx", &hdata->mx, &retval);
      getDelPrefixedInteger("MRC.my", &hdata->my, &retval);
      getDelPrefixedInteger("MRC.mz", &hdata->mz, &retval);
      getDelPrefixedFloat("MRC.xlen", &hdata->xlen, &retval);
      getDelPrefixedFloat("MRC.ylen", &hdata->ylen, &retval);
      getDelPrefixedFloat("MRC.zlen", &hdata->zlen, &retval);
      getDelPrefixedFloat("MRC.alpha", &hdata->alpha, &retval);
      getDelPrefixedFloat("MRC.beta", &hdata->beta, &retval);
      getDelPrefixedFloat("MRC.gamma", &hdata->gamma, &retval);
      getDelPrefixedInteger("MRC.mapc", &hdata->mapc, &retval);
      getDelPrefixedInteger("MRC.mapr", &hdata->mapr, &retval);
      getDelPrefixedInteger("MRC.maps", &hdata->maps, &retval);
      getDelPrefixedFloat("MRC.minimum", &hdata->amin, &retval);
      getDelPrefixedFloat("MRC.maximum", &hdata->amax, &retval);
      getDelPrefixedFloat("MRC.mean", &hdata->amean, &retval);
      getDelPrefixedInteger("MRC.ispg", &hdata->ispg, &retval);
      getDelPrefixedFloat("MRC.xorigin", &hdata->xorg, &retval);
      getDelPrefixedFloat("MRC.yorigin", &hdata->yorg, &retval);
      getDelPrefixedFloat("MRC.zorigin", &hdata->zorg, &retval);
      nsum = 6;
      if (AdocGetFloatArray(sPreData, 0, prefixedKey(sMrcPrefix, "MRC.tiltangles"),
                            hdata->tiltangles, &nsum, 6) < 0)
        retval++;
      deletePrefixedKeyValue("MRC.tiltangles");
      getDelPrefixedFloat("MRC.rms", &hdata->rms, &retval);
      getDelPrefixedInteger("MRC.nlabels", &hdata->nlabl, &retval);
      B3DCLAMP(hdata->nlabl, 0, 10);
      for (sub = 0; sub < hdata->nlabl && !retval; sub++) {
        sprintf(labelKey, "MRC.label%d", sub);
        if (AdocGetString(sPreData, 0, prefixedKey(sMrcPrefix, labelKey), &label))
          retval++;
        else {
          strncpy(&hdata->labels[sub][0], label, MRC_LABEL_SIZE);
          hdata->labels[sub][MRC_LABEL_SIZE] = 0;
          fixTitlePadding(hdata->labels[sub]);
          free(label);
          deletePrefixedKeyValue(labelKey);
        }
      }

      /* Just for thoroughness, get rid of the values that are defined by the file */
      deletePrefixedKeyValue("MRC.nx");
      deletePrefixedKeyValue("MRC.ny");
      deletePrefixedKeyValue("MRC.nz");
      deletePrefixedKeyValue("MRC.mode");

    } else if (hdfSource == IIHDF_CHIMERA) {
      /* SIGN? */
      if (AdocGetThreeFloats(sPreData, 0, "origin", &hdata->xorg, &hdata->yorg, 
                             &hdata->zorg) < 0)
        retval++;
      if (AdocGetThreeFloats(sPreData, 0, "step", &xscale, &yscale, &zscale) < 0)
        retval++;
      mrc_set_scale(hdata, (double)xscale, (double)yscale, (double)zscale);

    } else if (hdfSource == IIHDF_EMAN) {
      if (AdocGetFloat(emanSectType, 0, "EMAN.apix_x", &xscale) < 0 || 
          AdocGetFloat(emanSectType, 0, "EMAN.apix_y", &yscale) < 0 || 
          AdocGetFloat(emanSectType, 0, "EMAN.apix_z", &zscale) < 0)
        retval++;
      mrc_set_scale(hdata, (double)xscale, (double)yscale, (double)zscale);
      
      /* Get origins if they are global - they vary for boxed particles */
      if (!strcmp(emanSectType, sPreData)) {
        if (AdocGetFloat(emanSectType, 0, "EMAN.origin_x", &hdata->xorg) < 0 || 
            AdocGetFloat(emanSectType, 0, "EMAN.origin_y", &hdata->yorg) < 0 || 
            AdocGetFloat(emanSectType, 0, "EMAN.origin_z", &hdata->zorg) < 0)
          retval++;
      }

      /* For a stack, compute min/max/mean from all the dataset entries;
         otherwise just get the one min/max/mean */
      if (singleImStack) {
        origMatch = 1;
        nsum = 0;
        for (sub = 0; sub < inFile->nz && !retval; sub++) {
          if (!AdocGetFloat(sZvalue, sub, "EMAN.minimum", &tmp))
            hdata->amin = B3DMIN(hdata->amin, tmp);
          if (!AdocGetFloat(sZvalue, sub, "EMAN.maximum", &tmp))
            hdata->amax = B3DMAX(hdata->amax, tmp);
          if (!AdocGetFloat(sZvalue, sub, "EMAN.mean", &tmp)) {
            hdata->amean += tmp;
            nsum++;
          }

          /* If X and Y origins exist for sections and match so far, get them and make
             sure they still match */
          if (origMatch) {
            if (AdocGetFloat(sZvalue, sub, "EMAN.origin_x", &tmp) || 
                (sub && tmp != sectXorig))
              origMatch = 0;
            else if (!sub)
              sectXorig = tmp;
            if (AdocGetFloat(sZvalue, sub, "EMAN.origin_y", &tmp) || 
                (sub && tmp != sectYorig))
              origMatch = 0;
            else if (!sub)
              sectYorig = tmp;
          }
        }
        if (nsum)
          hdata->amean /= nsum;
        if (origMatch) {
          hdata->xorg = sectXorig;
          hdata->yorg = sectYorig;
        }

      } else {
        if (AdocGetFloat(emanSectType, 0, "EMAN.minimum", &hdata->amin) < 0 || 
            AdocGetFloat(emanSectType, 0, "EMAN.maximum", &hdata->amax) < 0 || 
            AdocGetFloat(emanSectType, 0, "EMAN.mean", &hdata->amean) < 0)
          retval++;
      }
      
    }

    /* Invert all origins for consistency, and invert on output */
    hdata->xorg *= -1;
    hdata->yorg *= -1;
    hdata->zorg *= -1;

    /* Set the chunk sizes for a volume dataset */
    if (!inFile->stackSetList) {
      iiVol->tileSizeX = dataset->chunkX;
      iiVol->tileSizeY = dataset->chunkY;
      iiVol->zChunkSize = dataset->chunkZ;
    }

    /* Make a filename with volume number appended */
    if (volInd && !retval) {
      iiVol->filename = (char *)malloc(strlen(inFile->filename) + 10);
      if (iiVol->filename)
        sprintf(iiVol->filename, "%s-%d", inFile->filename, volInd + 1);
      else
        retval = 1;
    }
    if (retval) {
      cleanupFromOpen(fileID, 1, inFile->numVolumes, inFile);
      return IIERR_MEMORY_ERR;
    }
    
    /* Shift header min/max/mean now for signed, assigned to scale min/max, finish up */
    if (hdata->bytesSigned) {
      hdata->amin += 128.;
      hdata->amax += 128.;
      hdata->amean += 128.;
    }
    iiVol->smin = hdata->amin;
    iiVol->smax = hdata->amax;
    iiVol->globalAdocIndex = inFile->globalAdocIndex;
    iiSyncFromMrcHeader(iiVol, hdata);
    setIOFuncsPlus(iiVol, hdfSource, strstr(inFile->fmode, "+") != NULL ? 1 : 0, fileID);
    
  }

  /* Close all but first dataset and leave them as unused until program opens them */
  for (volInd = 1; volInd < inFile->numVolumes; volInd++) {
    iiVol = inFile->iiVolumes[volInd];
    H5Dclose(iiVol->datasetID);
    iiVol->datasetIsOpen = 0;
    iiVol->state = IISTATE_UNUSED;
    iiVol->fp = NULL;
    strncpy(iiVol->fmode, inFile->fmode, 3);
  }
  cleanupFromOpen(fileID, 0, 0, inFile);
  return 0;
}

/*
 * Recursive function to scan a group from groups and datasets
 */
static int scanGroup(hid_t groupID, char *groupName, int *adocSection)
{
  H5G_info_t info;
  H5O_info_t objInfo;
  hid_t subID, dsetID, dtypeID, dspaceID, plistID;
  GroupData gdata, *gdptr;
  DatasetData dsData;
  int numObjs = 0, numSets = 0, groupNum = 0, groupNumbered = 0;
  int retval = 0;
  int ind, subCanBeSection, rank, signedInt, err, iitype, dsize, keepDSet, listIndex;
  char *objName, *endptr, *tmpName;
  size_t size;
  hsize_t currentDims[3], maxDims[3], chunkDims[3];
  H5T_class_t class;

  *adocSection = 0;

  /* Set up the structure, initializing to 0's and NULLS, and add it to the list */
  memset(&gdata, 0, sizeof(GroupData));
  gdata.groupID = groupID;
  gdata.name = groupName;
  if (!gdata.name)
    retval = IIERR_MEMORY_ERR;
  if (!retval) {
    listIndex = ilistSize(sGroups);
    if (ilistAppend(sGroups, &gdata))
      retval = IIERR_MEMORY_ERR;
    else
      gdptr = (GroupData *)ilistItem(sGroups, listIndex);
  }

  /* Try to convert group name to a number */
  objName = strrchr(groupName, '/');
  if (objName) {
    ind = strtol(objName + 1, &endptr, 10);
    if (!*endptr) {
      groupNum = ind;
      groupNumbered = 1;
    }
  }

  /* Find out if there are attributes */
  if (!retval && H5Oget_info(groupID, &objInfo) < 0)
    retval = IIERR_IO_ERROR;
  if (!retval)
    gdptr->numAttributes = (int)objInfo.num_attrs;
  /* printf("group info %d\n", (int)objInfo.num_attrs); */

  /* Get the number of objects and iterate through them */
  if (!retval && H5Gget_info(groupID, &info) < 0)
    retval = IIERR_IO_ERROR;
  if (!retval)
    numObjs = info.nlinks;
  
  for (ind = 0; ind < numObjs; ind++) {

    /* Get the name of the object */
    size = H5Lget_name_by_idx(groupID, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)ind,
                              NULL, 0, H5P_DEFAULT) + 1;
    if (size <= 0)
      retval = IIERR_IO_ERROR;
    else
      objName = B3DMALLOC(char, size);
    if (!retval && !objName)
      retval = IIERR_MEMORY_ERR;
    if (!retval && H5Lget_name_by_idx(groupID, ".", H5_INDEX_NAME, H5_ITER_INC,
                                      (hsize_t)ind, objName, size, H5P_DEFAULT) < 0)
      retval = IIERR_IO_ERROR;
    if (retval)
      break;
      
    /* printf("group %s index %d size %d  name %s\n", groupName, ind, (int)size, 
       objName); */

    /* Compose absolute path if it is relative (it is...) */
    if (objName[0] != '/') {
      tmpName = objName;
      dsize = strlen(groupName);
      objName = B3DMALLOC(char, size + dsize + 1);
      if (!objName) {
        retval = IIERR_MEMORY_ERR;
        break;
      }
      sprintf(objName, "%s/%s", dsize > 1 ? groupName : "", tmpName);
      free(tmpName);
    }

    /* Get the info */
    if (H5Oget_info_by_idx(groupID, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)ind,
                           &objInfo, H5P_DEFAULT) < 0) {
      retval = IIERR_IO_ERROR;
      break;
    }

    if (objInfo.type == H5O_TYPE_GROUP) {

      /* A GROUP.  Open and scan the group */
      gdptr->hasGroups = 1;
      subID = H5Gopen(groupID, objName, H5P_DEFAULT);
      if (subID < 0) {
        retval = IIERR_IO_ERROR;
        break;
      }
      /* printf("subID %d name %s\n", subID, objName); */
      err = scanGroup(subID, objName, &subCanBeSection);
      if (err) {
        H5Gclose(subID);
        retval = err;
        break;
      }

      /* Get the pointer again, it may have changed */
      gdptr = (GroupData *)ilistItem(sGroups, listIndex);

      /* If subgroup can't be a section, disqualify this group from being collection,
         otherwise if it has just attributes and this can still be a collection, mark
         it as one */
      if (subCanBeSection < 0)
        gdptr->adocCollection = -1;
      else if (subCanBeSection > 0 && !gdptr->adocCollection)
        gdptr->adocCollection = 1;
    } else if (objInfo.type == H5O_TYPE_DATASET) {

      /* A DATASET.  This disqualifies this group from being a collection */
      keepDSet = 0;
      gdptr->adocCollection = -1;
      gdptr->hasAnyDatasets = 1;
      
      /* Multiple datasets in a group disqualifies use of group numbers for Z */
      numSets++;
      if (numSets > 1)
        sNumberedGroups = 0;
      
      dsetID = H5Dopen(groupID, objName, H5P_DEFAULT);
      if (dsetID < 0) {
        retval = IIERR_IO_ERROR;
        break;
      }
      
      dtypeID = H5Dget_type(dsetID);
      class = H5Tget_class(dtypeID);
      /* printf("dsetID %d name %s\n", dsetID, objName); */

      /* See if there is a valid data type */
      iitype = -1;
      dsize = H5Tget_precision(dtypeID) / 8;
      signedInt = H5Tget_sign(dtypeID) == H5T_SGN_2 ? 1 : 0;
      if (class == H5T_INTEGER) {
        if (dsize == 1)
          iitype = signedInt ? IITYPE_BYTE : IITYPE_UBYTE;
        if (dsize == 2)
          iitype = signedInt ? IITYPE_SHORT : IITYPE_USHORT;

      } else if (class == H5T_FLOAT && dsize == 4) {
        iitype = IITYPE_FLOAT;
      } 

      /* If so, get the rank and dimensions, must be 2 or 3 dimensions */
      if (iitype >= 0) {
        dspaceID = H5Dget_space(dsetID);
        rank = H5Sget_simple_extent_ndims(dspaceID);
        if (rank == 2 || rank == 3) {
          H5Sget_simple_extent_dims(dspaceID, currentDims, maxDims);

          dsData.chunkX = dsData.chunkY = dsData.chunkZ = 0;
          dsData.name = objName;
          dsData.nx = currentDims[rank - 1];
          dsData.ny = currentDims[rank - 2];
          if (rank == 3)
            dsData.nz = currentDims[0];
          else
            dsData.nz = 1;
          dsData.dataType = iitype;
          dsData.swapped = 0;
          if (H5Tget_order(dtypeID) != H5Tget_order(H5T_NATIVE_INT))
            dsData.swapped = 1;

          plistID = H5Dget_create_plist(dsetID);
          if (H5Pget_layout(plistID) == H5D_CHUNKED) {
            if (H5Pget_chunk(plistID, 3, chunkDims) >= 0) {
              if (chunkDims[rank - 1] < dsData.nx)
                dsData.chunkX = chunkDims[rank - 1];
              if (chunkDims[rank - 2] < dsData.ny)
                dsData.chunkY = chunkDims[rank - 2];
              if (rank == 3)
                dsData.chunkZ = chunkDims[0];
            }
          }
          H5Pclose(plistID);

          /* Keep track of group numbers for groups containing a dataset */
          dsData.groupNum = groupNum;
          if (!groupNumbered)
            sNumberedGroups = 0;
          if (sNumberedGroups) {
            sMinGroupNum = B3DMIN(sMinGroupNum, groupNum);
            sMaxGroupNum = B3DMAX(sMaxGroupNum, groupNum);
          }
          dsData.dsetID = dsetID;
          if (H5Oget_info(dsetID, &objInfo) < 0)
            retval = IIERR_IO_ERROR;
          if (!retval)
            dsData.numAttributes = (int)objInfo.num_attrs;
          /* printf("dataset info %d grpnumed %d  num %d  numedgrp %d %d\n", 
             dsData.numAttributes, groupNumbered, groupNum, sNumberedGroups,
             dsData.dsetID); */
          if (!retval && ilistAppend(sDatasets, &dsData))
            retval = IIERR_MEMORY_ERR;
          if (!retval)
            keepDSet = 1;
        }

        H5Sclose(dspaceID);
      } else {
        free(objName);
      }
      H5Tclose(dtypeID);
      if (!keepDSet)
        H5Dclose(dsetID);
      if (retval)
        break;

    } else {
      free(objName);
    }
  }

  H5Gclose(groupID);
  if (retval)
    return retval;

  /* Set flag for whether this can't be a section (so above group can't be collection)
     or whether it could be one with attributes */
  if (gdptr->hasAnyDatasets || gdptr->hasGroups)
    *adocSection = -1;
  else if (gdptr->numAttributes)
    *adocSection = 1;
  return retval;
}

/*
 * Open a new file and create the standard top groups
 */
int iiHDFopenNew(ImodImageFile *inFile, const char *mode)
{
  int ind, err = 0;
  ImodImageFile *iiVol;
  ImodImageFile **newVolumes;
  hid_t fileID, mdfID, imagesID;

  if (inFile->file != IIFILE_UNKNOWN) {
    
    /* Opening a new volume in an existing file */
    if (inFile->file != IIFILE_HDF || inFile->hdfSource != IIHDF_IMOD || 
        inFile->stackSetList || !inFile->writeSection) {
      b3dError(stderr, "ERROR: iiHDFopenNew - Attempting to open second volume in "
               "non-HDF file, non-IMOD HDF file, stack file, or read-only file\n");
      return 1;
    }

    /* Manage the array of volumes and create new structures */
    newVolumes = B3DMALLOC(ImodImageFile *, inFile->numVolumes + 1);
    iiVol = iiNew();
    if (!inFile->iiVolumes || !iiVol)
      err = 1;
    if (!err) {
      for (ind = 0; ind < inFile->numVolumes; ind++)
        newVolumes[ind] = inFile->iiVolumes[ind];
      newVolumes[inFile->numVolumes++] = iiVol;
      for (ind = 0; ind < inFile->numVolumes; ind++) {
        newVolumes[ind]->iiVolumes = newVolumes;
        newVolumes[ind]->numVolumes = inFile->numVolumes;
      }
      iiVol->adocIndex = AdocNew();
      iiVol->globalAdocIndex = inFile->globalAdocIndex;
      iiVol->header = (char *)malloc(sizeof(MrcHeader));
      strncpy(iiVol->fmode, inFile->fmode, 3);
      if (!iiVol->header || iiVol->adocIndex < 0)
        err = 1;
    }

    /* Cleanup seems impossible... */
    if (err) {
      b3dError(stderr, "ERROR: iiHDFopenNew - Allocating structures for new HDF volume "
               "in existing file\n");
      return 1;
    }

    /* Finish setup; set Z chunk to 1 to signify this is a 3D volume */
    setIOFuncsPlus(iiVol, IIHDF_IMOD, 1, (hid_t)inFile->hdfFileID);
    mrc_head_new((MrcHeader *)inFile->header, 1, 1, 1, 0);
    iiVol->zChunkSize = 1;
    return 0;
  }

    /* Creating a new file for real */
  inFile->adocIndex = AdocNew();
  inFile->header = (char *)malloc(sizeof(MrcHeader));
  inFile->iiVolumes = B3DMALLOC(ImodImageFile *, 1);
  if (!inFile->header || inFile->adocIndex < 0 || !inFile->iiVolumes) {
    b3dError(stderr, "ERROR: iiHDFopenNew - Allocating MRC header or getting new "
             "autodoc\n");
    err = 1;
  }
  if (!err) {
    fileID = H5Fcreate(inFile->filename,  H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
    if (fileID < 0) {
      b3dError(stderr, "ERROR: iiHDFopenNew - Creating new HDF file named %s\n", 
               inFile->filename);
      err = 1;
    }
  }

  if (!err) {
    mdfID = H5Gcreate(fileID, "MDF", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if (mdfID < 0)
      err = 1;
    if (!err) {
      imagesID = H5Gcreate(fileID, "/MDF/images", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
      if (imagesID < 0) 
        err = 1;
      else
        H5Gclose(imagesID);
      H5Gclose(mdfID);
    }
    if (err) {
      H5Fclose(fileID);
      b3dError(stderr, "ERROR: iiHDFopenNew - Creating top groups in new HDF file %s\n", 
               inFile->filename);
    }
  }

  if (err) {
    B3DFREE(inFile->header);
    if (inFile->adocIndex >= 0)
      AdocClear(inFile->adocIndex);
    B3DFREE(inFile->iiVolumes);
    return 1;
  }
  
  inFile->iiVolumes[0] = inFile;
  inFile->numVolumes = 1;
  setIOFuncsPlus(inFile, IIHDF_IMOD, 1, fileID);
  mrc_head_new((MrcHeader *)inFile->header, 1, 1, 1, 0);
  return 0;
}

/*
 * Write all the header information associated with this volume
 */
static int hdfWriteHeader(ImodImageFile *inFile)
{
  int error = 0;
  int sectInd, ind, iz;
  StackSetData *stackSet;
  hid_t groupID;
  char labelKey[20];
  MrcHeader *hdata = (MrcHeader *)inFile->header;
  float mmmOffset = (!hdata->mode && hdata->bytesSigned) ? -128. : 0.;

  /* Initialize file now with current information */
  if (!inFile->stackSetList && !inFile->datasetName && initNewHDFfile(inFile)) {
    b3dError(stderr, "ERROR: hdfWriteHeader - Initializing the HDF file for output");
    return 1;
  }
  if (AdocSetCurrent(inFile->adocIndex))
    error = 1;
  else {
    sMrcPrefix = strdup("IMOD.MRC.");
    if (!sMrcPrefix)
      error = 1;
  }

  if (error) {
    b3dError(stderr, "ERROR: hdfWriteHeader - Setting current autodoc or allocating "
             "prefix\n");
    return 1;
  }

  /* Open top group unconditionally and put the max image ID there */
  groupID = H5Gopen((hid_t)inFile->hdfFileID, "/MDF/images", H5P_DEFAULT);
  if (groupID >= 0) {
    iz = inFile->numVolumes - 1;
    if (inFile->stackSetList) {
      
      /* For a stack, take the max of the actual highest Z and the nz in the header, since
       some programs write the header before the data */
      for (ind = inFile->zMapSize - 1; ind >= 0; ind--) {
        if (inFile->zToDataSetMap[ind] >= 0) {
          iz = inFile->zToDataSetMap[ind];
          break;
        }
      }
      iz = B3DMAX(iz, inFile->nz - 1);
    }
    if (addIntegerAttribute(groupID, "imageid_max", &iz, 1))
      error++;
  }

  /* Open the group where attributes will go if it is different */
  if (!error && groupID >= 0 && !inFile->stackSetList) {
    H5Gclose(groupID);
    groupID = openDatasetGroup(inFile, inFile->datasetName);
  }
  if (error || groupID < 0) {
    cleanupMallocBufs();
    b3dError(stderr, "ERROR: hdfWriteHeader - %s\n", groupID < 0 ? "Opening group for"
             " writing header attributes" : "Writing imageid_max attribute to header");
    if (groupID >= 0)
      H5Gclose(groupID);
    return 1;
  }

  /* Set the rgb and complex attributes and */
  ind = hdata->mode == MRC_MODE_RGB ? 1 : 0;
  if (addIntegerAttribute(groupID, "IMOD.is_rgb", &ind, 1))
    error++;
  ind = inFile->format == IIFORMAT_COMPLEX ? 1 : 0;
  if (addIntegerAttribute(groupID, "IMOD.is_complex", &ind, 1))
    error++;

  /* Set all the standard MRC fields with the IMOD.MRC prefix */
  if (!error) {
    addOnePrefixedInteger(groupID, "nxstart", hdata->nxstart, &error);
    addOnePrefixedInteger(groupID, "nystart", hdata->nystart, &error);
    addOnePrefixedInteger(groupID, "nzstart", hdata->nzstart, &error);
    addOnePrefixedInteger(groupID, "mx", hdata->mx, &error);
    addOnePrefixedInteger(groupID, "my", hdata->my, &error);
    addOnePrefixedInteger(groupID, "mz", hdata->mz, &error);
    addOnePrefixedFloat(groupID, "xlen", hdata->xlen, &error);
    addOnePrefixedFloat(groupID, "ylen", hdata->ylen, &error);
    addOnePrefixedFloat(groupID, "zlen", hdata->zlen, &error);
    addOnePrefixedFloat(groupID, "alpha", hdata->alpha, &error);
    addOnePrefixedFloat(groupID, "beta", hdata->beta, &error);
    addOnePrefixedFloat(groupID, "gamma", hdata->gamma, &error);
    addOnePrefixedInteger(groupID, "mapc", hdata->mapc, &error);
    addOnePrefixedInteger(groupID, "mapr", hdata->mapr, &error);
    addOnePrefixedInteger(groupID, "maps", hdata->maps, &error);
    addOnePrefixedFloat(groupID, "minimum", hdata->amin + mmmOffset, &error);
    addOnePrefixedFloat(groupID, "maximum", hdata->amax + mmmOffset, &error);
    addOnePrefixedFloat(groupID, "mean", hdata->amean + mmmOffset, &error);
    addOnePrefixedInteger(groupID, "ispg", hdata->ispg, &error);

    /* Write origin out with inverted sign */
    addOnePrefixedFloat(groupID, "xorigin", -hdata->xorg, &error);
    addOnePrefixedFloat(groupID, "yorigin", -hdata->yorg, &error);
    addOnePrefixedFloat(groupID, "zorigin", -hdata->zorg, &error);
    if (addFloatAttribute(groupID, "IMOD.MRC.tiltangles", hdata->tiltangles, 6))
      error++;
    addOnePrefixedFloat(groupID, "rms", hdata->rms, &error);
    addOnePrefixedInteger(groupID, "nlabels", hdata->nlabl, &error);
    for (ind = 0; ind < B3DMIN(10, hdata->nlabl); ind++) {
      sprintf(labelKey, "IMOD.MRC.label%d", ind);
      hdata->labels[ind][MRC_LABEL_SIZE] = 0;
      if (addStringAttribute(groupID, labelKey,  hdata->labels[ind]))
        error++;
    }
  }

  if (error) {
    H5Gclose(groupID);
    cleanupMallocBufs();
    b3dError(stderr, "ERROR: hdfWriteHeader - Writing basic MRC-type header "
             "attributes\n");
    return 1;
  }

  /* Now write any other global autodoc stuff in the same place, prefixed with IMOD */
  if (adocToAttributes(groupID, sPreData, 0, "IMOD."))
    error = 1;

  /* Done with the group */
  H5Gclose(groupID);
  
  /* Write any global multivolume stuff if there is a single volume, and write attribute
     only sections for single volume or stack case.  Multiple volumes require explicit
     call from program to write this */
  if (!error && inFile->numVolumes < 2)
    error = hdfWriteGlobalAdoc(inFile);

  if (error) {
    cleanupMallocBufs();
    b3dError(stderr, "ERROR: hdfWriteHeader - Writing other global or attribute-only "
             "metadata\n");
    return 1;
  }

  /* Finally, for a stack, write ZValue sections to section images */
  if (inFile->stackSetList) {
    for (iz = 0; iz < inFile->zMapSize && !error; iz++) {
      ind = inFile->zToDataSetMap[iz];
      if (ind < 0)
        continue;
      sprintf(labelKey, "%d", iz);
      sectInd = AdocLookupSection(sZvalue, labelKey);
      if (sectInd < 0)
        continue;
      stackSet = (StackSetData *)ilistItem(inFile->stackSetList, ind);
      groupID = openDatasetGroup(inFile, stackSet->name);
      if (groupID < 0)
        error = 1;
      else {
        if (adocToAttributes(groupID, sZvalue, sectInd, NULL))
          error = 1;
        H5Gclose(groupID);
      }
    }
  }
  cleanupMallocBufs();
  if (error)
    b3dError(stderr, "ERROR: hdfWriteHeader - Writing metadata for individual Z "
             "slices\n");
  return error;
}

/* Write global autodoc information: multivolume global adoc and attribute-only info */
int hdfWriteGlobalAdoc(ImodImageFile *inFile)
{
  int err = 0, sectInd, coll, numColl, numSect;
  char *collName;
  hid_t groupID;
  if (!inFile->writeHeader)
    return 1;

  /* Switch to global or only adoc and open top group */
  if (AdocSetCurrent(inFile->globalAdocIndex >= 0 ? inFile->globalAdocIndex : 
                     inFile->adocIndex))
    return 1;
  groupID = H5Gopen((hid_t)inFile->hdfFileID, "/MDF/images", H5P_DEFAULT);
  if (groupID < 0)
    return 1;

  /* If there is a global adoc, write global stuff here */
  if (inFile->globalAdocIndex >= 0)
    err = adocToAttributes(groupID, sPreData, 0, NULL);

  /* In any case, write any section-only attributes except ZValue here */
  numColl = AdocGetNumCollections();
  for (coll = 0; coll < numColl && !err; coll++) {
    if (AdocGetCollectionName(coll, &collName))
      err = 1;
    else if (!strcmp(collName, sZvalue)) {
      numSect = AdocGetNumberOfSections(collName);
      for (sectInd = 0; sectInd < numSect && !err; sectInd++)
        if (adocToAttributes(groupID, collName, sectInd, NULL))
          err = 1;
      free(collName);
    }
  }

  H5Gclose(groupID);
  return err;
}

/*
 * Additional step in syncing an external MRC header to this file: copy header structure
 */
static int hdfSyncFromMrcHeader(ImodImageFile *inFile, MrcHeader *hdata)
{
  if ((MrcHeader *)inFile->header != hdata)
    *((MrcHeader *)inFile->header) = *hdata;
  return 0;
}

/*
 * Close a file: if there are no volumes left open, then close all the datasets, mark
 * as closed, and then close file.
 */
static void hdfClose(ImodImageFile *inFile)
{
  int ind, numLeft = 0;
  StackSetData *stackd;
  if (inFile->datasetIsOpen)
    H5Dclose((hid_t)inFile->datasetID);
  inFile->datasetIsOpen = 0;
  for (ind = 0; ind < inFile->numVolumes; ind++) {
    if (inFile->iiVolumes[ind] && inFile->iiVolumes[ind] != inFile && 
        inFile->iiVolumes[ind]->fp)
      numLeft++;
  }
  if (!numLeft && inFile->fp) {
    for (ind = 0; ind < ilistSize(inFile->stackSetList); ind++) {
      stackd = (StackSetData *)ilistItem(inFile->stackSetList, ind);
      if (stackd->isOpen)
        H5Dclose((hid_t)stackd->dsetID);
      stackd->isOpen = 0;
    }
    /* printf("Close file for %p  %d\n", inFile, inFile->hdfFileID); */
    H5Fclose((hid_t)inFile->hdfFileID);
  }
  inFile->fp = NULL;
}

/*
 * Reopen a file: just open it and save the file ID, unless it is open on some volume
 */
static int hdfReopen(ImodImageFile *inFile)
{
  hid_t fileID;
  int ind;
  for (ind = 0; ind < inFile->numVolumes; ind++) {
    if (inFile->iiVolumes[ind] && inFile->iiVolumes[ind]->fp) {
      inFile->fp = (FILE *)inFile;
      /* printf("file for %p already open\n", inFile);*/
      return 0;
    }
  }
  
  /*printf("Reopen file for %p  %s  %s\n", inFile,  inFile->iiVolumes[0]->filename); */
  fileID = H5Fopen(inFile->iiVolumes[0]->filename, 
                   strstr(inFile->fmode, "+") != NULL ? H5F_ACC_RDWR : H5F_ACC_RDONLY,
                   H5P_DEFAULT);
  if (fileID < 0)
    return IIERR_IO_ERROR;
  for (ind = 0; ind < inFile->numVolumes; ind++)
    inFile->iiVolumes[ind]->hdfFileID = (int)fileID;
  inFile->fp = (FILE *)inFile;
  return 0;
}

/*
 * Delete a file: iiDelete has already called hdfClose
 */
static void hdfDelete(ImodImageFile *inFile)
{
  int ind, j, numLeft = 0;
  StackSetData *stackd;

  /* Count other files left that have ever been opened by the program */
  for (ind = 0; ind < inFile->numVolumes; ind++) {
    if (inFile->iiVolumes[ind] && inFile->iiVolumes[ind] != inFile &&
        inFile->iiVolumes[ind]->state != IISTATE_UNUSED)
      numLeft++;
  }

  /* If there are only unused ones left, this is the end; delete those first */
  if (!numLeft) {
    for (ind = 0; ind < inFile->numVolumes; ind++)
      if (inFile->iiVolumes[ind] && inFile->iiVolumes[ind] != inFile &&
          inFile->iiVolumes[ind]->state == IISTATE_UNUSED) {
        iiDelete(inFile->iiVolumes[ind]);
        inFile->iiVolumes[ind] = NULL;
      }
  }

  /* Clear this file out of the volume list for all volumes */
  for (ind = 0; ind < inFile->numVolumes; ind++) {
    if (inFile->iiVolumes[ind] == inFile) {
      for (j = 0; j < inFile->numVolumes; j++)
        if (j != ind && inFile->iiVolumes[j])
          inFile->iiVolumes[j]->iiVolumes[ind] = NULL;
      inFile->iiVolumes[ind] = NULL;
    }
  }

  /* Clean up the global adoc data if no files left */
  if (!numLeft && inFile->globalAdocIndex >= 0)
    AdocClear(inFile->globalAdocIndex);
  if (!numLeft)
    B3DFREE(inFile->iiVolumes);

  /* We have to clean up the resident data in this volume because the iifile will be 
     freed */
  if (inFile->adocIndex >= 0)
    AdocClear(inFile->adocIndex);
  B3DFREE(inFile->datasetName);

  for (ind = 0; ind < ilistSize(inFile->stackSetList); ind++) {
    stackd = (StackSetData *)ilistItem(inFile->stackSetList, ind);
    B3DFREE(stackd->name);
  }
  ilistDelete(inFile->stackSetList);
  B3DFREE(inFile->zToDataSetMap);
  B3DFREE(inFile->header);
}

/* 
 * Read and write functions, passed on to calls in I/O module
 */
static int hdfReadSection(ImodImageFile *inFile, char *buf, int cz)
{
  return (hdfReadSectionAny(inFile, (unsigned char *)buf, cz, 0));
}

static int hdfReadSectionByte(ImodImageFile *inFile, char *buf, int cz)
{
  return (hdfReadSectionAny(inFile, (unsigned char *)buf, cz, MRSA_BYTE));
}

static int hdfReadSectionUShort(ImodImageFile *inFile, char *buf, int cz)
{
  return (hdfReadSectionAny(inFile, (unsigned char *)buf, cz, MRSA_USHORT));
}

static int hdfReadSectionFloat(ImodImageFile *inFile, char *buf, int cz)
{
  return (hdfReadSectionAny(inFile, (unsigned char *)buf, cz, MRSA_FLOAT));
}

static int hdfWriteSection(ImodImageFile *inFile, char *buf, int cz)
{
  return hdfWriteSectionAny(inFile, (unsigned char *)buf, cz, 0);
}

static int hdfWriteSectionFloat(ImodImageFile *inFile, char *buf, int cz)
{
  return hdfWriteSectionAny(inFile, (unsigned char *)buf, cz, 1);
}

/*
 * Set up the I/O and other functions and a few other iifile members
 */
static void setIOFuncsPlus(ImodImageFile *iiVol, int hdfSource, int writable, 
                           hid_t fileID)
{
  iiVol->hdfSource = hdfSource;
  iiVol->hdfFileID = (int)fileID;
  iiVol->file = IIFILE_HDF;
  iiVol->fp = (FILE *)iiVol;
   ((MrcHeader *)iiVol->header)->fp = iiVol->fp;
  iiVol->cleanUp = hdfDelete;
  iiVol->reopen = hdfReopen;
  iiVol->close = hdfClose;
  iiVol->fillMrcHeader = iiMRCfillHeader;
  iiVol->syncFromMrcHeader = hdfSyncFromMrcHeader;
  iiVol->readSection = hdfReadSection;
  iiVol->readSectionByte = hdfReadSectionByte;
  iiVol->readSectionUShort = hdfReadSectionUShort;
  iiVol->readSectionFloat = hdfReadSectionFloat;
  
  if (writable && hdfSource == IIHDF_IMOD) {
    iiVol->writeSection = hdfWriteSection;
    iiVol->writeSectionFloat = hdfWriteSectionFloat;
    iiVol->writeHeader = hdfWriteHeader;
  }
}

/*
 * Create and initialize a map from Z value to dataset; make it big enough and either
 * set it to a simple sequence or initialize with -1's
 */
static int setupZtoSetMap(hid_t fileID, ImodImageFile *inFile, int size, int sequence)
{
  int set;
  inFile->zToDataSetMap = B3DMALLOC(int, size);
  if (!inFile->zToDataSetMap) {
    cleanupFromOpen(fileID, 1, inFile->numVolumes, inFile);
    return 1;
  }
  inFile->zMapSize = size;
  for (set = 0; set < size; set++)
    inFile->zToDataSetMap[set] = sequence ? set : -1;
  return 0;
}

/*
 * Open the group containing a dataset for writing attributes
 */
static hid_t openDatasetGroup(ImodImageFile *inFile, const char *dsName)
{
  int ind;
  char *last = strrchr(dsName, '/');
  if (!last)
    return -1;
  ind = last - dsName;
  if (manageMallocBuf((void **)&sStringBuf, &sStrBufSize, ind + 1, 1))
    return -1;
  strncpy(sStringBuf, dsName, ind);
  sStringBuf[ind] = 0x00;
  return H5Gopen((hid_t)inFile->hdfFileID, sStringBuf, H5P_DEFAULT);
}

/*
 * Cleanup from failed or completed open
 */
static void cleanupFromOpen(hid_t fileID, int closeFile, int numVol,
                            ImodImageFile *inFile)
{
  int ind;
  GroupData *group;
  DatasetData *dataset;
  if (closeFile) {
    H5Fclose(fileID);
    inFile->fp = fopen(inFile->filename, inFile->fmode);
  }
  for (ind = 0; ind < ilistSize(sGroups); ind++) {
    group = (GroupData *)ilistItem(sGroups, ind);
    B3DFREE(group->name);
  }
  ilistDelete(sGroups);
  sGroups = NULL;
  for (ind = 0; ind < ilistSize(sDatasets); ind++) {
    dataset = (DatasetData *)ilistItem(sDatasets, ind);
    if (closeFile)
      H5Dclose(dataset->dsetID);
    B3DFREE(dataset->name);
  }
  ilistDelete(sDatasets);
  sDatasets = NULL;
  if (numVol) {
    for (ind = 1; ind < numVol; ind++) {
      B3DFREE(inFile->iiVolumes[ind]->header);
      free(inFile->iiVolumes[ind]);
    }
    B3DFREE(inFile->iiVolumes);
  }
  cleanupMallocBufs();
}

/*
 * Manage an allocated buffer to be big enough
 */
static int manageMallocBuf(void **buffer, int *size, int needed, int dsize)
{
  if (needed > *size) {
    B3DFREE(*buffer);
    *buffer = malloc(needed * dsize);
    *size = 0;
    if (! *buffer)
      return IIERR_MEMORY_ERR;
    *size = needed;
  }
  return 0;
}

/*
 * Clean up the allocated buffers
 */
static void cleanupMallocBufs()
{
  B3DFREE(sAttribName);
  sAttrNameSize = 0;
  B3DFREE(sFloatBuf);
  sFloatBufSize = 0;
  B3DFREE(sShortBuf);
  sShortBufSize = 0;
  B3DFREE(sUShortBuf);
  sUShortBufSize = 0;
  B3DFREE(sIntBuf);
  sIntBufSize = 0;
  B3DFREE(sStringBuf);
  sStrBufSize = 0;
  B3DFREE(sMrcPrefix);
}

/********************************************/
/* Attribute and autodoc routines           */
/********************************************/

/*
 * Copy all the attributes in the given object to the given collection and section index
 * of the current autodoc 
 */
static int attributesToAdoc(hid_t objID, int numAttrib, const char *typeName, int sectInd)
{
  hid_t attribID, typeID, spaceID, strTypeID;
  int ind, len, dsize, csize, signedInt, retval = 0, numVals, j;
  H5T_class_t class;
  H5S_class_t spaceType;
  b3dUInt16 *usbuf;

  /* Loop on the attributes in the object */
  /* printf("attributesToAdoc obj %d numAtt %d  type %s sect %d\n", objID, numAttrib,
     typeName, sectInd); */
  for (ind = 0; ind < numAttrib; ind++) {
    attribID = H5Aopen_by_idx(objID, ".", H5_INDEX_NAME, H5_ITER_INC, (hsize_t)ind,
                              H5P_DEFAULT, H5P_DEFAULT);

    /* Get dataspace and type, and process allowed types */
    spaceID = H5Aget_space(attribID);
    typeID = H5Aget_type(attribID);
    len = H5Aget_name(attribID, 0, NULL) + 1;
    if (manageMallocBuf((void **)&sAttribName, &sAttrNameSize, len, 1))
      return IIERR_MEMORY_ERR;
    H5Aget_name(attribID, sAttrNameSize, sAttribName);
  
    class = H5Tget_class(typeID);
    spaceType = H5Sget_simple_extent_type(spaceID);
    if ((class == H5T_INTEGER || class == H5T_FLOAT) && spaceType == H5S_SIMPLE) {

      /* Numeric types: get bytes, signed, and number of values */
      dsize = H5Tget_precision(typeID) / 8;
      signedInt = H5Tget_sign(typeID) == H5T_SGN_2 ? 1 : 0;
      numVals = H5Sget_simple_extent_npoints(spaceID);
      if (class == H5T_FLOAT && dsize == 4) {

        /* Float values: get the values and set them in the autodoc */
        if (manageMallocBuf((void **)&sFloatBuf, &sFloatBufSize, numVals, 4))
          retval = IIERR_MEMORY_ERR;
        if (!retval && H5Aread(attribID, H5T_NATIVE_FLOAT, sFloatBuf) < 0)
          retval = IIERR_IO_ERROR;
        if (!retval && numVals == 1) {
          if (AdocSetFloat(typeName, sectInd, sAttribName, sFloatBuf[0]))
            retval = IIERR_MEMORY_ERR;
        } else if (!retval && numVals == 2) {
          if (AdocSetTwoFloats(typeName, sectInd, sAttribName, sFloatBuf[0], 
                               sFloatBuf[1]))
            retval = IIERR_MEMORY_ERR;
        } else if (!retval && numVals == 3) {
          if (AdocSetThreeFloats(typeName, sectInd, sAttribName, sFloatBuf[0],
                                 sFloatBuf[1], sFloatBuf[2]))
            retval = IIERR_MEMORY_ERR;
        } else if (!retval) {
          if (AdocSetFloatArray(typeName, sectInd, sAttribName, sFloatBuf, numVals))
            retval = IIERR_MEMORY_ERR;
        }
        
      } else if (dsize == 2 || (dsize == 4 && signedInt)) {

        /* Integer values of various types: get them all into an integer array */
        if (manageMallocBuf((void **)&sIntBuf, &sIntBufSize, numVals, 4))
          retval = IIERR_MEMORY_ERR;
        if (!retval && dsize == 2 && 
            manageMallocBuf((void **)&sShortBuf, &sShortBufSize, numVals, 4))
          retval = IIERR_MEMORY_ERR;
        if (!retval && dsize == 2) {
          if (H5Aread(attribID, signedInt ? H5T_NATIVE_SHORT : H5T_NATIVE_USHORT, 
                      sShortBuf) < 0)
            retval = IIERR_IO_ERROR;
          if (!retval && signedInt) {
            for (j = 0; j < numVals; j++)
              sIntBuf[j] = sShortBuf[j];
          } else if (!retval) {
            usbuf = (b3dUInt16 *)sShortBuf;
            for (j = 0; j < numVals; j++)
              sIntBuf[j] = usbuf[j];
          }
        } else if (!retval) {
          if (H5Aread(attribID, H5T_NATIVE_INT, sIntBuf) < 0)
            retval = IIERR_IO_ERROR;
        }

        /* Now set the integer values in the autodoc */
        if (!retval && numVals == 1) {
          if (AdocSetInteger(typeName, sectInd, sAttribName, sIntBuf[0]))
            retval = IIERR_MEMORY_ERR;
        } else if (!retval && numVals == 2) {
          if (AdocSetTwoIntegers(typeName, sectInd, sAttribName, sIntBuf[0], sIntBuf[1]))
            retval = IIERR_MEMORY_ERR;
        } else if (!retval && numVals == 3) {
          if (AdocSetThreeIntegers(typeName, sectInd, sAttribName, sIntBuf[0], sIntBuf[1],
                                   sIntBuf[2]))
            retval = IIERR_MEMORY_ERR;
        } else if (!retval) {
          if (AdocSetIntegerArray(typeName, sectInd, sAttribName, sIntBuf, numVals))
            retval = IIERR_MEMORY_ERR;
        }
      }
    } else if (class == H5T_STRING && spaceType == H5S_SCALAR) {
      csize = H5Tget_size(typeID);
      strTypeID = H5Tget_native_type(typeID, H5T_DIR_ASCEND);
      if (manageMallocBuf((void **)&sStringBuf, &sStrBufSize, csize + 1, 1))
        retval = IIERR_MEMORY_ERR;
      if (!retval && H5Aread(attribID, strTypeID, sStringBuf) < 0)
        retval = IIERR_IO_ERROR;
      if (!retval && AdocSetKeyValue(typeName, sectInd, sAttribName, sStringBuf))
        retval = IIERR_MEMORY_ERR;
      H5Tclose(strTypeID);
    }
    H5Sclose(spaceID);
    H5Tclose(typeID);
    H5Aclose(attribID);
    if (retval)
      return retval;
  }
  return 0;
}

/*
 * Add all the entries in one section of the current autodoc to attributes under the
 * parent group/dataset.  If prefix is non-NULL, it will be attached to each key unless it
 * is already present.
 */
static int adocToAttributes(hid_t parentID, const char *typeName, int sectInd, 
                            const char *prefix)
{
  int valType, keyInd, numVals, numKeys, retval = 0;
  char *key, *valStr;
  const char *prefKey;

  numKeys = AdocGetNumberOfKeys(typeName, sectInd);
  if (numKeys < 0)
    return 1;
  for (keyInd = 0; keyInd < numKeys; keyInd++) {
    if (AdocGetKeyByIndex(typeName, sectInd, keyInd, &key) < 0)
      return 1;
    if (!key)
      continue;
    prefKey = key;
    if (prefix && !startsWith(key, prefix))
      prefKey = prefixedKey(prefix, key);
    if (AdocGetValTypeAndSize(typeName, sectInd, key, &valType, &numVals))
      return 1;
    if (valType == ADOC_STRING) {
      if (AdocGetString(typeName, sectInd, key, &valStr))
        return 1;
      retval = addStringAttribute(parentID, prefKey, valStr);
      free(valStr);
    } else if (valType >= ADOC_ONE_INT && valType <= ADOC_INT_ARRAY) {
      if (manageMallocBuf((void **)&sIntBuf, &sIntBufSize, numVals, 4))
        return 1;
      numVals = 0;
      if (AdocGetIntegerArray(typeName, sectInd, key, sIntBuf, &numVals, sIntBufSize))
        return 1;
      if (addIntegerAttribute(parentID, prefKey, sIntBuf, numVals))
        return 1;
      
    } else if (valType >= ADOC_ONE_FLOAT && valType <= ADOC_FLOAT_ARRAY) {
      if (manageMallocBuf((void **)&sFloatBuf, &sFloatBufSize, numVals, 4))
        return 1;
      numVals = 0;
      if (AdocGetFloatArray(typeName, sectInd, key, sFloatBuf, &numVals, 
                            sFloatBufSize))
        return 1;
      if (addFloatAttribute(parentID, prefKey, sFloatBuf, numVals))
        return 1;
    }
    if (retval < 0)
      return 1;
  }
  return 0;
}

/*
 * Add an integer attribute with one or more values
 */
static int addIntegerAttribute(hid_t parentID, const char *key, int *ivals, int numVals)
{
  int retval;
  hid_t spaceID, attribID;
  hsize_t hnumVals = numVals;
  if (H5Aexists_by_name(parentID, ".", key, H5P_DEFAULT) > 0)
    H5Adelete_by_name(parentID, ".", key, H5P_DEFAULT);
  spaceID = H5Screate_simple(1, &hnumVals, &hnumVals);
  if (spaceID < 0)
    return 1;
  attribID = H5Acreate(parentID, key, H5T_NATIVE_INT, spaceID, H5P_DEFAULT, H5P_DEFAULT);
  if (attribID < 0)
    retval = -1;
  else {
    retval = H5Awrite(attribID, H5T_NATIVE_INT, ivals);
    H5Aclose(attribID);
  }
  H5Sclose(spaceID);
  return (retval < 0 ? 1 : 0); 
}

/*
 * Add a float attribute with one or more values
 */
static int addFloatAttribute(hid_t parentID, const char *key, float *vals, int numVals)
{
  int retval;
  hid_t spaceID, attribID;
  hsize_t hnumVals = numVals;
  if (H5Aexists_by_name(parentID, ".", key, H5P_DEFAULT) > 0)
    H5Adelete_by_name(parentID, ".", key, H5P_DEFAULT);
  spaceID = H5Screate_simple(1, &hnumVals, &hnumVals);
  if (spaceID < 0)
    return 1;
  attribID = H5Acreate(parentID, key, H5T_NATIVE_FLOAT, spaceID, H5P_DEFAULT,
                       H5P_DEFAULT);
  if (attribID < 0)
    retval = -1;
  else {
    retval = H5Awrite(attribID, H5T_NATIVE_FLOAT, vals);
    H5Aclose(attribID);
  }
  H5Sclose(spaceID);
  return (retval < 0 ? 1 : 0); 
}

/*
 * Add a string attribute
 */
static int addStringAttribute(hid_t parentID, const char *key, const char *valStr)
{
  int retval;
  hid_t spaceID, attribID, typeID;

  if (H5Aexists_by_name(parentID, ".", key, H5P_DEFAULT) > 0)
    H5Adelete_by_name(parentID, ".", key, H5P_DEFAULT);
  spaceID = H5Screate(H5S_SCALAR);
  if (spaceID < 0)
    return 1;
  typeID = H5Tcopy(H5T_C_S1);
  if (typeID < 0) {
    H5Sclose(spaceID);
    return 1;
  }
  H5Tset_size(typeID, strlen(valStr) + 1);
  H5Tset_strpad(typeID, H5T_STR_NULLTERM);
  attribID = H5Acreate(parentID, key, typeID, spaceID, H5P_DEFAULT, H5P_DEFAULT);
  if (attribID < 0)
    retval = -1;
  else {
    retval = H5Awrite(attribID, typeID, valStr);
    H5Aclose(attribID);
  }
  H5Sclose(spaceID);
  H5Tclose(typeID);
  return (retval < 0 ? 1 : 0); 
}

/*
 * Convenience functions to add a single integer or float and increment an error argument
 */
static int addOnePrefixedInteger(hid_t parentID, const char *key, int ival, int *errSum)
{
  int err = addIntegerAttribute(parentID, prefixedKey(sMrcPrefix, key), &ival, 1);
  if (err)
    (*errSum)++;
  return err;
}

static int addOnePrefixedFloat(hid_t parentID, const char *key, float val, int *errSum)
{
  int err = addFloatAttribute(parentID, prefixedKey(sMrcPrefix, key), &val, 1);
  if (err)
    (*errSum)++;
  return err;
}

/*
 * Get an integer from autodoc with the current MRC prefix
 */
static int getPrefixedInteger(const char *key, int *value, int *errSum)
{
  int err = AdocGetInteger(sPreData, 0, prefixedKey(sMrcPrefix, key), value);
  if (err < 0)
    (*errSum)++;
  return err;
}

/*
 * Get an integer or float from autodoc with current MRC prefix and delete it
 */
static int getDelPrefixedInteger(const char *key, int *value, int *errSum)
{
  int err = AdocGetInteger(sPreData, 0, prefixedKey(sMrcPrefix, key), value);
  if (!err)
    deletePrefixedKeyValue(key);
  if (err < 0)
    (*errSum)++;
  return err;
}

static int getDelPrefixedFloat(const char *key, float *value, int *errSum)
{
  int err = AdocGetFloat(sPreData, 0, prefixedKey(sMrcPrefix, key), value);
  if (!err)
    deletePrefixedKeyValue(key);
  if (err < 0)
    (*errSum)++;
  return err;
}

/* Delete a value from autodoc under the current MRC prefix */
static int deletePrefixedKeyValue(const char *key)
{
  return AdocDeleteKeyValue(sPreData, 0, prefixedKey(sMrcPrefix, key));
}

/* Test whether fullStr starts with subStr */
static int startsWith(const char *fullStr, const char *subStr)
{
  if (strstr(fullStr, subStr) == fullStr)
    return 1;
  return 0;
}

/* Test whether fullStr ends with subStr */
static int endsWith(const char *fullStr, const char *subStr)
{
  char *subPtr = strstr(fullStr, subStr);
  if (!subPtr || strlen(subPtr) != strlen(subStr))
    return -1;
  return subPtr - fullStr;
}

/*
 * Compose a key with the given prefix in sStringBuf
 */
static const char *prefixedKey(const char *prefix, const char *key)
{
  int len1, len2;
  if (!prefix)
    return key;
  len1 = strlen(prefix);
  if (!len1)
    return key;
  len2 = strlen(key);
  if (manageMallocBuf((void **)&sStringBuf, &sStrBufSize, len1 + len2 + 1, 1))
    return key;
  sprintf(sStringBuf, "%s%s", prefix, key);
  return sStringBuf;
}

