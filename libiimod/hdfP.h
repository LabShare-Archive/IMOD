/*  hdfP.h  -  Includes and definitions needed only by the two HDF modules
 *
 *  $Id$
 */                                                                           

/* Common includes */
#include <stdlib.h>
#include <string.h>
#include "iimage.h"
#include "ilist.h"
#include "b3dutil.h"
#include "autodoc.h"
#include "hdf5.h"

/*
 * Structure to hold information about datasets in a stack of 2-D images
 */
typedef struct {
  char *name;
  hid_t dsetID;
  int isOpen;
} StackSetData;

/* Functions in hdf_imageio needed by iihdf */
int hdfReadSectionAny(ImodImageFile *inFile, unsigned char *buf, int cz, int type);
int hdfWriteSectionAny(ImodImageFile *inFile, unsigned char *buf, int cz, int asFloat);
int initNewHDFfile(ImodImageFile *inFile);
