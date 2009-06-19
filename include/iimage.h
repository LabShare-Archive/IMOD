/* iimage.h - definitions and declarations for IMOD image files
 */
/*  $Author$

$Date$

$Revision$
Log at end
*/

#ifndef IIMAGE_H
#define IIMAGE_H

#include "mrcfiles.h"
#include "mrcslice.h"
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

  /* DOC_CODE ImodImageFile definitions */
  /* Values for the file member of ImodImageFile, describing kind of file */
#define IIFILE_UNKNOWN 0
#define IIFILE_TIFF    1
#define IIFILE_MRC     2
#define IIFILE_QIMAGE  3
#define IIFILE_RAW     4

  /* Values for the format member of ImodImageFile, describing kind data */
#define IIFORMAT_LUMINANCE 0
#define IIFORMAT_RGB       1
#define IIFORMAT_RGBA      2
#define IIFORMAT_COMPLEX   3
#define IIFORMAT_COLORMAP  4

  /* Values for the type member of ImodImageFile, describing numeric type */
#define IITYPE_UBYTE   0
#define IITYPE_BYTE  1
#define IITYPE_SHORT  2
#define IITYPE_USHORT 3
#define IITYPE_INT    4
#define IITYPE_UINT   5
#define IITYPE_FLOAT  6

  /* Values for the state member of ImodImageFile, describing file state */
#define IISTATE_NOTINIT 0
#define IISTATE_PARK    1
#define IISTATE_READY   2
#define IISTATE_BUSY    4

  /* Error codes, used by check routines */
#define IIERR_BAD_CALL  -1
#define IIERR_NOT_FORMAT 1
#define IIERR_IO_ERROR   2
#define IIERR_MEMORY_ERR 3
#define IIERR_NO_SUPPORT 4
/* END_CODE */

  /* DOC_CODE Raw mode codes */
  /* Yet another set of mode values, which define the order for radio buttons
     in a raw type selector dialog */
#define RAW_MODE_BYTE          0
#define RAW_MODE_SHORT         1
#define RAW_MODE_USHORT        2
#define RAW_MODE_FLOAT         3
#define RAW_MODE_COMPLEX_FLOAT 4
#define RAW_MODE_RGB           5
/* END_CODE */

#define IIAXIS_X 1
#define IIAXIS_Y 2
#define IIAXIS_Z 3

  struct  ImodImageFileStruct;

  /* DOC_CODE ImodImageFile structure */
  typedef struct  ImodImageFileStruct ImodImageFile;
  struct  ImodImageFileStruct
  {
    char *filename;
    char *fmode;
    FILE *fp;
    char *description;
    int   state;

    /* Data set by new and open functions. */
    int   nx, ny, nz;
    int   file;       /* Type of file, i.e. MRC, TIF... */
    int   format;     /* Kind of data represented: i.e. gray, color, complex */
    int   type;       /* Type if numerical elements, i.e. byte, etc. */
    int   mode;       /* MRC mode value */

    /* optional data to be set if input file supports it. */
    float amin, amax, amean;
    float xscale, yscale, zscale;
    float xtrans, ytrans, ztrans;
    float xrot,   yrot,   zrot;
    int   time, wave;

    /* load info: change these for loading sub sections. */
    int   llx, lly, llz, urx, ury, urz;
    float slope, offset, smin, smax;
    int   axis;
    int   mirrorFFT;   /* Return mirrored FFT when scaling to bytes */

    /* extra storage used by individual file format functions. */
    int   headerSize;
    int   sectionSkip;
    char *header;
    char *userData;
    unsigned char *colormap;
    int  planesPerImage;     /* # of planes per TIFF image */
    int  contigSamples;      /* # of contiguous samples per pixel in plane */
    int  multipleSizes;      /* Flag that TIFF file has multiple sizes */
    int  rgbSamples;         /* Number of samples for RGB TIFF file */

    /* Callback functions used by different file formats. */
    int (*readSection)(ImodImageFile *inFile, char *buf, int inSection);
    int (*readSectionByte)(ImodImageFile *inFile, char *buf, int inSection);
    void (*cleanUp)(ImodImageFile *inFile);
    int (*writeSection)(ImodImageFile *inFile, char *buf, int inSection);
    void (*close)(ImodImageFile *inFile);
    int (*reopen)(ImodImageFile *inFile);

  };
/* END_CODE */

  /* DOC_CODE RawImageInfo structure */
  /* A structure for passing bare-bones information about an MRC-like (raw)
     file to a routine that makes an MRC header */
  typedef struct raw_image_info {
    int type;           /* Data type, one of the RAW_MODE_* values */
    int nx, ny, nz;     /* Size of file in X, Y, Z */
    int swapBytes;      /* Whether bytes are swapped */
    int headerSize;     /* Offset to data */
    float amin, amax;   /* Data min and max, set to 0 if unknown */
    int scanMinMax;     /* Flag that scan is needed, used internally */
    int allMatch;       /* Flag that all files match, used internally */
    int sectionSkip;    /* Padding after each section - there may be no padding
                           after last section */
  } RawImageInfo;
/* END_CODE */

  typedef int (*IIFileCheckFunction)(ImodImageFile *);
  typedef int (*IIRawCheckFunction)(FILE *, char *, RawImageInfo *);

  void iiAddCheckFunction(IIFileCheckFunction func);
  void iiInsertCheckFunction(IIFileCheckFunction func, int index);
  void iiDeleteCheckList();
  void iiAddRawCheckFunction(IIRawCheckFunction func, char *name);
  void iiDeleteRawCheckList();
  ImodImageFile *iiNew(void);
  ImodImageFile *iiOpen(const char *filename, char *mode);
  int  iiReopen(ImodImageFile *inFile);
  void iiClose(ImodImageFile *inFile);
  void iiDelete(ImodImageFile *inFile);
  int  iiSetMM(ImodImageFile *inFile, float inMin, float inMax);

  int iiReadSection(ImodImageFile *inFile, char *buf, int inSection);
  int iiReadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
  int iiLoadPCoord(ImodImageFile *inFile, struct LoadInfo *li, int nx, int ny, 
                   int nz);

  /* Create and write support. */
  int iiInit(ImodImageFile *i, int xsize, int ysize, int zsize, 
             int file, int format, int type);
  int iiWriteSection(ImodImageFile *inFile, char *buf, int inSection);

  /* Declarations for specific file types needed by other modules */
  int iiTIFFCheck(ImodImageFile *inFile);
  int iiMRCCheck(ImodImageFile *inFile);
  int iiMRCreadSection(ImodImageFile *inFile, char *buf, int inSection);
  int iiMRCreadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
  int iiMRCLoadPCoord(ImodImageFile *inFile, struct LoadInfo *li, int nx,
                      int ny, int nz);
  int tiffReadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
  int tiffReadSection(ImodImageFile *inFile, char *buf, int inSection);
  void tiffClose(ImodImageFile *inFile);
  int tiffGetField(ImodImageFile *inFile, int tag, void *value);
  int tiffGetArray(ImodImageFile *inFile, int tag, int *count, void *value);
  void tiffSuppressWarnings(void);
  void tiffSuppressErrors(void);
  void tiffFilterWarnings(void);
  int tiffOpenNew(ImodImageFile *inFile);
  int tiffWriteSection(ImodImageFile *inFile, void *buf, int compression, 
                       int inverted);
  int iiLikeMRCCheck(ImodImageFile *inFile);
  void iiLikeMRCDelete(ImodImageFile *inFile);
  int iiSetupRawHeaders(ImodImageFile *inFile, RawImageInfo *info);
  int analyzeDM3(FILE *fp, char *filename, RawImageInfo *info, int *dmtype);

#ifdef __cplusplus
}
#endif

#endif

/*
$Log$
Revision 3.16  2009/03/31 23:44:30  mast
New TIFF functions

Revision 3.15  2009/01/02 05:19:19  mast
const char * for Qt 4 port

Revision 3.14  2008/11/24 23:50:07  mast
Changes for using in SerialEM

Revision 3.13  2008/11/18 22:43:54  mast
Changed include to eliminate mrcc.h

Revision 3.12  2007/06/13 17:07:45  sueh
bug# 1019 Adding sectionSkip to ImodImageFile and RawImageInfo.

Revision 3.11  2006/09/21 22:26:35  mast
Adedd function to insert check function earlier in list

Revision 3.10  2006/09/03 22:17:07  mast
Fiddled for raw checking function, documented

Revision 3.9  2006/09/02 23:50:33  mast
Added mrc-like structure and functions

Revision 3.8  2006/08/27 23:47:12  mast
Added colormap

Revision 3.7  2005/02/11 01:42:33  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 3.6  2004/12/02 21:49:44  mast
Add declarations for mrc functions needed elsewhere

Revision 3.5  2004/11/30 03:47:10  mast
Declared new function to add check functions

Revision 3.4  2004/11/05 18:52:53  mast
Include local files with quotes, not brackets

Revision 3.3  2004/11/04 17:08:29  mast
Added element for mirroring FFTs

Revision 3.2  2004/01/05 17:24:00  mast
Renamed imin/imax to smin/smax and changed iiSetMM arguments to float

Revision 3.1  2002/12/01 15:39:50  mast
Declare extern C if c++

*/
