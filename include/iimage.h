/*  $Author$

$Date$

$Revision$

$Log$
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
#ifndef IIMAGE_H
#define IIMAGE_H

#include "mrcc.h"
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

#define IIFILE_UNKNOWN 0
#define IIFILE_TIFF    1
#define IIFILE_MRC     2
#define IIFILE_QIMAGE  3
#define IIFILE_RAW     4

#define IIFORMAT_LUMINANCE 0
#define IIFORMAT_RGB       1
#define IIFORMAT_RGBA      2
#define IIFORMAT_COMPLEX   3

#define IITYPE_UBYTE   0
#define IITYPE_BYTE  1
#define IITYPE_SHORT  2
#define IITYPE_USHORT 3
#define IITYPE_INT    4
#define IITYPE_UINT   5
#define IITYPE_FLOAT  6

#define IIAXIS_X 1
#define IIAXIS_Y 2
#define IIAXIS_Z 3

#define IISTATE_NOTINIT 0
#define IISTATE_PARK    1
#define IISTATE_READY   2
#define IISTATE_BUSY    4

  struct  ImodImageFileStruct;
  typedef struct ImodImageFileStruct ImodImageFile; 
  typedef int (*IIFileCheckFunction)(ImodImageFile *);

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

    /* extra storage used by each file format functions. */
    int   headerSize;
    char *header;
    char *userData;

    /* Callback functions used by different file formats. */
    int (*readSection)(ImodImageFile *inFile, char *buf, int inSection);
    int (*readSectionByte)(ImodImageFile *inFile, char *buf, int inSection);
    void (*cleanUp)(ImodImageFile *inFile);
    int (*writeSection)(ImodImageFile *inFile, char *buf, int inSection);
    void (*close)(ImodImageFile *inFile);
    int (*reopen)(ImodImageFile *inFile);

  };

  void iiAddCheckFunction(IIFileCheckFunction func);
  ImodImageFile *iiNew(void);
  ImodImageFile *iiOpen(char *filename, char *mode);
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

#ifdef __cplusplus
}
#endif

#endif
