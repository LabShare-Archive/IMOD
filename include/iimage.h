/* iimage.h - definitions and declarations for IMOD image files
 *  $Id$
 */

#ifndef IIMAGE_H
#define IIMAGE_H

#include "ilist.h"
#include "mrcfiles.h"
#include "mrcslice.h"
#include <math.h>

#ifdef __cplusplus
extern "C" {
#endif

  /* DOC_CODE ImodImageFile definitions */
  /* Values for the file member of ImodImageFile, describing kind of file */
  /* Except IIFILE_DEFAULT means use the default output file type */
#define IIFILE_DEFAULT -1
#define IIFILE_UNKNOWN 0
#define IIFILE_TIFF    1
#define IIFILE_MRC     2
#define IIFILE_QIMAGE  3
#define IIFILE_RAW     4
#define IIFILE_HDF     5

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
#define IISTATE_UNUSED  3
#define IISTATE_BUSY    4

  /* Error codes, used by check routines */
#define IIERR_BAD_CALL  -1
#define IIERR_NOT_FORMAT 1
#define IIERR_IO_ERROR   2
#define IIERR_MEMORY_ERR 3
#define IIERR_NO_SUPPORT 4

  /* Codes for source of HDF files */
#define IIHDF_IMOD       1
#define IIHDF_OTHER_MRC  2
#define IIHDF_EMAN       3
#define IIHDF_CHIMERA    4
#define IIHDF_UNKNOWN    5

  /* Flags for userData */
#define IIFLAG_BYTES_SWAPPED   1
#define IIFLAG_TVIPS_DATA      2
/* END_CODE */

  /* DOC_CODE Raw mode codes */
  /* Yet another set of mode values, which define the order for radio buttons
     in a raw type selector dialog */
#define RAW_MODE_SBYTE         0
#define RAW_MODE_BYTE          1
#define RAW_MODE_SHORT         2
#define RAW_MODE_USHORT        3
#define RAW_MODE_FLOAT         4
#define RAW_MODE_COMPLEX_FLOAT 5
#define RAW_MODE_RGB           6
/* END_CODE */

#define IIAXIS_X 1
#define IIAXIS_Y 2
#define IIAXIS_Z 3

#define MRSA_NOPROC 0
#define MRSA_BYTE   1
#define MRSA_FLOAT  2
#define MRSA_USHORT 3

  /* DOC_CODE ImodImageFile structure */
  typedef struct  ImodImageFileStruct ImodImageFile;
  typedef int (*iiSectionFunc)(ImodImageFile *inFile, char *buf, int inSection);

  struct  ImodImageFileStruct
  {
    char *filename;
    char fmode[4];
    FILE *fp;
    char *description;
    int   state;

    /* Data set by new and open functions. */
    int   nx, ny, nz;
    int   file;       /* Type of file, i.e. MRC, TIF... */
    int   format;     /* Kind of data represented: i.e. gray, color, complex */
    int   type;       /* Type if numerical elements, i.e. byte, etc. */
    int   mode;       /* MRC mode value */
    int   newFile;    /* Newly created file */
    
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
    int   padLeft;     /* Padding at start or end of line when reading or writing a */
    int   padRight;    /* subset of the array, in pixels not bytes */

    /* extra storage used by individual file format functions. */
    int   headerSize;
    int   sectionSkip;
    int   hasPieceCoords;    /* Flag that MRC file has piece coordinates in header */
    char *header;
    char *userData;
    unsigned int userFlags;  /* Flags for the userData */
    int userCount;           /* Number of bytes of userData */
    unsigned char *colormap;
    int  planesPerImage;     /* # of planes per TIFF image */
    int  contigSamples;      /* # of contiguous samples per pixel in plane */
    int  multipleSizes;      /* Flag that TIFF file has multiple sizes */
    int  rgbSamples;         /* Number of samples for RGB TIFF file */
    int  anyTiffPixSize;     /* Set non-0 to have TIFF pixel size put into [xyz]scale */
    int  tileSizeX;          /* Tile size in X, or 0 if no tiles */
    int  tileSizeY;          /* Tile size in Y if tiles, strip size if not */
    int  lastWrittenZ;       /* Last Z written, needed if sequential writing only */

    /* HDF variables */
    Ilist *stackSetList;     /* Names, ID's for stack of single-image datasets */
    int *zToDataSetMap;      /* Map from Z value to index in dataset list */
    int zMapSize;            /* Allocated size of Z map */
    char *datasetName;       /* Dataset name for single volume open in this iifile */
    int datasetID;           /* ID for dataset of single volume */
    int datasetIsOpen;       /* Flag that it is open */
    int numVolumes;          /* Number of volume datasets in the HDF file */
    ImodImageFile **iiVolumes;  /* Array of pointers to iifiles open in the HDF file */
    int adocIndex;           /* Index of autodoc with metadata for this iifile */
    int globalAdocIndex;     /* Index for global metadata in multifile case */
    int hdfSource;           /* Apparent source of file (IIHDF_ value) */
    int hdfFileID;           /* File ID */
    int zChunkSize;          /* Size of chunk in Z; must be nonzero for a volume */

    /* Callback functions used by different file formats. */
    iiSectionFunc readSection;
    iiSectionFunc readSectionByte;
    iiSectionFunc readSectionUShort;
    iiSectionFunc readSectionFloat;
    iiSectionFunc writeSection;
    iiSectionFunc writeSectionFloat;
    void (*cleanUp)(ImodImageFile *inFile);
    void (*close)(ImodImageFile *inFile);
    int (*reopen)(ImodImageFile *inFile);
    int (*fillMrcHeader)(ImodImageFile *inFile, MrcHeader *hdata);
    int (*syncFromMrcHeader)(ImodImageFile *inFile, MrcHeader *hdata);
    int (*writeHeader)(ImodImageFile *inFile);

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
    int yInverted;      /* Lines are inverted in Y */
    float pixel;        /* Pixel size in Angstroms, set to 0. if unknown */
    float zPixel;       /* Pixel size in Z if different, set to 0. otherwise */
  } RawImageInfo;
/* END_CODE */

/* Structure for line-processing function in mrcsec.c */
  typedef struct {
    int xStart, xEnd;
    int convert;
    unsigned char *bdata;
    unsigned char *buf;
    unsigned char *bufp;
    b3dUInt16 *usbufp;
    b3dFloat *fbufp;
    unsigned char *fft;
    b3dUInt16 *usfft;
    unsigned char *map;
    b3dUInt16 *usmap;
    int byte;
    int toShort;
    int mapSbytes;
    int doScale;
    int needData;
    int type;
    int xDimension;
    int xsize;
    int deltaYsign;
    unsigned int pixIndex;
    int cz;
    int readY;
    int line;
    int ymin, ymax;
    int yStart;
    int imYmin, imYmax;
    int imXsize;
    int toggleY;
    int seekEndY;
    int pixSize;
    int swapped;
    int bytesSinceCheck;
  } LineProcData;

  typedef int (*IIFileCheckFunction)(ImodImageFile *);
  typedef int (*IIRawCheckFunction)(FILE *, char *, RawImageInfo *);

  void iiAddCheckFunction(IIFileCheckFunction func);
  void iiInsertCheckFunction(IIFileCheckFunction func, int index);
  void iiDeleteCheckList();
  void iiAddRawCheckFunction(IIRawCheckFunction func, const char *name);
  void iiDeleteRawCheckList();
  void iiRegisterQuitCheck(int (*func)(int));
  int iiCheckForQuit();
  ImodImageFile *iiNew(void);
  ImodImageFile *iiOpen(const char *filename, const char *mode);
  ImodImageFile *iiOpenNew(const char *filename, const char *mode, int fileKind);
  int  iiReopen(ImodImageFile *inFile);
  void iiClose(ImodImageFile *inFile);
  void iiDelete(ImodImageFile *inFile);
  int iiFillMrcHeader(ImodImageFile *inFile, MrcHeader *hdata);
  void iiSyncFromMrcHeader(ImodImageFile *inFile, MrcHeader *hdata);
  int iiDefaultMinMaxMean(int type, float *amin, float *amax, float *amean);
  int iiWriteHeader(ImodImageFile *inFile);
  int  iiSetMM(ImodImageFile *inFile, float inMin, float inMax, float scaleMax);
  void iiFileChangeAddress(ImodImageFile *oldFile, ImodImageFile *newFile);
  FILE *iiFOpen(const char *filename, const char *mode);
  ImodImageFile *iiLookupFileFromFP(FILE *fp);
  void iiFClose(FILE *fp);
  void iiChangeCallCount(int delta);
  int iiCallingReadOrWrite();
  FILE *iiFOpenVolume(ImodImageFile *inFile, int volIndex);
  FILE *iiFOpenNewVolume(ImodImageFile *inFile);
  void iiAllowMultiVolume(int allow);
  int iiGetAdocIndex(ImodImageFile *inFile, int global, int openMdocOrNew);
  int iiTransferAdocSections(ImodImageFile *fromFile, ImodImageFile *toFile);
  int iiSetChunkSizes(ImodImageFile *inFile, int xSize, int ySize, int zSize);
  void iiConvertLineOfFloats(float *fbufp, unsigned char *bdata, int nx, int mrcMode, 
                             int bytesSigned);
  void iiSaveLoadParams(ImodImageFile *iiFile, ImodImageFile *iiSave);
  int iiRestoreLoadParams(int retVal, ImodImageFile *iiFile, ImodImageFile *iiSave);

  int iiReadSection(ImodImageFile *inFile, char *buf, int inSection);
  int iiReadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
  int iiReadSectionUShort(ImodImageFile *inFile, char *buf, int inSection);
  int iiReadSectionFloat(ImodImageFile *inFile, char *buf, int inSection);
  float iiReadPoint(ImodImageFile *inFile, int x, int y, int z);
  int iiLoadPCoord(ImodImageFile *inFile, int useMdoc, IloadInfo *li,
                   int nx, int ny, int nz);

  /* Create and write support. */
  int iiInit(ImodImageFile *i, int xsize, int ysize, int zsize, 
             int file, int format, int type);
  int iiWriteSection(ImodImageFile *inFile, char *buf, int inSection);
  int iiWriteSectionFloat(ImodImageFile *inFile, char *buf, int inSection);

  /* Declarations for specific file types needed by other modules */
  /* SOME OF THESE DO NOT NEED TO BE GLOBALS! */
  int iiTIFFCheck(ImodImageFile *inFile);
  int iiMRCCheck(ImodImageFile *inFile);
  int iiMRCfillHeader(ImodImageFile *inFile, MrcHeader *hdata);
  int iiMRCopenNew(ImodImageFile *inFile, const char *mode);
  void iiMRCsetIOFuncs(ImodImageFile *inFile, int rawFile);
  void iiMRCsetLoadInfo(ImodImageFile *inFile, IloadInfo *li);
  void iiMRCmodeToFormatType(ImodImageFile *iif, int mode, int bytesSigned);
  int iiMRCLoadPCoord(ImodImageFile *inFile, IloadInfo *li, int nx, int ny, int nz);
  int iiMRCcheckPCoord(MrcHeader *hdr);
  int tiffFillMrcHeader(ImodImageFile *inFile, MrcHeader *hdata);
  int tiffReadSectionByte(ImodImageFile *inFile, char *buf, int inSection);
  int tiffReadSectionUShort(ImodImageFile *inFile, char *buf, int inSection);
  int tiffReadSectionFloat(ImodImageFile *inFile, char *buf, int inSection);
  int tiffReadSection(ImodImageFile *inFile, char *buf, int inSection);
  void tiffClose(ImodImageFile *inFile);
  int tiffGetField(ImodImageFile *inFile, int tag, void *value);
  int tiffGetArray(ImodImageFile *inFile, int tag, b3dUInt16 *count, void *value);
  void tiffSuppressWarnings(void);
  void tiffSuppressErrors(void);
  void tiffFilterWarnings(void);
  int tiffOpenNew(ImodImageFile *inFile);
  int tiffWriteSection(ImodImageFile *inFile, void *buf, int compression, 
                       int inverted, int resolution, int quality);
  int tiffWriteSetup(ImodImageFile *inFile, int compression, int inverted, int resolution,
                     int quality, int *outRows, int *outNum, int *tileSizeX);
  int tiffWriteStrip(ImodImageFile *inFile, int strip, void *buf);
  void tiffWriteFinish(ImodImageFile *inFile);
  int iiTiffWriteSection(ImodImageFile *inFile, char *buf, int inSection);
  int iiTiffWriteSectionFloat(ImodImageFile *inFile, char *buf, int inSection);
  int tiffVersion(int *minor);
  int tiffReopen(ImodImageFile *inFile);
  void tiffDelete(ImodImageFile *inFile);
  void tiffSetMapping(int value);
  int iiLikeMRCCheck(ImodImageFile *inFile);
  void iiLikeMRCDelete(ImodImageFile *inFile);
  int iiSetupRawHeaders(ImodImageFile *inFile, RawImageInfo *info);
  int analyzeDM3(FILE *fp, char *filename, int dmformat, RawImageInfo *info, int *dmtype);

  int iiHDFCheck(ImodImageFile *inFile);
  int iiHDFopenNew(ImodImageFile *inFile, const char *mode);
  int hdfWriteGlobalAdoc(ImodImageFile *inFile);
  int iiProcessReadLine(MrcHeader *hdata, IloadInfo *li, LineProcData *d);
  int iiInitReadSectionAny(MrcHeader *hdata, IloadInfo *li, unsigned char *buf,
                           LineProcData *d, int *freeMap, int *yEnd, const char *caller);

#ifdef __cplusplus
}
#endif

#endif
