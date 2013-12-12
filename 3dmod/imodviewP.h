/*   imodview.h  -  header file for private imodview functions
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMODVIEWP_H
#define IMODVIEWP_H

class QStringList;

int ivwReopen(ImodImageFile *inFile);
unsigned char **ivwGetCurrentSection(ImodView *iv);
unsigned char **ivwMakeLinePointers(ImodView *iv, unsigned char *data,
                                    int xsize, int ysize, int mode);
int ivwSetupFastAccess(ImodView *vi, unsigned char ***outImdata,
                       int inNullvalue, int *cacheSum, int time = -1);
void ivwSetRGBChannel(int value);
int ivwSetupFastTileAccess(ImodView *vi, int cacheInd, int inNullvalue, int &cacheSum);
int ivwInitCache(ImodView *vi);

/* Determines size of data unit */
int ivwGetPixelBytes(int mode);
int  ivwPlistBlank(ImodView *vi, int cz);
     

void ivwBindMouse(ImodView *vw);

int  ivwScale(ImodView *vw);
int  ivwFlip(ImodView *vw);
void ivwInit(ImodView *vi, bool modview);
int  ivwPointVisible(ImodView *vw, Ipoint *pnt);
int imod_setxyzmouse(void);
int imod_redraw(ImodView *vw);

int  imodImageFileDesc(FILE *fin);
int  ivwLoadIMODifd(ImodView *vi, QStringList &plFileNames, bool &anyHavePieceList,
                    bool &anyImageFail);
int ivwLoadIFDpieceList(const char *plName, IloadInfo *li, int nx, int ny, int nz);
int  ivwLoadImage(ImodView *iv);
void ivwFlushCache(ImodView *vi, int time);
void ivwMultipleFiles(ImodView *iv, char *argv[], int firstfile, int lastimage, 
                      bool &anyHavePieceList);

void ivwTransModel(ImodView *iv);
void ivwSetModelTrans(ImodView *iv);
void ivwFlipModel(ImodView *iv, bool rotate = false);
void ivwCheckWildFlag(Imod *imod);
void ivwScaleDepth8(ImodView *iv, ivwSlice *tempSlicePtr);
void ivwReadZ(ImodView *iv, unsigned char *buf, int cz);
int ivwReadBinnedSection(ImodView *vi, char *buf, int section);
int ivwReadBinnedSection(ImodView *vi, ImodImageFile *image, char *buf, int section);
void ivwBinByN(unsigned char *array, int nxin, int nyin, int nbin, 
                      unsigned char *brray);
void ivwGetFileStartPos(ImodImageFile *image);
void ivwDumpFileSysCache(ImodImageFile *image);
void memreccpy
(unsigned char *tb,             /* copy data to buffer */
 unsigned char *fb,             /* copy data from buffer */
 int xcpy, int ycpy, int psize, /* amount/size of data to copy. */
 int tskip, int tox, int toy,   /* to buffer offsets, skip. */
 int fskip, int fox, int foy);   /* from buffer offsets, skip. */
void memLineCpy
(unsigned char **tlines,        /* line pointers to copy data to */
 unsigned char *fb,             /* copy data from buffer */
 int xcpy, int ycpy, int psize, /* amount / byte size of data to copy */
 int tox, int toy,              /* to buffer offsets */
 int fxsize, int fox, int foy);  /* from buffer X size and offsets */

bool ivwTimeMismatch(ImodView *vi, int timelock, Iobj *obj, Icont *cont);
int ivwWindowTime(ImodView *vi, int timelock);
int ivwRegisterInsertPoint(ImodView *vi, Icont *cont, Ipoint *pt, int index);
void startExtraObjectIfNone(ImodView *vi);
void ivwSetBlackWhiteFromModel(ImodView *vi);
bool ivwFixUnderSizeCoords(int size, int nx, int &llx, int &urx, int &offset, 
                           int &leftPad, int &rightPad);
int ivwGetImagePadding(ImodView *vi, int cy, int section, int time, int &llX, 
                       int &leftXpad, int &rightXpad, int &llY, int &leftYpad,
                       int &rightYpad, int &llz, int &leftZpad, int &rightZpad);

#endif

