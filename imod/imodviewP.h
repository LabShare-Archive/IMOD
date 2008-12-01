/*   imodview.h  -  header file for private imodview functions
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end
 */                                                                           

#ifndef IMODVIEWP_H
#define IMODVIEWP_H

int ivwReopen(ImodImageFile *inFile);
unsigned char **ivwGetCurrentSection(ImodView *iv);
unsigned char **ivwMakeLinePointers(ImodView *iv, unsigned char *data,
                                    int xsize, int ysize, int mode);
int ivwSetupFastAccess(ImodView *vi, unsigned char ***outImdata,
                       int inNullvalue, int *cacheSum, int time = -1);
int ivwInitCache(ImodView *vi);

/* Determines size of data unit */
int ivwGetPixelBytes(int mode);
int  ivwPlistBlank(ImodView *vi, int cz);
     

void ivwBindMouse(ImodView *vw);

int  ivwScale(ImodView *vw);
int  ivwFlip(ImodView *vw);
void ivwInit(ImodView *vi, bool modview);
int  ivwPointVisible(ImodView *vw, Ipoint *pnt);

int  imodImageFileDesc(FILE *fin);
int  ivwLoadIMODifd(ImodView *vi);
int  ivwLoadImage(ImodView *iv);
void ivwFlushCache(ImodView *vi, int time);
void ivwMultipleFiles(ImodView *iv, char *argv[], int firstfile, 
		      int lastimage);

void ivwTransModel(ImodView *iv);
void ivwSetModelTrans(ImodView *iv);
void ivwFlipModel(ImodView *iv);
void ivwCheckWildFlag(Imod *imod);
void ivwScaleDepth8(ImodView *iv, ivwSlice *tempSlicePtr);
void ivwReadZ(ImodView *iv, unsigned char *buf, int cz);
int ivwReadBinnedSection(ImodView *vi, char *buf, int section);
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

bool ivwTimeMismatch(ImodView *vi, int timelock, Iobj *obj, Icont *cont);
int ivwRegisterInsertPoint(ImodView *vi, Icont *cont, Ipoint *pt, int index);
void startExtraObjectIfNone(ImodView *vi);

#endif

/*

$Log$
Revision 1.11  2008/11/28 06:39:12  mast
Made extra object function global

Revision 1.10  2007/11/27 17:56:38  mast
Rearranged log

Revision 1.9  2007/05/29 14:43:48  mast
Added optional time argument to fast setup routine

Revision 1.8  2005/12/08 05:57:13  mast
Added time argument to cache flushing call

Revision 1.7  2004/11/20 05:05:27  mast
Changes for undo/redo capability

Revision 1.6  2004/11/07 22:59:52  mast
Make binning routine global

Revision 1.5  2004/10/27 20:38:30  mast
Changed arguments for cache dump routines

Revision 1.4  2004/10/22 22:17:22  mast
Added functions for dumping file system cache

Revision 1.3  2004/01/05 17:55:45  mast
Changes for binning

Revision 1.2  2003/12/30 06:39:21  mast
Make memreccpy globally available

Revision 1.1  2003/10/01 05:01:29  mast
Initial creation; functions pulled from imodP.h

*/
