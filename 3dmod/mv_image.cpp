/*
 *  mv_image.cpp -- View image slice in model view control dialog.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <qcheckbox.h>
#include <qlayout.h>
#include <qgl.h>
#include <qslider.h>
#include <qpushbutton.h>
#include <qtooltip.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>
#include <QTime>
#include "preferences.h"
#include "multislider.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "mv_gfx.h"
#include "mv_stereo.h"
#include "mv_image.h"
#include "mv_input.h"
#include "b3dgfx.h"
#include "display.h"
#include "control.h"
#include "preferences.h"
#include "xcramp.h"


#define MAX_SLICES 1024

enum {IIS_X_COORD = 0, IIS_Y_COORD, IIS_Z_COORD, IIS_X_SIZE, IIS_Y_SIZE,
      IIS_Z_SIZE, IIS_SLICES, IIS_TRANSPARENCY, IIS_BLACK, IIS_WHITE};

static void makeColorMap(void);
static void imodvDrawTImage(Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *p4,
                            Ipoint *clamp, unsigned char *data,
                            int width, int height);
static void setAlpha(int iz, int zst, int znd, int izdir);
static void setSliceLimits(int ciz, int miz, bool invertZ, int drawTrans, 
                           int &zst, int &znd, int &izdir);
static void setCoordLimits(int cur, int maxSize, int drawSize, 
                           int &str, int &end);
static int initTexMapping(void);
static void endTexMapping(void);

// 3/23/11: NEW NAMING CONVENTION, sVariable for all static variables

static GLubyte *sTdata = NULL;
static int sTexImageSize = 0;
static GLuint sTexName = 0;
static GLubyte sCmap[3][256];
static int sBlackLevel = 0;
static int sWhiteLevel = 255;
static int sFalsecolor = 0;
static int sImageTrans = 0;
static int sCmapInit = 0;
static int cmapZ = 0;
static int cmapTime = 0;
static int sNumSlices = 1;
static int sXdrawSize = -1;
static int sYdrawSize = -1;
static int sZdrawSize = -1;
static int sLastYsize = -1;
ImodvImage *sDia = NULL;
ImodvApp  *sA = NULL;
int    sFlags = 0;
  
  /* DNM 12/30/02: unused currently */
  /* int    xsize, ysize, zsize;
     int    *xd, *yd, *zd; */

static double sWallLoad, sWallDraw;

// Open, close, or raise the dialog box
void imodvImageEditDialog(ImodvApp *a, int state)
{
  if (!state){
    if (sDia)
      sDia->close();
    return;
  }
  if (sDia) {
    sDia->raise();
    return;
  }

  sDia = new ImodvImage(imodvDialogManager.parent(IMODV_DIALOG), "image view");
  sA = a;

  makeColorMap();
  imodvDialogManager.add((QWidget *)sDia, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)sDia, IMODV_DIALOG);
  imodvImageUpdate(a);
}

// Update the dialog box (just the view flag for now)
void imodvImageUpdate(ImodvApp *a)
{
  if (a->texMap && !sFlags) {
    sFlags |= IMODV_DRAW_CZ;
    if (sDia)
      diaSetChecked(sDia->mViewZBox, true);
  } else if (!a->texMap && sFlags) {
    sFlags = 0;
    imodvImageCleanup();
    if (sDia) {
      diaSetChecked(sDia->mViewXBox, false);
      diaSetChecked(sDia->mViewYBox, false);
      diaSetChecked(sDia->mViewZBox, false);
    }
  }
  if (sDia) {
    sDia->mViewXBox->setEnabled(!(a->stereo && a->imageStereo));
    sDia->mViewYBox->setEnabled(!(a->stereo && a->imageStereo));
    sDia->mSliders->setEnabled(IIS_SLICES, !(a->stereo && a->imageStereo));
  }
}

int imodvImageGetFlags(void)
{
  return sFlags;
}

// Set the number of slices and the transparency from movie controller - 
// do not update the image
void imodvImageSetThickTrans(int slices, int trans)
{
 int maxSlices = Imodv->vi->zsize < MAX_SLICES ?
    Imodv->vi->zsize : MAX_SLICES;
  if (slices < 1)
    slices = 1;
  if (slices > maxSlices)
    slices = maxSlices;
  sNumSlices = slices;
  if (trans < 0)
    trans = 0;
  if (trans > 100)
    trans = 100;
  sImageTrans = trans;
  if (sDia) {
    sDia->mSliders->setValue(IIS_SLICES, sNumSlices);
    sDia->mSliders->setValue(IIS_TRANSPARENCY, sImageTrans);
  }
}

// Return the number of slices and transparancy
int imodvImageGetThickness(void)
{
  return sNumSlices;
}
int imodvImageGetTransparency(void)
{
  return sImageTrans;
}

// Return the current state of drawing into a movie segment structure
void mvImageGetMovieState(MovieSegment &segment)
{
  segment.imgAxisFlags = sFlags;
  if (!Imodv->texMap) {
    segment.imgAxisFlags = 0;
    return;
  }
  segment.imgWhiteLevel = sWhiteLevel;
  segment.imgBlackLevel = sBlackLevel;
  segment.imgFalseColor = sFalsecolor;
  segment.imgXsize = sXdrawSize;
  segment.imgYsize = sYdrawSize;
  segment.imgZsize = sZdrawSize;
}

// Set ancillary parameters to the state for a movie segment
int mvImageSetMovieDrawState(MovieSegment &segment)
{
 if (!segment.imgAxisFlags) {

    // If there is no image drawing, turn off Imodv flag and then, if there is currently,
    // take the update route to updating this module
    Imodv->texMap = 0;
    if (sFlags) 
      imodvImageUpdate(Imodv);
    return 1;
  }
  sFlags = segment.imgAxisFlags;
  Imodv->texMap = 1;
  sWhiteLevel = segment.imgWhiteLevel;
  sBlackLevel = segment.imgBlackLevel;
  sFalsecolor = segment.imgFalseColor;
  makeColorMap();
  sXdrawSize = B3DMIN(segment.imgXsize, Imodv->vi->xsize);
  sYdrawSize = B3DMIN(segment.imgYsize, Imodv->vi->ysize);
  sZdrawSize = B3DMIN(segment.imgZsize, Imodv->vi->zsize);

  // Update the dialog as needed
  if (sDia) {

    sDia->mSliders->setValue(IIS_X_SIZE, sXdrawSize);
    sDia->mSliders->setValue(IIS_Y_SIZE, sYdrawSize);
    sDia->mSliders->setValue(IIS_Z_SIZE, sZdrawSize);
    diaSetChecked(sDia->mViewXBox, (sFlags & IMODV_DRAW_CX) != 0);
    diaSetChecked(sDia->mViewYBox, (sFlags & IMODV_DRAW_CY) != 0);
    diaSetChecked(sDia->mViewZBox, (sFlags & IMODV_DRAW_CZ) != 0);
    sDia->mSliders->setValue(IIS_BLACK, sBlackLevel);
    sDia->mSliders->setValue(IIS_WHITE, sWhiteLevel);
    diaSetChecked(sDia->mFalseBox, sFalsecolor != 0);
  }
  return 0;
}

// Set the drawing state and dialog to the start or end of the given movie segment
void mvImageSetMovieEndState(int startEnd, MovieSegment &segment)
{
  MovieTerminus *term = &segment.start;
  int maxSlices = B3DMIN(Imodv->vi->zsize, MAX_SLICES);
  if (startEnd == IMODV_MOVIE_END_STATE)
    term = &segment.end;

  if (mvImageSetMovieDrawState(segment))
    return;
 
  // Otherwise set the image position and copy all the data over
  sImageTrans = term->imgTransparency;
  ivwSetLocation(Imodv->vi, term->imgXcenter - 1, term->imgYcenter - 1, 
                 term->imgZcenter - 1);
  sNumSlices = B3DMIN(term->imgSlices, maxSlices);

  // Update the dialog as needed
  if (sDia) {

    // Set this to prevent Y/Z swapping of sizes in the update
    sLastYsize = Imodv->vi->ysize;
    sDia->updateCoords();
    sDia->mSliders->setValue(IIS_SLICES, sNumSlices);
    sDia->mSliders->setValue(IIS_TRANSPARENCY, sImageTrans);
  }
}

/****************************************************************************/
/* TEXTURE MAP IMAGE. */
/****************************************************************************/

// Make a color map
static void makeColorMap(void)
{
  int rampsize, cmapReverse = 0;
  float slope, point;
  int r,g,b,i;
  ImodvApp *a = Imodv;
  int white = a->vi->colormapImage ? 255 : sWhiteLevel;
  int black = a->vi->colormapImage ? 0 : sBlackLevel;

  /* DNM 10/26/03: kept it from reversing the actual levels by copying to
     separate variables; simplified reversal */
  if (black > white){
    cmapReverse = black;
    black = white;
    white = cmapReverse;
    cmapReverse = 1;
  }

  rampsize = white - black;
  if (rampsize < 1) rampsize = 1;
     
  for (i = 0; i < black; i++)
    sCmap[0][i] = 0;
  for (i = white; i < 256; i++)
    sCmap[0][i] = 255;
  slope = 256.0 / (float)rampsize;
  for (i = black; i < white; i++){
    point = (float)(i - black) * slope;
    sCmap[0][i] = (unsigned char)point;
  }
     
  if (cmapReverse && !a->vi->colormapImage){
    for(i = 0; i < 256; i++)
      sCmap[0][i] = 255 - sCmap[0][i];
  }
  if (sFalsecolor || a->vi->colormapImage){
    for(i = 0; i < 256; i++){
      xcramp_mapfalsecolor(sCmap[0][i], &r, &g, &b);

      // Here is where the map becomes BGR
      sCmap[2][i] = (unsigned char)r;
      sCmap[1][i] = (unsigned char)g;
      sCmap[0][i] = (unsigned char)b;
    }
  }else{
    for(i = 0; i < 256; i++){
      sCmap[1][i] = sCmap[2][i] = sCmap[0][i];
    }
  }

  sCmapInit = 1;
}

// Determine starting and ending slice and direction, and set the alpha
static void setSliceLimits(int ciz, int miz, bool invertZ, int drawTrans, 
                           int &zst, int &znd, int &izdir)
{
  zst = ciz - sNumSlices / 2;
  znd = zst + sNumSlices - 1;
  if (zst < 0)
    zst = 0;
  if (znd >= miz)
    znd = miz - 1;
  izdir = 1;

  // If transparency is needed and it is time to draw solid, or no transparency
  // is needed and it time to draw transparent, set up for no draw
  if (((znd > zst || sImageTrans) && !drawTrans) ||
      (zst == znd && !sImageTrans && drawTrans))
    znd = zst - 1;

  // Swap direction if needed
  if (invertZ) {
    izdir = zst;
    zst = znd;
    znd = izdir;
    izdir = -1;
  }
}

// Compute starting and ending draw coordinates from center coordinate, desired
// size and maximum size, shifting center if necessary
static void setCoordLimits(int cur, int maxSize, int drawSize, 
                           int &str, int &end)
{
  str = cur - drawSize / 2;
  str = B3DMAX(1, str);
  end = B3DMIN(str + drawSize, maxSize - 1);
  str = B3DMAX(1, end - drawSize);
}

// Set the alpha factor based on transparency, number of images and which image
// this one is
static void setAlpha(int iz, int zst, int znd, int izdir)
{
  float alpha, b;
  int nDraw = (znd - zst) / izdir + 1;
  int m = (iz - zst) / izdir + 1;

  // b is the final alpha factor for each plane after all drawing
  // alpha is computed from b for the plane # m
  b = (float)(0.01 * (100 - sImageTrans) / nDraw);
  alpha = (float)(b / (1. - (nDraw - m) * b));
  glColor4f(alpha, alpha, alpha, alpha);
  if (sImageTrans || nDraw > 1) {
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,  GL_ONE_MINUS_SRC_ALPHA);
  }
}

#define FILLDATA(a)  uvind = 4 * (fillXsize * v + u); \
  sTdata[uvind] = sCmap[0][a];                                  \
  sTdata[uvind + 1] = sCmap[1][a];                              \
  sTdata[uvind + 2] = sCmap[2][a];                              \
  sTdata[uvind + 3] = 255;

// The call from within the openGL calling routines to draw the image
void imodvDrawImage(ImodvApp *a, int drawTrans)
{
  Ipoint ll, lr, ur, ul, clamp;
  int tstep;
  int cix, ciy, ciz;
  int mix, miy, miz;
  int xstr, xend, ystr, yend, zstr, zend, fillXsize;
  unsigned char **idata;
  b3dUInt16 **usidata;
  unsigned char pix;
  int i, j, ypatch, jend, xpatch, zpatch, iend;
  int u, v, uvind;
  int iz, idir, numSave;
  int cacheSum, curtime;
  float clampEnd;
  unsigned char **imdata;
  b3dUInt16 **usimdata;
  unsigned char *bmap = NULL;
  bool flipped, invertX, invertY, invertZ;
  Imat *mat;
  Ipoint inp, outp;
  QTime drawTime;
  drawTime.start();
  sWallLoad = sWallDraw = 0.;
     
  mix = a->vi->xsize;
  miy = a->vi->ysize;
  miz = a->vi->zsize;
  if (sDia)
    sDia->updateCoords();

  if (!sFlags) 
    return;

  ivwGetLocation(a->vi, &cix, &ciy, &ciz);
  ivwGetTime(a->vi, &curtime);
  if (!sCmapInit || (a->vi->colormapImage && 
                    (ciz != cmapZ || curtime != cmapTime))) {
    makeColorMap();
    cmapTime = curtime;
    cmapZ = ciz;
  }

  if (sXdrawSize < 0) {
    sXdrawSize = mix;
    sYdrawSize = miy;
    sZdrawSize = miz;
  }

  // If doing stereo pairs, draw the pair as long as the step up stays in the
  // same set of images for an area
  if (a->imageStereo && a->stereo < 0) {
    iz = ciz + a->imageDeltaZ;
    if (ciz % a->imagesPerArea < iz % a->imagesPerArea)
      ciz = iz;
  }

  // Get data pointers if doing X or Y planes
  if ((sFlags & (IMODV_DRAW_CX | IMODV_DRAW_CY)) &&
       !(a->imageStereo && a->stereo)) {
    if (ivwSetupFastAccess(a->vi, &imdata, 0, &cacheSum))
      return;
    flipped = (!a->vi->vmSize || a->vi->fullCacheFlipped) && 
      a->vi->li->axis == 2;
    usimdata = (b3dUInt16 **)imdata;
  }
  if (a->vi->ushortStore) {
    bmap = ivwUShortInRangeToByteMap(a->vi);
    if (!bmap)
      return;
  }

  // If doing multiple slices, need to find direction in which to do them
  invertX = invertY = invertZ = false;
  if (a->vi->colormapImage && sNumSlices > 1) {
    sNumSlices = 1;
    if (sDia)
      sDia->mSliders->setValue(IIS_SLICES, sNumSlices);
  }
  numSave = sNumSlices;
  if (a->imageStereo && a->stereo)
    sNumSlices = 1;

  if (sNumSlices > 1) {
    mat = imodMatNew(3);
    imodMatRot(mat, (double)a->imod->view->rot.z, b3dZ);
    imodMatRot(mat, (double)a->imod->view->rot.y, b3dY);
    imodMatRot(mat, (double)a->imod->view->rot.x, b3dX);
    
    inp.x = 1.;
    inp.y = inp.z = 0.0f;
    imodMatTransform(mat, &inp, &outp);
    invertX = outp.z < 0.;
    inp.x = 0.;
    inp.y = 1.;
    imodMatTransform(mat, &inp, &outp);
    invertY = outp.z < 0.;
    inp.y = 0.;
    inp.z = 1.;
    imodMatTransform(mat, &inp, &outp);
    invertZ = outp.z < 0.;
    imodMatDelete(mat);
  }

  if (!sTexImageSize && initTexMapping())
    return;

  tstep = sTexImageSize - 2;
  clampEnd = (tstep + 1.) / (tstep + 2.);

  // This used to be 1 with RGB data being passed in
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);
  glEnable(GL_TEXTURE_2D);
 
 /* Draw Current Z image. */
  if (sFlags & IMODV_DRAW_CZ) {

    setSliceLimits(ciz, miz, invertZ, drawTrans, zstr, zend, idir);
    setCoordLimits(cix, mix, sXdrawSize, xstr, xend);
    setCoordLimits(ciy, miy, sYdrawSize, ystr, yend);
    for (iz = zstr; idir * (zend - iz) >= 0 ; iz += idir) {
      setAlpha(iz, zstr, zend, idir);
      ll.z = lr.z = ur.z = ul.z = iz;
      idata = ivwGetZSection(a->vi, iz);
      if (!idata)
        continue;
      usidata = (b3dUInt16 **)idata;

      // Loop on patches in Y, get limits to fill and set corners
      for (ypatch = ystr; ypatch < yend; ypatch += tstep){
        clamp.y = clampEnd;
        jend = ypatch + tstep;
        if (jend > yend) {
          jend = yend;
          clamp.y = (yend + 1. - ypatch) / (tstep + 2.);
        }
        lr.y = ll.y = ypatch;
        ul.y = ur.y = jend;
          
        // Loop on patches in X, get limits to fill and set corners
        for (xpatch = xstr; xpatch < xend; xpatch += tstep){
          clamp.x = clampEnd;
          iend = xpatch + tstep;
          if (iend > xend) {
            iend = xend;
            clamp.x = (xend + 1. - xpatch) / (tstep + 2.);
          }
          ul.x = ll.x = xpatch;
          ur.x = lr.x = iend;
          fillXsize = 2 + iend - xpatch;
        
          // Fill the data for one patch then draw the patch
          if (a->vi->ushortStore) {
            for (j = ypatch-1; j < jend+1; j++) {
              v = (j - (ypatch-1));
              for (i = xpatch-1; i < iend+1; i++) {
                u = i - (xpatch-1);
                pix = bmap[usidata[j][i]];
                FILLDATA(pix);
              }
            }
          } else {
            for (j = ypatch-1; j < jend+1; j++) {
              v = (j - (ypatch-1));
              for (i = xpatch-1; i < iend+1; i++) {
                u = i - (xpatch-1);
                pix = idata[j][i];
                FILLDATA(pix);
              }
            }
          }
          
          imodvDrawTImage(&ll, &lr, &ur, &ul, &clamp,
                          (unsigned char *)sTdata, fillXsize, 2 + jend - ypatch);
        }
      }
    }
  }
  glDisable(GL_BLEND);

  /* Draw Current X image. */
  if ((sFlags & IMODV_DRAW_CX) && 
      !(a->stereo && a->imageStereo)) {

    setSliceLimits(cix, mix, invertX, drawTrans, xstr, xend, idir);
    setCoordLimits(ciy, miy, sYdrawSize, ystr, yend);
    setCoordLimits(ciz, miz, sZdrawSize, zstr, zend);

    for (xpatch = xstr; idir * (xend - xpatch) >= 0 ; xpatch += idir) {
      setAlpha(xpatch, xstr, xend, idir);
      ll.x = lr.x = ur.x = ul.x = xpatch;

      for (zpatch = zstr; zpatch < zend; zpatch += tstep){
        clamp.y = clampEnd;
        jend = zpatch + tstep;
        if (jend > zend) {
          jend = zend;
          clamp.y = (zend + 1. - zpatch) / (tstep + 2.);
        }
        lr.z = ll.z = zpatch;
        ul.z = ur.z = jend;
        
        for (ypatch = ystr; ypatch < yend; ypatch += tstep){
          clamp.x = clampEnd;
          iend = ypatch + tstep;
          if (iend > yend) {
            iend = yend;
            clamp.x = (yend + 1. - ypatch) / (tstep + 2.);
          }
          ul.y = ll.y = ypatch;
          ur.y = lr.y = iend;
          fillXsize = 2 + iend - ypatch;
        
          // Handle cases of flipped or not with different loops to put test
          // on presence of data in the outer loop
          if (flipped) {
            for (i = ypatch-1; i < iend+1; i++) {
              u = i - (ypatch-1);
              if (imdata[i]) {
                if (a->vi->ushortStore) {
                  for (j = zpatch-1; j < jend+1; j++) {
                    v = j - (zpatch-1);
                    pix = bmap[usimdata[i][xpatch + (j * mix)]];
                    FILLDATA(pix);
                  }
                } else {
                  for (j = zpatch-1; j < jend+1; j++) {
                    v = j - (zpatch-1);
                    pix = imdata[i][xpatch + (j * mix)];
                    FILLDATA(pix);
                  }
                }
              } else {
                for (j = zpatch-1; j < jend+1; j++) {
                  v = j - (zpatch-1);
                  FILLDATA(0);
                }
              }
            }
          } else {
            for (j = zpatch-1; j < jend+1; j++) {
              v = j - (zpatch-1);
              if (imdata[j]) {
                if (a->vi->ushortStore) {
                  for (i = ypatch-1; i < iend+1; i++) {
                    u = i - (ypatch-1);
                    pix = bmap[usimdata[j][xpatch + (i * mix)]];
                    FILLDATA(pix);
                  }
                } else {
                  for (i = ypatch-1; i < iend+1; i++) {
                    u = i - (ypatch-1);
                    pix = imdata[j][xpatch + (i * mix)];
                    FILLDATA(pix);
                  }
                }
              } else {
                for (i = ypatch-1; i < iend+1; i++) {
                  u = i - (ypatch-1);
                  FILLDATA(0);
                }
              }
            }
          }
          
          imodvDrawTImage(&ll, &lr, &ur, &ul, &clamp,
                          (unsigned char *)sTdata, fillXsize, 2 + jend - zpatch);
        }
      }
    }       
  }
  glDisable(GL_BLEND);

  /* Draw Current Y image. */
  if ((sFlags & IMODV_DRAW_CY) && 
      !(a->stereo && a->imageStereo)) {
    setSliceLimits(ciy, miy, invertY, drawTrans, ystr, yend, idir);
    setCoordLimits(cix, mix, sXdrawSize, xstr, xend);
    setCoordLimits(ciz, miz, sZdrawSize, zstr, zend);

    for (ypatch = ystr; idir * (yend - ypatch) >= 0 ; ypatch += idir) {
      setAlpha(ypatch, ystr, yend, idir);
      ll.y = lr.y = ur.y = ul.y = ypatch;

      for (zpatch = zstr; zpatch < zend; zpatch += tstep) {
        clamp.y = clampEnd;
        jend = zpatch + tstep;
        if (jend > zend) {
          jend = zend;
          clamp.y = (zend + 1. - zpatch) / (tstep + 2.);
        }
        lr.z = ll.z = zpatch;
        ul.z = ur.z = jend;

        for (xpatch = xstr; xpatch < xend; xpatch += tstep) {
          clamp.x = clampEnd;
          iend = xpatch + tstep;
          if (iend > xend) {
            iend = xend;
            clamp.x = (xend + 1. - xpatch) / (tstep + 2.);
          }
          ul.x = ll.x = xpatch;
          ur.x = lr.x = iend;
          fillXsize = 2 + iend - xpatch;
          
          // This one is easier, one outer loop and flipped, non-flipped, or
          // no data cases for inner loop
          for (j = zpatch-1; j < jend+1; j++) {
            v = j - (zpatch-1);
            if (flipped && imdata[ypatch]) {
              if (a->vi->ushortStore) {
                for (i = xpatch-1; i < iend+1; i++) {
                  u = i - (xpatch-1);
                  pix = bmap[usimdata[ypatch][i + (j * mix)]];
                  FILLDATA(pix);
                }
              } else {
                for (i = xpatch-1; i < iend+1; i++) {
                  u = i - (xpatch-1);
                  pix = imdata[ypatch][i + (j * mix)];
                  FILLDATA(pix);
                }
              }
            } else if (!flipped && imdata[j]) {
              if (a->vi->ushortStore) {
                for (i = xpatch-1; i < iend+1; i++) {
                  u = i - (xpatch-1);
                  pix = bmap[usimdata[j][i + (ypatch * mix)]];
                  FILLDATA(pix);
                }
              } else {
                for (i = xpatch-1; i < iend+1; i++) {
                  u = i - (xpatch-1);
                  pix = imdata[j][i + (ypatch * mix)];
                  FILLDATA(pix);
                }
              }
            } else {
              for (i = xpatch-1; i < iend+1; i++) {
                u = i - (xpatch-1);
                FILLDATA(0);
              }
            }

          }
          imodvDrawTImage(&ll, &lr, &ur, &ul, &clamp,
                          (unsigned char *)sTdata, fillXsize, 2 + jend - zpatch);
        }
      }
    }
  }

  B3DFREE(bmap);
  glDisable(GL_BLEND);
  //glFlush();
  glDisable(GL_TEXTURE_2D);
  sNumSlices = numSave;
  if (imodDebug('v'))
    imodPrintStderr("Draw time %d  load %.2f draw %.2f\n", drawTime.elapsed(),
                    sWallLoad * 1000., sWallDraw * 1000.);
}

// Load the tile into the texture image and draw it within given coordinates
static void imodvDrawTImage(Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *p4,
                            Ipoint *clamp,
                            unsigned char *data,
                            int width, int height)
{
  float xclamp, yclamp, clamp0 = 1. / sTexImageSize;
  double wallStart = wallTime();

  xclamp = clamp->x;
  yclamp = clamp->y;

  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, width, height, GL_BGRA, GL_UNSIGNED_BYTE, data);
  /*GLenum error = glGetError();
    if (error)
    imodPrintStderr("GL error on glTexSubImage2D: %s\n", gluErrorString(error)); */
  sWallLoad += wallTime() - wallStart;
  wallStart = wallTime();

  glBegin(GL_QUADS);
  glTexCoord2f(clamp0, clamp0); glVertex3fv((GLfloat *)p1);
  glTexCoord2f(xclamp, clamp0); glVertex3fv((GLfloat *)p2);
  glTexCoord2f(xclamp, yclamp); glVertex3fv((GLfloat *)p3);
  glTexCoord2f(clamp0, yclamp); glVertex3fv((GLfloat *)p4);


  glEnd();
  glFlush();
  sWallDraw += wallTime() - wallStart;
}

/* 
 * Initialize texture mapping - get a texture and set its properties
 */
static int initTexMapping()
{
  int tstep;
  GLint texwid;

  // Start with assessing the texture size that works
  // Use power of 2 if required, otherwise a bit above to minimize tiles for 
  // common image sizes (less important with subarea loading)
  tstep = (Imodv->glExtFlags & B3DGLEXT_ANY_SIZE_TEX) ? 528 : 512;
  for (; tstep >= 64; tstep /= 2) {
    glTexImage2D(GL_PROXY_TEXTURE_2D, 0, 4, tstep, tstep, 0, GL_BGRA, GL_UNSIGNED_BYTE,
                 NULL);
    glGetTexLevelParameteriv(GL_PROXY_TEXTURE_2D, 0, GL_TEXTURE_WIDTH, &texwid);
    if (texwid > 0)
      break;

    // Do a final try with exactly 64 if the last one was not
    if (tstep > 64 && tstep / 2 < 64)
      tstep = 64;
  }
  if (texwid <= 0) {
    imodPrintStderr("Cannot get texture array for image display");
    endTexMapping();
    return 1;
  }

  // Allocate a big enough array
  sTdata = B3DMALLOC(GLubyte, 4 * tstep * tstep);
  if (!sTdata) {
    imodPrintStderr("Failed to allocate array for image display");
    endTexMapping();
    return 1;
  }

  // Get the texture for real now
  sTexImageSize = tstep;
  glGenTextures(1, &sTexName);
  glBindTexture(GL_TEXTURE_2D, sTexName);
  glTexImage2D(GL_TEXTURE_2D, 0, 4, tstep, tstep, 0, GL_BGRA, GL_UNSIGNED_BYTE, sTdata);

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
  return 0;
}

// Take care of all flags associated with texture mapping being on
static void endTexMapping()
{
  sFlags = 0;
  Imodv->texMap = 0;
  imodvImageCleanup();
}

// Free up image and texture arrays
void imodvImageCleanup()
{
  B3DFREE(sTdata);
  if (sTexImageSize)
    glDeleteTextures(1, &sTexName);
  sTexImageSize = 0;
  sTdata = NULL;
}

// THE ImodvImage CLASS IMPLEMENTATION

static const char *buttonLabels[] = {"Done", "Help"};
static const char *buttonTips[] = {"Close dialog box", "Open help window"};
static const char *sliderLabels[] = {"X", "Y", "Z", "X size", "Y size", "Z size",
                               "# of slices", "Transparency", 
                               "Black Level", "White Level"};

ImodvImage::ImodvImage(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, 1, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), "3dmodv Image View", "", name)
{
  mCtrlPressed = false;

  // Make view checkboxes
  mViewXBox = diaCheckBox("View X image", this, mLayout);
  mViewXBox->setChecked(sFlags & IMODV_DRAW_CX);
  connect(mViewXBox, SIGNAL(toggled(bool)), this, SLOT(viewXToggled(bool)));
  mViewYBox = diaCheckBox("View Y image", this, mLayout);
  mViewYBox->setChecked(sFlags & IMODV_DRAW_CY);
  connect(mViewYBox, SIGNAL(toggled(bool)), this, SLOT(viewYToggled(bool)));
  mViewZBox = diaCheckBox("View Z image", this, mLayout);
  mViewZBox->setChecked(sFlags & IMODV_DRAW_CZ);
  connect(mViewZBox, SIGNAL(toggled(bool)), this, SLOT(viewZToggled(bool)));
  mViewXBox->setToolTip("Display YZ plane at current X");
  mViewYBox->setToolTip("Display XZ plane at current Y");
  mViewZBox->setToolTip("Display XY plane at current Z");

  // Make multisliders
  mSliders = new MultiSlider(this, 10, sliderLabels);

  mSliders->setRange(IIS_X_COORD, 1, Imodv->vi->xsize);
  mSliders->setRange(IIS_X_SIZE, 1, Imodv->vi->xsize);
  if (sLastYsize < 0) {
    sXdrawSize = Imodv->vi->xsize;
    sYdrawSize = Imodv->vi->ysize;
    sZdrawSize = Imodv->vi->zsize;
    sLastYsize = Imodv->vi->ysize;
  }
  updateCoords();
  mSliders->setValue(IIS_X_SIZE, sXdrawSize);
  mSliders->setValue(IIS_Y_SIZE, sYdrawSize);
  mSliders->setValue(IIS_Z_SIZE, sZdrawSize);
  mSliders->setRange(IIS_SLICES, 1, 64);
  mSliders->setValue(IIS_SLICES, sNumSlices);
  mSliders->setRange(IIS_TRANSPARENCY, 0, 100);
  mSliders->setValue(IIS_TRANSPARENCY, sImageTrans);
  mSliders->setValue(IIS_BLACK, sBlackLevel);
  mSliders->setValue(IIS_WHITE, sWhiteLevel);
  mLayout->addLayout(mSliders->getLayout());
  (mSliders->getLayout())->setSpacing(4);
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderMoved(int, int, bool)));
  mSliders->getSlider(IIS_X_COORD)->setToolTip("Set current image X coordinate");
  mSliders->getSlider(IIS_Y_COORD)->setToolTip("Set current image Y coordinate");
  mSliders->getSlider(IIS_Z_COORD)->setToolTip("Set current image Z coordinate");
  mSliders->getSlider(IIS_X_SIZE)->setToolTip("Set image size to display in X");
  mSliders->getSlider(IIS_Y_SIZE)->setToolTip("Set image size to display in Y");
  mSliders->getSlider(IIS_Z_SIZE)->setToolTip("Set image size to display in Z");
  mSliders->getSlider(IIS_SLICES)->setToolTip("Set number of slices to display");
  mSliders->getSlider(IIS_TRANSPARENCY)->setToolTip("Set percent transparency");
  mSliders->getSlider(IIS_BLACK)->setToolTip("Set minimum black level of contrast ramp");
  mSliders->getSlider(IIS_WHITE)->setToolTip("Set maximum white level of contrast ramp");

  QPushButton *copyBut = diaPushButton("Use 3dmod Black/White", this, mLayout);
  copyBut->setToolTip("Set black and white levels from sliders in 3dmod Info window");
  connect(copyBut, SIGNAL(clicked()), this, SLOT(copyBWclicked()));

  // Make false color checkbox
  mFalseBox = diaCheckBox("False color", this, mLayout);
  mFalseBox->setChecked(sFalsecolor);
  connect(mFalseBox, SIGNAL(toggled(bool)), this, SLOT(falseToggled(bool)));
  mFalseBox->setToolTip("Display image in false color");

  if (Imodv->vi->colormapImage) {
    mFalseBox->setEnabled(false);
    copyBut->setEnabled(false);
    mSliders->getSlider(IIS_SLICES)->setEnabled(false);
    mSliders->getSlider(IIS_BLACK)->setEnabled(false);
    mSliders->getSlider(IIS_WHITE)->setEnabled(false);
  }

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
}

// Update the current coordinate sliders and their ranges, update the ranges
// of the Y and Z size sliders and swap y and Z size if flipped
void ImodvImage::updateCoords()
{
  int maxSlices = Imodv->vi->zsize < MAX_SLICES ? 
    Imodv->vi->zsize : MAX_SLICES;
  mSliders->setValue(IIS_X_COORD, (int)(Imodv->vi->xmouse + 1.5f));
  mSliders->setRange(IIS_Y_COORD, 1, Imodv->vi->ysize);
  mSliders->setValue(IIS_Y_COORD, (int)(Imodv->vi->ymouse + 1.5f));
  mSliders->setRange(IIS_Z_COORD, 1, Imodv->vi->zsize);
  mSliders->setValue(IIS_Z_COORD, (int)(Imodv->vi->zmouse + 1.5f));
  mSliders->setRange(IIS_Y_SIZE, 1, Imodv->vi->ysize);
  mSliders->setRange(IIS_Z_SIZE, 1, Imodv->vi->zsize);
  mSliders->setRange(IIS_SLICES, 1, maxSlices);
  
 if (sLastYsize != Imodv->vi->ysize) {
    int tmpSize = sYdrawSize;
    sYdrawSize = sZdrawSize;
    sZdrawSize = tmpSize;
    mSliders->setValue(IIS_Y_SIZE, sYdrawSize);
    mSliders->setValue(IIS_Z_SIZE, sZdrawSize);
    sLastYsize = Imodv->vi->ysize;
    if (sNumSlices > maxSlices) {
      sNumSlices = maxSlices;
      mSliders->setValue(IIS_SLICES, sNumSlices);
    }
  }
}

// Viewing image is turned on or off
void ImodvImage::viewToggled(bool state, int flag)
{
  if (!state) {
    sFlags &= ~flag;
    if (!sFlags)
      Imodv->texMap = 0;
  } else {
    sFlags |= flag;
    Imodv->texMap = 1;
  }
  imodvStereoUpdate();
  imodvDraw(Imodv);
}

// respond to a change of transparency or contrast
void ImodvImage::sliderMoved(int which, int value, bool dragging)
{
  switch (which) {
  case IIS_X_COORD:
    Imodv->vi->xmouse = value - 1;
    ivwBindMouse(Imodv->vi);
    break;
  case IIS_Y_COORD:
    Imodv->vi->ymouse = value - 1;
    ivwBindMouse(Imodv->vi);
    break;
  case IIS_Z_COORD:
    Imodv->vi->zmouse = value - 1;
    ivwBindMouse(Imodv->vi);
    break;

  case IIS_X_SIZE:
    sXdrawSize = value;
    break;
  case IIS_Y_SIZE:
    sYdrawSize = value;
    break;
  case IIS_Z_SIZE:
    sZdrawSize = value;
    break;

  case IIS_SLICES: 
    sNumSlices = value;
    break;

  case IIS_TRANSPARENCY: 
    sImageTrans = value;
    Imodv->texTrans = sImageTrans;
    break;

  case IIS_BLACK:
    sBlackLevel = value;
    makeColorMap();
    break;
  case IIS_WHITE:
    sWhiteLevel = value;
    makeColorMap();
    break;
  }

  // draw if slider clicked or is in hot state
  if (!dragging || ImodPrefs->hotSliderActive(mCtrlPressed)) {
    if (which > IIS_Z_COORD)
      imodvDraw(Imodv);
    else
      imodDraw(Imodv->vi, IMOD_DRAW_XYZ);
  }
}

// Set the black/white levels from 3dmod
void ImodvImage::copyBWclicked()
{
  sBlackLevel = App->cvi->blackInRange;
  sWhiteLevel = App->cvi->whiteInRange;
  mSliders->setValue(IIS_BLACK, sBlackLevel);
  mSliders->setValue(IIS_WHITE, sWhiteLevel);
  makeColorMap();
  imodvDraw(Imodv);
}

// User toggles false color
void ImodvImage::falseToggled(bool state)
{
  sFalsecolor = state ? 1 : 0;
  makeColorMap();
  imodvDraw(Imodv);
}

// Action buttons
void ImodvImage::buttonPressed(int which)
{
  if (which)
    imodShowHelpPage("modvImage.html#TOP");
  else
    close();
}
  
void ImodvImage::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::fontChange(oldFont);
}

// Accept a close event and set dia to null
void ImodvImage::closeEvent ( QCloseEvent * e )
{
  imodvDialogManager.remove((QWidget *)sDia);
  sDia = NULL;
  e->accept();
}

// Close on escape; watch for the hot slider key; pass on keypress
void ImodvImage::keyPressEvent ( QKeyEvent * e )
{
  if (utilCloseKey(e))
    close();
  else {
    if (hotSliderFlag() != NO_HOT_SLIDER && e->key() == hotSliderKey()) {
      mCtrlPressed = true;
      grabKeyboard();
    }
    imodvKeyPress(e);
  }
}

// pass on key release; watch for hot slider release
void ImodvImage::keyReleaseEvent ( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    mCtrlPressed = false;
    releaseKeyboard();
  }
  imodvKeyRelease(e);
}
