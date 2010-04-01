/*
 *  imodv_image.c -- View image slice in model view control dialog.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2006 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <qcheckbox.h>
#include <qlayout.h>
#include <qgl.h>
#include <qslider.h>
#include <qtooltip.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QCloseEvent>
#include "preferences.h"
#include "multislider.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "imodv_gfx.h"
#include "imodv_image.h"
#include "imodv_input.h"
#include "imod_display.h"
#include "control.h"
#include "preferences.h"
#include "xcramp.h"


#define TexImageSize 64

enum {IIS_X_COORD = 0, IIS_Y_COORD, IIS_Z_COORD, IIS_X_SIZE, IIS_Y_SIZE,
      IIS_Z_SIZE, IIS_SLICES, IIS_TRANSPARENCY, IIS_BLACK, IIS_WHITE};

static void mkcmap(void);
static void imodvDrawTImage(Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *p4,
                            Ipoint *clamp, unsigned char *data,
                            int width, int height);
static void setAlpha(int iz, int zst, int znd, int izdir);
static void setSliceLimits(int ciz, int miz, bool invertZ, int drawTrans, 
                           int &zst, int &znd, int &izdir);
static void setCoordLimits(int cur, int maxSize, int drawSize, 
                           int &str, int &end);

static GLubyte tdata[TexImageSize][TexImageSize][3];
static GLubyte Cmap[3][256];
static int BlackLevel = 0;
static int WhiteLevel = 255;
static int Falsecolor = 0;
static int ImageTrans = 0;
static int cmapInit = 0;
static int cmapZ = 0;
static int cmapTime = 0;
static int numSlices = 1;
static int xDrawSize = -1;
static int yDrawSize = -1;
static int zDrawSize = -1;
static int lastYsize = -1;
static int lastZsize = -1;

#define MAX_SLICES 256

struct imodvImageDataStruct {
  ImodvImage *dia;
  ImodvApp  *a;

  int    flags;
  
  /* DNM 12/30/02: unused currently */
  /* int    xsize, ysize, zsize;
     int    *xd, *yd, *zd; */

}imodvImageData = {0, 0, 0};

// Open, close, or raise the dialog box
void imodvImageEditDialog(ImodvApp *a, int state)
{
  if (!state){
    if (imodvImageData.dia)
      imodvImageData.dia->close();
    return;
  }
  if (imodvImageData.dia) {
    imodvImageData.dia->raise();
    return;
  }

  imodvImageData.dia = new ImodvImage(imodvDialogManager.parent(IMODV_DIALOG),
                                      "image view");
  imodvImageData.a = a;

  mkcmap();
  imodvDialogManager.add((QWidget *)imodvImageData.dia, IMODV_DIALOG);
  adjustGeometryAndShow((QWidget *)imodvImageData.dia, IMODV_DIALOG);
}

// Update the dialog box (just the view flag for now)
void imodvImageUpdate(ImodvApp *a)
{
  if (a->texMap && !imodvImageData.flags) {
    imodvImageData.flags |= IMODV_DRAW_CZ;
    if (imodvImageData.dia)
      diaSetChecked(imodvImageData.dia->mViewZBox, true);
  } else if (!a->texMap && imodvImageData.flags) {
    imodvImageData.flags = 0;
    if (imodvImageData.dia) {
      diaSetChecked(imodvImageData.dia->mViewXBox, false);
      diaSetChecked(imodvImageData.dia->mViewYBox, false);
      diaSetChecked(imodvImageData.dia->mViewZBox, false);
    }
  }
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
  numSlices = slices;
  if (trans < 0)
    trans = 0;
  if (trans > 100)
    trans = 100;
  ImageTrans = trans;
  if (imodvImageData.dia) {
    imodvImageData.dia->mSliders->setValue(IIS_SLICES, numSlices);
    imodvImageData.dia->mSliders->setValue(IIS_TRANSPARENCY, ImageTrans);
  }
}

// Return the number of slices and transparancy
int imodvImageGetThickness(void)
{
  return numSlices;
}
int imodvImageGetTransparency(void)
{
  return ImageTrans;
}

/****************************************************************************/
/* TEXTURE MAP IMAGE. */
/****************************************************************************/

// Make a color map
static void mkcmap(void)
{
  int rampsize, cmapReverse = 0;
  float slope, point;
  int r,g,b,i;
  ImodvApp *a = Imodv;
  int white = a->vi->colormapImage ? 255 : WhiteLevel;
  int black = a->vi->colormapImage ? 0 : BlackLevel;

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
    Cmap[0][i] = 0;
  for (i = white; i < 256; i++)
    Cmap[0][i] = 255;
  slope = 256.0 / (float)rampsize;
  for (i = black; i < white; i++){
    point = (float)(i - black) * slope;
    Cmap[0][i] = (unsigned char)point;
  }
     
  if (cmapReverse && !a->vi->colormapImage){
    for(i = 0; i < 256; i++)
      Cmap[0][i] = 255 - Cmap[0][i];
  }
  if (Falsecolor || a->vi->colormapImage){
    for(i = 0; i < 256; i++){
      xcramp_mapfalsecolor(Cmap[0][i], &r, &g, &b);
      Cmap[0][i] = (unsigned char)r;
      Cmap[1][i] = (unsigned char)g;
      Cmap[2][i] = (unsigned char)b;
    }
  }else{
    for(i = 0; i < 256; i++){
      Cmap[1][i] = Cmap[2][i] = Cmap[0][i];
    }
  }

  cmapInit = 1;
}

static void imodvDrawTImage(Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *p4,
                            Ipoint *clamp,
                            unsigned char *data,
                            int width, int height)
{
  float xclamp, yclamp;

  // 5/16/04: swap here instead of before calling
  xclamp = clamp->y;
  yclamp = clamp->x;

  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D(GL_TEXTURE_2D, 0, 3, width, height, 
	       0, GL_RGB, GL_UNSIGNED_BYTE,
	       data);

  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER,
		  GL_NEAREST);
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
		  GL_NEAREST);
  
  glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
         
  glEnable(GL_TEXTURE_2D);

  glBegin(GL_QUADS);
  glTexCoord2f(0.0, 0.0); glVertex3fv((GLfloat *)p1);
  glTexCoord2f(0.0, yclamp); glVertex3fv((GLfloat *)p2);
  glTexCoord2f(xclamp, yclamp); glVertex3fv((GLfloat *)p3);
  glTexCoord2f(xclamp, 0.0); glVertex3fv((GLfloat *)p4);


  glEnd();

  glFlush();
  glDisable(GL_TEXTURE_2D);
}

// Determine starting and ending slice and direction, and set the alpha
static void setSliceLimits(int ciz, int miz, bool invertZ, int drawTrans, 
                           int &zst, int &znd, int &izdir)
{
  zst = ciz - numSlices / 2;
  znd = zst + numSlices - 1;
  if (zst < 0)
    zst = 0;
  if (znd >= miz)
    znd = miz - 1;
  izdir = 1;

  // If transparency is needed and it is time to draw solid, or no transparency
  // is needed and it time to draw transparent, set up for no draw
  if (((znd > zst || ImageTrans) && !drawTrans) ||
      (zst == znd && !ImageTrans && drawTrans))
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
  if (str < 0)
    str = 0;
  end = str + drawSize;
  if (end > maxSize) {
    end = maxSize;
    str = end - drawSize;
  }
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
  b = (float)(0.01 * (100 - ImageTrans) / nDraw);
  alpha = (float)(b / (1. - (nDraw - m) * b));
  glColor4f(alpha, alpha, alpha, alpha);
  if (ImageTrans || nDraw > 1) {
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,  GL_ONE_MINUS_SRC_ALPHA);
  }
}

// The call from within the openGL calling routines to draw the image
void imodvDrawImage(ImodvApp *a, int drawTrans)
{
  Ipoint ll, lr, ur, ul, clamp;
  int tstep = TexImageSize;
  int cix, ciy, ciz;
  int mix, miy, miz;
  int xstr, xend, ystr, yend, zstr, zend;
  unsigned char **idata;
  unsigned char pix;
  int i, mi, j, mj;
  int u, v;
  int ix, iy, iz, idir;
  int cacheSum, curtime;
  unsigned char **imdata;
  bool flipped, invertX, invertY, invertZ;
  Imat *mat;
  Ipoint inp, outp;
     
  mix = a->vi->xsize;
  miy = a->vi->ysize;
  miz = a->vi->zsize;
  if (imodvImageData.dia)
    imodvImageData.dia->updateCoords();

  if (!imodvImageData.flags) 
    return;

  ivwGetLocation(a->vi, &cix, &ciy, &ciz);
  ivwGetTime(a->vi, &curtime);
  if (!cmapInit || (a->vi->colormapImage && 
                    (ciz != cmapZ || curtime != cmapTime))) {
    mkcmap();
    cmapTime = curtime;
    cmapZ = ciz;
  }

  if (xDrawSize < 0) {
    xDrawSize = mix;
    yDrawSize = miy;
    zDrawSize = miz;
  }


  // Get data pointers if doing X or Y planes
  if (imodvImageData.flags & (IMODV_DRAW_CX | IMODV_DRAW_CY)) {
    if (ivwSetupFastAccess(a->vi, &imdata, 0, &cacheSum))
      return;
    flipped = (!a->vi->vmSize || a->vi->fullCacheFlipped) && 
      a->vi->li->axis == 2;
  }

  // If doing multiple slices, need to find direction in which to do them
  invertX = invertY = invertZ = false;
  if (a->vi->colormapImage && numSlices > 1) {
    numSlices = 1;
    if (imodvImageData.dia)
      imodvImageData.dia->mSliders->setValue(IIS_SLICES, numSlices);
  }

  if (numSlices > 1) {
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

  /* Draw Current Z image. */
  if (imodvImageData.flags & IMODV_DRAW_CZ) {

    setSliceLimits(ciz, miz, invertZ, drawTrans, zstr, zend, idir);
    setCoordLimits(cix, mix, xDrawSize, xstr, xend);
    setCoordLimits(ciy, miy, yDrawSize, ystr, yend);
    for (iz = zstr; idir * (zend - iz) >= 0 ; iz += idir) {
      setAlpha(iz, zstr, zend, idir);
      ll.z = lr.z = ur.z = ul.z = iz;
      idata = ivwGetZSection(a->vi, iz);
      if (!idata)
        continue;

      // Loop on patches in X, get limits to fill and set corners
      for (ix = xstr; ix < xend; ix += tstep){
        clamp.x = 1.0f;
        mi = ix + tstep;
        if (mi > xend) {
          mi = xend;
          clamp.x = (xend-ix) / (float)tstep;
        }
        ul.x = ll.x = ix;
        ur.x = lr.x = mi;
        
        // Loop on patches in Y, get limits to fill and set corners
        for (iy = ystr; iy < yend; iy += tstep){
          clamp.y = 1.0f;
          mj = iy + tstep;
          if (mj > yend) {
            mj = yend;
            clamp.y = (yend - iy) / (float)tstep;
          }
          lr.y = ll.y = iy;
          ul.y = ur.y = mj;
          
          // Fill the data for one patch then draw the patch
          for (i = ix; i < mi; i++){
            u = i - ix;
            for (j = iy; j < mj; j++){
              v = (j - iy);
              pix = idata[j][i];
              
              tdata[u][v][0] = Cmap[0][pix];
              tdata[u][v][1] = Cmap[1][pix];
              tdata[u][v][2] = Cmap[2][pix];
            }
          }
          
          imodvDrawTImage(&ll, &lr, &ur, &ul, &clamp,
                          (unsigned char *)tdata, tstep, tstep);
        }
      }
    }
  }
  glDisable(GL_BLEND);

  /* Draw Current X image. */
  if (imodvImageData.flags & IMODV_DRAW_CX) {

    setSliceLimits(cix, mix, invertX, drawTrans, xstr, xend, idir);
    setCoordLimits(ciy, miy, yDrawSize, ystr, yend);
    setCoordLimits(ciz, miz, zDrawSize, zstr, zend);

    for (ix = xstr; idir * (xend - ix) >= 0 ; ix += idir) {
      setAlpha(ix, xstr, xend, idir);
      ll.x = lr.x = ur.x = ul.x = ix;

      for (iy = ystr; iy < yend; iy += tstep){
        clamp.x = 1.0f;
        mi = iy + tstep;
        if (mi > yend) {
          mi = yend;
          clamp.x = (yend - iy) / (float)tstep;
        }
        ul.y = ll.y = iy;
        ur.y = lr.y = mi;
        
        for (iz = zstr; iz < zend; iz += tstep){
          clamp.y = 1.0f;
          mj = iz + tstep;
          if (mj > zend) {
            mj = zend;
            clamp.y = (zend - iz) / (float)tstep;
          }
          lr.z = ll.z = iz;
          ul.z = ur.z = mj;
          
          // Handle cases of flipped or not with different loops to put test
          // on presence of data in the outer loop
          if (flipped) {
            for (i = iy; i < mi; i++) {
              u = i - iy;
              if (imdata[i]) {
                for (j = iz; j < mj; j++) {
                  v = (j - iz);
                  pix = imdata[i][ix + (j * mix)];
                  tdata[u][v][0] = Cmap[0][pix];
                  tdata[u][v][1] = Cmap[1][pix];
                  tdata[u][v][2] = Cmap[2][pix];
                }
              } else {
                for (j = iz; j < mj; j++) {
                  v = (j - iz);
                  tdata[u][v][0] = Cmap[0][0];
                  tdata[u][v][1] = Cmap[1][0];
                  tdata[u][v][2] = Cmap[2][0];
                }
              }
            }
          } else {
            for (j = iz; j < mj; j++) {
              v = (j - iz);
              if (imdata[j]) {
                for (i = iy; i < mi; i++) {
                  u = i - iy;
                  pix = imdata[j][ix + (i * mix)];
                  tdata[u][v][0] = Cmap[0][pix];
                  tdata[u][v][1] = Cmap[1][pix];
                  tdata[u][v][2] = Cmap[2][pix];
                }
              } else {
                for (i = iy; i < mi; i++) {
                  u = i - iy;
                  tdata[u][v][0] = Cmap[0][0];
                  tdata[u][v][1] = Cmap[1][0];
                  tdata[u][v][2] = Cmap[2][0];
                }
              }
            }
          }
          
          imodvDrawTImage(&ll, &lr, &ur, &ul, &clamp,
                          (unsigned char *)tdata, tstep, tstep);
        }
      }
    }       
  }
  glDisable(GL_BLEND);

  /* Draw Current Y image. */
  if (imodvImageData.flags & IMODV_DRAW_CY) {
    setSliceLimits(ciy, miy, invertY, drawTrans, ystr, yend, idir);
    setCoordLimits(cix, mix, xDrawSize, xstr, xend);
    setCoordLimits(ciz, miz, zDrawSize, zstr, zend);

    for (iy = ystr; idir * (yend - iy) >= 0 ; iy += idir) {
      setAlpha(iy, ystr, yend, idir);
      ll.y = lr.y = ur.y = ul.y = iy;

      for (ix = xstr; ix < xend; ix += tstep) {
        clamp.x = 1.0f;
        mi = ix + tstep;
        if (mi > xend) {
          mi = xend;
          clamp.x = (xend - ix) / (float)tstep;
        }
        ul.x = ll.x = ix;
        ur.x = lr.x = mi;
        
        for (iz = zstr; iz < zend; iz += tstep) {
          clamp.y = 1.0f;
          mj = iz + tstep;
          if (mj > zend) {
            mj = zend;
            clamp.y = (zend - iz) / (float)tstep;
          }
          lr.z = ll.z = iz;
          ul.z = ur.z = mj;

          // This one is easier, one outer loop and flipped, non-flipped, or
          // no data cases for inner loop
          for (j = iz; j < mj; j++) {
            v = (j - iz);
            if (flipped && imdata[iy]) {
              for (i = ix; i < mi; i++) {
                u = i - ix;
                pix = imdata[iy][i + (j * mix)];
                tdata[u][v][0] = Cmap[0][pix];
                tdata[u][v][1] = Cmap[1][pix];
                tdata[u][v][2] = Cmap[2][pix];
              }
            } else if (!flipped && imdata[j]) {
              for (i = ix; i < mi; i++) {
                u = i - ix;
                pix = imdata[j][i + (iy * mix)];
                tdata[u][v][0] = Cmap[0][pix];
                tdata[u][v][1] = Cmap[1][pix];
                tdata[u][v][2] = Cmap[2][pix];
              }
            } else {
              for (i = ix; i < mi; i++) {
                u = i - ix;
                tdata[u][v][0] = Cmap[0][0];
                tdata[u][v][1] = Cmap[1][0];
                tdata[u][v][2] = Cmap[2][0];
              }
            }

          }
          imodvDrawTImage(&ll, &lr, &ur, &ul, &clamp,
                          (unsigned char *)tdata, tstep, tstep);
        }
      }
    }
  }

  glDisable(GL_BLEND);
}


// THE ImodvImage CLASS IMPLEMENTATION

static char *buttonLabels[] = {"Done", "Help"};
static char *buttonTips[] = {"Close dialog box", "Open help window"};
static char *sliderLabels[] = {"X", "Y", "Z", "X size", "Y size", "Z size",
                               "# of slices", "Transparency", 
                               "Black Level", "White Level"};

ImodvImage::ImodvImage(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, 1, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), "3dmodv Image View", "", name)
{
  mCtrlPressed = false;

  // Make view checkboxes
  mViewXBox = diaCheckBox("View X image", this, mLayout);
  mViewXBox->setChecked(imodvImageData.flags & IMODV_DRAW_CX);
  connect(mViewXBox, SIGNAL(toggled(bool)), this, SLOT(viewXToggled(bool)));
  mViewYBox = diaCheckBox("View Y image", this, mLayout);
  mViewYBox->setChecked(imodvImageData.flags & IMODV_DRAW_CY);
  connect(mViewYBox, SIGNAL(toggled(bool)), this, SLOT(viewYToggled(bool)));
  mViewZBox = diaCheckBox("View Z image", this, mLayout);
  mViewZBox->setChecked(imodvImageData.flags & IMODV_DRAW_CZ);
  connect(mViewZBox, SIGNAL(toggled(bool)), this, SLOT(viewZToggled(bool)));
  mViewXBox->setToolTip("Display YZ plane at current X");
  mViewYBox->setToolTip("Display XZ plane at current Y");
  mViewZBox->setToolTip("Display XY plane at current Z");

  // Make multisliders
  mSliders = new MultiSlider(this, 10, sliderLabels);

  mSliders->setRange(IIS_X_COORD, 1, Imodv->vi->xsize);
  mSliders->setRange(IIS_X_SIZE, 1, Imodv->vi->xsize);
  if (lastYsize < 0) {
    xDrawSize = Imodv->vi->xsize;
    yDrawSize = Imodv->vi->ysize;
    zDrawSize = Imodv->vi->zsize;
    lastYsize = Imodv->vi->ysize;
  }
  updateCoords();
  mSliders->setValue(IIS_X_SIZE, xDrawSize);
  mSliders->setValue(IIS_Y_SIZE, yDrawSize);
  mSliders->setValue(IIS_Z_SIZE, zDrawSize);
  mSliders->setRange(IIS_SLICES, 1, 64);
  mSliders->setValue(IIS_SLICES, numSlices);
  mSliders->setRange(IIS_TRANSPARENCY, 0, 100);
  mSliders->setValue(IIS_TRANSPARENCY, ImageTrans);
  mSliders->setValue(IIS_BLACK, BlackLevel);
  mSliders->setValue(IIS_WHITE, WhiteLevel);
  mLayout->addLayout(mSliders->getLayout());
  (mSliders->getLayout())->setSpacing(4);
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderMoved(int, int, bool)));
  mSliders->getSlider(IIS_X_COORD)->setToolTip(
                "Set current image X coordinate");
  mSliders->getSlider(IIS_Y_COORD)->setToolTip(
                "Set current image Y coordinate");
  mSliders->getSlider(IIS_Z_COORD)->setToolTip(
                "Set current image Z coordinate");
  mSliders->getSlider(IIS_X_SIZE)->setToolTip(
                "Set image size to display in X");
  mSliders->getSlider(IIS_Y_SIZE)->setToolTip(
                "Set image size to display in Y");
  mSliders->getSlider(IIS_Z_SIZE)->setToolTip(
                "Set image size to display in Z");
  mSliders->getSlider(IIS_SLICES)->setToolTip(
                "Set number of slices to display");
  mSliders->getSlider(IIS_TRANSPARENCY)->setToolTip(
                "Set percent transparency");
  mSliders->getSlider(IIS_BLACK)->setToolTip(
                "Set minimum black level of contrast ramp");
  mSliders->getSlider(IIS_WHITE)->setToolTip(
                "Set maximum white level of contrast ramp");

  // Make false color checkbox
  mFalseBox = diaCheckBox("False color", this, mLayout);
  mFalseBox->setChecked(Falsecolor);
  connect(mFalseBox, SIGNAL(toggled(bool)), this, SLOT(falseToggled(bool)));
  mFalseBox->setToolTip("Display image in false color");

  if (Imodv->vi->colormapImage) {
    mFalseBox->setEnabled(false);
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
  
 if (lastYsize != Imodv->vi->ysize) {
    int tmpSize = yDrawSize;
    yDrawSize = zDrawSize;
    zDrawSize = tmpSize;
    mSliders->setValue(IIS_Y_SIZE, yDrawSize);
    mSliders->setValue(IIS_Z_SIZE, zDrawSize);
    lastYsize = Imodv->vi->ysize;
    if (numSlices > maxSlices) {
      numSlices = maxSlices;
      mSliders->setValue(IIS_SLICES, numSlices);
    }
  }
}

// Viewing image is turned on or off
void ImodvImage::viewToggled(bool state, int flag)
{
  if (!state) {
    imodvImageData.flags &= ~flag;
    if (!imodvImageData.flags)
      Imodv->texMap = 0;
  } else {
    imodvImageData.flags |= flag;
    Imodv->texMap = 1;
  }
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
    xDrawSize = value;
    break;
  case IIS_Y_SIZE:
    yDrawSize = value;
    break;
  case IIS_Z_SIZE:
    zDrawSize = value;
    break;

  case IIS_SLICES: 
    numSlices = value;
    break;

  case IIS_TRANSPARENCY: 
    ImageTrans = value;
    Imodv->texTrans = ImageTrans;
    break;

  case IIS_BLACK:
    BlackLevel = value;
    mkcmap();
    break;
  case IIS_WHITE:
    WhiteLevel = value;
    mkcmap();
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

// User toggles false color
void ImodvImage::falseToggled(bool state)
{
  Falsecolor = state ? 1 : 0;
  mkcmap();
  imodvDraw(Imodv);
}

// Action buttons
void ImodvImage::buttonPressed(int which)
{
  if (which)
    imodShowHelpPage("modvImage.html");
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
  imodvDialogManager.remove((QWidget *)imodvImageData.dia);
  imodvImageData.dia = NULL;
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

/*
$Log$
Revision 4.20  2009/03/22 19:54:25  mast
Show with new geometry adjust routine for Mac OS X 10.5/cocoa

Revision 4.19  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.18  2008/05/27 02:48:43  mast
Fixed multiple plane drawing of X and Y planes

Revision 4.17  2007/07/08 16:04:49  mast
Used new hot slider function

Revision 4.16  2007/06/22 00:54:21  mast
Converted help to html

Revision 4.15  2006/08/28 05:22:35  mast
Changes for colormapped images

Revision 4.14  2004/11/04 23:30:55  mast
Changes for rounded button style

Revision 4.13  2004/06/15 01:14:55  mast
Added functions to allow transparency and number of slices to be varied
during movie-making

Revision 4.12  2004/05/16 20:17:04  mast
Made it draw solid and transparent planes separately to interact better
with object transparency; added sliders to set display size; rewrote
drawing code with properly named variables for each dimension

Revision 4.11  2004/05/15 21:52:11  mast
Computed alpha factors correctly for the multiple slice display

Revision 4.10  2004/05/03 19:18:26  mast
Added ability to display multiple slices and X, Y, or Z planes

Revision 4.9  2004/01/22 19:12:43  mast
changed from pressed() to clicked() or accomodated change to actionClicked

Revision 4.8  2003/10/27 04:57:22  mast
Fixed problem with reverse contrast

Revision 4.7  2003/09/16 02:09:14  mast
Changed to access image data using new line pointers

Revision 4.6  2003/04/25 03:28:32  mast
Changes for name change to 3dmod

Revision 4.5  2003/04/17 18:43:38  mast
adding parent to window creation

Revision 4.4  2003/03/28 05:01:50  mast
Needed to remove include of glu.h for Mac

Revision 4.3  2003/03/26 23:23:15  mast
switched from hotslider.h to preferences.h

Revision 4.2  2003/02/27 17:24:33  mast
Had to include qgl.h instead of GL/gl.h under windows

Revision 4.1  2003/02/10 20:29:01  mast
autox.cpp

Revision 1.1.2.10  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.9  2003/01/18 00:58:37  mast
add tooltips to dialogframe call

Revision 1.1.2.8  2003/01/13 07:21:38  mast
Changes to use new dialog manager class

Revision 1.1.2.7  2003/01/01 05:43:44  mast
rationalizing toplevel versus dialog style

Revision 1.1.2.6  2002/12/30 17:32:42  mast
eliminate unused variables

Revision 1.1.2.5  2002/12/30 06:40:53  mast
Qt version

Revision 1.1.2.4  2002/12/19 04:37:13  mast
Cleanup of unused global variables and defines

Revision 1.1.2.3  2002/12/18 04:15:14  mast
new includes for imodv modules

Revision 1.1.2.2  2002/12/17 18:33:19  mast
using new includes for imodv compoennts

Revision 1.1.2.1  2002/12/15 21:14:02  mast
conversion to cpp

Revision 3.1  2002/12/01 15:34:41  mast
Changes to get clean compilation with g++

*/
