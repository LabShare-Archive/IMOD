/*  IMOD VERSION 2.50
 *
 *  imodv_image.c -- View image slice in model view control dialog.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/
/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#include <qcheckbox.h>
#include <qlayout.h>
#include <qgl.h>
#include <qtooltip.h>
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
#include "xcramp.h"


#define TexImageSize 64


static void mkcmap(void);
static void imodvDrawTImage(Ipoint *p1, Ipoint *p2, Ipoint *p3, Ipoint *p4,
                            Ipoint *clamp,
                            unsigned char *data,
                            int width, int height);

static GLubyte tdata[TexImageSize][TexImageSize][3];
static GLubyte Cmap[3][256];
static int BlackLevel = 0;
static int WhiteLevel = 255;
static int Falsecolor = 0;
static int ImageTrans = 0;
static int cmapInit = 0;
static int numSlices = 1;


struct{
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
  imodvImageData.dia->show();
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


/****************************************************************************/
/* TEXTURE MAP IMAGE. */
/****************************************************************************/

// Make a color map
static void mkcmap(void)
{
  int rampsize, cmapReverse = 0;
  float slope, point;
  int r,g,b,i;
  int white = WhiteLevel;
  int black = BlackLevel;

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
     
  if (cmapReverse){
    for(i = 0; i < 256; i++)
      Cmap[0][i] = 255 - Cmap[0][i];
  }
  if (Falsecolor){
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
  xclamp = clamp->x;
  yclamp = clamp->y;

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
static void setSliceLimits(int ciz, int miz, bool invertZ, int &zst, int &znd,
                           int &izdir)
{
  unsigned char alpha = 0xff;
  double dalpha;
  zst = ciz - numSlices / 2;
  znd = zst + numSlices - 1;
  if (zst < 0)
    zst = 0;
  if (znd >= miz)
    znd = miz - 1;
  izdir = 1;

  // Based on adjusting transparencies with increasing numbers of planes,
  // with 3 different starting transparencies.
  dalpha = 2.55 * (100 - ImageTrans);
  if (znd > zst)
    dalpha *= 1.1 * pow((double)(znd + 1 - zst), -0.86);
  if (dalpha > 255.)
    dalpha = 255.;
  alpha = (unsigned char)dalpha;
  
  if (invertZ) {
    izdir = zst;
    zst = znd;
    znd = izdir;
    izdir = -1;
  }
  
  glColor4ub(alpha, alpha, alpha,alpha);
  if (ImageTrans){
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,  GL_ONE_MINUS_SRC_ALPHA);
  }
}

// The call from within the openGL calling routines to draw the image
void imodvDrawImage(ImodvApp *a)

{
  Ipoint ll, lr, ur, ul, clamp;
  int tstep = TexImageSize;
  int cix, ciy, ciz;
  int mix, miy, miz;
  int x,y;
  unsigned char **idata;
  unsigned char pix;
  int i, mi, j, mj;
  int u, v;
  int zst, znd, iz, izdir;
  int imdataxsize, cacheSum;
  unsigned char **imdata;
  bool flipped, invertX, invertY, invertZ;
  Imat *mat;
  Ipoint inp, outp;
     
  mix = a->vi->xsize;
  miy = a->vi->ysize;
  miz = a->vi->zsize;
  if (imodvImageData.dia) {
    imodvImageData.dia->mSliders->setValue(0, (int)(a->vi->xmouse + 1.5f));
    imodvImageData.dia->mSliders->setRange(1, 1, miy);
    imodvImageData.dia->mSliders->setValue(1, (int)(a->vi->ymouse + 1.5f));
    imodvImageData.dia->mSliders->setRange(2, 1, miz);
    imodvImageData.dia->mSliders->setValue(2, (int)(a->vi->zmouse + 1.5f));
  }

  if (!imodvImageData.flags) 
    return;

  if (!cmapInit)
    mkcmap();

  ivwGetLocation(a->vi, &cix, &ciy, &ciz);

  // Get data pointers if doing X or Y planes
  if (imodvImageData.flags & (IMODV_DRAW_CX | IMODV_DRAW_CY)) {
    if (ivwSetupFastAccess(a->vi, &imdata, 0, &cacheSum))
      return;
    flipped = (!a->vi->vmSize || a->vi->fullCacheFlipped) && 
      a->vi->li->axis == 2;
  }

  // If doing multiple slices, need to find direction in which to do them
  invertX = invertY = invertZ = false;
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

    setSliceLimits(ciz, miz, invertZ, zst, znd, izdir);

    for (iz = zst; izdir * (znd - iz) >= 0 ; iz += izdir) {
      ll.z = iz; lr.z = iz; ur.z = iz; ul.z = iz;
      idata = ivwGetZSection(a->vi, iz);
      if (!idata)
        continue;

      for(x = 0; x < mix; x += tstep){
        clamp.x = 1.0f;
        mi = x + tstep;
        if (mi > mix) {
          mi = mix;
          clamp.x = (mix-x) / (float)tstep;
        }
        ul.x = ll.x = x;
        ur.x = lr.x = mi;
        
        for(y = 0; y < miy; y += tstep){
          clamp.y = 1.0f;
          mj = y + tstep;
          if (mj > miy) {
            mj = miy;
            clamp.y = (miy - y) / (float)tstep;
          }
          lr.y = ll.y = y;
          ul.y = ur.y = mj;
          
          for(i = x; i < mi; i++){
            u = i - x;
            for(j = y; j < mj; j++){
              v = (j - y);
              pix = idata[j][i];
              
              tdata[u][v][0] = Cmap[0][pix];
              tdata[u][v][1] = Cmap[1][pix];
              tdata[u][v][2] = Cmap[2][pix];
            }
          }
          
          clamp.z = clamp.x;
          clamp.x = clamp.y;
          clamp.y = clamp.z;
          imodvDrawTImage(&ll, &lr, &ur, &ul,
                          &clamp,
                          (unsigned char *)tdata, tstep, tstep);
        }
      }
    }       
  }

  /* Draw Current X image. */
  if (imodvImageData.flags & IMODV_DRAW_CX) {

    setSliceLimits(cix, mix, invertX, zst, znd, izdir);

    for (iz = zst; izdir * (znd - iz) >= 0 ; iz += izdir) {
      ll.x = iz; lr.x = iz; ur.x = iz; ul.x = iz;

      for(x = 0; x < miy; x += tstep){
        clamp.x = 1.0f;
        mi = x + tstep;
        if (mi > miy) {
          mi = miy;
          clamp.x = (miy-x) / (float)tstep;
        }
        ul.y = ll.y = x;
        ur.y = lr.y = mi;
        
        for(y = 0; y < miz; y += tstep){
          clamp.y = 1.0f;
          mj = y + tstep;
          if (mj > miz) {
            mj = miz;
            clamp.y = (miz - y) / (float)tstep;
          }
          lr.z = ll.z = y;
          ul.z = ur.z = mj;
          
          // Handle cases of flipped or not with different loops to put test
          // on presence of data in the outer loop
          if (flipped) {
            for (i = x; i < mi; i++) {
              u = i - x;
              if (imdata[i]) {
                for (j = y; j < mj; j++) {
                  v = (j - y);
                  pix = imdata[i][cix + (j * mix)];
                  tdata[u][v][0] = Cmap[0][pix];
                  tdata[u][v][1] = Cmap[1][pix];
                  tdata[u][v][2] = Cmap[2][pix];
                }
              } else {
                for (j = y; j < mj; j++) {
                  v = (j - y);
                  tdata[u][v][0] = Cmap[0][0];
                  tdata[u][v][1] = Cmap[1][0];
                  tdata[u][v][2] = Cmap[2][0];
                }
              }
            }
          } else {
            for (j = y; j < mj; j++) {
              v = (j - y);
              if (imdata[j]) {
                for (i = x; i < mi; i++) {
                  u = i - x;
                  pix = imdata[j][cix + (i * mix)];
                  tdata[u][v][0] = Cmap[0][pix];
                  tdata[u][v][1] = Cmap[1][pix];
                  tdata[u][v][2] = Cmap[2][pix];
                }
              } else {
                for (i = x; i < mi; i++) {
                  u = i - x;
                  tdata[u][v][0] = Cmap[0][0];
                  tdata[u][v][1] = Cmap[1][0];
                  tdata[u][v][2] = Cmap[2][0];
                }
              }
            }
          }
          
          clamp.z = clamp.x;
          clamp.x = clamp.y;
          clamp.y = clamp.z;
          imodvDrawTImage(&ll, &lr, &ur, &ul,
                          &clamp,
                          (unsigned char *)tdata, tstep, tstep);
        }
      }
    }       
  }

  /* Draw Current Y image. */
  if (imodvImageData.flags & IMODV_DRAW_CY) {
    setSliceLimits(ciy, miy, invertY, zst, znd, izdir);

    for (iz = zst; izdir * (znd - iz) >= 0 ; iz += izdir) {
      ll.y = iz; lr.y = iz; ur.y = iz; ul.y = iz;

      for (x = 0; x < mix; x += tstep) {
        clamp.x = 1.0f;
        mi = x + tstep;
        if (mi > mix) {
          mi = mix;
          clamp.x = (mix-x) / (float)tstep;
        }
        ul.x = ll.x = x;
        ur.x = lr.x = mi;
        
        for (y = 0; y < miz; y += tstep) {
          clamp.y = 1.0f;
          mj = y + tstep;
          if (mj > miz) {
            mj = miz;
            clamp.y = (miz - y) / (float)tstep;
          }
          lr.z = ll.z = y;
          ul.z = ur.z = mj;

          // This one is easier, one outer loop and flipped, non-flipped, or
          // no data cases for inner loop
          for (j = y; j < mj; j++) {
            v = (j - y);
            if (flipped && imdata[ciy]) {
              for (i = x; i < mi; i++) {
                u = i - x;
                pix = imdata[ciy][i + (j * mix)];
                tdata[u][v][0] = Cmap[0][pix];
                tdata[u][v][1] = Cmap[1][pix];
                tdata[u][v][2] = Cmap[2][pix];
              }
            } else if (!flipped && imdata[j]) {
              for (i = x; i < mi; i++) {
                u = i - x;
                pix = imdata[j][i + (ciy * mix)];
                tdata[u][v][0] = Cmap[0][pix];
                tdata[u][v][1] = Cmap[1][pix];
                tdata[u][v][2] = Cmap[2][pix];
              }
            } else {
              for (i = x; i < mi; i++) {
                u = i - x;
                tdata[u][v][0] = Cmap[0][0];
                tdata[u][v][1] = Cmap[1][0];
                tdata[u][v][2] = Cmap[2][0];
              }
            }

          }
          clamp.z = clamp.x;
          clamp.x = clamp.y;
          clamp.y = clamp.z;
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
static char *sliderLabels[] = {"X", "Y", "Z", "# of planes", "Transparency", 
                               "Black Level", "White Level"};

ImodvImage::ImodvImage(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, buttonLabels, buttonTips, true, "3dmodv Image View",
		"", name)
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
  QToolTip::add(mViewXBox, "Display YZ plane at current X");
  QToolTip::add(mViewYBox, "Display XZ plane at current Y");
  QToolTip::add(mViewZBox, "Display XY plane at current Z");

  // Make multisliders - set range of first to 0 - 100
  mSliders = new MultiSlider(this, 7, sliderLabels);

  mSliders->setRange(0, 1, Imodv->vi->xsize);
  mSliders->setValue(0, (int)(Imodv->vi->xmouse + 1.5f));
  mSliders->setRange(1, 1, Imodv->vi->ysize);
  mSliders->setValue(1, (int)(Imodv->vi->ymouse + 1.5f));
  mSliders->setRange(2, 1, Imodv->vi->zsize);
  mSliders->setValue(2, (int)(Imodv->vi->zmouse + 1.5f));
  mSliders->setRange(3, 1, 64);
  mSliders->setValue(3, numSlices);
  mSliders->setRange(4, 0, 100);
  mSliders->setValue(4, ImageTrans);
  mSliders->setValue(5, BlackLevel);
  mSliders->setValue(6, WhiteLevel);
  mLayout->addLayout(mSliders->getLayout());
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderMoved(int, int, bool)));
  QToolTip::add((QWidget *)mSliders->getSlider(0), "Set current image X"
                " coordinate");
  QToolTip::add((QWidget *)mSliders->getSlider(1), "Set current image Y "
                "coordinate");
  QToolTip::add((QWidget *)mSliders->getSlider(2), "Set current image Z "
                "coordinate");
  QToolTip::add((QWidget *)mSliders->getSlider(3), "Set number of planes to "
                "display");
  QToolTip::add((QWidget *)mSliders->getSlider(4), "Set transparency (needed "
                "to see multiple planes)");
  QToolTip::add((QWidget *)mSliders->getSlider(5), "Set minimum black level "
                "of contrast ramp");
  QToolTip::add((QWidget *)mSliders->getSlider(6), "Set maximum white level "
                "of contrast ramp");

  // Make false color checkbox
  mFalseBox = diaCheckBox("False color", this, mLayout);
  mFalseBox->setChecked(Falsecolor);
  connect(mFalseBox, SIGNAL(toggled(bool)), this, SLOT(falseToggled(bool)));
  QToolTip::add(mFalseBox, "Display image in false color");

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
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
  case 0:
    Imodv->vi->xmouse = value - 1;
    ivwBindMouse(Imodv->vi);
    break;

  case 1:
    Imodv->vi->ymouse = value - 1;
    ivwBindMouse(Imodv->vi);
    break;

  case 2:
    Imodv->vi->zmouse = value - 1;
    ivwBindMouse(Imodv->vi);
    break;

  case 3: 
    numSlices = value;
    break;

  case 4: 
    ImageTrans = value;
    Imodv->texTrans = ImageTrans;
    break;

  case 5:
    BlackLevel = value;
    mkcmap();
    break;

  case 6:
    WhiteLevel = value;
    mkcmap();
    break;
  }

  // draw if slider clicked or is in hot state
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !mCtrlPressed)) {
    if (which > 2)
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
    dia_vasmsg
      ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
       "Image Control Dialog Help.\n"
       "~~~~~~~~~~~~~~~~~~~~~~~~"
       "\n\n",
       "This dialog allows you to display image planes on the model data.  "
       "Any of the three native orthogonal planes can be displayed, alone or "
       "in combination.  Transparency can be set to allow the model to be seen"
       " through the image; a projection through multiple planes can also "
       "displayed.\n\n"
       "The first three check boxes (\"View X image\", etc.) turn on the "
       "display of the indicated image planes.  The hot key Z will turn on "
       "the Z "
       "image if no images are being displayed, or it will turn off all "
       "planes if any of them are on.\n\n"
       "The \"X\", \"Y\", and \"Z\" sliders will track the current point "
       "coordinate in "
       "3dmod image display windows and can be used to adjust both the "
       "current point position and the displayed slice in the model view "
       "window.\n\n"
       "The \"# of planes\" slider can be used to display multiple planes.  "
       "You will need some transparency to see through the top-most plane "
       "and see a projection of data in the multiple planes.  Transparency "
       "will probably need to be adjusted when you change the number of "
       "planes.\n\n"
       "Use the \"Transparency\" slider to see through the image.  If you are "
       "showing images in more than one plane, this will not work well "
       "because images are not drawn in order from back to front everywhere "
       "that they overlap.\n\n"
       "The \"Black Level\" and \"White Level\" sliders can be used to "
       "adjust the contrast of the image.  You can reverse the contrast "
       "by making Black be higher than White.\n\n"
       "The \"False color\" checkbox allows images to be displayed in the "
       "standard false color scheme.",
       NULL);
  else
    close();
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
  if (e->key() == Qt::Key_Escape)
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
