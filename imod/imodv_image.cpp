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
#include "preferences.h"
#include "multislider.h"
#include "dia_qtutils.h"
#include "imodv.h"
#include "imod.h"
#include "imodv_gfx.h"
#include "imodv_image.h"
#include "imodv_input.h"
#include "control.h"
#include "xcramp.h"


#define DRAW_CZ 1
#define DRAW_CY (2 << 1)
#define DRAW_CX (3 << 1)
#define DRAW_LZ (4 << 1)
#define DRAW_LY (5 << 1)
#define DRAW_LX (6 << 1)

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
  if (a->texMap && !imodvImageData.flags)
    imodvImageData.flags |= DRAW_CZ;
  else if (!a->texMap && imodvImageData.flags)
    imodvImageData.flags = 0;
  if (imodvImageData.dia)
    diaSetChecked(imodvImageData.dia->mViewBox, imodvImageData.flags != 0);
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


// The call from within the openGL calling routines to draw the image
void imodvDrawImage(ImodvApp *a)

{
  Ipoint ll, lr, ur, ul, clamp;
  int tstep = TexImageSize;
  int cix, ciy, ciz;
  int mix, miy;
  int x,y;
  unsigned char **idata;
  unsigned char pix;
  unsigned char alpha = 0xff;
  int i, mi, j, mj;
  int u, v;
     
  if (!imodvImageData.flags) return;

  if (!cmapInit)
    mkcmap();

  ivwGetLocation(a->vi, &cix, &ciy, &ciz);
  mix = a->vi->xsize;
  miy = a->vi->ysize;

  alpha = (int)(2.55 * (100 - ImageTrans));
  glColor4ub(alpha, alpha, alpha,alpha);
  if (ImageTrans){
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA,  GL_ONE_MINUS_SRC_ALPHA);
  }

  /* Draw Current Z image. */
  if (imodvImageData.flags & DRAW_CZ){

    ll.z = ciz; lr.z = ciz; ur.z = ciz; ul.z = ciz;
    idata = ivwGetCurrentZSection(a->vi);
    if (!idata){
      glDisable(GL_BLEND);
      return;
    }

    for(x = 0; x < mix; x += tstep){
      clamp.x = (mix-x);
      clamp.x /= (float)tstep;
      if (clamp.x > 1.0f) clamp.x = 1.0f;
      mi = x + tstep;
      if (mi > mix) mi = mix;
      ul.x = ll.x = x;
      lr.x = x + tstep;
      if (lr.x > mix) lr.x = mix;
      ur.x = lr.x;
               
      for(y = 0; y < miy; y += tstep){
	lr.y = ll.y = y;
	ur.y = y + tstep;
	if (ur.y > miy) ur.y = miy;
	ul.y = ur.y;

	clamp.y = (miy - y); 
	clamp.y /= (float)tstep;
	if (clamp.y > 1.0f) clamp.y = 1.0f;
                    
	mj = y + tstep;
	if (mj > miy) mj = miy;
                    
                    
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
  glDisable(GL_BLEND);
}


// THE ImodvImage CLASS IMPLEMENTATION

static char *buttonLabels[] = {"Done", "Help"};
static char *buttonTips[] = {"Close dialog box", "Open help window"};
static char *sliderLabels[] = {"Transparency", "Black Level", "White Level"};

ImodvImage::ImodvImage(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, buttonLabels, buttonTips, true, "3dmodv Image View",
		"", name)
{
  mCtrlPressed = false;

  // Make view checkbox
  mViewBox = diaCheckBox("View Z image", this, mLayout);
  mViewBox->setChecked(imodvImageData.flags & DRAW_CZ);
  connect(mViewBox, SIGNAL(toggled(bool)), this, SLOT(viewToggled(bool)));

  // Make multisliders - set range of first to 0 - 100
  mSliders = new MultiSlider(this, 3, sliderLabels);
  mSliders->setRange(0, 0, 100);
  mSliders->setValue(0, ImageTrans);
  mSliders->setValue(1, BlackLevel);
  mSliders->setValue(2, WhiteLevel);
  mLayout->addLayout(mSliders->getLayout());
  connect(mSliders, SIGNAL(sliderChanged(int, int, bool)), this, 
          SLOT(sliderMoved(int, int, bool)));

  // Make false color checkbox
  mFalseBox = diaCheckBox("False color", this, mLayout);
  mFalseBox->setChecked(Falsecolor);
  connect(mFalseBox, SIGNAL(toggled(bool)), this, SLOT(falseToggled(bool)));

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));
}

// Viewing image is turned on or off
void ImodvImage::viewToggled(bool state)
{
  if (!state) {
    imodvImageData.flags &= ~DRAW_CZ;
    Imodv->texMap = 0;
  } else {
    imodvImageData.flags |= DRAW_CZ;
    Imodv->texMap = 1;
  }
  imodvDraw(Imodv);
}

// respond to a change of transparency or contrast
void ImodvImage::sliderMoved(int which, int value, bool dragging)
{
  switch (which) {
  case 0: 
    ImageTrans = value;
    Imodv->texTrans = ImageTrans;
    break;

  case 1:
    BlackLevel = value;
    mkcmap();
    break;

  case 2:
    WhiteLevel = value;
    mkcmap();
    break;
  }

  // draw if slider clicked or is in hot state
  if (!dragging || (hotSliderFlag() == HOT_SLIDER_KEYDOWN && mCtrlPressed) ||
      (hotSliderFlag() == HOT_SLIDER_KEYUP && !mCtrlPressed))
    imodvDraw(Imodv);
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
       "Manipulate images in model view.",
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
