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

    $Log$
*/

#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/ToggleB.h>
#include <Xm/RowColumn.h>
#include <Xm/ArrowB.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <dia.h>
#include "imodv.h"
#include "imod.h"


#ifdef NO_IMODV_IMAGE

void imodvDrawImage(ImodvApp *a){ return };
void imodvImageEditDialog(ImodvApp *a, int state){ return };

#else

#define DRAW_CZ 1
#define DRAW_CY (2 << 1)
#define DRAW_CX (3 << 1)
#define DRAW_LZ (4 << 1)
#define DRAW_LY (5 << 1)
#define DRAW_LX (6 << 1)

#define TexImageSize 64

static GLubyte tdata[TexImageSize][TexImageSize][3];

static void mkcmap(void);
static GLubyte Cmap[3][256];
static int CmapInit   = 0;
static int BlackLevel = 0;
static int WhiteLevel = 255;
static int Falsecolor = 0;

static int ImageTrans = 0;

struct{
     diaDialog *dia;
     ImodvApp  *a;

     int    flags;
     int    xsize, ysize, zsize;
     int    *xd, *yd, *zd;

     Widget wTrans;
     Widget wCZToggle;

}imodvImageData = {0, 0, 0, 0, 0, 0, 0};

static Widget mkWorkArea(ImodvApp *a, Widget top);

static void help_cb()
{
     dia_vasmsg
	  ("~~~~~~~~~~~~~~~~~~~~~~~~\n"
	   "Image Control Dialog Help.\n"
	   "~~~~~~~~~~~~~~~~~~~~~~~~"
	   "\n\n",
	   "Manipulate images in model view.",
	   NULL);
     return;
}

static void workarea_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     mkWorkArea(imodvImageData.a, w);
     return;
}

static void done_cb(Widget w, XtPointer client, XtPointer call)
{
     diaDialog *dia = (diaDialog *)call;
     diaDestroyDialog(dia);

     imodvImageData.dia = NULL;
     return;
}

void imodvImageEditDialog(ImodvApp *a, int state)
{
     XtPointer cbd = (XtPointer)(&imodvImageData);

     if (!state){
	  if (imodvImageData.dia)
	       done_cb(NULL, NULL, (XtPointer)imodvImageData.dia);
	  return;
     }
     if (imodvImageData.dia){
	  XRaiseWindow(a->display, 
		       XtWindow(imodvImageData.dia->dialog));
	  return;
     }

     imodvImageData.dia = diaVaCreateDialog
	  ("Imodv: Image View", a->topLevel, a->context,
	   DiaNcontrolButton, "Done", done_cb, cbd,
	   DiaNcontrolButton, "Help", help_cb, cbd,
	   DiaNworkAreaFunc,  workarea_cb,     cbd,
	   DiaNwindowQuit,    done_cb,         cbd,
	   0);
     imodvImageData.a = Imodv;
     return;
}

/****************************************************************************/
/*  Image Dialog controls.                                                  */

static void imageZ_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     if (imodvImageData.flags & DRAW_CZ)
	  imodvImageData.flags &= ~DRAW_CZ;
     else
	  imodvImageData.flags |= DRAW_CZ;
     imodvDraw(Imodv);
}

static void trans_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     ImageTrans = cbs->value;
     Imodv->texTrans = ImageTrans;
     imodvDraw(Imodv);
}

static void fcolor_cb(Widget w, XtPointer client, XtPointer call)
{
     XmToggleButtonCallbackStruct *cbs = (XmToggleButtonCallbackStruct *)call;
     if (Falsecolor)
	  Falsecolor = 0;
     else
	  Falsecolor = 1;
     mkcmap();
     imodvDraw(Imodv);
}

static void level_cb(Widget w, XtPointer client, XtPointer call)
{
     XmScaleCallbackStruct *cbs = (XmScaleCallbackStruct *)call;
     int *level = (int *)client;

     *level = cbs->value;

     mkcmap();
     imodvDraw(Imodv);
}

static Widget mkWorkArea(ImodvApp *a, Widget top)
{
     Widget frame, col;

     frame = XtVaCreateWidget
	  ("frame", xmFrameWidgetClass, top, NULL);

     col = XtVaCreateWidget
	  ("rowcol", xmRowColumnWidgetClass, frame,
	   NULL);
     {
	  Widget scale, toggle;
	  int tstate;

	  imodvImageData.wCZToggle = XtVaCreateManagedWidget
	       ("View Z Image", xmToggleButtonWidgetClass, col, NULL);
          XtAddCallback(imodvImageData.wCZToggle, XmNvalueChangedCallback,
			imageZ_cb, NULL);
	  if (imodvImageData.flags & DRAW_CZ)
	       tstate = True;
	  else
	       tstate = False;
	  XmToggleButtonSetState(imodvImageData.wCZToggle, tstate, False);
	  

	  imodvImageData.wTrans =  XtVaCreateManagedWidget
	       ("scale", xmScaleWidgetClass, col,
		XmNorientation, XmHORIZONTAL,
		XmNminimum, 0, XmNmaximum, 255,
		XmNvalue, ImageTrans,
		XmNscaleMultiple, 1,
		XtVaTypedArg, XmNtitleString, XmRString, "Transparency", 13,
		NULL);
	  XtAddCallback(imodvImageData.wTrans, 
			XmNvalueChangedCallback, trans_cb, 0);

	  

	  toggle = XtVaCreateManagedWidget
	       ("False Color", xmToggleButtonWidgetClass, col, NULL);
	  XtAddCallback(toggle, XmNvalueChangedCallback,
			fcolor_cb, NULL);
	  XmToggleButtonSetState(toggle, Falsecolor, False);
	  

	  scale = XtVaCreateManagedWidget
	       ("scale", xmScaleWidgetClass, col,
		 XmNorientation, XmHORIZONTAL,
		XmNminimum, 0, XmNmaximum, 255,
		XmNvalue, BlackLevel,
		XmNscaleMultiple, 1,
		XtVaTypedArg, XmNtitleString, XmRString, "Black Level", 12,
		NULL);
	  XtAddCallback(scale, XmNvalueChangedCallback, 
			level_cb, (XtPointer)&BlackLevel);

	  scale = XtVaCreateManagedWidget
	       ("scale", xmScaleWidgetClass, col,
		XmNorientation, XmHORIZONTAL,
		XmNminimum, 0, XmNmaximum, 255,
		XmNvalue, WhiteLevel,
		XmNscaleMultiple, 1,
		XtVaTypedArg, XmNtitleString, XmRString, "White Level", 12,
		NULL);
	  XtAddCallback(scale, XmNvalueChangedCallback,
			level_cb, (XtPointer)&WhiteLevel);
     }
     XtManageChild(col);
     XtManageChild(frame);

     if (!CmapInit)
	  mkcmap();
     return(frame);
}     



/****************************************************************************/
/* TEXTURE MAP IMAGE. */
/****************************************************************************/

static void mkcmap(void)
{
     int rampsize, cmapReverse = 0;
     float slope, point;
     int r,g,b,i;

     if (BlackLevel > WhiteLevel){
	  cmapReverse = BlackLevel;
	  BlackLevel = WhiteLevel;
	  WhiteLevel = cmapReverse;
	  cmapReverse = 1;
     }

     rampsize = WhiteLevel - BlackLevel;
     if (rampsize < 1) rampsize = 1;
     
     for (i = 0; i < BlackLevel; i++)
	  Cmap[0][i] = 0;
     for (i = WhiteLevel; i < 256; i++)
	  Cmap[0][i] = 255;
     slope = 256.0 / (float)rampsize;
     for (i = BlackLevel; i < WhiteLevel; i++){
	  point = (float)(i - BlackLevel) * slope;
	  Cmap[0][i] = (unsigned char)point;
     }
     
     if (cmapReverse){
	  for(i = 0; i < 256; i++){
	       if (!Cmap[0][i])
		    Cmap[0][i] = 255;
	       else
		    if (Cmap[0][i] > 127){
			 Cmap[0][i] -= 128;
			 Cmap[0][i] *= -1;
			 Cmap[0][i] += 128;
		    }
		    else{
			 Cmap[0][i] -= 128;
			 Cmap[0][i] *= -1;
			 Cmap[0][i] += 128;
		    }
	  }
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



void imodvDrawImage(ImodvApp *a)

{
     Ipoint ll, lr, ur, ul, clamp;
     int tstep = TexImageSize;
     int cix, ciy, ciz;
     int mix, miy, miz;
     int x,y,z;
     unsigned char *idata;
     unsigned char pix;
     unsigned char alpha = 0xff;
     int t, mt, i, mi, j, mj;
     int base;
     int u, v;
     
     if (!imodvImageData.flags) return;

     mt = tstep * tstep;

     ivwGetLocation(a->vi, &cix, &ciy, &ciz);
     mix = a->vi->xsize;
     miy = a->vi->ysize;
     miz = a->vi->zsize;

     alpha = 255 - ImageTrans;
     glColor4ub(alpha, alpha, alpha,alpha);
     if (ImageTrans){
	   glEnable(GL_BLEND);
	   glBlendFunc(GL_SRC_ALPHA,  GL_ONE_MINUS_SRC_ALPHA);
     }

     /* Draw Current Z image. */
     if (imodvImageData.flags & DRAW_CZ){

	  ll.z = ciz; lr.z = ciz; ur.z = ciz; ul.z = ciz;
	  idata = ivwGetCurrentZSection(XYZ_vi);
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
		    base = x + (y * mix);
		    
		    mj = y + tstep;
		    if (mj > miy) mj = miy;
		    
		    
		    for(i = x; i < mi; i++){
			 u = i - x;
			 for(j = y; j < mj; j++){
			      v = (j - y);
			      pix = idata[i + (j * mix)];

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

#endif
