#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <Xm/Xm.h>
#include <Xm/MainW.h>
#include <Xm/Form.h>
#include <Xm/DrawingA.h>

#include "xplax.h"

static XtAppContext PlaxContext;
static Widget       PlaxTopLevel;
static Display     *PlaxDisplay;
static Visual      *PlaxVisual;

static int   PlaxCIndex[PLAX_RAMPSIZE];
static float PlaxScaleX;
static float PlaxScaleY;

static Widget      PlaxWidget;
static Drawable    PlaxD;
static GC          PlaxGC;
static Colormap    PlaxCmap;
static XFontStruct PlaxFont;
static int Plax_open;
static int Plax_exposed;
static void plax_input(void);
static void plax_input_open(void);
static void plax_cindex(int cindex);
static void plax_transform( int *x, int *y);
static void plax_resize(void);
static short plax_transx(short ix);
static short plax_transy(short iy);
static int plax_scale(int size);
static void expose_cb(Widget w, XtPointer client, XtPointer call);

/* DNM: change this to  a table that is filled in as fonts are called for */
#define FONT_ARRAY_SIZE 36
int PlaxFontsize[FONT_ARRAY_SIZE][2] = {
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0};

void plax_initialize(char *string, int strsize)
{
  realgraphicsmain_();
  exit(0);
}

int plax_open(void)
{
     XVisualInfo *vlist;
     XVisualInfo vistemp;
     Colormap cmap;
     XColor color;
     Screen *screen;
     Widget window, form;
     int vsize, v, i;
     int screen_num = 0;
     unsigned long plane[1];
     unsigned long pixels[256];
     Dimension width = 800;
     Dimension height = 640;
     int argc = 1;
     char *dummy = "Plax";
     char *argv[1];
     
     argv[0] = dummy;
     Plax_open = 0;

     PlaxTopLevel = XtVaAppInitialize
	  (&PlaxContext, dummy, 
	   NULL, 0, 
	   &argc, argv, 
	   NULL, NULL);

     if (!PlaxTopLevel){
	  fprintf(stderr, "Error opening display.\n");
	  return(-1);
     }

     PlaxDisplay = XtDisplay(PlaxTopLevel);

     form = XtVaCreateManagedWidget
	  ("form", xmFormWidgetClass, PlaxTopLevel,
	   XmNbottomOffset, 0,
	   XmNtopOffset, 0,
	   XmNleftOffset, 0,
	   XmNrightOffset, 0,
	   NULL);

     /* DNM: switched from core to drawing area widget so that the expose
	callback could be added */
     PlaxWidget = XtVaCreateManagedWidget
	  ("draw", xmDrawingAreaWidgetClass, form,
	   XmNbottomAttachment, XmATTACH_FORM,
	   XmNtopAttachment,    XmATTACH_FORM,
	   XmNleftAttachment,   XmATTACH_FORM,
	   XmNrightAttachment,  XmATTACH_FORM,
	   XmNwidth, width,
	   XmNheight, height,
	   XmNbottomOffset, 0,
	   XmNtopOffset, 0,
	   XmNleftOffset, 0,
	   XmNrightOffset, 0,
	   NULL);
     
     XtAddCallback(PlaxWidget, XmNexposeCallback, expose_cb, NULL);

     Plax_exposed = 0;

     XtRealizeWidget(PlaxTopLevel);
     
     PlaxScaleX = PlaxScaleY = 0.5f;
     Plax_open = 1;
     plax_input_open();

     return(0);
}

void plax_close(void)
{
     Plax_open = 0;
     XtDestroyWidget(PlaxTopLevel);
     plax_input();
     return;
}

void plax_flush(void)
{
     plax_input();
     return;
}

void plax_erase(void) {}

void plax_mapcolor(int *color, int *ired, int *igreen, int *iblue)
{
     XColor c;
     unsigned long pix    = *color;
     unsigned short red   = *ired;
     unsigned short green = *igreen;
     unsigned short blue  = *iblue;

     c.flags = DoRed | DoGreen | DoBlue;
     c.pixel = pix;
     c.red   = red << 8;
     c.green = green << 8;
     c.blue  = blue << 8;
     XAllocColor(PlaxDisplay, PlaxCmap, &c);

     PlaxCIndex[pix] = (int)c.pixel;

     return;
}

void plax_box(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
     int x1 = *ix1;
     int y1 = *iy1;
     int x2 = *ix2;
     int y2 = *iy2;
     int x, y, width, height;

     plax_cindex(*cindex);
     plax_transform(&x1, &y1);
     plax_transform(&x2, &y2);
     
     x = (x1 > x2) ? x2 : x1;
     y = (y1 > y2) ? y2 : y1;
     width = x1 - x2;
     height = y1 - y2;

     if (width < 0)
	  width *= -1;

     if (height < 0)
	  height *= -1;
     width += 1;
     height += 1;

     XFillRectangle(PlaxDisplay, PlaxD, PlaxGC,
		    x, y, width, height);

#ifdef PLAX_DEBUG
     printf("draw box (%d, %d, %d, %d)\n",
	    x1, y1, x2, y2);
#endif

     return;
}

void plax_boxo(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
     int x1 = *ix1;
     int y1 = *iy1;
     int x2 = *ix2;
     int y2 = *iy2;
     int x, y, width, height;

     plax_cindex(*cindex);
     plax_transform(&x1, &y1);
     plax_transform(&x2, &y2);

     width = x1 - x2;
     height = y1 - y2;

     if (width < 0)
	  width *= -1;
     if (height < 0)
	  height *= -1;
     width += 1;
     height += 1;

     x = (x1 > x2) ? x2 : x1;
     y = (y1 > y2) ? y2 : y1;

     XDrawRectangle(PlaxDisplay, PlaxD, PlaxGC,
		    x, y, width, height);
     return;
}

void plax_vect(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
     int x1 = *ix1;
     int y1 = *iy1;
     int x2 = *ix2;
     int y2 = *iy2;

     plax_cindex(*cindex);
     plax_transform(&x1, &y1);
     plax_transform(&x2, &y2);

     XDrawLine(PlaxDisplay, PlaxD, PlaxGC,
	       x1, y1, x2, y2);
     return;
}

void plax_vectw(int *linewidth, int *cindex, 
		int *ix1, int *iy1, int *ix2, int *iy2)
{
     static XGCValues val;
     int x1 = *ix1;
     int y1 = *iy1;
     int x2 = *ix2;
     int y2 = *iy2;
     
     val.line_width = *linewidth;
     XChangeGC(PlaxDisplay, PlaxGC, GCLineWidth, &val);
     plax_cindex(*cindex);
     plax_transform(&x1, &y1);
     plax_transform(&x2, &y2);     

     XDrawLine(PlaxDisplay, PlaxD, PlaxGC,
	       x1, y1, x2, y2);

     val.line_width = 1;
     XChangeGC(PlaxDisplay, PlaxGC, GCLineWidth, &val);
     return;
}

/* filled circle */
void plax_circ(int *cindex, int *radius, int *ix, int *iy)
{
     int x = *ix;
     int y = *iy;
     int size = plax_scale(*radius);

     plax_cindex(*cindex);
     plax_transform(&x, &y);

     XFillArc(PlaxDisplay, PlaxD, PlaxGC,
	      x - size,
	      y - size,
	      size * 2,
	      size * 2,
	      0,23040);
     return;
}

/* open circle */
void plax_circo(int *cindex, int *radius, int *ix, int *iy)
{
     int x = *ix;
     int y = *iy;
     int size = plax_scale(*radius);

     plax_cindex(*cindex);
     plax_transform(&x, &y);
     XDrawArc(PlaxDisplay, PlaxD, PlaxGC,
	      x - size,
	      y - size,
	      size * 2,
	      size * 2,
	      0,23040);
     return;
}

/* closed filled polygon */
void plax_poly(int *cindex, int *size, short *vec)
{
     static XPoint points[16];
     unsigned int csize = *size;
     int i;

     if (csize > 16)
	  csize = 16;

     plax_cindex(*cindex);

     for(i = 0; i < csize; i++){
	  points[i].x = plax_transx(vec[i * 2]);
	  points[i].y = plax_transy(vec[(i * 2) + 1]);
     }

     XFillPolygon(PlaxDisplay, PlaxD, PlaxGC,
		  points,
		  csize,
		  Convex,
		  CoordModeOrigin);
     return;
}

void plax_polyo(int *cindex, int *size, short *vec)
{
     static XPoint points [16];
     unsigned int csize = *size;
     int i;

     if (csize > 16)
	  csize = 16;
     plax_cindex(*cindex);


     for(i = 0; i < csize; i++){
	  points[i].x = plax_transx(vec[i * 2]);
	  points[i].y = plax_transy(vec[(i * 2) + 1]);
     }
     
     XDrawLines(PlaxDisplay, PlaxD, PlaxGC,
		points,
		csize,
		CoordModeOrigin);
     
     XDrawLine( PlaxDisplay, PlaxD, PlaxGC, points->x, points->y,
	       points[csize-1].x, points[csize-1].y);

     return;
}

/* DNM: changed this to keep track of last font used, to search for nearest
   size from requested font, and to keep track of the sizes found in the
   table */
void plax_sctext(int *thickness,
		 int *xsize,
		 int *iysize,
		 int *cindex,
		 int *ix, int *iy, 
#ifdef F77STRING
		 FString *f77str
#else
                 char *string, int strsize
#endif
		 )
{
#ifdef F77STRING
     int strsize  = f77str->length;
     char *string = f77str->string;
#endif
     static XFontStruct *font = NULL;
     static int lastsize = 0;
     static int fy = 0;
     static int lastbold;

     char fontname[64];
     int x = *ix;
     int y = *iy;
     int ysize = *iysize * 3;
     int hittop = 0;
     int hitbot = 0;
     int idir = -1;
     int fymin, fymax;
     int ifbold = 0;
     if (*thickness > 1)
	  ifbold = 1;

     ysize = plax_scale(ysize);

     if (ysize > FONT_ARRAY_SIZE)
          ysize = FONT_ARRAY_SIZE;
     if (ysize <= 1) {
          ysize = 1;
	  idir = 1;
     }

     /* Use size from table if it has been filled in */
     if (PlaxFontsize[ysize - 1][ifbold])
	  ysize = PlaxFontsize[ysize - 1][ifbold];

     /* If font doesn't exist or doesn't match last time, look for it */
     if (!font || ysize != lastsize || lastbold != ifbold) {
	  fy = ysize;
	  lastbold = ifbold;
	  lastsize = ysize;
	  
	  if (font)
	       XFreeFont(PlaxDisplay, font);
	  font = NULL;
	  fymin = fy;
	  fymax = fy;
	  while (!font) {

	       if (ifbold)
		    sprintf(fontname, "*courier-bold-r-normal--%d-*", fy);
	       else
		    sprintf(fontname, "*courier-medium-r-normal--%d-*", fy);
	       
	       /*  printf ("trying %s\n", fontname); */
	       font = XLoadQueryFont(PlaxDisplay, fontname);
	       if (!font){
		    /* See if currently at bottom or top of range */
		    if (fy == 1)
			 hitbot = 1;
		    if (fy == FONT_ARRAY_SIZE)
			 hittop = 1;

		    /* increase from the min or decrease from the max */
		    if (idir > 0) {
			 fy = fymax + 1;
			 fymax = fy;
		    } else {
			 fy = fymin - 1;
			 fymin = fy;
		    }

		    /* Reverse direction only if haven't hit either end */
		    if (!hittop && !hitbot)
			 idir = -idir;

		    /* If hit both ends, give up */
		    if (hittop && hitbot) {
			 fprintf(stderr, "Error loading fonts\n");
			 fy = 0;
			 return;
		    }
	       }
	  }
	  /* Save what was found to work for this size */
	  PlaxFontsize[ysize - 1][ifbold] = fy;
     }
     
     XSetFont(PlaxDisplay, PlaxGC, font->fid);

     plax_transform(&x, &y);
     plax_cindex(*cindex);

     XDrawString(PlaxDisplay, PlaxD, PlaxGC,
		 x, y, string, strsize);
     return;
}

void plax_loop(void)
{
     XEvent event_return;

     for (;;){
	  XtAppNextEvent(PlaxContext, &event_return);
	  
	  if (event_return.type == ResizeRequest){
	       plax_resize();
	  }
	  XtDispatchEvent(&event_return);
     }
}

void plax_putc(char *f)
{
	putchar(*f);
}

/*****************************************************************************/
/* Internal Functions                                                        */
/*****************************************************************************/


static int plax_scale(int size)
{
     int nsize;
     if (PlaxScaleX > PlaxScaleY)
	  nsize = size * PlaxScaleY;
     else
	  nsize = size * PlaxScaleX;

     if (nsize < 1)
	  nsize = 1;
     return(nsize);
}


static void plax_resize(void)
{
     static Dimension width= 0;
     static Dimension height = 0;
     Dimension nw, nh;

     if (!Plax_open)
	  return;

     XtVaGetValues(PlaxTopLevel, 
		   XmNwidth, &nw, 
		   XmNheight, &nh, NULL);

     if ((nw != width) || (nh != height)){
	  width = nw;
	  height = nh;
	  XtVaSetValues(PlaxWidget, XmNwidth, width, XmNheight, height, NULL);
     }

     XtVaGetValues(PlaxWidget,
		   XmNwidth, &nw,
		   XmNheight, &nh, NULL);

     PlaxScaleX = ((float)nw) / 1280.0f;
     PlaxScaleY = ((float)nh) / 1024.0f;
     return;
}

static void plax_input(void)
{
     XEvent event_return;
     
     while(XtAppPending(PlaxContext)){
	  XtAppNextEvent(PlaxContext, &event_return);
/*	  if (event_return.type == VisibilityNotify) */
/*	       plax_resize(); */
/*	  printf("%d.",event_return.type);fflush(stdout); */
	  XtDispatchEvent(&event_return);
     }
     plax_resize(); 
     return;
}

static void plax_input_open()
{
     /* DNM 11/19/00: This used to loop until a certain event was found, but
	it was not a robust method between SGI and Linux 6 and Linux 7.
	Switched to having an expose callback that sets the expose flag
	and finishes initialization (getting graphic context, etc */

     XEvent event_return;

     /* Process events until expose happens */
     while (!Plax_exposed){
	  XtAppNextEvent(PlaxContext, &event_return);
	  XtDispatchEvent(&event_return);
     }

     plax_input();
     return;
}

static void expose_cb(Widget w, XtPointer client, XtPointer call)
{
     if (!Plax_exposed) {
	  PlaxGC = XCreateGC(PlaxDisplay, XtWindow(PlaxWidget), 0, NULL);
	  PlaxD  = XtWindow(PlaxWidget);
	  XtVaGetValues(PlaxWidget, XmNcolormap, &PlaxCmap, NULL);
     }

     Plax_exposed = 1;
}

static void plax_cindex(int cindex)
{
     unsigned long pix = PlaxCIndex[cindex];

     plax_input();

     if (PlaxCIndex[cindex] < 0){
	  fprintf(stderr, "Warning color %d not set.\n", PlaxCIndex[cindex]);
	  return;
     }
     XSetForeground(PlaxDisplay, PlaxGC, pix);
     return;
}

static short plax_transx(short ix)
{
     float x;
     x = ((float)ix) * PlaxScaleX;
     return((short)x);
}

static short plax_transy(short iy)
{
     float y;
     y = PlaxScaleY * (1023.0f - iy);
     return((short)y);
}


static void plax_transform( int *x, int *y)
{
     float ix = *x; 
     float iy = *y;

     *x = PlaxScaleX * ix;
     *y = PlaxScaleY * (1023.0 - iy);
/*
     printf("transform (%g, %g) -> (%d, %d) : scale %g %g\n",
	    ix, iy, *x, *y, PlaxScaleX, PlaxScaleY);
*/
     return;
}

