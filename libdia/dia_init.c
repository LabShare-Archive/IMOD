/*****************************************************************************
 *                                                                           *
 *   FILE: dia_init.c                                                        *
 *                                                                           *
 *   PURPOSE: Dialog library init and internal functions.                    *
 *                                                                           *
 *   HISTORY:                                                                *
 *       Version 1.0  James Kremer  kremer@beagle.colorado.edu               *
 *                                                                           *
 *****************************************************************************
 *   Copyright (C) 1994,1995 by Boulder Laboratory for 3-Dimensional Fine    *
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

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/AtomMgr.h>
#include <X11/cursorfont.h>

XtAppContext  Dia_context = NULL;
char         *Dia_title = NULL;
Widget        Dia_toplevel = NULL;

XtAppContext dia_get_context(void)
{
     return(Dia_context);
}

Widget dia_get_toplevel(void)
{
     return(Dia_toplevel);
}

char *dia_get_title(void)
{
     return(Dia_title);
}

int dia_init(int *argc, char **argv)
{

     Dia_title = argv[0];

     Dia_toplevel = XtVaAppInitialize
	  (&Dia_context, "Dia", NULL, 0,
	   argc, argv,
	   NULL, NULL);

     if (!Dia_toplevel)
	  return(-1);

     return(0);
}

int dia_xinit(Widget w, XtAppContext context, char *title)
{
     Dia_context = context;
     Dia_toplevel = w;
     Dia_title = title;
     return(0);
}

Visual *dia_getvisual(void)
{
     Visual *visual;
     XtVaGetValues(Dia_toplevel, XmNvisual, &visual, NULL);
     return(visual);
}

int dia_getdepth(void)
{
     int depth;

     XtVaGetValues(Dia_toplevel, XmNdepth, &depth, NULL);
     return(depth);
}
Colormap dia_getcolormap(void)
{
     Colormap cmap;
     XtVaGetValues(Dia_toplevel, XmNcolormap, &cmap, NULL);
     return(cmap);
}


void dia_input(void)
{
     XEvent event_return;
     
     while(XtAppPending(Dia_context)){
	  XtAppNextEvent(Dia_context, &event_return);
	  XtDispatchEvent(&event_return);
     }
}

int dia_mainloop(void)
{
     XtAppMainLoop(Dia_context);
     return(0);
}


void dia_busy(int state)
{
     XEvent event_return;
     static int level = 0;
     
     if (state){
	  if (!level){
	       XFlush(XtDisplay(Dia_toplevel));
	       dia_input();
	  }
	  level++;
     }else{
	  level--;
	  if (level < 0) level = 0;
	  if (!level){
	       XFlush(XtDisplay(Dia_toplevel));
	       while(XtAppPending(Dia_context)){
		    XtAppNextEvent(Dia_context, &event_return);
	       }
	  }
     }
     return;
}


void diaBusyWindow(Widget w, int flag)
{
 static Cursor cursor = 0;
  XSetWindowAttributes attrs;

  if (flag){
       if (!cursor)
            cursor = XCreateFontCursor(XtDisplay(w), XC_watch);
       attrs.cursor = cursor;
       XChangeWindowAttributes(XtDisplay(w),
                               XtWindow(w), CWCursor, &attrs);
       dia_busy(flag);
   }else{
       attrs.cursor = None;
       XChangeWindowAttributes(XtDisplay(w),
                               XtWindow(w), CWCursor, &attrs);
       dia_busy(flag);
   }
}

void diaBusyCursor(int flag)
{
    diaBusyWindow(Dia_toplevel, flag);
}

void diaWindowQuit(Widget window, XtCallbackProc quit_cb, XtPointer data)
{
    Atom     wmclose;
    wmclose = XmInternAtom( XtDisplay(Dia_toplevel),
                            "WM_DELETE_WINDOW", False);
    XmAddWMProtocolCallback(window, wmclose, quit_cb,
			    (caddr_t)data);

}

/****************************************************************************/
/* internal init functions.
*/

void dia_map_cb(Widget widget, XtPointer client_data, XtPointer call_data)
{
     Window  rwin, cwin;
     Dimension w, h, tw, th, tx, ty;

     int rx, ry, wx, wy, mr;
     int sw, sh;
     int x, y;

     XtVaGetValues(widget, XmNwidth, &w, XmNheight, &h, NULL);
     XtVaGetValues(Dia_toplevel, XmNwidth, &tw, XmNheight, &th,
		   XmNx, &tx, XmNy, &ty, NULL);
     
     XQueryPointer(XtDisplay(Dia_toplevel),
		   DefaultRootWindow(XtDisplay(Dia_toplevel)),
		   &rwin, &cwin,
		   &rx, &ry,
		   &wx, &wy,
		   (unsigned int *)&mr);

/*     x = rx - (w/2); */
/*     y = ry - (h/2); */

     x = tx + (tw / 2);
     y = ty + (th / 2);
     x -= (w / 2);
     y -= (h / 2);

     
     sw = WidthOfScreen(XtScreen(Dia_toplevel));
     sh = HeightOfScreen(XtScreen(Dia_toplevel));
     if (x < 0) x = 0;
     if (y < 0) y = 0;
     if (x > (sw - w)) x = sw - w;
     if (h > (sh - h)) y = sh - h;

     XtVaSetValues(widget, XmNx, x, XmNy, y, NULL);
}



