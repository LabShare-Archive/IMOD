/*  IMOD VERSION 2.50
 *
 *  xzap.h -- Header file for ZaP Window.
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

#ifndef XZAP_H
#define XZAP_H

void zap_ginit_cb(Widget w, XtPointer client, XtPointer call);
void zap_input_cb(Widget w, XtPointer client, XtPointer call);

typedef struct zapwin
{
     Widget dialog;               /* Zap window widget. */
     Widget gfx;                  /* Image sub window.  */
     int    winx,      winy;      /* Image window size. */
     int    xborder,   yborder;   /* border around image window. */
     int    xstart,    ystart;
     int    xdrawsize, ydrawsize;
     int    xtrans,    ytrans,    ztrans;
     int    lmx,       lmy;

     int    ginit;
     int    hqgfx, hide;
     int    hqgfxsave;           /* Place to save hqgfx when dragging */
     int    resizedraw2x;        /* Flag to draw twice after resize */
     int    resizeSkipDraw;      /* Flag  to skip drawing during resize */
     XtIntervalId exposeTimeOut; /* Timeouts during expose cascade */

     int rubberband;    /* Rubber banding flag and corner coordinates */
     int bandllx;
     int bandurx;
     int bandlly;
     int bandury;

     int movieSnapCount; /* Counter if this window is doing movie snapshots */

     float  zoom;
     char   *data;
     XID    context;   /* generic context holder for X11 & OpenGL. */

     /* The graphic image buffer. */
     B3dCIImage *image;

     /* Toolbar data */
     Widget tools;
     Widget label, seclabel, zoomlabel, timelabel;
     Widget insertButton;
     Pixmap lowrespix;
     Pixmap highrespix;
     Pixmap zlockpix;
     Pixmap lockpix;
     Pixmap unlockpix;
     Pixmap smartcenpix;
     Pixmap keepcenpix;
     Pixmap insertAfterPix;
     Pixmap insertBeforePix;
     int section;
     int sectionStep; /* auto step image after new model point. */
     int time;
     int lock;
     int keepcentered;
     Cursor cursor;
     int mousemode;
     int popup;
     int   toolSection;
     float toolZoom;
     int   toolTime;

     short insertmode;
     short showslice;

     /* Pointer to view and control sturctures. */
     ImodView    *vi;
     int         ctrl;
     int toolstart;

     /* Special, lock time */
     Pixmap timePix;
     Pixmap timeLockPix;
     int    timeLock;
     int    twod;

}ZapWindow;

#endif
