/*  IMOD VERSION 2.42
 *
 *  autox.h
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
    Revision 3.2.2.2  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 3.2.2.1  2003/01/14 21:42:44  mast
    Qt version

    Revision 3.2  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/

#ifndef AUTOX_H
#define AUTOX_H

  /* Resolution times 100 */
#define AUTOX_MAX_RESOLUTION  200

/* bits for autox data */
#define AUTOX_BLANK 0
#define AUTOX_FLOOD 1
#define AUTOX_PATCH (1 << 1)
#define AUTOX_FILL  (AUTOX_FLOOD | AUTOX_PATCH)
#define AUTOX_BLACK (1 << 2)
#define AUTOX_WHITE (1 << 3)
#define AUTOX_OUT   (1 << 2)
#define AUTOX_IN    (1 << 3)
#define AUTOX_CHECK (1 << 5)
#define AUTOX_ALL   0xff


/* higher bits define seperate objects. (future) */
#define AUTOX_OBJECT(x) ((x) >> 4)
#define AUTOX_SET_OBJECT(x, o)  (((x)&(AUTOX_ALL >> 4)) | ((o) << 4) )

#define AUTOX_ALTMOUSE_DEFAULT 0   /* use default mouse controls. */
#define AUTOX_ALTMOUSE_PAINT   1   /* use mouse to paint data.    */
#define AUTOX_ALTMOUSE_DRAW    2   /* use mouse to draw edges.    */

typedef struct imod_autox_struct
{
     struct ViewInfo  *vw;     /* image data to model                       */
     unsigned char *data;      /* storage for classification                */
     double        shave;      /* min dis. between points.                  */
     int           threshold;  /* segmentation threshold.                   */
     int           reverse;    /* segment low areas if true.                */
     int           contrast;   /* view in high/low contrast .               */
     int           filled;     /* draw segments if true.                    */
     int           cz;         /* current z value for filled data.          */
     int           altmouse;   /* Let mouse change values if true.          */
     int           xysize;     /* size at which data array was obtained     */
     int           *xlist;     /* list of x coords of pixels to check       */
     int           *ylist;     /* list of y coords of pixels to check       */
     int           listsize;   /* size of this list                         */
     int           diagonal;   /* flag to follow diagonals when filling     */
} Autox;

  /* Functions called from elsewhere in the program */
  int autox_open(struct ViewInfo *vw);
  int autox_setlow(struct ViewInfo *vw, int x, int y);
  int autox_sethigh(struct ViewInfo *vw, int x, int y);
  int autox_fillmouse(struct ViewInfo *vw, int xm, int ym);
  int autox_build(Autox *ax);
  void autox_newsize(struct ViewInfo *vw);
  int autox_next(Autox *ax);
  int autox_smooth(Autox *ax);

  /* Functions called from the form class */
  void autoxHelp();
  void autoxSlider(int which, int value);
  void autoxContrastSelected(int which);
  void autoxAltmouse(int state);
  void autoxFollowDiagonals(int state);
  void autoxClosing();
  void autoxFill();
  void autoxBuild();
  void autoxClear();
  void autoxShrink();
  void autoxExpand();
  void autoxSmooth();
  void autoxNext();

#endif /* AUTOX_H */
