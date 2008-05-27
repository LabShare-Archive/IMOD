/*
 *  autox.h - header for autox.cpp
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  $Id$
 *  Log at end of file
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
  float         threshUsed; /* Threshold to pass to contour generator    */
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


/*

  $Log$
  Revision 3.3  2003/02/10 20:41:54  mast
  Merge Qt source
  
  Revision 3.2.2.2  2003/01/27 00:30:07  mast
  Pure Qt version and general cleanup
  
  Revision 3.2.2.1  2003/01/14 21:42:44  mast
  Qt version
  
  Revision 3.2  2002/12/01 15:34:41  mast
  Changes to get clean compilation with g++

*/
