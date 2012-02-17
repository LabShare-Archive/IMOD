/*
 *  autox.h - header for autox.cpp
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  $Id$
 *  No more Log
 */

#ifndef AUTOX_H
#define AUTOX_H

  /* Resolution times 100 */
#define AUTOX_MAX_RESOLUTION  200

/* bits for autox data.  Others are defined in imodel.h */
#define AUTOX_BLACK (1 << 2)
#define AUTOX_WHITE (1 << 3)
#define AUTOX_OUT   (1 << 2)
#define AUTOX_IN    (1 << 3)
#define AUTOX_ALL   0xff


/* higher bits define seperate objects. (future) */
#define AUTOX_OBJECT(x) ((x) >> 4)
#define AUTOX_SET_OBJECT(x, o)  (((x)&(AUTOX_ALL >> 4)) | ((o) << 4) )

#define AUTOX_ALTMOUSE_DEFAULT 0   /* use default mouse controls. */
#define AUTOX_ALTMOUSE_PAINT   1   /* use mouse to paint data.    */
#define AUTOX_ALTMOUSE_DRAW    2   /* use mouse to draw edges.    */

typedef struct ViewInfo ImodView;

typedef struct imod_autox_struct
{
  ImodView  *vw;            /* image data to model                       */
  unsigned char *data;      /* storage for classification                */
  unsigned char *byteSlice; /* Storage for byte slice if ushorts loaded  */
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
int autox_open(ImodView *vw);
int autox_setlow(ImodView *vw, int x, int y);
int autox_sethigh(ImodView *vw, int x, int y);
int autox_fillmouse(ImodView *vw, int xm, int ym);
int autox_build(Autox *ax);
void autox_newsize(ImodView *vw);
int autox_next(Autox *ax);
int autox_smooth(Autox *ax);
void autoxCrampSelected(ImodView *vw);

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
