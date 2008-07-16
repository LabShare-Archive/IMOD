/*
 *  imod_utilities.cpp - Utility functions used by windows and dialogs
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#include <stdarg.h>
#include <qcolor.h>
#include "imod.h"
#include "imod_utilities.h"
#include "b3dgfx.h"
#include "preferences.h"
#include "imod_input.h"
#include "imod_client_message.h"
#include "imod_assistant.h"
#include "dia_qtutils.h"  

/* Draw a symbol of the given type, size, and flags */
void utilDrawSymbol(int mx, int my, int sym, int size, int flags)
{
  switch (sym){
  case IOBJ_SYM_CIRCLE:
    if (flags  & IOBJ_SYMF_FILL)
      b3dDrawFilledCircle(mx, my, size);
    else
      b3dDrawCircle(mx, my, size);
    break;
  case IOBJ_SYM_SQUARE:
    if (flags  & IOBJ_SYMF_FILL)
      b3dDrawFilledSquare(mx, my, size);
    else
      b3dDrawSquare(mx, my, size);
    break;
  case IOBJ_SYM_TRIANGLE:
    if (flags  & IOBJ_SYMF_FILL)
      b3dDrawFilledTriangle(mx, my, size);
    else
      b3dDrawTriangle(mx, my, size);
    break;
  case IOBJ_SYM_STAR:
    break;
  case IOBJ_SYM_NONE:
    b3dDrawPoint(mx, my);
    break;

  default:
    return;

  }
}

/* If there are times, find longest string */
void utilGetLongestTimeString(ImodView *vi, QString *str)
{
  int maxlen, time, len, tmax;
  if (vi->nt){
    *str = " (999)";
    maxlen = -1;
    for (time = 1; time < vi->nt; time++) {
      len = strlen(ivwGetTimeIndexLabel(vi, time));
      if (len > maxlen) {
        maxlen = len;
        tmax = time;
      }
    }
    *str += ivwGetTimeIndexLabel(vi, tmax);
  }
}

/* Set the size of current and endpoint markers so that they do not conflict
   with symbol size or, if not symbols, with 3D sphere size */
void utilCurrentPointSize(Iobj *obj, int *modPtSize, int *backupSize,
                         int *imPtSize)
{
  // These two will be user preferences
  int minModSize = ImodPrefs->minCurrentModPtSize();
  int minImSize = ImodPrefs->minCurrentImPtSize();
  int symSize = 0;

  // Set sizes to minima
  *modPtSize = minModSize;
  *backupSize = minModSize + 2;
  *imPtSize = minImSize;
  if (!obj)
    return;

  // Determine an interfering symbol size
  if (obj->symbol != IOBJ_SYM_NONE && obj->symsize > 0)
    symSize = obj->symsize;
  if (!symSize && obj->pdrawsize > 0)
    symSize = obj->pdrawsize / App->cvi->xybin;

  // Make sure symbol and point sizes differ by at least 2
  if (symSize - *modPtSize < 2 && *modPtSize - symSize < 2)
    *modPtSize = symSize + 2;
  *backupSize = *modPtSize + 2;
  if (symSize - *backupSize < 2 && *backupSize - symSize < 2)
    *backupSize = symSize + 2;
  if (symSize - *imPtSize < 2 && *imPtSize - symSize < 2)
    *imPtSize = symSize + 2;
}

/* Turn on stippling if globally enabled and contour has flag set */
void utilEnableStipple(ImodView *vi, Icont *cont)
{
  if (vi->drawStipple && (cont->flags & ICONT_STIPPLED)) {
    glLineStipple(3, 0xAAAA);
    glEnable(GL_LINE_STIPPLE);
  }
}

/* Turn off stippling under same conditions */
void utilDisableStipple(ImodView *vi, Icont *cont)
{
  if (vi->drawStipple && (cont->flags & ICONT_STIPPLED))
    glDisable(GL_LINE_STIPPLE);
}

/* Clears the current window to the given color index */
void utilClearWindow(int index)
{
  glClearIndex(index);
  /* DNM: need to set clear colors for rgb mode */
  if (App->rgba) {
    QColor qcol = ImodPrefs->namedColor(index);
    glClearColor(qcol.red()/255., qcol.green()/255. , qcol.blue()/255., 0.);
  }
     
  glClear(GL_COLOR_BUFFER_BIT);
}

/*
 * Compute a Z rotation in plane from mouse movement around center
 */
float utilMouseZaxisRotation(int winx, int mx, int lastmx, int winy, int my,
                             int lastmy)
{
  float drot = 0.;
  float delCrit = 20.;
  double delx, dely, startang, endang;
  int xcen, ycen;
  xcen = winx / 2;
  ycen = winy / 2;
  delx = lastmx - xcen;
  dely = winy - 1 - lastmy - ycen;
  if (fabs(delx) > delCrit || fabs(dely) > delCrit) {
    startang = atan2(dely, delx) / RADIANS_PER_DEGREE;
    delx = mx - xcen;
    dely = winy - 1 - my - ycen;
    if (fabs(delx) > delCrit || fabs(dely) > delCrit) {
      endang = atan2(dely, delx) / RADIANS_PER_DEGREE;
      drot = endang - startang;
      if (drot < -360.)
        drot += 360.;
      if (drot > 360.)
        drot -= 360.;
    }
  }
  return drot;
}

/*
 * Sets an object flag or symflag to the given state
 */
void utilSetObjFlag(Iobj *obj, int flagType, bool state, b3dUInt32 flag)
{
  if (!obj || flagType < 0 || flagType > 1)
    return;
  if (flagType == 1) {
    if (!state)
      obj->symflags &= ~flag;
    else
      obj->symflags |= flag;
  } else {
    if (!state)
      obj->flags &= ~flag;
    else
      obj->flags |= flag;
  }
}

/* Appends either the model or file name to the window name, giving
   first priority to the model name if "modelFirst" is set */
char *imodwEithername(char *intro, char *filein, int modelFirst)
{
  char *retString;
  if (modelFirst) {
    retString = imodwGivenName(intro, filein);
    if (!retString)
      retString = imodwfname(intro);

  } else {
    retString = imodwfname(intro);
    if (!retString)
      retString = imodwGivenName(intro, filein);
  }
  return(retString);
}


/* Appends the given name to window name */
char *imodwGivenName(char *intro, char *filein)
{
  char *winame, *filename;
  int i;
     
  filename = filein;

  /* DNM: treat null name and null pointer the same */
  if (!filename || !*filename)
    return(NULL);
  for(i = 0; filein[i]; i++){
    if (filein[i] == '/'){
      filename = &(filein[i]);
      filename++;
    }
  }
  winame = (char *)malloc(strlen(filename) + strlen(intro) + 2);
  if (!winame)
    return(NULL);
  sprintf(winame, "%s %s", intro, filename);
  return(winame);
}

/* Appends image name to window names. */
char *imodwfname(char *intro)
{
  char *filename;
  filename = Imod_imagefile;

  /* DNM 7/21/02: if multiple files, output number of image files */
  if (!filename && App->cvi->nt > 1) {
    filename = (char *)malloc(20 + strlen(intro));
    if (!filename)
      return NULL;
    sprintf(filename, "%s %d image files", intro, App->cvi->nt);
    return(filename);
  }
  return (imodwGivenName(intro, filename));
}

/* Takes an intro without a :, and returns a qstring with intro: filename
   or just intro */
QString imodCaption(char *intro)
{
  QString qstr = intro;
  qstr += ":";
  char *name = imodwfname((char *)qstr.latin1());
  if (name) {
    qstr = name;
    free(name);
  } else
    qstr = intro;
  return qstr;
}

/* Takes fprintf-type arguments and gives an error message box if out is
   NULL or if under Windows; otherwise prints to file */
void imodError(FILE *out, const char *format, ...)
{
  char errorMess[512];
  va_list args;
  va_start(args, format);
  
  vsprintf(errorMess, format, args);
#ifdef _WIN32
  out = NULL;
#endif
  if (!out || (ClipHandler && ClipHandler->disconnectedFromStderr()))
    dia_err(errorMess);
  else
    fprintf(out, errorMess);
}

/* Takes an arbitrarily sized string and gives a message box on windows or
   prints to standard out otherwise */
void imodPrintInfo(const char *message)
{
#ifdef _WIN32
  bool windows = true;
#else
  bool windows = false;
#endif
  
  if (windows || (ClipHandler && ClipHandler->disconnectedFromStderr()))
    dia_puts((char *)message);
  else
    printf(message);
}

/* Takes fprintf-type arguments and prints to stderr, and flushes on Windows */
void imodPrintStderr(const char *format, ...)
{
  char errorMess[512];
  va_list args;
  va_start(args, format);
  vsprintf(errorMess, format, args);

  // Send to wprint if disconnected from stderr (THIS WILL BE A PROBLEM IF
  // WPRINT IS CHANGED TO SEND TO STDERR!)
  if (ClipHandler && ClipHandler->disconnectedFromStderr()) {
    wprint(errorMess);
    return;
  }
  fprintf(stderr, errorMess);
#ifdef _WIN32
  fflush(stderr);
#endif
}

/* Takes a message for "puts", adds newline, prints and flushes stderr */
void imodPuts(const char *message)
{
  if (ClipHandler && ClipHandler->disconnectedFromStderr()) {
    wprint("%s\n", message);
    return;
  }
  fprintf(stderr, "%s\n", message);
#ifdef _WIN32
  fflush(stderr);
#endif
}

/* Show a help page in Qt Assistant; provide a full
   path if the path is not relative to IMOD_DIR/html/3dmodHelp
   Returns 1 for error, 0 for success */
int imodShowHelpPage(const char *page)
{
  if (ImodHelp)
    return (ImodHelp->showPage(page) > 0 ? 1 : 0);
  else
    return 1;
}

/***********************************************************************
 * Core application plugin lookup functions.
 *
 */

int  imodDepth(void){ return(App->depth); }

void imodDefaultKeys(QKeyEvent *event, ImodView *vw)
{
  inputQDefaultKeys(event, vw);
}

int imodColorValue(int inColor)
{
  int pixel = 0;

  switch(inColor)
    {
    case COLOR_BACKGROUND:
      pixel = App->background; break;
    case COLOR_FOREGROUND:
      pixel = App->foreground; break;
    case COLOR_SELECT:
      pixel = App->select; break;
    case COLOR_SHADOW:
      pixel = App->shadow; break;
    case COLOR_END:
      pixel = App->endpoint; break;
    case COLOR_BEGIN:
      pixel = App->bgnpoint; break;
    case COLOR_POINT:
      pixel = App->curpoint; break;
    case COLOR_GHOST:
      pixel = App->ghost; break;
    case COLOR_MIN:
      pixel = App->cvi->rampbase; break;
    case COLOR_MAX:
      pixel =(App->cvi->rampbase + App->cvi->rampsize);break;
            
    }
  b3dColorIndex(pixel);
  return pixel;
}

/*

$Log$
Revision 1.4  2008/02/06 21:28:00  mast
Prevented writing to stdout/stderr if -L input (from etomo) has disconnected

Revision 1.3  2008/02/03 18:36:14  mast
Added function for converting mouse movement to in-plane rotation

Revision 1.2  2008/01/13 22:26:13  mast
Added clearing function

Revision 1.1  2007/12/04 18:41:51  mast
Added to get common functions out of xzap.cpp and imod.cpp


*/
