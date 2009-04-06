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
#include <qicon.h>
#include <qaction.h>
#include <qtoolbutton.h>
#include <qpushbutton.h>
#include <qtoolbar.h>
#include <qbitmap.h>
#include <QBoxLayout>
#include <qsignalmapper.h>

//Added by qt3to4:
#include <QKeyEvent>
#include <QMouseEvent>
#include "imod.h"
#include "imod_utilities.h"
#include "b3dgfx.h"
#include "preferences.h"
#include "imod_input.h"
#include "imod_client_message.h"
#include "imod_assistant.h"
#include "dia_qtutils.h"  
#include "tooledit.h"
#include "arrowbutton.h"

#define TOOLBUT_SIZE 20
#define BM_WIDTH 16
#define BM_HEIGHT 16

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
    glLineStipple(3, 0x5555);
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

/*
 * Adds arrow buttons and a zoom edit box to a tool bar
 */
ToolEdit *utilTBZoomTools(QWidget *parent, QToolBar *toolBar, 
                               ArrowButton **upArrow, ArrowButton **downArrow)
{
  ToolEdit *edit;
  utilTBArrowButton(Qt::UpArrow, parent, toolBar, upArrow,
                         "Increase zoom factor");
  utilTBArrowButton(Qt::DownArrow, parent, toolBar, downArrow,
                         "Decrease zoom factor");
  utilTBToolEdit(6, parent, toolBar, &edit, "Enter a zoom factor");
  return edit;
}

/*
 * Finds the next section in the current object with a contour, given curz is
 * the current Z and dir is 1 to go forward or -1 to go back
 */
int utilNextSecWithCont(ImodView *vi, Iobj *obj, int curz, int dir)
{
  int diff, co, contz;
  int newz = curz;
  Icont *cont;
  if (!obj)
    return newz;
  newz = -1;
  for (co = 0; co < obj->contsize; co++) {
    cont = &obj->cont[co];
    if (!cont->psize)
      continue;
    contz = B3DNINT(cont->pts[0].z);
    contz = B3DMAX(0, B3DMIN(contz, vi->zsize - 1));
    diff = dir * (contz - curz);
    if (diff > 0 && (newz < 0 || diff < dir * (newz - curz)))
      newz = contz;
  }
  return newz >= 0 ? newz : curz;
}

/*
 * Adds an arrow button to a tool bar and constrains its size
 */
QAction *utilTBArrowButton(Qt::ArrowType type, QWidget *parent, 
                                QToolBar *toolBar, ArrowButton **arrow,
                                const char *toolTip)
{
  QAction *action;
  *arrow = new ArrowButton(type, parent);
  action = toolBar->addWidget(*arrow);
  (*arrow)->setAutoRaise(TB_AUTO_RAISE);
  (*arrow)->setFocusPolicy(Qt::NoFocus);
  (*arrow)->setFixedSize(TOOLBUT_SIZE, TOOLBUT_SIZE);
  if (toolTip)
    (*arrow)->setToolTip(toolTip);
  return action;
}

/*
 * Adds a tool edit to a tool bar
 */
QAction *utilTBToolEdit(int width, QWidget *parent, QToolBar *toolBar,
                             ToolEdit **edit, const char *toolTip)
{
  QAction *action;
  *edit = new ToolEdit(parent, width);
  action = toolBar->addWidget(*edit);
  (*edit)->setFocusPolicy(Qt::ClickFocus);
  (*edit)->setAlignment(Qt::AlignRight);
  if (toolTip)
    (*edit)->setToolTip(toolTip);
  return action;
}  

/*
 * Adds an tool button to a tool bar and constrains its size
 */
QAction *utilTBToolButton(QWidget *parent, QToolBar *toolBar,
                          QToolButton **button, const char *toolTip)
{
  QAction *action;
  *button = new QToolButton(parent);
  action = toolBar->addWidget(*button);
  (*button)->setAutoRaise(TB_AUTO_RAISE);
  (*button)->setFixedSize(TOOLBUT_SIZE, TOOLBUT_SIZE);
  (*button)->setFocusPolicy(Qt::NoFocus);
  if (toolTip)
    (*button)->setToolTip(toolTip);
   return action;
}  

/*
 * Adds a push button to a tool bar
 */
QAction *utilTBPushButton(const char *text, QWidget *parent, QToolBar *toolBar,
                          QPushButton **button, const char *toolTip)
{
  QAction *action;
  *button = new QPushButton(text, parent);
  action = toolBar->addWidget(*button);
  (*button)->setFocusPolicy(Qt::NoFocus);
  if (toolTip)
    (*button)->setToolTip(toolTip);
   return action;
}  

/*
 * Take two X11 bitmaps in bitList and convert to off and on icons for num
 * buttons
 */
void utilBitListsToIcons(unsigned char *bitList[][2], QIcon *icons[], int num)
{
  for (int i = 0; i < num; i++) {
    icons[i] = new QIcon();
    icons[i]->addPixmap
      (QBitmap::fromData(QSize(BM_WIDTH, BM_HEIGHT), bitList[i][0]),
       QIcon::Normal, QIcon::Off);
    icons[i]->addPixmap
      (QBitmap::fromData(QSize(BM_WIDTH, BM_HEIGHT), bitList[i][1]), 
       QIcon::Normal, QIcon::On);
  }
}

/*
 * Set up a single toggle button out of a set.  Set the parent and either add
 * it to a toolbar and return the action, or add it to the layout.  Set the
 * mapping through the mapper, set the icon, set the state to off.
 */
QAction *utilSetupToggleButton(QWidget *parent, QToolBar *toolBar, 
                               QBoxLayout *layout, QSignalMapper *mapper,
                               QIcon *icons[], char *tips[], 
                               QToolButton *buts[], int states[], int ind)
{
  QAction *action = NULL;
  buts[ind] = new QToolButton(parent);
  if (toolBar)
    action = toolBar->addWidget(buts[ind]);
  else if (layout)
    layout->addWidget(buts[ind]);
  buts[ind]->setAutoRaise(TB_AUTO_RAISE);
  buts[ind]->setFixedSize(TOOLBUT_SIZE, TOOLBUT_SIZE);
  buts[ind]->setCheckable(true);
  buts[ind]->setChecked(false);
  buts[ind]->setIcon(*icons[ind]);
  buts[ind]->setFocusPolicy(Qt::NoFocus);
  states[ind] = 0;
  mapper->setMapping(buts[ind], ind);
  if (tips)
    buts[ind]->setToolTip(tips[ind]);
  return action;
}

/*
 * Raises window if needed on Mac OS X: Qt 4.5, second or third button
 */
void utilRaiseIfNeeded(QWidget *window, QMouseEvent *event)
{
#if defined(Q_OS_MACX) && QT_VERSION >= 0x040500
  if (!(event->buttons() & Qt::LeftButton)) {
    window->raise();

    // This specific call gets the window highlighted and the parent's menu
    // reinstalled, QApplication::setActiveWindow and setFocus don't
    window->activateWindow();
  }
#endif
}

bool utilNeedToSetCursor()
{
#if defined(Q_OS_MACX) && QT_VERSION >= 0x040500
  return true;
#else
  return false;
#endif
}


/* Appends either the model or file name to the window name, giving
   first priority to the model name if "modelFirst" is set */
char *imodwEithername(const char *intro, const char *filein, int modelFirst)
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
char *imodwGivenName(const char *intro, const char *filein)
{
  char *winame;
  const char *filename;
  int i;
     
  filename = filein;

  /* DNM: treat null name and null pointer the same */
  if (!filename || !*filename)
    return(NULL);
  for(i = 0; filein[i]; i++){
    if (filein[i] == '/'){
      filename = (&(filein[i]));
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
char *imodwfname(const char *intro)
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
QString imodCaption(const char *intro)
{
  QString qstr = intro;
  qstr += ":";
  char *name = imodwfname(LATIN1(qstr));
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
Revision 1.11  2009/03/30 18:25:44  mast
Added function to handle raising on mouse event, workaround Mac Qt 4.5.0

Revision 1.10  2009/03/26 05:41:01  mast
Change nearest section function to work for an object passed as argument

Revision 1.9  2009/02/26 20:02:54  mast
Set toolbar arrows to no focus

Revision 1.8  2009/02/25 05:35:53  mast
Add function for getting next/prev Z with contours

Revision 1.7  2009/02/05 05:23:44  tempuser
Change stipple to start with on bit instead of off bit to see dense lines

Revision 1.6  2009/01/15 16:33:17  mast
Qt 4 port

Revision 1.5  2008/07/16 04:31:09  mast
Add function to set object flag

Revision 1.4  2008/02/06 21:28:00  mast
Prevented writing to stdout/stderr if -L input (from etomo) has disconnected

Revision 1.3  2008/02/03 18:36:14  mast
Added function for converting mouse movement to in-plane rotation

Revision 1.2  2008/01/13 22:26:13  mast
Added clearing function

Revision 1.1  2007/12/04 18:41:51  mast
Added to get common functions out of xzap.cpp and imod.cpp


*/
