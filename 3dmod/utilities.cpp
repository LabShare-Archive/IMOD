/*
 *  utilities.cpp - Utility functions used by windows and dialogs
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2007 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

// NOTE: purely model-related utilities go in imod_edit.cpp

#include <stdarg.h>
#include <qcolor.h>
#include <qicon.h>
#include <qdir.h>
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
#include "display.h"
#include "imodv.h"
#include "imodplug.h"
#include "utilities.h"
#include "b3dgfx.h"
#include "undoredo.h"
#include "preferences.h"
#include "imod_input.h"
#include "client_message.h"
#include "imod_assistant.h"
#include "dia_qtutils.h"  
#include "tooledit.h"
#include "arrowbutton.h"
#include "scalebar.h"

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
  if (vi->numTimes){
    *str = " (999)";
    maxlen = -1;
    for (time = 1; time < vi->numTimes; time++) {
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
bool utilEnableStipple(ImodView *vi, Icont *cont)
{
  if (vi->drawStipple && (cont->flags & ICONT_STIPPLED)) {
    glLineStipple(3, 0x5555);
    glEnable(GL_LINE_STIPPLE);
    return true;
  }
  return false;
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
 * Takes pairs of icon images in fileList and convert to off and on icons for num
 * buttons
 */
void utilFileListsToIcons(const char *fileList[][2], QIcon *icons[], int num)
{
  for (int i = 0; i < num; i++) {
    icons[i] = new QIcon();
    icons[i]->addFile(QString(fileList[i][0]), QSize(BM_WIDTH, BM_HEIGHT),
                      QIcon::Normal, QIcon::Off);
    icons[i]->addFile(QString(fileList[i][1]), QSize(BM_WIDTH, BM_HEIGHT),
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
                               QIcon *icons[], const char *tips[], 
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

bool utilCloseKey(QKeyEvent *e)
{
  if (e->key() == Qt::Key_Escape)
    return true;
#ifdef Q_OS_MACX
  if (e->key() == Qt::Key_W && (e->modifiers() & Qt::ControlModifier))
    return true;
#endif
  return false;
}

/*
 * Does common initial tasks for a montage snapshot: allocates the arrays,
 * (returns true for an error), and saves and adjusts the scale bar data 
 */
bool utilStartMontSnap(int winx, int winy, int xFullSize, int yFullSize,
                       float factor, ScaleBar &barSaved, int &numChunks,
                       unsigned char **framePix, unsigned char ***fullPix,
                       unsigned char ***linePtrs)
{
  int iy, maxLines, line, ch, ndo;
  int chunkMax = 10000000;    // This was needed to get to 2.9 GB on WinXP
  ScaleBar *barReal = scaleBarGetParams();

  maxLines = B3DMAX(1, chunkMax / (4 * xFullSize));
  numChunks = (yFullSize + maxLines - 1) / maxLines;
  *fullPix = (unsigned char **)malloc(numChunks * sizeof (unsigned char *));
  if (!*fullPix)
    return true;
  for (ch = 0; ch < numChunks; ch++)
    (*fullPix)[ch] = NULL;
  *linePtrs = NULL;
  *framePix = NULL;
  *framePix = (unsigned char *)malloc(4 * winx * winy);
  *linePtrs = (unsigned char **)malloc(yFullSize * sizeof(unsigned char *));
  if (!*framePix || !*linePtrs) {
    utilFreeMontSnapArrays(*fullPix, numChunks, *framePix, *linePtrs);
    return true;
  }
  
  line = 0;
  for (ch = 0; ch < numChunks; ch++) {
    ndo = B3DMIN(yFullSize - line, maxLines);
    (*fullPix)[ch] = (unsigned char *)malloc(ndo * 4 * xFullSize);
    if (!(*fullPix)[ch]) {
      imodPrintStderr("Failed on chunk %d, after %u\n", ch, (unsigned int)ch * ndo * 4 * xFullSize);
      utilFreeMontSnapArrays(*fullPix, numChunks, *framePix, *linePtrs);
      return true;
    }
    for (iy = 0; iy < ndo; iy++)
      (*linePtrs)[iy+line] = (*fullPix)[ch] + 4 * xFullSize * iy;
    line += ndo;
  }

  // Save and modify scale bar directives
  barSaved = *barReal;
  barReal->minLength = B3DNINT(factor * barReal->minLength);
  barReal->thickness = B3DNINT(factor * barReal->thickness);
  barReal->indentX = B3DNINT(factor * barReal->indentX);
  barReal->indentY = B3DNINT(factor * barReal->indentY);
  return false;
}

/*
 * Frees the array of chunks and the other arrays if they are allocated
 */
void utilFreeMontSnapArrays(unsigned char **fullPix, int numChunks, 
                            unsigned char *framePix, unsigned char **linePtrs)
{
  for (int i = 0; i < numChunks; i++)
    if (fullPix[i])
      free(fullPix[i]);
  free(fullPix);
  if (framePix)
    free(framePix);
  if (linePtrs)
    free(linePtrs);
}


/*
 * Manages scale bar drawing during a montage snapshot 
 */
void utilMontSnapScaleBar(int ix, int iy, int frames, int winx, int winy, 
                          float scale, bool savedDraw)
{
  ScaleBar *barReal = scaleBarGetParams();
  int barpos = barReal->position;

  // Set up for scale bar if it is the right corner
  barReal->draw = false;
  if ((((barpos == 0 || barpos == 3) && ix == frames - 1) ||
       ((barpos == 1 || barpos == 2) && !ix)) &&
      (((barpos == 2 || barpos == 3) && iy == frames - 1) ||
       ((barpos == 0 || barpos == 1) && !iy))) {
    barReal->draw = savedDraw;
    scaleBarTestAdjust(winx, winy, scale);
  }
}

/*
 * Performs final tasks for a montage snapshot: sets up line pointers, composes
 * the filename, and performs the right kind of snapshot.
 */
void utilFinishMontSnap(unsigned char **linePtrs,
                        int xFullSize, int yFullSize, int format, int &fileno,
                        int digits, float zoom, const char *prefix, const char *message)
{
  int limits[4];
  QString fname, sname;

  limits[0] = limits[1] = 0;
  limits[2] = xFullSize;
  limits[3] = yFullSize;
  if (format == 2)
    ImodPrefs->set2ndSnapFormat();
  b3dSetDpiScaling(zoom);
  fname = b3dGetSnapshotName(prefix, format ? SnapShot_RGB : SnapShot_TIF, 
                             digits, fileno);
  sname = b3dShortSnapName(fname);
  imodPrintStderr("%s montage to %s", message, LATIN1(sname));
  if (format)
    b3dSnapshot_NonTIF(fname, 4, limits, linePtrs);
  else
    b3dSnapshot_TIF(fname, 4, limits, linePtrs, strcmp(prefix, "modv") != 0);
  if (format == 2)
    ImodPrefs->restoreSnapFormat();
  imodPuts("");
  b3dSetDpiScaling(1.);
}

// Returns a zoom-dependent for scaling a scroll wheel delta to a point size change
float utilWheelToPointSizeScaling(float zoom)
{
  float wheelScale = 1./1200.f;
  if (zoom < 4. && zoom >= 2.)
    wheelScale *= 2.;
  else if (zoom < 2. && zoom > 1.)
    wheelScale *= 3.;
  else if (zoom == 1.)
    wheelScale *= 4.;
  else if (zoom < 1.)
    wheelScale *= 5.;
  return wheelScale;
}

// Changes the size of the current point given the zoom and scroll wheel delta value
void utilWheelChangePointSize(ImodView *vi, float zoom, int delta)
{
  int ix, iy, pt;
  Iobj *obj;
  Icont *cont;
  float size;
  imodGetIndex(vi->imod, &ix, &iy, &pt);
  obj = imodObjectGet(vi->imod);
  cont = imodContourGet(vi->imod);
  if (!cont || pt < 0)
    return;
  size = imodPointGetSize(obj, cont, pt);
  if (!size && (!cont->sizes || (cont->sizes && cont->sizes[pt] < 0)))
    return;
  size += delta * utilWheelToPointSizeScaling(zoom);
  size = B3DMAX(0., size);
  vi->undo->contourDataChg();
  imodPointSetSize(cont, pt, size);
  vi->undo->finishUnit();
  imodDraw(vi, IMOD_DRAW_MOD);
}

/* Converts a flipped model to a rotated one if direction is FLIP_TO_ROTATION, or
 * a rotated model to a flipped one if direction is ROTATION_TO_FLIP, otherwise
 * does nothing */
void utilExchangeFlipRotation(Imod *imod, int direction)
{
  if ((direction == FLIP_TO_ROTATION && !(imod->flags & IMODF_FLIPYZ)) ||
      (direction == ROTATION_TO_FLIP && !(imod->flags & IMODF_ROT90X)))
    return;
  imodInvertZ(imod);
  setOrClearFlags(&imod->flags, IMODF_ROT90X, direction == FLIP_TO_ROTATION ? 1 : 0);
  setOrClearFlags(&imod->flags, IMODF_FLIPYZ, direction == ROTATION_TO_FLIP ? 1 : 0);
}


Icont *utilAutoNewContour(ImodView *vi, Icont *cont, bool notInPlane, bool timeMismatch,
                          int timeLock, int setSurface, const char *planeText,
                          const char *surfText)
{
  Iobj *obj = imodObjectGet(vi->imod);
  const char *baseMess = "Started a new contour even though last contour had only 1 pt. ";
  if (cont->psize == 1) {
    if (notInPlane && iobjClose(obj->flags))
      wprint("\a%s Use open contours to model across %s.\n", baseMess, planeText);
    else if (notInPlane)
      wprint("\a%s Turn off \"Start new contour at new Z\" to model "
             "across %s.\n", baseMess, planeText);
    else
      wprint("\a%s Set contour time to 0 to model across times.\n", baseMess);
  }
  inputNewContourOrSurface(vi, setSurface, timeLock);
  cont = imodContourGet(vi->imod);
  if (!cont)
    return NULL;
  if (setSurface == INCOS_NEW_SURF)
    wprint("Started new surface # %d due to change in %s\n", cont->surf, surfText);
  return cont;
}

// If a new surface is required, get one; in any case assign newSurf to cont
void utilAssignSurfToCont(ImodView *vi, Iobj *obj, Icont *cont, int newSurf)
{
  if (newSurf != cont->surf) {
    if (newSurf < 0) {
      vi->undo->objectPropChg();
      newSurf = imodel_unused_surface(obj);
      obj->surfsize = B3DMAX(newSurf, obj->surfsize);
      if (newSurf)
        wprint("Started new surface # %d for modeling in this plane\n", newSurf);
    }
    vi->undo->contourPropChg();
    cont->surf = newSurf;
  }
}

static GLUtesselator *sTessel = NULL;
static int sTessError;

#ifndef GLU_CALLBACK
#define GLU_CALLBACK GLvoid (*)()
#endif

static void tessError(GLenum error)
{
  sTessError = error;
  /*
    imodPrintStderr("gluError %d: %s\n", error,
    gluErrorString(error));
  */
}

void setupFilledContTesselator()
{
  if (sTessel)
    return;
  sTessel = gluNewTess();
  gluTessCallback(sTessel, GLU_BEGIN, (GLU_CALLBACK)glBegin);
  gluTessCallback(sTessel, GLU_VERTEX, (GLU_CALLBACK)glVertex3fv);
  gluTessCallback(sTessel, GLU_END, (GLU_CALLBACK)glEnd);
  /*        gluTessCallback(sTessel, GLU_EDGE_FLAG, glEdgeFlag); */
  gluTessCallback(sTessel, GLU_ERROR, (GLU_CALLBACK)tessError);
}

void drawFilledPolygon(Icont *cont)
{
  GLdouble v[3];
  Ipoint *pts;
  int pt;
  int psize;
  int ptstr, ptend;

  sTessError = 0;
  ptend = cont->psize;
  ptstr = 0;
  pts = cont->pts;
  /* imodPrintStderr(".%d-%d", co, ptend);
     fflush(stdout); */
  if (ptend) {

    psize = ptend + 1;
    do {
      sTessError = 0;
      psize--;
      
      if (psize - ptstr < 3)
        break;
      
      gluTessBeginPolygon(sTessel, NULL);
      gluTessBeginContour(sTessel);
      for (pt = ptstr; pt < psize; pt++) {
        v[0] = pts[pt].x;
        v[1] = pts[pt].y;
        v[2] = pts[pt].z;
        gluTessVertex(sTessel, v, &(pts[pt]));
      }
      gluTessEndContour(sTessel);
      gluTessEndPolygon(sTessel);
      
      if ((!sTessError) && ((psize - ptend) > 3)) {
        gluTessBeginPolygon(sTessel, NULL);
        gluTessBeginContour(sTessel);
        for (pt = psize; pt < ptend; pt++) {
          v[0] = pts[pt].x;
          v[1] = pts[pt].y;
          v[2] = pts[pt].z;
          gluTessVertex(sTessel, v, &(pts[pt]));
        }
        gluTessEndContour(sTessel);
        gluTessEndPolygon(sTessel);
      }
      
    } while(sTessError);
  }
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

/* Sets the window title of a model view dialog */
void setModvDialogTitle(QWidget *dia, const char *intro)
{
  QString qstr;
  char *window_name;
  int ind;
  window_name = imodwEithername(intro, Imodv->imod->fileName, 1);
  qstr = window_name;
  if (window_name)
    free(window_name);
  if (qstr.isEmpty()) {
    qstr = intro;
    ind = qstr.lastIndexOf(':');
    if (ind > 0)
      qstr = qstr.left(ind);
  }
  dia->setWindowTitle(qstr);
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
  if (!filename && App->cvi->numTimes > 1) {
    filename = (char *)malloc(20 + strlen(intro));
    if (!filename)
      return NULL;
    sprintf(filename, "%s %d image files", intro, App->cvi->numTimes);
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

/* Prints the message if the debug key is set */
void imodTrace(char key, const char *format, ...)
{
  if (!imodDebug(key))
    return;
  char errorMess[512];
  va_list args;
  va_start(args, format);
  vsprintf(errorMess, format, args);
  imodPuts(errorMess);
  va_end(args);
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

/*
 * Prints a measurement with optional conversion to units 
 */
void utilWprintMeasure(QString &baseMess, Imod *imod, float measure, bool area)
{
  measure *= imod->pixsize;
  if (area)
    measure *= imod->pixsize;
  if (strcmp("pixels", imodUnits(imod)))
    wprint("%s, %g %s%s\n", LATIN1(baseMess), measure, imodUnits(imod), area ? "^2" : "");
  else
    wprint("%s\n", LATIN1(baseMess));
}

/*
 * A replacement for diaOpenFileName that can go to a chooser plugin.
 * Documented in imodview.h
 */
QString utilOpenFileName(QWidget *parent, const char *caption, int numFilters,
                         const char *filters[])
{
  QString qname = QString(Dia_title) + QString(": ") + QString(caption);
  QString filter;
  for (int i = 0; i < numFilters; i++)
    filter += QString(filters[i]) + QString(";;");
  filter += QString("All Files (*)");

  // Qt 4.4 on Mac required explicit directory entry or it went to last dir
  qname = imodPlugGetOpenName(parent, qname, QDir::currentPath(), filter);
  return qname;
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
    case COLOR_ARROW:
      pixel = App->arrow; break;
    case COLOR_MIN:
      pixel = App->cvi->rampbase; break;
    case COLOR_MAX:
      pixel =(App->cvi->rampbase + App->cvi->rampsize);break;
            
    }
  b3dColorIndex(pixel);
  return pixel;
}
