/*
 *  beadfix.cpp -- Special module for fixing fiducial models
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 * $Id$
 */                                                                           

/* include needed Qt headers and imod headers
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qspinbox.h>
#include <QDoubleSpinBox>
#include <qradiobutton.h>
#include <qtooltip.h>
#include <qtoolbutton.h>
#include <QButtonGroup>
#include <QGroupBox>
#include <qlabel.h>
#include <qlayout.h>
#include <qdir.h>
#include <qstringlist.h>
#include <qfile.h>
#include <qslider.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <QPixmap>
#include <QMouseEvent>
#include <QCloseEvent>
#include <QKeyEvent>
#include <QEvent>


/*#include "../../imod/imod.h"
#include "../../imod/imodplug.h"
#include "../../imod/control.h" */

// To make internal, 
// 1) change from imodplugin.h (or whatever that ends up as) 
// to imod.h and control.h
//#include "imodplugin.h"
#include "parse_params.h"
#include "sliceproc.h"
#include "cfft.h"
#include "imod.h"
#include "client_message.h"
#include "control.h"
#include "dia_qtutils.h"
#include "beadfix.h"
#include "tooledit.h"
#include "multislider.h"
#include "pegged.xpm"
#include "unpegged.xpm"
#include "imod_input.h"
#include "info_cb.h"
#include "imod_edit.h"
#include "mv_objed.h"
#include "preferences.h"
#include "undoredo.h"
#include "xzap.h"

// 2) Declare the internal functions as static
// And set them into the member variables in the constructor
static const char *imodPlugInfo(int *type);
static int imodPlugKeys(ImodView *vw, QKeyEvent *event);
static void imodPlugExecute(ImodView *inImodView);
static void imodPlugExecuteType(ImodView *inImodView, int type, int reason);
static  int imodPlugExecuteMessage(ImodView *vw, QStringList *strings,
                                   int *arg);
static int imodPlugMouse(ImodView *vw, QMouseEvent *event, float imx,
                         float imy, int but1, int but2, int but3);

enum {SEED_MODE = 0, GAP_MODE, RES_MODE, CONT_MODE};

BeadFixerModule::BeadFixerModule()
{
  mInfo = imodPlugInfo;
  mExecuteType = imodPlugExecuteType;
  mExecute = imodPlugExecute;
  mExecuteMessage = imodPlugExecuteMessage;
  mKeys = imodPlugKeys;
  mMouse = imodPlugMouse;
}

#define MAXLINE 100
#define MAX_DIAMETER 50
#define MAX_OVERLAY 20
#define NUM_SAVED_VALS 16

/*
 * Static variables for resident plugin data
 */
static ImodView    *sView = NULL;
static BeadFixer *sWindow = NULL;
static int    sLeft = 0;
static int    sTop  = 0;                     /* Last window position */
static int    sAutoCenter = 0;
static int    sLightBead = 0;
static int    sDiameter = 10;
static int    sOverlayOn = 0;
static int    sOverlaySec = 4;
static int    sShowMode = 0;
static int    sReverseOverlay = 0;
static int    sAutoNewCont = 0;
static int    sDelOnAllSec = 0;
static int    sDelInAllObj = 0;
static int    sIgnoreSkips = 1;
static int    sSkipLowWeight = 1;
static float  sWeightThresh = 0.1;
static int    sLastMoveGlobal = 0;
static int    sLastMoveAllAll = 0;
static char   *sSkipList = NULL;
static char   *sFilename = NULL;

#define ERROR_NO_IMOD_DIR -64352


/*
 * Called by the imod plugin load function. 
 */
const char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE + 
      IMOD_PLUG_MOUSE;
  return("Bead Fixer");
}

/*
 *  Grab hotkey input. return 1 if we handle the key.
 */
int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  int keysym;
  int    keyhandled = 0;
  int    ctrl;
  int    shift;
  Ipoint mpt;

  /*
   * Don't grab keys if plug window isn't open.
   */
  if (!sView)
    return 0;
    
  /* The keysym values are Key_A ...
   * Key_Space, Key_Comma...
   */
  keysym = event->key();

  /*
   * Modifier key mask.  Set ctrl and shift to true
   * if the coresponding key is pressed.
   */
  ctrl   = event->modifiers() & Qt::ControlModifier;
  shift  = event->modifiers() & Qt::ShiftModifier;
    
    
  switch(keysym){
  case Qt::Key_Apostrophe: 
    if (sShowMode != RES_MODE && sShowMode != CONT_MODE)
      break;
    keyhandled = 1;
    if (sShowMode == RES_MODE)
      sWindow->nextRes();
    else
      sWindow->nextCont();
    break;
  case Qt::Key_QuoteDbl: 
    if (sShowMode != RES_MODE && sShowMode != CONT_MODE)
      break;
    keyhandled = 1;
    if (sShowMode == RES_MODE)
      sWindow->backUp();
    else
      sWindow->backUpCont();
    break;
  case Qt::Key_Space:
    if (sShowMode != GAP_MODE)
      break;
    keyhandled = 1;
    sWindow->nextGap();
    break;
  case Qt::Key_Semicolon:
    if (sShowMode != RES_MODE)
      break;
    keyhandled = 1;
    sWindow->movePoint();
    break;
  case Qt::Key_Colon:
    if (sShowMode != RES_MODE)
      break;
    keyhandled = 1;
    sWindow->mIteratingMoveAll = -1;
    if (ctrl)
      sWindow->moveAllAll();
    else
      sWindow->moveAllSlot();
    break;
  case Qt::Key_U:
    if (sShowMode != RES_MODE)
      break;
    keyhandled = 1;
    sWindow->undoMove();
    break;
  case Qt::Key_Slash:
    if (sShowMode != SEED_MODE)
      break;
    keyhandled = 1;
    sOverlayOn = 1 - sOverlayOn;
    diaSetChecked(sWindow->overlayBox, sOverlayOn != 0);
    sWindow->overlayToggled(sOverlayOn != 0);
    break;
  case Qt::Key_Insert:
    if ((event->modifiers() & Qt::KeypadModifier) && ivwGetMovieModelMode(vw)
        && !ivwGetTopZapMouse(sView, &mpt))
      keyhandled = sWindow->insertPoint(mpt.x, mpt.y, true);
    break;
  default:
    break;
  }
  return keyhandled;
}

static int imodPlugMouse(ImodView *vw, QMouseEvent *event, float imx,
                         float imy, int but1, int but2, int but3)
{
  int handled = 0;

  // Reject event if window not open or not in model mode
  if (!sView || !ivwGetMovieModelMode(vw))
    return 0;

  // insert point (potentially) for middle button,
  // Modify point for shift right if autocenter on
  if (event->type() == QEvent::MouseButtonPress && but2 != 0)
    handled = sWindow->insertPoint(imx, imy, false);
  else if (event->type() == QEvent::MouseButtonPress && but3 != 0 &&
           (event->modifiers() & Qt::ShiftModifier) && sAutoCenter && 
           sShowMode != RES_MODE)
    handled = sWindow->modifyPoint(imx, imy);
  return handled;
}

/*
 *  Execute any function or program you wish with this function.
 *  Here we open up a window for user interaction.
 *  see imodplug.h for a list of support functions.
 */

#define loadSaved(a,b) if (nvals > (b)) (a) = (int)(savedValues[(b)] + 0.01);

void imodPlugExecute(ImodView *inImodView)
{
  double savedValues[NUM_SAVED_VALS];
  int nvals;
  static int firstTime = 1;

  if (sWindow){
    /* 
     * Bring the window to the front if already open.
     */
    sWindow->raise();
    return;
  }

  sView = inImodView;

  /* 
   * Initialize data. 
   */
  sFilename = NULL;
  if (firstTime) {
    nvals = ImodPrefs->getGenericSettings("BeadFixer", savedValues, 
                                          NUM_SAVED_VALS);
    loadSaved(sAutoCenter, 2);
    loadSaved(sDiameter, 3);
    loadSaved(sLightBead, 4);
    if (nvals > 7)
      sOverlaySec = (int)(savedValues[5] + 0.01);
    loadSaved(sShowMode, 7);
    loadSaved(sReverseOverlay, 8);
    loadSaved(sAutoNewCont, 9);
    loadSaved(sDelOnAllSec, 10);
    loadSaved(sDelInAllObj, 11);
    loadSaved(sIgnoreSkips, 12);
    if (nvals > 13)
      sDiameter = B3DNINT(sDiameter * savedValues[13] / 
                               sView->xybin);
    loadSaved(sSkipLowWeight, 14);
    if (nvals > 15) 
      sWeightThresh = savedValues[15];
  }

  /*
   * This creates the plug window.
   */
  sWindow  = new BeadFixer(imodDialogManager.parent(IMOD_DIALOG),
                                "bead fixer");

  imodDialogManager.add((QWidget *)sWindow, IMOD_DIALOG);

  // Get window position from settings the first time
  if (!firstTime) {
    sWindow->move(sLeft, sTop);
  } else if (nvals >= 2) {
    sLeft = (int)savedValues[0];
    sTop = (int)savedValues[1];
    diaLimitWindowPos(sWindow->width(), sWindow->height(), sLeft, sTop);
    sWindow->move(sLeft, sTop);
  }
  firstTime = 0;

  adjustGeometryAndShow((QWidget *)sWindow, IMOD_DIALOG);
  sWindow->fixSize();
}

/* Execute other specific commands (update for model change) */
void imodPlugExecuteType(ImodView *inImodView, int type, int reason)
{
  if (reason == IMOD_REASON_MODUPDATE && sWindow)
    sWindow->modelUpdate();
  if (reason == IMOD_REASON_NEWMODEL && sWindow)
    sWindow->mLastob = -1;
}

/* Execute the message in the strings.
   Keep the action definitions in client_message.h */

int imodPlugExecuteMessage(ImodView *vw, QStringList *strings, int *arg)
{
  if (sWindow)
    return sWindow->executeMessage(strings, arg);
  return 0;
}

int BeadFixer::executeMessage(QStringList *strings, int *arg)
{
  int mode;
  int action = (*strings)[*arg].toInt();

  // If window not open or no filename, need to just ignore this message
  if (action == MESSAGE_BEADFIX_REREAD && (!sWindow || !sFilename))
    return 0;
  if (!sWindow)
    return 1;

  switch (action) {
  case MESSAGE_BEADFIX_OPENFILE:

    // The message is really to open if not open, so etomo can send it always
    // without reopening inappropriately
    if (sFilename) {
      ++(*arg);
      return sWindow->reread();
    }
    return sWindow->openFileByName(LATIN1((*strings)[++(*arg)]));
  case MESSAGE_BEADFIX_REREAD:
    if (!sFilename)
      return 0;
    return sWindow->reread();
  case MESSAGE_BEADFIX_SEEDMODE:
    sAutoNewCont = (*strings)[++(*arg)].toInt();
    diaSetChecked(seedModeBox, sAutoNewCont != 0);
    return 0;
  case MESSAGE_BEADFIX_AUTOCENTER:
    sAutoCenter = (*strings)[++(*arg)].toInt();
    diaSetChecked(autoCenBox, sAutoCenter != 0);
    return 0;
  case MESSAGE_BEADFIX_DIAMETER:
    sDiameter = (*strings)[++(*arg)].toInt();

    // Divide the diameter by the binning
    if (sView->xybin > 1)
      sDiameter = B3DNINT((float)sDiameter / sView->xybin);
    diaSetSpinBox(diameterSpin, sDiameter);
    return 0;
  case MESSAGE_BEADFIX_OPERATION:
    mode = (*strings)[++(*arg)].toInt();
    mode = B3DMIN(3, B3DMAX(0, mode));
    diaSetGroup(modeGroup, mode);
    modeSelected(mode);
    return 0;
  case MESSAGE_BEADFIX_SKIPLIST:
    sWindow->newSkipList((*strings)[++(*arg)]);
    sWindow->skipEdit->setText((*strings)[*arg]);
    return 0;
  case MESSAGE_BEADFIX_CLEARSKIP:
    sWindow->newSkipList(QString(""));
    sWindow->skipEdit->setText("");
    return 0;
  case MESSAGE_BEADFIX_DELALLSEC:
    sDelOnAllSec = (*strings)[++(*arg)].toInt();
    diaSetChecked(delAllSecBut, sDelOnAllSec != 0);
    return 0;
  }
  return 1;
}

/* Open a tiltalign log file to find points with big residuals */

void BeadFixer::openFile()
{
  QString qname;
  const char *filter[] = {"Align log files (align*.log)", "Log files (*.log)"};
  int firstFilt = 0;
#if defined(Q_OS_MACX) && QT_VERSION >= 0x040500 && QT_VERSION < 0x040805
  firstFilt = 1;
#endif
  qname  = utilOpenFileName(this, "Select Tiltalign log file", 2 - firstFilt,
                            &filter[firstFilt]);
  
  if (qname.isEmpty())
    return;
  openFileByName(LATIN1(qname));
}
 
/* Open the log file with the given name, returning 1 if error */
 
int BeadFixer::openFileByName(const char *filename)
{
  if (sFilename != NULL)
    free(sFilename);
  sFilename = strdup(filename);
  
  if (reread())
    return 1;

  rereadBut->setEnabled(true);    
  runAlignBut->setEnabled(true);
  return 0;
}


/* Read or reread the tiltalign log file whose name was already obtained */
/* Return -1 if there is an error opening file, or 1 if there is a memory error
   and the window will close */
int BeadFixer::reread()
{
  char line[MAXLINE];
  char *arealine;
  int newstyle, oldstyle = 0;
  int found = 0;
  int gotHeader = 0;
  int inpt, i, ob, co, ob2, co2;
  int numToSee = 0;
  bool hasWeights, skipLow;
  double baseval;
  int binning = sView->xybin;
  float tmp1, tmp2, tmp3, resid;
  Imod *imod = ivwGetModel(sView);
  FILE   *fp;
  ResidPt *rpt;
  QString globalResLine, localResLine, tip;
  QStringList strList;

  mLastLogError = "";

  if (sFilename == NULL) 
    return -1;

  // Initialize the extra object
  if (mExtraObj > 0) {
    Iobj *xobj = ivwGetAnExtraObject(sView, mExtraObj);
    if (xobj) {
      ivwClearAnExtraObject(sView, mExtraObj);
      imodObjectSetColor(xobj, 1., 0., 0.);
      imodObjectSetValue(xobj, IobjFlagClosed, 0);
    }
  }

  fp = fopen(sFilename, "r");
  if(fp == NULL) {
    wprint("\aError opening file!\n");
    return -1;
  }

  // Zero out the temporary value entries in all contours
  for (ob = 0; ob < imod->objsize; ob++)
    for (co = 0; co < imod->obj[ob].contsize; co++)
      imod->obj[ob].cont[co].tempVal = 0.;

  wprint("Reading log file...");
         
  mNumAreas = 0;
  mNumResid = 0;
  mCurrentRes = -1;
  mIndlook = -1;
  mLastContRes = -1.;
  mContResSum = mContResSumsq = 0.;
  mNumContRes = 0;
  mMaxContRes = -1.;
  mContResReported = false;
  mHasWeights = false;

  // Outer loop searching for lines at top of residuals
  while (fgets(line, MAXLINE, fp) != NULL) {
    if (PipStartsWith(line, " Residual err") || 
        PipStartsWith(line, "Residual err")) {
      if (strstr(line, "Residual error mean and sd:") &&
          globalResLine.isEmpty()) {
        strList = QString(line).split(" ", QString::SkipEmptyParts);
        if (strList.size() > 5)
          globalResLine = "Global mean residual: " + strList[5];
      }
    }

    // Look for ERROR: lines
    if (PipStartsWith(line, "ERROR:"))
      mLastLogError += QString(line);

    // Look for 3D coordinates with contour mean residuals
    if (strstr(line, " Z ") != NULL && strstr(line, " obj ") != NULL && 
        strstr(line, "mean resid") != NULL) {
      while (fgets(line, MAXLINE, fp) != NULL) {
        if (strlen(line) < 3)
          break;
        sscanf(line, "%d %f %f %f %d %d %f", &i, &tmp1, &tmp2, &tmp3, &ob, &co,
               &resid);
        if (ob > 0 && ob <= imod->objsize && co > 0 && 
            co <= imod->obj[ob-1].contsize)
          imod->obj[ob-1].cont[co-1].tempVal = resid;
      }

      // Break ties so that contours have unique values, get statistics
      for (ob = 0; ob < imod->objsize; ob++) {
        for (co = 0; co < imod->obj[ob].contsize; co++) {
          baseval = imod->obj[ob].cont[co].tempVal;
          if (baseval > 0.) {
            mMaxContRes = B3DMAX(mMaxContRes, baseval);
            mLastContRes = mMaxContRes + 1.;
            mContResSum += baseval;
            mContResSumsq += baseval * baseval;
            mNumContRes++;
            i = 1;
            for (ob2 = 0; ob2 < imod->objsize; ob2++)
              for (co2 = 0; co2 < imod->obj[ob2].contsize; co2++)
                if (imod->obj[ob2].cont[co2].tempVal == baseval)
                  imod->obj[ob2].cont[co2].tempVal += (i++) * 0.00001;
          }
        }
      }

    }

    // Look for lines at start of residuals
    newstyle = strstr(line,"   #     #     #      X         Y        X") != NULL;
    if (!newstyle)
      oldstyle = strstr(line,"   #     #      X         Y        X") != NULL;
    if (newstyle || oldstyle) {
      mObjcont = newstyle;
      gotHeader = 1;
      hasWeights = strstr(line, "Weight") != NULL;
      mHasWeights = mHasWeights || hasWeights;
      skipLow = mHasWeights && sSkipLowWeight;

      // Allocate area data now
      if (mNumAreas >= mAreaMax) {
        if (mAreaMax)
          mAreaList = (AreaData *)realloc
            (mAreaList, (mAreaMax + 10) * sizeof(AreaData));
        else
          mAreaList = (AreaData *)malloc(10 * sizeof(AreaData));
        mAreaMax += 10;
        if (!mAreaList) {
          wprint("\aMemory error in bead fixer!\n");
          fclose(fp);
          close();
          return 1;
        }
      }
      
      // Set up global area 
      if (!mNumAreas) {
        mAreaList[0].areaX = 0;
        mAreaList[0].areaY = 0;
        mAreaList[0].firstPt = 0;
        mAreaList[0].numPts = 0;
        mNumAreas = 1;
      }

      // Next, loop on residual entries until a short (blank) line
      while (fgets(line, MAXLINE, fp) != NULL) {
        if (strlen(line) < 3)
          break;

        // Allocate residual memory
        if (mNumResid >= mResidMax) {
          if (mResidMax)
            mResidList = (ResidPt *)realloc
              (mResidList, (mResidMax + 100) * sizeof(ResidPt));
          else
            mResidList = (ResidPt *)malloc(100 * sizeof(ResidPt));
          mResidMax += 100;
        }

        if (!mResidList) {
          wprint("\aMemory error in bead fixer!");
          fclose(fp);
          close();
          return 1;
        }

        // Read and store data
        rpt = &(mResidList[mNumResid++]);
        if (mObjcont)
          sscanf(line, "%d %d %d %f %f %f %f %f %f", 
                 &rpt->obj, &rpt->cont, &rpt->view, &rpt->xcen, &rpt->ycen, 
                 &rpt->xres, &rpt->yres, &rpt->sd, &rpt->weight);
        else {
          sscanf(line, "%d %d %f %f %f %f %f", 
                 &inpt, &rpt->view, &rpt->xcen, &rpt->ycen, 
                 &rpt->xres, &rpt->yres, &rpt->sd);
          rpt->obj = 1;
          rpt->cont = inpt;
        }
        if (!mHasWeights)
          rpt->weight = 1.;

        // Adjust for binning
        rpt->xcen /= binning;
        rpt->ycen /= binning;
        rpt->xres /= binning;
        rpt->yres /= binning;
        rpt->lookedAt = 0;
        mAreaList[mNumAreas - 1].numPts++;
        rpt->area = mNumAreas - 1;

        // Need to count up point to see either if looking once or skipping low weights
        if (skipLow || mLookonce) {
          if (!skipLow || rpt->weight > sWeightThresh) {

            // If examine once is on, see if point is on looked list or new list
            if (mLookonce) {
              found = 0;
              for (i = 0; i < mNumLooked && !found; i++)
                if (rpt->obj == mLookedList[i].obj && 
                    rpt->cont == mLookedList[i].cont &&
                    rpt->view == mLookedList[i].view)
                  found = 1;
              for (i = 0; i < mNumResid - 1 && !found; i++)
                if (rpt->obj == mResidList[i].obj && 
                    rpt->cont == mResidList[i].cont &&
                    rpt->view == mResidList[i].view && 
                    (!skipLow || mResidList[i].weight > sWeightThresh))
                  found = 1;
              if (!found)
                numToSee++;
            } else
              numToSee++;
          }
        }
      }

      // Now look for another local area
      found = 0;
      while (!found && fgets(line, MAXLINE, fp) != NULL) {
        arealine = strstr(line,"Doing local area");
        if (arealine) {
          arealine[22]=0x00;
          found = 1;
          
          sscanf(&arealine[16], "%d %d", &mAreaList[mNumAreas].areaX, 
                 &mAreaList[mNumAreas].areaY);
          mAreaList[mNumAreas].numPts = 0;
          mAreaList[mNumAreas++].firstPt = mNumResid;
        }
        if (strstr(line, "Residual error local mean:")) {
          strList = QString(line).split(" ", QString::SkipEmptyParts);
          if (strList.size() > 8)
            localResLine = "Local mean residual: " + strList[4] + " (" + 
              strList[6] + " - " + strList[8].trimmed() + ")";
        }
      }

      // If none found, this breaks the top loop scanning for residual top line
      if (!found)
        break;
    }
  }
  fclose(fp);

  setCurArea(0);
  moveAllBut->setText(mNumAreas > 1 ? "Move All in Local Area" :
                      "Move All by Residual");
  
  tip = "Move all remaining points in current area by residual - Hot key: colon";
  if (mNumAreas == 1)
    tip += " - OR Shift click to iterate";
  moveAllBut->setToolTip(QString(tip));
  nextResBut->setEnabled(mNumResid);
  movePointBut->setEnabled(false);    
  backUpBut->setEnabled(false);    
  nextContBut->setEnabled(mNumContRes);
  backContBut->setEnabled(false);    
  delContBut->setEnabled(false);
  skipLowWgtBox->setEnabled(mHasWeights);
  wgtThreshSpin->setEnabled(mHasWeights && sSkipLowWeight != 0);
  wgtThreshLabel->setEnabled(mHasWeights && sSkipLowWeight != 0);

  if (!globalResLine.isEmpty())
    wprint("\n%s\n", LATIN1(globalResLine));
  if (!localResLine.isEmpty())
    wprint("%s\n", LATIN1(localResLine));
  if (!gotHeader)
    wprint("\aResidual data not found\n");
  else if (!mLookonce && !skipLow)
    wprint(" %d total residuals.\n", mNumResid);
  else
    wprint(" %d total residuals, %d to examine.\n", mNumResid, numToSee);
  reportContRes();
  manageDoneLabel();
  return 0;
}

void BeadFixer::reportContRes()
{
  float avg, sd;
  if (sFilename == NULL || sShowMode != CONT_MODE || 
      mContResReported)
    return;
  if (mNumContRes) {
    sumsToAvgSDdbl(mContResSum, mContResSumsq, mNumContRes, 1, &avg, &sd);
    wprint("%d contour mean residuals:\n Average %.2f  SD %.2f  Max %.2f\n",
           mNumContRes, avg, sd, mMaxContRes);
  } else
    wprint("\aNo contour mean residual data found.\n");
  mContResReported = true;
}


// Set current area, manage next local set button enabling and text
void BeadFixer::setCurArea(int area)
{
  mCurArea = area;
  nextLocalBut->setEnabled(mCurArea < mNumAreas - 1);
  nextLocalBut->setText(mCurArea ? 
                        "Go to Next Local Set" : "Go to First Local Set");
  moveAllBut->setEnabled(mCurArea > 0 || mNumAreas == 1);
  moveAllAllBut->setEnabled(mCurArea > 0);
}

/* Jump to the next point with a big residual */

void BeadFixer::nextRes()
{
  int inobj, incont, inview, curpt, obj, nobj, cont, ncont, ipt, npnt;
  int obsav, cosav, ptsav, i, j, numToSee;
  int found;
  float  xr, yr, resval, dx, dy;
  Iobj *ob = NULL;
  Icont *con;
  Ipoint *pts;
  Ipoint tpt;
  float headLen = 2.5;
  ResidPt *rpt;
  bool belowThresh;
  bool outputResid = false;
  Imod *imod = ivwGetModel(sView);

  // Copy and reset the bell flag
  int bell = mBell;
  mBell = 0;

  ivwControlActive(sView, 0);

  mIndlook = -1;
  undoMoveBut->setEnabled(false);
  movePointBut->setEnabled(false);
  if (!mNumResid || mCurrentRes >= mNumResid)
    return;


  // Coming into here, currentRes points to the last residual if any
  do {
    mCurrentRes++;
    if (mCurrentRes >= mNumResid) {
      if (mMovingAll && mIteratingMoveAll <= 0)
        wprint("Moved %d points\n", mNumAllMoved);
      if (mIteratingMoveAll <= 0)
        wprint("\aNo more residuals!\n");
      nextResBut->setEnabled(false);
      nextLocalBut->setEnabled(false);
      manageDoneLabel();
      return;
    }
    
    rpt = &(mResidList[mCurrentRes]);
    inobj = rpt->obj;
    incont = rpt->cont;
    inview = rpt->view;
    xr = rpt->xres;
    yr = rpt->yres;

    /* See if point is on list */
    belowThresh = mHasWeights && sSkipLowWeight && rpt->weight <= sWeightThresh;
    found = 0;
    for (i = 0; i < mNumLooked && !found; i++)
      if (inobj == mLookedList[i].obj && 
          incont == mLookedList[i].cont
          && inview == mLookedList[i].view)
        found = 1;

    /* Continue with next point if looking once and this point was found
       on the list */
  } while ((found && mLookonce) || belowThresh);

  /* Add point to list if it wasn't found */
  if (!found) {
    if (mNumLooked >= mLookedMax) {
      if (mLookedMax)
        mLookedList = (LookedPt *)realloc
          (mLookedList, (mLookedMax + 100) * sizeof(LookedPt));
      else
        mLookedList = (LookedPt *)malloc(100 * sizeof(LookedPt));
      mLookedMax += 100;
    }

    if (!mLookedList) {
      wprint("\aMemory error in bead fixer!\n");
      close();
      return;
    }
    mLookedList[mNumLooked].obj = inobj;
    mLookedList[mNumLooked].cont = incont;
    mLookedList[mNumLooked++].view = inview;
  }

  // Adjust the area and issue message if changed; set bell unless suppressed
  if (rpt->area != mCurArea) {
    if (mMovingAll && mIteratingMoveAll <= 0)
      wprint("Moved %d points\n", mNumAllMoved);
    outputResid = true;
    numToSee = mAreaList[rpt->area].numPts;
    if (mLookonce) {
      numToSee = 1;
      for (j = mCurrentRes + 1; j < mNumResid; j++) {
        if (mResidList[j].area != rpt->area)
          break;
        found = 0;
        for (i = 0; i < mNumLooked && !found; i++)
          if (mResidList[j].obj == mLookedList[i].obj && 
              mResidList[j].cont == mLookedList[i].cont
              && mResidList[j].view == mLookedList[i].view)
            found = 1;
        if (!found)
          numToSee++;
      }
    }
    if (mIteratingMoveAll <= 0)
      wprint("Entering local area %d  %d,  %d res.,  %d to examine\n",
             mAreaList[rpt->area].areaX, mAreaList[rpt->area].areaY,
             mAreaList[rpt->area].numPts, numToSee);
    setCurArea(rpt->area);
    if (!bell)
      bell = 1;
  }

  rpt->lookedAt = 1;
  found = 0;
  curpt=0;
  nobj = imodGetMaxObject(imod); 
  imodGetIndex(imod, &obsav, &cosav, &ptsav);

  if (mObjcont) {

    /* New case of direct object-contour listing */
    if (inobj > nobj) {
      wprint("\aObject not found!\n");
      return;
    }
    obj = inobj - 1;
    cont = incont - 1;
    imodSetIndex(imod, obj, cont, 0);
    ob = imodObjectGet(imod);
    ncont = imodObjectGetMaxContour(ob);
    if (incont <= ncont) {
      found = 1;
      con = imodContourGet(imod);
      npnt = imodContourGetMaxPoint(con);
    }
  } else {

    /* Old case of "point #", need to count through valid contours */
    ob = imodObjectGetFirst(imod);

    for (obj=0; obj < nobj ; obj++) {
      ncont = imodObjectGetMaxContour(ob);
      con = imodContourGetFirst(imod);
      for (cont = 0; cont < ncont; cont++)  {
        npnt = imodContourGetMaxPoint(con);
        if (npnt > 1) curpt++;
        if(curpt == incont) {
          found = 1;
          break;
        }
        con = imodContourGetNext(imod);
      }
      if (found)
        break;
      ob = imodObjectGetNext(imod);
    }
  }

  if (!found || !con) {
    wprint("\aContour not found!\n");
    imodSetIndex(imod, obsav, cosav, ptsav);
    return;
  }
  pts = imodContourGetPoints(con);
  for (ipt = 0; ipt < npnt; ipt++) {
    if(floor((double)(pts[ipt].z + 1.5f)) == inview) {

      // Insist that point is close to where it should be
      dx = pts[ipt].x - rpt->xcen;
      dy = pts[ipt].y - rpt->ycen;
      if (dx * dx + dy * dy > 225.) {
        wprint("\aPoint is > 15 pixels from position in log file\n");
        imodSetIndex(imod, obsav, cosav, ptsav);
        return;
      }

      imodSetIndex(imod, obj, cont, ipt);
      resval = sqrt((double)(xr*xr + yr*yr));
      if (bell > 0 || ((outputResid || !mMovingAll) && mIteratingMoveAll <= 0)) {
        if (mHasWeights)
          wprint("%sResid =%6.2f (%5.1f,%5.1f),%5.2f SDs, wgt %.3f\n", 
                 bell > 0 ? "\a" : "", resval, xr, yr, rpt->sd, rpt->weight);
        else
          wprint("%sResidual =%6.2f (%5.1f,%5.1f),%5.2f SDs\n", bell > 0 ? "\a" : "",
                 resval, xr, yr, rpt->sd);
      }
      mIndlook = mCurrentRes;
      mObjlook = obj;
      mContlook = cont;
      mPtlook = ipt;
      mCurmoved = 0;
      movePointBut->setEnabled(true);

      // Make an arrow in the extra object
      con = imodContourNew();
      if (con) {
        if (mExtraObj)
          ob = ivwGetAnExtraObject(sView, mExtraObj);
        if (ob) {
          ivwClearAnExtraObject(sView, mExtraObj);
          tpt.x = rpt->xcen;
          tpt.y = rpt->ycen;
          tpt.z = pts[ipt].z;
          imodPointAppend(con, &tpt);
          tpt.x += xr;
          tpt.y += yr;
          imodPointAppend(con, &tpt);
          tpt.x -= 0.707 * (xr - yr) * headLen / resval;
          tpt.y -= 0.707 * (xr + yr) * headLen / resval;
          imodPointAppend(con, &tpt);
          tpt.x = rpt->xcen + xr;
          tpt.y = rpt->ycen + yr;
          imodPointAppend(con, &tpt);
          tpt.x -= 0.707 * (xr + yr) * headLen / resval;
          tpt.y -= 0.707 * (-xr + yr) * headLen / resval;
          imodPointAppend(con, &tpt);
          imodObjectAddContour(ob, con);
        }
        free(con);
      }
      if (!mMovingAll) {
        ivwRedraw(sView);
        manageDoneLabel();
      }

      backUpBut->setEnabled(mCurrentRes > 0);    
      return;
    }
  }
  wprint("\aPoint not found in contour!\n");
  imodSetIndex(imod, obsav, cosav, ptsav);
  return;
}

// Go to next local area by just setting the current point to before it
// Suppress bell since user selected action
void BeadFixer::nextLocal()
{
  if (mCurArea >= mNumAreas - 1)
    return;
  mCurrentRes = mAreaList[mCurArea + 1].firstPt - 1;
  mBell = -1;
  nextRes();
}
 
// Go back to last point
void BeadFixer::backUp()
{
  int i, areaX, areaY, newRes;
  ResidPt *rpt;
  newRes = -1;

  if (!mNumResid)
    return;

  // Find the previous residual (that was looked at, if lookonce is on)
  for (i = mCurrentRes - 1; i >= 0 && newRes < 0; i--)
    if ((!mLookonce || mResidList[i].lookedAt) && 
        (!(mHasWeights && sSkipLowWeight) || mResidList[i].weight > sWeightThresh))
      newRes = i;

  if (newRes < 0) {
    if (mLookonce) {
      wprint("\aThere is no previous residual.  Try turning off \"Examine "
             "points once\".\n");
    } else if (mHasWeights && sSkipLowWeight) {
      wprint("\aThere is no previous residual.  Try turning off \"Skip "
             "low weights\".\n");
    } else {
      wprint("\aThere is no previous residual.\n");
      backUpBut->setEnabled(false);
    }
    return;
  }

  // Take current point off the examined list to allow it to be seen again
  if (mCurrentRes < mNumResid) {
    rpt = &(mResidList[mCurrentRes]);
    for (i = 0; i < mNumLooked; i++)
      if (rpt->obj == mLookedList[i].obj && 
          rpt->cont == mLookedList[i].cont
          && rpt->view == mLookedList[i].view)
        mLookedList[i].obj = -1;
  }
        
  // Disable backup button if back to first
  if (!newRes)
    backUpBut->setEnabled(false);

  // Give message if moved between areas, set the bell flag
  rpt = &(mResidList[newRes]);
  if (rpt->area != mCurArea) {
    setCurArea(rpt->area);
    areaX = mAreaList[rpt->area].areaX;
    areaY = mAreaList[rpt->area].areaY;
    if (!areaX && !areaY)
      wprint("Backing up into global solution residuals.\n");
    else
      wprint("Backing up into local area %d %d.\n", areaX, areaY);
    mBell = 1;
  }

  // Point to one before desired residual
  // Turn off look once flag, set flag that there is a resid, and get residual
  mCurrentRes = newRes - 1;
  i = mLookonce;
  mLookonce = 0;
  nextResBut->setEnabled(true);
  nextRes();
  mLookonce = i;
}

void BeadFixer::nextCont()
{
  backContBut->setEnabled(true);
  if (moveToCont(1) > 0)
    return;
  wprint("\aNo more contours with lower mean residuals\n");
  nextContBut->setEnabled(false);
}

void BeadFixer::backUpCont()
{
  nextContBut->setEnabled(true);
  if (moveToCont(-1) > 0)
    return;
  wprint("\aNo more contours with higher mean residuals\n");
  backContBut->setEnabled(false);
}

// Move to next contour in either direction, set index and draw
int BeadFixer::moveToCont(int idir)
{
  int ob, co, midpoint;
  double nearval, val;
  Imod *imod = sView->imod;
  mObjForContRes = -1;
  for (ob = 0; ob < imod->objsize; ob++) {
    for (co = 0; co < imod->obj[ob].contsize; co++) {
      val = imod->obj[ob].cont[co].tempVal;
      if (val > 0 && idir * (val - mLastContRes) < 0 && 
          (mObjForContRes < 0 || idir * (val - nearval) > 0)) {
        mObjForContRes = ob;
        mContForContRes = co;
        nearval = val;
        midpoint = imod->obj[ob].cont[co].psize / 2 - 1;
      }
    }
  }
  if (mObjForContRes >= 0) {
    wprint("Contour mean residual %.2f\n", nearval);
    imodSetIndex(imod, mObjForContRes, mContForContRes, midpoint);
    imod->obj[mObjForContRes].flags |= IMOD_OBJFLAG_THICK_CONT;
    ivwRedraw(sView);
    mLastContRes = nearval;
  }
  delContBut->setEnabled(mObjForContRes >= 0);
  return mObjForContRes + 1;
}

// Delete current contour
void BeadFixer::delCont()
{
  int ob, co, pt;
  if (mObjForContRes < 0)
    return;
  imodGetIndex(sView->imod, &ob, &co, &pt);
  if (ob != mObjForContRes || co != mContForContRes) {
    wprint("\aCurrent contour is no longer the one selected based on "
           "residual\n");
    return;
  }
  inputDeleteContour(sView);
  mObjForContRes = -1;
  delContBut->setEnabled(false);
}

void BeadFixer::skipLowWgtToggled(bool state)
{
  sSkipLowWeight = state ? 1 : 0;
  wgtThreshSpin->setEnabled(mHasWeights && sSkipLowWeight != 0);
  wgtThreshLabel->setEnabled(mHasWeights && sSkipLowWeight != 0);
}

void BeadFixer::wgtThreshChanged(double value)
{
  setFocus();
  sWeightThresh = value;
}

void BeadFixer::onceToggled(bool state)
{
  mLookonce = state ? 1 : 0;
}

void BeadFixer::clearList()
{
  mNumLooked = 0;
}

void BeadFixer::movePoint()
{
  int obj, cont, pt;
  Ipoint *pts;
  Icont *con;
  ResidPt *rpt;
  Imod *imod = ivwGetModel(sView);
  ivwControlActive(sView, 0);
     
  if (!mNumResid || mCurmoved  || mObjlook < 0 || mIndlook < 0) 
    return;

  imodGetIndex(imod, &obj, &cont, &pt);
  if (obj != mObjlook || cont != mContlook || pt != mPtlook) {
    wprint("\aThe current point is not the same as the point with the "
           "last residual examined!\n");
    return;
  }

  /* move the point.  Use the original point coordinates as starting point */
  rpt = &(mResidList[mIndlook]);
  con = imodContourGet(imod);
  pts = imodContourGetPoints(con);
  sView->undo->pointShift();
  mOldpt = pts[pt];
  mOldpt.x = rpt->xcen;
  mOldpt.y = rpt->ycen;
  mNewpt = mOldpt;
  mNewpt.x += rpt->xres;
  mNewpt.y += rpt->yres;
  pts[pt] = mNewpt;
  sView->undo->finishUnit();
  mObjmoved = mObjlook;
  mContmoved = mContlook;
  mPtmoved = mPtlook;

  /* set flags and buttons */
  mCurmoved = 1;
  mDidmove = 1;
  movePointBut->setEnabled(false);
  undoMoveBut->setEnabled(true);
  
  if (!mMovingAll)
    ivwRedraw(sView);
}

void BeadFixer::undoMove()
{
  int obsav, cosav, ptsav;
  int nobj, ncont;
  Iobj *ob;
  Icont *con;
  Ipoint *pts;
  float dx, dy, distsq;
  Imod *imod = ivwGetModel(sView);
  ivwControlActive(sView, 0);
     
  if(!mNumResid || !mDidmove) 
    return;
  imodGetIndex(imod, &obsav, &cosav, &ptsav);

  nobj = imodGetMaxObject(imod); 

  if (mObjmoved < nobj) {
    imodSetIndex(imod, mObjmoved, mContmoved, 
                 mPtmoved);
    ob = imodObjectGet(imod);
    ncont = imodObjectGetMaxContour(ob);
    if (mContmoved < ncont) {
      con = imodContourGet(imod);
      pts = imodContourGetPoints(con);
      if (mPtmoved < imodContourGetMaxPoint(con)) {

        /* Check that point is within 10 pixels of where it was */
        dx = pts[mPtmoved].x - mNewpt.x;
        dy = pts[mPtmoved].y - mNewpt.y;
        distsq = dx * dx + dy * dy;
        if (distsq < 100. && pts[mPtmoved].z == mNewpt.z) {
          sView->undo->pointShift();
          pts[mPtmoved] = mOldpt;
          sView->undo->finishUnit();
          mDidmove = 0;
          mCurmoved = 0;
          undoMoveBut->setEnabled(false);
          movePointBut->setEnabled(true);
          ivwRedraw(sView);
          return;
        }    
      }
    }
  }
    
  wprint("\aMoved point no longer exists or is not close enough "
         "to where it was moved to!\n");
  imodSetIndex(imod, obsav, cosav, ptsav);
  undoMoveBut->setEnabled(false);
}

/*
 * Move all points in current area by residual
 */
void BeadFixer::moveAll(bool globalOK, bool skipDisplay)
{
  int startArea = mCurArea;
  
  // Do not allow it for the global area unless the flag allows it and there
  // are no locals
  mNumAllMoved = 0;
  if ((mCurArea <= 0 && (mNumAreas > 1 || !globalOK)) ||
      mCurrentRes >= mNumResid || !mNumResid)
    return;
  mMovingAll = true;
  while (mCurArea == startArea && mCurrentRes < mNumResid) {
    if (mIndlook >= 0 && !mCurmoved) {
      movePoint();
      mNumAllMoved++;
    }
    mBell = -1;
    nextRes();
  }
  mMovingAll = false;
  if (!skipDisplay || mCurrentRes >= mNumResid) {
    ivwRedraw(sView);
    manageDoneLabel();
  }
}

void BeadFixer::moveAllSlot()
{
  if (mCurArea <= 0) {
    if (mNumAreas > 1) {
      wprint("\aYou cannot move all global residuals when there are local "
             "areas.\n");
      return;
    }
    grabKeyboard();
    if (sLastMoveGlobal < 2 && mCurrentRes < mNumResid) {
      sLastMoveGlobal = dia_ask_forever
        ("Are you sure you want to move all points by residual?\n"
         "This may not be appropriate if:\n"
         "1) you have not looked at many residuals yet, or\n"
         "2) you are going to be using local alignments, or\n"
         "3) the large residuals were reported relative to residuals on\n"
         "  all views rather than neighboring views\n"
         "See Help for details.");
      if (!sLastMoveGlobal)
        return;
    }

  }
  if (mIteratingMoveAll == 0 && mNumAreas <= 1 && mShiftDown)
    mIteratingMoveAll = 1;
  moveAll(mNumAreas <= 1, mIteratingMoveAll > 0);
  if (mIteratingMoveAll > 0) 
    runAlign();
  else {
    mIteratingMoveAll = 0;
    releaseKeyboard();
  }
}

// Move all points in all local areas
void BeadFixer::moveAllAll()
{
  //  int numMoved = 0;
  if (sLastMoveAllAll < 2 && mCurrentRes < mNumResid) {
    sLastMoveAllAll = dia_ask_forever
      ("Are you sure you want to move points in all local areas?\n"
       "This may not be appropriate if:\n"
       "1) you have not looked at many residuals yet, or\n"
       "2) the large residuals were reported relative to residuals on\n"
       "  all views rather than neighboring views\n"
       "See Help for details.");
    if (!sLastMoveAllAll)
      return;
  }
  while (mCurrentRes < mNumResid) {
    moveAll(false, true);
    // numMoved += mNumAllMoved;
  }
  if (mIteratingMoveAll == 0 && mShiftDown)
    mIteratingMoveAll = 1;
  if (mIteratingMoveAll > 0)
    runAlign();
  else
    mIteratingMoveAll = 0;
}

// Iterate moving all gloabls, or all in all local areas - come here after save, align
// and reload of log file
void BeadFixer::iterateMoveAll()
{
  int numMoved = 0;
  if (!mShiftDown || !mNumResid) {
    if (mNumResid)
      wprint("Stopped iterating because you released the Shift key\n");
    mIteratingMoveAll = 0;
    releaseKeyboard();
    manageDoneLabel();
    return;
  }
  if (mNumAreas <= 1) {
    moveAll(true, true);
    numMoved = mNumAllMoved;
  } else {
    nextLocal();
    while (mCurrentRes < mNumResid) {
      moveAll(false, true);
      numMoved += mNumAllMoved;
    }
  }
  wprint("Moved %d points\n", numMoved);
  if (mShiftDown && numMoved > 0)
    runAlign();
  else {
    if (numMoved)
      wprint("Stopped iterating because you released the Shift key\n");
    releaseKeyboard();
    manageDoneLabel();
    mIteratingMoveAll = 0;
  }
}

void BeadFixer::manageDoneLabel()
{
  QString str = "Progress:  --%";
  int value = 0;
  if (mNumResid > 0) {
    value = B3DNINT((100. * B3DMAX(0, B3DMIN(mCurrentRes + 1, mNumResid))) / 
                    mNumResid);
    str.sprintf("Progress:  %d%%", value);
  }
  doneLabel->setText(str);
}


int BeadFixer::foundgap(int obj, int cont, int ipt, int before)
{
  Imod *imod = ivwGetModel(sView);

  if(mLastob == obj && mLastco == cont && mLastpt == ipt
     && mLastbefore == before)
    return 1;

  mLastob = obj;
  mLastco = cont;
  mLastpt = ipt;
  mLastbefore = before;
  imodSetIndex(imod, obj, cont, ipt);
  makeUpDownArrow(before);
  ivwRedraw(sView);
  return 0;
}

void BeadFixer::makeUpDownArrow(int before)
{
  int size = 12;
  int idir = before ? -1 : 1;
  Iobj *xobj = NULL;

  Imod *imod = ivwGetModel(sView);
  Ipoint pt;
  Ipoint *curpt;
  Icont * con;

  if (mExtraObj > 0)
    xobj = ivwGetAnExtraObject(sView, mExtraObj);
  if (!xobj)
    return;
  ivwClearAnExtraObject(sView, mExtraObj);

  // Initialize extra object
  imodObjectSetColor(xobj, 1., 1., 0.);
  imodObjectSetValue(xobj, IobjFlagClosed, 0);
  curpt = imodPointGet(imod);
  if (!curpt)
    return;
  pt = *curpt;
  pt.y += idir * size / 2;
  con = imodContourNew();
  if (con) {
    imodPointAppend(con, &pt);
    pt.y += idir * size;
    imodPointAppend(con, &pt);
    pt.x -= idir * size / 3;
    pt.y -= idir * size / 3;
    imodPointAppend(con, &pt);
    pt.x += idir * size / 3;
    pt.y += idir * size / 3;
    imodPointAppend(con, &pt);
    pt.x += idir * size / 3;
    pt.y -= idir * size / 3;
    imodPointAppend(con, &pt);
    imodObjectAddContour(xobj, con);
    free(con);
  }
}

/* Jump to next gap in the model, or place where it is not tracked to first
   or last section */

void BeadFixer::findGap(int idir)
{
  int  obj, nobj, cont, ncont, ipt, npnt;
  int obsav, cosav, ptsav, curob, curco, curpt, lookback;
  int iptmin, iptmax, iztst, ipt2, foundnext, zlimLo, zlimHi;
  float zcur, zmin, zmax;
  Iobj *ob;
  Icont *con;
  Ipoint *pts;
  int xsize, ysize, zsize;
  static int beforeVerbose = 1;

  Imod *imod = ivwGetModel(sView);
  ivwGetImageSize(sView, &xsize, &ysize, &zsize);

  /* This is needed to make button press behave just like hotkey in syncing
     the image */
  ivwControlActive(sView, 0);

  // Find lowest and highest Z values not in the skip list
  for (ipt = 0; ipt < zsize && inSkipList(ipt); ipt++) {}
  zlimLo = ipt;
  for (ipt = zsize - 1; ipt >= 0 && inSkipList(ipt); ipt--) {}
  zlimHi = ipt;

  con = imodContourGet(imod);
  imodGetIndex(imod, &obsav, &cosav, &ptsav);

  curob = mLastob;
  curco = mLastco;
  curpt = mLastpt;
  lookback = 0;

  if(mIfdidgap == 0 || mLastob < 0 || mLastco < 0 || mLastpt < 0) {
    curob = curco = curpt = 0;
    mLastob = -1;
    mLastbefore = 0;
    lookback = 1;
  }

  mIfdidgap = 1;

  /* If last one was at start of track, go back to first point of contour */
  if (mLastbefore)
    curpt = 0;

  imodSetIndex(imod, curob, curco, curpt);
  nobj = imodGetMaxObject(imod); 

  ob = imodObjectGet(imod);
  con = imodContourGet(imod);

  for (obj=curob; obj < nobj && obj >= 0; obj += idir) {
    ncont = imodObjectGetMaxContour(ob);
    for (cont = curco; cont < ncont && cont >= 0; cont += idir)  {
      npnt = imodContourGetMaxPoint(con);
      if(npnt > 0) {
        pts = imodContourGetPoints(con);

        /* find min and max z in contour */
        zmin = pts->z;
        iptmin = 0;
        zmax = zmin;
        iptmax = 0;
        for (ipt = 0; ipt < npnt; ipt++) {
          if (zmin > pts[ipt].z) {
            zmin = pts[ipt].z;
            iptmin = ipt;
          }
          if (zmax < pts[ipt].z) {
            zmax = pts[ipt].z;
            iptmax = ipt;
          }
        }

        // If looking back, check zmin, set it as gap before if not at low lim
        if(lookback == 1 && zmin > zlimLo + 0.5) {
          if(foundgap(obj,cont,iptmin, 1) == 0) {
            if (beforeVerbose)
              wprint("\aContour %d is missing points before current point.  "
                     "Use PageDown to get to view with missing point.\n",
                     cont+1);
            else
              wprint("\aContour %d is missing points before current point.\n",
                     cont+1);
            beforeVerbose = 0;
            return;
          }
        }

        /* from current point forward, check for existence of a point at 
           next non-skipped z value; if none, it's a gap */
        for (ipt = curpt; ipt < npnt && ipt >= 0; ipt += idir) {
          if (ipt != iptmax) {
            zcur = pts[ipt].z;

            // Get next Z value; skip forward to limit
            iztst = (int)(zcur + 1.5);
            while (iztst < zlimHi && inSkipList(iztst))
              iztst++;
            foundnext = iztst < zlimHi ? 0 : 1;
            for (ipt2 = 0; ipt2 < npnt; ipt2++) {
              if (iztst == (int)(pts[ipt2].z + 0.5)) {
                foundnext = 1;
                break;
              }
            }
            if (!foundnext)
              if(foundgap(obj, cont, ipt, 0) == 0) 
                return;
          }
        }

        /* If get to end of contour, check zmax against z of file */
        if (idir > 0) {
          if (zmax + 0.1f < zlimHi)
            if(foundgap(obj, cont, iptmax, 0) == 0) 
              return;
        } else if (zmin > zlimLo + 0.5) {
          if (foundgap(obj,cont,iptmin, 1) == 0) {
            wprint("\aContour %d is missing points before current point.\n",
                     cont+1);
            return;
          }
        }
      }
      if (idir > 0) {
        con = imodContourGetNext(imod);
        lookback = 1;
        curpt = 0;
      } else if (curco) {
        imodPrevContour(imod);
        con = imodContourGet(imod);
        curpt = imodContourGetMaxPoint(con) - 1;
      }
    }

    if (idir > 0) {
      ob = imodObjectGetNext(imod);
      con = imodContourGetFirst(imod);
      curco = 0;
    } else if (curob) {
      imodPrevObject(imod);
      ob = imodObjectGet(imod);
      curco = imodObjectGetMaxContour(ob) - 1;
      imodSetIndex(imod, curob - 1, curco, -1);
      con = imodContourGet(imod);
    }
  }
  if (idir > 0)
    wprint("\aNo more gaps found!\n");
  else
    wprint("\aNo gaps found back to beginning of model.\n");

  imodSetIndex(imod, obsav, cosav, ptsav);
  return;
}

void BeadFixer::resetStart()
{
  mIfdidgap = 0;
}

void BeadFixer::resetCurrent()
{
  int ob, co, pt;
  Imod *imod = ivwGetModel(sView);
  imodGetIndex(imod, &ob, &co, &pt);
  if (pt < 0)
    return;
  mLastob = ob;
  mLastco = co;
  mLastpt = pt;
}

void BeadFixer::reattach()
{
  Imod *imod = ivwGetModel(sView);
  if (mLastob < 0 || mLastco < 0 || mLastpt < 0)
    return;
  imodSetIndex(imod, mLastob, mLastco, mLastpt);
  ivwRedraw(sView);
}

/*
 * Process a new skip list by parsing it and storing string copy in plug data
 */
void BeadFixer::newSkipList(QString list)
{
  if (sSkipList) {
    free(sSkipList);
    sSkipList = NULL;
  }
  if (mSkipSecs) {
    free(mSkipSecs);
    mSkipSecs = NULL;
    mNumSkip = 0;
  }
  if (list.isEmpty())
    return;
  mSkipSecs = parselist(LATIN1(list), &mNumSkip);
  if (mSkipSecs) {
    sSkipList = strdup(LATIN1(list));
    for (int i = 0; i < mNumSkip; i++)
      mSkipSecs[i]--;
  }
}

// Find out if a Z value is in the skip list
bool BeadFixer::inSkipList(int zval)
{
  if (!sIgnoreSkips || !mNumSkip)
    return false;
  for (int i = 0; i < mNumSkip; i++)
    if (zval == mSkipSecs[i])
      return true;
  return false;
}

// Slots for skip list stuff
void BeadFixer::ignoreToggled(bool state)
{
  sIgnoreSkips = state ? 1 : 0;
  skipEdit->setEnabled(state);
}

void BeadFixer::skipListEntered()
{
  setFocus();
  newSkipList(skipEdit->text());
}

/* 
 * Insert a point: if autocentering, find nearest bead.  If in seed mode,
 * make a new contour unless this is an apparent continuation point
 */
int BeadFixer::insertPoint(float imx, float imy, bool keypad)
{
  Imod *imod = ivwGetModel(sView);
  Icont *cont;
  Iobj *obj;
  Ipoint *pts;
  static Ipoint newPt =  {0., 0., 0.};
  int  curx, cury, curz, index, ob, i,npnt;
  double zdiff, dist;
  int xsize, ysize, zsize;


  // Skip if in residual mode: so in seed or gap mode, it will handle insertion
  // and at least keep the contour in order
  if (sShowMode == RES_MODE)
    return 0;
  ivwGetLocation(sView, &curx, &cury, &curz);
  ivwGetImageSize(sView, &xsize, &ysize, &zsize);

  // Autocenter the point if selected, error and say handled if fail
  if (sAutoCenter && findCenter(imx, imy, curz)) {
    wprint("\aAutocentering failed to find a point\n");
    return 1;
  }

  // Do not start new contours in gap mode
  if (sShowMode == GAP_MODE && !imodContourGet(imod)) {
    wprint("\aNo automatic new contours in gap filling mode.\nUse \"Reattach"
           " to Point at Gap\" first to fill the current gap.\n");
    return 1;
  }

  obj = imodObjectGet(imod);
  cont = ivwGetOrMakeContour(sView, obj, 0);
  if (!cont) {
    wprint("\aFailed to get contour to add point to\n");
    return 1;
  }
  npnt = imodContourGetMaxPoint(cont);
  pts = imodContourGetPoints(cont);
  
  // With insert key, check for duplicate points
  if (keypad && curz == newPt.z && fabs((double)(newPt.x - imx)) < 2. &&
      fabs((double)(newPt.y - imy)) < 2.)
    return 1;
  newPt.x = imx;
  newPt.y = imy;
  newPt.z = curz;
    
  // Search current contour for closest point below the current Z and set the
  // insertion point after it
  index = 0;
  zdiff = 1000000;
  for (i = 0; i < npnt; i++) {
    if (pts[i].z < curz && curz - pts[i].z < zdiff) {
      zdiff = curz - pts[i].z;
      index = i + 1;
    }
  }

  // But in seed mode, see if need to start a new contour - i.e. if there is
  // a point at the same z or the point at nearest Z is farther away than
  // a criterion
  if (sAutoNewCont && sShowMode == SEED_MODE) {
    zdiff = 1000000;
    for (i = 0; i < npnt; i++) {
      if (fabs((double)(curz - pts[i].z)) < zdiff) {
        zdiff = fabs((double)(curz - pts[i].z));
        dist = imodPointDistance(&pts[i], &newPt);
      }
    }
    if (zdiff < 0.5 || dist > 2. * sDiameter) {
      imodGetIndex(imod, &ob, &i, &npnt);
      imodSetIndex(imod, ob, -1, -1);
      cont = ivwGetOrMakeContour(sView, obj, 0);
      if (!cont) {
        wprint("\aFailed to get contour to add point to\n");
        return 1;
      }
      index = 0;
    }
  }

  ivwRegisterInsertPoint(sView, cont, &newPt, index);

  // See if the arrow should be moved
  if (sShowMode == GAP_MODE) {

    // If looking before and still not at Z = 0
    if (mLastbefore && curz && !inSkipList(curz - 1))
      makeUpDownArrow(mLastbefore);
    else if (curz && curz <  zsize - 1 && !inSkipList(curz + 1)) {

      // Otherwise need to look through points and see if next one exists
      pts = imodContourGetPoints(cont);
      npnt = imodContourGetMaxPoint(cont);
      index = 0;
      for (i = 0; i < npnt; i++) {
        if (curz + 1 == (int)(pts[i].z + 0.5)) {
          index = 1;
          break;
        }
      }
      if (!index)
        makeUpDownArrow(0);
    }
  }
  
  ivwDraw(sView, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC);
  return 1;
}

/*
 * Move the current point using the autocentering function
 */
int BeadFixer::modifyPoint(float imx, float imy)
{
  Imod *imod = ivwGetModel(sView);
  int  curx, cury, curz;
  Icont *cont;
  Ipoint *pts;
  int ob, co, pt;

  ivwGetLocation(sView, &curx, &cury, &curz);
  imodGetIndex(imod, &ob, &co, &pt);
  if (pt < 0)
    return 0;
  cont = imodContourGet(imod);
  pts = imodContourGetPoints(cont);
  if ((int)floor(pts[pt].z + 0.5) != curz)
    return 0;
  if (findCenter(imx, imy, curz))
    return 0;
  sView->undo->pointShift();
  pts[pt].x = imx;
  pts[pt].y = imy;
  sView->undo->finishUnit();
  ivwDraw(sView, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC);
  return 1;
}

/*
static void printArray(float *filtBead, int nxdim, int nx, int ny)
{
  int ix, iy;
  printf("%d x %d array:\n", nx, ny);
  for (iy = 0; iy < ny; iy++) {
    for (ix = 0; ix < nx; ix++)
      printf("%3.0f ", filtBead[ix + iy * nxdim]);
    printf("\n");
  }
} */

/*
 * Find a nearby point with a large bead integral on the current section
 * and return the modified coordinates
 */
#define KERNEL_MAXSIZE 7
#define MAX_RING 20
int BeadFixer::findCenter(float &imx, float &imy, int curz)
{
  ImodView *vw = sView;
  double fartherCrit = 1.414;
  int ipolar = sLightBead ? 1 : -1;

  int xcen = (int)floor(imx + 0.5);
  int ycen = (int)floor(imy + 0.5);
  float beadSize = sDiameter;
  float radius = beadSize / 2.;
  int search = (int)B3DMAX(6, B3DMIN(MAX_DIAMETER, 4 * radius));

  double grandx, grandy, grandMax, delx, dely, cx, cy;
  int numRing, ring, x, y, ix, iy, xsize, ysize, zsize, ind, ixst, ixnd;
  int ixStart, iyStart, nxpad, nypad, nxpdim, nxout, nyout, ixofs, iyofs;
  float kernel[KERNEL_MAXSIZE * KERNEL_MAXSIZE];
  int boxSize, boxScaled, margin, ndat, grandRing;
  float scaleFactor, beadCenOfs, cenmean, annmean, rCenter, rInner, rOuter;
  float scaledSize = 8.;
  int linear = 1;
  int forward = 0;
  int inverse = 1;
  float center = 2.;
  float minInterp = 1.4f;
  float kernelSigma = 0.85f;
  float xBeadOfs, yBeadOfs, xOffset, yOffset, peakXcen, peakYcen, cval;
  float ringMax[MAX_RING], ringXcen[MAX_RING], ringYcen[MAX_RING];
  Islice *bytesl, *sl;
  float *corrSlice, *fullBead, *filtSlice, *splitBead;

  for (ix = 0; ix < MAX_RING; ix++)
    ringMax[ix] = -1.e30;

  ivwGetImageSize(sView, &xsize, &ysize, &zsize);

  // NOTE: there are several duplicate variables so that code can be kept
  // identical to imodfindbeads in places

  // Get radii for the integral
  rCenter = B3DMAX(1., 0.34 * beadSize);
  rInner = B3DMAX(rCenter + 1., 0.5 * beadSize + 1.);
  rOuter = rInner + 2.;

  // Get sizes for box and scaled box and correlation
  scaleFactor = beadSize / scaledSize;
  boxSize = 2 * (search + 1) + sDiameter;
  boxSize = B3DMIN(boxSize, B3DMIN(xsize, ysize));
  scaledSobel(NULL, boxSize, boxSize, scaleFactor, minInterp, linear, -1., 
              NULL, &boxScaled, &nyout, &xBeadOfs, &yBeadOfs);
  beadCenOfs = (boxSize / 2. -  xBeadOfs) / scaleFactor - boxScaled / 2;
  nxpad = niceFrame(boxScaled, 2, 19);
  nypad = nxpad;
  nxpdim = nxpad + 2;

  corrSlice = (float *)malloc(nxpdim * nypad * sizeof(float));
  fullBead = (float *)malloc(boxSize * boxSize * sizeof(float));
  filtSlice = (float *)malloc(boxScaled * boxScaled * sizeof(float));
  splitBead = (float *)malloc(nxpdim * nypad * sizeof(float));
  bytesl = sliceCreate(boxSize, boxSize, SLICE_MODE_USHORT);
  if (!corrSlice || !fullBead || !filtSlice || !splitBead || !bytesl) {
    wprint("\aCannot get memory for autocentering bead\n");
    if (corrSlice)
      free(corrSlice);
    if (fullBead)
      free(fullBead);
    if (filtSlice)
      free(filtSlice);
    if (splitBead)
      free(splitBead);
    if (bytesl)
      sliceFree(bytesl);
    return 1;
  }

  // Get the model
  makeModelBead(boxSize, beadSize, fullBead);
  scaledSobel(fullBead, boxSize, boxSize, scaleFactor, minInterp, linear, 
              center, filtSlice, &nxout, &nyout, &xBeadOfs, &yBeadOfs);
  sliceSplitFill(filtSlice, boxScaled, boxScaled, splitBead, nxpdim, nxpad,
                 nypad, 0, 0.);
  todfft(splitBead, &nxpad, &nypad, &forward);

  // Get the area to correlate
  ixStart = B3DMAX(0, xcen - boxSize / 2);
  if (ixStart + boxSize > xsize)
    ixStart = xsize - boxSize;
  iyStart = B3DMAX(0, ycen - boxSize / 2);
  if (iyStart + boxSize > ysize)
    iyStart = ysize - boxSize;
  for (y = 0; y < boxSize; y++)
    for (x = 0; x < boxSize; x++)
      bytesl->data.us[x + y * boxSize] = ivwGetValue(vw, x + ixStart, y + iyStart, curz);

  // Filter
  scaledGaussianKernel(&kernel[0], &ndat, KERNEL_MAXSIZE, kernelSigma);
  sl = slice_mat_filter(bytesl, kernel, ndat);
  sliceFree(bytesl);
  free(fullBead);
  if (!sl) {
    wprint("\aFailed to get memory for kernel filtered box\n");
    free(splitBead);
    free(filtSlice);
    free(corrSlice);
    return 1;
  }
  //printArray(sl->data.f, boxSize, boxSize, boxSize);
  scaledSobel(sl->data.f, boxSize, boxSize, scaleFactor, minInterp, linear,
              2., filtSlice, &nxout, &nyout,  &xOffset, &yOffset);
  //printArray(filtSlice, nxout, nxout, nxout);

  // Pad into array and correlate it
  sliceTaperOutPad(filtSlice, SLICE_MODE_FLOAT, boxScaled, boxScaled, 
                   corrSlice, nxpdim, nxpad, nypad, 0, 0.);
  todfft(corrSlice, &nxpad, &nypad, &forward);
  conjugateProduct(corrSlice, splitBead, nxpad, nypad);
  todfft(corrSlice, &nxpad, &nypad, &inverse);

  margin = (int)B3DMAX(1., radius / scaleFactor);
  ixst = (nxpad - nxout) / 2 + margin;
  ixnd = ixst + nxout - 2 * margin;

  for (iy = ixst; iy < ixnd; iy++) {
    for (ix = ixst; ix < ixnd; ix++) {
      ind = ix + iy * (nxpdim);
      cval = corrSlice[ind];
      if (corrSlice[ind - 1] < cval && corrSlice[ind + 1] <= cval &&
          corrSlice[ind - nxpdim] < cval && 
          corrSlice[ind + nxpdim] <= cval &&
          corrSlice[ind - 1 - nxpdim] < cval && 
          corrSlice[ind + 1 + nxpdim] < cval && 
          corrSlice[ind + 1 - nxpdim] < cval && 
          corrSlice[ind - 1 + nxpdim] < cval) {

        cx = parabolicFitPosition(corrSlice[ind - 1], cval,
                                  corrSlice[ind + 1]);
        cy = parabolicFitPosition(corrSlice[ind - nxpdim], cval, 
                                  corrSlice[ind + nxpdim]);

        // integer offset in scaled, filtered image
        ixofs = ix - (nxpad - nxout) / 2;
        iyofs = iy - (nypad - nyout) / 2;
        
        // Center of feature in full original box
        peakXcen = (ixofs + cx + beadCenOfs) * scaleFactor + xOffset;
        peakYcen = (iyofs + cy + beadCenOfs) * scaleFactor + yOffset;
        //imodPrintStderr("Peak %g at %f, %f\n", cval, peakXcen, peakYcen);

        // Distance from the clicked point and ring index
        delx = peakXcen - (xcen - ixStart);
        dely = peakYcen - (ycen - iyStart);
        ring = (int)(2. * sqrt(delx * delx + dely * dely) / radius);
        ring = B3DMIN(ring, MAX_RING - 1);
        if (cval > ringMax[ring]) {

          // It is a higher peak for the ring: now verify the polarity is right
          delx = beadIntegral(sl->data.f, boxSize, boxSize, boxSize, rCenter,
                              rInner, rOuter, peakXcen, peakYcen, &cenmean,
                              &annmean, NULL, 0., NULL);
          if (delx * ipolar > 0.) {
            ringMax[ring] = cval;
            ringXcen[ring] = peakXcen + ixStart;
            ringYcen[ring] = peakYcen + iyStart;
          }
        }
      }
    }
  }
  free(splitBead);
  free(filtSlice);
  free(corrSlice);
  sliceFree(sl);

  // Search in radius-sized rings from the selected point; in each ring
  // find the point with the largest peak
  grandMax = -1.e30;
  grandRing = 0;
  numRing = (int)(2. * search / radius + 1.);
  numRing = B3DMIN(numRing, MAX_RING - 1);
  for (ring = 0; ring < numRing; ring++) {
    /*if (ringMax[ring] > -1.e20)
      imodPrintStderr("ring %d max %g at %.2f,%.2f\n", ring, ringMax[ring], 
      ringXcen[ring],ringYcen[ring]);*/

    // If the max is sufficiently larger than the previous one
    if (ringMax[ring] > 
        pow(fartherCrit, (double)(ring - grandRing)) * grandMax) {
      grandx = ringXcen[ring];
      grandy = ringYcen[ring];
      grandMax = ringMax[ring];
      grandRing = ring;
    }
  }
  //imodPrintStderr("grand max %f at %f,%f\n",  grandMax, grandx, grandy);
  if (grandMax < -1.e29)
    return 1;
  imx = (float)grandx;
  imy = (float)grandy;
  return 0;
}

// Slots for centering/seed controls
void BeadFixer::seedToggled(bool state)
{
  sAutoNewCont = state;
}

void BeadFixer::autoCenToggled(bool state)
{
  sAutoCenter = state ? 1 : 0;
}

void BeadFixer::lightToggled(bool state)
{
  sLightBead = state ? 1 : 0;
  setOverlay(sOverlayOn, sOverlayOn);
}

void BeadFixer::diameterChanged(int value)
{
  setFocus();
  sDiameter = value;
}

void BeadFixer::overlayToggled(bool state)
{
  overlaySpin->setEnabled(state);
  sOverlayOn = state ? 1: 0;
  setOverlay(1, sOverlayOn);
}

void BeadFixer::overlayChanged(int value)
{
  setFocus();
  sOverlaySec = value;
  setOverlay(sOverlayOn, sOverlayOn);
}

void BeadFixer::reverseToggled(bool state)
{
  sReverseOverlay = state ? 1 : 0;
  setOverlay(sOverlayOn, sOverlayOn);
}

// set the overlay mode to given state if the doIt flag is set
void BeadFixer::setOverlay(int doIt, int state)
{
  if (doIt)
    ivwSetOverlayMode(sView, state ? sOverlaySec : 0, 
                      sReverseOverlay,
                      (sLightBead + sReverseOverlay) % 2);
}

void BeadFixer::threshChanged(int slider, int value, bool dragging)
{
  Imod *imod = ivwGetModel(sView);
  Iobj *obj = imodObjectGet(imod);
  int valblack = B3DNINT((255. * (value - mPeakMin)) / (mPeakMax - mPeakMin));
  if (!obj)
    return;
  if (valblack == obj->valblack && value != mLastThresh)
    valblack += value < mLastThresh ? -1 : 1;
  sView->undo->objectPropChg();
  obj->valblack = (unsigned char)valblack;
  sView->undo->finishUnit();
  ivwDraw(sView, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC);
  imodvObjedNewView();
}

void BeadFixer::deleteBelow()
{
  ImodView *vi = sView;
  Imod *imod = ivwGetModel(vi);
  Icont *lassoCont;
  Iobj *obj;
  int ob, i, ix, iy, iz, curobj, anydel = 0;
  Istore *store;
  Iindex index;
  float thresh, min, max;
  Ipoint selmin, selmax;

  index.point = -1;
  ivwGetLocation(vi, &ix, &iy, &iz);
  imodGetIndex(imod, &curobj, &ix, &iy);

  // Set up selection subvolume based on rubberband if any and whether doing all secs
  selmin.x = -vi->xsize;
  selmax.x = 2 * vi->xsize;
  selmin.y = -vi->ysize;
  selmax.y = 2 * vi->ysize;
  selmin.z = iz - 0.5;
  selmax.z = iz + 0.5;
  if (sDelOnAllSec) {
    selmin.z = 0;
    selmax.z = vi->zsize;
  }
  zapRubberbandCoords(selmin.x, selmax.x, selmin.y, selmax.y);
  lassoCont = getTopZapLassoContour(true);

  for (ob = 0; ob < imod->objsize; ob++) {
    obj = &imod->obj[ob];
    index.object = ob;
    if ((ob == curobj || sDelInAllObj) && 
        (obj->flags & IMOD_OBJFLAG_USE_VALUE)) {
      istoreGetMinMax(obj->store, obj->contsize, GEN_STORE_MINMAX1, &min,&max);
      thresh = obj->valblack * (max - min) / 255. + min;
      for (i = 0; i < ilistSize(obj->store); i++) {
        store = (Istore *)ilistItem(obj->store, i);
        if (store->type == GEN_STORE_VALUE1 && 
            !(store->flags & GEN_STORE_SURFACE) && store->value.f < thresh) {
          index.contour = store->index.i;
          if (index.contour > 0 && index.contour < obj->contsize) {
            if ((!lassoCont && imodContInSelectArea(obj, &obj->cont[index.contour],
                                                    selmin, selmax)) ||
                (lassoCont && imodContInsideCont(obj, &obj->cont[index.contour], 
                                                 lassoCont, selmin.z, selmax.z))) {
              if (!anydel)
                imodSelectionListClear(vi);
              imodSelectionListAdd(vi, index);
              anydel = 1;
            }
          }
        }
      }
    }
  }

  // Delete, and clear selection list in case they say no
  if (anydel) {
    inputDeleteContour(vi);
    imodSelectionListClear(vi);
  }
}


void BeadFixer::delAllSecToggled(bool state)
{
  sDelOnAllSec = state ? 1 : 0;
}

void BeadFixer::delAllObjToggled(bool state)
{
  sDelInAllObj = state ? 1 : 0;
}

void BeadFixer::turnOffToggled(bool state)
{
  Imod *imod = ivwGetModel(sView);
  Iobj *obj = imodObjectGet(imod);
  if (!obj)
    return;
  sView->undo->objectPropChg();
  if (state)
    obj->matflags2 |= MATFLAGS2_SKIP_LOW;
  else
    obj->matflags2 &= ~MATFLAGS2_SKIP_LOW;
  sView->undo->finishUnit();
  ivwDraw(sView, IMOD_DRAW_MOD | IMOD_DRAW_NOSYNC);
  imodvObjedNewView();
}

void BeadFixer::modeSelected(int value)
{
  bool resOrCont = (value == RES_MODE || value == CONT_MODE);
  for (int on = 0; on < 2; on++) {

    // Manage seed mode items
    if ((!on && value != SEED_MODE) || (on && value == SEED_MODE)) {
      showWidget(seedModeBox, value == SEED_MODE);
      showWidget(overlayHbox, value == SEED_MODE);
      showWidget(reverseBox, value == SEED_MODE);
      manageThreshWidgets(value == SEED_MODE);
    }

    // Manage gap filling items
    if ((!on && value != GAP_MODE) || (on && value == GAP_MODE)) {
      showWidget(ignoreSkipBut, value == GAP_MODE);
      showWidget(skipEdit, value == GAP_MODE);
      showWidget(nextGapBut, value == GAP_MODE);
      showWidget(prevGapBut, value == GAP_MODE);
      showWidget(reattachBut, value == GAP_MODE);
      showWidget(resetStartBut, value == GAP_MODE);
      showWidget(resetCurrentBut, value == GAP_MODE);
    }

    // Manage autocenter items
    if ((!on && resOrCont) || (on && !resOrCont)) {
      showWidget(cenLightHbox, !resOrCont);
      showWidget(diameterHbox, !resOrCont);
    }
    
    // Manage residual and contour mode shared items
    if ((!on && !resOrCont) || (on && resOrCont)) {
      showWidget(openFileBut, resOrCont);
      showWidget(runAlignBut, resOrCont);
      showWidget(rereadBut, resOrCont);
    }

    // Manage residual mode items
    if ((!on && value != RES_MODE) || (on && value == RES_MODE)) {
      showWidget(nextResBut, value == RES_MODE);
      showWidget(backUpBut, value == RES_MODE);
      showWidget(nextLocalBut, value == RES_MODE);
      showWidget(movePointBut, value == RES_MODE);
      showWidget(undoMoveBut, value == RES_MODE);
      showWidget(moveAllBut, value == RES_MODE);
      showWidget(moveAllAllBut, value == RES_MODE);
      showWidget(clearListBut, value == RES_MODE);
      showWidget(examineBox, value == RES_MODE);
      showWidget(doneLabel, value == RES_MODE);
      showWidget(weightHbox, value == RES_MODE);
      showWidget(skipLowWgtBox, value == RES_MODE);
    }

    // Manage contour mode items
    if ((!on && value != CONT_MODE) || (on && value == CONT_MODE)) {
      showWidget(nextContBut, value == CONT_MODE);
      showWidget(backContBut, value == CONT_MODE);
      showWidget(delContBut, value == CONT_MODE);
    }
  }
  fixSize();

  // Turn overlay mode on or off if needed
  if ((value == SEED_MODE || sShowMode == SEED_MODE) && sOverlayOn)
    setOverlay(1, value == SEED_MODE ? 1 : 0);
  sShowMode = value;
  reportContRes();
}

void BeadFixer::showWidget(QWidget *widget, bool state)
{
  if (!widget)
    return;
  if (state)
    widget->show();
  else
    widget->hide();
}

// Change the thresholding widgets based on a change in model */
void BeadFixer::modelUpdate()
{
  manageThreshWidgets(sShowMode == SEED_MODE);
  fixSize();
}

void BeadFixer::manageThreshWidgets(bool seedMode)
{
  Imod *imod = ivwGetModel(sView);
  Iobj *obj = imodObjectGet(imod);
  bool enabled;
  float min, max;

  enabled = obj && (obj->flags & IMOD_OBJFLAG_USE_VALUE) && seedMode;
  threshSlider->showWidgets(0, enabled);
  showWidget(delAllSecBut, enabled);
  showWidget(delAllObjBut, enabled);
  showWidget(turnOffBut, enabled);
  showWidget(deleteBelowBut, enabled);
  if (!enabled)
    return;
  istoreGetMinMax(obj->store, obj->contsize, GEN_STORE_MINMAX1, &min, &max);
  mPeakMin = B3DNINT(min * 1000.);
  mPeakMax = B3DNINT(max * 1000.);
  threshSlider->setRange(0, mPeakMin, mPeakMax);
  mLastThresh = B3DNINT(obj->valblack * (mPeakMax - mPeakMin) / 255. + 
                        mPeakMin);
  threshSlider->setValue(0, mLastThresh);
  diaSetChecked(turnOffBut, (obj->matflags2 & MATFLAGS2_SKIP_LOW) != 0);
}
 
// THE WINDOW CLASS CONSTRUCTOR
 
static const char *buttonLabels[] = {"Done", "Help"};
static const char *buttonTips[] = {"Close Bead Fixer", "Open help window"};
static const char *threshTitle[] = {"Threshold peak value"};

BeadFixer::BeadFixer(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, 1, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), "Bead Fixer", "", name)
{
  QCheckBox *box;
  QString qstr;
  overlayHbox = NULL;
  reverseBox = NULL;
  mRunningAlign = false;
  mTopTimerID = 0;
  mStayOnTop = false;
  mIteratingMoveAll = 0;
  mIfdidgap = 0;
  mLastob = -1;
  mCurmoved = 0;
  mObjlook = -1;
  mIndlook = -1;
  mDidmove = 0;
  mLookonce = 1;
  mNumResid = 0;
  mResidMax = 0;
  mLookedMax = 0;
  mNumLooked = 0;
  mCurArea = -1;
  mNumAreas = 0;
  mAreaList = NULL;
  mAreaMax = 0;
  mCurrentRes = -1;
  mBell = 0;
  mMovingAll = false;
  mNumSkip = 0;
  mSkipSecs = NULL;
  mShiftDown = false;
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  mExtraObj = ivwGetFreeExtraObjectNumber(sView);

  mLayout->setSpacing(4);
  topBox = new QWidget(this);
  mLayout->addWidget(topBox);
  QHBoxLayout* toplay = new QHBoxLayout(topBox);
  toplay->setSpacing(5);
  toplay->setContentsMargins(0,0,0,0);

  QToolButton *toolBut = new QToolButton(topBox);
  toplay->addWidget(toolBut);
  toolBut->setCheckable(true);
  toolBut->setFocusPolicy(Qt::NoFocus);
  QIcon iconSet;
  iconSet.addPixmap(QPixmap((const char **)pegged), QIcon::Normal, QIcon::On);
  iconSet.addPixmap(QPixmap((const char **)unpegged), QIcon::Normal, 
                    QIcon::Off);
  toolBut->setIcon(iconSet);
  toolBut->setChecked(false);
  QSize hint = toolBut->sizeHint();
  toolBut->setFixedWidth(hint.width());
  toolBut->setFixedHeight(hint.height());
  connect(toolBut, SIGNAL(toggled(bool)), this, SLOT(keepOnTop(bool)));
  toolBut->setToolTip("Keep bead fixer window on top");

  QGroupBox *gbox = new QGroupBox("Operation", topBox);
  toplay->addWidget(gbox);
  QVBoxLayout *gbLayout = new QVBoxLayout(gbox);
  gbLayout->setSpacing(0);
  gbLayout->setContentsMargins(4, 0, 4, 3);
  modeGroup = new QButtonGroup(this);
  connect(modeGroup, SIGNAL(buttonClicked(int)), this,
          SLOT(modeSelected(int)));

  QRadioButton *radio = diaRadioButton("Make seed", gbox, modeGroup, gbLayout,
                                       0, "Show tools for making seed model");
  radio = diaRadioButton("Fill gaps", gbox, modeGroup, gbLayout, 1, 
                         "Show tools for finding and filling gaps");
  radio = diaRadioButton("Fix big residuals", gbox, modeGroup, gbLayout, 2,
                         "Show tools for fixing big residuals");
  radio = diaRadioButton("Look at contours", gbox, modeGroup, gbLayout, 3,
                         "Show tools for moving to contours by mean residual");
  diaSetGroup(modeGroup, sShowMode);

  cenLightHbox = new QWidget(this);
  mLayout->addWidget(cenLightHbox);
  QHBoxLayout *cenlay = new QHBoxLayout(cenLightHbox);
  cenlay->setSpacing(8);
  cenlay->setContentsMargins(0,0,0,0);
  autoCenBox = diaCheckBox("Autocenter", cenLightHbox, cenlay);
  connect(autoCenBox, SIGNAL(toggled(bool)), this, SLOT(autoCenToggled(bool)));
  diaSetChecked(autoCenBox, sAutoCenter != 0);
  autoCenBox->setToolTip("Automatically center inserted point on nearby bead");

  box = diaCheckBox("Light", cenLightHbox, cenlay);
  box->setFocusPolicy(Qt::NoFocus);
  connect(box, SIGNAL(toggled(bool)), this, SLOT(lightToggled(bool)));
  diaSetChecked(box, sLightBead != 0);
  box->setToolTip("Beads are lighter not darker than background");

  diameterHbox = new QWidget(this);
  mLayout->addWidget(diameterHbox);
  QHBoxLayout *diamlay = new QHBoxLayout(diameterHbox);
  diamlay->setContentsMargins(0,0,0,0);
  diameterSpin = (QSpinBox *)diaLabeledSpin
    (0, 1., (float)MAX_DIAMETER, 1., "Diameter", diameterHbox, diamlay);
  QObject::connect(diameterSpin, SIGNAL(valueChanged(int)), this,
                   SLOT(diameterChanged(int)));
  diameterSpin->setToolTip("Diameter of beads in pixels");
  diaSetSpinBox(diameterSpin, sDiameter);
  diamlay->addStretch();

  seedModeBox = diaCheckBox("Automatic new contour", this, mLayout);
  connect(seedModeBox, SIGNAL(toggled(bool)), this, SLOT(seedToggled(bool)));
  diaSetChecked(seedModeBox, sAutoNewCont != 0);
  seedModeBox->setToolTip("Make new contour for every point in a new position");
  

  if (App->rgba && !App->cvi->rgbStore) {
    overlayHbox = new QWidget(this);
    mLayout->addWidget(overlayHbox);
    diamlay =  new QHBoxLayout(overlayHbox);
    diamlay->setContentsMargins(0,0,0,0);
    diamlay->setSpacing(4);
    overlayBox = diaCheckBox("Overlay - view", overlayHbox, diamlay);
    connect(overlayBox, SIGNAL(toggled(bool)), this, 
            SLOT(overlayToggled(bool)));
    overlayBox->setToolTip("Show another section in color overlay - Hot key: /");

    overlaySpin = (QSpinBox *)diaLabeledSpin(0, -(float)MAX_OVERLAY,
                                             (float)MAX_OVERLAY, 1., NULL,
                                             overlayHbox, diamlay);
    QObject::connect(overlaySpin, SIGNAL(valueChanged(int)), this,
                     SLOT(overlayChanged(int)));
    overlaySpin->setToolTip("Interval to overlay section");
    diaSetSpinBox(overlaySpin, sOverlaySec);
    overlaySpin->setEnabled(sOverlayOn != 0);

    reverseBox = diaCheckBox("Reverse overlay contrast", this, mLayout);
    connect(reverseBox, SIGNAL(toggled(bool)), this, 
            SLOT(reverseToggled(bool)));
    reverseBox->setToolTip("Show color overlay in reverse contrast");
    diaSetChecked(reverseBox, sReverseOverlay != 0);
  }    

  // Value threshold and deletion controls
  threshSlider = new MultiSlider(this, 1, threshTitle, 0., 1000., 3);
  mLayout->addLayout(threshSlider->getLayout());
  QObject::connect(threshSlider, SIGNAL(sliderChanged(int, int, bool)),
                   this, SLOT(threshChanged(int, int, bool)));
  threshSlider->getSlider(0)->setToolTip("Set threshold peak strength for viewing "
                                         "points");

  turnOffBut = diaCheckBox("Turn off below threshold", this, mLayout);
  connect(turnOffBut, SIGNAL(toggled(bool)), this,
          SLOT(turnOffToggled(bool)));
  turnOffBut->setToolTip("Do not show contours below the threshold");

  deleteBelowBut = diaPushButton("Delete Below", this, mLayout);
  connect(deleteBelowBut, SIGNAL(clicked()), this, SLOT(deleteBelow()));
  deleteBelowBut->setToolTip("Delete all contours below threshold");

  delAllSecBut = diaCheckBox("Delete on all sections", this, mLayout);
  connect(delAllSecBut, SIGNAL(toggled(bool)), this, SLOT(delAllSecToggled(bool)));
  delAllSecBut->setToolTip("Delete contours below threshold regardless"
                           " of Z value of points");
  diaSetChecked(delAllSecBut, sDelOnAllSec != 0);

  delAllObjBut = diaCheckBox("Delete in all objects", this, mLayout);
  connect(delAllObjBut, SIGNAL(toggled(bool)), this, SLOT(delAllObjToggled(bool)));
  delAllObjBut->setToolTip("Delete contours from all objects that are"
                           " below the respective object threshold");
  diaSetChecked(delAllObjBut, sDelInAllObj != 0);
  manageThreshWidgets(sShowMode == SEED_MODE);

  // GAP CONTROLS
  nextGapBut = diaPushButton("Go to Next Gap", this, mLayout);
  connect(nextGapBut, SIGNAL(clicked()), this, SLOT(nextGap()));
  nextGapBut->setToolTip("Go to gap in model - Hot key: spacebar");

  prevGapBut = diaPushButton("Go to Previous Gap", this, mLayout);
  connect(prevGapBut, SIGNAL(clicked()), this, SLOT(prevGap()));
  prevGapBut->setToolTip("Go back to previous gap in model");

  reattachBut = diaPushButton("Reattach to Gap Point", this, mLayout);
  connect(reattachBut, SIGNAL(clicked()), this, SLOT(reattach()));
  reattachBut->setToolTip("Make point at current gap be the current point"
                " again");

  resetStartBut = diaPushButton("Start from Beginning", this, mLayout);
  connect(resetStartBut, SIGNAL(clicked()), this, SLOT(resetStart()));
  resetStartBut->setToolTip("Look for gaps from beginning of model");

  resetCurrentBut = diaPushButton("Start from Current Point", this, mLayout);
  connect(resetCurrentBut, SIGNAL(clicked()), this, SLOT(resetCurrent()));
  resetCurrentBut->setToolTip("Look for gaps from current point");

  ignoreSkipBut = diaCheckBox("Ignore gaps at views:", this, mLayout);
  connect(ignoreSkipBut, SIGNAL(toggled(bool)), this, 
          SLOT(ignoreToggled(bool)));
  ignoreSkipBut->setToolTip("Skip over gaps at views in the list below");
  diaSetChecked(ignoreSkipBut, sIgnoreSkips != 0);

  skipEdit = new ToolEdit(this);
  mLayout->addWidget(skipEdit);
  if (sSkipList) {
    skipEdit->setText(QString(sSkipList));
    newSkipList(QString(sSkipList));
  }
  connect(skipEdit, SIGNAL(returnPressed()), this, SLOT(skipListEntered()));
  connect(skipEdit, SIGNAL(focusLost()), this, SLOT(skipListEntered()));
  skipEdit->setToolTip("Enter list of views to ignore gaps at");
  skipEdit->setEnabled(sIgnoreSkips != 0);

  // RESIDUAL CONTROLS
  openFileBut = diaPushButton("Open Tiltalign Log File", this, mLayout);
  connect(openFileBut, SIGNAL(clicked()), this, SLOT(openFile()));
  openFileBut->setToolTip("Select an alignment log file to open");

  runAlignBut = diaPushButton("Save && Run Tiltalign", this, mLayout);
  connect(runAlignBut, SIGNAL(clicked()), this, SLOT(runAlign()));
  runAlignBut->setEnabled(false);
  runAlignBut->setToolTip("Save model and run Tiltalign");

  rereadBut = diaPushButton("Reread Log File", this, mLayout);
  connect(rereadBut, SIGNAL(clicked()), this, SLOT(rereadFile()));
  rereadBut->setEnabled(false);
  rereadBut->setToolTip("Read the previously specified file again");

  nextLocalBut = diaPushButton("Go to First Local Set", this, mLayout);
  connect(nextLocalBut, SIGNAL(clicked()), this, SLOT(nextLocal()));
  nextLocalBut->setEnabled(false);
  nextLocalBut->setToolTip("Skip to residuals in next local area");

  nextResBut = diaPushButton("Go to Next Big Residual", this, mLayout);
  connect(nextResBut, SIGNAL(clicked()), this, SLOT(nextRes()));
  nextResBut->setEnabled(false);
  nextResBut->setToolTip("Show next highest residual - Hot key: "
                "apostrophe");

  movePointBut = diaPushButton("Move Point by Residual", this, mLayout);
  connect(movePointBut, SIGNAL(clicked()), this, SLOT(movePoint()));
  movePointBut->setEnabled(false);
  movePointBut->setToolTip("Move point to position that fits alignment"
                " solution - Hot key: semicolon");

  undoMoveBut = diaPushButton("Undo Move", this, mLayout);
  connect(undoMoveBut, SIGNAL(clicked()), this, SLOT(undoMove()));
  undoMoveBut->setEnabled(false);
  undoMoveBut->setToolTip(
                "Move point back to previous position - Hot key: Shift+U");

  moveAllBut = diaPushButton("Move All in Local Area", this, mLayout);
  connect(moveAllBut, SIGNAL(clicked()), this, SLOT(moveAllSlot()));
  moveAllBut->setEnabled(false);
  moveAllBut->setToolTip("Move all remaining points in current area by " "residual - "
                         "Hot key: colon - OR Shift click to iterate");

  moveAllAllBut = diaPushButton("Move in All Local Areas", this, mLayout);
  connect(moveAllAllBut, SIGNAL(clicked()), this, SLOT(moveAllAll()));
  moveAllAllBut->setEnabled(false);
  moveAllAllBut->setToolTip("Move all points in all remaining local areas by residual - "
                            "Hot key: "CTRL_STRING"-colon - OR Shift click to iterate");

  backUpBut = diaPushButton("Back Up to Last Point", this, mLayout);
  connect(backUpBut, SIGNAL(clicked()), this, SLOT(backUp()));
  backUpBut->setEnabled(false);
  backUpBut->setToolTip("Back up to last point examined - "
                "Hot key: double quote");

  skipLowWgtBox = diaCheckBox("Skip low weights", this, mLayout);
  connect(skipLowWgtBox, SIGNAL(toggled(bool)), this, SLOT(skipLowWgtToggled(bool)));
  diaSetChecked(skipLowWgtBox, sSkipLowWeight != 0);
  skipLowWgtBox->setToolTip("Skip points with weights less than or equal to threshold");

  weightHbox = new QWidget(this);
  mLayout->addWidget(weightHbox);
  diamlay = new QHBoxLayout(weightHbox);
  diamlay->setContentsMargins(0,0,0,0);
  wgtThreshSpin = (QDoubleSpinBox *)diaLabeledSpin(2, 0., 0.9, 0.05, "Threshold", this, 
                                                   diamlay, &wgtThreshLabel);
  QObject::connect(wgtThreshSpin, SIGNAL(valueChanged(double)), this,
                   SLOT(wgtThreshChanged(double)));
  wgtThreshSpin->setToolTip("Threshold weight for skipping points");
  diaSetDoubleSpinBox(wgtThreshSpin, sWeightThresh);
  
  examineBox = diaCheckBox("Examine points once", this, mLayout);
  connect(examineBox, SIGNAL(toggled(bool)), this, SLOT(onceToggled(bool)));
  diaSetChecked(examineBox, mLookonce != 0);
  examineBox->setToolTip("Skip over points examined before");

  clearListBut = diaPushButton("Clear Examined List", this, mLayout);
  connect(clearListBut, SIGNAL(clicked()), this, SLOT(clearList()));
  clearListBut->setToolTip("Allow all points to be examined again");

  doneLabel = diaLabel("Progress:  --%", this, mLayout);

  nextContBut = diaPushButton("Go to Next Contour", this, mLayout);
  connect(nextContBut, SIGNAL(clicked()), this, SLOT(nextCont()));
  nextContBut->setEnabled(false);
  nextContBut->setToolTip("Move to contour with next lower mean residual - "
                          "Hot key: apostrophe");

  backContBut = diaPushButton("Back Up Contour", this, mLayout);
  connect(backContBut, SIGNAL(clicked()), this, SLOT(backUpCont()));
  backContBut->setEnabled(false);
  backContBut->setToolTip("Back up to contour with higher mean residual -"
                          " Hot key: double quote");

  delContBut = diaPushButton("Delete Contour", this, mLayout);
  connect(delContBut, SIGNAL(clicked()), this, SLOT(delCont()));
  delContBut->setEnabled(false);
  delContBut->setToolTip("Delete current contour - Hot key: Shift+D");

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));
  modeSelected(sShowMode);
  setMouseTracking(true);
  setFontDependentWidths();
  fixSize();
}

// In Qt 4 it just wouldn't go down to its minimum size: so force it down 
// to the hint
void BeadFixer::fixSize()
{
  imod_info_input();
  adjustSize();
  QSize hint = sizeHint();
  resize(B3DMIN(width(), hint.width()), B3DMIN(height(), hint.height()));
}

void BeadFixer::buttonPressed(int which)
{
  if (!which)
    close();
  else
    imodShowHelpPage("beadfix.html#TOP");
}

// Change to flag to keep on top or run timer as for info window
void BeadFixer::keepOnTop(bool state)
{
#ifdef STAY_ON_TOP_HACK
  mStayOnTop = state;

  // Start or kill the timer
  if (state)
    mTopTimerID = startTimer(200);
  else if (mTopTimerID) {
    killTimer(mTopTimerID);
    mTopTimerID = 0;
  }

#else
  Qt::WindowFlags flags = windowFlags();
  if (state)
    flags |= Qt::WindowStaysOnTopHint;
  else
    flags ^= Qt::WindowStaysOnTopHint;

  // Here are the old Qt 3 notes: Linux now OK with pos()
  //QPoint p(geometry().x(), geometry().y());
  // Using pos() jumps on Windows
  // Also, pos() jumps up-left on Unix, geometry() jumps down-right
  // Unless we access the pos !
  QPoint p2 = pos();
  setWindowFlags(flags);
  move(p2);
  show();
#endif
}

// Timer event to keep window on top in Linux, or watch for tiltalign done
void BeadFixer::timerEvent(QTimerEvent *e)
{
  if (mStayOnTop)
    raise();
}

// Routine to run tiltalign: it needs to start the thread to make the
// system call, start a timer to watch results, and disable buttons
void BeadFixer::runAlign()
{
  if (mRunningAlign || !sFilename)
    return;

  inputSaveModel(sView);

  QString comStr, fileStr, vmsStr;
  QStringList arguments;
  int dotPos;
  char *imodDir = getenv("IMOD_DIR");
  if (!imodDir) {
    wprint("\aCannot run tiltalign; IMOD_DIR not defined.\n");
    if (mIteratingMoveAll)
      releaseKeyboard();
    return;
  }
  vmsStr = "vmstopy";
#ifdef _WIN32
  vmsStr += ".cmd";
#endif
  fileStr = sFilename;
  
  // Remove the leading path and the extension
  dotPos = fileStr.lastIndexOf('/');
  if (dotPos >= 0)
    fileStr = fileStr.right(fileStr.length() - dotPos - 1);
  dotPos = fileStr.lastIndexOf('.');
  if (dotPos > 0)
    fileStr.truncate(dotPos);

  // 7/3/06: The old way was to run vmstocsh and pipe to tcsh in a "system"
  // command inside a thread - but in Windows it hung with -L listening to 
  // stdin.  This way worked through "system" call but QProcess is cleaner
  arguments << "-x" << "-q";
  arguments << fileStr + ".com" << fileStr + ".log";

  mAlignProcess = new QProcess();
  connect(mAlignProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this,
          SLOT(alignExited(int, QProcess::ExitStatus )));
  mAlignProcess->start(vmsStr, arguments);

  mRunningAlign = true;
  rereadBut->setEnabled(false);
  openFileBut->setEnabled(false);
  runAlignBut->setEnabled(false);
  nextResBut->setEnabled(false);
  nextLocalBut->setEnabled(false);
  nextContBut->setEnabled(false);
  backContBut->setEnabled(false);
  delContBut->setEnabled(false);
  undoMoveBut->setEnabled(false);
  movePointBut->setEnabled(false);
  moveAllBut->setEnabled(false);
  moveAllAllBut->setEnabled(false);
}

// When align exits, check the status and reenable buttons
void BeadFixer::alignExited(int exitCode, QProcess::ExitStatus exitStatus)
{
  // Check if exit staus, clean up and reenable buttons
  if (exitStatus != QProcess::NormalExit)
    wprint("\aAbnormal exit trying to run tiltalign.\n");
  else if (exitCode)
    wprint("\aError (return code %d) running tiltalign.\n", exitCode);

  if (exitStatus == QProcess::NormalExit && App->listening)
    imodPrintStderr("ETOMO INFO: Tiltalign ran with exit code %d\n", exitCode);

  delete mAlignProcess;
  mRunningAlign = false;

  if (reread() <= 0) {
    rereadBut->setEnabled(true);
    runAlignBut->setEnabled(true);
    openFileBut->setEnabled(true);
    if (mIteratingMoveAll > 0)
      iterateMoveAll();
  } else if (mIteratingMoveAll > 0)
    releaseKeyboard();
  if (!mLastLogError.isEmpty())
    wprint("\a%s", LATIN1(mLastLogError));
}

// The window is closing, remove from manager
void BeadFixer::closeEvent ( QCloseEvent * e )
{
  double posValues[NUM_SAVED_VALS];

  // Delete the process object to disconnect
  if (mRunningAlign)
    delete mAlignProcess;
  mRunningAlign = false;
  if (mSkipSecs)
    free(mSkipSecs);
  mSkipSecs = NULL;

  // Get geometry and save in settings and in structure for next time
  QRect pos = ivwRestorableGeometry(sWindow);
  posValues[0] = pos.left();
  posValues[1] = pos.top();
  sTop = pos.top();
  sLeft = pos.left();
  posValues[2] = sAutoCenter;
  posValues[3] = sDiameter;
  posValues[4] = sLightBead;
  posValues[5] = sOverlaySec;
  posValues[6] = 0;    // Was up down arrow flag
  posValues[7] = sShowMode;
  posValues[8] = sReverseOverlay;
  posValues[9] = sAutoNewCont;
  posValues[10] = sDelOnAllSec;
  posValues[11] = sDelInAllObj;
  posValues[12] = sIgnoreSkips;
  posValues[13] = sView->xybin;
  posValues[14] = sSkipLowWeight;
  posValues[15] = sWeightThresh;
  
  ImodPrefs->saveGenericSettings("BeadFixer", NUM_SAVED_VALS, posValues);

  imodDialogManager.remove((QWidget *)sWindow);
  if (mExtraObj > 0)
    ivwFreeExtraObject(sView, mExtraObj);

  setOverlay((sShowMode == SEED_MODE && sOverlayOn) ? 1 : 0, 0);
  sOverlayOn = 0;

  if (mTopTimerID)
    killTimer(mTopTimerID);
  mTopTimerID = 0;

  sView = NULL;
  sWindow = NULL;
  if (mLookedMax && mLookedList)
    free(mLookedList);
  mLookedMax = 0;
  if (sFilename)
    free(sFilename);
  sFilename = NULL;
  if (mAreaList && mAreaMax)
    free(mAreaList);
  mAreaMax = 0;
  if (mResidList && mResidMax)
    free(mResidList);
  mResidMax = 0;
  
  e->accept();
}

// Set widths of buttons and top box
void BeadFixer::setFontDependentWidths()
{
  int width2 = diaGetButtonWidth(this, mRoundedStyle, 1.15,
                                 "Move Point by Residual");
  int width = diaGetButtonWidth(this, mRoundedStyle, 1.15,
                                "Open Tiltalign Log File");
  if (width < width2)
    width = width2;
  width2 = topBox->sizeHint().width() - 4;
  if (width < width2)
    width = width2;
  topBox->setFixedWidth(width);
  diameterHbox->setFixedWidth(width);
  cenLightHbox->setFixedWidth(width);
  //if (overlayHbox)
  //overlayHbox->setFixedWidth(width);
  //reverseBox->setFixedWidth(width);
  nextGapBut->setFixedWidth(width);
  prevGapBut->setFixedWidth(width);
  resetStartBut->setFixedWidth(width);
  resetCurrentBut->setFixedWidth(width);
  openFileBut->setFixedWidth(width);
  runAlignBut->setFixedWidth(width);
  rereadBut->setFixedWidth(width);
  nextLocalBut->setFixedWidth(width);
  nextResBut->setFixedWidth(width);
  movePointBut->setFixedWidth(width);
  undoMoveBut->setFixedWidth(width);
  backUpBut->setFixedWidth(width);
  nextContBut->setFixedWidth(width);
  backContBut->setFixedWidth(width);
  delContBut->setFixedWidth(width);
  moveAllBut->setFixedWidth(width);
  moveAllAllBut->setFixedWidth(width);
  clearListBut->setFixedWidth(width);
  reattachBut->setFixedWidth(width);
  ignoreSkipBut->setFixedWidth(width);
  skipEdit->setFixedWidth(width);
  weightHbox->setFixedWidth(width);
}

void BeadFixer::changeEvent(QEvent *e)
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  DialogFrame::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
  setFontDependentWidths();
  fixSize();
}

// Close on escape, pass on keys
void BeadFixer::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Shift)
    mShiftDown = true;
 if (utilCloseKey(e))
    close();
  else
    ivwControlKey(0, e);
}

void BeadFixer::keyReleaseEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Shift)
    mShiftDown = false;
  if (mIteratingMoveAll > 0)
    releaseKeyboard();
  ivwControlKey(1, e);
}

// Keep track of shift state when mouse moves (in case shift is down when enter)
void BeadFixer::mouseMoveEvent ( QMouseEvent * e )
{
  mShiftDown = (e->modifiers() & Qt::ShiftModifier) != 0;
}
