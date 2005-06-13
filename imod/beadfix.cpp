/*
 *  beadfix.c -- Special module for fixing fiducial models
 *
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

    $Date$

    $Revision$

    Log at end of file
*/

/* include needed Qt headers and imod headers
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qtooltip.h>
#include <qtoolbutton.h>
#include <qhbox.h>
#include <qlayout.h>
#include <qdir.h>
#include <qstringlist.h>
#include <qfile.h>


/*#include "../../imod/imod.h"
#include "../../imod/imodplug.h"
#include "../../imod/control.h" */

// To make internal, 
// 1) change from imodplugin.h (or whatever that ends up as) 
// to imod.h and control.h
//#include "imodplugin.h"
#include "imod.h"
#include "imod_client_message.h"
#include "control.h"
#include "dia_qtutils.h"
#include "beadfix.h"
#include "pegged.xpm"
#include "unpegged.xpm"
#include "imod_input.h"
#include "xzap.h"
#include "preferences.h"
#include "undoredo.h"

// 2) Declare the internal functions as static
// And set them into the member variables in the constructor
static char *imodPlugInfo(int *type);
static int imodPlugKeys(ImodView *vw, QKeyEvent *event);
static void imodPlugExecute(ImodView *inImodView);
static  int imodPlugExecuteMessage(ImodView *vw, QStringList *strings,
                                   int *arg);

BeadFixerModule::BeadFixerModule()
{
  mInfo = imodPlugInfo;
  mExecuteType = NULL;
  mExecute = imodPlugExecute;
  mExecuteMessage = imodPlugExecuteMessage;
  mKeys = imodPlugKeys;
}

#define MAXLINE 100


/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
  ImodView    *view;
  BeadFixer *window;
  int    left, top;                     /* Last window position */
  int    alignExitCode;    // Place for qalign thread to leave its exit code
  char   *filename;
}PlugData;


static PlugData thisPlug = { 0, 0, 0, 0, 0, NULL };

#define ERROR_NO_IMOD_DIR -64352



/*
 * Called by the imod plugin load function. 
 */
char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE;
  return("Bead Fixer");
}

/*
 *  Grab hotkey input. return 1 if we handle the key.
 */
int imodPlugKeys(ImodView *vw, QKeyEvent *event)
{
  PlugData *plug = &thisPlug;
  int keysym;
  int    keyhandled = 1;
  int    ctrl;
  int    shift;

  /*
   * Don't grab keys if plug window isn't open.
   */
  if (!plug->view)
    return 0;
    
  /* The keysym values are Key_A ...
   * Key_Space, Key_Comma...
   */
  keysym = event->key();

  /*
   * Modifier key mask.  Set ctrl and shift to true
   * if the coresponding key is pressed.
   */
  ctrl   = event->state() & Qt::ControlButton;
  shift  = event->state() & Qt::ShiftButton;
    
    
  switch(keysym){
  case Qt::Key_Apostrophe: 
    plug->window->nextRes();
    break;
  case Qt::Key_QuoteDbl: 
    plug->window->backUp();
    break;
  case Qt::Key_Space:
    plug->window->nextGap();
    break;
  case Qt::Key_Semicolon:
    plug->window->movePoint();
    break;
  case Qt::Key_Colon:
    plug->window->moveAll();
    break;
  case Qt::Key_U:
    plug->window->undoMove();
    break;
  default:
    keyhandled = 0;
    break;
  }
  return keyhandled;
}

/*
 *  Execute any function or program you wish with this function.
 *  Here we open up a window for user interaction.
 *  see imodplug.h for a list of support functions.
 */

void imodPlugExecute(ImodView *inImodView)
{
  PlugData *plug;
  double savedValues[2];
  static int firstTime = 1;

  plug = &thisPlug;

  if (plug->window){
    /* 
     * Bring the window to the front if already open.
     */
    plug->window->raise();
    return;
  }

  plug->view = inImodView;

  /* 
   * Initialize data. 
   */
  plug->filename = NULL;

  /*
   * This creates the plug window.
   */
  plug->window  = new BeadFixer(imodDialogManager.parent(IMOD_DIALOG),
                                "bead fixer");

  imodDialogManager.add((QWidget *)plug->window, IMOD_DIALOG);

  // Get window position from settings the first time
  if (!firstTime) {
    plug->window->move(plug->left, plug->top);
  } else if (ImodPrefs->getGenericSettings("BeadFixer", savedValues, 2) == 2) {
    plug->left = (int)savedValues[0];
    plug->top = (int)savedValues[1];
    zapLimitWindowPos(plug->window->width(), plug->window->height(), 
                      plug->left, plug->top);
    plug->window->move(plug->left, plug->top);
  }
  firstTime = 0;

  plug->window->show();
}

/* Execute the message in the strings.
   Keep the action definitions in imod_client_message.h */

int imodPlugExecuteMessage(ImodView *vw, QStringList *strings, int *arg)
{
  PlugData *plug = &thisPlug;
  int action = (*strings)[*arg].toInt();
  if (!plug->window)
    return 1;
  switch (action) {
  case MESSAGE_BEADFIX_OPENFILE:
    return plug->window->openFileByName((*strings)[++(*arg)].latin1());
  case MESSAGE_BEADFIX_REREAD:
    return plug->window->reread();
  }
  return 1;
}

/* Open a tiltalign log file to find points with big residuals */

void BeadFixer::openFile()
{
  QString qname;
  char *filter[] = {"Align log files (align*.log)", "Log files (*.log)"};

  qname  = diaOpenFileName(this, "Select Tiltalign log file", 2, filter);
  
  if (qname.isEmpty())
    return;
  openFileByName(qname.latin1());
}
 
/* Open the log file with the given name, returning 1 if error */
 
int BeadFixer::openFileByName(const char *filename)
{
  PlugData *plug = &thisPlug;
  if (plug->filename != NULL)
    free(plug->filename);
  plug->filename = strdup(filename);
  
  if (reread())
    return 1;

  rereadBut->setEnabled(true);    
#ifdef FIXER_CAN_RUN_ALIGN
  runAlignBut->setEnabled(true);
#endif
  return 0;
}


/* Read or reread the tiltalign log file whose name was already obtained */
/* Return -1 if there is an error opening file, or 1 if there is a memory error
   and the window will close */
int BeadFixer::reread()
{
  PlugData *plug = &thisPlug;

  char line[MAXLINE];
  char *arealine;
  int newstyle, oldstyle = 0;
  int found = 0;
  int inpt;
  FILE   *fp;
  Iobj *xobj = ivwGetExtraObject(plug->view);
  ResidPt *rpt;

  // Initialize extra object
  ivwClearExtraObject(plug->view);
  imodObjectSetColor(xobj, 1., 0., 0.);
  imodObjectSetValue(xobj, IobjFlagClosed, 0);

  if (plug->filename == NULL) 
    return -1;

  fp = fopen(plug->filename, "r");
  if(fp == NULL) {
    wprint("\aError opening file!\n");
    return -1;
  }

  wprint("Reading log file...");
         
  mNumAreas = 0;
  mNumResid = 0;
  mCurrentRes = -1;
  mIndlook = -1;

  // Outer loop searching for lines at top of residuals
  while (fgets(line, MAXLINE, fp) != NULL) {
    newstyle = strstr(line,"   #     #     #      X         Y        X")
      != NULL;
    if (!newstyle)
      oldstyle = strstr(line,"   #     #      X         Y        X")
        != NULL;
    if (newstyle || oldstyle) {
      mObjcont = newstyle;

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
          sscanf(line, "%d %d %d %f %f %f %f %f", 
                 &rpt->obj, &rpt->cont, &rpt->view, &rpt->xcen, &rpt->ycen, 
                 &rpt->xres, &rpt->yres, &rpt->sd);
        else {
          sscanf(line, "%d %d %f %f %f %f %f", 
                 &inpt, &rpt->view, &rpt->xcen, &rpt->ycen, 
                 &rpt->xres, &rpt->yres, &rpt->sd);
          rpt->obj = 1;
          rpt->cont = inpt;
        }
        rpt->lookedAt = 0;
        mAreaList[mNumAreas - 1].numPts++;
        rpt->area = mNumAreas - 1;
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
      }

      // If none found, this breaks the top loop scanning for residual top line
      if (!found)
        break;
    }
  }
  fclose(fp);

  setCurArea(0);
  nextResBut->setEnabled(mNumResid);
  backUpBut->setEnabled(false);    
  if (!mNumResid)
    wprint("\aResidual data not found\n");
  else
    wprint(" %d total residuals.\n", mNumResid);
  return 0;
}

// Set current area, manage next local set button enabling and text
void BeadFixer::setCurArea(int area)
{
  mCurArea = area;
  nextLocalBut->setEnabled(mCurArea < mNumAreas - 1);
  nextLocalBut->setText(mCurArea ? 
                        "Go to Next Local Set" : "Go to First Local Set");
  moveAllBut->setEnabled(mCurArea > 0);
}

/* Jump to the next point with a big residual */

void BeadFixer::nextRes()
{
  int inobj, incont, inpt, inview, curpt, obj, nobj, cont, ncont, ipt, npnt;
  int obsav, cosav, ptsav, i;
  int found = 0;
  float  xr, yr, resval;
  Iobj *ob;
  Icont *con;
  Ipoint *pts;
  Ipoint tpt;
  float headLen = 2.5;
  ResidPt *rpt;
  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);

  // Copy and reset the bell flag
  int bell = mBell;
  mBell = 0;

  ivwControlActive(plug->view, 0);

  mIndlook = -1;
  undoMoveBut->setEnabled(false);
  movePointBut->setEnabled(false);
  if (!mNumResid || mCurrentRes >= mNumResid)
    return;


  // Coming into here, currentRes points to the last residual if any
  do {
    mCurrentRes++;
    if (mCurrentRes >= mNumResid) {
      wprint("\aNo more residuals!\n");
      nextResBut->setEnabled(false);
      nextLocalBut->setEnabled(false);
      return;
    }
    
    rpt = &(mResidList[mCurrentRes]);
    inobj = rpt->obj;
    incont = rpt->cont;
    inview = rpt->view;
    xr = rpt->xres;
    yr = rpt->yres;

    /* See if point is on list */
    found = 0;
    for (i = 0; i < mNumLooked && !found; i++)
      if (inobj == mLookedList[i].obj && 
          incont == mLookedList[i].cont
          && inview == mLookedList[i].view)
        found = 1;

    /* Continue with next point if looking once and this point was found
       on the list */
  } while (mLookonce && found);

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
    wprint("Entering local area %d  %d,  %d residuals\n",
           mAreaList[rpt->area].areaX,
           mAreaList[rpt->area].areaY, mAreaList[rpt->area].numPts);
    setCurArea(rpt->area);
    if (!bell)
      bell = 1;
  }

  rpt->lookedAt = 1;
  found = 0;
  curpt=0;
  nobj = imodGetMaxObject(theModel); 
  imodGetIndex(theModel, &obsav, &cosav, &ptsav);

  if (mObjcont) {

    /* New case of direct object-contour listing */
    if (inobj > nobj) {
      wprint("\aObject not found!\n");
      return;
    }
    obj = inobj - 1;
    cont = incont - 1;
    imodSetIndex(theModel, obj, cont, 0);
    ob = imodObjectGet(theModel);
    ncont = imodObjectGetMaxContour(ob);
    if (incont <= ncont) {
      found = 1;
      con = imodContourGet(theModel);
      npnt = imodContourGetMaxPoint(con);
    }
  } else {

    /* Old case of "point #", need to count through valid contours */
    ob = imodObjectGetFirst(theModel);

    for (obj=0; obj < nobj ; obj++) {
      ncont = imodObjectGetMaxContour(ob);
      con = imodContourGetFirst(theModel);
      for (cont = 0; cont < ncont; cont++)  {
        npnt = imodContourGetMaxPoint(con);
        if (npnt > 1) curpt++;
        if(curpt == incont) {
          found = 1;
          break;
        }
        con = imodContourGetNext(theModel);
      }
      if (found)
        break;
      ob = imodObjectGetNext(theModel);
    }
  }

  if (!found || !con) {
    wprint("\aContour not found!\n");
    imodSetIndex(theModel, obsav, cosav, ptsav);
    return;
  }
  pts = imodContourGetPoints(con);
  for (ipt = 0; ipt < npnt; ipt++) {
    if(floor((double)(pts++->z + 1.5f)) == inview) {
      imodSetIndex(theModel, obj, cont, ipt);
      resval = sqrt((double)(xr*xr + yr*yr));
      if (bell > 0)
        wprint("\aResidual =%6.2f (%5.1f,%5.1f),%5.2f SDs\n",
               resval, xr, yr, rpt->sd);
      else
        wprint("Residual =%6.2f (%5.1f,%5.1f),%5.2f SDs\n",
               resval, xr, yr, rpt->sd);

      mIndlook = mCurrentRes;
      mObjlook = obj;
      mContlook = cont;
      mPtlook = ipt;
      mCurmoved = 0;
      movePointBut->setEnabled(true);

      // Make an arrow in the extra object
      con = imodContourNew();
      if (con) {
        ivwClearExtraObject(plug->view);
        ob = ivwGetExtraObject(plug->view);
        tpt.x = rpt->xcen;
        tpt.y = rpt->ycen;
        tpt.z = (--pts)->z;
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
      ivwRedraw(plug->view);

      backUpBut->setEnabled(mCurrentRes > 0);    
      return;
    }
  }
  wprint("\aPoint not found in contour!\n");
  imodSetIndex(theModel, obsav, cosav, ptsav);
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
    if (!mLookonce || mResidList[i].lookedAt)
      newRes = i;

  if (newRes < 0) {
    if (mLookonce) {
      wprint("\aThere is no previous residual.  Try turning off \"Examine "
             "points once\".\n");
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
  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);
  ivwControlActive(plug->view, 0);
     
  if (!mNumResid || mCurmoved  || mObjlook < 0 || mIndlook < 0) 
    return;

  imodGetIndex(theModel, &obj, &cont, &pt);
  if (obj != mObjlook || cont != mContlook || pt != mPtlook) {
    wprint("\aThe current point is not the same as the point with the "
           "last residual examined!\n");
    return;
  }

  /* move the point.  Use the original point coordinates as starting point */
  rpt = &(mResidList[mIndlook]);
  con = imodContourGet(theModel);
  pts = imodContourGetPoints(con);
  plug->view->undo->pointShift();
  mOldpt = pts[pt];
  mOldpt.x = rpt->xcen;
  mOldpt.y = rpt->ycen;
  mNewpt = mOldpt;
  mNewpt.x += rpt->xres;
  mNewpt.y += rpt->yres;
  pts[pt] = mNewpt;
  plug->view->undo->finishUnit();
  mObjmoved = mObjlook;
  mContmoved = mContlook;
  mPtmoved = mPtlook;

  /* set flags and buttons */
  mCurmoved = 1;
  mDidmove = 1;
  movePointBut->setEnabled(false);
  undoMoveBut->setEnabled(true);
  ivwRedraw(plug->view);
}

void BeadFixer::undoMove()
{
  int obsav, cosav, ptsav;
  int nobj, ncont;
  Iobj *ob;
  Icont *con;
  Ipoint *pts;
  float dx, dy, distsq;
  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);
  ivwControlActive(plug->view, 0);
     
  if(!mNumResid || !mDidmove) 
    return;
  imodGetIndex(theModel, &obsav, &cosav, &ptsav);

  nobj = imodGetMaxObject(theModel); 

  if (mObjmoved < nobj) {
    imodSetIndex(theModel, mObjmoved, mContmoved, 
                 mPtmoved);
    ob = imodObjectGet(theModel);
    ncont = imodObjectGetMaxContour(ob);
    if (mContmoved < ncont) {
      con = imodContourGet(theModel);
      pts = imodContourGetPoints(con);
      if (mPtmoved < imodContourGetMaxPoint(con)) {

        /* Check that point is within 10 pixels of where it was */
        dx = pts[mPtmoved].x - mNewpt.x;
        dy = pts[mPtmoved].y - mNewpt.y;
        distsq = dx * dx + dy * dy;
        if (distsq < 100. && pts[mPtmoved].z == mNewpt.z) {
          plug->view->undo->pointShift();
          pts[mPtmoved] = mOldpt;
          plug->view->undo->finishUnit();
          mDidmove = 0;
          mCurmoved = 0;
          undoMoveBut->setEnabled(false);
          movePointBut->setEnabled(true);
          ivwRedraw(plug->view);
          return;
        }    
      }
    }
  }
    
  wprint("\aMoved point no longer exists or is not close enough "
         "to where it was moved to!\n");
  imodSetIndex(theModel, obsav, cosav, ptsav);
  undoMoveBut->setEnabled(false);
}

/*
 * Move all points in current area by residual
 */
void BeadFixer::moveAll()
{
  int startArea = mCurArea;
  if (mCurArea <= 0 || mCurrentRes >= mNumResid)
    return;
  while (mCurArea == startArea && mCurrentRes < mNumResid) {
    if (mIndlook >= 0 && !mCurmoved)
      movePoint();
    mBell = -1;
    nextRes();
  }
}

int BeadFixer::foundgap(int obj, int cont, int ipt, int before)
{
  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);

  if(mLastob == obj && mLastco == cont && mLastpt == ipt
     && mLastbefore == before)
    return 1;

  mLastob = obj;
  mLastco = cont;
  mLastpt = ipt;
  mLastbefore = before;
  imodSetIndex(theModel, obj, cont, ipt);
  ivwRedraw(plug->view);
  return 0;
}

/* Jump to next gap in the model, or place where it is not tracked to first
   or last section */

void BeadFixer::nextGap()
{
  int  obj, nobj, cont, ncont, ipt, npnt;
  int obsav, cosav, ptsav, curob, curco, curpt, lookback;
  int iptmin, iptmax, iztst, ipt2, foundnext;
  float zcur, zmin, zmax;
  Iobj *ob;
  Icont *con;
  Ipoint *pts;
  int xsize, ysize, zsize;

  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);
  ivwGetImageSize(plug->view, &xsize, &ysize, &zsize);

  /* This is needed to make button press behave just like hotkey in syncing
     the image */
  ivwControlActive(plug->view, 0);

  con = imodContourGet(theModel);
  imodGetIndex(theModel, &obsav, &cosav, &ptsav);

  curob = obsav;
  curco = cosav;
  curpt = ptsav;
  lookback = 0;

  if(!con || mIfdidgap == 0) {
    curob = curco = curpt = 0;
    mLastob = -1;
    mLastbefore = 0;
    lookback = 1;
  }

  mIfdidgap = 1;

  /* If last one was at start of track, go back to first point of contour */
  if (mLastbefore)
    curpt = 0;

  imodSetIndex(theModel, curob, curco, curpt);
  nobj = imodGetMaxObject(theModel); 

  ob = imodObjectGet(theModel);
  con = imodContourGet(theModel);

  for (obj=curob; obj < nobj ; obj++) {
    ncont = imodObjectGetMaxContour(ob);
    for (cont = curco; cont < ncont; cont++)  {
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

        /* If looking back, check zmin, set it as gap before if not 0 */
        if(lookback == 1 && zmin > 0.5) {
          if(foundgap(obj,cont,iptmin, 1) == 0) {
            wprint("\aContour %d is incomplete at start of stack\n", cont+1);
            return;
          }
        }

        /* from current point forward, check for existence of a point at 
           next z value; if none, it's a gap */
        for (ipt = curpt; ipt < npnt; ipt++) {
          if (ipt != iptmax) {
            zcur = pts[ipt].z;
            iztst = (int)(zcur + 1.5);
            foundnext = 0;
            for (ipt2 = 0; ipt2 < npnt; ipt2++) {
              if (iztst == (int)(pts[ipt2].z + 0.5)) {
                foundnext = 1;
                break;
              }
            }
            if (!foundnext)
              if(foundgap(obj, cont, ipt, 0) == 0) return;
          }
        }

        /* If get to end of contour, check zmax against z of file */
        if(zmax + 1.1f < zsize) {
          if(foundgap(obj, cont, iptmax, 0) == 0) return;
        }
      }
      con = imodContourGetNext(theModel);
      lookback = 1;
      curpt = 0;
    }
    ob = imodObjectGetNext(theModel);
    curco = 0;
  }
  wprint("\aNo more gaps found!\n");
  imodSetIndex(theModel, obsav, cosav, ptsav);
  return;
}

// THE WINDOW CLASS CONSTRUCTOR

static char *buttonLabels[] = {"Done", "Help"};
static char *buttonTips[] = {"Close Bead Fixer", "Open help window"};

BeadFixer::BeadFixer(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, 1, buttonLabels, buttonTips, true, 
                ImodPrefs->getRoundedStyle(), "Bead Fixer", "", name)
{
  QPushButton *button;
  QCheckBox *box;
  QString qstr;
  mRunningAlign = false;
  mTopTimerID = 0;
  mStayOnTop = false;
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
  mRoundedStyle = ImodPrefs->getRoundedStyle();

  topBox = new QHBox(this);
  mLayout->addWidget(topBox);
  topBox->setSpacing(6);

  QToolButton *toolBut = new QToolButton(topBox);
  toolBut->setToggleButton(true);
  QIconSet iconSet;
  iconSet.setPixmap(QPixmap((const char **)pegged), QIconSet::Automatic, 
                    QIconSet::Normal, QIconSet::On);
  iconSet.setPixmap(QPixmap((const char **)unpegged), QIconSet::Automatic,
                    QIconSet::Normal, QIconSet::Off);
  toolBut->setIconSet(iconSet);
  toolBut->setOn(false);
  QSize hint = toolBut->sizeHint();
  toolBut->setFixedWidth(hint.width());
  connect(toolBut, SIGNAL(toggled(bool)), this, SLOT(keepOnTop(bool)));
  QToolTip::add(toolBut, "Keep bead fixer window on top");

  button = new QPushButton("Go to Next Gap", topBox);
  button->setFocusPolicy(QWidget::NoFocus);
  connect(button, SIGNAL(clicked()), this, SLOT(nextGap()));
  QToolTip::add(button, "Go to gap in model - Hot key: spacebar");

  openFileBut = diaPushButton("Open Tiltalign Log File", this, mLayout);
  connect(openFileBut, SIGNAL(clicked()), this, SLOT(openFile()));
  QToolTip::add(openFileBut, "Select an alignment log file to open");

#ifdef FIXER_CAN_RUN_ALIGN
  runAlignBut = diaPushButton("Save && Run Tiltalign", this, mLayout);
  connect(runAlignBut, SIGNAL(clicked()), this, SLOT(runAlign()));
  runAlignBut->setEnabled(false);
  QToolTip::add(runAlignBut, "Save model and run Tiltalign");
#endif

  rereadBut = diaPushButton("Reread Log File", this, mLayout);
  connect(rereadBut, SIGNAL(clicked()), this, SLOT(rereadFile()));
  rereadBut->setEnabled(false);
  QToolTip::add(rereadBut, "Read the previously specified file again");

  nextLocalBut = diaPushButton("Go to First Local Set", this, mLayout);
  connect(nextLocalBut, SIGNAL(clicked()), this, SLOT(nextLocal()));
  nextLocalBut->setEnabled(false);
  QToolTip::add(nextLocalBut, "Skip to residuals in next local area");

  nextResBut = diaPushButton("Go to Next Big Residual", this, mLayout);
  connect(nextResBut, SIGNAL(clicked()), this, SLOT(nextRes()));
  nextResBut->setEnabled(false);
  QToolTip::add(nextResBut, "Show next highest residual - Hot key: "
                "apostrophe");

  movePointBut = diaPushButton("Move Point by Residual", this, mLayout);
  connect(movePointBut, SIGNAL(clicked()), this, SLOT(movePoint()));
  movePointBut->setEnabled(false);
  QToolTip::add(movePointBut, "Move point to position that fits alignment"
                " solution - Hot key: semicolon");

  undoMoveBut = diaPushButton("Undo Move", this, mLayout);
  connect(undoMoveBut, SIGNAL(clicked()), this, SLOT(undoMove()));
  undoMoveBut->setEnabled(false);
  QToolTip::add(undoMoveBut, 
                "Move point back to previous position - Hot key: U");

  moveAllBut = diaPushButton("Move All in Local Area", this, mLayout);
  connect(moveAllBut, SIGNAL(clicked()), this, SLOT(moveAll()));
  moveAllBut->setEnabled(false);
  QToolTip::add(moveAllBut, "Move all points in current area by residual"
                " - Hot key: colon");

  backUpBut = diaPushButton("Back Up to Last Point", this, mLayout);
  connect(backUpBut, SIGNAL(clicked()), this, SLOT(backUp()));
  backUpBut->setEnabled(false);
  QToolTip::add(backUpBut, "Back up to last point examined - "
                "Hot key: double quote");

  box = diaCheckBox("Examine Points Once", this, mLayout);
  connect(box, SIGNAL(toggled(bool)), this, SLOT(onceToggled(bool)));
  diaSetChecked(box, mLookonce != 0);
  QToolTip::add(box, "Skip over points examined before");

  clearListBut = diaPushButton("Clear Examined List", this, mLayout);
  connect(clearListBut, SIGNAL(clicked()), this, SLOT(clearList()));
  QToolTip::add(clearListBut, "Allow all points to be examined again");

  connect(this, SIGNAL(actionClicked(int)), this, SLOT(buttonPressed(int)));

}

void BeadFixer::buttonPressed(int which)
{
  if (!which)
    close();
  else
    imodShowHelpPage("beadfix.html");
}

// Change to flag to keep on top or run timer as for info window
void BeadFixer::keepOnTop(bool state)
{
#ifdef STAY_ON_TOP_HACK
  mStayOnTop = state;

  // Only kill the timer if it is not needed for tiltalign thread
  if (mTopTimerID && !(state || mRunningAlign)) {
    killTimer(mTopTimerID);
    mTopTimerID = 0;
  }
  if (state)
    mTopTimerID = startTimer(200);
#else
  int flags = getWFlags();
  if (state)
    flags |= WStyle_StaysOnTop;
  else
    flags ^= WStyle_StaysOnTop;
  QPoint p(geometry().x(), geometry().y());
  // Using pos() jumps on Windows
  // Also, pos() jumps up-left on Unix, geometry() jumps down-right
  // Unless we access the pos !
  QPoint p2 = pos();
  reparent(0, flags, p, true);  
#endif
}

// Timer event to keep window on top in Linux, or watch for tiltalign done
void BeadFixer::timerEvent(QTimerEvent *e)
{
  PlugData *plug = &thisPlug;
  if (mStayOnTop)
    raise();
#ifdef FIXER_CAN_RUN_ALIGN

  // Check if tiltalign is done, clean up and reenable buttons
  if (mRunningAlign) {
    if (mTaThread->running())
      return;
    if (plug->alignExitCode == ERROR_NO_IMOD_DIR)
      wprint("\aCannot run tiltalign; IMOD_DIR not defined.");
    else if (plug->alignExitCode) 
      wprint("\aError (return code %d) running tiltalign.", 
             plug->alignExitCode);
    delete mTaThread;
    mRunningAlign = false;
    if (!mStayOnTop && mTopTimerID) {
      killTimer(mTopTimerID);
      mTopTimerID = 0;
    }
    if (reread() <= 0) {
      rereadBut->setEnabled(true);
      runAlignBut->setEnabled(true);
      openFileBut->setEnabled(true);
    }
  }
#endif
}

// Routine to run tilalign: it needs to start the thread to make the
// system call, start a timer to watch results, and disable buttons
void BeadFixer::runAlign()
{
  PlugData *plug = &thisPlug;
  if (mRunningAlign || !plug->filename)
    return;
#ifdef FIXER_CAN_RUN_ALIGN
  inputSaveModel(plug->view);
  mTaThread = new AlignThread;
  mTaThread->start();
  plug->alignExitCode = 0;

  // Kill timer if no longer needed
  if (!mStayOnTop)
    mTopTimerID = startTimer(200);
  mRunningAlign = true;
  rereadBut->setEnabled(false);
  openFileBut->setEnabled(false);
  runAlignBut->setEnabled(false);
  nextResBut->setEnabled(false);
  nextLocalBut->setEnabled(false);
#endif
}

// The window is closing, remove from manager
void BeadFixer::closeEvent ( QCloseEvent * e )
{
  PlugData *plug = &thisPlug;
  double posValues[2];

  // reject if running thread
  if (mRunningAlign) {
    e->ignore();
    return;
  }

  // Get geometry and save in settings and in structure for next time
  QRect pos = ivwRestorableGeometry(plug->window);
  posValues[0] = pos.left();
  posValues[1] = pos.top();
  plug->top = pos.top();
  plug->left = pos.left();
  ImodPrefs->saveGenericSettings("BeadFixer", 2, posValues);

  imodDialogManager.remove((QWidget *)plug->window);
  ivwClearExtraObject(plug->view);

  if (mTopTimerID)
    killTimer(mTopTimerID);
  mTopTimerID = 0;

  plug->view = NULL;
  plug->window = NULL;
  if (mLookedMax && mLookedList)
    free(mLookedList);
  mLookedMax = 0;
  if (plug->filename)
    free(plug->filename);
  plug->filename = NULL;
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
  topBox->setFixedWidth(width);
  openFileBut->setFixedWidth(width);
#ifdef FIXER_CAN_RUN_ALIGN
  runAlignBut->setFixedWidth(width);
#endif
  rereadBut->setFixedWidth(width);
  nextLocalBut->setFixedWidth(width);
  nextResBut->setFixedWidth(width);
  movePointBut->setFixedWidth(width);
  undoMoveBut->setFixedWidth(width);
  backUpBut->setFixedWidth(width);
  clearListBut->setFixedWidth(width);
}

void BeadFixer::fontChange( const QFont & oldFont )
{
  mRoundedStyle = ImodPrefs->getRoundedStyle();
  setFontDependentWidths();
  DialogFrame::fontChange(oldFont);
}

// Close on escape, pass on keys
void BeadFixer::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void BeadFixer::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}

#ifdef FIXER_CAN_RUN_ALIGN
// Thread to run tiltalign provided that IMOD_DIR is defined
void AlignThread::run()
{
  PlugData *plug = &thisPlug;
  QString comStr, fileStr, vmsStr;
  int dotPos;
  char *imodDir = getenv("IMOD_DIR");
  char *cshell = getenv("IMOD_CSHELL");
  if (!imodDir) {
    plug->alignExitCode = ERROR_NO_IMOD_DIR;
    return;
  }
  if (!cshell)
    cshell = "tcsh";
  fileStr = plug->filename;
  
  // Remove the leading path and the extension
  dotPos = fileStr.findRev('/');
  if (dotPos >= 0)
    fileStr = fileStr.right(fileStr.length() - dotPos - 1);
  dotPos = fileStr.findRev('.');
  if (dotPos > 0)
    fileStr.truncate(dotPos);

  vmsStr = QString(imodDir) + "/bin/vmstocsh";
  comStr.sprintf("%s %s.log < %s.com | %s -ef", 
                 (QDir::convertSeparators(vmsStr)).latin1(),
                 fileStr.latin1(), fileStr.latin1(), cshell);
  plug->alignExitCode = system(comStr.latin1());
}
#endif

/*
    $Log$
    Revision 1.24  2005/04/13 19:12:26  mast
    fixed tooltip

    Revision 1.23  2005/04/12 18:57:47  mast
    Added move all in local area, improved some button enabling

    Revision 1.22  2005/02/19 01:29:50  mast
    Removed function to clear extra object

    Revision 1.21  2004/12/22 22:22:05  mast
    Fixed bug in reading "old" log files

    Revision 1.20  2004/11/20 05:05:27  mast
    Changes for undo/redo capability

    Revision 1.19  2004/11/04 23:30:55  mast
    Changes for rounded button style

    Revision 1.18  2004/09/24 17:58:01  mast
    Added ability to execute messages for opening/rereading file

    Revision 1.17  2004/07/09 21:26:55  mast
    Strip directory path off when running align, to avoid spaces in path

    Revision 1.16  2004/06/25 20:05:40  mast
    Based the move by residual on residual data instead of current point value,
    and rewrote to make most plug variables be class members

    Revision 1.15  2004/06/24 15:34:15  mast
    Rewrote to read in all data to internal structures at once, and made it
    move between areas automatically, improved backup logic

    Revision 1.14  2004/06/23 04:12:32  mast
    Stupid change just before checking in

    Revision 1.13  2004/06/23 03:32:19  mast
    Changed to save and restore window position

    Revision 1.12  2004/06/20 22:43:15  mast
    Fixed problem that made no residuals be found.

    Revision 1.11  2004/06/12 15:13:03  mast
    Needed some new Qt includes

    Revision 1.10  2004/06/12 00:58:11  mast
    Switched to reading in whole file at once

    Revision 1.9  2004/05/11 14:17:53  mast
    Needed to put an enable of the run align button inside conditional

    Revision 1.8  2004/05/07 22:14:53  mast
    Switched to a variable other than QT_THREAD_SUPPORT for the run align button

    Revision 1.7  2004/05/04 17:52:32  mast
    Forgot to put AlignThread::run inside ifdef.

    Revision 1.6  2004/05/03 19:32:20  mast
    had to decalre exit code as int

    Revision 1.5  2004/05/03 19:17:43  mast
    Added ability to run tiltalign if there is thread support

    Revision 1.4  2004/04/29 00:28:40  mast
    Added button to keep window on top

    Revision 1.3  2004/03/30 18:56:26  mast
    Added hot key for next local set

    Revision 1.2  2004/01/22 19:12:43  mast
    changed from pressed() to clicked() or accomodated change to actionClicked

    Revision 1.1  2003/10/01 05:09:36  mast
    Conversion to internal module in 3dmod

    Revision 3.9  2003/08/01 00:16:51  mast
    Made "examine once" be default and rearranged buttons

    Revision 3.8  2003/07/07 21:32:49  mast
    Fix stupid malloc/realloc problem in pointer list

    Revision 3.7  2003/06/29 14:34:41  mast
    Fix problem of multiple vector displays

    Revision 3.6  2003/06/29 14:23:20  mast
    Added ability to back up to previous residual

    Revision 3.5  2003/06/27 20:25:11  mast
    Implemented display of residual vectors in extra object

    Revision 3.4  2003/05/29 05:03:43  mast
    Make filter for align*.log only

    Revision 3.3  2003/05/12 19:13:39  mast
    Add hot key summary and fix spelling

*/
