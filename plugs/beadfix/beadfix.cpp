/*
 *  beadfix.cpp -- Early version of the bead fixer to test building and
 *  to provide an example of plugin programming.
 *
 *  It is named "Bead Fixer2" and the class is BeadFixer2 so it can be
 *  loaded without conflicting with the special module BeadFixer.
 *
 *  It does NOT treat undo/redo correctly; see real imod/beadfix.cpp or
 *  other code in 3dmod for examples of that.
 *
 *  $Id$
 */


/* include needed Qt headers and imod headers */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qtooltip.h>
#include <qfiledialog.h>
#include <qstringlist.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QMouseEvent>
#include <QEvent>
#include <QBoxLayout>
#include <QCloseEvent>

/* Leave this commented out to test building as a plugin in with public 
   interfaces only; uncomment to test with access to structure members */
//#define NO_PRIVATE_STRUCT
#include "imodplugin.h"
#include "dia_qtutils.h"
#include "beadfix.h"

#define MAXLINE 100

typedef struct {
  int obj;
  int cont;
  int view;
} residpt;

typedef struct {
  int areaX;
  int areaY;
  int lastSeen;
} AreaData;

/*
 *  Define a structure to contain all local plugin data.
 */
typedef struct
{
  ImodView    *view;
  BeadFixer2 *window;

  char   *filename;
  FILE   *fp;
  int    ifdidgap;
  int    residok;
  int    xsize, ysize, zsize;
  int    lastob, lastco, lastpt, lastbefore;
  int    objcont;
  float  xresid, yresid;                /* last residual looked at */
  int    objlook, contlook, ptlook;     /* obj, cont, pt of that residual */
  int    curmoved;                      /* flag whether it has been moved */
  int    objmoved, contmoved, ptmoved;  /* obj, cont, pt of moved point */
  int    didmove;                       /* flag that a point was moved */
  Ipoint oldpt, newpt;                  /* old and new positions */
  int    lookonce;
  residpt *lookedlist;                  /* List of points examined */
  int    listsize;                      /* Number of items on list */
  int    listmax;                       /* Size allocated for list */
  long   *offsetList;                   /* File offsets of points examined */
  int    offsetSize;                     /* Number of file positions */
  int    offsetMax;                     /* Size allocated for list */
  int    curArea;                       /* Current local area index */
  AreaData *areaList;                   /* Data about areas */
  int    areaMax;                       /* Size allocated */
  int    extraObjNum;
  float  rad;
  float lastImx, lastImy;
}PlugData;


static PlugData thisPlug = { 0, 0 };

/*
 * Called by the imod plugin load function. 
 */
const char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE + 
      IMOD_PLUG_MOUSE + IMOD_PLUG_EVENT + IMOD_PLUG_CHOOSER;
  return("Bead Fixer2");
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
  ctrl   = event->modifiers() & Qt::ControlModifier;
  shift  = event->modifiers() & Qt::ShiftModifier;
    
    
  switch(keysym){
  case Qt::Key_Apostrophe: 
    plug->window->nextRes();
    break;
  case Qt::Key_Space:
    plug->window->nextGap();
    break;
  case Qt::Key_Semicolon:
    plug->window->movePoint();
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

  plug = &thisPlug;

  if (plug->window){
    /* 
     * Bring the window to the front if already open.
     */
    plug->window->raise();
    return;
  }

  plug->view = inImodView;
  ivwGetImageSize(inImodView, &plug->xsize, &plug->ysize, &plug->zsize);
  
  /* 
   * Initialize data. 
   */
  plug->filename = NULL;
  plug->fp = NULL;
  plug->ifdidgap = 0;
  plug->residok = 0;
  plug->lastob = -1;
  plug->curmoved = 0;
  plug->objlook = -1;
  plug->didmove = 0;
  plug->lookonce = 1;
  plug->listmax = 0;
  plug->listsize = 0;
  plug->offsetSize = 0;
  plug->offsetMax = 0;
  plug->offsetList = NULL;
  plug->curArea = -1;
  plug->areaList = NULL;
  plug->areaMax = 0;
  plug->extraObjNum = ivwGetFreeExtraObjectNumber(plug->view);
  plug->rad = 30.;
  plug->lastImx = plug->lastImy = 0.;
  /*
   * This creates the plug window.
   */
  plug->window  = new BeadFixer2(imodDialogManager.parent(IMOD_DIALOG),
                                "bead fixer");

  imodDialogManager.add((QWidget *)plug->window, IMOD_DIALOG);

  // This makes the widget the right size and keeps it on screen and off the
  // info window on the Mac
  adjustGeometryAndShow((QWidget *)plug->window, IMOD_DIALOG);
  ivwTrackMouseForPlugs(plug->view, 1);
}

/* Execute the message in the strings, leave arg as index of last component 
   of message */

int imodPlugExecuteMessage(ImodView *vw, QStringList *strings, int *arg)
{
  PlugData *plug = &thisPlug;
  int action = (*strings)[*arg].toInt();
  if (!plug->window || action != 1)
    return 1;
  plug->window->reread(0);
  return 0;
}

/*
 * Process a mouse event: An example of a circular cursor with radius 
 * specified in image coordinates
 */
int imodPlugMouse(ImodView *vw, QMouseEvent *event, float imx,
                         float imy, int but1, int but2, int but3)
{
  PlugData *plug = &thisPlug;
  Iobj *xobj = ivwGetAnExtraObject(plug->view, plug->extraObjNum);
  Icont *con;
  double angle;
  int i, ix, iy,iz;
  Ipoint tpt;
  if (!plug->window || !xobj)
    return 0;

  // Initialize extra object

  ivwClearAnExtraObject(plug->view, plug->extraObjNum);
  imodObjectSetColor(xobj, 1., 0., 0.);
  imodObjectSetValue(xobj, IobjFlagClosed, 1);
  con = imodContourNew();
  if (!con)
    return 0;
  ivwGetLocation(plug->view, &ix, &iy, &iz);
  ivwGetTopZapZslice(plug->view, &iz);
  ivwGetTopZapMouse(plug->view, &tpt);
  if (tpt.x != imx || tpt.y != imy || tpt.z != iz)
    imodPrintStderr("passed in %.2f %.2f, getTopZ %d, but TopZap returned "
                    "%.2f %.2f %.1f\n", imx, imy, iz, tpt.x, tpt.y, tpt.z);

  for (i = 0; i < 100; i++) {
    angle = 2 * 3.14159 * i / 100.;
    tpt.x = imx + plug->rad * cos(angle);
    tpt.y = imy + plug->rad * sin(angle);
    tpt.z = iz;
    imodPointAppend(con, &tpt);
  }
  imodContourSetFlag(con, ICONT_CURSOR_LIKE | ICONT_MMODEL_ONLY, 1);
  imodObjectAddContour(xobj, con);
  free(con);
  return 2;
}

/*
 * Process wheel events
 */
int imodPlugEvent(ImodView *vw, QEvent *event, float imx, float imy)
{
  if (!thisPlug.window)
    return 0;
  int retval = 0;
  if (event->type() == QEvent::User)
    imodPuts("Custom event");
  else if (event->type() == QEvent::Enter) {
    //imodPuts("Enter");
  } else if (event->type() == QEvent::Leave) {
    //imodPuts("Leave");
  } else if (event->type() == QEvent::Wheel) {
    imodPuts("Wheel");
  }
  return retval;
}

/* Get a filename to open */
QString imodPlugOpenFileName(QWidget *parent, const QString &caption,
                             const QString &dir, const QString &filter)
{
  imodPuts("CHOOSER PLUGIN!");
  return QFileDialog::getOpenFileName(parent, caption, dir, filter);
}

QStringList imodPlugOpenFileNames(QWidget *parent, const QString &caption,
                                  const QString &dir, const QString &filter)
{
  imodPuts("CHOOSER PLUGIN!");
  return QFileDialog::getOpenFileNames(parent, caption, dir, filter);
}

QString imodPlugSaveFileName(QWidget *parent, const QString &caption)
{
  imodPuts("CHOOSER PLUGIN!");
  return QFileDialog::getSaveFileName(parent, caption);
}


/* Open a tiltalign log file to find points with big residuals */

void BeadFixer2::openFile()
{
  QString qname;
  const char *filter[] = {"Align log files (align*.log)", "Log files (*.log)"};
  PlugData *plug = &thisPlug;

  qname  = diaOpenFileName(this, "Select Tiltalign log file", 2, filter);
  
  if (qname.isEmpty())
    return;

  if (plug->filename != NULL)
    free(plug->filename);
      
  plug->filename = strdup(qname.toLatin1());
  reread(0);

  if (plug->fp != NULL)
    rereadBut->setEnabled(true);    
  if (plug->fp != NULL)
    nextLocalBut->setEnabled(true);    

  return;
}


/* Read or reread the tiltalign log file whose name was already obtained */

void BeadFixer2::reread(int skipopen)
{
  PlugData *plug = &thisPlug;

  char line[MAXLINE];
  char *arealine;
  int newstyle, oldstyle = 0;
  int found = 0;
  Iobj *xobj = ivwGetExtraObject(plug->view);

  // Initialize extra object
  clearExtraObj();
  imodObjectSetColor(xobj, 1., 0., 0.);
  imodObjectSetValue(xobj, IobjFlagClosed, 0);

  if (plug->filename == NULL) 
    return;
  if (!skipopen) {
    if (plug->fp != NULL)
      fclose(plug->fp);
    plug->fp = fopen(plug->filename, "r");
    wprint("Rereading file.\n");

    plug->curArea = 0;
    if (!plug->areaMax) {
      plug->areaList = (AreaData *)malloc(10 * sizeof(AreaData));
      if (!plug->areaList) {
        wprint("\aMemory error in bead fixer!");
        close();
        return;
      }
      plug->areaMax = 10;
    }
    plug->areaList[0].areaX = 0;
    plug->areaList[0].areaY = 0;
    plug->areaList[0].lastSeen = -1;
    plug->offsetSize = 0;
    backUpBut->setEnabled(false);    

  } else {
    while (!found && fgets(line, MAXLINE, plug->fp) != NULL) {
      arealine = strstr(line,"Doing local area");
      if (arealine) {
        arealine[22]=0x00;
        wprint("Skipping to next local set, area%s.\n",
               &arealine[16]);
        found = 1;

        // Keep track of the area data
        plug->curArea++;
        if (plug->curArea >= plug->areaMax) {
          plug->areaMax += 10;
          plug->areaList = (AreaData *)realloc(plug->areaList, 
                                   plug->areaMax * sizeof(AreaData));
          if (!plug->areaList) {
            wprint("\aMemory error in bead fixer!");
            close();
            return;
          }
        }
        sscanf(&arealine[16], "%d %d", &plug->areaList[plug->curArea].areaX, 
               &plug->areaList[plug->curArea].areaY);
        plug->areaList[plug->curArea].lastSeen = -1;
      }
    }
    if (!found) {
      wprint("No more local sets.\n");
      return;
    }
  }

  if(plug->fp == NULL) {
    wprint("Error opening file!\n");
    return;
  }
    
  while(fgets(line, MAXLINE, plug->fp) != NULL) {
    newstyle = strstr(line,"   #     #     #      X         Y        X")
      != NULL;
    if (!newstyle)
      oldstyle = strstr(line,"   #     #      X         Y        X")
        != NULL;
    if (newstyle || oldstyle) {
      plug->objcont = newstyle;
      nextResBut->setEnabled(true);    
      nextLocalBut->setEnabled(true);    
      plug->residok = 1;
      return;
    }
  }

  wprint("Residual data not found\n");
  nextResBut->setEnabled(false);    
  nextLocalBut->setEnabled(false);    
  plug->residok = 0;
  return;
}


/* Jump to the next point with a big residual */

void BeadFixer2::nextRes()
{
  char line[MAXLINE];
  char *getres;
  int inobj, incont, inpt, inview, curpt, obj, nobj, cont, ncont, ipt, npnt;
  int obsav, cosav, ptsav, i;
  int found = 0;
  float xc, yc, xr, yr, sd, resval;
  Iobj *ob;
  Icont *con;
  Ipoint *pts;
  Ipoint tpt;
  float headLen = 2.5;
  long offsetBefore;

  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);
  ivwControlActive(plug->view, 0);

  if(plug->fp == NULL || plug->residok == 0) return;

  do {
    offsetBefore = ftell(plug->fp);
    getres = fgets(line, MAXLINE, plug->fp);
    // wprint("%s", line);
    if(getres == NULL || strlen(line) <3) {
      wprint("No more residuals in this list\n");
      nextResBut->setEnabled(false);    
      plug->residok=0;
      return;
    }
    if (plug->objcont)
      sscanf(line, "%d %d %d %f %f %f %f %f", 
             &inobj, &incont, &inview, &xc, &yc, &xr, &yr, &sd);

    else {
      sscanf(line, "%d %d %f %f %f %f %f", 
             &inpt, &inview, &xc, &yc, &xr, &yr, &sd);
      inobj = 1;
      incont = inpt;
    }

    /* See if point is on list */
    found = 0;
    for (i = 0; i < plug->listsize && !found; i++)
      if (inobj == plug->lookedlist[i].obj && 
          incont == plug->lookedlist[i].cont
          && inview == plug->lookedlist[i].view)
        found = 1;

    /* Continue with next point if looking once and this point was found
       on the list */
  } while (plug->lookonce && found);

  /* Add point to list if it wasn't found */
  if (!found) {
    if (plug->listsize >= plug->listmax) {
      if (plug->listmax)
        plug->lookedlist = (residpt *)realloc
          (plug->lookedlist, (plug->listmax + 100) * 
           sizeof(residpt));
      else
        plug->lookedlist = (residpt *)malloc(100 * sizeof(residpt));
      plug->listmax += 100;
    }
    plug->lookedlist[plug->listsize].obj = inobj;
    plug->lookedlist[plug->listsize].cont = incont;
    plug->lookedlist[plug->listsize++].view = inview;
  }

  found = 0;
  curpt=0;
  nobj = imodGetMaxObject(theModel); 
  imodGetIndex(theModel, &obsav, &cosav, &ptsav);

  if (plug->objcont) {

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
        if(curpt == inpt) {
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
      wprint("Residual =%6.2f (%5.1f,%5.1f),%5.2f SDs\n",
              resval, xr, yr, sd);
      plug->xresid = xr;
      plug->yresid = yr;
      plug->objlook = obj;
      plug->contlook = cont;
      plug->ptlook = ipt;
      plug->curmoved = 0;
      movePointBut->setEnabled(true);

      // Make an arrow in the extra object
      con = imodContourNew();
      if (con) {
	clearExtraObj();
        ob = ivwGetExtraObject(plug->view);
        imodPointAppend(con, --pts);
        tpt.x = pts->x + xr;
        tpt.y = pts->y + yr;
        tpt.z = pts->z;
        imodPointAppend(con, &tpt);
        tpt.x -= 0.707 * (xr - yr) * headLen / resval;
        tpt.y -= 0.707 * (xr + yr) * headLen / resval;
        imodPointAppend(con, &tpt);
        tpt.x = pts->x + xr;
        tpt.y = pts->y + yr;
        imodPointAppend(con, &tpt);
        tpt.x -= 0.707 * (xr + yr) * headLen / resval;
        tpt.y -= 0.707 * (-xr + yr) * headLen / resval;
        imodPointAppend(con, &tpt);
        imodObjectAddContour(ob, con);
        free(con);
      }
      ivwRedraw(plug->view);

      // Keep track of starting offset for reading this point
      if (plug->offsetSize >= plug->offsetMax) {
        if (!plug->offsetMax)
          plug->offsetList = (long *)malloc(100 * sizeof(long));
        else
          plug->offsetList = (long *)realloc(plug->offsetList, 
                                       (100 + plug->offsetMax) * sizeof(long));
        plug->offsetMax += 100;
        if (!plug->offsetList) {
          wprint("\aMemory error in bead fixer!");
          close();
          return;
        }
      }
      plug->areaList[plug->curArea].lastSeen = plug->offsetSize;
      plug->offsetList[plug->offsetSize++] = offsetBefore;
      backUpBut->setEnabled(plug->offsetSize > 1);    
      return;
    }
  }
  wprint("\aPoint not found in contour!\n");
  imodSetIndex(theModel, obsav, cosav, ptsav);
  return;
}
 
// Go back to last point
void BeadFixer2::backUp()
{
  PlugData *plug = &thisPlug;
  int i, areaX, areaY;
  if (plug->offsetSize < 2)
    return;

  // Back up offset and seek to file position before line
  plug->offsetSize -= 2;
  fseek(plug->fp, plug->offsetList[plug->offsetSize], SEEK_SET);

  // Look for point last seen in previous area and if so, change area
  for (i = 0; i < plug->curArea; i++) {
    if (plug->areaList[i].lastSeen >= plug->offsetSize) {
      plug->curArea = i;
      areaX = plug->areaList[i].areaX;
      areaY = plug->areaList[i].areaY;
      if (!areaX && !areaY)
        wprint("Backing up into global solution residuals.\n");
      else
        wprint("Backing up into local area %d %d.\n", areaX, areaY);
      break;
    }
  }

  // Turn off look once flag, set flag that there is a resid, and get residual
  i = plug->lookonce;
  plug->lookonce = 0;
  plug->residok = 1;
  nextResBut->setEnabled(true);
  nextRes();
  plug->lookonce = i;
}

void BeadFixer2::onceToggled(bool state)
{
  PlugData *plug = &thisPlug;
  plug->lookonce = state ? 1 : 0;
}

void BeadFixer2::clearList()
{
  PlugData *plug = &thisPlug;
  plug->listsize = 0;
}

void BeadFixer2::movePoint()
{
  int obj, cont, pt;
  Ipoint *pts;
  Icont *con;
  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);
  ivwControlActive(plug->view, 0);
     
  if(plug->fp == NULL || plug->curmoved  || plug->objlook < 0) 
    return;
  imodGetIndex(theModel, &obj, &cont, &pt);
  if (obj != plug->objlook || cont != plug->contlook || pt != plug->ptlook) {
    wprint("\aThe current point is not the same as the point with the "
           "last residual examined!\n");
    return;
  }

  /* move the point */
  con = imodContourGet(theModel);
  pts = imodContourGetPoints(con);
  plug->oldpt = pts[pt];
  plug->newpt = plug->oldpt;
  plug->newpt.x += plug->xresid;
  plug->newpt.y += plug->yresid;
  pts[pt] = plug->newpt;
  plug->objmoved = plug->objlook;
  plug->contmoved = plug->contlook;
  plug->ptmoved = plug->ptlook;

  /* set flags and buttons */
  plug->curmoved = 1;
  plug->didmove = 1;
  movePointBut->setEnabled(false);
  undoMoveBut->setEnabled(true);
  ivwRedraw(plug->view);
}

void BeadFixer2::undoMove()
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
     
  if(plug->fp == NULL || !plug->didmove) 
    return;
  imodGetIndex(theModel, &obsav, &cosav, &ptsav);

  nobj = imodGetMaxObject(theModel); 

  if (plug->objmoved < nobj) {
    imodSetIndex(theModel, plug->objmoved, plug->contmoved, 
                 plug->ptmoved);
    ob = imodObjectGet(theModel);
    ncont = imodObjectGetMaxContour(ob);
    if (plug->contmoved < ncont) {
      con = imodContourGet(theModel);
      pts = imodContourGetPoints(con);
      if (plug->ptmoved < imodContourGetMaxPoint(con)) {

        /* Check that point is within 10 pixels of where it was */
        dx = pts[plug->ptmoved].x - plug->newpt.x;
        dy = pts[plug->ptmoved].y - plug->newpt.y;
        distsq = dx * dx + dy * dy;
        if (distsq < 100. && pts[plug->ptmoved].z == plug->newpt.z) {
          pts[plug->ptmoved] = plug->oldpt;
          plug->didmove = 0;
          plug->curmoved = 0;
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

int BeadFixer2::foundgap(int obj, int cont, int ipt, int before)
{
  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);

  if(plug->lastob == obj && plug->lastco == cont && plug->lastpt == ipt
     && plug->lastbefore == before)
    return 1;

  plug->lastob = obj;
  plug->lastco = cont;
  plug->lastpt = ipt;
  plug->lastbefore = before;
  imodSetIndex(theModel, obj, cont, ipt);
  ivwRedraw(plug->view);
  return 0;
}

void BeadFixer2::clearExtraObj()
{
  PlugData *plug = &thisPlug;
  Iobj *obj = ivwGetExtraObject(plug->view);
  int ncont = imodObjectGetMaxContour(obj);
  int co;
  Icont *cont;
  if (!ncont)
    return;

  // Get the contour pointer.  "Remove" contours from the end, then delete
  // and free the contour data
  cont = imodObjectGetContour(obj, 0);
  for (co = ncont - 1; co >= 0; co--)
    imodObjectRemoveContour(obj, co);
  imodContoursDelete(cont, ncont);
}


/* Jump to next gap in the model, or place where it is not tracked to first
   or last section */

void BeadFixer2::nextGap()
{
  int  obj, nobj, cont, ncont, ipt, npnt;
  int obsav, cosav, ptsav, curob, curco, curpt, lookback;
  int iptmin, iptmax, iztst, ipt2, foundnext;
  float zcur, zmin, zmax;
  Iobj *ob;
  Icont *con;
  Ipoint *pts;

  PlugData *plug = &thisPlug;
  Imod *theModel = ivwGetModel(plug->view);

  /* This is needed to make button press behave just like hotkey in syncing
     the image */
  ivwControlActive(plug->view, 0);

  con = imodContourGet(theModel);
  imodGetIndex(theModel, &obsav, &cosav, &ptsav);

  curob = obsav;
  curco = cosav;
  curpt = ptsav;
  lookback = 0;

  if(!con || plug->ifdidgap == 0) {
    curob = curco = curpt = 0;
    plug->lastob = -1;
    plug->lastbefore = 0;
    lookback = 1;
  }

  plug->ifdidgap = 1;

  /* If last one was at start of track, go back to first point of contour */
  if (plug->lastbefore)
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
        if(zmax + 1.1f < plug->zsize) {
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

static const char *buttonLabels[] = {"Done", "Help"};
static const char *buttonTips[] = {"Close Bead Fixer", "Open help window"};

BeadFixer2::BeadFixer2(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, buttonLabels, buttonTips, true, "Bead Fixer", "", name)
{
  QPushButton *button;
  QCheckBox *box;
  QString qstr;
  PlugData *plug = &thisPlug;

  int width2 = fontMetrics().width("Move Point by Residual");
  int width = fontMetrics().width("Open Tiltalign Log File");
  if (width < width2)
    width = width2;
  width = (int)(1.15 * width);

  button = diaPushButton("Go to Next Gap", this, mLayout);
  connect(button, SIGNAL(clicked()), this, SLOT(nextGap()));
  button->setFixedWidth(width);
  button->setToolTip("Go to gap in model - Hot key: spacebar");

  button = diaPushButton("Open Tiltalign Log File", this, mLayout);
  connect(button, SIGNAL(clicked()), this, SLOT(openFile()));
  button->setFixedWidth(width);
  button->setToolTip("Select an alignment log file to open");

  rereadBut = diaPushButton("Reread Log File", this, mLayout);
  connect(rereadBut, SIGNAL(clicked()), this, SLOT(rereadFile()));
  rereadBut->setEnabled(false);
  rereadBut->setFixedWidth(width);
  rereadBut->setToolTip("Read the previously specified file again");

  nextLocalBut = diaPushButton("Go to Next Local Set", this, mLayout);
  connect(nextLocalBut, SIGNAL(clicked()), this, SLOT(nextLocal()));
  nextLocalBut->setEnabled(false);
  nextLocalBut->setFixedWidth(width);
  nextLocalBut->setToolTip("Skip to residuals in next local area");

  nextResBut = diaPushButton("Go to Next Big Residual", this, mLayout);
  connect(nextResBut, SIGNAL(clicked()), this, SLOT(nextRes()));
  nextResBut->setEnabled(false);
  nextResBut->setFixedWidth(width);
  nextResBut->setToolTip("Show next highest residual - Hot key: apostrophe");
  movePointBut = diaPushButton("Move Point by Residual", this, mLayout);
  connect(movePointBut, SIGNAL(clicked()), this, SLOT(movePoint()));
  movePointBut->setEnabled(false);
  movePointBut->setFixedWidth(width);
  movePointBut->setToolTip("Move point to position that fits alignment"
                           " solution - Hot key: semicolon");

  undoMoveBut = diaPushButton("Undo Move", this, mLayout);
  connect(undoMoveBut, SIGNAL(clicked()), this, SLOT(undoMove()));
  undoMoveBut->setEnabled(false);
  undoMoveBut->setFixedWidth(width);
  undoMoveBut->setToolTip("Move point back to previous position - Hot key: U");

  backUpBut = diaPushButton("Back Up to Last Point", this, mLayout);
  connect(backUpBut, SIGNAL(clicked()), this, SLOT(backUp()));
  backUpBut->setEnabled(false);
  backUpBut->setFixedWidth(width);
  backUpBut->setToolTip("Back up to last point examined");

  box = diaCheckBox("Examine Points Once", this, mLayout);
  connect(box, SIGNAL(toggled(bool)), this, SLOT(onceToggled(bool)));
  diaSetChecked(box, plug->lookonce != 0);
  box->setToolTip("Skip over points examined before");

  clearListBut = diaPushButton("Clear Examined List", this, mLayout);
  connect(clearListBut, SIGNAL(clicked()), this, SLOT(clearList()));
  clearListBut->setFixedWidth(width);
  clearListBut->setToolTip("Allow all points to be examined again");

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));

}

// This illustrates how to show a help page
void BeadFixer2::buttonPressed(int which)
{
  QString str;
  if (!which)
    close();
  else {
    // For a plugin incorporated into IMOD, this is the path in the compiled qhc file
    // Always have an anchor #TOP at the top so Qt Assistant starts at the top
    imodShowHelpPage("../plughelp/beadfix2.html#TOP");

    // Otherwise, use dia_vasmsg to pass a string from within the code (can contain html).
  }
}

// The window is closing, remove from manager
void BeadFixer2::closeEvent ( QCloseEvent * e )
{
  PlugData *plug = &thisPlug;
  imodDialogManager.remove((QWidget *)plug->window);
  clearExtraObj();
  ivwFreeExtraObject(plug->view, plug->extraObjNum);
  ivwTrackMouseForPlugs(plug->view, 0);

  plug->view = NULL;
  plug->window = NULL;
  if (plug->listmax)
    free(plug->lookedlist);
  plug->listmax = 0;
  if (plug->filename)
    free(plug->filename);
  if (plug->offsetList)
    free(plug->offsetList);
  if (plug->areaList)
    free(plug->areaList);
  e->accept();
}

// Close on escape, pass on keys
void BeadFixer2::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Escape)
    close();
  else
    ivwControlKey(0, e);
}

void BeadFixer2::keyReleaseEvent ( QKeyEvent * e )
{
  ivwControlKey(1, e);
}
