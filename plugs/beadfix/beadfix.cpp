/*
 *  beadfix.c -- Special plugin for fixing fiducial models
 *
 */

/*****************************************************************************
 *   Copyright (C) 1997-2000 by Boulder Laboratory for 3-Dimensional Fine    *
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
    Revision 3.11  2004/09/24 17:55:38  mast
    Added example of executing message

    Revision 3.10  2004/06/01 00:52:38  mast
    Set up as test for plugin building

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

/* include needed Qt headers and imod headers
 * This version is to test building as a plugin in with public interfaces only
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <qpushbutton.h>
#include <qcheckbox.h>
#include <qtooltip.h>
#include <qstringlist.h>

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
  
}PlugData;


static PlugData thisPlug = { 0, 0 };

/*
 * Called by the imod plugin load function. 
 */
char *imodPlugInfo(int *type)
{
  if (type)
    *type = IMOD_PLUG_MENU + IMOD_PLUG_KEYS + IMOD_PLUG_MESSAGE;
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
  ctrl   = event->state() & Qt::ControlButton;
  shift  = event->state() & Qt::ShiftButton;
    
    
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

  /*
   * This creates the plug window.
   */
  plug->window  = new BeadFixer2(imodDialogManager.parent(IMOD_DIALOG),
                                "bead fixer");

  imodDialogManager.add((QWidget *)plug->window, IMOD_DIALOG);
  plug->window->show();
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

/* Open a tiltalign log file to find points with big residuals */

void BeadFixer2::openFile()
{
  QString qname;
  char *filter[] = {"Align log files (align*.log)", "Log files (*.log)"};
  PlugData *plug = &thisPlug;

  qname  = diaOpenFileName(this, "Select Tiltalign log file", 2, filter);
  
  if (qname.isEmpty())
    return;

  if (plug->filename != NULL)
    free(plug->filename);
      
  plug->filename = strdup(qname.latin1());
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
  int obj, cont, pt;
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
  char line[MAXLINE];

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

static char *buttonLabels[] = {"Done", "Help"};
static char *buttonTips[] = {"Close Bead Fixer", "Open help window"};

BeadFixer2::BeadFixer2(QWidget *parent, const char *name)
  : DialogFrame(parent, 2, buttonLabels, buttonTips, true, "Bead Fixer", "",
                name)
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

  button = diaPushButton("Go to Next Gap", this, (QBoxLayout *)mLayout);
  connect(button, SIGNAL(clicked()), this, SLOT(nextGap()));
  button->setFixedWidth(width);
  QToolTip::add(button, "Go to gap in model - Hot key: spacebar");

  button = diaPushButton("Open Tiltalign Log File", this, (QBoxLayout *)mLayout);
  connect(button, SIGNAL(clicked()), this, SLOT(openFile()));
  button->setFixedWidth(width);
  QToolTip::add(button, "Select an alignment log file to open");

  rereadBut = diaPushButton("Reread Log File", this, (QBoxLayout *)mLayout);
  connect(rereadBut, SIGNAL(clicked()), this, SLOT(rereadFile()));
  rereadBut->setEnabled(false);
  rereadBut->setFixedWidth(width);
  QToolTip::add(rereadBut, "Read the previously specified file again");

  nextLocalBut = diaPushButton("Go to Next Local Set", this, 
                               (QBoxLayout *)mLayout);
  connect(nextLocalBut, SIGNAL(clicked()), this, SLOT(nextLocal()));
  nextLocalBut->setEnabled(false);
  nextLocalBut->setFixedWidth(width);
  QToolTip::add(nextLocalBut, "Skip to residuals in next local area");

  nextResBut = diaPushButton("Go to Next Big Residual", this,
                             (QBoxLayout *)mLayout);
  connect(nextResBut, SIGNAL(clicked()), this, SLOT(nextRes()));
  nextResBut->setEnabled(false);
  nextResBut->setFixedWidth(width);
  QToolTip::add(nextResBut, "Show next highest residual - Hot key: apostrophe");
  movePointBut = diaPushButton("Move Point by Residual", this, 
                               (QBoxLayout *)mLayout);
  connect(movePointBut, SIGNAL(clicked()), this, SLOT(movePoint()));
  movePointBut->setEnabled(false);
  movePointBut->setFixedWidth(width);
  QToolTip::add(movePointBut, "Move point to position that fits alignment"
                " solution - Hot key: semicolon");

  undoMoveBut = diaPushButton("Undo Move", this, (QBoxLayout *)mLayout);
  connect(undoMoveBut, SIGNAL(clicked()), this, SLOT(undoMove()));
  undoMoveBut->setEnabled(false);
  undoMoveBut->setFixedWidth(width);
  QToolTip::add(undoMoveBut, 
                "Move point back to previous position - Hot key: U");

  backUpBut = diaPushButton("Back Up to Last Point", this,
                            (QBoxLayout *)mLayout);
  connect(backUpBut, SIGNAL(clicked()), this, SLOT(backUp()));
  backUpBut->setEnabled(false);
  backUpBut->setFixedWidth(width);
  QToolTip::add(backUpBut, "Back up to last point examined");

  box = diaCheckBox("Examine Points Once", this, (QBoxLayout *)mLayout);
  connect(box, SIGNAL(toggled(bool)), this, SLOT(onceToggled(bool)));
  diaSetChecked(box, plug->lookonce != 0);
  QToolTip::add(box, "Skip over points examined before");

  clearListBut = diaPushButton("Clear Examined List", this, 
                               (QBoxLayout *)mLayout);
  connect(clearListBut, SIGNAL(clicked()), this, SLOT(clearList()));
  clearListBut->setFixedWidth(width);
  QToolTip::add(clearListBut, "Allow all points to be examined again");

  connect(this, SIGNAL(actionPressed(int)), this, SLOT(buttonPressed(int)));

}

void BeadFixer2::buttonPressed(int which)
{
  if (!which)
    close();
  else
    dia_vasmsg
      ("Bead Fixer Plugin Help\n",
       "--------------------------\n\n"
       "This Plugin makes it easier to fix tilt series fiducial "
       "models in two ways.  It has a function for finding the next "
       "gap or untracked place in the model and making that be the "
       "current point.  It also will read the log file from running "
       "Tiltalign and move to points with high residuals.\n\n"
       "Hot Key Summary\n"
       "---------------------\n"
       "spacebar\tGo to next gap\n"
       "' (apostrophe)\tGo to next big residual\n"
       ";\t\tMove point by resiual\n"
       "U\t\tUndo last moved point\n\n",
       "Select the Go to Next Gap button or hit the space bar key to "
       "move to the next gap or incomplete place in the model.  If "
       "there is a gap, the point BEFORE the gap will be displayed.  "
       "If a contour does not start on the first section, the first "
       "point in the contour (which is AFTER the gap) will be "
       "displayed and a message will be printed.  The program "
       "searches forward from the current point except the first time "
       "when the function is selected.  To begin the search at the "
       "beginning of the model again, detach from the current point "
       "by clicking the left mouse button far from any model point, "
       "then select the Next Gap function.\n\n"
       "Select the Open Tiltalign Log File button to open a log "
       "file.\n\n"
       "Select the Go to Next Big Residual button or hit the "
       "apostrophe key to move to the next point in the log file "
       "with a big residual.\n\n"
       "The X and Y displacements implied by the residual are printed "
       "in the Info window.  Also, the Zap window will draw an arrow "
       "corresponding to these displacements.  If the arrow ends at the place "
       "where the point should be located (i.e., the center of the bead), "
       "then select the Move Point by "
       "Residual button or push the semicolon key to move the point "
       "by those amounts in X and Y.\n\n"
       "After moving a point by its residual, select the Undo Move "
       "button or push the U key to undo the move.\n\n"
       "Select the Reread Log File button to reread the file, or a "
       "new version of it after rerunning Tiltalign.\n\n",
       "Select the Go to Next Local Set button if you have a log file "
       "with a series of local alignments and want to skip to the "
       "residuals from the next local alignment.\n\n",
       "Select the Back Up to Last Point button if you want to go back to the "
       "last residual that you examined.  This will not necessarily be the "
       "previous residual in the file, if you skipped over points by going to "
       "the next local set.  However, you can then work forward through the "
       "points that you skipped over.\n\n",
       "The program keeps a list of each point whose residual has "
       "been examined.  If the Examine Points Once toggle is "
       "selected, then any point already on the list is skipped over "
       "when you Go to Next Big Residual.  A point is added to the "
       "list regardless of whether Examine Points Once is selected."
       "\n\n"
       "Select the Clear Examined List button to empty the list of "
       "points that have been examined.  The list is not cleared when "
       "Examine Points Once is switched on or off.  To see some "
       "points for the second time, turn off Examine Points Once; to "
       "see all points again, push Clear Examined List.  When you Back Up to "
       "Last Point, you will see that point a second time, but if you move "
       "forward again, you will skip over all of the points that you have "
       "seen before unless you turn off Examine Points Once.\n",
       NULL);
}

// The window is closing, remove from manager
void BeadFixer2::closeEvent ( QCloseEvent * e )
{
  PlugData *plug = &thisPlug;
  imodDialogManager.remove((QWidget *)plug->window);
  clearExtraObj();

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
