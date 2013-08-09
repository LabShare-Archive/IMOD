/*
 *  finegrain.cpp - provide ability to change fine-grain display properties
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 * 
 * $Id$
 */

#include <qgl.h>
#include "form_finegrain.h"
#include "finegrain.h"
#include "imod.h"
#include "imod_edit.h"
#include "imod_input.h"
#include "info_cb.h"
#include "display.h"
#include "control.h"
#include "undoredo.h"
#include "b3dgfx.h"
#include "xzap.h"
#include "mv_light.h"

typedef struct fg_data_struct {
  FineGrainForm *dia;
  ImodView *vw;
  Iobj *obj;
  Icont *cont;
  int ptContSurf;
  int ptLoaded;
  int contLoaded;
  int objLoaded;
  int surfLoaded;
  int stateFlags;
  int rangeEnd;
  int showConnects;
  int stippleGaps;
  int changeAll;
  DrawProps contProps;
  Istore store;
} FgData;

static FgData fgd = {NULL, NULL, NULL, NULL, 0, -1, -1, -1, -1, 0, 0, 0, 0, 0};

static int getLoadedObjCont(int addType);
static int findNextChange(Ilist *list, int index, int surfFlag);
static void insertAndUpdate(int type);
static void handleContChange(Iobj *obj, int co, int surf, DrawProps *contProps,
                             DrawProps *ptProps, int *stateFlags, 
                             int handleFlags, int selected, int scaleThick);
static void ifgHandleStateChange(Iobj *obj, DrawProps *defProps, 
                                 DrawProps *ptProps, int *stateFlags, 
                                 int *changeFlags, int handleFlags, 
                                 int selected, int scaleThick);
static void ifgMapFalseColor(int gray, int *red, int *green, int *blue);
static void ifgHandleValue1(DrawProps *defProps, DrawProps *contProps, 
                            int *stateFlags, int *changeFlags);

/*
 * Open or raise the window
 */
void fineGrainOpen(ImodView *vw)
{
  if (fgd.dia){
    fgd.dia->raise();
    return;
  }
  fgd.vw = vw;
     
  fgd.dia = new FineGrainForm(imodDialogManager.parent(IMOD_DIALOG), 
                              Qt::Window);
  
  fineGrainUpdate();
  imodDialogManager.add((QWidget *)fgd.dia, IMOD_DIALOG);
  adjustGeometryAndShow((QWidget *)fgd.dia, IMOD_DIALOG);
}

/*
 * Update the window
 */
void fineGrainUpdate()
{
  int ob, co, pt, surf, contState, surfState;
  Imod *imod;
  Iobj *obj;
  Icont *cont;
  bool enable, nextEnable;
  DrawProps props, defProps;
  fgd.stateFlags = 0;
  if (!fgd.dia)
    return;

  imod = fgd.vw->imod;
  imodGetIndex(imod, &ob, &co, &pt);
  obj  = imodObjectGet(imod);
  cont = imodContourGet(imod);
  surf = -1;
  if (cont)
    surf = cont->surf;

  enable = (!fgd.ptContSurf && pt >= 0) || (fgd.ptContSurf && co >= 0);
  nextEnable = false;
  fgd.objLoaded = ob;
  fgd.contLoaded = co;
  fgd.ptLoaded = pt;
  fgd.surfLoaded = surf;

  // get point properties if valid point, otherwise get object and 
  // contour/surface properties
  if (!fgd.ptContSurf && pt >= 0) {
    fgd.stateFlags = istorePointDrawProps(obj, &fgd.contProps, &props, co, pt);
    nextEnable = (cont && (findNextChange(cont->store, pt, 0) >= 0));
  } else {
    if (!obj) {
      obj = imodObjectNew();
      istoreDefaultDrawProps(obj, &defProps);
      imodObjectDelete(obj);
    } else
      istoreDefaultDrawProps(obj, &defProps);
    if (cont) {
      istoreContSurfDrawProps(obj->store, &defProps, &props, 
                              fgd.ptContSurf > 1 ? -1 : co, surf, &contState,
                              &surfState);
      fgd.stateFlags = fgd.ptContSurf == 2 ? surfState : contState;
      nextEnable = (findNextChange(obj->store, fgd.ptContSurf == 2 ? surf : co,
                                   fgd.ptContSurf == 2 ? GEN_STORE_SURFACE : 0)
                    >= 0);
    }
    else
      props = defProps;
  }
  fgd.dia->update(fgd.ptContSurf, enable, &props, fgd.stateFlags, nextEnable);
}

int ifgShowConnections()
{
  return fgd.showConnects ;
}

int ifgStippleGaps()
{
  return fgd.stippleGaps ;
}

int ifgGetChangeAll()
{
  return fgd.changeAll;
}

/*
 * Change the selection of point/cont/surf
 */
void ifgPtContSurfSelected(int which)
{
  fgd.ptContSurf = which;
  fineGrainUpdate();
}

void ifgChangeAllToggled(bool state)
{
  fgd.changeAll = state ? 1 : 0;
}

/*
 * Go to next change
 */
void ifgGotoNextChange()
{
  Imod *imod = fgd.vw->imod;
  Iobj *obj;
  Icont *cont;
  int index;

  if (getLoadedObjCont(-1))
    return;
  obj  = imodObjectGet(imod);
  cont = imodContourGet(imod);

  switch (fgd.ptContSurf) {
  case 0:
    index = findNextChange(cont->store, fgd.ptLoaded, 0);
    if (index >= 0)
      imodSetIndex(imod, fgd.objLoaded, fgd.contLoaded, index);
    break;
  case 1:
    index = findNextChange(obj->store, fgd.contLoaded, 0);
    if (index >= 0) {
      imodSetIndex(imod, fgd.objLoaded, index, fgd.ptLoaded);
      inputRestorePointIndex(fgd.vw);
    }
    break;

  case 2:
    index = findNextChange(obj->store, fgd.surfLoaded, GEN_STORE_SURFACE);
    if (index >= 0)
      inputGotoSurface(fgd.vw, index);
    break;
  }

  // Copied these from imod_info_cb
  ivwControlActive(fgd.vw, 0);
  imod_setxyzmouse();
}

/*
 * Individual color changes entries: check for whether the change is to the
 * default and insert an end if appropriate
 */
void ifgLineColorChanged(int r, int g, int b)
{
  if (getLoadedObjCont(GEN_STORE_COLOR))
    return;
  if (r == (int)(255. * fgd.contProps.red + 0.5) &&
      g == (int)(255. * fgd.contProps.green + 0.5) && 
      b == (int)(255. * fgd.contProps.blue + 0.5)) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_COLOR))
      ifgEndChange(GEN_STORE_COLOR);
    return;
  }
  ifgColorChanged(GEN_STORE_COLOR, r, g, b);
}

void ifgFillColorChanged(int r, int g, int b)
{
  if (getLoadedObjCont(GEN_STORE_FCOLOR))
    return;
  if (r == (int)(255. * fgd.contProps.fillRed + 0.5) &&
      g == (int)(255. * fgd.contProps.fillGreen + 0.5) && 
      b == (int)(255. * fgd.contProps.fillBlue + 0.5)) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_FCOLOR))
      ifgEndChange(GEN_STORE_FCOLOR);
    return;
  }
  ifgColorChanged(GEN_STORE_FCOLOR, r, g, b);
}

/*
 * Common color change routine
 */
void ifgColorChanged(int type, int r, int g, int b)
{
  fgd.store.value.b[0] = r;
  fgd.store.value.b[1] = g;
  fgd.store.value.b[2] = b;
  fgd.store.flags = GEN_STORE_BYTE << 2;
  insertAndUpdate(type);
}

/*
 * Routines for other changes; also need to check for a change to the default
 */
void ifgTransChanged(int value)
{
  if (getLoadedObjCont(GEN_STORE_TRANS))
    return;
  if (fgd.contProps.trans == value) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_TRANS))
      ifgEndChange(GEN_STORE_TRANS);
    return;
  }
  ifgIntChanged(GEN_STORE_TRANS, value);
}

void ifgWidth2DChanged(int value)
{
  if (getLoadedObjCont(GEN_STORE_2DWIDTH))
    return;
  if (fgd.contProps.linewidth2 == value) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_2DWIDTH))
      ifgEndChange(GEN_STORE_2DWIDTH);
    return;
  }
  ifgIntChanged(GEN_STORE_2DWIDTH, value);
}

void ifgWidth3DChanged(int value)
{
  if (getLoadedObjCont(GEN_STORE_3DWIDTH))
    return;
  if (fgd.contProps.linewidth == value) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_3DWIDTH))
      ifgEndChange(GEN_STORE_3DWIDTH);
    return;
  }
  ifgIntChanged(GEN_STORE_3DWIDTH, value);
}

void ifgSymsizeChanged(int value)
{
  if (getLoadedObjCont(GEN_STORE_SYMSIZE))
    return;
  if (fgd.contProps.symsize == value) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_SYMSIZE))
      ifgEndChange(GEN_STORE_SYMSIZE);
    return;
  }
  ifgIntChanged(GEN_STORE_SYMSIZE, value);
}

/*
 * Common routine for change of an int quantity
 */
void ifgIntChanged(int type, int value)
{
  fgd.store.value.i = value;
  fgd.store.flags = 0;
  insertAndUpdate(type);
}

/*
 * Symbol type is combined with fill value
 */
void ifgSymtypeChanged(int symtype, bool filled)
{
  int contFilled = fgd.contProps.symflags & IOBJ_SYMF_FILL;
  if (getLoadedObjCont(GEN_STORE_SYMTYPE))
    return;

  if (fgd.contProps.symtype == symtype && 
      ((contFilled && filled) || (!contFilled && !filled))) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_SYMTYPE))
      ifgEndChange(GEN_STORE_SYMTYPE);
    return;
  }

  if (filled)
    symtype = -1 - symtype;
  fgd.store.value.i = symtype;
  fgd.store.flags = 0;
  insertAndUpdate(GEN_STORE_SYMTYPE);
}

/*
 * End a change (for point data only) 
 */
void ifgEndChange(int type)
{
  if (getLoadedObjCont(-1))
    return;
  fgd.vw->undo->contourDataChg();
  if (istoreEndChange(fgd.cont->store, type, fgd.ptLoaded))
    wprint("\aError modifying storage list to end change\n");
  fgd.vw->undo->finishUnit();
  if (imodDebug('g'))
    istoreDump(fgd.cont->store);
  imod_setxyzmouse();
  fineGrainUpdate();
}

/*
 * Clear out a change sequence for point data, or remove one item for
 * contour data.
 */
void ifgClearChange(int type)
{
  int err, i;
  Iindex *index;
  if (getLoadedObjCont(-1))
    return;
  if (fgd.ptContSurf) {
    fgd.vw->undo->objectPropChg(fgd.objLoaded);

    // If changing all, clear out each legal item on the selection list
    if (fgd.ptContSurf == 1 && fgd.changeAll && type != GEN_STORE_CONNECT &&
        ilistSize(fgd.vw->selectionList) > 1) {
      for (i = 0; i < ilistSize(fgd.vw->selectionList); i++) {
        index = (Iindex *)ilistItem(fgd.vw->selectionList, i);
        if (index->object == fgd.objLoaded && index->contour >= 0 && 
            index->contour < fgd.obj->contsize) {
          err = istoreClearOneIndexItem(fgd.obj->store, type, index->contour,
                                        0);
          if (err)
            break;
        }
      }
    } else {

      // Otherwise just do the one item
      err = istoreClearOneIndexItem
        (fgd.obj->store, type, 
         (fgd.ptContSurf == 2) ? fgd.surfLoaded : fgd.contLoaded,
         (fgd.ptContSurf == 2) ? 1 : 0);
    }
    if (imodDebug('g'))
      istoreDump(fgd.obj->store);
  } else {
    fgd.vw->undo->contourDataChg();
    err = istoreClearChange(fgd.cont->store, type, fgd.ptLoaded);
    if (imodDebug('g'))
      istoreDump(fgd.cont->store);
  }
  if (err)
    wprint("/aError modifying storage list to clear change\n");
  fgd.vw->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  fineGrainUpdate();
}

/*
 * A change in the gap setting for points
 */
void ifgGapChanged(bool state)
{
  if (getLoadedObjCont(-1))
    return;
  if (fgd.ptContSurf) {
    if (state)
      ifgIntChanged(GEN_STORE_GAP, 1);
    else
      ifgClearChange(GEN_STORE_GAP);
    return;
  }

  ifgToggleGap(fgd.vw, fgd.cont, fgd.ptLoaded, state);
  if (imodDebug('g'))
    istoreDump(fgd.cont->store);
  imod_setxyzmouse();
  fineGrainUpdate();
}


/*
 * Externally callable function to toggle a gap at a point
 */
int ifgToggleGap(ImodView *vw, Icont *cont, int ptIndex, bool state)
{
  int retval = 0;
  vw->undo->contourDataChg();
  if (state) {
    fgd.store.type = GEN_STORE_GAP;
    fgd.store.flags = GEN_STORE_ONEPOINT;
    fgd.store.index.i = ptIndex;
    if (istoreAddOneIndexItem(&cont->store, &fgd.store)) {
      wprint("\aError inserting gap into storage list\n");
      retval = 1;
    }
  } else if (istoreClearOneIndexItem(cont->store, GEN_STORE_GAP, ptIndex, 0)) {
    wprint("\aError removing gap from storage list\n");
    retval = 1;
  }
  vw->undo->finishUnit();
  return retval;
}


/*
 * A change in the connection number for points
 */
void ifgConnectChanged(int value)
{
  if (getLoadedObjCont(-1))
    return;
  if (fgd.ptContSurf) {
    if (value)
      ifgIntChanged(GEN_STORE_CONNECT, value);
    else
      ifgClearChange(GEN_STORE_CONNECT);
    return;
  }
  fgd.vw->undo->contourDataChg();
  if (value) {
    fgd.store.type = GEN_STORE_CONNECT;
    fgd.store.flags = GEN_STORE_ONEPOINT;
    fgd.store.index.i = fgd.ptLoaded;
    fgd.store.value.i = value;
    if (istoreAddOneIndexItem(&fgd.cont->store, &fgd.store))
      wprint("\aError inserting connection into storage list\n");
  } else if (istoreClearOneIndexItem(fgd.cont->store, GEN_STORE_CONNECT,
                                     fgd.ptLoaded, 0))
    wprint("\aError removing connection from storage list\n");
  if (imodDebug('g'))
    istoreDump(fgd.cont->store);
  fgd.vw->undo->finishUnit();
  fineGrainUpdate();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

/*
 * Toggling the flag to show connections
 */
void ifgShowConnectChanged(bool state)
{
  fgd.showConnects = state;
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ifgStippleGapsChanged(bool state)
{
  fgd.stippleGaps = state;
  imodDraw(App->cvi, IMOD_DRAW_MOD);
}

void ifgDump()
{
  if (getLoadedObjCont(-1))
    return;
  if (fgd.ptContSurf)
    istoreDump(fgd.obj->store);
  else
    istoreDump(fgd.cont->store);
  fflush(stdout);
}

void ifgHelp()
{
  imodShowHelpPage("finegrain.html#TOP");
}

void ifgClosing()
{
  imodDialogManager.remove((QWidget *)fgd.dia);
  fgd.dia = NULL;
}

/*
 * Check validity of the loaded contour/point and set the object and
 * contour pointers
 */
static int getLoadedObjCont(int addType)
{
  float rbX0, rbX1, rbY0, rbY1;
  int i, start;
  Ipoint *pts;
  Imod *imod = fgd.vw->imod;
  fgd.rangeEnd = -1;
  if (fgd.objLoaded >= imod->objsize) {
    wprint("\aObject last loaded into Fine Grain dialog is no longer valid\n");
    fineGrainUpdate();
    return 1;
  }

  fgd.obj = &imod->obj[fgd.objLoaded];
  if (fgd.contLoaded >= fgd.obj->contsize) {
    wprint("\aContour last loaded into Fine Grain dialog is no longer "
           "valid\n");
    fineGrainUpdate();
    return 1;
  }

  if (fgd.contLoaded < 0) {
    wprint("\aThere is no current contour\n");
    return 1;
  }

  fgd.cont = &fgd.obj->cont[fgd.contLoaded];
  if (fgd.ptContSurf == 2 && fgd.cont->surf != fgd.surfLoaded) {
    wprint("\aContour last loaded into Fine Grain dialog does not have the "
           "same surface number\n");
    fineGrainUpdate();
    return 1;
  }

  if (fgd.ptContSurf == 1 && fgd.changeAll &&
      ilistSize(fgd.vw->selectionList) > 1) {
    if (imodSelectionListQuery(fgd.vw, fgd.objLoaded, fgd.contLoaded) < -1) {
      wprint("\aContour last loaded into Fine Grain dialog is not in the list "
             "of selected contours\n");
      fineGrainUpdate();
      return 1;
    }
  }

  if (!fgd.ptContSurf) {
    if (addType >= 0 && zapRubberbandCoords(rbX0, rbX1, rbY0, rbY1)) {

      // Find first point inside contour
      start = -1;
      pts = fgd.cont->pts;
      for (i = 0; i < fgd.cont->psize; i++) {
        if (pts[i].x >= rbX0 && pts[i].x <= rbX1 && pts[i].y >= rbY0 && 
            pts[i].y <= rbY1) {
          start = i;
          break;
        }
      }
      if (start < 0) {
        wprint("\aNo points in the current contour are inside the rubber band"
               ".\n");
        return 1;
      }

      // Find last point inside contour
      for (i = start + 1; i < fgd.cont->psize; i++) {
        if (!(pts[i].x >= rbX0 && pts[i].x <= rbX1 && pts[i].y >= rbY0 && 
            pts[i].y <= rbY1)) {
          fgd.rangeEnd = i - 1;
          break;
        }
      }
      if (fgd.rangeEnd < 0) {
        if (!start) {
          wprint("\aCurrent contour is entirely inside rubber band; turn off "
                 "rubber band to operate on current point.\n");
          fineGrainUpdate();
          return 1;
        }
        fgd.rangeEnd = fgd.cont->psize - 1;
      }

      // Clear the range
      fgd.ptLoaded = start;
      fgd.vw->undo->contourDataChg();
      istoreClearRange(fgd.cont->store, addType, start, fgd.rangeEnd);

    } else if (fgd.ptLoaded >= fgd.cont->psize) {
      wprint("\aPoint last loaded into Fine Grain dialog is no longer "
             "valid\n");
      fineGrainUpdate();
      return 1;
    }
  }
  return 0;
}

/*
 * Insert an item into the appropriate list and do an update
 */
static void insertAndUpdate(int type)
{
  int err, i;
  Iindex *index;
  fgd.store.type = type;
  fgd.store.flags |= fgd.ptContSurf == 2 ? GEN_STORE_SURFACE : 0;
  if (fgd.ptContSurf) {
    fgd.vw->undo->objectPropChg(fgd.objLoaded);

    // If changing all, change each legal item on the selection list
    if (fgd.ptContSurf == 1 && fgd.changeAll && type != GEN_STORE_CONNECT &&
        ilistSize(fgd.vw->selectionList) > 1) {
      for (i = 0; i < ilistSize(fgd.vw->selectionList); i++) {
        index = (Iindex *)ilistItem(fgd.vw->selectionList, i);
        if (index->object == fgd.objLoaded && index->contour >= 0 && 
            index->contour < fgd.obj->contsize) {
          fgd.store.index.i = index->contour;
          err = istoreAddOneIndexItem(&fgd.obj->store, &fgd.store);
          if (err)
            break;
        }
      }
    } else {

      // Otherwise change the contour or surface
      fgd.store.index.i =  fgd.ptContSurf == 2 ? fgd.surfLoaded : 
        fgd.contLoaded;
      err = istoreAddOneIndexItem(&fgd.obj->store, &fgd.store);
    }
    if (imodDebug('g'))
      istoreDump(fgd.obj->store);
  } else {

    // Point change: start undo unit unless range clear did already, insert
    // the change, then insert an end if doing range
    if (fgd.rangeEnd < 0)
      fgd.vw->undo->contourDataChg();
    fgd.store.index.i = fgd.ptLoaded;
    err = istoreInsertChange(&fgd.cont->store, &fgd.store);
    if (!err && fgd.rangeEnd >= 0) {
      if (istoreEndChange(fgd.cont->store, type, fgd.rangeEnd))
        wprint("\aError modifying storage list to end change\n");
      imodSetIndex(fgd.vw->imod, fgd.objLoaded, fgd.contLoaded, fgd.ptLoaded);
    }
    if (imodDebug('g'))
      istoreDump(fgd.cont->store);
  }

  if (err)
    wprint("\aError inserting change into general storage list\n");
  fgd.vw->undo->finishUnit();
  imod_setxyzmouse();
  fineGrainUpdate();
}

/*
 * Finds the next change in the list after the current index that matches
 * the surface flag
 */
static int findNextChange(Ilist *list, int index, int surfFlag)
{
  Istore *stp;
  int after;
  if (!ilistSize(list))
    return -1;
  istoreLookup(list, index, &after);

  // Loop on items looking for one that matches the surface flag
  for (; after < ilistSize(list) ; after++) {
    stp = istoreItem(list, after);
    if (stp->flags & (GEN_STORE_NOINDEX | 3))
      return -1;
    if ((stp->flags & GEN_STORE_SURFACE) == surfFlag)
      return stp->index.i;
  }
  return -1;
}


/*
 * FUNCTIONS TO HANDLE DISPLAY PROPERTIES, CALLED FROM ELSEWHERE
 */

/* 
 * Modifies the width value up or down if selected is non-zero
 */
int ifgSelectedLineWidth(int width, int selected)
{
  if (selected)
    return (width < 3 ? width + 2 : width / 2);
  return width;
}

/* 
 * Sets up display properties for surface surf
 */
void ifgHandleSurfChange(Iobj *obj, int surf, DrawProps *contProps, 
                         DrawProps *ptProps, int *stateFlags, int handleFlags)
{
  handleContChange(obj, -1, surf, contProps, ptProps, stateFlags, handleFlags,
                   0, 0);
}

/* 
 * Sets up display properties for contour co
 */
int ifgHandleContChange(Iobj *obj, int co, DrawProps *contProps, 
                        DrawProps *ptProps, int *stateFlags, int handleFlags,
                        int selected, int scaleThick)
{
  handleContChange(obj, co, obj->cont[co].surf, contProps, ptProps, stateFlags,
                   handleFlags, selected, scaleThick);
  return (istoreFirstChangeIndex(obj->cont[co].store));
}

/* 
 * Sets up display properties for a contour (co and surf set) or just a 
 * surface (surf set, co < 0).  contProps and ptProps are returned with the
 * properties; stateFlags is returned with flags for which are changes from the
 * object default, handleFlags indicates which to handle, and selected is 1
 * for a selected contour.  scaleThick is the amount to scale line thicknesses
 * by (default 1).
 */
static void handleContChange(Iobj *obj, int co, int surf, DrawProps *contProps,
                             DrawProps *ptProps, int *stateFlags, 
                             int handleFlags, int selected, int scaleThick)
{
  int contState, surfState;
  DrawProps defProps;

  /* Get the properties for the contour */
  istoreDefaultDrawProps(obj, &defProps);
  *stateFlags = istoreContSurfDrawProps(obj->store, &defProps, contProps, co, 
                         surf, &contState, &surfState);

  // Analyze the value for color and gap changes
  if ((handleFlags & HANDLE_VALUE1) && (*stateFlags & CHANGED_VALUE1))
    ifgHandleValue1(&defProps, contProps, stateFlags, stateFlags);

  *ptProps = *contProps;
  ptProps->gap = 0;
  ptProps->valskip = 0;

  if (handleFlags & HANDLE_LINE_COLOR) {
    if (App->rgba)
      glColor3f(ptProps->red, ptProps->green, ptProps->blue);
  }

  if (handleFlags & HANDLE_MESH_COLOR) {
    ifgHandleColorTrans(obj, ptProps->red, ptProps->green, ptProps->blue, 
                 ptProps->trans);
  }

  if (handleFlags & HANDLE_MESH_FCOLOR) {
    ifgHandleColorTrans(obj, ptProps->fillRed, ptProps->fillGreen, 
                     ptProps->fillBlue, ptProps->trans);
  }

  if (handleFlags & HANDLE_2DWIDTH) {
    b3dLineWidth(ifgSelectedLineWidth(scaleThick * ptProps->linewidth2, 
                                      selected));
  }
      
  if (handleFlags & HANDLE_3DWIDTH) {
    glLineWidth(ptProps->linewidth);
    glPointSize(ptProps->linewidth);
  }
      
}


/* 
 * Handles the next change in the display.  Returns the index of the next
 * point at which a change occurs or 1- if none.  defProps has the default 
 * display properties.  ptProps and stateFlags are managed with the current
 * properties and flags for which are changed from the default, changeFlags 
 * is returned with flags for which are changed on this call.
 * handleFlags specifies which to handle and selected is 1 for the current 
 * contour.  scaleThick is the amount to scal line thicknesses by (default 1).
 */
int ifgHandleNextChange(Iobj *obj, Ilist *list, DrawProps *defProps, 
                        DrawProps *ptProps, int *stateFlags, int *changeFlags,
                        int handleFlags, int selected, int scaleThick)
{
  int nextChange = istoreNextChange(list, defProps, ptProps, stateFlags,
                                    changeFlags);
  ifgHandleStateChange(obj, defProps, ptProps, stateFlags, changeFlags, 
                       handleFlags, selected, scaleThick);
  return  nextChange;
}

/* 
 * Handles state changes in a display.  ptProps specifies the current
 * properties, changeFlags has flags for which are changed and need setting,
 * handleFlags specifies which to handle and selected is 1 for the current 
 * contour.
 */
static void ifgHandleStateChange(Iobj *obj, DrawProps *defProps, 
                                 DrawProps *ptProps, int *stateFlags, 
                                 int *changeFlags, int handleFlags, 
                                 int selected, int scaleThick)
{
  if ((handleFlags & HANDLE_VALUE1) && (*changeFlags & CHANGED_VALUE1))
    ifgHandleValue1(defProps, ptProps, stateFlags, changeFlags);

  if ((handleFlags & HANDLE_LINE_COLOR) && (*changeFlags & CHANGED_COLOR)) {
    if (App->rgba)
      glColor3f(ptProps->red, ptProps->green, ptProps->blue);
  }

  if ((handleFlags & HANDLE_MESH_COLOR) && 
      (*changeFlags & (CHANGED_COLOR | CHANGED_TRANS))) {
    ifgHandleColorTrans(obj, ptProps->red, ptProps->green, ptProps->blue, 
                 ptProps->trans);
  }

  if ((handleFlags & HANDLE_MESH_FCOLOR) && 
      (*changeFlags & (CHANGED_FCOLOR | CHANGED_TRANS))) {
    ifgHandleColorTrans(obj, ptProps->fillRed, ptProps->fillGreen, 
                     ptProps->fillBlue, ptProps->trans);
  }

  if ((handleFlags & HANDLE_3DWIDTH) && (*changeFlags & CHANGED_3DWIDTH)) {
    glLineWidth((GLfloat)ptProps->linewidth);
    glPointSize((GLfloat)ptProps->linewidth);
  }

  if ((handleFlags & HANDLE_2DWIDTH) && (*changeFlags & CHANGED_2DWIDTH)) {
    b3dLineWidth(ifgSelectedLineWidth(scaleThick * ptProps->linewidth2, 
                                      selected));
  }
}

/* 
 * Takes care of color and lighting calls for a new color and/or trans value
 */
void ifgHandleColorTrans(Iobj *obj, float r, float g, float b, int trans)
{
  glColor4f(r, g, b, 1.0f - (trans * 0.01f));
  light_adjust(obj, r, g, b, trans);
}

/* 
 * Handles the next change in the mesh display properties.  defProps provides
 * the default properties.  curProps, stateFlags, and nextItemIndex are
 * managed from call to call with the current properties, flags for which are
 * non-default, and the index of the next item with an actual change from the
 * the default.  The return value is the index at which the next change in
 * properties must be made (which can include a return to default).  curIndex
 * is the current mesh index, handleFlags indicates which properties to handle,
 * and changeFlags is returned with flags for ones changed on this call.
 */
int ifgHandleMeshChange(Iobj *obj, Ilist *list, DrawProps *defProps, 
                        DrawProps *curProps, int *nextItemIndex, int curIndex, 
                        int *stateFlags, int *changeFlags, int handleFlags)
{
  int nextChange;
  int needChange = *stateFlags;
  DrawProps lastProps;
  *stateFlags = 0;

  // If this index is the next one with items, then get the changes
  if (curIndex == *nextItemIndex) {

    // reset the properties to the default but save them first
    lastProps = *curProps;
    *curProps = *defProps;
    *nextItemIndex = istoreNextChange(list, defProps, curProps, stateFlags,
                                  changeFlags);

    // If handling value, get it interpreted and copy flags to state
    if ((handleFlags & HANDLE_VALUE1) && (*changeFlags & CHANGED_VALUE1)) {
      ifgHandleValue1(defProps, curProps, stateFlags, changeFlags);
      *stateFlags = *changeFlags;
    }

    // Change items are the original items or'd with the new items
    *changeFlags |= needChange;

    // But if a state is set and the value has not changed, clear the change
    if ((*stateFlags & CHANGED_COLOR) && curProps->red == lastProps.red &&
        curProps->green == lastProps.green && curProps->blue == lastProps.blue)
      *changeFlags &= ~CHANGED_COLOR;
    if ((*stateFlags & CHANGED_FCOLOR) && 
        curProps->fillRed == lastProps.fillRed &&
        curProps->fillGreen == lastProps.fillGreen && 
        curProps->fillBlue == lastProps.fillBlue)
      *changeFlags &= ~CHANGED_FCOLOR;
    if ((*stateFlags & CHANGED_TRANS) && curProps->trans == lastProps.trans)
      *changeFlags &= ~CHANGED_TRANS;
    if ((*stateFlags & CHANGED_3DWIDTH) && 
        curProps->linewidth == lastProps.linewidth)
      *changeFlags &= ~CHANGED_3DWIDTH;
  } else {

    // If no new items yet, restore all to default
    *changeFlags = needChange;
    *curProps = *defProps;
  }

  // Handle desired changes and returns to default.
  if ((handleFlags & HANDLE_MESH_COLOR) && 
      (*changeFlags & (CHANGED_COLOR | CHANGED_TRANS))) {
    ifgHandleColorTrans(obj, curProps->red, curProps->green, curProps->blue, 
                 curProps->trans);
  }

  if ((handleFlags & HANDLE_MESH_FCOLOR) && 
      (*changeFlags & (CHANGED_FCOLOR | CHANGED_TRANS))) {
    ifgHandleColorTrans(obj, curProps->fillRed, curProps->fillGreen, 
                     curProps->fillBlue, curProps->trans);
  }

  if ((handleFlags & HANDLE_3DWIDTH) && (*changeFlags & CHANGED_3DWIDTH)) {
    glLineWidth((GLfloat)curProps->linewidth);
    glPointSize((GLfloat)curProps->linewidth);
  }

  // Return next index if anything is non default, otherwise next item index
  if (*stateFlags)
    nextChange = curIndex + 1;  // 2?
  else
    nextChange = *nextItemIndex;
  return nextChange;
}

/* 
 * Advances within a contour to the next point that matches the desired
 * trans state in drawTrans.  contProps and stateFlags are input with the
 * contour properties and current state.  matchPt is returned with the point 
 * index or psize if there is none, ptProps is returned with the properties
 * at that point, and handleFlags specifies the properties to handle.
 */
int ifgContTransMatch(Iobj *obj, Icont *cont, int *matchPt, int drawTrans,
                      DrawProps *contProps, DrawProps *ptProps,
                      int *stateFlags, int *allChanges, int handleFlags)
{
  Istore *stp;
  Ilist *list = cont->store;
  int changeFlags, current, i, nextChange, revert;
  *matchPt = cont->psize;
  *allChanges = 0;

  if (!ilistSize(list))
    return -1;
  current = list->current;

  // Search forward for a store item that sets trans to matching state
  for (i = current; i < ilistSize(list); i++) {
    stp = istoreItem(list, i);
    if (stp->flags & (GEN_STORE_NOINDEX | 3) || stp->index.i >= cont->psize)
      break;

    // If find point, return index, reset list pointer and get state there
    revert = stp->flags & GEN_STORE_REVERT;
    if (stp->type == GEN_STORE_TRANS && 
        ((!revert && (stp->value.i ? 1 : 0) == drawTrans) 
         || (revert && (contProps->trans ? 1 : 0) == drawTrans))) {
      *matchPt = stp->index.i;
      list->current = current;
      nextChange = 0;
      while (nextChange >= 0 && nextChange <= *matchPt) {
        nextChange = istoreNextChange(list, contProps, ptProps, stateFlags,
                                  &changeFlags);
        *allChanges |= changeFlags;
      }

      // Now set display properties based on current state
      ifgHandleStateChange(obj, contProps, ptProps, stateFlags, allChanges, 
                           handleFlags, 0, 0);
      return nextChange;
    }
  }
  return -1;
}

/* 
 * Advances to the next mesh item whose trans state matches the desired state.
 * meshInd has an input value of the current mesh index (pointing to a 
 * non-matching item) and is set to the index of the matching value, or to the
 * index of an end code if there is none.  defTrans is the default trans state
 * and drawTrans is the state currently being drawn.
 */
int ifgMeshTransMatch(Imesh *mesh, int defTrans, int drawTrans, int *meshInd, 
                      int skipEnds)
{
  Istore *stp;
  int index = -1;
  int current, i, transSet;
  Ilist *list = mesh->store;
  
  if (!ilistSize(list))
    return -1;

  if (defTrans != drawTrans) {

    // If default trans state does not match the draw state, then search for a
    // trans change that matches the draw state and return -1 if reach end
    for (i = list->current; i < ilistSize(list); i++) {
      stp = istoreItem(list, i);
      if (stp->flags & (GEN_STORE_NOINDEX | 3))
        break;
        
      // Record current mesh index when it changes and set list pointer to
      // current list index so caller can pick up at the right place
      if (stp->index.i != index) {
        index = stp->index.i;
        current = i;
      }
      if (stp->type == GEN_STORE_TRANS && 
          (stp->value.i ? 1 : 0) == drawTrans) {
        *meshInd = index;
        list->current = current;
        return index;
      }
    }
    *meshInd = mesh->lsize - 2;
  } else {
    
    // If default state matches draw state, need to search for a mesh item
    // that does not have trans set, or that has trans matching state
    transSet = 1;
    for (i = list->current; i < ilistSize(list); i++) {
      stp = istoreItem(list, i);

      // If done with store list, then go to tests after loop
      if (stp->flags & (GEN_STORE_NOINDEX | 3))
        break;

      // When done processing an index, test if trans was set for that index
      // and if not, it is the place to start
      if (stp->index.i != index) {
        if (!transSet) {
          list->current = current;
          return index;
        }

        // Set new index and save list pointer at this place
        index = stp->index.i;
        current = i;
        transSet = 0;

        // If trans was set for previous index, now advance the mesh index to 
        // the next candidate and test whether this is now the next index being
        // examined.  Return if not, but skip over start/end codes if skipEnds set
        (*meshInd)++;
        while (*meshInd < index && *meshInd < mesh->lsize - 2) {
          if (mesh->list[*meshInd] >= 0 || !skipEnds)
            return index;
          (*meshInd)++;
        }
        
        // If at end of mesh, set to end codes and return
        if (*meshInd >= mesh->lsize - 2) {
          *meshInd = mesh->lsize - 2;
          return -1;
        }
      }

      if (stp->type == GEN_STORE_TRANS)
        transSet = 1;

      // If the current point matches the trans state, then it is done
      if (stp->type == GEN_STORE_TRANS && 
          (stp->value.i ? 1 : 0) == drawTrans) {
        list->current = current;
        return index;
      }
    }

    // Got to end of list, if there was a previous point with no trans, all set
    if (!transSet) {
      list->current = current;
      return index;
    }
      
    // If previous point had trans set, advance, make sure not to go past
    // end codes, and return -1 for no more items
    (*meshInd)++;
    if (*meshInd >= mesh->lsize - 2) 
      *meshInd = mesh->lsize - 2;
  }
  return -1;
}

/*
 * Routines to manage the color maps for value-type drawing
 */
static void ifgMapFalseColor(int gray, int *red, int *green, int *blue)
{
  static unsigned char cmap[3][256];
  static int first = 1;
  int *rampData;

  if (first){
    rampData = cmapStandardRamp();
    cmapConvertRamp(rampData, cmap);
    first = 0;
  }

  *red = cmap[0][gray];
  *green = cmap[1][gray];
  *blue = cmap[2][gray];
}

void ifgMakeValueMap(Iobj *obj, unsigned char cmap[3][256])
{
  float red, green, blue, mag;
  int blacklevel = 0, whitelevel = 255;
  int i, rampsize, cmapReverse = 0;
  float slope, point;
  int falsecolor = 0;
  int r,g,b;
  /*
   * calculate the color ramp to use.
   */
  if (obj->flags & IMOD_OBJFLAG_MCOLOR)
    falsecolor = 1;

  /* DNM 9/3/02 (8/31/06): Initialization of valwhite was needed because of 
     an endian problem, but is not correct */
  blacklevel = obj->valblack;
  whitelevel = obj->valwhite;

  if (blacklevel > whitelevel){
    cmapReverse = blacklevel;
    blacklevel = whitelevel;
    whitelevel = cmapReverse;
    cmapReverse = 1;
  }
  rampsize = whitelevel - blacklevel;
  if (rampsize < 1) rampsize = 1;

  for (i = 0; i < blacklevel; i++)
    cmap[0][i] = 0;
  for (i = whitelevel; i < 256; i++)
    cmap[0][i] = 255;
  slope = 256.0 / (float)rampsize;
  for (i = blacklevel; i < whitelevel; i++){
    point = (float)(i - blacklevel) * slope;
    cmap[0][i] = (unsigned char)point;
  }

  if (cmapReverse)
    for(i = 0; i < 256; i++)
      cmap[0][i] = 255 - cmap[0][i];
     
  if (falsecolor) {
    for(i = 0; i < 256; i++){
      ifgMapFalseColor(cmap[0][i], &r, &g, &b);
      cmap[0][i] = (unsigned char)r;
      cmap[1][i] = (unsigned char)g;
      cmap[2][i] = (unsigned char)b;
    }
  } else {
    red   = obj->red; green = obj->green; blue = obj->blue;
    if (obj->flags & IMOD_OBJFLAG_FCOLOR){
      red   = (float)obj->fillred / 255.0f;
      green = (float)obj->fillgreen / 255.0f;
      blue  = (float)obj->fillblue / 255.0f;
    }
    for(i = 0; i < 256; i++){
      mag = (float)cmap[0][i];
      cmap[0][i] = (unsigned char)(red   * mag);
      cmap[1][i] = (unsigned char)(green * mag);
      cmap[2][i] = (unsigned char)(blue  * mag);
    }
  }
}

/*
 * Static variables for controlling value drawing
 */
static float valMin, valSlope;
static int skipLow, skipHigh;
static unsigned char valueCmap[3][256];
static int valSetup = 0;
static int valConstant = 0;

/*
 * Test whether value drawing is possible and set up variables
 */
int ifgSetupValueDrawing(Iobj *obj, int type)
{
  float max;
  valSetup = 0;
  if (!(obj->flags & IMOD_OBJFLAG_USE_VALUE))
    return 0;
  if (!istoreGetMinMax(obj->store, obj->contsize, type, &valMin, &max))
    return 0;
  if (max <= valMin)
    return 0;
  valSlope = 255.9 / (max - valMin);
  valConstant = obj->matflags2 & MATFLAGS2_CONSTANT;
  if (!valConstant)
    ifgMakeValueMap(obj, valueCmap);

  // Interpret the flags for skipping low and high values as skipping low and
  // high indices, taking inversion of colormap into account
  skipLow = 0;
  skipHigh = 255;
  if (obj->matflags2 & MATFLAGS2_SKIP_LOW)
    skipLow = B3DMIN(obj->valblack, obj->valwhite);
  if (obj->matflags2 & MATFLAGS2_SKIP_HIGH)
    skipHigh = B3DMAX(obj->valblack, obj->valwhite);
  valSetup = 1;
  if (imodDebug('g'))
    imodPrintStderr("Setup min %f max %f slope %f bl %d wh %d skips %d %d\n", 
                    valMin, max, valSlope, obj->valblack, obj->valwhite,
                    skipLow, skipHigh);
  return 1;
}

// Routines to query and reset drawing state, used for contour-drawing routine
int ifgGetValueSetupState()
{
  return valSetup;
}

void ifgResetValueSetup()
{
  valSetup = 0;
}

// Handle the value change
static void ifgHandleValue1(DrawProps *defProps, DrawProps *contProps, 
                            int *stateFlags, int *changeFlags)
{
  int index;
  contProps->valskip = 0;
  if (valConstant) {

    // For constant color drawing, just set gaps
    if (*stateFlags & CHANGED_VALUE1) {
      index = valSlope * (contProps->value1 - valMin);
      if (index < skipLow || index > skipHigh) {
        contProps->gap = 1;
        contProps->valskip = 1;
      }
    }
    return;
  } else if (*stateFlags & CHANGED_VALUE1) {

    // Otherwise modify color, potentially set gap
    index = valSlope * (contProps->value1 - valMin);
    if (index < 0)
      index = 0;
    if (index > 255)
      index = 255;
    
    if (index < skipLow || index > skipHigh) {
      contProps->gap = 1;
      contProps->valskip = 1;
    }
                
    contProps->red = valueCmap[0][index] / 255.;
    contProps->green = valueCmap[1][index] / 255.;
    contProps->blue = valueCmap[2][index] / 255.;
  } else {
    contProps->red = defProps->red;
    contProps->green = defProps->green;
    contProps->blue = defProps->blue;
  }
  *changeFlags |= CHANGED_COLOR;
}
