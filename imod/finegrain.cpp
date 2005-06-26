/*
 *  finegrain.cpp - provide ability to change fine-grain display properties
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$
    
    $Date$

    $Revision$

    $Log$
*/

#include <qgl.h>
#include "form_finegrain.h"
#include "finegrain.h"
#include "imod.h"
#include "imod_display.h"
#include "control.h"
#include "undoredo.h"
#include "b3dgfx.h"
#include "imodv_light.h"

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
  DrawProps contProps;
  Istore store;
} FgData;

static FgData fgd = {NULL, NULL, NULL, NULL, 0, -1, -1, -1, -1, 0};

static int getLoadedObjCont();
static void insertAndUpdate(int type);
static void handleColorTrans(Iobj *obj, float r, float g, float b, int trans);

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
                               "fine grain edit",
                               Qt::WType_TopLevel | Qt::WDestructiveClose);
  
  fineGrainUpdate();
  imodDialogManager.add((QWidget *)fgd.dia, IMOD_DIALOG);

  fgd.dia->show();

}

/*
 * Update the window
 */
void fineGrainUpdate()
{
  int ob, co, pt, surf;
  Imod *imod;
  Iobj *obj;
  Icont *cont;
  bool enable;
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
  fgd.objLoaded = ob;
  fgd.contLoaded = co;
  fgd.ptLoaded = pt;
  fgd.surfLoaded = surf;

  // get point properties if valid point, otherwise get object and 
  // contour/surface properties
  if (!fgd.ptContSurf && pt >= 0) {
    fgd.stateFlags = istorePointDrawProps(obj, &fgd.contProps, &props, co, pt);
  } else {

    istoreDefaultDrawProps(obj, &defProps);
    if (cont) {
      fgd.stateFlags = istoreContSurfDrawProps(obj->store, &defProps, &props, 
                            fgd.ptContSurf > 1 ? -1 : co, surf);
    }
    else
      props = defProps;
  }
  fgd.dia->update(fgd.ptContSurf, enable, &props, fgd.stateFlags);
}

/*
 * Change the selection of point/cont/surf
 */
void ifgPtContSurfSelected(int which)
{
  fgd.ptContSurf = which;
  fineGrainUpdate();
}

/*
 * Individual color changes entries: check for whether the change is to the
 * default and insert an end if appropriate
 */
void ifgLineColorChanged(int r, int g, int b)
{
  if (fgd.contProps.red == (int)(255. * r + 0.5) &&
      fgd.contProps.green == (int)(255. * g + 0.5) && 
      fgd.contProps.blue == (int)(255. * b + 0.5)) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_COLOR))
      ifgEndChange(GEN_STORE_COLOR);
    return;
  }
  ifgColorChanged(GEN_STORE_COLOR, r, g, b);
}

void ifgFillColorChanged(int r, int g, int b)
{
  if (fgd.contProps.fillRed == (int)(255. * r + 0.5) &&
      fgd.contProps.fillGreen == (int)(255. * g + 0.5) && 
      fgd.contProps.fillBlue == (int)(255. * b + 0.5)) {
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
  if (getLoadedObjCont())
    return;
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
  if (fgd.contProps.trans == value) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_TRANS))
      ifgEndChange(GEN_STORE_TRANS);
    return;
  }
  ifgIntChanged(GEN_STORE_TRANS, value);
}

void ifgWidth2DChanged(int value)
{
  if (fgd.contProps.linewidth2 == value) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_2DWIDTH))
      ifgEndChange(GEN_STORE_2DWIDTH);
    return;
  }
  ifgIntChanged(GEN_STORE_2DWIDTH, value);
}

void ifgWidth3DChanged(int value)
{
  if (fgd.contProps.linewidth == value) {
    if (!fgd.ptContSurf && (fgd.stateFlags & CHANGED_3DWIDTH))
      ifgEndChange(GEN_STORE_3DWIDTH);
    return;
  }
  ifgIntChanged(GEN_STORE_3DWIDTH, value);
}

void ifgSymsizeChanged(int value)
{
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
  if (getLoadedObjCont())
    return;

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
  if (getLoadedObjCont())
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
  if (getLoadedObjCont())
    return;
  fgd.vw->undo->contourDataChg();
  if (istoreEndChange(fgd.cont->store, type, fgd.ptLoaded))
    wprint("/aError modifying storage list to end change\n");
  fgd.vw->undo->finishUnit();
  if (imodDebug('g'))
    istoreDump(fgd.cont->store);
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  fineGrainUpdate();
}

/*
 * Clear out a change sequence for point data, or remove one item for
 * contour data.
 */
void ifgClearChange(int type)
{
  int err;
  if (getLoadedObjCont())
    return;
  if (fgd.ptContSurf) {
    fgd.vw->undo->objectPropChg();
    err = istoreClearOneIndexItem
      (fgd.obj->store, type, 
       (fgd.ptContSurf == 2) ? fgd.surfLoaded : fgd.contLoaded,
       (fgd.ptContSurf == 2) ? GEN_STORE_SURFACE : 0);
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
  if (getLoadedObjCont())
    return;
  fgd.vw->undo->contourDataChg();
  if (fgd.ptContSurf) {
    if (state)
      ifgIntChanged(GEN_STORE_GAP, 1);
    else
      ifgClearChange(GEN_STORE_GAP);
    return;
  }
  if (state) {
    fgd.store.type = GEN_STORE_GAP;
    fgd.store.flags = GEN_STORE_ONEPOINT;
    fgd.store.index.i = fgd.ptLoaded;
    if (istoreAddOneIndexItem(&fgd.cont->store, &fgd.store))
      wprint("\aError inserting gap into storage list\n");
  } else if (istoreClearOneIndexItem(fgd.cont->store, GEN_STORE_GAP,
                                     fgd.ptLoaded, 0))
    wprint("\aError removing gap from storage list\n");
  fgd.vw->undo->finishUnit();
  if (imodDebug('g'))
    istoreDump(fgd.cont->store);
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  fineGrainUpdate();
}

/*
 * A change in the connection number for points
 */
void ifgConnectChanged(int value)
{
  if (getLoadedObjCont())
    return;
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
}

void ifgDump()
{
  if (getLoadedObjCont())
    return;
  if (fgd.ptContSurf)
    istoreDump(fgd.obj->store);
  else
    istoreDump(fgd.cont->store);
}

void ifgHelp()
{

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
static int getLoadedObjCont()
{
  Imod *imod = fgd.vw->imod;
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
  fgd.cont = &fgd.obj->cont[fgd.contLoaded];
  if (fgd.ptContSurf == 2 && fgd.cont->surf != fgd.surfLoaded) {
    wprint("\aContour last loaded into Fine Grain dialog does not have the "
           "same surface number\n");
    fineGrainUpdate();
    return 1;
  }
  if (!fgd.ptContSurf && fgd.ptLoaded >= fgd.cont->psize) {
    wprint("\aPoint last loaded into Fine Grain dialog is no longer valid\n");
    fineGrainUpdate();
    return 1;
  }
  return 0;
}

/*
 * Insert an item into the appropriate list and do an update
 */
static void insertAndUpdate(int type)
{
  int err;
  fgd.store.type = type;
  fgd.store.flags |= fgd.ptContSurf == 2 ? GEN_STORE_SURFACE : 0;
  if (fgd.ptContSurf) {
    fgd.vw->undo->objectPropChg();
    fgd.store.index.i = fgd.contLoaded;
    err = istoreAddOneIndexItem(&fgd.obj->store, &fgd.store);
    if (imodDebug('g'))
      istoreDump(fgd.obj->store);
  } else {
    fgd.vw->undo->contourDataChg();
    fgd.store.index.i = fgd.ptLoaded;
    err = istoreInsertChange(&fgd.cont->store, &fgd.store);
    if (imodDebug('g'))
      istoreDump(fgd.cont->store);
  }
  if (err)
    wprint("\aError inserting change into general storage list\n");
  fgd.vw->undo->finishUnit();
  imodDraw(App->cvi, IMOD_DRAW_MOD);
  fineGrainUpdate();
}

int ifgSelectedLineWidth(int width, int selected)
{
  if (selected)
    return (width < 3 ? width + 2 : width / 2);
  return width;
}

int ifgHandleContChange(Iobj *obj, int co, DrawProps *contProps, 
                        DrawProps *ptProps, int *stateFlags, int handleFlags,
                        int selected)
{
  DrawProps defProps;

  /* Get the properties for the contour */
  istoreDefaultDrawProps(obj, &defProps);
  *stateFlags = istoreContSurfDrawProps(obj->store, &defProps, contProps, co, 
                         obj->cont[co].surf);
  *ptProps = *contProps;
  ptProps->gap = 0;

  if (handleFlags & HANDLE_LINE_COLOR) {
    if (App->rgba)
      glColor3f(ptProps->red, ptProps->green, ptProps->blue);
  }

  if (handleFlags & HANDLE_MESH_COLOR) {
    handleColorTrans(obj, ptProps->red, ptProps->green, ptProps->blue, 
                 ptProps->trans);
  }

  if (handleFlags & HANDLE_MESH_FCOLOR) {
    handleColorTrans(obj, ptProps->fillRed, ptProps->fillGreen, 
                     ptProps->fillBlue, ptProps->trans);
  }

  if (handleFlags & HANDLE_2DWIDTH) {
    b3dLineWidth(ifgSelectedLineWidth(ptProps->linewidth2, selected));
  }
      
  if (handleFlags & HANDLE_3DWIDTH) {
    glLineWidth(ptProps->linewidth);
    glPointSize(ptProps->linewidth);
  }
      
  return (istoreFirstChangeIndex(obj->cont[co].store));
}


int ifgHandleNextChange(Iobj *obj, Ilist *list, DrawProps *defProps, 
                        DrawProps *ptProps, int *stateFlags, int *changeFlags,
                        int handleFlags, int selected)
{
  int nextChange = istoreNextChange(list, defProps, ptProps, stateFlags,
                                    changeFlags);
  if ((handleFlags & HANDLE_LINE_COLOR) && (*changeFlags & CHANGED_COLOR)) {
    if (App->rgba)
      glColor3f(ptProps->red, ptProps->green, ptProps->blue);
  }

  if ((handleFlags & HANDLE_MESH_COLOR) && 
      (*changeFlags & (CHANGED_COLOR | CHANGED_TRANS))) {
    glColor4f(ptProps->red, ptProps->green, ptProps->blue, 
              1.0f - (ptProps->trans * 0.01f));
    handleColorTrans(obj, ptProps->red, ptProps->green, ptProps->blue, 
                 ptProps->trans);
  }

  if ((handleFlags & HANDLE_MESH_FCOLOR) && 
      (*changeFlags & (CHANGED_FCOLOR | CHANGED_TRANS))) {
    glColor4f(ptProps->fillRed, ptProps->fillGreen, ptProps->fillBlue,
              1.0f - (ptProps->trans * 0.01f));
    handleColorTrans(obj, ptProps->fillRed, ptProps->fillGreen, 
                     ptProps->fillBlue, ptProps->trans);
  }

  if ((handleFlags & HANDLE_3DWIDTH) && (*changeFlags & CHANGED_3DWIDTH)) {
    glLineWidth((GLfloat)ptProps->linewidth);
    glPointSize((GLfloat)ptProps->linewidth);
  }

  if ((handleFlags & HANDLE_2DWIDTH) && (*changeFlags & CHANGED_2DWIDTH)) {
    b3dLineWidth(ifgSelectedLineWidth(ptProps->linewidth2, selected));
  }
      
  return  nextChange;
}

static void handleColorTrans(Iobj *obj, float r, float g, float b, int trans)
{
  glColor4f(r, g, b, 1.0f - (trans * 0.01f));
  light_adjust(obj, r, g, b, trans);
  if (trans) {
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    if (!(obj->flags & IMOD_OBJFLAG_TWO_SIDE))
      glEnable(GL_CULL_FACE);
  } else {
    glDisable(GL_BLEND);
    if (!(obj->flags & IMOD_OBJFLAG_TWO_SIDE))
      glDisable(GL_CULL_FACE);
  }
}
