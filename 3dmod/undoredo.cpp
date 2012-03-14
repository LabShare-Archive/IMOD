/*
 *  undoredo.cpp - provide undo/redo capability for all model changes
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

/* DOC_CODE Essay on Undo/Redo */
/* This module records various kinds of changes to the model so that changes 
   can be undone and redone.  An individual change is specified by one call
   to one of the four change routines.  Changes are organized into groups 
   called units, so each undo or redo operation does all of the changes in one
   unit.  The units are maintained in an ilist, mUnitList.  Each unit contains
   an ilist of the changes.  The unit also contains values for the current
   object/contour/point before and after the change, as well as state
   information that is required to match before a unit will be undone or 
   redone: the # of objects, the # of contours in the current object, and the
   number of points in the current contour. ^

   A unit is "opened" automatically when a change call comes in, i.e., it is
   created and added to the list and the starting state recorded.  Further
   changes are added to the same unit until an explicit call is made to
   finishUnit to close the unit.  If an error occurs while recording a change,
   the entire undo stack is flushed and further changes are ignore until the
   the next finishUnit call. ^

   This protocol dictates that a change call be made before any actual changes
   to the model (except current contour and point changes).  Also, the 
   finishUnit call must be made after all changes to the model, preferably
   after the current contour and point are set.  If an error occurs after
   starting a unit, flushUnit can be used to clear the changes within that
   unit but leave the previous undo stack intact. ^

   Most changes involve recording some data such as a contour structure or
   complete contour including points.  These data are copied into backup pool
   items maintained on the list mItemPool.  Each pool item has a unique 
   identifier that is also recorded in the change structure.  Data are freed 
   from these items when no longer needed and the pool list is periodically 
   repacked to remove empty items.  The number of backup units is limited by
   mMaxUnits and can be quite large; in practice it will be limited by the
   maximum number of bytes of data allowed in the pool, mMaxBytes. ^

   The variety of changes allow for relatively efficient recording of most 
   changes.  Points may be added or deleted singly or in groups, or the current
   point may be moved, without registering a contour change.  Contour changes
   may be of properties only, in which case points, labels, and general storage
   are not stored, or of data, in which case the whole contour is duplicated.
   Object changes will duplicate labels but not contours and meshes, unless an
   object is being deleted.  Model changes are excessive in copying all of the 
   views every time.  Object rearrangements invoke a model change.  A change
   of model view invokes an object change for each view, since that is the only
   way to preserve the current state of the objects and of the object views 
   separately. ^

   The presence of data in a contour or object general storage structure will
   sometimes modify the behavior.  A call to register addition or removal of 
   points will be converted to a contour data change call when the contour 
   store has data.  A call to register a contour removal, addition, or move
   will cause an object property change to be registered first if the affected
   object store has data.
 */
/* END_CODE */

#include "objgroup.h"
#include "undoredo.h"
#include "imod.h"
#include "imodv.h"
#include "mv_views.h"
#include "imod_edit.h"
#include "model_edit.h"
#include "mv_objed.h"
#include "mv_gfx.h"
#include "mv_modeled.h"
#include "display.h"
#include "info_cb.h"
#include "info_setup.h"
#include "form_info.h"
#include "sslice.h"
#include "vertexbuffer.h"

UndoRedo::UndoRedo(ImodView *vi)
{
  mVi = vi;
  mUnitList = ilistNew(sizeof(UndoUnit), 0);
  mItemPool = ilistNew(sizeof(BackupItem), 0);
  ilistQuantum(mItemPool, 32);
  ilistQuantum(mUnitList, 32);
  mID = 0;
  mRejectChanges = false;
  mUnitOpen = false;
  mMaxUnits = 1000;
  mMaxBytes = 5000000;
  mUndoIndex = -1;
  mNumFreedInPool = 0;
}

UndoRedo::~UndoRedo()
{
  removeUnits(0, ilistSize(mUnitList) - 1);
  ilistDelete(mUnitList);
  ilistDelete(mItemPool);
}

// Process a point change within a contour
void UndoRedo::pointChange(int type, int object, int contour, 
                     int point, int point2)
{
  UndoUnit *unit;
  UndoChange change;

  // The default is to operate on the current contour and point and 
  // set endpoint equal to start point
  if (object < -1)
    object = mVi->imod->cindex.object;
  if (contour < -1)
    contour = mVi->imod->cindex.contour;
  if (point < -1)
    point = mVi->imod->cindex.point;
  if (point2 < 0)
    point2 = point;
      

  // Check validity, allow point index one past current contour
  if (object < 0 || object >= mVi->imod->objsize || contour < 0 || 
      contour >= mVi->imod->obj[object].contsize || point < 0 || point2 < 0 ||
      point > point2 || point2 > mVi->imod->obj[object].cont[contour].psize) {

    // Dump everything?
    memoryError();
    return;
  }

  // Check contour for general storage, process as data change
  if ((type == PointsAdded || type == PointsRemoved) && 
      ilistSize(mVi->imod->obj[object].cont[contour].store)) {
    contourChange(ContourData, object, contour);
    return;
  }
    
  // Get a new or existing open unit
  unit = getOpenUnit();
  if (!unit)
    return;
  if (imodDebug('u'))
    imodPrintStderr("Point change type %d ob %d co %d pt1 %d pt2 %d\n",
                    type, object, contour, point, point2);

  // Set properties of the change, copy points to an item
  initChange(change, type, object, contour, point, point2);
  if (type != PointsAdded)
    copyPoints(&change, false);

  finishChange(unit, &change);
}


// Process a contour change with one or two objects/contours involved
void UndoRedo::contourChange(int type, int object, int contour,
                     int object2, int contour2)
{
  UndoUnit *unit;
  UndoChange change;
  BackupItem item;
  Icont *cont = NULL;

  // The default is to operate on the current contour
  if (object < -1)
    object = mVi->imod->cindex.object;
  if (contour < -1)
    contour = mVi->imod->cindex.contour;

  if (contour >= 0 && contour < mVi->imod->obj[object].contsize)
    cont = &mVi->imod->obj[object].cont[contour];

  // For contour removal/addition, register a property change if there is
  // general storage
  if (type == ContourRemoved || type == ContourAdded || type == ContourMoved) {
    if (ilistSize(mVi->imod->obj[object].store))
      objectPropChg(object);
    if (type == ContourMoved && ilistSize(mVi->imod->obj[object2].store))
      objectPropChg(object2);
  }

  // Get a new or existing open unit
  unit = getOpenUnit();
  if (!unit)
    return;
  if (imodDebug('u'))
    imodPrintStderr("Contour change type %d ob1 %d co1 %d ob2 %d co2 %d\n",
                    type, object, contour, object2, contour2);

  // Set properties of the change and item
  initChange(change, type, object, contour, object2, contour2);
  item.type = Contour;
  item.ID = -1;

  switch (type) {

    // Cases where a contour copy is needed
  case ContourData:
  case ContourProperty:
  case ContourRemoved:
    if (type == ContourProperty) {
      item.p.cont = imodContourNew();
      imodContourCopy(cont, item.p.cont);
    } else {
      item.p.cont = imodContourDup(cont);
    }
    if (!item.p.cont) {
      memoryError();
      return;
    }
    // If just a property change, null out the pointers
    if (type == ContourProperty) {
      item.p.cont->pts = NULL;
      item.p.cont->sizes = NULL;
      item.p.cont->label = NULL;
      item.p.cont->store = NULL;
    }

    // Add item to pool; clean up contour if fails
    item.ID = mID++;
    change.ID = item.ID;
    change.bytes = contourBytes(item.p.cont);
    if (ilistAppend(mItemPool, &item)) {
      imodContourDelete(item.p.cont);
      memoryError();
      return;
    }
    break;

  case ContourAdded:
  case ContourMoved:
  default:
    break;
    
  }

  finishChange(unit, &change);
}

// Process a move of all contours from object to object2.  Assume object property
// change has already been recorded in advance
void UndoRedo::allContourMove(int object, int object2)
{
  UndoUnit *unit;
  UndoChange change;
  BackupItem item;
  Icont *cont = NULL;
  int co;

  unit = getOpenUnit();
  if (!unit)
    return;
  if (imodDebug('u'))
    imodPrintStderr("Move all contours ob1 %d ob2 %d\n", object, object2);
  for (co = 0; co < mVi->imod->obj[object].contsize; co++) {
    cont = &mVi->imod->obj[object].cont[co];

    // Each contour is marked as contour zero because each one is 0 when it is actually
    // moved over to the other object
    initChange(change, ContourMoved, object, 0, object2, 
               mVi->imod->obj[object2].contsize + co);
    item.type = Contour;
    item.ID = -1;
    item.p.cont = imodContourDup(cont);
    if (!item.p.cont) {
      memoryError();
      return;
    }

    // Add item to pool; clean up contour if fails
    item.ID = mID++;
    change.ID = item.ID;
    change.bytes = contourBytes(item.p.cont);
    if (ilistAppend(mItemPool, &item)) {
      imodContourDelete(item.p.cont);
      memoryError();
      return;
    }
    finishChange(unit, &change);
  }
}

// Process an object change
void UndoRedo::objectChange(int type, int object, int object2)
{
  UndoUnit *unit;
  UndoChange change;
  BackupItem item;
  Iobj *obj = NULL;

  if (object < -1)
    object = mVi->imod->cindex.object;

  if (object >= 0 && object < mVi->imod->objsize)
    obj = &mVi->imod->obj[object];

  // First invoke a model change for any kind of object rearrangement
  if (type != ObjectChanged)
    modelChange(ModelChanged);

  // Get a new or existing open unit
  unit = getOpenUnit();
  if (!unit)
    return;

  if (imodDebug('u'))
    imodPrintStderr("Object change type %d ob1 %d ob2 %d\n", type, object,
                    object2);

  // Set properties of the change and item
  initChange(change, type, object, -1, object2);
  item.type = Object;
  item.ID = -1;

  switch (type) {
  case ObjectChanged:
    item.p.obj = imodObjectNew();
    if (!item.p.obj) {
      memoryError();
      return;
    }
    imodObjectCopy(obj, item.p.obj);
    item.p.obj->cont = NULL;
    item.p.obj->mesh = NULL;
    item.p.obj->vertBufCont = NULL;
    item.p.obj->vertBufSphere = NULL;
    item.p.obj->label = imodLabelDup(obj->label);
    item.p.obj->store = ilistDup(obj->store);
    item.p.obj->meshParam = imeshParamsDup(obj->meshParam);
    
    item.ID = change.ID = mID++;
    break;

  case ObjectRemoved:
    item.p.obj = imodObjectDup(obj);
    if (!item.p.obj) {
      memoryError();
      return;
    }
    item.ID = change.ID = mID++;
    break;

  case ObjectAdded:
  case ObjectMoved:
  default:
    break;
  }

  // Add item to pool
  if (item.ID >= 0) {
    change.bytes = objectBytes(item.p.obj);
    if (ilistAppend(mItemPool, &item)) {
      imodObjectDelete(item.p.obj);
      memoryError();
      return;
    }
  }

  finishChange(unit, &change);
}

// Process model change
void UndoRedo::modelChange(int type, Ipoint *point)
{
  UndoUnit *unit;
  UndoChange change;
  BackupItem item;
  Iview *vwi, *vwo;
  Imod *mod = mVi->imod;
  int i;

  // If view has changed, need to save all objects
  if (type == ViewChanged)
    for (i = 0; i < mod->objsize; i++)
      objectPropChg(i);

  // Get a new or existing open unit
  unit = getOpenUnit();
  if (!unit)
    return;

  if (imodDebug('u'))
    imodPuts("model change");

  // Set properties of the change and item
  initChange(change, type);
  item.ID = change.ID = mID++;

  if (type == ModelShifted) {

    // For a shift, get a point and copy
    item.type = Points;
    change.bytes = sizeof(Ipoint);
    item.p.pts = (Ipoint *)malloc(sizeof(Ipoint));
    if (!point || !item.p.pts) {
      memoryError();
      return;
    }
    *(item.p.pts) = *point;
  } else {      

    item.type = Model;

    // Just get a model structure
    item.p.mod = (Imod *)malloc(sizeof(Imod));
    if (!item.p.mod) {
      memoryError();
      return;
    }

    // Copy the model elements but null pointers for items not copied
    memcpy(item.p.mod, mod, sizeof(Imod));
    item.p.mod->obj = NULL;
    item.p.mod->viewsize = 0;
    item.p.mod->fileName = NULL;
    item.p.mod->refImage = NULL;
    item.p.mod->store = ilistDup(mod->store);
    item.p.mod->slicerAng = ilistDup(mod->slicerAng);
    item.p.mod->groupList = objGroupListDup(mod->groupList);
    

    // Copy the views
    if (mod->viewsize) {
      item.p.mod->view = imodViewNew(mod->viewsize);
      if (!item.p.mod->view) {
        imodDelete(item.p.mod);
        memoryError();
        return;
      }
      item.p.mod->viewsize = mod->viewsize;

      // Copy view data view by view, zero out objvsize
      for (i= 0; i < mod->viewsize; i++) {
        item.p.mod->view[i] = mod->view[i];
        item.p.mod->view[i].objvsize = 0;
      }

      // Copy object views en mass for each view
      for (i= 0; i < mod->viewsize; i++) {
        vwi = &(mod->view[i]);
        vwo = &(item.p.mod->view[i]);
        vwo->objview = (Iobjview *)malloc(vwi->objvsize * sizeof(Iobjview));
        if (!vwo->objview) {
          imodDelete(item.p.mod);
          memoryError();
          return;
        }
        vwo->objvsize = vwi->objvsize;
        memcpy(vwo->objview, vwi->objview, vwi->objvsize * sizeof(Iobjview));
      }
    }

  change.bytes = modelBytes(item.p.mod);
  }

  // Add item to pool
  if (ilistAppend(mItemPool, &item)) {
    imodDelete(item.p.mod);
    memoryError();
    return;
  }

  finishChange(unit, &change);
}

// Initialize a change structure
void UndoRedo::initChange(UndoChange &change, int type, int object, 
                  int contour, int object2, int contour2)
{
  change.type = type;
  change.object = object;
  change.obj2_pt1 = object2;
  change.contour = contour;
  change.cont2_pt2 = contour2;
  change.bytes = 0;
  change.ID = -1;
}

// Finish a change by adding change to unit's list
void UndoRedo::finishChange(UndoUnit *unit, UndoChange *change)
{
  // Try to add change to the list
  if (ilistAppend(unit->changes, change)) {
    if (change->ID >= 0)
      freePoolItem((BackupItem *)ilistLast(mItemPool));
    memoryError();
    return;
  }
  updateButtons();
}


// To finish a unit, get the state and reset the open and reject flags
void UndoRedo::finishUnit()
{
  UndoUnit *unit;
  if (!mUnitOpen)
    return;
  unit = (UndoUnit *)ilistLast(mUnitList);
  if (unit)
    recordState(unit->after);
  mUnitOpen = false;
  mRejectChanges = false;
  mID %= 2000000000;
  trimLists(1);
  if (mNumFreedInPool > ilistSize(mItemPool) / 8)
    compactPool();
  updateButtons();
}

// If something went wrong with a posted change, this will remove the unit
void UndoRedo::flushUnit()
{
  if (ilistSize(mUnitList) > 0)
    removeUnits(ilistSize(mUnitList) - 1, ilistSize(mUnitList) - 1);
  mUnitOpen = false;
  mRejectChanges = false;
  updateButtons();
}

// When there is a new model, everything has to go
void UndoRedo::clearUnits()
{
  removeUnits(0, ilistSize(mUnitList) - 1);
  mUnitOpen = false;
  mRejectChanges = false;
  updateButtons();
}

// This does a collection of tasks when a change comes in, culminating in 
// returning the last unit, after adding one if needed
UndoUnit *UndoRedo::getOpenUnit()
{
  UndoUnit unit;

  // If we are in an undo state, this is a continuation, which requires
  // flushing everything from here forward
  if (mUndoIndex >= 0) {
    removeUnits(mUndoIndex, ilistSize(mUnitList) - 1);
    mUndoIndex = -1;
  }

  // If we need to reject changes, return null
  if (mRejectChanges)
      return NULL;

  // Start a new unit if one is not open
  if (!mUnitOpen) {
    recordState(unit.before);
    unit.changes = ilistNew(sizeof(UndoChange), 0);
    if (!unit.changes) {
      memoryError();
      return NULL;
    }
    if (ilistAppend(mUnitList, &unit)) {
      ilistDelete(unit.changes);
      memoryError();
      return NULL;
    }
    mUnitOpen = true;
  }

  // Get and return the last unit
  return (UndoUnit *)ilistLast(mUnitList);
}

void UndoRedo::memoryError()
{
  mRejectChanges = mUnitOpen;
  removeUnits(0, ilistSize(mUnitList) - 1);
  mUndoIndex = -1;
  updateButtons();
}
  
// Undo one unit of changes
int UndoRedo::undo()
{
  UndoUnit *unit;
  UndoChange *change;
  BackupItem *item;
  int ch, err;

  // Finish a unit if one is open
  finishUnit();

  // return if nothing to undo
  if (!mUndoIndex || !ilistSize(mUnitList))
    return NoneAvailable;

  // Set index for the unit to undo and get the unit; check the state
  if (mUndoIndex < 0)
    mUndoIndex = ilistSize(mUnitList);
  unit = (UndoUnit *)ilistItem(mUnitList, mUndoIndex - 1);

  if (!stateMatches(unit->after))
    return StateMismatch;
  mUndoIndex--;

  // Loop backwards on the changes
  vbCleanupVBD(mVi->imod);
  for (ch = ilistSize(unit->changes) - 1; ch >= 0; ch--) {
    change = (UndoChange *)ilistItem(unit->changes, ch);
    item = findPoolItem(change->ID);
    switch (change->type) {
    case PointsAdded:
      err = copyPoints(change, true);
      break;
    case PointsRemoved:
      err = restorePoints(change, item);
      break;
    case PointShifted:
      err = shiftPoint(change, item);
      break;
    case ContourData:
      err = exchangeContours(change, item, false);
      break;
    case ContourProperty:
      err = exchangeContours(change, item, true);
      break;
    case ContourRemoved:
      err = restoreContour(change, item);
      break;
    case ContourAdded:
      err = removeContour(change);
      break;
    case ContourMoved:
      err = moveContour(change->obj2_pt1, change->cont2_pt2, change->object, 
                        change->contour);
      break;
    case ObjectChanged:
      err = exchangeObjects(change, item);
      break;
    case ObjectRemoved:
      err = restoreObject(change, item);
      break;
    case ObjectAdded:
      err = removeObject(change);
      break;
    case ObjectMoved:
      err = 0;
      if (imodMoveObject(mVi->imod, change->obj2_pt1, change->object))
        err = MemoryError;
      break;
    case ViewChanged:
    case ModelChanged:
      err = exchangeModels(change, item);
      break;
    case ModelShifted:
      err = shiftModel(item, -1);
      break;

    default:
      break;
    }

    if (err) {
      memoryError();
      break;
    }
  }

  if (!err)
    mVi->imod->cindex = unit->before.index;
  finishUndoRedo();

  return err;
}

// Redo undone changes
int UndoRedo::redo()
{
  UndoUnit *unit;
  UndoChange *change;
  BackupItem *item;
  int ch, err;

  // return if nothing to redo
  if (mUndoIndex < 0 || mUndoIndex > ilistSize(mUnitList) - 1)
    return NoneAvailable;

  // get the unit; check the state before
  unit = (UndoUnit *)ilistItem(mUnitList, mUndoIndex);
  if (!stateMatches(unit->before))
    return StateMismatch;

  // Loop forwards on the changes
  vbCleanupVBD(mVi->imod);
  for (ch = 0; ch < ilistSize(unit->changes); ch++) {
    change = (UndoChange *)ilistItem(unit->changes, ch);
    item = findPoolItem(change->ID);
    err = 0;
    switch (change->type) {
    case PointsAdded:
      err = restorePoints(change, item);
      break;
    case PointsRemoved:
      err = copyPoints(change, true);
      break;
    case PointShifted:
      err = shiftPoint(change, item);
      break;
    case ContourData:
      err = exchangeContours(change, item, false);
      break;
    case ContourProperty:
      err = exchangeContours(change, item, true);
      break;
    case ContourRemoved:
      err = removeContour(change);
      break;
    case ContourAdded:
      err = restoreContour(change, item);
      break;
    case ContourMoved:
      err = moveContour(change->object, change->contour, change->obj2_pt1, 
                        change->cont2_pt2);
      break;
    case ObjectChanged:
      err = exchangeObjects(change, item);
      break;
    case ObjectRemoved:
      err = removeObject(change);
      break;
    case ObjectAdded:
      err = restoreObject(change, item);
      break;
    case ObjectMoved:
      if (imodMoveObject(mVi->imod, change->object, change->obj2_pt1))
        err = MemoryError;
      break;
    case ViewChanged:
    case ModelChanged:
      err = exchangeModels(change, item);
      break;
    case ModelShifted:
      err = shiftModel(item, 1);
      break;

    default:
      break;
    }

    if (err) {
      memoryError();
      break;
    }
  }

  // Set the index forward after the operation
  if (!err) {
    mVi->imod->cindex = unit->after.index;
    mUndoIndex++;
  }

  finishUndoRedo();

  return err;
}

// Common finish to the undo or redo operations
void UndoRedo::finishUndoRedo()
{
  Iindex *index = &mVi->imod->cindex;
  updateButtons();

  // Make sure indexes are OK
  imodSetIndex(mVi->imod, index->object, index->contour, index->point);

  // Do some things
  imodSelectionListClear(mVi);
  if (mVi->modelViewVi) {
    imodvDraw(Imodv);
  } else {
    imod_setxyzmouse();
    imod_info_setobjcolor();
    imodModelEditUpdate();
  }
  imodvObjedNewView();
  imodvPixelChanged();
  mID %= 2000000000;
  if (mNumFreedInPool > ilistSize(mItemPool) / 8)
    compactPool();
}

// Copy points from the given contour to a new backup item, optionally remove 
// from contour
int UndoRedo::copyPoints(UndoChange *change, bool remove)
{
  int pt;
  BackupItem item;
  Icont *cont = &mVi->imod->obj[change->object].cont[change->contour];

  // Make a scratch contour, set up item and IDs
  item.type = Contour;
  item.p.cont = imodContourNew();
  if (!item.p.cont)
    return MemoryError;
  change->ID = item.ID = mID++;

  // Copy points into scratch contour, copy sizes if they exist
  for (pt = change->obj2_pt1; pt <= change->cont2_pt2; pt++)
    if (!imodPointAppend(item.p.cont, &cont->pts[pt]))
      return MemoryError;
  if (cont->sizes)
    for (pt = change->obj2_pt1; pt <= change->cont2_pt2; pt++)
      imodPointSetSize(item.p.cont, pt - change->obj2_pt1, cont->sizes[pt]);

  change->bytes = contourBytes(item.p.cont);

  // If removing points, do so now
  if (remove)
    for (pt = change->cont2_pt2; pt >= change->obj2_pt1; pt--)
      imodPointDelete(cont, pt);

  if (ilistAppend(mItemPool, &item))
    return MemoryError;
  return NoError;
}

// Restore points to a contour, remove backup item
int UndoRedo::restorePoints(UndoChange *change, BackupItem *item)
{
  int pt;
  Icont *cont = &mVi->imod->obj[change->object].cont[change->contour];
  if (!item)
    return NoBackupItem;
  
  // Insert the points in the contour and add the sizes if any
  for (pt = change->obj2_pt1; pt <= change->cont2_pt2; pt++) {
    if (!imodPointAdd(cont, &item->p.cont->pts[pt - change->obj2_pt1], pt))
      return MemoryError;

    if (item->p.cont->sizes)
      imodPointSetSize(cont, pt, item->p.cont->sizes[pt - change->obj2_pt1]);
  }

  // Remove item including all of its data
  freePoolItem(item);
  change->ID = -1;
  change->bytes = 0;
  return NoError;
}

// Exchange a point position between contour and backup item
int UndoRedo::shiftPoint(UndoChange *change, BackupItem *item)
{
  Icont *cont = &mVi->imod->obj[change->object].cont[change->contour];
  Ipoint temp;
  if (!item)
    return NoBackupItem;
  temp = *(item->p.cont->pts);
  *(item->p.cont->pts) = cont->pts[change->obj2_pt1];
  cont->pts[change->obj2_pt1] = temp;
  return NoError;
}


// Exchange a contour between the object and the backup item, either
// including all pointers to data, or just the structure
int UndoRedo::exchangeContours(UndoChange *change, BackupItem *item, 
                               bool structOnly)
{
  Icont temp;
  Icont *cont = &mVi->imod->obj[change->object].cont[change->contour];
  if (!item)
    return NoBackupItem;

  // Exchange the structures
  temp = *(item->p.cont);
  *(item->p.cont) = *cont;
  *cont = temp;
  
  // If copying structure only, restore the pointers to all data from the
  // copy now in the backup item, and null them out there
  if (structOnly) {
    cont->pts = item->p.cont->pts;
    cont->sizes = item->p.cont->sizes;
    cont->label = item->p.cont->label;
    cont->store = item->p.cont->store;
    item->p.cont->pts = NULL;
    item->p.cont->sizes = NULL;
    item->p.cont->label = NULL;
    item->p.cont->store = NULL;
  }
  change->bytes = contourBytes(item->p.cont);
  return NoError;
}

// Insert contour into object from backup item and clear out item
int UndoRedo::restoreContour(UndoChange *change, BackupItem *item)
{
  if (!item)
    return NoBackupItem;
  if (imodDebug('u'))
    imodPrintStderr("Restoring contour %d, size %d to object %d, size %d\n",
                    change->contour, item->p.cont->psize, change->object, 
                    mVi->imod->obj[change->object].contsize);
  if (imodObjectInsertContour(&mVi->imod->obj[change->object], item->p.cont, 
                              change->contour) < 0)
    return MemoryError;
  item->p.cont->pts = NULL;
  item->p.cont->sizes = NULL;
  item->p.cont->label = NULL;
  item->p.cont->store = NULL;
  freePoolItem(item);
  change->ID = -1;
  change->bytes = 0;
  return NoError;
}

// Remove contour from object and copy it into a new backup item
int UndoRedo::removeContour(UndoChange *change)
{
  BackupItem item;
  Iobj *obj = &mVi->imod->obj[change->object];

  // Set up item and IDs
  item.type = Contour;
  item.p.cont = imodContourNew();
  if (!item.p.cont)
    return MemoryError;
  change->ID = item.ID = mID++;

  // Copy contour and remove it
  imodContourCopy(&obj->cont[change->contour], item.p.cont);
  imodObjectRemoveContour(obj, change->contour);
  change->bytes = contourBytes(item.p.cont);
  if (ilistAppend(mItemPool, &item))
    return MemoryError;
  return NoError;
}

// Move a contour from one position to another
int UndoRedo::moveContour(int obFrom, int coFrom, int obTo, int coTo)
{
  Icont temp;
  Iobj *objFrom = &mVi->imod->obj[obFrom];
  Iobj *objTo = &mVi->imod->obj[obTo];

  imodContourCopy(&objFrom->cont[coFrom], &temp);
  imodObjectRemoveContour(objFrom, coFrom);
  if (imodObjectInsertContour(objTo, &temp, coTo) < 0)
    return MemoryError;
  return NoError;
}

int UndoRedo::exchangeObjects(UndoChange *change, BackupItem *item)
{
  Iobj temp;
  Iobj *obj = &mVi->imod->obj[change->object];
  if (!item)
    return NoBackupItem;

  if (imodDebug('u'))
    imodPrintStderr("Exchanging to object %d, size %d, store size obj %d, "
                    "item %d, maxsurf obj %d item %d\n",
                    change->object, obj->contsize, 
                    ilistSize(obj->store), ilistSize(item->p.obj->store),
                    obj->surfsize, item->p.obj->surfsize);

  // Exchange the structures
  vbCleanupVBD(obj);
  temp = *(item->p.obj);
  *(item->p.obj) = *obj;
  *obj = temp;

  // Restore object's cont and mesh pointers from copy in item
  obj->mesh = item->p.obj->mesh;
  obj->cont = item->p.obj->cont;
  item->p.obj->mesh = NULL;
  item->p.obj->cont = NULL;
  change->bytes = objectBytes(item->p.obj);
  return NoError;
}

// Insert an object from the item and clear out the item
int UndoRedo::restoreObject(UndoChange *change, BackupItem *item)
{
  int ob;
  if (!item)
    return NoBackupItem;
 
  // Get a new object at end then shift objects up as needed and copy item
  if (imodNewObject(mVi->imod))
    return MemoryError;
  for (ob = mVi->imod->objsize - 2; ob >= change->object; ob--)
    imodObjectCopy(&mVi->imod->obj[ob], &mVi->imod->obj[ob + 1]);
  imodObjectCopy(item->p.obj, &mVi->imod->obj[change->object]);

  // Clear out the pointers and free copy
  item->p.obj->mesh = NULL;
  item->p.obj->cont = NULL;
  item->p.obj->label= NULL;
  item->p.obj->store = NULL;
  item->p.obj->meshParam = NULL;
  freePoolItem(item);
  change->ID = -1;
  change->bytes = 0;
  imod_cmap(mVi->imod);
  return NoError;
}

// Remove an object from the model and save in a new pool item
int UndoRedo::removeObject(UndoChange *change)
{
  BackupItem item;
  Iobj *obj = &mVi->imod->obj[change->object];

  // Set up item and IDs
  item.type = Object;
  item.p.obj = imodObjectNew();
  if (!item.p.obj)
    return MemoryError;
  change->ID = item.ID = mID++;

  // Copy object and remove it after nulling its pointers
  obj->mesh = NULL;
  obj->cont = NULL;
  obj->label= NULL;
  obj->store = NULL;
  obj->meshParam = NULL;
  imodDeleteObject(mVi->imod, change->object);
  change->bytes = objectBytes(item.p.obj);
  imod_cmap(mVi->imod);
  if (ilistAppend(mItemPool, &item))
    return MemoryError;
  return NoError;
}

// Exchange structures between model and pool item
int UndoRedo::exchangeModels(UndoChange *change, BackupItem *item)
{
  Imod temp;
  Imod *mod = mVi->imod;
  if (!item)
    return NoBackupItem;

  if (imodDebug('u'))
    imodPrintStderr("Exchanging model, angles model %x item %x size model %d" 
                    " item %d\n", mod->slicerAng, item->p.mod->slicerAng,
                    ilistSize(mod->slicerAng), 
                    ilistSize(item->p.mod->slicerAng));

  // Exchange the structures
  temp = *mod; 
  *mod = *(item->p.mod);
  *(item->p.mod) = temp;

  // Restore pointers for unduplicated items and clear them in copy
  mod->file = temp.file;
  mod->obj = temp.obj;
  mod->fileName = temp.fileName;
  mod->refImage = temp.refImage;
  item->p.mod->obj = NULL;
  item->p.mod->fileName = NULL;
  item->p.mod->refImage = NULL;
  change->bytes = modelBytes(item->p.mod);
  slicerNewTime(true);
  if (!ImodvClosed)
    imodvUpdateModel(Imodv, false);
  return NoError;
}

// Shift model forward or back
int UndoRedo::shiftModel(BackupItem *item, int dir)
{
  Ipoint point;
  if (!item)
    return NoBackupItem;
  point = *(item->p.pts);
  if (dir < 0) {
    point.x *= -1.;
    point.y *= -1.;
    point.z *= -1.;
  }
  imodTransXYZ(mVi->imod, point);
  return NoError;
}

// Update the undo and redo button states
void UndoRedo::updateButtons()
{
  if (ImodInfoWidget)
    ImodInfoWidget->setUndoRedo
      (mUndoIndex != 0 && ilistSize(mUnitList) > 0,
       mUndoIndex >=0 && mUndoIndex < ilistSize(mUnitList));
}

// Compute bytes occupied by a contour in the pool; add points and sizes 
// if their pointers are not null
int UndoRedo::contourBytes(Icont *cont)
{
  int count = sizeof(cont);
  if (cont->pts)
    count += cont->psize * sizeof(Ipoint);
  if (cont->sizes)
    count += cont->psize * sizeof(b3dFloat);
  count += labelBytes(cont->label);
  count += ilistSize(cont->store) * sizeof(Istore);
  return count;
}

// Compute size in bytes of a lable, if any
int UndoRedo::labelBytes(Ilabel *label)
{
  int i, count;
  if (!label)
    return 0;
  count  = sizeof(Ilabel) + label->len;
  for (i = 0; i < label->nl; i++)
    count += sizeof (IlabelItem) + label->label[i].len;
  return count;
}

// Compute size in bytes of an object; always include labels; add contours
// and meshes if their pointers are non-null
int UndoRedo::objectBytes(Iobj *obj)
{
  int i, count;
  count = sizeof(Iobj) + labelBytes(obj->label);
  if (obj->cont) {
    for (i = 0; i < obj->contsize; i++)
      count += contourBytes(&obj->cont[i]);
  }
  if (obj->mesh) {
    for (i = 0; i < obj->meshsize; i++)
      count += sizeof(Imesh) + obj->mesh[i].vsize * sizeof(Ipoint) +
        obj->mesh[i].lsize * sizeof(b3dInt32);
  }
  count += ilistSize(obj->store) * sizeof(Istore);
  return count;
}

// Compute size in bytes of model structure in backup pool
int UndoRedo::modelBytes(Imod *mod)
{
  int i, count;
  count = sizeof (Imod);
  if (mod->refImage)
    count += sizeof(IrefImage);
  for (i = 0; i < mod->viewsize; i++)
    count += sizeof(Iview) + mod->view[i].objvsize * sizeof(Iobjview);
  count += ilistSize(mod->store) * sizeof(Istore);
  count += ilistSize(mod->slicerAng) * sizeof(SlicerAngles);
  count += objGroupListBytes(mod->groupList);
  return count;
}

// Record current state of model in the state structure, i.e., current index
// and number of objects, contours, or points as appropriate
void UndoRedo::recordState(UndoState &state)
{
  Iobj *obj;
  state.index = mVi->imod->cindex;
  state.size.object = mVi->imod->objsize;
  if (state.index.object >=0) {
    obj = &mVi->imod->obj[state.index.object];
    state.size.contour = obj->contsize;
    if (state.index.contour >= 0)
      state.size.point = obj->cont[state.index.contour].psize;
  }
}

// Return true if current model state matches that in the structure, ignoring
// the current indexes
bool UndoRedo::stateMatches(UndoState state)
{
  Iobj *obj;
  if (state.size.object != mVi->imod->objsize)
    return false;
  if (state.index.object >=0) {
    obj = &mVi->imod->obj[state.index.object];
    if (state.size.contour != obj->contsize)
      return false;
    if (state.index.contour >= 0 && 
        state.size.point != obj->cont[state.index.contour].psize)
      return false;
  }
  return true;
}

// Free the model structure in a pool item and mark as deleted with ID -1
void UndoRedo::freePoolItem(BackupItem *item)
{
  if (!item || item->ID < 0)
    return;
  switch(item->type) {

  case Model:    // Model: object delete routine will skip if null object
    imodDelete(item->p.mod);
    break;

  case Object:   // Object: both contour and mesh delete skip if null
    imodObjectDelete(item->p.obj);
    break;

  case Contour:  // Contour: point and size freeing skip if null
    imodContourDelete(item->p.cont);
    break;

  case Points:
    if (item->p.pts)
      free(item->p.pts);
    break;
  }

  mNumFreedInPool++;
  item->ID = -1;
}

// Add up the space occupied by the everything from the end backwards and
// trim units from the front of the list if space is exceeded
void UndoRedo::trimLists(int numKeep)
{
  int sum = 0;
  int nUnits = 0;
  int i, j;
  UndoUnit *unit;
  UndoChange *change;
  for (i = ilistSize(mUnitList) - 1; i >= 0; i--) {
    unit = (UndoUnit *)ilistItem(mUnitList, i);
    sum += sizeof(UndoUnit) + sizeof(Ilist);

    // For each change on the list, add the size
    for (j = 0; j < ilistSize(unit->changes); j++) {
      change = (UndoChange *)ilistItem(unit->changes, j);
      sum += sizeof(UndoChange) + change->bytes;
    }

    // If the sum is too high and we are past the number to keep, or if
    // there are too many units, remove units from start to here and quit
    // or remove 1/10 of the units if over the maximum count
    nUnits++;
    if ((nUnits > numKeep && sum > mMaxBytes) || nUnits > mMaxUnits) {
      if (nUnits > mMaxUnits)
        i += mMaxUnits / 10;
      removeUnits(0, i);
      break;
    }
  }
  if (imodDebug('u'))
    imodPrintStderr("Backup pool sum %d back to unit %d%s", sum, 
                    B3DMAX(i, 0), i >= 0 ? ", removed units\n" : "\n");
}

// Remove undo units and all of their changes between start and end of list
void UndoRedo::removeUnits(int start, int end)
{
  int i, j;
  UndoUnit *unit;
  UndoChange *change;
  BackupItem *item;
  if (imodDebug('u'))
    imodPrintStderr("Removing backup units %d to %d\n", start, end);
  for (i = end; i >= start; i--) {
    unit = (UndoUnit *)ilistItem(mUnitList, i);
    
    // For each change on the list, find pool item and free it
    for (j = 0; j < ilistSize(unit->changes); j++) {
      change = (UndoChange *)ilistItem(unit->changes, j);
      item = findPoolItem(change->ID);
      freePoolItem(item);
    }
    
    // Delete the change list and remove the unit from unit list
    ilistDelete(unit->changes);
    ilistRemove(mUnitList, i);
  }
  compactPool();
}

// Compact the pool list and set its size
void UndoRedo::compactPool()
{
  int i, j;
  BackupItem *item, *item2;
  if (imodDebug('u'))
    imodPrintStderr("Compacting backup pool, # freed = %d\n", mNumFreedInPool);
  j = 0;
  for (i = 0; i < ilistSize(mItemPool); i++) {
    item = (BackupItem *)ilistItem(mItemPool, i);
    if (item->ID >= 0) {
      if (j < i) {
        item2 = (BackupItem *)ilistItem(mItemPool, j);
        memcpy(item2, item, sizeof(BackupItem));
      }
      j++;
    }
  }
  ilistTruncate(mItemPool, j);
  mNumFreedInPool = 0;
}

// Look through the pool for the item with the given ID
BackupItem *UndoRedo::findPoolItem(int ID)
{
  BackupItem *item;
  int i;
  if (ID < 0)
    return NULL;
  for (i = 0; i < ilistSize(mItemPool); i++) {
    item = (BackupItem *)ilistItem(mItemPool, i);
    if (item->ID == ID)
      return item;
  }
  return NULL;
}

///////////////////////////////////////////////
// Exported functions that take a vi argument
void undoPointChange(ImodView *vi, int type, int object,
                     int contour, int point, int point2) {
  vi->undo->pointChange(type, object,
                  contour, point, point2);
}
void undoContourChange(ImodView *vi, int type, int object,
                       int contour, int object2, int contour2) {
  vi->undo->contourChange(type, object,
                    contour, object2, contour2);
}
void undoObjectChange(ImodView *vi, int type, int object,
                      int object2) {
  vi->undo->objectChange(type, object,
                   object2);
}
void undoModelChange(ImodView *vi, int type, Ipoint *point) {
  vi->undo->modelChange(type, point);
}

  // Convenience calls that assume the current obj/cont/pt as much as possible
void undoPointShiftCP(ImodView *vi) {
  vi->undo->pointShift();
}
void undoPointShift(ImodView *vi, int point) {
  vi->undo->pointShift(point);
}

void undoPointAdditionCC(ImodView *vi, int point) {
  vi->undo->pointAddition(point);
}
void undoPointAdditionCC2(ImodView *vi, int point, int point2) {
  vi->undo->pointAddition(point, point2);
}
void undoPointAddition(ImodView *vi, int object, int contour,
                       int point) {
  vi->undo->pointAddition(object, contour,
                    point);
}

void undoPointRemovalCP(ImodView *vi) {
  vi->undo->pointRemoval();
}
void undoPointRemoval(ImodView *vi, int point) {
  vi->undo->pointRemoval(point);
}
void undoPointRemoval2(ImodView *vi, int point, int point2) {
  vi->undo->pointRemoval(point, point2);
}

void undoContourDataChgCC(ImodView *vi) {
  vi->undo->contourDataChg();
}
void undoContourDataChg(ImodView *vi, int object, int contour) {
  vi->undo->contourDataChg(object, contour);
}

void undoContourPropChgCC(ImodView *vi) {
  vi->undo->contourPropChg();
}
void undoContourPropChg(ImodView *vi, int object, int contour) {
  vi->undo->contourPropChg(object, contour);
}

void undoContourRemovalCC(ImodView *vi) {
  vi->undo->contourRemoval();
}
void undoContourRemovalCO(ImodView *vi, int contour) {
  vi->undo->contourRemoval(contour);
}
void undoContourRemoval(ImodView *vi, int object, int contour) {
  vi->undo->contourRemoval(object, contour);
}

void undoContourAdditionCO(ImodView *vi, int contour) {
  vi->undo->contourAddition(contour);
}
void undoContourAddition(ImodView *vi, int object, int contour) {
  vi->undo->contourAddition(object, contour);
}

void undoContourMove(ImodView *vi, int object, int contour,
                     int object2, int contour2) {
  vi->undo->contourMove(object, contour,
                  object2, contour2);
}

void undoObjectPropChgCO(ImodView *vi) {
  vi->undo->objectPropChg();
}
void undoObjectPropChg(ImodView *vi, int object) {
  vi->undo->objectPropChg(object);
}

void undoObjectRemovalCO(ImodView *vi) {
  vi->undo->objectRemoval();
}
void undoObjectRemoval(ImodView *vi, int object) {
  vi->undo->objectRemoval(object);
}

void undoObjectAddition(ImodView *vi, int object) {
  vi->undo->objectAddition(object);
}
void undoObjectMove(ImodView *vi, int object, int object2) {
  vi->undo->objectMove(object, object2);
}

void undoModelShift(ImodView *vi, Ipoint *point) {
  vi->undo->modelShift(point);
}

void undoFinishUnit(ImodView *vi) {
  vi->undo->finishUnit();
}
void undoFlushUnit(ImodView *vi) {
  vi->undo->flushUnit();
}
