/*   undoredo.h  -  declarations for undoredo.cpp
 *
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */                                                                           
#ifndef UNDOREDO_H
#define UNDOREDO_H

#ifndef IMODP_H
typedef struct ViewInfo ImodView;
#endif
typedef struct undo_state UndoState;
typedef struct undo_unit UndoUnit;
typedef struct undo_change  UndoChange;
typedef struct backup_item BackupItem;

#ifndef UNDOREDOP_H
#include "undoredoP.h"
#endif

/* Define macro for export of functions under Windows */
#ifndef DLL_EX_IM
#ifdef _WIN32
#define DLL_EX_IM _declspec(dllexport)
#else
#define DLL_EX_IM
#endif
#endif

extern "C" {

  // The full-blown calls
  /*!
   * Register change of given [type] for [contour] in [object], between
   * [point] and [point2].  The default for [object], [contour] and [point] 
   * are the current object, contour, or point if any is < -1;
   * the default for [point2] is [point] if [point2] < 0. 
   */
  void DLL_EX_IM undoPointChange(ImodView *vi, int type, int object,
                                 int contour, int point, int point2);

  /*!
   * Register change of given [type] for [contour] in [object], potentially
   * involving [contour2] in [object2].  The default for [object] or 
   * [contour] are the current object or contour if one is < -1.
   */
  void DLL_EX_IM undoContourChange(ImodView *vi, int type, int object,
                                   int contour, int object2, int contour2);

  /*!
   * Register change of given [type] for [[object], potentially
   * involving [object2].  The default for [object] is
   * the current object if it is < -1.
   */
  void DLL_EX_IM undoObjectChange(ImodView *vi, int type, int object,
                                  int object2);

  /*! Register change in the model of given [type].  [point] specifies the
    shift for a PointShift */
  void DLL_EX_IM undoModelChange(ImodView *vi, int type, Ipoint *point);

  // Convenience calls that assume the current obj/cont/pt as much as possible
  /*! Register shift of current point */
  void DLL_EX_IM undoPointShiftCP(ImodView *vi);

  /*! Register shift of point with index [point] in current contour */
  void DLL_EX_IM undoPointShift(ImodView *vi, int point);

  /*! Register addition of point with index [point] in current contour */
  void DLL_EX_IM undoPointAdditionCC(ImodView *vi, int point);

  /*! Register addition of points from index [point] to index [point2] in 
    current contour */
  void DLL_EX_IM undoPointAdditionCC2(ImodView *vi, int point, int point2);

  /*! Register addition of a point at index [point] in [contour] of [object] */
  void DLL_EX_IM undoPointAddition(ImodView *vi, int object, int contour,
                                   int point);

  /*! Register deletion of the current point */
  void DLL_EX_IM undoPointRemovalCP(ImodView *vi);
  /*! Register deletion of the point at index [point] in the current contour */
  void DLL_EX_IM undoPointRemoval(ImodView *vi, int point);

  /*! Register deletion of points from index [point] to [point2] in the
   current contour. */
  void DLL_EX_IM undoPointRemoval2(ImodView *vi, int point, int point2);

  /*! Register change in the data (points) of the current contour */
  void DLL_EX_IM undoContourDataChgCC(ImodView *vi);

  /*! Register change in the data  of [contour] in [object] */
  void DLL_EX_IM undoContourDataChg(ImodView *vi, int object, int contour);

  /*! Register change in properties only of the current contour */
  void DLL_EX_IM undoContourPropChgCC(ImodView *vi);

  /*! Register change in properties only of [contour] in [object] */
  void DLL_EX_IM undoContourPropChg(ImodView *vi, int object, int contour);

  /*! Register deletion of current contour */
  void DLL_EX_IM undoContourRemovalCC(ImodView *vi);

  /*! Register deletion of contour with index [contour] in current object */
  void DLL_EX_IM undoContourRemovalCO(ImodView *vi, int contour);

  /*! Register deletion of [contour] in [object] */
  void DLL_EX_IM undoContourRemoval(ImodView *vi, int object, int contour);

  /*! Register addition of contour at index [contour] in current object */
  void DLL_EX_IM undoContourAdditionCO(ImodView *vi, int contour);

  /*! Register addition of contour at index [contour] in [object] */
  void DLL_EX_IM undoContourAddition(ImodView *vi, int object, int contour);

  /*! Register move of a contour from [object], [contour] to [object2],
   [contour2]. */
  void DLL_EX_IM undoContourMove(ImodView *vi, int object, int contour,
                                 int object2, int contour2);

  /*! Register a change in properties of the current object */
  void DLL_EX_IM undoObjectPropChgCO(ImodView *vi);

  /*! Register a change in properties of object at index [object] */
  void DLL_EX_IM undoObjectPropChg(ImodView *vi, int object);

  /*! Register deletion of the current object */
  void DLL_EX_IM undoObjectRemovalCO(ImodView *vi);

  /*! Register deletion of an object at index [object] */
  void DLL_EX_IM undoObjectRemoval(ImodView *vi, int object);

  /*! Register addition of an object at index [object] */
  void DLL_EX_IM undoObjectAddition(ImodView *vi, int object);

  /*! Register moving an object from index [object] to [object2] */
  void DLL_EX_IM undoObjectMove(ImodView *vi, int object, int object2);

  /*! Register a shift of the model by amounts in [point] */
  void DLL_EX_IM undoModelShift(ImodView *vi, Ipoint *point);

  /*! Finish (close) a unit of changes to the model.  Either this function 
   * or the flush function must always be called eventually after registering
   * changes. */
  void DLL_EX_IM undoFinishUnit(ImodView *vi);

  /*! Flush all the changes that were registered since this unit of changes 
    was started.  Call this function if an operation is aborted due to an
    error after changes were registered.  */
  void DLL_EX_IM undoFlushUnit(ImodView *vi);


class DLL_EX_IM UndoRedo 
{
 public:
  enum ChangeTypes {ContourData, ContourProperty, ContourRemoved, ContourAdded, 
                    ContourMoved, ObjectChanged, ObjectRemoved, ObjectAdded, ObjectMoved,
                    ModelChanged, PointsAdded, PointsRemoved, PointShifted, ModelShifted,
                    ViewChanged};
  enum ItemTypes {Model, Object, Contour, Points};
  enum ErrorTypes {NoError = 0, NoneAvailable, StateMismatch, MemoryError,
                   NoBackupItem};

  UndoRedo(ImodView *vi);
  ~UndoRedo();

  // The full-blown calls with default values
  void pointChange(int type, int object = -2, int contour = -2, 
                     int point = -2, int point2 = -1);
  void contourChange(int type, int object = -2, int contour = -2, 
                     int object2 = -1, int contour2 = -1);
  void objectChange(int type, int object = -2, int object2 = -1);
  void modelChange(int type = ModelChanged, Ipoint *point = NULL);

  // Convenience calls that assume the current obj/cont/pt as much as possible
  void pointShift() {pointChange(PointShifted);};
  void pointShift(int point) {pointChange(PointShifted, -2, -2, point);};

  void pointAddition(int point) {pointChange(PointsAdded, -2, -2, point);};
  void pointAddition(int point, int point2)
    {pointChange(PointsAdded, -2, -2, point, point2);};
  void pointAddition(int object, int contour, int point)
    {pointChange(PointsAdded, object, contour, point);};

  void pointRemoval() {pointChange(PointsRemoved);};
  void pointRemoval(int point) {pointChange(PointsRemoved, -2, -2, point);};
  void pointRemoval(int point, int point2)
    {pointChange(PointsRemoved, -2, -2, point, point2);};

  void contourDataChg() {contourChange(ContourData);};
  void contourDataChg(int object, int contour) 
    {contourChange(ContourData, object, contour);};

  void contourPropChg() {contourChange(ContourProperty);};
  void contourPropChg(int object, int contour) 
    {contourChange(ContourProperty, object, contour);};

  void contourRemoval() {contourChange(ContourRemoved);};
  void contourRemoval(int contour)
    {contourChange(ContourRemoved, -2, contour);};
  void contourRemoval(int object, int contour)
    {contourChange(ContourRemoved, object, contour);};

  void contourAddition(int contour)
    {contourChange(ContourAdded, -2, contour);};
  void contourAddition(int object, int contour)
    {contourChange(ContourAdded, object, contour);};

  void contourMove(int object, int contour, int object2, int contour2)
    {contourChange(ContourMoved, object, contour, object2, contour2);};
  void allContourMove(int object, int object2);

  void objectPropChg() {objectChange(ObjectChanged);};
  void objectPropChg(int object) {objectChange(ObjectChanged, object);};

  void objectRemoval() {objectChange(ObjectRemoved);};
  void objectRemoval(int object) {objectChange(ObjectRemoved, object);};

  void objectAddition(int object) {objectChange(ObjectAdded, object);};
  void objectMove(int object, int object2) 
    {objectChange(ObjectMoved, object, object2);};

  void modelShift(Ipoint *point) {modelChange(ModelShifted, point);};

  UndoUnit *getOpenUnit();
  void finishUnit();
  void flushUnit();
  void clearUnits();
  int undo();
  int redo();

 private:
  int contourBytes(Icont *cont);
  int objectBytes(Iobj *obj);
  int modelBytes(Imod *mod);
  int labelBytes(Ilabel *label);
  void recordState(UndoState &state);
  bool stateMatches(UndoState state);
  void freePoolItem(BackupItem *item);
  void removeUnits(int start, int end);
  void finishChange(UndoUnit *unit, UndoChange *change);
  void initChange(UndoChange &change, int type, int object = -1, 
                  int contour = -1, int object2 = -1, int contour2 = -1);
  void compactPool();
  void trimLists(int numKeep);
  void memoryError();
  BackupItem *findPoolItem(int ID);
  int exchangeContours(UndoChange *change, BackupItem *item, bool structOnly);
  int restoreContour(UndoChange *change, BackupItem *item);
  int removeContour(UndoChange *change);
  int moveContour(int obFrom, int coFrom, int coTo, int obTo);
  int exchangeObjects(UndoChange *change, BackupItem *item);
  int restoreObject(UndoChange *change, BackupItem *item);
  int removeObject(UndoChange *change);
  int exchangeModels(UndoChange *change, BackupItem *item);
  int copyPoints(UndoChange *change, bool remove);
  int restorePoints(UndoChange *change, BackupItem *item);
  int shiftPoint(UndoChange *change, BackupItem *item);
  int shiftModel(BackupItem *item, int dir);
  void finishUndoRedo();
  void updateButtons();

  ImodView *mVi;
  Ilist *mUnitList;
  Ilist *mItemPool;
  int mID;
  bool mRejectChanges;
  bool mUnitOpen;
  int mMaxUnits;
  int mMaxBytes;
  int mUndoIndex;
  int mNumFreedInPool;
};

}
#endif
