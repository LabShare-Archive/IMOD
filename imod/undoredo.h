/*   undoredo.h  -  declarations for undoredo.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
*/
#ifndef UNDOREDO_H
#define UNDOREDO_H

#include "imodel.h"
typedef struct ViewInfo ImodView;

// Structure for assessing/setting state of model before/after  change
typedef struct undo_state {
  Iindex index;
  Iindex size;
} UndoState;

// Structure to describe one set of changes applied as a unit
typedef struct undo_unit {
  UndoState before;
  UndoState after;
  Ilist *changes;
} UndoUnit;

// A structure to describe a basic change of the model
typedef struct undo_change {
  int type;        // Type of model change
  int object;      // Object being changed
  int contour;     // Contour being changed
  int obj2_pt1;    // Object being moved to / Starting point
  int cont2_pt2;   // Contour being moved to / Ending point
  int ID;          // Unique ID for item in pool, -1 if none
  int bytes;       // Number of bytes occupied by item in pool
} UndoChange;

union ModObjCont {
  Imod *mod;
  Iobj *obj;
  Icont *cont;
  Ipoint *pts;
};

// Structure for an item in the backup pool
typedef struct backup_item {
  union ModObjCont p;       // The pointer to the model item
  int type;                 // Type item type
  int ID;                   // The unique ID, or -1 if deleted already
} BackupItem;

class UndoRedo 
{
 public:
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
  enum ChangeTypes {ContourData, ContourProperty, ContourRemoved, 
                    ContourAdded, ContourMoved, ObjectChanged, ObjectRemoved,
                    ObjectAdded, ObjectMoved, ModelChanged, PointsAdded,
                    PointsRemoved, PointShifted, ModelShifted};
  enum ItemTypes {Model, Object, Contour, Points};
  enum ErrorTypes {NoError = 0, NoneAvailable, StateMismatch, MemoryError,
                   NoBackupItem};

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

#endif
