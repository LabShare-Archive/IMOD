/*   undoredo.h  -  declarations for undoredo.cpp
 *
 *   Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */                                                                           

/*

$Log$
Revision 4.2  2004/11/21 05:54:02  mast
Changes for working from model view

Revision 4.1  2004/11/20 05:04:40  mast
initial addition

*/
#ifndef UNDOREDOP_H
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

#endif
