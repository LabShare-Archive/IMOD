/*   control.h  -  public declarations for control.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$
Log at end of file
*/

#ifndef CONTROL_H
#define CONTROL_H

/* Classes of windows for the dialog manager */
#define IMODV_DIALOG 0
#define IMOD_DIALOG  1
#define IMOD_IMAGE   2

/* Types of windows for finding geometries */
enum {ZAP_WINDOW_TYPE, UNKNOWN_TYPE};

typedef struct ilist_struct Ilist;

#ifndef IMODP_H
  typedef struct ViewInfo ImodView;
#endif
  class QKeyEvent;
  class QString;
  class QWidget;
class QRect;

typedef void (*ImodControlProc)(struct ViewInfo *, void *, int);
typedef void (*ImodControlKey)(struct ViewInfo *, void *, int,  QKeyEvent *);

#ifndef CONTROLP_H
#include "controlP.h"
#endif

extern "C" {

/****************************************************************************/
/* Create a new drawing control for an imod view. 
 * A nonzero integer that is used as the inCtrlId
 * in other control functions is returned.
 *
 * The inDrawFunction is called when the imod draw function
 * is called.  The integer in the third argument contains
 * the draw flags.
 *
 * The inQuitFunction is called when a user quits imod.
 *
 */

int ivwNewControl(ImodView *inImodView,
		  ImodControlProc inDrawFunction,
		  ImodControlProc inQuitFunction,
		  ImodControlKey inKeyFunction,
		  void *data);

/* delete the control associated with the inCtrlId value.
 * this will also call the close or quit method of the control.
 */
int ivwDeleteControl(ImodView *iv, int inCtrlId);

/* remove the control associated with the inCtrlId value.
 * do not call the close method of the control
 */
int ivwRemoveControl(ImodView *iv, int inCtrlId);

/* move control to top of control Q if it exists
 * also sets or clears the control active flag.
 * returns the id of the highest priority control id.
 */
int ivwControlPriority(ImodView *iv, int inCtrlId);

/* make the given control the active one.
 * A zero value for inCtrlId make no control the active one.
 */
void ivwControlActive(ImodView *iv, int inCtrlId);    

/* Pass a key event on to the first control on the list that has a callback
 */
void ivwControlKey(int released, QKeyEvent *e);


// A dialog manager class for hiding, showing, and closing windows in concert
class DialogManager
{
 public:
  DialogManager();
  ~DialogManager() {};
  void add(QWidget *widget, int dlgClass = IMODV_DIALOG, 
           int dlgType = UNKNOWN_TYPE);
  void remove(QWidget *widget);
  void close();
  void hide();
  void show();
  QWidget *parent(int dlgClass);
  void raise(int dlgClass);
  QRect biggestGeometry(int dlgType);

 private:
  Ilist *mDialogList;
  QRect mLastZapGeom;
};

/* Global instances */
extern DialogManager imodvDialogManager;
extern DialogManager imodDialogManager;
}
#endif

/*
$Log$
Revision 4.6  2003/09/24 17:32:25  mast
Add declaration for restorable geometry call

Revision 4.5  2003/09/17 05:54:36  mast
Add variable for geometry of last zap window closed

Revision 4.4  2003/09/17 04:46:43  mast
Added function to return size of biggest Zap window

Revision 4.3  2003/05/23 02:46:05  mast
Added raise function

Revision 4.2  2003/04/17 19:00:59  mast
new function to provide machine-dependent parent widget

Revision 4.1  2003/02/10 20:41:54  mast
Merge Qt source

Revision 1.1.2.6  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.5  2003/01/14 21:46:38  mast
renamed dialog manager for imod

Revision 1.1.2.4  2003/01/13 07:20:21  mast
Added dialog manager class

Revision 1.1.2.3  2003/01/06 15:39:08  mast
add another orphan declaration

Revision 1.1.2.2  2003/01/04 03:44:16  mast
Add declaration for removeControl; stick inputQDefaultKeys declaration here

Revision 1.1.2.1  2003/01/02 15:36:19  mast
Initial creation

*/
