/*   control.h  -  declarations for control.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
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
#ifndef CONTROL_H
#define CONTROL_H

/* Classes of windows for the dialog manager */
#define IMODV_DIALOG 0
#define IMOD_DIALOG  1
#define IMOD_IMAGE   2

  // Include rather than forward declare - Ilist has no structure name
#include "ilist.h"

#ifndef IMODP_H
  typedef struct ViewInfo ImodView;
#endif
  class QKeyEvent;
  class QString;
  class QWidget;

/* Each window that shows the view below uses this control 
 * stucture to have the view update the window.
 */
typedef void (*ImodControlProc)(struct ViewInfo *, void *, int);
typedef void (*ImodControlKey)(struct ViewInfo *, void *, int,  QKeyEvent *);

typedef struct
{
  void *userData;
  ImodControlProc draw_cb;
  ImodControlProc close_cb;
  ImodControlKey  key_cb;
  int  id;
  int  status;
  
}ImodControl;

/* This structure sits inside of each view and is the
 * master controller.
 */
typedef struct imod_control_list
{ 
  Ilist *      list;
  int          active;
  int          top;
  int          reason;
  int          workID;
}ImodControlList;


/* private control functions.  The rest were declared in imod.h */
void ivwControlListDrawCancel(ImodView *iv);
void ivwControlListDraw(ImodView *iv, int reason);
void ivwControlListDelete(ImodView *iv);
void ivwControlKey(/*ImodView *iv,*/ int released, QKeyEvent *e);
void ivwWorkProc(ImodView *iv);

       /* The functions from imod.h */
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

// A dialog manager class for hiding, showing, and closing windows in concert
class DialogManager
{
 public:
  DialogManager();
  ~DialogManager() {};
  void add(QWidget *widget, int dlgClass = IMODV_DIALOG);
  void remove(QWidget *widget);
  void close();
  void hide();
  void show();
  QWidget *parent(int dlgClass);

 private:
  Ilist *mDialogList;
};

/* Global instances */
extern DialogManager imodvDialogManager;
extern DialogManager imodDialogManager;

#endif
