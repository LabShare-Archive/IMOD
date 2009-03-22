/*   control.h  -  public declarations for control.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef CONTROL_H
#define CONTROL_H

/* Define macro for export of functions under Windows */
#ifndef DLL_EX_IM
#ifdef _WIN32
#define DLL_EX_IM _declspec(dllexport)
#else
#define DLL_EX_IM
#endif
#endif

#include <qrect.h>
//Added by qt3to4:
#include <QKeyEvent>

/* DOC_SECTION MANAGERDEF */
/* DOC_CODE Dialog Manager */
// A dialog manager class for hiding, showing, and closing windows in concert
class DialogManager;

/* Global instances */
extern DLL_EX_IM DialogManager imodvDialogManager;
extern DLL_EX_IM DialogManager imodDialogManager;

/* Classes of windows for the dialog manager */
#define IMODV_DIALOG 0
#define IMOD_DIALOG  1
#define IMOD_IMAGE   2

/* Types of windows for finding geometries and finding top windows */
enum {ZAP_WINDOW_TYPE, MULTIZ_WINDOW_TYPE, SLICER_WINDOW_TYPE,
      GRAPH_WINDOW_TYPE, UNKNOWN_TYPE};
/* END_CODE */

/*!
 * Processes events, adjusts size of [widget] if [doSize] is true, makes sure 
 * it is fully on the desktop, then shows the widget.  On Mac OSX, if 
 * [dlgClass] is non-negative, the 
 * widget is retored to the window manager's position then, if necessary,
 * moved so as not to overlay the parent (Info window or model view).
 */
void DLL_EX_IM adjustGeometryAndShow(QWidget *widget, int dlgClass, 
                                     bool doSize = true);

/* END_SECTION */

typedef struct ilist_struct Ilist;

/* DOC_SECTION CONTROLDEF */
/* DOC_CODE Control callbacks */
/* Function typedefs for the callback functions passed to @ivwNewControl.  
   The second argument will have the [data] value passed to @ivwNewControl. */
typedef void (*ImodControlProc)(struct ViewInfo *, void *, int);
typedef void (*ImodControlKey)(struct ViewInfo *, void *, int,  QKeyEvent *);
/* END_CODE */
/* END_SECTION */

#ifndef IMODP_H
typedef struct ViewInfo ImodView;
#endif
class QKeyEvent;
class QString;
class QWidget;
//class QObjectList;

#ifndef CONTROLP_H
#include "controlP.h"
#endif

extern "C" {

/****************************************************************************/

/*!
 * Creates a new image drawing control for a 3dmod view and returns a
 * nonzero integer that is used as the [inCtrlId] when calling other control 
 * functions.  ^
 * ^  The [inDrawFunction] is called when the 3dmod draw function is called.
 * The integer in the third argument contains the draw flags.
 * ^  The [inQuitFunction] is called when a user quits 3dmod.
 * ^  The [inKeyFunction] is optional and if it is not NULL it is called with 
 * key events that dialogs do not accept.
 * ^  [data] should be a pointer to the window's data structure so that 
 * multiple instances can be distinguished
 */
int DLL_EX_IM ivwNewControl(ImodView *inImodView,
                            ImodControlProc inDrawFunction,
                            ImodControlProc inQuitFunction,
                            ImodControlKey inKeyFunction,
                            void *data);

/*!
 * Deletes the control associated with the ID [inCtrlId].
 * This will also call the quit callback function of the control.
 */
int DLL_EX_IM ivwDeleteControl(ImodView *iv, int inCtrlId);

/*!
 * Removes the control associated with the ID [inCtrlId].
 * This does not call the quit callback function of the control and is 
 * typically  called by the control itself when it is closing.
 */
int DLL_EX_IM ivwRemoveControl(ImodView *iv, int inCtrlId);

/*!
 * Moves the control with ID [inCtrlId] to the top of the control queue if it
 * exists.  Also sets this one as the active control, which means that it alone
 * is drawn with the IMOD_DRAW_ACTIVE flag.  If necessary, pending draws are
 * stopped before the conrol queue is reordered.
 * Returns the ID of the highest priority control.
 */
int DLL_EX_IM ivwControlPriority(ImodView *iv, int inCtrlId);

/*!
 * Makes the control with ID [inCtrlId] the active one, which means that if it
 * is also the top priority control, it is drawn with the IMOD_DRAW_ACTIVE
 * flag.  A zero value for inCtrlId make no control the active one.
 */
void DLL_EX_IM ivwControlActive(ImodView *iv, int inCtrlId);    

/*!
 * Passes a key event [e] on to the first control on the list that has a key
 * callback.
 */
void DLL_EX_IM ivwControlKey(int released, QKeyEvent *e);

/* DOC_SECTION MANAGER */

// A dialog manager class for hiding, showing, and closing windows in concert
class DLL_EX_IM DialogManager
{
 public:
  DialogManager();
  ~DialogManager() {};

  /*! Adds [widget] to the list for this {DialogManager}.  The class of dialog
   * or window is given in [dlgClass] and the specific type, if any, in
   * [dlgType].  */
  void add(QWidget *widget, int dlgClass = IMODV_DIALOG, 
           int dlgType = UNKNOWN_TYPE);

  /*! Removes [widget] from the list for this {DialogManager}. */
  void remove(QWidget *widget);
  void close();
  void hide();
  void show();

  /*! On Mac OSX, returns the parent window for the give type of 
   * dialog/window, [dlgClass].  Elsewhere, returns NULL.
   */
  QWidget *parent(int dlgClass);
  void raise(int dlgClass);
  QRect biggestGeometry(int dlgType);
  int windowCount(int dlgType);
  void windowList(QObjectList *objList, int dlgClass, int dlgType);

 private:
  Ilist *mDialogList;
  QRect mLastZapGeom;
};

/* END_SECTION */

}
#endif

/*
$Log$
Revision 4.16  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.15  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 4.14  2008/01/11 17:31:45  mast
Needed to include qrect.h, not just forward declare class

Revision 4.13  2007/12/04 22:02:41  mast
Changes for documentation

Revision 4.12  2006/08/24 21:30:52  mast
Added identifier for graph windows

Revision 4.11  2004/08/12 17:02:33  mast
added SLICER type

Revision 4.10  2004/05/31 23:10:56  mast
Added macros for exporting/importing under Windows

Revision 4.9  2004/05/05 17:33:19  mast
Added function to get list of windows of one type

Revision 4.8  2004/04/28 23:52:39  mast
Added windowCount method

Revision 4.7  2003/10/01 04:59:52  mast
Split into public and private files

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
