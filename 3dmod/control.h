/*   control.h  -  public declarations for control.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  No more Log
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
class QKeyEvent;
class QString;
class QWidget;

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
enum {ZAP_WINDOW_TYPE, MULTIZ_WINDOW_TYPE, SLICER_WINDOW_TYPE, XYZ_WINDOW_TYPE,
      GRAPH_WINDOW_TYPE, TUMBLER_WINDOW_TYPE, UNKNOWN_TYPE};
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
   * [dlgType] (default UNKNOWN_TYPE), and a control ID can be supplied in [ctrlId].  */
  void add(QWidget *widget, int dlgClass = IMODV_DIALOG, 
           int dlgType = UNKNOWN_TYPE, int ctrlId = -1);

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
  QObject *getTopWindow(int dlgType);
  QObject *getTopWindow(int dlgType, int dlgType2, int &typeFound);
  QObject *getTopWindow(bool withBand, bool withLasso, int type, int *index);

 private:
  Ilist *mDialogList;
  QRect mLastZapGeom;
};

/* END_SECTION */

}
#endif
