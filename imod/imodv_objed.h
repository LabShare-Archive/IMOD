//Added by qt3to4:
#include <QTimerEvent>
/*   imodv_objed.h  -  declarations for imodv_objed.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */                                                                           

#ifndef IMODV_OBJED_H
#define IMODV_OBJED_H

typedef struct __imodv_struct ImodvApp;
typedef struct Mod_Object Iobj;
#include <qwidget.h>
class QFrame;

/******************************************************************
 * Object Edit Field allows easier expansion
 ******************************************************************/
typedef struct
{
  char    *label;                  /* Label used for list widget.      */
  void    (*mkwidget)(int index);  /* Function used to make edit area. */
  void    (*setwidget)(void);      /* Function to adjust internal data */
  void    (*fixwidget)(void);      /* Function to set widget sizes     */
  QWidget *control;                /* Runtime widget storage.          */

}ObjectEditField;

extern ObjectEditField objectEditFieldData[];

/******************************************************************
 * Public Interface function prototypes.
 ******************************************************************/

/* Close up and clean the object edit dialog. */
int object_edit_kill(void);

/* Create and init the object edit dialog. */
void objed(ImodvApp *a);
Iobj *objedObject(void);
void imodvObjedNewView(void);
void objedToggleObj(int ob, bool state);
void imodvObjedDrawData(int option);
void imodvObjedFramePicked(int item);
void imodvObjedStyleData(int option);
void imodvObjedEditData(int option);
void imodvObjedSelect(int which);
bool meshingBusy(void);
void imodvObjedHelp();
void imodvObjedDone();
void imodvObjedClosing();
void imodvObjedCtrlKey(bool pressed);
void imodvObjedName(const char *name);
void imodvObjedSelect(int which);
void imodvObjedMakeOnOffs(QFrame *frame);
void imodvObjedMoveToAxis(int which);
void imodvObjedToggleClip(void);

#ifdef QT_THREAD_SUPPORT
#include <qthread.h>

class MeshThread : public QThread
{
 public:
  MeshThread() {};
  ~MeshThread() {};

 protected:
  void run();
};
#endif

class ImodvObjed : public QObject
{
  Q_OBJECT

 public:
  ImodvObjed(QObject *parent = NULL, const char *name = NULL);
  ~ImodvObjed() {};
  int meshOneObject(Iobj *obj);
  int startMeshingNext();
  void makeSpinChanged(int which, double value);

  public slots:
    void lineColorSlot(int color, int value, bool dragging);
  void multipleColorSlot(bool state);
  void fillToggleSlot(bool state);
  void fillPntToggleSlot(bool state);
  void bothSidesSlot(bool state);
  void fillColorSlot(int color, int value, bool dragging);
  void materialSlot(int which, int value, bool dragging);
  void pointSizeSlot(int value);
  void pointQualitySlot(int value);
  void pointNoDrawSlot(bool state);
  void globalQualitySlot(int value);
  void lineWidthSlot(int which, int value, bool dragging);
  void lineAliasSlot(bool state);
  void lineThickenSlot(bool state);
  void autoNewContSlot(bool state);
  void openObjectSlot(bool state);
  void meshShowSlot(int value);
  void meshFalseSlot(bool state);
  void meshConstantSlot(bool state);
  void meshSkipLoSlot(bool state);
  void meshSkipHiSlot(bool state);
  void meshLevelSlot(int which, int value, bool dragging);
  void clipGlobalSlot(int value);
  void clipSkipSlot(bool state);
  void clipShowSlot(bool state);
  void clipPlaneSlot(int value);
  void clipResetSlot(int which);
  void clipInvertSlot();
  void clipToggleSlot(bool state);
  void clipMoveAllSlot(bool state);
  void moveCenterSlot();
  void moveAxisSlot(int which);
  void subsetSlot(int which);
  void makePassSlot(int value);
  void makeDiamSlot(double value);
  void makeTolSlot(double value);
  void makeZincSlot(int value);
  void makeFlatSlot(double value);
  void makeStateSlot(int which);
  void makeDoitSlot();
  void makeDoAllSlot();
  void toggleObjSlot(int ob);

 protected:
  void timerEvent(QTimerEvent *e);

 private:
  int mTimerID;
#ifdef QT_THREAD_SUPPORT
  QThread *mMeshThread;
#endif

};


#endif

/*

$Log$
Revision 4.17  2009/01/15 16:33:18  mast
Qt 4 port

Revision 4.16  2008/11/16 04:24:01  mast
Function for processing meshing spin boxes

Revision 4.15  2008/06/17 20:16:58  mast
Added slot for not drawing spheres when drawing mesh

Revision 4.14  2008/05/27 05:46:33  mast
Meshing and moveto axis changes

Revision 4.13  2008/05/22 15:43:21  mast
Changes for extra object editability

Revision 4.12  2008/01/21 17:48:12  mast
Split object list off

Revision 4.11  2007/09/22 00:06:20  mast
Added constant mesh color slot

Revision 4.10  2007/09/20 22:06:55  mast
Changes for visualizing clipping plane

Revision 4.9  2007/07/08 16:44:14  mast
Added slots for open/closed and auto new cont

Revision 4.8  2006/09/12 15:48:20  mast
Added panel to run meshing

Revision 4.7  2006/08/31 23:27:45  mast
Changes for stored value display

Revision 4.6  2005/06/06 17:24:59  mast
Added sphere fill color slot

Revision 4.5  2004/09/21 20:20:14  mast
Declarations for multiple clipping plane slots, new object list in
scroll view, and editing multiple colore

Revision 4.4  2004/04/28 05:28:52  mast
Changes for drawing current contour thicker

Revision 4.3  2003/06/27 19:28:46  mast
Changes for manipulating point quality

Revision 4.2  2003/03/26 17:15:31  mast
Adjust sizes for font changes

Revision 4.1  2003/02/10 20:41:55  mast
Merge Qt source

Revision 1.1.2.4  2003/01/27 00:30:07  mast
Pure Qt version and general cleanup

Revision 1.1.2.3  2003/01/01 05:43:44  mast
rationalizing toplevel versus dialog style

Revision 1.1.2.2  2002/12/27 01:21:16  mast
Qt version

Revision 1.1.2.1  2002/12/18 04:10:30  mast
initial creation

*/
