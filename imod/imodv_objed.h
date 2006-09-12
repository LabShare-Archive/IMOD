/*   imodv_objed.h  -  declarations for imodv_objed.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$
Log at end
*/

#ifndef IMODV_OBJED_H
#define IMODV_OBJED_H

typedef struct __imodv_struct ImodvApp;
#include <qwidget.h>
class QGridLayout;
class QFrame;
class QScrollView;

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
void imodvObjedNewView(void);
void imodvObjectListDialog(ImodvApp *a, int state);
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
  void globalQualitySlot(int value);
  void lineWidthSlot(int which, int value, bool dragging);
  void lineAliasSlot(bool state);
  void lineThickenSlot(bool state);
  void meshShowSlot(int value);
  void meshFalseSlot(bool state);
  void meshSkipLoSlot(bool state);
  void meshSkipHiSlot(bool state);
  void meshLevelSlot(int which, int value, bool dragging);
  void clipGlobalSlot(int value);
  void clipSkipSlot(bool state);
  void clipPlaneSlot(int value);
  void clipResetSlot();
  void clipInvertSlot();
  void clipToggleSlot(bool state);
  void clipMoveAllSlot(bool state);
  void moveCenterSlot();
  void moveAxisSlot(int which);
  void subsetSlot(int which);
  void makePassSlot(int value);
  void makeDiamSlot(int value);
  void makeTolSlot(int value);
  void makeZincSlot(int value);
  void makeFlatSlot(int value);
  void makeStateSlot(int which);
  void makeDoitSlot();
  void toggleObjSlot(int ob);
  void toggleListSlot(int ob);

 protected:
  void timerEvent(QTimerEvent *e);

 private:
  int mTimerID;
#ifdef QT_THREAD_SUPPORT
  QThread *mMeshThread;
#endif

};



class ImodvOlist : public QWidget
{
  Q_OBJECT

 public:
  ImodvOlist(QWidget *parent, const char *name = NULL, 
                WFlags fl =  Qt::WDestructiveClose | Qt::WType_TopLevel);
  ~ImodvOlist() {};

  QGridLayout *mGrid;
  QFrame *mFrame;
  QScrollView *mScroll;

  public slots:
    void donePressed();

 protected:
    void closeEvent ( QCloseEvent * e );
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
};

#endif

/*
$Log$
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
