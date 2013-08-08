//Added by qt3to4:
#include <QTimerEvent>
/*   mv_objed.h  -  declarations for mv_objed.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           

#ifndef IMODV_OBJED_H
#define IMODV_OBJED_H

typedef struct __imodv_struct ImodvApp;
typedef struct Mod_Object Iobj;
#include <qwidget.h>
class QFrame;
class QPaintEvent;

/******************************************************************
 * Object Edit Field allows easier expansion
 ******************************************************************/
typedef struct
{
  const char *label;               /* Label used for list widget.      */
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
  void makeZeditSlot();
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

class MeshColorBar : public QWidget
{
  Q_OBJECT

 public:
  MeshColorBar(QWidget * parent = 0);
  ~MeshColorBar() {};
 protected:
  void paintEvent ( QPaintEvent * event );
  
};
#endif
