/*   info_setup.h  -  declarations for InfoWindow class implemented in
 *                      info_setup.cpp and info_menu.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 */                                                                           
#ifndef IMOD_INFO_H
#define IMOD_INFO_H

#include <qmainwindow.h>
#include <qprocess.h>
//Added by qt3to4:
#include <QCloseEvent>
#include <QTimerEvent>
#include <QKeyEvent>
#include <QEvent>

class InfoControls;
class QTextEdit;
class QTimer;
class InfoWindow;
class QAction;

typedef struct Mod_Model Imod;

extern InfoControls *ImodInfoWidget;
extern InfoWindow *ImodInfoWin;
extern int ImodForbidLevel;

enum {FILE_MENU_NEW, FILE_MENU_OPEN, FILE_MENU_RELOAD, FILE_MENU_SAVE,
      FILE_MENU_SAVEAS, FILE_MENU_SNAPDIR, FILE_MENU_SNAPGRAY, 
      FILE_MENU_MOVIEMONT, FILE_MENU_TIFF, FILE_MENU_EXTRACT, 
      FILE_MENU_SAVEINFO, FILE_MENU_QUIT,
      FWRITE_MENU_IMOD, FWRITE_MENU_WIMP, FWRITE_MENU_NFF, FWRITE_MENU_SYNU,
      EDIT_MENU_GRAIN, EDIT_MENU_ANGLES, EDIT_MENU_SCALEBAR,
      EDIT_MENU_PREFS,
      EMODEL_MENU_HEADER, EMODEL_MENU_OFFSETS, EMODEL_MENU_CLEAN, 
      EOBJECT_MENU_NEW, EOBJECT_MENU_DELETE, EOBJECT_MENU_COLOR, 
      EOBJECT_MENU_TYPE, EOBJECT_MENU_INFO, EOBJECT_MENU_MOVE, 
      EOBJECT_MENU_CLEAN, EOBJECT_MENU_FIXZ, EOBJECT_MENU_FLATTEN,
      EOBJECT_MENU_RENUMBER, EOBJECT_MENU_COMBINE,
      ESURFACE_MENU_NEW, ESURFACE_MENU_GOTO, ESURFACE_MENU_MOVE, 
      ESURFACE_MENU_DELETE,
      ECONTOUR_MENU_NEW, ECONTOUR_MENU_DELETE, ECONTOUR_MENU_MOVE, 
      ECONTOUR_MENU_SORT, ECONTOUR_MENU_AUTO, ECONTOUR_MENU_TYPE, 
      ECONTOUR_MENU_INFO, ECONTOUR_MENU_BREAK, ECONTOUR_MENU_JOIN, 
      ECONTOUR_MENU_FIXZ, ECONTOUR_MENU_INVERT, ECONTOUR_MENU_COPY,
      ECONTOUR_MENU_LOOPBACK, ECONTOUR_MENU_FILLIN,
      EPOINT_MENU_DELETE, EPOINT_MENU_SORTZ, EPOINT_MENU_SORTDIST, 
      EPOINT_MENU_DIST, EPOINT_MENU_VALUE, EPOINT_MENU_SIZE, 
      EIMAGE_MENU_PROCESS, EIMAGE_MENU_COLORMAP, EIMAGE_MENU_RELOAD, 
      EIMAGE_MENU_FLIP, EIMAGE_MENU_FILLCACHE, EIMAGE_MENU_FILLER, 
      IMAGE_MENU_GRAPH, IMAGE_MENU_SLICER, IMAGE_MENU_LINKSLICE, IMAGE_MENU_TUMBLER, 
      IMAGE_MENU_MODV, IMAGE_MENU_ZAP, IMAGE_MENU_XYZ, IMAGE_MENU_PIXEL,
      IMAGE_MENU_LOCATOR, IMAGE_MENU_MULTIZ, IMAGE_MENU_ISOSURFACE, HELP_MENU_CONTROLS,
      HELP_MENU_MAN, HELP_MENU_MENUS, HELP_MENU_HOTKEY, HELP_MENU_ABOUT,
      LAST_MENU_ID};

class InfoWindow : public QMainWindow
{
  Q_OBJECT

 public:
  InfoWindow(QWidget * parent = 0, const char * name = 0, 
              Qt::WFlags f = Qt::Window) ;
  ~InfoWindow() {};
  void manageMenus();
  void keepOnTop(bool state);
  void setFontDependentWidths();
  void openSelectedWindows(const char *keys, int modelViewOpen);
  void setupAutoContrast();
  void setInitialHeights();
  void resizeToHeight(int newHeight);
  int getResizedHeight() {return mResizedHeight;};
  void doOrSetupMove(int x, int y);

  public slots:
  void fileSlot(int item);
  void fileWriteSlot(int item);
  void editSlot(int item);
  void editModelSlot(int item);
  void editObjectSlot(int item);
  void editSurfaceSlot(int item);
  void editContourSlot(int item);
  void editPointSlot(int item);
  void editImageSlot(int item);
  void imageSlot(int item);
  void pluginSlot(int item);
  void helpSlot(int item);
  void trimvolExited(int exitCode, QProcess::ExitStatus exitStatus);
  void trimvolError(QProcess::ProcessError error);
  void imodinfoExited(int exitCode, QProcess::ExitStatus exitStatus);
  void imodinfoError(QProcess::ProcessError error);

 protected:
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void closeEvent ( QCloseEvent * e );
    void timerEvent(QTimerEvent *e);
    void changeEvent(QEvent *e);
    bool event(QEvent *e);

 private:
  void extract();
  void objectInfo();
  QAction *mActions[LAST_MENU_ID];
  QTextEdit *mStatusEdit;
  bool mMinimized;
  int mTopTimerID;
  int mAutoTimerID;
  int mInfoTimerID;
  int mOldFontHeight;
  QProcess *mTrimvolProcess;
  QString mTrimvolOutput;
  int mTrimvolType;
  QProcess *mImodinfoProcess;
  int mResizedHeight;
  int mTargetHeight;
  int mTargetMoveX, mTargetMoveY;
};

/* GLOBAL FUNCTIONS */
void MaintainModelName(Imod *mod);
int imod_info_open();


#endif    /* IMOD_INFO_H */
