/*   imod_info.h  -  declarations for InfoWindow class implemented in
 *                      imod_info.cpp and imod_menu.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 3.4  2003/03/14 15:54:29  mast
    New function to keep window on top

    Revision 3.3  2003/02/27 19:33:51  mast
    Add function to manage menus based on current state

    Revision 3.2  2003/02/10 20:41:55  mast
    Merge Qt source

    Revision 3.1.2.5  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 3.1.2.4  2003/01/14 21:49:54  mast
    add dialog hiding/showing control

    Revision 3.1.2.3  2003/01/13 01:05:05  mast
    Qt version

    Revision 3.1.2.2  2002/12/19 04:37:13  mast
    Cleanup of unused global variables and defines

    Revision 3.1.2.1  2002/12/17 21:37:47  mast
    Move global variable declarations outside extern "C" construct

    Revision 3.1  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/
#ifndef IMOD_INFO_H
#define IMOD_INFO_H

#include <qmainwindow.h>

class InfoControls;
class QTextEdit;
class QTimer;
class QPopupMenu;
class InfoWindow;

typedef struct Mod_Model Imod;

extern InfoControls *ImodInfoWidget;
extern InfoWindow *ImodInfoWin;
extern int ImodForbidLevel;

enum {FILE_MENU_NEW, FILE_MENU_OPEN, FILE_MENU_SAVE, FILE_MENU_SAVEAS,
      FILE_MENU_TIFF, FILE_MENU_QUIT,
      FWRITE_MENU_IMOD, FWRITE_MENU_WIMP, FWRITE_MENU_NFF, FWRITE_MENU_SYNU,
      EDIT_MENU_MOVIES, 
      EMODEL_MENU_HEADER, EMODEL_MENU_OFFSETS, EMODEL_MENU_CLEAN, 
      EOBJECT_MENU_NEW, EOBJECT_MENU_DELETE, EOBJECT_MENU_COLOR, 
      EOBJECT_MENU_TYPE, EOBJECT_MENU_INFO, EOBJECT_MENU_MOVE, 
      EOBJECT_MENU_CLEAN,  
      ESURFACE_MENU_NEW, ESURFACE_MENU_GOTO, ESURFACE_MENU_MOVE, 
      ECONTOUR_MENU_NEW, ECONTOUR_MENU_DELETE, ECONTOUR_MENU_MOVE, 
      ECONTOUR_MENU_SORT, ECONTOUR_MENU_AUTO, ECONTOUR_MENU_TYPE, 
      ECONTOUR_MENU_INFO, ECONTOUR_MENU_BREAK, ECONTOUR_MENU_JOIN, 
      ECONTOUR_MENU_FIXZ, ECONTOUR_MENU_INVERT, ECONTOUR_MENU_COPY,
      ECONTOUR_MENU_LOOPBACK, ECONTOUR_MENU_FILLIN,
      EPOINT_MENU_DELETE, EPOINT_MENU_SORTZ, EPOINT_MENU_SORTDIST, 
      EPOINT_MENU_DIST, EPOINT_MENU_VALUE, EPOINT_MENU_SIZE, 
      EIMAGE_MENU_PROCESS, EIMAGE_MENU_COLORMAP, EIMAGE_MENU_RELOAD, 
      EIMAGE_MENU_FLIP, EIMAGE_MENU_FILLCACHE, EIMAGE_MENU_FILLER, 
      IMAGE_MENU_GRAPH, IMAGE_MENU_SLICER, IMAGE_MENU_TUMBLER, 
      IMAGE_MENU_MODV, IMAGE_MENU_ZAP, IMAGE_MENU_XYZ, IMAGE_MENU_PIXEL,
      HELP_MENU_MAN, HELP_MENU_MENUS, HELP_MENU_HOTKEY, HELP_MENU_ABOUT};


class InfoWindow : public QMainWindow
{
  Q_OBJECT

 public:
  InfoWindow(QWidget * parent = 0, const char * name = 0, 
              WFlags f = WType_TopLevel | WDestructiveClose) ;
  ~InfoWindow() {};
  void manageMenus();
  void keepOnTop(bool state);

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
  void hideTimeout();

 protected:
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void closeEvent ( QCloseEvent * e );
    void showEvent(QShowEvent *e);
    void hideEvent(QHideEvent *e);
    void timerEvent(QTimerEvent *e);

 private:
  QPopupMenu *mFileMenu;
  QPopupMenu *mEditMenu;
  QPopupMenu *mImageMenu;
  QPopupMenu *mHelpMenu;
  QPopupMenu *mFModelMenu;
  QPopupMenu *mFWriteMenu;
  QPopupMenu *mEModelMenu;
  QPopupMenu *mEObjectMenu;
  QPopupMenu *mESurfaceMenu;
  QPopupMenu *mEPointMenu;
  QPopupMenu *mEContourMenu;
  QPopupMenu *mEImageMenu;
  QPopupMenu *mPlugMenu;
  QTextEdit *mStatusEdit;
  QTimer *mHideTimer;
  bool mMinimized;
  int mTopTimerID;
};

/* GLOBAL FUNCTIONS */
void MaintainModelName(Imod *mod);
int imod_info_open();

#endif    /* IMOD_INFO_H */
