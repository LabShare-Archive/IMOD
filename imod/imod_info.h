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
    Log at end of file
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
      EDIT_MENU_MOVIES, EDIT_MENU_PREFS,
      EMODEL_MENU_HEADER, EMODEL_MENU_OFFSETS, EMODEL_MENU_CLEAN, 
      EOBJECT_MENU_NEW, EOBJECT_MENU_DELETE, EOBJECT_MENU_COLOR, 
      EOBJECT_MENU_TYPE, EOBJECT_MENU_INFO, EOBJECT_MENU_MOVE, 
      EOBJECT_MENU_CLEAN, EOBJECT_MENU_FIXZ, EOBJECT_MENU_RENUMBER,
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
  void setFontDependentWidths();
  void openSelectedWindows(char *keys);

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

 protected:
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void closeEvent ( QCloseEvent * e );
    void timerEvent(QTimerEvent *e);
    void fontChange( const QFont & oldFont );
    bool event(QEvent *e);

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
  bool mMinimized;
  int mTopTimerID;
};

/* GLOBAL FUNCTIONS */
void MaintainModelName(Imod *mod);
int imod_info_open();

/*
    $Log$
    Revision 3.13  2004/11/01 23:25:13  mast
    Added delete surface menu entry

    Revision 3.12  2004/09/21 20:17:11  mast
    Added menu option to renumber object

    Revision 3.11  2003/06/19 05:48:55  mast
    Added object break by Z

    Revision 3.10  2003/04/16 18:46:51  mast
    hide/show changes

    Revision 3.9  2003/04/11 18:56:34  mast
    switch to watching event types to manage hide/show events

    Revision 3.8  2003/03/28 23:51:10  mast
    changes for Mac problems

    Revision 3.7  2003/03/26 17:15:31  mast
    Adjust sizes for font changes

    Revision 3.6  2003/03/24 17:58:09  mast
    Changes for new preferences capability

    Revision 3.5  2003/03/18 19:30:36  mast
    Add timer to keep window on top

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

#endif    /* IMOD_INFO_H */
