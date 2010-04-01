/*   imod_info.h  -  declarations for InfoWindow class implemented in
 *                      imod_info.cpp and imod_menu.cpp
 *
 *   Copyright (C) 1995-2003 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
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
      FILE_MENU_SNAPQUALITY, FILE_MENU_TIFF, FILE_MENU_EXTRACT, 
      FILE_MENU_SAVEINFO, FILE_MENU_QUIT,
      FWRITE_MENU_IMOD, FWRITE_MENU_WIMP, FWRITE_MENU_NFF, FWRITE_MENU_SYNU,
      EDIT_MENU_MOVIES, EDIT_MENU_GRAIN, EDIT_MENU_ANGLES, EDIT_MENU_SCALEBAR,
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
      IMAGE_MENU_GRAPH, IMAGE_MENU_SLICER, IMAGE_MENU_TUMBLER, 
      IMAGE_MENU_MODV, IMAGE_MENU_ZAP, IMAGE_MENU_XYZ, IMAGE_MENU_PIXEL,
      IMAGE_MENU_LOCATOR, IMAGE_MENU_MULTIZ, IMAGE_MENU_ISOSURFACE,
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
  void openSelectedWindows(char *keys);
  void setupAutoContrast();

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

 protected:
    void keyPressEvent ( QKeyEvent * e );
    void keyReleaseEvent ( QKeyEvent * e );
    void closeEvent ( QCloseEvent * e );
    void timerEvent(QTimerEvent *e);
    void fontChange( const QFont & oldFont );
    bool event(QEvent *e);

 private:
  void extract();
  QAction *mActions[LAST_MENU_ID];
  QTextEdit *mStatusEdit;
  bool mMinimized;
  int mTopTimerID;
  int mAutoTimerID;
  QProcess *mTrimvolProcess;
  QString mTrimvolOutput;
};

/* GLOBAL FUNCTIONS */
void MaintainModelName(Imod *mod);
int imod_info_open();

/*
    $Log$
    Revision 3.27  2009/01/15 16:33:17  mast
    Qt 4 port

    Revision 3.26  2008/12/10 01:04:50  mast
    Added menu item to set jpeg quality

    Revision 3.25  2008/05/27 05:50:28  mast
    New menu items and autocontrast after delay

    Revision 3.24  2008/02/22 00:33:44  sueh
    bug# 1076 Added extract menu option, and extract(), and trimvolExited().

    Revision 3.23  2008/01/25 20:22:58  mast
    Changes for new scale bar

    Revision 3.22  2008/01/13 22:58:35  mast
    Changes for multi-Z window

    Revision 3.21  2007/11/10 04:07:10  mast
    Changes for setting snapshot directory

    Revision 3.20  2007/08/13 16:04:50  mast
    Changes for locator window

    Revision 3.19  2007/07/08 16:47:00  mast
    Added object combine

    Revision 3.18  2007/05/25 05:28:16  mast
    Changes for addition of slicer angle storage

    Revision 3.17  2006/09/01 20:49:29  mast
    Added menu item to flatten contours in object

    Revision 3.16  2005/10/14 22:04:39  mast
    Changes for Model reload capability

    Revision 3.15  2005/06/26 19:37:24  mast
    Added fine-grain entries

    Revision 3.14  2004/11/20 05:05:27  mast
    Changes for undo/redo capability

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
