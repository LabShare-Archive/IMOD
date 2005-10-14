/*
 *  imod_info.cpp -- open the imod information window.  Implements the
 *                    InfoWindow class declared in imod_info.h.  The control
 *                    widget is the InfoControls class implemented in 
 *                    form_info.ui[.h].  The slots for menu actions are
 *                    part of the InfoWindow class, defined in imod_menu.cpp
 *                    The InfoControls class interacts with global functions
 *                    in imod_info_cb.cpp.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

    $Date$

    $Revision$
    Log at end of file
*/

#include "form_info.h"
#include <qmenubar.h>
#include <qpopupmenu.h>
#include <qvbox.h>
#include <qlayout.h>
#include <qtextedit.h>
#include <qframe.h>
#include <qtimer.h>
#include "imod_info.h"
#include "imod.h"
#include "imod_info_cb.h"
#include "imodplug.h"
#include "iproc.h"
#include "preferences.h"
#include "control.h"

#define INFO_MIN_LINES 3.5
#define INFO_STARTING_LINES 4.75

/* some declarations */
void wprintWidget(QTextEdit *edit);

static char *truncate_name(char *name, int limit);

/* Global variables: the window, and its controls  */

InfoWindow *ImodInfoWin;
InfoControls *ImodInfoWidget;


/*
 * THE CONSTRUCTOR FOR InfoWindow
 */
InfoWindow::InfoWindow(QWidget * parent, const char * name, WFlags f)
  : QMainWindow(parent, name, f)
{
  mMinimized = false;

  QMenuBar *menuBar = new QMenuBar(this);
  mFileMenu = new QPopupMenu();
  menuBar->insertItem("&File", mFileMenu);
  mFWriteMenu = new QPopupMenu();

  mFileMenu->insertItem("&New Model", FILE_MENU_NEW);
  mFileMenu->insertItem("&Open Model", FILE_MENU_OPEN);
  mFileMenu->insertItem("&Reload Model", FILE_MENU_RELOAD);
  mFileMenu->insertItem("&Save Model", FILE_MENU_SAVE);
  mFileMenu->setAccel(Key_S, FILE_MENU_SAVE);
  mFileMenu->insertItem("S&ave Model As...", FILE_MENU_SAVEAS);
  mFileMenu->insertItem("&Write Model As", mFWriteMenu);
  mFileMenu->insertItem("&Memory to TIF...", FILE_MENU_TIFF);
  mFileMenu->insertItem("&Quit", FILE_MENU_QUIT);

  mFWriteMenu->insertItem("&Imod", FWRITE_MENU_IMOD);
  mFWriteMenu->insertItem("&Wimp", FWRITE_MENU_WIMP);
  mFWriteMenu->insertItem("&NFF", FWRITE_MENU_NFF);
  mFWriteMenu->insertItem("&Synu", FWRITE_MENU_SYNU);

  // The edit menu, mostly submenus
  mEditMenu = new QPopupMenu();
  menuBar->insertItem("&Edit", mEditMenu);
  mEModelMenu = new QPopupMenu();
  mEObjectMenu = new QPopupMenu();
  mESurfaceMenu = new QPopupMenu();
  mEContourMenu = new QPopupMenu();
  mEPointMenu = new QPopupMenu();
  mEImageMenu = new QPopupMenu();

  mEditMenu->insertItem("&Model", mEModelMenu);
  mEditMenu->insertItem("&Object", mEObjectMenu);
  mEditMenu->insertItem("&Surface", mESurfaceMenu);
  mEditMenu->insertItem("&Contour", mEContourMenu);
  mEditMenu->insertItem("&Point", mEPointMenu);
  mEditMenu->insertItem("&Image", mEImageMenu); 
  mEditMenu->insertItem("M&ovies...", EDIT_MENU_MOVIES);
  mEditMenu->insertItem("&Fine Grain...", EDIT_MENU_GRAIN);
  mEditMenu->insertItem("Op&tions...", EDIT_MENU_PREFS);

  // Edit model submenu
  mEModelMenu->insertItem("&Header...", EMODEL_MENU_HEADER);
  mEModelMenu->insertItem("&Offsets...", EMODEL_MENU_OFFSETS);
  mEModelMenu->insertItem("&Clean", EMODEL_MENU_CLEAN);

  // Edit object submenu
  mEObjectMenu->insertItem("&New", EOBJECT_MENU_NEW);
  mEObjectMenu->insertItem("&Delete", EOBJECT_MENU_DELETE);
  mEObjectMenu->insertItem("&Type...", EOBJECT_MENU_TYPE);
  mEObjectMenu->insertItem("&Color...", EOBJECT_MENU_COLOR);
  mEObjectMenu->insertItem("&Move...", EOBJECT_MENU_MOVE);
  mEObjectMenu->insertItem("&Info", EOBJECT_MENU_INFO);
  mEObjectMenu->insertItem("C&lean", EOBJECT_MENU_CLEAN);
  mEObjectMenu->insertItem("&Break by Z", EOBJECT_MENU_FIXZ);
  mEObjectMenu->insertItem("&Renumber...", EOBJECT_MENU_RENUMBER);

  // Edit Surface submenu
  mESurfaceMenu->insertItem("&New", ESURFACE_MENU_NEW);
  mESurfaceMenu->setAccel(SHIFT + Key_N, ESURFACE_MENU_NEW);
  mESurfaceMenu->insertItem("&Go To...", ESURFACE_MENU_GOTO);
  mESurfaceMenu->insertItem("&Move...", ESURFACE_MENU_MOVE);
  mESurfaceMenu->insertItem("&Delete", ESURFACE_MENU_DELETE);

  // Edit Contour submenu
  mEContourMenu->insertItem("&New", ECONTOUR_MENU_NEW);
  mEContourMenu->setAccel(Key_N, ECONTOUR_MENU_NEW);
  mEContourMenu->insertItem("&Delete", ECONTOUR_MENU_DELETE);
  mEContourMenu->setAccel(SHIFT + Key_D, ECONTOUR_MENU_DELETE);
  mEContourMenu->insertItem("&Move...", ECONTOUR_MENU_MOVE);
  mEContourMenu->insertItem("&Copy...", ECONTOUR_MENU_COPY);
  mEContourMenu->insertItem("&Sort", ECONTOUR_MENU_SORT);
  mEContourMenu->insertSeparator();
  mEContourMenu->insertItem("&Break...", ECONTOUR_MENU_BREAK);
  mEContourMenu->insertItem("&Join...", ECONTOUR_MENU_JOIN);
  mEContourMenu->insertItem("&Break by Z", ECONTOUR_MENU_FIXZ);
  mEContourMenu->insertItem("&Fill in Z", ECONTOUR_MENU_FILLIN);
  mEContourMenu->insertItem("&Loopback", ECONTOUR_MENU_LOOPBACK);
  mEContourMenu->insertItem("&Invert", ECONTOUR_MENU_INVERT);
  mEContourMenu->insertSeparator();
  mEContourMenu->insertItem("&Info", ECONTOUR_MENU_INFO);
  mEContourMenu->insertItem("&Auto...", ECONTOUR_MENU_AUTO);
  mEContourMenu->insertItem("&Type...", ECONTOUR_MENU_TYPE);

  // Edit Point submenu
  mEPointMenu->insertItem("&Delete", EPOINT_MENU_DELETE);
  mEPointMenu->insertItem("Si&ze...", EPOINT_MENU_SIZE);
  mEPointMenu->insertItem("D&istance", EPOINT_MENU_DIST);
  mEPointMenu->insertItem("&Value", EPOINT_MENU_VALUE);
  mEPointMenu->insertItem("&Sort by Z", EPOINT_MENU_SORTZ);
  mEPointMenu->insertItem("S&ort by dist", EPOINT_MENU_SORTDIST);

  // Edit Image submenu
  mEImageMenu->insertItem("F&lip", EIMAGE_MENU_FLIP);
  mEImageMenu->insertItem("P&rocess...", EIMAGE_MENU_PROCESS);
  /*  mEImageMenu->insertItem("C&olormap...", EIMAGE_MENU_COLORMAP);
      mEImageMenu->setItemEnabled(EIMAGE_MENU_COLORMAP, App->rgba == 0 && 
      App->depth > 8); */
  mEImageMenu->insertItem("R&eload...", EIMAGE_MENU_RELOAD);
  mEImageMenu->insertItem("F&ill Cache", EIMAGE_MENU_FILLCACHE);
  mEImageMenu->insertItem("C&ache Filler...", EIMAGE_MENU_FILLER);

  // The image menu
  mImageMenu = new QPopupMenu();
  menuBar->insertItem("&Image", mImageMenu);

  mImageMenu->insertItem("&ZaP", IMAGE_MENU_ZAP);
  mImageMenu->setAccel(Key_Z, IMAGE_MENU_ZAP);
  mImageMenu->insertItem("&XYZ", IMAGE_MENU_XYZ);
  mImageMenu->insertItem("&Slicer", IMAGE_MENU_SLICER);
  mImageMenu->setAccel(Key_Backslash, IMAGE_MENU_SLICER);
  mImageMenu->insertItem("&Model View", IMAGE_MENU_MODV);
  mImageMenu->setAccel(Key_V, IMAGE_MENU_MODV);
  mImageMenu->insertItem("&Pixel View", IMAGE_MENU_PIXEL);
  mImageMenu->insertItem("&Graph", IMAGE_MENU_GRAPH);
  mImageMenu->setAccel(SHIFT + Key_G, IMAGE_MENU_GRAPH);
  mImageMenu->insertItem("&Tumbler", IMAGE_MENU_TUMBLER);

  // plugin menu
  int plugs   = imodPlugLoaded(IMOD_PLUG_MENU);
  if (plugs) {
    mPlugMenu = new QPopupMenu();
    menuBar->insertItem("&Special", mPlugMenu);
    imodPlugMenu(mPlugMenu); /* Install all menu plugs */
    connect(mPlugMenu, SIGNAL(activated(int)), this, SLOT(pluginSlot(int)));
  }

  // Help menu
  mHelpMenu = new QPopupMenu();
  menuBar->insertSeparator();
  menuBar->insertItem("&Help", mHelpMenu);

  mHelpMenu->insertItem("&Man Page", HELP_MENU_MAN);
  mHelpMenu->insertItem("&Menus", HELP_MENU_MENUS);
  mHelpMenu->insertItem("&Hot Keys", HELP_MENU_HOTKEY);
  mHelpMenu->insertSeparator();
  mHelpMenu->insertItem("&About", HELP_MENU_ABOUT);

  // Make the connections
  connect(mFileMenu, SIGNAL(activated(int)), this, SLOT(fileSlot(int)));
  connect(mEditMenu, SIGNAL(activated(int)), this, SLOT(editSlot(int)));
  connect(mImageMenu, SIGNAL(activated(int)), this, SLOT(imageSlot(int)));
  connect(mHelpMenu, SIGNAL(activated(int)), this, SLOT(helpSlot(int)));
  connect(mFWriteMenu, SIGNAL(activated(int)), this, SLOT(fileWriteSlot(int)));
  connect(mEModelMenu, SIGNAL(activated(int)), this, SLOT(editModelSlot(int)));
  connect(mEObjectMenu, SIGNAL(activated(int)), this,
	  SLOT(editObjectSlot(int)));
  connect(mESurfaceMenu, SIGNAL(activated(int)), this,
	  SLOT(editSurfaceSlot(int)));
  connect(mEContourMenu, SIGNAL(activated(int)), this,
	  SLOT(editContourSlot(int)));
  connect(mEPointMenu, SIGNAL(activated(int)), this, SLOT(editPointSlot(int)));
  connect(mEImageMenu, SIGNAL(activated(int)), this, SLOT(editImageSlot(int)));


  // Now make a central VBox for the control widget and text edit
  // Put widget in a sunken frame because menu bar is flat
  QVBox *central = new QVBox(this);
  QFrame *frame = new QFrame(central);
  frame->setFrameStyle(QFrame::Sunken | QFrame::StyledPanel);
  QVBoxLayout *layout = new QVBoxLayout(frame, 5);
  ImodInfoWidget = new InfoControls(frame);
  layout->addWidget(ImodInfoWidget);
  setCentralWidget(central);

  // Get the status window 
  mStatusEdit = new QTextEdit(central);
  setFontDependentWidths();

  setFocusPolicy(StrongFocus);
  mStatusEdit->setFocusPolicy(NoFocus);
  wprintWidget(mStatusEdit);

  setIcon(*(App->iconPixmap));
}

void InfoWindow::setFontDependentWidths()
{
  // fix the minimum size of the status window
  // set its starting size by adjusting the window size
  mStatusEdit->setMinimumHeight((int)(INFO_MIN_LINES * 
				      fontMetrics().height()));
  // This is needed to get the right hint out of the info widget
  imod_info_input();

  // Fix the minimum size of the ImodInfoWidget
  QSize hint = ImodInfoWidget->sizeHint();
  ImodInfoWidget->setMinimumHeight(hint.height());
  
  QSize editHint = mStatusEdit->sizeHint();
  hint = sizeHint();
  resize(hint.width(), hint.height() - editHint.height() +
	 (int)(INFO_STARTING_LINES * fontMetrics().height()));
}

void InfoWindow::fontChange( const QFont & oldFont )
{
  ImodInfoWidget->setFontDependentWidths();
  setFontDependentWidths();
}

void InfoWindow::pluginSlot(int item)
{
  imodPlugOpen(item);
}

// Key events: keep track of the control key; pass on keys (except escape!)
void InfoWindow::keyPressEvent ( QKeyEvent * e )
{
  if (e->key() == hotSliderKey() && hotSliderFlag() != NO_HOT_SLIDER) {
    imodInfoCtrlPress(1);
    grabKeyboard();
  }
  if (e->key() != Qt::Key_Escape)
    ivwControlKey(0, e);
}

void InfoWindow::keyReleaseEvent ( QKeyEvent * e )
{
  if (e->key() == hotSliderKey()) {
    imodInfoCtrlPress(0);
    releaseKeyboard();
  }
  ivwControlKey(1, e);
}

void InfoWindow::closeEvent ( QCloseEvent * e )
{
  imod_quit();
  e->ignore();
}

// Manage the hiding and showing of other windows when this
// window is minimized or brought back
// hideEvent override does not occur on Mac, and the isMinimized() function
// is not reliable in RH 7.3/ Qt 3.0.5
// Watch for both event types in each case due to further X11/Mac differences
bool InfoWindow::event(QEvent *e)
{
  //imodPrintStderr("event type %d\n", e->type());
  if (Imod_debug && e->type() == QEvent::ShowMinimized)
    imodPrintStderr("ShowMinimized\n");
  if (Imod_debug && e->type() == QEvent::Hide)
    imodPrintStderr("Hide\n");
  if (Imod_debug && e->type() == QEvent::ShowNormal)
    imodPrintStderr("ShowNormal\n");
  if (Imod_debug && e->type() == QEvent::Show)
    imodPrintStderr("Show\n");
  if ((e->type() == QEvent::ShowMinimized || e->type() == QEvent::Hide) &&
      !mMinimized) {
    if (Imod_debug)
      imodPuts("minimizing");
    mMinimized = true;
    imodDialogManager.hide();
  } else if ((e->type() == QEvent::ShowNormal || e->type() == QEvent::Show)
             && mMinimized) {
    if (Imod_debug)
      imodPuts("maximizing");
    mMinimized = false;
    imodDialogManager.show();
  }
  return QWidget::event(e);
}

// Enable menu items based on new information
void InfoWindow::manageMenus()
{
  bool imageOK = !(App->cvi->fakeImage || App->cvi->rawImageStore);
  mFileMenu->setItemEnabled(FILE_MENU_TIFF, App->cvi->rawImageStore != 0);
  mEImageMenu->setItemEnabled(EIMAGE_MENU_FILLCACHE, 
			      App->cvi->vmSize != 0 || App->cvi->nt > 0);
  mEImageMenu->setItemEnabled(EIMAGE_MENU_FILLER, 
			      App->cvi->vmSize != 0 || App->cvi->nt > 0);
  mImageMenu->setItemEnabled(IMAGE_MENU_SLICER, App->cvi->rawImageStore == 0);
  mImageMenu->setItemEnabled(IMAGE_MENU_XYZ, App->cvi->rawImageStore == 0);
  if (!imageOK) {
    ImodInfoWidget->setFloat(-1);
    mEImageMenu->setItemEnabled(EIMAGE_MENU_PROCESS, false);
    mImageMenu->setItemEnabled(IMAGE_MENU_GRAPH, false);
    mImageMenu->setItemEnabled(IMAGE_MENU_TUMBLER, false);
    mImageMenu->setItemEnabled(IMAGE_MENU_PIXEL, false);
  }

  // These are run-time items.  If more instances appear this should be
  // split into initial and runtime calls
  mEImageMenu->setItemEnabled(EIMAGE_MENU_FLIP, !iprocBusy());
  mEImageMenu->setItemEnabled(EIMAGE_MENU_RELOAD, !iprocBusy() && imageOK);
  mFileMenu->setItemEnabled(FILE_MENU_RELOAD, App->cvi->reloadable != 0);
}


// Change the keep on top flag - have to reparent the widget for it to work
void InfoWindow::keepOnTop(bool state)
{
#ifdef STAY_ON_TOP_HACK
  if (mTopTimerID)
    killTimer(mTopTimerID);
  if (state)
    mTopTimerID = startTimer(200);
#else
  int flags = getWFlags();
  if (state)
    flags |= WStyle_StaysOnTop;
  else
    flags ^= WStyle_StaysOnTop;
  QPoint p(geometry().x(), geometry().y());
  // Using pos() jumps on Windows
  // Also, pos() jumps up-left on Unix, geometry() jumps down-right
  // Unless we access the pos !
  QPoint p2 = pos();
  /* imodPrintStderr("before geom %d %d  pos %d %d\n", geometry().x(), 
     geometry().y(), pos().x(), pos().y()); */
  reparent(0, flags, p, true);  
  /* imodPrintStderr("after geom %d %d  pos %d %d\n", geometry().x(), 
     geometry().y(), pos().x(), pos().y()); */
#endif
}

void InfoWindow::timerEvent(QTimerEvent *e)
{
  raise();
}

void InfoWindow::openSelectedWindows(char *keys)
{
  bool imageOK = !(App->cvi->fakeImage || App->cvi->rawImageStore);
  if (!keys)
    return;
  if (strchr(keys, 'G'))
    editSlot(EDIT_MENU_GRAIN);
  if (strchr(keys, 'n'))
    editSlot(EDIT_MENU_MOVIES);
  if (strchr(keys, 't'))
    editObjectSlot(EOBJECT_MENU_TYPE);
  if (strchr(keys, 'l'))
    editObjectSlot(EOBJECT_MENU_COLOR);
  if (strchr(keys, 'h'))
    editModelSlot(EMODEL_MENU_HEADER);
  if (strchr(keys, 'o'))
    editModelSlot(EMODEL_MENU_OFFSETS);
  if (strchr(keys, 's'))
    editSurfaceSlot(ESURFACE_MENU_GOTO);
  if (strchr(keys, 'v'))
    editContourSlot(ECONTOUR_MENU_MOVE);
  if (strchr(keys, 'a'))
    editContourSlot(ECONTOUR_MENU_AUTO);
  if (strchr(keys, 'b'))
    editContourSlot(ECONTOUR_MENU_BREAK);
  if (strchr(keys, 'j'))
    editContourSlot(ECONTOUR_MENU_JOIN);
  if (strchr(keys, 'c'))
    editContourSlot(ECONTOUR_MENU_COPY);
  if (strchr(keys, 'p') && imageOK)
    editImageSlot(EIMAGE_MENU_PROCESS);
  if (strchr(keys, 'r') && imageOK)
    editImageSlot(EIMAGE_MENU_RELOAD);
  if (strchr(keys, 'f') && (App->cvi->vmSize != 0 || App->cvi->nt > 0))
    editImageSlot(EIMAGE_MENU_FILLER);
  if (strchr(keys, 'g') && imageOK)
    imageSlot(IMAGE_MENU_GRAPH);
  if (strchr(keys, 'u') && imageOK)
    imageSlot(IMAGE_MENU_TUMBLER);
  if (strchr(keys, 'x') && imageOK)
    imageSlot(IMAGE_MENU_PIXEL);
  if (strchr(keys, 'T'))
    imodPlugOpenByName("Line Track");
  if (strchr(keys, 'F') && imageOK)
    imodPlugOpenByName("Bead Fixer");
}


/* 1/12/03: removed unused imod_info_force, imod_info_pending and 
   imod_info_dispatch; moved imod_info_input to imof_info_cb */
/* DNM 12/6/02: removed unused imod_xinit */

/*
 * Open the info window
 */
int imod_info_open()
{
  char *filename;

  // Open the window
  ImodInfoWin = new InfoWindow(NULL, "info window");

  // Initialize the caption and image line
  ImodInfoWin->setCaption(imodCaption("3dmod"));

  filename = imodwfname("Image:");
  filename = truncate_name(filename, 23);
  if (filename) {
    ImodInfoWidget->setImageName(filename);
    free(filename);
  } else
    ImodInfoWidget->setImageName(" ");

  // Initialize the model line (could we use MaintainModelName?)
  filename = imodwGivenName(" ", Imod_filename);
  filename = truncate_name(filename, 23);
  if (filename) {
    ImodInfoWidget->setModelName(filename);
    free(filename);
  } else 
    ImodInfoWidget->setModelName(" ");
    
  // Try not having these
  /*  imod_info_forbid();
  imod_info_input();
  imod_info_enable(); */

  ImodInfoWin->show();

  return(0);
}


/* Redisplay the model filename after it's changed */

void MaintainModelName(Imod *mod)
{
  char *filestr;
  int namelen;
     
  filestr = imodwGivenName(" ", Imod_filename);
  filestr = truncate_name(filestr, 23);
  if(!filestr) {
    filestr = (char *) malloc(1);
    if (filestr)
      *filestr = 0x00;
  }

  if (filestr) {
    ImodInfoWidget->setModelName(filestr);
    free(filestr);
  }

  /* Copy filename into model structure */
  if (mod->fileName)
    free(mod->fileName);
  namelen = strlen(Imod_filename)+1;
  mod->fileName = (char *)malloc(namelen);
  if (mod->fileName)
    memcpy(mod->fileName, Imod_filename, namelen);

  // This is common path for most changes of reloadable flag
  ImodInfoWin->manageMenus();
}

static char *truncate_name(char *name, int limit)
{
  int len, i;
  char *newName;
  if(!name)
    return(name);
  len = strlen(name);
  if (len <= limit)
    return(name);
  newName = (char *) malloc(limit + 3);
  for (i = 0; i < limit - 1; i++)
    newName[i] = name[i];
  newName[limit - 1] = '.';
  newName[limit] = '.';
  newName[limit + 1] = '.';
  newName[limit + 2] = 0x00;
  free(name);
  return(newName);
}

/*
    $Log$
    Revision 4.32  2005/09/15 14:20:22  mast
    Added image movie window

    Revision 4.31  2005/06/26 19:37:06  mast
    cleanup

    Revision 4.30  2005/02/12 01:29:33  mast
    Prevented image processing and reload from being opened from menus when
    there is no image data; prevented these and other windows from being
    opened with -E when inappropriate

    Revision 4.29  2004/11/21 05:59:42  mast
    Rationalized key letters for opening windows

    Revision 4.28  2004/11/20 05:05:27  mast
    Changes for undo/redo capability

    Revision 4.27  2004/11/07 23:04:20  mast
    Disabled flip and reload when processing

    Revision 4.26  2004/11/01 23:25:13  mast
    Added delete surface menu entry

    Revision 4.25  2004/09/21 20:17:54  mast
    Added menu option to renumber object

    Revision 4.24  2004/05/31 23:35:26  mast
    Switched to new standard error functions for all debug and user output

    Revision 4.23  2003/11/26 18:17:03  mast
    Fix test for whether to open zap and model view with raw images

    Revision 4.22  2003/09/18 05:57:08  mast
    Disable float button later in process to exclude RGB images

    Revision 4.21  2003/08/01 00:13:56  mast
    Make event reports happen in debug mode

    Revision 4.20  2003/06/19 05:48:35  mast
    Added object break by Z

    Revision 4.19  2003/05/18 22:59:28  mast
    Remove icon include file

    Revision 4.18  2003/05/18 22:08:48  mast
    Changes to add an application icon

    Revision 4.17  2003/04/25 03:28:32  mast
    Changes for name change to 3dmod

    Revision 4.16  2003/04/23 17:50:44  mast
    no longer need to watch windowActivate event due to fix on Mac

    Revision 4.15  2003/04/16 18:46:51  mast
    hide/show changes

    Revision 4.14  2003/04/11 22:30:29  mast
    return value from new event watcher

    Revision 4.13  2003/04/11 18:56:34  mast
    switch to watching event types to manage hide/show events

    Revision 4.12  2003/03/28 23:51:10  mast
    changes for Mac problems

    Revision 4.11  2003/03/26 23:23:15  mast
    switched from hotslider.h to preferences.h

    Revision 4.10  2003/03/26 17:15:30  mast
    Adjust sizes for font changes

    Revision 4.9  2003/03/24 17:58:09  mast
    Changes for new preferences capability

    Revision 4.8  2003/03/18 19:30:23  mast
    Add a timer hack to keep window on top

    Revision 4.7  2003/03/15 00:29:40  mast
    disable peg on the SGI

    Revision 4.6  2003/03/14 21:28:09  mast
    Making the reparent not move the window in some unix cases

    Revision 4.5  2003/03/14 17:27:53  mast
    Getting stays on top to work under Windows

    Revision 4.4  2003/03/14 15:54:00  mast
    Adding new function to keep window on top

    Revision 4.3  2003/03/03 22:19:53  mast
    Added function for enabling menu items after menu creation, reorganized
    menus

    Revision 4.2  2003/02/27 19:33:01  mast
    fixing window size for windows

    Revision 4.1  2003/02/10 20:29:00  mast
    autox.cpp

    Revision 1.1.2.4  2003/01/23 20:02:15  mast
    rearrange contour menu

    Revision 1.1.2.3  2003/01/18 01:15:00  mast
    fix enabling of cache filler menu items

    Revision 1.1.2.2  2003/01/14 21:49:06  mast
    Initialize hiding timer properly

    Revision 1.1.2.1  2003/01/13 01:00:08  mast
    Qt version

    Revision 3.2.2.2  2002/12/19 04:37:12  mast
    Cleanup of unused global variables and defines

    Revision 3.2.2.1  2002/12/07 01:23:44  mast
    changed calls to get model name string

    Revision 3.2  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

    Revision 3.1  2001/12/17 18:45:19  mast
    Added menu entries for cache filling

*/
