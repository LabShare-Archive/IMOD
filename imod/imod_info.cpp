/*  IMOD VERSION 2.50
 *
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
 */

/*****************************************************************************
 *   Copyright (C) 1995-2001 by Boulder Laboratory for 3-Dimensional Fine    *
 *   Structure ("BL3DFS") and the Regents of the University of Colorado.     *
 *                                                                           *
 *   BL3DFS reserves the exclusive rights of preparing derivative works,     *
 *   distributing copies for sale, lease or lending and displaying this      *
 *   software and documentation.                                             *
 *   Users may reproduce the software and documentation as long as the       *
 *   copyright notice and other notices are preserved.                       *
 *   Neither the software nor the documentation may be distributed for       *
 *   profit, either in original form or in derivative works.                 *
 *                                                                           *
 *   THIS SOFTWARE AND/OR DOCUMENTATION IS PROVIDED WITH NO WARRANTY,        *
 *   EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION, WARRANTY OF          *
 *   MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.       *
 *                                                                           *
 *   This work is supported by NIH biotechnology grant #RR00592,             *
 *   for the Boulder Laboratory for 3-Dimensional Fine Structure.            *
 *   University of Colorado, MCDB Box 347, Boulder, CO 80309                 *
 *****************************************************************************/

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
#include "hotslider.h"
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
  mEditMenu->insertItem("Movies...", EDIT_MENU_MOVIES);

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

  // Edit Surface submenu
  mESurfaceMenu->insertItem("&New", ESURFACE_MENU_NEW);
  mESurfaceMenu->setAccel(SHIFT + Key_N, ESURFACE_MENU_NEW);
  mESurfaceMenu->insertItem("&Go To...", ESURFACE_MENU_GOTO);
  mESurfaceMenu->insertItem("&Move...", ESURFACE_MENU_MOVE);

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

  if (App->cvi->fakeImage)
    ImodInfoWidget->setFloat(-1);
  

  // Get the status window and fix its minimum size; 
  // set its starting size by adjusting the window size
  mStatusEdit = new QTextEdit(central);
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
  
  setFocusPolicy(StrongFocus);
  mStatusEdit->setFocusPolicy(NoFocus);
  wprintWidget(mStatusEdit);

  mHideTimer = new QTimer(this, "imod info hide timer");
  connect(mHideTimer, SIGNAL(timeout()), this, SLOT(hideTimeout()));
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

// Routines to manage the hiding and showing of other windows when this
// window is minimized or brought back
void InfoWindow::showEvent(QShowEvent *e)
{
  if (mMinimized) {
     imodDialogManager.show();
  }
  mMinimized = false;
}

// For a hide event, hide dialogs if the window is minimized; but 
// if not, do a one-shot timer to check again; workaround to bug 
// in RH  7.3/ Qt 3.0.5
void InfoWindow::hideEvent(QHideEvent *e)
{
  hideTimeout();
  if (!mMinimized)
    mHideTimer->start(1, true);
}

void InfoWindow::hideTimeout()
{
  if (isMinimized()) {
    mMinimized = true;
    imodDialogManager.hide();
  }
}

// Enable menu items based on new information
void InfoWindow::manageMenus()
{
  mFileMenu->setItemEnabled(FILE_MENU_TIFF, App->cvi->rawImageStore != 0);
  mEImageMenu->setItemEnabled(EIMAGE_MENU_FILLCACHE, 
			      App->cvi->vmSize != 0 || App->cvi->nt > 0);
  mEImageMenu->setItemEnabled(EIMAGE_MENU_FILLER, 
			      App->cvi->vmSize != 0 || App->cvi->nt > 0);
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
  ImodInfoWin->setCaption(imodCaption("Imod"));

  filename = imodwfname("Image:");
  filename = truncate_name(filename, 23);
  if (filename) {
    ImodInfoWidget->setImageName(filename);
    free(filename);
  } else
    ImodInfoWidget->setImageName(" ");

  // Initialize the model line (could we use MaintainModelName?)
  filename = imodwGivenName("Model:", Imod_filename);
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
     
  filestr = imodwGivenName("Model:", Imod_filename);
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
