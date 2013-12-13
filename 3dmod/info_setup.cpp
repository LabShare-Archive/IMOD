/*
 *  info_setup.cpp -- open the imod information window.  Implements the
 *                    InfoWindow class declared in info_setup.h.  The control
 *                    widget is the InfoControls class implemented in 
 *                    form_info.ui[.h].  The slots for menu actions are
 *                    part of the InfoWindow class, defined in info_menu.cpp
 *                    The InfoControls class interacts with global functions
 *                    in info_cb.cpp.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include "form_info.h"
#include <qmenubar.h>
#include <qmenu.h>
#include <qdir.h>
#include <qaction.h>
#include <qsignalmapper.h>
#include <qlayout.h>
#include <qtextedit.h>
#include <qframe.h>
#include <qtimer.h>
#include <qstringlist.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <QKeyEvent>
#include <QEvent>
#include <QVBoxLayout>
#include <QCloseEvent>
#include "info_setup.h"
#include "imod.h"
#include "imod_io.h"
#include "info_cb.h"
#include "mv_objed.h"
#include "imodplug.h"
#include "iproc.h"
#include "preferences.h"
#include "control.h"
#include "xzap.h"

#define INFO_MIN_LINES 4.0
#define INFO_STARTING_LINES 4.75

#define ADD_ACTION(a, b, c) mActions[c] = a##Menu->addAction(b); \
connect(mActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(mActions[c], c);

#define ADD_ACTION_KEY(a, b, c, d) mActions[c] = a##Menu->addAction(b); \
mActions[c]->setShortcut(QKeySequence(d)); \
connect(mActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(mActions[c], c);

/* some declarations */

static char *truncate_name(char *name, int limit);

/* Global variables: the window, and its controls  */

InfoWindow *ImodInfoWin = NULL;
InfoControls *ImodInfoWidget = NULL;


/*
 * THE CONSTRUCTOR FOR InfoWindow
 */
InfoWindow::InfoWindow(QWidget * parent, const char * name, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  mMinimized = false;
  mAutoTimerID = 0;
  mTopTimerID = 0;
  mInfoTimerID = 0;
  mTrimvolProcess = NULL;
  mImodinfoProcess = NULL;
  mTargetMoveX = mTargetMoveY = -1;
  setAttribute(Qt::WA_DeleteOnClose);
  setAttribute(Qt::WA_AlwaysShowToolTips);
  
  QSignalMapper *fileMapper = new QSignalMapper(this);
  QSignalMapper *editMapper = new QSignalMapper(this);
  QSignalMapper *imageMapper = new QSignalMapper(this);
  QSignalMapper *helpMapper = new QSignalMapper(this);
  QSignalMapper *fWriteMapper = new QSignalMapper(this);
  QSignalMapper *eModelMapper = new QSignalMapper(this);
  QSignalMapper *eObjectMapper = new QSignalMapper(this);
  QSignalMapper *eSurfaceMapper = new QSignalMapper(this);
  QSignalMapper *ePointMapper = new QSignalMapper(this);
  QSignalMapper *eContourMapper = new QSignalMapper(this);
  QSignalMapper *eImageMapper = new QSignalMapper(this);

  // File menu
  QMenu *fileMenu = menuBar()->addMenu("&File");
  ADD_ACTION(file, "&New Model", FILE_MENU_NEW);
  ADD_ACTION(file, "&Open Model", FILE_MENU_OPEN);
  ADD_ACTION(file, "&Reload Model", FILE_MENU_RELOAD);
  ADD_ACTION_KEY(file, "&Save Model", FILE_MENU_SAVE, Qt::Key_S);
  ADD_ACTION(file, "S&ave Model As...", FILE_MENU_SAVEAS);
  QMenu *fWriteMenu = fileMenu->addMenu("&Write Model As");
  ADD_ACTION(file,  "&Movie/Montage...", FILE_MENU_MOVIEMONT);
  ADD_ACTION(file, "S&et Snap Dir...", FILE_MENU_SNAPDIR);
  ADD_ACTION(file, "&Gray TIF Snaps", FILE_MENU_SNAPGRAY);
  mActions[FILE_MENU_SNAPGRAY]->setCheckable(true);
  //ADD_ACTION(file, "&JPEG Quality...", FILE_MENU_SNAPQUALITY);
  ADD_ACTION(file, "Memory to &TIF...", FILE_MENU_TIFF);
  ADD_ACTION(file, "E&xtract File...", FILE_MENU_EXTRACT);
  ADD_ACTION(file, "Save &Info Text...", FILE_MENU_SAVEINFO);
  ADD_ACTION(file, "&Quit", FILE_MENU_QUIT);

  ADD_ACTION(fWrite, "&Imod", FWRITE_MENU_IMOD);
  ADD_ACTION(fWrite, "&Wimp", FWRITE_MENU_WIMP);
  ADD_ACTION(fWrite, "&NFF", FWRITE_MENU_NFF);
  ADD_ACTION(fWrite, "&Synu", FWRITE_MENU_SYNU);

  // The edit menu, mostly submenus
  QMenu *editMenu = menuBar()->addMenu("&Edit");
  QMenu *eModelMenu = editMenu->addMenu("&Model");  
  QMenu *eObjectMenu = editMenu->addMenu("&Object"); 
  QMenu *eSurfaceMenu = editMenu->addMenu("&Surface");
  QMenu *eContourMenu = editMenu->addMenu("&Contour");
  QMenu *ePointMenu = editMenu->addMenu("&Point");  
  QMenu *eImageMenu = editMenu->addMenu("&Image");
  //ADD_ACTION(edit, "M&ovies...", EDIT_MENU_MOVIES);
  ADD_ACTION(edit, "&Fine Grain...", EDIT_MENU_GRAIN);
  ADD_ACTION(edit, "&Angles...", EDIT_MENU_ANGLES);
  ADD_ACTION(edit, "Scale &Bar...", EDIT_MENU_SCALEBAR);
  ADD_ACTION(edit, "Op&tions...", EDIT_MENU_PREFS);

  // Edit model submenu
  ADD_ACTION(eModel, "&Header...", EMODEL_MENU_HEADER);
  ADD_ACTION(eModel, "&Offsets...", EMODEL_MENU_OFFSETS);
  ADD_ACTION(eModel, "&Clean", EMODEL_MENU_CLEAN);

  // Edit object submenu
  ADD_ACTION(eObject, "&New", EOBJECT_MENU_NEW);
  ADD_ACTION(eObject, "&Delete", EOBJECT_MENU_DELETE);
  ADD_ACTION(eObject, "&Type...", EOBJECT_MENU_TYPE);
  ADD_ACTION(eObject, "&Color...", EOBJECT_MENU_COLOR);
  ADD_ACTION(eObject, "&Move...", EOBJECT_MENU_MOVE);
  ADD_ACTION(eObject, "C&ombine", EOBJECT_MENU_COMBINE);
  ADD_ACTION(eObject, "&Info", EOBJECT_MENU_INFO);
  ADD_ACTION(eObject, "C&lean", EOBJECT_MENU_CLEAN);
  ADD_ACTION(eObject, "&Break by Z", EOBJECT_MENU_FIXZ);
  ADD_ACTION(eObject, "&Flatten", EOBJECT_MENU_FLATTEN);
  ADD_ACTION(eObject, "&Renumber...", EOBJECT_MENU_RENUMBER);

  // Edit Surface submenu
  ADD_ACTION_KEY(eSurface, "&New", ESURFACE_MENU_NEW, Qt::SHIFT + Qt::Key_N);
  ADD_ACTION(eSurface, "&Go To...", ESURFACE_MENU_GOTO);
  ADD_ACTION(eSurface, "&Move...", ESURFACE_MENU_MOVE);
  ADD_ACTION(eSurface, "&Delete", ESURFACE_MENU_DELETE);

  // Edit Contour submenu
  ADD_ACTION_KEY(eContour, "&New", ECONTOUR_MENU_NEW, Qt::Key_N);
  ADD_ACTION_KEY(eContour, "&Delete", ECONTOUR_MENU_DELETE, 
                 Qt::SHIFT + Qt::Key_D);
  ADD_ACTION(eContour, "&Move...", ECONTOUR_MENU_MOVE);
  ADD_ACTION(eContour, "&Copy...", ECONTOUR_MENU_COPY);
  ADD_ACTION(eContour, "&Sort", ECONTOUR_MENU_SORT);
  eContourMenu->addSeparator();
  ADD_ACTION(eContour, "&Break...", ECONTOUR_MENU_BREAK);
  ADD_ACTION(eContour, "&Join...", ECONTOUR_MENU_JOIN);
  ADD_ACTION(eContour, "&Break by Z", ECONTOUR_MENU_FIXZ);
  ADD_ACTION(eContour, "&Fill in Z", ECONTOUR_MENU_FILLIN);
  ADD_ACTION(eContour, "&Loopback", ECONTOUR_MENU_LOOPBACK);
  ADD_ACTION(eContour, "&Invert", ECONTOUR_MENU_INVERT);
  eContourMenu->addSeparator();
  ADD_ACTION(eContour, "&Info", ECONTOUR_MENU_INFO);
  ADD_ACTION(eContour, "&Auto...", ECONTOUR_MENU_AUTO);
  ADD_ACTION(eContour, "&Type...", ECONTOUR_MENU_TYPE);

  // Edit Point submenu
  ADD_ACTION(ePoint, "&Delete", EPOINT_MENU_DELETE);
  ADD_ACTION(ePoint, "Si&ze...", EPOINT_MENU_SIZE);
  ADD_ACTION(ePoint, "D&istance", EPOINT_MENU_DIST);
  ADD_ACTION(ePoint, "&Value", EPOINT_MENU_VALUE);
  ADD_ACTION(ePoint, "&Sort by Z", EPOINT_MENU_SORTZ);
  ADD_ACTION(ePoint, "S&ort by dist", EPOINT_MENU_SORTDIST);

  // Edit Image submenu
  ADD_ACTION(eImage, "F&lip/Rotate", EIMAGE_MENU_FLIP);
  ADD_ACTION(eImage, "P&rocess...", EIMAGE_MENU_PROCESS);
  ADD_ACTION(eImage, "R&eload...", EIMAGE_MENU_RELOAD);
  ADD_ACTION(eImage, "F&ill Cache", EIMAGE_MENU_FILLCACHE);
  ADD_ACTION(eImage, "C&ache Filler...", EIMAGE_MENU_FILLER);

  // The image menu
  QMenu *imageMenu = menuBar()->addMenu("&Image");
  ADD_ACTION_KEY(image, "&ZaP", IMAGE_MENU_ZAP,  Qt::Key_Z);
  ADD_ACTION(image, "Multi-&Z", IMAGE_MENU_MULTIZ);
  ADD_ACTION(image, "&XYZ", IMAGE_MENU_XYZ);
  ADD_ACTION_KEY(image, "&Slicer", IMAGE_MENU_SLICER, Qt::Key_Backslash);
  ADD_ACTION_KEY(image, "&Model View", IMAGE_MENU_MODV, Qt::Key_V);
  ADD_ACTION(image, "&Pixel View", IMAGE_MENU_PIXEL);
  ADD_ACTION_KEY(image, "&Graph", IMAGE_MENU_GRAPH, Qt::SHIFT + Qt::Key_G);
  ADD_ACTION(image, "&Locator", IMAGE_MENU_LOCATOR);
  ADD_ACTION_KEY(image, "&Isosurface", IMAGE_MENU_ISOSURFACE, 
                 Qt::SHIFT + Qt::Key_U);
  ADD_ACTION(image, "&Tumbler", IMAGE_MENU_TUMBLER);
  
  // plugin menu
  int plugs   = imodPlugLoaded(IMOD_PLUG_MENU);
  if (plugs) {
    QMenu *plugMenu = menuBar()->addMenu("&Special");
    QSignalMapper *plugMapper = new QSignalMapper(this);
    imodPlugMenu(plugMenu, plugMapper); /* Install all menu plugs */
    connect(plugMapper, SIGNAL(mapped(int)), this, SLOT(pluginSlot(int)));
  }
  
  // Help menu
  menuBar()->addSeparator();
  QMenu *helpMenu = menuBar()->addMenu("&Help");
  ADD_ACTION(help, "&Menus", HELP_MENU_MENUS);
  ADD_ACTION(help, "&Controls", HELP_MENU_CONTROLS);
  helpMenu->addSeparator();
  ADD_ACTION(help, "&Hot Keys", HELP_MENU_HOTKEY);
  ADD_ACTION(help, "Ma&n Page", HELP_MENU_MAN);
  helpMenu->addSeparator();
  ADD_ACTION(help, "&About", HELP_MENU_ABOUT);

  // Make the connections
  connect(fileMapper, SIGNAL(mapped(int)), this, SLOT(fileSlot(int)));
  connect(editMapper, SIGNAL(mapped(int)), this, SLOT(editSlot(int)));
  connect(imageMapper, SIGNAL(mapped(int)), this, SLOT(imageSlot(int)));
  connect(helpMapper, SIGNAL(mapped(int)), this, SLOT(helpSlot(int)));
  connect(fWriteMapper, SIGNAL(mapped(int)), this, SLOT(fileWriteSlot(int)));
  connect(eModelMapper, SIGNAL(mapped(int)), this, SLOT(editModelSlot(int)));
  connect(eObjectMapper, SIGNAL(mapped(int)), this, SLOT(editObjectSlot(int)));
  connect(eSurfaceMapper, SIGNAL(mapped(int)), this,
          SLOT(editSurfaceSlot(int)));
  connect(ePointMapper, SIGNAL(mapped(int)), this, SLOT(editPointSlot(int)));
  connect(eContourMapper, SIGNAL(mapped(int)), this, 
          SLOT(editContourSlot(int)));
  connect(eImageMapper, SIGNAL(mapped(int)), this, SLOT(editImageSlot(int)));
  

  // Now make a central VBox for the control widget and text edit
  // Put widget in a sunken frame because menu bar is flat
  QWidget *central = new QWidget(this);
  QVBoxLayout *cenlay = new QVBoxLayout(central);
  QFrame *frame = new QFrame(central);
  cenlay->addWidget(frame);
  cenlay->setContentsMargins(0, 0, 0, 0);
  cenlay->setSpacing(0);
  
  frame->setFrameStyle(QFrame::Sunken | QFrame::StyledPanel);
  QVBoxLayout *layout = new QVBoxLayout(frame);
  layout->setContentsMargins(2, 2, 2, 2);
  ImodInfoWidget = new InfoControls(frame);
  layout->addWidget(ImodInfoWidget);
  setCentralWidget(central);

  // Get the status window 
  mStatusEdit = new QTextEdit(central);
  cenlay->addWidget(mStatusEdit);
  cenlay->setStretchFactor(mStatusEdit, 100);
  setFontDependentWidths();
  mOldFontHeight = fontMetrics().height();

  setFocusPolicy(Qt::StrongFocus);
  mStatusEdit->setFocusPolicy(Qt::ClickFocus);
  wprintWidget(mStatusEdit);

  setWindowIcon(*(App->iconPixmap));
}

// Set the minimum height for widget and edit box, start timer and set up fro resize
void InfoWindow::setInitialHeights()
{
  imod_info_input();
  int editHeight = mStatusEdit->height();
  int newEdit = B3DNINT(INFO_MIN_LINES * mOldFontHeight);

  // The height may be less than the hint, but things look crammed together then
  QSize hint = ImodInfoWidget->sizeHint();
  int delWidget = B3DMAX(0, ImodInfoWidget->height() - hint.height());
  imodTrace('i', "edith %d newh %d hint %d delWi %d curh", editHeight, newEdit, 
            hint.height(), delWidget, height());
  ImodInfoWidget->setMinimumHeight(hint.height());
  mStatusEdit->setMinimumHeight(newEdit);
  mInfoTimerID = startTimer(10);
  resizeToHeight(height() - (editHeight - newEdit) - delWidget);
}

// Resize window to desired height if the timer isn't active, and save the desired height
// for use when removing low/high sliders
void InfoWindow::resizeToHeight(int newHeight)
{
  int i;
  if (mInfoTimerID) {
    mTargetHeight = newHeight;
    imodTrace('i', "Setting target height %d", newHeight);
  } else {
    imodTrace('i', "resizing to height %d", newHeight);
    for (i = 0; i < 5; i++) {
      resize(width(), newHeight);
      imod_info_input();
      if (height() == newHeight)
        break;
    }
    if (Imod_debug && i)
      imodPrintStderr("Needed %d tries to resize info window\n", i);
  }
  mResizedHeight = newHeight;
}

// If info timer is still active, save the move to be done when it is up; otherwise do
// the move
void InfoWindow::doOrSetupMove(int x, int y)
{
  if (mInfoTimerID) {
    mTargetMoveX = x;
    mTargetMoveY = y;
  } else
    move(x, y);
}

// This is here in case something needs it
void InfoWindow::setFontDependentWidths()
{
}

void InfoWindow::changeEvent(QEvent *e)
{
  QMainWindow::changeEvent(e);
  if (e->type() != QEvent::FontChange)
    return;
  ImodInfoWidget->setFontDependentWidths();
  setFontDependentWidths();
  imod_info_input();

  // Fix the minimum size of the ImodInfoWidget and status edit
  int widgetHi = ImodInfoWidget->adjustedHeightHint();
  int widgDelta  = widgetHi - ImodInfoWidget->minimumHeight();
  int nlines = B3DNINT(mStatusEdit->height() / mOldFontHeight);
  mOldFontHeight = fontMetrics().height();
  int editDelta = B3DNINT(nlines * mOldFontHeight) - mStatusEdit->height();
  mStatusEdit->setMinimumHeight(B3DNINT(INFO_MIN_LINES * mOldFontHeight));
  ImodInfoWidget->setMinimumHeight(widgetHi);
  imodTrace('i', "Font change resize");
  resizeToHeight(height() + widgDelta + editDelta);
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
#ifdef Q_OS_MACX
  if (e->key() == Qt::Key_Q && (e->modifiers() & Qt::ControlModifier)) {
    close();
    return;
  }
#endif

  // These are needed on all architectures to pass Ctrl-C and Ctrl-V through to info pane
  if (e->key() == Qt::Key_C && (e->modifiers() & Qt::ControlModifier)) {
    wprintCopy();
    return;
  }
  if (e->key() == Qt::Key_V && (e->modifiers() & Qt::ControlModifier)) {
    wprintPaste();
    return;
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
  bool minimized = e->type() == QEvent::WindowStateChange &&
    (windowState() & Qt::WindowMinimized);
  bool normal = e->type() == QEvent::WindowStateChange &&
    (windowState() & (Qt::WindowNoState | Qt::WindowMaximized));
  //imodPrintStderr("event type %d\n", e->type());
  if (Imod_debug && minimized)
    imodPrintStderr("State change to minimized\n");
  if (Imod_debug && e->type() == QEvent::Hide)
    imodPrintStderr("Hide\n");
  if (Imod_debug && normal)
    imodPrintStderr("State change to normal\n");
  if (Imod_debug && e->type() == QEvent::Show)
    imodPrintStderr("Show\n");
  if ((minimized || e->type() == QEvent::Hide) && !mMinimized) {
    if (Imod_debug)
      imodPuts("minimizing");
    mMinimized = true;
    imodDialogManager.hide();
  } else if ((normal || e->type() == QEvent::Show) && mMinimized) {
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
  ImodView *vi = App->cvi;
  bool imageOK = !(vi->fakeImage || vi->rgbStore);
  mActions[FILE_MENU_TIFF]->setEnabled(vi->rgbStore != 0);
  mActions[FILE_MENU_EXTRACT]->setEnabled
    (vi->rgbStore == 0 && vi->fakeImage == 0 && vi->multiFileZ <= 0 &&
     vi->noReadableImage == 0 && vi->pyrCache == NULL && !vi->li->plist);
  /*fprintf(stderr, "vi->multiFileZ=%d\n", vi->multiFileZ);*/
  mActions[EIMAGE_MENU_FILLCACHE]->setEnabled(vi->vmSize != 0 || vi->numTimes > 0 ||
                                              vi->pyrCache != NULL);
  mActions[EIMAGE_MENU_FILLER]->setEnabled((vi->vmSize != 0 || vi->numTimes > 0) &&
                                           vi->pyrCache == NULL);
  //mActions[IMAGE_MENU_SLICER]->setEnabled(vi->rawImageStore == 0);
  mActions[IMAGE_MENU_ISOSURFACE]->setEnabled(imageOK);
  mActions[ECONTOUR_MENU_AUTO]->setEnabled(imageOK);
  if (!imageOK) {
    mActions[IMAGE_MENU_GRAPH]->setEnabled(false);
  }
  mActions[EIMAGE_MENU_PROCESS]->setEnabled(imageOK && !vi->colormapImage && 
                                            !vi->pyrCache);
  if (!imageOK || vi->colormapImage) {
    mActions[IMAGE_MENU_TUMBLER]->setEnabled(false);
    ImodInfoWidget->setFloat(-1);
  }
  mActions[IMAGE_MENU_PIXEL]->setEnabled(vi->fakeImage == 0);    

  // These are run-time items.  If more instances appear this should be
  // split into initial and runtime calls
  mActions[EIMAGE_MENU_FLIP]->setEnabled(!iprocBusy() && !vi->colormapImage && 
                                         !vi->pyrCache);
  mActions[EIMAGE_MENU_RELOAD]->setEnabled
    (!iprocBusy() && imageOK && !vi->colormapImage && !vi->noReadableImage && 
     !vi->pyrCache);
  mActions[FILE_MENU_RELOAD]->setEnabled(vi->reloadable != 0);
  mActions[EOBJECT_MENU_DELETE]->setEnabled(!meshingBusy());
  mActions[EOBJECT_MENU_RENUMBER]->setEnabled(!meshingBusy());
}

//Runs trimvol on current time with rubberband and low/high Z coordinates.
void InfoWindow::extract()
{
  MrcHeader *mrchead = (MrcHeader *)App->cvi->image->header;
  int i;
  char *imodDir = getenv("IMOD_DIR");
  const char *cshell = "python";
  QStringList arguments;
  delete mTrimvolProcess;
  mTrimvolProcess = NULL;
  if (App->cvi->rgbStore != 0 || App->cvi->fakeImage != 0 ||
      App->cvi->multiFileZ > 0||App->cvi->image->file != IIFILE_MRC ||
      sliceModeIfReal(mrchead->mode) < 0) {
	wprint("\aUnable to extract - not a real MRC file.\n");
	return;
  }
  if (!imodDir) {
    wprint("\aCannot run trimvol; IMOD_DIR not defined.\n");
    return;
  }
  ZapFuncs *zap = getTopZapWindow(true);
  if (!zap) {
    zap = getTopZapWindow(false);
    if (zap)
      wprint("\aExtracting region visible in top Zap window.\n");
    else {
      wprint("\aThere is no Zap window with or without a rubberband.\n");
      return;
    }
  } else if (zap->mStartingBand)
    wprint("\aExtracting volume visible in Zap window with rubberband on but not "
           "drawn.\n");

  mTrimvolOutput = imodPlugGetSaveName(this, "MRC File to extract to:");
  if (mTrimvolOutput.isEmpty())
    return;
  QString commandString = zap->printInfo(false);
  if (commandString.isEmpty()) {
    return;
  }
  mActions[FILE_MENU_EXTRACT]->setEnabled(false);
  QString filePath;
  if (!Imod_IFDpath.isEmpty()) {
    QDir dir = QDir(Imod_IFDpath);
    filePath = dir.filePath(App->cvi->image->filename);
  }
  else {
    filePath = App->cvi->image->filename;
  }
  mTrimvolProcess = new QProcess();
  arguments << "-u";
  QStringList command = commandString.split(" ", QString::SkipEmptyParts);
  command[0] = QDir::convertSeparators(QString(imodDir) + "/bin/trimvol");
  for (i = 0; i < command.count(); i++)
    arguments << command[i];
    
  arguments << QDir::convertSeparators(filePath);
  arguments << QDir::convertSeparators(mTrimvolOutput);
  wprint("trimvol ");
  for (i = 2; i < arguments.count(); i++)
    wprint("%s ", LATIN1(arguments[i]));
  wprint("\n");
  connect(mTrimvolProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this,
          SLOT(trimvolExited(int, QProcess::ExitStatus )));
  connect(mTrimvolProcess, SIGNAL(error(QProcess::ProcessError)), this,
          SLOT(trimvolError(QProcess::ProcessError )));
  mTrimvolProcess->start(cshell, arguments);
}

// Reports result when trimvol finishes
void InfoWindow::trimvolExited(int exitCode, QProcess::ExitStatus exitStatus) 
{
  mActions[FILE_MENU_EXTRACT]->setEnabled(true);
  if (mTrimvolProcess == NULL) {
    return;
  }
  if (!exitCode && exitStatus == QProcess::NormalExit) {
    wprint("%s created.\n", LATIN1(mTrimvolOutput));
    /*mTrimvolProcess->setReadChannel(QProcess::StandardOutput);
      while (mTrimvolProcess->canReadLine()) {
      wprint("out:\n%s\n", mTrimvolProcess->readLine().constData());
      } */
  } else {
    mTrimvolProcess->setReadChannel(QProcess::StandardOutput);
    wprint("\aTrimvol failed.\n");
    while (mTrimvolProcess->canReadLine()) {
      QString out = mTrimvolProcess->readLine();
      if (out.startsWith("ERROR:")) {
        wprint("%s\n", LATIN1(out));
      }
    }
  }
  mTrimvolProcess->setReadChannel(QProcess::StandardError);
  while (mTrimvolProcess->canReadLine()) {
    wprint("err:\n%s\n", mTrimvolProcess->readLine().constData());
  }
  delete mTrimvolProcess;
  mTrimvolProcess = NULL;
}

// If an error occurs, test for failed to start and report that
void InfoWindow::trimvolError(QProcess::ProcessError error)
{
  if (error != QProcess::FailedToStart)
    return;
  wprint("\aCould not start trimvol - is python on the PATH?\n");
  mActions[FILE_MENU_EXTRACT]->setEnabled(true);
  
  // Cannot delete the qprocess from this slot; just delete it if it runs again
}

// Run imodinfo to get object info
void InfoWindow::objectInfo()
{
  QStringList arguments;
  char *filename;
  delete mImodinfoProcess;
  mImodinfoProcess = NULL;
  filename = currentSavedModelFile();
  if (!filename) {
    wprint("\aCannot run imodinfo - no current model file exists.\n");
    return;
  }
  mImodinfoProcess = new QProcess();
  arguments << "-o" << QString("%1").arg(App->cvi->imod->cindex.object + 1) << filename;
  connect(mImodinfoProcess, SIGNAL(finished(int, QProcess::ExitStatus)), this,
          SLOT(imodinfoExited(int, QProcess::ExitStatus )));
  connect(mImodinfoProcess, SIGNAL(error(QProcess::ProcessError)), this,
          SLOT(imodinfoError(QProcess::ProcessError )));
  mImodinfoProcess->start("imodinfo", arguments);
  mActions[EOBJECT_MENU_INFO]->setEnabled(false);
  wprint("Running imodinfo on current object\n");
}

// Reports result when imodinfo finishes
void InfoWindow::imodinfoExited(int exitCode, QProcess::ExitStatus exitStatus) 
{
  QString out;
  mActions[EOBJECT_MENU_INFO]->setEnabled(true);
  if (!mImodinfoProcess)
    return;

  if (exitCode || exitStatus != QProcess::NormalExit)
    wprint("\aimodinfo failed.\n");

  // Print everything but comments and blank lines and unneeded things
  mImodinfoProcess->setReadChannel(QProcess::StandardOutput);
  while (mImodinfoProcess->canReadLine()) {
    out = mImodinfoProcess->readLine();
    if (! out.trimmed().isEmpty() && !out.startsWith('#') && !out.contains("Light") &&
        !out.contains("Color") && !out.contains("Contours =") && 
        !out.contains("Shininess"))
      wprint(LATIN1((out.trimmed() + "\n")));
  }
  mImodinfoProcess->setReadChannel(QProcess::StandardError);
  while (mImodinfoProcess->canReadLine()) {
    wprint("err:\n%s\n", mImodinfoProcess->readLine().constData());
  }
  delete mImodinfoProcess;
  mImodinfoProcess = NULL;
}

// If an error occurs, test for failed to start and report that
void InfoWindow::imodinfoError(QProcess::ProcessError error)
{
  if (error != QProcess::FailedToStart)
    return;
  wprint("\aCould not start imodinfo.\n");
  mActions[EOBJECT_MENU_INFO]->setEnabled(true);
}

// Start a timer for initial autocontrast
void InfoWindow::setupAutoContrast()
{
  mAutoTimerID = startTimer(10);
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
  Qt::WindowFlags flags = windowFlags();
  if (state)
    flags |= Qt::WindowStaysOnTopHint;
  else
    flags ^= Qt::WindowStaysOnTopHint;

  // Here are old Qt 3 notes.  But with Qt 4, linux jumped to corner sometimes
  // but pos() works on both Windows and Linux now
  //QPoint p(geometry().x(), geometry().y());
  // Using pos() jumps on Windows
  // Also, pos() jumps up-left on Unix, geometry() jumps down-right
  // Unless we access the pos !
  QPoint p2 = pos();
  /*imodPrintStderr("before geom %d %d  pos %d %d\n", geometry().x(), 
    geometry().y(), pos().x(), pos().y()); */
  //imodPrintStderr("pos %d %d\n", p2.x(), p2.y());
  setWindowFlags(flags);
  move(p2);
  show();
  /*imodPrintStderr("after geom %d %d  pos %d %d\n", geometry().x(), 
    geometry().y(), pos().x(), pos().y()); */
#endif
}

// Timer: initially it is for doing autocontrast or fixing info window; then for raising
void InfoWindow::timerEvent(QTimerEvent *e)
{
  int mean, sd;
  if (mAutoTimerID && e->timerId() == mAutoTimerID ) {
    killTimer(mAutoTimerID);
    ImodPrefs->getAutoContrastTargets(mean, sd);
    imodInfoAutoContrast(mean, sd);
    mAutoTimerID = 0;
  } else if (mInfoTimerID && e->timerId() == mInfoTimerID) {

    // Info window timeout: now go set the target height and position
    killTimer(mInfoTimerID);
    mInfoTimerID = 0;
    imodTrace('i', "Info timer fired and killed");
    if (mTargetHeight > 0)
      resizeToHeight(mTargetHeight);
    mTargetHeight = 0;
    if (mTargetMoveX >= 0 && mTargetMoveY >= 0)
      move(mTargetMoveX, mTargetMoveY);
    mTargetMoveX = mTargetMoveY = 0;
  } else
    raise();
}

void InfoWindow::openSelectedWindows(const char *keys, int modelViewOpen)
{
  bool imageOK = !(App->cvi->fakeImage || App->cvi->rgbStore);
  if (!keys)
    return;

  // Model view uses mBCDILMOSVN
  if (strchr(keys, 'a'))
    editContourSlot(ECONTOUR_MENU_AUTO);
  if (strchr(keys, 'b'))
    editContourSlot(ECONTOUR_MENU_BREAK);
  if (strchr(keys, 'c'))
    editContourSlot(ECONTOUR_MENU_COPY);
  if (strchr(keys, 'e'))
    editSlot(EDIT_MENU_SCALEBAR);
  if (strchr(keys, 'f') && (App->cvi->vmSize != 0 || App->cvi->numTimes > 0))
    editImageSlot(EIMAGE_MENU_FILLER);
  if (strchr(keys, 'g') && imageOK)
    imageSlot(IMAGE_MENU_GRAPH);
  if (strchr(keys, 'h'))
    editModelSlot(EMODEL_MENU_HEADER);
  if (strchr(keys, 'j'))
    editContourSlot(ECONTOUR_MENU_JOIN);
  if (strchr(keys, 'l'))
    editObjectSlot(EOBJECT_MENU_COLOR);
  if (strchr(keys, 'm') && imageOK && modelViewOpen == FALSE)
    imageSlot(IMAGE_MENU_LOCATOR);
  if (strchr(keys, 'n'))
    fileSlot(FILE_MENU_MOVIEMONT);
  if (strchr(keys, 'o'))
    editModelSlot(EMODEL_MENU_OFFSETS);
  if (strchr(keys, 'p') && imageOK)
    editImageSlot(EIMAGE_MENU_PROCESS);
  if (strchr(keys, 'r') && imageOK)
    editImageSlot(EIMAGE_MENU_RELOAD);
  if (strchr(keys, 's'))
    editSurfaceSlot(ESURFACE_MENU_GOTO);
  if (strchr(keys, 't'))
    editObjectSlot(EOBJECT_MENU_TYPE);
  if (strchr(keys, 'u') && imageOK)
    imageSlot(IMAGE_MENU_TUMBLER);
  if (strchr(keys, 'v'))
    editContourSlot(ECONTOUR_MENU_MOVE);
  if (strchr(keys, 'x') && !App->cvi->fakeImage)
    imageSlot(IMAGE_MENU_PIXEL);
  if (strchr(keys, 'z'))
    imageSlot(IMAGE_MENU_MULTIZ);
  if (strchr(keys, 'A'))
    editSlot(EDIT_MENU_ANGLES);
  if (strchr(keys, 'F') && imageOK)
    imodPlugOpenByName("Bead Fixer");
  if (strchr(keys, 'G'))
    editSlot(EDIT_MENU_GRAIN);
  if (strchr(keys, 'P'))
    imodPlugOpenAllExternal();
  if (strchr(keys, 'T') && imageOK)
    imodPlugOpenByName("Line Track");
  if (strchr(keys, '1'))
    imod_set_mmode(IMOD_MMODEL);
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
  ImodInfoWin = new InfoWindow(NULL);

  // Initialize the caption and image line
  ImodInfoWin->setWindowTitle(imodCaption("3dmod"));

  filename = imodwfname("Image:");
  filename = truncate_name(filename, 23);
  if (filename) {
    ImodInfoWidget->setImageName(filename);
    free(filename);
  } else
    ImodInfoWidget->setImageName(" ");

  // Try not having these
  /*  imod_info_forbid();
  imod_info_input();
  imod_info_enable(); */

  ImodInfoWin->show();
  ImodInfoWin->setInitialHeights();
  
  // Initialize the model line (could we use MaintainModelName?)
  // Do this after show so the label size works for deciding on big label
  filename = imodwGivenName(" ", Imod_filename);
  if (filename) {
    ImodInfoWidget->setModelName(filename);
    free(filename);
  } else 
    ImodInfoWidget->setModelName(" ");

  return(0);
}


/* Redisplay the model filename after it's changed */

void MaintainModelName(Imod *mod)
{
  char *filestr;
  int namelen;
     
  filestr = imodwGivenName(" ", Imod_filename);
  //filestr = truncate_name(filestr, 23);
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
