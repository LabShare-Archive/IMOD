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
 *
 *  $Id$
 *  Log at end of file
 */

#include "form_info.h"
#include <qmenubar.h>
#include <qmenu.h>
#include <qaction.h>
#include <qsignalmapper.h>
#include <qlayout.h>
#include <qtextedit.h>
#include <qframe.h>
#include <qtimer.h>
#include <qfiledialog.h>
#include <qstringlist.h>
//Added by qt3to4:
#include <QTimerEvent>
#include <QKeyEvent>
#include <QEvent>
#include <QVBoxLayout>
#include <QCloseEvent>
#include "imod_info.h"
#include "imod.h"
#include "imod_info_cb.h"
#include "imodv_objed.h"
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
  ADD_ACTION(file, "S&et Snap Dir...", FILE_MENU_SNAPDIR);
  ADD_ACTION(file, "&Gray TIF snaps", FILE_MENU_SNAPGRAY);
  mActions[FILE_MENU_SNAPGRAY]->setCheckable(true);
  ADD_ACTION(file, "&JPEG Quality...", FILE_MENU_SNAPQUALITY);
  ADD_ACTION(file, "&Memory to TIF...", FILE_MENU_TIFF);
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
  ADD_ACTION(edit, "M&ovies...", EDIT_MENU_MOVIES);
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
  ADD_ACTION(eImage, "F&lip", EIMAGE_MENU_FLIP);
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
  ADD_ACTION(help, "&Man Page", HELP_MENU_MAN);
  ADD_ACTION(help, "&Menus", HELP_MENU_MENUS);
  ADD_ACTION(help, "&Hot Keys", HELP_MENU_HOTKEY);
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
  setFontDependentWidths();

  setFocusPolicy(Qt::StrongFocus);
  mStatusEdit->setFocusPolicy(Qt::ClickFocus);
  wprintWidget(mStatusEdit);

  setWindowIcon(*(App->iconPixmap));
}

// This is not really working well at this point - need to rethink info layouts
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
#ifdef Q_OS_MACX
  if (e->key() == Qt::Key_Q && (e->modifiers() & Qt::ControlModifier)) {
    close();
    return;
  }
#endif
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
  bool imageOK = !(vi->fakeImage || vi->rawImageStore);
  mActions[FILE_MENU_TIFF]->setEnabled(vi->rawImageStore != 0);
  mActions[FILE_MENU_EXTRACT]->setEnabled
    (vi->rawImageStore == 0 && vi->fakeImage == 0 && vi->multiFileZ <= 0 &&
     vi->noReadableImage == 0);
  /*fprintf(stderr, "vi->multiFileZ=%d\n", vi->multiFileZ);*/
  mActions[EIMAGE_MENU_FILLCACHE]->setEnabled(vi->vmSize != 0 || vi->nt > 0);
  mActions[EIMAGE_MENU_FILLER]->setEnabled(vi->vmSize != 0 || vi->nt > 0);
  mActions[IMAGE_MENU_SLICER]->setEnabled(vi->rawImageStore == 0);
  mActions[IMAGE_MENU_XYZ]->setEnabled(vi->rawImageStore == 0);
  mActions[IMAGE_MENU_ISOSURFACE]->setEnabled(imageOK);
  mActions[ECONTOUR_MENU_AUTO]->setEnabled(imageOK);
  if (!imageOK) {
    mActions[IMAGE_MENU_GRAPH]->setEnabled(false);
  }
  if (!imageOK || vi->colormapImage) {
    mActions[EIMAGE_MENU_PROCESS]->setEnabled(false);
    mActions[IMAGE_MENU_TUMBLER]->setEnabled(false);
    ImodInfoWidget->setFloat(-1);
  }
  mActions[IMAGE_MENU_PIXEL]->setEnabled(vi->fakeImage == 0);    

  // These are run-time items.  If more instances appear this should be
  // split into initial and runtime calls
  mActions[EIMAGE_MENU_FLIP]->setEnabled(!iprocBusy() && !vi->colormapImage);
  mActions[EIMAGE_MENU_RELOAD]->setEnabled
    (!iprocBusy() && imageOK && !vi->colormapImage && !vi->noReadableImage);
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
  char *cshell = getenv("IMOD_CSHELL");
  QStringList arguments;
  if (App->cvi->rawImageStore != 0 || App->cvi->fakeImage != 0 ||
      App->cvi->multiFileZ > 0||App->cvi->image->file != IIFILE_MRC ||
      sliceModeIfReal(mrchead->mode) < 0) {
	wprint("\aUnable to extract - not a real MRC file.\n");
	return;
  }
  if (!imodDir) {
    wprint("\aCannot run trimvol; IMOD_DIR not defined.\n");
    return;
  }
  if (!cshell)
    cshell = "tcsh";
  ZapStruct *zap = getTopZapWindow(true);
  if (!zap) {
    zap = getTopZapWindow(false);
    if (zap)
      wprint("\aExtracting region of single section visible in zap window"
             ".\n");
    else {
      wprint("\aThere is no zap window with or without a rubberband.\n");
      return;
    }
  }
  mTrimvolOutput = QFileDialog::getSaveFileName
    (this, "MRC File to extract to:");
  if (mTrimvolOutput.isEmpty())
    return;
  QString commandString = zapPrintInfo(zap, false);
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
  arguments << "-f";
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
  mTrimvolProcess->start(cshell, arguments);
}

// Reports result when trimvol finishes
void InfoWindow::trimvolExited(int exitCode, QProcess::ExitStatus exitStatus) 
{
  mActions[FILE_MENU_EXTRACT]->setEnabled(true);
  if (mTrimvolProcess == 0) {
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
  mTrimvolProcess = 0;
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

// Timer: initially it is for doing autocontrast; then for raising
void InfoWindow::timerEvent(QTimerEvent *e)
{
  int mean, sd;
  if (mAutoTimerID) {
    killTimer(mAutoTimerID);
    ImodPrefs->getAutoContrastTargets(mean, sd);
    imodInfoAutoContrast(mean, sd);
    mAutoTimerID = 0;
  } else
    raise();
}

void InfoWindow::openSelectedWindows(char *keys)
{
  bool imageOK = !(App->cvi->fakeImage || App->cvi->rawImageStore);
  if (!keys)
    return;

  // Model view uses mBCDILMOSV
  if (strchr(keys, 'a'))
    editContourSlot(ECONTOUR_MENU_AUTO);
  if (strchr(keys, 'b'))
    editContourSlot(ECONTOUR_MENU_BREAK);
  if (strchr(keys, 'c'))
    editContourSlot(ECONTOUR_MENU_COPY);
  if (strchr(keys, 'e'))
    editSlot(EDIT_MENU_SCALEBAR);
  if (strchr(keys, 'f') && (App->cvi->vmSize != 0 || App->cvi->nt > 0))
    editImageSlot(EIMAGE_MENU_FILLER);
  if (strchr(keys, 'g') && imageOK)
    imageSlot(IMAGE_MENU_GRAPH);
  if (strchr(keys, 'h'))
    editModelSlot(EMODEL_MENU_HEADER);
  if (strchr(keys, 'j'))
    editContourSlot(ECONTOUR_MENU_JOIN);
  if (strchr(keys, 'l'))
    editObjectSlot(EOBJECT_MENU_COLOR);
  if (strchr(keys, 'm') && imageOK)
    imageSlot(IMAGE_MENU_LOCATOR);
  if (strchr(keys, 'n'))
    editSlot(EDIT_MENU_MOVIES);
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
  if (strchr(keys, 'T'))
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

  // Initialize the model line (could we use MaintainModelName?)
  filename = imodwGivenName(" ", Imod_filename);
  //filename = truncate_name(filename, 23);
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


/*

$Log$
Revision 4.55  2009/02/27 19:17:16  mast
Fixed output of trimvol command to info window

Revision 4.54  2009/01/16 20:23:44  mast
Comment out debug output

Revision 4.53  2009/01/15 16:33:17  mast
Qt 4 port

Revision 4.52  2008/12/10 01:04:50  mast
Added menu item to set jpeg quality

Revision 4.51  2008/12/01 15:42:01  mast
Changes for undo/redo and selection in 3dmodv standalone

Revision 4.50  2008/07/30 17:07:42  mast
Fixed keep on top, broken by autocontrast at start

Revision 4.49  2008/05/27 05:52:11  mast
New menu items: checkable RGB to gray snaps, isosurface.  Also start
a timer to do autocontrast after all windows opened

Revision 4.48  2008/05/25 21:23:36  mast
fix trying to wprint with a Qstring

Revision 4.47  2008/04/02 04:13:02  mast
Manage menu for no readable image

Revision 4.46  2008/03/06 00:13:27  mast
Added key to start in model mode

Revision 4.45  2008/02/28 00:06:57  mast
Changed to run trimvol with tcsh -f ..., needed for Windows

Revision 4.44  2008/02/22 00:33:31  sueh
bug# 1076 Added extract menu option, and extract(), and trimvolExited().

Revision 4.43  2008/01/25 20:22:58  mast
Changes for new scale bar

Revision 4.42  2008/01/21 05:55:44  mast
Added key to open plugins

Revision 4.41  2008/01/13 22:58:35  mast
Changes for multi-Z window

Revision 4.40  2007/11/10 04:07:10  mast
Changes for setting snapshot directory

Revision 4.39  2007/08/13 16:04:50  mast
Changes for locator window

Revision 4.38  2007/07/08 16:47:42  mast
Added object combine

Revision 4.37  2007/05/25 05:28:16  mast
Changes for addition of slicer angle storage

Revision 4.36  2006/09/12 15:43:30  mast
Disable object delete and renumber if meshing

Revision 4.35  2006/09/01 20:49:29  mast
Added menu item to flatten contours in object

Revision 4.34  2006/08/28 05:17:56  mast
Manipulate menus more for colormapped images loaded

Revision 4.33  2005/10/14 22:04:39  mast
Changes for Model reload capability

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
