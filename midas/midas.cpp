/*
 *  midas.c -- Main manual image alignment program.
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>
#include <stdarg.h>

#include "midas.h"
#include "mrcc.h"
#include "b3dutil.h"
#include "math.h"
#include <qmenu.h>
#include <qmenubar.h>
#include <qgridlayout.h>
#include <qstringlist.h>
#include <qregexp.h>
#include <qlayout.h>
#include <qtooltip.h>
#include <QDoubleSpinBox>
#include <QKeyEvent>
#include <QKeySequence>
#include <QLabel>
#include <QFrame>
#include <QCloseEvent>
#include <QAction>
#include "arrowbutton.h"
#include "dia_qtutils.h"
#ifndef NO_IMOD_FORK
#include <unistd.h>
#endif

#define ADD_ACTION(a, b, c) menuActions[c] = a##Menu->addAction(b); \
connect(menuActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(menuActions[c], c);

#define ADD_ACTION_KEY(a, b, c, d) menuActions[c] = a##Menu->addAction(b); \
menuActions[c]->setShortcut(QKeySequence(d)); \
connect(menuActions[c], SIGNAL(triggered()), a##Mapper, SLOT(map())); \
a##Mapper->setMapping(menuActions[c], c);

MidasView *VW;
int Midas_debug = 0;

#define ARROW_SIZE 19

static void usage(void)
{
  const char *pname = "midas";
  QString qstr;
  
  printf("%s version %s\n", pname, MIDAS_VERSION_STRING);
  imodCopyright();
  qstr.sprintf("Usage: %s [options] <mrc filename> [transform filename]\n", pname);
  qstr += "Options:\n";
  qstr += "   -g\t\t Output global transforms (default is local)\n";
  qstr += "   -r <filename>\t Load reference image file\n";
  qstr += "   -rz <section>\t Section # for reference (default 0)\n";
  qstr += "   -p <filename>\t Load piece list file for fixing montages\n";
  qstr += "   -c <size list>\t Align chunks of sections; list # of sections in chunks\n";
  qstr += "   -cs <size list>\t Align chunks of sections; list # of sample slices\n";
  qstr += "   -B <factor>\t Bin images by the given factor\n";
  qstr += "   -C <size>\t Set cache size to given number of sections\n"; 
  qstr += "   -s <min,max>\t Set intensity scaling; min to 0 and max to 255\n";
  qstr += "   -b 0\t Turn on interpolation\n";
  qstr += "   -a <angle>\t Rotate all images by angle.\n";
  qstr += "   -t <filename>\t Load tilt angles from file and allow cosine "
    "stretching.\n";
  qstr += "   -o <filename>\t Output transforms to given file instead of input file\n";
  qstr += "   -e <number>\t Show given number of buttons with largest edge errors\n";
  qstr += "   -O <letters>\t Two letters for colors of previous/current in overlay\n";
  qstr += "   -S\t\t Use single-buffered visual\n";
  qstr += "   -D\t\t Debug mode - do not run in background\n";
  qstr += "   -q\t\t Suppress reminder message when fixing edges\n";
#ifdef _WIN32
  dia_puts(LATIN1(qstr));
#else
  printf(LATIN1(qstr));
#endif
  exit(3);
}

int main (int argc, char **argv)
{
  MidasView midasView, *vw;
  FILE *file;
  int i, k, bottom, top;
  bool doubleBuffer = true;
  int styleSet = 0;
  QStringList chunkList;
  bool ok;
  int chunkErr = 0;
  int oarg = 0;
  int sampleSlices = -1;

#ifdef NO_IMOD_FORK
  int dofork = 0;
#else
  int dofork = 1;
#endif

  vw = VW = &midasView;

  // Prescan for style and debug flags
  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '-' && argv[i][1] == 's' && argv[i][2] == 't'
        && argv[i][3] == 'y' && argv[i][4] == 'l' && argv[i][5] == 'e')
      styleSet = 1;
    if (argv[i][0] == '-' && argv[i][1] == 'D')
      dofork = 0;
  }

#ifndef NO_IMOD_FORK
  // Fork before starting Qt application
  if (dofork) {
    if (fork())
      exit(0);
  }
#endif

#ifdef Q_OS_MACX
  // fix OS X 10.9 font issue https://bugreports.qt-project.org/browse/QTBUG-32789
  // MV_10_8 is not defined in Qt 4.6 etc, this is the value in Qt 4.8
  if (QSysInfo::MacintoshVersion > 0x000A)
    QFont::insertSubstitution(".Lucida Grande UI", "Lucida Grande");
#endif

  QApplication myapp(argc, argv);
  diaSetTitle("Midas");
  b3dSetStoreError(1);
  setlocale(LC_NUMERIC, "C");

  if (argc < 2)
    usage();

#ifdef __linux
  if (!styleSet)
    QApplication::setStyle("windows");
#endif

  new_view(VW);

  for (i = 1; i < argc ; i++){
    if (argv[i][0] == '-'){
      switch (argv[i][1]){
      case 'r': /* reference image */
        if (argv[i][2] == 'z'){
          vw->xsec = atoi(argv[++i]);
        }else{
          vw->refname = argv[++i];
        }
        break;
                    
      case 'p': /* piece list */
        vw->plname = argv[++i];
        break;

      case 'g':
        vw->xtype = XTYPE_XG;
        break;

      case 'C':
        vw->cachein = atoi(argv[++i]);
        break;

      case 'b':
        if (!atoi(argv[++i]))
          vw->fastInterp = 0;
        break;

      case 'B':
        vw->binning = atoi(argv[++i]);
        vw->binning = B3DMAX(1, B3DMIN(8, vw->binning));
        break;

      case 'e':
        vw->numTopErr = atoi(argv[++i]);
        break;

      case 'a':
        vw->globalRot = atof(argv[++i]);
        vw->rotMode = 1;
        break;

      case 't': /* tilt angles */
        vw->tiltname = argv[++i];
        vw->cosStretch = -1;
        vw->rotMode = 1;
        break;

      case 's':
        sscanf(argv[++i], "%f%*c%f", &(vw->sminin), &(vw->smaxin));
        break;

      case 'D':
        Midas_debug = 1;
        break;

      case 'q':
        vw->quiet = 1;
        break;

      case 'S':
        doubleBuffer = false;
        break;

      case 'c':
        if (sampleSlices >= 0) {
          printf("ERROR: midas - You cannot enter both -c and -cs\n");
          exit(1);
        }
        sampleSlices = argv[i][2] == 's' ? 1 : 0;
        chunkList = QString(argv[++i]).split(",", QString::SkipEmptyParts);
        vw->numChunks = chunkList.count();
        if (sampleSlices)
          vw->numChunks = chunkList.count() / 2 + 1;
        break;

      case 'o':
        vw->oname = strdup(argv[++i]);
        break;

      case 'O':
        oarg = ++i;
        break;

      default:
        printf("ERROR: midas - Illegal option entered: %s\n", argv[i]);
        usage();
        break;
      }
    }else 
      break;
  }
     
  if (i < argc - 2 || i == argc)
    usage();
     

  /* If there are two args left, the last one is name of transform file */
  if (i == argc - 2) {
    /* It gets freed by a save-as, so need to copy to malloc'd space */
    vw->xname = (char *)malloc(strlen(argv[argc - 1]) + 2);
    strcpy(vw->xname, argv[argc - 1]);
    file = fopen(vw->xname, "r");
    if (file) {
      /* If file opened, close and mark that need to read it */
      fclose(file);
      vw->didsave = -1;
    } else {
      /* Otherwise give warning that new file will be used */
      fprintf(stderr, "Transform file (%s) not found;\n"
              " transforms will be saved in a "
              "new file by that name.\n", vw->xname);
    }
  }

  // Process overlay color entry
  if (oarg) {
    if (strlen(argv[oarg]) != 2)
      midas_error("Two letters must be entered with -O: two of r g b c m y", "", 1);
    for (k = 0; k < 3; k++)
      vw->imageForChannel[k] = 0;
    for (k = 0; k < 2; k++) {
      switch (argv[oarg][k]) {
      case 'r':
        vw->imageForChannel[2] += k + 1;
        break;
      case 'g':
        vw->imageForChannel[1] += k + 1;
        break;
      case 'b':
        vw->imageForChannel[0] += k + 1;
        break;
      case 'c':
        vw->imageForChannel[1] += k + 1;
        vw->imageForChannel[0] += k + 1;
        break;
      case 'm':
        vw->imageForChannel[2] += k + 1;
        vw->imageForChannel[0] += k + 1;
        break;
      case 'y':
        vw->imageForChannel[2] += k + 1;
        vw->imageForChannel[1] += k + 1;
        break;
      default:
        midas_error("The letters entered with -O must be two of r g b c m y", "", 1);
      }
    }

    // If there is any overlap one will add to 3
    for (k = 0; k < 3; k++)
      if (vw->imageForChannel[k] > 2)
        midas_error("The two letters entered with -O must specify different "
                    "color channels for previous and current images", "", 1);
  }

  // Check other entries if doing montage mode
  if (vw->plname) {
    if (vw->refname || vw->rotMode || vw->numChunks)
      midas_error("You cannot use the -p option with the ", 
                  (char *)(vw->rotMode ? "-a or -t option." : 
                           (vw->refname ? "-r option." : "-c option.")), 1);

    if (vw->xtype == XTYPE_XG)
      dia_puts("The -g option has no effect when fixing montage overlaps.");
    vw->xtype = XTYPE_MONT;
  }            

  // Check features if doing reference mode or chunk mode
  if (vw->refname || vw->numChunks) {
    if (vw->rotMode)
      dia_puts("The -a or -t options have no effect with alignment to a "
              "reference section or in chunk mode.");
    vw->rotMode = 0;
    vw->cosStretch = 0;
    if (vw->xtype == XTYPE_XG)
      dia_puts("The -g option has no effect with alignment to a "
              "reference section or in chunk mode.");
    if (vw->refname)
      vw->xtype = XTYPE_XREF;
  }

  if (vw->cosStretch && vw->xtype == XTYPE_XG)
    midas_error("Global alignment mode cannot be used with cosine stretching", "", 1);

  // If doing chunk mode, get sizes, make sure no zeros, defer further checking
  if (vw->numChunks) {
    if (vw->refname)
      midas_error("Chunk alignment cannot be done in reference alignment mode", "", 1);
    if (sampleSlices && chunkList.count() % 2)
      midas_error("A list of sample slices must have an even number of values", "", 1);
    vw->chunk = (Midas_chunk *)malloc((vw->numChunks + 1) * sizeof(Midas_chunk));
    if (!vw->chunk)
      midas_error("Error getting memory for chunk data.", "", 3);

    // Check the entries for positive integers
    for (k = 0; k < vw->numChunks; k++)
      if (chunkList[k].toInt(&ok)<= 0 || !ok)
        chunkErr = 1;
    if (chunkErr || vw->numChunks < 2) 
      midas_error(sampleSlices ? "The -cs option must be followed by a comma-separated "
                  "list" : "The -c option must be followed by a comma-separated list",
                  sampleSlices ? "of the number of slices in each bottom and top sample."
                  : "of the number of sections in each chunk.", 1);

    vw->chunk[0].start = 0;
    if (sampleSlices) {

      // Interpret the input for sample slices, which is top/bottom&top/.../bottom
      for (k = 0; k < vw->numChunks; k++) {
        bottom = 0;
        if (k)
          bottom = chunkList[2 * k - 1].toInt(&ok);
        top = 0;
        if (k < vw->numChunks - 1)
          top = chunkList[2 * k].toInt(&ok);
        vw->chunk[k].size = bottom + top;
        vw->chunk[k + 1].start = vw->chunk[k].start + vw->chunk[k].size;
        vw->chunk[k].minRefSec = vw->chunk[k].start + bottom;
        vw->chunk[k].maxCurSec = vw->chunk[k].minRefSec - 1;
      }
    } else {
      
      // Or process chunk sizes
      for (k = 0; k < vw->numChunks; k++) {
        vw->chunk[k].size = chunkList[k].toInt(&ok);
        vw->chunk[k + 1].start = vw->chunk[k].start + vw->chunk[k].size;
        vw->chunk[k].minRefSec = vw->chunk[k].start;
        vw->chunk[k].maxCurSec = vw->chunk[k + 1].start - 1;
      }
    }

  }
  VW->warpingOK = VW->xtype != XTYPE_XG && VW->xtype != XTYPE_MONT && !VW->rotMode;

  if (load_view(VW, argv[i]))
    midas_error("Error opening ", argv[i], 3);

  // Increase the default point size if font is specified in points,
  // or if not, increase the pixel size.  GAVE IT UP 10/12/08
  /*  QFont newFont = QApplication::font();
  float pointSize = newFont.pointSizeFloat();
  if (pointSize > 0) {
    if (Midas_debug)
      fprintf(stderr, "Default font point size %f\n", pointSize);
    newFont.setPointSizeFloat(pointSize * 1.2);
  } else {
    int pixelSize = newFont.pixelSize();
    if (Midas_debug)
      fprintf(stderr, "Default font pixel size %d\n", pixelSize);
    newFont.setPixelSize((int)floor(pixelSize * 1.2 + 0.5));
  }
  QApplication::setFont(newFont); */

  // Create the components (window creates the GL widget)
  vw->midasSlots = new MidasSlots();
  vw->midasWindow = new MidasWindow(doubleBuffer);
  //myapp.setMainWidget(vw->midasWindow);
  VW->midasSlots->updateWarpEdit();

  vw->midasWindow->show();
  vw->midasWindow->setFocus();
#ifdef Q_OS_MACX
  vw->midasWindow->raise();
#endif
  return myapp.exec();
}


MidasWindow::MidasWindow(bool doubleBuffer, QWidget * parent, Qt::WFlags f)
  : QMainWindow(parent, f)
{
  int newWidth, newHeight, xleft, ytop;
  int commandWidth, commandHeight, menu;

  // Yes this goes out of scope, but this was a prototype for 3dmod
  QAction *menuActions[LAST_MENU_ID];

  // Create the menus in the menubar
  QMenu *fileMenu = menuBar()->addMenu("&File");
  QMenu *editMenu = menuBar()->addMenu("&Edit");
  menuBar()->addSeparator();
  QMenu *helpMenu = menuBar()->addMenu("&Help");
  QSignalMapper *fileMapper = new QSignalMapper(this);
  QSignalMapper *editMapper = new QSignalMapper(this);
  QSignalMapper *helpMapper = new QSignalMapper(this);
  connect(fileMapper, SIGNAL(mapped(int)), VW->midasSlots, 
          SLOT(slotFilemenu(int)));
  connect(editMapper, SIGNAL(mapped(int)), VW->midasSlots, 
          SLOT(slotEditmenu(int)));
  connect(helpMapper, SIGNAL(mapped(int)), VW->midasSlots, 
          SLOT(slotHelpmenu(int)));

  // Create file menu
  ADD_ACTION(file, "&Load transforms", FILE_MENU_LOAD);
  ADD_ACTION_KEY(file, "&Save transforms", FILE_MENU_SAVE, Qt::Key_S);
  ADD_ACTION(file, "Sa&ve transforms as...", FILE_MENU_SAVE_AS);
  ADD_ACTION(file, "Save &contrast-scaled image...", FILE_MENU_SAVE_IMAGE);
  ADD_ACTION(file, "&Transform model...", FILE_MENU_TRANSFORM);
  fileMenu->addSeparator();
  ADD_ACTION(file, "&Quit", FILE_MENU_QUIT);
  if (VW->xtype == XTYPE_XF || VW->xtype == XTYPE_MONT)
    menuActions[FILE_MENU_TRANSFORM]->setEnabled(false);
  
  // Create Edit menu
  ADD_ACTION(edit, "&Store section transform", EDIT_MENU_STORE);
  ADD_ACTION(edit, "&Reset to unit transform", EDIT_MENU_RESET);
  ADD_ACTION(edit, "Re&vert to stored transform", EDIT_MENU_REVERT);
  ADD_ACTION(edit, "&Mirror around X axis", EDIT_MENU_MIRROR);
  menuActions[EDIT_MENU_MIRROR]->setEnabled(VW->xtype != XTYPE_MONT);
  ADD_ACTION_KEY(edit, "&Delete control point", EDIT_MENU_DELETEPT, 
                 Qt::SHIFT + Qt::Key_D);
  menuActions[EDIT_MENU_DELETEPT]->setEnabled(VW->warpingOK);
  
  // Create Help menu
  ADD_ACTION(help, "&Controls", HELP_MENU_CONTROLS);
  ADD_ACTION(help, "&Hotkeys", HELP_MENU_HOTKEYS);
  ADD_ACTION(help, "&Mouse", HELP_MENU_MOUSE);
  ADD_ACTION(help, "Man &Page", HELP_MENU_MANPAGE);
  helpMenu->addSeparator();
  ADD_ACTION(help, "&About Midas", HELP_MENU_ABOUT);

  // Create main widget control panel
  QWidget *mainbox = new QWidget(this);
  QHBoxLayout *mainlay = new QHBoxLayout(mainbox);
  mainlay->setContentsMargins(2, 2, 2, 2);
  setCentralWidget(mainbox);
  QWidget *outer = new QWidget(mainbox);
  mainlay->addWidget(outer);
  QVBoxLayout *col = new QVBoxLayout(outer);
  col->setSpacing(4);
  col->setContentsMargins(3, 3, 3, 3);

  // Need GLwidget next
  QGLFormat glFormat;
  glFormat.setRgba(true);
  glFormat.setDoubleBuffer(doubleBuffer);
  VW->midasGL = new MidasGL(glFormat, mainbox);
  mainlay->addWidget(VW->midasGL);

  mainlay->setStretchFactor(col, 0);
  mainlay->setStretchFactor(VW->midasGL, 1);

  createSectionControls(col);
  makeSeparator(col, 2);
  createContrastControls(col);
  makeSeparator(col, 2);
  createZoomBlock(col);
  makeSeparator(col, 1);
  createViewToggle(col);
  makeSeparator(col, 2);
  createParameterDisplay(col);

  col->addStretch();

  // set window width from current width plus image width
  QSize comSize = col->sizeHint();
  QSize winSize = sizeHint();
  newWidth = winSize.width() + VW->xsize;
  commandWidth = comSize.width();

  // Set height from max of command area and image height, plus difference
  // between command and window height, which is menu area
  commandHeight = comSize.height();
  menu = winSize.height() - commandHeight;
  newHeight = menu + B3DMAX(commandHeight, VW->ysize);

  // But limit by size of display, allow extra on top for title bar, make
  // sure it is on screen for Windows
  diaLimitWindowSize(newWidth, newHeight);

  QRect pos = geometry();
  xleft = pos.x();
  ytop = pos.y();
  diaLimitWindowPos(newWidth, newHeight, xleft, ytop);

  resize(newWidth, newHeight);
  move(xleft, ytop);

  // Zoom down so it fits.
  while ((VW->xsize * VW->truezoom > 1.1 * (newWidth - commandWidth) || 
          VW->ysize * VW->truezoom > 1.1 * (newHeight - menu)) && 
         VW->zoomind > 0)
    VW->midasSlots->slotZoom(-1);
  
  // For edges, shift them so the whole edge shows
  if (VW->xtype == XTYPE_MONT) {
    VW->xtrans = -newWidth / 2;
    VW->ytrans = -newHeight / 2;
  }

  // This should be a good thing, because widgets were all initialized with
  // extreme numbers
  outer->setFixedWidth(commandWidth);
  setFocusPolicy(Qt::StrongFocus);
}

MidasWindow::~MidasWindow()
{
}

// Process a close event by calling the quit routine and ignoring event
void MidasWindow::closeEvent ( QCloseEvent * e )
{
  VW->midasSlots->slotMidas_quit();
  e->ignore();
}

void MidasWindow::keyPressEvent ( QKeyEvent * e )
{
  VW->midasSlots->midas_keyinput(e);
}

void MidasWindow::keyReleaseEvent ( QKeyEvent * e )
{
  if (e->key() == Qt::Key_Control) {
    VW->ctrlPressed = 0;
    VW->midasGL->manageMouseLabel(" ");
  }
  if (e->key() == Qt::Key_Shift) {
    VW->shiftPressed = 0;
    VW->midasGL->manageMouseLabel(" ");
  }
}

void MidasWindow::makeSeparator(QVBoxLayout *parent, int width)
{
  QFrame *frame = new QFrame();
  parent->addWidget(frame);
  frame->setFrameStyle(QFrame::Plain | QFrame::HLine);
  frame->setLineWidth(width);
}

void MidasWindow::makeTwoArrows(QHBoxLayout *parent, int direction, int signal,
                                QSignalMapper *mapper, bool repeat, const char *tip1, 
                                const char *tip2, ArrowButton **arrow1,
                                ArrowButton **arrow2)
  
{
  parent->setSpacing(4);
  ArrowButton *arrow = new ArrowButton(direction < 0 ? 
                                       Qt::LeftArrow : Qt::UpArrow, NULL);
  parent->addWidget(arrow);
  arrow->setFixedWidth(ARROW_SIZE);
  arrow->setFixedHeight(ARROW_SIZE);
  arrow->setAutoRepeat(repeat);
  if (tip1)
    arrow->setToolTip(QString(tip1));
  mapper->setMapping(arrow, direction * signal);
  QObject::connect(arrow, SIGNAL(clicked()), mapper, SLOT(map()));
  if (arrow1)
    *arrow1 = arrow;

  arrow = new ArrowButton(direction < 0 ? Qt::RightArrow : Qt::DownArrow, 
                          NULL);
  parent->addWidget(arrow);
  arrow->setFixedWidth(ARROW_SIZE);
  arrow->setFixedHeight(ARROW_SIZE);
  arrow->setAutoRepeat(repeat);
  if (tip2)
    arrow->setToolTip(QString(tip2));
  mapper->setMapping(arrow, -direction * signal);
  QObject::connect(arrow, SIGNAL(clicked()), mapper, SLOT(map()));
  if (arrow2)
    *arrow2 = arrow;
}

QLabel *MidasWindow::makeArrowRow(QVBoxLayout *parent, int direction, int signal, 
                                  QSignalMapper *mapper, bool repeat, QString textlabel,
                                  int decimals, int digits, float value, 
                                  const char *tip1, const char *tip2,
                                  ArrowButton **arrow1, ArrowButton **arrow2,
                                  QLabel **textLabel)
{
  char string[32];
  QLabel *label;
  QString str;
  QHBoxLayout *row = diaHBoxLayout(parent);
  makeTwoArrows(row, direction, signal, mapper, repeat, tip1, tip2, arrow1, arrow2);
  
  label = diaLabel(LATIN1(textlabel), NULL, row);
  label->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
  if (textLabel)
    *textLabel = label;
  VW->midasSlots->sprintf_decimals(string, decimals, digits, value);
  str = string;
  label = diaLabel(LATIN1(str), NULL, row);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  return (label);
}

QSignalMapper *MidasWindow::makeLabeledArrows(QVBoxLayout *parent, QString textlabel,
                                              QLabel **outLabel, bool repeat, 
                                              const char *tip1, const char *tip2)
{
  QHBoxLayout *row = diaHBoxLayout(parent);
  QSignalMapper *mapper = new QSignalMapper();
  
  makeTwoArrows(row, 1, 1, mapper, repeat, tip1, tip2);
  
  *outLabel = diaLabel(LATIN1(textlabel), NULL, row);
  (*outLabel)->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
  //row->setStretchFactor(*outLabel, 5);
  return (mapper);
}

QSpinBox *MidasWindow::makeSpinBoxRow(QHBoxLayout *row, const char *labText,
                                      int minz, int maxz)
{
  QSpinBox *spin = (QSpinBox *)diaLabeledSpin
    (0, (float)minz, (float)maxz, 1., labText, NULL,  row);
  spin->setFixedWidth(60);
  return spin;
}

void MidasWindow::createParameterDisplay(QVBoxLayout *col)
{
  int i;
  QLabel *label;
  QString str;
  QCheckBox *check;
  
  for (i = 0; i < 3; i++) {
    VW->mouseLabel[i] = diaLabel(" ", NULL, col);
    VW->mouseLabel[i]->setAlignment(Qt::AlignCenter);
  }
  VW->midasGL->manageMouseLabel(" ");

  if (VW->warpingOK) {
    VW->warpToggle = diaCheckBox("Add/edit warp points", NULL, col);
    QObject::connect(VW->warpToggle, SIGNAL(toggled(bool)), 
                     VW->midasSlots, SLOT(slotEditWarp(bool)));
    VW->warpToggle->setToolTip("Add or modify points for warping section");
  }

  QSignalMapper *paramMapper = new QSignalMapper(col);
  QSignalMapper *incMapper = new QSignalMapper(col);
  VW->wParameter[3] = makeArrowRow
    (col, -1, 4, paramMapper, true, "X translation",
     VW->midasSlots->getParamDecimals(3), VW->midasSlots->getParamDigits(3),
     -1000.0, "Shift current image left (hot key Left Arrow)", 
     "Shift current image right (hot key Right Arrow)");
  VW->wParameter[4] = makeArrowRow
    (col, 1, 5, paramMapper, true, "Y translation",
     VW->midasSlots->getParamDecimals(4), VW->midasSlots->getParamDigits(4),
     -1000.0, "Shift current image up (hot key Up Arrow)", 
     "Shift current image down (hot key Down Arrow");
  VW->wIncrement[2] = makeArrowRow
    (col, 1, 3, incMapper, false, "   increment ",
     VW->midasSlots->getIncDecimals(2),  VW->midasSlots->getIncDigits(2),
     VW->increment[2], "Make shift increment bigger", 
     "Make shift increment smaller");
  if (VW->xtype != XTYPE_MONT) {
    VW->wLinearTrans = diaLabel("Linear trans:  0.0, 0.0", NULL, col);
    VW->wLinearTrans->hide();
    makeSeparator(col, 1);
    VW->wParameter[0] = makeArrowRow
      (col, -1, 1, paramMapper, true, "Rotation    ",
       VW->midasSlots->getParamDecimals(0), VW->midasSlots->getParamDigits(0),
       -179., "Rotate current image counterclockwise (hot key o)", 
       "Rotate current image clockwise (hot key l)", &VW->arrowsToGray[0], 
       &VW->arrowsToGray[1], &VW->labelsToGray[0]);
    VW->wIncrement[0] = makeArrowRow
      (col, 1, 1, incMapper, false, "   increment",
       VW->midasSlots->getIncDecimals(0), VW->midasSlots->getIncDigits(0),
       VW->increment[0], "Make rotation increment bigger",
       "Make rotation increment smaller", &VW->arrowsToGray[2], &VW->arrowsToGray[3],
       &VW->labelsToGray[1]);
    makeSeparator(col, 1);

    VW->wParameter[1] = makeArrowRow
      (col, 1, 2, paramMapper, true, "Magnification",
       VW->midasSlots->getParamDecimals(1),  VW->midasSlots->getParamDigits(1),
       1.0, "Scale current image up (hot key p)", "Scale current image down (hot key ;)",
       &VW->arrowsToGray[4], &VW->arrowsToGray[5], &VW->labelsToGray[2]);
    VW->wParameter[2] = makeArrowRow
      (col, 1, 3, paramMapper, true, "Stretch      ",
       VW->midasSlots->getParamDecimals(2),  VW->midasSlots->getParamDigits(2),
       1.0, "Stretch current image along axis (hot key [)", 
       "Compress current image along axis (hot key ')", &VW->arrowsToGray[6], 
       &VW->arrowsToGray[7], &VW->labelsToGray[3]);
    VW->wIncrement[1] = makeArrowRow
      (col, 1, 2, incMapper, false, "   factor    ", 
       VW->midasSlots->getIncDecimals(1),  VW->midasSlots->getIncDigits(1),
       VW->increment[1], "Increase amount by which to scale and stretch",
       "Decrease amount by which to scale and stretch", &VW->arrowsToGray[8], 
       &VW->arrowsToGray[9], &VW->labelsToGray[4]);

  }     

  // Connect mappers to slots
  QObject::connect(paramMapper, SIGNAL(mapped(int)), VW->midasSlots,
                   SLOT(slotParameter(int)));
  QObject::connect(incMapper, SIGNAL(mapped(int)), VW->midasSlots,
                   SLOT(slotIncrement(int)));

  if (VW->xtype != XTYPE_MONT) {
    VW->anglescale = diaSlider(-900, 900, 100, 0, NULL, col);
    VW->anglescale->setPageStep(10);
    QHBoxLayout *slideBox = diaHBoxLayout(col);
    QLabel *slideName = diaLabel("Stretch Angle", NULL, slideBox);
    slideName->setAlignment(Qt::AlignLeft);
    VW->anglelabel = diaLabel("0.0", NULL, slideBox);
    VW->anglelabel->setAlignment(Qt::AlignRight);
    QObject::connect(VW->anglescale, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotAngle(int)));
    VW->anglescale->setToolTip("Set angle at which new stretch is applied"
                                 " (hot keys ] and \\)");
    VW->labelsToGray[5] = slideName;
    VW->labelsToGray[6] = VW->anglelabel;
  }

  makeSeparator(col, 1);
  QPushButton *button = diaPushButton("Cross-Correlate", NULL, col);
  QObject::connect(button, SIGNAL(clicked()), VW->midasSlots,
                   SLOT(slotCorrelate()));
  button->setToolTip("Find shift by cross-correlating the two images in boxed"
                     " region (hot key C)");
  QHBoxLayout *corrParamBox = diaHBoxLayout(col);
  int maxdim = B3DMAX(VW->xsize, VW->ysize);
  VW->corrBoxSpin = (QSpinBox *)diaLabeledSpin
    (0, 32., (float)(0.8 * maxdim), 16., "Box", NULL, corrParamBox);
  VW->corrLimitSpin = (QSpinBox *)diaLabeledSpin
    (0, 2., (float)(0.5 * maxdim), 2., "Limit", NULL, corrParamBox);
  VW->corrBoxSpin->setValue(VW->corrBoxSize);
  VW->corrLimitSpin->setValue(VW->corrShiftLimit);
  QObject::connect(VW->corrBoxSpin, SIGNAL(valueChanged(int)),
                   VW->midasSlots, SLOT(slotCorrBoxSize(int)));
  QObject::connect(VW->corrLimitSpin, SIGNAL(valueChanged(int)),
                   VW->midasSlots, SLOT(slotCorrShiftLimit(int)));
  VW->corrBoxSpin->setToolTip("Size of square box around center point to"
                              " correlate");
  VW->corrLimitSpin->setToolTip("Maximum shift in X or Y to search for "
                              "correlation peak");

  if (VW->warpingOK) {
    makeSeparator(col, 1);
    QSignalMapper *selectMapper = new QSignalMapper(col);
    QHBoxLayout *selectBox = diaHBoxLayout(col);
    VW->wSelectLabel = diaLabel("Select:", NULL, selectBox);
    VW->wSelectBiggest = diaPushButton("Biggest Warp", NULL, selectBox);
    VW->wSelectBiggest->setToolTip("Select warp point with biggest shift");
    makeTwoArrows(selectBox, 1, 1, selectMapper, true, "Select warp point with next "
                  "bigger shift", "Select warp point with next smaller shift",
                  &VW->wSelectMore, &VW->wSelectLess);
    selectBox->addStretch();
    selectMapper->setMapping(VW->wSelectBiggest, 0);
    QObject::connect(VW->wSelectBiggest, SIGNAL(clicked()), selectMapper, SLOT(map()));
    QObject::connect(selectMapper, SIGNAL(mapped(int)), VW->midasSlots,
                     SLOT(slotSelectWarpPointBySize(int)));
    VW->wDrawVectors = diaCheckBox("Draw warp vectors", NULL, col);
    QObject::connect(VW->wDrawVectors, SIGNAL(toggled(bool)), VW->midasSlots,
                     SLOT(slotDrawVectors(bool)));
    VW->wDrawVectors->setToolTip("Draw lines for shifts at warp points");
  }
  if (VW->xtype != XTYPE_MONT) {
    if (VW->rotMode) {
      makeSeparator(col, 1);
      QHBoxLayout *globRotBox = diaHBoxLayout(col);
      //QLabel *globLabel = diaLabel("Global rotation", NULL, globRotBox);
      //globLabel->setAlignment(Qt::AlignLeft);

      VW->globRotSpin = (QDoubleSpinBox *)diaLabeledSpin
        (1, -180., 180., 1., "Global rotation", NULL, globRotBox);
      VW->globRotSpin->setFixedWidth
        (VW->globRotSpin->fontMetrics().width("-180.0000"));
      VW->globRotSpin->setValue(VW->globalRot);
      QObject::connect(VW->globRotSpin, SIGNAL(valueChanged(double)), 
                       VW->midasSlots, SLOT(slotGlobRot(double)));
      VW->globRotSpin->setToolTip("Angle to rotate both images by to make "
                                  "tilt axis vertical");
      check = diaCheckBox("Mouse shifts X only", NULL, col);
      check->setChecked(false);
      QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
                       SLOT(slotConstrainMouse(bool)));
      check->setToolTip("Allow shifts with mouse only in X (perpendicular to"
                        " tilt axis)");
      if (VW->cosStretch) {
        check = diaCheckBox("Apply cosine stretch", NULL, col);
        check->setChecked(false);
        QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
                         SLOT(slotCosStretch(bool)));
        check->setToolTip("Show current image stretched by ratio of cosines "
                          "of tilt angles");
        VW->cosStretch = 0;

        QHBoxLayout *tiltOffBox = diaHBoxLayout(col);
        VW->tiltOffSpin = (QDoubleSpinBox *)diaLabeledSpin
          (1, -90., 90., 1., "Tilt angle offset", NULL, tiltOffBox);
        VW->tiltOffSpin->setFixedWidth
          (VW->tiltOffSpin->fontMetrics().width("-180.0000"));
        VW->tiltOffSpin->setValue(0.);
        VW->tiltOffSpin->setToolTip("Amount to add to all tilt angles");
        QObject::connect(VW->tiltOffSpin, SIGNAL(valueChanged(double)), 
                         VW->midasSlots, SLOT(slotTiltOff(double)));
      }
    }

  } else {
    makeSeparator(col, 2);

    VW->numTopErr = B3DMIN(MAX_TOP_ERR, B3DMAX(2, VW->numTopErr));
    if (VW->nxpieces == 1 || VW->nypieces == 1)
      VW->numTopErr = 2;
    else if (VW->numTopErr > VW->maxedge[0] + VW->maxedge[1])
      VW->numTopErr = 2 * ((VW->maxedge[0] + VW->maxedge[1]) / 2);
    VW->wMeanerr = diaLabel("Mean error: 100.00", NULL, col);
    VW->wMeanerr->setAlignment(Qt::AlignLeft);
    str.sprintf("Top %d errors:", VW->numTopErr);
    label = diaLabel(LATIN1(str), NULL, col);
    label->setAlignment(Qt::AlignLeft);

    QGridLayout *grid = new QGridLayout();
    col->addLayout(grid);
    grid->setSpacing(5);
    QSignalMapper *mapper = new QSignalMapper();
    for (i = 0; i < VW->numTopErr; i++) {
      VW->wToperr[i] = new QPushButton("X 199: 50.00  ");
      grid->addWidget(VW->wToperr[i], i / 2, i % 2);
      mapper->setMapping(VW->wToperr[i], i);
      QObject::connect(VW->wToperr[i], SIGNAL(clicked()), mapper, SLOT(map()));
      VW->wToperr[i]->setFocusPolicy(Qt::NoFocus);
    }
    QObject::connect(mapper, SIGNAL(mapped(int)), 
                     VW->midasSlots, SLOT(slotTop_error(int)));

    VW->wCurerr = diaLabel("This edge: -50.00, -50.00", NULL, col);
    VW->wCurerr->setAlignment(Qt::AlignLeft);
    VW->wLeaverr = diaLabel("Leave-out: -50.00, -50.00", NULL, col);
    VW->wLeaverr->setAlignment(Qt::AlignLeft);

    VW->wApplyLeave = diaPushButton("Apply Leave-out Error", NULL, col);
    QObject::connect(VW->wApplyLeave, SIGNAL(clicked()), VW->midasSlots,
                     SLOT(slotLeave_out()));
    VW->wApplyLeave->setToolTip("Set this edge shift to value implied by "
                                "all the other edge shifts");

    QHBoxLayout *robustBox = diaHBoxLayout(col);
    check = diaCheckBox("Robust fits,  crit.", NULL, robustBox);
    QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
                     SLOT(slotRobustFit(bool)));
    check->setToolTip("Use weighted fitting to try to eliminate bad "
                      "displacements");
    VW->robustFit = VW->nxpieces * VW->nypieces > 10 ? 1 : 0;
    diaSetChecked(check, VW->robustFit != 0);
    
    VW->robustSpin = (QDoubleSpinBox *)diaLabeledSpin(1, 0.5f, 3.0f, 0.1f,
                                                      NULL, NULL, robustBox);
    VW->robustSpin->setValue(VW->robustCrit);
    VW->robustSpin->setEnabled( VW->robustFit != 0);
    QObject::connect(VW->robustSpin, SIGNAL(valueChanged(double)), 
                     VW->midasSlots, SLOT(slotRobustCrit(double)));
    VW->robustSpin->setToolTip("Set criterion for identifying a displacement"
                               " as an outlier");
    
    if (VW->nxpieces * VW->nypieces > 1200) {
      check = diaCheckBox("Skip error computation", NULL, col);
      QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
                       SLOT(slotSkipError(bool)));
      check->setToolTip("Speed up display by not computing edge errors");
    }
  }
}

void MidasWindow::createSectionControls(QVBoxLayout *parent)
{
  QHBoxLayout *row;
  QVBoxLayout *col = parent;
  QLabel *label;
  int maxz;

  // Reference section text box
  if (VW->xtype != XTYPE_MONT) {
    row = diaHBoxLayout(col);
    maxz = (VW->xtype == XTYPE_XREF) ? VW->refzsize : VW->zsize;
    VW->refSpin = makeSpinBoxRow(row, "Reference Sec. ", 1, maxz);
    QObject::connect(VW->refSpin, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotRefValue(int)));
    VW->refSpin->setToolTip("Set section being aligned to");
  }

  // Current section text box
  row = diaHBoxLayout(col);
  maxz = (VW->xtype == XTYPE_MONT) ? VW->maxzpiece + 1 : VW->zsize;
  VW->curSpin = makeSpinBoxRow(row, "Current Sec. ", 1, maxz);
  QObject::connect(VW->curSpin, SIGNAL(valueChanged(int)),
                   VW->midasSlots, SLOT(slotCurValue(int)));
  VW->curSpin->setToolTip("Set the current section to align (hot keys a and b)");
  
  if (VW->numChunks) {

    // Chunk mode: just add a chunk spin box
    row = diaHBoxLayout(col);
    VW->chunkSpin = makeSpinBoxRow(row, "Current Chunk ", 2, VW->numChunks);
    QObject::connect(VW->chunkSpin, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotChunkValue(int)));
    VW->chunkSpin->setToolTip("Set the chunk (tomogram) to align to the "
                              "previous one (hot keys A and B)");
  
  } else if (VW->xtype != XTYPE_MONT) {
    
    // Non-montage: the difference checkbox and mode label

    VW->difftoggle  = diaCheckBox("Keep Curr - Ref diff = 1", NULL, col);
    VW->difftoggle->setChecked(VW->xtype != XTYPE_XREF);
    QObject::connect(VW->difftoggle, SIGNAL(toggled(bool)), 
                     VW->midasSlots, SLOT(slotKeepdiff(bool)));
    VW->difftoggle->setToolTip("Always change current and reference sections together");
  }

  if (!VW->numChunks && VW->xtype != XTYPE_MONT) {
    if (VW->xtype == XTYPE_XG)
      label = diaLabel ("Global Alignment Mode", NULL, col);
    else
      label = diaLabel(VW->xtype == XTYPE_XREF ? "Reference Alignment Mode" :
                       "Local Alignment Mode", NULL, col);
    label->setAlignment(Qt::AlignCenter);

  } else if (VW->xtype == XTYPE_MONT) { 

    // Make the X and Y radio buttons, and edge number textbox
    row = diaHBoxLayout(col);
    row->setSpacing(3);
    VW->edgeGroup = new QButtonGroup();
    VW->wXedge = diaRadioButton("X", NULL, VW->edgeGroup, row, 0, NULL);
    VW->wYedge = diaRadioButton("Y", NULL, VW->edgeGroup, row, 1, NULL);
    QObject::connect(VW->edgeGroup, SIGNAL(buttonClicked(int)),
                     VW->midasSlots, SLOT(slotXory(int)));
    VW->wXedge->setToolTip("Select edges between adjacent pieces in X");
    VW->wYedge->setToolTip("Select edges between adjacent pieces in Y");

    VW->edgeSpin = makeSpinBoxRow(row, "Edge ", 1, VW->maxedge[VW->xory]);
    QObject::connect(VW->edgeSpin, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotEdgeValue(int)));
    VW->edgeSpin->setToolTip("Set sequential edge number (hot keys A and B)");
    row = diaHBoxLayout(col);
    row->setSpacing(3);
    VW->lowerXspin = makeSpinBoxRow(row, "Lower X", 1, VW->nxpieces);
    VW->lowerYspin = (QSpinBox *)diaLabeledSpin
      (0, 1., (float)VW->nypieces, 1., "Y", NULL,  row);
    VW->lowerYspin->setFixedWidth(60);
    QObject::connect(VW->lowerXspin, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotLowerXvalue(int)));
    QObject::connect(VW->lowerYspin, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotLowerYvalue(int)));
    VW->lowerXspin->setToolTip("Set frame number in X of piece below edge"
                               " (hot keys x and X)");
    VW->lowerYspin->setToolTip("Set frame number in Y of piece below edge"
                               " (hot keys y and Y)");

    VW->wExcludeEdge = diaCheckBox("Exclude edge", NULL, col);
    QObject::connect(VW->wExcludeEdge, SIGNAL(toggled(bool)), 
                     VW->midasSlots, SLOT(slotExcludeEdge(bool)));
    VW->wExcludeEdge->setToolTip("Mark this as an excluded edge here and"
                                 " in Blendmont");

    VW->wSkipExcluded = diaCheckBox("Skip excluded edges", NULL, col);
    VW->wSkipExcluded->setChecked(VW->anySkipped != 0);
    VW->wSkipExcluded->setEnabled(VW->anySkipped != 0);
    QObject::connect(VW->wSkipExcluded, SIGNAL(toggled(bool)), 
                     VW->midasSlots, SLOT(slotSkipExcluded(bool)));
    VW->excludeSkipped = VW->anySkipped;
    VW->wSkipExcluded->setToolTip("Skip over excluded edges and"
                                  " exclude them from error computations");

    VW->midasSlots->manage_xory(VW);
    
  }
}

void MidasWindow::createZoomBlock(QVBoxLayout *parent)
{
  QString str;

  QSignalMapper *mapper = makeLabeledArrows
    (parent, "Zoom  1.00", &VW->zoomlabel, false, "Increase the display zoom "
     "(hot key =)", "Decrease the display zoom (hot key -)");
  QObject::connect(mapper, SIGNAL(mapped(int)), VW->midasSlots,
                     SLOT(slotZoom(int)));

  QCheckBox *check = diaCheckBox("Interpolate", NULL, parent);
  check->setChecked(VW->fastInterp == 0);
  QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
                     SLOT(slotInterpolate(bool)));
  check->setToolTip("Transform image by linear interpolation instead of block"
                    " copies or nearest point interpolation");
}

void MidasWindow::createViewToggle(QVBoxLayout *parent)
{

  VW->overlaytoggle = diaCheckBox("Overlay view", NULL, parent);
  VW->overlaytoggle->setChecked(VW->vmode == MIDAS_VIEW_COLOR);
  QObject::connect(VW->overlaytoggle, SIGNAL(toggled(bool)), VW->midasSlots,
                   SLOT(slotOverlay(bool)));
  VW->overlaytoggle->setToolTip("Show both images in two-color overlay (hot "
                                "keys Insert or Delete)");

  // ONE widget needs to be able to accept focus or the spin boxes keep it
  VW->overlaytoggle->setFocusPolicy(Qt::TabFocus);

  QPushButton *button = diaPushButton("Toggle Ref/Cur", NULL, parent);
  QObject::connect(button, SIGNAL(pressed()), VW->midasSlots,
                     SLOT(slotAlign_arm()));
  QObject::connect(button, SIGNAL(released()), VW->midasSlots,
                     SLOT(slotAlign_disarm()));
  button->setToolTip("Toggle between current image and reference image");
}

void MidasWindow::createContrastControls(QVBoxLayout *parent)
{
  QHBoxLayout *row;

  row = diaHBoxLayout(parent);
  row->setSpacing(3);
  diaLabel("Black", NULL, row);
  VW->wBlacklevel = diaSlider(0, 255, 1, 0, NULL, row);
  VW->wBlackval = diaLabel("000", NULL, row);
  QObject::connect(VW->wBlacklevel, SIGNAL(valueChanged(int)),
                   VW->midasSlots, SLOT(slotBlacklevel(int)));
  QObject::connect(VW->wBlacklevel, SIGNAL(sliderPressed()),
                   VW->midasSlots, SLOT(slotBlackPressed()));
  QObject::connect(VW->wBlacklevel, SIGNAL(sliderReleased()),
                   VW->midasSlots, SLOT(slotBlackReleased()));
  VW->wBlacklevel->setToolTip("Set lower end of contrast ramp (hot keys F1 "
                              "and F2)");

  row = diaHBoxLayout(parent);
  row->setSpacing(3);
  diaLabel("White", NULL, row);
  VW->wWhitelevel = diaSlider(0, 255, 1, 255, NULL, row);
  row->addWidget(VW->wWhitelevel);
  VW->wWhiteval = diaLabel("255", NULL, row);
  QObject::connect(VW->wWhitelevel, SIGNAL(valueChanged(int)),
                   VW->midasSlots, SLOT(slotWhitelevel(int)));
  QObject::connect(VW->wWhitelevel, SIGNAL(sliderPressed()),
                   VW->midasSlots, SLOT(slotWhitePressed()));
  QObject::connect(VW->wWhitelevel, SIGNAL(sliderReleased()),
                   VW->midasSlots, SLOT(slotWhiteReleased()));
  VW->wWhitelevel->setToolTip("Set upper end of contrast ramp (hot keys F3 "
                              "and F4)");
     
  QCheckBox *check = diaCheckBox("Apply to only one sec.", NULL, parent);
  check->setChecked(false);
  QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
                     SLOT(slotApplyone(bool)));
  check->setToolTip("Adjust contrast independently for the current section");

  VW->reversetoggle = diaCheckBox("Reverse contrast", NULL, parent);
  VW->reversetoggle->setChecked(false);
  QObject::connect(VW->reversetoggle, SIGNAL(toggled(bool)), VW->midasSlots,
                     SLOT(slotReverse(bool)));
  VW->reversetoggle->setToolTip("Show images in inverted contrast");

  QPushButton *button = diaPushButton("Auto Contrast", NULL, parent);
  QObject::connect(button, SIGNAL(clicked()), VW->midasSlots,
                     SLOT(slotAutoContrast()));
  button->setToolTip("Adjust contrast to standard level based on mean/SD of "
                     "image (hot key Ctrl-A)");
}



void midas_error(const char *tmsg, const char *bmsg, int retval)
{
  QString str;
  /* if (VW->midasWindow == NULL)
    fprintf(stderr, "%s %s\n", tmsg, bmsg);
    else { */
    str.sprintf("%s %s\n", tmsg, bmsg);
    dia_err(LATIN1(str));
    //}  
  if (retval)
    exit(retval);

  return;
}

/* Takes fprintf-type arguments and prints to stderr, and flushes on Windows */
void printStderr(const char *format, ...)
{
  char errorMess[512];
  va_list args;
  va_start(args, format);
  vsprintf(errorMess, format, args);
  fprintf(stderr, errorMess);
#ifdef _WIN32
  fflush(stderr);
#endif
}

