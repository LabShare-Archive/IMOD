/*  IMOD VERSION 2.50
 *
 *  midas.c -- Main manual image alignment program.
 *              Renamed from midas because there is already a program 
 *              called midas
 * 
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "midas.h"
#include "mrcc.h"
#include "b3dutil.h"
#include "math.h"
#include <qpopupmenu.h>
#include <qmenubar.h>
#include <qgrid.h>
#include <qstringlist.h>
#include <qregexp.h>
#include <qlayout.h>
#include "arrowbutton.h"
#include "floatspinbox.h"
#include "dia_qtutils.h"
#ifndef NO_IMOD_FORK
#include <unistd.h>
#endif

struct Midas_view *VW;
int Midas_debug = 0;

#define ARROW_SIZE 19

static void usage(void)
{
     char *pname = "midas";
     QString qstr;

     printf("%s version %s\n", pname, MIDAS_VERSION_STRING);
     imodCopyright();
     qstr.sprintf("Usage: %s [x opts] [options] <mrc filename> "
	     "[transform filename]\n", pname);
     qstr += "options:\n";
     qstr += "\t-g              output global transforms (default"
       " is local)\n";
     qstr += "\t-r <filename>   load reference image file\n";
     qstr += "\t-rz <section>   section # for reference (default 0)\n";
     qstr += "\t-p <filename>   load piece list file for fixing montages\n";
     qstr += "\t-c <size list>  align chunks of sections; list # of sections "
       "in chunks\n",
     qstr += "\t-C <size>       set cache size to given number of "
       "sections\n"; 
     qstr += "\t-s <min,max>    set intensity scaling; min to 0 and"
       " max to 255\n";
     qstr += "\t-b <size>       set initial size for block copies\n";
     qstr += "\t-a <angle>      rotate all images by angle.\n";
     qstr += "\t-o <filename>   output transforms to given file instead of "
       "input file\n";
     qstr += "\t-S              use single-buffered visual\n";
     qstr += "\t-D              debug mode - do not run in background\n";
#ifdef _WIN32
     dia_puts((char *)qstr.latin1());
#else
     printf(qstr.latin1());
#endif
     exit(3);
}

int main (int argc, char **argv)
{
  struct Midas_view MidasView, *vw;
  FILE *file;
  int i, k;
  bool doubleBuffer = true;
  int styleSet = 0;
  QStringList chunkList;
  bool ok;
  int chunkErr = 0;

#ifdef NO_IMOD_FORK
  int dofork = 0;
#else
  int dofork = 1;
#endif

  vw = VW = &MidasView;

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

  QApplication myapp(argc, argv);
  diaSetTitle("Midas");
  b3dSetStoreError(1);

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
	vw->boxsize = atoi(argv[++i]);
	break;

      case 'a':
	vw->globalRot = atof(argv[++i]);
        vw->rotMode = 1;
	break;

      case 's':
	sscanf(argv[++i], "%f%*c%f", &(vw->sminin), &(vw->smaxin));
	break;

      case 'D':
	Midas_debug = 1;
	break;

      case 'S':
	doubleBuffer = false;
	break;

      case 'c':
        chunkList = QStringList::split(',', QString(argv[++i]));
        vw->numChunks = chunkList.count();
        break;

      case 'o':
        vw->oname = strdup(argv[++i]);
        break;

      default:
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

  // Check other entries if doing montage mode
  if (vw->plname) {
    if (vw->refname || vw->rotMode || vw->numChunks)
      midas_error("You cannot use the -p option with the ", 
                  (char *)(vw->rotMode ? "-a option." : 
                           (vw->refname ? "-r option." : "-c option.")), 1);

    if (vw->didsave != -1)
      midas_error("The last entry on the line must be the name of"
	      " an existing edge\n correlation displacement file.", "", 1);

    if (vw->xtype == XTYPE_XG)
      dia_puts("The -g option has no effect when fixing montage overlaps.");
    vw->xtype = XTYPE_MONT;
  }	       

  // Check features if doing reference mode or chunk mode
  if (vw->refname || vw->numChunks) {
    if (vw->rotMode)
      dia_puts("The -a option has no effect with alignment to a "
	      "reference section or in chunk mode.");
    vw->rotMode = 0;
    if (vw->xtype == XTYPE_XG)
      dia_puts("The -g option has no effect with alignment to a "
	      "reference section or in chunk.");
    if (vw->refname)
      vw->xtype = XTYPE_XREF;
  }

  // If doing chunk mode, get sizes, make sure no zeros, defer further checking
  if (vw->numChunks) {
    if (vw->refname)
      midas_error("Chunk alignment cannot be done in reference alignment mode",
                  "", 1);
    vw->chunk = (struct Midas_chunk *)malloc((vw->numChunks + 1) * 
                                                  sizeof(struct Midas_chunk));
    if (!vw->chunk)
      midas_error("Error getting memory for chunk data.", "", 3);

    vw->chunk[0].start = 0;
    for (k = 0; k < vw->numChunks; k++) {
      vw->chunk[k].size = chunkList[k].toInt(&ok);
      if (vw->chunk[k].size <= 0 || !ok)
        chunkErr = 1;
      vw->chunk[k + 1].start = vw->chunk[k].start + vw->chunk[k].size;
    }

    if (chunkErr || vw->numChunks < 2) 
      midas_error("The -c option must be followed by a comma-separated list",
                  " of the number of sections in each chunk.", 1);
  }

  if (load_view(VW, argv[i]))
    midas_error("Error opening ", argv[i], 3);

  // Increase the default point size if font is specified in points,
  // or if not, increase the pixel size
  QFont newFont = QApplication::font();
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
  QApplication::setFont(newFont);

  // Create the components (window creates the GL widget)
  vw->midasSlots = new MidasSlots();
  vw->midasWindow = new MidasWindow(doubleBuffer);
  myapp.setMainWidget(vw->midasWindow);

  vw->midasWindow->show();
  vw->midasWindow->setFocus();

  return myapp.exec();
}


MidasWindow::MidasWindow(bool doubleBuffer, QWidget * parent, 
			 const char * name, WFlags f)
  : QMainWindow(parent, name, f)
{
  int deskWidth, deskHeight, newWidth, newHeight;
  int commandWidth, commandHeight, id;

  // Create file menu
  QPopupMenu *fileMenu = new QPopupMenu;
  fileMenu->insertItem("&Load transforms", FILE_MENU_LOAD);
  id = fileMenu->insertItem("&Save transforms", FILE_MENU_SAVE);
  fileMenu->setAccel(Key_S, id);   // Want it to show upas little s but...
  fileMenu->insertItem("Sa&ve transforms as...", FILE_MENU_SAVE_AS);
  fileMenu->insertItem("Save &contrast-scaled image...", FILE_MENU_SAVE_IMAGE);
  fileMenu->insertItem("&Transform model...", FILE_MENU_TRANSFORM);
  fileMenu->insertSeparator();
  fileMenu->insertItem("&Quit", FILE_MENU_QUIT);
  QObject::connect(fileMenu, SIGNAL(activated(int)), VW->midasSlots,
		   SLOT(slotFilemenu(int)));
  if (VW->xtype == XTYPE_XF || VW->xtype == XTYPE_MONT)
    fileMenu->setItemEnabled(FILE_MENU_TRANSFORM, false);
  
  // Create Edit menu
  QPopupMenu *editMenu = new QPopupMenu;
  editMenu->insertItem("&Store section transform", EDIT_MENU_STORE);
  editMenu->insertItem("&Reset to unit transform", EDIT_MENU_RESET);
  editMenu->insertItem("Re&vert to stored transform", EDIT_MENU_REVERT);
  QObject::connect(editMenu, SIGNAL(activated(int)), VW->midasSlots,
		   SLOT(slotEditmenu(int)));
  
  // Create Help menu
  QPopupMenu *helpMenu = new QPopupMenu;
  helpMenu->insertItem("&Controls", HELP_MENU_CONTROLS);
  helpMenu->insertItem("&Hotkeys", HELP_MENU_HOTKEYS);
  helpMenu->insertItem("&Mouse", HELP_MENU_MOUSE);
  helpMenu->insertSeparator();
  helpMenu->insertItem("&About Midas", HELP_MENU_ABOUT);
  QObject::connect(helpMenu, SIGNAL(activated(int)), VW->midasSlots,
		   SLOT(slotHelpmenu(int)));

  // Create and fill menu bar
  QMenuBar *menuBar = new QMenuBar(this);
  menuBar->insertItem("&File", fileMenu);
  menuBar->insertItem("&Edit", editMenu);
  menuBar->insertSeparator();
  menuBar->insertItem("&Help", helpMenu);

  // Create main widget control panel
  QHBox *mainbox = new QHBox(this);
  setCentralWidget(mainbox);
  QVBox *outer = new QVBox(mainbox);
  QVBox *col = new QVBox(outer);
  col->setSpacing(7);
  col->setMargin(5);

  // Need GLwidget next
  QGLFormat glFormat;
  glFormat.setRgba(true);
  glFormat.setDoubleBuffer(doubleBuffer);
  VW->midasGL = new MidasGL(glFormat, mainbox);

  mainbox->setStretchFactor(col, 0);
  mainbox->setStretchFactor(VW->midasGL, 1);

  createSectionControls(col);
  makeSeparator(col, 2);
  createContrastControls(col);
  makeSeparator(col, 2);
  createZoomBlock(col);
  makeSeparator(col, 1);
  createViewToggle(col);
  makeSeparator(col, 2);
  createParameterDisplay(col);

  // The alternative, skipping the outer box and adding the spacer to col,
  // worked just as well as long as a stretch factor was set for the spacer
  // But this seems less mysterious and does not even require stretch factors
  new QVBox(outer);
  
  // set window width from current width plus image width
  QSize comSize = col->sizeHint();
  QSize winSize = sizeHint();
  newWidth = winSize.width() + VW->xsize;
  commandWidth = comSize.width();

  // Set height from max of command area and image height, plus difference
  // between command and window height, which is menu area
  commandHeight = comSize.height();
  newHeight = (winSize.height() - commandHeight) +
    commandHeight > VW->ysize ? commandHeight : VW->ysize;
  if (Midas_debug)
    printf ("BUG: image %d %d  window %d %d   controls %d %d  resize %d %d\n", 
	    VW->xsize, VW->ysize, winSize.width(), winSize.height(),
	    commandWidth, commandHeight, newWidth, newHeight);
  // Looks like a compiler bug on g++, so try again

  newHeight = (winSize.height() - commandHeight);
  newHeight +=  commandHeight > VW->ysize ? commandHeight : VW->ysize;

  // But limit by size of display, allow extra on top for title bar
  deskWidth = QApplication::desktop()->width() - 40;
  deskHeight = QApplication::desktop()->height() - 60;
  if (newWidth > deskWidth)
    newWidth = deskWidth;
  if (newHeight > deskHeight)
    newHeight = deskHeight;

  if (Midas_debug)
    printf ("image %d %d  window %d %d   controls %d %d  resize %d %d\n", 
	    VW->xsize, VW->ysize, winSize.width(), winSize.height(),
	    commandWidth, commandHeight, newWidth, newHeight);
  resize(newWidth, newHeight);

  // This should be a good thing, because widgets were all initialized with
  // exteme numbers
  col->setFixedWidth(commandWidth);
  setFocusPolicy(StrongFocus);
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
  if (e->key() == Key_Control) {
    VW->ctrlPressed = 0;
    VW->midasGL->manageMouseLabel(" ");
  }
  if (e->key() == Key_Shift) {
    VW->shiftPressed = 0;
    VW->midasGL->manageMouseLabel(" ");
  }
}

void MidasWindow::makeSeparator(QVBox *parent, int width)
{
  QFrame *frame = new QFrame(parent);
  frame->setFrameStyle(QFrame::Plain | QFrame::HLine);
  frame->setLineWidth(width);
}

void MidasWindow::makeTwoArrows(QHBox *parent, int direction, 
                                int signal, QSignalMapper *mapper, bool repeat)

{
  parent->setSpacing(4);
  ArrowButton *arrow = new ArrowButton(direction < 0 ? 
					Qt::LeftArrow : Qt::UpArrow, parent);
  arrow->setFixedWidth(ARROW_SIZE);
  arrow->setFixedHeight(ARROW_SIZE);
  arrow->setAutoRepeat(repeat);
  mapper->setMapping(arrow, direction * signal);
  QObject::connect(arrow, SIGNAL(clicked()), mapper, SLOT(map()));

  arrow = new ArrowButton(direction < 0 ? Qt::RightArrow : Qt::DownArrow,
			  parent);
  arrow->setFixedWidth(ARROW_SIZE);
  arrow->setFixedHeight(ARROW_SIZE);
  arrow->setAutoRepeat(repeat);
  mapper->setMapping(arrow, -direction * signal);
  QObject::connect(arrow, SIGNAL(clicked()), mapper, SLOT(map()));
}

QLabel *MidasWindow::makeArrowRow(QVBox *parent, int direction, int signal,
				  QSignalMapper *mapper, bool repeat,
				  QString textlabel, int decimals, int digits,
				  float value)
{
  char string[32];
  QLabel *label;
  QString str;
  QHBox *row = new QHBox(parent);
  makeTwoArrows(row, direction, signal, mapper, repeat);
  
  label = new QLabel(textlabel, row);
  label->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
  VW->midasSlots->sprintf_decimals(string, decimals, digits, value);
  str = string;
  label = new QLabel(str, row);
  label->setAlignment(Qt::AlignRight | Qt::AlignVCenter);
  return (label);
}

QSignalMapper *MidasWindow::makeLabeledArrows(QVBox *parent, QString textlabel,
					      QLabel **outLabel, bool repeat)
{
  QHBox *row = new QHBox(parent);
  QSignalMapper *mapper = new QSignalMapper(row);
  
  makeTwoArrows(row, 1, 1, mapper, repeat);
  
  *outLabel = new QLabel(textlabel, row);
  (*outLabel)->setAlignment(Qt::AlignLeft | Qt::AlignVCenter);
  //row->setStretchFactor(*outLabel, 5);
  return (mapper);
}

QSpinBox *MidasWindow::makeSpinBoxRow(QHBox *row, char *labText,
                                 int minz, int maxz)
{
  QLabel *label = new QLabel(QString(labText), row);
  label->setAlignment(AlignRight | AlignVCenter);
  row->setStretchFactor(label, 1);
  QSpinBox *spin = new QSpinBox(minz, maxz, 1, row);
  spin->setFixedWidth(60);
  spin->setFocusPolicy(ClickFocus);
  return spin;
}

void MidasWindow::createParameterDisplay(QVBox *col)
{
  int i;
  QLabel *label;
  
  VW->mouseLabel = new QLabel(" ", col);
  VW->mouseLabel->setAlignment(Qt::AlignCenter);
  VW->midasGL->manageMouseLabel(" ");

  QSignalMapper *paramMapper = new QSignalMapper(col);
  QSignalMapper *incMapper = new QSignalMapper(col);
  VW->wParameter[3] = makeArrowRow
    (col, -1, 4, paramMapper, true, "X translation",
     VW->midasSlots->getParamDecimals(3), VW->midasSlots->getParamDigits(3),
     -1000.0);
  VW->wParameter[4] = makeArrowRow
    (col, 1, 5, paramMapper, true, "Y translation",
     VW->midasSlots->getParamDecimals(4), VW->midasSlots->getParamDigits(4),
     -1000.0);
  VW->wIncrement[2] = makeArrowRow
    (col, 1, 3, incMapper, false, "   increment ",
     VW->midasSlots->getIncDecimals(2),  VW->midasSlots->getIncDigits(2),
     VW->increment[2]);
  if (VW->xtype != XTYPE_MONT) {
    makeSeparator(col, 1);
    VW->wParameter[0] = makeArrowRow
      (col, -1, 1, paramMapper, true, "Rotation    ",
       VW->midasSlots->getParamDecimals(0), VW->midasSlots->getParamDigits(0),
       -179.);
    VW->wIncrement[0] = makeArrowRow
      (col, 1, 1, incMapper, false, "   increment",
       VW->midasSlots->getIncDecimals(0), VW->midasSlots->getIncDigits(0),
       VW->increment[0]);
    makeSeparator(col, 1);

    VW->wParameter[1] = makeArrowRow
      (col, 1, 2, paramMapper, true, "Magnification",
       VW->midasSlots->getParamDecimals(1),  VW->midasSlots->getParamDigits(1),
       1.0);
    VW->wParameter[2] = makeArrowRow
      (col, 1, 3, paramMapper, true, "Stretch      ",
       VW->midasSlots->getParamDecimals(2),  VW->midasSlots->getParamDigits(2),
       1.0);
    VW->wIncrement[1] = makeArrowRow
      (col, 1, 2, incMapper, false, "   factor    ", 
       VW->midasSlots->getIncDecimals(1),  VW->midasSlots->getIncDigits(1),
       VW->increment[1]);

  }     

  // Connect mappers to slots
  QObject::connect(paramMapper, SIGNAL(mapped(int)), VW->midasSlots,
		   SLOT(slotParameter(int)));
  QObject::connect(incMapper, SIGNAL(mapped(int)), VW->midasSlots,
		   SLOT(slotIncrement(int)));

  if (VW->xtype != XTYPE_MONT) {
    VW->anglescale = new QSlider(-900, 900, 100, 0, Qt::Horizontal, col);
    VW->anglescale->setLineStep(10);
    VW->anglescale->setFocusPolicy(NoFocus);
    QHBox *slideBox = new QHBox(col);
    QLabel *slideName = new QLabel("Stretch Angle", slideBox);
    slideName->setAlignment(Qt::AlignLeft);
    VW->anglelabel = new QLabel("0.0", slideBox);
    VW->anglelabel->setAlignment(Qt::AlignRight);
    QObject::connect(VW->anglescale, SIGNAL(valueChanged(int)),
		     VW->midasSlots, SLOT(slotAngle(int)));

    if (VW->rotMode) {
      makeSeparator(col, 1);
      QHBox *globRotBox = new QHBox(col);
      QLabel *globLabel = new QLabel("Global rotation", globRotBox);
      globLabel->setAlignment(Qt::AlignLeft);

      VW->globRotSpin = new FloatSpinBox(1, -1800, 1800, 10, globRotBox);
      VW->globRotSpin->setFixedWidth
        (globLabel->fontMetrics().width("-180.0000"));
      VW->globRotSpin->setValue((int)floor(VW->globalRot * 10. + 0.5));
      VW->globRotSpin->setFocusPolicy(ClickFocus);
      QObject::connect(VW->globRotSpin, SIGNAL(valueChanged(int)), 
                       VW->midasSlots, SLOT(slotGlobRot(int)));

      QCheckBox *check = new QCheckBox("Mouse shifts X only", col);
      check->setChecked(false);
      check->setFocusPolicy(NoFocus);
      QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
                       SLOT(slotConstrainMouse(bool)));
    }

  } else {
    makeSeparator(col, 2);

    VW->wMeanerr = new QLabel("Mean error: 100.00", col);
    VW->wMeanerr->setAlignment(Qt::AlignLeft);
    label = new QLabel("Top 4 errors:", col);
    label->setAlignment(Qt::AlignLeft);

    QGrid *grid = new QGrid(2, Qt::Horizontal, col);
    grid->setSpacing(5);
    QSignalMapper *mapper = new QSignalMapper(col);
    for (i = 0; i < 4; i++) {
      VW->wToperr[i] = new QPushButton("X 199: 50.00  ", grid);
      mapper->setMapping(VW->wToperr[i], i);
      QObject::connect(VW->wToperr[i], SIGNAL(clicked()), mapper, SLOT(map()));
      VW->wToperr[i]->setFocusPolicy(NoFocus);
    }
    QObject::connect(mapper, SIGNAL(mapped(int)), 
		     VW->midasSlots, SLOT(slotTop_error(int)));

    VW->wCurerr = new QLabel("This edge: -50.00, -50.00", col);
    VW->wCurerr->setAlignment(Qt::AlignLeft);
    VW->wLeaverr = new QLabel("Leave-out: -50.00, -50.00", col);
    VW->wLeaverr->setAlignment(Qt::AlignLeft);

    QPushButton *button = new QPushButton("Apply Leave-out Error", col);
    button->setFocusPolicy(NoFocus);
    QObject::connect(button, SIGNAL(clicked()), VW->midasSlots,
		     SLOT(slotLeave_out()));
  }

}

void MidasWindow::createSectionControls(QVBox *parent)
{
  QHBox *row;
  QVBox *col = parent;
  QLabel *label;
  QSignalMapper *mapper;
  int maxz;

  // Reference section text box
  if (VW->xtype != XTYPE_MONT) {
    row = new QHBox(col);
    maxz = (VW->xtype == XTYPE_XREF) ? VW->refzsize : VW->zsize;
    VW->refSpin = makeSpinBoxRow(row, "Reference Sec. ", 1, maxz);
    QObject::connect(VW->refSpin, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotRefValue(int)));
  }

  // Current section text box
  row = new QHBox(col);
  maxz = (VW->xtype == XTYPE_MONT) ? VW->maxzpiece + 1 : VW->zsize;
  VW->curSpin = makeSpinBoxRow(row, "Current Sec. ", 1, maxz);
  QObject::connect(VW->curSpin, SIGNAL(valueChanged(int)),
                   VW->midasSlots, SLOT(slotCurValue(int)));
  
  if (VW->numChunks) {

    // Chunk mode: just add a chunk spin box
    row = new QHBox(col);
    VW->chunkSpin = makeSpinBoxRow(row, "Current Chunk ", 2, VW->numChunks);
    QObject::connect(VW->chunkSpin, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotChunkValue(int)));
  
  } else if (VW->xtype != XTYPE_XREF && VW->xtype != XTYPE_MONT) {
    
    // Non-montage: the difference checkbox and mode label

    VW->difftoggle  = new QCheckBox("Keep Curr - Ref diff = 1", col);
    VW->difftoggle->setChecked(true);
    VW->difftoggle->setFocusPolicy(NoFocus);
    QObject::connect(VW->difftoggle, SIGNAL(toggled(bool)), 
		     VW->midasSlots, SLOT(slotKeepdiff(bool)));

    if (VW->xtype == XTYPE_XF)
      label = new QLabel ("Local Alignment Mode", col);
    else
      label = new QLabel ("Global Alignment Mode", col);
    label->setAlignment(Qt::AlignCenter);

  } else if (VW->xtype == XTYPE_MONT) { 

    // Make the X and Y radio buttons, and edge number textbox
    row = new QHBox(col);
    row->setSpacing(3);
    VW->edgeGroup = new QButtonGroup(2, Qt::Horizontal, row);
    VW->wXedge = new QRadioButton("X", VW->edgeGroup);
    VW->wYedge = new QRadioButton("Y", VW->edgeGroup);
    VW->wXedge->setFocusPolicy(NoFocus);
    VW->wYedge->setFocusPolicy(NoFocus);
    QObject::connect(VW->edgeGroup, SIGNAL(clicked(int)),
		     VW->midasSlots, SLOT(slotXory(int)));

    VW->edgeSpin = makeSpinBoxRow(row, "Edge ", 1, VW->maxedge[VW->xory]);
    QObject::connect(VW->edgeSpin, SIGNAL(valueChanged(int)),
                     VW->midasSlots, SLOT(slotEdgeValue(int)));

    VW->midasSlots->manage_xory(VW);
    
  } else {
    label = new QLabel("Reference Alignment Mode", col);
    label->setAlignment(Qt::AlignCenter);
  }
}

void MidasWindow::createZoomBlock(QVBox *parent)
{
  QString str;

  QSignalMapper *mapper = makeLabeledArrows(parent, "Zoom  1.00", 
					    &VW->zoomlabel, false);
  QObject::connect(mapper, SIGNAL(mapped(int)), VW->midasSlots,
		     SLOT(slotZoom(int)));

  str.sprintf("Block size %2d", VW->boxsize);
  mapper = makeLabeledArrows(parent, str, &VW->blocklabel, false);
  QObject::connect(mapper, SIGNAL(mapped(int)), VW->midasSlots,
		     SLOT(slotBlock(int)));
  
  QCheckBox *check = new QCheckBox("Interpolate", parent);
  check->setChecked(VW->fastip == 0);
  check->setFocusPolicy(NoFocus);
  QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
		     SLOT(slotInterpolate(bool)));
}

void MidasWindow::createViewToggle(QVBox *parent)
{

  VW->overlaytoggle = new QCheckBox("Overlay view", parent);
  VW->overlaytoggle->setChecked(VW->vmode == MIDAS_VIEW_COLOR);
  VW->overlaytoggle->setFocusPolicy(NoFocus);
  QObject::connect(VW->overlaytoggle, SIGNAL(toggled(bool)), VW->midasSlots,
		   SLOT(slotOverlay(bool)));

  QPushButton *button = new QPushButton("Toggle Ref/Cur", parent);
  button->setFocusPolicy(NoFocus);
  QObject::connect(button, SIGNAL(pressed()), VW->midasSlots,
		     SLOT(slotAlign_arm()));
  QObject::connect(button, SIGNAL(released()), VW->midasSlots,
		     SLOT(slotAlign_disarm()));
}

void MidasWindow::createContrastControls(QVBox *parent)
{
  QHBox *row;

  row = new QHBox(parent);
  row->setSpacing(3);
  new QLabel("Black", row);
  VW->wBlacklevel = new QSlider(0, 255, 1, 0, Qt::Horizontal, row);
  VW->wBlacklevel->setFocusPolicy(NoFocus);
  VW->wBlackval = new QLabel("000", row);
  QObject::connect(VW->wBlacklevel, SIGNAL(valueChanged(int)),
		   VW->midasSlots, SLOT(slotBlacklevel(int)));
  QObject::connect(VW->wBlacklevel, SIGNAL(sliderPressed()),
		   VW->midasSlots, SLOT(slotBlackPressed()));
  QObject::connect(VW->wBlacklevel, SIGNAL(sliderReleased()),
		   VW->midasSlots, SLOT(slotBlackReleased()));

  row = new QHBox(parent);
  row->setSpacing(3);
  new QLabel("White", row);
  VW->wWhitelevel = new QSlider(0, 255, 1, 255, Qt::Horizontal, row);
  VW->wWhitelevel->setFocusPolicy(NoFocus);
  VW->wWhiteval = new QLabel("255", row);
  QObject::connect(VW->wWhitelevel, SIGNAL(valueChanged(int)),
		   VW->midasSlots, SLOT(slotWhitelevel(int)));
  QObject::connect(VW->wWhitelevel, SIGNAL(sliderPressed()),
		   VW->midasSlots, SLOT(slotWhitePressed()));
  QObject::connect(VW->wWhitelevel, SIGNAL(sliderReleased()),
		   VW->midasSlots, SLOT(slotWhiteReleased()));
     
  QCheckBox *check = new QCheckBox("Apply to only one sec.", parent);
  check->setChecked(false);
  check->setFocusPolicy(NoFocus);
  QObject::connect(check, SIGNAL(toggled(bool)), VW->midasSlots,
		     SLOT(slotApplyone(bool)));

  VW->reversetoggle = new QCheckBox("Reverse contrast", parent);
  VW->reversetoggle->setChecked(false);
  VW->reversetoggle->setFocusPolicy(NoFocus);
  QObject::connect(VW->reversetoggle, SIGNAL(toggled(bool)), VW->midasSlots,
		     SLOT(slotReverse(bool)));
}




void midas_error(char *tmsg, char *bmsg, int retval)
{
  QString str;
  /* if (VW->midasWindow == NULL)
    fprintf(stderr, "%s %s\n", tmsg, bmsg);
    else { */
    str.sprintf("%s %s\n", tmsg, bmsg);
    dia_err((char *)str.latin1());
    //}  
  if (retval)
    exit(retval);

  return;
}

/*
    $Log$
    Revision 3.13  2004/10/25 18:51:52  mast
    Added optoin to output to different file from input file

    Revision 3.12  2004/07/12 18:42:30  mast
    Changes for chunk alignment and for switching to spin boxes

    Revision 3.11  2004/07/07 19:25:31  mast
    Changed exit(-1) to exit(3) for Cygwin

    Revision 3.10  2004/05/28 18:56:13  mast
    needed to parse gloabal rotation as float

    Revision 3.9  2003/12/17 21:44:19  mast
    Changes to implement global rotations

    Revision 3.8  2003/11/01 16:43:10  mast
    changed to put out virtually all error messages to a window

    Revision 3.7  2003/06/20 19:35:41  mast
    Connected top error buttons to mapper

    Revision 3.6  2003/05/26 01:02:33  mast
    Added label to show mouse action

    Revision 3.5  2003/02/28 21:36:08  mast
    connect to focusLost signal of ToolEdit

    Revision 3.4  2003/02/28 18:10:58  mast
    Fix include fiddling

    Revision 3.3  2003/02/27 23:06:51  mast
    Fiddling with includes some more

    Revision 3.2  2003/02/27 20:19:10  mast
    Changes in includes for Windows

    Revision 3.1  2003/02/10 20:49:57  mast
    Merge Qt source

    Revision 1.1.2.4  2003/01/30 01:10:25  mast
    Move fork to before starting application

    Revision 1.1.2.3  2003/01/26 23:20:33  mast
    using new library

    Revision 1.1.2.2  2002/12/06 19:05:01  mast
    Changes for binary file reading under windows

    Revision 1.1.2.1  2002/12/05 03:13:02  mast
    New Qt version

    Revision 3.4  2002/11/05 23:54:24  mast
    Changed to get a visual then pass it to GLw.

    Revision 3.3  2002/11/05 23:29:13  mast
    Changed to call imodCopyright

    Revision 3.2  2002/08/19 04:46:10  mast
    Changed number of columns in edge number text box to 4

*/
