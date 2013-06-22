/*  qtplax.cpp - Qt-based graphics routines for Fortran programs
 *
 *  Copyright (C) 1995-2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <locale.h>

#include "qtplax.h"
//Added by qt3to4:
#include <QCloseEvent>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QEvent>
#include <QWaitCondition>
#include <QMenu>
#include <QImage>
#include <QAction>
#include "b3dutil.h"
#ifdef QTPLAX_ATEXIT_HACK
#include <sys/types.h>
#include <unistd.h>
#endif

#include <qapplication.h>
#include <qfont.h>
#include <qdatetime.h>
#include <qpen.h>
#include <qfiledialog.h>
#include <qpainter.h>
#include <qprinter.h>
#include <qprintdialog.h>
#include <qpolygon.h>
#include <qbrush.h>
#include <qlabel.h>

#define LIST_CHUNK  1024

// The characters are too small even for Mac users
// The window needs to be smaller on Mac to allow more of screen for terminal
#ifdef Q_OS_MACX
#define TEXT_SIZE_SCALE 3.3
#define DEFAULT_HEIGHT 600
#else
#ifdef _WIN32
#define TEXT_SIZE_SCALE 2.5
#define DEFAULT_HEIGHT 540
#else
#define TEXT_SIZE_SCALE 2.25    // was 2.5
#define DEFAULT_HEIGHT 640
#endif
#endif

static QRgb  sRGB[PLAX_RAMPSIZE];
static float sScaleX;
static float sScaleY;
static b3dInt32 *sDrawList = NULL;
static int sListSize = 0;
static int sListMax = 0;
static int sOutListInd = 0;
static int sLastSize = 0;

static char *sProgName = NULL;
static QApplication *sApp = NULL;
static PlaxWindow *sPlaxWidget = NULL;
static QPainter *sPainter = NULL;
static bool sSpecialPainter = false;
static int sPlaxWidth = 5 * DEFAULT_HEIGHT / 4;
static int sPlaxHeight = DEFAULT_HEIGHT;
static int sPlaxTop = 30;
static int sPlaxLeft = 10;
static int sPlaxOpen;
static int sPlaxExposed = 0;
static int sPenColor = -1;
static int sPenWidth;
static int sBrushClosed;
static int sBrushColor = -1;
static int sNextTextAlign = 0;
static int sNoGraph = 0;
static char *sMessage = NULL;
static char *sToolTip = NULL;
static int sExitOnClose = 0;
static bool sUserScaleSet = false;
static float sUserXscale, sUserXadd, sUserYscale, sUserYadd;
static int argc;
static char **argv;

static PlaxThread *sAppThread = NULL;
static QMutex *sMutex;
static QWaitCondition *sCloseWaiter;

static int addBytesToList(char *bytes, int num);
static int addTwoArgs(int code, int *i1, int *i2);
static int addFourArgs(int code, int *i1, int *i2, int *i3, int *i4);
static int addFiveArgs(int code, int *i1, int *i2, int *i3, int *i4, int *i5);
static int addSixArgs(int code, int *i1, int *i2, int *i3, int *i4, int *i5,
                      int *i6);
static int allocate_list_chunk();
static int startPlaxApp();
static void plax_input(void);
static void plax_input_open(void);
static void plax_transform( int *x, int *y);
static short plax_transx(short ix);
static short plax_transy(short iy);
static int plax_scale(int size);
static void plax_set_pen(int color, int width);
static void plax_set_brush(int color, int closed);
static void plax_draw_vect(b3dInt32 x1, b3dInt32 y1, b3dInt32 x2, b3dInt32 y2);
static void plax_draw_box(b3dInt32 cindex, b3dInt32 x1, b3dInt32 y1,
                          b3dInt32 x2, b3dInt32 y2);
static void plax_draw_circ(b3dInt32 cindex, b3dInt32 radius, b3dInt32 x, 
                           b3dInt32 y);
static void plax_draw_poly(b3dInt32 cindex, b3dInt32 csize, b3dInt16 *vec,
                           int iffill);
static void plax_draw_text(b3dInt32 thickness,
                           b3dInt32 iysize,
                           b3dInt32 cindex,
                           b3dInt32 x, b3dInt32 y, 
                           b3dInt32 strsize, char *string);
static void draw();


#ifdef F77FUNCAP
#define fortiargc_ FORTIARGC
#define fortgetarg_ FORTGETARG
#define realgraphicsmain_ REALGRAPHICSMAIN
#endif

extern "C" {
  int fortiargc_();
  void fortgetarg_(int *i, char *string, int len);
  void realgraphicsmain_();
}

enum {PCALL_MAPCOLOR, PCALL_BOX, PCALL_BOXO, PCALL_VECT, PCALL_VECTW, PCALL_CIRC,
      PCALL_CIRCO, PCALL_POLY, PCALL_POLYO, PCALL_SCTEXT, PCALL_ALIGN};


PlaxWindow::PlaxWindow(QWidget *parent, Qt::WFlags fl) :
  QWidget(parent, fl)
{

  // Setting the background has no effect if callers clear to their desired background
  QPalette palette;
  palette.setColor(backgroundRole(), "black");
  setPalette(palette);
  mTimerID = 0;
  mRedrawCount = 0;
  mNumRedraws = 1;
  if (getenv("PLAX_REDRAWS"))
    mNumRedraws = atoi(getenv("PLAX_REDRAWS"));
  B3DCLAMP(mNumRedraws, 1, 10);
}

// Ignore close events unless exiting
void PlaxWindow::closeEvent ( QCloseEvent * e )
{
  if (sExitOnClose) {
    sAppThread->terminate();
    ::exit(0);
    //sCloseWaiter->wakeAll();
  }
  e->ignore();
}

// Paint event: Qt 4 always erases, just draw
void PlaxWindow::paintEvent ( QPaintEvent * e)
{
  sPlaxExposed = 1;
  sOutListInd = 0;
  /* fprintf(stderr, "paint %d %d %d %d\n", e->rect().top(), e->rect().left(),
     e->rect().width(), e->rect().height()); */
  draw();
}

// Resize: record size and set scale
void PlaxWindow::resizeEvent ( QResizeEvent * )
{
  //puts("resize event");
  sPlaxWidth = width();
  sPlaxHeight = height();
  sScaleX = ((float)width()) / 1280.0f;
  sScaleY = ((float)height()) / 1024.0f;

  // Make it repaint the whole thing
  sOutListInd = 0;
}

void PlaxWindow::timerEvent(QTimerEvent *)
{
  static int widthInc = 1;
  resize(sPlaxWidth + widthInc, sPlaxHeight);
  widthInc = -widthInc;
  mRedrawCount++;
  if (mRedrawCount < mNumRedraws)
    return;
  killTimer(mTimerID);
  mTimerID = 0;
  mRedrawCount = 0;
}

// Redraw signal is sent for showing, hiding, or drawing
void PlaxWindow::redrawSlot()
{
  // Printing this seems to solve problems, so let's just flush both streams
  //puts("Got signal");
  fflush(stdout);
  fflush(stderr);
  if (!sPlaxOpen)
    hide();
  else if (sPlaxOpen > 0) {
    show();
    raise();
  } else {

    // Qt4 insists on this going through a paint event, which requires complete
    // erasure anyway.  Doin a resize works, it complained about the other two ways!
    // But 9/21/11: Switched to delayed redraws and provided possibility of multiple 
    // redraws
    if (!mTimerID)
      mTimerID = startTimer(10);
    mRedrawCount = 0;
    /* QCoreApplication::postEvent(sPlaxWidget, new QPaintEvent
       (QRect(0, 0, sPlaxWidth, sPlaxHeight))); */
    /* QPaintEvent event = QPaintEvent(QRect(0, 0, sPlaxWidth, sPlaxHeight));
       paintEvent(&event); */
    sPlaxOpen = 1;
  }
  QApplication::processEvents();
}

void PlaxWindow::lock()
{
  sMutex->lock();
}
void PlaxWindow::unlock()
{
  sMutex->unlock();
}

void PlaxWindow::mousePressEvent(QMouseEvent * e )
{
  if (e->buttons() & Qt::LeftButton && sUserScaleSet) {
    QMenu popup(this);
    QString str;
    float xx = e->x() / sScaleX;
    float yy = 1023. - e->y() / sScaleY;
    str = QString("%1, %2").arg((xx - sUserXadd) / sUserXscale, 0, 'g', 3).
      arg((yy - sUserYadd) / sUserYscale, 0, 'g', 3);
    popup.addAction(str);
    popup.exec(QCursor::pos());
    return;
  }

  if (!(e->buttons() & Qt::RightButton))
    return;
  QMenu popup(this);
  QAction *savePng = popup.addAction("Save to PNG");
  QAction *print = popup.addAction("Print");
  connect(savePng, SIGNAL(triggered(bool)), this, SLOT(savePNGslot(bool)));
  connect(print, SIGNAL(triggered(bool)), this, SLOT(printSlot(bool)));
  popup.exec(QCursor::pos());
}

void PlaxWindow::savePNGslot(bool state)
{
  QImage *image = new QImage(sPlaxWidth, sPlaxHeight, QImage::Format_RGB32);
  sSpecialPainter = true;
  sPainter = new QPainter(image);
  sOutListInd = 0;
  QCoreApplication::postEvent(sPlaxWidget, new QPaintEvent
                              (QRect(0, 0, sPlaxWidth, sPlaxHeight)));
  QApplication::processEvents();
  QString filename = QFileDialog::getSaveFileName(this, "File name for saving as PNG");
  if (!filename.endsWith(".png", Qt::CaseInsensitive))
    filename += ".png";
  image->save(filename, "PNG");
  delete sPainter;
  delete image;
  sSpecialPainter = false;
}
void PlaxWindow::printSlot(bool state)
{
  QPrinter printer;
#if QT_VERSION >= 0x040400
  printer.setPaperSize(QPrinter::Letter);
#endif
  QPrintDialog *dialog = new QPrintDialog(&printer, this);
  dialog->setWindowTitle(tr("Print Graph"));
  if (dialog->exec() != QDialog::Accepted)
    return;
  sSpecialPainter = true;
  sPainter = new QPainter(&printer);
  sOutListInd = 0;
  QCoreApplication::postEvent(sPlaxWidget, new QPaintEvent
                              (QRect(0, 0, sPlaxWidth, sPlaxHeight)));
  QApplication::processEvents();
  delete sPainter;
  sSpecialPainter = false;

}

// The thread class
PlaxThread::PlaxThread()
{
  QThread::start();
  QObject::connect(this, SIGNAL(redraw()), sPlaxWidget, SLOT(redrawSlot()));
}

// start the fortran in the second thread
void PlaxThread::run()
{
  realgraphicsmain_();
  ::exit(0);
}

void PlaxThread::sendSignal()
{
  emit redraw();
}

#define FSTRING_LEN  80
void plax_initialize(char *string, int strsize)
{
  char fstring[FSTRING_LEN];
  char *style = "-style=windows";
  int i;

  sProgName = f2cString(string, strsize);

  // Get the arguments.  Fortran numbers them 0 to iargc()
  argc = fortiargc_() + 1;
  argv = (char **)malloc((argc + 1) * sizeof(char *));
  if (!argv) {
    fprintf(stderr, "ERROR: %s - getting memory for program arguments.\n", sProgName);
    exit (3);
  }
  
  for (i = 0; i < argc; i++) {
    fortgetarg_(&i, fstring, FSTRING_LEN);
    
    argv[i] = f2cString(fstring, FSTRING_LEN);
    if (!argv[i]) {
      fprintf(stderr, "ERROR: %s - getting memory for program arguments.\n", sProgName);
      exit (3);
    }
  }

  argv[argc] = strdup(style);
  if (argv[argc])
    argc++;

  // Look for -s and -p arguments
  for (i = 1; i < argc - 1; i++) {
    if (!strcmp("-s", argv[i])) {
      sscanf(argv[++i], "%d%*c%d", &sPlaxWidth, &sPlaxHeight);
      continue;
    }
    if (!strcmp("-p", argv[i])) {
      sscanf(argv[++i], "%d%*c%d", &sPlaxLeft, &sPlaxTop);
    }

    // Get arguments for tooltip or message box
    if (!strcmp("-message", argv[i])) {
      sMessage = strdup(argv[++i]);
    }
    if (!strcmp("-tooltip", argv[i])) {
      sToolTip = strdup(argv[++i]);
    }

    // Also look for an argument to avoid trying to start Qt app
    if (!strcmp("-nograph", argv[i])) {
      sNoGraph = 1;
      realgraphicsmain_();
      exit(0);
    }
  }

  // Otherwise start the Qt application and start second thread that calls
  // Fortran
  sMutex = new QMutex();
  sCloseWaiter = new QWaitCondition();

  if (startPlaxApp())
    exit (3);
  sAppThread = new PlaxThread();

  sApp->exec();

  exit(0);
}


int plax_open(void)
{
  if (sNoGraph)
    return 0;
  sPlaxOpen = 1;

  // Qt in main thread: just show the widget now
  //puts("posting event");
  sAppThread->sendSignal();
  return 0;
}


// "Close" is really hiding the widget directly or via event
void plax_close(void)
{
  if (sNoGraph)
    return;
  sPlaxOpen = 0;
  sAppThread->sendSignal();
}

// To flush the display, post a paint event without erasure, specifying the
// whole region
void plax_flush(void)
{
  if (sNoGraph)
    return;
  // Could not make it draw reliably except when the window resized, 
  // just surrender to resizing the window on every draw.
  // Check this out on a new version of Qt
  // 5/12/06: Linux got worse and worse so surrendered to making this universal
  // (It used to be just win32)
  sPlaxOpen = -1;
  //puts("posting event");
  sAppThread->sendSignal();
  /*#else
    QThread::postEvent(sPlaxWidget, new QPaintEvent
    (QRect(0, 0, sPlaxWidth, sPlaxHeight), false));
    #endif */
}

void plax_wait_for_close(void)
{
  QMutex mutex;
  sExitOnClose = 1;
  sCloseWaiter->wait(&mutex);
}

// Under Linux, it hangs on a Ctrl C, so kill the process group
#ifdef QTPLAX_ATEXIT_HACK
static void exitQAppOnExit()
{
  char buf[64];
  pid_t pid = getpid();
  sprintf(buf, "/bin/kill -9 -%u", pid);
  system(buf);
}
#endif

// Start the Qt application and get the drawing widget
static int startPlaxApp()
{
  sApp = new QApplication(argc, argv);
  setlocale(LC_NUMERIC, "C");

#ifdef QTPLAX_ATEXIT_HACK
  atexit(exitQAppOnExit);
#endif

  //    for (i = 0; i < argc; i++)
  //  printf("%d %d - %s\n", i, strlen(argv[i]),argv[i]);
  
  // IT WAS NOT OK TO Free the memory
  
  sPlaxWidget = new PlaxWindow(NULL);
     
  if (!sPlaxWidget){
    fprintf(stderr, "Error opening plax display.\n");
    return (-1);
  }

  sScaleX = sScaleY = 0.5f;
  sPlaxWidget->setGeometry(sPlaxLeft, sPlaxTop, sPlaxWidth, sPlaxHeight);
  sPlaxWidget->setAttribute(Qt::WA_DeleteOnClose);
  sPlaxWidget->setWindowTitle(QString(sProgName) + "   (Left click for position, "
                              "right click to save as PNG or print)");
  sPlaxExposed = 0;
  if (sToolTip)
    sPlaxWidget->setToolTip(sToolTip);
  if (sMessage) {
    QLabel *label = new QLabel(sMessage, NULL);
    label->move(sPlaxLeft + sPlaxWidth + 10, sPlaxTop + sPlaxHeight / 2);
    label->show();
  }
  return 0;
}

/*
 * The routines called from Fortran to put the drawing commands on the stack
 */
void plax_mapcolor(int *color, int *ired, int *igreen, int *iblue)
{
  addFourArgs(PCALL_MAPCOLOR, color, ired, igreen, iblue);
}

void plax_box(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  addFiveArgs(PCALL_BOX, cindex, ix1, iy1, ix2, iy2);
}

void plax_boxo(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  addFiveArgs(PCALL_BOXO, cindex, ix1, iy1, ix2, iy2);
}

void plax_vect(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  addFiveArgs(PCALL_VECT, cindex, ix1, iy1, ix2, iy2);
}

void plax_vectw(int *linewidth, int *cindex, 
                int *ix1, int *iy1, int *ix2, int *iy2)
{
  if (sNoGraph)
    return;
  sPlaxWidget->lock();
  addSixArgs(PCALL_VECTW, linewidth, cindex, ix1, iy1, ix2, iy2);
  sPlaxWidget->unlock();
}

/* filled circle */
void plax_circ(int *cindex, int *radius, int *ix, int *iy)
{
  addFourArgs(PCALL_CIRC, cindex, radius, ix, iy);
}

/* open circle */
void plax_circo(int *cindex, int *radius, int *ix, int *iy)
{
  addFourArgs(PCALL_CIRCO, cindex, radius, ix, iy);
}

/* closed filled polygon */
void plax_poly(int *cindex, int *size, b3dInt16 *vec)
{
  if (sNoGraph)
    return;
  sPlaxWidget->lock();
  if (!addTwoArgs(PCALL_POLY, cindex, size))
    addBytesToList((char *)vec, 4 * *size);
  sPlaxWidget->unlock();
}

void plax_polyo(int *cindex, int *size, b3dInt16 *vec)
{
  if (sNoGraph)
    return;
  sPlaxWidget->lock();
  if (!addTwoArgs(PCALL_POLYO, cindex, size))
      addBytesToList((char *)vec, 4 * *size);
  sPlaxWidget->unlock();
}

void plax_sctext(int *thickness,
                 int *xsize,
                 int *iysize,
                 int *cindex,
                 int *ix, int *iy, 
                 char *string, int strsize
                 )
{
  if (sNoGraph)
    return;
  sPlaxWidget->lock();
  if (!addSixArgs(PCALL_SCTEXT, thickness, iysize, cindex, ix, iy, 
                  &strsize))
    addBytesToList(string, strsize);
  sPlaxWidget->unlock();
}

void plax_next_text_align(int *type)
{
  if (sNoGraph)
    return;
  if (sListSize + 4 > sListMax)
    if (allocate_list_chunk())
      return;
  sDrawList[sListSize++] = PCALL_ALIGN;
  sDrawList[sListSize++] = *type;
}

void plax_drawing_scale(float *xscale, float *xadd, float *yscale, float *yadd)
{
  sUserScaleSet = true;
  sUserXscale = *xscale;
  sUserXadd = *xadd;
  sUserYscale = *yscale;
  sUserYadd = *yadd;
}

void plax_erase()
{
  sOutListInd = 0;
  sListSize = 0;
}


void plax_putc(char *f)
{
  putchar(*f);
}

/*****************************************************************************/
/* Internal Functions                                                        */
/*****************************************************************************/


static int addBytesToList(char *bytes, int num)
{
  int numInt = (num + 3) / 4;
  if (sListSize + numInt > sListMax) {
    while (sListSize + numInt > sListMax) {
      if (allocate_list_chunk())
        return 1;
    }
  }
  memcpy(&sDrawList[sListSize], bytes, num);
    
  sListSize += numInt;
  return 0;
}

static int addTwoArgs(int code, int *i1, int *i2)
{
  if (sListSize + 4 > sListMax)
    if (allocate_list_chunk())
      return 1;
  sDrawList[sListSize++] = code;
  sDrawList[sListSize++] = *i1;
  sDrawList[sListSize++] = *i2;
  return 0;
}

static int addFourArgs(int code, int *i1, int *i2, int *i3, int *i4)
{
  if (sNoGraph)
    return 0;
  if (sListSize + 5 > sListMax)
    if (allocate_list_chunk())
      return 1;
  sPlaxWidget->lock();
  sDrawList[sListSize++] = code;
  sDrawList[sListSize++] = *i1;
  sDrawList[sListSize++] = *i2;
  sDrawList[sListSize++] = *i3;
  sDrawList[sListSize++] = *i4;
  sPlaxWidget->unlock();
  return 0;
}

static int addFiveArgs(int code, int *i1, int *i2, int *i3, int *i4, int *i5)
{
  if (sNoGraph)
    return 0;
  if (sListSize + 6 > sListMax)
    if (allocate_list_chunk())
      return 1;
  sPlaxWidget->lock();
  sDrawList[sListSize++] = code;
  sDrawList[sListSize++] = *i1;
  sDrawList[sListSize++] = *i2;
  sDrawList[sListSize++] = *i3;
  sDrawList[sListSize++] = *i4;
  sDrawList[sListSize++] = *i5;
  sPlaxWidget->unlock();
  return 0;
}

static int addSixArgs(int code, int *i1, int *i2, int *i3, int *i4, int *i5,
                       int *i6)
{
  if (sListSize + 7 > sListMax)
    if (allocate_list_chunk())
      return 1;
  sDrawList[sListSize++] = code;
  sDrawList[sListSize++] = *i1;
  sDrawList[sListSize++] = *i2;
  sDrawList[sListSize++] = *i3;
  sDrawList[sListSize++] = *i4;
  sDrawList[sListSize++] = *i5;
  sDrawList[sListSize++] = *i6;
  return 0;
}

static int allocate_list_chunk()
{
  if (sListMax > 0)
    sDrawList = (b3dInt32 *)realloc(sDrawList, 4 * (sListMax + LIST_CHUNK));
  else
    sDrawList = (b3dInt32 *)malloc(4 * LIST_CHUNK);
  sListMax += LIST_CHUNK;
  if (sDrawList)
    return 0;
  fprintf(stderr, "QTPLAX: Error getting memory for drawing list.\n");
  sListSize = 0;
  sListMax = 0;
  sOutListInd = 0;
  return 1;
}

static void draw()
{
  int ind;
  
  if (!sSpecialPainter)
    sPainter = new QPainter(sPlaxWidget);
  sLastSize = 0;
  sPlaxWidget->lock();

  /* fprintf(stderr, "Ind %d Size %d\n", sOutListInd, sListSize); */
  
  // Draw starting after the last item drawn
  while (sOutListInd < sListSize) {

    ind = sOutListInd++;
    switch (sDrawList[ind++]) {
    case PCALL_MAPCOLOR:
      sRGB[sDrawList[ind]] = qRgb(sDrawList[ind + 1], sDrawList[ind + 2], 
                                    sDrawList[ind + 3]);
      sPenColor = -1;
      sBrushColor = -1;
      sOutListInd += 4;
      break;

    case PCALL_BOX:
      plax_set_brush(sDrawList[ind], 1);
      plax_draw_box(sDrawList[ind], sDrawList[ind + 1], sDrawList[ind + 2], 
                    sDrawList[ind + 3], sDrawList[ind + 4]);
      sOutListInd += 5;
      break;

    case PCALL_BOXO:
      plax_set_brush(sDrawList[ind], 0);
      plax_draw_box(sDrawList[ind], sDrawList[ind + 1], sDrawList[ind + 2], 
                    sDrawList[ind + 3], sDrawList[ind + 4]);
      sOutListInd += 5;
      break;

    case PCALL_VECT:
      plax_set_pen(sDrawList[ind], 0);
      plax_draw_vect(sDrawList[ind + 1], sDrawList[ind + 2], sDrawList[ind + 3],
                     sDrawList[ind + 4]);
      sOutListInd += 5;
      break;

    case PCALL_VECTW:
      plax_set_pen(sDrawList[ind + 1], sDrawList[ind]);
      plax_draw_vect(sDrawList[ind + 2], sDrawList[ind + 3], sDrawList[ind + 4],
                     sDrawList[ind + 5]);
      sOutListInd += 6;
      break;

    case PCALL_CIRC:
      plax_set_brush(sDrawList[ind], 1);
      plax_draw_circ(sDrawList[ind], sDrawList[ind + 1], sDrawList[ind + 2],
                     sDrawList[ind + 3]);
      sOutListInd += 4;
      break;

    case PCALL_CIRCO:
      plax_set_brush(sDrawList[ind], 0);
      plax_draw_circ(sDrawList[ind], sDrawList[ind + 1], sDrawList[ind + 2],
                     sDrawList[ind + 3]);
      sOutListInd += 4;
      break;

    case PCALL_POLY:
      plax_draw_poly(sDrawList[ind], sDrawList[ind + 1], 
                     (b3dInt16 *)(&sDrawList[ind + 2]), 1);
      break;

    case PCALL_POLYO:
      plax_draw_poly(sDrawList[ind], sDrawList[ind + 1], 
                     (b3dInt16 *)(&sDrawList[ind + 2]), 0);
      break;

    case PCALL_SCTEXT:
      plax_draw_text(sDrawList[ind], sDrawList[ind + 1], sDrawList[ind + 2],
                     sDrawList[ind + 3], sDrawList[ind + 4], sDrawList[ind + 5],
                     (char *)(&sDrawList[ind + 6]));
      sOutListInd += 6 + (sDrawList[ind + 5] + 3) / 4;
      break;

    case PCALL_ALIGN:
      sNextTextAlign = sDrawList[ind];
      sOutListInd += 1;
      break;
    }

  }
  sPlaxWidget->unlock();
  //sPainter->flush();
  if (!sSpecialPainter)
    delete sPainter;
  //  plax_input();
}


static void plax_draw_box(b3dInt32 cindex, b3dInt32 x1, b3dInt32 y1,
                          b3dInt32 x2, b3dInt32 y2)
{
  int x, y, width, height;

  plax_set_pen(cindex, 0);
  plax_transform(&x1, &y1);
  plax_transform(&x2, &y2);
     
  x = (x1 > x2) ? x2 : x1;
  y = (y1 > y2) ? y2 : y1;
  width = x1 - x2;
  height = y1 - y2;

  if (width < 0)
    width *= -1;

  if (height < 0)
    height *= -1;
  width += 1;
  height += 1;

  sPainter->drawRect(x, y, width, height);

#ifdef PLAX_DEBUG
  printf("draw box (%d, %d, %d, %d)\n",
	 x1, y1, x2, y2);
#endif

}

static void plax_draw_circ(b3dInt32 cindex, b3dInt32 radius, b3dInt32 x, 
                           b3dInt32 y)
{
  int size = plax_scale(radius);
  plax_set_pen(cindex, 0);

  plax_transform(&x, &y);
  sPainter->drawEllipse(x - size, y - size, size * 2, size * 2);
}

static void plax_draw_vect(b3dInt32 x1, b3dInt32 y1, b3dInt32 x2, b3dInt32 y2)
{
  plax_transform(&x1, &y1);
  plax_transform(&x2, &y2);
  sPainter->drawLine(x1, y1, x2, y2);
}

static void plax_draw_poly(b3dInt32 cindex, b3dInt32 csize, b3dInt16 *vec,
                           int iffill)
{
  int i;
  QPolygon points(csize);
  
  plax_set_pen(cindex, 0);
  plax_set_brush(cindex, iffill);
  
  for (i = 0; i < csize; i++)
    points.setPoint(i, (int)plax_transx(vec[i * 2]), 
                    (int)plax_transy(vec[(i * 2) + 1]));
  
  if (iffill)
    sPainter->drawConvexPolygon(points);
  else
    sPainter->drawPolygon(points);
  
  sOutListInd += 2 + csize;
}

static void plax_draw_text(b3dInt32 thickness,
                           b3dInt32 iysize,
                           b3dInt32 cindex,
                           b3dInt32 x, b3dInt32 y, 
                           b3dInt32 strsize, char *string)

{
  static int lastbold;
  int width, height, align, maxWidth, maxHeight;

  int ysize = (int)(iysize * TEXT_SIZE_SCALE);
  int ifbold = 0;
  char *cstring = f2cString(string, strsize);

  if (!cstring)
    return;

#ifdef PLAX_DEBUG
  printf("draw text %d %d %d %d %d %s\n", thickness, iysize, cindex, x, y,
         cstring);
#endif

  if (thickness > 1)
    ifbold = 1;

  ysize = plax_scale(ysize);
  if (ysize <= 1)
    ysize = 1;

  if (sLastSize != ysize || lastbold != ifbold) {
    //QFont newFont("courier", ysize);
    QFont newFont;
    newFont.setPointSize(ysize);
    newFont.setStyleHint(QFont::TypeWriter);
    newFont.setItalic(false);
    newFont.setBold(ifbold != 0);
    sPainter->setFont(newFont);
  }
  lastbold = ifbold;
  sLastSize = ysize;

  plax_transform(&x, &y);
  plax_set_pen(cindex, 0);
  
  if (sNextTextAlign % 10) {
    maxWidth = 2 * B3DMIN(x, sPlaxWidth - x);
    maxHeight = 2 * B3DMIN(y - 1, sPlaxHeight - y - 1);
    switch (sNextTextAlign % 10) {
    case 1:  // Top center
      align = Qt::AlignHCenter | Qt::AlignTop;
      width = B3DMIN(maxWidth, sPlaxWidth / 2);
      height = B3DMIN(sPlaxHeight - 1 - y, sPlaxHeight / 2);
      x -= width / 2;
      break;

    case 2: // right center
      align = Qt::AlignVCenter | Qt::AlignRight;
      width = B3DMIN(x - 1, sPlaxWidth / 2);
      height = B3DMIN(maxHeight, sPlaxHeight / 2);
      x -= width;
      y -= height / 2;
      break;

    case 3:  // Bottom center
      align = Qt::AlignHCenter | Qt::AlignBottom;
      width = B3DMIN(maxWidth, sPlaxWidth / 2);
      height = B3DMIN(y - 1, sPlaxHeight / 2);
      x -= width / 2;
      y -= height;
      break;

    case 4:  // left center
      align = Qt::AlignVCenter | Qt::AlignLeft;
      width = B3DMIN(sPlaxWidth - x - 1, sPlaxWidth / 2);
      height = B3DMIN(maxHeight, sPlaxHeight / 2);
      y -= height / 2;
      break;

    default: // all centered
      align = Qt::AlignVCenter | Qt::AlignHCenter;
      height = B3DMIN(maxHeight, sPlaxHeight / 2);
      width = B3DMIN(maxWidth, sPlaxWidth / 2);
      x -= width / 2;
      y -= height / 2;
      break;
    }
    if (sNextTextAlign >= 10) 
      align |= Qt::TextWordWrap;
    sPainter->drawText(x, y, width, height, align, QString(cstring));
    //printf("%d %d %d %d %d %s\n", x,y, width, height, align,cstring);
    sNextTextAlign = 0;

  } else {
    //printf("%d %d %s\n", x,y, cstring);
    sPainter->drawText(x, y, QString(cstring));
  }
  fflush(stdout);
  free(cstring);
}

static int plax_scale(int size)
{
  int nsize;
  if (sScaleX > sScaleY)
    nsize = (int)(size * sScaleY);
  else
    nsize = (int)(size * sScaleX);

  if (nsize < 1)
    nsize = 1;
  return(nsize);
}


static void plax_input(void)
{
  qApp->processEvents();
}

static void plax_input_open()
{
  /* Process events until expose happens */
  while (!sPlaxExposed){
    qApp->processEvents();
  } 

  plax_input();
}


static short plax_transx(short ix)
{
  float x;
  x = ((float)ix) * sScaleX;
  return((short)x);
}

static short plax_transy(short iy)
{
  float y;
  y = sScaleY * (1023.0f - iy);
  return((short)y);
}


static void plax_transform( int *x, int *y)
{
  float ix = *x; 
  float iy = *y;

  *x = (int)(sScaleX * ix);
  *y = (int)(sScaleY * (1023.0 - iy));
  /*
    printf("transform (%g, %g) -> (%d, %d) : scale %g %g\n",
    ix, iy, *x, *y, sScaleX, sScaleY);
  */
  return;
}

static void plax_set_pen(int color, int width)
{
  if (color == sPenColor && width == sPenWidth)
    return;
  sPainter->setPen(QPen(QColor(sRGB[color]), width));
  sPenColor = color;
  sPenWidth = width;
}

static void plax_set_brush(int color, int closed)
{
  if ((!closed || color == sBrushColor) && closed == sBrushClosed)
    return;
  if (closed)
    sPainter->setBrush(QColor(sRGB[color]));
  else
    sPainter->setBrush(Qt::NoBrush);
  sBrushColor = color;
  sBrushClosed = closed;
}  
