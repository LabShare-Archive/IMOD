/*  $Author$

$Date$

$Revision$

Log at end
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "qtplax.h"
//Added by qt3to4:
#include <QCloseEvent>
#include <QPaintEvent>
#include <QResizeEvent>
#include <QEvent>
#include "b3dutil.h"
#ifdef QTPLAX_ATEXIT_HACK
#include <sys/types.h>
#include <unistd.h>
#endif

#include <qapplication.h>
#include <qfont.h>
#include <qdatetime.h>
#include <qpen.h>
#include <qpainter.h>
#include <qpolygon.h>
#include <qbrush.h>

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
#define TEXT_SIZE_SCALE 2.5
#define DEFAULT_HEIGHT 640
#endif
#endif

static QRgb  PlaxRGB[PLAX_RAMPSIZE];
static float PlaxScaleX;
static float PlaxScaleY;
static b3dInt32 *PlaxList = NULL;
static int ListSize = 0;
static int ListMax = 0;
static int OutListInd = 0;

static char *PlaxProgName = NULL;
static QApplication *PlaxApp = NULL;
static PlaxWindow *PlaxWidget = NULL;
static QPainter *PlaxPainter = NULL;
static int PlaxWidth = 5 * DEFAULT_HEIGHT / 4;
static int PlaxHeight = DEFAULT_HEIGHT;
static int PlaxTop = 30;
static int PlaxLeft = 10;
static int Plax_open;
static int Plax_exposed = 0;
static int PlaxPenColor = -1;
static int PlaxPenWidth;
static int PlaxBrushClosed;
static int PlaxBrushColor = -1;
static int PlaxNoGraph = 0;
static int argc;
static char **argv;

static PlaxThread *AppThread = NULL;
static QMutex *PlaxMutex;

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

enum {PLAX_MAPCOLOR, PLAX_BOX, PLAX_BOXO, PLAX_VECT, PLAX_VECTW, PLAX_CIRC,
      PLAX_CIRCO, PLAX_POLY, PLAX_POLYO, PLAX_SCTEXT};


PlaxWindow::PlaxWindow(QWidget *parent, Qt::WFlags fl) :
  QWidget(parent, fl)
{
  QPalette palette;
  palette.setColor(backgroundRole(), "black");
  setPalette(palette);
}

// Ignore close events
void PlaxWindow::closeEvent ( QCloseEvent * e )
{
  e->ignore();
}

// Paint event: make sure painter existes, Qt 4 always erases
void PlaxWindow::paintEvent ( QPaintEvent * e)
{
  Plax_exposed = 1;
  OutListInd = 0;
  /*fprintf(stderr, "paint %d %d %d %d\n", e->rect().top(), e->rect().left(),
    e->rect().width(), e->rect().height()); */
  draw();
}

// Resize: record size and set scale
void PlaxWindow::resizeEvent ( QResizeEvent * )
{
  //puts("resize event");
  PlaxWidth = width();
  PlaxHeight = height();
  PlaxScaleX = ((float)width()) / 1280.0f;
  PlaxScaleY = ((float)height()) / 1024.0f;

  // Make it repaint the whole thing
  // Get a new painter to fit the new size
  OutListInd = 0;
}

// Redraw signal is sent for showing, hiding, or drawing
void PlaxWindow::redrawSlot()
{
  static int widthInc = 1;
  //puts("Got signal");
  if (!Plax_open)
    hide();
  else if (Plax_open > 0) {
    show();
    raise();
  } else {

    // Qt4 insists on this going through a paint event, which requires complete
    // erasure anyway.  This works, it complained about the other two ways!
    resize(PlaxWidth + widthInc, PlaxHeight);
    widthInc = -widthInc;
    /* QCoreApplication::postEvent(PlaxWidget, new QPaintEvent
       (QRect(0, 0, PlaxWidth, PlaxHeight))); */
    /* QPaintEvent event = QPaintEvent(QRect(0, 0, PlaxWidth, PlaxHeight));
       paintEvent(&event); */
    Plax_open = 1;
  }
}

void PlaxWindow::lock()
{
  PlaxMutex->lock();
}
void PlaxWindow::unlock()
{
  PlaxMutex->unlock();
}

// The thread class
PlaxThread::PlaxThread()
{
  QThread::start();
  QObject::connect(this, SIGNAL(redraw()), PlaxWidget, SLOT(redrawSlot()));
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
  int i, j;

  PlaxProgName = f2cString(string, strsize);

  // Get the arguments.  Fortran numbers them 0 to iargc()
  argc = fortiargc_() + 1;
  argv = (char **)malloc((argc + 1) * sizeof(char *));
  if (!argv) {
    fprintf(stderr, "ERROR: %s - getting memory for program arguments.\n",
            PlaxProgName);
    exit (3);
  }
  
  for (i = 0; i < argc; i++) {
    fortgetarg_(&i, fstring, FSTRING_LEN);
    
    argv[i] = f2cString(fstring, FSTRING_LEN);
    if (!argv[i]) {
      fprintf(stderr, "ERROR: %s - getting memory for program arguments.\n",
              PlaxProgName);
      exit (3);
    }
  }

  argv[argc] = strdup(style);
  if (argv[argc])
    argc++;

  // Look for -s and -p arguments
  for (i = 1; i < argc - 1; i++) {
    if (!strcmp("-s", argv[i])) {
      sscanf(argv[++i], "%d%*c%d", &PlaxWidth, &PlaxHeight);
      continue;
    }
    if (!strcmp("-p", argv[i])) {
      sscanf(argv[++i], "%d%*c%d", &PlaxLeft, &PlaxTop);
    }

    // Also look for an argument to avoid trying to start Qt app
    if (!strcmp("-nograph", argv[i])) {
      PlaxNoGraph = 1;
      realgraphicsmain_();
      exit(0);
    }
  }

  // Otherwise start the Qt application and start second thread that calls
  // Fortran
  PlaxMutex = new QMutex();

  if (startPlaxApp())
    exit (3);
  AppThread = new PlaxThread();

  PlaxApp->exec();

  exit(0);
}


int plax_open(void)
{
  if (PlaxNoGraph)
    return 0;
  Plax_open = 1;

  // Qt in main thread: just show the widget now
  //puts("posting event");
  AppThread->sendSignal();
  return 0;
}


// "Close" is really hiding the widget directly or via event
void plax_close(void)
{
  if (PlaxNoGraph)
    return;
  Plax_open = 0;
  AppThread->sendSignal();
}

// To flush the display, post a paint event without erasure, specifying the
// whole region
void plax_flush(void)
{
  if (PlaxNoGraph)
    return;
  // Could not make it draw reliably except when the window resized, just
  // just surrender to resizing the window on every draw.
  // Check this out on a new version of Qt
  // 5/12/06: Linux got worse and worse so surrendered to making this universal
  // (It used to be just win32)
  Plax_open = -1;
  //puts("posting event");
  AppThread->sendSignal();
  /*#else
    QThread::postEvent(PlaxWidget, new QPaintEvent
    (QRect(0, 0, PlaxWidth, PlaxHeight), false));
    #endif */
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
  PlaxApp = new QApplication(argc, argv);

#ifdef QTPLAX_ATEXIT_HACK
  atexit(exitQAppOnExit);
#endif

  //    for (i = 0; i < argc; i++)
  //  printf("%d %d - %s\n", i, strlen(argv[i]),argv[i]);
  
  // IT WAS NOT OK TO Free the memory
  
  PlaxWidget = new PlaxWindow(NULL);
     
  if (!PlaxWidget){
    fprintf(stderr, "Error opening plax display.\n");
    return (-1);
  }

  PlaxScaleX = PlaxScaleY = 0.5f;
  PlaxWidget->setGeometry(PlaxLeft, PlaxTop, PlaxWidth, PlaxHeight);
  PlaxWidget->setAttribute(Qt::WA_DeleteOnClose);
  Plax_exposed = 0;
  return 0;
}

/*
 * The routines called from Fortran to put the drawing commands on the stack
 */
void plax_mapcolor(int *color, int *ired, int *igreen, int *iblue)
{
  addFourArgs(PLAX_MAPCOLOR, color, ired, igreen, iblue);
}

void plax_box(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  addFiveArgs(PLAX_BOX, cindex, ix1, iy1, ix2, iy2);
}

void plax_boxo(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  addFiveArgs(PLAX_BOXO, cindex, ix1, iy1, ix2, iy2);
}

void plax_vect(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  addFiveArgs(PLAX_VECT, cindex, ix1, iy1, ix2, iy2);
}

void plax_vectw(int *linewidth, int *cindex, 
                int *ix1, int *iy1, int *ix2, int *iy2)
{
  PlaxWidget->lock();
  addSixArgs(PLAX_VECTW, linewidth, cindex, ix1, iy1, ix2, iy2);
  PlaxWidget->unlock();
}

/* filled circle */
void plax_circ(int *cindex, int *radius, int *ix, int *iy)
{
  addFourArgs(PLAX_CIRC, cindex, radius, ix, iy);
}

/* open circle */
void plax_circo(int *cindex, int *radius, int *ix, int *iy)
{
  addFourArgs(PLAX_CIRCO, cindex, radius, ix, iy);
}

/* closed filled polygon */
void plax_poly(int *cindex, int *size, b3dInt16 *vec)
{
  PlaxWidget->lock();
  if (!addTwoArgs(PLAX_POLY, cindex, size))
    addBytesToList((char *)vec, 4 * *size);
  PlaxWidget->unlock();
}

void plax_polyo(int *cindex, int *size, b3dInt16 *vec)
{
  PlaxWidget->lock();
  if (!addTwoArgs(PLAX_POLYO, cindex, size))
      addBytesToList((char *)vec, 4 * *size);
  PlaxWidget->unlock();
}

void plax_sctext(int *thickness,
                 int *xsize,
                 int *iysize,
                 int *cindex,
                 int *ix, int *iy, 
                 char *string, int strsize
                 )
{
  PlaxWidget->lock();
  if (!addSixArgs(PLAX_SCTEXT, thickness, iysize, cindex, ix, iy, 
                  &strsize))
    addBytesToList(string, strsize);
  PlaxWidget->unlock();
}

void plax_erase()
{
  OutListInd = 0;
  ListSize = 0;
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
  char dummy[1024];
  int numInt = (num + 3) / 4;
  if (ListSize + numInt > ListMax) {
    while (ListSize + numInt > ListMax) {
      if (allocate_list_chunk())
        return 1;
    }
  }
  memcpy(&PlaxList[ListSize], bytes, num);
    
  ListSize += numInt;
  return 0;
}

static int addTwoArgs(int code, int *i1, int *i2)
{
  if (ListSize + 4 > ListMax)
    if (allocate_list_chunk())
      return 1;
  PlaxList[ListSize++] = code;
  PlaxList[ListSize++] = *i1;
  PlaxList[ListSize++] = *i2;
  return 0;
}

static int addFourArgs(int code, int *i1, int *i2, int *i3, int *i4)
{
  if (ListSize + 5 > ListMax)
    if (allocate_list_chunk())
      return 1;
  PlaxWidget->lock();
  PlaxList[ListSize++] = code;
  PlaxList[ListSize++] = *i1;
  PlaxList[ListSize++] = *i2;
  PlaxList[ListSize++] = *i3;
  PlaxList[ListSize++] = *i4;
  PlaxWidget->unlock();
  return 0;
}

static int addFiveArgs(int code, int *i1, int *i2, int *i3, int *i4, int *i5)
{
  if (ListSize + 6 > ListMax)
    if (allocate_list_chunk())
      return 1;
  PlaxWidget->lock();
  PlaxList[ListSize++] = code;
  PlaxList[ListSize++] = *i1;
  PlaxList[ListSize++] = *i2;
  PlaxList[ListSize++] = *i3;
  PlaxList[ListSize++] = *i4;
  PlaxList[ListSize++] = *i5;
  PlaxWidget->unlock();
  return 0;
}

static int addSixArgs(int code, int *i1, int *i2, int *i3, int *i4, int *i5,
                       int *i6)
{
  if (ListSize + 7 > ListMax)
    if (allocate_list_chunk())
      return 1;
  PlaxList[ListSize++] = code;
  PlaxList[ListSize++] = *i1;
  PlaxList[ListSize++] = *i2;
  PlaxList[ListSize++] = *i3;
  PlaxList[ListSize++] = *i4;
  PlaxList[ListSize++] = *i5;
  PlaxList[ListSize++] = *i6;
  return 0;
}

static int allocate_list_chunk()
{
  if (ListMax > 0)
    PlaxList = (b3dInt32 *)realloc(PlaxList, 4 * (ListMax + LIST_CHUNK));
  else
    PlaxList = (b3dInt32 *)malloc(4 * LIST_CHUNK);
  ListMax += LIST_CHUNK;
  if (PlaxList)
    return 0;
  fprintf(stderr, "QTPLAX: Error getting memory for drawing list.\n");
  ListSize = 0;
  ListMax = 0;
  OutListInd = 0;
  return 1;
}

static void draw()
{
  int ind;
  b3dInt16 *vec;
  
  PlaxPainter = new QPainter(PlaxWidget);
  PlaxWidget->lock();

  /* fprintf(stderr, "Ind %d Size %d\n", OutListInd, ListSize); */
  
  // Draw starting after the last item drawn
  while (OutListInd < ListSize) {

    ind = OutListInd++;
    switch (PlaxList[ind++]) {
    case PLAX_MAPCOLOR:
      PlaxRGB[PlaxList[ind]] = qRgb(PlaxList[ind + 1], PlaxList[ind + 2], 
                                    PlaxList[ind + 3]);
      PlaxPenColor = -1;
      PlaxBrushColor = -1;
      OutListInd += 4;
      break;

    case PLAX_BOX:
      plax_set_brush(PlaxList[ind], 1);
      plax_draw_box(PlaxList[ind], PlaxList[ind + 1], PlaxList[ind + 2], 
                    PlaxList[ind + 3], PlaxList[ind + 4]);
      OutListInd += 5;
      break;

    case PLAX_BOXO:
      plax_set_brush(PlaxList[ind], 0);
      plax_draw_box(PlaxList[ind], PlaxList[ind + 1], PlaxList[ind + 2], 
                    PlaxList[ind + 3], PlaxList[ind + 4]);
      OutListInd += 5;
      break;

    case PLAX_VECT:
      plax_set_pen(PlaxList[ind], 0);
      plax_draw_vect(PlaxList[ind + 1], PlaxList[ind + 2], PlaxList[ind + 3],
                     PlaxList[ind + 4]);
      OutListInd += 5;
      break;

    case PLAX_VECTW:
      plax_set_pen(PlaxList[ind + 1], PlaxList[ind]);
      plax_draw_vect(PlaxList[ind + 2], PlaxList[ind + 3], PlaxList[ind + 4],
                     PlaxList[ind + 5]);
      OutListInd += 6;
      break;

    case PLAX_CIRC:
      plax_set_brush(PlaxList[ind], 1);
      plax_draw_circ(PlaxList[ind], PlaxList[ind + 1], PlaxList[ind + 2],
                     PlaxList[ind + 3]);
      OutListInd += 4;
      break;

    case PLAX_CIRCO:
      plax_set_brush(PlaxList[ind], 0);
      plax_draw_circ(PlaxList[ind], PlaxList[ind + 1], PlaxList[ind + 2],
                     PlaxList[ind + 3]);
      OutListInd += 4;
      break;

    case PLAX_POLY:
      plax_draw_poly(PlaxList[ind], PlaxList[ind + 1], 
                     (b3dInt16 *)(&PlaxList[ind + 2]), 1);
      break;

    case PLAX_POLYO:
      plax_draw_poly(PlaxList[ind], PlaxList[ind + 1], 
                     (b3dInt16 *)(&PlaxList[ind + 2]), 0);
      break;

    case PLAX_SCTEXT:
      plax_draw_text(PlaxList[ind], PlaxList[ind + 1], PlaxList[ind + 2],
                     PlaxList[ind + 3], PlaxList[ind + 4], PlaxList[ind + 5],
                     (char *)(&PlaxList[ind + 6]));
      OutListInd += 6 + (PlaxList[ind + 5] + 3) / 4;
      break;
    }

  }
  PlaxWidget->unlock();
  //PlaxPainter->flush();
  delete PlaxPainter;
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

  PlaxPainter->drawRect(x, y, width, height);

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
  PlaxPainter->drawEllipse(x - size, y - size, size * 2, size * 2);
}

static void plax_draw_vect(b3dInt32 x1, b3dInt32 y1, b3dInt32 x2, b3dInt32 y2)
{
  plax_transform(&x1, &y1);
  plax_transform(&x2, &y2);
  PlaxPainter->drawLine(x1, y1, x2, y2);
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
    PlaxPainter->drawConvexPolygon(points);
  else
    PlaxPainter->drawPolygon(points);
  
  OutListInd += 2 + csize;
}

static void plax_draw_text(b3dInt32 thickness,
                           b3dInt32 iysize,
                           b3dInt32 cindex,
                           b3dInt32 x, b3dInt32 y, 
                           b3dInt32 strsize, char *string)

{
  static int lastsize = 0;
  static int lastbold;

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

  if (lastsize != ysize || lastbold != ifbold) {
    QFont newFont("courier", ysize);
    if (ifbold)
      newFont.setBold(true);
    PlaxPainter->setFont(newFont);
  }
  lastbold = ifbold;
  lastsize = ysize;

  plax_transform(&x, &y);
  plax_set_pen(cindex, 0);

  PlaxPainter->drawText(x, y, QString(cstring));
  free(cstring);
}

static int plax_scale(int size)
{
  int nsize;
  if (PlaxScaleX > PlaxScaleY)
    nsize = (int)(size * PlaxScaleY);
  else
    nsize = (int)(size * PlaxScaleX);

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
  while (!Plax_exposed){
    qApp->processEvents();
  } 

  plax_input();
}


static short plax_transx(short ix)
{
  float x;
  x = ((float)ix) * PlaxScaleX;
  return((short)x);
}

static short plax_transy(short iy)
{
  float y;
  y = PlaxScaleY * (1023.0f - iy);
  return((short)y);
}


static void plax_transform( int *x, int *y)
{
  float ix = *x; 
  float iy = *y;

  *x = (int)(PlaxScaleX * ix);
  *y = (int)(PlaxScaleY * (1023.0 - iy));
  /*
    printf("transform (%g, %g) -> (%d, %d) : scale %g %g\n",
    ix, iy, *x, *y, PlaxScaleX, PlaxScaleY);
  */
  return;
}

static void plax_set_pen(int color, int width)
{
  if (color == PlaxPenColor && width == PlaxPenWidth)
    return;
  PlaxPainter->setPen(QPen(QColor(PlaxRGB[color]), width));
  PlaxPenColor = color;
  PlaxPenWidth = width;
}

static void plax_set_brush(int color, int closed)
{
  if ((!closed || color == PlaxBrushColor) && closed == PlaxBrushClosed)
    return;
  if (closed)
    PlaxPainter->setBrush(QColor(PlaxRGB[color]));
  else
    PlaxPainter->setBrush(Qt::NoBrush);
  PlaxBrushColor = color;
  PlaxBrushClosed = closed;
}  

/*
$Log$
Revision 1.17  2009/01/15 16:32:58  mast
Qt 4 port

Revision 1.16  2006/05/12 14:28:29  mast
Use the resize trick everywhere to get draws to work reliably on Linux

Revision 1.15  2005/11/19 17:00:45  mast
Have it call fortran routine to call getarg/iargc since these can be intrinsic

Revision 1.14  2005/02/11 01:42:32  mast
Warning cleanup: implicit declarations, main return type, parentheses, etc.

Revision 1.13  2004/07/07 19:25:31  mast
Changed exit(-1) to exit(3) for Cygwin

Revision 1.12  2004/04/08 17:06:12  mast
Used a separate define for killing process at exit

Revision 1.11  2003/10/26 05:34:23  mast
add resize workaround for Windows

Revision 1.10  2003/10/24 03:43:18  mast
provide capitalized versions of Fortran funcs, add sizes for Windows

Revision 1.9  2003/10/14 21:30:23  mast
raise widget after showing it

Revision 1.8  2003/09/24 23:04:43  mast
Reinstate resident QPainter for single thread case, flush painter at end

Revision 1.7  2003/09/24 20:41:41  mast
Made it compilable without multi-thread support

Revision 1.6  2003/09/23 21:08:33  mast
Made the painter be created and destroyed on each draw instead of being
resident, eliminated code for SECOND_THREAD, and made Mac window only a
bit smaller now that it is resizable.

Revision 1.5  2003/08/29 16:59:45  mast
Created multithreaded can of worms

Revision 1.4  2003/08/13 20:02:25  mast
Eliminate empty #define statement

Revision 1.3  2003/08/12 23:52:11  mast
Make window size smaller for Mac only

Revision 1.2  2003/08/12 21:44:36  mast
Changes to try to help text drawingon the Mac

*/
