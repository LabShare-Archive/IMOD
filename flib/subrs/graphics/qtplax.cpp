/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.3  2003/08/12 23:52:11  mast
Make window size smaller for Mac only

Revision 1.2  2003/08/12 21:44:36  mast
Changes to try to help text drawingon the Mac

*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "qtplax.h"
#include <qapplication.h>
#include <qfont.h>
#include <qdatetime.h>
#include <qpen.h>
#include <qpointarray.h>
#include <qpainter.h>
#include <qbrush.h>


static int   PlaxCIndex[PLAX_RAMPSIZE];
static QRgb  PlaxRGB[PLAX_RAMPSIZE];
static float PlaxScaleX;
static float PlaxScaleY;

static QApplication *PlaxApp = NULL;
static PlaxWindow *PlaxWidget = NULL;
static QPainter *PlaxPainter = NULL;
static int Plax_open;
static int Plax_exposed;
static int PlaxPenColor = -1;
static int PlaxPenWidth;
static int PlaxBrushClosed;
static int PlaxBrushColor = -1;
static void plax_input(void);
static void plax_input_open(void);
static void plax_transform( int *x, int *y);
static short plax_transx(short ix);
static short plax_transy(short iy);
static int plax_scale(int size);
static void plax_set_pen(int color, int width);
static void plax_set_brush(int color, int closed);
static void plax_draw_vect(int x1, int y1, int x2, int y2);
static void plax_draw_box(int cindex, int x1, int y1, int x2, int y2);
static void plax_draw_circ(int cindex, int radius, int x, int y);
static char *f2cString(char *str, int strSize);

extern "C" {
  int iargc_();
  void getarg_(int *i, char *string, int len);
}

PlaxWindow::PlaxWindow(QWidget *parent, const char *name, WFlags fl) :
  QWidget(parent, name)
{
  setPaletteBackgroundColor("black");
}

void PlaxWindow::closeEvent ( QCloseEvent * e )
{
  if (Plax_open)
    e->ignore();
  else {
    e->accept();
    PlaxWidget = NULL;
    delete PlaxPainter;
    PlaxPainter = NULL;
  }
}

void PlaxWindow::paintEvent ( QPaintEvent * )
{
  Plax_exposed = 1;
}

void PlaxWindow::resizeEvent ( QResizeEvent * )
{
  PlaxScaleX = ((float)width()) / 1280.0f;
  PlaxScaleY = ((float)height()) / 1024.0f;
}

#define FSTRING_LEN  80

// The characters are too small even for Mac users
// The window needs to be smaller on Mac to allow half the screen for terminal
#ifdef Q_OS_MACX
#define TEXT_SIZE_SCALE 3.3
#define DEFAULT_HEIGHT 512
#else
#define TEXT_SIZE_SCALE 2.5
#define DEFAULT_HEIGHT 640
#endif

int plax_open(void)
{
  static int width = DEFAULT_HEIGHT * 5 / 4;
  static int height = DEFAULT_HEIGHT;
  static int top = 30;
  static int left = 10;
  int argc, i, j;
  char *style = "-style=windows";
  char **argv;
  char fstring[FSTRING_LEN];
     
  Plax_open = 0;

  if (!PlaxApp) {
    // Get the arguments.  Fortran numbers them 0 to iargc()
    argc = iargc_() + 1;
    argv = (char **)malloc((argc + 1) * sizeof(char *));
    if (!argv) {
      fprintf(stderr, "Error getting memory for program arguments.\n");
      return (-1);
    }

    for (i = 0; i < argc; i++) {
      getarg_(&i, fstring, FSTRING_LEN);

      argv[i] = f2cString(fstring, FSTRING_LEN);
      if (!argv[i]) {
	fprintf(stderr, "Error getting memory for program arguments.\n");
	return (-1);
      }
    }

    argv[argc] = strdup(style);
    if (argv[argc])
      argc++;

    PlaxApp = new QApplication(argc, argv);

    //    for (i = 0; i < argc; i++)
    //  printf("%d %d - %s\n", i, strlen(argv[i]),argv[i]);

    // Look for -s and -p arguments
    for (i = 1; i < argc - 1; i++) {
      if (!strcmp("-s", argv[i])) {
	sscanf(argv[++i], "%d%*c%d", &width, &height);
	continue;
      }
      if (!strcmp("-p", argv[i])) {
	sscanf(argv[++i], "%d%*c%d", &left, &top);
      }
    }

    // IT WAS NOT OK TO Free the memory

  }

  PlaxWidget = new PlaxWindow(NULL, "plax window");
     
  if (!PlaxWidget){
    fprintf(stderr, "Error opening plax display.\n");
    return(-1);
  }

  PlaxScaleX = PlaxScaleY = 0.5f;
  PlaxWidget->setGeometry(left, top, width, height);
  Plax_exposed = 0;
  Plax_open = 1;
  PlaxWidget->show();

  plax_input_open();

  // This needs to be down here to work on the Mac
  PlaxPainter = new QPainter(PlaxWidget);
  return(0);
}

void plax_close(void)
{
  Plax_open = 0;
  if (PlaxWidget)
    PlaxWidget->close();
  plax_input();
  return;
}

void plax_flush(void)
{
  plax_input();
  return;
}

void plax_mapcolor(int *color, int *ired, int *igreen, int *iblue)
{
  PlaxRGB[*color] = qRgb(*ired, *igreen, *iblue);
  PlaxPenColor = -1;
  PlaxBrushColor = -1;
}

void plax_box(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  plax_set_brush(*cindex, 1);
  plax_draw_box(*cindex, *ix1, *iy1, *ix2, *iy2);
}

void plax_boxo(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  plax_set_brush(*cindex, 0);
  plax_draw_box(*cindex, *ix1, *iy1, *ix2, *iy2);
}

void plax_vect(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2)
{
  plax_set_pen(*cindex, 0);
  plax_draw_vect(*ix1, *iy1, *ix2, *iy2);
}

void plax_vectw(int *linewidth, int *cindex, 
                int *ix1, int *iy1, int *ix2, int *iy2)
{
  plax_set_pen(*cindex, *linewidth);
  plax_draw_vect(*ix1, *iy1, *ix2, *iy2);
}

/* filled circle */
void plax_circ(int *cindex, int *radius, int *ix, int *iy)
{
  plax_set_brush(*cindex, 1);
  plax_draw_circ(*cindex, *radius, *ix, *iy);
}

/* open circle */
void plax_circo(int *cindex, int *radius, int *ix, int *iy)
{
  plax_set_brush(*cindex, 0);
  plax_draw_circ(*cindex, *radius, *ix, *iy);
}

/* closed filled polygon */
void plax_poly(int *cindex, int *size, short *vec)
{
  int csize = *size;
  QPointArray points(csize);
  int i;

  plax_set_pen(*cindex, 0);
  plax_set_brush(*cindex, 1);

  for (i = 0; i < csize; i++)
    points.setPoint(i, (int)plax_transx(vec[i * 2]), 
		    (int)plax_transy(vec[(i * 2) + 1]));

  PlaxPainter->drawConvexPolygon(points);
}

void plax_polyo(int *cindex, int *size, short *vec)
{
  int csize = *size;
  QPointArray points(csize);
  int i;

  plax_set_pen(*cindex, 0);
  plax_set_brush(*cindex, 0);

  for (i = 0; i < csize; i++)
    points.setPoint(i, (int)plax_transx(vec[i * 2]), 
		    (int)plax_transy(vec[(i * 2) + 1]));

  PlaxPainter->drawPolygon(points);
}



/* DNM: changed this to keep track of last font used, to search for nearest
   size from requested font, and to keep track of the sizes found in the
   table */
void plax_sctext(int *thickness,
                 int *xsize,
                 int *iysize,
                 int *cindex,
                 int *ix, int *iy, 
                 char *string, int strsize
                 )
{
  static int lastsize = 0;
  static int lastbold;

  int x = *ix;
  int y = *iy;
  int ysize = (int)(*iysize * TEXT_SIZE_SCALE);
  int ifbold = 0;
  char *cstring = f2cString(string, strsize);

  if (!cstring)
    return;

  if (*thickness > 1)
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
  plax_set_pen(*cindex, 0);

  PlaxPainter->drawText(x, y, QString(cstring));
  free(cstring);
}

void plax_putc(char *f)
{
  putchar(*f);
}

/*****************************************************************************/
/* Internal Functions                                                        */
/*****************************************************************************/


static void plax_draw_box(int cindex, int x1, int y1, int x2, int y2)
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

static void plax_draw_circ(int cindex, int radius, int x, int y)
{
  int size = plax_scale(radius);
  plax_set_pen(cindex, 0);

  plax_transform(&x, &y);
  PlaxPainter->drawEllipse(x - size, y - size, size * 2, size * 2);
}

static void plax_draw_vect(int x1, int y1, int x2, int y2)
{
  plax_transform(&x1, &y1);
  plax_transform(&x2, &y2);
  PlaxPainter->drawLine(x1, y1, x2, y2);
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

/* Create a C string with a copy of a Fortran string */
static char *f2cString(char *str, int strSize)
{
  int i;
  char *newStr;

  /* find last non-blank character */
  for (i = strSize - 1; i >= 0; i--)
    if (str[i] != ' ')
      break;

  newStr = (char *)malloc(i + 2);
  if (!newStr) {
    return NULL;
  }

  /* copy string if non-null, then put terminator at end */
  if (i >= 0)
    strncpy(newStr, str, i + 1);
  newStr[i + 1] = 0x00;
  return newStr;
}
