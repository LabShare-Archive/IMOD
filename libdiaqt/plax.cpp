#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "plax.h"
#include <qapplication.h>
#include <qfont.h>
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

PlaxWindow::PlaxWindow(QWidget *parent, const char *name, WFlags fl) :
  QWidget(parent, name)
{
  PlaxPainter = new QPainter(this);
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

int plax_open(void)
{
  unsigned long plane[1];
  unsigned long pixels[256];
  int width = 800;
  int height = 640;
  int argc = 2;
  char *dummy = "Plax";
  char *style = "-style=windows";
  char *argv[2];
     
  argv[0] = dummy;
  argv[1] = style;
  Plax_open = 0;

  if (!PlaxApp)
    PlaxApp = new QApplication(argc, argv);
  PlaxWidget = new PlaxWindow(NULL, "plax window");
     
  if (!PlaxWidget){
    fprintf(stderr, "Error opening plax display.\n");
    return(-1);
  }

  PlaxScaleX = PlaxScaleY = 0.5f;
  PlaxWidget->resize(width, height);
  Plax_exposed = 0;
  Plax_open = 1;
  PlaxWidget->show();

  plax_input_open();

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
  int ysize = (int)(*iysize * 2.5);
  int ifbold = 0;
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

  PlaxPainter->drawText(x, y, QString(string), 0, strsize);
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

