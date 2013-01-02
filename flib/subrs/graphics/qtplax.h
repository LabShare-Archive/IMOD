/* Header for qtplax.cpp with defines for Fortran-callable routines
 *
 *  $Id$
 */
#ifndef _PLAX_H_
#define _PLAX_H_

/* DNM 8/17/00: add this to get the flags in as needed */
#include "imodconfig.h"

#include <qthread.h>
#include <qmutex.h>
#include <qwidget.h>
//Added by qt3to4:
#include <QPaintEvent>
#include <QResizeEvent>
#include <QCloseEvent>

/* First color used in color index ramp */
#define PLAX_LOWRAMP 40
#define PLAX_RAMPSIZE 256

#ifndef PLAX_CTEST

#ifdef F77FUNCAP

#define plax_open     PLAX_OPEN
#define plax_close    PLAX_CLOSE
#define plax_flush    PLAX_FLUSH
#define plax_mapcolor PLAX_MAPCOLOR
#define plax_box      PLAX_BOX
#define plax_boxo     PLAX_BOXO
#define plax_vect     PLAX_VECT
#define plax_vectw    PLAX_VECTW
#define plax_circ     PLAX_CIRC
#define plax_circo    PLAX_CIRCO
#define plax_poly     PLAX_POLY
#define plax_polyo    PLAX_POLYO
#define plax_sctext   PLAX_SCTEXT
#define plax_erase    PLAX_ERASE
#define plax_initialize  PLAX_INITIALIZE
#define plax_wait_for_close PLAX_WAIT_FOR_CLOSE
#define plax_next_text_align PLAX_NEXT_TEXT_ALIGN
#define plax_drawing_scale PLAX_DRAWING_SCALE

#else

#ifdef G77__HACK
#define plax_open     plax_open__
#define plax_close    plax_close__
#define plax_flush    plax_flush__
#define plax_mapcolor plax_mapcolor__
#define plax_box      plax_box__
#define plax_boxo     plax_boxo__
#define plax_vect     plax_vect__
#define plax_vectw    plax_vectw__
#define plax_circ     plax_circ__
#define plax_circo    plax_circo__
#define plax_poly     plax_poly__
#define plax_polyo    plax_polyo__
#define plax_sctext   plax_sctext__
#define plax_putc     putc_
#define plax_erase    plax_erase__
#define plax_initialize  plax_initialize__
#define plax_wait_for_close plax_wait_for_close__
#define plax_next_text_align plax_next_text_align__
#define plax_drawing_scale plax_drawing_scale__

#else

#define plax_open     plax_open_
#define plax_close    plax_close_
#define plax_flush    plax_flush_
#define plax_mapcolor plax_mapcolor_
#define plax_box      plax_box_
#define plax_boxo     plax_boxo_
#define plax_vect     plax_vect_
#define plax_vectw    plax_vectw_
#define plax_circ     plax_circ_
#define plax_circo    plax_circo_
#define plax_poly     plax_poly_
#define plax_polyo    plax_polyo_
#define plax_sctext   plax_sctext_
#define plax_erase    plax_erase_
#define plax_initialize  plax_initialize_
#define plax_wait_for_close plax_wait_for_close_
#define plax_next_text_align plax_next_text_align_
#define plax_drawing_scale plax_drawing_scale_

#endif

#endif

#endif /* PLAX_CTEST */

extern "C" {
void plax_initialize(char *string, int strsize);
int plax_open(void);
void plax_close(void);
void plax_flush(void);
void plax_erase(void);
void plax_wait_for_close(void);
void plax_mapcolor(int *color, int *ired, int *igreen, int *iblue);
void plax_box(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2);
void plax_boxo(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2);
void plax_vect(int *cindex, int *ix1, int *iy1, int *ix2, int *iy2);
void plax_vectw(int *linewidth, int *cindex,
		int *ix1, int *iy1, int *ix2, int *iy2);
void plax_circ(int *cindex, int *radius, int *ix, int *iy);
void plax_circo(int *cindex, int *radius, int *ix, int *iy);
void plax_poly(int *cindex, int *size, b3dInt16 *vec);
void plax_polyo(int *cindex, int *size, b3dInt16 *vec);
void plax_sctext(int *thickness, int *xsize, int *iysize, int *cindex,
                 int *ix, int *iy, char *string, int strsize);
void plax_next_text_align(int *type);
void plax_drawing_scale(float *xscale, float *xadd, float *yscale, float *yadd);
}

class PlaxWindow : public QWidget
{
  Q_OBJECT

 public:
  PlaxWindow(QWidget *parent, Qt::WFlags fl = Qt::Window);
  ~PlaxWindow() {};
  void lock();
  void unlock();

  public slots:
  void redrawSlot();
  void savePNGslot(bool state);
  void printSlot(bool state);

 protected:
    void closeEvent ( QCloseEvent * e );
    void paintEvent ( QPaintEvent * );
    void resizeEvent ( QResizeEvent * );
    void timerEvent(QTimerEvent *e);
    void mousePressEvent(QMouseEvent * e );

 private:
    int mTimerID;
    int mRedrawCount;
    int mNumRedraws;
};

class PlaxThread : public QThread
{
  Q_OBJECT

 public:
  PlaxThread();
  ~PlaxThread() {};
  void sendSignal();

 signals:
  void redraw();

 protected:
  void run();
};

#endif /* plax.h */
