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

#define plax_open     P_START
#define plax_close    P_END
#define plax_flush    P_B_FLUSH
#define plax_mapcolor P_CLT8
#define plax_box      P_BOX
#define plax_boxo     P_BOXO
#define plax_vect     P_VECT
#define plax_vectw    P_VECTW
#define plax_circ     P_CIRC
#define plax_circo    P_CIRCO
#define plax_poly     P_POLY
#define plax_polyo    P_POLYO
#define plax_sctext   P_SCTEXT
#define plax_erase    PLAX_ERASE
#define plax_initialize  PLAX_INITIALIZE

#else

#ifdef G77__HACK
/* add HACK for gnu FORTRAN...  __  7/20/00 CER   */
#define plax_open     p_start__
#define plax_close    p_end__
#define plax_flush    p_b_flush__
#define plax_mapcolor p_clt8__
#define plax_box      p_box__
#define plax_boxo     p_boxo__
#define plax_vect     p_vect__
#define plax_vectw    p_vectw__
#define plax_circ     p_circ__
#define plax_circo    p_circo__
#define plax_poly     p_poly__
#define plax_polyo    p_polyo__
#define plax_sctext   p_sctext__
#define plax_putc     putc_
#define plax_erase    plax_erase__
#define plax_initialize  plax_initialize__

#else

#define plax_open     p_start_
#define plax_close    p_end_
#define plax_flush    p_b_flush_
#define plax_mapcolor p_clt8_
#define plax_box      p_box_
#define plax_boxo     p_boxo_
#define plax_vect     p_vect_
#define plax_vectw    p_vectw_
#define plax_circ     p_circ_
#define plax_circo    p_circo_
#define plax_poly     p_poly_
#define plax_polyo    p_polyo_
#define plax_sctext   p_sctext_
#define plax_erase    plax_erase_
#define plax_initialize  plax_initialize_

#endif

#endif

#endif /* PLAX_CTEST */

extern "C" {
void plax_initialize(char *string, int strsize);
int plax_open(void);
void plax_close(void);
void plax_flush(void);
void plax_erase(void);
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
void plax_sctext(int *thickness,
		 int *xsize,
		 int *iysize,
		 int *cindex,
		 int *ix, int *iy,
                 char *string, int strsize);
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

 protected:
    void closeEvent ( QCloseEvent * e );
    void paintEvent ( QPaintEvent * );
    void resizeEvent ( QResizeEvent * );

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
