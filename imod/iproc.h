/*
 *  $Id$
 *
 *  Original author: James Kremer
 *  Revised by: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2005 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */
/*  $Author$

    $Date$

    $Revision$

    Log at end of file
*/

#ifndef BD_IPROC_H_
#define BD_IPROC_H_

#define PROC_BACKGROUND 0
#define PROC_FOREGROUND 255

#include "mrcslice.h"
#include "dialog_frame.h"
class QWidgetStack;
class QListBox;
class QVBoxLayout;
class QLabel;
class QSpinBox;
class ToolEdit;

typedef struct ViewInfo ImodView;

#ifdef QT_THREAD_SUPPORT
#include <qthread.h>

class IProcThread : public QThread
{
 public:
  IProcThread() {};
  ~IProcThread() {};

 protected:
  void run();
};
#endif

class IProcWindow : public DialogFrame
{
  Q_OBJECT

 public:
  IProcWindow(QWidget *parent, const char *name = NULL);
  ~IProcWindow() {};
  bool mRunningProc;
  void limitFFTbinning();

  public slots:
  void buttonClicked(int which);
  void buttonPressed(int which);
  void edgeSelected(int which);
  void filterSelected(int which);
  void filterHighlighted(int which);
  void threshChanged(int which, int value, bool dragging);
  void fourFiltChanged(int which, int value, bool dragging);
  void binningChanged(int val);
  void subsetChanged(bool state);
  void medSizeChanged(int val);
  void med3DChanged(bool state);
  void andfIterChanged(int val);
  void andfFuncClicked(int val);
  void andfKEntered();

 protected:
  void closeEvent ( QCloseEvent * e );
  void keyPressEvent ( QKeyEvent * e );
  void keyReleaseEvent ( QKeyEvent * e );
  void fontChange( const QFont & oldFont );
  void timerEvent(QTimerEvent *e);

 private:
  QWidgetStack *mStack;
  QListBox *mListBox;
  void apply();
  void startProcess();
  void finishProcess();
  int mTimerID;
#ifdef QT_THREAD_SUPPORT
  QThread *mProcThread;
#endif
};

typedef struct
{
  IProcWindow   *dia;
  ImodView      *vi;        /* image data to model                       */
  unsigned char *iwork;     /* Image data processing buffer.             */
  unsigned char *isaved;     /* buffer for saving original data.         */
  double        **andfImage; /* Double buffers for aniso diff */
  double        **andfImage2;

  int           idatasec;   /* data section. */
  int           idatatime;  /* time value of section */
  int           procnum;
  int           modified;   /* flag that section data are modified */

  int           threshold;  /* Parameters for individual filters */
  int           edge;
  float         radius1;
  float         radius2;
  float         sigma1;
  float         sigma2;
  int           fftBinning;
  bool          fftSubset;
  float         fftScale;
  float         fftXrange;
  float         fftYrange;
  QSpinBox      *fftBinSpin;
  QLabel        *fftLabel1;
  QLabel        *fftLabel2;
  bool          median3D;
  int           medianSize;
  struct MRCvolume medianVol;
  int           andfIterations;
  int           andfIterDone;
  double        andfK;
  double        andfLambda;
  int           andfStopFunc;
  ToolEdit      *andfKEdit;
  ToolEdit      *andfLambdaEdit;
  QLabel        *andfScaleLabel;
  QLabel        *andfDoneLabel;
  
} ImodIProc;


typedef struct
{
  char *name;         /* Name of index */
  void (*cb)();       /* callback to do action */
  /* function to make widget */
  void (*mkwidget)(IProcWindow *, QWidget *, QVBoxLayout *); 
  char *label;
} ImodIProcData;

int inputIProcOpen(ImodView *vw);
int iprocRethink(ImodView *vw);
bool iprocBusy(void);

#endif /* BD_IPROC_H_ */
/*
    $Log$
    Revision 3.10  2005/01/07 21:59:13  mast
    Added median filter

    Revision 3.9  2004/11/11 15:55:34  mast
    Changes to do FFT in a subarea

    Revision 3.8  2004/11/07 23:04:47  mast
    Changes for thread and FFT stuff

    Revision 3.7  2004/11/04 23:30:55  mast
    Changes for rounded button style

    Revision 3.6  2004/01/22 19:10:12  mast
    Added slot for real button press

    Revision 3.5  2004/01/05 18:03:50  mast
    renamed vw to vi

    Revision 3.4  2003/09/16 02:10:55  mast
    Added working array because displayed image data cannot be used directly

    Revision 3.3  2003/02/10 20:41:56  mast
    Merge Qt source

    Revision 3.2.2.2  2003/01/27 00:30:07  mast
    Pure Qt version and general cleanup

    Revision 3.2.2.1  2003/01/23 19:57:06  mast
    Qt version

    Revision 3.2  2002/12/01 15:34:41  mast
    Changes to get clean compilation with g++

*/
