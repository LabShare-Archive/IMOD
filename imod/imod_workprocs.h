/*   imod_workprocs.h  -  declarations for imod_workprocs.cpp
 *
 *   Copyright (C) 1995-2002 by Boulder Laboratory for 3-Dimensional Electron
 *   Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *   Colorado.  See implementation file for full copyright notice.
 */                                                                           

/*  $Author$

$Date$

$Revision$

$Log$
Revision 1.1.2.1  2003/01/26 23:36:53  mast
initial creation

*/

#ifndef IMOD_WORKPROCS_H
#define IMOD_WORKPROCS_H

#include <qobject.h>
typedef struct ViewInfo ImodView;
class QTimer;

class ImodWorkproc : public QObject
{
  Q_OBJECT

 public:
  ImodWorkproc(ImodView *vw);
  ~ImodWorkproc() {};

  QTimer *mAutoSaveTimer;
  QTimer *mMovieTimer;
  QTimer *mControlTimer;

  public slots:
    void autoSaveTimeout();
    void movieTimeout();
    void controlTimeout();
  
 private:
    void movieTimer();
    void movieProc();
    void movie_inc(ImodView *vi, float *mouse, int *movie, int axis,
                   int *show);

  ImodView *mVi;
  int mDisplayBusy;
  int mTimerFired;
};

int imod_start_autosave(ImodView *vw);
int imodMovieXYZT(struct ViewInfo *vi, int x, int y, int z, int t);

#endif
