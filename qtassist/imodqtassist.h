/*
 *  imodqtassist.h - declarations for imodqtassist.cpp
 *
 *  Author: David Mastronarde   email: mast@colorado.edu
 *
 *  Copyright (C) 1995-2004 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of 
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 */

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 1.1  2004/12/22 05:49:02  mast
    Addition to package

*/
#ifndef IMODQTASSIST_H
#define IMODQTASSIST_H

#include <qobject.h>
class AssistantListener : public QObject
{
  Q_OBJECT

public: 
  AssistantListener() {mWarned = false;};  
  ~AssistantListener() {};

  public slots:
    void assistantError(const QString &msg);

 protected:
  void timerEvent(QTimerEvent *e);

 private:
  bool mWarned;
};

#ifdef QT_THREAD_SUPPORT
#include <qthread.h>

class AssistantThread : public QThread
{
 public:
  AssistantThread() {};
  ~AssistantThread() {};

 protected:
  void run();
};
#endif

#endif
