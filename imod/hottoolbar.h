/*
 *  hottoolbar.h -  A toolbar subclass that will pass on keys
 *
 *  $Id$
 *
 *  $Log$
 *
 */
#ifndef HOTTOOLBAR_H
#defin
 *
 */
#ifndef HOTTOOLBAR_H
#define HOTTOOLBAR_H

#include <qtoolbar.h>

/* 
 * A HotToolBar emits keyPress( QKeyEvent * e) and keyRelease( QKeyEvent * e)
 * To use it, connect these signals to the slots the keyPressEvent and
 * keyReleaseEvent slots of the parent widget.
 */
class HotToolBar : public QToolBar
{
  Q_OBJECT
 public:
  HotToolBar( QMainWindow * parent = 0, const char * name = 0) 
    : QToolBar(parent, name) { };
  ~HotToolBar() {}

 signals:
  void keyPress(QKeyEvent *e);
  void keyRelease(QKeyEvent *e);

 protected:
  void keyPressEvent ( QKeyEvent * e ) {emit keyPress(e);};
  void keyReleaseEvent ( QKeyEvent * e ) {emit keyRelease(e);};
};

#endif
