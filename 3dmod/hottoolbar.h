/*
 *  hottoolbar.h -  A toolbar subclass that will pass on keys
 *
 *  $Id$
 *
 *  $Log$
 *  Revision 1.3  2007/05/31 16:34:31  mast
 *  Took out of slicer and made separate so zap could use it too
 *
 */
#ifndef HOTTOOLBAR_H
#define HOTTOOLBAR_H

#include <qtoolbar.h>
//Added by qt3to4:
#include <QKeyEvent>

/* 
 * A HotToolBar emits keyPress( QKeyEvent * e) and keyRelease( QKeyEvent * e)
 * To use it, connect these signals to the slots the keyPressEvent and
 * keyReleaseEvent slots of the parent widget.
 */
class HotToolBar : public QToolBar
{
  Q_OBJECT
 public:
  HotToolBar(QWidget * parent = 0) 
    : QToolBar(parent) { };
  HotToolBar( const QString & title, QWidget * parent = 0) 
    : QToolBar(title, parent) { };
  ~HotToolBar() {}

 signals:
  void keyPress(QKeyEvent *e);
  void keyRelease(QKeyEvent *e);

 protected:
  void keyPressEvent ( QKeyEvent * e ) {emit keyPress(e);};
  void keyReleaseEvent ( QKeyEvent * e ) {emit keyRelease(e);};
};

#endif
