/*   formv_objed.h  -  declarations for formv_objed.cpp
 *
 *  $Id$
 *  $Log$
 *  Revision 4.1  2009/01/15 16:33:17  mast
 *  Qt 4 port
 *
 *
 */
#ifndef IMODVOBJEDFORM_H
#define IMODVOBJEDFORM_H

#include <qvariant.h>

class ColorSelectorGL;
class QStackedWidget;

#include "ui_formv_objed.h"

class imodvObjedForm : public QWidget, public Ui::imodvObjedForm
{
  Q_OBJECT

    public:
  imodvObjedForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~imodvObjedForm();

  public slots:
    virtual void init();
  virtual void setFontDependentSizes( int width, int height );
  virtual void changeEvent(QEvent *e);
  virtual void objectSelected( int which );
  virtual void editSelected( int item );
  virtual void objSliderChanged( int value );
  virtual void nameChanged( const QString & str );
  virtual void typeSelected( int item );
  virtual void styleSelected( int item );
  virtual void frameSelected( int item );
  virtual void donePressed();
  virtual void helpPressed();
  virtual void updateObject( int ob, int numObj, int drawType, int drawStyle, QColor color, char * name );
  virtual void updateMeshing(int ob);
  virtual void updateColorBox( QColor color );
  virtual void setCurrentFrame( int frame, int editData );
  virtual void closeEvent( QCloseEvent * e );
  virtual void keyPressEvent( QKeyEvent * e );
  virtual void keyReleaseEvent( QKeyEvent * e );

 protected:
  int mBoxRGB[3];
  QStackedWidget *mStack;
  ColorSelectorGL *mGLw;

  protected slots:
    virtual void languageChange();

};

#endif // IMODVOBJEDFORM_H
