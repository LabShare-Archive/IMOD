/*   form_scalebar.h  -  declarations for form_scalebar.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef SCALEBARFORM_H
#define SCALEBARFORM_H

#include <qvariant.h>

typedef struct scale_bar ScaleBar;

#include "ui_form_scalebar.h"

class ScaleBarForm : public QWidget, public Ui::ScaleBarForm
{
  Q_OBJECT

    public:
  ScaleBarForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~ScaleBarForm();

  virtual void updateValues( float zapv, float multiZv, float slicerv, float xyzv, float modvv, char * units );

  public slots:
    virtual void drawToggled( bool state );
  virtual void whiteToggled( bool state );
  virtual void verticalToggled( bool state );
  virtual void colorToggled( bool state );
  virtual void invertToggled( bool state );
  virtual void lengthChanged( int value );
  virtual void thicknessChanged( int value );
  virtual void positionChanged( int value );
  virtual void indentXchanged( int value );
  virtual void indentYchanged( int value );
  virtual void customToggled( bool state );
  virtual void customValChanged( int value );
  virtual void startUpdateTimer();
  virtual void timerEvent( QTimerEvent * e );
  virtual void helpPressed();

 protected:
  virtual void keyPressEvent( QKeyEvent * e );
  virtual void keyReleaseEvent( QKeyEvent * e );
  virtual void closeEvent( QCloseEvent * e );

  protected slots:
    virtual void languageChange();

 private:
  ScaleBar *mParams;
  int mTimerID;

  void init();

};

#endif // SCALEBARFORM_H
