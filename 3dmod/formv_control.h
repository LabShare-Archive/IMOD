/*   formv_control.h  -  declarations for formv_control.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef IMODVCONTROLFORM_H
#define IMODVCONTROLFORM_H

#include <qvariant.h>
#include "ui_formv_control.h"

class imodvControlForm : public QWidget, public Ui::imodvControlForm
{
  Q_OBJECT

    public:
  imodvControlForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~imodvControlForm();

  public slots:
    virtual void init();
  virtual void setFontDependentWidths();
  virtual void displayFarLabel( int value );
  virtual void displayNearLabel( int value );
  virtual void displayPerspectiveLabel( int value );
  virtual void displayRateLabel( int value );
  virtual void displayZscaleLabel( int value );
  virtual void zoomDown();
  virtual void zoomUp();
  virtual void newScale();
  virtual void kickBoxToggled( bool state );
  virtual void nearChanged( int value );
  virtual void farChanged( int value );
  virtual void perspectiveChanged( int value );
  virtual void zScaleChanged( int value );
  virtual void rotateXminus();
  virtual void rotateXplus();
  virtual void rotateYminus();
  virtual void rotateYplus();
  virtual void rotateZminus();
  virtual void rotateZplus();
  virtual void newXrotation();
  virtual void newYrotation();
  virtual void newZrotation();
  virtual void startStop();
  virtual void linkBoxToggled( bool state );
  virtual void linkCenterToggled( bool state );
  virtual void rateChanged( int value );
  virtual void newSpeed();
  virtual void increaseSpeed();
  virtual void decreaseSpeed();
  virtual void OKPressed();
  virtual void helpPressed();
  virtual void setAxisText( int axis, float value );
  virtual void setScaleText( float value );
  virtual void setKickBox( bool state );
  virtual void setViewSlider( int which, int value );
  virtual void setRotationRate( int value );
  virtual void setSpeedText( float value );
  virtual void closeEvent( QCloseEvent * e );
  virtual void farPressed();
  virtual void farReleased();
  virtual void nearPressed();
  virtual void nearReleased();
  virtual void zScaleReleased();
  virtual void perspectivePressed();
  virtual void perspectiveReleased();
  virtual void zScalePressed();
  virtual void ratePressed();
  virtual void rateReleased();
  virtual void keyPressEvent( QKeyEvent * e );
  virtual void keyReleaseEvent( QKeyEvent * e );
  virtual void changeEvent(QEvent *e);

 protected:
  int mRateDisplayed;
  bool mCtrlPressed;
  int mZscaleDisplayed;
  bool mZscalePressed;
  int mPerspectiveDisplayed;
  bool mPerspectivePressed;
  int mFarDisplayed;
  bool mFarPressed;
  int mNearDisplayed;
  bool mNearPressed;
  bool mRatePressed;

  protected slots:
    virtual void languageChange();

};

#endif // IMODVCONTROLFORM_H
