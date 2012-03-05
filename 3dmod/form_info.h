/*   form_info.h  -  declarations for form_info.cpp
 *
 *  $Id$
 *  $Log$
 *  Revision 4.1  2009/01/15 16:33:17  mast
 *  Qt 4 port
 *
 *
 */
#ifndef INFOCONTROLS_H
#define INFOCONTROLS_H

#include <qvariant.h>
#include "ui_form_info.h"

class QButtonGroup;

class InfoControls : public QWidget, public Ui::InfoControls
{
  Q_OBJECT

    public:
  InfoControls(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~InfoControls();

  public slots:
    virtual void init();
  virtual void setFontDependentWidths();
  virtual void xyzChanged( int item );
  virtual void ocpChanged( int item );
  virtual void blackChanged( int value );
  virtual void blackPressed();
  virtual void blackReleased();
  virtual void displayBlack( int value );
  virtual void whiteChanged( int value );
  virtual void whitePressed();
  virtual void whiteReleased();
  virtual void displayWhite( int value );
  virtual void lowChanged( int value );
  virtual void lowPressed();
  virtual void lowReleased();
  virtual void displayLow( int value );
  virtual void highChanged( int value );
  virtual void highPressed();
  virtual void highReleased();
  virtual void displayHigh( int value );
  virtual void movieModelSelected( int item );
  virtual void floatToggled( bool state );
  virtual void raisePressed();
  virtual void showPointToggled( bool state );
  virtual void keepOnTopToggled( bool state );
  virtual void subareaToggled( bool state );
  virtual void autoClicked();
  virtual void undoClicked();
  virtual void redoClicked();

 public:
  void setFloat( int state );
  void setUndoRedo( bool undoOn, bool redoOn );
  void setBWSliders( int black, int white );
  void setLHSliders( int low, int high, float smin, float smax, bool showReal );
  void setLHSliders( int low, int high) {setLHSliders(low, high, mScaleMin, mScaleMax, 
                                                      mShowLHReal);};
  void setMovieModel( int which );
  void updateOCP( int * newVal, int * maxVal );
  void updateXYZ( int * newVal, int * maxVal );
  void setObjectColor( QColor foreColor, QColor backColor );
  void setModelName( const char * name );
  void setImageName( const char * name );
  void setShowPoint( int state );
  void formatLHvalue(int value);
  void hideLowHighGrid();
  int adjustedHeightHint();

 protected:
  bool mShowPoint;
  QLabel *mOCPLabel[3];
  QLabel *mXYZLabel[3];
  int mDisplayedWhite;
  int mDisplayedBlack;
  int mDisplayedLow;
  int mDisplayedHigh;
  bool mCtrlPressed;
  bool mBlackPressed;
  bool mWhitePressed;
  bool mLowPressed;
  bool mHighPressed;
  float mScaleMin;
  float mScaleMax;
  bool mShowLHReal;
  int mLastXYZval[3];
  int mLastXYZmax[3];
  int mLastOCPval[3];
  int mLastOCPmax[3];
  QSpinBox *mOCPBox[3];
  QSpinBox *mXYZBox[3];
  QString mStr;
  QButtonGroup *modeGroup;

  protected slots:
    virtual void languageChange();

};

#endif // INFOCONTROLS_H
