/*   form_info.h  -  declarations for form_info.cpp
 *
 *  $Id$
 *  $Log$
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
  virtual void movieModelSelected( int item );
  virtual void floatToggled( bool state );
  virtual void raisePressed();
  virtual void showPointToggled( bool state );
  virtual void keepOnTopToggled( bool state );
  virtual void subareaToggled( bool state );
  virtual void autoClicked();
  virtual void undoClicked();
  virtual void redoClicked();
  virtual void setFloat( int state );
  virtual void setUndoRedo( bool undoOn, bool redoOn );
  virtual void setBWSliders( int black, int white );
  virtual void setMovieModel( int which );
  virtual void updateOCP( int * newVal, int * maxVal );
  virtual void updateXYZ( int * newVal, int * maxVal );
  virtual void setObjectColor( QColor foreColor, QColor backColor );
  virtual void setModelName( char * name );
  virtual void setImageName( char * name );
  virtual void setShowPoint( int state );

 protected:
  bool mShowPoint;
  QLabel *mOCPLabel[3];
  QLabel *mXYZLabel[3];
  int mDisplayedWhite;
  int mDisplayedBlack;
  bool mCtrlPressed;
  bool mBlackPressed;
  bool mWhitePressed;
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
