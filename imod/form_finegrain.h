/*   form_finegrain.h  -  declarations for form_finegrain.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef FINEGRAINFORM_H
#define FINEGRAINFORM_H

#include <qvariant.h>

class MultiSlider;
class ColorSelector;
class QButtonGroup;

#include "ui_form_finegrain.h"

class FineGrainForm : public QWidget, public Ui::FineGrainForm
{
  Q_OBJECT

    public:
  FineGrainForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~FineGrainForm();

  public slots:
    virtual void setFontDependentWidths();
  virtual void update( int ptContSurf, bool enabled, DrawProps * props, int stateFlags, bool nextEnabled );
  virtual void ptContSurfSelected( int which );
  virtual void nextChangeClicked();
  virtual void setLineColor();
  virtual void setFillColor();
  virtual void newLineColor( int red, int green, int blue );
  virtual void newFillColor( int red, int green, int blue );
  virtual void lineColorDone();
  virtual void fillColorDone();
  virtual void lineColorClosing();
  virtual void fillColorClosing();
  virtual void lastLineColor();
  virtual void lastFillColor();
  virtual void lastTrans();
  virtual void last2DWidth();
  virtual void last3DWidth();
  virtual void lastSymtype();
  virtual void lastSymsize();
  virtual void endClicked( int which );
  virtual void clearClicked( int which );
  virtual void transSliderChanged( int which, int value, bool dragging );
  virtual void width2DChanged( int value );
  virtual void width3DChanged( int value );
  virtual void symsizeChanged( int value );
  virtual void symtypeSelected( int value );
  virtual void fillToggled( bool state );
  virtual void gapToggled( bool state );
  virtual void connectChanged( int value );
  virtual void helpClicked();
  virtual void closeEvent( QCloseEvent * e );
  virtual void keyPressEvent( QKeyEvent * e );
  virtual void keyReleaseEvent( QKeyEvent * e );
  virtual void fontChange( const QFont & oldFont );

 protected:
  int mCurFillBlue;
  int mCurBlue;
  int mCurFillGreen;
  int mCurGreen;
  int mCurFillRed;
  int mCurRed;
  bool mLastSymFill;
  int mLastFillBlue;
  int mLastFillGreen;
  int mLastFillRed;
  int mLastBlue;
  int mLastGreen;
  int mLastRed;
  int mLastTrans;
  int mLastSymType;
  int mLastSymsize;
  int mLast3Dwidth;
  int mLast2Dwidth;
  QPushButton *mLastButs[7];
  QPushButton *mEndButs[7];
  QPushButton *mClearButs[7];
  int mTypeValues[7];
  int mChangeFlags[7];
  int mPtContSurf;
  QLabel *mDSlabels[7];
  MultiSlider *mTransSlider;
  bool mCtrlPressed;
  int mSymTable[4];
  ColorSelector *mLineSelector;
  ColorSelector *mFillSelector;
  QButtonGroup *surfContPtGroup;

  protected slots:
    virtual void languageChange();

 private:
  void init();

};

#endif // FINEGRAINFORM_H
