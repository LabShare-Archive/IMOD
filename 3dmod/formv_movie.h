/*   formv_movie.h  -  declarations for formv_movie.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef IMODVMOVIEFORM_H
#define IMODVMOVIEFORM_H

#include <qvariant.h>
#include "ui_formv_movie.h"

class imodvMovieForm : public QWidget, public Ui::imodvMovieForm
{
  Q_OBJECT

    public:
  imodvMovieForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~imodvMovieForm();

  public slots:
    virtual void init();
  virtual void setNonTifLabel();
  virtual void fullXPressed();
  virtual void fullYPressed();
  virtual void setStartPressed();
  virtual void setEndPressed();
  virtual void reverseToggled( bool state );
  virtual void longWayToggled( bool state );
  virtual void movieMontSelected( int item );
  virtual void rgbTiffSelected( int item );
  virtual void writeToggled( bool state );
  virtual void closePressed();
  virtual void makePressed();
  virtual void stopPressed();
  virtual void helpPressed();
  virtual void readStartEnd( int item, float & startVal, float & endVal );
  virtual void setStart( int item, float value );
  virtual void setEnd( int item, float value );
  virtual void setButtonStates( bool longWay, bool reverse, int movieMont, int rgbTiff, bool writeFiles );
  virtual void getButtonStates( int & longWay, int & reverse, int & movieMont, int & rgbTiff, int & writeFiles );
  virtual void getFrameBoxes( int & nMovieFrames, int & nMontFrames );
  virtual void setFrameBoxes( int nMovieFrames, int nMontFrames );
  virtual void manageSensitivities( int movieMont );
  virtual void closeEvent( QCloseEvent * e );
  virtual void keyPressEvent( QKeyEvent * e );
  virtual void keyReleaseEvent( QKeyEvent * e );

 protected:
  bool mReverse;
  QString mStr;
  QLineEdit *endEdits[12];
  QLineEdit *startEdits[12];
  bool mLongWay;
  int mRgbTiff;
  int mMovieMont;
  bool mWriteFiles;
  QButtonGroup *makeGroup;
  QButtonGroup *writeGroup;

  protected slots:
    virtual void languageChange();

};

#endif // IMODVMOVIEFORM_H
