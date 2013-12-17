/*   form_slicerangle.h  -  declarations for form_slicerangle.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef SLICERANGLEFORM_H
#define SLICERANGLEFORM_H

#include <qvariant.h>

typedef struct slicer_angles SlicerAngles;

#include "ui_form_slicerangle.h"

class SlicerAngleForm : public QWidget, public Ui::SlicerAngleForm
{
  Q_OBJECT

    public:
  SlicerAngleForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~SlicerAngleForm();

  virtual void loadTable( int time );
  virtual SlicerAngles * findAngles( int row, int & index );
  virtual void newTime( bool refresh );

  public slots:
    virtual void updateEnables();
  virtual void changeEvent(QEvent *e);
  virtual void setFontDependentWidths();
  virtual void setTimeLabel();
  virtual void setCurrentOrNewRow( int time, bool newrow );
  virtual void setAnglesFromRow( int time );
  virtual void getAngClicked();
  virtual void setAngClicked();
  virtual void setAngles( bool draw );
  virtual void deleteClicked();
  virtual void newClicked();
  virtual void removeClicked();
  virtual void insertClicked();
  virtual void renumberClicked();
  virtual void copyClicked();
  virtual void renumberChanged( int value );
  virtual void copyChanged( int value );
  virtual void helpClicked();
  virtual void cellChanged( int row, int col );
  virtual void selectionChanged();
  virtual void updateTopIfContinuous();
  virtual void loadRow( SlicerAngles * slanp, int row, bool block);
  virtual void switchTime( int newtime, bool doSet );
  virtual void finishOrFlushUnit( bool changed );
  virtual void topSlicerDrawing( float * angles, float cx, float cy, float cz, int time, int dragging, bool continuous );
  virtual void closeEvent( QCloseEvent * e );
  virtual void keyPressEvent( QKeyEvent * e );

  protected slots:
    virtual void languageChange();

 private:
  bool mIgnoreCurChg;
  int mTimeInc;
  int mMaxImageTime;
  int mMaxModelTime;
  int *mCurRow;
  int mCurTime;

  void init();
  void destroy();

};

#endif // SLICERANGLEFORM_H
