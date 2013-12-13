/*   formv_cont_edit.h  -  declarations for formv_cont_edit.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef CONTSURFPOINT_H
#define CONTSURFPOINT_H

#include <qvariant.h>
#include "ui_form_cont_edit.h"

class ContSurfPoint : public QWidget, public Ui::ContSurfPoint
{
    Q_OBJECT

public:
    ContSurfPoint(QWidget* parent = 0, Qt::WindowFlags fl = 0);
    ~ContSurfPoint();

public slots:
    virtual void init();
    virtual void setFontDependentWidths();
    virtual void surfaceChanged( int value );
    virtual void upContPressed();
    virtual void downContPressed();
    virtual void newSurfPressed();
    virtual void surfGhostToggled( bool state );
    virtual void surfLabelChanged( const QString & str );
    virtual void closedClicked();
    virtual void openClicked();
    virtual void timeChanged( int value );
    virtual void contLabelChanged( const QString & str );
    virtual void pointSliderChanged( int value );
    virtual void displayPointSize( float value, int defval );
    virtual void pointSliderPressed();
    virtual void pointSliderReleased();
    virtual void pointSizeEntered();
    virtual void mouseSizeToggled( bool state );
    virtual void pointLabelChanged( const QString & str );
    virtual void ghostChanged( int value );
    virtual void upGhostToggled( bool state );
    virtual void downGhostToggled( bool state );
    virtual void lighterGhostToggled( bool state );
    virtual void allObjGhostToggled( bool state );
    virtual void helpPressed();
    virtual void setClosedOpen( int open, int enabled );
    virtual void setGhostState( int interval, int ghostmode );
    virtual void setPointSize( float size, int defval );
    virtual void setSurface( int value, int maxVal );
    virtual void setTimeIndex( int value, int maxVal );
    virtual void setLabels( QString surfLabel, int noSurf, QString contLabel, int noCont, QString ptLabel, int noPoint );
    virtual void closeEvent( QCloseEvent * e );
    virtual void keyPressEvent( QKeyEvent * e );
    virtual void keyReleaseEvent( QKeyEvent * e );

protected:
    float mSizeDisplayed;
    bool mSliderPressed;
    bool mCtrlPressed;

protected slots:
    virtual void languageChange();

    virtual void changeEvent(QEvent *e);


};

#endif // CONTSURFPOINT_H
