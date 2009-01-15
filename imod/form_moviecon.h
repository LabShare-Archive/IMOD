/*   form_moviecon.h  -  declarations for form_moviecon.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef MOVIECONTROLLER_H
#define MOVIECONTROLLER_H

#include <qvariant.h>

class MultiSlider;
class QButtonGroup;

#include "ui_form_moviecon.h"

class MovieController : public QWidget, public Ui::MovieController
{
    Q_OBJECT

public:
    MovieController(QWidget* parent = 0, Qt::WindowFlags fl = 0);
    ~MovieController();

    virtual void setNonTifLabel();

public slots:
    virtual void init();
    virtual void setFontDependentWidths();
    virtual void axisSelected( int which );
    virtual void sliderChanged( int which, int value, bool dragging );
    virtual void upPressed();
    virtual void downPressed();
    virtual void rateEntered();
    virtual void snapshotSelected( int which );
    virtual void extentSelected( int which );
    virtual void startHereSelected( int which );
    virtual void montageToggled( int state );
    virtual void newMontageValue( int value );
    virtual void wholeImageToggled( bool state );
    virtual void scaleThickToggled( bool state );
    virtual void scalingChanged( int value );
    virtual void donePressed();
    virtual void helpPressed();
    virtual void resetPressed();
    virtual void enableTime( int enabled );
    virtual void setActualRate( QString str );
    virtual void setRateBox( float value );
    virtual void setSliders( int start, int maxStart, int end, int minEnd, int maxEnd, int increment, int enable );
    virtual void closeEvent( QCloseEvent * e );
    virtual void keyPressEvent( QKeyEvent * e );
    virtual void keyReleaseEvent( QKeyEvent * e );
    virtual void fontChange( const QFont & oldFont );

protected:
    MultiSlider *mSliders;
    QButtonGroup *axisGroup;
    QButtonGroup *snapshotGroup;
    QButtonGroup *extentGroup;
    QButtonGroup *startHereGroup;

protected slots:
    virtual void languageChange();

};

#endif // MOVIECONTROLLER_H
