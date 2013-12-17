/*   form_autox.h  -  declarations for form_autox.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef AUTOXWINDOW_H
#define AUTOXWINDOW_H

#include <qvariant.h>

class MultiSlider;
class QButtonGroup;

#include "ui_form_autox.h"

class AutoxWindow : public QWidget, public Ui::AutoxWindow
{
    Q_OBJECT

public:
    AutoxWindow(QWidget* parent = 0, Qt::WindowFlags fl = 0);
    ~AutoxWindow();

public slots:
    virtual void init();
    virtual void setFontDependentWidths();
    virtual void contrastSelected( int which );
    virtual void sliderChanged( int which, int value, bool dragging );
    virtual void altMouse( bool state );
    virtual void followDiagonals( bool state );
    virtual void buildPressed();
    virtual void clearPressed();
    virtual void fillPressed();
    virtual void nextPressed();
    virtual void expandPressed();
    virtual void shrinkPressed();
    virtual void smoothPressed();
    virtual void helpPressed();
    virtual void setStates( int contrast, int threshold, int resolution, int altMouse, int follow );
    virtual void closeEvent( QCloseEvent * e );
    virtual void keyPressEvent( QKeyEvent * e );
    virtual void keyReleaseEvent( QKeyEvent * e );

protected:
    bool mCtrlPressed;
    MultiSlider *mSliders;
    QButtonGroup *contrastGroup;

protected slots:
    virtual void languageChange();

    virtual void changeEvent(QEvent *e);


};

#endif // AUTOXWINDOW_H
