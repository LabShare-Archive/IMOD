/*   formv_depthcue.h  -  declarations for formv_depthcue.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef IMODVDEPTHCUEFORM_H
#define IMODVDEPTHCUEFORM_H

#include <qvariant.h>
#include "ui_formv_depthcue.h"

class imodvDepthcueForm : public QWidget, public Ui::imodvDepthcueForm
{
    Q_OBJECT

public:
    imodvDepthcueForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
    ~imodvDepthcueForm();

public slots:
    virtual void init();
    virtual void depthcueToggled( bool state );
    virtual void displayStartLabel( int value );
    virtual void startChanged( int value );
    virtual void startPressed();
    virtual void startReleased();
    virtual void displayEndLabel( int value );
    virtual void endChanged( int value );
    virtual void endPressed();
    virtual void endReleased();
    virtual void donePressed();
    virtual void helpPressed();
    virtual void setStates( int enabled, int start, int end );
    virtual void closeEvent( QCloseEvent * e );
    virtual void keyPressEvent( QKeyEvent * e );
    virtual void keyReleaseEvent( QKeyEvent * e );

protected:
    bool mEndPressed;
    QString mStr;
    bool mStartPressed;
    int mStartDisplayed;
    int mEndDisplayed;
    bool mCtrlPressed;

protected slots:
    virtual void languageChange();

};

#endif // IMODVDEPTHCUEFORM_H
