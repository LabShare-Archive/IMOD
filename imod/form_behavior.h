/*   form_behavior.h  -  declarations for form_behavior.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef BEHAVIORFORM_H
#define BEHAVIORFORM_H

#include <qvariant.h>
typedef struct imod_pref_struct ImodPrefStruct;

#include "ui_form_behavior.h"


class BehaviorForm : public QWidget, public Ui::BehaviorForm
{
    Q_OBJECT

public:
    BehaviorForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
    ~BehaviorForm();

public slots:
    virtual void init();
    virtual void setFontDependentWidths();
    virtual void update();
    virtual void unload();
    //virtual void toolTipsToggled( bool state );

protected:
    ImodPrefStruct *mPrefs;

protected slots:
    virtual void languageChange();

};

#endif // BEHAVIORFORM_H
