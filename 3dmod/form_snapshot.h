/*   form_behavior.h  -  declarations for form_behavior.cpp
 *
 *  $Id$
 *  $Log$
 *  Revision 4.1  2009/01/15 16:33:17  mast
 *  Qt 4 port
 *
 *
 */
#ifndef SNAPSHOTFORM_H
#define SNAPSHOTFORM_H

#include <qvariant.h>
typedef struct imod_pref_struct ImodPrefStruct;

#include "ui_form_snapshot.h"


class SnapshotForm : public QWidget, public Ui::SnapshotForm
{
    Q_OBJECT

public:
    SnapshotForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
    ~SnapshotForm();

public slots:
    virtual void init();
    virtual void setFontDependentWidths();
    virtual void update();
    virtual void unload();
    virtual void showOtherFormats(int item);


protected:
    ImodPrefStruct *mPrefs;

protected slots:
    virtual void languageChange();

};

#endif // SNAPSHOTFORM_H
