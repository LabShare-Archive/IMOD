/*   form_mouse.h  -  declarations for form_mouse.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef MOUSEFORM_H
#define MOUSEFORM_H

#include <qvariant.h>

class QButtonGroup;
typedef struct imod_pref_struct ImodPrefStruct;

#include "ui_form_mouse.h"

class MouseForm : public QWidget, public Ui::MouseForm
{
  Q_OBJECT

    public:
  MouseForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~MouseForm();

  public slots:
    virtual void init();
  virtual void update();
  virtual void flagChanged( int value );
  virtual void keyChanged( int value );
  virtual void mappingChanged( int value );
  virtual void swapToggled( bool state );

 protected:
  ImodPrefStruct *mPrefs;
  QButtonGroup *hotKeyGroup;
  QButtonGroup *activeGroup;
  QButtonGroup *mouseGroup;

  protected slots:
    virtual void languageChange();

 private:
    void setMouseLabels();

};

#endif // MOUSEFORM_H
