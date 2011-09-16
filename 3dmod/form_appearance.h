/*   form_appearance.h  -  declarations for form_appearance.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef APPEARANCEFORM_H
#define APPEARANCEFORM_H

#include <qvariant.h>
typedef struct imod_pref_struct ImodPrefStruct;
#include "ui_form_appearance.h"

class AppearanceForm : public QWidget, public Ui::AppearanceForm
{
  Q_OBJECT

    public:
  AppearanceForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~AppearanceForm();

  public slots:
    virtual void init();
  virtual void setFontDependentWidths();
  virtual void update();
  virtual void fontPressed();
  virtual void imagePtChanged( int value );
  virtual void modelPtChanged( int value );
  virtual void markerColorClicked();
  virtual void autoMeanChanged( int value );
  virtual void autoSDChanged( int value );
  virtual void styleSelected( const QString & key );
  virtual void setTargetClicked();
  virtual void unload();
  virtual void displayCurrentZoom();
  virtual void newZoomIndex( int value );
  virtual void unloadZoomValue();
  virtual void newZoomValue();
  virtual void shiftZoomsDown();
  virtual void shiftZoomsUp();
  virtual void restoreDefaultZooms();

 protected:
  bool mZoomValChanged;
  ImodPrefStruct *mPrefs;
  int mZoomIndex;

  protected slots:
    virtual void languageChange();

 private:
  void destroy();

};

#endif // APPEARANCEFORM_H
