/*   formv_views.h  -  declarations for formv_views.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef IMODVVIEWSFORM_H
#define IMODVVIEWSFORM_H

#include <qvariant.h>
#include "ui_formv_views.h"

class imodvViewsForm : public QWidget, public Ui::imodvViewsForm
{
  Q_OBJECT

public:
  imodvViewsForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~imodvViewsForm();
  void setFontDependentWidths();
  void init();
  void setAutostore( int state );
  void addItem( char * label );
  void selectItem( int item, bool block );
  void removeAllItems();
  void manageListWidth();

public slots:
 virtual void storePressed();
 virtual void revertPressed();
 virtual void newPressed();
 virtual void deletePressed();
 virtual void savePressed();
 virtual void autostoreToggled( bool state );
 virtual void viewSelected( int item );
 virtual void donePressed();
 virtual void newLabelEntered();
 virtual void helpPressed();
 virtual void closeEvent( QCloseEvent * e );
 virtual void keyPressEvent( QKeyEvent * e );
 virtual void keyReleaseEvent( QKeyEvent * e );
 virtual void changeEvent(QEvent *e);
 
protected slots:
  virtual void languageChange();

};

#endif // IMODVVIEWSFORM_H
