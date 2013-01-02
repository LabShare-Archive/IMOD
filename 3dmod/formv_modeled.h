/*   formv_modeled.h  -  declarations for formv_modeled.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef IMODVMODELEDFORM_H
#define IMODVMODELEDFORM_H

#include <qvariant.h>
#include "ui_formv_modeled.h"
class QButtonGroup;
class imodvModeledForm : public QWidget, public Ui::imodvModeledForm
{
  Q_OBJECT

    public:
  imodvModeledForm(QWidget* parent = 0, Qt::WindowFlags fl = 0);
  ~imodvModeledForm();

  public slots:
    virtual void modelChanged( int which );
  virtual void editClicked( int which );
  virtual void moveClicked( int which );
  virtual void viewSelected( int which );
  virtual void nameChanged( const QString & name );
  virtual void newPixelSize();
  virtual void sameScaleClicked();
  virtual void donePressed();
  virtual void helpPressed();
  virtual QString getPixelString();
  virtual void setModel( int curModel, int numModels, QString filename, QString internalName, QString pixelString );
  virtual void setMoveEdit( int move, int edit );
  virtual void setViewSelection( int which );
  virtual void closeEvent( QCloseEvent * e );
  virtual void keyPressEvent( QKeyEvent * e );
  virtual void keyReleaseEvent( QKeyEvent * e );

  protected slots:
    virtual void languageChange();

 protected:
  QButtonGroup *moveGroup;
  QButtonGroup *editGroup;
};

#endif // IMODVMODELEDFORM_H
