/*   form_rawimage.h  -  declarations for form_rawimage.cpp
 *
 *  $Id$
 *  $Log$
 *
 */
#ifndef RAWIMAGEFORM_H
#define RAWIMAGEFORM_H

#include <qvariant.h>

typedef struct raw_image_info RawImageInfo;
class QButtonGroup;

#include "ui_form_rawimage.h"

class RawImageForm : public QDialog, public Ui::RawImageForm
{
  Q_OBJECT

    public:
  RawImageForm(QWidget* parent = 0, bool modal = false,
               Qt::WindowFlags fl = 0);
  ~RawImageForm();

  virtual void load( QString fileName, RawImageInfo * info );
  virtual void unload( RawImageInfo * info );

  public slots:
    virtual void manageState();

  protected slots:
    virtual void languageChange();

 protected:
  QButtonGroup *dataTypeGroup;

};

#endif // RAWIMAGEFORM_H
