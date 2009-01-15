#ifndef ARROWBUTTON_H
#define ARROWBUTTON_H

#include <qtoolbutton.h>
#include "dllexport.h"

class DLL_IM_EX ArrowButton : public QToolButton
{
  Q_OBJECT
    public:

  ArrowButton ( Qt::ArrowType type, QWidget * parent, const char * name = 0 );
  ~ArrowButton();
};
#endif
