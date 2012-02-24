// A simple class to incorporate nice arrow icons into tool buttons

#include "arrowbutton.h"
#include <qpixmap.h>
#include <qicon.h>

/*!
 * A toolbutton with an arrow icon that is nicer than the Qt arrow button.  [type] should 
 * be one of Qt::UpArrow, Qt::DownArrow, Qt::LeftArrow, Qt::RightArrow.  [name] defaults 
 * to 0 and is now irrelevant.  To use this class, you must have the four arrow.png files
 * in a directory named images under the top directory of your project, and they must
 * be included in a resource file, as in 3dmod and midas.
 */
ArrowButton::ArrowButton ( Qt::ArrowType type, QWidget * parent, 
                           const char * name) 
  : QToolButton(parent)
{
  QString image;
  switch (type) {
  case Qt::LeftArrow:
    image = ":/images/leftarrow.png";
    break;
  case Qt::RightArrow:
    image = ":/images/rightarrow.png";
    break;
  case Qt::UpArrow:
    image = ":/images/uparrow.png";
    break;
  case Qt::DownArrow:
    image = ":/images/downarrow.png";
    break;
  }
  setIcon(QIcon(image));
}

ArrowButton::~ArrowButton()
{
}
