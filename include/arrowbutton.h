#include <qtoolbutton.h>
class ArrowButton : public QToolButton
{
  Q_OBJECT
    public:

  ArrowButton ( ArrowType type, QWidget * parent, const char * name = 0 );
  ~ArrowButton();
};
