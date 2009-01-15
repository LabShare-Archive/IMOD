#ifndef IMODSENDEVENT_H
#define IMODSENDEVENT_H
#include <qapplication.h>
//Added by qt3to4:
#include <QTimerEvent>

class ImodSendEvent : public QApplication
{
  Q_OBJECT

 public:
  ImodSendEvent(int &argc, char **argv);
  ~ImodSendEvent() {};

 public slots:
  void clipboardChanged();
  
 protected:
  void timerEvent(QTimerEvent *e);

};

#endif
