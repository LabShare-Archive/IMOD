#ifndef IMODSENDEVENT_H
#define IMODSENDEVENT_H
#include <qapplication.h>

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
