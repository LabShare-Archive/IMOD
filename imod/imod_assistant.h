/*
 *  imod_assistant.h - declarations for imod_assistant.cpp
 */

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 1.1  2004/11/22 00:21:46  mast
    Addition to program

*/
#ifndef IMOD_ASSISTANT_H
#define IMOD_ASSISTANT_H

#include <qobject.h>
#include <qstring.h>
class QAssistantClient;

class ImodAssistant : public QObject
{
  Q_OBJECT

public:
  ImodAssistant(const char *path, const char *adpFile, bool absolute = false);
  ~ImodAssistant();
  void showPage(const char *page, bool absolute = false);

public slots:
  void assistantError(const QString &msg);
  
private:
  QString mPath;
  QString mAdp;
  QAssistantClient *mAssistant;
};

extern ImodAssistant *ImodHelp;
#endif
