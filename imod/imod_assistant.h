/*
 *  imod_assistant.h - declarations for imod_assistant.cpp
 */

/*  $Author$

    $Date$

    $Revision$

    $Log$
    Revision 1.6  2005/02/24 22:28:07  mast
    Added variables for fallback IMOD_DIR

    Revision 1.5  2004/12/24 02:11:40  mast
    Removed absolute argument from show page

    Revision 1.4  2004/12/06 04:39:19  mast
    Made truly standalone, took out of library back into 3dmod

    Revision 1.2  2004/12/04 19:21:21  mast
    Added DLL macro definition and use

    Revision 1.1  2004/12/04 02:05:39  mast
    Added to libdiaqt

    Revision 1.2  2004/11/24 18:30:16  mast
    Add adp file on startup

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
  ImodAssistant(const char *path, const char *adpFile, char *messageTitle,
                bool absolute = false, bool keepSideBar = false);
  ~ImodAssistant();
  int showPage(const char *page);

 signals:
  void error(const QString &msg);

public slots:
  void assistantError(const QString &msg);
  
private:
  QString mPath;
  QString mAdp;
  QString mImodDir;
  bool mAssumedIMOD;
  bool mKeepSideBar;
  QAssistantClient *mAssistant;
  QString mTitle;
};

#endif
