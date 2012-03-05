/*
 *  imod_assistant.h - declarations for imod_assistant.cpp
 *
 *  $Id$
 */

#ifndef IMOD_ASSISTANT_H
#define IMOD_ASSISTANT_H

#include <qobject.h>
#include <qstring.h>
#include <qprocess.h>

class ImodAssistant : public QObject
{
  Q_OBJECT

public:
  ImodAssistant(const char *path, const char *adpFile, const char *messageTitle,
                bool absolute = false, bool keepSideBar = false, 
                const char *prefix = NULL, bool prefAbsolute = false);
  ~ImodAssistant();
  int showPage(const char *page);

 signals:
  void error(const QString &msg);

public slots:
  void assistantExited(int exitCode, QProcess::ExitStatus exitStatus);
  
private:
  QString mPath;
  QString mQhc;
  QString mImodDir;
  QString mPrefix;
  int mAssumedIMOD;
  bool mKeepSideBar;
  QProcess *mAssistant;
  QString mTitle;
  bool mExiting;
};

#endif
