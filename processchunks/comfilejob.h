/*
 * comfilejob.h
 * Describes the .com files.  One ComFileJobs instance is created.
 *
 *  Created on: Dec 16, 2010
 *      Author: sueh
 */

#ifndef COMFILEJOB_H_
#define COMFILEJOB_H_

#include <QString>

class ComFileJob {
public:
  ComFileJob();
  ~ComFileJob();

  typedef enum flag {
    sync = -1, notDone, assigned, done
  } FlagType;

  void setup(const QString &comFile, const bool singleFile);
  inline const QString getLogFileName() {
    return QString("%1.log").arg(getRoot());
  }
  ;
  inline const QString getCshFileName() {
    return QString("%1.csh").arg(getRoot());
  }
  ;
  inline const QString getJobFileName() {
    return QString("%1.job").arg(getRoot());
  }
  ;
  inline const QString getQidFileName() {
    return QString("%1.qid").arg(getRoot());
  }
  ;
  inline const QString &getComFileName() {
    return mComFileName;
  }
  ;
  inline const int getNumChunkErr() {
    return mNumChunkErr;
  }
  ;
  inline void incrementNumChunkErr() {
    mNumChunkErr++;
  }
  ;
  void setFlagNotDone(const bool singleFile);
  inline void setFlag(const FlagType flag) {
    mFlag = flag;
  }
  ;
  inline const FlagType getFlag() {
    return mFlag;
  }
  ;
  const QString getRoot();

private:
  QString mComFileName;
  int mNumChunkErr;
  FlagType mFlag;
};

#endif /* COMFILEJOB_H_ */

/*
 $Log$
 */
