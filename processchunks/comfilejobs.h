/*
 *  List of chunks (.com files).
 *
 *  Contains an array of chunks.  The number of chunks is unlimited so this
 *  class needs to be as light-weight in terms of memory use as possible.
 *
 *  Contains the error count and the status of each chunk.
 *
 *  Author: Sue Held
 *
 *  Copyright (C) 2010,2011 by Boulder Laboratory for 3-Dimensional Electron
 *  Microscopy of Cells ("BL3DEMC") and the Regents of the University of
 *  Colorado.  See dist/COPYRIGHT for full copyright notice.
 *
 *  $Id$
 *  Log at end of file
 */

#ifndef COMFILEJOB_H_
#define COMFILEJOB_H_

class ComFileJobs {
public:
  ComFileJobs(const QStringList comFileArray, const bool singleFile);
  ~ComFileJobs();

  typedef struct job {
    QString comFileName;
    int numChunkErr;
    int flag;
  } Job;

  void setup(const QString &comFile, const bool singleFile);
  inline const QString getLogFileName(const int index) {
    return QString("%1.log").arg(getRoot(index));
  }
  ;
  inline const QString getCshFileName(const int index) {
    return QString("%1.csh").arg(getRoot(index));
  }
  ;
  inline const QString getJobFileName(const int index) {
    return QString("%1.job").arg(getRoot(index));
  }
  ;
  inline const QString getQidFileName(const int index) {
    return QString("%1.qid").arg(getRoot(index));
  }
  ;
  inline const QString &getComFileName(const int index) {
    return mJobArray[index].comFileName;
  }
  ;
  inline const int getNumChunkErr(const int index) {
    return mJobArray[index].numChunkErr;
  }
  ;
  inline void incrementNumChunkErr(const int index) {
    mJobArray[index].numChunkErr++;
  }
  ;
  void setFlagNotDone(const int index, const bool singleFile);
  inline void setFlag(const int index, const int flag) {
    mJobArray[index].flag = flag;
  }
  ;
  inline const int getFlag(const int index) {
    return mJobArray[index].flag;
  }
  ;
  const QString getRoot(const int index);

private:
  int mNumJobs;
  Job* mJobArray;
};

#endif /* COMFILEJOB_H_ */

/*
 $Log$
 */
