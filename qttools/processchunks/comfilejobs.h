/*
 *  A list of chunks (.com files).
 *
 *  Contains an array of chunks.  The maximum number of chunks is around
 *  100,000 so this class needs to be as scaleable as possible, minimizing its
 *  memory footprint.
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
 */

#ifndef COMFILEJOB_H_
#define COMFILEJOB_H_

class ComFileJobs {
public:
  ComFileJobs(const QStringList comFileArray, const bool singleFile);
  ~ComFileJobs();

  typedef struct job {
    char* root;
    int numChunkErr;
    int flag;
  } Job;

  void setup(const QString &comFile, const bool singleFile);
  inline const QString getLogFileName(const int index) {
    return QString("%1.log").arg(QString(mJobArray[index].root));
  }
  ;
  inline const QString getCshFileName(const int index) {
    /* return QString("%1.csh").arg(QString(mJobArray[index].root)); CSH -> PY */
    return QString("%1.py").arg(QString(mJobArray[index].root));
  }
  ;
  inline const QString getJobFileName(const int index) {
    return QString("%1.job").arg(QString(mJobArray[index].root));
  }
  ;
  inline const QString getQidFileName(const int index) {
    return QString("%1.qid").arg(QString(mJobArray[index].root));
  }
  ;
  //Currently the chunk files all end in .com.  If this changes this class will
  //have to know what chunk file extension to use for each chunk.
  inline const QString getComFileName(const int index) {
    return QString("%1.com").arg(QString(mJobArray[index].root));
  }
  ;
  inline int getNumChunkErr(const int index) {
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
  inline int getFlag(const int index) {
    return mJobArray[index].flag;
  }
  ;
  const char *getRoot(const int index) {
    return mJobArray[index].root;
  }
  ;

private:
  int mNumJobs;
  Job* mJobArray;
};

#endif /* COMFILEJOB_H_ */
