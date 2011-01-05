/*
 *  List of chunks (.com files).
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

#include "processchunks.h"

ComFileJobs::ComFileJobs(const QStringList comFileArray, const bool singleFile) {
  mNumJobs = comFileArray.size();
  mJobArray = new Job[mNumJobs];
  int i;
  for (i = 0; i < mNumJobs; i++) {
    mJobArray[i].comFileName = comFileArray.at(i);
    mJobArray[i].numChunkErr = 0;
    setFlagNotDone(i, singleFile);
  }
}

ComFileJobs::~ComFileJobs() {
  delete[] mJobArray;
}

//Set mFlag to -1 for sync com files.
//Return true for non-sync files.
void ComFileJobs::setFlagNotDone(const int index, const bool singleFile) {
  if (singleFile || mJobArray[index].comFileName.endsWith("-start.com")
      || mJobArray[index].comFileName.endsWith("-finish.com")
      || mJobArray[index].comFileName.endsWith("-sync.com")) {
    mJobArray[index].flag = CHUNK_SYNC;
  }
  else {
    mJobArray[index].flag = CHUNK_NOT_DONE;
  }
}

const QString ComFileJobs::getRoot(const int index) {
  int i = mJobArray[index].comFileName.lastIndexOf(".");
  if (i != -1) {
    return mJobArray[index].comFileName.mid(0, i);
  }
  else {
    return mJobArray[index].comFileName;
  }
}

/*
 $Log$
 */

