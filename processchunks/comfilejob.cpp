/*
 * comfilejobs.cpp
 *
 *  Created on: Dec 16, 2010
 *      Author: sueh
 */

#include "comfilejob.h"

ComFileJob::ComFileJob() {
  mNumChunkErr = 0;
}

ComFileJob::~ComFileJob() {
}

void ComFileJob::setup(const QString &comFile, const bool singleFile) {
  mComFileName = comFile;
  setFlagNotDone(singleFile);
}

//Set mFlag to -1 for sync com files.
//Return true for non-sync files.
void ComFileJob::setFlagNotDone(const bool singleFile) {
  if (singleFile || mComFileName.endsWith("-start.com")
      || mComFileName.endsWith("-finish.com") || mComFileName.endsWith(
      "-sync.com")) {
    mFlag = sync;
  }
  else {
    mFlag = notDone;
  }
}

const QString ComFileJob::getRoot() {
  int i = mComFileName.lastIndexOf(".");
  if (i != -1) {
    return mComFileName.mid(0, i);
  }
  else {
    return mComFileName;
  }
}

/*
 $Log$
 */

