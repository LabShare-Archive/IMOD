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
    //Save the root (file name without the extension) because it is used more
    //the the file name.  Currently chunk file names always end in ".com".
    QString comFileName = comFileArray.at(i);
    int extIndex = comFileName.lastIndexOf(".");
    QString root;
    if (extIndex == -1) {
      root = comFileName;
    }
    else {
      root = comFileName.mid(0, extIndex);
    }
    mJobArray[i].root = strdup(root.toLatin1().data());
    mJobArray[i].numChunkErr = 0;
    setFlagNotDone(i, singleFile);
  }
}

ComFileJobs::~ComFileJobs() {
  int i;
  for (i = 0; i < mNumJobs; i++) {
    free(mJobArray[i].root);
  }
  delete[] mJobArray;
}

//Set mFlag to -1 for sync com files.
//Return true for non-sync files.
void ComFileJobs::setFlagNotDone(const int index, const bool singleFile) {
  QString sRoot(mJobArray[index].root);
  if (singleFile || sRoot.endsWith("-start") || sRoot.endsWith("-finish")
      || sRoot.endsWith("-sync")) {
    mJobArray[index].flag = CHUNK_SYNC;
  }
  else {
    mJobArray[index].flag = CHUNK_NOT_DONE;
  }
}

/*
 $Log$
 Revision 1.1  2011/01/05 20:46:57  sueh
 bug# 1426 ComFileJobs is lighter weight then ComFileJob.  Only one
 instance is created.

 */

