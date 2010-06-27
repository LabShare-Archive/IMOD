/*
 * machinehandler.h
 *
 *  Created on: Jun 23, 2010
 *      Author: sueh
 */

#ifndef MACHINEHANDLER_H_
#define MACHINEHANDLER_H_

class QString;
class QTextStream;
class QTime;

class MachineHandler {

public:
  MachineHandler(char *machineName, int numCpus);
  MachineHandler(QString &machineName);
  ~MachineHandler();

  long nameToLong(bool *ok);
  void setValues(char *machineName, int numCpus);
  void incrementNumCpus();
  char *getName();
  void setup();
  void reset();

  bool operator==(QString &other);

private:

  QString *mName;
  int mNumCpus;
  bool *mAssigned;
  QTime *mStartTime;
  bool mFailed, mChunkErred;
};

#endif /* MACHINEHANDLER_H_ */
