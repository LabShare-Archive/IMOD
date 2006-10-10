package etomo.process;

import java.util.Vector;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class ReconnectProcess implements SystemProcessInterface, Runnable {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;
  private final BaseProcessManager processManager;
  private final FileSizeProcessMonitor monitor;
  private final ProcessData processData;
  private final AxisID axisID;

  private ProcessEndState endState = null;
  private ProcessResultDisplay processResultDisplay = null;
  private ProcessMessages messages = null;
  private final LogFile logFile;

  ReconnectProcess(BaseManager manager, BaseProcessManager processManager,
      FileSizeProcessMonitor monitor, ProcessData processData, AxisID axisID) {
    this.manager = manager;
    this.processManager = processManager;
    this.monitor = monitor;
    this.processData = processData;
    this.axisID = axisID;
    logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
        processData.getProcessName());
  }

  public void run() {
    if (processData == null || !processData.isRunning()) {
      return;
    }
    new Thread(monitor).start();
    //make sure nothing else is writing or backing up the log file
    long logWriteId = LogFile.NO_ID;
    logWriteId = logFile.openForWriting();
    while (processData.isRunning()) {
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }
    //release the log file
    logFile.closeForWriting(logWriteId);
    monitor.stop();
    while (!monitor.isRunning()) {
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }
    messages = ProcessMessages.getInstance("Reconstruction of",
        "slices complete.");
    ProcessName processName = processData.getProcessName();
    try {
      messages.addProcessOutput(logFile);
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    int exitValue = 0;
    if (messages.isError() || !messages.isSuccess()) {
      exitValue = 1;
    }
    processManager.msgReconnectDone(this, exitValue);
  }

  public ProcessData getProcessData() {
    return processData;
  }

  public String getShellProcessID() {
    return processData.getPid();
  }

  /**
   * No access to standard error
   */
  public String[] getStdError() {
    return null;
  }

  /**
   * Return the log file as the standard output
   */
  public String[] getStdOutput() {
    //BufferedReader logReader;
    long readId = LogFile.NO_ID;
    try {
      readId = logFile.openReader();
      //logReader = new BufferedReader(new FileReader(DatasetFiles.getLogFile(
      //    manager, axisID, processData.getProcessName())));
    }
    catch (LogFile.ReadException e) {
      return null;
    }
    Vector log = new Vector();
    String line;
    try {
      while ((line = logFile.readLine(readId)) != null) {
        log.add(line);
      }
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    if (log.size() == 0) {
      logFile.closeReader(readId);
      return null;
    }
    if (log.size() == 1) {
      logFile.closeReader(readId);
      return new String[] { (String) log.get(0) };
    }
    logFile.closeReader(readId);
    return (String[]) log.toArray(new String[log.size()]);
  }

  ProcessMessages getProcessMessages() {
    return messages;
  }

  /**
   * Always true because always true in ComScriptProcess and we are only doing
   * tilt so far
   */
  public boolean isNohup() {
    return true;
  }

  /**
   * Always true because we reconnecting to an existing process
   */
  public boolean isStarted() {
    return true;
  }

  public final void kill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }

  public void notifyKilled() {
    setProcessEndState(ProcessEndState.KILLED);
  }

  public void signalKill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }

  public void pause(AxisID axisID) {
    throw new IllegalStateException("pause is not used by any ReconnectProcess");
  }

  public void setProcessEndState(ProcessEndState endState) {
    if (monitor == null) {
      this.endState = endState;
    }
    else {
      monitor.setProcessEndState(endState);
    }
  }

  public final void setProcessResultDisplay(
      ProcessResultDisplay processResultDisplay) {
    this.processResultDisplay = processResultDisplay;
  }

  final ProcessEndState getProcessEndState() {
    if (monitor == null) {
      return endState;
    }
    return monitor.getProcessEndState();
  }

  final AxisID getAxisID() {
    return axisID;
  }

  final ProcessResultDisplay getProcessResultDisplay() {
    return processResultDisplay;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2006/08/03 21:31:55  sueh
 * <p> bug# 769 Run():  check the end of the log for a line that shows the process
 * <p> completed successfully.
 * <p>
 * <p> Revision 1.1  2006/08/02 22:26:44  sueh
 * <p> bug# 769 Attaches to an existing process on entering etomo.  Controls the
 * <p> monitor and checks processData.isRunning().
 * <p> </p>
 */
