package etomo.process;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.Vector;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.util.DatasetFiles;

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

  ReconnectProcess(BaseManager manager, BaseProcessManager processManager,
      FileSizeProcessMonitor monitor, ProcessData processData, AxisID axisID) {
    this.manager = manager;
    this.processManager = processManager;
    this.monitor = monitor;
    this.processData = processData;
    this.axisID = axisID;
  }

  public void run() {
    if (processData == null || !processData.isRunning()) {
      return;
    }
    new Thread(monitor).start();
    while (processData.isRunning()) {
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }
    monitor.stop();
    while (!monitor.isRunning()) {
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }
    messages = ProcessMessages.getInstance();
    ProcessName processName = processData.getProcessName();
    try {
      messages.addProcessOutput(DatasetFiles.getLogFile(manager, axisID,
          processData.getProcessName()));
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    int exitValue = 0;
    if (messages.isError()) {
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
    BufferedReader logReader;
    try {
      logReader = new BufferedReader(new FileReader(DatasetFiles.getLogFile(
          manager, axisID, processData.getProcessName())));
    }
    catch (FileNotFoundException e) {
      return null;
    }
    Vector log = new Vector();
    String line;
    try {
      while ((line = logReader.readLine()) != null) {
        log.add(line);
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    if (log.size() == 0) {
      return null;
    }
    if (log.size() == 1) {
      return new String[] { (String) log.get(0) };
    }
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
 * <p> $Log$ </p>
 */
