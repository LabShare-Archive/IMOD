package etomo.process;

import java.io.File;
import java.util.Vector;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstProcessSeries;
import etomo.type.ConstStringProperty;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.UIHarness;

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
  private final ProcessMonitor monitor;
  private final ProcessData processData;
  private final AxisID axisID;

  private ProcessEndState endState = null;
  private ProcessResultDisplay processResultDisplay = null;
  private ProcessMessages messages = null;
  private LogFile logFile;
  private String logSuccessTag = null;
  private boolean monitorControl = false;

  private ReconnectProcess(BaseManager manager,
      BaseProcessManager processManager, ProcessMonitor monitor,
      ProcessData processData, AxisID axisID) {
    this.manager = manager;
    this.processManager = processManager;
    this.monitor = monitor;
    this.processData = processData;
    this.axisID = axisID;
  }

  static ReconnectProcess getInstance(BaseManager manager,
      BaseProcessManager processManager, ProcessMonitor monitor,
      ProcessData processData, AxisID axisID) throws LogFile.FileException {
    ReconnectProcess instance = new ReconnectProcess(manager, processManager,
        monitor, processData, axisID);
    instance.logFile = LogFile.getInstance(manager.getPropertyUserDir(),
        axisID, processData.getProcessName());
    return instance;
  }

  static ReconnectProcess getMonitorInstance(BaseManager manager,
      BaseProcessManager processManager, ProcessMonitor monitor,
      ProcessData processData, AxisID axisID, String logFileName,
      String logSuccessTag, ConstStringProperty subDirName)
      throws LogFile.FileException {
    ReconnectProcess instance = new ReconnectProcess(manager, processManager,
        monitor, processData, axisID);
    if (subDirName.isEmpty()) {
      instance.logFile = LogFile.getInstance(manager.getPropertyUserDir(),
          logFileName);
    }
    else {
      instance.logFile = LogFile.getInstance(new File(manager
          .getPropertyUserDir(), subDirName.toString()), logFileName);
    }
    instance.logSuccessTag = logSuccessTag;
    instance.monitorControl = true;
    return instance;
  }

  public ConstProcessSeries getProcessSeries() {
    return null;
  }

  public void run() {
    if (processData == null || !processData.isRunning()) {
      return;
    }
    new Thread(monitor).start();
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    long logWriteId = LogFile.NO_ID;
    if (!monitorControl) {
      //make sure nothing else is writing or backing up the log file
      logWriteId = logFile.openForWriting();
    }
    while ((!monitorControl && processData.isRunning())
        || (monitorControl && monitor.isRunning())) {
      try {
        Thread.sleep(500);
      }
      catch (InterruptedException e) {
      }
    }
    if (!monitorControl) {
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
    }
    if (logSuccessTag == null) {
      messages = ProcessMessages.getInstance("Reconstruction of",
          "slices complete.");
    }
    else {
      messages = ProcessMessages.getInstance(logSuccessTag);
    }
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
    if (monitor!=null) {
      monitor.kill(this,axisID);
    }
   // processManager.signalKill(this, axisID);
  }

  public void notifyKilled() {
    setProcessEndState(ProcessEndState.KILLED);
  }

  public void signalKill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }

  public final void pause(AxisID axisID) {
    if (monitor == null) {
      UIHarness.INSTANCE.openMessageDialog("Unable to pause.",
          "Function Not Available");
    }
    else {
      monitor.pause(this, axisID);
    }
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
 * <p> Revision 1.6  2008/05/03 00:42:49  sueh
 * <p> bug# 847 Passing ProcessSeries to process object constructors so it can
 * <p> be passed to process done functions.
 * <p>
 * <p> Revision 1.5  2008/01/31 20:20:28  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.4  2008/01/14 20:33:09  sueh
 * <p> bug# 1050 Made class more generic.  Allowing any ProcessMonitor class
 * <p> instead of only FileSizeProcessMonitor classes.  Switched to getInstance to get
 * <p> standard instances.  Added getMonitorInstance to get an instance where the
 * <p> process is controlled by the monitor (processchunks).
 * <p>
 * <p> Revision 1.3  2006/10/10 05:13:31  sueh
 * <p> bug# 931 Managing the log file with LogFile.
 * <p>
 * <p> Revision 1.2  2006/08/03 21:31:55  sueh
 * <p> bug# 769 Run():  check the end of the log for a line that shows the process
 * <p> completed successfully.
 * <p>
 * <p> Revision 1.1  2006/08/02 22:26:44  sueh
 * <p> bug# 769 Attaches to an existing process on entering etomo.  Controls the
 * <p> monitor and checks processData.isRunning().
 * <p> </p>
 */
