package etomo.process;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Map;
import java.util.Vector;

import etomo.BaseManager;
import etomo.ProcessSeries;
import etomo.ProcessingMethodMediator;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstStringProperty;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.ui.swing.UIHarness;

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
public final class ReconnectProcess implements SystemProcessInterface, Runnable {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;

  private final BaseProcessManager processManager;
  private final ProcessMonitor monitor;
  private final ProcessData processData;
  private final AxisID axisID;
  private final boolean popupChunkWarnings;

  private ProcessSeries processSeries;
  private ProcessEndState endState = null;
  private ProcessResultDisplay processResultDisplay = null;
  private ProcessMessages messages = null;
  private LogFile logFile;
  private String logSuccessTag = null;
  // When true the process is being controlled by the monitor.
  private boolean monitorControl = false;

  /**
   * Currently handles levels<=2.  Add levels parameter to dumpState calls to handle
   * levels>2.
   * @param levels
   */
  public void dumpState(int levels) {
    System.err.print("[");
    levels--;
    if (levels > 0) {
      System.err.println("manager:");
      if (manager != null) {
        manager.dumpState();
      }
      System.err.println(",processManager:");
      if (processManager != null) {
        processManager.dumpState();
      }
      System.err.println(",monitor:");
      if (monitor != null) {
        monitor.dumpState();
      }
      System.err.println(",processData:");
      if (processData != null) {
        processData.dumpState();
      }
      System.err.println(",axisID:");
      if (axisID != null) {
        axisID.dumpState();
      }
      System.err.println(",processSeries:");
      if (processSeries != null) {
        processSeries.dumpState();
      }
      System.err.println(",endState:");
      if (endState != null) {
        endState.dumpState();
      }
      System.err.println(",processResultDisplay:");
      if (processResultDisplay != null) {
        processResultDisplay.dumpState();
      }
      System.err.println(",messages:");
      if (messages != null) {
        messages.dumpState();
      }
      System.err.println(",logFile:");
      if (logFile != null) {
        logFile.dumpState();
      }
    }
    System.err.println(",logSuccessTag:" + logSuccessTag + "]");
  }

  private ReconnectProcess(BaseManager manager, BaseProcessManager processManager,
      ProcessMonitor monitor, ProcessData processData, AxisID axisID,
      final ProcessSeries processSeries, final boolean popupChunkWarnings) {
    this.manager = manager;
    this.processManager = processManager;
    this.monitor = monitor;
    this.processData = processData;
    this.axisID = axisID;
    this.processSeries = processSeries;
    this.popupChunkWarnings = popupChunkWarnings;
  }

  static ReconnectProcess getInstance(BaseManager manager,
      BaseProcessManager processManager, ProcessMonitor monitor, ProcessData processData,
      AxisID axisID, final ProcessSeries processSeries) throws LogFile.LockException {
    ReconnectProcess instance = new ReconnectProcess(manager, processManager, monitor,
        processData, axisID, processSeries, true);
    instance.logFile = LogFile.getInstance(manager.getPropertyUserDir(), axisID,
        processData.getProcessName());
    return instance;
  }

  static ReconnectProcess getLogInstance(BaseManager manager,
      BaseProcessManager processManager, ProcessMonitor monitor, ProcessData processData,
      AxisID axisID, String logFileName, String logSuccessTag,
      ConstStringProperty subDirName, final ProcessSeries processSeries,
      final boolean popupChunkWarnings) throws LogFile.LockException {
    ReconnectProcess instance = new ReconnectProcess(manager, processManager, monitor,
        processData, axisID, processSeries, popupChunkWarnings);
    if (subDirName.isEmpty()) {
      instance.logFile = LogFile.getInstance(manager.getPropertyUserDir(), logFileName);
    }
    else {
      instance.logFile = LogFile.getInstance(new File(manager.getPropertyUserDir(),
          subDirName.toString()), logFileName);
    }
    instance.logSuccessTag = logSuccessTag;
    instance.monitorControl = true;
    return instance;
  }

  /**
   * In a reconnect the computerMap is loaded from the data file.  Setting it
   * here would override it.  Should never happen because the process monitor
   * should not set the computer map in the process during a reconnect.
   */
  public final void setComputerMap(Map<String,String> computerMap) {
  }

  public ProcessSeries getProcessSeries() {
    return processSeries;
  }

  public void setProcessingMethod(ProcessingMethod processingMethod) {
    // already have processing method from process data
  }

  public ProcessingMethod getProcessingMethod() {
    if (processData == null) {
      return null;
    }
    return processData.getProcessingMethod();
  }

  public void run() {
    if (processData == null || !processData.isRunning()
        || processData.isOnDifferentHost()) {
      return;
    }

    ProcessingMethodMediator mediator = manager.getProcessingMethodMediator(axisID);
    mediator.register(this);

    new Thread(monitor).start();
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    LogFile.WritingId logWritingId = null;
    try {
      if (!monitorControl) {
        // make sure nothing else is writing or backing up the log file
        logWritingId = logFile.openForWriting();
      }
      while ((!monitorControl && processData.isRunning())
          || (monitorControl && monitor.isRunning())) {
        try {
          Thread.sleep(500);
        }
        catch (InterruptedException e) {
        }
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    if (!monitorControl) {
      // release the log file
      logFile.closeForWriting(logWritingId);
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
      messages = ProcessMessages.getInstance(manager, "Reconstruction of",
          "slices complete.");
    }
    else {
      messages = ProcessMessages.getInstance(manager, logSuccessTag);
    }
    ProcessName processName = processData.getProcessName();
    try {
      messages.addProcessOutput(logFile);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    int exitValue = 0;
    if (messages.isError() || !messages.isSuccess()) {
      exitValue = 1;
    }
    processManager.msgReconnectDone(this, exitValue, popupChunkWarnings);
    mediator.deregister(this);
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
    // BufferedReader logReader;
    LogFile.ReaderId readerId = null;
    try {
      readerId = logFile.openReader();
      // logReader = new BufferedReader(new FileReader(DatasetFiles.getLogFile(
      // manager, axisID, processData.getProcessName())));
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return null;
    }
    catch (FileNotFoundException e) {
      return null;
    }
    Vector log = new Vector();
    String line;
    try {
      while ((line = logFile.readLine(readerId)) != null) {
        log.add(line);
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    if (log.size() == 0) {
      logFile.closeRead(readerId);
      return null;
    }
    if (log.size() == 1) {
      logFile.closeRead(readerId);
      return new String[] { (String) log.get(0) };
    }
    logFile.closeRead(readerId);
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

  public boolean isDone() {
    if (monitor == null) {
      return false;
    }
    return !monitor.isRunning();
  }

  /**
   * Always true because we reconnecting to an existing process
   */
  public boolean isStarted() {
    return true;
  }

  public final void kill(AxisID axisID) {
    if (monitor != null) {
      monitor.kill(this, axisID);
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
      UIHarness.INSTANCE.openMessageDialog(manager, "Unable to pause.",
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

  public final void setProcessResultDisplay(ProcessResultDisplay processResultDisplay) {
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
 * <p> Revision 1.15  2011/02/22 04:09:57  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.14  2011/02/03 06:05:46  sueh
 * <p> bug# 1422 Getting the processing method when reconnecting.
 * <p>
 * <p> Revision 1.13  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.12  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.11  2009/04/20 20:05:46  sueh
 * <p> bug# 1192 Added a empty setComputerMap function.  The process is not
 * <p> the source of process information so setComputerMap shouldn't do
 * <p> anything.
 * <p>
 * <p> Revision 1.10  2009/04/13 22:52:17  sueh
 * <p> bug# 1207
 * <p>
 * <p> Revision 1.9  2009/03/17 00:43:53  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.8  2009/02/04 23:26:53  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.7  2008/05/16 22:47:03  sueh
 * <p> bug# 1109 Calling monitor.kill instead of signalKill.  Kill processes call
 * <p> signalKill.  And for processchunks signalKill, which sends a kill signal
 * <p> doesn't work as well as sending a message to the pipe file.  Also enabling
 * <p> pause so it can be used by processchunks.
 * <p>
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
