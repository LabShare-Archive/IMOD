package etomo.process;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.ProcessSeries;
import etomo.comscript.DetachedCommandDetails;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
final class DetachedProcess extends BackgroundProcess {
  public static final String rcsid = "$Id$";

  private final AxisID axisID;
  private final BaseManager manager;
  private final OutfileProcessMonitor monitor;
  private final BaseProcessManager processManager;

  /**
   * If subdirName is set then multiple processes may be able to run in the same
   * directory under that same axis at the same time using different dataset
   * files (.epp).  The sub-directory is used to hide generic files that would
   * cause conflict.  Use the sub-directory name to create a unique run file
   * name.
   */
  private String subdirName = null;
  private String shortCommandName = "";

  DetachedProcess(BaseManager manager, DetachedCommandDetails commandDetails,
      BaseProcessManager processManager, AxisID axisID, OutfileProcessMonitor monitor,
      ProcessResultDisplay processResultDisplay, ProcessName processName,
      ProcessSeries processSeries, boolean popupChunkWarnings,
      final ProcessingMethod processingMethod) {
    super(manager, commandDetails, processManager, axisID, processName, processSeries,
        popupChunkWarnings);
    this.axisID = axisID;
    this.manager = manager;
    this.monitor = monitor;
    this.processManager = processManager;
    setProcessResultDisplay(processResultDisplay);
    ProcessData processData = getProcessData();
    processData.setProcessingMethod(processingMethod);
    if (monitor != null) {
      processData.setSubProcessName(monitor.getSubProcessName());
    }
  }

  void setSubdirName(String input) {
    subdirName = input;
    getProcessData().setSubDirName(input);
  }

  void setShortCommandName(String input) {
    shortCommandName = input;
  }

  boolean newProgram() {
    String[] runCommand;
    try {
      runCommand = createRunCommand();
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(), "Can't Run Process");
      return false;
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(), "Can't Run "
          + getCommandName());
      return false;
    }
    if (!getDetachedCommand().isValid()) {
      processDone(1);
      return false;
    }
    SystemProgram program = new BackgroundSystemProgram(manager, runCommand, monitor,
        axisID);
    program.setAcceptInputWhileRunning(true);
    setProgram(program);
    return true;
  }

  void waitForPid() {
    if (monitor == null) {
      super.waitForPid();
      return;
    }
    new Thread(new PidThread(monitor, getProcessData())).start();
  }

  /**
   * Returns false if the process will stop if Etomo exits.  Returns true if the
   * process can continue when Etomo exits.  Always returns true because the
   * process is detached.
   */
  public boolean isNohup() {
    return true;
  }

  private final String[] createRunCommand() throws IOException, LogFile.LockException {
    List<String> runCommand = new ArrayList<String>();
    runCommand.add("python");
    runCommand.add("-u");
    runCommand.add(BaseManager.getIMODBinPath() + "startprocess");
    		
    runCommand.add("-o");
    runCommand.add(monitor.getProcessOutputFileName());
    if (subdirName != null) {
      runCommand.add("-d");
      runCommand.add(subdirName);
    }
    String[] commandArray = getDetachedCommand().getCommandArray();
    if (commandArray != null) {
      for (int i = 0; i < commandArray.length; i++) {
        runCommand.add(commandArray[i]);
      }
    }
    return runCommand.toArray(new String[runCommand.size()]);
  }

  /**
   * @deprecated
   * @return
   * @throws IOException
   * @throws LogFile.LockException
   */
  private final String[] makeRunFile() throws IOException, LogFile.LockException {
    String commandName;
    if (subdirName == null) {
      commandName = getCommandName();
    }
    else {
      commandName = subdirName + "-" + shortCommandName;
    }
    AxisID axisID = getAxisID();
    File runFile = DatasetFiles.getShellScript(manager, commandName, axisID);
    if (runFile.exists()) {
      runFile.delete();
    }
    if (runFile.exists()) {
      runFile.delete();
    }
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(runFile));
    bufferedWriter.write("nohup");
    bufferedWriter.newLine();
    if (subdirName != null) {
      bufferedWriter.write("cd " + subdirName);
      bufferedWriter.newLine();
    }
    DetachedCommandDetails detachedCommandDetails = getDetachedCommand();
    if (detachedCommandDetails.isCommandNiced()) {
      bufferedWriter.write(detachedCommandDetails.getNiceCommand());
      bufferedWriter.newLine();
    }
    bufferedWriter.write(detachedCommandDetails.getCommandString() + " >& ");
    bufferedWriter.write(monitor.getProcessOutputFileName());
    bufferedWriter.write("&");
    bufferedWriter.newLine();
    bufferedWriter.close();
    if (getWorkingDirectory() == null) {
      setWorkingDirectory(new File(manager.getPropertyUserDir()));
    }
    String[] runCommand = new String[3];
    runCommand[0] = "tcsh";
    runCommand[1] = "-f";
    runCommand[2] = runFile.getName();
    return runCommand;
  }

  public String getShellProcessID() {
    if (monitor == null) {
      return super.getShellProcessID();
    }
    return monitor.getPid();
  }

  public void notifyKilled() {
    super.notifyKilled();
    if (monitor == null) {
      return;
    }
    monitor.endMonitor(getProcessEndState());
  }

  ProcessMessages getMonitorMessages() {
    return monitor.getProcessMessages();
  }

  public synchronized final void setProcessEndState(ProcessEndState endState) {
    super.setProcessEndState(endState);
    monitor.setProcessEndState(endState);
  }

  final ProcessEndState getProcessEndState() {
    return monitor.getProcessEndState();
  }

  void processDone(int exitValue, boolean errorFound) {
    processManager.msgProcessDone(this, exitValue, errorFound);
  }

  public final void kill(AxisID axisID) {
    monitor.kill(this, axisID);
  }

  public final void pause(AxisID axisID) {
    monitor.pause(this, axisID);
  }

  final String getStatusString() {
    return monitor.getStatusString();
  }

  private DetachedCommandDetails getDetachedCommand() {
    return (DetachedCommandDetails) getCommand();
  }

  private final class PidThread implements Runnable {
    private final OutfileProcessMonitor monitor;
    private final ProcessData processData;

    private PidThread(OutfileProcessMonitor monitor, ProcessData processData) {
      this.monitor = monitor;
      this.processData = processData;
    }

    public void run() {
      // wait until monitor is running
      while (!monitor.isProcessRunning()) {
        try {
          Thread.sleep(100);
        }
        catch (InterruptedException except) {
          return;
        }
      }
      // wait for pid
      String pid = null;
      while (pid == null && monitor.isProcessRunning()) {
        try {
          pid = monitor.getPid();
          Thread.sleep(100);
        }
        catch (InterruptedException except) {
          return;
        }
      }
      if (pid != null) {
        processData.setPid(pid);
        if (!processData.isEmpty()) {
          // Save the process data for this process so that it will already be saved
          // if there is a crash. This is especially important for Processchunks
          // because of the resume after exit functionality.
          manager.saveStorable(axisID, processData);
        }
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.28  2011/02/22 03:59:58  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.27  2011/02/03 06:00:27  sueh
 * <p> bug# 1422 Added the processing method to the constructor so it can be
 * <p> used for reconnecting.
 * <p>
 * <p> Revision 1.26  2010/11/13 16:03:45  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.25  2010/10/20 23:04:20  sueh
 * <p> bug# 1364 Removing unnecessary variable runInBackground.
 * <p>
 * <p> Revision 1.24  2010/10/19 06:38:09  sueh
 * <p> bug# 1364 Added runInBackground.
 * <p>
 * <p> Revision 1.23  2010/07/02 03:17:47  sueh
 * <p> bug# 1388 Added popupChunkWarnings to the constructor.
 * <p>
 * <p> Revision 1.22  2010/02/26 20:37:59  sueh
 * <p> Changing the complex popup titles are making it hard to complete the
 * <p> uitests.
 * <p>
 * <p> Revision 1.21  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.20  2009/09/01 03:17:56  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.19  2009/03/17 00:35:46  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.18  2009/02/04 23:24:42  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.17  2008/05/16 22:26:48  sueh
 * <p> bug# 1109 Setting subprocess name in processData from the monitor.
 * <p>
 * <p> Revision 1.16  2008/05/03 00:37:52  sueh
 * <p> bug# 847 Passing ProcessSeries to process object constructors so it can
 * <p> be passed to process done functions.
 * <p>
 * <p> Revision 1.15  2008/01/14 21:29:22  sueh
 * <p> bug# 1050 In setSubdirName setting processData.subDirName.
 * <p>
 * <p> Revision 1.14  2007/12/17 18:36:25  sueh
 * <p> bug# 1061 Added the option to write a second command line to the run file.
 * <p>
 * <p> Revision 1.13  2007/12/10 22:10:20  sueh
 * <p> bug# 1041 Removed subdirName from the constructor because it is optional.
 * <p>
 * <p> Revision 1.12  2007/11/06 19:21:12  sueh
 * <p> bug# 1-47 Allowed processchunks to be executed in a subdirectory.
 * <p>
 * <p> Revision 1.11  2007/05/11 15:39:59  sueh
 * <p> bug# 964 Overrode processDone(int,boolean) to call ProcessManager.
 * <p> msgProcessDone(DetachedProcess...).
 * <p>
 * <p> Revision 1.10  2006/12/01 00:56:01  sueh
 * <p> bug# 937 Overrode getShellProcessID and notifyKilled so that a process
 * <p> associated with this class can be killed.
 * <p>
 * <p> Revision 1.9  2006/10/11 10:08:14  sueh
 * <p> bug# 931 Added delete functionality to LogFile - changed BackupException to
 * <p> FileException.
 * <p>
 * <p> Revision 1.8  2006/07/20 23:13:34  sueh
 * <p> bug# 885 Check for a valid command before running.
 * <p>
 * <p> Revision 1.7  2006/06/15 16:16:53  sueh
 * <p> bug# 871 Added isNohup().
 * <p>
 * <p> Revision 1.6  2006/06/06 17:16:54  sueh
 * <p> bug# change threadData to processData.
 * <p>
 * <p> Revision 1.5  2006/06/05 16:24:25  sueh
 * <p> bug# 766 Added ProcessData to the base class.  Added class to get the pid from
 * <p> the monitor.
 * <p>
 * <p> Revision 1.4  2006/01/26 21:54:28  sueh
 * <p> bug# 401 Added a ProcessResultDisplay member variable
 * <p>
 * <p> Revision 1.3  2006/01/06 02:39:47  sueh
 * <p> bug# 792 Using DetachedCommand instead of Command because it can
 * <p> create a safe command string that can go into a run file.
 * <p>
 * <p> Revision 1.2  2005/11/19 02:21:40  sueh
 * <p> bug# 744 Added makeRunFile, newProgram, and pause.
 * <p>
 * <p> Revision 1.1  2005/11/14 21:24:28  sueh
 * <p> bug 744 A class that extends BackgroundProcess and runs detached so
 * <p> that Etomo can exit while the process is running.
 * <p> </p>
 */
