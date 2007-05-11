package etomo.process;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import etomo.BaseManager;
import etomo.comscript.DetachedCommand;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.UIHarness;
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
  
  public DetachedProcess(BaseManager manager, DetachedCommand command,
      BaseProcessManager processManager, AxisID axisID,
      OutfileProcessMonitor monitor, ProcessResultDisplay processResultDisplay, ProcessName processName) {
    super(manager, command, processManager, axisID,processName);
    this.axisID = axisID;
    this.manager = manager;
    this.monitor = monitor;
    this.processManager=processManager;
    setProcessResultDisplay(processResultDisplay);
  }

   final boolean newProgram() {
    String[] runCommand;
    try {
      runCommand = makeRunFile();
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Can't Run "
          + getCommandName());
      return false;
    }
    catch (LogFile.FileException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Can't Run "
          + getCommandName());
      return false;
    }
    if (!((DetachedCommand) getCommand()).isValid()) {
      processDone(1);
      return false;
    }
    SystemProgram program = new BackgroundSystemProgram(manager
        .getPropertyUserDir(), runCommand, monitor, axisID);
    program.setAcceptInputWhileRunning(true);
    setProgram(program);
    return true;
  }
  
  protected void waitForPid() {
    if (monitor == null) {
      super.waitForPid();
      return;
    }
    new Thread(new PidThread(monitor, getProcessData())).start();
  }
  
  /**
   * Always returns true because the process is detached.
   */
  public boolean isNohup() {
    return true;
  }

  private final String[] makeRunFile() throws IOException,LogFile.FileException {
    String commandName = getCommandName();
    AxisID axisID = getAxisID();
    File runFile = DatasetFiles.getShellScript(manager, commandName, axisID);
    if (runFile.exists()) {
      runFile.delete();
    }
    if (runFile.exists()) {
      runFile.delete();
    }
    BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(runFile));
    if (bufferedWriter == null) {
      throw new IOException("unable to write to " + runFile.getAbsolutePath());
    }
    bufferedWriter.write("nohup");
    bufferedWriter.newLine();
    bufferedWriter.write(((DetachedCommand) getCommand()).getCommandString()
        + " >& " + monitor.getProcessOutputFileName() + "&");
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
  
  void processDone(int exitValue,boolean errorFound) {
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
  
  final class PidThread implements Runnable {
    private final OutfileProcessMonitor monitor;
    private final ProcessData processData;
    
    PidThread(OutfileProcessMonitor monitor, ProcessData processData) {
      this.monitor = monitor;
      this.processData = processData;
    }
    
    public void run() {
      //wait until monitor is running
      while (!monitor.isProcessRunning()) {
        try {
          Thread.sleep(100);
        }
        catch (InterruptedException except) {
          return;
        } 
      }
      //wait for pid
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
      }
    }
  }
}
/**
 * <p> $Log$
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