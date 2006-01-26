package etomo.process;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import etomo.BaseManager;
import etomo.comscript.DetachedCommand;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
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
  private final DetachedProcessMonitor monitor;

  public DetachedProcess(BaseManager manager, DetachedCommand command,
      BaseProcessManager processManager, AxisID axisID,
      DetachedProcessMonitor monitor) {
    super(manager, command, processManager, axisID);
    this.axisID = axisID;
    this.manager = manager;
    this.monitor = monitor;
  }
  
  public DetachedProcess(BaseManager manager, DetachedCommand command,
      BaseProcessManager processManager, AxisID axisID,
      DetachedProcessMonitor monitor, ProcessResultDisplay processResultDisplay) {
    super(manager, command, processManager, axisID);
    this.axisID = axisID;
    this.manager = manager;
    this.monitor = monitor;
    setProcessResultDisplay(processResultDisplay);
  }

  protected final boolean newProgram() {
    String[] runCommand;
    try {
      runCommand = makeRunFile();
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Can't Run "
          + getCommandName());
      return false;
    }
    SystemProgram program = new BackgroundSystemProgram(manager
        .getPropertyUserDir(), runCommand, monitor, axisID);
    program.setAcceptInputWhileRunning(true);
    setProgram(program);
    return true;
  }

  private final String[] makeRunFile() throws IOException {
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
  
  protected ProcessMessages getMonitorMessages() {
    return monitor.getProcessMessages();
  }

  public synchronized final void setProcessEndState(ProcessEndState endState) {
    super.setProcessEndState(endState);
    monitor.setProcessEndState(endState);
  }

  final ProcessEndState getProcessEndState() {
    return monitor.getProcessEndState();
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
}
/**
 * <p> $Log$
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