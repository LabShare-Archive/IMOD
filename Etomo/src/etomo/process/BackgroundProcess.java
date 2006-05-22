package etomo.process;

import java.io.File;
import java.util.ArrayList;

import etomo.BaseManager;
import etomo.comscript.Command;
import etomo.comscript.CommandDetails;
import etomo.comscript.ProcessDetails;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessResultDisplay;
import etomo.ui.UIHarness;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.26  2006/05/11 19:51:23  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 3.25  2006/01/31 20:37:56  sueh
 * <p> bug# 521 Added setProcessResultDisplay to SystemProcessInterface.
 * <p> This allows the last ProcessResultDisplay used by the combine monitor
 * <p> to be assigned to the process.
 * <p>
 * <p> Revision 3.24  2006/01/26 21:50:07  sueh
 * <p> bug# 401 Added a ProcessResultDisplay member variable
 * <p>
 * <p> Revision 3.23  2006/01/20 20:49:31  sueh
 * <p> bug# 401 Make sure that errorFound is true if ProcessEndState is FAILED
 * <p> and the exit value is 1.
 * <p>
 * <p> Revision 3.22  2006/01/06 02:38:37  sueh
 * <p> bug# 792 Added getCommand().
 * <p>
 * <p> Revision 3.21  2005/11/30 21:14:23  sueh
 * <p> bug# 744 Fixed processDone().  Was not popping up error messages from
 * <p> standard out.
 * <p>
 * <p> Revision 3.20  2005/11/19 02:10:48  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetachedProcess
 * <p> without having to add unnecessary functions to it.
 * <p> Added storage to BackgroundProcess for ProcessDetails in addition to
 * <p> Command.  Command is used for all of BackgroundProcess's needs.
 * <p> ProcessDetails can be passed to postProcess() and errorProcess().
 * <p> Both are optional.
 * <p> DetachedProcess inherits BackgroundProcess.  Added get functions to
 * <p> BackgroundProcess so the member variables can stay private.  Moved
 * <p> the instanciation of the SystemProgram to newProgram so it can be
 * <p> overridden.
 * <p>
 * <p> Revision 3.19  2005/10/27 00:24:37  sueh
 * <p> bug# 725 Added another constructor which passes forceNextProcess.
 * <p>
 * <p> Revision 3.18  2005/10/21 19:54:53  sueh
 * <p> bug# 742 Added getCurrentStdError().
 * <p>
 * <p> Revision 3.17  2005/09/22 20:46:04  sueh
 * <p> bug# 532 Fixed bug in setProcessEndState().  EndState may come from
 * <p> the endState in the monitor, so it should be set there.
 * <p>
 * <p> Revision 3.16  2005/08/30 22:40:53  sueh
 * <p> bug# 532 Added error log print statement to setCurrentStdInput().
 * <p>
 * <p> Revision 3.15  2005/08/30 18:32:20  sueh
 * <p> bug# 532 ProcessMonitor had too many functions in it that where only
 * <p> used by ProcesschunkProcessMonitor.  Using
 * <p> BackgroundProcessMonitor with BackgroundProcess's which need a
 * <p> monitor.
 * <p>
 * <p> Revision 3.14  2005/08/27 22:20:32  sueh
 * <p> bug# 532 Added getMonitorErrorMessage() to get an error message from
 * <p> the monitor.  Changed SystemProgram.setCurrentStdInput() to throw
 * <p> IOException instead of handling it.  This allows
 * <p> IntermittentSystemProgram to handle a failed intermittent command.
 * <p>
 * <p> Revision 3.13  2005/08/22 16:15:26  sueh
 * <p> bug# 532 Added boolean acceptInputWhileRunning to prevent the closing
 * <p> of the standard in too early.  AcceptInputWhileRunning defaults to false
 * <p> because it prevents several types of processes from running.
 * <p>
 * <p> Revision 3.12  2005/08/15 18:16:20  sueh
 * <p> bug# 532 Changed pause() to throw an exception if the monitor isn't set.
 * <p>
 * <p> Revision 3.11  2005/08/15 18:06:28  sueh
 * <p> bug# 532   Processchunks needs to be killed with an interrupt instead of
 * <p> a kill, so a processchunks specific class has to make the decision of
 * <p> what type of signal to send.  BaseProcessManager.kill calls
 * <p> BackgroundProcess.kill.  If the monitor is set then
 * <p> BackgroundProcess.kill calls monitor.kill so that the processchunks
 * <p> monitor can ask for an interrupt instead of a kill.  Otherwise a kill is sent.
 * <p> The pause function always sends an interrupt, but it should only be used
 * <p> by processchunks.
 * <p> The processchunks monitor needs to be able to write to standard input
 * <p> after the interrupt is received.
 * <p> Added functions:  kill, paused, signalKill, signalInterrupt, and
 * <p> setCurrentStdInput.
 * <p>
 * <p> Revision 3.10  2005/08/04 19:41:21  sueh
 * <p> bug# 532 passing monitor to constructor when necessary.  Added
 * <p> getCurrentStdOutput() to get the standard output without waiting for the
 * <p> process to be finished.
 * <p>
 * <p> Revision 3.9  2005/07/29 00:50:56  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.8  2005/07/26 17:35:05  sueh
 * <p> bug# 701 Added ProcessEndState to record how the process ended.
 * <p>
 * <p> Revision 3.7  2005/05/18 22:34:07  sueh
 * <p> bug# 662 Added member variable boolean forceNextProcess to force
 * <p> BaseManager.startNextProcess() to be run regardless of the value of
 * <p> exitValue.
 * <p>
 * <p> Revision 3.6  2005/04/25 20:43:03  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.5  2005/01/08 01:47:14  sueh
 * <p> bug# 578 Removed getMode().  Use getCommand().getCommandMode().
 * <p>
 * <p> Revision 3.4  2004/12/16 02:24:32  sueh
 * <p> bug# 564 In run():  avoid using commandLine.  Use commandArray if it is
 * <p> available.
 * <p>
 * <p> Revision 3.3  2004/12/04 00:39:14  sueh
 * <p> bug# 569 Handling directory paths with spaces:  converting from a
 * <p> command line to a command array to prevent the command line from
 * <p> being split on white space.
 * <p>
 * <p> Revision 3.2  2004/11/19 23:17:18  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.1.2.5  2004/11/12 22:50:28  sueh
 * <p> bug# 520 Removed pass-through commands to Command.  Removed
 * <p> String getCommand() - using getCommandName instead.  Added
 * <p> Command getCommand().
 * <p>
 * <p> Revision 3.1.2.4  2004/11/08 22:18:17  sueh
 * <p> bug# 520 Added getMode(), which gets the mode the from Command.
 * <p>
 * <p> Revision 3.1.2.3  2004/10/18 17:46:22  sueh
 * <p> bug# 520 Fixed getCommandLine():  when the command is in an array,
 * <p> getCommandLine() should return the complete command in a string.
 * <p>
 * <p> Revision 3.1.2.2  2004/10/08 15:52:29  sueh
 * <p> bug# 520 Addded a command array option to the constructor.  Integrated
 * <p> the new Command option into the existing code (run, getCommand and
 * <p> getCommandLine).
 * <p>
 * <p> Revision 3.1.2.1  2004/10/06 01:34:35  sueh
 * <p> bug# 520 Using BaseProcessManager in BackgroundProcess.  Created a
 * <p>  constructor that constructs a BackgroundProcess with a Command.
 * <p> Added functions to get information from the Command.
 * <p>
 * <p> Revision 3.1  2004/08/30 18:42:02  sueh
 * <p> bug# 508 adding notifyKill()
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.6  2003/06/05 04:23:10  rickg
 * <p> method name change to getShellProcessID
 * <p>
 * <p> Revision 2.5  2003/05/23 22:48:44  rickg
 * <p> Implemented getting the PID of tcsh shell scripts that emmit it
 * <p> to stderr
 * <p>
 * <p> Revision 2.4  2003/05/23 14:27:36  rickg
 * <p> Implements SystemProcessInterface
 * <p>
 * <p> Revision 2.3  2003/05/12 23:24:54  rickg
 * <p> Comment fixes
 * <p>
 * <p> Revision 2.2  2003/05/08 23:19:03  rickg
 * <p> Standardized debug setting
 * <p>
 * <p> Revision 2.1  2003/01/29 20:45:11  rickg
 * <p> Debug messages to stderr instead of stdout
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:28:09  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2003/01/08 04:00:21  rickg
 * <p> Mods in progress
 * <p>
 * <p> Revision 1.2  2003/01/04 00:21:29  rickg
 * <p> Methods to get the command and command line.
 * <p>
 * <p> Revision 1.1  2003/01/03 00:56:19  rickg
 * <p> Initial revision
 * <p>
 * <p> </p>
 */
public class BackgroundProcess extends Thread implements SystemProcessInterface {

  public static final String rcsid = "$Id$";
  private String commandLine = null;
  private String[] commandArray = null;
  private File workingDirectory = null;
  private final BaseProcessManager processManager;
  private boolean demoMode = false;
  private boolean debug = false;
  private String[] stdOutput;
  private String[] stdError;
  private StringBuffer commandProcessID;
  private File outputFile = null;
  private ProcessDetails processDetails = null;
  private Command command = null;
  private CommandDetails commandDetails = null;
  private AxisID axisID;
  private boolean forceNextProcess = false;

  private String stdoutLogFile = "";
  private String stderrLogFile = "";

  private boolean started = false;
  private ProcessEndState endState = null;
  private final BaseManager manager;
  private SystemProgram program = null;
  private ProcessResultDisplay processResultDisplay = null;
  private ArrayList commandArrayList = null;

  public BackgroundProcess(BaseManager manager, ArrayList commandArrayList,
      BaseProcessManager processManager, AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    this.manager = manager;
    this.axisID = axisID;
    this.commandArrayList = commandArrayList;
    this.processManager = processManager;
    this.processResultDisplay = processResultDisplay;
    commandProcessID = new StringBuffer("");
  }

  public BackgroundProcess(BaseManager manager, CommandDetails commandDetails,
      BaseProcessManager processManager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    this.commandDetails = commandDetails;
    command = commandDetails;
    processDetails = commandDetails;
    this.commandArray = command.getCommandArray();
    this.processManager = processManager;
    commandProcessID = new StringBuffer("");
  }

  public BackgroundProcess(BaseManager manager, CommandDetails commandDetails,
      BaseProcessManager processManager, AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    this.manager = manager;
    this.axisID = axisID;
    command = commandDetails;
    processDetails = commandDetails;
    this.commandDetails = commandDetails;
    this.commandArray = command.getCommandArray();
    this.processManager = processManager;
    this.processResultDisplay = processResultDisplay;
    commandProcessID = new StringBuffer("");
  }

  public BackgroundProcess(BaseManager manager, Command command,
      BaseProcessManager processManager, AxisID axisID, boolean forceNextProcess) {
    this.manager = manager;
    this.axisID = axisID;
    this.command = command;
    this.commandArray = command.getCommandArray();
    this.processManager = processManager;
    this.forceNextProcess = forceNextProcess;
    commandProcessID = new StringBuffer("");
  }

  public BackgroundProcess(BaseManager manager, String[] commandArray,
      BaseProcessManager processManager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    this.commandArray = commandArray;
    this.processManager = processManager;
    commandProcessID = new StringBuffer("");
  }

  public BackgroundProcess(BaseManager manager, String[] commandArray,
      BaseProcessManager processManager, AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    this.manager = manager;
    this.axisID = axisID;
    this.commandArray = commandArray;
    this.processManager = processManager;
    this.processResultDisplay = processResultDisplay;
    commandProcessID = new StringBuffer("");
  }

  public BackgroundProcess(BaseManager manager, Command command,
      BaseProcessManager processManager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    this.command = command;
    this.commandArray = command.getCommandArray();
    this.processManager = processManager;
    commandProcessID = new StringBuffer("");
  }

  public BackgroundProcess(BaseManager manager, String[] commandArray,
      BaseProcessManager processManager, AxisID axisID, boolean forceNextProcess) {
    this.manager = manager;
    this.axisID = axisID;
    this.commandArray = commandArray;
    this.processManager = processManager;
    this.forceNextProcess = forceNextProcess;
    commandProcessID = new StringBuffer("");
  }

  public BackgroundProcess(BaseManager manager, String[] commandArray,
      BaseProcessManager processManager, AxisID axisID,
      boolean forceNextProcess, ProcessResultDisplay processResultDisplay) {
    this.manager = manager;
    this.axisID = axisID;
    this.commandArray = commandArray;
    this.processManager = processManager;
    this.forceNextProcess = forceNextProcess;
    this.processResultDisplay = processResultDisplay;
    commandProcessID = new StringBuffer("");
  }

  public final AxisID getAxisID() {
    return axisID;
  }

  final ProcessResultDisplay getProcessResultDisplay() {
    return processResultDisplay;
  }

  public final void setProcessResultDisplay(
      ProcessResultDisplay processResultDisplay) {
    this.processResultDisplay = processResultDisplay;
  }

  /**
   * Returns the demoMode.
   * @return boolean
   */
  public final boolean isDemoMode() {
    return demoMode;
  }

  /**
   * Returns the enableDebug.
   * @return boolean
   */
  public final boolean isDebug() {
    return debug;
  }

  final boolean isForceNextProcess() {
    return forceNextProcess;
  }

  /**
   * Returns the workingDirectory.
   * @return File
   */
  public final File getWorkingDirectory() {
    return workingDirectory;
  }

  protected Command getCommand() {
    return command;
  }

  /**
   * Returns the full command line.
   * @return File
   */
  public final String getCommandLine() {
    StringBuffer buffer;
    if (commandLine == null) {
      buffer = new StringBuffer();
      if (commandArray != null) {
        for (int i = 0; i < commandArray.length; i++) {
          buffer.append(commandArray[i] + " ");
        }
        commandLine = buffer.toString();
      }
      else if (command != null) {
        commandLine = command.getCommandLine().trim();
      }
      else if (commandDetails != null) {
        commandLine = commandDetails.getCommandLine().trim();
      }
      else if (commandArrayList != null) {
        buffer = new StringBuffer();
        for (int i = 0; i < commandArrayList.size(); i++) {
          buffer.append((String) commandArrayList.get(i));
        }
        commandLine = buffer.toString();
      }
    }
    if (commandLine == null) {
      throw new IllegalStateException("commandLine is null");
    }
    return commandLine.toString();
  }

  public final ProcessDetails getProcessDetails() {
    return processDetails;
  }

  /**
   * Returns command name of the process
   * @return File
   */
  public final String getCommandName() {
    if (command != null) {
      return command.getCommandName();
    }
    if (commandArray != null) {
      return commandArray[0];
    }
    String[] words = getCommandLine().split("\\s");
    return words[0];
  }

  /**
   * Set the working directory in which the com script is to be run.
   */
  public final void setWorkingDirectory(File workingDirectory) {
    this.workingDirectory = workingDirectory;
  }

  /**
   * Sets the demoMode.
   * @param demoMode The demoMode to set
   */
  public final void setDemoMode(boolean demoMode) {
    this.demoMode = demoMode;
  }

  /**
   * @param debug
   */
  public final void setDebug(boolean debug) {
    this.debug = debug;
  }

  protected boolean newProgram() {
    if (commandArray != null) {
      program = new SystemProgram(manager.getPropertyUserDir(), commandArray,
          axisID);
    }
    else if (command != null) {
      program = new SystemProgram(manager.getPropertyUserDir(), command
          .getCommandArray(), axisID);
    }
    else if (commandArrayList != null) {
      program = new SystemProgram(manager.getPropertyUserDir(),
          commandArrayList, axisID);
    }
    else {
      processDone(1);
      return false;
    }
    return true;
  }

  String getStatusString() {
    return null;
  }

  /**
   * Execute the command and notify the ProcessManager when it is done
   */
  public final void run() {
    started = true;
    if (!newProgram()) {
      return;
    }
    program.setWorkingDirectory(workingDirectory);
    program.setDebug(debug);

    if (demoMode) {
      try {
        sleep(3000);
        program.setExitValue(0);
      }
      catch (InterruptedException except) {
        except.printStackTrace();
        System.err.println("Sleep interrupted");
      }
    }
    else {
      // Execute the command
      ParsePID parsePID = new ParsePID(program, commandProcessID);
      Thread parsePIDThread = new Thread(parsePID);
      parsePIDThread.start();
      program.run();
    }

    //  Get any output from the command
    stdError = program.getStdError();
    stdOutput = program.getStdOutput();

    // Send a message back to the ProcessManager that this thread is done.
    processDone(getProgram().getExitValue());
  }

  protected final void processDone(int exitValue) {
    ProcessMessages processMessages = getProcessMessages();
    ProcessMessages monitorMessages = getMonitorMessages();
    //  Check to see if the exit value is non-zero
    ProcessEndState endState = getProcessEndState();
    boolean errorFound = false;
    if (exitValue == 0) {
      //treate any error message as a failure
      //popup error messages from the process
      if (processMessages.isError()) {
        errorFound = true;
        UIHarness.INSTANCE.openErrorMessageDialog(processMessages,
            "Process Error", axisID);
      }
      //popup error messages from the monitor
      if (monitorMessages != null && monitorMessages.isError()) {
        errorFound = true;
        UIHarness.INSTANCE.openErrorMessageDialog(monitorMessages,
            "Process Monitor Error", axisID);
        if (endState == ProcessEndState.FAILED) {
          errorFound = true;
        }
      }
    }
    else if (endState != ProcessEndState.KILLED
        && endState != ProcessEndState.PAUSED) {
      errorFound = true;
      ProcessMessages errorMessage = ProcessMessages.getInstance();
      //add the stderr
      errorMessage.addError("<html>Command failed: " + getCommandLine());
      if (stdError != null && stdError.length > 0) {
        errorMessage.addError();
        errorMessage.addError("<html><U>Standard error output:</U>");
        errorMessage.addError(stdError);
      }
      //add the last chunk error
      if (monitorMessages != null) {
        String chunkErrorMessage = monitorMessages.getLastChunkError();
        if (chunkErrorMessage != null) {
          errorMessage.addError();
          errorMessage.addError("<html><U>Last chunk error:</U>");
          errorMessage.addError(chunkErrorMessage);
        }
        //add any monitor error messages
        if (monitorMessages.isError()) {
          errorMessage.addError();
          errorMessage.addError("<html><U>Monitor error messages:</U>");
          errorMessage.addError(monitorMessages);
        }
      }
      errorMessage.addProcessOutput(stdOutput);
      //make sure script knows about failure
      setProcessEndState(ProcessEndState.FAILED);
      //popup error messages
      UIHarness.INSTANCE.openErrorMessageDialog(errorMessage, getCommandName()
          + " terminated", axisID);
    }
    processManager.msgProcessDone(this, exitValue, errorFound);
  }

  private final ProcessMessages getProcessMessages() {
    return program.getProcessMessages();
  }

  protected ProcessMessages getMonitorMessages() {
    return null;
  }

  protected final SystemProgram getProgram() {
    return program;
  }

  /**
   * Returns the stdError.
   * @return String[]
   */
  public final String[] getStdError() {
    return stdError;
  }

  /**
   * Returns the stdOutput.
   * @return String[]
   */
  public final String[] getStdOutput() {
    return stdOutput;
  }

  public final boolean isStarted() {
    return started;
  }

  /**
   * Get the shell process ID if it is available
   * @return
   */
  public final String getShellProcessID() {
    if (commandProcessID == null) {
      return "";
    }
    return commandProcessID.toString();
  }

  /**
   * nothing to do
   */
  public final void notifyKilled() {
    setProcessEndState(ProcessEndState.KILLED);
  }

  /**
   * set end state
   * @param endState
   */
  public synchronized void setProcessEndState(ProcessEndState endState) {
    this.endState = ProcessEndState.precedence(this.endState, endState);
  }

  ProcessEndState getProcessEndState() {
    return endState;
  }

  public void kill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }

  public void pause(AxisID axisID) {
    throw new IllegalStateException("pause is not valid in BackgroundProcess");
  }

  public void signalKill(AxisID axisID) {
    processManager.signalKill(this, axisID);
  }

  protected final void setProgram(SystemProgram program) {
    this.program = program;
  }
}