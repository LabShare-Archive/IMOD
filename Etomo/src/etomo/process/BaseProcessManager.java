package etomo.process;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.Command;
import etomo.comscript.ComscriptState;
import etomo.comscript.LoadAverageParam;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.ui.UIHarness;
import etomo.util.Utilities;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.21  2005/08/27 22:23:50  sueh
* <p> bug# 532 In msgBackgroundPRocessDone() exclude errors starting with
* <p> "CHUNK ERROR:".  These error may be repeats many times and should
* <p> be handled by the monitor.  Also try to append an error string from the
* <p> monitor.  This allows the processchunks monitor to supply the last chunk
* <p> error it found.
* <p>
* <p> Revision 1.20  2005/08/22 16:17:46  sueh
* <p> bug# 532 Added start and stopGetLoadAverage().
* <p>
* <p> Revision 1.19  2005/08/15 17:55:29  sueh
* <p> bug# 532   Processchunks needs to be killed with an interrupt instead of
* <p> a kill, so a processchunks specific class has to make the decision of
* <p> what type of signal to send.  Change BaseProcessManager.kill() to call
* <p> SystemProcessInterface.kill().  When the correct signal is chosen,
* <p> SystemProcessInterface will call either signalInterrupt or signalKill.  Also
* <p> added pause(), which will be associaated with the Pause button and will
* <p> work like kill.  Added functions:  getThread, interruptProcess, pause,
* <p> signalInterrupt, signalKill.
* <p>
* <p> Revision 1.18  2005/08/04 19:42:50  sueh
* <p> bug# 532 Passing monitor to BackgroundProcess when necessary.
* <p>
* <p> Revision 1.17  2005/08/01 18:00:42  sueh
* <p> bug# 532 In msgBackgroundProcessDone() passed process.axisID
* <p> instead of null to processDone().
* <p>
* <p> Revision 1.16  2005/07/29 00:51:13  sueh
* <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
* <p> because the current manager changes when the user changes the tab.
* <p> Passing the manager where its needed.
* <p>
* <p> Revision 1.15  2005/07/26 17:57:12  sueh
* <p> bug# 701 Changing all comscript monitors to implement ProcessMonitor
* <p> so that they call all be passed to the ComScriptProcess constructor.
* <p> Changed kill(AxisID).  Before killing, call
* <p> SystemProcessInterface.setProcessEndState().  After killing call
* <p> SystemProcessInterface.notifyKilled().  This sets the end state to kill as
* <p> soon as possible.  NotifyKilled() (was notifyKill) is used to stop monitors
* <p> that can't receives interruption from the processes they are monitoring.
* <p> Modify msgComScriptDone() and msgBackgroundProcessDone().  Do not
* <p> open an error dialog if a kill or pause ProcessEndState is set.  Set the
* <p> process's ProcessEndState to FAILED if an error dialo is being opened.
* <p> The process may or may not find out about the error, so the function that
* <p> finds the problem needs to set ProcessEndState.
* <p>
* <p> Revision 1.14  2005/07/01 21:08:00  sueh
* <p> bug# 619 demo:  temporarily made isAxisBusy public
* <p>
* <p> Revision 1.13  2005/06/21 00:42:24  sueh
* <p> bug# 522 Added moved touch() from JoinProcessManager to
* <p> BaseProcessManager for MRCHeaderTest.
* <p>
* <p> Revision 1.12  2005/05/18 22:34:19  sueh
* <p> bug# 662 Added member variable boolean forceNextProcess to force
* <p> BaseManager.startNextProcess() to be run regardless of the value of
* <p> exitValue.
* <p>
* <p> Revision 1.11  2005/04/26 17:36:26  sueh
* <p> bug# 615 Change the name of the UIHarness member variable to
* <p> uiHarness.
* <p>
* <p> Revision 1.10  2005/04/25 20:44:28  sueh
* <p> bug# 615 Passing the axis where a command originates to the message
* <p> functions so that the message will be popped up in the correct window.
* <p> This requires adding AxisID to many objects.  Move the interface for
* <p> popping up message dialogs to UIHarness.  It prevents headless
* <p> exceptions during a test execution.  It also allows logging of dialog
* <p> messages during a test.  It also centralizes the dialog interface and
* <p> allows the dialog functions to be synchronized to prevent dialogs popping
* <p> up in both windows at once.  All Frame functions will use UIHarness as a
* <p> public interface.
* <p>
* <p> Revision 1.9  2005/01/05 19:52:39  sueh
* <p> bug# 578 Moved startBackgroundComScript(String, Runnable, AxisID,
* <p> ComscriptState, String) and startComScript(String, Runnable, AxisID,
* <p> String) from ProcessManager to BaseProcessManager since they are
* <p> generic.  Added startComScript(Command, Runnable, AxisID) to handle
* <p> situations where postProcess(ComScriptProcess) needs to query the
* <p> command.
* <p>
* <p> Revision 1.8  2004/12/14 21:33:25  sueh
* <p> bug# 565: Fixed bug:  Losing process track when backing up .edf file and
* <p> only saving metadata.  Removed unnecessary class JoinProcessTrack.
* <p> bug# 572:  Removing state object from meta data and managing it with a
* <p> manager class.
* <p> Saving all objects to the .edf/ejf file each time a save is done.
* <p>
* <p> Revision 1.7  2004/12/13 19:08:56  sueh
* <p> bug# 565 Saving process track to edf file as well as meta data in the
* <p> start... functions.
* <p>
* <p> Revision 1.6  2004/12/09 05:04:34  sueh
* <p> bug# 565 Added save meta data to each msg...Done function regardless
* <p> of success or failure.
* <p>
* <p> Revision 1.5  2004/12/09 04:52:54  sueh
* <p> bug# 565 Saving meta data on each top of start function.
* <p>
* <p> Revision 1.4  2004/11/24 00:59:23  sueh
* <p> bug# 520 msgBackgroundProcess:  call errorProcess is exitValue != 0.
* <p>
* <p> Revision 1.3  2004/11/20 01:58:22  sueh
* <p> bug# 520 Passing exitValue to postProcess(BackgroundProcess).
* <p>
* <p> Revision 1.2  2004/11/19 23:17:50  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.8  2004/11/12 22:52:59  sueh
* <p> bug# 520 Using overloading to simiplify the postProcess function names.
* <p>
* <p> Revision 1.1.2.7  2004/10/25 23:10:39  sueh
* <p> bug# 520 Added a call to backgroundErrorProcess() for post processing
* <p> when BackgroundProcess fails.
* <p>
* <p> Revision 1.1.2.6  2004/10/21 02:39:49  sueh
* <p> bug# 520 Created functions to manager InteractiveSystemProgram:
* <p> startInteractiveSystemProgram, msgInteractivesystemProgramDone,
* <p> interactiveSystemProgramPostProcess.
* <p>
* <p> Revision 1.1.2.5  2004/10/18 19:08:18  sueh
* <p> bug# 520 Replaced manager with abstract BaseManager getManager().
* <p> The type of manager that is stored will be decided by
* <p> BaseProcessManager's children.  Moved startSystemProgramThread() to
* <p> the base class.  Added an interface to this function to handle
* <p> String[] command.
* <p>
* <p> Revision 1.1.2.4  2004/10/11 02:02:37  sueh
* <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
* <p> property.  This property would need a different value for each manager.
* <p> This variable can be retrieved from the manager if the object knows its
* <p> manager.  Otherwise it can retrieve it from the current manager using the
* <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
* <p> gets the value from the "user.dir" property.
* <p>
* <p> Revision 1.1.2.3  2004/10/08 15:55:17  sueh
* <p> bug# 520 Handled command array in BackgroundProcess.  Since
* <p> EtomoDirector is a singleton, made all functions and member variables
* <p> non-static.
* <p>
* <p> Revision 1.1.2.2  2004/10/06 01:38:00  sueh
* <p> bug# 520 Added abstract backgroundPostProcessing to handle
* <p> non-generic processing during msgBackgroundProcessDone().  Added
* <p> startBackgroundProcess() functions to handle constructing
* <p> BackgroundProcess with a Command rather then a String.
* <p>
* <p> Revision 1.1.2.1  2004/09/29 17:48:18  sueh
* <p> bug# 520 Contains functionality that is command for ProcessManager and
* <p> JoinProcessManager.
* <p> </p>
*/
public abstract class BaseProcessManager {
  public static  final String  rcsid =  "$Id$";
  
  final static String CHUNK_ERROR_TAG = "CHUNK ERROR:";
  SystemProcessInterface threadAxisA = null;
  SystemProcessInterface threadAxisB = null;
  Thread processMonitorA = null;
  Thread processMonitorB = null;
  private HashMap killedList = new HashMap();
  EtomoDirector etomoDirector = EtomoDirector.getInstance();
  protected UIHarness uiHarness = UIHarness.INSTANCE;
  
  protected abstract void postProcess(ComScriptProcess script);
  protected abstract void postProcess(BackgroundProcess process);
  protected abstract void errorProcess(BackgroundProcess process);
  protected abstract BaseManager getManager();
  protected abstract void postProcess(InteractiveSystemProgram program);
  protected abstract void errorProcess(ComScriptProcess process);
  
  public BaseProcessManager() {
  }
  

  public final void startGetLoadAverage(LoadAverageParam param, LoadAverageMonitor monitor) {
    IntermittentSystemProgram.startInstance(getManager(), param, monitor);
  }
  
  public final void stopGetLoadAverage(LoadAverageParam param, LoadAverageMonitor monitor) {
    IntermittentSystemProgram.stopInstance(getManager(), param, monitor);
  }
  
  /**
   * run touch command on file
   * @param file
   */
  public void touch(File file) {
    String[] commandArray = { "touch", file.getAbsolutePath() };
    startSystemProgramThread(commandArray, AxisID.ONLY);
  }
  
  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  protected ComScriptProcess startComScript(
    String command,
    ProcessMonitor processMonitor,
    AxisID axisID)
    throws SystemProcessException {
    return startComScript(
      new ComScriptProcess(getManager(), command, this, axisID, null, processMonitor),
      command,
      processMonitor,
      axisID);
  }
  
  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  protected ComScriptProcess startComScript(Command command,
      ProcessMonitor processMonitor, AxisID axisID) throws SystemProcessException {
    return startComScript(new ComScriptProcess(getManager(), command, this, axisID, null, processMonitor),
        command.getCommandLine(), processMonitor, axisID);
  }
  
  /**
   * Start a managed background command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @return
   * @throws SystemProcessException
   */
  protected ComScriptProcess startBackgroundComScript(String command, 
      BackgroundComScriptMonitor processMonitor, AxisID axisID, 
    ComscriptState comscriptState, String watchedFileName)
    throws SystemProcessException {
    return startComScript(new BackgroundComScriptProcess(getManager(), command, this, axisID,
      watchedFileName, processMonitor, comscriptState),
      command, processMonitor, axisID);
  }

  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @param watchedFileName watched file to delete
   * @return
   * @throws SystemProcessException
   */
  protected ComScriptProcess startComScript(
    String command,
    ProcessMonitor processMonitor,
    AxisID axisID,
    String watchedFileName)
    throws SystemProcessException {
    return startComScript(
      new ComScriptProcess(getManager(), command, this, axisID, watchedFileName, processMonitor),
      command,
      processMonitor,
      axisID);
  }
  
  /**
   * Start a managed command script for the specified axis
   * @param command
   * @param processMonitor
   * @param axisID
   * @param watchedFileName watched file to delete
   * @return
   * @throws SystemProcessException
   */
  protected ComScriptProcess startComScript(ComScriptProcess comScriptProcess, 
    String command,
    Runnable processMonitor,
    AxisID axisID)
    throws SystemProcessException {
    // Make sure there isn't something going on in the current axis
    isAxisBusy(axisID);

    // Run the script as a thread in the background
    comScriptProcess.setWorkingDirectory(new File(getManager().getPropertyUserDir()));
    comScriptProcess.setDebug(etomoDirector.isDebug());
    comScriptProcess.setDemoMode(etomoDirector.isDemo());
    getManager().saveTestParamFile(axisID);
    comScriptProcess.start();

    // Map the thread to the correct axis
    mapAxisThread(comScriptProcess, axisID);

    if (etomoDirector.isDebug()) {
      System.err.println("Started " + command);
      System.err.println("  Name: " + comScriptProcess.getName());
    }

    Thread processMonitorThread = null;
    // Replace the process monitor with a DemoProcessMonitor if demo mode is on
    if (etomoDirector.isDemo()) {
      processMonitor = new DemoProcessMonitor(getManager(), axisID, command,
        comScriptProcess.getDemoTime());
    }

    //  Start the process monitor thread if a runnable process is provided
    if (processMonitor != null) {
      // Wait for the started flag within the comScriptProcess, this ensures
      // that log file has already been moved
      while (!comScriptProcess.isStarted() && !comScriptProcess.isError()) {
        try {
          Thread.sleep(100);
        }
        catch (InterruptedException e) {
          break;
        }
      }
      processMonitorThread = new Thread(processMonitor);
      processMonitorThread.start();
      mapAxisProcessMonitor(processMonitorThread, axisID);
    }

    return comScriptProcess;
  }
  
  /**
   * Check to see if specified axis is busy, throw a system a
   * ProcessProcessException if it is.
   * 
   * @param axisID
   * @throws SystemProcessException
   */
  protected void isAxisBusy(AxisID axisID) throws SystemProcessException {
    // Check to make sure there is not another process already running on this
    // axis.
    if (axisID == AxisID.SECOND) {
      if (threadAxisB != null) {
        throw new SystemProcessException(
          "A process is already executing in the current axis");
      }
    }
    else {
      if (threadAxisA != null) {
        throw new SystemProcessException(
          "A process is already executing in the current axis");
      }
    }
  }

  /**
   * Save the process thread reference for the appropriate axis
   * 
   * @param thread
   * @param axisID
   */
  protected void mapAxisThread(SystemProcessInterface thread, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      threadAxisB = thread;
    }
    else {
      threadAxisA = thread;
    }
  }
  
  /**
   * Save the process monitor thread reference for the appropriate axis
   * 
   * @param processMonitor
   * @param axisID
   */
  private void mapAxisProcessMonitor(Thread processMonitor, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      processMonitorB = processMonitor;
    }
    else {
      processMonitorA = processMonitor;
    }
  }
  
  private SystemProcessInterface getThread(AxisID axisID) {
    SystemProcessInterface thread = null;
    if (axisID == AxisID.SECOND) {
      thread = threadAxisB;
    }
    else {
      thread = threadAxisA;
    }
    return thread;
  }
  
  public void pause(AxisID axisID) {
    SystemProcessInterface thread = getThread(axisID);
    if (thread == null) {
      return;
    }
    thread.pause(axisID);
  }
  
  public void kill(AxisID axisID) {
    SystemProcessInterface thread = getThread(axisID);
    if (thread == null) {
      return;
    }
    thread.kill(axisID);
  }

  void signalInterrupt(SystemProcessInterface thread, AxisID axisID) { 
    interruptProcess(thread.getShellProcessID(), axisID);
  }
  
  /**
   * Kill the thread for the specified axis
   */
  void signalKill(SystemProcessInterface thread, AxisID axisID) {
    String processID = "";
    thread.setProcessEndState(ProcessEndState.KILLED);
    
    processID = thread.getShellProcessID();  
    killProcessGroup(processID, axisID);
    killProcessAndDescendants(processID, axisID);
    
    thread.notifyKilled();
  }

  protected void killProcessGroup(String processID, AxisID axisID) {
    if (processID == null || processID.equals("")) {
      return;
    }
    long pid = Long.parseLong(processID);
    if (pid == 0 || pid == 1) {
      return;
    }
    long groupPid = pid * -1;
    String groupProcessID = Long.toString(groupPid);
    kill("-19", groupProcessID, axisID);
    kill("-9", groupProcessID, axisID);
  }
  
  protected void interruptProcess(String processID, AxisID axisID) {
    if (processID == null || processID.equals("")) {
      return;
    }
    long pid = Long.parseLong(processID);
    if (pid == 0 || pid == 1) {
      return;
    }
    kill("-2", processID, axisID);
  }
  
  /**
   * Recursively kill all the descendents of a process and then kill the
   * process.  Function assumes that the process will continue spawning while
   * the descendant processes are being killed.  Function attempts to stop
   * spawning with a Stop signal.  The Stop signal may not work in all cases and
   * OS's, so the function refreshes the list of child processes until there are
   * no more child processes.  The function avoids getting stuck on an
   * unkillable process by recording each PID it sent a "kill -9" to.
   * 
   * The algorithm:
   * 1. Stop the root process.
   * 2. Go down to a leaf, stopping each process encountered.
   * 3. Kill the leaf.
   * 4. Go up to the parent of the killed leaf.
   * 5. If the parent is now a leaf, kill it and continue from step 4.
   * 6. If the parent is not a leaf, continue from step 2.
   * 
   * @param processID
   */
  protected void killProcessAndDescendants(String processID, AxisID axisID) {
    if (processID == null || processID.equals("")) {
      return;
    }
    //try to prevent process from spawning with a SIGSTOP signal
    kill("-19", processID, axisID);

    //kill all decendents of process before killing process
    String[] childProcessIDList = null;
    do {
      //get unkilled child processes
      childProcessIDList = getChildProcessList(processID, axisID);
      if (childProcessIDList != null) {
        for (int i = 0; i < childProcessIDList.length; i++) {
          killProcessAndDescendants(childProcessIDList[i], axisID);
        }
      }
    } while (childProcessIDList != null);
    //there are no more unkilled child processes so kill process with a SIGKILL
    //signal
    kill("-9", processID, axisID);
    //record killed process
    killedList.put(processID, "");
  }

  private void kill(String signal, String processID, AxisID axisID) {
    SystemProgram killShell = new SystemProgram(getManager()
        .getPropertyUserDir(), "kill " + signal + " " + processID, axisID);
    killShell.run();
    //System.out.println("kill " + signal + " " + processID + " at " + killShell.getRunTimestamp());
    Utilities.debugPrint("kill " + signal + " " + processID + " at " + killShell.getRunTimestamp());
  }

  /**
   * Return a the PIDs of child processes for the specified parent process.  A
   * new ps command is run each time this function is called so that the most
   * up-to-date list of child processes is used.  Only processes the have not
   * already received a "kill -9" signal are returned.
   * 
   * @param processID
   * @return A PID of a child process or null
   */
  private String[] getChildProcessList(String processID, AxisID axisID) {
    Utilities.debugPrint("in getChildProcessList: processID=" + processID);
    //ps -l: get user processes on this terminal
    SystemProgram ps = new SystemProgram(getManager().getPropertyUserDir(),
        "ps axl", axisID);
    ps.run();
    //System.out.println("ps axl date=" +  ps.getRunTimestamp());
    //  Find the index of the Parent ID and ProcessID
    String[] stdout = ps.getStdOutput();
    String header = stdout[0].trim();
    String[] labels = header.split("\\s+");
    int idxPID = -1;
    int idxPPID = -1;
    int idxCMD = -1;
    int idxPGID = -1;
    int found = 0;
    for (int i = 0; i < labels.length; i++) {
      if (labels[i].equals("PID")) {
        idxPID = i;
        found++;
      }
      if (labels[i].equals("PPID")) {
        idxPPID = i;
        found++;
      }
      if (labels[i].equals("CMD") || labels[i].equals("COMMAND")) {
        idxCMD = i;
        found++;
      }
      if (labels[i].equals("PGID")) {
        idxPGID = i;
      }
      if (found >= 3) {
        break;
      }
    }
    //  Return null if the PID or PPID fields are not found
    if (idxPPID == -1 || idxPID == -1) {
      return null;
    }

    // Walk through the process list finding the PID of the children
    ArrayList childrenPID = new ArrayList();
    String[] fields;
    //System.out.println(stdout[0]);
    for (int i = 1; i < stdout.length; i++) {
      //System.out.println(stdout[i]);
      fields = stdout[i].trim().split("\\s+");
      if (fields[idxPPID].equals(processID)
        && !killedList.containsKey(fields[idxPID])) {
        if (idxCMD != -1) {
          Utilities.debugPrint(
          "child found:PID="
            + fields[idxPID]
            + ",PPID="
            + fields[idxPPID]
            + ",name="
            + fields[idxCMD]);
        }
        childrenPID.add(fields[idxPID]);
      }
    }

    // If there are no children return null
    if (childrenPID.size() == 0) {
      return null;
    }

    // Connvert the ArrayList into a String[]
    String[] children = (String[]) childrenPID.toArray(new String[childrenPID
      .size()]);
    return children;
  }
  
  /**
   * Return a PID of a child process for the specified parent process.  A new
   * ps command is run each time this function is called so that the most
   * up-to-date list of child processes is used.  Only processes the have not
   * already received a "kill -9" signal are returned.
   * 
   * @param processID
   * @return A PID of a child process or null
   */
  protected String getChildProcess(String processID, AxisID axisID) {
    Utilities.debugPrint("in getChildProcess: processID=" + processID);
    //ps -l: get user processes on this terminal
    SystemProgram ps = new SystemProgram(getManager().getPropertyUserDir(),
        "ps axl", axisID);
    ps.run();

    //  Find the index of the Parent ID and ProcessID
    String[] stdout = ps.getStdOutput();
    String header = stdout[0].trim();
    String[] labels = header.split("\\s+");
    int idxPID = -1;
    int idxPPID = -1;
    int idxCMD = -1;
    int found = 0;
    for (int i = 0; i < labels.length; i++) {
      if (labels[i].equals("PID")) {
        idxPID = i;
        found++;
      }
      if (labels[i].equals("PPID")) {
        idxPPID = i;
        found++;
      }
      if (labels[i].equals("CMD") || labels[i].equals("COMMAND")) {
        idxCMD = i;
        found++;
      }
      if (found >= 3) {
        break;
      }
    }
    //  Return null if the PID or PPID fields are not found
    if (idxPPID == -1 || idxPID == -1) {
      return null;
    }

    // Walk through the process list finding the PID of the children
    String[] fields;
    for (int i = 1; i < stdout.length; i++) {
      fields = stdout[i].trim().split("\\s+");
      if (fields[idxPPID].equals(processID)
        && !killedList.containsKey(fields[idxPID])) {
        if (idxCMD != -1) {
          Utilities.debugPrint(
            "child found:PID="
              + fields[idxPID]
              + ",PPID="
              + fields[idxPPID]
              + ",name="
              + fields[idxCMD]);
        }
        return fields[idxPID];
      }
    }
    return null;
  }

  /**
   * A message specifying that a com script has finished execution
   * 
   * @param script
   *          the ComScriptProcess execution object that finished
   * @param exitValue
   *          the exit value for the com script
   */
  public void msgComScriptDone(ComScriptProcess script, int exitValue) {
    System.err.println("msgComScriptDone:scriptName=" + script.getScriptName()
      + ",processName=" + script.getProcessName());
    if (exitValue != 0) {
      String[] stdError = script.getStdError();
      String[] combined;
      //    Is the last string "Killed"
      if (stdError == null) {
        stdError = new String[0];
      }
      if (stdError != null && stdError.length > 0
          && stdError[stdError.length - 1].trim().equals("Killed")) {
        combined = new String[1];
        combined[0] = "<html>Terminated: " + script.getScriptName();
      }
      else {
        String[] message = script.getErrorMessage();
        combined = new String[message.length + stdError.length + 5];
        int j = 0;
        combined[j++] = "<html>Com script failed: " + script.getScriptName();
        combined[j++] = "  ";
        combined[j++] = "<html><U>Log file errors:</U>";

        for (int i = 0; i < message.length; i++, j++) {
          combined[j] = message[i];
        }
        combined[j++] = "  ";
        combined[j++] = "<html><U>Standard error output:</U>";
        for (int i = 0; i < stdError.length; i++, j++) {
          combined[j] = stdError[i];
        }
      }
      if (script.getProcessEndState() != ProcessEndState.KILLED
          && script.getProcessEndState() != ProcessEndState.PAUSED) {
        uiHarness.openMessageDialog(combined, script.getScriptName()
            + " terminated", script.getAxisID());
        //make sure script knows about failure
        script.setProcessEndState(ProcessEndState.FAILED);
      }
      errorProcess(script);
    }
    else {
      postProcess(script);

      String[] warningMessages = script.getWarningMessage();
      String[] dialogMessage;
      if (warningMessages != null && warningMessages.length > 0) {
        dialogMessage = new String[warningMessages.length + 2];
        dialogMessage[0] = "Com script: " + script.getScriptName();
        dialogMessage[1] = "<html><U>Warnings:</U>";
        int j = 2;
        for (int i = 0; i < warningMessages.length; i++) {
          dialogMessage[j++] = warningMessages[i];
        }
        uiHarness.openMessageDialog(dialogMessage, script.getScriptName()
            + " warnings", script.getAxisID());
      }

    }
    getManager().saveTestParamFile(script.getAxisID());
    //  Null out the correct thread
    // Interrupt the process monitor and nulll out the appropriate references
    if (threadAxisA == script) {
      if (processMonitorA != null) {
        processMonitorA.interrupt();
        processMonitorA = null;
      }
      threadAxisA = null;
    }
    if (threadAxisB == script) {
      if (processMonitorB != null) {
        processMonitorB.interrupt();
        processMonitorB = null;
      }
      threadAxisB = null;
    }

    //  Inform the app manager that this process is complete
    getManager().processDone(script.getName(), exitValue,
      script.getProcessName(), script.getAxisID(), script.getProcessEndState());
  }
  
  /**
   * Start a managed background process
   * 
   * @param command
   * @param axisID
   * @throws SystemProcessException
   */
  protected BackgroundProcess startBackgroundProcess(String commandLine,
      AxisID axisID) throws SystemProcessException {

    isAxisBusy(axisID);

    BackgroundProcess backgroundProcess = new BackgroundProcess(getManager(),
        commandLine, this, axisID);
    return startBackgroundProcess(backgroundProcess, commandLine, axisID, null);
  }
  
  protected BackgroundProcess startBackgroundProcess(String[] commandArray,
      AxisID axisID) throws SystemProcessException {

    isAxisBusy(axisID);

    BackgroundProcess backgroundProcess = new BackgroundProcess(getManager(),
        commandArray, this, axisID);
    return startBackgroundProcess(backgroundProcess, commandArray.toString(),
        axisID, null);
  }
  
  protected BackgroundProcess startInteractiveBackgroundProcess(String[] commandArray,
      AxisID axisID, BackgroundProcessMonitor monitor) throws SystemProcessException {

    isAxisBusy(axisID);

    BackgroundProcess backgroundProcess = new BackgroundProcess(getManager(),
        commandArray, this, axisID, monitor);
    backgroundProcess.setAcceptInputWhileRunning(true);
    return startBackgroundProcess(backgroundProcess, commandArray.toString(),
        axisID, monitor);
  }

  protected BackgroundProcess startBackgroundProcess(Command command,
      AxisID axisID) throws SystemProcessException {
    isAxisBusy(axisID);
    BackgroundProcess backgroundProcess = new BackgroundProcess(getManager(),
        command, this, axisID);
    return startBackgroundProcess(backgroundProcess, command.getCommandLine(),
        axisID, null);
  }
  
  protected BackgroundProcess startBackgroundProcess(Command command,
      AxisID axisID, boolean forceNextProcess) throws SystemProcessException {
    isAxisBusy(axisID);
    BackgroundProcess backgroundProcess = new BackgroundProcess(getManager(), command, this,
        axisID, forceNextProcess);
    return startBackgroundProcess(backgroundProcess, command.getCommandLine(),
        axisID, null);
  }

  private BackgroundProcess startBackgroundProcess(
      BackgroundProcess backgroundProcess, String commandLine, AxisID axisID, Runnable processMonitor)
      throws SystemProcessException {
    backgroundProcess.setWorkingDirectory(new File(getManager().getPropertyUserDir()));
    backgroundProcess.setDemoMode(etomoDirector.isDemo());
    backgroundProcess.setDebug(etomoDirector.isDebug());
    getManager().saveTestParamFile(axisID);
    backgroundProcess.start();
    if (etomoDirector.isDebug()) {
      System.err.println("Started " + commandLine);
      System.err.println("  Name: " + backgroundProcess.getName());
    }
    mapAxisThread(backgroundProcess, axisID);
    //  Start the process monitor thread if a runnable process is provided
    if (processMonitor != null) {
      // Wait for the started flag within the backgroundProcess
      while (!backgroundProcess.isStarted()) {
        try {
          Thread.sleep(100);
        }
        catch (InterruptedException e) {
          break;
        }
      }
      Thread processMonitorThread = new Thread(processMonitor);
      processMonitorThread.start();
      mapAxisProcessMonitor(processMonitorThread, axisID);
    }

    return backgroundProcess;
  }
  
  protected InteractiveSystemProgram startInteractiveSystemProgram(
      Command commandParam) throws SystemProcessException {
    InteractiveSystemProgram program = new InteractiveSystemProgram(
        getManager(), commandParam, this, commandParam.getAxisID());
    program.setWorkingDirectory(new File(getManager().getPropertyUserDir()));
    Thread thread = new Thread(program);
    getManager().saveTestParamFile(commandParam.getAxisID());
    thread.start();
    program.setName(thread.getName());
    if (etomoDirector.isDebug()) {
      System.err.println("Started " + program.getCommandLine());
      System.err.println("  Name: " + thread.getName());
    }
    return program;
  }
  
  /**
   * Start an arbitrary command as an unmanaged background thread
   */
  protected void startSystemProgramThread(String[] command, AxisID axisID) {
    // Initialize the SystemProgram object
    SystemProgram sysProgram = new SystemProgram(getManager()
        .getPropertyUserDir(), command, axisID);
    startSystemProgramThread(sysProgram);
  }
  
  protected void startSystemProgramThread(String command, AxisID axisID) {
    // Initialize the SystemProgram object
    SystemProgram sysProgram = new SystemProgram(getManager()
        .getPropertyUserDir(), command, axisID);
    startSystemProgramThread(sysProgram);
  }
  
  private void startSystemProgramThread(SystemProgram sysProgram) { 
    sysProgram.setWorkingDirectory(new File(getManager().getPropertyUserDir()));
    sysProgram.setDebug(etomoDirector.isDebug());

    //  Start the system program thread
    Thread sysProgThread = new Thread(sysProgram);
    getManager().saveTestParamFile(sysProgram.getAxisID());
    sysProgThread.start();
    if (etomoDirector.isDebug()) {
      System.err.println("Started " + sysProgram.getCommandLine());
      System.err.println("  working directory: "
        + getManager().getPropertyUserDir());
    }
  }

  /**
   * A message specifying that a background process has finished execution
   * 
   * @param script
   *          the BackgroundProcess execution object that finished
   * @param exitValue
   *          the exit value for the process
   */
  public void msgBackgroundProcessDone(BackgroundProcess process, int exitValue) {
    String statusString = null;
    //  Check to see if the exit value is non-zero
    if (exitValue != 0) {
      String[] stdError = process.getStdError();
      ArrayList message = new ArrayList();

      // Is the last string "Killed"
      if ((stdError.length > 0)
        && (stdError[stdError.length - 1].trim().equals("Killed"))) {
        message.add("<html>Terminated: " + process.getCommandLine());
      }
      else {
        int j = 0;
        message.add("<html>Command failed: " + process.getCommandLine());
        message.add("  ");
        message.add("<html><U>Standard error output:</U>");
        for (int i = 0; i < stdError.length; i++, j++) {
          message.add(stdError[i]);
        }
      }
      ProcessEndState endState = process.getProcessEndState();
      if (endState != ProcessEndState.KILLED
          && endState != ProcessEndState.PAUSED) {
        String monitorErrorMessage = process.getMonitorErrorMessage();
        if (monitorErrorMessage != null) {
          message.add(monitorErrorMessage);
        }
        int messageSize = message.size();
        String[] messageArray = new String[messageSize];
        for (int i = 0; i < messageSize; i++) {
          messageArray[i] = (String) message.get(i);
        }
        uiHarness.openMessageDialog(messageArray,
            process.getCommandName() + " terminated", process.getAxisID());
        //make sure script knows about failure
        process.setProcessEndState(ProcessEndState.FAILED);
      }
      else if (process.getProcessEndState() == ProcessEndState.PAUSED) {
        statusString = process.getStatusString();
      }
    }

    // Another possible error message source is ERROR: in the stdout stream
    String[] stdOutput = process.getStdOutput();
    ArrayList errors = new ArrayList();
    boolean foundError = false;
    for (int i = 0; i < stdOutput.length; i++) {
      if (!foundError) {
        int index = stdOutput[i].indexOf("ERROR:");
        int chunkIndex = stdOutput[i].indexOf(CHUNK_ERROR_TAG);
        if (index != -1 && chunkIndex == -1) {
          foundError = true;
          errors.add(stdOutput[i]);
        }
      }
      else {
        errors.add(stdOutput[i]);
      }
    }
    String[] errorMessage = (String[]) errors
      .toArray(new String[errors.size()]);

    if (errorMessage.length > 0) {
      uiHarness.openMessageDialog(errorMessage,
          "Background Process Error", process.getAxisID());
      errorProcess(process);
    }

    // Command succeeded, check to see if we need to show any application
    // specific info
    else {
      if (exitValue != 0) {
        errorProcess(process);
      }
      else {
        postProcess(process);
      }
      getManager().saveTestParamFile(process.getAxisID());
    }

    // Null the reference to the appropriate thread
    if (process == threadAxisA) {
      threadAxisA = null;
    }
    if (process == threadAxisB) {
      threadAxisB = null;
    }

    //  Inform the app manager that this process is complete
    getManager().processDone(process.getName(), exitValue, null, process.getAxisID(),
        process.isForceNextProcess(), process.getProcessEndState(), statusString);
  }
  
  public void msgInteractiveSystemProgramDone(InteractiveSystemProgram program, int exitValue) {
    postProcess(program);
    getManager().saveTestParamFile(program.getAxisID());
  }

}
