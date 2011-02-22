package etomo.process;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Hashtable;

import etomo.BaseManager;
import etomo.comscript.IntermittentCommand;
import etomo.type.AxisID;
import etomo.util.HashedArray;
import etomo.util.RemotePath;

/**
 * <p>Description:  This class runs a command and then intermittently sends
 * another commmand through standard in.  The original command is halted by a
 * stop function.
 * 
 * This class saves the commands it receives in a static storage.  A new instance
 * of this class is created only when a command that is not already stored is
 * sent to it.
 * 
 * The parameters for the static getInstance function are IntermittentCommand,
 * which contains the command, the intermittent command, and the interval; and
 * IntermittentProcessMonitor, which processes standard out.  The
 * IntermittentProcessMonitor is added to an instance-level list.  If the
 * command is already running when the instance receives a new
 * IntermittentProcessMonitor, then the IntermittentProcessMonitor is
 * "hooked into" the existing command via the standard out.  So only one command
 * per instance can be run at a time.  Any previous commands will stop as soon
 * as they see that they are not the most recent command.
 * 
 * The difficulty of this class comes from its relationship with
 * IntermittantProcessMonitor.  These two classes have a many-to-many
 * relationship and are interdependent.
 * 
 * Function stop(IntermittantProcessMonitor monitor):
 * This function drops the monitor from it's output and tells the monitor to
 * stop monitoring it.  It will stop running only if it is no longer being
 * monitored.  
 * 
 * Function end(IntermittantProcessMonitor):
 * This function calls stop(IntermittantProcessMonitor monitor) and then removes
 * the monitor from it's list.  It is used when a manager exits.
 * 
 * Function fail():
 * This function drops all of its monitors from it's output and stops.  The
 * process monitors will keep running, but they will ignore the stopped process.
 * 
 * Function restart():
 * This function tells the RestartThread that it should try to restart the
 * process.
 * 
 * This class used to be called IntermittentSystemProgram.</p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */

public final class IntermittentBackgroundProcess implements Runnable {
  public static final String rcsid = "$Id$";

  private static Hashtable instances = new Hashtable();//one instance per IntermittentCommand instance
  //stopped:  means that the program needs to stop.
  private boolean stopped = true;
  private boolean canRestart = true;
  private HashedArray monitors = new HashedArray();
  private final IntermittentCommand command;
  private final BaseManager manager;
  private IntermittentSystemProgram program = null;
  //outputKeyPhrase:  string to look for in the standard output.  Assumes all
  //monitors use the same key phrase for a single instance of
  //IntermittentCommand.  This is important for intermittent commands because
  //there is a lot more standard output then there is for a single command, and
  //processing it can slow Etomo down.
  private String outputKeyPhrase = null;
  //failureReason is null unless the process fails
  private FailureReason failureReason = null;

  public String toString() {
    return "[stopped=" + stopped + "," + super.toString() + "]";
  }

  static void startInstance(BaseManager manager, IntermittentCommand command,
      IntermittentProcessMonitor monitor) {
    getInstance(manager, command, monitor).start(monitor);
  }

  static void endInstance(BaseManager manager, IntermittentCommand command,
      IntermittentProcessMonitor monitor) {
    IntermittentBackgroundProcess intermittentBackgroundProcess = getInstance(command);
    if (intermittentBackgroundProcess != null) {
      intermittentBackgroundProcess.end(monitor);
    }
  }

  static void stopInstance(BaseManager manager, IntermittentCommand command,
      IntermittentProcessMonitor monitor) {
    IntermittentBackgroundProcess intermittentBackgroundProcess = getInstance(command);
    if (intermittentBackgroundProcess != null) {
      intermittentBackgroundProcess.stop(monitor);
    }
  }

  private static IntermittentBackgroundProcess getInstance(BaseManager manager,
      IntermittentCommand command, IntermittentProcessMonitor monitor) {
    IntermittentBackgroundProcess intermittentBackgroundProcess = getInstance(command);
    if (intermittentBackgroundProcess == null) {
      return createInstance(manager, command, monitor);
    }
    return intermittentBackgroundProcess;
  }

  private static IntermittentBackgroundProcess getInstance(IntermittentCommand command) {
    return (IntermittentBackgroundProcess) instances.get(command);
  }

  private static synchronized IntermittentBackgroundProcess createInstance(
      BaseManager manager, IntermittentCommand command, IntermittentProcessMonitor monitor) {
    IntermittentBackgroundProcess intermittentBackgroundProcess = getInstance(command);
    if (intermittentBackgroundProcess == null) {
      intermittentBackgroundProcess = new IntermittentBackgroundProcess(manager, command,
          monitor);
      instances.put(intermittentBackgroundProcess.command, intermittentBackgroundProcess);
    }
    return intermittentBackgroundProcess;
  }

  private IntermittentBackgroundProcess(BaseManager manager, IntermittentCommand command,
      IntermittentProcessMonitor monitor) {
    this.command = command;
    this.manager = manager;
    if (outputKeyPhrase == null) {
      outputKeyPhrase = monitor.getOutputKeyPhrase();
    }
  }

  private synchronized void start(IntermittentProcessMonitor monitor) {
    boolean newMonitor = false;
    //run the instance, if it is not running
    //this is the only place that stopped should be set to false
    if (stopped) {
      stopped = false;
      canRestart = true;
      new Thread(this).start();
    }
    //Once the thread is started, add the monitor if it is new, make sure not to
    //add it more then once
    if (!monitors.containsKey(monitor)) {
      monitors.add(monitor);
    }
    monitor.setProcess(this);
  }

  /**
   * Ask the monitor to stop (it will only stop if this is its only running
   * process).  Drop the monitor from the program.  Check whether all the
   * monitors associated with this process are stopped.  If so, stop.  This is
   * used when a parallel processing panel is hidden.
   * @param monitor
   */
  synchronized void stop(IntermittentProcessMonitor monitor) {
    monitor.stopMonitoring(this);
    if (program != null) {
      program.msgDroppedMonitor(monitor);
    }
    //Set monitorsStopped is this process is no longer being monitored by any
    //monitor.
    boolean monitorsStopped = true;
    for (int i = 0; i < monitors.size(); i++) {
      if (((IntermittentProcessMonitor) monitors.get(i)).isMonitoring(this)) {
        monitorsStopped = false;
        break;
      }
    }
    //If this process is not being monitored, then stop it and prevent it from
    //being restarted by ProcessRestarter.
    if (monitorsStopped) {
      canRestart = false;
      stopped = true;
    }
  }

  synchronized void restartAll() {
    if (!canRestart) {
      return;
    }
    for (int i = 0; i < monitors.size(); i++) {
      IntermittentProcessMonitor monitor = (IntermittentProcessMonitor) monitors.get(i);
      start(monitor);
    }
  }

  /**
   * Call stop(monitor) and then remove the monitor.  This is used when a
   * manager exits.
   * @param monitor
   */
  synchronized void end(IntermittentProcessMonitor monitor) {
    stop(monitor);
    monitors.remove(monitor);
  }

  public static synchronized void stop() {
    Enumeration enumeration = instances.elements();
    while (enumeration.hasMoreElements()) {
      ((IntermittentBackgroundProcess) enumeration.nextElement()).stopAll();
    }
    //wait while the processes are ending
    try {
      Thread.sleep(10);
    }
    catch (InterruptedException e) {
    }
    //Are the programs done?
    enumeration = instances.elements();
    boolean done = true;
    while (enumeration.hasMoreElements()) {
      if (!((IntermittentBackgroundProcess) enumeration.nextElement()).program.isDone()) {
        done = false;
      }
    }
    if (done) {
      return;
    }
    System.err.println("Waiting for processes to stop.");
    //make sure programs have ended
    try {
      Thread.sleep(1000);
    }
    catch (InterruptedException e) {
    }
    done = true;
    while (enumeration.hasMoreElements()) {
      if (!((IntermittentBackgroundProcess) enumeration.nextElement()).program.isDone()) {
        done = false;
      }
    }
    if (!done) {
      System.err.println("Error:  processes haven't stopped.");
    }
  }

  private synchronized void stopAll() {
    if (program != null) {
      for (int i = 0; i < monitors.size(); i++) {
        program.msgDroppedMonitor((IntermittentProcessMonitor) monitors.get(i));
      }
    }
    stopped = true;
  }

  /**
   * Drop all the monitors from the program.  Stop this process.  This is used
   * when the process fails.
   */
  synchronized void fail() {
    if (program != null) {
      for (int i = 0; i < monitors.size(); i++) {
        program.msgDroppedMonitor((IntermittentProcessMonitor) monitors.get(i));
      }
    }
    stopped = true;
  }

  final boolean isStopped() {
    return stopped;
  }

  public void run() {
    failureReason = null;
    //use a local SystemProgram because stops and starts may overlap
    IntermittentSystemProgram localProgram = null;
    String[] localStartCommand = command.getLocalStartCommand();
    String[] remoteStartCommand = command.getRemoteStartCommand();
    boolean localSection = RemotePath.INSTANCE.isLocalSection(command.getComputer(),
        manager, AxisID.ONLY);
    String intermittentCommand = command.getIntermittentCommand();
    if (localSection && localStartCommand != null) {
      localProgram = IntermittentSystemProgram.getStartInstance(manager, manager
          .getPropertyUserDir(), localStartCommand, AxisID.ONLY, outputKeyPhrase);
    }
    else if (!localSection && remoteStartCommand != null) {
      localProgram = IntermittentSystemProgram.getStartInstance(manager, manager
          .getPropertyUserDir(), command.getRemoteStartCommand(), AxisID.ONLY,
          outputKeyPhrase);
    }
    else if (intermittentCommand != null) {
      localProgram = IntermittentSystemProgram.getIntermittentInstance(manager, manager
          .getPropertyUserDir(), intermittentCommand, AxisID.ONLY, outputKeyPhrase);
    }
    //place the most recent local SystemProgram in the member SystemProgram
    //non-local request (getting and setting standard input and output) will go
    //to the most recent local SystemProgram.
    program = localProgram;
    //Commented out localProgram.useStartCommand because this comparison was only being
    //executed when localProgram was null, so it was meaningless.
    if (localProgram != null/* || localProgram.useStartCommand()*/) {
      localProgram.setAcceptInputWhileRunning(true);
      localProgram.start();
    }
    int interval = command.getInterval();
    try {
      //see load average requests while the program is not stopped and this
      //program is the same as the most recent program run
      while (localProgram != null && !stopped && localProgram == program) {
        if (localProgram.useStartCommand()) {
          localProgram.setCurrentStdInput(intermittentCommand);
        }
        else {
          if (localProgram.isDone() || !localProgram.isStarted()) {
            localProgram.start();
          }
        }
        if (command.notifySentIntermittentCommand() && monitors != null) {
          for (int i = 0; i < monitors.size(); i++) {
            ((IntermittentProcessMonitor) monitors.get(i))
                .msgSentIntermittentCommand(command);
          }
        }
        Thread.sleep(interval);
      }
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      stopped = true;
      if (monitors != null) {
        for (int i = 0; i < monitors.size(); i++) {
          ((IntermittentProcessMonitor) monitors.get(i))
              .msgIntermittentCommandFailed(command);
        }
      }
    }
    try {
      if (failureReason != null && failureReason.equals("")) {
        //If there was a problem and we don't know what it is (see
        //LoadAverageMonitor.ProgramState.getFailureReason()), then destroy the
        //process.  Processes with identified problems should fail naturally
        //because we are setting the PreferredAuthentications option in ssh.
        if (localProgram != null) {
          localProgram.destroy();
        }
      }
      else {
        String endCommand = command.getEndCommand();
        if (endCommand != null && localProgram != null) {
          localProgram.setCurrentStdInput(command.getEndCommand());
        }
      }
    }
    catch (IOException e) {
      if (localProgram != null) {
        localProgram.destroy();
      }
    }
  }

  IntermittentCommand getCommand() {
    return command;
  }

  FailureReason getFailureReason() {
    return failureReason;
  }

  void setFailureReason(FailureReason input) {
    failureReason = input;
  }

  void clearStdError() {
    if (program == null) {
      return;
    }
    program.clearStdError();
  }

  String[] getStdOutput(IntermittentProcessMonitor monitor) {
    //don't get output for a stopped monitor because this would make
    //OutputBufferManager start saving output for the monitor
    if (program == null || monitors == null || !monitors.containsKey(monitor)) {
      return null;
    }
    return program.getStdOutput(monitor);
  }

  String[] getStdError() {
    //don't get error for a stopped monitor because this would make
    //OutputBufferManager start saving output for the monitor
    if (program == null) {
      return null;
    }
    return program.getStdError();
  }

  void setCurrentStdInput(String input) {
    if (program == null || stopped) {
      return;
    }
    try {
      program.setCurrentStdInput(input);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.19  2010/05/21 00:13:21  sueh
 * <p> bug# 1374 In run removing a null pointer exception.
 * <p>
 * <p> Revision 1.18  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.17  2010/01/11 23:54:23  sueh
 * <p> bug# 1299 In run:  intermittentCommand can now be null - handling this.
 * <p>
 * <p> Revision 1.16  2009/04/13 22:30:26  sueh
 * <p> bug# 1207 Using FailureReason instead of FailureReasonInterface.
 * <p>
 * <p> Revision 1.15  2009/03/17 00:36:30  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.14  2007/09/27 20:18:57  sueh
 * <p> bug# 1044 Changed IntermittentSystemProgram to have the option of not
 * <p> running a start command.  It this case, this intermittent command is set that the
 * <p> instance is run multiple times.
 * <p>
 * <p> Revision 1.13  2007/06/07 21:27:14  sueh
 * <p> bug# 994 Decreased the wait time in stop() to make etomo end faster.
 * <p>
 * <p> Revision 1.12  2007/06/06 16:06:15  sueh
 * <p> bug# 994 Increased the wait time in stop().
 * <p>
 * <p> Revision 1.11  2007/06/05 17:41:52  sueh
 * <p> bug# 994 Increased the wait time in stop().
 * <p>
 * <p> Revision 1.10  2007/06/04 23:05:17  sueh
 * <p> bug# 994 Increased the wait time in stop().
 * <p>
 * <p> Revision 1.9  2007/05/31 22:26:18  sueh
 * <p> bug# 994 Increased the wait time in stop().
 * <p>
 * <p> Revision 1.8  2007/05/29 19:17:25  sueh
 * <p> bug# 994 Added static stop() and stopAll() to stop the current programs on all the instances.
 * <p>
 * <p> Revision 1.7  2007/05/26 00:25:17  sueh
 * <p> bug# 964 Replaced restart with canRestart.  Moved RestartThread
 * <p> functionality to ProcessRestarter.
 * <p>
 * <p> Revision 1.6  2007/05/25 00:22:06  sueh
 * <p> bug# 994 Added failureReason and clearStdError().
 * <p>
 * <p> Revision 1.5  2006/11/28 23:54:10  sueh
 * <p> bug# 934 Added RestartThread.  Added functions fail, restartAll, restart,
 * <p> endRestartThread, and end.  Fixed stop so that it doesn't remove the monitor.
 * <p>
 * <p> Revision 1.4  2006/03/16 01:51:39  sueh
 * <p> Avoiding null pointer exception in stop().
 * <p>
 * <p> Revision 1.3  2005/12/01 00:24:12  sueh
 * <p> bug# 775 Need to decide whether the computer that the command is
 * <p> talking to is local or remote.  Use RemotePath to find this out.
 * <p>
 * <p> Revision 1.2  2005/09/14 20:25:58  sueh
 * <p> bug# 532 Added drop() to remove a monitor from the listener list.  It is
 * <p> important for the called to prevent any last-minute gets after the drop() is
 * <p> called; the get() will add the monitor back to the listener list, if it is sent
 * <p> after the drop().
 * <p>
 * <p> Revision 1.1  2005/09/10 01:48:53  sueh
 * <p> bug# 532 Changed IntermittentSystemProgram to
 * <p> IntermittentBackgroundProcess.  Made intermittentSystemProgram a child
 * <p> of SystemProgram.  Made OutputBufferManager in independent class
 * <p> instead of being inside SystemProgram.  IntermittentSystemProgram can
 * <p> use OutputBufferManager to do things only necessary for intermittent
 * <p> programs, such as deleting standard output after it is processed and
 * <p> keeping separate lists of standard output for separate monitors.
 * <p> </p>
 */
