package etomo.process;

import java.io.IOException;
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

public class IntermittentBackgroundProcess implements Runnable {
  public static final String rcsid = "$Id$";

  private static Hashtable instances = new Hashtable();//one instance per IntermittentCommand instance
  //stopped:  means that the program needs to stop.
  private boolean stopped = true;
  //restart:  means that there was an exception or a connect time out.  The
  //current instance of this class is added to the RestartThread when restart is
  //set to true.  Restart is turned off when the program is started.
  private boolean restart = false;
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

  public String toString() {
    return "[stopped=" + stopped + "," + super.toString() + "]";
  }

  static final void startInstance(BaseManager manager,
      IntermittentCommand command, IntermittentProcessMonitor monitor) {
    getInstance(manager, command, monitor).start(monitor);
  }

  static final void endInstance(BaseManager manager,
      IntermittentCommand command, IntermittentProcessMonitor monitor) {
    IntermittentBackgroundProcess intermittentBackgroundProcess = getInstance(command);
    if (intermittentBackgroundProcess != null) {
      intermittentBackgroundProcess.end(monitor);
    }
  }

  static final void stopInstance(BaseManager manager,
      IntermittentCommand command, IntermittentProcessMonitor monitor) {
    IntermittentBackgroundProcess intermittentBackgroundProcess = getInstance(command);
    if (intermittentBackgroundProcess != null) {
      intermittentBackgroundProcess.stop(monitor);
    }
  }

  private static final IntermittentBackgroundProcess getInstance(
      BaseManager manager, IntermittentCommand command,
      IntermittentProcessMonitor monitor) {
    IntermittentBackgroundProcess intermittentBackgroundProcess = getInstance(command);
    if (intermittentBackgroundProcess == null) {
      return createInstance(manager, command, monitor);
    }
    return intermittentBackgroundProcess;
  }

  private static final IntermittentBackgroundProcess getInstance(
      IntermittentCommand command) {
    return (IntermittentBackgroundProcess) instances.get(command);
  }

  private static synchronized final IntermittentBackgroundProcess createInstance(
      BaseManager manager, IntermittentCommand command,
      IntermittentProcessMonitor monitor) {
    IntermittentBackgroundProcess intermittentBackgroundProcess = getInstance(command);
    if (intermittentBackgroundProcess == null) {
      intermittentBackgroundProcess = new IntermittentBackgroundProcess(
          manager, command, monitor);
      instances.put(intermittentBackgroundProcess.command,
          intermittentBackgroundProcess);
    }
    return intermittentBackgroundProcess;
  }

  private IntermittentBackgroundProcess(BaseManager manager,
      IntermittentCommand command, IntermittentProcessMonitor monitor) {
    this.command = command;
    this.manager = manager;
    if (outputKeyPhrase == null) {
      outputKeyPhrase = monitor.getOutputKeyPhrase();
    }
  }

  private synchronized final void start(IntermittentProcessMonitor monitor) {
    boolean newMonitor = false;
    //run the instance, if it is not running
    //this is the only place that stopped should be set to false
    if (stopped) {
      restart = false;
      stopped = false;
      new Thread(this).start();
    }
    //Once the thread is started, add the monitor if it is new, make sure not to
    //add it more then once
    if (!monitors.containsKey(monitor)) {
      monitors.add(monitor);
    }
    monitor.setProcess(this);
  }

  synchronized void restart() {
    if (!restart) {
      restart = true;
      RestartThread.restart(this);
    }
  }

  synchronized void restartAll() {
    if (!restart) {
      return;
    }
    for (int i = 0; i < monitors.size(); i++) {
      IntermittentProcessMonitor monitor = (IntermittentProcessMonitor) monitors
          .get(i);
      start(monitor);
    }
  }

  /**
   * Call stop(monitor) and then remove the monitor.  This is used when a
   * manager exits.
   * @param monitor
   */
  synchronized final void end(IntermittentProcessMonitor monitor) {
    stop(monitor);
    monitors.remove(monitor);
  }

  public static void endRestartThread() throws InterruptedException {
    RestartThread.endThread();
  }

  /**
   * Ask the monitor to stop (it will only stop if it this is its only running
   * process).  Drop the monitor from the program.  Check whether all the
   * monitors associated with this process are stopped.  If so, stop.  This is
   * used when a monitor is hidden.
   * @param monitor
   */
  synchronized void stop(IntermittentProcessMonitor monitor) {
    restart = false;
    monitor.stopMonitoring(this);
    if (program != null) {
      program.msgDroppedMonitor(monitor);
    }
    boolean monitorsStopped = true;
    for (int i = 0; i < monitors.size(); i++) {
      if (((IntermittentProcessMonitor) monitors.get(i)).isMonitoring(this)) {
        monitorsStopped = false;
        break;
      }
    }
    if (monitorsStopped) {
      stopped = true;
    }
  }

  /**
   * Drop all the monitors from the program.  Stop this process.  This is used
   * when the process fails.
   */
  synchronized final void fail() {
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

  public final void run() {
    //use a local SystemProgram because stops and starts may overlap
    IntermittentSystemProgram localProgram;
    if (RemotePath.INSTANCE.isLocalSection(command.getComputer(), manager,
        AxisID.ONLY)) {
      localProgram = new IntermittentSystemProgram(
          manager.getPropertyUserDir(), command.getLocalCommand(), AxisID.ONLY,
          outputKeyPhrase);
    }
    else {
      localProgram = new IntermittentSystemProgram(
          manager.getPropertyUserDir(), command.getRemoteCommand(),
          AxisID.ONLY, outputKeyPhrase);
    }
    //place the most recent local SystemProgram in the member SystemProgram
    //non-local request (getting and setting standard input and output) will go
    //to the most recent local SystemProgram.
    program = localProgram;
    localProgram.setAcceptInputWhileRunning(true);
    new Thread(localProgram).start();
    int interval = command.getInterval();
    String intermittentCommand = command.getIntermittentCommand();
    try {
      //see load average requests while the program is not stopped and this
      //program is the same as the most recent program run
      while (!stopped && localProgram == program) {
        localProgram.setCurrentStdInput(intermittentCommand);
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
      localProgram.setCurrentStdInput(command.getEndCommand());
    }
    catch (IOException e) {
    }
  }

  final IntermittentCommand getCommand() {
    return command;
  }

  final String[] getStdOutput(IntermittentProcessMonitor monitor) {
    //don't get output for a stopped monitor because this would make
    //OutputBufferManager start saving output for the monitor
    if (program == null || monitors == null || !monitors.containsKey(monitor)) {
      return null;
    }
    return program.getStdOutput(monitor);
  }

  final void setCurrentStdInput(String input) {
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

  /**
   * RestartThread creates a single instance and run one worker thread, it tries
   * to restart IntermittentBackgroundProcesses in it's processList member
   * variable at intervals, removing each one from the list.
   * 
   * RestartThread is lazy and won't create the instances or run the worker
   * thread until a restart is requested.  Once the worker thread is started, it
   * will run until stop() is called.  This is done by
   * EtomoDirector.exitProgram().  RestartThread is shared by multiple managers,
   * just like the instances of IntermittentBackgroupProcess.
   */
  private static final class RestartThread implements Runnable {
    private static RestartThread instance = null;

    private final HashedArray processList = new HashedArray();

    private boolean stop = false;

    private RestartThread() {
    }

    /**
     * Try to restart the process every five minutes.  Runs the RestartThread
     * instance, if it is not running.  Adds the process to the list of
     * processes to be restarted.
     * @param process
     */
    static void restart(IntermittentBackgroundProcess process) {
      if (instance == null) {
        runInstance();
      }
      instance.addProcess(process);
    }

    /**
     * A message that a process has been successfully started.  Causes the
     * RestartThread to stop trying to restart it.
     * @param process
     */
    static void msgStarted(IntermittentBackgroundProcess process) {
      if (instance != null) {
        instance.removeProcess(process);
      }
    }

    /**
     * Adds a process to processList.
     * @param process
     */
    private void addProcess(IntermittentBackgroundProcess process) {
      if (!processList.containsKey(process)) {
        processList.add(process);
      }
    }

    /**
     * Removes a process from processList
     * @param process
     */
    private void removeProcess(IntermittentBackgroundProcess process) {
      if (processList.containsKey(process)) {
        processList.remove(process);
      }
    }

    /**
     * Creates and runs the RestartThread instance, if the instance doesn't
     * exist.
     */
    static synchronized void runInstance() {
      if (instance == null) {
        instance = new RestartThread();
        Thread thread = new Thread(instance);
        thread.start();
      }
    }

    /**
     * Try to restart all the processes in processList every five minutes.
     */
    public void run() {
      while (!stop) {
        try {
          for (int i = 0; i < 60 * 5; i++) {
            if (stop) {
              return;
            }
            Thread.sleep(1000);
          }
        }
        catch (InterruptedException e) {
          e.printStackTrace();
        }
        if (stop) {
          return;
        }
        for (int i = 0; i < processList.size(); i++) {
          //restart the process for all monitors which have failed
          IntermittentBackgroundProcess process = (IntermittentBackgroundProcess) processList
              .get(i);
          processList.remove(process);
          process.restartAll();
        }
      }
    }

    static void endThread() {
      if (instance == null) {
        return;
      }
      instance.stop();
    }

    /**
     * Stops the RestartThread instance.
     */
    private synchronized void stop() {
      if (stop == true) {
        return;
      }
      stop = true;
      try {
        Thread.sleep(1001);
      }
      catch (InterruptedException e) {
      }
    }
  }
}
/**
 * <p> $Log$
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
