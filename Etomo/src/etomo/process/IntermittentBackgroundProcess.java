package etomo.process;

import java.io.IOException;
import java.util.Hashtable;

import etomo.BaseManager;
import etomo.comscript.IntermittentCommand;
import etomo.type.AxisID;
import etomo.util.HashedArray;

/**
* <p>Description:  This class runs a command and then intermittently sends
* another commmand through standard in.  The original command is halted by a
* stop function.
* 
* This class saves the commands it receives in a static storage.  A new instance
* of this class is created only when a command is has not stored is sent to it.
* 
* The parameters for the static getInstance function are IntermittentCommand,
* which contains the command, the intermittent command, and the interval; and
* IntermittentProcessMonitor, which processes standard out.  The
* IntermittentProcessMonitor is added to an instance-level list.  If the command
* is already running when the instance receives a new
* IntermittentProcessMonitor, then the IntermittentProcessMonitor is
* "hooked into" the existing command via the standard out.  So only one command
* per instance can be run at a time.  Any previous commands will stop as soon
* as they see that they are not the most recent command.
* 
* This class contains a stop(SystemProgramMonitor) function, which stops the
* command in the current instance, but only when a stop has been issued for all
* IntermittentProcessMonitors.  The IntermittentProcessMonitor that passes a
* stop is removed from the list and should stop monitoring.
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
  public static  final String  rcsid =  "$Id$";
  
  private static Hashtable instances = new Hashtable();//one instance per IntermittentCommand instance
  private boolean stopped = true;
  private HashedArray monitors = new HashedArray();
  private final IntermittentCommand command;
  private final BaseManager manager;
  private IntermittentSystemProgram program = null;
  //outputKeyPhrase:  string to look for in the standard output.  Assumes all
  //monitors use the same key phrase for a single instance of
  //IntermittentCommand.  This is important for intermittent commands because
  //there is alot more standard output then there is for a single command, and
  //processing it can slow Etomo down.
  private String outputKeyPhrase = null;
  
  public String toString() {
    return "[stopped=" + stopped + "," + super.toString() + "]";
  }
  
  static final void startInstance(BaseManager manager,
      IntermittentCommand command, IntermittentProcessMonitor monitor) {
    getInstance(manager, command, monitor).start(monitor);
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
      intermittentBackgroundProcess = new IntermittentBackgroundProcess(manager, command, monitor);
      instances.put(intermittentBackgroundProcess.command, intermittentBackgroundProcess);
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
      stopped = false;
      new Thread(this).start();
    }
    //Once the thread is started, add the monitor if it is new, make sure not to
    //add it more then once
    if (!monitors.containsKey(monitor)) {
      monitors.add(monitor);
      monitor.setProcess(this);
    }
  }
  
  synchronized final void stop(IntermittentProcessMonitor monitor) {
    monitors.remove(monitor);
    if (monitors.size() == 0) {
      stopped = true;
    }
    program.msgDroppedMonitor(monitor);
  }
  
  final boolean isStopped() {
    return stopped;
  }
  
  public final void run() {
    //use a local SystemProgram because stops and starts may overlap
    IntermittentSystemProgram localProgram = new IntermittentSystemProgram(manager.getPropertyUserDir(),
        command.getCommand(), AxisID.ONLY, outputKeyPhrase);
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
            ((IntermittentProcessMonitor) monitors.get(i)).msgSentIntermittentCommand(command);
          }
        }
        Thread.sleep(interval);
      }
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      if (monitors != null) {
        for (int i = 0; i < monitors.size(); i++) {
          ((IntermittentProcessMonitor) monitors.get(i)).msgIntermittentCommandFailed(command);
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

}
/**
* <p> $Log$
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