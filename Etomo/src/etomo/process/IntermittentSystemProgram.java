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
* SystemProgramMonitor, which processes standard out.  The SystemProgramMonitor
* is added to an instance-level list.  If the command is already running when
* the instance receives a new SystemProgramMonitor, then the
* SystemProgramMonitor is "hooked into" the existing command via the standard
* out.  So only one command per instance can be run at a time.
* 
* This class contains a stop(SystemProgramMonitor) function, which stops the
* command in the current instance, but only when a stop has been issued for all
* SystemProgramMonitors.  The SystemProgramMonitor passed to stop is removed
* from the list and should stop monitoring.</p>
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
public class IntermittentSystemProgram implements Runnable {
  public static  final String  rcsid =  "$Id$";
  
  private static Hashtable instances = new Hashtable();//one instance per IntermittentCommand instance
  private boolean stopped = true;
  private HashedArray monitors = new HashedArray();
  private final IntermittentCommand command;
  private final BaseManager manager;
  private SystemProgram program = null;
  
  public String toString() {
    return "[stopped=" + stopped + "," + super.toString() + "]";
  }
  
  static final void startInstance(BaseManager manager,
      IntermittentCommand command, SystemProgramMonitor monitor) {
    getInstance(manager, command, monitor).start(monitor);
  }

  static final void stopInstance(BaseManager manager,
      IntermittentCommand command, SystemProgramMonitor monitor) {
    IntermittentSystemProgram intermittentSystemProgram = getInstance(command);
    if (intermittentSystemProgram != null) {
      intermittentSystemProgram.stop(monitor);
    }
  }

  private static final IntermittentSystemProgram getInstance(
      BaseManager manager, IntermittentCommand command,
      SystemProgramMonitor monitor) {
    IntermittentSystemProgram intermittentSystemProgram = getInstance(command);
    if (intermittentSystemProgram == null) {
      return createInstance(manager, command, monitor);
    }
    return intermittentSystemProgram;
  }

  private static final IntermittentSystemProgram getInstance(
      IntermittentCommand command) {
    return (IntermittentSystemProgram) instances.get(command);
  }

  private static synchronized final IntermittentSystemProgram createInstance(
      BaseManager manager, IntermittentCommand command,
      SystemProgramMonitor monitor) {
    IntermittentSystemProgram intermittentSystemProgram = getInstance(command);
    if (intermittentSystemProgram == null) {
      intermittentSystemProgram = new IntermittentSystemProgram(manager, command, monitor);
      instances.put(intermittentSystemProgram.command, intermittentSystemProgram);
    }
    return intermittentSystemProgram;
  }

  private IntermittentSystemProgram(BaseManager manager,
      IntermittentCommand command, SystemProgramMonitor monitor) {
    this.command = command;
    this.manager = manager;
  }

  private synchronized final void start(SystemProgramMonitor monitor) {
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
      monitor.setIntermittentSystemProgram(this);
    }
  }
  
  synchronized final void stop(SystemProgramMonitor monitor) {
    monitors.remove(monitor);
    if (monitors.size() == 0) {
      stopped = true;
    }
  }
  
  final boolean isStopped() {
    return stopped;
  }
  
  public final void run() {
    //use a local SystemProgram because stops and starts may overlap
    SystemProgram localProgram = new SystemProgram(manager.getPropertyUserDir(),
        command.getCommand(), AxisID.ONLY);
    //place the most recent local SystemProgram in the member SystemProgram
    //non-local request (getting and setting standard input and output) will go
    //to the most recent local SystemProgram.
    program = localProgram;
    localProgram.setAcceptInputWhileRunning(true);
    localProgram.setCollectOutput(false);
    new Thread(localProgram).start();
    int interval = command.getInterval();
    String intermittentCommand = command.getIntermittentCommand();
    try {
      //see load average requests while the program is not stopped and this
      //
      while (!stopped && localProgram == program) {
        localProgram.setCurrentStdInput(intermittentCommand);
        if (monitors != null) {
          for (int i = 0; i < monitors.size(); i++) {
            ((SystemProgramMonitor) monitors.get(i)).msgSentIntermittentCommand(command);
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
          ((SystemProgramMonitor) monitors.get(i)).msgIntermittentCommandFailed(command);
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
  
  final String[] getStdOutput() {
    if (program == null) {
      return null;
    }
    return program.getStdOutput();
  }
  
  final String[] getStdError() {
    if (program == null) {
      return null;
    }
    return program.getStdError();
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
* <p> Revision 1.9  2005/09/07 20:34:24  sueh
* <p> bug# 532 In getStdOutput() return the entire stdOutput structure.
* <p>
* <p> Revision 1.8  2005/09/02 18:58:42  sueh
* <p> bug# 532 removed a null pointer exception problem from run().
* <p>
* <p> Revision 1.7  2005/09/01 17:50:10  sueh
* <p> bug# 532 Added setCurrentStdInput() to allow load average monitor to
* <p> confirm the connect, the first time a connect between computers is made.
* <p>
* <p> Revision 1.6  2005/08/31 17:16:33  sueh
* <p> bug# 532 Allow monitor to call the stop(SystemProgramMonitor).
* <p>
* <p> Revision 1.5  2005/08/30 23:33:50  sueh
* <p> bug# 532 Telling monitors that intermittent command was sent.
* <p>
* <p> Revision 1.4  2005/08/30 18:43:20  sueh
* <p> bug# 532 When the intermitent command throughs a IOException.  Call
* <p> intermittentCommandFailed in all monitors.
* <p>
* <p> Revision 1.3  2005/08/27 22:29:31  sueh
* <p> bug# 532 Handle IOException from SystemProgram.setCurrentStdInput()
* <p> so that failed intermittent commands can halted.
* <p>
* <p> Revision 1.2  2005/08/24 00:20:47  sueh
* <p> bug# 532 removed the running member variable.  Only needed the stopped
* <p> member variable.
* <p>
* <p> Revision 1.1  2005/08/22 16:31:15  sueh
* <p> bug# 532 Runs a command using SystemProgram.  Keeps standard
* <p> input open.   Sends a string through standard input at intervals.
* <p> </p>
*/