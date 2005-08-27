package etomo.process;

import java.io.IOException;
import java.util.HashSet;
import java.util.Hashtable;

import etomo.BaseManager;
import etomo.comscript.IntermittentCommand;
import etomo.type.AxisID;

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
  
  private static Hashtable instances = new Hashtable();//one instance per IntermittentCommand
  private final String key;
  private boolean stopped = true;
  private HashSet monitors = new HashSet();
  private final IntermittentCommand command;
  private final BaseManager manager;
  private SystemProgram program = null;
  
  static final void startInstance(BaseManager manager,
      IntermittentCommand command, SystemProgramMonitor monitor) {
    getInstance(manager, command, monitor).start(monitor);
  }

  static final void stopInstance(BaseManager manager,
      IntermittentCommand command, SystemProgramMonitor monitor) {
    IntermittentSystemProgram program = getInstance(command);
    if (program != null) {
      program.stop(monitor);
    }
  }

  private static final IntermittentSystemProgram getInstance(
      BaseManager manager, IntermittentCommand command,
      SystemProgramMonitor monitor) {
    IntermittentSystemProgram program = getInstance(command);
    if (program == null) {
      return createInstance(manager, command, monitor);
    }
    return program;
  }

  private static final IntermittentSystemProgram getInstance(
      IntermittentCommand command) {
    return (IntermittentSystemProgram) instances.get(command.getKey());
  }

  private static synchronized final IntermittentSystemProgram createInstance(
      BaseManager manager, IntermittentCommand command,
      SystemProgramMonitor monitor) {
    IntermittentSystemProgram program = getInstance(command);
    if (program == null) {
      program = new IntermittentSystemProgram(manager, command, monitor);
      instances.put(program.key, program);
    }
    return program;
  }

  private IntermittentSystemProgram(BaseManager manager,
      IntermittentCommand command, SystemProgramMonitor monitor) {
    this.command = command;
    this.key = command.getKey();
    this.manager = manager;
  }

  private final void start(SystemProgramMonitor monitor) {
    boolean newMonitor = false;
    //add the monitor if it is new, make sure not to add it more then once
    synchronized (monitors) {
      newMonitor = !monitors.contains(monitor);
      if (newMonitor) {
        monitors.add(monitor);
      }
    }
    //if the monitor is new || stopped, run it
    if (newMonitor) {
      monitor.setIntermittentSystemProgram(this);
      new Thread(monitor).start();
    }
    //run the instance, if it is not running
    //this is the only place that stopped should be set to false
    boolean startThreadNow = false;
    synchronized (monitors) {
      if (stopped) {
        stopped = false;
        startThreadNow = true;
      }
    }
    //StartThreadNow can only be true if stopped was true and then was set to
    //false in synchronized code.  This way only one thread can call start() for
    //this instance.
    if (startThreadNow) {
      new Thread(this).start();
    }
  }
  
  private final void stop(SystemProgramMonitor monitor) {
    monitor.stop(key);
    synchronized (monitors) {
      monitors.remove(monitor);
    }
    if (monitors.size() == 0) {
      stopped = true;
    }
  }
  
  public final void run() {
    program = new SystemProgram(manager.getPropertyUserDir(),
        command.getCommand(), AxisID.ONLY);
    program.setAcceptInputWhileRunning(true);
    new Thread(program).start();
    int interval = command.getInterval();
    String intermittentCommand = command.getIntermittentCommand();
    try {
      while (!stopped) {
        program.setCurrentStdInput(intermittentCommand);
        Thread.sleep(interval);
      }
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    try {
      program.setCurrentStdInput(command.getEndCommand());
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    program = null;
  }
  
  final SystemProgram getSystemProgram() {
    return program;
  }
  
  final String getKey() {
    return key;
  }
  
  final String[] getStdOutput(int index) {
    if (program == null) {
      return null;
    }
    String[] output = program.getStdOutput();
    if (output == null) {
      return null;
    }
    if (index >= output.length) {
      return null;
    }
    String[] outputSection = new String[output.length - index];
    int sectionIndex = 0;
    for (int i = index; i < output.length; i++) {
      outputSection[sectionIndex++] = output[i];
    }
    return outputSection;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/08/24 00:20:47  sueh
* <p> bug# 532 removed the running member variable.  Only needed the stopped
* <p> member variable.
* <p>
* <p> Revision 1.1  2005/08/22 16:31:15  sueh
* <p> bug# 532 Runs a command using SystemProgram.  Keeps standard
* <p> input open.   Sends a string through standard input at intervals.
* <p> </p>
*/