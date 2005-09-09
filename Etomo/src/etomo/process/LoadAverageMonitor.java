package etomo.process;

import etomo.comscript.IntermittentCommand;
import etomo.ui.LoadAverageDisplay;
import etomo.util.HashedArray;

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

public class LoadAverageMonitor implements SystemProgramMonitor {
  public static  final String  rcsid =  "$Id$";
  
  private HashedArray programs = new HashedArray();
  private final LoadAverageDisplay display;
  private boolean running = false;
  
  public LoadAverageMonitor(LoadAverageDisplay display) {
    this.display = display;
  }
  
  public void run() {
    try {
      while (!isStopped()) {
        for (int i = 0; i < programs.size(); i++) {
          ProgramState programState = (ProgramState) programs.get(i);
          if (!programState.program.isStopped()) {
            processData(programState);
            if (programState.waitForCommand > 12) {
              programState.waitForCommand = 0;
              msgIntermittentCommandFailed(programState.program.getCommand());
              programState.program.stop(this);
            }
          }
        }
        Thread.sleep(1000);
      }
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
    running = false;
  }
  
  public void setIntermittentSystemProgram(
      IntermittentSystemProgram program) {
    String key = program.getCommand().getKey();
    ProgramState programState = (ProgramState) programs.get(key);
    if (programState == null) {
      programs.add(key, new ProgramState(program));
    }
    else {
      programState.waitForCommand = 0;
    }
    synchronized (programs) {
      if (!running) {
        running = true;
        new Thread(this).start();
      }
    }
  }
  
  private boolean isStopped() {
    if (programs.size() == 0) {
      return false;
    }
    for (int i = 0; i < programs.size(); i++) {
      if(!((ProgramState) programs.get(i)).program.isStopped()) {
        return false;
      }
    }
    return true;
  }
  
  private void processData(ProgramState programState) {
    //process standard out
    String[] stdout = programState.program.getStdOutput();
    if (stdout == null) {
      return;
    }
    for (int i = 0; i < stdout.length; i++) {
      if (stdout[i].indexOf("load average") != -1) {
        programState.waitForCommand = 0;
        String[] array = stdout[i].trim().split("\\s+");
        display.setLoadAverage(programState.program.getCommand().getKey(),
            getLoad(array[array.length - 3]), getLoad(array[array.length - 2]),
            getLoad(array[array.length - 1]));
      }
    }
  }
  
  private final double getLoad(String load) {
    load = load.trim();
    if (load.charAt(load.length() - 1) == ',') {
      return Double.parseDouble(load.substring(0, load.length() - 1));
    }
    return Double.parseDouble(load);
  }
  
  public final void msgIntermittentCommandFailed(IntermittentCommand command) {
    String key = command.getKey();
    if (programs.containsKey(key)) {
      display.msgLoadAverageFailed(key, "load timeout");
    }
  }
  
  public final void msgSentIntermittentCommand(IntermittentCommand command) {
    ProgramState programState = (ProgramState) programs.get(command.getKey());
    if (programState == null) {
      return;
    }
    programState.waitForCommand++;
  }
  
  private final class ProgramState {
    private final IntermittentSystemProgram program;
    private int waitForCommand = 0;
    
    private ProgramState(IntermittentSystemProgram program) {
      this.program = program;
    }
    
    public String toString() {
      return "[program=" + program + ",\nwaitForCommand="
          + waitForCommand + "," + super.toString() + "]";
    }
  }
}
/**
* <p> $Log$
* <p> Revision 1.6  2005/09/07 20:38:09  sueh
* <p> bug# 532 ProcessData(): Looking at the subset of the stdoutput that hasn't
* <p> been processed.  Handling the first time connection question by sending
* <p> "yes".
* <p>
* <p> Revision 1.5  2005/09/01 17:52:43  sueh
* <p> bug# 532 Set waitForCommand to 0 after the problem is found.  Clear the
* <p> load averages on the display when the connection is cut.  Handle first time
* <p> connections between computers.
* <p>
* <p> Revision 1.4  2005/08/31 17:18:24  sueh
* <p> bug# 532 Handle an unresponsive computer by dropping from
* <p> processchunks after 12 unresponses.
* <p>
* <p> Revision 1.3  2005/08/30 18:44:36  sueh
* <p> bug# 532 Added intermittentCommandFailed() to handle a failed w
* <p> command.
* <p>
* <p> Revision 1.2  2005/08/24 00:21:58  sueh
* <p> bug# 532  In processData() changed string used to find the load average
* <p> line so it would both for Linux and Mac.
* <p>
* <p> Revision 1.1  2005/08/22 16:35:18  sueh
* <p> bug# 532 Monitors a group of load average commands running in
* <p> IntermittentSystemProgram instances.  Gets results from standard
* <p> output.  Places results in a LoadAverageDisplay instance.   Runs until it
* <p> receives a stop call associated with each of the
* <p> IntermittentSystemProgram instances.
* <p> </p>
*/