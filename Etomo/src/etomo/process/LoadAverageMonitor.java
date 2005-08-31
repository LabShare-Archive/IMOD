package etomo.process;

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
  
  public LoadAverageMonitor(LoadAverageDisplay display) {
    this.display = display;
  }
  
  public void run() {
    try {
      while (!isStopped()) {
        for (int i = 0; i < programs.size(); i++) {
          ProgramState programState = (ProgramState) programs.get(i);
          processData(programState);
          if (programState.waitForCommand > 12) {
            msgIntermittentCommandFailed(programState.program.getKey());
            programState.program.stop(this);
          }
        }
        
        Thread.sleep(500);
      }
    }
    catch (InterruptedException e) {
      e.printStackTrace();
    }
  }
  
  public void setIntermittentSystemProgram(
      IntermittentSystemProgram program) {
    String key = program.getKey();
    if (!programs.containsKey(key)) {
      programs.add(key, new ProgramState(program));
    }
  }
  
  public void stop(String computer) {
    ((ProgramState) programs.get(computer)).stopped = true;
  }
  
  private boolean isStopped() {
    if (programs.size() == 0) {
      return false;
    }
    for (int i = 0; i < programs.size(); i++) {
      if(!((ProgramState) programs.get(i)).stopped) {
        return false;
      }
    }
    return true;
  }
  
  private void processData(ProgramState programState) {
    String[] output = programState.program
        .getStdOutput(programState.stdOutputIndex);
    if (output == null) {
      return;
    }
    programState.stdOutputIndex += output.length;
    for (int i = 0; i < output.length; i++) {
      programState.waitForCommand--;
      if (output[i].indexOf("load average") != -1) {
        String[] array = output[i].trim().split("\\s+");
        display.setLoadAverage(programState.program.getKey(),
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
  
  public final void msgIntermittentCommandFailed(String key) {
    if (programs.containsKey(key)) {
      display.loadAverageFailed(key);
    }
  }
  
  public final void msgSentIntermittentCommand(String key) {
    ((ProgramState) programs.get(key)).waitForCommand++;
  }
  
  private final class ProgramState {
    private final IntermittentSystemProgram program;
    private int stdOutputIndex = 0;
    private boolean stopped = false;
    private int waitForCommand = 0;
    
    private ProgramState(IntermittentSystemProgram program) {
      this.program = program;
    }
  }
}
/**
* <p> $Log$
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