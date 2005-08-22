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
          processData((ProgramState) programs.get(i));
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
    if (!programs.contains(program)) {
      programs.add(program.getKey(), new ProgramState(program));
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
        .getStdOutput(programState.outputIndex);
    if (output == null) {
      return;
    }
    programState.outputIndex += output.length;
    for (int i = 0; i < output.length; i++) {
      if (output[i].indexOf("load average:") != -1) {
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
  
  private final class ProgramState {
    private final IntermittentSystemProgram program;
    private int outputIndex = 0;
    private boolean stopped = false;
    
    private ProgramState(IntermittentSystemProgram program) {
      this.program = program;
    }
  }
}
/**
* <p> $Log$ </p>
*/