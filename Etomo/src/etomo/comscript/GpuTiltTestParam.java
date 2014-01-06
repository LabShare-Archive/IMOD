package etomo.comscript;

import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class GpuTiltTestParam {
  public static final String rcsid = "$Id:$";

  public static final String OUTPUT_KEYWORD = "differed";
  private final EtomoNumber nMinutes = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final EtomoNumber gpuNumber = new EtomoNumber();

  public void setNMinutes(final String input) {
    nMinutes.set(input);
  }

  public void setGpuNumber(final Number input) {
    gpuNumber.set(input);
  }

  public String[] getCommand() {
    List<String> command = new ArrayList<String>();
    command.add("python");
    command.add("-u");
    command.add(BaseManager.getIMODBinPath() + ProcessName.GPU_TILT_TEST.toString());
    command.add("-PID");
    command.add(nMinutes.toString());
    command.add(gpuNumber.toString());
    return command.toArray(new String[command.size()]);
  }
}
