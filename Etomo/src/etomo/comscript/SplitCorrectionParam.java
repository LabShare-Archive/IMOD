package etomo.comscript;

import java.util.ArrayList;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2008/10/27 17:49:45  sueh
 * <p> bug # 1141 Class to create a splitcorrection call.
 * <p> </p>
 */
public final class SplitCorrectionParam implements ConstSplitCorrectionParam {
  public static final String rcsid = "$Id$";

  private final AxisID axisID;
  private final EtomoNumber cpus = new EtomoNumber();

  private int maxZ = 0;
  private String[] commandArray = null;

  public SplitCorrectionParam(AxisID axisID) {
    this.axisID = axisID;
  }

  public String[] getCommand() {
    if (commandArray == null) {
      buildCommand();
    }
    return commandArray;
  }

  private void buildCommand() {
    ArrayList command = new ArrayList();
    command.add("python");
    command.add("-u");
    command.add(BaseManager.getIMODBinPath() + ProcessName.SPLIT_CORRECTION);
    if (!cpus.isNull() && !cpus.equals(0)) {
      command.add("-m");
      //target # of chunks
      int target = 2 * cpus.getInt();
      //max slices
      EtomoNumber maxSlices = new EtomoNumber();
      maxSlices.set((maxZ + target - 1) / target);
      command.add(maxSlices.toString());
    }
    command.add(ProcessName.CTF_CORRECTION.getComscript(axisID));
    int commandSize = command.size();
    commandArray = new String[commandSize];
    for (int i = 0; i < commandSize; i++) {
      commandArray[i] = (String) command.get(i);
    }
  }

  public void setCpus(String input) {
    cpus.set(input);
  }

  public void setMaxZ(int input) {
    maxZ = input;
  }
}
