package etomo.comscript;

import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;
import etomo.ui.UIExpertUtilities;
import etomo.util.DatasetFiles;

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
 * <p> $Log$ </p>
 */
public final class RunraptorParam {
  public static final String rcsid = "$Id$";

  private final List command = new ArrayList();
  private final EtomoNumber diam = new EtomoNumber(EtomoNumber.Type.LONG);
  private final EtomoNumber mark = new EtomoNumber();

  private final BaseManager manager;
  private final AxisID axisID;

  private boolean useRawStack = false;

  public RunraptorParam(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    diam.setValidFloor(1);
    mark.setValidFloor(10);
  }

  private void buildCommand() {
    command.add("tcsh");
    command.add("-f");
    command
        .add(BaseManager.getIMODBinPath() + ProcessName.RUNRAPTOR.toString());
    command.add("-diam");
    command.add(diam.toString());
    command.add("-mark");
    command.add(mark.toString());
    if (useRawStack) {
      command.add(DatasetFiles.getStackName(manager, axisID));
    }
    else {
      command.add(DatasetFiles.getPrealignedStackName(manager, axisID));
    }
  }

  public String setDiam(String input, boolean mayBeBinned) {
    diam.set(input);
    if (!diam.isValid()) {
      return diam.getInvalidReason();
    }
    if (mayBeBinned) {
      diam.set(Math.round((double) diam.getLong()
          / UIExpertUtilities.INSTANCE.getStackBinning(manager, axisID,
              ".preali")));
      if (!diam.isValid()) {
        return "The binned diameter is " + diam + ".  "
            + diam.getInvalidReason();
      }
    }
    return null;
  }

  public String setMark(String input) {
    mark.set(input);
    if (!mark.isValid()) {
      return mark.getInvalidReason();
    }
    return null;
  }

  public void setUseRawStack(boolean input) {
    useRawStack = input;
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public ProcessName getProcessName() {
    return ProcessName.RUNRAPTOR;
  }

  public String[] getCommandArray() {
    if (command.isEmpty()) {
      buildCommand();
    }
    String[] commandArray;
    if (command.size() == 1) {
      commandArray = new String[] { (String) command.get(0) };
    }
    commandArray = (String[]) command.toArray(new String[command.size()]);
    for (int i = 0; i < commandArray.length; i++) {
      System.err.print(commandArray[i] + " ");
    }
    System.err.println();
    return commandArray;
  }
}
