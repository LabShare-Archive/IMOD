package etomo.comscript;

import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;
import etomo.ui.swing.UIExpertUtilities;
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
 * <p> $Log$
 * <p> Revision 1.5  2011/02/22 03:26:56  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.4  2010/11/13 16:03:15  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2009/06/05 01:49:00  sueh
 * <p> bug# 1219 Removed unused getCommandMode.
 * <p>
 * <p> Revision 1.2  2009/06/01 18:46:38  sueh
 * <p> bug# 1216 Validation:  preventing null parameters.
 * <p>
 * <p> Revision 1.1  2009/05/02 01:07:18  sueh
 * <p> bug# 1216 Parameters for runraptor.
 * <p> </p>
 */
public final class RunraptorParam {
  public static final String rcsid = "$Id$";

  private static final String DIAM_OPTION = "diam";
  private static final String MARK_OPTION = "mark";

  private final List command = new ArrayList();
  private final EtomoNumber diam = new EtomoNumber();
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
    command.add("python");
    command.add("-u");
    command.add(BaseManager.getIMODBinPath() + ProcessName.RUNRAPTOR.toString());
    command.add("-PID");
    command.add("-" + DIAM_OPTION);
    command.add(diam.toString());
    command.add("-" + MARK_OPTION);
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
    if (diam.isNull()) {
      return "Empty " + DIAM_OPTION + " parameter.";
    }
    if (!diam.isValid()) {
      return diam.getInvalidReason();
    }
    if (mayBeBinned) {
      diam.set(Math.round(diam.getInt()
          / UIExpertUtilities.INSTANCE.getStackBinning(manager, axisID, ".preali")));
      if (!diam.isValid()) {
        return "The binned diameter is " + diam + ".  " + diam.getInvalidReason();
      }
    }
    return null;
  }

  public String setMark(String input) {
    mark.set(input);
    if (mark.isNull()) {
      return "Empty " + MARK_OPTION + " parameter.";
    }
    if (!mark.isValid()) {
      return mark.getInvalidReason();
    }
    return null;
  }

  public void setUseRawStack(boolean input) {
    useRawStack = input;
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
