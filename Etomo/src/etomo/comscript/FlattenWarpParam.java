package etomo.comscript;

import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.ProcessName;
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
public final class FlattenWarpParam {
  public static final String rcsid = "$Id$";

  public static final String ONE_SURFACE_OPTION = "OneSurface";
  public static final String WARP_SPACING_X_AND_Y_OPTION = "WarpSpacingXandY";

  private final List command = new ArrayList();
  private final FortranInputString warpSpacingXandY = new FortranInputString(2);//optional

  private final BaseManager manager;

  private boolean oneSurface = false;

  public FlattenWarpParam(BaseManager manager) {
    this.manager = manager;
  }

  private void buildCommand() {
    command.add(BaseManager.getIMODBinPath()
        + ProcessName.FLATTEN_WARP.toString());
    command.add("-PID");
    command.add("-InputFile");
    command.add(DatasetFiles.getFlattenWarpInputName(manager));
    command.add("-OutputFile");
    command.add(DatasetFiles.getFlattenWarpOutputName(manager));
    if (oneSurface) {
      command.add("-" + ONE_SURFACE_OPTION);
    }
    if (!warpSpacingXandY.isNull()) {
      command.add("-"+WARP_SPACING_X_AND_Y_OPTION);
      command.add(warpSpacingXandY.toString(true));
    }
  }

  public void setOneSurface(boolean input) {
    oneSurface = input;
  }

  /**
   * @param number
   * @return error message if number is not a number and not blank, otherwise null
   */
  public String setWarpSpacingX(String number) {
    try {
      warpSpacingXandY.set(0, number);
    }
    catch (NumberFormatException e) {
      return e.getMessage();
    }
    return null;
  }

  /**
   * 
   * @param number
   * @return error message if number is not a number and not blank, otherwise null
   */
  public String setWarpSpacingY(String number) {
    try {
      warpSpacingXandY.set(1, number);
    }
    catch (NumberFormatException e) {
      return e.getMessage();
    }
    return null;
  }

  public ProcessName getProcessName() {
    return ProcessName.FLATTEN_WARP;
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
