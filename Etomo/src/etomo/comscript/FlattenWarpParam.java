package etomo.comscript;

import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.FileType;
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
 * <p> $Log$
 * <p> Revision 1.2  2009/12/19 01:07:54  sueh
 * <p> bug# 1294 Added lambdaForSmoothing and middleContourFile.
 * <p>
 * <p> Revision 1.1  2009/06/05 01:46:57  sueh
 * <p> bug# 1219 Represents the flattenwarp process interface.
 * <p> </p>
 */
public final class FlattenWarpParam {
  public static final String rcsid = "$Id$";

  public static final String ONE_SURFACE_OPTION = "OneSurface";
  public static final String WARP_SPACING_X_AND_Y_OPTION = "WarpSpacingXandY";
  public static final String LAMBDA_FOR_SMOOTHING_OPTION = "LambdaForSmoothing";

  private final List command = new ArrayList();
  private final FortranInputString warpSpacingXandY = new FortranInputString(2);//optional

  private final BaseManager manager;

  private FortranInputString lambdaForSmoothing = null;//optional
  private boolean oneSurface = false;
  private String middleContourFile = null;

  public FlattenWarpParam(BaseManager manager) {
    this.manager = manager;
  }

  private void buildCommand() {
    command.add(BaseManager.getIMODBinPath()
        + ProcessName.FLATTEN_WARP.toString());
    command.add("-PID");
    command.add("-InputFile");
    command.add(FileType.FLATTEN_WARP_INPUT_MODEL.getFileName(manager));
    command.add("-OutputFile");
    command.add(DatasetFiles.getFlattenWarpOutputName(manager));
    if (oneSurface) {
      command.add("-" + ONE_SURFACE_OPTION);
    }
    if (!warpSpacingXandY.isNull()) {
      command.add("-" + WARP_SPACING_X_AND_Y_OPTION);
      command.add(warpSpacingXandY.toString(true));
    }
    if (middleContourFile != null) {
      command.add("-MiddleContourFile");
      command.add(middleContourFile);
    }
    if (lambdaForSmoothing != null && !lambdaForSmoothing.isNull()) {
      command.add("-" + LAMBDA_FOR_SMOOTHING_OPTION);
      command.add(lambdaForSmoothing.toString(true));
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

  /**
   * @param input
   * @return error message if number is not a number and not blank, otherwise null
   */
  public String setLambdaForSmoothing(String input) {
    try {
      lambdaForSmoothing = FortranInputString.getInstance(input);
    }
    catch (NumberFormatException e) {
      return e.getMessage();
    }
    catch (FortranInputSyntaxException e) {
      return e.getMessage();
    }
    return null;
  }

  public void setMiddleContourFile(String input) {
    middleContourFile = input;
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
