package etomo.comscript;

import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.type.ScriptParameter;
import etomo.type.StringParameter;
import etomo.util.DatasetFiles;

/**
 * <p>Description: Models the warpvol command.  Currently set up to run after
 * flattenwarp.  Could be made more general by allowing the input and output
 * files to be set.</p>
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
public final class WarpVolParam implements ConstWarpVolParam, CommandParam {
  public static final String rcsid = "$Id$";
  
  public static final String INPUT_FILE_OPTION = "InputFile";
  public static final String TEMPORARY_DIRECTORY_OPTION="TemporaryDirectory";
  public static final String OUTPUT_SIZE_X_Y_Z_OPTION ="OutputSizeXYZ";
  public static final String INTERPOLATION_ORDER_OPTION="InterpolationOrder";

  static final String COMMAND = "warpvol";

  private final List command = new ArrayList();
  StringParameter inputFile = new StringParameter(INPUT_FILE_OPTION);
  StringParameter temporaryDirectory = new StringParameter(TEMPORARY_DIRECTORY_OPTION);//optional
  FortranInputString outputSizeXYZ = new FortranInputString(OUTPUT_SIZE_X_Y_Z_OPTION, 3);//optional
  ScriptParameter interpolationOrder = new ScriptParameter(INTERPOLATION_ORDER_OPTION);//optional
  private final BaseManager manager;
  private final AxisID axisID;

  public WarpVolParam(BaseManager manager, AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    outputSizeXYZ.setIntegerType(true);
    reset();
  }

  /**
   }* Get parameter values that may be displayed on the screen.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    temporaryDirectory.parse(scriptCommand);
    outputSizeXYZ.validateAndSet(scriptCommand);
    interpolationOrder.parse(scriptCommand);
  }

  /**
   * Update all parameter values.
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    scriptCommand.useKeywordValue();
    inputFile.updateComScript(scriptCommand);
    temporaryDirectory.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, "OutputFile",
        FileType.FLATTEN_OUTPUT.getFileName(manager), true);
    ParamUtilities.updateScriptParameter(scriptCommand, "TransformFile",
        DatasetFiles.getFlattenWarpOutputName(manager), true);
    outputSizeXYZ.updateScriptParameter(scriptCommand);
    ParamUtilities
        .updateScriptParameter(scriptCommand, "SameSizeAsInput", true);
  }
  

  private void reset() {
    outputSizeXYZ.setDefault();
    interpolationOrder.reset();
  }

  public void initializeDefaults() {
    reset();
  }
  
  public void setTemporaryDirectory(String input) {
    temporaryDirectory.set(input);
  }
  
  public void setInputFile(FileType fileType) {
    inputFile.set(fileType.getFileName(manager));
  }
  
  public String getTemporaryDirectory() {
    return temporaryDirectory.toString();
  }
  
  public String getOutputSizeZ() {
    if (outputSizeXYZ.isNull(2)) {
      return "";
    }
    return outputSizeXYZ.toString(2);
  }

  /**
   * Assign number to outputSizeXYZ at index 2.
   * @param number
   * @return error message if number is not blank or a number
   */
  public String setOutputSizeZ(final String number) {
    try {
      outputSizeXYZ.set(2, number);
    }
    catch (NumberFormatException e) {
      return e.getMessage();
    }
    return null;
  }

  public boolean isInterpolationOrderLinear() {
    return interpolationOrder.equals(1);
  }

  /**
   * If input is true, set interpolationOrder to 1, otherwise default it.
   * @param input
   */
  public void setInterpolationOrderLinear(boolean input) {
    if (input) {
      interpolationOrder.set(1);
    }
    else {
      interpolationOrder.reset();
    }
  }
}
