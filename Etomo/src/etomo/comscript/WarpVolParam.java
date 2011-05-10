package etomo.comscript;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.type.ImageFileType;
import etomo.type.ProcessName;
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
 * <p> $Log$
 * <p> Revision 1.5  2011/02/21 19:32:15  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.4  2010/04/28 16:12:36  sueh
 * <p> bug# 1344 Added a list of modes.  Added mode to the constructor.  Added
 * <p> getOutputImageFileType functions.  Implemented Command.
 * <p>
 * <p> Revision 1.3  2010/02/17 04:47:14  sueh
 * <p> bug# 1301 Added outputFile
 * <p>
 * <p> Revision 1.2  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.1  2009/06/05 01:51:06  sueh
 * <p> bug# 1219 Represents any .com file which runs warpvol.
 * <p> </p>
 */
public final class WarpVolParam implements ConstWarpVolParam, CommandParam {
  public static final String rcsid = "$Id$";

  public static final String INPUT_FILE_OPTION = "InputFile";
  public static final String TEMPORARY_DIRECTORY_OPTION = "TemporaryDirectory";
  public static final String OUTPUT_SIZE_X_Y_Z_OPTION = "OutputSizeXYZ";
  public static final String INTERPOLATION_ORDER_OPTION = "InterpolationOrder";

  static final String COMMAND = "warpvol";

  private final List command = new ArrayList();
  StringParameter inputFile = new StringParameter(INPUT_FILE_OPTION);
  StringParameter outputFile = new StringParameter("OutputFile");
  StringParameter temporaryDirectory = new StringParameter(TEMPORARY_DIRECTORY_OPTION);//optional
  FortranInputString outputSizeXYZ = new FortranInputString(OUTPUT_SIZE_X_Y_Z_OPTION, 3);//optional
  ScriptParameter interpolationOrder = new ScriptParameter(INTERPOLATION_ORDER_OPTION);//optional
  private final BaseManager manager;
  private final AxisID axisID;
  private final CommandMode mode;

  WarpVolParam(BaseManager manager, AxisID axisID, CommandMode mode) {
    this.manager = manager;
    this.axisID = axisID;
    this.mode = mode;
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
    outputFile.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, "TransformFile", DatasetFiles
        .getFlattenWarpOutputName(manager), true);
    outputSizeXYZ.updateScriptParameter(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, "SameSizeAsInput", true);
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

  public void setInputFile(ImageFileType imageFileType) {
    inputFile.set(imageFileType.getFileName(manager));
  }

  public void setOutputFile(String input) {
    outputFile.set(input);
  }

  public void setInputFile(File file) {
    inputFile.set(file.getAbsolutePath());
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

  public FileType getOutputImageFileType() {
    if (mode == Mode.POST_PROCESSING) {
      return FileType.FLATTEN_OUTPUT;
    }
    if (mode == Mode.TOOLS) {
      return FileType.FLATTEN_TOOL_OUTPUT;
    }
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  private FileType getComscriptFileType() {
    if (mode == Mode.POST_PROCESSING) {
      return FileType.FLATTEN_COMSCRIPT;
    }
    if (mode == Mode.TOOLS) {
      return FileType.FLATTEN_TOOL_COMSCRIPT;
    }
    return null;
  }

  public String getCommand() {
    FileType comscriptFileType = getComscriptFileType();
    if (comscriptFileType != null) {
      return comscriptFileType.getFileName(manager, axisID);
    }
    return null;
  }

  public String[] getCommandArray() {
    String[] array = { getCommandLine() };
    return array;
  }

  public File getCommandInputFile() {
    return null;
  }

  public String getCommandLine() {
    return getCommand();
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public String getCommandName() {
    return ProcessName.FLATTEN.toString();
  }

  public File getCommandOutputFile() {
    return null;
  }

  public ProcessName getProcessName() {
    return ProcessName.FLATTEN;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  static final class Mode implements CommandMode {
    static final Mode POST_PROCESSING = new Mode();
    static final Mode TOOLS = new Mode();
  }
}
