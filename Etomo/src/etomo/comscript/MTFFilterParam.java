/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright(c) 2002, 2003, 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.10  2011/02/22 03:17:56  sueh
 * <p> $bug# 1437 Reformatting.
 * <p> $
 * <p> $Revision 1.9  2010/04/28 16:01:59  sueh
 * <p> $bug# 1344 Implemented Command.  Added getOutputImageFileType
 * <p> $functions.  Made Const ancestor class an interface.
 * <p> $
 * <p> $Revision 1.8  2004/06/13 17:03:23  rickg
 * <p> $Solvematch mid change
 * <p> $
 * <p> $Revision 1.7  2004/06/09 21:18:12  rickg
 * <p> $Changed upateParameter method to updateScriptParameter
 * <p> $
 * <p> $Revision 1.6  2004/05/03 18:01:17  sueh
 * <p> $bug# 418 standardizing set function
 * <p> $
 * <p> $Revision 1.5  2004/04/16 01:50:33  sueh
 * <p> $bug# 409 added startingAndEndingZ, formatted
 * <p> $
 * <p> $Revision 1.4  2004/04/12 17:15:46  sueh
 * <p> $bug# 409  Change HighFrequencyRadiusSigma to LowPassRadiusSigma.  Add
 * <p> $initializeDefaults() to set the default comscript values.
 * <p> $
 * <p> $Revision 1.3  2004/03/29 20:51:46  sueh
 * <p> $bug# 409 add inputFile
 * <p> $
 * <p> $Revision 1.2  2004/03/25 00:47:18  sueh
 * <p> $bug# 409, bug# 418 added InverseRooloffRadiusSigma, moved utilities functions
 * <p> $to ParamUtilities
 * <p> $
 * <p> $Revision 1.1  2004/03/24 18:15:51  sueh
 * <p> $bug# 409 MTF filter params
 * <p> $$ </p>
 */

package etomo.comscript;

import java.io.File;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.FileType;
import etomo.type.ProcessName;

public final class MTFFilterParam implements ConstMTFFilterParam, CommandParam {
  public static final String rcsid = "$$Id$$";

  private String inputFile;
  private String outputFile;
  private String mtfFile;
  private double maximumInverse;
  private final FortranInputString lowPassRadiusSigma;
  private final FortranInputString inverseRolloffRadiusSigma;
  private final FortranInputString startingAndEndingZ;

  private final BaseManager manager;
  private final AxisID axisID;

  MTFFilterParam(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    lowPassRadiusSigma = new FortranInputString(2);
    inverseRolloffRadiusSigma = new FortranInputString(2);
    startingAndEndingZ = new FortranInputString(2);
    startingAndEndingZ.setIntegerType(0, true);
    startingAndEndingZ.setIntegerType(1, true);
    reset();
  }

  private void reset() {
    inputFile = new String();
    outputFile = new String();
    mtfFile = new String();
    maximumInverse = Double.NaN;
    lowPassRadiusSigma.setDefault();
    inverseRolloffRadiusSigma.setDefault();
    startingAndEndingZ.setDefault();
  }

  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws FortranInputSyntaxException, InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    if (scriptCommand.isKeywordValuePairs()) {
      if (scriptCommand.hasKeyword("InputFile")) {
        inputFile = scriptCommand.getValue("InputFile");
      }
      if (scriptCommand.hasKeyword("OutputFile")) {
        outputFile = scriptCommand.getValue("OutputFile");
      }
      if (scriptCommand.hasKeyword("MtfFile")) {
        mtfFile = scriptCommand.getValue("MtfFile");
      }
      if (scriptCommand.hasKeyword("MaximumInverse")) {
        maximumInverse = Double.parseDouble(scriptCommand.getValue("MaximumInverse"));
      }
      if (scriptCommand.hasKeyword("LowPassRadiusSigma")) {
        lowPassRadiusSigma.validateAndSet(scriptCommand.getValue("LowPassRadiusSigma"));
      }
      if (scriptCommand.hasKeyword("InverseRolloffRadiusSigma")) {
        inverseRolloffRadiusSigma.validateAndSet(scriptCommand
            .getValue("InverseRolloffRadiusSigma"));
      }
      if (scriptCommand.hasKeyword("StartingAndEndingZ")) {
        startingAndEndingZ.validateAndSet(scriptCommand.getValue("StartingAndEndingZ"));
      }
    }
    else {
      throw new InvalidParameterException(
          "MTF Filter:  Missing parameter, -StandardInput.  Use Etomo to create .com file.");
    }
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    scriptCommand.useKeywordValue();
    ParamUtilities.updateScriptParameter(scriptCommand, "InputFile", inputFile, true);
    ParamUtilities.updateScriptParameter(scriptCommand, "OutputFile", outputFile);
    ParamUtilities.updateScriptParameter(scriptCommand, "MtfFile", mtfFile);
    ParamUtilities.updateScriptParameter(scriptCommand, "MaximumInverse", maximumInverse);
    ParamUtilities.updateScriptParameter(scriptCommand, "LowPassRadiusSigma",
        lowPassRadiusSigma);
    ParamUtilities.updateScriptParameter(scriptCommand, "InverseRolloffRadiusSigma",
        inverseRolloffRadiusSigma);
    ParamUtilities.updateScriptParameter(scriptCommand, "StartingAndEndingZ",
        startingAndEndingZ);
  }

  public void initializeDefaults() {
    maximumInverse = 4.0;
    inverseRolloffRadiusSigma.set(0, 0.12);
    inverseRolloffRadiusSigma.set(1, 0.05);
    lowPassRadiusSigma.set(0, 0.35);
    lowPassRadiusSigma.set(1, 0.05);
  }

  public String getMtfFile() {
    return mtfFile;
  }

  public String getMaximumInverseString() {
    return ParamUtilities.valueOf(maximumInverse);
  }

  public String getLowPassRadiusSigmaString() {
    return lowPassRadiusSigma.toString(true);
  }

  public String getStartingAndEndingZString() {
    return startingAndEndingZ.toString(true);
  }

  public boolean isStartingZSet() {
    return !startingAndEndingZ.isDefault(0) && !startingAndEndingZ.isEmpty(0);
  }

  public boolean isEndingZSet() {
    return !startingAndEndingZ.isDefault(1) && !startingAndEndingZ.isEmpty(1);
  }

  public int getStartingZ() {
    return startingAndEndingZ.getInt(0);
  }

  public int getEndingZ() {
    return startingAndEndingZ.getInt(1);
  }

  public String getInverseRolloffRadiusSigmaString() {
    return inverseRolloffRadiusSigma.toString(true);
  }

  public String getOutputFile() {
    return outputFile;
  }

  public void setInputFile(String inputFile) {
    this.inputFile = new String(inputFile);
  }

  public void setOutputFile(String outputFile) {
    this.outputFile = new String(outputFile);
  }

  public void setMtfFile(String mtfFile) {
    this.mtfFile = new String(mtfFile);
  }

  public void setMaximumInverse(String maximumInverse) {
    this.maximumInverse = ParamUtilities.parseDouble(maximumInverse);
  }

  public void setLowPassRadiusSigma(String lowPassRadiusSigma)
      throws FortranInputSyntaxException {
    ParamUtilities.set(lowPassRadiusSigma, this.lowPassRadiusSigma);
  }

  public void setInverseRolloffRadiusSigma(String inverseRolloffRadiusSigma)
      throws FortranInputSyntaxException {
    ParamUtilities.set(inverseRolloffRadiusSigma, this.inverseRolloffRadiusSigma);
  }

  public void setStartingAndEndingZ(String startingAndEndingZ)
      throws FortranInputSyntaxException {
    ParamUtilities.set(startingAndEndingZ, this.startingAndEndingZ);
  }

  public FileType getOutputImageFileType() {
    return FileType.MTF_FILTERED_STACK;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String getCommand() {
    return FileType.MTF_FILTER_COMSCRIPT.getFileName(manager, axisID);
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
    return ProcessName.MTFFILTER.toString();
  }

  public File getCommandOutputFile() {
    return null;
  }

  public ProcessName getProcessName() {
    return ProcessName.MTFFILTER;
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
}
