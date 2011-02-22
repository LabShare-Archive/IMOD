package etomo.comscript;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.7  2006/09/13 23:16:56  sueh
 * <p> bug# 921 Added initialShiftXYZ.
 * <p>
 * <p> Revision 3.6  2006/08/29 20:03:19  sueh
 * <p> bug# 924 Added kernelSigma.
 * <p>
 * <p> Revision 3.5  2006/08/25 22:51:18  sueh
 * <p> bug# 918 Convert to PIP
 * <p>
 * <p> Revision 3.4  2004/04/12 16:50:22  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.3  2004/03/11 18:14:32  sueh
 * <p> bug# 386 changing updateComScriptCommand()
 * <p>
 * <p> Revision 3.2  2004/03/05 18:17:44  sueh
 * <p> bug# 250 changed updateComScriptCommand() - allow cmdLineArgs to grow
 * <p>
 * <p> Revision 3.1  2004/03/02 21:52:19  sueh
 * <p> bug# 250 changed parseComScriptCommand() - correcting parameters
 * <p> changed updateComScriptCommand() - adding parameters
 * <p> added borders, move reset() to const
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.6  2003/07/25 22:54:14  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.5  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.4  2003/04/29 20:13:40  rickg
 * <p> Corrected range for number of patchcrawl args
 * <p>
 * <p> Revision 2.3  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.2  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */
/**
 * @author rickg
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code Template
 */
public class Patchcrawl3DParam extends ConstPatchcrawl3DParam implements CommandParam {
  public static final String rcsid = "$Id:";

  public static final String COMMAND = "corrsearch3d";
  private boolean convertToPIP = false;

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    if (!scriptCommand.isKeywordValuePairs()) {
      convertToPIP = true;
      Patchcrawl3DPrePIPParam prePIPparam = new Patchcrawl3DPrePIPParam();
      prePIPparam.parseComScriptCommand(scriptCommand);
      load(prePIPparam);
      return;
    }
    convertToPIP = false;
    reset();
    try {
      patchSizeXYZ.validateAndSet(scriptCommand);
      numberOfPatchesXYZ.validateAndSet(scriptCommand);
      xMinAndMax.validateAndSet(scriptCommand);
      yMinAndMax.validateAndSet(scriptCommand);
      if (scriptCommand.hasKeyword(REGION_MODEL_KEY)) {
        regionModel = scriptCommand.getValue(REGION_MODEL_KEY);
      }
      initialShiftXYZ.validateAndSet(scriptCommand);
      kernelSigma.parse(scriptCommand, true);
    }
    catch (NumberFormatException except) {
      throw new BadComScriptException(except.getMessage());
    }

  }

  /**
   * Handle old version of the comscript
   * @param param
   * @throws FortranInputSyntaxException
   */
  public void load(Patchcrawl3DPrePIPParam param) throws FortranInputSyntaxException {
    if (!convertToPIP) {
      throw new IllegalStateException("!convertToPIP in load()");
    }
    reset();
    //xPatchSize, yPatchSize, zPatchSize
    patchSizeXYZ.set(0, param.getXPatchSize());
    patchSizeXYZ.set(1, param.getYPatchSize());
    patchSizeXYZ.set(2, param.getZPatchSize());
    //nX, nY, nZ
    numberOfPatchesXYZ.set(0, param.getNX());
    numberOfPatchesXYZ.set(1, param.getNY());
    numberOfPatchesXYZ.set(2, param.getNZ());
    //xLow, xHigh
    xMinAndMax.set(0, param.getXLow());
    xMinAndMax.set(1, param.getXHigh());
    //yLow, yHigh
    yMinAndMax.set(0, param.getYLow());
    yMinAndMax.set(1, param.getYHigh());
    //zLow, zHigh
    zMinAndMax.set(0, param.getZLow());
    zMinAndMax.set(1, param.getZHigh());
    //drop maxShift
    //fileA
    referenceFile = param.getFileA();
    //fileB
    fileToAlign = param.getFileB();
    //outputFile
    outputFile = param.getOutputFile();
    //transformFile
    bSourceTransform = param.getTransformFile();
    //originalFileB
    bSourceOrSizeXYZ = param.getOriginalFileB();
    //borders
    FortranInputString borders = param.getBordersFortranInputString();
    bSourceBorderXLoHi.set(0, borders.getInt(0));
    bSourceBorderXLoHi.set(1, borders.getInt(1));
    bSourceBorderYZLoHi.set(0, borders.getInt(2));
    bSourceBorderYZLoHi.set(1, borders.getInt(3));
    //boundaryModel
    regionModel = param.getBoundaryModel();
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command
   */
  private ComScriptInputArg[] getInputArguments(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    String command = scriptCommand.getCommand();
    //  Check to be sure that it is the right command
    if (!command.equals(COMMAND) && !(command.equals("patchcrawl3d") && convertToPIP)) {
      throw (new BadComScriptException("Not a " + COMMAND + " command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    return inputArgs;
  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript(etomo.comscript.ComScriptCommand)
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    //  get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getInputArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    //  Switch to keyword/value pairs
    scriptCommand.useKeywordValue();
    patchSizeXYZ.updateScriptParameter(scriptCommand);
    numberOfPatchesXYZ.updateScriptParameter(scriptCommand);
    xMinAndMax.updateScriptParameter(scriptCommand);
    yMinAndMax.updateScriptParameter(scriptCommand);
    zMinAndMax.updateScriptParameter(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, REGION_MODEL_KEY, regionModel);
    initialShiftXYZ.updateScriptParameter(scriptCommand);
    kernelSigma.updateComScript(scriptCommand);
    if (convertToPIP) {
      scriptCommand.setCommand(COMMAND);
      ParamUtilities.updateScriptParameter(scriptCommand, REFERENCE_FILE_KEY,
          referenceFile);
      ParamUtilities.updateScriptParameter(scriptCommand, FILE_TO_ALIGN_KEY, fileToAlign);
      ParamUtilities.updateScriptParameter(scriptCommand, OUTPUT_FILE_KEY, outputFile);
      ParamUtilities.updateScriptParameter(scriptCommand, B_SOURCE_TRANSFORM_KEY,
          bSourceTransform);
      ParamUtilities.updateScriptParameter(scriptCommand, B_SOURCE_OR_SIZE_XYZ_KEY,
          bSourceOrSizeXYZ);
      bSourceBorderXLoHi.updateScriptParameter(scriptCommand);
      bSourceBorderYZLoHi.updateScriptParameter(scriptCommand);
    }
  }

  public void initializeDefaults() {
  }

  /**
   * Sets the nX.
   * @param nX The nX to set
   */
  public void setNX(int nX) {
    numberOfPatchesXYZ.set(X_INDEX, nX);
  }

  /**
   * Sets the nY.
   * @param nY The nY to set
   */
  public void setNY(int nY) {
    numberOfPatchesXYZ.set(Y_INDEX, nY);
  }

  /**
   * Sets the nZ.
   * @param nZ The nZ to set
   */
  public void setNZ(int nZ) {
    numberOfPatchesXYZ.set(Z_INDEX, nZ);
  }

  /**
   * Sets the xHigh.
   * @param xHigh The xHigh to set
   */
  public void setXHigh(int xHigh) {
    xMinAndMax.set(1, xHigh);
  }

  /**
   * Sets the xLow.
   * @param xLow The xLow to set
   */
  public void setXLow(int xLow) {
    xMinAndMax.set(0, xLow);
  }

  /**
   * Sets the xPatchSize.
   * @param xPatchSize The xPatchSize to set
   */
  public void setXPatchSize(int xPatchSize) {
    patchSizeXYZ.set(0, xPatchSize);
  }

  /**
   * Sets the yHigh.
   * @param yHigh The yHigh to set
   */
  public void setYHigh(int yHigh) {
    yMinAndMax.set(1, yHigh);
  }

  /**
   * Sets the yLow.
   * @param yLow The yLow to set
   */
  public void setYLow(int yLow) {
    yMinAndMax.set(0, yLow);
  }

  /**
   * Sets the yPatchSize.
   * @param yPatchSize The yPatchSize to set
   */
  public void setYPatchSize(int yPatchSize) {
    patchSizeXYZ.set(Y_INDEX, yPatchSize);
  }

  /**
   * Sets the zHigh.
   * @param zHigh The zHigh to set
   */
  public void setZHigh(int zHigh) {
    zMinAndMax.set(1, zHigh);
  }

  /**
   * Sets the zLow.
   * @param zLow The zLow to set
   */
  public void setZLow(int zLow) {
    zMinAndMax.set(0, zLow);
  }

  /**
   * Sets the zPatchSize.
   * @param zPatchSize The zPatchSize to set
   */
  public void setZPatchSize(int zPatchSize) {
    patchSizeXYZ.set(Z_INDEX, zPatchSize);
  }

  public void setUseBoundaryModel(boolean useBoundaryModel) {
    if (useBoundaryModel) {
      regionModel = ConstMatchorwarpParam.getDefaultPatchRegionModel();
    }
    else {
      regionModel = "";
    }
  }

  public void setInitialShiftX(String initialShiftX) {
    if (initialShiftX.matches("\\s*")) {
      initialShiftXYZ.setDefault(X_INDEX);
    }
    else {
      initialShiftXYZ.set(X_INDEX, initialShiftX);
    }
  }

  public void setInitialShiftY(String initialShiftY) {
    if (initialShiftY.matches("\\s*")) {
      initialShiftXYZ.setDefault(Y_INDEX);
    }
    else {
      initialShiftXYZ.set(Y_INDEX, initialShiftY);
    }
  }

  public void setInitialShiftZ(String initialShiftZ) {
    if (initialShiftZ.matches("\\s*")) {
      initialShiftXYZ.setDefault(Z_INDEX);
    }
    else {
      initialShiftXYZ.set(Z_INDEX, initialShiftZ);
    }
  }

  public void setKernelSigma(boolean kernelSigmaActive, String kernelSigma) {
    this.kernelSigma.setActive(kernelSigmaActive);
    this.kernelSigma.set(kernelSigma);
  }

  public static String getTitle() {
    return COMMAND.substring(0, 1).toUpperCase() + COMMAND.substring(1);
  }
}
