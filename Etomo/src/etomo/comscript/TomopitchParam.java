package etomo.comscript;

import java.util.Vector;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * Univeristy of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log$
 * <p> $Revision 1.4  2004/04/30 16:20:44  sueh
 * <p> $bug# 427 changing comment
 * <p> $
 * <p> $Revision 1.3  2004/04/29 20:25:01  sueh
 * <p> $bug# 427 used statics to avoid typos, added ParameterFile to
 * <p> $updateComSCriptCommand, moved toString() to const
 * <p> $
 * <p> $Revision 1.2  2004/04/27 00:54:26  sueh
 * <p> $bug# 427 added reset model files, so models can be changed
 * <p> $fixing model files, making model files required
 * <p> $
 * <p> $Revision 1.1  2004/04/26 21:19:05  sueh
 * <p> $bug# 427
 * <p> $$ </p>
 */

public class TomopitchParam
  extends ConstTomopitchParam
  implements CommandParam {
  public static final String rcsid =
    "$$Id$$";

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the tomopitch command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws
      BadComScriptException,
      FortranInputSyntaxException,
      InvalidParameterException {

    //  get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getInputArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    if (scriptCommand.isKeywordValuePairs()) {
      if (scriptCommand.hasKeyword(MODEL_FILE)) {
        String[] modelFiles = scriptCommand.getValues(MODEL_FILE);
        for (int i = 0; i < modelFiles.length; i++) {
          this.modelFiles.add(modelFiles[i]);
        }
      }
      if (scriptCommand.hasKeyword(EXTRA_THICKNESS)) {
        extraThickness =
          Double.parseDouble(scriptCommand.getValue(EXTRA_THICKNESS));    
      }
      if (scriptCommand.hasKeyword(SPACING_IN_Y)) {
        spacingInY = Double.parseDouble(scriptCommand.getValue(SPACING_IN_Y));
      }
      if (scriptCommand.hasKeyword(SCALE_FACTOR)) {
        scaleFactor = Double.parseDouble(scriptCommand.getValue(SCALE_FACTOR));
      }
      if (scriptCommand.hasKeyword(PARAMETER_FILE)) {
        parameterFile = scriptCommand.getValue(PARAMETER_FILE);
      } 
      return;
    }
    int inputLine = 0;
    extraThickness = Double.parseDouble(inputArgs[inputLine++].getArgument());
    spacingInY = Double.parseDouble(inputArgs[inputLine++].getArgument());
    int numberOfModelFiles = Integer.parseInt(inputArgs[inputLine++].getArgument());
    for (int i = 0; i < numberOfModelFiles; i++) {
      modelFiles.add(inputArgs[inputLine++].getArgument());
    }
  }

  /**
   * Update the script command with the current valus of this TomopitchParam
   * object
   * @param scriptCommand the script command to be updated
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
    ParamUtilities.updateParameterStrings(
      scriptCommand,
      MODEL_FILE,
      modelFiles,
      true);
    ParamUtilities.updateParameter(
      scriptCommand,
      EXTRA_THICKNESS,
      extraThickness);
    ParamUtilities.updateParameter(scriptCommand, SPACING_IN_Y, spacingInY);
    ParamUtilities.updateParameter(scriptCommand, SCALE_FACTOR, scaleFactor);
    ParamUtilities.updateParameter(
      scriptCommand,
      PARAMETER_FILE,
      parameterFile);
  }
  
  public void initializeDefaults() {
  }

  public void resetModelFiles() {
    modelFiles = new Vector();
  }
  public void setModelFile(String modelFile) {
    modelFiles.add(modelFile);
  }
  public void setExtraThickness(String extraThickness) {
    this.extraThickness = ParamUtilities.getDouble(extraThickness);
  }
  public void setSpacingInY(String spacingInY) {
    this.spacingInY = ParamUtilities.getDouble(spacingInY);
  }
  public void setScaleFactor(double scaleFactor) {
    this.scaleFactor = scaleFactor;
  }
  public void setParameterFile(String parameterFile) {
    this.parameterFile = parameterFile;
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command
   */
  private ComScriptInputArg[] getInputArguments(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Check to be sure that it is a tiltxcorr xommand
    if (!scriptCommand.getCommand().equals(COMMAND)) {
      throw (new BadComScriptException("Not a " + COMMAND + " command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    return inputArgs;
  }
}
