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
      if (scriptCommand.hasKeyword("ModelFile")) {
        String[] modelFiles = scriptCommand.getValues("ModelFile");
        for (int i = 0; i < modelFiles.length; i++) {
          this.modelFiles.add(modelFiles[i]);
        }
      }
      if (scriptCommand.hasKeyword("ExtraThickness")) {
        extraThickness =
          Double.parseDouble(scriptCommand.getValue("ExtraThickness"));    
      }
      if (scriptCommand.hasKeyword("SpacingInY")) {
        spacingInY = Double.parseDouble(scriptCommand.getValue("SpacingInY"));
      }
      if (scriptCommand.hasKeyword("ScaleFactor")) {
        scaleFactor = Double.parseDouble(scriptCommand.getValue("ScaleFactor"));
      }
      if (scriptCommand.hasKeyword("ParameterFile")) {
        parameterFile = scriptCommand.getValue("ParameterFile");
      } 
      return;
    }
    int inputLine = 0;
    extraThickness = Double.parseDouble(inputArgs[inputLine++].getArgument());
    spacingInY = Double.parseDouble(inputArgs[inputLine++].getArgument());
    numberOfModelFiles = Integer.parseInt(inputArgs[inputLine++].getArgument());
    for (int i = 0; i < numberOfModelFiles; i++) {
      modelFiles.add(inputArgs[inputLine++].getArgument());
    }
  }

  /**
   * Update the script command with the current valus of this TiltxcorrParam
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
      "ModelFile",
      modelFiles,
      true);
    ParamUtilities.updateParameter(
      scriptCommand,
      "ExtraThickness",
      extraThickness);
    if (!Double.isNaN(spacingInY)) {
      scriptCommand.setValue("SpacingInY", Double.toString(spacingInY));
    }
    else {
      scriptCommand.deleteKey("SpacingInY");
    }
    if (!Double.isNaN(scaleFactor)) {
      scriptCommand.setValue("ScaleFactor", Double.toString(scaleFactor));
    }
    else {
      scriptCommand.deleteKey("ScaleFactor");
    }
    if (!Double.isNaN(scaleFactor)) {
      scriptCommand.setValue("ScaleFactor", Double.toString(scaleFactor));
    }
    else {
      scriptCommand.deleteKey("ScaleFactor");
    }
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
    this.extraThickness = ParamUtilities.setDouble(extraThickness);
  }
  public void setSpacingInY(String spacingInY) {
    this.spacingInY = ParamUtilities.setDouble(spacingInY);
  }
  public void setScaleFactor(double scaleFactor) {
    this.scaleFactor = scaleFactor;
  }
  public void setParameterFile(String parameterFile) {
    this.parameterFile = parameterFile;
  }
  /**
   * Return a multiline string describing the class attributes.
   */
  public String toString() {
    StringBuffer modelFileString = new StringBuffer();
    for (int i = 0; i < modelFiles.size(); i++) {
      if (i != 0) {
        modelFileString.append(",");
      }
      modelFileString.append(modelFiles.get(i));
    }
    return "Model files: "
      + modelFileString
      + "\n"
      + "Extra Thickness: "
      + extraThickness
      + "\n"
      + "Space in Y: "
      + spacingInY
      + "\n"
      + "Scale Factor: "
      + scaleFactor
      + "\n"
      + "Parameter file: "
      + parameterFile
      + "\n"
      + "Number of Model Files (old-style comscript): "
      + Integer.toString(numberOfModelFiles)
      + "\n";
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command
   */
  private ComScriptInputArg[] getInputArguments(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Check to be sure that it is a tiltxcorr xommand
    if (!scriptCommand.getCommand().equals("tomopitch")) {
      throw (new BadComScriptException("Not a tomopitch command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    return inputArgs;
  }
}
