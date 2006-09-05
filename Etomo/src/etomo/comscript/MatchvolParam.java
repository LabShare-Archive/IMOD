package etomo.comscript;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class MatchvolParam implements CommandParam {
  public static final String rcsid = "$Id$";

  public static final String COMMAND = "matchvol";
  
  private FortranInputString outputSizeXYZ = new FortranInputString("OutputSizeXYZ", 3);

  public MatchvolParam() {
    outputSizeXYZ.setIntegerType(true);
    reset();
  }
  
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    String[] cmdLineArgs = scriptCommand.getCommandLineArgs();
    reset();
    try {
      try {
      outputSizeXYZ.validateAndSet(scriptCommand);
      }
      catch (NumberFormatException except) {
        outputSizeXYZ.setDivider(' ');
        outputSizeXYZ.validateAndSet(scriptCommand);
        outputSizeXYZ.resetDivider();
      }
    }
    catch (NumberFormatException except) {
      outputSizeXYZ.resetDivider();
      throw new BadComScriptException(except.getMessage());
    }
  }

  private void reset() {
    outputSizeXYZ.reset();
    outputSizeXYZ.resetDivider();
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
    if (!command.equals(COMMAND)) {
      throw (new BadComScriptException("Not a " + COMMAND + " command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    return inputArgs;
  }

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
    outputSizeXYZ.updateScriptParameter(scriptCommand);
  }
  
  public void initializeDefaults() {
  }
  
  public void setOutputSizeY(String outputSizeY) {
    outputSizeXYZ.set(1, outputSizeY);
  }
  
  public int getOutputSizeY() {
    return outputSizeXYZ.getInt(1);
  }
}
/**
 * <p> $Log$ </p>
 */
