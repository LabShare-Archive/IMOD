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
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class CCDEraserParam
  extends ConstCCDEraserParam
  implements CommandParam {
  public static final String rcsid =
    "$Id$";
  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the ccderaser command
   * and parameters.
   */
  public void initialize(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Check to be sure that it is a ccderaser command
    if (!scriptCommand.getCommand().equals("ccderaser")) {
      throw (new BadComScriptException("Not a ccderaser command"));
    }

    //  Extract the parameters
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length != 8) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to ccderaser command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected 8."));
    }
    inputFile = inputArgs[0].getArgument();
    outputFile = inputArgs[1].getArgument();
    modelFile = inputArgs[2].getArgument();
    globalReplacementList = inputArgs[3].getArgument();
    localReplacementList = inputArgs[4].getArgument();
    borderPixels = inputArgs[5].getArgument();
    polynomialOrder = inputArgs[6].getArgument();
    includeAdjacentPoints = inputArgs[7].getArgument().matches("\\s*1\\s*");
  }

  /**
   * Update the script command with the
   */
  public void updateComScript(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Check to be sure that it is a ccderaser xommand
    if (!scriptCommand.getCommand().equals("ccderaser")) {
      throw (new BadComScriptException("Not a ccderaser command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length != 8) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to ccderaser command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected 8."));
    }

    //  Fill in the input argument sequence
    inputArgs[0].setArgument(inputFile);
    scriptCommand.setInputArgument(0, inputArgs[0]);
    inputArgs[1].setArgument(outputFile);
    scriptCommand.setInputArgument(1, inputArgs[1]);
    inputArgs[2].setArgument(modelFile);
    scriptCommand.setInputArgument(3, inputArgs[3]);
    inputArgs[3].setArgument(globalReplacementList);
    scriptCommand.setInputArgument(3, inputArgs[3]);
    inputArgs[4].setArgument(localReplacementList);
    scriptCommand.setInputArgument(4, inputArgs[4]);
    inputArgs[5].setArgument(borderPixels);
    scriptCommand.setInputArgument(5, inputArgs[5]);
    inputArgs[6].setArgument(polynomialOrder);
    scriptCommand.setInputArgument(6, inputArgs[6]);
    if (includeAdjacentPoints) {
      inputArgs[7].setArgument("1");
    }
    else {
      inputArgs[7].setArgument("0");
    }
  }

  public void setInputFile(String inputFile) {
    this.inputFile = inputFile;
  }

  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
  }

  public void setModelFile(String modelFile) {
    this.modelFile = modelFile;
  }

  public void setGlobalReplacementList(String globalReplacementList) {
    this.globalReplacementList = globalReplacementList;
  }

  public void setLocalReplacementList(String localReplacementList) {
    this.localReplacementList = localReplacementList;
  }

  public void setBorderPixels(String borderPixels) {
    this.borderPixels = borderPixels;
  }

  public void setPolynomialOrder(String polynomialOrder) {
    this.polynomialOrder = polynomialOrder;
  }

  public void setIncludeAdjacentPoints(boolean includeAdjacentPoints) {
    this.includeAdjacentPoints = includeAdjacentPoints;
  }
}
