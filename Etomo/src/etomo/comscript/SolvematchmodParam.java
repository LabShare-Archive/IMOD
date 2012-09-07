package etomo.comscript;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002-2004</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ 
 */

public class SolvematchmodParam
  extends ConstSolvematchmodParam
  implements CommandParam {

  /**
   * 
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws
      BadComScriptException,
      FortranInputSyntaxException,
      InvalidParameterException {
    //  Check to be sure that it is a solvematch command
    if (!scriptCommand.getCommand().equals("solvematch")) {
      throw (new BadComScriptException("Not a solvematch command"));
    }

    //  Extract the parameters
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length != 12) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to ccderaser command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected 12."));
    }
    int i = 0;
    toFiducialCoordinatesFile = inputArgs[i++].getArgument();
    setMatchBToA(toFiducialCoordinatesFile);
    fromFiducialCoordinatesFile = inputArgs[i++].getArgument();
    if (matchBToA) {
      fiducialMatchListA.parseString(inputArgs[i++].getArgument());
      fiducialMatchListB.parseString(inputArgs[i++].getArgument());
    }
    else {
      fiducialMatchListB.parseString(inputArgs[i++].getArgument());
      fiducialMatchListA.parseString(inputArgs[i++].getArgument());
    }
    xAxistTilt.validateAndSet(inputArgs[i++].getArgument());
    residualThreshold = Double.parseDouble(inputArgs[i++].getArgument());
    nSurfaces = Integer.parseInt(inputArgs[i++].getArgument());
    toReconstructionFile = inputArgs[i++].getArgument();
    toMatchingModel = inputArgs[i++].getArgument();
    fromReconstructionFile = inputArgs[i++].getArgument();
    fromMatchingModel = inputArgs[i++].getArgument();
    outputTransformationFile = inputArgs[i++].getArgument();

  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript(etomo.comscript.ComScriptCommand)
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException {
    //  Check to be sure that it is a solvematch command
    if (!scriptCommand.getCommand().equals("solvematch")) {
      throw (new BadComScriptException("Not a solvematch command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length != 12) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to solvematch command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected 12."));
    }
    
    //matchBToA has to be set correctly.  In case parseComScriptCommand() hasn't
    //been called, set is here to.
    setMatchBToA(inputArgs[0].getArgument());
    
    //  Fill in the input argument sequence
    inputArgs[0].setArgument(toFiducialCoordinatesFile);
    scriptCommand.setInputArgument(0, inputArgs[0]);

    inputArgs[1].setArgument(fromFiducialCoordinatesFile);
    scriptCommand.setInputArgument(1, inputArgs[1]);
    if (matchBToA) {
      inputArgs[2].setArgument(fiducialMatchListA.toString());
      scriptCommand.setInputArgument(2, inputArgs[2]);

      inputArgs[3].setArgument(fiducialMatchListB.toString());
      scriptCommand.setInputArgument(3, inputArgs[3]);
    }
    else {
      inputArgs[2].setArgument(fiducialMatchListB.toString());
      scriptCommand.setInputArgument(2, inputArgs[2]);
      
      inputArgs[3].setArgument(fiducialMatchListA.toString());
      scriptCommand.setInputArgument(3, inputArgs[3]);
    }
    inputArgs[4].setArgument(xAxistTilt.toString());
    scriptCommand.setInputArgument(4, inputArgs[4]);

    inputArgs[5].setArgument(residualThreshold);
    scriptCommand.setInputArgument(5, inputArgs[5]);

    inputArgs[6].setArgument(nSurfaces);
    scriptCommand.setInputArgument(6, inputArgs[6]);

    inputArgs[7].setArgument(toReconstructionFile);
    scriptCommand.setInputArgument(7, inputArgs[7]);

    inputArgs[8].setArgument(toMatchingModel);
    scriptCommand.setInputArgument(8, inputArgs[8]);

    inputArgs[9].setArgument(fromReconstructionFile);
    scriptCommand.setInputArgument(9, inputArgs[9]);

    inputArgs[10].setArgument(fromMatchingModel);
    scriptCommand.setInputArgument(10, inputArgs[10]);

    inputArgs[11].setArgument(outputTransformationFile);
    scriptCommand.setInputArgument(11, inputArgs[11]);
  }
  
  protected void setMatchBToA(String toFile) {
    if (toFile == null || toFile.matches("\\s*")) {
      return;
    }
    if (toFile.matches("^\\s*\\S+?afid.xyz\\s*$")) {
      matchBToA = true;
    }
    else if (toFile.matches("^\\s*\\S+?bfid.xyz\\s*$")) {
      matchBToA = false;
    }
    return;
  }

  public void initializeDefaults() {
  }

  /**
   * Sets the fiducialMatchListA.
   * @param fiducialMatchListA The fiducialMatchListA to set
   */
  public void setFiducialMatchListA(String list) {
    fiducialMatchListA.parseString(list);
  }

  /**
   * Sets the fiducialMatchListB.
   * @param fiducialMatchListB The fiducialMatchListB to set
   */
  public void setFiducialMatchListB(String list) {
    fiducialMatchListB.parseString(list);
  }

  /**
   * Sets the fromFiducialCoordinatesFile.
   * @param fromFiducialCoordinatesFile The fromFiducialCoordinatesFile to set
   */
  public void setFromFiducialCoordinatesFile(String fromFiducialCoordinatesFile) {
    this.fromFiducialCoordinatesFile = fromFiducialCoordinatesFile;
  }

  /**
   * Sets the fromMatchingModel.
   * @param fromMatchingModel The fromMatchingModel to set
   */
  public void setFromMatchingModel(String fromMatchingModel) {
    this.fromMatchingModel = fromMatchingModel;
  }

  /**
   * Sets the fromReconstructionFile.
   * @param fromReconstructionFile The fromReconstructionFile to set
   */
  public void setFromReconstructionFile(String fromReconstructionFile) {
    this.fromReconstructionFile = fromReconstructionFile;
  }

  /**
   * Sets the nSurfaces.
   * @param nSurfaces The nSurfaces to set
   */
  public void setNSurfaces(int nSurfaces) {
    this.nSurfaces = nSurfaces;
  }

  /**
   * Sets the outputTransformationFile.
   * @param outputTransformationFile The outputTransformationFile to set
   */
  public void setOutputTransformationFile(String outputTransformationFile) {
    this.outputTransformationFile = outputTransformationFile;
  }

  /**
   * Sets the residualThreshold.
   * @param residualThreshold The residualThreshold to set
   */
  public void setResidualThreshold(double residualThreshold) {
    this.residualThreshold = residualThreshold;
  }

  /**
   * Sets the toFiducialCoordinatesFile.
   * @param toFiducialCoordinatesFile The toFiducialCoordinatesFile to set
   */
  public void setToFiducialCoordinatesFile(String toFiducialCoordinatesFile) {
    this.toFiducialCoordinatesFile = toFiducialCoordinatesFile;
  }

  /**
   * Sets the toMatchingModel.
   * @param toMatchingModel The toMatchingModel to set
   */
  public void setToMatchingModel(String toMatchingModel) {
    this.toMatchingModel = toMatchingModel;
  }

  /**
   * Sets the toReconstructionFile.
   * @param toReconstructionFile The toReconstructionFile to set
   */
  public void setToReconstructionFile(String toReconstructionFile) {
    this.toReconstructionFile = toReconstructionFile;
  }

  /**
   * Sets the xAxistTilt.
   * @param xAxistTilt The xAxistTilt to set
   */
  public void setXAxistTilt(FortranInputString xAxistTilt) {
    this.xAxistTilt = xAxistTilt;
  }

}
