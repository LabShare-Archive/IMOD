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
 * <p> Revision 2.2  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */
public class SolvematchshiftParam
  extends ConstSolvematchshiftParam
  implements CommandParam {
  public static final String rcsid =
    "$Id$";

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void initialize(ComScriptCommand scriptCommand)
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
    if (inputArgs.length != 8) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to ccderaser command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected 8."));
    }
    int i = 0;
    tofiducialCoordinatesFile = inputArgs[i++].getArgument();
    fromfiducialCoordinatesFile = inputArgs[i++].getArgument();
    fiducialMatchListA.parseString(inputArgs[i++].getArgument());
    fiducialMatchListB.parseString(inputArgs[i++].getArgument());
    xAxistTilt.validateAndSet(inputArgs[i++].getArgument());
    residualThreshold = Double.parseDouble(inputArgs[i++].getArgument());
    nSurfaces = Integer.parseInt(inputArgs[i++].getArgument());
    outputTransformationFile = inputArgs[i++].getArgument();
  }

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#updateComScript(etomo.comscript.ComScriptCommand)
   */
  public void updateComScript(ComScriptCommand scriptCommand)
    throws BadComScriptException {
      //  Check to be sure that it is a solvematch command
      if (!scriptCommand.getCommand().equals("solvematch")) {
        throw (new BadComScriptException("Not a solvematch command"));
      }

      //  Get the input arguments parameters to preserve the comments
      ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
      if (inputArgs.length != 8) {
        throw (
          new BadComScriptException(
            "Incorrect number of input arguments to solvematch command\nGot "
              + String.valueOf(inputArgs.length)
              + " expected 8."));
      }

      //  Fill in the input argument sequence
      inputArgs[0].setArgument(tofiducialCoordinatesFile);
      scriptCommand.setInputArgument(0, inputArgs[0]);
      
      inputArgs[1].setArgument(fromfiducialCoordinatesFile);
      scriptCommand.setInputArgument(1, inputArgs[1]);
      
      inputArgs[2].setArgument(fiducialMatchListA.toString());
      scriptCommand.setInputArgument(3, inputArgs[3]);
      
      inputArgs[3].setArgument(fiducialMatchListA.toString());
      scriptCommand.setInputArgument(3, inputArgs[3]);
      
      inputArgs[4].setArgument(xAxistTilt.toString());
      scriptCommand.setInputArgument(4, inputArgs[4]);
      
      inputArgs[5].setArgument(residualThreshold);
      scriptCommand.setInputArgument(5, inputArgs[5]);
      
      inputArgs[6].setArgument(nSurfaces);
      scriptCommand.setInputArgument(6, inputArgs[6]);
      
      inputArgs[7].setArgument(outputTransformationFile);
      scriptCommand.setInputArgument(7, inputArgs[7]);

  }

  /**
   * Sets the fiducialMatchListA.
   * @param list The fiducialMatchListA to set
   */
  public void setFiducialMatchListA(String list) {
    fiducialMatchListA.parseString(list);
  }

  /**
   * Sets the fiducialMatchListB.
   * @param list The fiducialMatchListB to set
   */
  public void setFiducialMatchListB(String list) {
    fiducialMatchListB.parseString(list);
  }

  /**
   * Sets the fromfiducialCoordinatesFile.
   * @param fromfiducialCoordinatesFile The fromfiducialCoordinatesFile to set
   */
  public void setFromfiducialCoordinatesFile(String fromfiducialCoordinatesFile) {
    this.fromfiducialCoordinatesFile = fromfiducialCoordinatesFile;
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
   * Sets the tofiducialCoordinatesFile.
   * @param tofiducialCoordinatesFile The tofiducialCoordinatesFile to set
   */
  public void setTofiducialCoordinatesFile(String tofiducialCoordinatesFile) {
    this.tofiducialCoordinatesFile = tofiducialCoordinatesFile;
  }

  /**
   * Sets the xAxistTilt.
   * @param xAxistTilt The xAxistTilt to set
   */
  public void setXAxistTilt(FortranInputString xAxistTilt) {
    this.xAxistTilt = xAxistTilt;
  }

}