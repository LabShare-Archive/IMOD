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
 * <p> Revision 3.4  2005/11/14 21:22:51  sueh
 * <p> removed extra ;'s.
 * <p>
 * <p> Revision 3.3  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.2  2004/05/14 00:51:50  sueh
 * <p> bug# 434 set matchBToA from toFiducialCoordinatesFile.
 * <p> Set matchBToA in both parse and update when toFiducialCoordinatesFile
 * <p> is available.  If update is called before parse, matchBToA is
 * <p> correct.  If matchBToA, load the first fiducial list into A, otherwise
 * <p> load it into B.  Also fixed a problem where the first fiducial list
 * <p> wasn't being updated.
 * <p>
 * <p> Revision 3.1  2004/04/12 16:50:47  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.7  2003/07/25 22:54:14  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.6  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.5  2003/05/13 22:40:04  rickg
 * <p> Changed error string
 * <p>
 * <p> Revision 2.4  2003/03/18 00:32:33  rickg
 * <p> combine development in progress
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
public class SolvematchshiftParam extends ConstSolvematchshiftParam implements
    CommandParam {
  public static final String rcsid = "$Id$";

  /* (non-Javadoc)
   * @see etomo.comscript.CommandParam#initialize(etomo.comscript.ComScriptCommand)
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    //  Check to be sure that it is a solvematch command
    if (!scriptCommand.getCommand().equals("solvematch")) {
      throw (new BadComScriptException("Not a solvematch command"));
    }

    //  Extract the parameters
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length != 8) {
      throw (new BadComScriptException(
          "Incorrect number of input arguments to solvematch command\nGot "
              + String.valueOf(inputArgs.length) + " expected 8."));
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
    if (inputArgs.length != 8) {
      throw (new BadComScriptException(
          "Incorrect number of input arguments to solvematch command\nGot "
              + String.valueOf(inputArgs.length) + " expected 8."));
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

    inputArgs[7].setArgument(outputTransformationFile);
    scriptCommand.setInputArgument(7, inputArgs[7]);

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
   * Sets the fromFiducialCoordinatesFile.
   * @param fromFiducialCoordinatesFile The fromFiducialCoordinatesFile to set
   */
  public void setFromFiducialCoordinatesFile(String fromFiducialCoordinatesFile) {
    this.fromFiducialCoordinatesFile = fromFiducialCoordinatesFile;
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
   * Sets the xAxistTilt.
   * @param xAxistTilt The xAxistTilt to set
   */
  public void setXAxistTilt(FortranInputString xAxistTilt) {
    this.xAxistTilt = xAxistTilt;
  }

}