package etomo.comscript;

import etomo.type.TiltAngleType;

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
 * <p> Revision 2.6  2003/09/09 17:17:30  rickg
 * <p> Changed view list to view range and made it a 2 element integer
 * <p> FortranInputString
 * <p>
 * <p> Revision 2.5  2003/08/07 17:59:06  rickg
 * <p> Merged in tilt angle fix from beta2a branch
 * <p>
 * <p> Revision 2.4  2003/07/25 22:52:14  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.3  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.2.2.1  2003/07/25 22:41:41  rickg
 * <p> Fixed bug in parsing tilt angle specification (created beta2a
 * <p> branch).
 * <p>
 * <p> Revision 2.2  2003/03/20 17:25:05  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/10/17 16:21:16  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class TiltxcorrParam
  extends ConstTiltxcorrParam
  implements CommandParam {
  public static final String rcsid =
    "$Id$";

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the tiltxcorr command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
    throws BadComScriptException, FortranInputSyntaxException {

    //  get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getInputArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    int inputLine = 0;
    inputFile = inputArgs[inputLine++].getArgument();
    pieceListFile = inputArgs[inputLine++].getArgument();
    outputFile = inputArgs[inputLine++].getArgument();

    int typeSpec = Integer.parseInt(inputArgs[inputLine++].getArgument());
    tiltAngleSpec.setType(TiltAngleType.parseInt(typeSpec));
    if (tiltAngleSpec.getType() == TiltAngleType.FILE) {
      tiltAngleSpec.setTiltAngleFilename(inputArgs[inputLine++].getArgument());
    }
    else if (tiltAngleSpec.getType() == TiltAngleType.RANGE) {
      String pair = inputArgs[inputLine++].getArgument();
      String values[] = pair.split(",");
      if (values.length != 2) {
        throw new BadComScriptException("Incorrect tilt angle specification type");
      }
      tiltAngleSpec.setRangeMin(Double.parseDouble(values[0]));
      tiltAngleSpec.setRangeStep(Double.parseDouble(values[1]));
    }
    else if (tiltAngleSpec.getType() == TiltAngleType.LIST) {
      throw new BadComScriptException("Unimplemented tilt angle specification type");
    }
    else {
      throw new BadComScriptException("Incorrect tilt angle specification type");
    }

    imageRotation = Double.parseDouble(inputArgs[inputLine++].getArgument());
    try {
      filterParams.validateAndSet(inputArgs[inputLine++].getArgument());
      excludeCentralPeak =
        inputArgs[inputLine++].getArgument().matches("\\s*1\\s*");
      trim.validateAndSet(inputArgs[inputLine++].getArgument());
      padPercent.validateAndSet(inputArgs[inputLine++].getArgument());
      taperPercent.validateAndSet(inputArgs[inputLine++].getArgument());
      viewRange.validateAndSet(inputArgs[inputLine++].getArgument());
    }
    catch (FortranInputSyntaxException except) {
      String message =
        "Parse error in tiltxcorr command, standard input argument: "
          + String.valueOf(inputLine)
          + "\n"
          + except.getMessage();
      throw new FortranInputSyntaxException(message, except.getNewString());
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

    //  Fill in the input argument sequence
    inputArgs[0].setArgument(inputFile);
    inputArgs[1].setArgument(pieceListFile);
    inputArgs[2].setArgument(outputFile);
    TiltAngleType type = tiltAngleSpec.getType();
    inputArgs[3].setArgument(String.valueOf(type.toInt()));
    inputArgs[4].setArgument(tiltAngleSpec.getTiltAngles());
    inputArgs[5].setArgument(String.valueOf(imageRotation));
    inputArgs[6].setArgument(filterParams.toString());
    if (excludeCentralPeak) {
      inputArgs[7].setArgument("1");
    }
    else {
      inputArgs[7].setArgument("0");
    }
    inputArgs[8].setArgument(trim.toString());
    inputArgs[9].setArgument(padPercent.toString());
    inputArgs[10].setArgument(taperPercent.toString());
    inputArgs[11].setArgument(viewRange);
    scriptCommand.setInputArguments(inputArgs);

  }

  /**
   * Set the input file name
   */
  public void setInputFile(String inputFile) {
    this.inputFile = inputFile;
  }

  /**
   * Set the piece list file
   */
  public void setPieceListFile(String pieceListFile) {
    this.pieceListFile = pieceListFile;
  }

  /**
   * Set the output file
   */
  public void setOutputFile(String outputFile) {
    this.outputFile = outputFile;
  }

  /**
   * Set the filter parameters.
   */
  public void setFilterParams(String newFilterParams)
    throws FortranInputSyntaxException {
    filterParams.validateAndSet(newFilterParams);
  }

  /**
   * Set the trimming parameters.
   */
  public void setTrim(String newTrimValues)
    throws FortranInputSyntaxException {
    trim.validateAndSet(newTrimValues);
  }

  /**
   * Set the padding percentage.
   */
  public void setPadPercent(String newPadPercent)
    throws FortranInputSyntaxException {
    padPercent.validateAndSet(newPadPercent);
  }

  /**
   * Set the taper percentage.
   */
  public void setTaperPercent(String newTaperPercent)
    throws FortranInputSyntaxException {
    taperPercent.validateAndSet(newTaperPercent);
  }

  /**
   * Set the range of view to process.
   */
  public void setViewRange(String newPair) throws FortranInputSyntaxException {
    viewRange.validateAndSet(newPair);
  }

  /**
   * Set/unset the exclude central peak flag.
   */
  public void setExcludeCentralPeak(boolean excludeCentralPeak) {
    this.excludeCentralPeak = excludeCentralPeak;
  }

  /**
   * Return a multiline string describing the class attributes.
   */
  public String toString() {
    return "Input file: "
      + inputFile
      + "\n"
      + "Piece list file: "
      + pieceListFile
      + "\n"
      + "Output file: "
      + outputFile
      + "\n"
      + "Exclude central peak : "
      + String.valueOf(excludeCentralPeak)
      + "\n"
      + "Trim x: "
      + trim
      + "\n"
      + "Trim y: "
      + padPercent
      + "\n"
      + "Pad x: "
      + taperPercent
      + "\n"
      + "Pad y: "
      + viewRange
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
    if (!scriptCommand.getCommand().equals("tiltxcorr")) {
      throw (new BadComScriptException("Not a tiltxcorr command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length != 12) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to tiltxcorr command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected 12."));
    }

    return inputArgs;
  }
}
