package etomo.comscript;

import java.util.StringTokenizer;
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
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
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
      inputFile = scriptCommand.getValue("InputFile");
      pieceListFile = scriptCommand.getValue("PieceListFile");
      outputFile = scriptCommand.getValue("OutputFile");
      firstTiltAngle =
        Double.parseDouble(scriptCommand.getValue("FirstTiltAngle"));
      tiltIncrement =
        Double.parseDouble(scriptCommand.getValue("TiltIncrement"));
      tiltFile = scriptCommand.getValue("TiltFile");
      StringTokenizer tokens =
        new StringTokenizer(scriptCommand.getValue("TiltAngles"), ",");
      int index = 0;
      while (tokens.hasMoreTokens()) {
        tiltAngles[index++] = Double.parseDouble(tokens.nextToken());
      }
      rotationAngle =
        Double.parseDouble(scriptCommand.getValue("RotationAngle"));
      filterRadius1 =
        Double.parseDouble(scriptCommand.getValue("FilterRadius1"));
      filterRadius2 =
        Double.parseDouble(scriptCommand.getValue("FilterRadius2"));
      filterSigma1 = Double.parseDouble(scriptCommand.getValue("FilterSigma1"));
      filterSigma2 = Double.parseDouble(scriptCommand.getValue("FilterSigma2"));
      excludeCentralPeak = scriptCommand.hasKeyword("ExcludeCentralPeak");
      bordersInXandY.validateAndSet(scriptCommand.getValue("BordersInXandY"));
      padsInXandY.validateAndSet(scriptCommand.getValue("PadsInXandY"));
      tapersInXandY.validateAndSet(scriptCommand.getValue("TapersInXandY"));
      startingEndingViews.validateAndSet(
        scriptCommand.getValue("StartingEndingViews"));
      return;
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

    rotationAngle = Double.parseDouble(inputArgs[inputLine++].getArgument());
    try {
      filterParams.validateAndSet(inputArgs[inputLine++].getArgument());
      excludeCentralPeak =
        inputArgs[inputLine++].getArgument().matches("\\s*1\\s*");
      bordersInXandY.validateAndSet(inputArgs[inputLine++].getArgument());
      padsInXandY.validateAndSet(inputArgs[inputLine++].getArgument());
      tapersInXandY.validateAndSet(inputArgs[inputLine++].getArgument());
      startingEndingViews.validateAndSet(inputArgs[inputLine++].getArgument());
    }
    catch (FortranInputSyntaxException except) {
      String message =
        "Parse error in tiltxcorr command, standard input argument: "
          + String.valueOf(inputLine)
          + "\n"
          + except.getMessage();
      throw new FortranInputSyntaxException(message, except.getNewString());
    }
    sequentialInputToPip();
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

    scriptCommand.setValue("InputFile", inputFile);

    if (!pieceListFile.equals("")) {
      scriptCommand.setValue("PieceListFile", pieceListFile);
    }
    else {
      scriptCommand.deleteKey("PieceListFile");
    }
    if (!outputFile.equals("")) {
      scriptCommand.setValue("OutputFile", outputFile);
    }
    else {
      scriptCommand.deleteKey("OutputFile");
    }
    if (firstTiltAngle != Double.NaN) {
      scriptCommand.setValue("FirstTiltAngle", String.valueOf(firstTiltAngle));
    }
    else {
      scriptCommand.deleteKey("FirstTiltAngle");
    }
    if (tiltIncrement != Double.NaN) {
      scriptCommand.setValue("TiltIncrement", String.valueOf(tiltIncrement));
    }
    else {
      scriptCommand.deleteKey("TiltIncrement");
    }
    if (!tiltFile.equals("")) {
      scriptCommand.setValue("TiltFile", tiltFile);
    }
    else {
      scriptCommand.deleteKey("TiltFile");
    }
    if (tiltAngles != null && tiltAngles.length > 0) {
      StringBuffer buffer = new StringBuffer();
      for (int index = 0; index < tiltAngles.length; index++) {
        buffer.append(String.valueOf(tiltAngles[index]));
        if (index < tiltAngles.length - 1) {
          buffer.append(",");
        }
      }
      scriptCommand.setValue("TiltAngles", buffer.toString());
    }
    else {
      scriptCommand.deleteKey("TiltAngles");
    }
    if (rotationAngle != Double.NaN) {
      scriptCommand.setValue("RotationAngle", String.valueOf(rotationAngle));
    }
    else {
      scriptCommand.deleteKey("RotationAngle");
    }
    if (filterRadius1 != Double.NaN) {
      scriptCommand.setValue("FilterRadius1", String.valueOf(filterRadius1));
    }
    else {
      scriptCommand.deleteKey("FilterRadius1");
    }
    if (filterRadius2 != Double.NaN) {
      scriptCommand.setValue("FilterRadius2", String.valueOf(filterRadius2));
    }
    else {
      scriptCommand.deleteKey("FilterRadius2");
    }
    if (filterSigma1 != Double.NaN) {
      scriptCommand.setValue("FilterSigma1", String.valueOf(filterSigma1));
    }
    else {
      scriptCommand.deleteKey("FilterSigma1");
    }
    if (filterSigma2 != Double.NaN) {
      scriptCommand.setValue("FilterSigma2", String.valueOf(filterSigma2));
    }
    else {
      scriptCommand.deleteKey("FilterSigma2");
    }
    if (excludeCentralPeak) {
      scriptCommand.setValue("ExcludeCentralPeak", "");
    }
    else {
      scriptCommand.deleteKey("ExcludeCentralPeak");
    }
    if (!bordersInXandY.equals("")) {
      scriptCommand.setValue("BordersInXandY", bordersInXandY.toString());
    }
    else {
      scriptCommand.deleteKey("BordersInXandY");
    }
    if (!padsInXandY.equals("")) {
      scriptCommand.setValue("PadsInXandY", padsInXandY.toString());
    }
    else {
      scriptCommand.deleteKey("PadsInXandY");
    }
    if (!tapersInXandY.equals("")) {
      scriptCommand.setValue("TapersInXandY", tapersInXandY.toString());
    }
    else {
      scriptCommand.deleteKey("TapersInXandY");
    }
    if (!startingEndingViews.equals("")) {
      scriptCommand.setValue(
        "StartingEndingViews",
        startingEndingViews.toString());
    }
    else {
      scriptCommand.deleteKey("StartingEndingViews");
    }
  }

  protected void sequentialInputToPip() {
    //LIST is not implemented and EXTRACT isn't used with tiltxcorr
    if (tiltAngleSpec.getType() == TiltAngleType.FILE) {
      tiltFile = tiltAngleSpec.getTiltAngleFilename();
    }
    else if (tiltAngleSpec.getType() == TiltAngleType.RANGE) {
      firstTiltAngle = tiltAngleSpec.getRangeMin();
      tiltIncrement = tiltAngleSpec.getRangeStep();
    }
    filterSigma1 = filterParams.getDouble(0);
    filterSigma2 = filterParams.getDouble(1);
    filterRadius1 = filterParams.getDouble(2);
    filterRadius2 = filterParams.getDouble(3);
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

  public void setFilterRadius1(double filterRadius1) {
    this.filterRadius1 = filterRadius1;
  }
  public void setFilterRadius2(double filterRadius2) {
    this.filterRadius2 = filterRadius2;
  }
  public void setFilterSigma1(double filterSigma1) {
    this.filterSigma1 = filterSigma1;
  }
  public void setFilterSigma2(double filterSigma2) {
    this.filterSigma2 = filterSigma2;
  }

  /**
   * Set the borders in x and y.
   */
  public void setBordersInXandY(String newBordersInXandYValues)
    throws FortranInputSyntaxException {
    bordersInXandY.validateAndSet(newBordersInXandYValues);
  }

  /**
   * Set the pads in x and y.
   */
  public void setPadsInXandY(String newPadsInXandY)
    throws FortranInputSyntaxException {
    padsInXandY.validateAndSet(newPadsInXandY);
  }

  /**
   * Set the taper percentage.
   */
  public void setTapersInXandY(String newTapersInXandY)
    throws FortranInputSyntaxException {
    tapersInXandY.validateAndSet(newTapersInXandY);
  }

  /**
   * Set the range of view to process.
   */
  public void setStartingEndingViews(String newPair)
    throws FortranInputSyntaxException {
    startingEndingViews.validateAndSet(newPair);
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
      + "Borders In X and Y: "
      + bordersInXandY
      + "\n"
      + "Pads In X and Y: "
      + padsInXandY
      + "\n"
      + "Tapers In X and Y: "
      + tapersInXandY
      + "\n"
      + "Starting Ending Views: "
      + startingEndingViews
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
    return inputArgs;
  }
}
