package etomo.comscript;

import java.util.StringTokenizer;

import etomo.BaseManager;
import etomo.type.AxisID;
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
 * <p> Revision 3.12  2005/08/24 22:33:32  sueh
 * <p> bug# 715 Implemented Command to allow param to be checked in
 * <p> postProcess() and errorProcess().
 * <p>
 * <p> Revision 3.11  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.10  2004/06/09 21:18:12  rickg
 * <p> Changed upateParameter method to updateScriptParameter
 * <p>
 * <p> Revision 3.9  2004/05/03 18:02:21  sueh
 * <p> bug# 418 standardizing update parameters, and sets
 * <p>
 * <p> Revision 3.8  2004/04/12 16:51:15  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.7  2004/03/13 02:27:10  sueh
 * <p> bug# 373 Changed updateComScriptCommand() - not saving default
 * <p> FortranInputStrings
 * <p>
 * <p> Revision 3.6  2004/03/12 21:19:24  sueh
 * <p> bug# 373 Changed UpdateComScriptCommand() - removed XMinAndMax,
 * <p> YMinAndMax from comscript when they are blank.
 * <p>
 * <p> Revision 3.5  2004/03/12 21:00:46  sueh
 * <p> bug# 373 Changed parseComScriptCommand() - copied data to tiltFile,
 * <p> firstTiltAngle, tiltIncrement, filterSigma1, filterSigma2, filterRadius1, filterRadius2.
 * <p>
 * <p> Revision 3.4  2004/03/12 20:04:44  sueh
 * <p> bug# 412 added absoluteCosineStretch, cumulativeCorreslation, noCosineStretch,
 * <p> testOutput, xMinAndMax, yMinAndMax
 * <p> corrected parseComScriptCommand() - fixed optional parameters
 * <p> corrected updateComScriptCommand() - incorrect comparisons
 * <p>
 * <p> Revision 3.3  2004/01/30 02:12:10  sueh
 * <p> bug# 373 removing prints and formatting
 * <p>
 * <p> Revision 3.2  2004/01/30 02:09:43  sueh
 * <p> bug# 373 corrected updating comscript and parsing comscript
 * <p>
 * <p> Revision 3.1  2004/01/30 01:28:41  sueh
 * <p> bug# 373 add PIP-style parsing, changed update comscript
 * <p> to PIP, changed functions to match autodoc
 * <p>
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

  public TiltxcorrParam(BaseManager manager, AxisID axisID) {
    super(manager, axisID);
  }
  
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
      if (scriptCommand.hasKeyword("FirstTiltAngle")) {
        firstTiltAngle =
          Double.parseDouble(scriptCommand.getValue("FirstTiltAngle"));
      }
      if (scriptCommand.hasKeyword("TiltIncrement")) {
        tiltIncrement =
          Double.parseDouble(scriptCommand.getValue("TiltIncrement"));
      }
      tiltFile = scriptCommand.getValue("TiltFile");
      StringTokenizer tokens =
        new StringTokenizer(scriptCommand.getValue("TiltAngles"), ",");
      int index = 0;
      while (tokens.hasMoreTokens()) {
        tiltAngles[index++] = Double.parseDouble(tokens.nextToken());
      }
      if (scriptCommand.hasKeyword("RotationAngle")) {
        rotationAngle =
          Double.parseDouble(scriptCommand.getValue("RotationAngle"));
      }
      if (scriptCommand.hasKeyword("FilterRadius1")) {
        filterRadius1 =
          Double.parseDouble(scriptCommand.getValue("FilterRadius1"));
      }
      if (scriptCommand.hasKeyword("FilterRadius2")) {
        filterRadius2 =
          Double.parseDouble(scriptCommand.getValue("FilterRadius2"));
      }
      if (scriptCommand.hasKeyword("FilterSigma1")) {
        filterSigma1 =
          Double.parseDouble(scriptCommand.getValue("FilterSigma1"));
      }
      if (scriptCommand.hasKeyword("FilterSigma2")) {
        filterSigma2 =
          Double.parseDouble(scriptCommand.getValue("FilterSigma2"));
      }
      excludeCentralPeak = scriptCommand.hasKeyword("ExcludeCentralPeak");
      
      if (scriptCommand.hasKeyword("BordersInXandY")) {
        bordersInXandY.validateAndSet(scriptCommand.getValue("BordersInXandY"));
      }
      if (scriptCommand.hasKeyword("XMinAndMax")) {
        xMinAndMax.validateAndSet(scriptCommand.getValue("XMinAndMax"));
      }
      if (scriptCommand.hasKeyword("YMinAndMax")) {
        yMinAndMax.validateAndSet(scriptCommand.getValue("YMinAndMax"));
      }
      if (scriptCommand.hasKeyword("PadsInXandY")) {
        padsInXandY.validateAndSet(scriptCommand.getValue("PadsInXandY"));
      }
      if (scriptCommand.hasKeyword("TapersInXandY")) {
        tapersInXandY.validateAndSet(scriptCommand.getValue("TapersInXandY"));
      }
      cumulativeCorrelation = scriptCommand.hasKeyword("CumulativeCorrelation");
      absoluteCosineStretch = scriptCommand.hasKeyword("AbsoluteCosineStretch");
      noCosineStretch = scriptCommand.hasKeyword("NoCosineStretch");
      if (scriptCommand.hasKeyword("TestOutput")) {
        testOutput = scriptCommand.getValue("TestOutput");
      }
      if (scriptCommand.hasKeyword("StartingEndingViews")) {
        startingEndingViews.validateAndSet(
          scriptCommand.getValue("StartingEndingViews"));
      }
      angleOffset.parse(scriptCommand);
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
      tiltFile = tiltAngleSpec.getTiltAngleFilename();
    }
    else if (tiltAngleSpec.getType() == TiltAngleType.RANGE) {
      String pair = inputArgs[inputLine++].getArgument();
      String values[] = pair.split(",");
      if (values.length != 2) {
        throw new BadComScriptException("Incorrect tilt angle specification type");
      }
      tiltAngleSpec.setRangeMin(Double.parseDouble(values[0]));
      tiltAngleSpec.setRangeStep(Double.parseDouble(values[1]));
      firstTiltAngle = tiltAngleSpec.getRangeMin();
      tiltIncrement = tiltAngleSpec.getRangeStep();
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
      filterSigma1 = filterParams.getDouble(0);
      filterSigma2 = filterParams.getDouble(1);
      filterRadius1 = filterParams.getDouble(2);
      filterRadius2 = filterParams.getDouble(3);
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

    ParamUtilities.updateScriptParameter(scriptCommand, "InputFile", inputFile, true);
    ParamUtilities.updateScriptParameter(scriptCommand, "PieceListFile", pieceListFile);
    ParamUtilities.updateScriptParameter(scriptCommand, "OutputFile", outputFile);
    ParamUtilities.updateScriptParameter(scriptCommand, "FirstTiltAngle", firstTiltAngle);
    ParamUtilities.updateScriptParameter(scriptCommand, "TiltIncrement", tiltIncrement);
    ParamUtilities.updateScriptParameter(scriptCommand, "TiltFile", tiltFile);
    ParamUtilities.updateScriptParameter(scriptCommand, "TiltAngles", tiltAngles);
    ParamUtilities.updateScriptParameter(scriptCommand, "RotationAngle", rotationAngle);
    ParamUtilities.updateScriptParameter(scriptCommand, "FilterRadius1", filterRadius1);
    ParamUtilities.updateScriptParameter(scriptCommand, "FilterRadius2", filterRadius2);
    ParamUtilities.updateScriptParameter(scriptCommand, "FilterSigma1", filterSigma1);
    ParamUtilities.updateScriptParameter(scriptCommand, "FilterSigma2", filterSigma2);
    ParamUtilities.updateScriptParameter(scriptCommand, "ExcludeCentralPeak", excludeCentralPeak);
    ParamUtilities.updateScriptParameter(scriptCommand, "BordersInXandY", bordersInXandY);
    ParamUtilities.updateScriptParameter(scriptCommand, "XMinAndMax", xMinAndMax);
    ParamUtilities.updateScriptParameter(scriptCommand, "YMinAndMax", yMinAndMax);
    ParamUtilities.updateScriptParameter(scriptCommand, "PadsInXandY", padsInXandY);
    ParamUtilities.updateScriptParameter(scriptCommand, "TapersInXandY", tapersInXandY);
    ParamUtilities.updateScriptParameter(scriptCommand, "CumulativeCorrelation", cumulativeCorrelation);
    ParamUtilities.updateScriptParameter(scriptCommand, "AbsoluteCosineStretch", absoluteCosineStretch);
    ParamUtilities.updateScriptParameter(scriptCommand, "NoCosineStretch", noCosineStretch);
    ParamUtilities.updateScriptParameter(scriptCommand, "TestOutput", testOutput);
    ParamUtilities.updateScriptParameter(scriptCommand, "StartingEndingViews", startingEndingViews);
    angleOffset.updateComScript(scriptCommand);
  }
  
  public void initializeDefaults() {
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

  public void setFilterRadius1(String filterRadius1) {
    this.filterRadius1 = ParamUtilities.parseDouble(filterRadius1);
  }
  public void setFilterRadius2(String filterRadius2) {
    this.filterRadius2 = ParamUtilities.parseDouble(filterRadius2);
  }
  public void setFilterSigma1(String filterSigma1) {
    this.filterSigma1 = ParamUtilities.parseDouble(filterSigma1);
  }
  public void setFilterSigma2(String filterSigma2) {
    this.filterSigma2 = ParamUtilities.parseDouble(filterSigma2);
  }

  /**
   * Set the borders in x and y.
   */
  public void setBordersInXandY(String bordersInXandY)
    throws FortranInputSyntaxException {
    ParamUtilities.set(bordersInXandY, this.bordersInXandY);
  }
  
  public void setXMin(String xMin) {
    ParamUtilities.set(xMin, xMinAndMax, 0);
  }
  
  public void setXMax(String xMax) {
    ParamUtilities.set(xMax, xMinAndMax, 1);
  }

  public void setYMin(String yMin) {
    ParamUtilities.set(yMin, yMinAndMax, 0);
  }
  
  public void setYMax(String yMax) {
    ParamUtilities.set(yMax, yMinAndMax, 1);
  }
  
  /**
   * Set the pads in x and y.
   */
  public void setPadsInXandY(String padsInXandY)
    throws FortranInputSyntaxException {
    ParamUtilities.set(padsInXandY, this.padsInXandY);
  }

  /**
   * Set the taper percentage.
   */
  public void setTapersInXandY(String tapersInXandY)
    throws FortranInputSyntaxException {
    ParamUtilities.set(tapersInXandY, this.tapersInXandY);
  }
  
  public void setCumulativeCorrelation(boolean cumulativeCorrelation) {
    this.cumulativeCorrelation = cumulativeCorrelation;
  }

  public void setAbsoluteCosineStretch(boolean absoluteCosineStretch) {
    this.absoluteCosineStretch = absoluteCosineStretch;
  }

  public void setNoCosineStretch(boolean noCosineStretch) {
    this.noCosineStretch = noCosineStretch;
  }
  /**
   * Set the range of view to process.
   */
  public void setStartingEndingViews(String startingEndingViews)
    throws FortranInputSyntaxException {
    ParamUtilities.set(startingEndingViews, this.startingEndingViews);
  }

  /**
   * Set/unset the exclude central peak flag.
   */
  public void setExcludeCentralPeak(boolean excludeCentralPeak) {
    this.excludeCentralPeak = excludeCentralPeak;
  }
  
  public void setTestOutput(String testOutput) {
    this.testOutput = new String(testOutput);
  }
  
  public void setAngleOffset(String input) {
    angleOffset.set(input);
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
