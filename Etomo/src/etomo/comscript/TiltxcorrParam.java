package etomo.comscript;

import java.io.File;
import java.io.IOException;
import java.util.StringTokenizer;

import etomo.BaseManager;
import etomo.type.AxisID;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.StringParameter;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.ui.swing.UIHarness;
import etomo.util.MRCHeader;

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
 * <p> Revision 3.20  2011/05/10 16:49:36  sueh
 * <p> bug# 1482 Changed getSubcommandProcessName to return a string so that the root name chould be set to
 * <p> subcommandProcessName.
 * <p>
 * <p> Revision 3.19  2011/02/24 23:35:30  sueh
 * <p> bug# 1452 Added setRotationAngle.
 * <p>
 * <p> Revision 3.18  2011/02/22 03:34:29  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.17  2010/04/28 16:09:35  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 3.16  2010/03/18 22:42:09  sueh
 * <p> bug# 1311 Don't put a display value in iterateCorrelations since it is not
 * <p> used in xcorr.com (this didn't cause any problems).
 * <p>
 * <p> Revision 3.15  2010/03/08 21:01:34  sueh
 * <p> bug# 1311 Added partialSave to turn off field requirements so the sync
 * <p> between xcorr.com and xcorr_pt.com can be done.
 * <p>
 * <p> Revision 3.14  2010/03/03 04:53:45  sueh
 * <p> bug# 1311 Turned ConstTiltxcorrParam into an interface.
 * <p>
 * <p> Revision 3.13  2009/03/24 20:27:01  sueh
 * <p> bug# 1201 Added angleOffset.
 * <p>
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

public final class TiltxcorrParam implements ConstTiltxcorrParam, CommandParam,
    ConstCommandParam {
  public static final String rcsid = "$Id$";

  public static final String GOTO_LABEL = "doxcorr";
  private static final String COMMAND = "tiltxcorr";
  public static final String SIZE_OF_PATCHES_X_AND_Y_KEY = "SizeOfPatchesXandY";
  public static final String OVERLAP_OF_PATCHES_X_AND_Y_KEY = "OverlapOfPatchesXandY";
  public static final String OVERLAP_OF_PATCHES_X_AND_Y_DEFAULT = ".33,.33";
  public static final String NUMBER_OF_PATCHES_X_AND_Y_KEY = "NumberOfPatchesXandY";
  public static final String ITERATE_CORRELATIONS_KEY = "IterateCorrelations";
  public static final int ITERATE_CORRELATIONS_DEFAULT = 1;
  public static final int ITERATE_CORRELATIONS_MIN = 1;
  public static final int ITERATE_CORRELATIONS_MAX = 4;
  public static final String SHIFT_LIMITS_X_AND_Y_KEY = "ShiftLimitsXandY";
  public static final String LENGTH_AND_OVERLAP_KEY = "LengthAndOverlap";
  public static final String BOUNDARY_MODEL_KEY = "BoundaryModel";
  public static final String FILTER_SIGMA_1_DEFAULT = "0.03";
  public static final String FILTER_RADIUS_2_DEFAULT = "0.25";
  public static final String FILTER_SIGMA_2_DEFAULT = "0.05";
  public static final String SKIP_VIEWS_KEY = "SkipViews";
  public static final String FILTER_RADIUS_2_KEY = "FilterRadius2";
  public static final String FILTER_SIGMA_2_KEY = "FilterSigma2";
  public static final String SEARCH_MAG_CHANGES_KEY = "SearchMagChanges";
  public static final String VIEWS_WITH_MAG_CHANGES_KEY = "ViewsWithMagChanges";
  public static final String FILTER_SIGMA1_KEY = "FilterSigma1";

  // PIP and sequential input
  private String inputFile;
  private String pieceListFile;
  private String outputFile;
  private boolean excludeCentralPeak;
  private double rotationAngle; // was imageRotation
  private final FortranInputString bordersInXandY; // was trim
  private final FortranInputString xMinAndMax;
  private final FortranInputString yMinAndMax;
  private final FortranInputString padsInXandY; // was padPercent;
  private final FortranInputString tapersInXandY; // was taperPercent

  private boolean cumulativeCorrelation;
  private boolean absoluteCosineStretch;
  private boolean noCosineStretch;
  private String testOutput;
  private final FortranInputString startingEndingViews; // was viewRange

  private final AxisID axisID;
  private final BaseManager manager;

  // PIP only
  // was tiltAngleSpec
  private double firstTiltAngle;
  private double tiltIncrement;
  private String tiltFile;
  private double[] tiltAngles;

  // was filterParams
  private double filterRadius1;
  private ScriptParameter filterRadius2 = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      FILTER_RADIUS_2_KEY);
  private ScriptParameter filterSigma1 = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      FILTER_SIGMA1_KEY);
  private ScriptParameter filterSigma2 = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      FILTER_SIGMA_2_KEY);
  private final ScriptParameter angleOffset = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "AngleOffset");

  // sequential input only
  private final TiltAngleSpec tiltAngleSpec;
  private final FortranInputString filterParams;

  // Patch tracking
  private final FortranInputString sizeOfPatchesXandY;
  private final FortranInputString overlapOfPatchesXandY;
  private final FortranInputString numberOfPatchesXandY;
  private final ScriptParameter iterateCorrelations = new ScriptParameter(
      ITERATE_CORRELATIONS_KEY);
  private final FortranInputString shiftLimitsXandY;
  /**
   * @deprecated
   * Read lengthAndOverlap, but do not write it out.
   */
  private final FortranInputString lengthAndOverlap;
  private final StringParameter boundaryModel = new StringParameter(BOUNDARY_MODEL_KEY);
  private final ProcessName processName;
  private final StringParameter skipViews = new StringParameter(SKIP_VIEWS_KEY);
  private final StringParameter prealignmentTransformFile = new StringParameter(
      "PrealignmentTransformFile");
  private ScriptParameter imagesAreBinned = new ScriptParameter("ImagesAreBinned");
  private EtomoBoolean2 searchMagChanges = new EtomoBoolean2(SEARCH_MAG_CHANGES_KEY);
  private final StringParameter viewsWithMagChanges = new StringParameter(
      "ViewsWithMagChanges");

  private boolean partialSave = false;
  private boolean validate = false;

  /**
   * Return a multiline string describing the class attributes.
   */
  public String toString() {
    return "[tiltFile:" + tiltFile + ",iterateCorrelations:" + iterateCorrelations + "]";
  }

  public TiltxcorrParam(final BaseManager manager, final AxisID axisID,
      final ProcessName processName) {
    this.manager = manager;
    this.axisID = axisID;
    this.processName = processName;
    tiltAngleSpec = new TiltAngleSpec();
    filterParams = new FortranInputString(4);
    bordersInXandY = new FortranInputString(2);
    bordersInXandY.setIntegerType(0, true);
    bordersInXandY.setIntegerType(1, true);
    xMinAndMax = new FortranInputString(2);
    xMinAndMax.setIntegerType(0, true);
    xMinAndMax.setIntegerType(1, true);
    yMinAndMax = new FortranInputString(2);
    yMinAndMax.setIntegerType(0, true);
    yMinAndMax.setIntegerType(1, true);
    padsInXandY = new FortranInputString(2);
    padsInXandY.setIntegerType(0, true);
    padsInXandY.setIntegerType(1, true);
    tapersInXandY = new FortranInputString(2);
    tapersInXandY.setIntegerType(0, true);
    tapersInXandY.setIntegerType(1, true);
    startingEndingViews = new FortranInputString(2);
    startingEndingViews.setIntegerType(0, true);
    startingEndingViews.setIntegerType(1, true);
    sizeOfPatchesXandY = new FortranInputString(SIZE_OF_PATCHES_X_AND_Y_KEY, 2);
    sizeOfPatchesXandY.setIntegerType(0, true);
    sizeOfPatchesXandY.setIntegerType(1, true);
    overlapOfPatchesXandY = new FortranInputString(OVERLAP_OF_PATCHES_X_AND_Y_KEY, 2);
    numberOfPatchesXandY = new FortranInputString(NUMBER_OF_PATCHES_X_AND_Y_KEY, 2);
    numberOfPatchesXandY.setIntegerType(0, true);
    numberOfPatchesXandY.setIntegerType(1, true);
    iterateCorrelations.setFloor(ITERATE_CORRELATIONS_MIN);
    iterateCorrelations.setCeiling(ITERATE_CORRELATIONS_MAX);
    shiftLimitsXandY = new FortranInputString(SHIFT_LIMITS_X_AND_Y_KEY, 2);
    shiftLimitsXandY.setIntegerType(0, true);
    shiftLimitsXandY.setIntegerType(1, true);
    lengthAndOverlap = new FortranInputString(LENGTH_AND_OVERLAP_KEY, 2);
    lengthAndOverlap.setIntegerType(0, true);
    lengthAndOverlap.setIntegerType(1, true);
    reset();
  }

  /**
   * Set validate to true to cause validations to happen.
   */
  public void setValidate(final boolean validate) {
    this.validate = validate;
  }

  public void setViewsWithMagChanges(final String input) {
    viewsWithMagChanges.set(input);
  }

  public static String getBordersInXandYDefault(final BaseManager manager,
      final AxisID axisID, final FileType fileType) {
    EtomoNumber bordersInX = new EtomoNumber();
    EtomoNumber bordersInY = new EtomoNumber();
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        fileType.getFileName(manager, axisID), axisID);
    try {
      header.read(manager);
      int x = header.getNColumns();
      if (x == -1) {
        return "";
      }
      bordersInX.set(Math.round((x * .05)));
      int y = header.getNRows();
      if (y == -1) {
        return "";
      }
      bordersInY.set(Math.round((y * .05)));
    }
    catch (IOException e) {
      e.printStackTrace();
      return "";
    }
    catch (etomo.util.InvalidParameterException e) {
      e.printStackTrace();
      return "";
    }
    return bordersInX.toString() + "," + bordersInY.toString();
  }

  private void reset() {
    validate = false;
    inputFile = new String();
    pieceListFile = new String();
    outputFile = new String();
    excludeCentralPeak = false;
    rotationAngle = Double.NaN;
    bordersInXandY.setDefault();
    xMinAndMax.setDefault();
    yMinAndMax.setDefault();
    padsInXandY.setDefault();
    tapersInXandY.setDefault();
    cumulativeCorrelation = false;
    absoluteCosineStretch = false;
    noCosineStretch = false;
    testOutput = new String();
    startingEndingViews.setDefault();
    firstTiltAngle = Double.NaN;
    tiltIncrement = Double.NaN;
    tiltFile = new String();
    tiltAngles = null;
    filterRadius1 = Double.NaN;
    filterRadius2.reset();
    filterSigma1.reset();
    filterSigma2.reset();
    TiltAngleSpec tiltAngleSpec = new TiltAngleSpec();
    filterParams.setDefault();
    angleOffset.reset();
    sizeOfPatchesXandY.setDefault();
    overlapOfPatchesXandY.setDefault();
    numberOfPatchesXandY.setDefault();
    iterateCorrelations.reset();
    shiftLimitsXandY.setDefault();
    lengthAndOverlap.setDefault();
    skipViews.reset();
    prealignmentTransformFile.reset();
    imagesAreBinned.reset();
    searchMagChanges.reset();
  }

  public void resetBoundaryModel() {
    boundaryModel.reset();
  }

  public void resetNumberOfPatchesXandY() {
    numberOfPatchesXandY.setDefault();
  }

  public void resetOverlapOfPatchesXandY() {
    overlapOfPatchesXandY.setDefault();
  }

  public boolean isParseComments() {
    return true;
  }

  public boolean isSearchMagChanges() {
    return searchMagChanges.is();
  }

  public boolean isViewsWithMagChangesNull() {
    return viewsWithMagChanges.isEmpty();
  }

  public boolean isFilterRadius2Set() {
    return !filterRadius2.isNull();
  }

  public boolean isFilterSigma1Set() {
    return !filterSigma1.isNull();
  }

  public boolean isFilterSigma2Set() {
    return !filterSigma2.isNull();
  }

  public String getProcessNameString() {
    return processName.toString();
  }

  public ProcessName getProcessName() {
    return processName;
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the tiltxcorr command
   * and parameters.
   */
  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {

    // get the input arguments from the command
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
        firstTiltAngle = Double.parseDouble(scriptCommand.getValue("FirstTiltAngle"));
      }
      if (scriptCommand.hasKeyword("TiltIncrement")) {
        tiltIncrement = Double.parseDouble(scriptCommand.getValue("TiltIncrement"));
      }
      tiltFile = scriptCommand.getValue("TiltFile");
      StringTokenizer tokens = new StringTokenizer(scriptCommand.getValue("TiltAngles"),
          ",");
      int index = 0;
      while (tokens.hasMoreTokens()) {
        tiltAngles[index++] = Double.parseDouble(tokens.nextToken());
      }
      if (scriptCommand.hasKeyword("RotationAngle")) {
        rotationAngle = Double.parseDouble(scriptCommand.getValue("RotationAngle"));
      }
      if (scriptCommand.hasKeyword("FilterRadius1")) {
        filterRadius1 = Double.parseDouble(scriptCommand.getValue("FilterRadius1"));
      }
      filterRadius2.parse(scriptCommand);
      filterSigma1.parse(scriptCommand);
      filterSigma2.parse(scriptCommand);
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
        startingEndingViews.validateAndSet(scriptCommand.getValue("StartingEndingViews"));
      }
      angleOffset.parse(scriptCommand);
      sizeOfPatchesXandY.validateAndSet(scriptCommand);
      overlapOfPatchesXandY.validateAndSet(scriptCommand);
      numberOfPatchesXandY.validateAndSet(scriptCommand);
      iterateCorrelations.parse(scriptCommand);
      shiftLimitsXandY.validateAndSet(scriptCommand);
      lengthAndOverlap.validateAndSet(scriptCommand);
      boundaryModel.parse(scriptCommand);
      skipViews.parse(scriptCommand);
      prealignmentTransformFile.parse(scriptCommand);
      imagesAreBinned.parse(scriptCommand);
      searchMagChanges.parse(scriptCommand);
      viewsWithMagChanges.parse(scriptCommand);
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
      filterSigma1.set(filterParams.getDouble(0));
      filterSigma2.set(filterParams.getDouble(1));
      filterRadius1 = filterParams.getDouble(2);
      filterRadius2.set(filterParams.getDouble(3));
      excludeCentralPeak = inputArgs[inputLine++].getArgument().matches("\\s*1\\s*");
      bordersInXandY.validateAndSet(inputArgs[inputLine++].getArgument());
      padsInXandY.validateAndSet(inputArgs[inputLine++].getArgument());
      tapersInXandY.validateAndSet(inputArgs[inputLine++].getArgument());
      startingEndingViews.validateAndSet(inputArgs[inputLine++].getArgument());
    }
    catch (FortranInputSyntaxException except) {
      String message = "Parse error in tiltxcorr command, standard input argument: "
          + String.valueOf(inputLine) + "\n" + except.getMessage();
      throw new FortranInputSyntaxException(message, except.getNewString());
    }
    sequentialInputToPip();
  }

  public void setPartialSave(boolean input) {
    partialSave = input;
  }

  /**
   * Update the script command with the current valus of this TiltxcorrParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException {
    // get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getInputArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    // When partialSave is on, don't require any fields.
    boolean required = true && !partialSave;

    // Switch to keyword/value pairs
    scriptCommand.useKeywordValue();
    ParamUtilities.updateScriptParameter(scriptCommand, "InputFile", inputFile, required);
    ParamUtilities.updateScriptParameter(scriptCommand, "PieceListFile", pieceListFile);
    ParamUtilities.updateScriptParameter(scriptCommand, "OutputFile", outputFile);
    ParamUtilities.updateScriptParameter(scriptCommand, "FirstTiltAngle", firstTiltAngle);
    ParamUtilities.updateScriptParameter(scriptCommand, "TiltIncrement", tiltIncrement);
    ParamUtilities.updateScriptParameter(scriptCommand, "TiltFile", tiltFile);
    ParamUtilities.updateScriptParameter(scriptCommand, "TiltAngles", tiltAngles);
    ParamUtilities.updateScriptParameter(scriptCommand, "RotationAngle", rotationAngle);
    ParamUtilities.updateScriptParameter(scriptCommand, "FilterRadius1", filterRadius1);
    filterRadius2.updateComScript(scriptCommand);
    filterSigma1.updateComScript(scriptCommand);
    filterSigma2.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, "ExcludeCentralPeak",
        excludeCentralPeak);
    ParamUtilities.updateScriptParameter(scriptCommand, "BordersInXandY", bordersInXandY);
    ParamUtilities.updateScriptParameter(scriptCommand, "XMinAndMax", xMinAndMax);
    ParamUtilities.updateScriptParameter(scriptCommand, "YMinAndMax", yMinAndMax);
    ParamUtilities.updateScriptParameter(scriptCommand, "PadsInXandY", padsInXandY);
    ParamUtilities.updateScriptParameter(scriptCommand, "TapersInXandY", tapersInXandY);
    ParamUtilities.updateScriptParameter(scriptCommand, "CumulativeCorrelation",
        cumulativeCorrelation);
    ParamUtilities.updateScriptParameter(scriptCommand, "AbsoluteCosineStretch",
        absoluteCosineStretch);
    ParamUtilities.updateScriptParameter(scriptCommand, "NoCosineStretch",
        noCosineStretch);
    ParamUtilities.updateScriptParameter(scriptCommand, "TestOutput", testOutput);
    ParamUtilities.updateScriptParameter(scriptCommand, "StartingEndingViews",
        startingEndingViews);
    angleOffset.updateComScript(scriptCommand);
    sizeOfPatchesXandY.updateScriptParameter(scriptCommand);
    overlapOfPatchesXandY.updateScriptParameter(scriptCommand);
    numberOfPatchesXandY.updateScriptParameter(scriptCommand);
    iterateCorrelations.updateComScript(scriptCommand);
    shiftLimitsXandY.updateScriptParameter(scriptCommand);
    boundaryModel.updateComScript(scriptCommand);
    skipViews.updateComScript(scriptCommand);
    prealignmentTransformFile.updateComScript(scriptCommand);
    imagesAreBinned.updateComScript(scriptCommand);
    searchMagChanges.updateComScript(scriptCommand);
    viewsWithMagChanges.updateComScript(scriptCommand);
  }

  public void initializeDefaults() {
  }

  public void setTiltAngleSpec(final TiltAngleSpec input) {
    tiltAngleSpec.set(input);
    if (tiltAngleSpec.getType() == TiltAngleType.EXTRACT) {
      tiltFile = FileType.RAW_TILT_ANGLES.getFileName(manager, axisID);
    }
  }

  public TiltAngleSpec getTiltAngleSpec() {
    return tiltAngleSpec;
  }

  private void sequentialInputToPip() {
    // LIST is not implemented and EXTRACT isn't used with tiltxcorr
    if (tiltAngleSpec.getType() == TiltAngleType.FILE) {
      tiltFile = tiltAngleSpec.getTiltAngleFilename();
    }
    else if (tiltAngleSpec.getType() == TiltAngleType.RANGE) {
      firstTiltAngle = tiltAngleSpec.getRangeMin();
      tiltIncrement = tiltAngleSpec.getRangeStep();
    }
    filterSigma1.set(filterParams.getDouble(0));
    filterSigma2.set(filterParams.getDouble(1));
    filterRadius1 = filterParams.getDouble(2);
    filterRadius2.set(filterParams.getDouble(3));
  }

  /**
   * Set the input file name
   */
  public void setInputFile(final String inputFile) {
    this.inputFile = inputFile;
  }

  /**
   * Returns error message if invalid.
   * @param input
   * @return
   */
  public String setIterateCorrelations(final Number input) {
    iterateCorrelations.set(input);
    if (!iterateCorrelations.isValid()) {
      return iterateCorrelations.getInvalidReason();
    }
    return null;
  }

  /**
   * Set the piece list file
   */
  public void setPieceListFile(final String pieceListFile) {
    this.pieceListFile = pieceListFile;
  }

  public boolean setSizeOfPatchesXandY(final String input, final String description)
      throws FortranInputSyntaxException {
    sizeOfPatchesXandY.validateAndSet(input);
    if (validate) {
      if (!sizeOfPatchesXandY.isNull(0) && sizeOfPatchesXandY.isNull(1)) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Two values are required for "
            + description + ".", "Entry Error", axisID);
        return false;
      }
    }
    return true;
  }

  /**
   * Set the output file
   */
  public void setOutputFile(final String outputFile) {
    this.outputFile = outputFile;
  }

  public void setOverlapOfPatchesXandY(final String input)
      throws FortranInputSyntaxException {
    overlapOfPatchesXandY.validateAndSet(input);
  }

  public void setFilterRadius1(final String filterRadius1) {
    this.filterRadius1 = ParamUtilities.parseDouble(filterRadius1);
  }

  public void setFilterRadius2(final String filterRadius2) {
    this.filterRadius2.set(filterRadius2);
  }

  public void setFilterSigma1(final String filterSigma1) {
    this.filterSigma1.set(filterSigma1);
  }

  public void setFilterSigma2(final String filterSigma2) {
    this.filterSigma2.set(filterSigma2);
  }

  /**
   * Set the borders in x and y.
   */
  public void setBordersInXandY(final String bordersInXandY)
      throws FortranInputSyntaxException {
    ParamUtilities.set(bordersInXandY, this.bordersInXandY);
  }

  public void setBoundaryModel(final String input) {
    boundaryModel.set(input);
  }

  public void setXMin(final String xMin) {
    ParamUtilities.set(xMin, xMinAndMax, 0);
  }

  public void setXMax(final String xMax) {
    ParamUtilities.set(xMax, xMinAndMax, 1);
  }

  public void setYMin(final String yMin) {
    ParamUtilities.set(yMin, yMinAndMax, 0);
  }

  public void setYMax(final String yMax) {
    ParamUtilities.set(yMax, yMinAndMax, 1);
  }

  /**
   * Set the pads in x and y.
   */
  public void setPadsInXandY(final String padsInXandY) throws FortranInputSyntaxException {
    ParamUtilities.set(padsInXandY, this.padsInXandY);
  }

  public void setShiftLimitsXandY(final String input) throws FortranInputSyntaxException {
    shiftLimitsXandY.validateAndSet(input);
  }

  /**
   * Set the taper percentage.
   */
  public void setTapersInXandY(final String tapersInXandY)
      throws FortranInputSyntaxException {
    ParamUtilities.set(tapersInXandY, this.tapersInXandY);
  }

  public void setCumulativeCorrelation(final boolean cumulativeCorrelation) {
    this.cumulativeCorrelation = cumulativeCorrelation;
  }

  public void setAbsoluteCosineStretch(final boolean absoluteCosineStretch) {
    this.absoluteCosineStretch = absoluteCosineStretch;
  }

  public void setNoCosineStretch(final boolean noCosineStretch) {
    this.noCosineStretch = noCosineStretch;
  }

  public void setNumberOfPatchesXandY(final String input)
      throws FortranInputSyntaxException {
    numberOfPatchesXandY.validateAndSet(input);
  }

  /**
   * Set the range of view to process.
   */
  public void setStartingEndingViews(final String startingEndingViews)
      throws FortranInputSyntaxException {
    ParamUtilities.set(startingEndingViews, this.startingEndingViews);
  }

  /**
   * Set/unset the exclude central peak flag.
   */
  public void setExcludeCentralPeak(final boolean excludeCentralPeak) {
    this.excludeCentralPeak = excludeCentralPeak;
  }

  public void setTestOutput(final String testOutput) {
    this.testOutput = new String(testOutput);
  }

  public void setAngleOffset(final String input) {
    angleOffset.set(input);
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command
   */
  private ComScriptInputArg[] getInputArguments(final ComScriptCommand scriptCommand)
      throws BadComScriptException {

    // Check to be sure that it is a tiltxcorr xommand
    if (!scriptCommand.getCommand().equals("tiltxcorr")) {
      throw (new BadComScriptException("Not a tiltxcorr command"));
    }

    // Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    return inputArgs;
  }

  public String getInputFile() {
    return inputFile;
  }

  public int getIterateCorrelations() {
    if (!iterateCorrelations.isNull() && iterateCorrelations.isValid()) {
      return iterateCorrelations.getInt();
    }
    return ITERATE_CORRELATIONS_DEFAULT;
  }

  /**
   * @deprecated
   */
  public String getLengthFromLengthAndOverlap() {
    return lengthAndOverlap.toString(0, true);
  }

  /**
   * @deprecated
   */
  public String getOverlapFromLengthAndOverlap() {
    return lengthAndOverlap.toString(1, true);
  }

  public String getNumberOfPatchesXandY() {
    return numberOfPatchesXandY.toString(true);
  }

  public String getPieceListFile() {
    return pieceListFile;
  }

  public String getOutputFile() {
    return outputFile;
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public String getOverlapOfPatchesXandY() {
    return overlapOfPatchesXandY.toString(true);
  }

  public String getFirstTiltAngleString() {
    return ParamUtilities.valueOf(firstTiltAngle);
  }

  public String getTiltIncrementString() {
    return ParamUtilities.valueOf(tiltIncrement);
  }

  public String getViewsWithMagChanges() {
    return viewsWithMagChanges.toString();
  }

  public String getTiltFile() {
    return tiltFile;
  }

  public String[] getTiltAnglesString() {
    return ParamUtilities.valueOf(tiltAngles);
  }

  public String getRotationAngleString() {
    return ParamUtilities.valueOf(rotationAngle);
  }

  public void setRotationAngle(final double input) {
    rotationAngle = input;
  }

  public void setSearchMagChanges(final boolean input) {
    searchMagChanges.set(input);
  }

  public String getShiftLimitsXandY() {
    return shiftLimitsXandY.toString(true);
  }

  public String getSizeOfPatchesXandY() {
    return sizeOfPatchesXandY.toString(true);
  }

  public String getFilterRadius1String() {
    return ParamUtilities.valueOf(filterRadius1);
  }

  public String getFilterRadius2String() {
    return filterRadius2.toString();
  }

  public String getFilterSigma1String() {
    return filterSigma1.toString();
  }

  public String getFilterSigma2String() {
    return filterSigma2.toString();
  }

  public String getBordersInXandY() {
    return bordersInXandY.toString(true);
  }

  public String getXMinString() {
    return xMinAndMax.toString(0);
  }

  public String getXMaxString() {
    return xMinAndMax.toString(1);
  }

  public String getYMinString() {
    return yMinAndMax.toString(0);
  }

  public String getYMaxString() {
    return yMinAndMax.toString(1);
  }

  public String getPadsInXandYString() {
    return padsInXandY.toString(true);
  }

  public String getTaperPercentString() {
    return tapersInXandY.toString(true);
  }

  public String getSkipViews() {
    return skipViews.toString();
  }

  public void setSkipViews(final String input) {
    skipViews.set(input);
  }

  public void setPrealignmentTransformFileDefault() {
    prealignmentTransformFile.set(FileType.PRE_XG.getFileName(manager, axisID));
  }

  public void setImagesAreBinned(int input) {
    imagesAreBinned.set(input);
  }

  public boolean isCumulativeCorrelation() {
    return cumulativeCorrelation;
  }

  public boolean isAbsoluteCosineStretch() {
    return absoluteCosineStretch;
  }

  public boolean isBordersInXandYSet() {
    return !bordersInXandY.isDefault();
  }

  public boolean isBoundaryModelSet() {
    return !boundaryModel.isEmpty();
  }

  public boolean isNoCosineStretch() {
    return noCosineStretch;
  }

  public boolean isNumberOfPatchesXandYSet() {
    return !numberOfPatchesXandY.isDefault();
  }

  public boolean isOverlapOfPatchesXandYSet() {
    return !overlapOfPatchesXandY.isDefault();
  }

  public String getStartingEndingViews() {
    return startingEndingViews.toString(true);
  }

  public boolean getExcludeCentralPeak() {
    return excludeCentralPeak;
  }

  public String getTestOutput() {
    return testOutput;
  }

  public String getAngleOffset() {
    return angleOffset.toString();
  }

  public String getCommandName() {
    return COMMAND;
  }

  public String getCommand() {
    return processName.getComscript(axisID);
  }

  public String getCommandLine() {
    return getCommand();
  }

  public String[] getCommandArray() {
    return processName.getComscriptArray(axisID);
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public File getCommandOutputFile() {
    return new File(manager.getPropertyUserDir(), outputFile);
  }

  public File getCommandInputFile() {
    return null;
  }

  public AxisID getAxisID() {
    return axisID;
  }
}
