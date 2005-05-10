package etomo.comscript;

import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.InvalidEtomoNumberException;
import etomo.type.ScriptParameter;

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
 * <p> Revision 3.3  2005/05/09 22:42:20  sueh
 * <p> bug# 658 Moved the old BeadtrackParam to OldBeadtrackParam.  The
 * <p> new version of BeadtrackParam inherits OldBeadtrackParam and can
 * <p> read from the most recent pre-PIP track.com files also well as from
 * <p> current ones.  Writes the most recent version version of track.com.
 * <p> Uses the member variables and get and set functions from
 * <p> OldBeadtrackParam and OldConstBeadtrackParam except where a
 * <p> replacement member variable is needed.  Uses the OldBeadtrackParam
 * <p> parse function to read pre-PIP scripts.  Contains static strings to translate
 * <p> between old member variables and the PIP script.  Replacing the old
 * <p> member variables which are int and double to better handle null values.
 * <p> Replacing old member variables that are incorrect.  Added convertToPIP()
 * <p> and set(OldConstBeadtrackParam) to read in pre-PIP com scripts.  Added
 * <p> a new updateComScript() function.  Added gets and sets where
 * <p> necessary.
 * <p>
 * <p> Revision 3.2  2005/01/13 00:41:42  sueh
 * <p> bug# 576 Converted tiltAngleGroups and magnificationGroups to
 * <p> FortranInputString[].
 * <p>
 * <p> Revision 3.1  2004/04/12 16:48:32  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/08/07 17:59:06  rickg
 * <p> Merged in tilt angle fix from beta2a branch
 * <p>
 * <p> Revision 2.4  2003/07/25 22:46:57  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.3  2003/06/25 22:16:29  rickg
 * <p> changed name of com script parse method to parseComScript
 * <p>
 * <p> Revision 2.2  2003/03/20 17:21:07  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/10/07 22:22:11  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class BeadtrackParam extends OldBeadtrackParam
  implements CommandParam {
  public static final String rcsid =
    "$Id$";

  //Const code
  
  protected static final String INPUT_FILE_KEY = "ImageFile";
  protected static final String PIECE_LIST_FILE_KEY = "PieceListFile";
  protected static final String SEED_MODEL_FILE_KEY = "InputSeedModel";
  protected static final String OUTPUT_MODEL_FILE_KEY = "OutputModel";
  protected static final String TILT_ANGLE_GROUPS_KEY = "TiltNondefaultGroup";
  protected static final String MAGNIFICATION_GROUPS_KEY = "MagNondefaultGroup";
  protected static final String N_MIN_VIEWS_KEY = "MinViewsForTiltalign";
  protected static final String FILL_GAPS_KEY = "FillGaps";
  protected static final String SEARCH_BOX_PIXELS_KEY = "BoxSizeXandY";
  protected static final String FIDUCIAL_EXTRAPOLATION_PARAMS_KEY = "PointsToFitMaxAndMin";
  protected static final String RESCUE_ATTEMPT_PARAMS_KEY = "DensityRescueFractionAndSD";
  protected static final String RESCUE_RELAXATION_PARAMS_KEY = "RescueRelaxationDensityAndDistance";
  protected static final String MEAN_RESID_CHANGE_LIMITS_KEY = "ResidualsToAnalyzeMaxAndMin";
  protected static final String DELETION_PARAMS_KEY ="DeletionCriterionMinAndSD";
  
  private boolean initialized = false;
  
  protected StringList skipViews;//was viewSkipList
  protected ScriptParameter rotationAngle;//was imageRotation
  protected ScriptParameter tiltDefaultGrouping;//was tiltAngleGroupParams
  protected ScriptParameter magDefaultGrouping;//was magnificationGroupParams
  protected ScriptParameter minViewsForTiltalign;//was nMinViews
  protected ScriptParameter centroidRadius;//was fiducialParams(0)
  protected EtomoBoolean2 lightBeads;//was fiducialParams(1)
  protected ScriptParameter maxGapSize;//was maxGap
  protected ScriptParameter minTiltRangeToFindAxis;//was tiltAngleMinRange(0)
  protected ScriptParameter minTiltRangeToFindAngles;//was tiltAngleMinRange(1)
  protected ScriptParameter maxBeadsToAverage;//was maxFiducialsAvg
  protected ScriptParameter distanceRescueCriterion;//was minRescueDistance
  protected ScriptParameter postFitRescueResidual;//was residualDistanceLimit
  protected ScriptParameter densityRelaxationPostFit;//was secondPassParams(0)
  protected ScriptParameter maxRescueDistance;//was secondPassParams(1)
  
  protected void initialize() {
    if (initialized) {
      reset();
    }
    initialized = true;
    skipViews = new StringList("SkipViews");
    rotationAngle = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        "RotationAngle");
    additionalViewGroups.setKey("SeparateGroup");
    
    tiltAngleSpec.setRangeMinKey("FirstTiltAngle", "first");
    tiltAngleSpec.setRangeStepKey("TiltIncrement", "increment");
    tiltAngleSpec.setTiltAngleFilenameKey("TiltFile", "tiltfile");
    tiltAngleSpec.setTiltAnglesKey("TiltAngles", "angles");
    
    tiltDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        "TiltDefaultGrouping");
    tiltDefaultGrouping.setNullIsValid(false).setRecommendValue(7);
    
    magDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        "MagDefaultGrouping");
    minViewsForTiltalign = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        "MinViewsForTiltalign");
    
    centroidRadius = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        "CentroidRadius");
    centroidRadius.setNullIsValid(false).setRecommendValue(3.98);
    
    lightBeads = new EtomoBoolean2("LightBeads");
    
    maxGapSize = new ScriptParameter(EtomoNumber.INTEGER_TYPE, "MaxGapSize");
    maxGapSize.setDefault(5).useDefaultAsDisplayValue();
    
    minTiltRangeToFindAxis = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        "MinTiltRangeToFindAxis");
    minTiltRangeToFindAxis.setDefault(10).useDefaultAsDisplayValue();
    
    minTiltRangeToFindAngles = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        "MinTiltRangeToFindAngles");
    minTiltRangeToFindAngles.setDefault(20).useDefaultAsDisplayValue();
    
    maxBeadsToAverage = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        "MaxBeadsToAverage");
    maxBeadsToAverage.setDefault(4).useDefaultAsDisplayValue();
    
    rescueAttemptParams.setIntegerType(1, false);
    distanceRescueCriterion = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        "DistanceRescueCriterion");
    postFitRescueResidual = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        "PostFitRescueResidual");
    densityRelaxationPostFit = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        "DensityRelaxationPostFit");
    maxRescueDistance = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        "MaxRescueDistance");
    deletionParams.setIntegerType(1, false);
  }
  
  void reset() {
    if (!initialized) {
      initialize();
    }
    inputFile = null;
    pieceListFile = null;
    seedModelFile = null;
    outputModelFile = null;
    skipViews.reset();
    rotationAngle.reset();
    additionalViewGroups.reset();
    tiltAngleSpec.reset();
    
    tiltDefaultGrouping.reset();
    tiltAngleGroups = null;
    magDefaultGrouping.reset();
    magnificationGroups = null;
    minViewsForTiltalign.reset();
    centroidRadius.reset();
    lightBeads.reset();
    fillGaps = true;
    maxGapSize.reset();
    minTiltRangeToFindAxis.reset();
    minTiltRangeToFindAngles.reset();
    searchBoxPixels.reset();
    maxBeadsToAverage.reset();
    
    fiducialExtrapolationParams.set(0, 7);
    fiducialExtrapolationParams.set(1, 3);

    rescueAttemptParams.reset();
    distanceRescueCriterion.reset();
    rescueRelaxationParams.reset();
    postFitRescueResidual.reset();
    densityRelaxationPostFit.reset();
    maxRescueDistance.reset();
    
    meanResidChangeLimits.set(0, 9);
    meanResidChangeLimits.set(1, 5);
    deletionParams.reset();
  }
  
  public String getSkipViews() {
    return skipViews.toString();
  }
  
  public ConstEtomoNumber getRotationAngle() {
    return rotationAngle;
  }
  
  /**
   * @return
   */
  public ConstEtomoNumber getTiltDefaultGrouping() {
    return tiltDefaultGrouping;
  }
  
  public int getMagnificationGroupSize() {
    return magDefaultGrouping.getInteger();
  }
  
  public ConstEtomoNumber getMinViewsForTiltalign() {
    return minViewsForTiltalign;
  }
  
  public String getFiducialParams() {
    FortranInputString fortranInputString = new FortranInputString(2);
    fortranInputString.setIntegerType(new boolean[] { false, true });
    fortranInputString.set(0, centroidRadius);
    fortranInputString.set(1, lightBeads);
    return fortranInputString.toString();
  }
  
  public ConstEtomoNumber getMaxGapSize() {
    return maxGapSize;
  }
  
  public String getTiltAngleMinRange() {
    FortranInputString fortranInputString = new FortranInputString(2);
    fortranInputString.set(0, minTiltRangeToFindAxis);
    fortranInputString.set(1, minTiltRangeToFindAngles);
    return fortranInputString.toString();
  }
  
  public ConstEtomoNumber getMaxBeadsToAverage() {
    return maxBeadsToAverage;
  }
  
  public ConstEtomoNumber getDistanceRescueCriterion() {
    return distanceRescueCriterion;
  }
  
  public ConstEtomoNumber getPostFitRescueResidual() {
    return postFitRescueResidual;
  }
  
  public String getSecondPassParams() {
    FortranInputString fortranInputString = new FortranInputString(2);
    fortranInputString.set(0, densityRelaxationPostFit);
    fortranInputString.set(1, maxRescueDistance);
    return fortranInputString.toString();
  }

  //Non-const code
  
  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the beadtrack command
   * and parameters.
   */
  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    reset();
    if (!scriptCommand.isKeywordValuePairs()) {
      convertToPIP(scriptCommand);
    }
    else {
      inputFile = scriptCommand.getValue(INPUT_FILE_KEY);
      pieceListFile = scriptCommand.getValue(PIECE_LIST_FILE_KEY);
      seedModelFile = scriptCommand.getValue(SEED_MODEL_FILE_KEY);
      outputModelFile = scriptCommand.getValue(OUTPUT_MODEL_FILE_KEY);
      skipViews.parse(scriptCommand, false);
      rotationAngle.parse(scriptCommand);
      additionalViewGroups.parse(scriptCommand, true);
      tiltAngleSpec.parse(scriptCommand);
      tiltDefaultGrouping.parse(scriptCommand);
      tiltAngleGroups = ParamUtilities.setParamIfPresent(scriptCommand,
          TILT_ANGLE_GROUPS_KEY, nondefaultGroupSize,
          nondefaultGroupIntegerType);
      magDefaultGrouping.parse(scriptCommand);
      magnificationGroups = ParamUtilities.setParamIfPresent(scriptCommand,
          MAGNIFICATION_GROUPS_KEY, nondefaultGroupSize,
          nondefaultGroupIntegerType);
      minViewsForTiltalign.parse(scriptCommand);
      centroidRadius.parse(scriptCommand);
      lightBeads.parse(scriptCommand);
      fillGaps = scriptCommand.hasKeyword(FILL_GAPS_KEY);
      maxGapSize.parse(scriptCommand);
      minTiltRangeToFindAxis.parse(scriptCommand);
      minTiltRangeToFindAngles.parse(scriptCommand);
      searchBoxPixels.validateAndSet(scriptCommand
          .getValue(SEARCH_BOX_PIXELS_KEY));
      maxBeadsToAverage.parse(scriptCommand);
      fiducialExtrapolationParams.validateAndSet(scriptCommand
          .getValue(FIDUCIAL_EXTRAPOLATION_PARAMS_KEY));
      rescueAttemptParams.validateAndSet(scriptCommand
          .getValue(RESCUE_ATTEMPT_PARAMS_KEY));
      distanceRescueCriterion.parse(scriptCommand);
      rescueRelaxationParams.validateAndSet(scriptCommand
          .getValue(RESCUE_RELAXATION_PARAMS_KEY));
      postFitRescueResidual.parse(scriptCommand);
      densityRelaxationPostFit.parse(scriptCommand);
      maxRescueDistance.parse(scriptCommand);
      meanResidChangeLimits.validateAndSet(scriptCommand
          .getValue(MEAN_RESID_CHANGE_LIMITS_KEY));
      deletionParams
          .validateAndSet(scriptCommand.getValue(DELETION_PARAMS_KEY));
    }
  }
  
  private void convertToPIP(ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    OldBeadtrackParam oldParam = new OldBeadtrackParam();
    oldParam.parseComScriptCommand(scriptCommand);
    set(oldParam);
  }
  
  public void initializeDefaults() {
    tiltAngleGroups = null;
    magnificationGroups = null;
  }

  /**
   * Update the supplied ComScriptCommand with the parameters of this object.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command.
   * @throws BadComScriptException when the script command does not contain a
   * beadtrack command or the number of input arguments is incorrect.
   */
  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    String invalidReason = validate();
    if (invalidReason != null && !invalidReason.matches("\\s*")) {
      throw new BadComScriptException(invalidReason);
    }
    //  Switch to keyword/value pairs
    scriptCommand.useKeywordValue();

    ParamUtilities.updateScriptParameter(scriptCommand, INPUT_FILE_KEY,
        inputFile);
    ParamUtilities.updateScriptParameter(scriptCommand, PIECE_LIST_FILE_KEY,
        pieceListFile);
    ParamUtilities.updateScriptParameter(scriptCommand, SEED_MODEL_FILE_KEY,
        seedModelFile);
    ParamUtilities.updateScriptParameter(scriptCommand, OUTPUT_MODEL_FILE_KEY,
        outputModelFile);
    ParamUtilities.updateScriptParameter(scriptCommand, OUTPUT_MODEL_FILE_KEY,
        outputModelFile);
    ParamUtilities.updateScriptParameter(scriptCommand, skipViews.getKey(),
        skipViews);
    rotationAngle.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, additionalViewGroups
        .getKey(), additionalViewGroups);
    tiltAngleSpec.updateComScript(scriptCommand);
    tiltDefaultGrouping.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, TILT_ANGLE_GROUPS_KEY,
        tiltAngleGroups);
    magDefaultGrouping.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand,
        MAGNIFICATION_GROUPS_KEY, magnificationGroups);
    minViewsForTiltalign.updateComScript(scriptCommand);
    centroidRadius.updateComScript(scriptCommand);
    lightBeads.updateComScript(scriptCommand);
    ParamUtilities
        .updateScriptParameter(scriptCommand, FILL_GAPS_KEY, fillGaps);
    maxGapSize.updateComScript(scriptCommand);
    minTiltRangeToFindAxis.updateComScript(scriptCommand);
    minTiltRangeToFindAngles.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, SEARCH_BOX_PIXELS_KEY,
        searchBoxPixels);
    maxBeadsToAverage.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand,
        FIDUCIAL_EXTRAPOLATION_PARAMS_KEY, fiducialExtrapolationParams);
    ParamUtilities.updateScriptParameter(scriptCommand,
        RESCUE_ATTEMPT_PARAMS_KEY, rescueAttemptParams);
    distanceRescueCriterion.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand,
        RESCUE_RELAXATION_PARAMS_KEY, rescueRelaxationParams);
    postFitRescueResidual.updateComScript(scriptCommand);
    densityRelaxationPostFit.updateComScript(scriptCommand);
    maxRescueDistance.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand,
        MEAN_RESID_CHANGE_LIMITS_KEY, meanResidChangeLimits);
    ParamUtilities.updateScriptParameter(scriptCommand, DELETION_PARAMS_KEY,
        deletionParams);
  }
  
  private void set(OldConstBeadtrackParam param) {
    if (param == null) {
      throw new IllegalStateException("param is null");
    }
    inputFile = param.inputFile;
    pieceListFile = param.pieceListFile;
    seedModelFile = param.seedModelFile;
    outputModelFile = param.outputModelFile;
    viewSkipList = param.viewSkipList;
    rotationAngle.set(param.imageRotation);
    additionalViewGroups.parseString(param.additionalViewGroups);
    tiltAngleSpec.set(param.tiltAngleSpec);
    if (param.tiltAngleGroupParams != null) {
      tiltDefaultGrouping.set(param.tiltAngleGroupParams, 0);
    }
    if (param.tiltAngleGroups != null) {
      tiltAngleGroups = new FortranInputString[param.tiltAngleGroups.length];
      for (int i = 0; i < param.tiltAngleGroups.length; i++) {
        tiltAngleGroups[i] = new FortranInputString(param.tiltAngleGroups[i]);
      }
    }
    if (param.magnificationGroupParams != null) {
      magDefaultGrouping.set(param.magnificationGroupParams, 0);
    }
    
    if (param.magnificationGroups != null) {
      magnificationGroups = new FortranInputString[param.magnificationGroups.length];
      for (int i = 0; i < param.magnificationGroups.length; i++) {
        magnificationGroups[i] = new FortranInputString(param.magnificationGroups[i]);
      }
    }
    minViewsForTiltalign.set(param.nMinViews);
    if (fiducialParams != null) {
      centroidRadius.set(param.fiducialParams, 0);
      lightBeads.set(param.fiducialParams, 1);
    }
    fillGaps = param.fillGaps;
    maxGapSize.set(param.maxGap);
    if (tiltAngleMinRange != null) {
      minTiltRangeToFindAxis.set(param.tiltAngleMinRange, 0);
      minTiltRangeToFindAngles.set(param.tiltAngleMinRange, 1);
    }
    if (param.searchBoxPixels != null) {
      searchBoxPixels.set(param.searchBoxPixels);
    }
    maxBeadsToAverage.set(param.maxFiducialsAvg);
    if (param.fiducialExtrapolationParams != null) {
      fiducialExtrapolationParams.set(param.fiducialExtrapolationParams);
    }
    if (param.rescueAttemptParams != null) {
      rescueAttemptParams.set(param.rescueAttemptParams);
    }
    distanceRescueCriterion.set(param.minRescueDistance);
    if (param.rescueRelaxationParams != null) {
      rescueRelaxationParams.set(param.rescueRelaxationParams);
    }
    postFitRescueResidual.set(param.residualDistanceLimit);
    if (param.secondPassParams != null) {
      densityRelaxationPostFit.set(param.secondPassParams, 0);
      maxRescueDistance.set(param.secondPassParams, 1);
    }
    if (param.meanResidChangeLimits != null) {
      meanResidChangeLimits.set(param.meanResidChangeLimits);
    }
    if (param.deletionParams != null) {
      deletionParams.set(param.deletionParams);
    }
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the beadtrack command
   */
  private ComScriptInputArg[] getInputArguments(ComScriptCommand scriptCommand)
    throws BadComScriptException {

    //  Check to be sure that it is a beadtrack command
    if (!scriptCommand.getCommand().equals("beadtrack")) {
      throw (new BadComScriptException("Not a beadtrack command"));
    }

    //  Extract the parameters
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length < 26) {
      throw (
        new BadComScriptException(
          "Incorrect number of input arguments to beadtrack command\nGot "
            + String.valueOf(inputArgs.length)
            + " expected at least 26."));
    }

    return inputArgs;
  }
  
  public void setSkipViews(String skipViews) {
    this.skipViews.parseString(skipViews);
  }
  
  public ConstEtomoNumber setRotationAngle(int rotationAngle) {
    return this.rotationAngle.set(rotationAngle);
  }
  
  public ConstEtomoNumber setTiltDefaultGrouping(String tiltDefaultGrouping) {
    return this.tiltDefaultGrouping.set(tiltDefaultGrouping);
  }
  
  public void setTiltAngleGroups(String newTiltAngleGroups)
      throws FortranInputSyntaxException {
    tiltAngleGroups = ParamUtilities.parse(newTiltAngleGroups, true,
        nondefaultGroupSize);
  }
  
  public ConstEtomoNumber setMagDefaultGrouping(String magnificationGroupParams) {
    return this.magDefaultGrouping.set(magnificationGroupParams);
  }

  public void setMagnificationGroups(String newMagnificationGroups)
      throws FortranInputSyntaxException {
    magnificationGroups = ParamUtilities.parse(newMagnificationGroups, true,
        nondefaultGroupSize);
  }
  
  public ConstEtomoNumber setMinViewsForTiltalign(String minViewsForTiltalign) {
    return this.minViewsForTiltalign.set(minViewsForTiltalign);
  }
  
  public void setFiducialParams(String fiducialParams)
      throws FortranInputSyntaxException, InvalidEtomoNumberException {
    FortranInputString fortranInputString = new FortranInputString(2);
    fortranInputString.setIntegerType(new boolean[] { false, true });
    fortranInputString.validateAndSet(fiducialParams);
    centroidRadius.set(fortranInputString, 0).validate();
    lightBeads.set(fortranInputString, 1).validate();
  }

  public ConstEtomoNumber setMaxGapSize(String maxGapSize) {
    return this.maxGapSize.set(maxGapSize);
  }

  public void setTiltAngleMinRange(String tiltAngleMinRange) throws FortranInputSyntaxException,
      InvalidEtomoNumberException {
    FortranInputString fortranInputString = new FortranInputString(2);
    fortranInputString.validateAndSet(tiltAngleMinRange);
    minTiltRangeToFindAxis.set(fortranInputString, 0).validate();
    minTiltRangeToFindAngles.set(fortranInputString, 1).validate();
  }
  
  public ConstEtomoNumber setMaxBeadsToAverage(String maxBeadsToAverage) {
    return this.maxBeadsToAverage.set(maxBeadsToAverage);
  }
  
  public ConstEtomoNumber setDistanceRescueCriterion(String distanceRescueCriterion) {
    return this.distanceRescueCriterion.set(distanceRescueCriterion);
  }
  
  public ConstEtomoNumber setPostFitRescueResidual(String postFitRescueResidual) {
    return this.postFitRescueResidual.set(postFitRescueResidual);
  }
  
  public void setSecondPassParams(String secondPassParams)
      throws FortranInputSyntaxException, InvalidEtomoNumberException {
    FortranInputString fortranInputString = new FortranInputString(2);
    fortranInputString.validateAndSet(secondPassParams);
    densityRelaxationPostFit.set(fortranInputString, 0).validate();
    maxRescueDistance.set(fortranInputString, 1).validate();
  }
}
