package etomo.comscript;

import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
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
 * <p> Revision 3.6  2005/05/12 18:43:23  sueh
 * <p> bug# 658 Added localAreaTracking, localAreaTargetSize,
 * <p> minBeadsInArea, minOverlapBeads, maxViewsInAlign, and
 * <p> roundsOfTracking.  Made protected member variables and functions
 * <p> private because there is not ConstBeadtrackParam class.
 * <p>
 * <p> Revision 3.5  2005/05/12 01:18:01  sueh
 * <p> bug# 658 Made static strings with the autodoc keys public and added
 * <p> more so tooltips could come from autdoc.  Split secondPassParams,
 * <p> tiltAngleMinRange, and fiducialParams into two fields on the screen.
 * <p>
 * <p> Revision 3.4  2005/05/10 19:43:54  sueh
 * <p> bug# 658 Removing print statements.
 * <p>
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
  
  public static final String INPUT_FILE_KEY = "ImageFile";
  private static final String PIECE_LIST_FILE_KEY = "PieceListFile";
  public static final String SEED_MODEL_FILE_KEY = "InputSeedModel";
  public static final String OUTPUT_MODEL_FILE_KEY = "OutputModel";
  public static final String SKIP_VIEW_LIST_KEY = "SkipViews";
  private static final String IMAGE_ROTATION_KEY = "RotationAngle";
  public static final String ADDITIONAL_VIEW_GROUPS_KEY = "SeparateGroup";
  public static final String TILT_ANGLE_GROUP_PARAMS_KEY = "TiltDefaultGrouping"; 
  public static final String TILT_ANGLE_GROUPS_KEY = "TiltNondefaultGroup";
  public static final String MAGNIFICATION_GROUP_PARAMS_KEY = "MagDefaultGrouping";
  public static final String MAGNIFICATION_GROUPS_KEY = "MagNondefaultGroup";
  public static final String N_MIN_VIEWS_KEY = "MinViewsForTiltalign";
  public static final String FILL_GAPS_KEY = "FillGaps";
  public static final String MAX_GAP_KEY = "MaxGapSize";
  public static final String SEARCH_BOX_PIXELS_KEY = "BoxSizeXandY";
  public static final String MAX_FIDUCIALS_AVG_KEY = "MaxBeadsToAverage";
  public static final String FIDUCIAL_EXTRAPOLATION_PARAMS_KEY = "PointsToFitMaxAndMin";
  public static final String RESCUE_ATTEMPT_PARAMS_KEY = "DensityRescueFractionAndSD";
  public static final String MIN_RESCUE_DISTANCE_KEY = "DistanceRescueCriterion";
  public static final String RESCUE_RELAXATION_PARAMS_KEY = "RescueRelaxationDensityAndDistance";
  public static final String RESIDUAL_DISTANCE_LIMIT_KEY = "PostFitRescueResidual";
  public static final String MEAN_RESID_CHANGE_LIMITS_KEY = "ResidualsToAnalyzeMaxAndMin";
  public static final String DELETION_PARAMS_KEY ="DeletionCriterionMinAndSD";
  public static final String DENSITY_RELAXATION_POST_FIT_KEY = "DensityRelaxationPostFit";
  public static final String MAX_RESCUE_DISTANCE_KEY = "MaxRescueDistance";
  public static final String MIN_TILT_RANGE_TO_FIND_AXIS_KEY = "MinTiltRangeToFindAxis";
  public static final String MIN_TILT_RANGE_TO_FIND_ANGLES_KEY = "MinTiltRangeToFindAngles";
  public static final String CENTROID_RADIUS_KEY = "CentroidRadius";
  public static final String LIGHT_BEADS_KEY = "LightBeads";
  
  public static final String LOCAL_AREA_TRACKING_KEY = "LocalAreaTracking";
  public static final String LOCAL_AREA_TARGET_SIZE_KEY = "LocalAreaTargetSize";
  public static final String MIN_BEADS_IN_AREA_KEY = "MinBeadsInArea";
  public static final String MIN_OVERLAP_BEADS_KEY = "MinOverlapBeads";
  public static final String MAX_VIEWS_IN_ALIGN_KEY = "MaxViewsInAlign";
  public static final String ROUNDS_OF_TRACKING_KEY = "RoundsOfTracking";
  
  private boolean initialized = false;
  
  private StringList skipViews;//was viewSkipList
  private ScriptParameter rotationAngle;//was imageRotation
  private ScriptParameter tiltDefaultGrouping;//was tiltAngleGroupParams
  private ScriptParameter magDefaultGrouping;//was magnificationGroupParams
  private ScriptParameter minViewsForTiltalign;//was nMinViews
  private ScriptParameter centroidRadius;//was fiducialParams(0)
  private EtomoBoolean2 lightBeads;//was fiducialParams(1)
  private ScriptParameter maxGapSize;//was maxGap
  private ScriptParameter minTiltRangeToFindAxis;//was tiltAngleMinRange(0)
  private ScriptParameter minTiltRangeToFindAngles;//was tiltAngleMinRange(1)
  private ScriptParameter maxBeadsToAverage;//was maxFiducialsAvg
  private ScriptParameter distanceRescueCriterion;//was minRescueDistance
  private ScriptParameter postFitRescueResidual;//was residualDistanceLimit
  private ScriptParameter densityRelaxationPostFit;//was secondPassParams(0)
  private ScriptParameter maxRescueDistance;//was secondPassParams(1)
  
  private EtomoBoolean2 localAreaTracking;
  private ScriptParameter localAreaTargetSize;
  private ScriptParameter minBeadsInArea;
  private ScriptParameter minOverlapBeads;
  private ScriptParameter maxViewsInAlign;
  private ScriptParameter roundsOfTracking;
  
  private void initialize() {
    if (initialized) {
      reset();
    }
    initialized = true;
    
    skipViews = new StringList(SKIP_VIEW_LIST_KEY);
    rotationAngle = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        IMAGE_ROTATION_KEY);
    additionalViewGroups.setKey(ADDITIONAL_VIEW_GROUPS_KEY);
    
    tiltAngleSpec.setRangeMinKey("FirstTiltAngle", "first");
    tiltAngleSpec.setRangeStepKey("TiltIncrement", "increment");
    tiltAngleSpec.setTiltAngleFilenameKey("TiltFile", "tiltfile");
    tiltAngleSpec.setTiltAnglesKey("TiltAngles", "angles");
    
    tiltDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        TILT_ANGLE_GROUP_PARAMS_KEY);    
    magDefaultGrouping = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        MAGNIFICATION_GROUP_PARAMS_KEY);
    minViewsForTiltalign = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        N_MIN_VIEWS_KEY);
    centroidRadius = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        CENTROID_RADIUS_KEY);    
    lightBeads = new EtomoBoolean2(LIGHT_BEADS_KEY);
    maxGapSize = new ScriptParameter(EtomoNumber.INTEGER_TYPE, MAX_GAP_KEY);
    minTiltRangeToFindAxis = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        MIN_TILT_RANGE_TO_FIND_AXIS_KEY);
    minTiltRangeToFindAngles = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        MIN_TILT_RANGE_TO_FIND_ANGLES_KEY);
    maxBeadsToAverage = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        MAX_FIDUCIALS_AVG_KEY);
    rescueAttemptParams.setIntegerType(1, false);
    distanceRescueCriterion = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        MIN_RESCUE_DISTANCE_KEY);
    postFitRescueResidual = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        RESIDUAL_DISTANCE_LIMIT_KEY);
    densityRelaxationPostFit = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        DENSITY_RELAXATION_POST_FIT_KEY);
    maxRescueDistance = new ScriptParameter(EtomoNumber.DOUBLE_TYPE,
        MAX_RESCUE_DISTANCE_KEY);
    deletionParams.setIntegerType(1, false);
    
    localAreaTracking = new EtomoBoolean2(LOCAL_AREA_TRACKING_KEY);
    localAreaTracking.setDisplayAsInteger(true);
    
    localAreaTargetSize = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        LOCAL_AREA_TARGET_SIZE_KEY);
    minBeadsInArea = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        MIN_BEADS_IN_AREA_KEY);
    minOverlapBeads = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        MIN_OVERLAP_BEADS_KEY);
    maxViewsInAlign = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        MAX_VIEWS_IN_ALIGN_KEY);
    roundsOfTracking = new ScriptParameter(EtomoNumber.INTEGER_TYPE,
        ROUNDS_OF_TRACKING_KEY);
  }
  
  private void reset() {
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
    
    localAreaTracking.reset();
    localAreaTargetSize.reset();
    minBeadsInArea.reset();
    minOverlapBeads.reset();
    maxViewsInAlign.reset();
    roundsOfTracking.reset();
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
  
  public ConstEtomoNumber getCentroidRadius() {
    return centroidRadius;
  }
  
  public ConstEtomoNumber getLightBeads() {
    return lightBeads;
  }
  
  public ConstEtomoNumber getMaxGapSize() {
    return maxGapSize;
  }
  
  public ConstEtomoNumber getMinTiltRangeToFindAxis() {
    return minTiltRangeToFindAxis;
  }
  
  public ConstEtomoNumber getMinTiltRangeToFindAngles() {
    return minTiltRangeToFindAngles;
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
  
  public ConstEtomoNumber getDensityRelaxationPostFit() {
    return densityRelaxationPostFit;
  }
  
  public ConstEtomoNumber getMaxRescueDistance() {
    return maxRescueDistance;
  }
  
  public ConstEtomoNumber getLocalAreaTracking() {
    return localAreaTracking;
  }
  
  public ConstEtomoNumber getLocalAreaTargetSize() {
    return localAreaTargetSize;
  }

  public ConstEtomoNumber getMinBeadsInArea() {
    return minBeadsInArea;
  }
  
  public ConstEtomoNumber getMinOverlapBeads() {
    return minOverlapBeads;
  }
  
  public ConstEtomoNumber getMaxViewsInAlign() {
    return maxViewsInAlign;
  }
  
  public ConstEtomoNumber getRoundsOfTracking() {
    return roundsOfTracking;
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
      
      localAreaTracking.parse(scriptCommand);
      localAreaTargetSize.parse(scriptCommand);
      minBeadsInArea.parse(scriptCommand);
      minOverlapBeads.parse(scriptCommand);
      maxViewsInAlign.parse(scriptCommand);
      roundsOfTracking.parse(scriptCommand);
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
    initialize();
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

    localAreaTracking.updateComScript(scriptCommand);
    localAreaTargetSize.updateComScript(scriptCommand);
    minBeadsInArea.updateComScript(scriptCommand);
    minOverlapBeads.updateComScript(scriptCommand);
    maxViewsInAlign.updateComScript(scriptCommand);
    roundsOfTracking.updateComScript(scriptCommand);
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
    
    localAreaTargetSize.set(1000);
    minBeadsInArea.set(8);
    minOverlapBeads.set(5);
    roundsOfTracking.set(1);
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
  
  public ConstEtomoNumber setCentroidRadius(String centroidRadius) {
    return this.centroidRadius.set(centroidRadius);
  }

  public ConstEtomoNumber setLightBeads(boolean lightBeads) {
    return this.lightBeads.set(lightBeads);
  }
  
  public ConstEtomoNumber setMaxGapSize(String maxGapSize) {
    return this.maxGapSize.set(maxGapSize);
  }
  
  public ConstEtomoNumber setMinTiltRangeToFindAxis(String minTiltRangeToFindAxis) {
    return this.minTiltRangeToFindAxis.set(minTiltRangeToFindAxis);
  }
  
  public ConstEtomoNumber setMinTiltRangeToFindAngles(String minTiltRangeToFindAngles) {
    return this.minTiltRangeToFindAngles.set(minTiltRangeToFindAngles);
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
  
  public ConstEtomoNumber setDensityRelaxationPostFit(
      String densityRelaxationPostFit) {
    return this.densityRelaxationPostFit.set(densityRelaxationPostFit);
  }
  
  public ConstEtomoNumber setMaxRescueDistance(
      String maxRescueDistance) {
    return this.maxRescueDistance.set(maxRescueDistance);
  }

  public ConstEtomoNumber setLocalAreaTracking(boolean localAreaTracking) {
    return this.localAreaTracking.set(localAreaTracking);
  }
  
  public ConstEtomoNumber setLocalAreaTargetSize(String localAreaTargetSize) {
    return this.localAreaTargetSize.set(localAreaTargetSize);
  }
  
  public ConstEtomoNumber setMinBeadsInArea(String minBeadsInArea) {
    return this.minBeadsInArea.set(minBeadsInArea);
  }
  
  public ConstEtomoNumber setMinOverlapBeads(String minOverlapBeads) {
    return this.minOverlapBeads.set(minOverlapBeads);
  }
  
  public ConstEtomoNumber setMaxViewsInAlign(String maxViewsInAlign) {
    return this.maxViewsInAlign.set(maxViewsInAlign);
  }
  
  public ConstEtomoNumber setRoundsOfTracking(String roundsOfTracking) {
    return this.roundsOfTracking.set(roundsOfTracking);
  }
}
