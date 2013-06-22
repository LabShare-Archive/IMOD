package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
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
 * <p> Revision 3.32  2011/02/21 21:10:48  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.31  2010/09/23 21:05:00  sueh
 * <p> bug# 1404 Allowing additionalViewGroups to have multiple entries.
 * <p>
 * <p> Revision 3.30  2010/04/28 15:43:36  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 3.29  2010/03/12 03:57:16  sueh
 * <p> bug# 1325 Added isBeadDiameterSet.
 * <p>
 * <p> Revision 3.28  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.27  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 3.26  2009/12/11 17:25:40  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 3.25  2009/12/08 02:32:52  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 3.24  2009/09/05 00:35:29  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 3.23  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.22  2009/05/02 01:06:44  sueh
 * <p> bug# 1216 In getBeadDiameter changed the return value to
 * <p> ConstEtomoNumber.
 * <p>
 * <p> Revision 3.21  2009/03/17 00:30:34  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.20  2009/02/04 23:15:15  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 3.19  2008/12/12 17:38:17  sueh
 * <p> bug# 1160 Shared the BeadDiameter key.
 * <p>
 * <p> Revision 3.18  2008/12/09 21:27:28  sueh
 * <p> bug# 1160 Added beadDiameter.  Converted centroidRadius to
 * <p> beadDiameter if beadDiameter was not added by copytomocoms.
 * <p>
 * <p> Revision 3.17  2007/03/21 18:08:39  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Creating Autodoc using a factory.
 * <p>
 * <p> Revision 3.16  2007/03/07 20:58:07  sueh
 * <p> bug# 981
 * <p>
 * <p> Revision 3.15  2007/03/01 01:11:42  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 3.14  2007/02/05 21:31:34  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 3.13  2006/01/12 17:00:47  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.
 * <p>
 * <p> Revision 3.12  2005/08/27 21:07:11  sueh
 * <p> bug# 532 Changed Autodoc.get() to getInstance().
 * <p>
 * <p> Revision 3.11  2005/07/29 19:43:59  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 3.10  2005/05/17 19:13:48  sueh
 * <p> bug# 658 Passing a HashMap of required values from the autodoc to
 * <p> ScriptParameter constructors.
 * <p>
 * <p> Revision 3.9  2005/05/14 00:59:34  sueh
 * <p> bug# 658 When converting to PIP, set roundsOfTracking to 2 (to match
 * <p> copytomocoms).
 * <p>
 * <p> Revision 3.8  2005/05/13 17:44:33  sueh
 * <p> bug# 658 Changed the position of the new fields in the comscript.  They
 * <p> follow Max Gap.
 * <p>
 * <p> Revision 3.7  2005/05/12 19:09:01  sueh
 * <p> bug# 658 Updating TrackLocalArea as an integer in the comscript.
 * <p> Setting localAreaTargetSize, minBeadsInArea, minOverlapBeads, and
 * <p> roundsOfTracking when converting from a non-PIP script.
 * <p>
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

public class BeadtrackParam extends OldBeadtrackParam implements CommandParam,
    CommandDetails {
  public static final String rcsid = "$Id$";

  public static final ProcessName PROCESS_NAME = ProcessName.TRACK;

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
  public static final String DELETION_PARAMS_KEY = "DeletionCriterionMinAndSD";
  public static final String DENSITY_RELAXATION_POST_FIT_KEY = "DensityRelaxationPostFit";
  public static final String MAX_RESCUE_DISTANCE_KEY = "MaxRescueDistance";
  public static final String MIN_TILT_RANGE_TO_FIND_AXIS_KEY = "MinTiltRangeToFindAxis";
  public static final String MIN_TILT_RANGE_TO_FIND_ANGLES_KEY = "MinTiltRangeToFindAngles";
  public static final String LIGHT_BEADS_KEY = "LightBeads";
  public static final String BEAD_DIAMETER_KEY = "BeadDiameter";

  public static final String LOCAL_AREA_TRACKING_KEY = "LocalAreaTracking";
  public static final String LOCAL_AREA_TARGET_SIZE_KEY = "LocalAreaTargetSize";
  public static final String MIN_BEADS_IN_AREA_KEY = "MinBeadsInArea";
  public static final String MIN_OVERLAP_BEADS_KEY = "MinOverlapBeads";
  public static final String MAX_VIEWS_IN_ALIGN_KEY = "MaxViewsInAlign";
  public static final String ROUNDS_OF_TRACKING_KEY = "RoundsOfTracking";
  public static final String SOBEL_FILTER_CENTERING_KEY="SobelFilterCentering";
  public static final String KERNEL_SIGMA_FOR_SOBEL_KEY = "KernelSigmaForSobel";

  private final BaseManager manager;

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
  private ScriptParameter imagesAreBinned;
  private ScriptParameter beadDiameter;
  private EtomoBoolean2 sobelFilterCentering;
  private ScriptParameter kernelSigmaForSobel;
  
  private AxisID axisID;

  public BeadtrackParam(AxisID axisID, BaseManager manager) {
    this.axisID = axisID;
    this.manager = manager;
  }

  private void initialize() {
    if (initialized) {
      reset();
    }
    initialized = true;

    HashMap requiredMap = getRequiredMap();
    skipViews = new StringList(SKIP_VIEW_LIST_KEY);
    rotationAngle = new ScriptParameter(EtomoNumber.Type.DOUBLE, IMAGE_ROTATION_KEY,
        requiredMap);
    additionalViewGroups.setKey(ADDITIONAL_VIEW_GROUPS_KEY);
    additionalViewGroups.setSuccessiveEntriesAccumulate();

    tiltAngleSpec.setRangeMinKey("FirstTiltAngle", "first");
    tiltAngleSpec.setRangeStepKey("TiltIncrement", "increment");
    tiltAngleSpec.setTiltAngleFilenameKey("TiltFile", "tiltfile");
    tiltAngleSpec.setTiltAnglesKey("TiltAngles", "angles");

    tiltDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        TILT_ANGLE_GROUP_PARAMS_KEY, requiredMap);
    magDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        MAGNIFICATION_GROUP_PARAMS_KEY, requiredMap);
    minViewsForTiltalign = new ScriptParameter(EtomoNumber.Type.INTEGER, N_MIN_VIEWS_KEY,
        requiredMap);
    centroidRadius = new ScriptParameter(EtomoNumber.Type.DOUBLE, "CentroidRadius");
    lightBeads = new EtomoBoolean2(LIGHT_BEADS_KEY, requiredMap);
    maxGapSize = new ScriptParameter(EtomoNumber.Type.INTEGER, MAX_GAP_KEY, requiredMap);
    minTiltRangeToFindAxis = new ScriptParameter(EtomoNumber.Type.DOUBLE,
        MIN_TILT_RANGE_TO_FIND_AXIS_KEY, requiredMap);
    minTiltRangeToFindAngles = new ScriptParameter(EtomoNumber.Type.DOUBLE,
        MIN_TILT_RANGE_TO_FIND_ANGLES_KEY, requiredMap);
    maxBeadsToAverage = new ScriptParameter(EtomoNumber.Type.INTEGER,
        MAX_FIDUCIALS_AVG_KEY, requiredMap);
    rescueAttemptParams.setIntegerType(1, false);
    distanceRescueCriterion = new ScriptParameter(EtomoNumber.Type.DOUBLE,
        MIN_RESCUE_DISTANCE_KEY, requiredMap);
    postFitRescueResidual = new ScriptParameter(EtomoNumber.Type.DOUBLE,
        RESIDUAL_DISTANCE_LIMIT_KEY, requiredMap);
    densityRelaxationPostFit = new ScriptParameter(EtomoNumber.Type.DOUBLE,
        DENSITY_RELAXATION_POST_FIT_KEY, requiredMap);
    maxRescueDistance = new ScriptParameter(EtomoNumber.Type.DOUBLE,
        MAX_RESCUE_DISTANCE_KEY, requiredMap);
    deletionParams.setIntegerType(1, false);

    localAreaTracking = new EtomoBoolean2(LOCAL_AREA_TRACKING_KEY, requiredMap);
    localAreaTracking.setDisplayAsInteger(true);

    localAreaTargetSize = new ScriptParameter(EtomoNumber.Type.INTEGER,
        LOCAL_AREA_TARGET_SIZE_KEY, requiredMap);
    minBeadsInArea = new ScriptParameter(EtomoNumber.Type.INTEGER, MIN_BEADS_IN_AREA_KEY,
        requiredMap);
    minOverlapBeads = new ScriptParameter(EtomoNumber.Type.INTEGER,
        MIN_OVERLAP_BEADS_KEY, requiredMap);
    maxViewsInAlign = new ScriptParameter(EtomoNumber.Type.INTEGER,
        MAX_VIEWS_IN_ALIGN_KEY, requiredMap);
    roundsOfTracking = new ScriptParameter(EtomoNumber.Type.INTEGER,
        ROUNDS_OF_TRACKING_KEY, requiredMap);
    imagesAreBinned = new ScriptParameter("ImagesAreBinned");
    beadDiameter = new ScriptParameter(EtomoNumber.Type.DOUBLE, "BeadDiameter");
    sobelFilterCentering = new EtomoBoolean2(SOBEL_FILTER_CENTERING_KEY);
    kernelSigmaForSobel=new ScriptParameter(EtomoNumber.Type.DOUBLE,KERNEL_SIGMA_FOR_SOBEL_KEY);
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
    imagesAreBinned.reset();
    beadDiameter.reset();
    sobelFilterCentering.reset();
    kernelSigmaForSobel.reset();
  }

  private HashMap getRequiredMap() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.BEADTRACK, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
    if (autodoc == null) {
      return null;
    }
    return autodoc.getAttributeValues(EtomoAutodoc.FIELD_SECTION_NAME,
        EtomoAutodoc.REQUIRED_ATTRIBUTE_NAME);
  }

  public String getCommand() {
    return "track" + axisID.getExtension() + ".com";
  }

  public String getSkipViews() {
    return skipViews.toString();
  }

  public ConstEtomoNumber getRotationAngle() {
    return rotationAngle;
  }

  public boolean isMessageReporter() {
    return false;
  }

  /**
   * @return
   */
  public ConstEtomoNumber getTiltDefaultGrouping() {
    return tiltDefaultGrouping;
  }

  public int getMagnificationGroupSize() {
    return magDefaultGrouping.getInt();
  }

  public ConstEtomoNumber getMinViewsForTiltalign() {
    return minViewsForTiltalign;
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

  public boolean isBeadDiameterSet() {
    return !beadDiameter.isNull();
  }

  public ConstEtomoNumber getBeadDiameter() {
    return beadDiameter;
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
  
  public boolean isSobelFilterCentering() {
    return sobelFilterCentering.is();
  }
  
  public String getKernelSigmaForSobel() {
    return kernelSigmaForSobel.toString();
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
      skipViews.parse(scriptCommand);
      rotationAngle.parse(scriptCommand);
      additionalViewGroups.parse(scriptCommand);
      tiltAngleSpec.parse(scriptCommand);
      tiltDefaultGrouping.parse(scriptCommand);
      tiltAngleGroups = ParamUtilities.setParamIfPresent(scriptCommand,
          TILT_ANGLE_GROUPS_KEY, nondefaultGroupSize, nondefaultGroupIntegerType);
      magDefaultGrouping.parse(scriptCommand);
      magnificationGroups = ParamUtilities.setParamIfPresent(scriptCommand,
          MAGNIFICATION_GROUPS_KEY, nondefaultGroupSize, nondefaultGroupIntegerType);
      minViewsForTiltalign.parse(scriptCommand);
      centroidRadius.parse(scriptCommand);
      lightBeads.parse(scriptCommand);
      fillGaps = scriptCommand.hasKeyword(FILL_GAPS_KEY);
      maxGapSize.parse(scriptCommand);
      minTiltRangeToFindAxis.parse(scriptCommand);
      minTiltRangeToFindAngles.parse(scriptCommand);
      searchBoxPixels.validateAndSet(scriptCommand.getValue(SEARCH_BOX_PIXELS_KEY));
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
      deletionParams.validateAndSet(scriptCommand.getValue(DELETION_PARAMS_KEY));

      localAreaTracking.parse(scriptCommand);
      localAreaTargetSize.parse(scriptCommand);
      minBeadsInArea.parse(scriptCommand);
      minOverlapBeads.parse(scriptCommand);
      maxViewsInAlign.parse(scriptCommand);
      roundsOfTracking.parse(scriptCommand);
      beadDiameter.parse(scriptCommand);
      sobelFilterCentering.parse(scriptCommand);
      kernelSigmaForSobel.parse(scriptCommand);
    }
    //backward compatibility bug# 1160
    if (!centroidRadius.isNull()) {
      if (beadDiameter.isNull()) {
        beadDiameter.set(2 * centroidRadius.getDouble() - 3);
      }
      centroidRadius.reset();
    }
  }

  private void convertToPIP(ComScriptCommand scriptCommand) throws BadComScriptException,
      FortranInputSyntaxException, InvalidParameterException {
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

    ParamUtilities.updateScriptParameter(scriptCommand, INPUT_FILE_KEY, inputFile);
    ParamUtilities.updateScriptParameter(scriptCommand, PIECE_LIST_FILE_KEY,
        pieceListFile);
    ParamUtilities.updateScriptParameter(scriptCommand, SEED_MODEL_FILE_KEY,
        seedModelFile);
    ParamUtilities.updateScriptParameter(scriptCommand, OUTPUT_MODEL_FILE_KEY,
        outputModelFile);
    ParamUtilities.updateScriptParameter(scriptCommand, OUTPUT_MODEL_FILE_KEY,
        outputModelFile);
    ParamUtilities.updateScriptParameter(scriptCommand, skipViews.getKey(), skipViews);
    rotationAngle.updateComScript(scriptCommand);
    // ParamUtilities.updateScriptParameter(scriptCommand, additionalViewGroups
    //    .getKey(), additionalViewGroups);
    additionalViewGroups.updateComScript(scriptCommand);
    tiltAngleSpec.updateComScript(scriptCommand);
    tiltDefaultGrouping.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, TILT_ANGLE_GROUPS_KEY,
        tiltAngleGroups);
    magDefaultGrouping.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, MAGNIFICATION_GROUPS_KEY,
        magnificationGroups);
    minViewsForTiltalign.updateComScript(scriptCommand);
    centroidRadius.updateComScript(scriptCommand);
    lightBeads.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, FILL_GAPS_KEY, fillGaps);
    maxGapSize.updateComScript(scriptCommand);

    localAreaTracking.updateComScript(scriptCommand);
    localAreaTargetSize.updateComScript(scriptCommand);
    minBeadsInArea.updateComScript(scriptCommand);
    minOverlapBeads.updateComScript(scriptCommand);
    maxViewsInAlign.updateComScript(scriptCommand);
    roundsOfTracking.updateComScript(scriptCommand);
    sobelFilterCentering.updateComScript(scriptCommand);
    kernelSigmaForSobel.updateComScript(scriptCommand);

    minTiltRangeToFindAxis.updateComScript(scriptCommand);
    minTiltRangeToFindAngles.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, SEARCH_BOX_PIXELS_KEY,
        searchBoxPixels);
    maxBeadsToAverage.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand,
        FIDUCIAL_EXTRAPOLATION_PARAMS_KEY, fiducialExtrapolationParams);
    ParamUtilities.updateScriptParameter(scriptCommand, RESCUE_ATTEMPT_PARAMS_KEY,
        rescueAttemptParams);
    distanceRescueCriterion.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, RESCUE_RELAXATION_PARAMS_KEY,
        rescueRelaxationParams);
    postFitRescueResidual.updateComScript(scriptCommand);
    densityRelaxationPostFit.updateComScript(scriptCommand);
    maxRescueDistance.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, MEAN_RESID_CHANGE_LIMITS_KEY,
        meanResidChangeLimits);
    ParamUtilities.updateScriptParameter(scriptCommand, DELETION_PARAMS_KEY,
        deletionParams);
    imagesAreBinned.updateComScript(scriptCommand);
    beadDiameter.updateComScript(scriptCommand);
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
    roundsOfTracking.set(2);
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
    tiltAngleGroups = ParamUtilities.parse(newTiltAngleGroups, true, nondefaultGroupSize);
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

  public ConstEtomoNumber setBeadDiameter(String input) {
    return beadDiameter.set(input);
  }

  public ConstEtomoNumber setDensityRelaxationPostFit(String densityRelaxationPostFit) {
    return this.densityRelaxationPostFit.set(densityRelaxationPostFit);
  }

  public ConstEtomoNumber setMaxRescueDistance(String maxRescueDistance) {
    return this.maxRescueDistance.set(maxRescueDistance);
  }

  public ConstEtomoNumber setLocalAreaTracking(boolean localAreaTracking) {
    return this.localAreaTracking.set(localAreaTracking);
  }
  
  public void setSobelFilterCentering(boolean input) {
    sobelFilterCentering.set(input);
  }
  
  public void setKernelSigmaForSobel(String input) {
    kernelSigmaForSobel.set(input);
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

  public void setImagesAreBinned(int input) {
    imagesAreBinned.set(input);
  }

  public ConstEtomoNumber setMaxViewsInAlign(String maxViewsInAlign) {
    return this.maxViewsInAlign.set(maxViewsInAlign);
  }

  public ConstEtomoNumber setRoundsOfTracking(String roundsOfTracking) {
    return this.roundsOfTracking.set(roundsOfTracking);
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String[] getCommandArray() {
    String[] array = { getCommandLine() };
    return array;
  }

  public String getCommandLine() {
    return FileType.TRACK_COMSCRIPT.getFileName(manager, axisID);
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public String getCommandName() {
    return PROCESS_NAME.toString();
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
  }

  public String getName() {
    return PROCESS_NAME.toString();
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public File getCommandOutputFile() {
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public boolean getBooleanValue(final etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Field.LIGHT_BEADS) {
      return lightBeads.is();
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public int getIntValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public static final class Field implements etomo.comscript.FieldInterface {

    public static final Field LIGHT_BEADS = new Field();

    private Field() {
    }
  }
}
