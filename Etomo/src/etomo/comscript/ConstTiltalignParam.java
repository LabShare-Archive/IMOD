package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.TiltAngleSpec;

/**
 * <p>Description: A read only model of the parameter interface for the
 *  tiltalign program</p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 */
public class ConstTiltalignParam implements CommandDetails {
  public static final String rcsid = "$Id$";

  public static final int SINGLE_OPTION = -1;
  public static final int FIXED_OPTION = 0;
  public static final int NONE_OPTION = FIXED_OPTION;
  public static final int ALL_OPTION = 1;
  public static final int TILT_ALL_OPTION = 2;
  public static final int BEAM_SEARCH_OPTION = 2;
  public static final int AUTOMAPPED_OPTION = 3;
  public static final int TILT_AUTOMAPPED_OPTION = 5;

  public static final String EXCLUDE_LIST_KEY = "ExcludeList";
  public static final String SEPARATE_GROUP_KEY = "SeparateGroup";
  public static final String RESIDUAL_REPORT_CRITERION_KEY = "ResidualReportCriterion";
  public static final String SURFACES_TO_ANALYZE_KEY = "SurfacesToAnalyze";
  public static final String ANGLE_OFFSET_KEY = "AngleOffset";
  public static final String AXIS_Z_SHIFT_KEY = "AxisZShift";
  public static final String METRO_FACTOR_KEY = "MetroFactor";
  public static final String MAXIMUM_CYCLES_KEY = "MaximumCycles";
  public static final String LOCAL_ALIGNMENTS_KEY = "LocalAlignments";
  public static final String NUMBER_OF_LOCAL_PATCHES_X_AND_Y_KEY = "NumberOfLocalPatchesXandY";
  public static final String TARGET_PATCH_SIZE_X_AND_Y_KEY = "TargetPatchSizeXandY";
  public static final String MIN_SIZE_OR_OVERLAP_X_AND_Y_KEY = "MinSizeOrOverlapXandY";
  public static final String MIN_FIDS_TOTAL_AND_EACH_SURFACE_KEY = "MinFidsTotalAndEachSurface";
  public static final String TILT_OPTION_KEY = "TiltOption";
  public static final String TILT_DEFAULT_GROUPING_KEY = "TiltDefaultGrouping";
  public static final String TILT_NONDEFAULT_GROUP_KEY = "TiltNondefaultGroup";
  public static final String MAG_OPTION_KEY = "MagOption";
  public static final String MAG_REFERENCE_VIEW_KEY = "MagReferenceView";
  public static final String MAG_DEFAULT_GROUPING_KEY = "MagDefaultGrouping";
  public static final String MAG_NONDEFAULT_GROUP_KEY = "MagNondefaultGroup";
  public static final String ROT_OPTION_KEY = "RotOption";
  public static final String ROT_ANGLE_KEY = "RotationAngle";
  public static final String ROT_DEFAULT_GROUPING_KEY = "RotDefaultGrouping";
  public static final String ROT_NONDEFAULT_GROUP_KEY = "RotNondefaultGroup";
  public static final String SKEW_OPTION_KEY = "SkewOption";
  public static final String X_STRETCH_DEFAULT_GROUPING_KEY = "XStretchDefaultGrouping";
  public static final String X_STRETCH_NONDEFAULT_GROUP_KEY = "XStretchNondefaultGroup";
  public static final String SKEW_DEFAULT_GROUPING_KEY = "SkewDefaultGrouping";
  public static final String SKEW_NONDEFAULT_GROUP_KEY = "SkewNondefaultGroup";
  public static final String LOCAL_ROT_OPTION_KEY = "LocalRotOption";
  public static final String LOCAL_ROT_DEFAULT_GROUPING_KEY = "LocalRotDefaultGrouping";
  public static final String LOCAL_ROT_NONDEFAULT_GROUP_KEY = "LocalRotNondefaultGroup";
  public static final String LOCAL_TILT_OPTION_KEY = "LocalTiltOption";
  public static final String LOCAL_TILT_DEFAULT_GROUPING_KEY = "LocalTiltDefaultGrouping";
  public static final String LOCAL_TILT_NONDEFAULT_GROUP_KEY = "LocalTiltNondefaultGroup";
  public static final String LOCAL_MAG_OPTION_KEY = "LocalMagOption";
  public static final String LOCAL_MAG_DEFAULT_GROUPING_KEY = "LocalMagDefaultGrouping";
  public static final String LOCAL_MAG_NONDEFAULT_GROUP_KEY = "LocalMagNondefaultGroup";
  public static final String LOCAL_SKEW_OPTION_KEY = "LocalSkewOption";
  public static final String LOCAL_X_STRETCH_DEFAULT_GROUPING_KEY = "LocalXStretchDefaultGrouping";
  public static final String LOCAL_X_STRETCH_NONDEFAULT_GROUP_KEY = "LocalXStretchNondefaultGroup";
  public static final String LOCAL_SKEW_DEFAULT_GROUPING_KEY = "LocalSkewDefaultGrouping";
  public static final String LOCAL_SKEW_NONDEFAULT_GROUP_KEY = "LocalSkewNondefaultGroup";
  public static final String PROJECTION_STRETCH_KEY = "ProjectionStretch";
  public static final String FIX_XYZ_COORDINATES_KEY = "FixXYZCoordinates";
  static final String OUTPUT_X_AXIS_TILT_FILE_KEY = "OutputXAxisTiltFile";
  public static final String BEAM_TILT_OPTION_KEY = "BeamTiltOption";
  public static final String FIXED_OR_INITIAL_BEAM_TILT = "FixedOrInitialBeamTilt";
  public static final String ROBUST_FITTING_KEY = "RobustFitting";
  public static final String K_FACTOR_SCALING_KEY = "KFactorScaling";
  public static final String WEIGHT_WHOLE_TRACKS_KEY = "WeightWholeTracks";

  static final String modelFileString = "ModelFile";
  static final String imageFileString = "ImageFile";
  static final String outputModelFileString = "OutputModelFile";
  static final String outputResidualFileString = "OutputResidualFile";
  static final String outputModelAndResidualString = "OutputModelAndResidual";
  static final String outputFidXYZFileString = "OutputFidXYZFile";
  static final String outputTiltFileString = "OutputTiltFile";
  static final String outputTransformFileString = "OutputTransformFile";
  static final String outputZFactorFileString = "OutputZFactorFile";
  static final String includeStartEndIncString = "IncludeStartEndInc";
  static final String includeListString = "IncludeList";
  static final String outputLocalFileString = "OutputLocalFile";
  static final String localOutputOptionsString = "LocalOutputOptions";

  static final String modelFileExtension = ".3dmod";
  static final String residualFileExtension = ".resid";
  static final String zFactorFileExtension = ".zfac";
  static final String localFileExtension = "local.xf";
  static final boolean[] nondefaultGroupIntegerType = { true, true, true };
  protected static final int nondefaultGroupSize = 3;

  private static final int[] optionValidValues = { FIXED_OPTION, ALL_OPTION,
      AUTOMAPPED_OPTION };
  private static final int[] tiltOptionValidValues = { FIXED_OPTION, TILT_ALL_OPTION,
      TILT_AUTOMAPPED_OPTION };
  private static final int[] distortionOptionValidValues = { FIXED_OPTION,
      AUTOMAPPED_OPTION };
  private static final int[] localOptionValidValues = { FIXED_OPTION, AUTOMAPPED_OPTION };
  private static final int[] localTiltOptionValidValues = { FIXED_OPTION,
      TILT_AUTOMAPPED_OPTION };
  private static final int[] rotOptionValidValues = { FIXED_OPTION, ALL_OPTION,
      AUTOMAPPED_OPTION, SINGLE_OPTION };
  private static final int[] surfacesToAnalyzeValidValues = { 0, 1, 2 };
  private static final ProcessName PROCESS_NAME = ProcessName.ALIGN;
  private static final String commandFileExtension = ".com";
  public static final String TARGET_PATCH_SIZE_X_AND_Y_DEFAULT = "700,700";
  public static final String NUMBER_OF_LOCAL_PATCHES_X_AND_Y_DEFAULT = "5,5";

  String modelFile;
  String imageFile;
  String outputModelAndResidual;
  String outputModelFile;
  String outputResidualFile;
  String outputFidXYZFile;
  String outputTiltFile;
  String outputTransformFile;
  String outputZFactorFile;
  FortranInputString includeStartEndInc;
  StringList includeList;
  StringList excludeList;
  ScriptParameter rotationAngle;
  StringList separateGroup;
  TiltAngleSpec tiltAngleSpec;
  ScriptParameter angleOffset;
  EtomoBoolean2 projectionStretch;
  ScriptParameter rotOption;
  ScriptParameter rotDefaultGrouping;
  FortranInputString[] rotNondefaultGroup;
  ScriptParameter rotationFixedView;
  ScriptParameter localRotOption;
  ScriptParameter localRotDefaultGrouping;
  FortranInputString[] localRotNondefaultGroup;
  ScriptParameter tiltOption;
  ScriptParameter tiltDefaultGrouping;
  FortranInputString[] tiltNondefaultGroup;
  ScriptParameter localTiltOption;
  ScriptParameter localTiltDefaultGrouping;
  FortranInputString[] localTiltNondefaultGroup;
  ScriptParameter magReferenceView;
  ScriptParameter magOption;
  ScriptParameter magDefaultGrouping;
  FortranInputString[] magNondefaultGroup;
  ScriptParameter localMagReferenceView;
  ScriptParameter localMagOption;
  ScriptParameter localMagDefaultGrouping;
  FortranInputString[] localMagNondefaultGroup;
  ScriptParameter xStretchOption;
  ScriptParameter xStretchDefaultGrouping;
  FortranInputString[] xStretchNondefaultGroup;
  ScriptParameter localXStretchOption;
  ScriptParameter localXStretchDefaultGrouping;
  FortranInputString[] localXStretchNondefaultGroup;
  ScriptParameter skewOption;
  ScriptParameter skewDefaultGrouping;
  FortranInputString[] skewNondefaultGroup;
  ScriptParameter localSkewOption;
  ScriptParameter localSkewDefaultGrouping;
  FortranInputString[] localSkewNondefaultGroup;
  ScriptParameter residualReportCriterion;
  ScriptParameter surfacesToAnalyze;
  ScriptParameter metroFactor;
  ScriptParameter maximumCycles;
  ScriptParameter axisZShift;
  EtomoBoolean2 localAlignments;
  String outputLocalFile;
  FortranInputString numberOfLocalPatchesXandY;
  FortranInputString targetPatchSizeXandY;
  FortranInputString minSizeOrOverlapXandY;
  FortranInputString minFidsTotalAndEachSurface;
  EtomoBoolean2 fixXYZCoordinates;
  FortranInputString localOutputOptions;
  ScriptParameter imagesAreBinned;
  ScriptParameter beamTiltOption;
  final ScriptParameter fixedOrInitialBeamTilt = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "FixedOrInitialBeamTilt");
  String outputXAxisTiltFile = "";
  final EtomoBoolean2 robustFitting = new EtomoBoolean2(ROBUST_FITTING_KEY);
  final EtomoBoolean2 weightWholeTracks= new EtomoBoolean2(WEIGHT_WHOLE_TRACKS_KEY);
  ScriptParameter kFactorScaling = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      K_FACTOR_SCALING_KEY);

  AxisID axisID;
  String datasetName;
  boolean loadedFromFile = false;
  final BaseManager manager;

  public ConstTiltalignParam(final BaseManager manager, final String datasetName,
      final AxisID axisID) {
    this.axisID = axisID;
    this.datasetName = datasetName;
    this.manager = manager;
    rotationAngle = new ScriptParameter(EtomoNumber.Type.DOUBLE, ROT_ANGLE_KEY);
    tiltAngleSpec = new TiltAngleSpec();
    tiltAngleSpec.setRangeMinKey("FirstTiltAngle", "first");
    tiltAngleSpec.setRangeStepKey("TiltIncrement", "increment");
    tiltAngleSpec.setTiltAngleFilenameKey("TiltFile", "tiltFile");
    angleOffset = new ScriptParameter(EtomoNumber.Type.DOUBLE, ANGLE_OFFSET_KEY);
    projectionStretch = new EtomoBoolean2(PROJECTION_STRETCH_KEY);
    rotOption = new ScriptParameter(EtomoNumber.Type.INTEGER, ROT_OPTION_KEY);
    rotOption.setValidValues(rotOptionValidValues).setDisplayValue(AUTOMAPPED_OPTION);
    rotDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        ROT_DEFAULT_GROUPING_KEY);
    rotDefaultGrouping.setDisplayValue(3);
    rotationFixedView = new ScriptParameter(EtomoNumber.Type.INTEGER, "RotationFixedView");
    localRotOption = new ScriptParameter(EtomoNumber.Type.INTEGER, LOCAL_ROT_OPTION_KEY);
    localRotOption.setValidValues(localOptionValidValues).setDisplayValue(
        AUTOMAPPED_OPTION);
    localRotDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        LOCAL_ROT_DEFAULT_GROUPING_KEY);
    localRotDefaultGrouping.setDisplayValue(6);
    tiltOption = new ScriptParameter(EtomoNumber.Type.INTEGER, TILT_OPTION_KEY);
    tiltOption.setValidValues(tiltOptionValidValues).setDisplayValue(TILT_ALL_OPTION);
    tiltDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        TILT_DEFAULT_GROUPING_KEY);
    tiltDefaultGrouping.setDisplayValue(5);
    localTiltOption = new ScriptParameter(EtomoNumber.Type.INTEGER, LOCAL_TILT_OPTION_KEY);
    localTiltOption.setValidValues(localTiltOptionValidValues).setDisplayValue(
        TILT_AUTOMAPPED_OPTION);
    localTiltDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        LOCAL_TILT_DEFAULT_GROUPING_KEY);
    localTiltDefaultGrouping.setDisplayValue(6);
    magReferenceView = new ScriptParameter(EtomoNumber.Type.INTEGER,
        MAG_REFERENCE_VIEW_KEY);
    magOption = new ScriptParameter(EtomoNumber.Type.INTEGER, MAG_OPTION_KEY);
    magOption.setValidValues(optionValidValues).setDisplayValue(ALL_OPTION);
    magDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        MAG_DEFAULT_GROUPING_KEY);
    magDefaultGrouping.setDisplayValue(4);
    localMagReferenceView = new ScriptParameter(EtomoNumber.Type.INTEGER,
        "LocalMagReferenceView");
    localMagOption = new ScriptParameter(EtomoNumber.Type.INTEGER, LOCAL_MAG_OPTION_KEY);
    localMagOption.setValidValues(localOptionValidValues).setDisplayValue(
        AUTOMAPPED_OPTION);
    localMagDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        LOCAL_MAG_DEFAULT_GROUPING_KEY);
    localMagDefaultGrouping.setDisplayValue(7);
    xStretchOption = new ScriptParameter(EtomoNumber.Type.INTEGER, "XStretchOption");
    xStretchOption.setValidValues(distortionOptionValidValues).setDisplayValue(
        NONE_OPTION);
    xStretchDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        X_STRETCH_DEFAULT_GROUPING_KEY);
    xStretchDefaultGrouping.setDisplayValue(7);
    localXStretchOption = new ScriptParameter(EtomoNumber.Type.INTEGER,
        "LocalXStretchOption");
    localXStretchOption.setValidValues(localOptionValidValues).setDisplayValue(
        AUTOMAPPED_OPTION);
    localXStretchDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        LOCAL_X_STRETCH_DEFAULT_GROUPING_KEY);
    localXStretchDefaultGrouping.setDisplayValue(7);
    skewOption = new ScriptParameter(EtomoNumber.Type.INTEGER, SKEW_OPTION_KEY);
    skewOption.setValidValues(distortionOptionValidValues).setDisplayValue(NONE_OPTION);
    skewDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        SKEW_DEFAULT_GROUPING_KEY);
    skewDefaultGrouping.setDisplayValue(11);
    localSkewOption = new ScriptParameter(EtomoNumber.Type.INTEGER, LOCAL_SKEW_OPTION_KEY);
    localSkewOption.setValidValues(optionValidValues).setDisplayValue(AUTOMAPPED_OPTION);
    localSkewDefaultGrouping = new ScriptParameter(EtomoNumber.Type.INTEGER,
        LOCAL_SKEW_DEFAULT_GROUPING_KEY);
    localSkewDefaultGrouping.setDisplayValue(11);
    residualReportCriterion = new ScriptParameter(EtomoNumber.Type.DOUBLE,
        RESIDUAL_REPORT_CRITERION_KEY);
    surfacesToAnalyze = new ScriptParameter(EtomoNumber.Type.INTEGER,
        SURFACES_TO_ANALYZE_KEY);
    surfacesToAnalyze.setValidValues(surfacesToAnalyzeValidValues);
    metroFactor = new ScriptParameter(EtomoNumber.Type.DOUBLE, METRO_FACTOR_KEY);
    maximumCycles = new ScriptParameter(EtomoNumber.Type.INTEGER, MAXIMUM_CYCLES_KEY);
    axisZShift = new ScriptParameter(EtomoNumber.Type.DOUBLE, AXIS_Z_SHIFT_KEY);
    localAlignments = new EtomoBoolean2(LOCAL_ALIGNMENTS_KEY);
    localAlignments.setDisplayAsInteger(true);
    fixXYZCoordinates = new EtomoBoolean2(FIX_XYZ_COORDINATES_KEY);
    fixXYZCoordinates.setDisplayAsInteger(true);
    // do not default imagesAreBinnned
    imagesAreBinned = new ScriptParameter("ImagesAreBinned");
    imagesAreBinned.setFloor(1);
    beamTiltOption = new ScriptParameter(BEAM_TILT_OPTION_KEY);
    beamTiltOption.setDefault(FIXED_OPTION);
    beamTiltOption.setDisplayValue(FIXED_OPTION);
    beamTiltOption.setValidValues(new int[] { FIXED_OPTION, BEAM_SEARCH_OPTION });
    fixedOrInitialBeamTilt.setDefault(0);
    reset();
  }

  void reset() {
    loadedFromFile = false;
    modelFile = "";
    imageFile = "";
    outputModelAndResidual = "";
    outputModelFile = "";
    outputResidualFile = "";
    outputFidXYZFile = "";
    outputTiltFile = "";
    outputTransformFile = "";
    outputZFactorFile = "";
    includeStartEndInc = new FortranInputString(3);
    includeStartEndInc.setIntegerType(new boolean[] { true, true, true });
    includeList = new StringList();
    excludeList = new StringList();
    rotationAngle.reset();
    separateGroup = new StringList();
    separateGroup.setKey(SEPARATE_GROUP_KEY);
    separateGroup.setSuccessiveEntriesAccumulate();
    tiltAngleSpec.reset();
    angleOffset.reset();
    projectionStretch.reset();
    rotOption.reset();
    rotDefaultGrouping.reset();
    rotNondefaultGroup = null;
    rotationFixedView.reset();
    localRotOption.reset();
    localRotDefaultGrouping.reset();
    localRotNondefaultGroup = null;
    tiltOption.reset();
    tiltDefaultGrouping.reset();
    tiltNondefaultGroup = null;
    localTiltOption.reset();
    localTiltDefaultGrouping.reset();
    localTiltNondefaultGroup = null;
    magReferenceView.reset();
    magOption.reset();
    magDefaultGrouping.reset();
    magNondefaultGroup = null;
    localMagOption.reset();
    localMagDefaultGrouping.reset();
    localMagNondefaultGroup = null;
    xStretchOption.reset();
    xStretchDefaultGrouping.reset();
    xStretchNondefaultGroup = null;
    localXStretchOption.reset();
    localXStretchDefaultGrouping.reset();
    localXStretchNondefaultGroup = null;
    skewOption.reset();
    skewDefaultGrouping.reset();
    skewNondefaultGroup = null;
    localSkewOption.reset();
    localSkewDefaultGrouping.reset();
    localSkewNondefaultGroup = null;
    residualReportCriterion.reset();
    surfacesToAnalyze.reset();
    metroFactor.reset();
    maximumCycles.reset();
    axisZShift.reset();
    localAlignments.reset();
    outputLocalFile = "";
    numberOfLocalPatchesXandY = new FortranInputString(2);
    targetPatchSizeXandY = new FortranInputString(2);
    numberOfLocalPatchesXandY.setIntegerType(new boolean[] { true, true });
    targetPatchSizeXandY.setIntegerType(new boolean[] { true, true });
    minSizeOrOverlapXandY = new FortranInputString(2);
    minFidsTotalAndEachSurface = new FortranInputString(2);
    minFidsTotalAndEachSurface.setIntegerType(new boolean[] { true, true });
    fixXYZCoordinates.reset();
    localOutputOptions = new FortranInputString(3);
    localOutputOptions.setIntegerType(new boolean[] { true, true, true });
    imagesAreBinned.reset();
    beamTiltOption.reset();
    fixedOrInitialBeamTilt.reset();
    outputXAxisTiltFile = "";
    robustFitting.reset();
    weightWholeTracks.reset();
    kFactorScaling.reset();
  }

  public ConstEtomoNumber getImagesAreBinned() {
    return imagesAreBinned;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  String validate() {
    StringBuffer invalidReason = new StringBuffer();
    if (!rotOption.isValid()) {
      invalidReason.append(rotOption.getDescription() + ": "
          + rotOption.getInvalidReason() + "\n");
    }
    if (!localRotOption.isValid()) {
      invalidReason.append(localRotOption.getDescription() + ": "
          + localRotOption.getInvalidReason() + "\n");
    }
    if (!tiltOption.isValid()) {
      invalidReason.append(tiltOption.getDescription() + ": "
          + tiltOption.getInvalidReason() + "\n");
    }
    if (!localTiltOption.isValid()) {
      invalidReason.append(localTiltOption.getDescription() + ": "
          + localTiltOption.getInvalidReason() + "\n");
    }
    if (!magOption.isValid()) {
      invalidReason.append(magOption.getDescription() + ": "
          + magOption.getInvalidReason() + "\n");
    }
    if (!localMagOption.isValid()) {
      invalidReason.append(localMagOption.getDescription() + ": "
          + localMagOption.getInvalidReason() + "\n");
    }
    if (!xStretchOption.isValid()) {
      invalidReason.append(xStretchOption.getDescription() + ": "
          + xStretchOption.getInvalidReason() + "\n");
    }
    if (!localXStretchOption.isValid()) {
      invalidReason.append(localXStretchOption.getDescription() + ": "
          + localXStretchOption.getInvalidReason() + "\n");
    }
    if (!skewOption.isValid()) {
      invalidReason.append(skewOption.getDescription() + ": "
          + skewOption.getInvalidReason() + "\n");
    }
    if (!localSkewOption.isValid()) {
      invalidReason.append(localSkewOption.getDescription() + ": "
          + localSkewOption.getInvalidReason() + "\n");
    }
    if (!surfacesToAnalyze.isValid()) {
      invalidReason.append(surfacesToAnalyze.getDescription() + ": "
          + surfacesToAnalyze.getInvalidReason() + "\n");
    }
    if (!beamTiltOption.isValid()) {
      invalidReason.append(beamTiltOption.getDescription() + ": "
          + beamTiltOption.getInvalidReason() + "\n");
    }
    if (!fixedOrInitialBeamTilt.isValid()) {
      invalidReason.append(fixedOrInitialBeamTilt.getDescription() + ": "
          + fixedOrInitialBeamTilt.getInvalidReason() + "\n");
    }
    return invalidReason.toString();
  }

  public boolean isExcludeListAvailable() {
    return (includeStartEndInc.isDefault() || !includeStartEndInc.valuesSet())
        && includeList.getNElements() == 0;
  }

  public String getCommand() {
    return PROCESS_NAME.toString() + axisID.getExtension() + commandFileExtension;
  }

  public String getCommandLine() {
    return getCommand();
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

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public String[] getCommandArray() {
    String[] array = { getCommandLine() };
    return array;
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
    return null;
  }

  public File getCommandInputFile() {
    return null;
  }

  public int getIntValue(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.USE_OUTPUT_Z_FACTOR_FILE) {
      return useOutputZFactorFile();
    }
    if (fieldInterface == Fields.LOCAL_ALIGNMENTS) {
      return localAlignments.is();
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String getString(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public String[] getStringArray(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Fields.AXIS_Z_SHIFT) {
      return axisZShift.getDouble();
    }
    if (fieldInterface == Fields.ANGLE_OFFSET) {
      return angleOffset.getDouble();
    }
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public Hashtable getHashtable(etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  /**
   * @return Returns the angleOffset.
   */
  public ConstEtomoNumber getAngleOffset() {
    return angleOffset;
  }

  /**
   * @return Returns the axisZShift.
   */
  public ConstEtomoNumber getAxisZShift() {
    return axisZShift;
  }

  /**
   * @return Returns the excludeList.
   */
  public String getExcludeList() {
    return excludeList.toString();
  }

  /**
   * 
   * @return
   */
  public ConstEtomoNumber getFixXYZCoordinates() {
    return fixXYZCoordinates;
  }

  /**
   * @return Returns the imageFile.
   */
  public String getImageFile() {
    return imageFile;
  }

  /**
   * @return Returns the localAlignments.
   */
  public ConstEtomoNumber getLocalAlignments() {
    return localAlignments;
  }

  /**
   * @return Returns the localMagDefaultGrouping.
   */
  public ConstEtomoNumber getLocalMagDefaultGrouping() {
    return localMagDefaultGrouping;
  }

  /**
   * @return Returns the localMagNondefaultGroup.
   */
  public String getLocalMagNondefaultGroup() {
    return ParamUtilities.valueOf(localMagNondefaultGroup);
  }

  /**
   * @return Returns the localMagOption.
   */
  public ConstEtomoNumber getLocalMagOption() {
    return localMagOption;
  }

  /**
   * @return Returns the localRotDefaultGrouping.
   */
  public ConstEtomoNumber getLocalRotDefaultGrouping() {
    return localRotDefaultGrouping;
  }

  /**
   * @return Returns the localRotNondefaultGroup.
   */
  public String getLocalRotNondefaultGroup() {
    return ParamUtilities.valueOf(localRotNondefaultGroup);
  }

  /**
   * @return Returns the localRotOption.
   */
  public ConstEtomoNumber getLocalRotOption() {
    return localRotOption;
  }

  public ConstEtomoNumber getBeamTiltOption() {
    return beamTiltOption;
  }

  public ConstEtomoNumber getFixedOrInitialBeamTilt() {
    return fixedOrInitialBeamTilt;
  }

  /**
   * @return Returns the localSkewDefaultGrouping.
   */
  public ConstEtomoNumber getLocalSkewDefaultGrouping() {
    return localSkewDefaultGrouping;
  }

  /**
   * @return Returns the localSkewNondefaultGroup.
   */
  public String getLocalSkewNondefaultGroup() {
    return ParamUtilities.valueOf(localSkewNondefaultGroup);
  }

  /**
   * @return Returns the localSkewOption.
   */
  public ConstEtomoNumber getLocalSkewOption() {
    return localSkewOption;
  }

  /**
   * @return Returns the localTiltDefaultGrouping.
   */
  public ConstEtomoNumber getLocalTiltDefaultGrouping() {
    return localTiltDefaultGrouping;
  }

  /**
   * @return Returns the localTiltNondefaultGroup.
   */
  public String getLocalTiltNondefaultGroup() {
    return ParamUtilities.valueOf(localTiltNondefaultGroup);
  }

  /**
   * @return Returns the localTiltOption.
   */
  public ConstEtomoNumber getLocalTiltOption() {
    return localTiltOption;
  }

  /**
   * @return Returns the localXStretchDefaultGrouping.
   */
  public ConstEtomoNumber getLocalXStretchDefaultGrouping() {
    return localXStretchDefaultGrouping;
  }

  /**
   * @return Returns the localXStretchNondefaultGroup.
   */
  public String getLocalXStretchNondefaultGroup() {
    return ParamUtilities.valueOf(localXStretchNondefaultGroup);
  }

  /**
   * @return Returns the localXStretchOption.
   */
  public ConstEtomoNumber getLocalXStretchOption() {
    return localXStretchOption;
  }

  /**
   * @return Returns the magDefaultGrouping.
   */
  public ConstEtomoNumber getMagDefaultGrouping() {
    return magDefaultGrouping;
  }

  /**
   * @return Returns the magNondefaultGroup.
   */
  public String getMagNondefaultGroup() {
    return ParamUtilities.valueOf(magNondefaultGroup);
  }

  /**
   * @return Returns the magOption.
   */
  public ConstEtomoNumber getMagOption() {
    return magOption;
  }

  /**
   * @return Returns the magReferenceView.
   */
  public ConstEtomoNumber getMagReferenceView() {
    return magReferenceView;
  }

  /**
   * @return Returns the maximumCycles.
   */
  public ConstEtomoNumber getMaximumCycles() {
    return maximumCycles;
  }

  /**
   * @return Returns the metroFactor.
   */
  public ConstEtomoNumber getMetroFactor() {
    return metroFactor;
  }

  /**
   * @return Returns the minFidsTotalAndEachSurface.
   */
  public String getMinFidsTotalAndEachSurface() {
    return minFidsTotalAndEachSurface.toString(true);
  }

  /**
   * @return Returns the minSizeOrOverlapXandY.
   */
  public String getMinSizeOrOverlapXandY() {
    return minSizeOrOverlapXandY.toString(true);
  }

  /**
   * @return Returns the modelFile.
   */
  public String getModelFile() {
    return modelFile;
  }

  /**
   * @return Returns the numberOfLocalPatchesXandY.
   */
  public String getNumberOfLocalPatchesXandY() {
    return numberOfLocalPatchesXandY.toString(true);
  }

  /**
   * @return true if the parameter was not in the .com file
   */
  public boolean isTargetPatchSizeXandYEmpty() {
    return targetPatchSizeXandY.isEmpty();
  }

  /**
   * @return true if the parameter was not in the .com file
   */
  public boolean isNumberOfLocalPatchesXandYEmpty() {
    return numberOfLocalPatchesXandY.isEmpty();
  }

  /**
   * @return Returns the targetPatchSizeXandY.
   */
  public String getTargetPatchSizeXandY() {
    return targetPatchSizeXandY.toString(true);
  }

  /**
   * @return Returns the outputFidXYZFile.
   */
  public String getOutputFidXYZFile() {
    return outputFidXYZFile;
  }

  public FileType getOutputImageFileType() {
    return FileType.FIDUCIAL_3D_MODEL;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  /**
   * @return Returns the outputLocalFile.
   */
  public String getOutputLocalFile() {
    return outputLocalFile;
  }

  /**
   * @return Returns the outputModelFile.
   */
  public String getOutputModelFile() {
    return outputModelFile;
  }

  /**
   * @return Returns the outputResidualFile.
   */
  public String getOutputResidualFile() {
    return outputResidualFile;
  }

  /**
   * @return Returns the outputTiltFile.
   */
  public String getOutputTiltFile() {
    return outputTiltFile;
  }

  /**
   * @return Returns the outputTransformFile.
   */
  public String getOutputTransformFile() {
    return outputTransformFile;
  }

  /**
   * This must called after skewOption, or localAlignment, and localSkewOption
   * have been set.
   * @return
   */
  public boolean useOutputZFactorFile() {
    return !skewOption.equals(FIXED_OPTION)
        || (localAlignments.is() && !localSkewOption.equals(FIXED_OPTION));
  }

  /**
   * @return Returns the outputZFactorFile.
   */
  public String getOutputZFactorFile() {
    return outputZFactorFile;
  }

  /**
   * build an outputZFactorFile value from datasetName and axisID
   * @param datasetName
   * @param axisID
   * @return
   */
  public static String getOutputZFactorFileName(String datasetName, AxisID axisID) {
    return datasetName + axisID.getExtension() + zFactorFileExtension;
  }

  public static String getOutputLocalFileName(String datasetName, AxisID axisID) {
    return datasetName + axisID.getExtension() + localFileExtension;
  }

  /**
   * @return Returns the projectionStretch.
   */
  public ConstEtomoNumber getProjectionStretch() {
    return projectionStretch;
  }

  /**
   * @return Returns the residualReportCriterion.
   */
  public ConstEtomoNumber getResidualReportCriterion() {
    return residualReportCriterion;
  }

  public boolean isRobustFitting() {
    return robustFitting.is();
  }

  public boolean isWeightWholeTracks() {
    return weightWholeTracks.is();
  }

  public String getKFactorScaling() {
    return kFactorScaling.toString();
  }

  /**
   * @return Returns the rotationAngle.
   */
  public ConstEtomoNumber getRotationAngle() {
    return rotationAngle;
  }

  /**
   * @return Returns the rotationFixedView.
   */
  public ConstEtomoNumber getRotationFixedView() {
    return rotationFixedView;
  }

  /**
   * @return Returns the rotDefaultGrouping.
   */
  public ConstEtomoNumber getRotDefaultGrouping() {
    return rotDefaultGrouping;
  }

  /**
   * @return Returns the rotNondefaultGroup.
   */
  public String getRotNondefaultGroup() {
    return ParamUtilities.valueOf(rotNondefaultGroup);
  }

  /**
   * @return Returns the rotOption.
   */
  public ConstEtomoNumber getRotOption() {
    return rotOption;
  }

  /**
   * @return Returns the separateGroup.
   */
  public String getSeparateGroup() {
    return separateGroup.toString();
  }

  /**
   * @return Returns the skewDefaultGrouping.
   */
  public ConstEtomoNumber getSkewDefaultGrouping() {
    return skewDefaultGrouping;
  }

  /**
   * @return Returns the skewNondefaultGroup.
   */
  public String getSkewNondefaultGroup() {
    return ParamUtilities.valueOf(skewNondefaultGroup);
  }

  /**
   * @return Returns the skewOption.
   */
  public ConstEtomoNumber getSkewOption() {
    return skewOption;
  }

  /**
   * @return Returns the surfacesToAnalyze.
   */
  public ConstEtomoNumber getSurfacesToAnalyze() {
    return surfacesToAnalyze;
  }

  /**
   * @return Returns the tiltAngleSpec.
   */
  public TiltAngleSpec getTiltAngleSpec() {
    return tiltAngleSpec;
  }

  /**
   * @return Returns the tiltDefaultGrouping.
   */
  public ConstEtomoNumber getTiltDefaultGrouping() {
    return tiltDefaultGrouping;
  }

  /**
   * @return Returns the tiltNondefaultGroup.
   */
  public String getTiltNondefaultGroup() {
    return ParamUtilities.valueOf(tiltNondefaultGroup);
  }

  /**
   * @return Returns the tiltOption.
   */
  public ConstEtomoNumber getTiltOption() {
    return tiltOption;
  }

  /**
   * @return Returns the xStretchDefaultGrouping.
   */
  public ConstEtomoNumber getXStretchDefaultGrouping() {
    return xStretchDefaultGrouping;
  }

  /**
   * @return Returns the xStretchNondefaultGroup.
   */
  public String getXStretchNondefaultGroup() {
    return ParamUtilities.valueOf(xStretchNondefaultGroup);
  }

  /**
   * @return Returns the xStretchOption.
   */
  public ConstEtomoNumber getXStretchOption() {
    return xStretchOption;
  }

  /**
   * identifies an old version
   * @return
   */
  public boolean isOldVersion() {
    return loadedFromFile && imagesAreBinned.isNull();
  }

  public static final class Fields implements etomo.comscript.FieldInterface {
    private Fields() {
    }

    public static final Fields USE_OUTPUT_Z_FACTOR_FILE = new Fields();
    public static final Fields LOCAL_ALIGNMENTS = new Fields();
    public static final Fields AXIS_Z_SHIFT = new Fields();
    public static final Fields ANGLE_OFFSET = new Fields();
  }
}

/**
 * <p> $Log$
 * <p> Revision 3.43  2011/02/21 21:22:38  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.42  2010/09/23 21:05:19  sueh
 * <p> bug# 1404 Allowing separateGroup to have multiple entries.
 * <p>
 * <p> Revision 3.41  2010/04/28 15:51:44  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 3.40  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.39  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 3.38  2009/12/11 17:26:22  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 3.37  2009/12/08 02:34:35  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 3.36  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 3.35  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.34  2008/07/16 20:13:31  sueh
 * <p> bug# 1126 Added ROT_ANGLE_KEY.
 * <p>
 * <p> Revision 3.33  2007/12/13 01:03:50  sueh
 * <p> bug# 1056 Changed etomo.comscript.Fields to etomo.comscript.FieldInterface.
 * <p>
 * <p> Revision 3.32  2007/11/06 19:07:52  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 3.31  2007/05/11 15:26:07  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 3.30  2007/03/07 20:59:50  sueh
 * <p> bug# 981 Added beamTiltOption and fixedOrInitialBeamTilt.
 * <p>
 * <p> Revision 3.29  2007/03/03 00:34:10  sueh
 * <p> bug# 973 Added targetPatchSizeXandY.  Added defaults for targetPatchSizeXandY and numberOfLocalPatchesXandY for metadata, since
 * <p> only one of these parameters can be used.
 * <p>
 * <p> Revision 3.28  2007/02/05 21:41:27  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 3.27  2006/08/22 22:45:06  sueh
 * <p> bug# 913 Added single rot option
 * <p>
 * <p> Revision 3.26  2006/05/22 22:36:19  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 3.25  2006/05/11 19:41:43  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 3.24  2006/04/06 18:56:13  sueh
 * <p> bug# 808 Implementing ProcessDetails.  Added Fields to pass requests to
 * <p> the generic gets.
 * <p>
 * <p> Revision 3.23  2006/01/20 20:46:19  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 3.22  2005/11/19 01:52:06  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 3.21  2005/07/21 21:32:14  sueh
 * <p> bug# 532 ConstEtomoNumber.getInvalidReason() is no longer returning
 * <p> the description.
 * <p>
 * <p> Revision 3.20  2005/06/14 21:54:21  sueh
 * <p> bug# 681 Changed fiXYZCoordinates to displayAsInteger.
 * <p>
 * <p> Revision 3.19  2005/06/10 22:48:44  sueh
 * <p> bug# 583, bug# 682 Moved binning calculation to ApplicationManager.
 * <p> Upgraded align.com to have all unbinned parameters and a binning value.
 * <p> Added member variables:  imagesAreBinned, loadedFromFile.  Added
 * <p> function:  isOldVersion.
 * <p>
 * <p> Revision 3.18  2005/05/12 01:20:51  sueh
 * <p> bug# 567 Removed defaults so that fields would always be preserved in
 * <p> the comscript.
 * <p>
 * <p> Revision 3.17  2005/05/09 22:50:05  sueh
 * <p> bug# 658 Adapting to changes in TiltAngleSpec.  Keys are not passed at
 * <p> instanciation.  Short keys are passed to set functions with the regular
 * <p> keys.  Substituted EtomoBoolean2 for EtomoBoolean.  EtomoBoolean is
 * <p> not part of the EtomoNumber family of objects and is not worth keeping
 * <p> up to date.
 * <p>
 * <p> Revision 3.16  2005/04/25 20:39:08  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.15  2005/02/24 00:50:23  sueh
 * <p> bug# 600 Fixed a bug that was saving a value to the wrong parameter.
 * <p>
 * <p> Revision 3.14  2005/02/21 22:53:57  sueh
 * <p> bug# 600 Making parameter string statics public.
 * <p>
 * <p> Revision 3.13  2005/02/18 01:27:23  sueh
 * <p> bug# 600 Moving parameter names to public statics, so they can be used
 * <p> for tooltips.
 * <p>
 * <p> Revision 3.12  2005/01/25 21:36:55  sueh
 * <p> bug# 567 Adding a way to generate OutputLocalFile.  Converting
 * <p> EtomoNumbers to ScriptParameter.
 * <p>
 * <p> Revision 3.11  2005/01/13 19:01:28  sueh
 * <p> bug# 567 Changed isExcludeListAvailable():  taking the null value and the
 * <p> default value into account.
 * <p>
 * <p> Revision 3.10  2005/01/13 00:44:45  sueh
 * <p> bug# 576 Converted includeStartEndInc to FortranInputString.
 * <p>
 * <p> Revision 3.9  2005/01/12 00:41:12  sueh
 * <p> bug# 579 Make localAlignments available to the Command interface.
 * <p>
 * <p> Revision 3.8  2005/01/11 20:15:25  sueh
 * <p> bug# 567 Added fixXYZCoordinates, localMagReferenceView, and
 * <p> localOutputOptions.
 * <p>
 * <p> Revision 3.7  2005/01/11 01:00:07  sueh
 * <p> bug# 567 Adding storage for outputModelAndResidual, in case we want to
 * <p> use it.
 * <p>
 * <p> Revision 3.6  2005/01/08 01:36:02  sueh
 * <p> bug# 578 Removed getBinning() since its not needed by Command.
 * <p> Removed Command access to skewOption and xStretchOption.
 * <p> Add Command access to useOutputZFactorFile().
 * <p>
 * <p> Revision 3.5  2005/01/06 17:59:54  sueh
 * <p> bug# 578 Changed getIntegerValue() to give access to skewOption and
 * <p> xStretchOption  to Command interface.
 * <p>
 * <p> Revision 3.4  2005/01/05 18:56:00  sueh
 * <p> bug# 578 Adding AxisID to constructor.  Implementing Command.
 * <p>
 * <p> Revision 3.3  2004/12/29 23:30:51  sueh
 * <p> bug# 567 Corrected a parameter name in the constructor.
 * <p>
 * <p> Revision 3.2  2004/12/29 01:51:57  sueh
 * <p> bug# 567 Adding reset value for rotDefaultGrouping.  Getting
 * <p> rotNondefaultGroup as a string.
 * <p>
 * <p> Revision 3.1  2004/12/28 23:59:58  sueh
 * <p> bug# 567 Placed the version of ConstTiltalignParam for the old-style comscript
 * <p> into OldTiltalignParam.  This version contains only the new parameters,
 * <p> get functions, reset(), and validate().
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/10/14 20:30:43  rickg
 * <p> Bug#279  Label layout and name changes
 * <p>DefaultGrouping
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/24 01:09:41  rickg
 * <p> Min local patch size changed to double
 * <p>
 * <p> Revision 1.4  2002/12/18 19:07:09  rickg
 * <p> Added getters for metro factor and cycle limit
 * <p>
 * <p> Revision 1.3  2002/12/10 21:37:21  rickg
 * <p> changed reportStddevThreshold to residualThreshold
 * <p>
 * <p> Revision 1.2  2002/12/03 05:22:56  rickg
 * <p> added getLocalRotationSolutionGroupSize
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
