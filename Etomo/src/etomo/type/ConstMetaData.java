package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.ApplicationManager;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.SqueezevolParam;
import etomo.comscript.TiltParam;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TransferfidParam;
import etomo.comscript.TrimvolParam;
import etomo.storage.autodoc.Autodoc;

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
 */
public abstract class ConstMetaData extends BaseMetaData {
  public static final String rcsid = "$Id$";

  private static final String latestRevisionNumber = "1.7";
  private static final String newTomogramTitle = "Setup Tomogram";

  protected static final String TOMO_GEN_A_TILT_PARALLEL_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName()
      + AxisID.FIRST.getExtension().toUpperCase() + ".Tilt.Parallel";
  protected static final String TOMO_GEN_B_TILT_PARALLEL_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName()
      + AxisID.SECOND.getExtension().toUpperCase() + ".Tilt.Parallel";
  protected static final String COMBINE_VOLCOMBINE_PARALLEL_GROUP = DialogType.TOMOGRAM_COMBINATION
      .getStorableName()
      + ".Volcombine.Parallel";
  protected static final String B_STACK_PROCESSED_GROUP = "BStackProcessed";
  private static final int DEFAULT_SAMPLE_THICKNESS = 200;

  private final ApplicationManager manager;

  protected String datasetName = "";
  protected String backupDirectory = "";
  protected String distortionFile = "";
  protected String magGradientFile = "";

  protected DataSource dataSource = DataSource.CCD;
  protected ViewType viewType = ViewType.SINGLE_VIEW;

  protected double pixelSize = Double.NaN;
  protected boolean useLocalAlignmentsA = true;
  protected boolean useLocalAlignmentsB = true;
  protected double fiducialDiameter = Double.NaN;
  protected float imageRotationA = Float.NaN;
  protected float imageRotationB = Float.NaN;
  protected int binning = Integer.MIN_VALUE;
  protected boolean fiducialessAlignmentA = false;
  protected boolean fiducialessAlignmentB = false;
  protected boolean wholeTomogramSampleA = false;
  protected boolean wholeTomogramSampleB = false;
  //binning values - null if missing
  protected EtomoNumber tomoPosBinningA = new EtomoNumber(
      EtomoNumber.Type.INTEGER, "TomoPosBinningA");
  protected EtomoNumber tomoPosBinningB = new EtomoNumber(
      EtomoNumber.Type.INTEGER, "TomoPosBinningB");
  protected EtomoNumber tomoGenBinningA = new EtomoNumber(
      EtomoNumber.Type.INTEGER, "TomoGenBinningA");
  protected EtomoNumber tomoGenBinningB = new EtomoNumber(
      EtomoNumber.Type.INTEGER, "TomoGenBinningB");

  //  Axis specific data
  protected TiltAngleSpec tiltAngleSpecA = new TiltAngleSpec();
  protected String excludeProjectionsA = "";

  protected TiltAngleSpec tiltAngleSpecB = new TiltAngleSpec();
  protected String excludeProjectionsB = "";
  protected EtomoBoolean2 useZFactorsA = new EtomoBoolean2("UseZFactorsA");
  protected EtomoBoolean2 useZFactorsB = new EtomoBoolean2("UseZFactorsB");
  protected EtomoBoolean2 adjustedFocusA = new EtomoBoolean2("AdjustedFocusA");
  protected EtomoBoolean2 adjustedFocusB = new EtomoBoolean2("AdjustedFocusB");

  protected boolean comScriptsCreated = false;

  protected CombineParams combineParams;
  protected final TrimvolParam trimvolParam;
  protected final SqueezevolParam squeezevolParam;
  protected final TransferfidParam transferfidParamA;
  protected final TransferfidParam transferfidParamB;
  protected final EtomoBoolean2 defaultParallel = new EtomoBoolean2(
      "DefaultParallel");
  protected EtomoBoolean2 tomoGenTiltParallelA = null;
  protected EtomoBoolean2 tomoGenTiltParallelB = null;
  protected EtomoBoolean2 combineVolcombineParallel = null;
  protected EtomoBoolean2 bStackProcessed = null;
  private StringBuffer message = new StringBuffer();
  protected final EtomoNumber sampleThicknessA = new EtomoNumber(AxisID.FIRST
      .toString()
      + '.' + ProcessName.SAMPLE + '.' + ConstTiltParam.THICKNESS_KEY);
  protected final EtomoNumber sampleThicknessB = new EtomoNumber(AxisID.SECOND
      .toString()
      + '.' + ProcessName.SAMPLE + '.' + ConstTiltParam.THICKNESS_KEY);
  protected String firstAxisPrepend = null;
  protected String secondAxisPrepend = null;
  protected final TiltParam.Storables tiltParamA = new TiltParam.Storables();
  protected final TiltParam.Storables tiltParamB = new TiltParam.Storables();
  protected String targetPatchSizeXandY = "";
  protected String numberOfLocalPatchesXandY = "";
  protected final EtomoBoolean2 noBeamTiltSelectedA = new EtomoBoolean2(
      AxisID.FIRST.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName() + ".NoBeamTiltSelected");
  protected final EtomoBoolean2 fixedBeamTiltSelectedA = new EtomoBoolean2(
      AxisID.FIRST.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName()
          + ".FixedBeamTiltSelected");
  protected final EtomoNumber fixedBeamTiltA = new EtomoNumber(
      EtomoNumber.Type.FLOAT, AxisID.FIRST.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName() + ".FixedBeamTilt");
  protected final EtomoBoolean2 noBeamTiltSelectedB = new EtomoBoolean2(
      AxisID.SECOND.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName() + ".NoBeamTiltSelected");
  protected final EtomoBoolean2 fixedBeamTiltSelectedB = new EtomoBoolean2(
      AxisID.SECOND.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName()
          + ".FixedBeamTiltSelected");
  protected final EtomoNumber fixedBeamTiltB = new EtomoNumber(
      EtomoNumber.Type.FLOAT, AxisID.SECOND.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName() + ".FixedBeamTilt");

  public abstract void load(Properties props);

  public abstract void load(Properties props, String prepend);

  public ConstMetaData(ApplicationManager manager) {
    this.manager = manager;
    squeezevolParam = new SqueezevolParam(manager);
    combineParams = new CombineParams(manager);
    trimvolParam = new TrimvolParam(manager);
    transferfidParamA = new TransferfidParam(manager, AxisID.FIRST);
    transferfidParamB = new TransferfidParam(manager, AxisID.SECOND);
    fileExtension = ".edf";
    useZFactorsA.setDisplayValue(true);
    useZFactorsB.setDisplayValue(true);
    sampleThicknessA.setDisplayValue(DEFAULT_SAMPLE_THICKNESS);
    sampleThicknessB.setDisplayValue(DEFAULT_SAMPLE_THICKNESS);
    noBeamTiltSelectedA.setDisplayValue(true);//backwards compatibility
    noBeamTiltSelectedB.setDisplayValue(true);//backwards compatibility
  }

  String getFirstAxisPrepend() {
    return firstAxisPrepend;
  }

  String getSecondAxisPrepend() {
    return secondAxisPrepend;
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props, String prepend) {
    String group;
    if (prepend == "") {
      prepend = "Setup";
    }
    else {
      prepend += ".Setup";
    }
    group = prepend + ".";
    props.setProperty(group + "RevisionNumber", latestRevisionNumber);
    props.setProperty(group + "ComScriptsCreated", String
        .valueOf(comScriptsCreated));
    props.setProperty(group + "DatasetName", datasetName);
    props.setProperty(group + "BackupDirectory", backupDirectory);

    props.setProperty(group + "DataSource", dataSource.toString());
    props.setProperty(group + "AxisType", axisType.toString());
    props.setProperty(group + "ViewType", viewType.toString());

    props.setProperty(group + "PixelSize", String.valueOf(pixelSize));
    props.setProperty(group + "UseLocalAlignmentsA", String
        .valueOf(useLocalAlignmentsA));
    props.setProperty(group + "UseLocalAlignmentsB", String
        .valueOf(useLocalAlignmentsB));
    props.setProperty(group + "FiducialDiameter", String
        .valueOf(fiducialDiameter));
    props.setProperty(group + "ImageRotationA", String.valueOf(imageRotationA));
    props.setProperty(group + "ImageRotationB", String.valueOf(imageRotationB));
    tiltAngleSpecA.store(props, group + "AxisA");
    props.setProperty(group + "AxisA.ExcludeProjections", String
        .valueOf(excludeProjectionsA));

    tiltAngleSpecB.store(props, group + "AxisB");
    props.setProperty(group + "AxisB.ExcludeProjections", String
        .valueOf(excludeProjectionsB));

    combineParams.store(props, group);
    props.setProperty(group + "DistortionFile", distortionFile);
    props.setProperty(group + "MagGradientFile", magGradientFile);
    props.setProperty(group + "Binning", String.valueOf(binning));
    props.setProperty(group + "FiducialessAlignmentA", String
        .valueOf(fiducialessAlignmentA));
    props.setProperty(group + "FiducialessAlignmentB", String
        .valueOf(fiducialessAlignmentB));
    props.setProperty(group + "WholeTomogramSampleA", String
        .valueOf(wholeTomogramSampleA));
    props.setProperty(group + "WholeTomogramSampleB", String
        .valueOf(wholeTomogramSampleB));
    trimvolParam.store(props, group);
    squeezevolParam.store(props, prepend);
    useZFactorsA.store(props, prepend);
    useZFactorsB.store(props, prepend);
    transferfidParamA.store(props, prepend);
    transferfidParamB.store(props, prepend);
    tomoPosBinningA.store(props, prepend);
    tomoPosBinningB.store(props, prepend);
    tomoGenBinningA.store(props, prepend);
    tomoGenBinningB.store(props, prepend);
    if (tomoGenTiltParallelA != null) {
      tomoGenTiltParallelA.store(props, prepend);
    }
    if (tomoGenTiltParallelB != null) {
      tomoGenTiltParallelB.store(props, prepend);
    }
    if (combineVolcombineParallel != null) {
      combineVolcombineParallel.store(props, prepend);
    }
    if (bStackProcessed != null) {
      bStackProcessed.store(props, prepend);
    }
    defaultParallel.store(props, prepend);
    sampleThicknessA.store(props, prepend);
    sampleThicknessB.store(props, prepend);
    tiltParamA.store(props, group + firstAxisPrepend);
    tiltParamB.store(props, group + secondAxisPrepend);
    props.setProperty(group + Autodoc.TILTALIGN + "."
        + TiltalignParam.TARGET_PATCH_SIZE_X_AND_Y_KEY, targetPatchSizeXandY);
    props.setProperty(group + Autodoc.TILTALIGN + "."
        + TiltalignParam.NUMBER_OF_LOCAL_PATCHES_X_AND_Y_KEY,
        numberOfLocalPatchesXandY);
    noBeamTiltSelectedA.store(props, prepend);
    fixedBeamTiltSelectedA.store(props, prepend);
    fixedBeamTiltA.store(props, prepend);
    noBeamTiltSelectedB.store(props, prepend);
    fixedBeamTiltSelectedB.store(props, prepend);
    fixedBeamTiltB.store(props, prepend);
  }

  public ConstEtomoNumber getNoBeamTiltSelected(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return noBeamTiltSelectedB;
    }
    return noBeamTiltSelectedA;
  }

  public ConstEtomoNumber getFixedBeamTiltSelected(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fixedBeamTiltSelectedB;
    }
    return fixedBeamTiltSelectedA;
  }

  public ConstEtomoNumber getFixedBeamTilt(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fixedBeamTiltB;
    }
    return fixedBeamTiltA;
  }

  public String getTargetPatchSizeXandY() {
    return targetPatchSizeXandY;
  }

  public String getNumberOfLocalPatchesXandY() {
    return numberOfLocalPatchesXandY;
  }

  public void getTiltParam(TiltParam tiltParam, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      tiltParam.set(tiltParamB);
    }
    else {
      tiltParam.set(tiltParamA);
    }
  }

  public TrimvolParam getTrimvolParam() {
    return trimvolParam;
  }

  public SqueezevolParam getSqueezevolParam() {
    return squeezevolParam;
  }

  public void getTransferfidAFields(TransferfidParam transferfidParam) {
    this.transferfidParamA.getStorableFields(transferfidParam);
  }

  public void getTransferfidBFields(TransferfidParam transferfidParam) {
    this.transferfidParamB.getStorableFields(transferfidParam);
  }

  public String getDatasetName() {
    return datasetName;
  }

  public String getMetaDataFileName() {
    if (datasetName.equals("")) {
      return "";
    }
    return datasetName + fileExtension;
  }

  public String getName() {
    if (datasetName.equals("")) {
      return newTomogramTitle;
    }
    return datasetName;
  }

  public static String getNewFileTitle() {
    return newTomogramTitle;
  }

  public String getBackupDirectory() {
    return backupDirectory;
  }

  public String getDistortionFile() {
    return distortionFile;
  }

  public String getMagGradientFile() {
    return magGradientFile;
  }

  public ConstEtomoNumber getAdjustedFocusA() {
    return adjustedFocusA;
  }

  public ConstEtomoNumber getAdjustedFocusB() {
    return adjustedFocusB;
  }

  public DataSource getDataSource() {
    return dataSource;
  }

  public ViewType getViewType() {
    return viewType;
  }

  public double getPixelSize() {
    return pixelSize;
  }

  public boolean getUseLocalAlignments(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return useLocalAlignmentsB;
    }
    return useLocalAlignmentsA;
  }

  public ConstEtomoNumber getTomoPosBinning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomoPosBinningB;
    }
    return tomoPosBinningA;
  }

  public ConstEtomoNumber getTomoGenBinning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomoGenBinningB;
    }
    return tomoGenBinningA;
  }

  public ConstEtomoNumber getCombineVolcombineParallel() {
    return combineVolcombineParallel;
  }

  public ConstEtomoNumber getTomoGenTiltParallel(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomoGenTiltParallelB;
    }
    return tomoGenTiltParallelA;
  }

  public ConstEtomoNumber getDefaultParallel() {
    return defaultParallel;
  }

  public ConstEtomoNumber getUseZFactors(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return useZFactorsB;
    }
    return useZFactorsA;
  }

  public double getFiducialDiameter() {
    return fiducialDiameter;
  }

  public float getImageRotation(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return imageRotationB;
    }
    return imageRotationA;
  }

  public int getBinning() {
    return binning;
  }

  public ConstEtomoNumber getBStackProcessed() {
    return bStackProcessed;
  }

  public EtomoNumber getSampleThickness(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sampleThicknessB;
    }
    return sampleThicknessA;
  }

  public TiltAngleSpec getTiltAngleSpecA() {
    return tiltAngleSpecA;
  }

  public TiltAngleSpec getTiltAngleSpecB() {
    return tiltAngleSpecB;
  }

  public String getExcludeProjectionsA() {
    return excludeProjectionsA;
  }

  public String getExcludeProjectionsB() {
    return excludeProjectionsB;
  }

  public boolean getComScriptCreated() {
    return comScriptsCreated;
  }

  public boolean isFiducialessAlignment(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fiducialessAlignmentB;
    }
    return fiducialessAlignmentA;
  }

  public boolean isDistortionCorrection() {
    return !distortionFile.equals("") || !magGradientFile.equals("");
  }

  public boolean isWholeTomogramSample(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return wholeTomogramSampleB;
    }
    return wholeTomogramSampleA;
  }

  public ConstCombineParams getConstCombineParams() {
    return combineParams;
  }

  public CombineParams getCombineParams() {
    return combineParams;
  }

  public boolean isValid() {
    return isValid(true, null);
  }

  public boolean isValid(boolean fromScreen) {
    return isValid(fromScreen, null);
  }

  public boolean isValid(File paramFile) {
    return isValid(false, paramFile);
  }

  public boolean isValid(boolean fromScreen, File paramFile) {
    invalidReason = "";

    String helpString;
    if (!fromScreen) {
      helpString = "  Check the Etomo data file.";
    }
    else {
      helpString = "";
    }

    if (axisType == null || axisType == AxisType.NOT_SET) {
      invalidReason = "Axis type should be either Dual Axis or Single Axis."
          + helpString;
      return false;
    }

    if (!isDatasetNameValid(paramFile)) {
      invalidReason += helpString;
      return false;
    }

    // Is the pixel size greater than zero
    if (fromScreen && pixelSize <= 0.0) {
      invalidReason = "Pixel size is not greater than zero.";
      return false;
    }

    // Is the fiducial diameter greater than zero
    if (fromScreen && fiducialDiameter <= 0.0) {
      invalidReason = "Fiducial diameter is not greater than zero.";
      return false;
    }

    return true;
  }

  public boolean isDatasetNameValid() {
    return isDatasetNameValid(null);
  }

  public boolean isDatasetNameValid(File paramFile) {
    invalidReason = "";
    if (datasetName.equals("")) {
      invalidReason = "Dataset name has not been set.";
      return false;
    }
    if (paramFile == null) {
      if (getValidDatasetDirectory(manager.getPropertyUserDir()) != null) {
        return true;
      }
    }
    else {
      if (getValidDatasetDirectory(new File(paramFile.getParent())
          .getAbsolutePath()) != null) {
        return true;
      }
    }
    return false;
  }

  public File getValidDatasetDirectory(String workingDirName) {
    // Does the working directory exist
    // If is doesn't then use the backup directory.    
    File workingDir = new File(workingDirName);
    File backupDir = new File(backupDirectory);
    File currentDir;

    //find a valid directory and set directory and type
    if (isValid(workingDir, true)) {
      currentDir = workingDir;
    }
    else if (isValid(backupDir, true)) {
      currentDir = backupDir;
    }
    else {
      //can't find a valid directory, report error

      //if no directory exists then exit
      if (!workingDir.exists() && !backupDir.exists()) {
        invalidReason = "The working directory: "
            + workingDir.getAbsolutePath() + " and the backup directory: "
            + backupDir.getAbsolutePath() + " do not exist";
        return null;
      }

      //decide which directory to complain about:
      //complain about the working directory, if it exists
      if (workingDir.exists()) {
        currentDir = workingDir;
      }
      else {
        currentDir = backupDir;
      }

      if (!currentDir.canRead()) {
        invalidReason = "Can't read " + currentDir.getAbsolutePath()
            + " directory";
        return null;
      }

      if (!currentDir.canWrite()) {
        invalidReason = "Can't write " + currentDir.getAbsolutePath()
            + " directory";
        return null;
      }

      throw new IllegalStateException("Working directory ="
          + workingDir.toString() + ",backupDir=" + backupDir.toString());
    }

    // Does the appropriate image stack exist in the working directory
    if (axisType == AxisType.DUAL_AXIS) {
      currentDir = findValidFile(datasetName + "a.st", currentDir, backupDir);
    }
    else {
      currentDir = findValidFile(datasetName + ".st", currentDir, backupDir);
    }
    return currentDir;
  }

  /**
   * Checks a file's state.  Checks whether a file exists and
   * is readable.  Optionally checks whether a file is
   * writable.
   * 
   * @param file
   * @param writeable - If true, the file must be writeable
   * @return boolean
   */
  protected static boolean isValid(File file, boolean writeable) {
    if (file == null) {
      return false;
    }
    if (!file.exists()) {
      return false;
    }

    return file.canRead() && (!writeable || file.canWrite());
  }

  protected void appendMessage(String string) {
    message.append(string);
  }

  /**
   * Finds a file in either the current directory or an
   * alternate directory.  The file must be readable.
   * 
   * The current directory state variable can point to the 
   * alternative directory state instance.
   * In this case, only the alternative directory is checked.
   * 
   * Side Effect:
   * If the function returns null, it places an error message
   * into ConstMetaData.invalidReason.
   * 
   * @param fileName - Name of file to look for
   * @param curDir - The current directory.  This directory should be valid.
   * @param altDir - The alternate directory.
   * @return Success:  directory where file found.  Failure: null.
   * @throws IllegalArgumentException if any parameter is null
   */
  protected File findValidFile(String fileName, File curDir, File altDir) {
    if (fileName == null || curDir == null || altDir == null
        || !isValid(curDir, true)) {
      throw new IllegalArgumentException(
          "ConstMetaData.findValidFile(String,File,File)");
    }

    // Does the appropriate image stack exist in the working or backup directory
    File file = new File(curDir, fileName);
    while (!file.exists()) {
      if (curDir == altDir || !isValid(altDir, true)) {
        message.append(fileName + " does not exist in  "
            + curDir.getAbsolutePath());
        invalidReason = message.toString();
        message = new StringBuffer();
        return null;
      }
      curDir = altDir;
      file = new File(curDir, fileName);
    }

    if (!file.canRead()) {
      invalidReason = "Can't read " + fileName;
      return null;
    }

    return curDir;
  }

  /**
   * Finds a file in the current directory.  The file must be readable.
   * 
   * Side Effect:
   * If the function returns null, it places an error message
   * into ConstMetaData.invalidReason.
   * 
   * @param fileName - Name of file to look for
   * @param curDir - The current directory.  This directory should be valid.
   * @return Success:  directory where file found.  Failure: null.
   * @throws IllegalArgumentException if any parameter is null
   */
  protected File findValidFile(String fileName, File curDir) {
    if (fileName == null || curDir == null || !isValid(curDir, true)) {
      throw new IllegalArgumentException(
          "ConstMetaData.findValidFile(String,File)");
    }

    // Does the appropriate image stack exist in the working or backup directory
    File file = new File(curDir, fileName);

    if (!file.exists()) {
      invalidReason = fileName + " does not exist in "
          + curDir.getAbsolutePath();
      return null;
    }

    if (!file.canRead()) {
      invalidReason = "Can't read " + fileName;
      return null;
    }

    return curDir;
  }

  public boolean equals(Object object) {
    if (!(object instanceof ConstMetaData))
      return false;

    ConstMetaData cmd = (ConstMetaData) object;
    if (!datasetName.equals(cmd.getDatasetName()))
      return false;
    if (!backupDirectory.equals(cmd.getBackupDirectory()))
      return false;
    if (!distortionFile.equals(cmd.getDistortionFile()))
      return false;
    if (!dataSource.equals(cmd.getDataSource()))
      return false;
    if (axisType != cmd.getAxisType())
      return false;
    if (!viewType.equals(cmd.getViewType()))
      return false;
    if (!(pixelSize == cmd.getPixelSize()))
      return false;
    if (!(useLocalAlignmentsA == cmd.getUseLocalAlignments(AxisID.FIRST)))
      return false;
    if (!(useLocalAlignmentsB == cmd.getUseLocalAlignments(AxisID.SECOND)))
      return false;
    if (!(fiducialDiameter == cmd.getFiducialDiameter()))
      return false;
    if (!(imageRotationA == cmd.getImageRotation(AxisID.FIRST)))
      return false;
    if (!(imageRotationB == cmd.getImageRotation(AxisID.SECOND)))
      return false;
    if (!(binning == cmd.getBinning()))
      return false;
    if (!(fiducialessAlignmentA == cmd.isFiducialessAlignment(AxisID.FIRST)))
      return false;
    if (!(fiducialessAlignmentB == cmd.isFiducialessAlignment(AxisID.SECOND)))
      return false;

    // TODO tilt angle spec needs to be more complete
    if (!(tiltAngleSpecA.getType() == cmd.getTiltAngleSpecA().getType()))
      return false;
    if (!excludeProjectionsA.equals(cmd.getExcludeProjectionsA()))
      return false;

    if (!(tiltAngleSpecB.getType() == cmd.getTiltAngleSpecB().getType()))
      return false;
    if (!excludeProjectionsB.equals(cmd.getExcludeProjectionsB()))
      return false;
    if (!(comScriptsCreated == cmd.getComScriptCreated()))
      return false;
    if (!combineParams.equals(cmd.getConstCombineParams())) {
      return false;
    }
    if (!trimvolParam.equals(cmd.getTrimvolParam())) {
      return false;
    }
    if (!squeezevolParam.equals(cmd.getSqueezevolParam())) {
      return false;
    }
    if (!tomoPosBinningA.equals(cmd.tomoPosBinningA)) {
      return false;
    }
    if (!tomoPosBinningB.equals(cmd.tomoPosBinningB)) {
      return false;
    }
    if (!tomoGenBinningA.equals(cmd.tomoGenBinningA)) {
      return false;
    }
    if (!tomoGenBinningB.equals(cmd.tomoGenBinningB)) {
      return false;
    }
    return true;
  }
}

/**
 * <p> $Log$
 * <p> Revision 3.39  2007/03/03 01:00:46  sueh
 * <p> bug# 973 Added targetPatchSizeXandY and numberOfLocalPatchesXandY.
 * <p>
 * <p> Revision 3.38  2007/02/05 23:24:53  sueh
 * <p> bug# 962 Added Model and Rejoin fields.
 * <p>
 * <p> Revision 3.37  2006/09/19 22:32:39  sueh
 * <p> bug# 920 Added first and second axisPrepends for storing axis-level values.
 * <p> Added TiltParam.Storables for A and B.
 * <p>
 * <p> Revision 3.36  2006/05/11 19:57:02  sueh
 * <p> bug# 838 Added sample thickness.
 * <p>
 * <p> Revision 3.35  2006/03/23 19:45:54  sueh
 * <p> bug# 609 Improving the error message when a dual axis image stack does
 * <p> not end in a.st.
 * <p>
 * <p> Revision 3.34  2006/03/16 01:53:41  sueh
 * <p> bug# 828 Added getCombineParams().
 * <p>
 * <p> Revision 3.33  2005/12/13 02:16:25  sueh
 * <p> bug# 773 Added defaultParallel
 * <p>
 * <p> Revision 3.32  2005/10/27 00:32:22  sueh
 * <p> bug# 725 Added bStackProcessed.
 * <p>
 * <p> Revision 3.31  2005/10/12 21:24:16  sueh
 * <p> bug# 532 Changed the parallel booleans to EtomoBoolean2 so that they
 * <p> can remember whether they where set or not.  The default for the parallel
 * <p> checkboxes is based on the existance and validity of cpu.adoc.
 * <p>
 * <p> Revision 3.30  2005/09/29 18:45:29  sueh
 * <p> bug# 532 Saving the state of the parallel checkbox states.
 * <p>
 * <p> Revision 3.29  2005/09/16 17:50:23  sueh
 * <p> bug# 532 Added combineParallelProcess.
 * <p>
 * <p> Revision 3.28  2005/09/02 18:58:55  sueh
 * <p> bug# 720 Pass the manager to TrimvolParam instead of propertyUserDir
 * <p> because TrimvolParam is constructed by MetaData before
 * <p> propertyUserDir is set.
 * <p>
 * <p> Revision 3.27  2005/08/22 17:10:27  sueh
 * <p> bug# 532 Added a member variable to save the setting of the gen tomo
 * <p> parallel process checkbox.
 * <p>
 * <p> Revision 3.26  2005/07/29 00:53:15  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.25  2005/06/10 23:24:38  sueh
 * <p> bug# 583, bug# 677, bug# 584, bug# 679, bug# 683  Storing
 * <p> screen binning for Tomo Pos and Tomo Gen in MetaData separately.
 * <p> Added member variables:  tomoGenBinningA, tomoGenBinningB,
 * <p> tomoPosBinningA, tomoPosBinningB.
 * <p>
 * <p> Revision 3.24  2005/04/07 21:58:02  sueh
 * <p> bug# 626 Added isDistortionCorrection(), which returns true if magGradient
 * <p> or image distortion files are set.
 * <p>
 * <p> Revision 3.23  2005/03/02 20:25:03  sueh
 * <p> bug# 533 Added adjustedFocus.  Only used with montaging and mag
 * <p> gradient correction.
 * <p>
 * <p> Revision 3.22  2005/03/02 00:12:01  sueh
 * <p> bug# 611 Added mag gradients correction file.
 * <p>
 * <p> Revision 3.21  2005/02/19 00:09:01  sueh
 * <p> bug# 606 Removed MetaData (Setup) zfactors, fiducialess, wholetomogram,
 * <p> and localalignments.  Add them for A and B.
 * <p>
 * <p> Revision 3.20  2005/02/15 21:05:30  sueh
 * <p> bug# 603 Removed SectionType (single or serial sections).
 * <p>
 * <p> Revision 3.19  2005/01/25 22:07:48  sueh
 * <p> Converting useZFactors to EtomoBoolean2.
 * <p>
 * <p> Revision 3.18  2005/01/21 23:06:48  sueh
 * <p> bug# 509 bug# 591  Removed transferfidNumberViews.  Added
 * <p> transferfidParamA and transferfidParamB to hold the user-modifiable
 * <p> transferfid values.  Removed initializeTransferfidParam() and added
 * <p> getTransferfidParamAFields() and getTransferfidParamBFields() to get/set
 * <p> storable fields in TransferfidParam.
 * <p>
 * <p> Revision 3.17  2005/01/12 00:42:45  sueh
 * <p> bug# 579 Make the reset value on useZFactors true.
 * <p>
 * <p> Revision 3.16  2005/01/11 18:07:27  sueh
 * <p> bug# 578 Added useZFactors.
 * <p>
 * <p> Revision 3.15  2004/12/14 21:45:05  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 3.14  2004/12/08 21:30:21  sueh
 * <p> bug# 564 Added access to TomogramState member variable.
 * <p>
 * <p> Revision 3.13  2004/12/07 22:47:38  sueh
 * <p> bug# 564 Added TomogramState member variable.
 * <p>
 * <p> Revision 3.12  2004/12/02 18:29:16  sueh
 * <p> bug# 557 Added a SqueezevolParam instance to be stored in the .edf file.
 * <p>
 * <p> Revision 3.11  2004/11/19 23:33:52  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.10.4.4  2004/11/19 00:15:59  sueh
 * <p> bug# 520 Changed the file extension to contain the period.
 * <p>
 * <p> Revision 3.10.4.3  2004/10/11 02:07:25  sueh
 * <p> bug# 520 Fixed a bug in ConstMetaData where the open edf file menu
 * <p> item wasn't working because it was validating the propertyUserDir of the
 * <p> current manager, not the parent of the edf file being opened.  Now able
 * <p> to pass in the edf file to get the parent from to use in validation.
 * <p>
 * <p> Revision 3.10.4.2  2004/10/01 19:47:26  sueh
 * <p> bug# 520 provide a standard way to get the identifier of a meta data file
 * <p> (getName).  Define a new join string that will go in the menu.  Set a file
 * <p> extension value.
 * <p>
 * <p> Revision 3.10.4.1  2004/09/29 19:23:26  sueh
 * <p> bug# 520 Added base class BaseMetaData.  Made
 * <p> latestRevisionNumber static.  Moved revision functionality to base class.
 * <p> Moved axisType and invalid reason to base class.  Moved store()
 * <p> functions to this class.  Implemented Storable with abstract load
 * <p> functions.
 * <p>
 * <p> Revision 3.10  2004/06/22 02:01:52  sueh
 * <p> bug# 441 added TrimvolParam, updated equals().
 * <p>
 * <p> Revision 3.9  2004/06/01 18:54:49  rickg
 * <p> Bug #391 whole tomogram sampling state implementation
 * <p>
 * <p> Revision 3.8  2004/05/25 23:57:49  sueh
 * <p> bug# 355 Change isValid() so it can be used with setup dialog
 * <p> or a .edf file.  Tell the user to check the .edf file, when necessary.
 * <p>
 * <p> Revision 3.7  2004/05/25 23:23:40  rickg
 * <p> Bug #391 method refactor
 * <p>
 * <p> Revision 3.6  2004/04/06 03:00:40  rickg
 * <p> Updated imageRotation to store axis separately
 * <p>
 * <p> Revision 3.5  2004/02/24 18:52:22  sueh
 * <p> bug# 385 initialized binning to null
 * <p>
 * <p> Revision 3.4  2004/02/20 23:44:45  sueh
 * <p> bug# 386 added distortionFile and binning
 * <p>
 * <p> Revision 3.3  2004/02/07 03:10:10  sueh
 * <p> bug# 169 Created dataset validation function that returns
 * <p> the valid directory
 * <p>
 * <p> Revision 3.2  2003/12/23 21:53:16  sueh
 * <p> bug# 371 Remove validation test for b stack.
 * <p>
 * <p> Revision 3.1  2003/12/08 22:31:16  sueh
 * <p> bug# 169 adding a new function isDatasetNameValid.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.11  2003/10/28 02:23:59  sueh
 * <p> bug267
 * <p>
 * <p> Revision 2.10  2003/10/23 23:04:59  sueh
 * <p> bug267 removing prints
 * <p>
 * <p> Revision 2.9  2003/10/23 22:34:35  sueh
 * <p> bug267 removing prints
 * <p>
 * <p> Revision 2.8  2003/10/23 19:11:16  sueh
 * <p> bug267 look for stack in the working directory and the
 * <p> backup directory when validating directories.
 * <p>
 * <p> Revision 2.7  2003/10/06 22:35:18  sueh
 * <p> transferfidNumberViews needs a default because there is
 * <p> no conscript
 * <p>
 * <p> Revision 2.6  2003/09/26 19:46:16  sueh
 * <p> bug223 removed task marks
 * <p>
 * <p> Revision 2.5  2003/09/26 19:43:48  sueh
 * <p> bug223 no field should be persistant.  Changed MetaData.
 * <p> Added TransferfidNumberViews.
 * <p> Changed the done fine allignment and open fine allignment functions
 * <p> to work with MetaData
 * <p>
 * <p> Revision 2.4  2003/05/12 01:24:24  rickg
 * <p> Return invalid working directory in reason
 * <p>
 * <p> Revision 2.3  2003/05/07 17:54:08  rickg
 * <p> Working direcotry is no longer stored in the metadata
 * <p> System property user.dir now defines the working directory
 * <p>
 * <p> Revision 2.2  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.1  2003/03/18 23:47:20  rickg
 * <p> Changed method name to get CombineParams reference
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:37:54  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/10/08 23:53:42  rickg
 * <p> getCombineParams now returns a ConstCombineParam object
 * <p>
 * <p> Revision 1.2  2002/09/30 23:48:32  rickg
 * <p> Reformatted after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
