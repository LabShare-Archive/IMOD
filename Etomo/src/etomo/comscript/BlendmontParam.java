package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.logic.DatasetTool;
import etomo.logic.TomogramTool;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.EtomoState;
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.StringParameter;
import etomo.ui.swing.UIHarness;
import etomo.util.Goodframe;
import etomo.util.Montagesize;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005 - 2006</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class BlendmontParam implements CommandParam, CommandDetails {
  public static final String rcsid = "$Id$";

  public static final String GOTO_LABEL = "doblend";
  public static final String COMMAND_NAME = "blendmont";
  public static final int LINEAR_INTERPOLATION_ORDER = 1;
  public static final String OUTPUT_FILE_EXTENSION = ".ali";
  public static final String DISTORTION_CORRECTED_STACK_EXTENSION = ".dcst";
  public static final String BLENDMONT_STACK_EXTENSION = ".bl";
  public static final String IMAGE_OUTPUT_FILE_KEY = "ImageOutputFile";
  public static final String IMAGES_ARE_BINNED_KEY = "ImagesAreBinned";
  public static final String DISTORTION_FIELD_KEY = "DistortionField";
  public static final String VERY_SLOPPY_MONTAGE_KEY = "VerySloppyMontage";
  public static final String ROBUST_FIT_CRITERION_KEY = "RobustFitCriterion";
  public static final String STARTING_AND_ENDING_X_KEY = "StartingAndEndingX";
  public static final String STARTING_AND_ENDING_Y_KEY = "StartingAndEndingY";
  public static final String BIN_BY_FACTOR_KEY = "BinByFactor";
  public static final String FILL_VALUE_KEY = "FillValue";

  private final StringParameter distortionField = new StringParameter("DistortionField");
  private final StringParameter imageInputFile = new StringParameter("ImageInputFile");
  private final StringParameter pieceListInput = new StringParameter("PieceListInput");
  private final StringParameter rootNameForEdges = new StringParameter("RootNameForEdges");
  private final ScriptParameter imagesAreBinned = new ScriptParameter("ImagesAreBinned");
  private final EtomoBoolean2 sloppyMontage = new EtomoBoolean2("SloppyMontage");
  private final EtomoBoolean2 verySloppyMontage = new EtomoBoolean2(
      VERY_SLOPPY_MONTAGE_KEY);
  private final ScriptParameter robustFitCriterion = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, ROBUST_FIT_CRITERION_KEY);
  private final ScriptParameter fillValue = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      FILL_VALUE_KEY);
  private final StringParameter transformFile = new StringParameter("TransformFile");
  /**
   * @version 3.10
   * Script is from an earlier version if false.
   */
  private final EtomoBoolean2 adjustOrigin = new EtomoBoolean2("AdjustOrigin");
  private final FortranInputString startingAndEndingX = new FortranInputString(
      STARTING_AND_ENDING_X_KEY, 2);
  private final FortranInputString startingAndEndingY = new FortranInputString(
      STARTING_AND_ENDING_Y_KEY, 2);
  private final EtomoNumber imageRotation = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final FortranInputString unalignedStartingXandY = new FortranInputString(
      "UnalignedStartingXandY", 2);

  private final AxisID axisID;
  private final String datasetName;
  private final EtomoBoolean2 readInXcorrs;
  private final EtomoBoolean2 oldEdgeFunctions;
  private final ScriptParameter interpolationOrder;
  private final EtomoBoolean2 justUndistort;
  private final ScriptParameter binByFactor;
  private final BaseManager manager;

  private String userSizeToOutputInXandY = "";
  private FileType imageOutputFileType = null;
  private String imageOutputFile = null;
  private boolean fiducialess = false;
  private Mode mode = Mode.XCORR;
  private ProcessName processName = ProcessName.XCORR;
  private boolean imageOutputFileFor3dFind = false;
  private boolean validate = false;
  private boolean fromScratch = false;

  public BlendmontParam(final BaseManager manager, final String datasetName,
      final AxisID axisID) {
    this(manager, datasetName, axisID, Mode.XCORR);
  }

  public BlendmontParam(final BaseManager manager, final String datasetName,
      final AxisID axisID, final Mode mode) {
    this.manager = manager;
    this.datasetName = datasetName;
    this.axisID = axisID;
    this.mode = mode;
    readInXcorrs = new EtomoBoolean2("ReadInXcorrs");
    readInXcorrs.setDisplayAsInteger(true);
    oldEdgeFunctions = new EtomoBoolean2("OldEdgeFunctions");
    oldEdgeFunctions.setDisplayAsInteger(true);
    interpolationOrder = new ScriptParameter(EtomoNumber.Type.INTEGER,
        "InterpolationOrder");
    justUndistort = new EtomoBoolean2("JustUndistort");
    imageOutputFile = null;
    imageOutputFileType = null;
    binByFactor = new ScriptParameter(EtomoNumber.Type.INTEGER, BIN_BY_FACTOR_KEY);
    // Only explcitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script
    binByFactor.setDefault(1);
    startingAndEndingX.setIntegerType(new boolean[] { true, true });
    startingAndEndingX.setDivider(' ');
    startingAndEndingY.setIntegerType(new boolean[] { true, true });
    startingAndEndingY.setDivider(' ');
    unalignedStartingXandY.setIntegerType(new boolean[] { true, true });
    setProcessName();
  }

  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    readInXcorrs.parse(scriptCommand);
    oldEdgeFunctions.parse(scriptCommand);
    interpolationOrder.parse(scriptCommand);
    justUndistort.parse(scriptCommand);
    imageOutputFile = scriptCommand.getValue(IMAGE_OUTPUT_FILE_KEY);
    imageOutputFileType = null;
    binByFactor.parse(scriptCommand);
    startingAndEndingX.validateAndSet(scriptCommand);
    startingAndEndingY.validateAndSet(scriptCommand);
    adjustOrigin.parse(scriptCommand);
    distortionField.parse(scriptCommand);
    imageInputFile.parse(scriptCommand);
    pieceListInput.parse(scriptCommand);
    rootNameForEdges.parse(scriptCommand);
    imagesAreBinned.parse(scriptCommand);
    sloppyMontage.parse(scriptCommand);
    verySloppyMontage.parse(scriptCommand);
    robustFitCriterion.parse(scriptCommand);
    fillValue.parse(scriptCommand);
    transformFile.parse(scriptCommand);
    unalignedStartingXandY.validateAndSet(scriptCommand);
  }

  public void updateComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException {
    if (fromScratch) {
      scriptCommand.useKeywordValue();
    }
    readInXcorrs.updateComScript(scriptCommand);
    oldEdgeFunctions.updateComScript(scriptCommand);
    interpolationOrder.updateComScript(scriptCommand);
    justUndistort.updateComScript(scriptCommand);
    scriptCommand.setValue(IMAGE_OUTPUT_FILE_KEY, imageOutputFile);
    binByFactor.updateComScript(scriptCommand);
    startingAndEndingX.updateScriptParameter(scriptCommand);
    startingAndEndingY.updateScriptParameter(scriptCommand);
    adjustOrigin.updateComScript(scriptCommand);
    distortionField.updateComScript(scriptCommand);
    imageInputFile.updateComScript(scriptCommand);
    pieceListInput.updateComScript(scriptCommand);
    rootNameForEdges.updateComScript(scriptCommand);
    imagesAreBinned.updateComScript(scriptCommand);
    sloppyMontage.updateComScript(scriptCommand);
    verySloppyMontage.updateComScript(scriptCommand);
    robustFitCriterion.updateComScript(scriptCommand);
    fillValue.updateComScript(scriptCommand);
    transformFile.updateComScript(scriptCommand);
    unalignedStartingXandY.updateScriptParameter(scriptCommand);
  }

  public void setValidate(final boolean validate) {
    this.validate = validate;
  }

  private void reset() {
    validate = false;
    readInXcorrs.reset();
    oldEdgeFunctions.reset();
    interpolationOrder.reset();
    justUndistort.reset();
    imageOutputFile = null;
    imageOutputFileType = null;
    imageOutputFileFor3dFind = false;
    binByFactor.reset();
    startingAndEndingX.reset();
    startingAndEndingY.reset();
    adjustOrigin.reset();
    fiducialess = false;
    userSizeToOutputInXandY = "";
    imageRotation.reset();
    distortionField.reset();
    imageInputFile.reset();
    pieceListInput.reset();
    rootNameForEdges.reset();
    imagesAreBinned.reset();
    sloppyMontage.reset();
    verySloppyMontage.reset();
    robustFitCriterion.reset();
    fillValue.reset();
    transformFile.reset();
    unalignedStartingXandY.reset();
  }

  public void initializeDefaults() {
  }

  /**
   * If nx is the size of the raw montage in X, and the user requests a size mx,
   * then the starting coordinate to give blendmont is:
   * int(nx/2) - int((mx+1)/2)
   * The ending coordinate is the starting coordinate + mx - 1
   * Blendmont expects unbinned numbers here.
   * 
   if the user size is set, then this works even if the tilt axis angle is
   * closer to 90 degress.  If the user size is not set and the tilt axis angle
   * is closer to 90 degrees, then use x and y from goodframe (see TiltParam.
   * setMontageFullImage()).  Mx = y from goodframe.  My = x from goodframe.
   * Then apply the formula above.
   * 
   * Wlll be called with an empty string by Positioning - whole tomogram.
   * @param sizeToOutputInXandY
   * @throws FortranInputSyntaxException
   * @throws etomo.util.InvalidParameterException
   * @throws IOException
   */
  public boolean convertToStartingAndEndingXandY(String sizeToOutputInXandY,
      final double imageRotation, final String description)
      throws FortranInputSyntaxException, etomo.util.InvalidParameterException,
      IOException {
    boolean retval = true;
    // make sure an empty string really causes sizeToOutputInXandY to be empty.
    if (sizeToOutputInXandY.equals("")) {
      sizeToOutputInXandY = "/";
    }
    startingAndEndingX.reset();
    startingAndEndingY.reset();
    userSizeToOutputInXandY = sizeToOutputInXandY;
    this.imageRotation.set(imageRotation);
    FortranInputString fisSizeToOutputInXandY = new FortranInputString(2);
    fisSizeToOutputInXandY.setIntegerType(new boolean[] { true, true });
    fisSizeToOutputInXandY.validateAndSet(sizeToOutputInXandY);
    if ((fisSizeToOutputInXandY.isDefault() || fisSizeToOutputInXandY.isEmpty())
        && Utilities.is90DegreeImageRotation(imageRotation)) {
      Goodframe goodframe = etomo.comscript.Utilities.getGoodframeFromMontageSize(axisID,
          manager);
      if (goodframe != null) {
        // transposing x and y
        fisSizeToOutputInXandY.set(1, goodframe.getOutput(0));
        fisSizeToOutputInXandY.set(0, goodframe.getOutput(1));
      }
    }
    if (validate && description != null) {
      if (!fisSizeToOutputInXandY.isNull(0) && fisSizeToOutputInXandY.isNull(1)) {
        UIHarness.INSTANCE.openMessageDialog(manager, "Two values are required for "
            + description + ".", "Entry Error", axisID);
        retval = false;
      }
    }
    if (fisSizeToOutputInXandY.isDefault() || fisSizeToOutputInXandY.isEmpty()) {
      return retval;
    }
    Montagesize montagesize = Montagesize.getInstance(manager, axisID,
        DatasetTool.STANDARD_DATASET_EXT);
    montagesize.read(manager);
    convertToStartingAndEnding(startingAndEndingX, montagesize.getX().getInt(),
        fisSizeToOutputInXandY.getInt(0));
    convertToStartingAndEnding(startingAndEndingY, montagesize.getY().getInt(),
        fisSizeToOutputInXandY.getInt(1));
    return retval;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  private void convertToStartingAndEnding(final FortranInputString startingAndEnding,
      final int montageSize, final int size) {
    int starting = ((int) montageSize / 2) - ((int) (size + 1) / 2);
    startingAndEnding.set(0, starting);
    if (size == 0) {
      startingAndEnding.set(1, starting);
    }
    else {
      startingAndEnding.set(1, starting + size - 1);
    }
  }

  public void resetStartingAndEndingXandY() {
    startingAndEndingX.reset();
    startingAndEndingY.reset();
    userSizeToOutputInXandY = "";
    imageRotation.reset();
  }

  public void setMode(final Mode mode) {
    this.mode = mode;
    setProcessName();
  }

  public Mode getMode() {
    return mode;
  }

  public boolean fillValueEquals(final int value) {
    return fillValue.equals(value);
  }

  public void setTransformFile(final String input) {
    transformFile.set(input);
  }

  public void setUnalignedStartingXandY(final String[] input) {
    unalignedStartingXandY.reset();
    if (input == null) {
      return;
    }
    int i = 0;
    if (input.length > i) {
      unalignedStartingXandY.set(i, input[i]);
    }
    i++;
    if (input.length > i) {
      unalignedStartingXandY.set(i, input[i]);
    }
  }

  public FileType getOutputImageFileType() {
    if (imageOutputFileType != null) {
      return imageOutputFileType;
    }
    return FileType.getInstance(manager, axisID, true, true, imageOutputFile);
  }

  public FileType getOutputImageFileType2() {
    if (mode == Mode.WHOLE_TOMOGRAM_SAMPLE) {
      // Handle tiltParam here so the user doesn't have to wait.
      AxisType axisType = manager.getBaseMetaData().getAxisType();
      return FileType.TILT_OUTPUT;
    }
    return null;
  }

  public void setFillValue(final int input) {
    fillValue.set(input);
  }

  public void setPieceListInput(final String input) {
    pieceListInput.set(input);
  }

  public void setFromScratch(final boolean input) {
    fromScratch = input;
  }

  public void setRootNameForEdges(final String input) {
    rootNameForEdges.set(input);
  }

  public void setStartingAndEndingXAndY(final TomogramTool.PairXAndY pairXAndY) {
    resetStartingAndEndingXandY();
    if (pairXAndY == null) {
      return;
    }
    if (!pairXAndY.isXNull()) {
      startingAndEndingX.set(0, pairXAndY.getFirstX());
      startingAndEndingX.set(1, pairXAndY.getSecondX());
    }
    if (!pairXAndY.isYNull()) {
      startingAndEndingY.set(0, pairXAndY.getFirstY());
      startingAndEndingY.set(1, pairXAndY.getSecondY());
    }
  }

  public void setVerySloppyMontage(final boolean input) {
    if (input) {
      sloppyMontage.set(false);
    }
    else {
      sloppyMontage.set(true);
    }
    verySloppyMontage.set(input);
  }

  public void setImagesAreBinned(final Number input) {
    imagesAreBinned.set(input);
  }

  public void setImageInputFile(final File input) {
    if (input == null) {
      imageInputFile.reset();
    }
    imageInputFile.set(input.getName());
  }

  public void setImageInputFile(final String input) {
    imageInputFile.set(input);
  }

  /**
   * For setting the output file before the manager is set up.  Not for 3d find.
   * @param fileType
   * @param rootName
   * @param axisType
   */
  public void setImageOutputFile(final FileType fileType, final String rootName,
      final AxisType axisType) {
    imageOutputFileFor3dFind = false;
    imageOutputFile = fileType.deriveFileName(rootName, axisType, manager, axisID);
    imageOutputFileType = fileType;
  }

  /**
   * Set the output for for 3d find.
   * @param fileType
   */
  public void setImageOutputFileFor3dFind(final FileType fileType) {
    imageOutputFileFor3dFind = true;
    imageOutputFile = fileType.getFileName(manager, axisID);
    imageOutputFileType = fileType;
    setProcessName();
  }

  /**
   * Sets the state of blendmont parameters based on the .edc and .xef files
   * @return true if blendmont needs to be run, false if blendmont does not need
   * to be run
   */
  public boolean setBlendmontState(final ConstEtomoNumber invalidEdgeFunctions) {
    // TEMP
    System.err.println("setBlendmontState:mode=" + mode);
    if (mode == Mode.UNDISTORT) {
      if (!imageOutputFileFor3dFind) {
        imageOutputFile = datasetName + axisID.getExtension()
            + DISTORTION_CORRECTED_STACK_EXTENSION;
        imageOutputFileType = FileType.DISTORTION_CORRECTED_STACK;
      }
      justUndistort.set(true);
      return true;
    }
    else {
      justUndistort.set(false);
      if (!imageOutputFileFor3dFind) {
        if (mode == Mode.XCORR) {
          imageOutputFile = datasetName + axisID.getExtension()
              + BLENDMONT_STACK_EXTENSION;
          imageOutputFileType = FileType.XCORR_BLEND_OUTPUT;
        }
        else if (mode == Mode.PREBLEND) {
          imageOutputFile = datasetName + axisID.getExtension() + ".preali";
          imageOutputFileType = FileType.PREALIGNED_STACK;
        }
        else if (mode == Mode.BLEND || mode == Mode.WHOLE_TOMOGRAM_SAMPLE) {
          imageOutputFile = datasetName + axisID.getExtension() + ".ali";
          imageOutputFileType = FileType.ALIGNED_STACK;
        }
        if (mode == Mode.SERIAL_SECTION_BLEND) {
          sloppyMontage.set(true);
        }
      }
    }
    File ecdFile = new File(manager.getPropertyUserDir(), datasetName
        + axisID.getExtension() + ".ecd");
    File xefFile = new File(manager.getPropertyUserDir(), datasetName
        + axisID.getExtension() + ".xef");
    File yefFile = new File(manager.getPropertyUserDir(), datasetName
        + axisID.getExtension() + ".yef");
    // Read in xcorr output if it exists. Turn on for preblend and blend.
    // In serial sections since blend is based on preblend, then .ecd file file must be
    // there and we must use the same edge functions.
    readInXcorrs.set(mode == Mode.PREBLEND || mode == Mode.BLEND
        || mode == Mode.SERIAL_SECTION_BLEND || mode == Mode.WHOLE_TOMOGRAM_SAMPLE
        || ecdFile.exists());
    // Use existing edge functions, if they are up to date and valid. Turn on for blend.
    oldEdgeFunctions
        .set(mode == Mode.BLEND
            || mode == Mode.SERIAL_SECTION_BLEND
            || mode == Mode.WHOLE_TOMOGRAM_SAMPLE
            || (invalidEdgeFunctions.getInt() != EtomoState.TRUE_VALUE
                && xefFile.exists() && yefFile.exists()
                && ecdFile.lastModified() <= xefFile.lastModified() && ecdFile
                .lastModified() <= yefFile.lastModified()));
    if (mode == Mode.XCORR) {
      // If xcorr output exists and the edge functions are up to date, then don't
      // run blendmont, as long as the blendmont output is more recent then the
      // stack.
      File stackFile = new File(manager.getPropertyUserDir(), datasetName
          + axisID.getExtension() + DatasetTool.STANDARD_DATASET_EXT);
      File blendFile = new File(manager.getPropertyUserDir(), datasetName
          + axisID.getExtension() + BLENDMONT_STACK_EXTENSION);
      if (readInXcorrs.is() && oldEdgeFunctions.is() && blendFile.exists()
          && stackFile.lastModified() < blendFile.lastModified()) {
        return false;
      }
    }
    return true;
  }

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public String getCommandLine() {
    return getCommand();
  }

  public String getCommand() {
    return processName.getComscript(axisID);
  }

  public String[] getCommandArray() {
    return processName.getComscriptArray(axisID);
  }

  public CommandMode getCommandMode() {
    return mode;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public boolean isVerySloppyMontage() {
    return verySloppyMontage.is();
  }

  public File getCommandOutputFile() {
    return new File(manager.getPropertyUserDir(), imageOutputFile);
  }

  public File getCommandInputFile() {
    return null;
  }

  public String getImageOutputFile() {
    return imageOutputFile;
  }

  public int getIntValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(final FieldInterface field) {
    if (field == Field.OLD_EDGE_FUNCTIONS) {
      return oldEdgeFunctions.is();
    }
    if (field == Field.FIDUCIALESS) {
      return fiducialess;
    }
    if (field == Field.LINEAR_INTERPOLATION) {
      return isLinearInterpolation();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(final FieldInterface field) {
    if (field == Field.USER_SIZE_TO_OUTPUT_IN_X_AND_Y) {
      return userSizeToOutputInXandY;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(final FieldInterface field) {
    if (field == Field.IMAGE_ROTATION) {
      return imageRotation;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public void resetDistortionField() {
    distortionField.reset();
  }

  public void setDistortionField(final String input) {
    distortionField.set(input);
  }

  /**
   * Call every time mode or overrideModeForImageOutputFile changes.
   */
  private void setProcessName() {
    processName = getProcessName(mode, imageOutputFileFor3dFind);
  }

  public static ProcessName getProcessName(Mode mode) {
    return getProcessName(mode, false);
  }

  private static ProcessName getProcessName(Mode mode, boolean for3dFind) {
    if (mode == Mode.PREBLEND) {
      return ProcessName.PREBLEND;
    }
    if (mode == Mode.BLEND) {
      if (for3dFind) {
        return ProcessName.BLEND_3D_FIND;
      }
      return ProcessName.BLEND;
    }
    if (mode == Mode.BLEND_3DFIND) {
      return ProcessName.BLEND_3D_FIND;
    }
    if (mode == Mode.UNDISTORT) {
      return ProcessName.UNDISTORT;
    }
    if (mode == Mode.XCORR) {
      return ProcessName.XCORR;
    }
    if (mode == Mode.WHOLE_TOMOGRAM_SAMPLE) {
      return ProcessName.BLEND;
    }
    if (mode == Mode.SERIAL_SECTION_PREBLEND) {
      return ProcessName.PREBLEND;
    }
    if (mode == Mode.SERIAL_SECTION_BLEND) {
      return ProcessName.BLEND;
    }
    throw new IllegalArgumentException("mode=" + mode);
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
  }

  public String getName() {
    return processName.toString();
  }

  public ProcessName getProcessName() {
    return processName;
  }

  public static File getDistortionCorrectedFile(final String workingDir,
      String datasetName, AxisID axisID) {
    return new File(workingDir, datasetName + axisID.getExtension()
        + DISTORTION_CORRECTED_STACK_EXTENSION);
  }

  public boolean isLinearInterpolation() {
    return interpolationOrder.getInt() == LINEAR_INTERPOLATION_ORDER;
  }

  public void setLinearInterpolation(final boolean linearInterpolation) {
    if (linearInterpolation) {
      interpolationOrder.set(LINEAR_INTERPOLATION_ORDER);
    }
    else {
      interpolationOrder.reset();
    }
  }

  public void setFiducialess(final boolean input) {
    fiducialess = input;
  }

  public boolean isFiducialess() {
    return fiducialess;
  }

  public void setAdjustOrigin(final boolean input) {
    adjustOrigin.set(input);
  }

  public void setBinByFactor(final int binByFactor) {
    this.binByFactor.set(binByFactor);
  }

  public void setBinByFactor(final Number input) {
    binByFactor.set(input);
  }

  public ConstEtomoNumber getBinByFactor() {
    return binByFactor;
  }

  public boolean isRobustFitCriterion() {
    return !robustFitCriterion.isNull();
  }

  public String getRobustFitCriterion() {
    return robustFitCriterion.toString();
  }

  public void setRobustFitCriterion(final String input) {
    robustFitCriterion.set(input);
  }

  public static final class Field implements FieldInterface {
    private Field() {
    }

    public static final Field OLD_EDGE_FUNCTIONS = new Field();
    public static final Field FIDUCIALESS = new Field();
    public static final Field IMAGE_ROTATION = new Field();
    public static final Field LINEAR_INTERPOLATION = new Field();
    public static final Field USER_SIZE_TO_OUTPUT_IN_X_AND_Y = new Field();
  }

  public final static class Mode implements CommandMode {
    public static final Mode XCORR = new Mode("XCorr");
    public static final Mode PREBLEND = new Mode("Preblend");
    public static final Mode BLEND = new Mode("Blend");
    public static final Mode BLEND_3DFIND = new Mode("Blend_3dfind");
    public static final Mode UNDISTORT = new Mode("Undistort");
    public static final Mode WHOLE_TOMOGRAM_SAMPLE = new Mode("WholeTomogramSample");
    public static final Mode SERIAL_SECTION_PREBLEND = new Mode("SerialSections_Preblend");
    public static final Mode SERIAL_SECTION_BLEND = new Mode("SerialSections_Blend");

    private final String string;

    private Mode(String string) {
      this.string = string;
    }

    public String toString() {
      return string;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.43  2011/05/10 16:49:35  sueh
 * <p> bug# 1482 Changed getSubcommandProcessName to return a string so that the root name chould be set to
 * <p> subcommandProcessName.
 * <p>
 * <p> Revision 1.42  2011/04/09 06:22:28  sueh
 * <p> bug# 1416 Replaced FileType..DUAL_AXIS_TOMOGRAM and SINGLE_AXIS_TOMOGRAM with TILT_OUTPUT.
 * <p>
 * <p> Revision 1.41  2011/02/28 22:45:02  sueh
 * <p> bug# 1452 Making imageRotation double.
 * <p>
 * <p> Revision 1.40  2011/02/24 23:34:06  sueh
 * <p> bug# 1452 imageRotation needs to be double everywhere.
 * <p>
 * <p> Revision 1.39  2011/02/21 21:11:08  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.38  2010/04/28 15:44:39  sueh
 * <p> bug# 1344 Added getOutputImageFileType functions.
 * <p>
 * <p> Revision 1.37  2010/02/17 04:47:54  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.36  2010/01/11 23:49:01  sueh
 * <p> bug# 1299 Added isMessageReporter.
 * <p>
 * <p> Revision 1.35  2009/12/11 17:25:49  sueh
 * <p> bug# 1291 Added getCommandInputFile to implement Command.
 * <p>
 * <p> Revision 1.34  2009/12/08 02:33:17  sueh
 * <p> bug# 1286 Implemented Loggable.
 * <p>
 * <p> Revision 1.33  2009/09/21 17:43:23  sueh
 * <p> bug# 1267 Added Mod.BEND_3DFIND.
 * <p>
 * <p> Revision 1.32  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 1.31  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.30  2009/03/17 00:30:45  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.29  2009/02/26 17:25:23  sueh
 * <p> bug# 1184 Changed Goodframe so that it can handle any number of
 * <p> inputs and outputs.
 * <p>
 * <p> Revision 1.28  2008/12/15 22:57:47  sueh
 * <p> bug# 1161 In convertToStartingAndEndingXandY handle 90 degree tilt
 * <p> axis angles.
 * <p>
 * <p> Revision 1.27  2007/12/13 01:03:03  sueh
 * <p> bug# 1056 Added adjustOrigin.
 * <p>
 * <p> Revision 1.26  2007/11/06 19:05:26  sueh
 * <p> bug# 1047 Added getSubcommandDetails.
 * <p>
 * <p> Revision 1.25  2007/08/24 16:33:56  sueh
 * <p> bug# 1042 Set startingAndEndingX and Y divider to a space.
 * <p>
 * <p> Revision 1.24  2007/08/16 16:26:03  sueh
 * <p> bug# 1035 Added startingAndEndingX and Y.  Added functions
 * <p> convertToStartingAndEndingXandY and resetStartingAndEndingXandY.
 * <p>
 * <p> Revision 1.23  2007/05/11 15:06:21  sueh
 * <p> bug# 964 Added getStringArray().
 * <p>
 * <p> Revision 1.22  2007/02/05 21:32:22  sueh
 * <p> bug# 962 Put mode info into an inner class.
 * <p>
 * <p> Revision 1.21  2006/06/05 16:04:56  sueh
 * <p> bug# 766 In ProcessName:  Changed getCommand() and getCommandArray() to
 * <p> getComscript... because the fuctions are specialized for comscripts.
 * <p>
 * <p> Revision 1.20  2006/05/22 22:35:02  sueh
 * <p> bug# 577 Added getCommand().
 * <p>
 * <p> Revision 1.19  2006/05/11 19:37:53  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 1.18  2006/04/06 18:48:54  sueh
 * <p> bug# 808 Implementing ProcessDetails.  Added Fields to pass requests to
 * <p> the generic gets.
 * <p>
 * <p> Revision 1.17  2006/01/20 20:45:08  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.16  2005/11/19 01:49:47  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * <p> Revision 1.15  2005/11/03 00:49:07  sueh
 * <p> bug# 740 Added getImageOutputFile().
 * <p>
 * <p> Revision 1.14  2005/09/01 17:45:12  sueh
 * <p> bug# 688 putting temporary prints (for finding cause of undistort
 * <p> parameters being set in xcorr) into the error log
 * <p>
 * <p> Revision 1.13  2005/08/24 22:30:00  sueh
 * <p> bug# 715 Implementing Command so BlendmontParam it can be used in
 * <p> postProcess() and errorProcess().  Change setBlendmontState() to check
 * <p> TomogramState.invalidEdgeFunctions.
 * <p>
 * <p> Revision 1.12  2005/07/29 19:45:21  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.11  2005/07/29 00:42:53  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.10  2005/07/19 20:19:18  sueh
 * <p> bug# 688 Correct file name for the different modes.
 * <p>
 * <p> Revision 1.9  2005/07/18 22:02:17  sueh
 * <p> bug# 688 Setting imageOutputFile and justUndistort when mode is not
 * <p> undistort.
 * <p>
 * <p> Revision 1.8  2005/07/14 21:58:42  sueh
 * <p> bug# 626 Added binByFactor, defaulted to 1.
 * <p> Added WHOLE_TOMOGRAM_SAMPLE_MODE.
 * <p>
 * <p> Revision 1.7  2005/05/09 22:43:39  sueh
 * <p> bug# 658 Changed ScriptParameter. and EtomoBoolean2.setInScript() to
 * <p> updateComScript() to standardize function names.
 * <p>
 * <p> Revision 1.6  2005/04/07 21:49:05  sueh
 * <p> bug# 626 Added undistort mode to write a blendmont command to
 * <p> undistort.com.  Undistort mode is used to create a new stack with
 * <p> distortion correction.
 * <p>
 * <p> Revision 1.5  2005/03/29 19:52:20  sueh
 * <p> bug# 623 Adding the extension for the output file created by blendmont.
 * <p>
 * <p> Revision 1.4  2005/03/11 01:32:20  sueh
 * <p> bug# 533 Added interpolationOrder
 * <p>
 * <p> Revision 1.3  2005/03/09 17:59:41  sueh
 * <p> bug# 533 Added a mode for blendmont in the blend.com script.  In this
 * <p> mode readInXcorrs and oldEdgeFunctions are always true.
 * <p>
 * <p> Revision 1.2  2005/03/08 00:44:12  sueh
 * <p> bug# 533 Added a mode because the rules for setting readInXcorrs are
 * <p> different in xcorr and preblend.  Changed set...State functions to set
 * <p> readInXcorrs correctly.
 * <p>
 * <p> Revision 1.1  2005/03/04 00:07:03  sueh
 * <p> bug# 533 Param object for the blendmont command.
 * <p> </p>
 */
