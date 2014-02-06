package etomo.comscript;

import java.io.File;

import etomo.BaseManager;
import etomo.logic.ClusteredPointsAllowed;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.StringParameter;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public class AutofidseedParam implements CommandParam, Command {
  public static final String rcsid = "$Id:$";

  private static final ProcessName PROCESS_NAME = ProcessName.AUTOFIDSEED;
  public static final String BOUNDARY_MODEL_KEY = "BoundaryModel";
  public static final String EXCLUDE_INSIDE_AREAS_KEY = "ExcludeInsideAreas";
  public static final String BORDERS_IN_X_AND_Y_KEY = "BordersInXandY";
  public static final String MIN_GUESS_NUM_BEADS_KEY = "MinGuessNumBeads";
  public static final String MIN_SPACING_KEY = "MinSpacing";
  public static final String PEAK_STORAGE_FRACTION_KEY = "PeakStorageFraction";
  public static final String TARGET_NUMBER_OF_BEADS_KEY = "TargetNumberOfBeads";
  public static final String TARGET_DENSITY_OF_BEADS_KEY = "TargetDensityOfBeads";
  public static final String TWO_SURFACES_KEY = "TwoSurfaces";
  public static final String APPEND_TO_SEED_MODEL_KEY = "AppendToSeedModel";
  public static final String IGNORE_SURFACE_DATA_KEY = "IgnoreSurfaceData";
  public static final String DROP_TRACKS_KEY = "DropTracks";
  public static final String MAX_MAJOR_TO_MINOR_RATIO_KEY = "MaxMajorToMinorRatio";
  public static final String CLUSTERED_POINTS_ALLOWED_KEY = "ClusteredPointsAllowed";
  public static final String ADJUST_SIZES_KEY = "AdjustSizes";
  public static final String ELONGATED_POINTS_ALLOWED_KEY = "ElongatedPointsAllowed";

  private final StringParameter trackCommandFile = new StringParameter("TrackCommandFile");
  private final ScriptParameter minGuessNumBeads = new ScriptParameter(
      MIN_GUESS_NUM_BEADS_KEY);
  private final ScriptParameter minSpacing = new ScriptParameter(EtomoNumber.Type.DOUBLE,
      MIN_SPACING_KEY);
  private final ScriptParameter peakStorageFraction = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, PEAK_STORAGE_FRACTION_KEY);
  private final StringParameter boundaryModel = new StringParameter(BOUNDARY_MODEL_KEY);
  private final EtomoBoolean2 excludeInsideAreas = new EtomoBoolean2(
      EXCLUDE_INSIDE_AREAS_KEY);
  private final FortranInputString bordersInXandY = new FortranInputString(
      BORDERS_IN_X_AND_Y_KEY, 2);
  private final EtomoBoolean2 twoSurfaces = new EtomoBoolean2(TWO_SURFACES_KEY);
  private final EtomoBoolean2 appendToSeedModel = new EtomoBoolean2(
      APPEND_TO_SEED_MODEL_KEY);
  private final ScriptParameter targetNumberOfBeads = new ScriptParameter(
      TARGET_NUMBER_OF_BEADS_KEY);
  private final ScriptParameter targetDensityOfBeads = new ScriptParameter(
      TARGET_DENSITY_OF_BEADS_KEY);
  private final ScriptParameter maxMajorToMinorRatio = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, MAX_MAJOR_TO_MINOR_RATIO_KEY);
  private final ScriptParameter clusteredPointsAllowed = new ScriptParameter(
      CLUSTERED_POINTS_ALLOWED_KEY);
  private final StringList ignoreSurfaceData = new StringList(IGNORE_SURFACE_DATA_KEY);
  private final StringList dropTracks = new StringList(DROP_TRACKS_KEY);
  private final ScriptParameter elongatedPointsAllowed = new ScriptParameter(
      ELONGATED_POINTS_ALLOWED_KEY);
  private final EtomoBoolean2 adjustSizes = new EtomoBoolean2(ADJUST_SIZES_KEY);

  private final BaseManager manager;
  private final AxisID axisID;

  public AutofidseedParam(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    bordersInXandY.setIntegerType(true);
    initializeDefaults();
  }

  public void initializeDefaults() {
    trackCommandFile.set(FileType.TRACK_COMSCRIPT.getFileName(manager, axisID));
    minGuessNumBeads.reset();
    minSpacing.set("0.85");
    peakStorageFraction.set("1");
    boundaryModel.reset();
    excludeInsideAreas.reset();
    bordersInXandY.reset();
    twoSurfaces.reset();
    appendToSeedModel.reset();
    targetNumberOfBeads.reset();
    targetDensityOfBeads.reset();
    maxMajorToMinorRatio.reset();
    clusteredPointsAllowed.reset();
    ignoreSurfaceData.reset();
    dropTracks.reset();
    elongatedPointsAllowed.reset();
    adjustSizes.reset();
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the tiltxcorr command
   * and parameters.
   */
  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException, FortranInputSyntaxException,
      InvalidParameterException {
    initializeDefaults();
    trackCommandFile.parse(scriptCommand);
    minGuessNumBeads.parse(scriptCommand);
    minSpacing.parse(scriptCommand);
    peakStorageFraction.parse(scriptCommand);
    boundaryModel.parse(scriptCommand);
    excludeInsideAreas.parse(scriptCommand);
    bordersInXandY.validateAndSet(scriptCommand);
    twoSurfaces.parse(scriptCommand);
    appendToSeedModel.parse(scriptCommand);
    targetNumberOfBeads.parse(scriptCommand);
    targetDensityOfBeads.parse(scriptCommand);
    maxMajorToMinorRatio.parse(scriptCommand);
    clusteredPointsAllowed.parse(scriptCommand);
    ignoreSurfaceData.parse(scriptCommand);
    dropTracks.parse(scriptCommand);
    elongatedPointsAllowed.parse(scriptCommand);
    adjustSizes.parse(scriptCommand);
  }

  /**
   * Update the script command with the current valus of this TiltxcorrParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException {
    // Switch to keyword/value pairs
    scriptCommand.useKeywordValue();
    trackCommandFile.updateComScript(scriptCommand);
    minGuessNumBeads.updateComScript(scriptCommand);
    minSpacing.updateComScript(scriptCommand);
    peakStorageFraction.updateComScript(scriptCommand);
    boundaryModel.updateComScript(scriptCommand);
    excludeInsideAreas.updateComScript(scriptCommand);
    bordersInXandY.updateScriptParameter(scriptCommand);
    twoSurfaces.updateComScript(scriptCommand);
    appendToSeedModel.updateComScript(scriptCommand);
    targetNumberOfBeads.updateComScript(scriptCommand);
    targetDensityOfBeads.updateComScript(scriptCommand);
    maxMajorToMinorRatio.updateComScript(scriptCommand);
    clusteredPointsAllowed.updateComScript(scriptCommand);
    ignoreSurfaceData.updateComScript(scriptCommand);
    dropTracks.updateComScript(scriptCommand);
    elongatedPointsAllowed.updateComScript(scriptCommand);
    adjustSizes.updateComScript(scriptCommand);
  }

  public boolean isAdjustSizes() {
    return adjustSizes.is();
  }

  public boolean isElongatedPointsAllowedSet() {
    return !elongatedPointsAllowed.isNull() && !elongatedPointsAllowed.equals(0);
  }

  public void setAdjustSizes(final boolean input) {
    adjustSizes.set(input);
  }

  public void setElongatedPointsAllowed(final Number input) {
    elongatedPointsAllowed.set(input);
  }

  public void resetElongatedPointsAllowed() {
    elongatedPointsAllowed.reset();
  }

  public ConstEtomoNumber getElongatedPointsAllowed() {
    return elongatedPointsAllowed;
  }

  public String getMinGuessNumBeads() {
    return minGuessNumBeads.toString();
  }

  public String getMinSpacing() {
    return minSpacing.toString();
  }

  public String getPeakStorageFraction() {
    return peakStorageFraction.toString();
  }

  public boolean isBoundaryModel() {
    return !boundaryModel.isEmpty();
  }

  public boolean isExcludeInsideAreas() {
    return excludeInsideAreas.is();
  }

  public String getBordersInXandY() {
    return bordersInXandY.toString(true);
  }

  public boolean isTwoSurfaces() {
    return twoSurfaces.is();
  }

  public boolean isAppendToSeedModel() {
    return appendToSeedModel.is();
  }

  public boolean isTargetNumberOfBeads() {
    return !targetNumberOfBeads.isNull();
  }

  public String getTargetNumberOfBeads() {
    return targetNumberOfBeads.toString();
  }

  public boolean isTargetDensityOfBeads() {
    return !targetDensityOfBeads.isNull();
  }

  public String getTargetDensityOfBeads() {
    return targetDensityOfBeads.toString();
  }

  public String getMaxMajorToMinorRatio() {
    return maxMajorToMinorRatio.toString();
  }

  public boolean isClusteredPointsAllowed() {
    return !clusteredPointsAllowed.isNull();
  }

  public ClusteredPointsAllowed getClusteredPointsAllowed() {
    return ClusteredPointsAllowed.getInstance(clusteredPointsAllowed.getInt());
  }

  public String getIgnoreSurfaceData() {
    return ignoreSurfaceData.toString();
  }

  public String getDropTracks() {
    return dropTracks.toString();
  }

  public void setMinGuessNumBeads(final String input) {
    minGuessNumBeads.set(input);
  }

  public void setMinSpacing(final String input) {
    minSpacing.set(input);
  }

  public void setPeakStorageFraction(final String input) {
    peakStorageFraction.set(input);
  }

  public void setBoundaryModel(final boolean set) {
    if (set) {
      boundaryModel.set(FileType.AUTOFIDSEED_BOUNDARY_MODEL.getFileName(manager, axisID));
    }
    else {
      boundaryModel.reset();
    }
  }

  public void setExcludeInsideAreas(final boolean input) {
    excludeInsideAreas.set(input);
  }

  public void setBordersInXandY(final String input) throws FortranInputSyntaxException {
    bordersInXandY.validateAndSet(input);
  }

  public void setTwoSurfaces(final boolean input) {
    twoSurfaces.set(input);
  }

  public void setAppendToSeedModel(final boolean input) {
    appendToSeedModel.set(input);
  }

  public void setTargetNumberOfBeads(final String input) {
    targetNumberOfBeads.set(input);
  }

  public void resetTargetNumberOfBeads() {
    targetNumberOfBeads.reset();
  }

  public void setTargetDensityOfBeads(final String input) {
    targetDensityOfBeads.set(input);
  }

  public void resetTargetDensityOfBeads() {
    targetDensityOfBeads.reset();
  }

  public void setMaxMajorToMinorRatio(final String input) {
    maxMajorToMinorRatio.set(input);
  }

  public void setClusteredPointsAllowed(final boolean input) {
    if (input) {
      clusteredPointsAllowed.set(ClusteredPointsAllowed.CLUSTERED.getValue());
    }
    else {
      clusteredPointsAllowed.reset();
    }
  }

  public void resetClusteredPointsAllowed() {
    clusteredPointsAllowed.reset();
  }

  public void setIgnoreSurfaceData(final String input) {
    ignoreSurfaceData.parseString(input);
  }

  public void setDropTracks(final String input) {
    dropTracks.parseString(input);
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public File getCommandOutputFile() {
    return FileType.SEED_MODEL.getFile(manager, axisID);
  }

  public String getCommandName() {
    return PROCESS_NAME.toString();
  }

  public String getCommandLine() {
    return PROCESS_NAME.getComscript(axisID);
  }

  public String[] getCommandArray() {
    return new String[] { PROCESS_NAME.getComscript(axisID) };
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String getCommand() {
    return PROCESS_NAME.getComscript(axisID);
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public ProcessName getProcessName() {
    return PROCESS_NAME;
  }

  public File getCommandInputFile() {
    return FileType.PREALIGNED_STACK.getFile(manager, axisID);
  }

  public boolean isMessageReporter() {
    return false;
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }
}
