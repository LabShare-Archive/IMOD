package etomo.comscript;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Hashtable;
import java.util.List;

import etomo.BaseManager;
import etomo.ManagerKey;
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
import etomo.type.StringParameter;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.2  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p> </p>
 */
public final class FindBeads3dParam implements ConstFindBeads3dParam,
    CommandParam {
  public static final String rcsid = "$Id$";

  public static final String BEAD_SIZE_TAG = "BeadSize";
  public static final String LIGHT_BEADS_TAG = "LightBeads";
  public static final String MIN_RELATIVE_STRENGTH_TAG = "MinRelativeStrength";
  public static final String THRESHOLD_FOR_AVERAGING_TAG = "ThresholdForAveraging";
  public static final String STORAGE_THRESHOLD_TAG = "StorageThreshold";
  public static final String MIN_SPACING_TAG = "MinSpacing";
  public static final String GUESS_NUM_BEADS_TAG = "GuessNumBeads";
  public static final String MAX_NUM_BEADS_TAG = "MaxNumBeads";

  private final StringParameter inputFile = new StringParameter("InputFile");
  private final StringParameter outputFile = new StringParameter("OutputFile");
  private final ScriptParameter beadSize = new ScriptParameter(
      EtomoNumber.Type.FLOAT, BEAD_SIZE_TAG);
  private final EtomoBoolean2 lightBeads = new EtomoBoolean2(LIGHT_BEADS_TAG);
  private final ScriptParameter minRelativeStrength = new ScriptParameter(
      EtomoNumber.Type.FLOAT, MIN_RELATIVE_STRENGTH_TAG);
  private final ScriptParameter thresholdForAveraging = new ScriptParameter(
      EtomoNumber.Type.FLOAT, THRESHOLD_FOR_AVERAGING_TAG);
  private final ScriptParameter storageThreshold = new ScriptParameter(
      EtomoNumber.Type.FLOAT, STORAGE_THRESHOLD_TAG);
  private final ScriptParameter minSpacing = new ScriptParameter(
      EtomoNumber.Type.FLOAT, MIN_SPACING_TAG);
  private final ScriptParameter guessNumBeads = new ScriptParameter(
      GUESS_NUM_BEADS_TAG);
  private final ScriptParameter maxNumBeads = new ScriptParameter(
      MAX_NUM_BEADS_TAG);

  private final AxisID axisID;
  private final BaseManager manager;

  public FindBeads3dParam(final BaseManager manager, final AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
    reset();
  }

  private void reset() {
    inputFile.reset();
    outputFile.reset();
    beadSize.reset();
    lightBeads.reset();
    minRelativeStrength.reset();
    thresholdForAveraging.reset();
    storageThreshold.reset();
    minSpacing.reset();
    guessNumBeads.reset();
    maxNumBeads.reset();
  }

  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    beadSize.parse(scriptCommand);
    minRelativeStrength.parse(scriptCommand);
    thresholdForAveraging.parse(scriptCommand);
    storageThreshold.parse(scriptCommand);
    minSpacing.parse(scriptCommand);
    guessNumBeads.parse(scriptCommand);
    maxNumBeads.parse(scriptCommand);
  }

  public void updateComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException {
    scriptCommand.useKeywordValue();
    inputFile.updateComScript(scriptCommand);
    outputFile.updateComScript(scriptCommand);
    beadSize.updateComScript(scriptCommand);
    lightBeads.updateComScript(scriptCommand);
    minRelativeStrength.updateComScript(scriptCommand);
    thresholdForAveraging.updateComScript(scriptCommand);
    storageThreshold.updateComScript(scriptCommand);
    minSpacing.updateComScript(scriptCommand);
    guessNumBeads.updateComScript(scriptCommand);
    maxNumBeads.updateComScript(scriptCommand);
  }

  public void initializeDefaults() {
  }

  public void setInputFile(String input) {
    inputFile.set(input);
  }

  public void setOutputFile(String input) {
    outputFile.set(input);
  }

  public void setBeadSize(String input) {
    beadSize.set(input);
  }

  public String getBeadSize() {
    return beadSize.toString();
  }

  public void setLightBeads(boolean input) {
    lightBeads.set(input);
  }

  public void setMinRelativeStrength(String input) {
    minRelativeStrength.set(input);
  }

  public String getMinRelativeStrength() {
    return minRelativeStrength.toString();
  }

  public void setThresholdForAveraging(String input) {
    thresholdForAveraging.set(input);
  }

  public String getThresholdForAveraging() {
    return thresholdForAveraging.toString();
  }

  public void setStorageThreshold(ConstEtomoNumber input) {
    storageThreshold.set(input);
  }

  public void setStorageThreshold(String input) {
    storageThreshold.set(input);
  }

  public ConstEtomoNumber getStorageThreshold() {
    return storageThreshold;
  }

  public void setMinSpacing(String input) {
    minSpacing.set(input);
  }

  public String getMinSpacing() {
    return minSpacing.toString();
  }

  public List getLogMessage(ManagerKey managerKey)
      throws LogFile.LockException, FileNotFoundException, IOException {
    return null;
  }

  public String getName() {
    return ProcessName.FIND_BEADS_3D.toString();
  }

  public ProcessName getProcessName() {
    return ProcessName.FIND_BEADS_3D;
  }

  public void setGuessNumBeads(String input) {
    guessNumBeads.set(input);
  }

  public String getGuessNumBeads() {
    return guessNumBeads.toString();
  }

  public void setMaxNumBeads(String input) {
    maxNumBeads.set(input);
  }

  public String getMaxNumBeads() {
    return maxNumBeads.toString();
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String[] getCommandArray() {
    String[] array = { getCommandLine() };
    return array;
  }

  public String getCommandLine() {
    return FileType.FIND_BEADS_3D_COMSCRIPT.getFileName(manager, axisID);
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public String getCommandName() {
    return FileType.FIND_BEADS_3D_COMSCRIPT.getLeftSide();
  }

  public File getCommandOutputFile() {
    return null;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public ProcessName getSubcommandProcessName() {
    return null;
  }

  public String getCommand() {
    return FileType.FIND_BEADS_3D_COMSCRIPT.getFileName(manager, axisID);
  }

  public boolean getBooleanValue(
      final etomo.comscript.FieldInterface fieldInterface) {
    throw new IllegalArgumentException("field=" + fieldInterface);
  }

  public double getDoubleValue(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue(final FieldInterface field) {
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
}
