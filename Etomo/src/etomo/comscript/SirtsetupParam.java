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
import etomo.type.StringParameter;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2011</p>
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
* <p> Revision 1.2  2011/05/03 02:43:13  sueh
* <p> but# 1416 Implemented CommandDetails.
* <p>
* <p> Revision 1.1  2011/04/04 16:47:34  sueh
* <p> bug# 1416 Param class for sirtsetup.
* <p> </p>
*/
public final class SirtsetupParam implements CommandParam, CommandDetails {
  public static final String rcsid = "$Id$";

  public static final String CLEAN_UP_PAST_START_KEY = "CleanUpPastStart";
  public static final String LEAVE_ITERATIONS_KEY = "LeaveIterations";
  public static final String RADIUS_AND_SIGMA_KEY = "RadiusAndSigma";
  public static final String RESUME_FROM_ITERATION_KEY = "ResumeFromIteration";
  public static final String SCALE_TO_INTEGER_KEY = "ScaleToInteger";
  public static final String START_FROM_ZERO_KEY = "StartFromZero";
  public static final String SUBAREA_SIZE_KEY = "SubareaSize";
  public static final String Y_OFFSET_OF_SUBAREA_KEY = "YOffsetOfSubarea";
  public static final String FLAT_FILTER_FRACTION_KEY = "FlatFilterFraction";
  public static final String SKIP_VERT_SLICE_OUTPUT_KEY = "SkipVertSliceOutput";

  private final StringParameter commandFile = new StringParameter("CommandFile");
  private final ScriptParameter numberOfProcessors = new ScriptParameter(
      "NumberOfProcessors");
  private final EtomoBoolean2 startFromZero = new EtomoBoolean2(START_FROM_ZERO_KEY);
  private final ScriptParameter resumeFromIteration = new ScriptParameter(
      RESUME_FROM_ITERATION_KEY);
  private final StringParameter leaveIterations = new StringParameter(
      LEAVE_ITERATIONS_KEY);
  private final FortranInputString radiusAndSigma = new FortranInputString(
      RADIUS_AND_SIGMA_KEY, 2);
  private final FortranInputString subareaSize = new FortranInputString(SUBAREA_SIZE_KEY,
      2);
  private final ScriptParameter yOffsetOfSubarea = new ScriptParameter(
      Y_OFFSET_OF_SUBAREA_KEY);
  private final FortranInputString scaleToInteger = new FortranInputString(
      SCALE_TO_INTEGER_KEY, 2);
  private final EtomoBoolean2 cleanUpPastStart = new EtomoBoolean2(
      CLEAN_UP_PAST_START_KEY);
  private final ScriptParameter flatFilterFraction = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, FLAT_FILTER_FRACTION_KEY);
  private final EtomoBoolean2 skipVertSliceOutput = new EtomoBoolean2(
      SKIP_VERT_SLICE_OUTPUT_KEY);

  private final AxisID axisID;
  private final BaseManager manager;

  public SirtsetupParam(final BaseManager manager, final AxisID axisID) {
    this.axisID = axisID;
    this.manager = manager;
    commandFile.set(FileType.TILT_COMSCRIPT.getFileName(manager, axisID));
    radiusAndSigma.setIntegerType(false);
    radiusAndSigma.set(0, .4);
    radiusAndSigma.set(1, .05);
    subareaSize.setIntegerType(true);
    scaleToInteger.setIntegerType(false);
  }

  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    // reset
    startFromZero.reset();
    resumeFromIteration.reset();
    leaveIterations.reset();
    subareaSize.reset();
    yOffsetOfSubarea.reset();
    scaleToInteger.reset();
    cleanUpPastStart.reset();
    flatFilterFraction.reset();
    skipVertSliceOutput.reset();
    // parse
    startFromZero.parse(scriptCommand);
    resumeFromIteration.parse(scriptCommand);
    leaveIterations.parse(scriptCommand);
    radiusAndSigma.validateAndSet(scriptCommand);
    subareaSize.validateAndSet(scriptCommand);
    yOffsetOfSubarea.parse(scriptCommand);
    scaleToInteger.validateAndSet(scriptCommand);
    cleanUpPastStart.parse(scriptCommand);
    flatFilterFraction.parse(scriptCommand);
    skipVertSliceOutput.parse(scriptCommand);
  }

  public void updateComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException {
    scriptCommand.useKeywordValue();
    commandFile.updateComScript(scriptCommand);
    numberOfProcessors.updateComScript(scriptCommand);
    startFromZero.updateComScript(scriptCommand);
    resumeFromIteration.updateComScript(scriptCommand);
    leaveIterations.updateComScript(scriptCommand);
    radiusAndSigma.updateScriptParameter(scriptCommand);
    subareaSize.updateScriptParameter(scriptCommand);
    yOffsetOfSubarea.updateComScript(scriptCommand);
    scaleToInteger.updateScriptParameter(scriptCommand);
    cleanUpPastStart.updateComScript(scriptCommand);
    flatFilterFraction.updateComScript(scriptCommand);
    skipVertSliceOutput.updateComScript(scriptCommand);
  }

  public void initializeDefaults() {
  }

  public void setNumberOfProcessors(final String input) {
    numberOfProcessors.set(input);
  }

  public void setStartFromZero(final boolean input) {
    startFromZero.set(input);
  }

  public void setSkipVertSliceOutput(final boolean input) {
    skipVertSliceOutput.set(input);
  }

  public boolean isStartFromZero() {
    return startFromZero.is();
  }

  public boolean isSkipVertSliceOutput() {
    return skipVertSliceOutput.is();
  }

  public void resetResumeFromIteration() {
    resumeFromIteration.reset();
  }

  public boolean isResumeFromIterationNull() {
    return resumeFromIteration.isNull();
  }

  public String getLeaveIterations() {
    return leaveIterations.toString();
  }

  public String getRadiusAndSigma(int index) {
    return radiusAndSigma.toString(index, true);
  }

  public void setRadiusAndSigma(int index, String input) {
    radiusAndSigma.set(index, input);
  }

  public String getSubareaSize() {
    return subareaSize.toString();
  }

  public String getYOffsetOfSubarea() {
    return yOffsetOfSubarea.toString();
  }

  public boolean isYOffsetOfSubareaNull() {
    return yOffsetOfSubarea.isNull();
  }

  public boolean isScaleToIntegerNull() {
    return scaleToInteger.isNull();
  }

  public boolean isSubareaSizeNull() {
    return subareaSize.isNull();
  }

  public boolean isCleanUpPastStart() {
    return cleanUpPastStart.is();
  }

  public void resetSubareaSize() {
    subareaSize.reset();
  }

  public void resetYOffsetOfSubarea() {
    yOffsetOfSubarea.reset();
  }

  public void setCleanUpPastStart(final boolean input) {
    cleanUpPastStart.set(input);
  }

  public void setLeaveIterations(final String input) {
    leaveIterations.set(input);
  }

  public void setResumeFromIteration(final ConstEtomoNumber input) {
    resumeFromIteration.set(input);
  }

  public void setScaleToInteger(final boolean input) {
    if (input) {
      scaleToInteger.set(0, -20000);
      scaleToInteger.set(1, 20000);
    }
    else {
      scaleToInteger.reset();
    }
  }

  public void setSubareaSize(final String input) throws FortranInputSyntaxException {
    subareaSize.validateAndSet(input);
  }

  public void setYOffsetOfSubarea(final String input) {
    yOffsetOfSubarea.set(input);
  }

  public void setFlatFilterFraction(final String input) {
    flatFilterFraction.set(input);
  }

  public String getFlatFilterFraction() {
    return flatFilterFraction.toString();
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String getCommand() {
    return ProcessName.SIRTSETUP.getComscript(axisID);
  }

  public String[] getCommandArray() {
    return ProcessName.SIRTSETUP.getComscriptArray(axisID);
  }

  public File getCommandInputFile() {
    return FileType.TILT_COMSCRIPT.getFile(manager, axisID);
  }

  public String getCommandLine() {
    return getCommand();
  }

  public CommandMode getCommandMode() {
    return null;
  }

  public String getCommandName() {
    // In this case the .com file name and the command in the .com file are the same.
    return ProcessName.SIRTSETUP.toString();
  }

  public File getCommandOutputFile() {
    return null;
  }

  public FileType getOutputImageFileType() {
    return null;
  }

  public FileType getOutputImageFileType2() {
    return null;
  }

  public ProcessName getProcessName() {
    return ProcessName.SIRTSETUP;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public String getSubcommandProcessName() {
    return null;
  }

  public boolean isMessageReporter() {
    return false;
  }

  public List getLogMessage() throws LogFile.LockException, FileNotFoundException,
      IOException {
    return null;
  }

  public String getName() {
    return ProcessName.SIRTSETUP.toString();
  }

  public boolean getBooleanValue(final etomo.comscript.FieldInterface fieldInterface) {
    if (fieldInterface == Field.SUBAREA) {
      return !subareaSize.isNull();
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
    if (field == Field.Y_OFFSET_OF_SUBSET) {
      return yOffsetOfSubarea.getInt();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(final FieldInterface field) {
    if (field == Field.SUBAREA_SIZE) {
      return subareaSize.toString(true);
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public static final class Field implements etomo.comscript.FieldInterface {
    public static final Field SUBAREA = new Field();
    public static final Field SUBAREA_SIZE = new Field();
    public static final Field Y_OFFSET_OF_SUBSET = new Field();

    private Field() {
    }
  }
}