package etomo.comscript;

import java.io.File;
import java.io.IOException;
import java.util.Hashtable;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.EtomoState;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
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
public class BlendmontParam implements CommandParam, CommandDetails {
  public static final String rcsid = "$Id$";

  public static final String GOTO_LABEL = "doblend";
  public static final String COMMAND_NAME = "blendmont";
  public static final int LINEAR_INTERPOLATION_ORDER = 1;
  public static final String OUTPUT_FILE_EXTENSION = ".ali";
  public static final String DISTORTION_CORRECTED_STACK_EXTENSION = ".dcst";
  public static final String BLENDMONT_STACK_EXTENSION = ".bl";

  public static final String IMAGE_OUTPUT_FILE_KEY = "ImageOutputFile";

  private AxisID axisID;
  private String datasetName;
  private EtomoBoolean2 readInXcorrs;
  private EtomoBoolean2 oldEdgeFunctions;
  private ScriptParameter interpolationOrder;
  private EtomoBoolean2 justUndistort;
  private String imageOutputFile;
  private Mode mode = Mode.XCORR;
  private ScriptParameter binByFactor;

  private final ApplicationManager manager;
  private final FortranInputString startingAndEndingX = new FortranInputString(
      "StartingAndEndingX", 2);
  private final FortranInputString startingAndEndingY = new FortranInputString(
      "StartingAndEndingY", 2);

  public BlendmontParam(ApplicationManager manager, String datasetName,
      AxisID axisID) {
    this(manager, datasetName, axisID, Mode.XCORR);
  }

  public BlendmontParam(ApplicationManager manager, String datasetName,
      AxisID axisID, Mode mode) {
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
    binByFactor = new ScriptParameter(EtomoNumber.Type.INTEGER, "BinByFactor");
    // Only explcitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script  
    binByFactor.setDefault(1);
    startingAndEndingX.setIntegerType(new boolean[] { true, true });
    startingAndEndingY.setIntegerType(new boolean[] { true, true });
  }

  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    readInXcorrs.parse(scriptCommand);
    oldEdgeFunctions.parse(scriptCommand);
    interpolationOrder.parse(scriptCommand);
    justUndistort.parse(scriptCommand);
    imageOutputFile = scriptCommand.getValue(IMAGE_OUTPUT_FILE_KEY);
    binByFactor.parse(scriptCommand);
    startingAndEndingX.validateAndSet(scriptCommand);
    startingAndEndingY.validateAndSet(scriptCommand);
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    readInXcorrs.updateComScript(scriptCommand);
    oldEdgeFunctions.updateComScript(scriptCommand);
    interpolationOrder.updateComScript(scriptCommand);
    justUndistort.updateComScript(scriptCommand);
    scriptCommand.setValue(IMAGE_OUTPUT_FILE_KEY, imageOutputFile);
    binByFactor.updateComScript(scriptCommand);
    startingAndEndingX.updateScriptParameter(scriptCommand);
    startingAndEndingY.updateScriptParameter(scriptCommand);
  }

  private void reset() {
    readInXcorrs.reset();
    oldEdgeFunctions.reset();
    interpolationOrder.reset();
    justUndistort.reset();
    imageOutputFile = null;
    binByFactor.reset();
    startingAndEndingX.reset();
    startingAndEndingY.reset();
  }

  public void initializeDefaults() {
  }

  /**
   * If nx is the size of the raw montage in X, and the user requests a size mx,
   * then the starting coordinate to give blendmont is:
   * int(nx/2) - int((mx+1)/2)
   * The ending coordinate is the starting coordinate + mx - 1
   * Blendmont expects unbinned numbers here.
   * @param sizeToOutputInXandY
   * @throws FortranInputSyntaxException
   * @throws etomo.util.InvalidParameterException
   * @throws IOException
   */
  public void convertToStartingAndEndingXandY(String sizeToOutputInXandY)
      throws FortranInputSyntaxException, etomo.util.InvalidParameterException,
      IOException {
    startingAndEndingX.reset();
    startingAndEndingY.reset();
    FortranInputString fisSizeToOutputInXandY = new FortranInputString(2);
    fisSizeToOutputInXandY.setIntegerType(new boolean[] { true, true });
    fisSizeToOutputInXandY.validateAndSet(sizeToOutputInXandY);
    if (fisSizeToOutputInXandY.isDefault() || fisSizeToOutputInXandY.isEmpty()) {
      return;
    }
    Montagesize montagesize = Montagesize.getInstance(manager, axisID);
    montagesize.read();
    convertToStartingAndEnding(startingAndEndingX, montagesize.getX().getInt(),
        fisSizeToOutputInXandY.getInt(0));
    convertToStartingAndEnding(startingAndEndingY, montagesize.getY().getInt(),
        fisSizeToOutputInXandY.getInt(1));
  }

  private void convertToStartingAndEnding(FortranInputString startingAndEnding,
      int montageSize, int size) {
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
  }

  public void setMode(Mode mode) {
    this.mode = mode;
  }

  public Mode getMode() {
    return mode;
  }

  /**
   * Sets the state of blendmont parameters based on the .edc and .xef files
   * @return true if blendmont needs to be run, false if blendmont does not need
   * to be run
   */
  public boolean setBlendmontState() {
    //TEMP
    System.err.println("setBlendmontState:mode=" + mode);
    if (mode == Mode.UNDISTORT) {
      imageOutputFile = datasetName + axisID.getExtension()
          + DISTORTION_CORRECTED_STACK_EXTENSION;
      justUndistort.set(true);
      return true;
    }
    else {
      justUndistort.set(false);
      if (mode == Mode.XCORR) {
        imageOutputFile = datasetName + axisID.getExtension()
            + BLENDMONT_STACK_EXTENSION;
      }
      else if (mode == Mode.PREBLEND) {
        imageOutputFile = datasetName + axisID.getExtension() + ".preali";
      }
      else if (mode == Mode.BLEND || mode == Mode.WHOLE_TOMOGRAM_SAMPLE) {
        imageOutputFile = datasetName + axisID.getExtension() + ".ali";
      }
    }
    File ecdFile = new File(manager.getPropertyUserDir(), datasetName
        + axisID.getExtension() + ".ecd");
    File xefFile = new File(manager.getPropertyUserDir(), datasetName
        + axisID.getExtension() + ".xef");
    File yefFile = new File(manager.getPropertyUserDir(), datasetName
        + axisID.getExtension() + ".yef");
    File stackFile = new File(manager.getPropertyUserDir(), datasetName
        + axisID.getExtension() + ".st");
    File blendFile = new File(manager.getPropertyUserDir(), datasetName
        + axisID.getExtension() + BLENDMONT_STACK_EXTENSION);
    //Read in xcorr output if it exists.  Turn on for preblend and blend.
    readInXcorrs.set(mode == Mode.PREBLEND || mode == Mode.BLEND
        || mode == Mode.WHOLE_TOMOGRAM_SAMPLE || ecdFile.exists());
    //Use existing edge functions, if they are up to date and valid.  Turn on for blend.
    oldEdgeFunctions
        .set(mode == Mode.BLEND
            || mode == Mode.WHOLE_TOMOGRAM_SAMPLE
            || (manager.getState().getInvalidEdgeFunctions(axisID).getInt() != EtomoState.TRUE_VALUE
                && xefFile.exists()
                && yefFile.exists()
                && ecdFile.lastModified() <= xefFile.lastModified() && ecdFile
                .lastModified() <= yefFile.lastModified()));
    //If xcorr output exists and the edge functions are up to date, then don't
    //run blendmont, as long as the blendmont output is more recent then the
    //stack.
    if (readInXcorrs.is() && oldEdgeFunctions.is() && blendFile.exists()
        && stackFile.lastModified() < blendFile.lastModified()) {
      return false;
    }
    else {
      return true;
    }
  }

  public String getCommandName() {
    return COMMAND_NAME;
  }

  public String getCommandLine() {
    return getCommand();
  }

  public String getCommand() {
    return getProcessName().getComscript(axisID);
  }

  public String[] getCommandArray() {
    return getProcessName().getComscriptArray(axisID);
  }

  public CommandMode getCommandMode() {
    return mode;
  }

  public File getCommandOutputFile() {
    return new File(manager.getPropertyUserDir(), imageOutputFile);
  }

  public String getImageOutputFile() {
    return imageOutputFile;
  }

  public int getIntValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public boolean getBooleanValue(etomo.comscript.Fields field) {
    if (field == Fields.OLD_EDGE_FUNCTIONS) {
      return oldEdgeFunctions.is();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public static ProcessName getProcessName(Mode mode) {
    if (mode == Mode.PREBLEND) {
      return ProcessName.PREBLEND;
    }
    if (mode == Mode.BLEND) {
      return ProcessName.BLEND;
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
    throw new IllegalArgumentException("mode=" + mode);
  }

  public ProcessName getProcessName() {
    return getProcessName(mode);
  }

  public static File getDistortionCorrectedFile(String workingDir,
      String datasetName, AxisID axisID) {
    return new File(workingDir, datasetName + axisID.getExtension()
        + DISTORTION_CORRECTED_STACK_EXTENSION);
  }

  public boolean isLinearInterpolation() {
    return interpolationOrder.getInt() == LINEAR_INTERPOLATION_ORDER;
  }

  public void setLinearInterpolation(boolean linearInterpolation) {
    if (linearInterpolation) {
      interpolationOrder.set(LINEAR_INTERPOLATION_ORDER);
    }
    else {
      interpolationOrder.reset();
    }
  }

  public final void setBinByFactor(int binByFactor) {
    this.binByFactor.set(binByFactor);
  }

  public final ConstEtomoNumber getBinByFactor() {
    return binByFactor;
  }

  public static final class Fields implements etomo.comscript.Fields {
    private Fields() {
    }

    public static final Fields OLD_EDGE_FUNCTIONS = new Fields();
  }

  public final static class Mode implements CommandMode {
    public static final Mode XCORR = new Mode();
    public static final Mode PREBLEND = new Mode();
    public static final Mode BLEND = new Mode();
    public static final Mode UNDISTORT = new Mode();
    public static final Mode WHOLE_TOMOGRAM_SAMPLE = new Mode();
  }
}
/**
 * <p> $Log$
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
