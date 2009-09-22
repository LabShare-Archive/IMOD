/**
 * <p>Description: Tilt command model.</p>
 *
 * <p>Copyright: Copyright (c) 2002-2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.36  2009/09/21 17:46:53  sueh
 * <p> bug# 1267 Getting stack binning based on the file instead of assuming its
 * <p> the .ali file.
 * <p>
 * <p> Revision 3.35  2009/09/05 00:35:39  sueh
 * <p> bug# 1256 Added blank getIteratorElementList.
 * <p>
 * <p> Revision 3.34  2009/09/01 03:17:46  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.33  2009/03/17 00:33:07  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.32  2009/02/26 17:25:32  sueh
 * <p> bug# 1184 Changed Goodframe so that it can handle any number of
 * <p> inputs and outputs.
 * <p>
 * <p> Revision 3.31  2009/02/13 02:13:00  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.
 * <p>
 * <p> Revision 3.30  2008/12/15 23:01:08  sueh
 * <p> bug# 1161 In setFullImage, setMontageFullImage, setMontageSubsetStart,
 * <p> and setSubsetStart handle 90 degree tilt axis angles.
 * <p>
 * <p> Revision 3.29  2008/07/15 17:46:26  sueh
 * <p> bug# 1124 In updateComScriptCommand never use xTiltFile if
 * <p> fiducialess is true.
 * <p>
 * <p> Revision 3.28  2008/02/01 01:36:36  sueh
 * <p> bug# 1075 Handling header failure in setSubsetStart.
 * <p>
 * <p> Revision 3.27  2008/01/28 22:55:34  sueh
 * <p> bug# 1071 In getCommandMode returning commandMode instead of null.  Added toString to Mode.
 * <p>
 * <p> Revision 3.26  2007/12/13 01:06:36  sueh
 * <p> bug# 1056 Added adjustOrigin.  Merged ConstTiltParam with TiltParam and made
 * <p> ConstTiltParam an interface.
 * <p>
 * <p> Revision 3.25  2007/08/29 20:36:42  sueh
 * <p> bug# 1035 In setSubsetStart handling IOException.
 * <p>
 * <p> Revision 3.24  2007/08/16 16:31:25  sueh
 * <p> bug# 1035 Calculating instead of setting subset start.  Removed parameters
 * <p> from setSubsetStart.  Added setMontageSubsetStart.
 * <p>
 * <p> Revision 3.23  2007/03/07 21:03:27  sueh
 * <p> bug# 981 Gave XTILTFILE a default and made sure that it would only be used
 * <p> when the corresponding file existed.
 * <p>
 * <p> Revision 3.22  2007/02/05 22:47:55  sueh
 * <p> bug# 962 Made EtomoNumber type info an inner class.
 * <p>
 * <p> Revision 3.21  2006/09/19 22:00:07  sueh
 * <p> bug# 920 Added Storables, an object for storing variables that don't going into the
 * <p> the .com file.  Storables can be added to MetaData or another storable type
 * <p> without creating an instance of TiltParam.  Moved fiducialess to Storables.
 * <p>
 * <p> Revision 3.20  2006/09/13 23:22:01  sueh
 * <p> bug# 920, bug# 926 Remoing local file and z factor file when fiducialess is true.
 * <p>
 * <p> Revision 3.19  2006/05/19 19:29:42  sueh
 * <p> bug# 866 Doing integer conversions in the param, not the GUI.
 * <p>
 * <p> Revision 3.18  2006/05/11 19:49:03  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p> Changed xAxisTilt to double.
 * <p>
 * <p> Revision 3.17  2005/10/27 00:23:37  sueh
 * <p> bug# 725 Modified setMontageFullImage() to only look in the stack.
 * <p> Added setFullImage() for non-montage cases.
 * <p>
 * <p> Revision 3.16  2005/08/01 17:59:36  sueh
 * <p> bug# 532 Added static command name.
 * <p>
 * <p> Revision 3.15  2005/07/29 19:45:43  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 3.14  2005/07/29 00:50:04  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.13  2005/07/20 17:43:18  sueh
 * <p> bug# 705 Stop printing the stack trace for IOException bugs coming from
 * <p> MRCHeader, because its filling up the error log with exceptions that are
 * <p> related to real problems.
 * <p>
 * <p> Revision 3.12  2005/06/21 01:02:04  sueh
 * <p> bug# 522 Simplified Montagesize.Montagesize().  The director and
 * <p> dataset can be found when the File for the stack is being constructed.
 * <p>
 * <p> Revision 3.11  2005/06/20 16:40:53  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in setMontageFullImage().
 * <p>
 * <p> Revision 3.10  2005/06/16 19:57:29  sueh
 * <p> bug# 692 Fixed bug found in updateComScriptCommand during unit tests.
 * <p> Trying to get a long with getInteger().
 * <p>
 * <p> Revision 3.9  2005/06/13 23:35:25  sueh
 * <p> bug# 583 Preventing tilt.com from being overwritten with a default
 * <p> imageBinned after the .ali file is deleted.  DoneTomogramGeneration()
 * <p> needs update and save tilt.com, but the result from getStackBinning will
 * <p> be wrong if the .ali file has been deleted.  Move the responsibility for
 * <p> getting the right imageBinned to TiltParam.  Modify getStackBinning() to
 * <p> have an option to return a null value when it fails to calculate the stack
 * <p> binning.  If TiltParam.setImageBinned() gets a null value and
 * <p> imageBinned is not null, it won't override the current imageBinned value.
 * <p>
 * <p> Revision 3.8  2005/06/10 22:55:19  sueh
 * <p> bug# 583, bug# 682  Upgraded tilt.com to have all unbinned parameters
 * <p> and a binning value.  No longer managing full image size in tilt.com,
 * <p> except to upgrade the file.  Added function:  updateOldVersion.
 * <p> Removed functions:  setFullImageX, setFullImageY.
 * <p>
 * <p> Revision 3.7  2005/04/25 20:41:18  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.6  2005/03/29 19:54:19  sueh
 * <p> bug# 623 When montaging, setting full image size when updating tilt.com
 * <p> from the .ali file.  When the .ali file is not available set the full image size
 * <p> from running goodframe on the X and Y sizes in the .st file.
 * <p>
 * <p> Revision 3.5  2005/01/12 18:34:07  sueh
 * <p> bug# 505 Added excludeList2.
 * <p>
 * <p> Revision 3.4  2005/01/08 01:46:13  sueh
 * <p> bug# 578 Added dataset name and axis id to constructor.  Read and
 * <p> update ZFACTORFILE in comscript.
 * <p>
 * <p> Revision 3.3  2004/07/20 23:06:28  sueh
 * <p> bug# 502 adding fiducialess, which is not stored in tilt.  It
 * <p> inactivates the local file parameter
 * <p>
 * <p> Revision 3.2  2004/04/12 16:51:07  sueh
 * <p> bug# 409 changed interface class CommandParam
 * <p>
 * <p> Revision 3.1  2004/03/24 02:55:44  rickg
 * <p> Bug# 395 Implemented ability to create binned tomogram
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.9  2003/10/22 21:31:20  rickg
 * <p> Bug# 287 Default value handling for SLICE OFFSET and SHIFT
 * <p>
 * <p> Revision 2.8  2003/08/21 22:17:48  rickg
 * <p> Added density scaling setters
 * <p>
 * <p> Revision 2.7  2003/07/25 22:52:37  rickg
 * <p> CommandParam method name changes
 * <p>
 * <p> Revision 2.6  2003/06/25 22:15:32  rickg
 * <p> Manage all tilt parameters
 * <p>
 * <p> Revision 2.5  2003/06/23 23:27:39  rickg
 * <p> Stricter typing of parameters
 * <p>
 * <p> Revision 2.4  2003/06/10 22:59:59  rickg
 * <p> Changes to match full implementation in progress
 * <p>
 * <p> Revision 2.3  2003/05/23 21:26:47  rickg
 * <p> Implemented radial filter parameters
 * <p>
 * <p> Revision 2.2  2003/03/20 17:24:22  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3  2003/01/03 20:02:39  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.2  2002/10/17 16:19:53  rickg
 * <p> Implemented useExcludeList flag
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
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
import etomo.type.FileType;
import etomo.type.IteratorElementList;
import etomo.type.ProcessName;
import etomo.type.ScriptParameter;
import etomo.type.StringParameter;
import etomo.ui.UIExpertUtilities;
import etomo.ui.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.Goodframe;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

public final class TiltParam implements ConstTiltParam, CommandParam {
  public static final String rcsid = "$Id$";

  public static final String SUBSETSTART_KEY = "SUBSETSTART";
  public static final String COMMAND_NAME = "tilt";

  private final StringParameter inputFile = new StringParameter(
      "InputProjections");
  private final StringParameter outputFile = new StringParameter("OutputFile");
  //tempFullImage: utility variable that is not kept up to date contains fullImageX and fullImageY
  StringParameter tempFullImage = new StringParameter("FULLIMAGE");
  private int fullImageX = Integer.MIN_VALUE;
  private int fullImageY = Integer.MIN_VALUE;
  private final StringParameter localAlignFile = new StringParameter(
      "LOCALFILE");
  //TODO localScale not used and doesn't go into the .com file - what is it for?
  private float localScale = Float.NaN;
  private final ScriptParameter logOffset = new ScriptParameter(
      EtomoNumber.Type.FLOAT, "LOG");
  private final ScriptParameter mode = new ScriptParameter("MODE");
  //tempOffset is not kept up to date
  private final StringParameter tempOffset = new StringParameter("OFFSET");
  private final EtomoNumber tiltAngleOffset = new EtomoNumber(
      EtomoNumber.Type.DOUBLE);
  private float tiltAxisOffset = Float.NaN;
  private final EtomoBoolean2 parallel = new EtomoBoolean2("PARALLEL");
  private final EtomoBoolean2 perpendicular = new EtomoBoolean2("PERPENDICULAR");
  private final StringParameter tempRadial = new StringParameter("RADIAL");
  private float radialBandwidth = Float.NaN;
  private float radialFalloff = Float.NaN;
  private final StringParameter tempScale = new StringParameter("SCALE");
  private float scaleFLevel = Float.NaN;
  private float scaleCoeff = Float.NaN;
  private final StringParameter tempShift = new StringParameter("SHIFT");
  private float xShift = Float.NaN;
  private final EtomoNumber zShift = new EtomoNumber(EtomoNumber.Type.DOUBLE);
  private final StringParameter tempSlice = new StringParameter("SLICE");
  private int idxSliceStart = Integer.MIN_VALUE;
  private int idxSliceStop = Integer.MIN_VALUE;
  private int incrSlice = Integer.MIN_VALUE;
  private final StringParameter tempSubsetStart = new StringParameter(
      SUBSETSTART_KEY);
  private int idxXSubsetStart = Integer.MIN_VALUE;
  private int idxYSubsetStart = Integer.MIN_VALUE;
  private final ScriptParameter thickness = new ScriptParameter("THICKNESS");
  private StringParameter tiltFile = new StringParameter("TILTFILE");
  private final ScriptParameter width = new ScriptParameter("WIDTH");
  private final ScriptParameter xAxisTilt = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, "XAXISTILT");
  private StringParameter xTiltFile = new StringParameter("XTILTFILE");
  private boolean useZFactors = false;
  private StringParameter zFactorFileName = new StringParameter("ZFACTORFILE");
  private boolean loadedFromFile = false;
  private Mode commandMode = Mode.DEFAULT;
  private ProcessName processName = ProcessName.TILT;

  private final StringList excludeList2 = new StringList(0);
  private final StringList excludeList = new StringList(0);
  private final ScriptParameter imageBinned = new ScriptParameter(
      EtomoNumber.Type.LONG, "IMAGEBINNED");

  private final EtomoBoolean2 fiducialess = new EtomoBoolean2("Fiducialess");
  /**
   * @version 3.10
   * Script is from an earlier version if false.
   */
  private final EtomoBoolean2 adjustOrigin = new EtomoBoolean2("AdjustOrigin");
  private final StringParameter projectModel = new StringParameter(
      "ProjectModel");

  private final String datasetName;
  private final ApplicationManager manager;
  private final AxisID axisID;

  public TiltParam(final ApplicationManager manager, final String datasetName,
      final AxisID axisID) {
    this.manager = manager;
    this.datasetName = datasetName;
    this.axisID = axisID;
    //do not default imageBinned
    imageBinned.setFloor(1);
    inputFile.set("");
    outputFile.set("");
    excludeList.setKey("EXCLUDELIST");
    excludeList2.setKey("EXCLUDELIST2");
    localAlignFile.set("");
    tiltFile.set("");
    xTiltFile.set("");
    projectModel.set("");
  }

  public ConstEtomoNumber getImageBinned() {
    return imageBinned;
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public String getCommandName() {
    return processName.toString();
  }

  public ProcessName getProcessName() {
    return processName;
  }

  public String getCommand() {
    return processName.getComscript(axisID);
  }

  public String getCommandLine() {
    return getCommand();
  }

  public String[] getCommandArray() {
    return processName.getComscriptArray(axisID);
  }

  public CommandMode getCommandMode() {
    return commandMode;
  }

  public CommandDetails getSubcommandDetails() {
    return null;
  }

  public ProcessName getSubcommandProcessName() {
    return null;
  }

  public File getCommandOutputFile() {
    return new File(manager.getPropertyUserDir(), outputFile.toString());
  }

  public String getInputFile() {
    return inputFile.toString();
  }

  public float getLogShift() {
    return logOffset.getFloat();
  }

  public void setAdjustOrigin(boolean input) {
    adjustOrigin.set(input);
  }

  public void setCommandMode(Mode input) {
    commandMode = input;
  }

  public void setCommandMode(String input) {
    commandMode = Mode.getInstance(input);
  }

  public boolean hasLogOffset() {
    return !logOffset.isNull();
  }

  public int getMode() {
    return mode.getInt();
  }

  public boolean hasMode() {
    return !mode.isNull();
  }

  public String getLocalAlignFile() {
    return localAlignFile.toString();
  }

  public boolean hasLocalAlignFile() {
    if (localAlignFile.equals(""))
      return false;
    return true;
  }

  public String getOutputFile() {
    return outputFile.toString();
  }

  public boolean isParallel() {
    return parallel.is();
  }

  public boolean isFiducialess() {
    return fiducialess.is();
  }

  public boolean isPerpendicular() {
    return perpendicular.is();
  }

  public float getRadialBandwidth() {
    return radialBandwidth;
  }

  public boolean hasRadialWeightingFunction() {
    if (Float.isNaN(radialBandwidth))
      return false;
    return true;
  }

  public int getThickness() {
    return thickness.getInt();
  }

  public boolean hasThickness() {
    return !thickness.isNull();
  }

  public String getTiltFile() {
    return tiltFile.toString();
  }

  public double getXAxisTilt() {
    return xAxisTilt.getDouble();
  }

  public boolean hasXAxisTilt() {
    return !xAxisTilt.isNull();
  }

  /**
   * Gets the excludeList.
   * @return Returns a String
   */
  public String getExcludeList() {
    return excludeList.toString();
  }

  /**
   * Gets the excludeList2.
   * @return Returns a String
   */
  public String getExcludeList2() {
    return excludeList2.toString();
  }

  /**
   * @return
   */
  public float getRadialFalloff() {
    return radialFalloff;
  }

  /**
   * @return
   */
  public int getWidth() {
    return width.getInt();
  }

  public boolean hasWidth() {
    return !width.isNull();
  }

  /**
   * @return
   */
  public float getXShift() {
    return xShift;
  }

  public boolean hasXShift() {
    if (Float.isNaN(xShift))
      return false;
    return true;
  }

  /**
   * @return
   */
  public ConstEtomoNumber getZShift() {
    return zShift;
  }

  public boolean hasZShift() {
    return !zShift.isNull();
  }

  public boolean isUseZFactors() {
    return useZFactors;
  }

  /**
   * @return
   */
  public int getIncrSlice() {
    return incrSlice;
  }

  public boolean hasSliceIncr() {
    if (incrSlice == Integer.MIN_VALUE)
      return false;
    return true;
  }

  /**
   * @return
   */
  public int getIdxSliceStart() {
    return idxSliceStart;
  }

  /**
   * @return
   */
  public int getIdxSliceStop() {
    return idxSliceStop;
  }

  public boolean hasSlice() {
    if (idxSliceStop == Integer.MIN_VALUE)
      return false;
    return true;
  }

  /**
   * @return
   */
  public ConstEtomoNumber getTiltAngleOffset() {
    return tiltAngleOffset;
  }

  public boolean hasTiltAngleOffset() {
    return !tiltAngleOffset.isNull();
  }

  /**
   * @return
   */
  public float getTiltAxisOffset() {
    return tiltAxisOffset;
  }

  public boolean hasTiltAxisOffset() {
    if (Float.isNaN(tiltAxisOffset))
      return false;
    return true;
  }

  /**
   * @return
   */
  public float getScaleCoeff() {
    return scaleCoeff;
  }

  /**
   * @return
   */
  public float getScaleFLevel() {
    return scaleFLevel;
  }

  public boolean hasScale() {
    if (Float.isNaN(scaleFLevel))
      return false;
    return true;
  }

  /**
   * @return Returns the fullImageX.
   */
  public int getFullImageX() {
    return fullImageX;
  }

  /**
   * @return Returns the fullImageY.
   */
  public int getFullImageY() {
    return fullImageY;
  }

  /**
   * identifies an old version
   * @return
   */
  public boolean isOldVersion() {
    return loadedFromFile && imageBinned.isNull();
  }

  public boolean getBooleanValue(final FieldInterface field) {
    if (field == Field.FIDUCIALESS) {
      return fiducialess.is();
    }
    if (field == Field.ADJUST_ORIGIN) {
      return adjustOrigin.is();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(FieldInterface field) {
    if (field == Field.X_AXIS_TILT) {
      return xAxisTilt.getDouble();
    }
    if (field == Field.Z_SHIFT) {
      return zShift.getDouble();
    }
    if (field == Field.TILT_ANGLE_OFFSET) {
      return tiltAngleOffset.getDouble();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public int getIntValue(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public IteratorElementList getIteratorElementList(final FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the newst command
   * and parameters.
   */
  public void parseComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException {
    if (!scriptCommand.isKeywordValuePairs()) {
      //tilt.com doesn't contain -StandardInput - use old parse method
      backwardCompatibleParseComScriptCommand(scriptCommand);
    }
    else {
      inputFile.parse(scriptCommand);
      outputFile.parse(scriptCommand);
      imageBinned.parse(scriptCommand);
      excludeList.parse(scriptCommand, true);
      excludeList2.parse(scriptCommand, true);
      tempFullImage.parse(scriptCommand);
      if (!tempFullImage.isEmpty()) {
        String[] params = tempFullImage.toString().split("\\s+", 2);
        fullImageX = Integer.parseInt(params[0]);
        fullImageY = Integer.parseInt(params[1]);
      }
      localAlignFile.parse(scriptCommand);
      logOffset.parse(scriptCommand);
      mode.parse(scriptCommand);
      tempOffset.parse(scriptCommand);
      if (!tempOffset.isEmpty()) {
        String[] params = tempOffset.toString().split("\\s+", 2);
        tiltAngleOffset.set(params[0]);
        if (params.length > 1) {
          tiltAxisOffset = Float.parseFloat(params[1]);
        }
      }
      parallel.parse(scriptCommand);
      perpendicular.parse(scriptCommand);
      tempRadial.parse(scriptCommand);
      if (!tempRadial.isEmpty()) {
        String[] params = tempRadial.toString().split("\\s+", 2);
        radialBandwidth = Float.parseFloat(params[0]);
        radialFalloff = Float.parseFloat(params[1]);
      }
      tempScale.parse(scriptCommand);
      if (!tempScale.isEmpty()) {
        String[] params = tempScale.toString().split("\\s+", 2);
        scaleFLevel = Float.parseFloat(params[0]);
        scaleCoeff = Float.parseFloat(params[1]);
      }
      tempShift.parse(scriptCommand);
      if (!tempShift.isEmpty()) {
        String[] params = tempShift.toString().split("\\s+", 2);
        xShift = Float.parseFloat(params[0]);
        if (params.length > 1) {
          zShift.set(params[1]);
        }
      }
      tempSlice.parse(scriptCommand);
      if (!tempSlice.isEmpty()) {
        String[] params = tempSlice.toString().split("\\s+", 3);
        idxSliceStart = Integer.parseInt(params[0]);
        idxSliceStop = Integer.parseInt(params[1]);
        if (params.length > 2) {
          incrSlice = Integer.parseInt(params[2]);
        }
      }
      tempSubsetStart.parse(scriptCommand);
      if (!tempSubsetStart.isEmpty()) {
        String[] params = tempSubsetStart.toString().split("\\s+", 2);
        idxXSubsetStart = Integer.parseInt(params[0]);
        idxYSubsetStart = Integer.parseInt(params[1]);
      }
      thickness.parse(scriptCommand);
      tiltFile.parse(scriptCommand);
      width.parse(scriptCommand);
      xAxisTilt.parse(scriptCommand);
      zFactorFileName.parse(scriptCommand);
      if (!zFactorFileName.isEmpty()) {
        useZFactors = true;
      }
      xTiltFile.parse(scriptCommand);
      adjustOrigin.parse(scriptCommand);
      projectModel.parse(scriptCommand);
      loadedFromFile = true;
    }
  }

  /**
   * Get the parameters from the ComScriptCommand
   * @param scriptCommand the ComScriptCommand containg the newst command
   * and parameters.
   */
  public void backwardCompatibleParseComScriptCommand(
      final ComScriptCommand scriptCommand) throws BadComScriptException {
    //  get the input arguments from the command
    ComScriptInputArg[] inputArgs;
    try {
      inputArgs = getInputArguments(scriptCommand);
    }
    catch (BadComScriptException except) {
      throw (except);
    }

    //  Get the input and output file names from the input arguments
    int nInputArgs = inputArgs.length;
    int argIndex = 0;
    inputFile.set(inputArgs[argIndex++].getArgument());
    outputFile.set(inputArgs[argIndex++].getArgument());
    boolean foundDone = false;
    for (int i = argIndex; i < nInputArgs; i++) {
      // split the line into the parameter name and the rest of the line
      String[] tokens = inputArgs[i].getArgument().split("\\s+", 2);
      if (tokens[0].equalsIgnoreCase("IMAGEBINNED")) {
        imageBinned.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("DONE")) {
        foundDone = true;
      }
      if (tokens[0].equalsIgnoreCase("EXCLUDELIST")) {
        excludeList.parseString(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("EXCLUDELIST2")) {
        excludeList2.parseString(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("FULLIMAGE")) {
        String[] params = tokens[1].split("\\s+", 2);
        fullImageX = Integer.parseInt(params[0]);
        fullImageY = Integer.parseInt(params[1]);
      }
      if (tokens[0].equalsIgnoreCase("LOCALFILE")) {
        localAlignFile.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("LOG")) {
        logOffset.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("MODE")) {
        mode.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("OFFSET")) {
        String[] params = tokens[1].split("\\s+", 2);
        tiltAngleOffset.set(params[0]);
        if (params.length > 1) {
          tiltAxisOffset = Float.parseFloat(params[1]);
        }
      }
      if (tokens[0].equalsIgnoreCase("PARALLEL")) {
        perpendicular.set(false);
        parallel.set(true);
      }
      if (tokens[0].equalsIgnoreCase("PERPENDICULAR")) {
        perpendicular.set(true);
        parallel.set(false);
      }
      if (tokens[0].equalsIgnoreCase("RADIAL")) {
        String[] params = tokens[1].split("\\s+", 2);
        radialBandwidth = Float.parseFloat(params[0]);
        radialFalloff = Float.parseFloat(params[1]);
      }
      if (tokens[0].equalsIgnoreCase("SCALE")) {
        String[] params = tokens[1].split("\\s+", 2);
        scaleFLevel = Float.parseFloat(params[0]);
        scaleCoeff = Float.parseFloat(params[1]);
      }
      if (tokens[0].equalsIgnoreCase("SHIFT")) {
        String[] params = tokens[1].split("\\s+", 2);
        xShift = Float.parseFloat(params[0]);
        if (params.length > 1) {
          zShift.set(params[1]);
        }
      }
      if (tokens[0].equalsIgnoreCase("SLICE")) {
        String[] params = tokens[1].split("\\s+", 3);
        idxSliceStart = Integer.parseInt(params[0]);
        idxSliceStop = Integer.parseInt(params[1]);
        if (params.length > 2) {
          incrSlice = Integer.parseInt(params[2]);
        }
      }
      if (tokens[0].equalsIgnoreCase("SUBSETSTART")) {
        String[] params = tokens[1].split("\\s+", 2);
        idxXSubsetStart = Integer.parseInt(params[0]);
        idxYSubsetStart = Integer.parseInt(params[1]);
      }
      if (tokens[0].equalsIgnoreCase("THICKNESS")) {
        thickness.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("TILTFILE")) {
        tiltFile.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("WIDTH")) {
        width.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("XAXISTILT")) {
        xAxisTilt.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("XTILTFILE")) {
        xTiltFile.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("ZFACTORFILE")) {
        useZFactors = true;
        zFactorFileName.set(tokens[1]);
      }
      if (tokens[0].equalsIgnoreCase("AdjustOrigin")) {
        adjustOrigin.set(true);
      }
      if (tokens[0].equalsIgnoreCase("ProjectModel")) {
        projectModel.set(tokens[1]);
      }
    }
    loadedFromFile = true;
  }

  /**
   * Update the script command with the current valus of this TiltParam
   * object
   * @param scriptCommand the script command to be updated
   */
  public void updateComScriptCommand(final ComScriptCommand scriptCommand)
      throws BadComScriptException {
    if (!scriptCommand.isKeywordValuePairs()) {
    }
    //  Switch to keyword/value pairs
    scriptCommand.useKeywordValue();

    inputFile.updateComScript(scriptCommand);
    outputFile.updateComScript(scriptCommand);
    imageBinned.updateComScript(scriptCommand);
    ParamUtilities.updateScriptParameter(scriptCommand, excludeList.getKey(),
        excludeList);
    ParamUtilities.updateScriptParameter(scriptCommand, excludeList2.getKey(),
        excludeList2);
    if (fullImageX > Integer.MIN_VALUE) {
      tempFullImage.set(String.valueOf(fullImageX) + " "
          + String.valueOf(fullImageY));
    }
    else {
      tempFullImage.reset();
    }
    tempFullImage.updateComScript(scriptCommand);
    if (fiducialess.is()) {
      localAlignFile.reset();
    }
    localAlignFile.updateComScript(scriptCommand);
    logOffset.updateComScript(scriptCommand);
    mode.updateComScript(scriptCommand);
    if (!tiltAngleOffset.isNull()) {
      String arg = tiltAngleOffset.toString();
      if (!Float.isNaN(tiltAxisOffset)) {
        arg += " " + String.valueOf(tiltAxisOffset);
      }
      tempOffset.set(arg);
    }
    else {
      tempOffset.reset();
    }
    tempOffset.updateComScript(scriptCommand);
    parallel.updateComScript(scriptCommand);
    perpendicular.updateComScript(scriptCommand);
    if (!Float.isNaN(radialBandwidth)) {
      tempRadial.set(String.valueOf(radialBandwidth) + " "
          + String.valueOf(radialFalloff));
    }
    else {
      tempRadial.reset();
    }
    tempRadial.updateComScript(scriptCommand);
    if (!Float.isNaN(scaleFLevel)) {
      tempScale.set(String.valueOf(scaleFLevel) + " "
          + String.valueOf(scaleCoeff));
    }
    else {
      tempScale.reset();
    }
    tempScale.updateComScript(scriptCommand);
    StringBuffer shiftBuffer = new StringBuffer();
    if (!Float.isNaN(xShift) || !zShift.isNull()) {
      StringBuffer arg = new StringBuffer();
      if (Float.isNaN(xShift)) {
        arg.append("0 ");
      }
      else {
        arg.append(String.valueOf(xShift));
      }
      if (!zShift.isNull()) {
        arg.append(" " + zShift.toString());
      }
      tempShift.set(arg.toString());
    }
    else {
      tempShift.reset();
    }
    tempShift.updateComScript(scriptCommand);
    if (idxSliceStart > Integer.MIN_VALUE) {
      String arg = String.valueOf(idxSliceStart) + " "
          + String.valueOf(idxSliceStop);
      if (incrSlice > Integer.MIN_VALUE) {
        arg += " " + String.valueOf(incrSlice);
      }
      tempSlice.set(arg);
    }
    else {
      tempSlice.reset();
    }
    tempSlice.updateComScript(scriptCommand);
    if (idxXSubsetStart > Integer.MIN_VALUE) {
      tempSubsetStart.set(String.valueOf(idxXSubsetStart) + " "
          + String.valueOf(idxYSubsetStart));
    }
    else {
      tempSubsetStart.reset();
    }
    tempSubsetStart.updateComScript(scriptCommand);
    thickness.updateComScript(scriptCommand);
    tiltFile.updateComScript(scriptCommand);
    width.updateComScript(scriptCommand);
    xAxisTilt.updateComScript(scriptCommand);
    if (useZFactors && !fiducialess.is()) {
      if (zFactorFileName.isEmpty()) {
        zFactorFileName.set(TiltalignParam.getOutputZFactorFileName(
            datasetName, axisID));
      }
    }
    else {
      zFactorFileName.reset();
    }
    zFactorFileName.updateComScript(scriptCommand);
    //A fiducialess align means that tilt should not use the xtilt file.
    if (!fiducialess.is()) {
      //backwards compatibility: if xTiltFile is empty, set the default xtilt file name
      if (xTiltFile.isEmpty()) {
        xTiltFile.set(DatasetFiles.getXTiltFileName(manager, axisID));
      }
      //use xtilt file if the file exists
      //This is backwards compatibility issue since the only good reason for the
      //file not to exist is that the state comes from an earlier version.
      if (!(new File(manager.getPropertyUserDir(), xTiltFile.toString())
          .exists())) {
        xTiltFile.reset();
      }
    }
    else {
      xTiltFile.reset();
    }
    xTiltFile.updateComScript(scriptCommand);
    adjustOrigin.updateComScript(scriptCommand);
    projectModel.updateComScript(scriptCommand);
  }

  public void initializeDefaults() {
  }

  public ConstEtomoNumber setImageBinned(final int imageBinned) {
    return this.imageBinned.set(imageBinned);
  }

  public void setImageBinned(final long input) {
    imageBinned.set(input);
  }

  /**
   * If the current binning can be retrieved, set imageBinned to current
   * binning.  If not, and imageBinned is null then set imageBinned to 1.
   * @return
   */
  public ConstEtomoNumber setImageBinned() {
    EtomoNumber currentBinning = new EtomoNumber(EtomoNumber.Type.LONG);
    currentBinning.set(UIExpertUtilities.INSTANCE.getStackBinningFromFileName(
        manager, axisID, inputFile.toString(), true));
    if (!currentBinning.isNull()) {
      imageBinned.set(currentBinning);
    }
    else if (imageBinned.isNull()) {
      imageBinned.set(1);
    }
    return imageBinned;
  }

  public void setFiducialess(final boolean input) {
    fiducialess.set(input);
  }

  /**
   * Sets the excludeList.
   * @param excludeList The excludeList to set
   */
  public void setExcludeList(final String list) {
    excludeList.parseString(list);
  }

  /**
   * Sets the excludeList2.
   * @param excludeList2 The excludeList2 to set
   */
  public void setExcludeList2(final String list) {
    excludeList2.parseString(list);
  }

  public void resetExcludeList() {
    excludeList.setNElements(0);
  }

  /**
   * If the tilt axis angle is closer to 90 degree, x and y need to be transposed.
   * The .ali or _3dfind.ali file will already be transposed.  So just transpose
   * the goodframe outputs.
   * @throws InvalidParameterException
   * @throws IOException
   */
  public void setMontageSubsetStart()
      throws etomo.util.InvalidParameterException, IOException {
    resetSubsetStart();
    Goodframe goodframe = etomo.comscript.Utilities
        .getGoodframeFromMontageSize(axisID, manager);
    if (goodframe != null) {
      MRCHeader header = MRCHeader.getInstanceFromFileName(manager, axisID,
          inputFile.toString(), manager.getManagerKey());
      try {
        if (!header.read()) {
          //ok if tilt is being updated before .ali exists
          return;
        }
        int goodframeX;
        int goodframeY;
        if (etomo.comscript.Utilities.is90DegreeImageRotation(manager
            .getConstMetaData().getImageRotation(axisID))) {
          //transpose x and y
          goodframeX = goodframe.getOutput(1).getInt();
          goodframeY = goodframe.getOutput(0).getInt();
        }
        else {
          goodframeX = goodframe.getOutput(0).getInt();
          goodframeY = goodframe.getOutput(1).getInt();
        }
        idxXSubsetStart = (int) ((goodframeX - header.getNColumns()
            * setImageBinned().getLong()) / 2);
        idxYSubsetStart = (int) ((goodframeY - header.getNRows()
            * setImageBinned().getLong()) / 2);
      }
      catch (IOException e) {
        //ok if tilt is being updated before .ali exists
      }
    }
  }

  public void resetSubsetStart() {
    idxXSubsetStart = 0;
    idxYSubsetStart = 0;
  }

  /**
   * If the tilt axis angle is closer to 90 degree, x and y need to be transposed.
   * The .ali file will already be transposed.  So just transpose the stackHeader
   * columns (x) and rows (y).
   * @return
   */
  public boolean setSubsetStart() {
    resetSubsetStart();
    MRCHeader stackHeader = MRCHeader.getInstance(manager, axisID, ".st",
        manager.getManagerKey());
    try {
      if (!stackHeader.read()) {
        return true;
      }
      MRCHeader aliHeader = MRCHeader.getInstanceFromFileName(manager, axisID,
          inputFile.toString(), manager.getManagerKey());
      if (!aliHeader.read()) {
        return true;
      }
      int stackX;
      int stackY;
      if (etomo.comscript.Utilities.is90DegreeImageRotation(manager
          .getConstMetaData().getImageRotation(axisID))) {
        stackX = stackHeader.getNRows();
        stackY = stackHeader.getNColumns();
      }
      else {
        stackX = stackHeader.getNColumns();
        stackY = stackHeader.getNRows();
      }
      idxXSubsetStart = (int) ((stackX - aliHeader.getNColumns()
          * setImageBinned().getLong()) / 2);
      idxYSubsetStart = (int) ((stackY - aliHeader.getNRows()
          * setImageBinned().getLong()) / 2);
    }
    catch (IOException e) {
      e.printStackTrace();
      return true;
    }
    catch (etomo.util.InvalidParameterException e) {
      e.printStackTrace();
      if (Utilities.isAprilFools()) {
        UIHarness.INSTANCE
            .openMessageDialog(
                "A horrible horrible thing happened while I was setting the subset "
                    + "start in tilt.com.  Your tomogram would have been a disaster.  "
                    + "But I caught the problem before it ruined your life.\n"
                    + "Don't bother to thank me.\n" + e.getMessage(),
                "Just Awful", axisID, manager.getManagerKey());
      }
      else {
        UIHarness.INSTANCE.openMessageDialog(
            "Unable to set subset start in tilt.com.\n" + e.getMessage(),
            "Setting Tilt.com Failed", axisID, manager.getManagerKey());
      }
      return false;
    }
    return true;
  }

  /**
   * If the tilt angle axis is closer to 90 degree, transpose x and y.
   */
  public void setMontageFullImage() {
    Goodframe goodframe = etomo.comscript.Utilities
        .getGoodframeFromMontageSize(axisID, manager);
    if (goodframe != null) {
      if (etomo.comscript.Utilities.is90DegreeImageRotation(manager
          .getConstMetaData().getImageRotation(axisID))) {
        fullImageX = goodframe.getOutput(1).getInt();
        fullImageY = goodframe.getOutput(0).getInt();
      }
      else {
        fullImageX = goodframe.getOutput(0).getInt();
        fullImageY = goodframe.getOutput(1).getInt();
      }
    }
  }

  /**
   * If the tilt angle axis is closer to 90 degree, transpose x and y.
   */
  public void setFullImage(final File stack) {
    try {
      MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
          stack.getName(), axisID, manager.getManagerKey());
      if (!header.read()) {
        return;
      }
      if (etomo.comscript.Utilities.is90DegreeImageRotation(manager
          .getConstMetaData().getImageRotation(axisID))) {
        fullImageX = header.getNRows();
        fullImageY = header.getNColumns();
      }
      else {
        fullImageX = header.getNColumns();
        fullImageY = header.getNRows();
      }
      return;
    }
    catch (etomo.util.InvalidParameterException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
    }
  }

  public void setFullImageX(int input) {
    fullImageX = input;
  }

  public void setFullImageY(int input) {
    fullImageY = input;
  }

  /**
   * @param i
   */
  public void setIncrSlice(final int i) {
    incrSlice = i;
  }

  public void resetIncrSlice() {
    incrSlice = Integer.MIN_VALUE;
  }

  /**
   * @param i
   */
  public void setIdxSliceStart(final int i) {
    idxSliceStart = i;
  }

  /**
   * @param i
   */
  public void setIdxSliceStop(final int i) {
    idxSliceStop = i;
  }

  public void setIdxXSubsetStart(final int input) {
    idxXSubsetStart = input;
  }

  public void setIdxYSubsetStart(final int input) {
    idxYSubsetStart = input;
  }

  public void resetIdxSlice() {
    idxSliceStart = Integer.MIN_VALUE;
    idxSliceStop = Integer.MIN_VALUE;
    incrSlice = Integer.MIN_VALUE;
  }

  public void setInputFile(final String file) {
    inputFile.set(file);
  }

  public void setLoadedFromFile(final boolean input) {
    loadedFromFile = input;
  }

  public void resetInputFile() {
    inputFile.set("");
  }

  public void setLocalAlignFile(final String filename) {
    localAlignFile.set(filename);
  }

  public void setLocalScale(final float input) {
    localScale = input;
  }

  public void setLogOffset(final float input) {
    logOffset.set(input);
  }

  public void setProjectModel(final FileType input) {
    projectModel.set(input.getFileName(manager, axisID));
  }

  public void resetLocalAlignFile() {
    localAlignFile.set("");
  }

  public void setLogShift(final float shift) {
    logOffset.set(shift);
  }

  public void resetLogShift() {
    logOffset.reset();
  }

  public void setMode(final int newMode) {
    mode.set(newMode);
  }

  public void resetMode() {
    mode.reset();
  }

  public void setOutputFile(final String file) {
    outputFile.set(file);
  }

  public void resetOutputFile() {
    outputFile.set("");
  }

  public void setParallel() {
    parallel.set(true);
    perpendicular.set(false);
  }

  public void setPerpendicular() {
    parallel.set(false);
    perpendicular.set(true);
  }

  public void setProcessName(ProcessName input) {
    processName = input;
  }

  public void setProcessName(String input) {
    processName = ProcessName.getInstance(input);
    if (processName == null) {
      processName = ProcessName.TILT;
    }
  }

  public void resetAxisOrder() {
    parallel.reset();
    perpendicular.reset();
  }

  public void setRadialBandwidth(final float value) {
    radialBandwidth = value;
  }

  /**
   * @param string
   */
  public void setRadialFalloff(final float value) {
    radialFalloff = value;
  }

  public void resetRadialFilter() {
    radialBandwidth = Float.NaN;
    radialFalloff = Float.NaN;
  }

  public void setScale(final float fLevel, final float coef) {
    scaleCoeff = coef;
    scaleFLevel = fLevel;
  }

  public void resetScale() {
    scaleCoeff = Float.NaN;
    scaleFLevel = Float.NaN;
  }

  /**
   * @param scaleCoeff
   */
  public void setScaleCoeff(final float scaleCoeff) {
    this.scaleCoeff = scaleCoeff;
  }

  /**
   * @param scaleFLevel
   */
  public void setScaleFLevel(final float scaleFLevel) {
    this.scaleFLevel = scaleFLevel;
  }

  public void setThickness(final int input) {
    thickness.set(input);
  }

  public void setThickness(final String newThickness) {
    thickness.set(newThickness);
  }

  public void resetThickness() {
    thickness.reset();
  }

  public void setTiltAngleOffset(final double input) {
    tiltAngleOffset.set(input);
  }

  /**
   * @param d
   */
  public void setTiltAngleOffset(final float d) {
    tiltAngleOffset.set(d);
  }

  public void setTiltAngleOffset(final String tiltAngleOffset) {
    this.tiltAngleOffset.set(tiltAngleOffset);
  }

  public void resetTiltAngleOffset() {
    tiltAngleOffset.reset();
  }

  /**
   * @param d
   */
  public void setTiltAxisOffset(final float d) {
    tiltAxisOffset = d;
  }

  public void resetTiltAxisOffset() {
    tiltAxisOffset = Float.NaN;
  }

  public void setTiltFile(final String filename) {
    tiltFile.set(filename);
  }

  /**
   * @param i
   */
  public void setWidth(final int i) {
    width.set(i);
  }

  public void resetWidth() {
    width.reset();
  }

  public void setXAxisTilt(final double input) {
    xAxisTilt.set(input);
  }

  public void setXAxisTilt(final String angle) {
    xAxisTilt.set(angle);
  }

  public void resetXAxisTilt() {
    xAxisTilt.reset();
  }

  /**
   * @param d
   */
  public void setXShift(final float d) {
    xShift = d;
  }

  public void setXTiltFile(final String input) {
    xTiltFile.set(input);
  }

  public void setZFactorFileName(final String input) {
    zFactorFileName.set(input);
  }

  public void resetXShift() {
    xShift = Float.NaN;
  }

  /**
   * @param d
   */
  public void setZShift(final double d) {
    zShift.set(d);
  }

  public void setZShift(final String zShift) {
    this.zShift.set(zShift);
  }

  public void resetZShift() {
    zShift.reset();
  }

  public void setUseZFactors(final boolean useZFactors) {
    this.useZFactors = useZFactors;
  }

  /**
   * Get the standand input arguments from the ComScriptCommand validating the
   * name of the command and the appropriate number of input arguments.
   * @param scriptCommand the ComScriptCommand containing the tilt command
   */
  private ComScriptInputArg[] getInputArguments(
      final ComScriptCommand scriptCommand) throws BadComScriptException {

    //  Check to be sure that it is a tiltxcorr xommand
    if (!scriptCommand.getCommand().equals("tilt")) {
      throw (new BadComScriptException("Not a tiltalign command"));
    }

    //  Get the input arguments parameters to preserve the comments
    ComScriptInputArg[] inputArgs = scriptCommand.getInputArguments();
    if (inputArgs.length < 3) {
      throw (new BadComScriptException(
          "Incorrect number of input arguments to tiltalign command\nGot "
              + String.valueOf(inputArgs.length) + " expected at least 3."));
    }
    return inputArgs;
  }

  /**
   * Backward compatibility fix.  Unbinned all the parameters which where binned
   * in the old version.  Ignore parameters with reset values.  Will throw an
   * IllegalStateException if it doesn't think that it is an old version.  The
   * param should be loaded from a com file before running this function.
   * @param binning
   * @return true if changes where made
   */
  public boolean upgradeOldVersion(final int correctionBinning,
      final long currentBinning) {
    if (!isOldVersion()) {
      return false;
    }
    imageBinned.set(currentBinning);
    //Currently this function only multiplies by binning, so there is nothing to
    //do if binning is 1.
    if (correctionBinning != 1) {
      if (fullImageX != Integer.MIN_VALUE && fullImageX != 0) {
        fullImageX *= correctionBinning;
      }
      if (fullImageY != Integer.MIN_VALUE && fullImageY != 0) {
        fullImageY *= correctionBinning;
      }
      if (!width.isNull() && !width.equals(0)) {
        width.multiply(correctionBinning);
      }
      if (!zShift.isNull()) {
        float fZShift = zShift.getFloat();
        if (fZShift != 0) {
          fZShift *= correctionBinning;
          zShift.set(fZShift);
        }
      }
      if (!Float.isNaN(xShift) && xShift != 0) {
        xShift *= correctionBinning;
      }
      if (idxSliceStart != Integer.MIN_VALUE && idxSliceStart != 0) {
        idxSliceStart *= correctionBinning;
      }
      if (idxSliceStop != Integer.MIN_VALUE && idxSliceStop != 0) {
        idxSliceStop *= correctionBinning;
      }
      if (!thickness.isNull() && !thickness.equals(0)) {
        thickness.multiply(correctionBinning);
      }
    }
    StringBuffer buffer = new StringBuffer("\nUpgraded tilt"
        + axisID.getExtension() + ".com:\n");
    if (correctionBinning > 1) {
      buffer.append("Multiplied binned FullImage, Width, Offset,"
          + " IdxSliceStart, and/or Thickness by " + correctionBinning + ".\n");
    }
    buffer.append("Added " + imageBinned.getName() + " " + currentBinning
        + ".\n");
    System.err.println(buffer.toString());
    return true;
  }

  public static final class Field implements etomo.comscript.FieldInterface {
    private Field() {
    }

    public static final Field X_AXIS_TILT = new Field();
    public static final Field FIDUCIALESS = new Field();
    public static final Field Z_SHIFT = new Field();
    public static final Field TILT_ANGLE_OFFSET = new Field();
    public static final Field ADJUST_ORIGIN = new Field();
  }

  public static final class Mode implements CommandMode {
    public static final Mode SAMPLE = new Mode("SAMPLE");
    public static final Mode WHOLE = new Mode("WHOLE");
    public static final Mode TILT = new Mode("TILT");

    private static final Mode DEFAULT = TILT;

    private final String string;

    private Mode(String string) {
      this.string = string;
    }

    private static Mode getInstance(String input) {
      if (SAMPLE.string.equals(input)) {
        return SAMPLE;
      }
      if (WHOLE.string.equals(input)) {
        return WHOLE;
      }
      if (TILT.string.equals(input)) {
        return TILT;
      }
      return DEFAULT;
    }

    public String toString() {
      return string;
    }
  }
}