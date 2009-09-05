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
import java.util.ArrayList;
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
import etomo.ui.UIExpertUtilities;
import etomo.ui.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.Goodframe;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

public final class TiltParam implements ConstTiltParam, CommandParam {
  public static final String rcsid = "$Id$";

  public static final String SUBSETSTART_KEY = "SUBSETSTART";
  public static final String COMMAND_NAME = "tilt";

  private static final String X_AXIS_TILT_KEY = "XAXISTILT";
  private static final String THICKNESS_KEY = "THICKNESS";

  private String inputFile = "";
  private String outputFile = "";
  private String angles = "";
  private float compressionFraction = Float.NaN;
  private String compression = "";
  private int cosInterpOrder = Integer.MIN_VALUE;
  private float cosInterpFactor = Float.NaN;
  private String densityWeightParams = "";
  private String exclude = "";
  private int fastBackProjInterpOrder = Integer.MIN_VALUE;
  private int fullImageX = Integer.MIN_VALUE;
  private int fullImageY = Integer.MIN_VALUE;
  private String include = "";
  private String localAlignFile = "";
  private float localScale = Float.NaN;
  private float logOffset = Float.NaN;
  private float mask = Float.NaN;
  private int mode = Integer.MIN_VALUE;
  private float tiltAxisOffset = Float.NaN;
  private boolean parallel = false;
  private boolean perpendicular = false;
  private float radialBandwidth = Float.NaN;
  private float radialFalloff = Float.NaN;
  private int nReplicate = Integer.MIN_VALUE;
  private int incReplicate = Integer.MIN_VALUE;
  private float scaleFLevel = Float.NaN;
  private float scaleCoeff = Float.NaN;
  private float xShift = Float.NaN;
  private int idxSliceStart = Integer.MIN_VALUE;
  private int idxSliceStop = Integer.MIN_VALUE;
  private int incrSlice = Integer.MIN_VALUE;
  private int idxXSubsetStart = Integer.MIN_VALUE;
  private int idxYSubsetStart = Integer.MIN_VALUE;
  private int thickness = Integer.MIN_VALUE;
  private String tiltFile = "";
  private String title = "";
  private int width = Integer.MIN_VALUE;
  private double xAxisTilt = Double.NaN;
  private String xTiltFile = "";
  private int xTiltInterp = Integer.MIN_VALUE;
  private boolean useZFactors = false;
  private String zFactorFileName;
  private boolean loadedFromFile = false;
  private Mode commandMode = Mode.DEFAULT;
  private ProcessName processName = ProcessName.TILT;

  private final StringList excludeList2 = new StringList(0);
  private final StringList excludeList = new StringList(0);
  private final ScriptParameter imageBinned = new ScriptParameter(
      EtomoNumber.Type.LONG, "IMAGEBINNED");
  private final EtomoNumber tiltAngleOffset = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, "OFFSET");
  private final EtomoNumber zShift = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      "SHIFT");
  private final EtomoBoolean2 fiducialess = new EtomoBoolean2("Fiducialess");
  /**
   * @version 3.10
   * Script is from an earlier version if false.
   */
  private final EtomoBoolean2 adjustOrigin = new EtomoBoolean2("AdjustOrigin");
  private String projectModel = "";

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
    return new File(manager.getPropertyUserDir(), outputFile);
  }

  public String getInputFile() {
    return inputFile;
  }

  public float getLogShift() {
    return logOffset;
  }

  public void setAdjustOrigin(boolean input) {
    adjustOrigin.set(input);
  }

  public void setAngles(String input) {
    angles = input;
  }

  public void setCommandMode(Mode input) {
    commandMode = input;
  }

  public void setCommandMode(String input) {
    commandMode = Mode.getInstance(input);
  }

  public void setCompression(String input) {
    compression = input;
  }

  public void setCompressionFraction(float input) {
    compressionFraction = input;
  }

  public void setCosInterpFactor(float input) {
    cosInterpFactor = input;
  }

  public void setCosInterpOrder(int input) {
    cosInterpOrder = input;
  }

  public void setDensityWeightParams(String input) {
    densityWeightParams = input;
  }

  public void setExclude(String input) {
    exclude = input;
  }

  public boolean hasLogOffset() {
    if (Float.isNaN(logOffset))
      return false;
    return true;
  }

  public int getMode() {
    return mode;
  }

  public boolean hasMode() {
    if (mode == Integer.MIN_VALUE)
      return false;
    return true;
  }

  public String getLocalAlignFile() {
    return localAlignFile;
  }

  public boolean hasLocalAlignFile() {
    if (localAlignFile.equals(""))
      return false;
    return true;
  }

  public String getOutputFile() {
    return outputFile;
  }

  public boolean isParallel() {
    return parallel;
  }

  public boolean isFiducialess() {
    return fiducialess.is();
  }

  public boolean isPerpendicular() {
    return perpendicular;
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
    return thickness;
  }

  public boolean hasThickness() {
    if (thickness == Integer.MIN_VALUE)
      return false;
    return true;
  }

  public String getTiltFile() {
    return tiltFile;
  }

  public double getXAxisTilt() {
    return xAxisTilt;
  }

  public boolean hasXAxisTilt() {
    if (Double.isNaN(xAxisTilt))
      return false;
    return true;
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
    return width;
  }

  public boolean hasWidth() {
    if (width == Integer.MIN_VALUE)
      return false;
    return true;
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
    if (field == Field.LOADED_FROM_FILE) {
      return loadedFromFile;
    }
    if (field == Field.PARALLEL) {
      return parallel;
    }
    if (field == Field.PERPENDICULAR) {
      return perpendicular;
    }
    if (field == Field.USE_Z_FACTORS) {
      return useZFactors;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public float getFloatValue(FieldInterface field) {
    if (field == Field.COMPRESSION_FRACTION) {
      return compressionFraction;
    }
    if (field == Field.COS_INTERP_FACTOR) {
      return cosInterpFactor;
    }
    if (field == Field.LOCAL_SCALE) {
      return localScale;
    }
    if (field == Field.LOG_OFFSET) {
      return logOffset;
    }
    if (field == Field.MASK) {
      return mask;
    }
    if (field == Field.RADIAL_BANDWIDTH) {
      return radialBandwidth;
    }
    if (field == Field.RADIAL_FALLOFF) {
      return radialFalloff;
    }
    if (field == Field.SCALE_COEFF) {
      return scaleCoeff;
    }
    if (field == Field.SCALE_F_LEVEL) {
      return scaleFLevel;
    }
    if (field == Field.TILT_AXIS_OFFSET) {
      return tiltAxisOffset;
    }
    if (field == Field.X_SHIFT) {
      return xShift;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public String[] getStringArray(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(FieldInterface field) {
    if (field == Field.ANGLES) {
      return angles;
    }
    if (field == Field.COMPRESSION) {
      return compression;
    }
    if (field == Field.DENSITY_WEIGHT_PARAMS) {
      return densityWeightParams;
    }
    if (field == Field.EXCLUDE) {
      return exclude;
    }
    if (field == Field.EXCLUDE_LIST) {
      return excludeList.toString();
    }
    if (field == Field.EXCLUDE_LIST_2) {
      return excludeList2.toString();
    }
    if (field == Field.INCLUDE) {
      return include;
    }
    if (field == Field.INPUT_FILE) {
      return inputFile;
    }
    if (field == Field.LOCAL_ALIGN_FILE) {
      return localAlignFile;
    }
    if (field == Field.OUTPUT_FILE) {
      return outputFile;
    }
    if (field == Field.TILT_FILE) {
      return tiltFile;
    }
    if (field == Field.TITLE) {
      return title;
    }
    if (field == Field.X_TILT_FILE) {
      return xTiltFile;
    }
    if (field == Field.Z_FACTOR_FILE_NAME) {
      return zFactorFileName;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(FieldInterface field) {
    if (field == Field.X_AXIS_TILT) {
      return xAxisTilt;
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
    if (field == Field.IMAGE_BINNED) {
      return imageBinned;
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(FieldInterface field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public int getIntValue(FieldInterface field) {
    if (field == Field.COS_INTERP_ORDER) {
      return cosInterpOrder;
    }
    if (field == Field.FAST_BACK_PROJ_INTERP_ORDER) {
      return fastBackProjInterpOrder;
    }
    if (field == Field.FULL_IMAGE_X) {
      return fullImageX;
    }
    if (field == Field.FULL_IMAGE_Y) {
      return fullImageY;
    }
    if (field == Field.IDX_SLICE_START) {
      return idxSliceStart;
    }
    if (field == Field.IDX_SLICE_STOP) {
      return idxSliceStop;
    }
    if (field == Field.IDX_X_SUBSET_START) {
      return idxXSubsetStart;
    }
    if (field == Field.IDX_Y_SUBSET_START) {
      return idxYSubsetStart;
    }
    if (field == Field.INCR_SLICE) {
      return incrSlice;
    }
    if (field == Field.INC_REPLICATE) {
      return incReplicate;
    }
    if (field == Field.MODE) {
      return mode;
    }
    if (field == Field.THICKNESS) {
      return thickness;
    }
    if (field == Field.N_REPLICATE) {
      return nReplicate;
    }
    if (field == Field.WIDTH) {
      return width;
    }
    if (field == Field.X_TILT_INTERP) {
      return xTiltInterp;
    }
    throw new IllegalArgumentException("field=" + field);
  }
  
  public IteratorElementList getIteratorElementList(
      final FieldInterface field) {
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
      throws BadComScriptException {
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
    inputFile = inputArgs[argIndex++].getArgument();
    outputFile = inputArgs[argIndex++].getArgument();
    boolean foundDone = false;
    for (int i = argIndex; i < nInputArgs; i++) {
      // split the line into the parameter name and the rest of the line
      String[] tokens = inputArgs[i].getArgument().split("\\s+", 2);
      if (imageBinned.isNamed(tokens[0])) {
        imageBinned.set(tokens[1]);
      }
      if (tokens[0].equals("ANGLES")) {
        angles = tokens[1];
      }
      if (tokens[0].equals("COMPFRACTION")) {
        compressionFraction = Float.parseFloat(tokens[1]);
      }
      if (tokens[0].equals("COMPRESS")) {
        compression = tokens[1];
      }
      if (tokens[0].equals("COSINTERP")) {
        String[] params = tokens[1].split("\\s+", 2);
        cosInterpOrder = Integer.parseInt(params[0]);
        cosInterpFactor = Float.parseFloat(params[1]);
      }
      if (tokens[0].equals("DENSWEIGHT")) {
        densityWeightParams = tokens[1];
      }
      if (tokens[0].equals("DONE")) {
        foundDone = true;
      }
      if (tokens[0].equals("EXCLUDE")) {
        exclude = tokens[1];
      }
      if (tokens[0].equals("EXCLUDELIST")) {
        excludeList.parseString(tokens[1]);
      }
      if (tokens[0].equals("EXCLUDELIST2")) {
        excludeList2.parseString(tokens[1]);
      }
      if (tokens[0].equals("FBPINTERP")) {
        fastBackProjInterpOrder = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("FULLIMAGE")) {
        String[] params = tokens[1].split("\\s+", 2);
        fullImageX = Integer.parseInt(params[0]);
        fullImageY = Integer.parseInt(params[1]);
      }
      if (tokens[0].equals("INCLUDE")) {
        include = tokens[1];
      }
      if (tokens[0].equals("LOCALFILE")) {
        localAlignFile = tokens[1];
      }
      if (tokens[0].equals("LOG")) {
        logOffset = Float.parseFloat(tokens[1]);
      }
      if (tokens[0].equals("MASK")) {
        mask = Float.parseFloat(tokens[1]);
      }
      if (tokens[0].equals("MODE")) {
        mode = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals(tiltAngleOffset.getName())) {
        String[] params = tokens[1].split("\\s+", 2);
        tiltAngleOffset.set(params[0]);
        if (params.length > 1) {
          tiltAxisOffset = Float.parseFloat(params[1]);
        }
      }
      if (tokens[0].equals("PARALLEL")) {
        perpendicular = false;
        parallel = true;
      }
      if (tokens[0].equals("PERPENDICULAR")) {
        perpendicular = true;
        parallel = false;
      }
      if (tokens[0].equals("RADIAL")) {
        String[] params = tokens[1].split("\\s+", 2);
        radialBandwidth = Float.parseFloat(params[0]);
        radialFalloff = Float.parseFloat(params[1]);
      }
      if (tokens[0].equals("REPLICATE")) {
        String[] params = tokens[1].split("\\s+", 2);
        nReplicate = Integer.parseInt(params[0]);
        incReplicate = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("SCALE")) {
        String[] params = tokens[1].split("\\s+", 2);
        scaleFLevel = Float.parseFloat(params[0]);
        scaleCoeff = Float.parseFloat(params[1]);
      }
      if (tokens[0].equals(zShift.getName())) {
        String[] params = tokens[1].split("\\s+", 2);
        xShift = Float.parseFloat(params[0]);
        if (params.length > 1) {
          zShift.set(params[1]);
        }
      }
      if (tokens[0].equals("SLICE")) {
        String[] params = tokens[1].split("\\s+", 3);
        idxSliceStart = Integer.parseInt(params[0]);
        idxSliceStop = Integer.parseInt(params[1]);
        if (params.length > 2) {
          incrSlice = Integer.parseInt(params[2]);
        }
      }
      if (tokens[0].equals(SUBSETSTART_KEY)) {
        String[] params = tokens[1].split("\\s+", 2);
        idxXSubsetStart = Integer.parseInt(params[0]);
        idxYSubsetStart = Integer.parseInt(params[1]);
      }
      if (tokens[0].equals(THICKNESS_KEY)) {
        thickness = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("TILTFILE")) {
        tiltFile = tokens[1];
      }
      if (tokens[0].equals("TITLE")) {
        title = tokens[1];
      }
      if (tokens[0].equals("WIDTH")) {
        width = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals(X_AXIS_TILT_KEY)) {
        xAxisTilt = Double.parseDouble(tokens[1]);
      }
      if (tokens[0].equals("XTILTFILE")) {
        xTiltFile = tokens[1];
      }
      if (tokens[0].equals("XTILTINTERP")) {
        xTiltInterp = Integer.parseInt(tokens[1]);
      }
      if (tokens[0].equals("ZFACTORFILE")) {
        useZFactors = true;
        zFactorFileName = tokens[1];
      }
      if (adjustOrigin.equalsNameIgnoreCase(tokens[0])) {
        adjustOrigin.set(true);
      }
      if (tokens[0].equals("ProjectModel")) {
        projectModel = tokens[1];
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
    // Create a new command line argument array

    ArrayList cmdLineArgs = new ArrayList(32);
    ComScriptInputArg newArg = new ComScriptInputArg();
    newArg.setArgument(inputFile);
    cmdLineArgs.add(newArg);
    newArg = new ComScriptInputArg();
    newArg.setArgument(outputFile);
    cmdLineArgs.add(newArg);
    if (imageBinned.isNotNullAndNotDefault()) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(imageBinned.getName() + " " + imageBinned.getLong());
      cmdLineArgs.add(newArg);
    }
    if (!angles.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("ANGLES " + angles);
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(compressionFraction)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("COMPFRACTION " + String.valueOf(compressionFraction));
      cmdLineArgs.add(newArg);
    }
    if (!compression.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("COMPRESS " + compression);
      cmdLineArgs.add(newArg);
    }
    if (cosInterpOrder > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("COSINTERP " + String.valueOf(cosInterpOrder) + " "
          + String.valueOf(cosInterpFactor));
      cmdLineArgs.add(newArg);
    }
    if (!densityWeightParams.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("DENSWEIGHT " + densityWeightParams);
      cmdLineArgs.add(newArg);
    }
    if (!exclude.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDE " + exclude);
      cmdLineArgs.add(newArg);
    }
    if (excludeList.getNElements() > 0) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDELIST " + excludeList.toString());
      cmdLineArgs.add(newArg);
    }
    if (excludeList2.getNElements() > 0) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("EXCLUDELIST2 " + excludeList2.toString());
      cmdLineArgs.add(newArg);
    }
    if (fastBackProjInterpOrder > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg
          .setArgument("FBPINTERP " + String.valueOf(fastBackProjInterpOrder));
      cmdLineArgs.add(newArg);
    }
    if (fullImageX > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("FULLIMAGE " + String.valueOf(fullImageX) + " "
          + String.valueOf(fullImageY));
      cmdLineArgs.add(newArg);
    }
    if (!include.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("INCLUDE " + include);
      cmdLineArgs.add(newArg);
    }
    if (!localAlignFile.equals("") && !fiducialess.is()) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("LOCALFILE " + localAlignFile);
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(logOffset)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("LOG " + String.valueOf(logOffset));
      cmdLineArgs.add(newArg);
    }
    if (mode > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("MODE " + String.valueOf(mode));
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(mask)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("MASK " + String.valueOf(mask));
      cmdLineArgs.add(newArg);
    }
    if (!tiltAngleOffset.isNull()) {
      String arg = tiltAngleOffset.getName() + " " + tiltAngleOffset.toString();
      if (!Float.isNaN(tiltAxisOffset)) {
        arg += " " + String.valueOf(tiltAxisOffset);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg);
      cmdLineArgs.add(newArg);
    }
    if (parallel) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("PARALLEL");
      cmdLineArgs.add(newArg);
    }
    if (perpendicular) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("PERPENDICULAR");
      cmdLineArgs.add(newArg);
    }
    if (adjustOrigin.is()) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(adjustOrigin.getName());
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(radialBandwidth)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("RADIAL " + String.valueOf(radialBandwidth) + " "
          + String.valueOf(radialFalloff));
      cmdLineArgs.add(newArg);
    }
    if (nReplicate > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("REPLICATE " + String.valueOf(nReplicate) + " "
          + String.valueOf(incReplicate));
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(scaleFLevel)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("SCALE " + String.valueOf(scaleFLevel) + " "
          + String.valueOf(scaleCoeff));
      cmdLineArgs.add(newArg);
    }
    if (!Float.isNaN(xShift) || !zShift.isNull()) {
      StringBuffer arg = new StringBuffer(zShift.getName() + " ");
      if (Float.isNaN(xShift)) {
        arg.append("0 ");
      }
      else {
        arg.append(String.valueOf(xShift));
      }
      if (!zShift.isNull()) {
        arg.append(" " + zShift.toString());
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg.toString());
      cmdLineArgs.add(newArg);
    }
    if (idxSliceStart > Integer.MIN_VALUE) {
      String arg = "SLICE " + String.valueOf(idxSliceStart) + " "
          + String.valueOf(idxSliceStop);
      if (incrSlice > Integer.MIN_VALUE) {
        arg += " " + String.valueOf(incrSlice);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument(arg);
      cmdLineArgs.add(newArg);
    }
    if (idxXSubsetStart > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("SUBSETSTART " + String.valueOf(idxXSubsetStart) + " "
          + String.valueOf(idxYSubsetStart));
      cmdLineArgs.add(newArg);
    }
    if (thickness > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(THICKNESS_KEY + " " + String.valueOf(thickness));
      cmdLineArgs.add(newArg);
    }
    if (!tiltFile.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("TILTFILE " + tiltFile);
      cmdLineArgs.add(newArg);
    }
    if (width > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("WIDTH " + String.valueOf(width));
      cmdLineArgs.add(newArg);
    }
    if (!Double.isNaN(xAxisTilt)) {
      newArg = new ComScriptInputArg();
      newArg.setArgument(X_AXIS_TILT_KEY + " " + String.valueOf(xAxisTilt));
      cmdLineArgs.add(newArg);
    }
    //A fiducialess align means that tilt should not use the xtilt file.
    if (!fiducialess.is()) {
      //backwards compatibility: if xTiltFile is empty, set the default xtilt file name
      if (xTiltFile == null || xTiltFile.matches("\\s*")) {
        xTiltFile = DatasetFiles.getXTiltFileName(manager, axisID);
      }
      //use xtilt file if the file exists
      //This is backwards compatibility issue since the only good reason for the
      //file not to exist is that the state comes from an earlier version.
      if (new File(manager.getPropertyUserDir(), xTiltFile).exists()) {
        newArg = new ComScriptInputArg();
        newArg.setArgument("XTILTFILE " + xTiltFile);
        cmdLineArgs.add(newArg);
      }
    }

    if (xTiltInterp > Integer.MIN_VALUE) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("XTILTINTERP " + String.valueOf(xTiltInterp));
      cmdLineArgs.add(newArg);
    }
    if (useZFactors && !fiducialess.is()) {
      if (zFactorFileName == null || zFactorFileName.matches("\\s*")) {
        zFactorFileName = TiltalignParam.getOutputZFactorFileName(datasetName,
            axisID);
      }
      newArg = new ComScriptInputArg();
      newArg.setArgument("ZFACTORFILE " + zFactorFileName);
      cmdLineArgs.add(newArg);
    }
    if (!projectModel.equals("")) {
      newArg = new ComScriptInputArg();
      newArg.setArgument("ProjectModel " + projectModel);
      cmdLineArgs.add(newArg);
    }
    newArg = new ComScriptInputArg();
    newArg.setArgument("DONE");
    cmdLineArgs.add(newArg);
    int nArgs = cmdLineArgs.size();
    scriptCommand.setInputArguments((ComScriptInputArg[]) cmdLineArgs
        .toArray(new ComScriptInputArg[nArgs]));
  }

  public void initializeDefaults() {
  }

  public ConstEtomoNumber setImageBinned(final int imageBinned) {
    return this.imageBinned.set(imageBinned);
  }

  public void setImageBinned(final long input) {
    imageBinned.set(input);
  }

  public void setInclude(final String input) {
    include = input;
  }

  public void setIncReplicate(final int input) {
    incReplicate = input;
  }

  /**
   * If the current binning can be retrieved, set imageBinned to current
   * binning.  If not, and imageBinned is null then set imageBinned to 1.
   * @return
   */
  public ConstEtomoNumber setImageBinned() {
    EtomoNumber currentBinning = new EtomoNumber(EtomoNumber.Type.LONG);
    currentBinning.set(UIExpertUtilities.INSTANCE.getStackBinning(manager,
        axisID, ".ali", true));
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

  public void setFastBackProjInterpOrder(final int input) {
    fastBackProjInterpOrder = input;
  }

  public void resetExcludeList() {
    excludeList.setNElements(0);
  }

  /**
   * If the tilt axis angle is closer to 90 degree, x and y need to be transposed.
   * The .ali file will already be transposed.  So just transpose the goodframe
   * outputs.
   * @throws InvalidParameterException
   * @throws IOException
   */
  public void setMontageSubsetStart() throws InvalidParameterException,
      IOException {
    resetSubsetStart();
    Goodframe goodframe = etomo.comscript.Utilities
        .getGoodframeFromMontageSize(axisID, manager);
    if (goodframe != null) {
      MRCHeader header = MRCHeader.getInstance(manager, axisID, ".ali", manager
          .getManagerKey());
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

  public void setNReplicate(final int input) {
    nReplicate = input;
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
      MRCHeader aliHeader = MRCHeader.getInstance(manager, axisID, ".ali",
          manager.getManagerKey());
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
    catch (InvalidParameterException e) {
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
    catch (InvalidParameterException e) {
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
    inputFile = file;
  }

  public void setLoadedFromFile(final boolean input) {
    loadedFromFile = input;
  }

  public void resetInputFile() {
    inputFile = "";
  }

  public void setLocalAlignFile(final String filename) {
    localAlignFile = filename;
  }

  public void setLocalScale(final float input) {
    localScale = input;
  }

  public void setLogOffset(final float input) {
    logOffset = input;
  }

  public void setProjectModel(final FileType input) {
    projectModel = input.getFileName(manager, axisID);
  }

  public void resetLocalAlignFile() {
    localAlignFile = "";
  }

  public void setLogShift(final float shift) {
    logOffset = shift;
  }

  public void setMask(final float input) {
    mask = input;
  }

  public void resetLogShift() {
    logOffset = Float.NaN;
  }

  public void setMode(final int newMode) {
    mode = newMode;
  }

  public void resetMode() {
    mode = Integer.MIN_VALUE;
  }

  public void setOutputFile(final String file) {
    outputFile = file;
  }

  public void resetOutputFile() {
    outputFile = "";
  }

  public void setParallel() {
    parallel = true;
    perpendicular = false;
  }

  public void setParallel(final boolean input) {
    parallel = input;
  }

  public void setPerpendicular() {
    parallel = false;
    perpendicular = true;
  }

  public void setPerpendicular(final boolean input) {
    perpendicular = input;
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
    parallel = false;
    perpendicular = false;
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
    thickness = input;
  }

  public void setThickness(final String newThickness) {
    thickness = Integer.parseInt(newThickness);
  }

  public void resetThickness() {
    thickness = Integer.MIN_VALUE;
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
    tiltFile = filename;
  }

  public void setTitle(final String input) {
    title = input;
  }

  /**
   * @param i
   */
  public void setWidth(final int i) {
    width = i;
  }

  public void resetWidth() {
    width = Integer.MIN_VALUE;
  }

  public void setXAxisTilt(final double input) {
    xAxisTilt = input;
  }

  public void setXAxisTilt(final String angle) {
    xAxisTilt = Double.parseDouble(angle);
  }

  public void resetXAxisTilt() {
    xAxisTilt = Double.NaN;
  }

  /**
   * @param d
   */
  public void setXShift(final float d) {
    xShift = d;
  }

  public void setXTiltFile(final String input) {
    xTiltFile = input;
  }
  
  public void setXTiltInterp(final int input) {
    xTiltInterp=input;
  }
  
  public void setZFactorFileName(final String input) {
    zFactorFileName=input;
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
      if (width != Integer.MIN_VALUE && width != 0) {
        width *= correctionBinning;
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
      if (thickness != Integer.MIN_VALUE && thickness != 0) {
        thickness *= correctionBinning;
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

    public static final Field ANGLES = new Field();
    public static final Field COMPRESSION = new Field();
    public static final Field COS_INTERP_ORDER = new Field();
    public static final Field COMPRESSION_FRACTION = new Field();
    public static final Field COS_INTERP_FACTOR = new Field();
    public static final Field X_AXIS_TILT = new Field();
    public static final Field FIDUCIALESS = new Field();
    public static final Field Z_SHIFT = new Field();
    public static final Field TILT_ANGLE_OFFSET = new Field();
    public static final Field ADJUST_ORIGIN = new Field();
    public static final Field DENSITY_WEIGHT_PARAMS = new Field();
    public static final Field EXCLUDE = new Field();
    public static final Field EXCLUDE_LIST = new Field();
    public static final Field EXCLUDE_LIST_2 = new Field();
    public static final Field FAST_BACK_PROJ_INTERP_ORDER = new Field();
    public static final Field FULL_IMAGE_X = new Field();
    public static final Field FULL_IMAGE_Y = new Field();
    public static final Field IDX_SLICE_START = new Field();
    public static final Field IDX_SLICE_STOP = new Field();
    public static final Field IDX_X_SUBSET_START = new Field();
    public static final Field IDX_Y_SUBSET_START = new Field();
    public static final Field IMAGE_BINNED = new Field();
    public static final Field INCLUDE = new Field();
    public static final Field INCR_SLICE = new Field();
    public static final Field INC_REPLICATE = new Field();
    public static final Field INPUT_FILE = new Field();
    public static final Field LOADED_FROM_FILE = new Field();
    public static final Field LOCAL_ALIGN_FILE = new Field();
    public static final Field LOCAL_SCALE = new Field();
    public static final Field LOG_OFFSET = new Field();
    public static final Field MASK = new Field();
    public static final Field MODE = new Field();
    public static final Field N_REPLICATE = new Field();
    public static final Field OUTPUT_FILE = new Field();
    public static final Field PARALLEL = new Field();
    public static final Field PERPENDICULAR = new Field();
    public static final Field RADIAL_BANDWIDTH = new Field();
    public static final Field RADIAL_FALLOFF = new Field();
    public static final Field SCALE_COEFF = new Field();
    public static final Field SCALE_F_LEVEL = new Field();
    public static final Field THICKNESS = new Field();
    public static final Field TILT_AXIS_OFFSET = new Field();
    public static final Field TILT_FILE = new Field();
    public static final Field TITLE = new Field();
    public static final Field USE_Z_FACTORS = new Field();
    public static final Field WIDTH = new Field();
    public static final Field X_SHIFT = new Field();
    public static final Field X_TILT_FILE = new Field();
    public static final Field  X_TILT_INTERP = new Field();
    public static final Field  Z_FACTOR_FILE_NAME = new Field();
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