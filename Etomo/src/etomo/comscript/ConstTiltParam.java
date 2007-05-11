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
 * <p> Revision 3.12  2007/03/07 21:00:26  sueh
 * <p> bug# 981 Reduced visibility of protected fields to package private.
 * <p>
 * <p> Revision 3.11  2007/02/05 21:41:49  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 3.10  2006/09/19 21:58:39  sueh
 * <p> bug# 920 Added Storables, an object for storing variables that don't going into the
 * <p> the .com file.  Storables can be added to MetaData or another storable type
 * <p> without creating an instance of TiltParam.  Moved fiducialess to Storables.
 * <p>
 * <p> Revision 3.9  2006/09/13 23:12:40  sueh
 * <p> bug# 920 Changed xOffset and zOffset to xShift and zShift.  Changed zShift
 * <p> and tiltAngleOffset to EtomoNumber.
 * <p>
 * <p> Revision 3.8  2006/05/11 19:41:58  sueh
 * <p> bug# 838 Add CommandDetails, which extends Command and
 * <p> ProcessDetails.  Changed ProcessDetails to only contain generic get
 * <p> functions.  Command contains all the command oriented functions.
 * <p>
 * <p> Revision 3.7  2005/07/29 00:44:46  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.6  2005/06/10 22:49:57  sueh
 * <p> bug# 583, bug# 682 Moved binning calculation to ApplicationManager.
 * <p> Upgraded tilt.com to have all unbinned parameters and a binning value.
 * <p> Added member variables:  imageBinned, loadedFromFile.  Added
 * <p> function:  isOldVersion.
 * <p>
 * <p> Revision 3.5  2005/01/12 18:33:43  sueh
 * <p> bug# 505 Added excludeList2.
 * <p>
 * <p> Revision 3.4  2005/01/08 01:37:28  sueh
 * <p> bug# 578  Added useZFactors, which is set by the user, and
 * <p> zFactorFileName, which is generated or comes from the comscript.
 * <p>
 * <p> Revision 3.3  2004/07/20 23:05:29  sueh
 * <p> bug# 502 adding fiducialess, which is not retrieved from tilt
 * <p>
 * <p> Revision 3.2  2004/03/24 18:11:55  rickg
 * <p> xAxisTilt default value corrected
 * <p>
 * <p> Revision 3.1  2004/03/24 02:55:44  rickg
 * <p> Bug# 395 Implemented ability to create binned tomogram
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.6  2003/10/22 21:30:28  rickg
 * <p> Bug# 287 Default value handling for SLICE OFFSET and SHIFT
 * <p>
 * <p> Revision 2.5  2003/08/21 22:17:27  rickg
 * <p> Added density scaling getters
 * <p>
 * <p> Revision 2.4  2003/06/25 22:15:50  rickg
 * <p> Manage all tilt parameters
 * <p>
 * <p> Revision 2.3  2003/06/10 22:55:36  rickg
 * <p> Modeled all of the parameters from the man page
 * <p>
 * <p> Revision 2.2  2003/05/23 21:26:32  rickg
 * <p> Implemented radial filter parameters
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
package etomo.comscript;

import java.util.Hashtable;
import java.util.Properties;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstIntKeyList;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.ScriptParameter;

public class ConstTiltParam implements ProcessDetails {
  public static final String rcsid = "$Id$";

  public static final String COMMAND_NAME = "tilt";
  public static final String THICKNESS_KEY = "THICKNESS";
  public static final String X_AXIS_TILT_KEY = "XAXISTILT";

  String inputFile;
  String outputFile;
  String angles;
  float compressionFraction;
  String compression;
  int cosInterpOrder;
  float cosInterpFactor;
  String densityWeightParams;
  String exclude;
  StringList excludeList;
  int fastBackProjInterpOrder;
  int fullImageX;
  int fullImageY;
  String include;
  String localAlignFile;
  float localScale;
  float logOffset;

  float mask;

  int mode;

  EtomoNumber tiltAngleOffset = new EtomoNumber(EtomoNumber.Type.FLOAT,
      "OFFSET");

  float tiltAxisOffset;

  boolean parallel;

  boolean perpendicular;

  float radialBandwidth;

  float radialFalloff;

  int nReplicate;

  int incReplicate;

  float scaleFLevel;
  float scaleCoeff;

  float xShift;

  EtomoNumber zShift = new EtomoNumber(EtomoNumber.Type.FLOAT, "SHIFT");

  int idxSliceStart;

  int idxSliceStop;

  int incrSlice;

  int idxXSubsetStart;

  int idxYSubsetStart;

  int thickness;

  String tiltFile;

  String title;

  int width;

  double xAxisTilt;

  String xTiltFile;
  int xTiltInterp;

  boolean useZFactors;
  String zFactorFileName;
  StringList excludeList2;
  ScriptParameter imageBinned;

  boolean loadedFromFile = false;
  String datasetName;
  AxisID axisID;
  Storables storables = new Storables();

  final ApplicationManager manager;

  public ConstTiltParam(final ApplicationManager manager,
      final String datasetName, final AxisID axisID) {
    this.manager = manager;
    this.datasetName = datasetName;
    this.axisID = axisID;
    //do not default imageBinned
    imageBinned = new ScriptParameter(EtomoNumber.Type.LONG, "IMAGEBINNED");
    imageBinned.setFloor(1);
    reset();
    storables.reset();
  }

  void reset() {
    loadedFromFile = false;
    inputFile = "";
    outputFile = "";
    angles = "";
    compressionFraction = Float.NaN;
    compression = "";
    cosInterpOrder = Integer.MIN_VALUE;
    cosInterpFactor = Float.NaN;
    densityWeightParams = "";
    exclude = "";
    excludeList = new StringList(0);
    fastBackProjInterpOrder = Integer.MIN_VALUE;
    fullImageX = Integer.MIN_VALUE;
    fullImageY = Integer.MIN_VALUE;
    include = "";
    localAlignFile = "";
    localScale = Float.NaN;
    logOffset = Float.NaN;
    mask = Float.NaN;
    mode = Integer.MIN_VALUE;
    tiltAngleOffset.reset();
    tiltAxisOffset = Float.NaN;
    parallel = false;
    perpendicular = false;
    radialBandwidth = Float.NaN;
    radialFalloff = Float.NaN;
    nReplicate = Integer.MIN_VALUE;
    incReplicate = Integer.MIN_VALUE;
    scaleFLevel = Float.NaN;
    scaleCoeff = Float.NaN;
    xShift = Float.NaN;
    zShift.reset();
    idxSliceStart = Integer.MIN_VALUE;
    idxSliceStop = Integer.MIN_VALUE;
    incrSlice = Integer.MIN_VALUE;
    idxXSubsetStart = Integer.MIN_VALUE;
    idxYSubsetStart = Integer.MIN_VALUE;
    thickness = Integer.MIN_VALUE;
    tiltFile = "";
    title = "";
    width = Integer.MIN_VALUE;
    xAxisTilt = Double.NaN;
    xTiltFile = "";
    xTiltInterp = Integer.MIN_VALUE;
    useZFactors = false;
    excludeList2 = new StringList(0);
    imageBinned.reset();
  }

  public ConstEtomoNumber getImageBinned() {
    return imageBinned;
  }

  public String getInputFile() {
    return inputFile;
  }

  public float getLogShift() {
    return logOffset;
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

  public boolean getBooleanValue(etomo.comscript.Fields field) {
    if (field == Fields.FIDUCIALESS) {
      return storables.getFiducialess().is();
    }
    throw new IllegalArgumentException("field=" + field);
  }
  
  public String[] getStringArray(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public String getString(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public double getDoubleValue(etomo.comscript.Fields field) {
    if (field == Fields.X_AXIS_TILT) {
      return xAxisTilt;
    }
    if (field == Fields.Z_SHIFT) {
      return zShift.getDouble();
    }
    if (field == Fields.TILT_ANGLE_OFFSET) {
      return tiltAngleOffset.getDouble();
    }
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstEtomoNumber getEtomoNumber(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public ConstIntKeyList getIntKeyList(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public int getIntValue(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public Hashtable getHashtable(etomo.comscript.Fields field) {
    throw new IllegalArgumentException("field=" + field);
  }

  public static final class Fields implements etomo.comscript.Fields {
    private Fields() {
    }

    public static final Fields X_AXIS_TILT = new Fields();
    public static final Fields FIDUCIALESS = new Fields();
    public static final Fields Z_SHIFT = new Fields();
    public static final Fields TILT_ANGLE_OFFSET = new Fields();
  }

  public static final class Storables {
    private final EtomoBoolean2 fiducialess = new EtomoBoolean2("Fiducialess");

    void reset() {
      fiducialess.reset();
    }

    public void store(Properties props, String prepend) {
      prepend = createPrepend(prepend);
      fiducialess.store(props, prepend);
    }

    public void load(Properties props, String prepend) {
      reset();
      prepend = createPrepend(prepend);
      fiducialess.load(props, prepend);
    }

    public void set(ConstTiltParam.Storables storables) {
      fiducialess.set(storables.fiducialess);
    }

    public void set(ConstTiltParam tiltParam) {
      set(tiltParam.storables);
    }

    private static String createPrepend(String prepend) {
      if (prepend == "") {
        return ComScriptManager.PARAM_KEY + '.' + COMMAND_NAME;
      }
      return prepend + "." + ComScriptManager.PARAM_KEY + '.' + COMMAND_NAME;
    }

    protected ConstEtomoNumber getFiducialess() {
      return fiducialess;
    }

    protected void setFiducialess(boolean fiducialess) {
      this.fiducialess.set(fiducialess);
    }
  }
}