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

import etomo.type.AxisID;

public class ConstTiltParam {
  public static final String rcsid = 
  "$Id$";
  
  protected String inputFile;

  protected String outputFile;

  protected String angles;

  protected float compressionFraction;

  protected String compression;

  protected int cosInterpOrder;

  protected float cosInterpFactor;

  protected String densityWeightParams;

  protected String exclude;

  protected StringList excludeList;

  protected int fastBackProjInterpOrder;

  protected int fullImageX;

  protected int fullImageY;

  protected String include;

  protected String localAlignFile;

  protected float localScale;

  protected float logOffset;

  protected float mask;

  protected int mode;

  protected float tiltAngleOffset;

  protected float tiltAxisOffset;

  protected boolean parallel;

  protected boolean perpendicular;

  protected float radialBandwidth;

  protected float radialFalloff;

  protected int nReplicate;

  protected int incReplicate;

  protected float scaleFLevel;

  protected float scaleCoeff;

  protected float xOffset;

  protected float zOffset;

  protected int idxSliceStart;

  protected int idxSliceStop;

  protected int incrSlice;

  protected int idxXSubsetStart;

  protected int idxYSubsetStart;

  protected int thickness;

  protected String tiltFile;

  protected String title;

  protected int width;

  protected float xAxisTilt;

  protected String xTiltFile;

  protected int xTiltInterp;
  
  protected boolean fiducialess;
  
  protected boolean useZFactors;
  protected String zFactorFileName;
  protected StringList excludeList2;
  
  protected String datasetName;
  protected AxisID axisID;

  public ConstTiltParam(String datasetName, AxisID axisID) {
    this.datasetName = datasetName;
    this.axisID = axisID;
    reset();
  }

  protected void reset() {
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
    tiltAngleOffset = Float.NaN;
    tiltAxisOffset = Float.NaN;
    parallel = false;
    perpendicular = false;
    radialBandwidth = Float.NaN;
    radialFalloff = Float.NaN;
    nReplicate = Integer.MIN_VALUE;
    incReplicate = Integer.MIN_VALUE;
    scaleFLevel = Float.NaN;
    scaleCoeff = Float.NaN;
    xOffset = Float.NaN;
    zOffset = Float.NaN;
    idxSliceStart = Integer.MIN_VALUE;
    idxSliceStop = Integer.MIN_VALUE;
    incrSlice = Integer.MIN_VALUE;
    idxXSubsetStart = Integer.MIN_VALUE;
    idxYSubsetStart = Integer.MIN_VALUE;
    thickness = Integer.MIN_VALUE;
    tiltFile = "";
    title = "";
    width = Integer.MIN_VALUE;
    xAxisTilt = Float.NaN;
    xTiltFile = "";
    xTiltInterp = Integer.MIN_VALUE;
    fiducialess = false;
    useZFactors = false;
    excludeList2 = new StringList(0);
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

  public float getXAxisTilt() {
    return xAxisTilt;
  }

  public boolean hasXAxisTilt() {
    if (Float.isNaN(xAxisTilt))
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
  public float getXOffset() {
    return xOffset;
  }

  public boolean hasXOffset() {
    if (Float.isNaN(xOffset))
      return false;
    return true;
  }

  /**
   * @return
   */
  public float getZOffset() {
    return zOffset;
  }

  public boolean hasZOffset() {
    if (Float.isNaN(zOffset))
      return false;
    return true;
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
  public float getTiltAngleOffset() {
    return tiltAngleOffset;
  }

  public boolean hasTiltAngleOffset() {
    if (Float.isNaN(tiltAngleOffset))
      return false;
    return true;
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
  

}