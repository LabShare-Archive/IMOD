package etomo.comscript;

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

public class ConstTiltParam {
  public static final String rcsid =
    "$Id$";

  protected String inputFile = "";

  protected String outputFile = "";

  protected String angles = "";
  protected boolean useAngles = false;

  protected double compressionFraction;
  protected boolean useCompressionFraction = false;

  protected String compression;
  protected boolean useCompression = false;

  protected int cosInterpOrder;
  protected double cosInterpFactor;
  protected boolean useCosInterp = false;

  protected String densityWeightParams = "";
  protected boolean useDensityWeight = false;

  protected String exclude = "";
  protected boolean useExclude = false;

  protected StringList excludeList = new StringList(0);
  protected boolean useExcludeList = false;

  protected int fastBackProjInterpOrder;
  protected boolean useFastBackProjInterpOrder = false;

  protected int fullImageX;
  protected int fullImageY;
  protected boolean useFullImage = false;

  protected String include = "";
  protected boolean useInclude = false;

  protected String localAlignFile = "";
  protected boolean useLocalAlignFile = false;

  protected double localScale;
  protected boolean useLocalScale = false;

  protected double logOffset;
  protected boolean useLogOffset = false;

  protected double mask;
  protected boolean useMask = false;

  protected int mode;
  protected boolean useMode = false;

  protected double tiltAngleOffset;
	protected boolean useTiltAngleOffset = false;
  protected double tiltAxisOffset;
	protected boolean useTiltAxisOffset = false;

  protected boolean parallel = false;
  protected boolean useParallel = false;

  protected boolean perpendicular = false;
  protected boolean usePerpendicular = false;

  protected double radialBandwidth = 0.0;
  protected double radialFalloff = 0.0;
  protected boolean useRadialWeightingFunction = false;

  protected int  nReplicate;
  protected int  incReplicate;
  protected boolean useReplicate = false;
  
  protected double scaleFLevel;
  protected double scaleCoeff;
  protected boolean useScale = false;
 
  protected double xOffset;
	protected boolean useXOffset = false;
  protected double zOffset;
	protected boolean useZOffset = false;

  protected int idxSliceStart;
  protected int idxSliceStop;
	protected boolean useSlice = false;
	
  protected int idxSliceIncr;
	protected boolean useSliceIncr = false;

  protected int idxXSubsetStart;
  protected int idxYSubsetStart;
  protected boolean useSubsetStart = false;

  protected int thickness;
  protected boolean useThickness = false;

  protected String tiltFile = "";
  protected boolean useTiltfile = false;

  protected String title = "";
  protected boolean useTitle = false;

  protected int width;
  protected boolean useWidth = false;

  protected double xAxisTilt;
  protected boolean useXAxisTilt = false;

  protected String xTiltFile = "";
  protected boolean useXTiltFile = false;

  protected int xTiltInterp;
  protected boolean useXTiltInterp = false;

  public String getInputFile() {
    return inputFile;
  }

  public double getLogShift() {
    return logOffset;
  }

  public int getMode() {
    return mode;
  }

  public String getLocalAlignFile() {
    return localAlignFile;
  }

  public boolean getUseLocalAlignFile() {
    return useLocalAlignFile;
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

  public double getRadialBandwidth() {
    return radialBandwidth;
  }

  public int getThickness() {
    return thickness;
  }

  public String getTiltFile() {
    return tiltFile;
  }

  public double getXAxisTilt() {
    return xAxisTilt;
  }

  /**
   * Gets the excludeList.
   * @return Returns a String
   */
  public String getExcludeList() {
    return excludeList.toString();
  }

  /**
   * @return
   */
  public double getRadialFalloff() {
    return radialFalloff;
  }

  /**
   * @return
   */
  public int getWidth() {
    return width;
  }

  /**
   * @return
   */
  public double getXOffset() {
    return xOffset;
  }

  /**
   * @return
   */
  public double getZOffset() {
    return zOffset;
  }


  /**
   * @return
   */
  public int getIdxSliceIncr() {
    return idxSliceIncr;
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

  /**
   * @return
   */
  public double getTiltAngleOffset() {
    return tiltAngleOffset;
  }

  /**
   * @return
   */
  public double getTiltAxisOffset() {
    return tiltAxisOffset;
  }


  /**
   * @return
   */
  public boolean hasTiltAngleOffset() {
    return useTiltAngleOffset;
  }

	/**
	 * @return
	 */
	public boolean hasTiltAxisOffset() {
		return useTiltAxisOffset;
	}

  /**
   * @return
   */
  public boolean hasAngles() {
    return useAngles;
  }

  /**
   * @return
   */
  public boolean hasCompression() {
    return useCompression;
  }

  /**
   * @return
   */
  public boolean hasCompressionFraction() {
    return useCompressionFraction;
  }

  /**
   * @return
   */
  public boolean hasCosInterp() {
    return useCosInterp;
  }

  /**
   * @return
   */
  public boolean hasDensityWeight() {
    return useDensityWeight;
  }

  /**
   * @return
   */
  public boolean hasExclude() {
    return useExclude;
  }

  /**
   * @return
   */
  public boolean hasExcludeList() {
    return useExcludeList;
  }

  /**
   * @return
   */
  public boolean hasFastBackProjInterpOrder() {
    return useFastBackProjInterpOrder;
  }

  /**
   * @return
   */
  public boolean hasFullImage() {
    return useFullImage;
  }

  /**
   * @return
   */
  public boolean hasInclude() {
    return useInclude;
  }

  /**
   * @return
   */
  public boolean hasLocalScale() {
    return useLocalScale;
  }

  /**
   * @return
   */
  public boolean hasLogOffset() {
    return useLogOffset;
  }

  /**
   * @return
   */
  public boolean hasMask() {
    return useMask;
  }

  /**
   * @return
   */
  public boolean hasMode() {
    return useMode;
  }

  /**
   * @return
   */
  public boolean hasParallel() {
    return useParallel;
  }

  /**
   * @return
   */
  public boolean hasPerpendicular() {
    return usePerpendicular;
  }

  /**
   * @return
   */
  public boolean hasRadialWeightingFunction() {
    return useRadialWeightingFunction;
  }

  /**
   * @return
   */
  public boolean hasReplicate() {
    return useReplicate;
  }

  /**
   * @return
   */
  public boolean hasScale() {
    return useScale;
  }

  /**
   * @return
   */
  public boolean hasXOffset() {
    return useXOffset;
  }

	/**
	 * @return
	 */
	public boolean hasZOffset() {
		return useZOffset;
	}

  /**
   * @return
   */
  public boolean hasSlice() {
    return useSlice;
  }

	/**
	 * @return
	 */
	public boolean hasSliceIncr() {
		return useSliceIncr;
	}

  /**
   * @return
   */
  public boolean hasSubsetStart() {
    return useSubsetStart;
  }

  /**
   * @return
   */
  public boolean hasThickness() {
    return useThickness;
  }

  /**
   * @return
   */
  public boolean hasTiltfile() {
    return useTiltfile;
  }

  /**
   * @return
   */
  public boolean hasTitle() {
    return useTitle;
  }

  /**
   * @return
   */
  public boolean hasWidth() {
    return useWidth;
  }

  /**
   * @return
   */
  public boolean hasXAxisTilt() {
    return useXAxisTilt;
  }

  /**
   * @return
   */
  public boolean hasXTiltFile() {
    return useXTiltFile;
  }

  /**
   * @return
   */
  public boolean hasXTiltInterp() {
    return useXTiltInterp;
  }

  /**
   * @return
   */
  public double getScaleCoeff() {
    return scaleCoeff;
  }

  /**
   * @return
   */
  public double getScaleFLevel() {
    return scaleFLevel;
  }

}