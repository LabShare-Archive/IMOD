package etomo.comscript;

import java.util.ArrayList;

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

  protected ArrayList angles = new ArrayList();
  protected boolean useAngles = false;

  protected double compressionFraction;
  protected boolean useCompressionFraction = false;

  protected double compression;
  protected boolean useCompression = false;

  protected int cosInterpOrder;
  protected double cosInterpFactor;
  protected boolean useCosInterp = false;

  protected String densityWeightNadjacent = "";
  protected ArrayList densityWeights = new ArrayList();
  protected boolean useDensityWeight = false;

  protected ArrayList exclude = new ArrayList();
  protected boolean useExclude = false;

  protected StringList excludeList = new StringList(0);
  protected boolean useExcludeList = false;

  protected int fastBackProjInterpOrder;
  protected boolean useFastBackProjInterpOrder = false;

  protected int fullImageX;
  protected int fullImageY;
  protected boolean useFullImage = false;

  protected ArrayList include = new ArrayList();
  protected boolean useInclude = false;

  protected String localAlignFile = "";
  protected boolean useLocalAlignFile = false;

  protected double localScale;
  protected boolean useLocalScale = false;

  protected double logShift;
  protected boolean useLogShift = false;

  protected double mask;
  protected boolean useMask = false;

  protected int mode;
  protected boolean useMode = false;

  protected double offsetAngle;
  protected double offsetX;
  protected boolean useOffset = false;

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
  protected double zOffset;
  protected boolean useShift = false;

  protected int idxSliceStart;
  protected int idxSliceStop;
  protected int idxSliceIncr;
  protected boolean useSlice = false;

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
    return logShift;
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

}
