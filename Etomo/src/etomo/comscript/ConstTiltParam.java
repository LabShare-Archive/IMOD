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

  protected String compressionFraction = "";
  protected boolean useCompressionFraction = false;

  protected String compression = "";
  protected boolean useCompression = false;

  protected String cosInterpOrder = "";
  protected boolean useCosInterpOrder = false;

  protected String densityWeight = "";
  protected boolean useDensityWeight = false;

  protected String exclude = "";
  protected boolean useExclude = false;

  protected StringList excludeList = new StringList(0);
  protected boolean useExcludeList = false;

  protected int fastBackProjInterpOrder;
  protected boolean useFastBackProjInterpOrder = false;

  protected String fullImage = "";
  protected boolean useFullImage = false;

  protected String include = "";
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

  protected String offset = "";
  protected boolean useOffset = false;

  protected boolean parallel = false;
  protected boolean useParallel = false;

  protected boolean perpendicular = false;
  protected boolean usePerpendicular = false;

  protected String radialWeightingFunction = "";
  protected boolean useRadialWeightingFunction = false;

  protected String scale = "";
  protected boolean useScale = false;

  protected String shift = "";
  protected boolean useShift = false;

  protected String slice = "";
  protected boolean useSlice = false;

  protected String subsetStart = "";
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

  protected int xTiltOrder;
  protected boolean useXTiltORder = false;

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

  public String getRadialWeightingFunction() {
    return radialWeightingFunction;
  }

  public String getSubsetStart() {
    return subsetStart;
  }

  public String getScale() {
    return scale;
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

}
