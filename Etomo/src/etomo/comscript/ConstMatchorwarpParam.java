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
 * <p> Revision 3.1  2004/03/06 03:46:07  sueh
 * <p> bug# 380 added useLinearInterpolation
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */
public class ConstMatchorwarpParam {
  public static final String rcsid = "$Id$";
  
  protected String size = "";
  protected double refineLimit;
  protected boolean useRefinelimit = false;
  protected String warpLimit = "";
  protected String modelFile = "";
  protected String patchFile = "";
  protected String solveFile = "";
  protected String refineFile = "";
  protected String inverseFile = "";
  protected String warpFile = "";
  protected String tempDir = "";
  protected int xLowerExclude = 0;
  protected int xUpperExclude = 0;
  protected int zLowerExclude = 0;
  protected int zUpperExclude = 0;
  protected boolean trial = false;
  protected String inputFile = "";
  protected String outputFile = "";
  protected boolean useLinearInterpolation = false;
  
  public ConstMatchorwarpParam() {
  }

  
  /**
   * @return String
   */
  public String getInverseFile() {
    return inverseFile;
  }

  /**
   * @return String
   */
  public String getModelFile() {
    return modelFile;
  }
  
  public boolean isUseModelFile() {
    return !ParamUtilities.isEmpty(modelFile);
  }

  /**
   * @return String
   */
  public String getPatchFile() {
    return patchFile;
  }

  /**
   * @return double
   */
  public double getRefineLimit() {
    return refineLimit;
  }

  /**
   * @return String
   */
  public String getSize() {
    return size;
  }

  /**
   * @return String
   */
  public String getSolveFile() {
    return solveFile;
  }

  /**
   * @return String
   */
  public String getTempDir() {
    return tempDir;
  }

  /**
   * @return String
   */
  public String getWarpFile() {
    return warpFile;
  }

  /**
   * @return String
   */
  public String getWarpLimit() {
    return warpLimit;
  }

  /**
   * @return int
   */
  public int getXLowerExclude() {
    return xLowerExclude;
  }

  /**
   * @return int
   */
  public int getXUpperExclude() {
    return xUpperExclude;
  }

  /**
   * @return int
   */
  public int getZLowerExclude() {
    return zLowerExclude;
  }

  /**
   * @return int
   */
  public int getZUpperExclude() {
    return zUpperExclude;
  }
  /**
   * @return String
   */
  public String getInputFile() {
    return inputFile;
  }

  /**
   * @return String
   */
  public String getOutputFile() {
    return outputFile;
  }

  /**
   * @return boolean
   */
  public boolean isTrial() {
    return trial;
  }
  /**
   * @return boolean
   */
  public boolean isUseRefinelimit() {
    return useRefinelimit;
  }

  /**
   * @return String
   */
  public String getRefineFile() {
    return refineFile;
  }
  
  public boolean isUseLinearInterpolation() {
    return useLinearInterpolation;
  }
  
  /**
   * Return the default patch region model file name
   * @return String
   */
  public static String getDefaultPatchRegionModel() {
    return "patch_region.mod";
  }

}

