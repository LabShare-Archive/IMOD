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
 * <p> $Log$ </p>
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

}

