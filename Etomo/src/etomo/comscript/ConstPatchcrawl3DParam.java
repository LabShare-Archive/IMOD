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
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */

public class ConstPatchcrawl3DParam {
  public static final String rcsid = "$Id$";

  protected int xPatchSize;
  protected int yPatchSize;
  protected int zPatchSize;
  protected int nX;
  protected int nY;
  protected int nZ;
  protected int xLow;
  protected int xHigh;
  protected int yLow;
  protected int yHigh;
  protected int zLow;
  protected int zHigh;
  protected int maxShift;
  protected String fileA = "";
  protected String fileB = "";
  protected String outputFile = "";
  protected String transformFile = "";
  protected String originalFileB = "";
  protected String boundaryModel = "";
  
  public ConstPatchcrawl3DParam() {
  }

  /**
   * @return String
   */
  public String getFileA() {
    return fileA;
  }

  /**
   * @return String
   */
  public String getFileB() {
    return fileB;
  }

  /**
   * @return int
   */
  public int getNX() {
    return nX;
  }

  /**
   * @return int
   */
  public int getNY() {
    return nY;
  }

  /**
   * @return int
   */
  public int getNZ() {
    return nZ;
  }

  /**
   * @return String
   */
  public String getOriginalFileB() {
    return originalFileB;
  }

  /**
   * @return String
   */
  public String getTransformFile() {
    return transformFile;
  }

  /**
   * @return int
   */
  public int getXHigh() {
    return xHigh;
  }

  /**
   * @return int
   */
  public int getXLow() {
    return xLow;
  }

  /**
   * @return int
   */
  public int getXPatchSize() {
    return xPatchSize;
  }

  /**
   * @return int
   */
  public int getYHigh() {
    return yHigh;
  }

  /**
   * @return int
   */
  public int getYLow() {
    return yLow;
  }

  /**
   * @return int
   */
  public int getYPatchSize() {
    return yPatchSize;
  }

  /**
   * @return int
   */
  public int getZHigh() {
    return zHigh;
  }

  /**
   * @return int
   */
  public int getZLow() {
    return zLow;
  }

  /**
   * @return int
   */
  public int getZPatchSize() {
    return zPatchSize;
  }

  /**
   * @return int
   */
  public int getMaxShift() {
    return maxShift;
  }


  /**
   * @return String
   */
  public String getBoundaryModel() {
    return boundaryModel;
  }

  /**
   * @return String
   */
  public String getOutputFile() {
    return outputFile;
  }

}
