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

public class ConstSolvematchshiftParam {
  public static final String rcsid = "$Id$";

  protected String tofiducialCoordinatesFile;
  protected String fromfiducialCoordinatesFile;
  protected StringList fiducialMatchListA = new StringList(0);
  protected StringList fiducialMatchListB = new StringList(0);
  protected FortranInputString xAxistTilt;
  protected double residualThreshold;
  protected int nSurfaces;
  protected String outputTransformationFile;
  
  public ConstSolvematchshiftParam() {
  }
  /**
   * @return StringList
   */
  public StringList getFiducialMatchListA() {
    return fiducialMatchListA;
  }

  /**
   * @return StringList
   */
  public StringList getFiducialMatchListB() {
    return fiducialMatchListB;
  }

  /**
   * @return String
   */
  public String getFromfiducialCoordinatesFile() {
    return fromfiducialCoordinatesFile;
  }

  /**
   * @return int
   */
  public int getNSurfaces() {
    return nSurfaces;
  }

  /**
   * @return String
   */
  public String getOutputTransformationFile() {
    return outputTransformationFile;
  }

  /**
   * @return double
   */
  public double getResidualThreshold() {
    return residualThreshold;
  }

  /**
   * @return String
   */
  public String getTofiducialCoordinatesFile() {
    return tofiducialCoordinatesFile;
  }

  /**
   * @return FortranInputString
   */
  public FortranInputString getXAxistTilt() {
    return xAxistTilt;
  }

  /**
   * Sets the fiducialMatchListA.
   * @param fiducialMatchListA The fiducialMatchListA to set
   */
  public void setFiducialMatchListA(StringList fiducialMatchListA) {
    this.fiducialMatchListA = fiducialMatchListA;
  }

  /**
   * Sets the fiducialMatchListB.
   * @param fiducialMatchListB The fiducialMatchListB to set
   */
  public void setFiducialMatchListB(StringList fiducialMatchListB) {
    this.fiducialMatchListB = fiducialMatchListB;
  }

  /**
   * Sets the fromfiducialCoordinatesFile.
   * @param fromfiducialCoordinatesFile The fromfiducialCoordinatesFile to set
   */
  public void setFromfiducialCoordinatesFile(String fromfiducialCoordinatesFile) {
    this.fromfiducialCoordinatesFile = fromfiducialCoordinatesFile;
  }

  /**
   * Sets the nSurfaces.
   * @param nSurfaces The nSurfaces to set
   */
  public void setNSurfaces(int nSurfaces) {
    this.nSurfaces = nSurfaces;
  }

  /**
   * Sets the outputTransformationFile.
   * @param outputTransformationFile The outputTransformationFile to set
   */
  public void setOutputTransformationFile(String outputTransformationFile) {
    this.outputTransformationFile = outputTransformationFile;
  }

  /**
   * Sets the residualThreshold.
   * @param residualThreshold The residualThreshold to set
   */
  public void setResidualThreshold(double residualThreshold) {
    this.residualThreshold = residualThreshold;
  }

  /**
   * Sets the tofiducialCoordinatesFile.
   * @param tofiducialCoordinatesFile The tofiducialCoordinatesFile to set
   */
  public void setTofiducialCoordinatesFile(String tofiducialCoordinatesFile) {
    this.tofiducialCoordinatesFile = tofiducialCoordinatesFile;
  }

  /**
   * Sets the xAxistTilt.
   * @param xAxistTilt The xAxistTilt to set
   */
  public void setXAxistTilt(FortranInputString xAxistTilt) {
    this.xAxistTilt = xAxistTilt;
  }

}
