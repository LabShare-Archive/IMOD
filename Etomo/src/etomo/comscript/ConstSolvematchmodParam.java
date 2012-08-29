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
 * <p> Revision 3.2  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.1  2004/05/14 00:44:32  sueh
 * <p> bug# 434 added a match b to a state variable
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.3  2003/03/18 00:32:33  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.2  2003/03/17 06:45:16  rickg
 * <p> in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */
public class ConstSolvematchmodParam {
  public static final String rcsid = "$Id$";

  protected boolean matchBToA = true;

  protected String toFiducialCoordinatesFile;
  protected String fromFiducialCoordinatesFile;
  protected StringList fiducialMatchListA = new StringList(0);
  protected StringList fiducialMatchListB = new StringList(0);
  protected FortranInputString xAxistTilt = new FortranInputString(2);
  protected double residualThreshold;
  protected int nSurfaces;
  protected String toReconstructionFile;
  protected String toMatchingModel;
  protected String fromReconstructionFile;
  protected String fromMatchingModel;
  protected String outputTransformationFile;

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
  public String getFromFiducialCoordinatesFile() {
    return fromFiducialCoordinatesFile;
  }

  /**
   * @return String
   */
  public String getFromMatchingModel() {
    return fromMatchingModel;
  }

  /**
   * @return String
   */
  public String getFromReconstructionFile() {
    return fromReconstructionFile;
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
  public String getToFiducialCoordinatesFile() {
    return toFiducialCoordinatesFile;
  }

  /**
   * @return String
   */
  public String getToMatchingModel() {
    return toMatchingModel;
  }

  /**
   * @return String
   */
  public String getToReconstructionFile() {
    return toReconstructionFile;
  }

  /**
   * @return FortranInputString
   */
  public FortranInputString getXAxistTilt() {
    return xAxistTilt;
  }

}
