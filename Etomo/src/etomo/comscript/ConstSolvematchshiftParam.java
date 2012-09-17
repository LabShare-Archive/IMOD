package etomo.comscript;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002-2004</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.4  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.3  2004/06/14 23:28:33  rickg
 * <p> Bug #383  reisdual changed to float.
 * <p>
 * <p> Revision 3.2  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.1  2004/05/14 00:45:02  sueh
 * <p> bug# 434 added a match B to A state variable
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/03/18 00:32:33  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.4  2003/03/17 06:46:30  rickg
 * <p> in progress
 * <p>
 * <p> Revision 2.3  2003/03/07 07:22:50  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.2  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * </p>
 */

public class ConstSolvematchshiftParam {
  public static final String rcsid = "$Id$";

  protected boolean matchBToA = true;
  protected String toFiducialCoordinatesFile;
  protected String fromFiducialCoordinatesFile;
  protected StringList fiducialMatchListA = new StringList(0);
  protected StringList fiducialMatchListB = new StringList(0);
  protected FortranInputString xAxistTilt = new FortranInputString(2);
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
  public String getFromFiducialCoordinatesFile() {
    return fromFiducialCoordinatesFile;
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
   * @return FortranInputString
   */
  public FortranInputString getXAxistTilt() {
    return xAxistTilt;
  }

}
