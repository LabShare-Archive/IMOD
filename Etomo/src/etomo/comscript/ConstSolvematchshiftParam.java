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
 * <p> Revision 2.3  2003/03/07 07:22:50  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.2  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p> </p>
 */

public class ConstSolvematchshiftParam {
	public static final String rcsid =
		"$Id$";

	protected String tofiducialCoordinatesFile;
	protected String fromfiducialCoordinatesFile;
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

}
