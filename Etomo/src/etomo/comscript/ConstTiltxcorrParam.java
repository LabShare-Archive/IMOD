package etomo.comscript;

import etomo.type.TiltAngleSpec;

/**
 * <p>Description: A read only model of the parameter interface for the
 *  tiltxcorr program</p>
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
 * <p> Revision 3.2  2004/01/30 02:08:59  sueh
 * <p> bug# 373 initialized values
 * <p>
 * <p> Revision 3.1  2004/01/30 01:27:09  sueh
 * <p> bug# 373 Changed fields and functions to match autodoc
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/09/09 17:18:47  rickg
 * <p> Changed view list to view range and made it a 2 element integer
 * <p> FortranInputString
 * <p> padPercent, taperPercent, and trim are now integer FortranInputString
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

public class ConstTiltxcorrParam {
  public static final String rcsid =
    "$Id$";

  //PIP and sequential input
  protected String inputFile = "";
  protected String pieceListFile = "";
  protected String outputFile = "";
  protected boolean excludeCentralPeak;
  protected double rotationAngle; //was imageRotation
  protected FortranInputString bordersInXandY; //was trim
  protected FortranInputString padsInXandY; // was padPercent;
  protected FortranInputString tapersInXandY; //was taperPercent
  protected FortranInputString startingEndingViews; //was viewRange

  //PIP only
  //was tiltAngleSpec
  protected double firstTiltAngle = Double.NaN;
  protected double tiltIncrement = Double.NaN;
  protected String tiltFile = "";
  protected double[] tiltAngles = null;

  //was filterParams
  protected double filterRadius1 = Double.NaN;
  protected double filterRadius2 = Double.NaN;
  protected double filterSigma1 = Double.NaN;
  protected double filterSigma2 = Double.NaN;

  //sequential input only
  protected TiltAngleSpec tiltAngleSpec;
  protected FortranInputString filterParams;

  public ConstTiltxcorrParam() {
    tiltAngleSpec = new TiltAngleSpec();
    filterParams = new FortranInputString(4);
    bordersInXandY = new FortranInputString(2);
    bordersInXandY.setIntegerType(0, true);
    bordersInXandY.setIntegerType(1, true);
    padsInXandY = new FortranInputString(2);
    padsInXandY.setIntegerType(0, true);
    padsInXandY.setIntegerType(1, true);
    tapersInXandY = new FortranInputString(2);
    tapersInXandY.setIntegerType(0, true);
    tapersInXandY.setIntegerType(1, true);
    startingEndingViews = new FortranInputString(2);
    startingEndingViews.setIntegerType(0, true);
    startingEndingViews.setIntegerType(1, true);
  }

  public String getInputFile() {
    return inputFile;
  }
  public String getPieceListFile() {
    return pieceListFile;
  }
  public String getOutputFile() {
    return outputFile;
  }
  public double getFirstTiltAngle() {
    return firstTiltAngle;
  };

  public double getTiltIncrement() {
    return tiltIncrement;
  };
  public String getTiltFile() {
    return tiltFile;
  };
  public double[] getTiltAngles() {
    return tiltAngles;
  };
  public double getRotationAngle() {
    return rotationAngle;
  }
  public double getFilterRadius1() {
    return filterRadius1;
  }
  public double getFilterRadius2() {
    return filterRadius2;
  }
  public double getFilterSigma1() {
    return filterSigma1;
  }
  public double getFilterSigma2() {
    return filterSigma2;
  }
  public String getBordersInXandY() {
    return bordersInXandY.toString();
  }
  public String getPadsInXandY() {
    return padsInXandY.toString();
  }
  public String getTaperPercent() {
    return tapersInXandY.toString();
  }
  public String getStartingEndingViews() {
    return startingEndingViews.toString();
  }
  public boolean getExcludeCentralPeak() {
    return excludeCentralPeak;
  }

}
