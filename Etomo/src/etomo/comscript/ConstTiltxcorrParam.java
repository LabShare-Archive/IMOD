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
 * <p> Revision 3.3  2004/01/30 02:11:38  sueh
 * <p> bug# 373 formatting
 * <p>
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
  protected String inputFile;
  protected String pieceListFile;
  protected String outputFile;
  protected boolean excludeCentralPeak;
  protected double rotationAngle; //was imageRotation
  protected FortranInputString bordersInXandY; //was trim
  protected FortranInputString xMinAndMax;
  protected FortranInputString yMinAndMax;
  protected FortranInputString padsInXandY; // was padPercent;
  protected FortranInputString tapersInXandY; //was taperPercent
  protected boolean cumulativeCorrelation;
  protected boolean absoluteCosineStretch;
  protected boolean noCosineStretch;
  protected String testOutput;
  protected FortranInputString startingEndingViews; //was viewRange

  //PIP only
  //was tiltAngleSpec
  protected double firstTiltAngle;
  protected double tiltIncrement;
  protected String tiltFile;
  protected double[] tiltAngles;

  //was filterParams
  protected double filterRadius1;
  protected double filterRadius2;
  protected double filterSigma1;
  protected double filterSigma2;

  //sequential input only
  protected TiltAngleSpec tiltAngleSpec;
  protected FortranInputString filterParams;

  public ConstTiltxcorrParam() {
    tiltAngleSpec = new TiltAngleSpec();
    filterParams = new FortranInputString(4);
    bordersInXandY = new FortranInputString(2);
    bordersInXandY.setIntegerType(0, true);
    bordersInXandY.setIntegerType(1, true);
    xMinAndMax = new FortranInputString(2);
    xMinAndMax.setIntegerType(0, true);
    xMinAndMax.setIntegerType(1, true);
    yMinAndMax = new FortranInputString(2);
    yMinAndMax.setIntegerType(0, true);
    yMinAndMax.setIntegerType(1, true);
    padsInXandY = new FortranInputString(2);
    padsInXandY.setIntegerType(0, true);
    padsInXandY.setIntegerType(1, true);
    tapersInXandY = new FortranInputString(2);
    tapersInXandY.setIntegerType(0, true);
    tapersInXandY.setIntegerType(1, true);
    startingEndingViews = new FortranInputString(2);
    startingEndingViews.setIntegerType(0, true);
    startingEndingViews.setIntegerType(1, true);
    reset();
  }
  
  protected void reset() {
    inputFile = new String("");
    pieceListFile = new String("");
    outputFile = new String("");
    excludeCentralPeak = false;
    rotationAngle = Double.NaN;
    bordersInXandY.set(0, Double.NaN);
    bordersInXandY.set(1, Double.NaN);
    xMinAndMax.set(0, Double.NaN);
    xMinAndMax.set(1, Double.NaN);
    yMinAndMax.set(0, Double.NaN);
    yMinAndMax.set(1, Double.NaN);
    padsInXandY.set(0, Double.NaN);
    padsInXandY.set(1, Double.NaN);
    tapersInXandY.set(0, Double.NaN);
    tapersInXandY.set(1, Double.NaN);
    cumulativeCorrelation = false;
    absoluteCosineStretch = false;
    noCosineStretch = false;
    testOutput = new String("");
    startingEndingViews.set(0, Double.NaN);
    startingEndingViews.set(1, Double.NaN);
    firstTiltAngle = Double.NaN;
    tiltIncrement = Double.NaN;
    tiltFile = new String("");
    tiltAngles = null;
    filterRadius1 = Double.NaN;
    filterRadius2 = Double.NaN;
    filterSigma1 = Double.NaN;
    filterSigma2 = Double.NaN;
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
  public String getXMinString() {
    return xMinAndMax.toString(0);
  }
  public String getXMaxString() {
    return xMinAndMax.toString(1);
  }
  public String getYMinString() {
    return yMinAndMax.toString(0);
  }
  public String getYMaxString() {
    return yMinAndMax.toString(1);
  }
  public String getPadsInXandY() {
    return padsInXandY.toString();
  }
  public String getTaperPercent() {
    return tapersInXandY.toString();
  }
  public boolean isCumulativeCorrelation() {
    return cumulativeCorrelation;
  }
  public boolean isAbsoluteCosineStretch() {
    return absoluteCosineStretch;
  }
  public boolean isNoCosineStretch() {
    return noCosineStretch;
  }
  public String getStartingEndingViews() {
    return startingEndingViews.toString();
  }
  public boolean getExcludeCentralPeak() {
    return excludeCentralPeak;
  }
  public String getTestOutput() {
    return testOutput;
  }
}
