package etomo.comscript;

import etomo.type.ProcessName;
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
 * <p> Revision 3.8  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.7  2004/05/03 18:00:51  sueh
 * <p> bug# 418 standardizing get functions
 * <p> And param testing proof of concept
 * <p>
 * <p> Revision 3.6  2004/03/13 02:24:41  sueh
 * <p> bug# 373 getting an empty string for a default FortranInputString
 * <p>
 * <p> Revision 3.5  2004/03/12 20:57:55  sueh
 * <p> bug# 373 Changed reset().
 * <p>
 * <p> Revision 3.4  2004/03/12 19:59:48  sueh
 * <p> bug# 412 added absoluteCosineStretch, cumulativeCorreslation, noCosineStretch,
 * <p> testOutput, xMinAndMax, yMinAndMax, reset()
 * <p>
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

public class ConstTiltxcorrParam implements ConstCommandParam {
  public static final String rcsid =
    "$Id$";

  public static final String GOTO_LABEL = "doxcorr";
  
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
  private ProcessName processName = ProcessName.XCORR;
  private String command = "tiltxcorr";

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
    inputFile = new String();
    pieceListFile = new String();
    outputFile = new String();
    excludeCentralPeak = false;
    rotationAngle = Double.NaN;
    bordersInXandY.setDefault();
    xMinAndMax.setDefault();
    yMinAndMax.setDefault();
    padsInXandY.setDefault();
    tapersInXandY.setDefault();
    cumulativeCorrelation = false;
    absoluteCosineStretch = false;
    noCosineStretch = false;
    testOutput = new String();
    startingEndingViews.setDefault();
    firstTiltAngle = Double.NaN;
    tiltIncrement = Double.NaN;
    tiltFile = new String();
    tiltAngles = null;
    filterRadius1 = Double.NaN;
    filterRadius2 = Double.NaN;
    filterSigma1 = Double.NaN;
    filterSigma2 = Double.NaN;
    TiltAngleSpec tiltAngleSpec = new TiltAngleSpec();
    filterParams.setDefault();
  }
  
  public boolean isParseComments() {
    return true;
  }
  
  public String getProcessNameString() {
    return processName.toString();
  }
  
  public String getCommand() {
    return command;
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
  public String getFirstTiltAngleString() {
    return ParamUtilities.valueOf(firstTiltAngle);
  };

  public String getTiltIncrementString() {
    return ParamUtilities.valueOf(tiltIncrement);
  };
  public String getTiltFile() {
    return tiltFile;
  };
  public String[] getTiltAnglesString() {
    return ParamUtilities.valueOf(tiltAngles);
  };
  public String getRotationAngleString() {
    return ParamUtilities.valueOf(rotationAngle);
  }
  public String getFilterRadius1String() {
    return ParamUtilities.valueOf(filterRadius1);
  }
  public String getFilterRadius2String() {
    return ParamUtilities.valueOf(filterRadius2);
  }
  public String getFilterSigma1String() {
    return ParamUtilities.valueOf(filterSigma1);
  }
  public String getFilterSigma2String() {
    return ParamUtilities.valueOf(filterSigma2);
  }
  public String getBordersInXandY() {
    return bordersInXandY.toString(true);
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
  public String getPadsInXandYString() {
    return padsInXandY.toString(true);
  }
  public String getTaperPercentString() {
    return tapersInXandY.toString(true);
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
    return startingEndingViews.toString(true);
  }
  public boolean getExcludeCentralPeak() {
    return excludeCentralPeak;
  }
  public String getTestOutput() {
    return testOutput;
  }
}
