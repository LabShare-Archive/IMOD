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
 * <p> $Log$ </p>
 */

public class ConstTiltxcorrParam {
  public static final String rcsid = "$Id$";

  protected String inputFile;
  protected String pieceListFile;
  protected String outputFile;
  protected TiltAngleSpec tiltAngleSpec;
  protected double imageRotation;
  protected FortranInputString filterParams;
  protected boolean excludeCentralPeak;
  protected FortranInputString trim;
  protected FortranInputString padPercent;
  protected FortranInputString taperPercent;
  protected String viewList;

  public ConstTiltxcorrParam() {
    tiltAngleSpec = new TiltAngleSpec();
    filterParams = new FortranInputString(4);
    trim = new FortranInputString(2);
    padPercent = new FortranInputString(2);
    taperPercent = new FortranInputString(2);
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

  public TiltAngleSpec getTiltAngleSpec(){
    return new TiltAngleSpec(tiltAngleSpec);
  };

  public double getImageRotation() {
    return imageRotation;
  }

  public String getFilterParams() {
    return filterParams.toString();
  }
  public String getTrim() {
    return trim.toString();
  }
  public String getPadPercent() {
    return padPercent.toString();
  }
  public String getTaperPercent() {
    return taperPercent.toString();
  }
  public String getViewList() {
    return viewList;
  }
  public boolean getExcludeCentralPeak() {
    return excludeCentralPeak;
  }

}
