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
  protected FortranInputString viewRange;

  public ConstTiltxcorrParam() {
    tiltAngleSpec = new TiltAngleSpec();
    filterParams = new FortranInputString(4);
    trim = new FortranInputString(2);
    trim.setIntegerType(0, true);
    trim.setIntegerType(1, true);
    padPercent = new FortranInputString(2);
    padPercent.setIntegerType(0, true);
    padPercent.setIntegerType(1, true);
    taperPercent = new FortranInputString(2);
    taperPercent.setIntegerType(0, true);
    taperPercent.setIntegerType(1, true);
    viewRange = new FortranInputString(2);
    viewRange.setIntegerType(0, true);
    viewRange.setIntegerType(1, true);
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

  public TiltAngleSpec getTiltAngleSpec() {
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
  public String getViewRange() {
    return viewRange.toString();
  }
  public boolean getExcludeCentralPeak() {
    return excludeCentralPeak;
  }

}
