/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright(c) 2002, 2003, 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.2  2004/03/25 00:43:23  sueh
 * <p> $bug# 409, bug# 418 remove default, add InverseRolloffRadiusSigma, use
 * <p> $ParamUtilities
 * <p> $
 * <p> $Revision 1.1  2004/03/24 18:15:18  sueh
 * <p> $bug# 409 MTF Filter const params
 * <p> $$ </p>
 */

package etomo.comscript;

public class ConstMTFFilterParam {
  public static final String rcsid = "$$Id$$";
  
  String inputFile;
  String outputFile;
  String mtfFile;
  double maximumInverse;
  FortranInputString highFrequencyRadiusSigma;
  FortranInputString inverseRolloffRadiusSigma;
  
  public ConstMTFFilterParam() {
    highFrequencyRadiusSigma = new FortranInputString(2);
    inverseRolloffRadiusSigma = new FortranInputString(2);
    reset();
  }
  
  protected void reset() {
    inputFile = new String();
    outputFile = new String();
    mtfFile = new String();
    maximumInverse = 4.0;
    highFrequencyRadiusSigma.setDefault();
    inverseRolloffRadiusSigma.setDefault();
  }
  
  public String getMtfFile() {
    return mtfFile;
  }
  public String getMaximumInverseString() {
    return ParamUtilities.getString(maximumInverse);
  }
  public String getHighFrequencyRadiusSigmaString() {
    return highFrequencyRadiusSigma.toString(true);
  }
  public String getInverseRolloffRadiusSigmaString() {
    return inverseRolloffRadiusSigma.toString(true);
  }
}
