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
 * <p> $Revision 1.8  2005/01/26 04:26:37  sueh
 * <p> $bug# 83 Added getOutputFile().
 * <p> $
 * <p> $Revision 1.7  2004/06/14 23:25:55  rickg
 * <p> $Bug #383  ParamUtilities interface change.
 * <p> $$
 * <p> Revision 1.6  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p> 
 * <p> Revision 1.5  2004/04/16 01:48:50  sueh
 * <p> bug# 409 added startingAndEndingZ (Starting and Ending Views)
 * <p> 
 * <p> Revision 1.4  2004/04/12 17:13:46  sueh
 * <p> bug# 409  Change HighFrequencyRadiusSigma to LowPassRadiusSigma.  reset()
 * <p> should unset values, not set to comscript defaults.
 * <p> 
 * <p> Revision 1.3  2004/03/29 20:47:47  sueh
 * <p> bug# 409 user cannot change output file
 * <p> 
 * <p> Revision 1.2  2004/03/25 00:43:23  sueh
 * <p> bug# 409, bug# 418 remove default, add InverseRolloffRadiusSigma, use
 * <p> ParamUtilities
 * <p> 
 * <p> Revision 1.1  2004/03/24 18:15:18  sueh
 * <p> bug# 409 MTF Filter const params
 * </p>
 */
package etomo.comscript;

public class ConstMTFFilterParam {
  public static final String rcsid = "$$Id$$";
  
  String inputFile;
  String outputFile;
  String mtfFile;
  double maximumInverse;
  FortranInputString lowPassRadiusSigma;
  FortranInputString inverseRolloffRadiusSigma;
  FortranInputString startingAndEndingZ;
  
  public ConstMTFFilterParam() {
    lowPassRadiusSigma = new FortranInputString(2);
    inverseRolloffRadiusSigma = new FortranInputString(2);
    startingAndEndingZ = new FortranInputString(2);
    startingAndEndingZ.setIntegerType(0, true);
    startingAndEndingZ.setIntegerType(1, true);
    reset();
  }
  
  protected void reset() {
    inputFile = new String();
    outputFile = new String();
    mtfFile = new String();
    maximumInverse = Double.NaN;
    lowPassRadiusSigma.setDefault();
    inverseRolloffRadiusSigma.setDefault();
    startingAndEndingZ.setDefault();
  }
  
  public String getMtfFile() {
    return mtfFile;
  }
  public String getMaximumInverseString() {
    return ParamUtilities.valueOf(maximumInverse);
  }
  public String getLowPassRadiusSigmaString() {
    return lowPassRadiusSigma.toString(true);
  }
  public String getStartingAndEndingZString() {
    return startingAndEndingZ.toString(true);
  }
  public boolean isStartingZSet() {
    return !startingAndEndingZ.isDefault(0) && !startingAndEndingZ.isEmpty(0);
  }
  public boolean isEndingZSet() {
    return !startingAndEndingZ.isDefault(1) && !startingAndEndingZ.isEmpty(1);
  }
  public int getStartingZ() {
    return startingAndEndingZ.getInt(0);
  }
  public int getEndingZ() {
    return startingAndEndingZ.getInt(1);
  }
  public String getInverseRolloffRadiusSigmaString() {
    return inverseRolloffRadiusSigma.toString(true);
  }
  public String getOutputFile() {
    return outputFile;
  }
}
