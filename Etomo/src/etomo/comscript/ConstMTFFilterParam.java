package etomo.comscript;

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
 * <p> $$Log$$ </p>
 */
public class ConstMTFFilterParam {
  public static final String rcsid = "$$Id$$";
  
  String inputFile;
  String outputFile;
  String mtfFile;
  double maximumInverse;
  double defaultMaximumInverse = 4.0;
  FortranInputString highFrequencyRadiusSigma;
  
  public ConstMTFFilterParam() {
    highFrequencyRadiusSigma = new FortranInputString(2);
    reset();
  }
  
  protected void reset() {
    inputFile = new String();
    outputFile = new String("datasetname_filt.ali");
    mtfFile = new String();
    maximumInverse = defaultMaximumInverse;
    highFrequencyRadiusSigma.setDefault();
  }
  
  public String getOutputFile() {
    return outputFile;
  }
  public String getMtfFile() {
    return mtfFile;
  }
  public String getMaximumInverseString() {
    if (Double.isNaN(maximumInverse)) {
      return new String();
    }
    return String.valueOf(maximumInverse);
  }
  public String getHighFrequencyRadiusSigmaString() {
    return highFrequencyRadiusSigma.toString(true);
  }
}
