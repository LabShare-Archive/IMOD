package etomo.comscript;

/**
 * <p>Description: A read only model of the parameter interface for the
 *  ccderaser program</p>
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
 * <p> Revision 3.1  2004/06/25 00:32:00  sueh
 * <p> bug# 467 OuterRadius is out-of-date.
 * <p> New parameter annulusWidth
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.2  2003/07/11 23:15:53  rickg
 * <p> new ccderaser mode
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
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

public class ConstCCDEraserParam {
  public static final String rcsid =
    "$Id$";

  public static final String ANNULUS_WIDTH_KEY = "AnnulusWidth";
  public static final String INPUT_FILE_KEY = "InputFile";
  public static final String OUTPUT_FILE_KEY = "OutputFile";
  public static final String FIND_PEAKS_KEY = "FindPeaks";
  public static final String PEAK_CRITERION_KEY = "PeakCriterion";
  public static final String DIFF_CRITERION_KEY = "DiffCriterion";
  public static final String GROW_CRITERION_KEY = "GrowCriterion";
  public static final String SCAN_CRITERION_KEY = "ScanCriterion";
  public static final String MAXIMUM_RADIUS_KEY = "MaximumRadius";
  public static final String X_Y_SCAN_SIZE_KEY = "XYScanSize";
  public static final String EDGE_EXCLUSION_WIDTH_KEY = "EdgeExclusionWidth";
  public static final String LINE_OBJECTS_KEY = "LineObjects";
  public static final String ALL_SECTION_OBJECTS_KEY = "AllSectionObjects";
  public static final String BORDER_SIZE_KEY = "BorderSize";
  public static final String POLYNOMIAL_ORDER_KEY = "PolynomialOrder";
  public static final String TRIAL_MODE_KEY = "TrialMode";
  
  protected String inputFile = "";
  protected String outputFile = "";

  protected boolean findPeaks = false;
  protected String peakCriterion = "";
  protected String diffCriterion = "";
  protected String growCriterion = "";
  protected String scanCriterion = "";
  protected String edgeExclusion = "";
  protected String maximumRadius = "";
  protected String annulusWidth;
  protected String xyScanSize = "";
  protected String pointModel = "";
  protected boolean trialMode;
  
  protected String modelFile = "";
  protected String globalReplacementList = "";
  protected String localReplacementList = "";
  protected String borderPixels = "";
  protected String polynomialOrder = "";
  protected boolean includeAdjacentPoints = true;
      
  //out of date parameter
  String outerRadius = ""; //replaced by annulusWidth
  
  public boolean isValid() {
    boolean valid = true;

    // Check to see if any of the integer parameters do not parse as integers
    // or there range is in appropriate
    try {
      int intBorderPixels = Integer.parseInt(borderPixels);
      if (intBorderPixels < 0) {
        valid = false;
      }
    }
    catch (NumberFormatException e) {
      valid = false;
    }
    return valid;
  }

  public String getInputFile() {
    return inputFile;
  }
  public String getOutputFile() {
    return outputFile;
  }
  public String getModelFile() {
    return modelFile;
  }
  public String getGlobalReplacementList() {
    return globalReplacementList;
  }
  public String getlocalReplacementList() {
    return localReplacementList;
  }
  public String getBorderPixels() {
    return borderPixels;
  }
  public String getPolynomialOrder() {
    return polynomialOrder;
  }
  public boolean getIncludeAdjacentPoints() {
    return includeAdjacentPoints;
  }

  /**
   * @return
   */
  public String getDiffCriterion() {
    return diffCriterion;
  }

  /**
   * @return
   */
  public String getEdgeExclusion() {
    return edgeExclusion;
  }

  /**
   * @return
   */
  public boolean isFindPeaks() {
    return findPeaks;
  }

  /**
   * @return
   */
  public String getGrowCriterion() {
    return growCriterion;
  }

  /**
   * @return
   */
  public String getMaximumRadius() {
    return maximumRadius;
  }

  /**
   * @return
   */
  public String getAnnulusWidth() {
    return annulusWidth;
  }

  /**
   * @return
   */
  public String getPeakCriterion() {
    return peakCriterion;
  }

  /**
   * @return
   */
  public String getPointModel() {
    return pointModel;
  }

  /**
   * @return
   */
  public boolean isTrialMode() {
    return trialMode;
  }

  /**
   * @return
   */
  public String getXyScanSize() {
    return xyScanSize;
  }

  /**
   * @return
   */
  public String getScanCriterion() {
    return scanCriterion;
  }


}
