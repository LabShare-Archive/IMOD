/**
 * <p>Description: A constant model of the solvematch com script.  The solvematch
 * com script supercedes the Solvematchshift and Solvematchmod com scripts.</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.2  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.1  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p> </p>
 */
package etomo.comscript;

import etomo.type.FiducialMatch;

// Implementation note: this is not derived from either ConstSolvematchmodParam
// ConstSolvematchshiftParam because that old functionality should be able to be
// absorbed by this class
public class ConstSolvematchParam {
  public static final String OUTPUT_FILE = "OutputFile";
  // The A and B in the keywords are misleading and only correct if we are
  // matching from A to B.  Their association swaps if the matching direction is
  // from B to A.  That is why static strings are called TO and FROM, they
  // should make the code in this class easier to read.
  public static final String TO_FIDUCIAL_FILE = "AFiducialFile";
  public static final String FROM_FIDUCIAL_FILE = "BFiducialFile";
  public static final String TO_CORRESPONDENCE_LIST = "ACorrespondenceList";
  public static final String FROM_CORRESPONDENCE_LIST = "BCorrespondenceList";
  public static final String SCALE_FACTORS = "ScaleFactors";
  public static final String XAXIS_TILTS = "XAxisTilts";
  public static final String MAXIMUM_RESIDUAL = "MaximumResidual";
  public static final String TO_TOMOGRAM_OR_SIZE_XYZ = "ATomogramOrSizeXYZ";
  public static final String FROM_TOMOGRAM_OR_SIZE_XYZ = "BTomogramOrSizeXYZ";
  public static final String SURFACE_OR_USE_MODELS = "SurfacesOrUseModels";
  public static final String TO_MATCHING_MODEL = "AMatchingModel";
  public static final String FROM_MATCHING_MODEL = "BMatchingModel";
  
  public static final int USE_MODEL_ONLY_OPTION = -2;
  public static final int ONE_SIDE_INVERTED_OPTION = -1;
  public static final int USE_MODEL_OPTION = 0;
  public static final int ONE_SIDE_OPTION = 1;
  public static final int BOTH_SIDES_OPTION = 2;

  protected boolean matchBToA = true;

  protected String outputFile = "";
  protected String toFiducialFile = "";
  protected String fromFiducialFile = "";
  protected StringList toCorrespondenceList = new StringList(0);
  protected StringList fromCorrespondenceList = new StringList(0);
  protected FortranInputString xAxistTilt = new FortranInputString(2);
  protected int surfacesOrModel = Integer.MIN_VALUE;
  protected float maximumResidual = Float.NaN;
  protected String toMatchingModel = "";
  protected String fromMatchingModel = "";
  protected String toTomogramOrSizeXYZ = "";
  protected String fromTomogramOrSizeXYZ = "";
  protected FortranInputString scaleFactors = new FortranInputString(2);

  /**
   * @return FortranInputString
   */
  public FortranInputString getXAxistTilt() {
    return xAxistTilt;
  }

  /**
   * @return FortranInputString
   */
  public FortranInputString getScaleFactors() {
    return scaleFactors;
  }

  /**
   * @return Returns the fromCorrespondenceList.
   */
  public StringList getFromCorrespondenceList() {
    return fromCorrespondenceList;
  }

  /**
   * @return Returns the fromFiducialFile.
   */
  public String getFromFiducialFile() {
    return fromFiducialFile;
  }

  /**
   * @return Returns the fromMatchingModel.
   */
  public String getFromMatchingModel() {
    return fromMatchingModel;
  }

  /**
   * @return Returns the fromTomogramOrSizeXYZ.
   */
  public String getFromTomogramOrSizeXYZ() {
    return fromTomogramOrSizeXYZ;
  }

  /**
   * @return Returns the matchBToA.
   */
  public boolean isMatchBToA() {
    return matchBToA;
  }

  /**
   * @return Returns the maximumResidual.
   */
  public float getMaximumResidual() {
    return maximumResidual;
  }

  /**
   * @return Returns the surfaces or model code.
   */
  public FiducialMatch getSurfacesOrModel() {
    switch (surfacesOrModel) {
      case USE_MODEL_ONLY_OPTION :
        return FiducialMatch.USE_MODEL_ONLY;
      case ONE_SIDE_INVERTED_OPTION :
        return FiducialMatch.ONE_SIDE_INVERTED;
      case USE_MODEL_OPTION :
        return FiducialMatch.USE_MODEL;
      case ONE_SIDE_OPTION :
        return FiducialMatch.ONE_SIDE;
      case BOTH_SIDES_OPTION :
        return FiducialMatch.BOTH_SIDES;
      default :
        return FiducialMatch.NOT_SET;
    }
  }

  /**
   * @return Returns the outputTransformationFile.
   */
  public String getOutputFile() {
    return outputFile;
  }

  /**
   * @return Returns the toCorrespondenceList.
   */
  public StringList getToCorrespondenceList() {
    return toCorrespondenceList;
  }

  /**
   * @return Returns the toFiducialFile.
   */
  public String getToFiducialFile() {
    return toFiducialFile;
  }

  /**
   * @return Returns the toMatchingModel.
   */
  public String getToMatchingModel() {
    return toMatchingModel;
  }

  /**
   * @return Returns the toTomogramOrSizeXYZ.
   */
  public String getToTomogramOrSizeXYZ() {
    return toTomogramOrSizeXYZ;
  }
}