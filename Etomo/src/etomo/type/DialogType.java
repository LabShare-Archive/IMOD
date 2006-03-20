package etomo.type;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.4  2005/09/29 18:46:03  sueh
 * <p> bug# 532 Simplified the storable names.
 * <p>
 * <p> Revision 1.3  2005/09/27 23:18:39  sueh
 * <p> bug# 532 Added getStorableName() to return standard names to use in the
 * <p> .edf file.
 * <p>
 * <p> Revision 1.2  2005/03/24 17:48:38  sueh
 * <p> bug# 621 Added Clean Up dialog.
 * <p>
 * <p> Revision 1.1  2005/01/14 03:06:31  sueh
 * <p> bug# 511 An enumerator class the identifies the dialogs in the
 * <p> tomogram reconstruction side of Etomo.  Contains a string to be used as
 * <p> button labels.  Also contains an index to be used to find dialog related
 * <p> data in arrays.  Stored in ProcessControlPanel and ProcessDialog as way to pass the
 * <p> identifier of the dialog to generic functions in ApplicationManager (see
 * <p> saveDialog, getDialog, getAdvanced).  Used to create and maintain the
 * <p> advanced arrays, which preserve the advanced settings for each dialog.
 * <p> </p>
 */
public final class DialogType {
  public static final String rcsid = "$Id$";

  private static final int setupIndex = 0;
  private static final int preProcessingIndex = 1;
  private static final int coarseAlignmentIndex = 2;
  private static final int fiducialModelIndex = 3;
  private static final int fineAlignmentIndex = 4;
  private static final int tomogramPositioningIndex = 5;
  private static final int tomogramGenerationIndex = 6;
  private static final int tomogramCombinationIndex = 7;
  private static final int postProcessingIndex = 8;
  private static final int cleanUpIndex = 9;

  public static final int TOTAL_RECON = cleanUpIndex + 1;

  private static final int parallelIndex = 0;

  public static final int TOTAL_PARALLEL = parallelIndex + 1;

  private final String name;
  private final int index;
  private final TabType tabType;

  private DialogType(TabType tabType, int index) {
    this.index = index;
    name = toString(tabType, index);
    this.tabType = tabType;
  }

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  public String getStorableName() {
    return getStorableName(tabType, index);
  }

  public int toIndex() {
    return index;
  }

  public static final DialogType SETUP = new DialogType(TabType.RECON, setupIndex);
  public static final DialogType PRE_PROCESSING = new DialogType(TabType.RECON, 
      preProcessingIndex);
  public static final DialogType COARSE_ALIGNMENT = new DialogType(TabType.RECON, 
      coarseAlignmentIndex);
  public static final DialogType FIDUCIAL_MODEL = new DialogType(TabType.RECON, 
      fiducialModelIndex);
  public static final DialogType FINE_ALIGNMENT = new DialogType(TabType.RECON, 
      fineAlignmentIndex);

  public static final DialogType TOMOGRAM_POSITIONING = new DialogType(TabType.RECON, 
      tomogramPositioningIndex);
  public static final DialogType TOMOGRAM_GENERATION = new DialogType(TabType.RECON, 
      tomogramGenerationIndex);
  public static final DialogType TOMOGRAM_COMBINATION = new DialogType(TabType.RECON, 
      tomogramCombinationIndex);
  public static final DialogType POST_PROCESSING = new DialogType(TabType.RECON, 
      postProcessingIndex);
  public static final DialogType CLEAN_UP = new DialogType(TabType.RECON, cleanUpIndex);
  
  public static final DialogType PARALLEL = new DialogType(TabType.PARALLEL, parallelIndex);

  private String toString(TabType tabType, int index) {
    if (tabType == TabType.RECON) {
      switch (index) {
      case setupIndex:
        return "Setup";
      case preProcessingIndex:
        return "Pre-processing";
      case coarseAlignmentIndex:
        return "Coarse Alignment";
      case fiducialModelIndex:
        return "Fiducial Model Gen.";
      case fineAlignmentIndex:
        return "Fine Alignment";
      case tomogramPositioningIndex:
        return "Tomogram Positioning";
      case tomogramGenerationIndex:
        return "Tomogram Generation";
      case tomogramCombinationIndex:
        return "Tomogram Combination";
      case postProcessingIndex:
        return "Post-processing";
      case cleanUpIndex:
        return "Clean Up";
      }
    }
    else if (tabType == TabType.PARALLEL) {
      switch (index) {
      case parallelIndex:
        return "Parallel";
      }
    }
    return "";
  }

  private String getStorableName(TabType tabType, int index) {
    if (tabType == TabType.RECON) {
      switch (index) {
      case setupIndex:
        return "Setup";
      case preProcessingIndex:
        return "PreProc";
      case coarseAlignmentIndex:
        return "CoarseAlign";
      case fiducialModelIndex:
        return "FidModel";
      case fineAlignmentIndex:
        return "FineAlign";
      case tomogramPositioningIndex:
        return "TomoPos";
      case tomogramGenerationIndex:
        return "TomoGen";
      case tomogramCombinationIndex:
        return "Combine";
      case postProcessingIndex:
        return "PostProc";
      case cleanUpIndex:
        return "CleanUp";
      }
    }
    else if (tabType == TabType.PARALLEL) {
      switch (index) {
      case parallelIndex:
        return "Parallel";
      }
    }
    return "";
  }
}