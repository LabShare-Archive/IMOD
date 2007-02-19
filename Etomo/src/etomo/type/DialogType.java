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
 * <p> Revision 1.9  2006/10/19 16:19:12  sueh
 * <p> bug# 438 Changed compact label for Fiducial Model to "Track".
 * <p>
 * <p> Revision 1.8  2006/07/31 21:42:13  sueh
 * <p> bug# 438 Added getCompactLabel
 * <p>
 * <p> Revision 1.7  2006/04/28 20:56:23  sueh
 * <p> bug# 787 Added equals(String)
 * <p>
 * <p> Revision 1.6  2006/04/25 18:56:17  sueh
 * <p> bug# 787 Added getInstance(String).
 * <p>
 * <p> Revision 1.5  2006/03/20 17:57:34  sueh
 * <p> bug# 835 Added the PARALLEL dialog, made DialogType work with
 * <p> different managers instead of just ApplicationManager.
 * <p>
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
  
  private static final int peetIndex = 0;

  public static final int TOTAL_PEET = peetIndex + 1;
  
  private static final String SETUP_RECON_NAME = "SetupRecon";
  private static final String PRE_PROCESSING_NAME = "PreProc";
  private static final String COARSE_ALIGNMENT_NAME = "CoarseAlign";
  private static final String FIDUCIAL_MODEL_NAME = "FidModel";
  private static final String FINE_ALIGNMENT_NAME = "FineAlign";
  private static final String TOMOGRAM_POSITIONING_NAME = "TomoPos";
  private static final String TOMOGRAM_GENERATION_NAME = "TomoGen";
  private static final String TOMOGRAM_COMBINATION_NAME = "Combine";
  private static final String POST_PROCESSING_NAME = "PostProc";
  private static final String CLEAN_UP_NAME = "CleanUp";
  private static final String PARALLEL_NAME = "Parallel";
  private static final String PEET_NAME = "Peet";

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
  
  public String getCompactLabel() {
    return getCompactLabel(tabType, index);
  }

  public int toIndex() {
    return index;
  }

  public static final DialogType SETUP_RECON = new DialogType(TabType.RECON, setupIndex);
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
  
  public static final DialogType PEET = new DialogType(TabType.PEET, peetIndex);

  private String toString(TabType tabType, int index) {
    if (tabType == TabType.RECON) {
      switch (index) {
      case setupIndex:
        return "Setup Tomogram";
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
    else if (tabType == TabType.PEET) {
      switch (index) {
      case peetIndex:
        return "PEET";
      }
    }
    return "";
  }
  
  /**
   * Return a name without spaces.  All storable names must be unique to
   * DialogType.
   * @param tabType
   * @param index
   * @return
   */
  private String getCompactLabel(TabType tabType, int index) {
    if (tabType == TabType.RECON) {
      switch (index) {
      case setupIndex:
        return "Setup";
      case preProcessingIndex:
        return "Pre";
      case coarseAlignmentIndex:
        return "Coarse";
      case fiducialModelIndex:
        return "Track";
      case fineAlignmentIndex:
        return "Fine";
      case tomogramPositioningIndex:
        return "Pos";
      case tomogramGenerationIndex:
        return "Gen";
      case tomogramCombinationIndex:
        return "Comb";
      case postProcessingIndex:
        return "Post";
      case cleanUpIndex:
        return "Clean";
      }
    }
    else if (tabType == TabType.PARALLEL) {
      switch (index) {
      case parallelIndex:
        return "Para";
      }
    }
    else if (tabType == TabType.PEET) {
      switch (index) {
      case peetIndex:
        return "PEET";
      }
    }
    return "";
  }

  /**
   * Return a name without spaces.  All storable names must be unique to
   * DialogType.
   * @param tabType
   * @param index
   * @return
   */
  private String getStorableName(TabType tabType, int index) {
    if (tabType == TabType.RECON) {
      switch (index) {
      case setupIndex:
        return SETUP_RECON_NAME;
      case preProcessingIndex:
        return PRE_PROCESSING_NAME;
      case coarseAlignmentIndex:
        return COARSE_ALIGNMENT_NAME;
      case fiducialModelIndex:
        return FIDUCIAL_MODEL_NAME;
      case fineAlignmentIndex:
        return FINE_ALIGNMENT_NAME;
      case tomogramPositioningIndex:
        return TOMOGRAM_POSITIONING_NAME;
      case tomogramGenerationIndex:
        return TOMOGRAM_GENERATION_NAME;
      case tomogramCombinationIndex:
        return TOMOGRAM_COMBINATION_NAME;
      case postProcessingIndex:
        return POST_PROCESSING_NAME;
      case cleanUpIndex:
        return CLEAN_UP_NAME;
      }
    }
    else if (tabType == TabType.PARALLEL) {
      switch (index) {
      case parallelIndex:
        return PARALLEL_NAME;
      }
    }
    else if (tabType == TabType.PEET) {
      switch (index) {
      case peetIndex:
        return PEET_NAME;
      }
    }
    return "";
  }
  
  public boolean equals(String storableName) {
    if (storableName == null) {
      return false;
    }
    return getStorableName(tabType, index).equals(storableName);
  }
  
  /**
   * 
   * @param storableName
   * @return
   */
  public static DialogType getInstance(String storableName) {
    if (storableName == null) {
      return null;
    }
    if (storableName.equals(SETUP_RECON_NAME)) {
      return SETUP_RECON;
    }
    if (storableName.equals(PRE_PROCESSING_NAME)) {
      return PRE_PROCESSING;
    }
    if (storableName.equals(COARSE_ALIGNMENT_NAME)) {
      return COARSE_ALIGNMENT;
    }
    if (storableName.equals(FIDUCIAL_MODEL_NAME)) {
      return FIDUCIAL_MODEL;
    }
    if (storableName.equals(FINE_ALIGNMENT_NAME)) {
      return FINE_ALIGNMENT;
    }
    if (storableName.equals(TOMOGRAM_POSITIONING_NAME)) {
      return TOMOGRAM_POSITIONING;
    }
    if (storableName.equals(TOMOGRAM_GENERATION_NAME)) {
      return TOMOGRAM_GENERATION;
    }
    if (storableName.equals(TOMOGRAM_COMBINATION_NAME)) {
      return TOMOGRAM_COMBINATION;
    }
    if (storableName.equals(POST_PROCESSING_NAME)) {
      return POST_PROCESSING;
    }
    if (storableName.equals(CLEAN_UP_NAME)) {
      return CLEAN_UP;
    }
    if (storableName.equals(PARALLEL_NAME)) {
      return PARALLEL;
    }
    if (storableName.equals(PEET_NAME)) {
      return PEET;
    }
    return null;
  }
}