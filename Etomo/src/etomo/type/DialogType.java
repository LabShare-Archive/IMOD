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
public class DialogType {
  public static  final String  rcsid =  "$Id$";
  
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
  
  public static final int TOTAL = cleanUpIndex + 1;
  
  private final String name;
  private final int index;

  private DialogType(int index) {
    this.index = index;
    name = toString(index);
  }

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }
  
  public int toIndex() {
    return index;
  }
  
  public static final DialogType SETUP = new DialogType(setupIndex);
  public static final DialogType PRE_PROCESSING = new DialogType(preProcessingIndex);
  public static final DialogType COARSE_ALIGNMENT = new DialogType(coarseAlignmentIndex);
  public static final DialogType FIDUCIAL_MODEL = new DialogType(fiducialModelIndex);
  public static final DialogType FINE_ALIGNMENT = new DialogType(fineAlignmentIndex);
  
  public static final DialogType TOMOGRAM_POSITIONING = new DialogType(tomogramPositioningIndex);
  public static final DialogType TOMOGRAM_GENERATION = new DialogType(tomogramGenerationIndex);
  public static final DialogType TOMOGRAM_COMBINATION = new DialogType(tomogramCombinationIndex);
  public static final DialogType POST_PROCESSING = new DialogType(postProcessingIndex);
  public static final DialogType CLEAN_UP = new DialogType(cleanUpIndex);
  
  private String toString(int index) {
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
    return "";
  }
}
