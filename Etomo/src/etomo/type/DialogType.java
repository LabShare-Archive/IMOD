package etomo.type;

import java.util.Properties;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2010</p>
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
 * <p> Revision 1.17  2011/02/22 05:37:32  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.16  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.15  2009/10/23 19:44:00  sueh
 * <p> bug# 1275 Changed the name of TabType to DataFileType to clarify what it
 * <p> corresponds to.
 * <p>
 * <p> Revision 1.14  2009/02/05 23:44:13  sueh
 * <p> bug# 1148 Documenting storable names.
 * <p>
 * <p> Revision 1.13  2008/10/16 20:57:06  sueh
 * <p> bug# 1141 Added FINAL_ALIGNED_STACK.
 * <p>
 * <p> Revision 1.12  2008/05/28 02:48:27  sueh
 * <p> bug# 1111 Added JOIN.
 * <p>
 * <p> Revision 1.11  2007/11/06 19:35:59  sueh
 * <p> bug# 1047 Added anisotropic diffusion dialog type.
 * <p>
 * <p> Revision 1.10  2007/02/19 21:54:26  sueh
 * <p> bug# 964 Added PEET dialog type.
 * <p>
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

  private static final String PROPERTIES_KEY = "DialogType";

  private static final int setupIndex = 0;
  private static final int preProcessingIndex = 1;
  private static final int coarseAlignmentIndex = 2;
  private static final int fiducialModelIndex = 3;
  private static final int fineAlignmentIndex = 4;
  private static final int tomogramPositioningIndex = 5;
  private static final int finalAlignedStackIndex = 6;
  private static final int tomogramGenerationIndex = 7;
  private static final int tomogramCombinationIndex = 8;
  private static final int postProcessingIndex = 9;
  private static final int cleanUpIndex = 10;

  public static final int TOTAL_RECON = cleanUpIndex + 1;

  private static final int joinIndex = 0;

  public static final int TOTAL_JOIN = joinIndex + 1;

  private static final int parallelIndex = 0;
  private static final int anisotropicDiffusionIndex = 1;

  public static final int TOTAL_PARALLEL = anisotropicDiffusionIndex + 1;

  private static final int peetIndex = 1;

  public static final int TOTAL_PEET = peetIndex + 1;

  private static final int serialSectionsIndex = 1;

  public static final int TOTAL_SERIAL_SECTIONS = serialSectionsIndex + 1;

  // Storable names cannot be changed without handling the resulting backwards
  // compatibility errors.
  private static final String SETUP_RECON_NAME = "SetupRecon";
  private static final String PRE_PROCESSING_NAME = "PreProc";
  private static final String COARSE_ALIGNMENT_NAME = "CoarseAlign";
  private static final String FIDUCIAL_MODEL_NAME = "FidModel";
  private static final String FINE_ALIGNMENT_NAME = "FineAlign";
  private static final String TOMOGRAM_POSITIONING_NAME = "TomoPos";
  private static final String FINAL_ALIGNED_STACK_NAME = "FinalStack";
  private static final String TOMOGRAM_GENERATION_NAME = "TomoGen";
  private static final String TOMOGRAM_COMBINATION_NAME = "Combine";
  private static final String POST_PROCESSING_NAME = "PostProc";
  private static final String CLEAN_UP_NAME = "CleanUp";
  private static final String JOIN_NAME = "Join";
  private static final String PARALLEL_NAME = "Parallel";
  private static final String ANISOTROPIC_DIFFUSION_NAME = "AnisotropicDiffusion";
  private static final String PEET_STARTUP_NAME = "PeetStart";
  private static final String PEET_NAME = "Peet";
  private static final String TOOLS_NAME = "Tools";
  private static final String DIRECTIVE_EDITOR_NAME = "DirectiveEditor";

  private final String name;
  private final int index;
  private final DataFileType dataFileType;

  private DialogType(DataFileType dataFileType, int index) {
    this.index = index;
    name = toString(dataFileType, index);
    this.dataFileType = dataFileType;
  }

  /**
   * Returns a string representation of the object.
   */
  public String toString() {
    return name;
  }

  /**
   * Storable names cannot be changed without handling the resulting backwards
   * compatibility errors.
   * @return
   */
  public String getStorableName() {
    return getStorableName(dataFileType, index);
  }

  public String getCompactLabel() {
    return getCompactLabel(dataFileType, index);
  }

  public int toIndex() {
    return index;
  }

  public static final DialogType SETUP_RECON = new DialogType(DataFileType.RECON,
      setupIndex);
  public static final DialogType PRE_PROCESSING = new DialogType(DataFileType.RECON,
      preProcessingIndex);
  public static final DialogType COARSE_ALIGNMENT = new DialogType(DataFileType.RECON,
      coarseAlignmentIndex);
  public static final DialogType FIDUCIAL_MODEL = new DialogType(DataFileType.RECON,
      fiducialModelIndex);
  public static final DialogType FINE_ALIGNMENT = new DialogType(DataFileType.RECON,
      fineAlignmentIndex);

  public static final DialogType TOMOGRAM_POSITIONING = new DialogType(
      DataFileType.RECON, tomogramPositioningIndex);
  public static final DialogType FINAL_ALIGNED_STACK = new DialogType(DataFileType.RECON,
      finalAlignedStackIndex);
  public static final DialogType TOMOGRAM_GENERATION = new DialogType(DataFileType.RECON,
      tomogramGenerationIndex);
  public static final DialogType TOMOGRAM_COMBINATION = new DialogType(
      DataFileType.RECON, tomogramCombinationIndex);
  public static final DialogType POST_PROCESSING = new DialogType(DataFileType.RECON,
      postProcessingIndex);
  public static final DialogType CLEAN_UP = new DialogType(DataFileType.RECON,
      cleanUpIndex);

  public static final DialogType JOIN = new DialogType(DataFileType.JOIN, joinIndex);

  public static final DialogType PARALLEL = new DialogType(DataFileType.PARALLEL,
      parallelIndex);
  public static final DialogType ANISOTROPIC_DIFFUSION = new DialogType(
      DataFileType.PARALLEL, anisotropicDiffusionIndex);

  public static final DialogType PEET_STARTUP = new DialogType(DataFileType.PEET, 0);
  public static final DialogType PEET = new DialogType(DataFileType.PEET, peetIndex);

  public static final DialogType SERIAL_SECTIONS_STARTUP = new DialogType(
      DataFileType.SERIAL_SECTIONS, 0);
  public static final DialogType SERIAL_SECTIONS = new DialogType(
      DataFileType.SERIAL_SECTIONS, serialSectionsIndex);

  public static final DialogType TOOLS = new DialogType(DataFileType.TOOLS, 0);
  public static final DialogType DIRECTIVE_EDITOR = new DialogType(
      DataFileType.DIRECTIVE_EDITOR, 0);

  private String toString(DataFileType dataFileType, int index) {
    if (dataFileType == DataFileType.RECON) {
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
      case finalAlignedStackIndex:
        return "Final Aligned Stack";
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
    else if (dataFileType == DataFileType.JOIN) {
      switch (index) {
      case joinIndex:
        return "Join";
      }
    }
    else if (dataFileType == DataFileType.PARALLEL) {
      switch (index) {
      case parallelIndex:
        return "Parallel";
      case anisotropicDiffusionIndex:
        return "Anisotropic Diffusion";
      }
    }
    else if (dataFileType == DataFileType.PEET) {
      switch (index) {
      case 0:
        return "PEET Startup";
      case peetIndex:
        return "PEET";
      }
    }
    else if (dataFileType == DataFileType.TOOLS) {
      switch (index) {
      case 0:
        return "Tools";
      }
    }
    else if (dataFileType == DataFileType.DIRECTIVE_EDITOR) {
      switch (index) {
      case 0:
        return "Directive Editor";
      }
    }
    return "";
  }

  /**
   * Return a name without spaces.  All storable names must be unique to
   * DialogType.
   * @param dataFileType
   * @param index
   * @return
   */
  private String getCompactLabel(DataFileType dataFileType, int index) {
    if (dataFileType == DataFileType.RECON) {
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
      case finalAlignedStackIndex:
        return "Stack";
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
    else if (dataFileType == DataFileType.PARALLEL) {
      switch (index) {
      case parallelIndex:
        return "Para";
      case anisotropicDiffusionIndex:
        return "NAD";
      }
    }
    else if (dataFileType == DataFileType.PEET) {
      switch (index) {
      case 0:
        return "PEET-Start";
      case peetIndex:
        return "PEET";
      }
    }
    else if (dataFileType == DataFileType.TOOLS) {
      switch (index) {
      case 0:
        return "Tools";
      }
    }
    else if (dataFileType == DataFileType.DIRECTIVE_EDITOR) {
      switch (index) {
      case 0:
        return "Dir-Ed";
      }
    }
    return "";
  }

  /**
   * Storable names cannot be changed without handling the resulting backwards
   * compatibility errors.
   * Return a name without spaces.  All storable names must be unique to
   * DialogType.
   * @param dataFileType
   * @param index
   * @return
   */
  private String getStorableName(DataFileType dataFileType, int index) {
    if (dataFileType == DataFileType.RECON) {
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
      case finalAlignedStackIndex:
        return FINAL_ALIGNED_STACK_NAME;
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
    else if (dataFileType == DataFileType.PARALLEL) {
      switch (index) {
      case parallelIndex:
        return PARALLEL_NAME;
      case anisotropicDiffusionIndex:
        return ANISOTROPIC_DIFFUSION_NAME;
      }
    }
    else if (dataFileType == DataFileType.PEET) {
      switch (index) {
      case 0:
        return PEET_STARTUP_NAME;
      case peetIndex:
        return PEET_NAME;
      }
    }
    else if (dataFileType == DataFileType.TOOLS) {
      switch (index) {
      case 0:
        return TOOLS_NAME;
      }
    }
    else if (dataFileType == DataFileType.DIRECTIVE_EDITOR) {
      switch (index) {
      case 0:
        return DIRECTIVE_EDITOR_NAME;
      }
    }
    return "";
  }

  public boolean equals(String storableName) {
    if (storableName == null) {
      return false;
    }
    return getStorableName(dataFileType, index).equals(storableName);
  }

  public void store(Properties props) {
    props.setProperty(PROPERTIES_KEY, getStorableName(dataFileType, index));
  }

  static private String createKey(String prepend) {
    if (prepend.endsWith(".")) {
      return prepend + PROPERTIES_KEY;
    }
    return prepend + "." + PROPERTIES_KEY;
  }

  public void store(Properties props, String prepend) {
    props.setProperty(createKey(prepend), getStorableName(dataFileType, index));
  }

  static public void remove(Properties props, String prepend) {
    props.remove(createKey(prepend));
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
    if (storableName.equals(FINAL_ALIGNED_STACK_NAME)) {
      return FINAL_ALIGNED_STACK;
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
    if (storableName.equals(ANISOTROPIC_DIFFUSION_NAME)) {
      return ANISOTROPIC_DIFFUSION;
    }
    if (storableName.equals(PEET_STARTUP_NAME)) {
      return PEET_STARTUP;
    }
    if (storableName.equals(PEET_NAME)) {
      return PEET;
    }
    if (storableName.equals(TOOLS_NAME)) {
      return TOOLS;
    }
    if (storableName.equals(DIRECTIVE_EDITOR_NAME)) {
      return DIRECTIVE_EDITOR;
    }
    return null;
  }

  /**
   * Load property value without a default
   * @param props
   * @param prepend
   * @return
   */
  public static DialogType load(Properties props, String prepend) {
    return getInstance(props.getProperty(createKey(prepend)));
  }

  public static DialogType load(DataFileType dataFileType, Properties props) {
    DialogType defaultType = getDefault(dataFileType);
    if (defaultType != null) {
      return getInstance(props.getProperty(DialogType.PROPERTIES_KEY,
          defaultType.toString()));
    }
    return getInstance(props.getProperty(DialogType.PROPERTIES_KEY));
  }

  public static DialogType getDefault(DataFileType dataFileType) {
    if (dataFileType == DataFileType.PARALLEL) {
      return PARALLEL;
    }
    if (dataFileType == DataFileType.PEET) {
      return PEET;
    }
    if (dataFileType == DataFileType.DIRECTIVE_EDITOR) {
      return DIRECTIVE_EDITOR;
    }
    return null;
  }
}