package etomo.type;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.regex.Pattern;

import etomo.BaseManager;
import etomo.process.ImodManager;

/**
 * <p>Description: A class that can describe the types of files used in Etomo.
 * Gives a description of the name where possible.  Includes the ImodManager
 * key if it exists.  Gives the location of the file if it is in a subdirectory
 * rather then in the main dataset directory.  The files types with name
 * descriptions are stored in a list.  Units test are used to prevent name
 * collisions of these files.</p>
 * 
 * <p>Copyright: Copyright 2008 - 2010</p>
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
 * <p> Revision 1.14  2011/06/30 00:20:23  sueh
 * <p> Bug# 1502 In getFileName removed Thread.dumpStack call.
 * <p>
 * <p> Revision 1.13  2011/05/03 02:54:19  sueh
 * <p> bug# 1416 Added tilt_for_sirt.com, which has its axis letter after "tilt".  Modified equals(...String filename) to
 * <p> handle an extension which doesn't start with a ".".  Using static getInstance functions to avoid having the
 * <p> instances jump constructors.  Add template boolean so that a warning is printed when a getFileName is run on
 * <p> a template file type.  Added getTemplate to avoid printing a warning when a template file name is intentionally
 * <p> retrieved.
 * <p>
 * <p> Revision 1.12  2011/04/09 06:34:52  sueh
 * <p> bug# 1416 Added composite and composite file types (TILT_OUTPUT, and SIRT file types).  Made the functions
 * <p> work with composite.  Removed DUAL_ and SINGLE_AXIS_TOMOGRAM.  Replaced toString and toString2 with
 * <p> getDescription and getIModManagerKey2.
 * <p>
 * <p> Revision 1.11  2011/04/04 17:06:06  sueh
 * <p> bug# 1416 Added/modified ALIGNED_STACK, description, DUAL_AXIS_TOMOGRAM,
 * <p> SINGLE_AXIS_TOMOGRAM, SIRT_OUTPUT_TEMPLACE, SIRT_SCALE_OUTPUT_TEMPLATE,
 * <p> SIRTSETUP_COMSCRIPT, TILT_COMSCRIPT, constructors, getDescription, getFile, getFileName, getRoot,
 * <p> hasFixedName.  Removed hasNameDescription.
 * <p>
 * <p> Revision 1.10  2011/02/15 04:56:37  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.9  2010/04/28 16:27:44  sueh
 * <p> bug# 1344 Added a second imodManagerKey and inSubdirectory
 * <p> boolean.  Collecting the instances with file descriptions into
 * <p> nameFileTypeList.  Implemented getting the subdirectory for peet file
 * <p> types.  Added getInstance functions so a FileType instance can be derived
 * <p> from a name description or a file name.  Added an iterator function for unit
 * <p> testing.
 * <p>
 * <p> Revision 1.8  2010/03/09 22:06:20  sueh
 * <p> bug# 1325 Changed CCD_ERASER_INPUT to RAW_STACK.
 * <p>
 * <p> Revision 1.7  2010/03/03 04:57:27  sueh
 * <p> bug# 1311 Changed FileType.NEWST_OR_BLEND_OUTPUT to
 * <p> ALIGNED_STACK.  Added file types for patch tracking.
 * <p>
 * <p> Revision 1.6  2010/02/17 04:52:18  sueh
 * <p> bug# 1301 Sorted file types to make is easier to detect duplicates.  Added
 * <p> flattening tool file types.
 * <p>
 * <p> Revision 1.5  2010/01/21 21:29:58  sueh
 * <p> bug# 1305 Added ANISOTROPIC_DIFFUSION_OUTPUT.
 * <p>
 * <p> Revision 1.4  2009/12/19 01:10:40  sueh
 * <p> bug# 1294 Added FIDUCIAL_3D_MODEL and
 * <p> SMOOTHING_ASSESSMENT_OUTPUT_MODEL.
 * <p>
 * <p> Revision 1.3  2009/12/11 17:28:21  sueh
 * <p> bug# 1291 Added CCD_ERASER_INPUT and CCD_ERASER_OUTPUT.
 * <p>
 * <p> Revision 1.2  2009/09/01 02:30:35  sueh
 * <p> bug# 1222 Added new file types.
 * <p>
 * <p> Revision 1.1  2009/06/05 02:05:14  sueh
 * <p> bug# 1219 A class to help other classes specify files without knowing very
 * <p> much about them.
 * <p> </p>
 */
public final class FileType {
  public static final String rcsid = "$Id$";

  private static final List namedFileTypeList = new Vector();

  // FileType instances should be placed in alphabetical order, sorted first by
  // type string and then by extension. That should make it be easier to tell
  // if we have a file name collision. FileType instances may be used by
  // multiple types of dataset managers.
  //
  // Dataset coexistence:
  //
  // .edf can coexist with:
  // any .epp
  // any ToolsManager unless it has a different dataset name
  //
  // .ejf can coexist with:
  // Other .ejfs
  // any .epp
  // any ToolsManager unless it has a different dataset name
  //
  // .epe can coexist with:
  // any .epp
  // any ToolsManager unless it has a different dataset name
  //
  // .epp can coexist with:
  // anything
  //
  // ToolsManager can coexist with
  // any .epp
  // any .edf, .ejf, or .epe as long as it has a different dataset name

  // File types with a name description
  public static final FileType FIDUCIAL_3D_MODEL = FileType.getImodInstance(true, true,
      "", ".3dmod", ImodManager.FIDUCIAL_MODEL_KEY);
  public static final FileType ALIGNED_STACK = FileType.getDescribedImodInstance(true,
      true, "", ".ali", ImodManager.FINE_ALIGNED_KEY, "the final aligned stack");
  public static final FileType NEWST_OR_BLEND_3D_FIND_OUTPUT = FileType.getImodInstance(
      true, true, "_3dfind", ".ali", ImodManager.FINE_ALIGNED_3D_FIND_KEY);
  public static final FileType CTF_CORRECTED_STACK = FileType.getImodInstance(true, true,
      "_ctfcorr", ".ali", ImodManager.CTF_CORRECTION_KEY);
  public static final FileType ERASED_BEADS_STACK = FileType.getImodInstance(true, true,
      "_erase", ".ali", ImodManager.ERASED_FIDUCIALS_KEY);
  public static final FileType MTF_FILTERED_STACK = FileType.getImodInstance(true, true,
      "_filt", ".ali", ImodManager.MTF_FILTER_KEY);
  public static final FileType TRANSFORMED_REFINING_MODEL = FileType.getImodInstance(
      true, false, "_refine", ".alimod", ImodManager.TRANSFORMED_MODEL_KEY);
  public static final FileType XCORR_BLEND_OUTPUT = FileType.getInstance(true, true, "",
      ".bl");
  public static final FileType PREBLEND_COMSCRIPT = FileType.getInstance(false,
      true, "preblend", ".com");
  public static final FileType CTF_CORRECTION_COMSCRIPT = FileType.getInstance(false,
      true, "ctfcorrection", ".com");
  public static final FileType FIND_BEADS_3D_COMSCRIPT = FileType.getInstance(false,
      true, "findbeads3d", ".com");
  public static final FileType FLATTEN_COMSCRIPT = FileType.getInstance(false, false,
      "flatten", ".com");
  public static final FileType FLATTEN_TOOL_COMSCRIPT = FileType.getInstance(true, false,
      "_flatten", ".com");
  public static final FileType MTF_FILTER_COMSCRIPT = FileType.getInstance(false, true,
      "mtffilter", ".com");
  public static final FileType SIRTSETUP_COMSCRIPT = FileType.getInstance(false, true,
      "sirtsetup", ".com");
  public static final FileType TILT_COMSCRIPT = FileType.getInstance(false, true, "tilt",
      ".com");
  public static final FileType TILT_FOR_SIRT_COMSCRIPT = FileType.getInstance(false,
      true, "tilt", "_for_sirt.com");
  public static final FileType TRACK_COMSCRIPT = FileType.getInstance(false, true,
      "track", ".com");
  public static final FileType CROSS_CORRELATION_COMSCRIPT = FileType.getInstance(false,
      true, "xcorr", ".com");
  public static final FileType PATCH_TRACKING_COMSCRIPT = FileType.getInstance(false,
      true, "xcorr_pt", ".com");
  public static final FileType DISTORTION_CORRECTED_STACK = FileType.getInstance(true,
      true, "", ".dcst");
  public static final FileType FIDUCIAL_MODEL = FileType.getInstance(true, true, "",
      ".fid");
  public static final FileType CCD_ERASER_BEADS_INPUT_MODEL = FileType.getInstance(true,
      true, "_erase", ".fid");
  public static final FileType FLATTEN_TOOL_OUTPUT = FileType.getImodInstance(true,
      false, "", ".flat", ImodManager.FLATTEN_TOOL_OUTPUT_KEY);
  public static final FileType NAD_TEST_INPUT = FileType.getImodInstanceInSubdirectory(
      false, false, "test", ".input", ImodManager.TEST_VOLUME_KEY);
  public static final FileType JOIN = FileType.getImodInstance(true, false, "", ".join",
      ImodManager.JOIN_KEY);
  public static final FileType MODELED_JOIN = FileType.getImodInstance(true, false,
      "_modeled", ".join", ImodManager.MODELED_JOIN_KEY);
  public static final FileType TRIAL_JOIN = FileType.getImodInstance(true, false,
      "_trial", ".join", ImodManager.TRIAL_JOIN_KEY);
  public static final FileType TILT_ALIGN_LOG = FileType.getInstance(false, true,
      "align", ".log");
  public static final FileType GPU_TEST_LOG = FileType.getInstance(false, false,
      "gputest", ".log");
  public static final FileType FIND_BEADS_3D_OUTPUT_MODEL = FileType.getInstance(true,
      true, "_3dfind", ".mod");
  public static final FileType SMOOTHING_ASSESSMENT_OUTPUT_MODEL = FileType
      .getImodInstance(true, true, "_checkflat", ".mod",
          ImodManager.SMOOTHING_ASSESSMENT_KEY);
  public static final FileType FLATTEN_WARP_INPUT_MODEL = FileType.getInstance(true,
      false, "_flat", ".mod");
  public static final FileType PATCH_VECTOR_MODEL = FileType.getImodInstance(false,
      false, "patch_vector", ".mod", ImodManager.PATCH_VECTOR_MODEL_KEY);
  public static final FileType PATCH_VECTOR_CCC_MODEL = FileType.getImodInstance(false,
      false, "patch_vector_ccc", ".mod", ImodManager.PATCH_VECTOR_CCC_MODEL_KEY);
  public static final FileType PATCH_TRACKING_BOUNDARY_MODEL = FileType.getInstance(true,
      true, "_ptbound", ".mod");
  public static final FileType ANISOTROPIC_DIFFUSION_OUTPUT = FileType.getImodInstance(
      true, false, "", ".nad", ImodManager.ANISOTROPIC_DIFFUSION_VOLUME_KEY);
  public static final FileType PIECE_LIST = FileType.getInstance(true, true, "", ".pl");
  public static final FileType PREALIGNED_STACK = FileType.getImodInstance(true, true,
      "", ".preali", ImodManager.COARSE_ALIGNED_KEY);
  public static final FileType PRE_XG = FileType.getImodInstance(true, true, "",
      ".prexg", null);
  public static final FileType RAW_TILT_ANGLES = FileType.getInstance(true, true, "",
      ".rawtlt");
  public static final FileType TRIM_VOL_OUTPUT = FileType.getImodInstance(true, false,
      "", ".rec", ImodManager.TRIMMED_VOLUME_KEY);
  private static final FileType TILT_OUTPUT_DUAL = FileType.getInstance(true, true, "",
      ".rec");
  public static final FileType TILT_3D_FIND_OUTPUT = FileType.getImodInstance(true, true,
      "_3dfind", ".rec", ImodManager.FULL_VOLUME_3D_FIND_KEY);
  public static final FileType FLATTEN_OUTPUT = FileType.getImodInstance(true, false,
      "_flat", ".rec", ImodManager.FLAT_VOLUME_KEY);
  private static final FileType TILT_OUTPUT_SINGLE = FileType.getInstance(true, true,
      "_full", ".rec");
  public static final FileType COMBINED_VOLUME = FileType.getImodInstance(false, false,
      "sum", ".rec", ImodManager.COMBINED_TOMOGRAM_KEY);
  public static final FileType JOIN_SAMPLE_AVERAGES = FileType.getImodInstance(true,
      false, "", ".sampavg", ImodManager.JOIN_SAMPLE_AVERAGES_KEY);
  public static final FileType JOIN_SAMPLE = FileType.getImodInstance(true, false, "",
      ".sample", ImodManager.JOIN_SAMPLES_KEY);
  public static final FileType SIRT_SUBAREA_SCALED_OUTPUT_TEMPLATE = FileType
      .getTemplateInstance(true, true, "_sub", ".sint");
  public static final FileType SQUEEZE_VOL_OUTPUT = FileType.getImodInstance(true, false,
      "", ".sqz", ImodManager.SQUEEZED_VOLUME_KEY);
  public static final FileType SIRT_SUBAREA_OUTPUT_TEMPLATE = FileType
      .getTemplateInstance(true, true, "_sub", ".srec");
  public static final FileType RAW_STACK = FileType.getTwoImodInstance(true, true, "",
      ".st", ImodManager.RAW_STACK_KEY, ImodManager.PREVIEW_KEY);
  public static final FileType FIXED_XRAYS_STACK = FileType.getImodInstance(true, true,
      "_fixed", ".st", ImodManager.ERASED_STACK_KEY);
  public static final FileType ORIGINAL_RAW_STACK = FileType.getInstance(true, true,
      "_orig", ".st");
  public static final FileType LOCAL_TRANSFORMATION_LIST = FileType.getInstance(true,
      false, "", ".xf");
  public static final FileType AUTO_LOCAL_TRANSFORMATION_LIST = FileType.getInstance(
      true, false, "_auto", ".xf");
  public static final FileType EMPTY_LOCAL_TRANSFORMATION_LIST = FileType.getInstance(
      true, false, "_empty", ".xf");
  public static final FileType MIDAS_LOCAL_TRANSFORMATION_LIST = FileType.getInstance(
      true, false, "_midas", ".xf");

  public static final FileType TILT_OUTPUT = FileType.getDifferentDualSingleInstance(
      TILT_OUTPUT_SINGLE, TILT_OUTPUT_DUAL, ImodManager.FULL_VOLUME_KEY, "the tomogram");
  // Template for .sintnn
  public static final FileType SIRT_SCALED_OUTPUT_TEMPLATE = FileType
      .getDerivedTemplateInstance(TILT_OUTPUT, ".sint", ImodManager.SIRT_KEY);
  // Template for .srecnn
  public static final FileType SIRT_OUTPUT_TEMPLATE = FileType
      .getDerivedTemplateInstance(TILT_OUTPUT, ".srec", ImodManager.SIRT_KEY);

  // File types without a specific name
  public static final FileType AVERAGED_VOLUMES = FileType
      .getUnamedInstance(ImodManager.AVG_VOL_KEY);
  public static final FileType NAD_TEST_VARYING_ITERATIONS = FileType
      .getUnamedInstanceInSubdirectory(ImodManager.VARYING_ITERATION_TEST_KEY);
  public static final FileType NAD_TEST_VARYING_K = FileType
      .getUnamedInstanceInSubdirectory(ImodManager.VARYING_K_TEST_KEY);
  public static final FileType POSITIONING_SAMPLE = FileType
      .getUnamedInstance(ImodManager.SAMPLE_KEY);
  public static final FileType REFERENCE_VOLUMES = FileType
      .getUnamedInstance(ImodManager.REF_KEY);
  public static final FileType TRIAL_TOMOGRAM = FileType
      .getUnamedInstance(ImodManager.TRIAL_TOMOGRAM_KEY);

  private final boolean usesDataset;
  private final boolean usesAxisID;
  private final String typeString;
  private final String extension;
  private final String imodManagerKey;
  private final String imodManagerKey2;
  private final String description;
  private final boolean composite;
  private final boolean inSubdirectory;
  private final FileType subFileType;
  private final FileType singleFileType;
  private final FileType dualFileType;
  private final boolean unnamed;
  private final boolean template;

  private FileType parentFileType = null;

  private FileType(final boolean usesDataset, final boolean usesAxisID,
      final String typeString, final String extension, final String imodManagerKey,
      final String imodManagerKey2, final String description, final boolean composite,
      final boolean inSubdirectory, final FileType subFileType,
      final FileType singleFileType, final FileType dualFileType, final boolean unnamed,
      final boolean template) {
    this.usesDataset = usesDataset;
    this.usesAxisID = usesAxisID;
    this.typeString = typeString;
    this.extension = extension;
    this.imodManagerKey = imodManagerKey;
    this.imodManagerKey2 = imodManagerKey2;
    this.description = description;
    this.composite = composite;
    this.inSubdirectory = inSubdirectory;
    this.subFileType = subFileType;
    this.singleFileType = singleFileType;
    this.dualFileType = dualFileType;
    this.unnamed = unnamed;
    this.template = template;
    if (!unnamed) {
      namedFileTypeList.add(this);
    }
  }

  private static FileType getTemplateInstance(final boolean usesDataset,
      final boolean usesAxisID, final String typeString, final String extension) {
    return new FileType(usesDataset, usesAxisID, typeString, extension, null, null, null,
        false, false, null, null, null, false, true);
  }

  /**
   * Get file types that are quite different in dual and single (BBa.rec and BBa_full.rec).
   * @param singleFileType
   * @param dualFileType
   * @param imodManagerKey
   * @param description
   */
  private static FileType getDifferentDualSingleInstance(final FileType singleFileType,
      final FileType dualFileType, final String imodManagerKey, final String description) {
    FileType instance = new FileType(false, false, null, null, imodManagerKey, null,
        description, true, false, null, singleFileType, dualFileType, false, false);
    // Child file types are not valid by themselves
    singleFileType.parentFileType = instance;
    dualFileType.parentFileType = instance;
    return instance;
  }

  private static FileType getInstance(final boolean usesDataset,
      final boolean usesAxisID, final String typeString, final String extension) {
    return new FileType(usesDataset, usesAxisID, typeString, extension, null, null, null,
        false, false, null, null, null, false, false);
  }

  /**
   * For use when everything is coming from another file type, except the extension
   * (BBa.srec, BBa_full.srec, BBa.sint, BBa_full.sint).
   * @param usesDataset
   * @param usesAxisID
   * @param typeString
   * @param extension
   * @param imodManagerKey
   */
  private static FileType getDerivedTemplateInstance(final FileType subFileType,
      final String extension, final String imodManagerKey) {
    return new FileType(false, false, null, extension, imodManagerKey, null, null, true,
        false, subFileType, null, null, false, true);
  }

  private static FileType getImodInstance(final boolean usesDataset,
      final boolean usesAxisID, final String typeString, final String extension,
      final String imodManagerKey) {
    return new FileType(usesDataset, usesAxisID, typeString, extension, imodManagerKey,
        null, null, false, false, null, null, null, false, false);
  }

  private static FileType getImodInstanceInSubdirectory(final boolean usesDataset,
      final boolean usesAxisID, final String typeString, final String extension,
      final String imodManagerKey) {
    return new FileType(usesDataset, usesAxisID, typeString, extension, imodManagerKey,
        null, null, false, true, null, null, null, false, false);
  }

  private static FileType getTwoImodInstance(final boolean usesDataset,
      final boolean usesAxisID, final String typeString, final String extension,
      final String imodManagerKey, final String imodManagerKey2) {
    return new FileType(usesDataset, usesAxisID, typeString, extension, imodManagerKey,
        imodManagerKey2, null, false, false, null, null, null, false, false);
  }

  private static FileType getDescribedImodInstance(final boolean usesDataset,
      final boolean usesAxisID, final String typeString, final String extension,
      final String imodManagerKey, final String description) {
    return new FileType(usesDataset, usesAxisID, typeString, extension, imodManagerKey,
        null, description, false, false, null, null, null, false, false);
  }

  private static FileType getUnamedInstance(final String imodManagerKey) {
    return new FileType(false, false, "", "", imodManagerKey, null, null, false, false,
        null, null, null, true, false);
  }

  private static FileType getUnamedInstanceInSubdirectory(final String imodManagerKey) {
    return new FileType(false, false, "", "", imodManagerKey, null, null, false, true,
        null, null, null, true, false);
  }

  /**
   * Get FileType instance from its name description.
   * @param usesDataset
   * @param usesAxisID
   * @param typeString
   * @param extension
   * @return
   */
  public static FileType getInstance(final BaseManager manager, boolean usesDataset,
      boolean usesAxisID, String typeString, String extension) {
    Iterator iterator = namedFileTypeList.iterator();
    while (iterator.hasNext()) {
      FileType fileType = (FileType) iterator.next();
      if (fileType.equals(manager, usesDataset, usesAxisID, typeString, extension)) {
        if (fileType.parentFileType != null) {
          // This is a child file type which is not valid by itself
          return fileType.parentFileType;
        }
        return fileType;
      }
    }
    return null;
  }

  /**
   * Get FileType instance from its name description.  Uses regular expression pattern
   * matching.
   * @param manager
   * @param axisID
   * @param usesDataset
   * @param usesAxisID
   * @param fileName
   * @return
   */
  public static FileType getInstance(final BaseManager manager, final AxisID axisID,
      final boolean usesDataset, final boolean usesAxisID, final String fileName) {
    if (fileName == null) {
      return null;
    }
    // Create a pattern for the dataset + axis, or for the axis by itself
    String fixedPattern = "";
    String axisPattern = "";
    String axisExtension = axisID.getExtension();
    if (usesDataset) {
      // If the dataset is part of the file name, then there is a fixed pattern
      if (usesAxisID) {
        fixedPattern = Pattern.quote(manager.getBaseMetaData().getName() + axisExtension);
      }
      else {
        fixedPattern = Pattern.quote(manager.getBaseMetaData().getName());
      }
      // Eliminate file names that should start with the dataset but don't.
      if (!fileName.matches(fixedPattern + ".*")) {
        return null;
      }
    }
    else if (usesAxisID && !axisExtension.equals("")) {
      // If the dataset is not part of the file name, there is no fixed pattern, but the
      // axis is part of the file name, so add it to axisPattern.
      axisPattern = Pattern.quote(axisExtension);
    }
    Iterator iterator = namedFileTypeList.iterator();
    while (iterator.hasNext()) {
      FileType fileType = (FileType) iterator.next();
      // Ignore child file types. Return a file type that equals patterns and booleans.
      if (fileType.parentFileType == null
          && fileType.equals(manager, fileName, usesDataset, usesAxisID, fixedPattern,
              axisPattern)) {
        return fileType;
      }
    }
    return null;
  }

  /**
   * Returns true if the file name can be matched to the fixedPattern, typeString,
   * axisPattern, and extension.
   * @param manager
   * @param fileName
   * @param fixedPattern
   * @param axisPattern
   * @return
   */
  private boolean equals(final BaseManager manager, final String fileName,
      final boolean usesDataset, final boolean usesAxisID, final String fixedPattern,
      final String axisPattern) {
    if (composite) {
      // Handle type files which are based on another file type but have their own
      // extension.
      if (subFileType != null && extension != null) {
        return subFileType.equals(manager, fileName, usesDataset, usesAxisID,
            fixedPattern, axisPattern, Pattern.quote(extension));
      }
      // Handle file types with single and dual file types instead of descriptions.
      return getChildFileType(manager).equals(manager, fileName, usesDataset, usesAxisID,
          fixedPattern, axisPattern);
    }
    return usesDataset == this.usesDataset
        && usesAxisID == this.usesAxisID
        && fileName.matches(fixedPattern + Pattern.quote(typeString) + axisPattern
            + Pattern.quote(extension));
  }

  /**
   * Returns true if the file name can be matches to the fixedPattern, typeString,
   * axisPattern, and extensionPattern.
   * @param manager
   * @param fileName
   * @param fixedPattern
   * @param axisPattern
   * @param extensionPattern
   * @return
   */
  private boolean equals(final BaseManager manager, final String fileName,
      final boolean usesDataset, final boolean usesAxisID, final String fixedPattern,
      final String axisPattern, final String extensionPattern) {
    if (composite) {
      // Handle type files which are based on another file type but have their own
      // extension.
      if (subFileType != null) {
        return subFileType.equals(manager, fileName, usesDataset, usesAxisID,
            fixedPattern, axisPattern, extensionPattern);
      }
      // Handle file types with single and dual file types instead of descriptions.
      return getChildFileType(manager).equals(manager, fileName, usesDataset, usesAxisID,
          fixedPattern, axisPattern, extensionPattern);
    }
    return usesDataset == this.usesDataset
        && usesAxisID == this.usesAxisID
        && fileName.matches(fixedPattern + Pattern.quote(typeString) + axisPattern
            + extensionPattern);
  }

  /**
   * Compares the member variables that make a file type unique:  usesDataset,
   * usesAxisID, typeString, and extension.
   * @param fileType
   * @return
   */
  public boolean equals(final BaseManager manager, final FileType fileType) {
    return equals(manager, fileType.usesDataset, fileType.usesAxisID,
        fileType.typeString, fileType.extension);
  }

  /**
   * Returns true if the dataset, axis, type string and extension are equal.
   * @param usesDataset
   * @param usesAxisID
   * @param typeString
   * @param extension
   * @return
   */
  private boolean equals(final BaseManager manager, final boolean usesDataset,
      final boolean usesAxisID, final String typeString, final String extension) {
    if (composite) {
      // Handle type files which are based on another file type but have their own
      // extension.
      if (subFileType != null && this.extension != null) {
        return this.extension.equals(extension)
            && subFileType.equals(manager, usesDataset, usesAxisID, typeString);
      }
      // Handle file types with single and dual file types instead of descriptions.
      return getChildFileType(manager).equals(manager, usesDataset, usesAxisID,
          typeString, extension);
    }
    return this.usesDataset == usesDataset && this.usesAxisID == usesAxisID
        && this.typeString.equals(typeString) && this.extension.equals(extension);
  }

  /**
   * Returns true if the dataset and axis settings, and type string are equal.  Ignores
   * the extension.
   * @param usesDataset
   * @param usesAxisID
   * @param typeString
   * @return
   */
  private boolean equals(final BaseManager manager, final boolean usesDataset,
      final boolean usesAxisID, final String typeString) {
    if (composite) {
      // Handle type files which are based on another file type
      if (subFileType != null) {
        return subFileType.equals(manager, usesDataset, usesAxisID, typeString);
      }
      // Handle file types with single and dual file types instead of descriptions.
      return getChildFileType(manager).equals(manager, usesDataset, usesAxisID,
          typeString);
    }
    if (this.usesDataset == usesDataset && this.usesAxisID == usesAxisID
        && this.typeString.equals(typeString)) {
      return true;
    }
    return false;
  }

  private FileType getChildFileType(final BaseManager manager) {
    if (composite) {
      if (singleFileType != null && dualFileType != null) {
        if (manager.getBaseMetaData().getAxisType() == AxisType.DUAL_AXIS) {
          return dualFileType;
        }
        return singleFileType;
      }
      else if (subFileType != null) {
        return subFileType;
      }
    }
    return this;
  }

  public boolean hasFixedName(final BaseManager manager) {
    if (composite) {
      if (subFileType != null && extension != null) {
        return !extension.equals("") || subFileType.hasFixedName(manager);
      }
      return getChildFileType(manager).hasFixedName(manager);
    }
    return usesAxisID || usesDataset || !extension.equals("") || !typeString.equals("");
  }

  boolean isInSubdirectory() {
    return inSubdirectory;
  }

  public String getDescription(final BaseManager manager) {
    if (description != null) {
      return description;
    }
    if (imodManagerKey != null) {
      return imodManagerKey;
    }
    return "";
  }

  public String getExtension(final BaseManager manager) {
    if (composite) {
      if (subFileType != null && extension != null) {
        return extension;
      }
      return getChildFileType(manager).getExtension(manager);
    }
    return extension;
  }

  public String getImodManagerKey(final BaseManager manager) {
    return imodManagerKey;
  }

  public String getImodManagerKey2(final BaseManager manager) {
    return imodManagerKey2;
  }

  public File getFile(BaseManager manager, AxisID axisID) {
    if (manager == null || !hasFixedName(manager)) {
      return null;
    }
    String fileName = getFileName(manager, axisID);
    if (fileName == null || fileName.equals("")) {
      return null;
    }
    // Make the file. The file may be in a subdirectory below the dataset
    // location.
    String subdirName;
    if (inSubdirectory && (subdirName = manager.getFileSubdirectoryName()) != null) {
      File subdir = new File(manager.getPropertyUserDir(), subdirName);
      return new File(subdir, fileName);
    }
    return new File(manager.getPropertyUserDir(), fileName);
  }

  /**
   * Return the file name, stripped of its extension.
   * @param manager
   * @param axisID
   * @return
   */
  public String getRoot(BaseManager manager, AxisID axisID) {
    // Template OK at least for now - the current template types have extra text after the
    // extension.
    String fileName = getFileName(manager, axisID, true);
    int index = fileName.lastIndexOf('.');
    return fileName.substring(0, index);
  }

  public String getTemplate(final BaseManager manager, final AxisID axisID) {
    return getFileName(manager, axisID, true);
  }

  public String getFileName(final BaseManager manager, final AxisID axisID) {
    return getFileName(manager, axisID, false);
  }

  private String getFileName(final BaseManager manager, final AxisID axisID,
      final boolean templateOK) {
    if (manager == null || !hasFixedName(manager)) {
      return null;
    }
    if (template && !templateOK) {
      System.err.println("Warning:  Getting the file name of template " + toString());
    }
    if (composite && (subFileType == null || extension == null)) {
      return getChildFileType(manager).getFileName(manager, axisID, true);
    }
    return getLeftSide(manager, axisID) + extension;
  }

  private String getLeftSide(final BaseManager manager, final AxisID axisID) {
    BaseMetaData metaData = manager.getBaseMetaData();
    return getLeftSide(metaData.getName(), metaData.getAxisType(), manager, axisID);
  }

  /**
   * Get the typeString with the dataset and axis letter added as necessary.  For example,
   * the left side of BBa_fixed.st is "BBa_fixed", the left side of tilta.com is "tilta", the
   * left side of tilt.com is "tilt", and the left side of tilta_for_sirt.com is "tilta".
   * @param manager
   * @param axisID
   * @return
   */
  private String getLeftSide(final String rootName, final AxisType axisType,
      final BaseManager manager, final AxisID axisID) {
    if (manager == null || !hasFixedName(manager)) {
      return null;
    }
    if (composite) {
      return getChildFileType(manager).getLeftSide(rootName, axisType, manager, axisID);
    }
    if (!usesDataset && !usesAxisID) {
      // Example: flatten.com
      return typeString;
    }
    String axisIDExtension = "";
    if (usesAxisID) {
      axisIDExtension = correctAxisID(axisType, axisID).getExtension();
    }
    if (usesDataset) {
      // With the dataset the axis follows the dataset
      // Example: BBa_erase.fid
      return rootName + axisIDExtension + typeString;
    }
    // Without the dataset the axis follows the left extension
    // Example: tilta.com
    return typeString + axisIDExtension;
  }

  /**
   * Derive a file name with the same type as this instance, but with a different root
   * name and/or a different axis type as the manager parameter.
   * @param rootName
   * @param axisType
   * @param axisID
   * @param manager
   * @return
   */
  public String deriveFileName(final String rootName, final AxisType axisType,
      final BaseManager manager, final AxisID axisID) {
    return getLeftSide(rootName, axisType, manager, axisID) + getExtension(manager);
  }

  /**
   * Returns the non-generic part of the the left side of the file name.  For
   * example, the type string for BBa_fixed.st is "_fixed", the type string
   * for tilta.com is "tilt", and the type string for tilt.com is "tilt".
   * @return
   */
  public String getTypeString(final BaseManager manager) {
    if (composite) {
      return getChildFileType(manager).getTypeString(manager);
    }
    return typeString;
  }

  /**
   * A null axisID or an ONLY axisID is sometimes used to signify a FIRST axisID
   * in a dual axis dataset.  A similar problem may exist for single axis
   * datasets.  The axisID must be corrected to get a valid file name.
   * 
   * This is not true for Tomogram Combination file names, which do not have an
   * axisID letter (equivalent to AxisID.ONLY).  However these files would have
   * the usesAxisID member variable set to false, so that is not a problem.
   * @param axisType
   * @param axisID
   * @return corrected axisID
   */
  private static AxisID correctAxisID(AxisType axisType, AxisID axisID) {
    if (axisType == AxisType.DUAL_AXIS && (axisID == null || axisID == AxisID.ONLY)) {
      return AxisID.FIRST;
    }
    if (axisType == AxisType.SINGLE_AXIS && (axisID == null || axisID == AxisID.FIRST)) {
      return AxisID.ONLY;
    }
    if (axisType == null || axisType == AxisType.NOT_SET) {
      throw new IllegalStateException(
          "AxisType is not set.  AxisType must be set before getting a dataset "
              + "file name containing the axisID extension.");
    }
    return axisID;
  }

  /**
   * @return namedFileTypeList.iterator()
   */
  static Iterator iterator() {
    return namedFileTypeList.iterator();
  }

  public String toString() {
    if (this == FIDUCIAL_3D_MODEL) {
      return "FIDUCIAL_3D_MODEL";
    }
    if (this == ALIGNED_STACK) {
      return "ALIGNED_STACK";
    }
    if (this == XCORR_BLEND_OUTPUT) {
      return "XCORR_BLEND_OUTPUT";
    }
    if (this == DISTORTION_CORRECTED_STACK) {
      return "DISTORTION_CORRECTED_STACK";
    }
    if (this == FIDUCIAL_MODEL) {
      return "FIDUCIAL_MODEL";
    }
    if (this == FLATTEN_TOOL_OUTPUT) {
      return "FLATTEN_TOOL_OUTPUT";
    }
    if (this == JOIN) {
      return "JOIN";
    }
    if (this == ANISOTROPIC_DIFFUSION_OUTPUT) {
      return "ANISOTROPIC_DIFFUSION_OUTPUT";
    }
    if (this == PREALIGNED_STACK) {
      return "PREALIGNED_STACK";
    }
    if (this == RAW_TILT_ANGLES) {
      return "RAW_TILT_ANGLES";
    }
    if (this == TRIM_VOL_OUTPUT) {
      return "TRIM_VOL_OUTPUT";
    }
    if (this == TILT_OUTPUT_DUAL) {
      return "TILT_OUTPUT_DUAL";
    }
    if (this == JOIN_SAMPLE_AVERAGES) {
      return "JOIN_SAMPLE_AVERAGES";
    }
    if (this == JOIN_SAMPLE) {
      return "JOIN_SAMPLE";
    }
    if (this == SQUEEZE_VOL_OUTPUT) {
      return "SQUEEZE_VOL_OUTPUT";
    }
    if (this == NEWST_OR_BLEND_3D_FIND_OUTPUT) {
      return "NEWST_OR_BLEND_3D_FIND_OUTPUT";
    }
    if (this == FIND_BEADS_3D_OUTPUT_MODEL) {
      return "FIND_BEADS_3D_OUTPUT_MODEL";
    }
    if (this == TILT_3D_FIND_OUTPUT) {
      return "TILT_3D_FIND_OUTPUT";
    }
    if (this == SMOOTHING_ASSESSMENT_OUTPUT_MODEL) {
      return "SMOOTHING_ASSESSMENT_OUTPUT_MODEL";
    }
    if (this == CTF_CORRECTED_STACK) {
      return "CTF_CORRECTED_STACK";
    }
    if (this == CTF_CORRECTION_COMSCRIPT) {
      return "CTF_CORRECTION_COMSCRIPT";
    }
    if (this == ERASED_BEADS_STACK) {
      return "ERASED_BEADS_STACK";
    }
    if (this == CCD_ERASER_BEADS_INPUT_MODEL) {
      return "CCD_ERASER_BEADS_INPUT_MODEL";
    }
    if (this == MTF_FILTERED_STACK) {
      return "MTF_FILTERED_STACK";
    }
    if (this == FIND_BEADS_3D_COMSCRIPT) {
      return "FIND_BEADS_3D_COMSCRIPT";
    }
    if (this == FIXED_XRAYS_STACK) {
      return "FIXED_XRAYS_STACK";
    }
    if (this == FLATTEN_WARP_INPUT_MODEL) {
      return "FLATTEN_WARP_INPUT_MODEL";
    }
    if (this == FLATTEN_COMSCRIPT) {
      return "FLATTEN_COMSCRIPT";
    }
    if (this == FLATTEN_OUTPUT) {
      return "FLATTEN_OUTPUT";
    }
    if (this == FLATTEN_TOOL_COMSCRIPT) {
      return "FLATTEN_TOOL_COMSCRIPT";
    }
    if (this == TILT_OUTPUT_SINGLE) {
      return "TILT_OUTPUT_SINGLE";
    }
    if (this == TILT_OUTPUT) {
      return "TILT_OUTPUT";
    }
    if (this == SIRT_SCALED_OUTPUT_TEMPLATE) {
      return "SIRT_SCALED_OUTPUT_TEMPLATE";
    }
    if (this == SIRT_OUTPUT_TEMPLATE) {
      return "SIRT_OUTPUT_TEMPLATE";
    }
    if (this == MODELED_JOIN) {
      return "MODELED_JOIN";
    }
    if (this == MTF_FILTER_COMSCRIPT) {
      return "MTF_FILTER_COMSCRIPT";
    }
    if (this == ORIGINAL_RAW_STACK) {
      return "ORIGINAL_RAW_STACK";
    }
    if (this == PATCH_VECTOR_MODEL) {
      return "PATCH_VECTOR_MODEL";
    }
    if (this == PATCH_VECTOR_CCC_MODEL) {
      return "PATCH_VECTOR_CCC_MODEL";
    }
    if (this == PATCH_TRACKING_BOUNDARY_MODEL) {
      return "PATCH_TRACKING_BOUNDARY_MODEL";
    }
    if (this == TRANSFORMED_REFINING_MODEL) {
      return "TRANSFORMED_REFINING_MODEL";
    }
    if (this == SIRTSETUP_COMSCRIPT) {
      return "SIRTSETUP_COMSCRIPT";
    }
    if (this == COMBINED_VOLUME) {
      return "COMBINED_VOLUME";
    }
    if (this == NAD_TEST_INPUT) {
      return "NAD_TEST_INPUT";
    }
    if (this == TILT_COMSCRIPT) {
      return "TILT_COMSCRIPT";
    }
    if (this == TILT_FOR_SIRT_COMSCRIPT) {
      return "TILT_FOR_SIRT_COMSCRIPT";
    }
    if (this == TRACK_COMSCRIPT) {
      return "TRACK_COMSCRIPT";
    }
    if (this == TRIAL_JOIN) {
      return "TRIAL_JOIN";
    }
    if (this == CROSS_CORRELATION_COMSCRIPT) {
      return "CROSS_CORRELATION_COMSCRIPT";
    }
    if (this == PATCH_TRACKING_COMSCRIPT) {
      return "PATCH_TRACKING_COMSCRIPT";
    }
    if (this == AVERAGED_VOLUMES) {
      return "AVERAGED_VOLUMES";
    }
    if (this == NAD_TEST_VARYING_ITERATIONS) {
      return "NAD_TEST_VARYING_ITERATIONS";
    }
    if (this == NAD_TEST_VARYING_K) {
      return "NAD_TEST_VARYING_K";
    }
    if (this == POSITIONING_SAMPLE) {
      return "POSITIONING_SAMPLE";
    }
    if (this == REFERENCE_VOLUMES) {
      return "REFERENCE_VOLUMES";
    }
    if (this == TRIAL_TOMOGRAM) {
      return "TRIAL_TOMOGRAM";
    }
    return super.toString();
  }
}