package etomo.type;

import java.io.File;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

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

  //FileType instances should be placed in alphabetical order, sorted first by
  //leftExtension and then by extension.  That should make it be easier to tell
  //if we have a file name collision.  FileType instances may be used by
  //multiple types of dataset managers.
  //
  //Dataset coexistence:
  //
  //.edf can coexist with:
  //any .epp
  //any ToolsManager unless it has a different dataset name
  //
  //.ejf can coexist with:
  //Other .ejfs
  //any .epp
  //any ToolsManager unless it has a different dataset name
  //
  //.epe can coexist with:
  //any .epp
  //any ToolsManager unless it has a different dataset name
  //
  //.epp can coexist with:
  //anything
  //
  //ToolsManager can coexist with
  //any .epp
  //any .edf, .ejf, or .epe as long as it has a different dataset name

  //File types with a name description
  public static final FileType FIDUCIAL_3D_MODEL = new FileType(true, true, "",
      ".3dmod", ImodManager.FIDUCIAL_MODEL_KEY);
  public static final FileType ALIGNED_STACK = new FileType(true, true, "",
      ".ali", ImodManager.FINE_ALIGNED_KEY);
  public static final FileType XCORR_BLEND_OUTPUT = new FileType(true, true,
      "", ".bl", null);
  public static final FileType DISTORTION_CORRECTED_STACK = new FileType(true,
      true, "", ".dcst", null);
  public static final FileType FIDUCIAL_MODEL = new FileType(true, true, "",
      ".fid", null);
  public static final FileType FLATTEN_TOOL_OUTPUT = new FileType(true, false,
      "", ".flat", ImodManager.FLATTEN_TOOL_OUTPUT_KEY);
  public static final FileType JOIN = new FileType(true, false, "", ".join",
      ImodManager.JOIN_KEY);
  public static final FileType ANISOTROPIC_DIFFUSION_OUTPUT = new FileType(
      true, false, "", ".nad", ImodManager.ANISOTROPIC_DIFFUSION_VOLUME_KEY);
  public static final FileType PREALIGNED_STACK = new FileType(true, true, "",
      ".preali", ImodManager.COARSE_ALIGNED_KEY);
  public static final FileType RAW_TILT_ANGLES = new FileType(true, true, "",
      ".rawtlt", null);
  public static final FileType TRIM_VOL_OUTPUT = new FileType(true, false, "",
      ".rec", ImodManager.TRIMMED_VOLUME_KEY);
  public static final FileType DUAL_AXIS_TOMOGRAM = new FileType(true, true,
      "", ".rec", ImodManager.FULL_VOLUME_KEY);
  public static final FileType JOIN_SAMPLE_AVERAGES = new FileType(true, false,
      "", ".sampavg", ImodManager.JOIN_SAMPLE_AVERAGES_KEY);
  public static final FileType JOIN_SAMPLE = new FileType(true, false, "",
      ".sample", ImodManager.JOIN_SAMPLES_KEY);
  public static final FileType SQUEEZE_VOL_OUTPUT = new FileType(true, false,
      "", ".sqz", ImodManager.SQUEEZED_VOLUME_KEY);
  public static final FileType RAW_STACK = new FileType(true, true, "", ".st",
      ImodManager.RAW_STACK_KEY, ImodManager.PREVIEW_KEY);
  public static final FileType NEWST_OR_BLEND_3D_FIND_OUTPUT = new FileType(
      true, true, "_3dfind", ".ali", ImodManager.FINE_ALIGNED_3D_FIND_KEY);
  public static final FileType FIND_BEADS_3D_OUTPUT_MODEL = new FileType(true,
      true, "_3dfind", ".mod", null);
  public static final FileType TILT_3D_FIND_OUTPUT = new FileType(true, true,
      "_3dfind", ".rec", ImodManager.FULL_VOLUME_3D_FIND_KEY);
  public static final FileType SMOOTHING_ASSESSMENT_OUTPUT_MODEL = new FileType(
      true, true, "_checkflat", ".mod", ImodManager.SMOOTHING_ASSESSMENT_KEY);
  public static final FileType CTF_CORRECTED_STACK = new FileType(true, true,
      "_ctfcorr", ".ali", ImodManager.CTF_CORRECTION_KEY);
  public static final FileType ERASED_BEADS_STACK = new FileType(true, true,
      "_erase", ".ali", ImodManager.ERASED_FIDUCIALS_KEY);
  public static final FileType CCD_ERASER_BEADS_INPUT_MODEL = new FileType(
      true, true, "_erase", ".fid", null);
  public static final FileType MTF_FILTERED_STACK = new FileType(true, true,
      "_filt", ".ali", ImodManager.MTF_FILTER_KEY);
  public static final FileType FIXED_XRAYS_STACK = new FileType(true, true,
      "_fixed", ".st", ImodManager.ERASED_STACK_KEY);
  public static final FileType FLATTEN_WARP_INPUT_MODEL = new FileType(true,
      false, "_flat", ".mod", null);
  public static final FileType FLATTEN_OUTPUT = new FileType(true, false,
      "_flat", ".rec", ImodManager.FLAT_VOLUME_KEY);
  public static final FileType FLATTEN_TOOL_COMSCRIPT = new FileType(true,
      false, "_flatten", ".com", null);
  public static final FileType SINGLE_AXIS_TOMOGRAM = new FileType(true, false,
      "_full", ".rec", ImodManager.FULL_VOLUME_KEY);
  public static final FileType MODELED_JOIN = new FileType(true, false,
      "_modeled", ".join", ImodManager.MODELED_JOIN_KEY);
  public static final FileType ORIGINAL_RAW_STACK = new FileType(true, true,
      "_orig", ".st", null);
  public static final FileType PATCH_TRACKING_BOUNDARY_MODEL = new FileType(
      true, true, "_ptbound", ".mod", null);
  public static final FileType TRANSFORMED_REFINING_MODEL = new FileType(true,
      false, "_refine", ".alimod", ImodManager.TRANSFORMED_MODEL_KEY);
  public static final FileType TRIAL_JOIN = new FileType(true, false, "_trial",
      ".join", ImodManager.TRIAL_JOIN_KEY);
  public static final FileType CTF_CORRECTION_COMSCRIPT = new FileType(false,
      true, "ctfcorrection", ".com", null);
  public static final FileType FIND_BEADS_3D_COMSCRIPT = new FileType(false,
      true, "findbeads3d", ".com", null);
  public static final FileType FLATTEN_COMSCRIPT = new FileType(false, false,
      "flatten", ".com", null);
  public static final FileType MTF_FILTER_COMSCRIPT = new FileType(false, true,
      "mtffilter", ".com", null);
  public static final FileType PATCH_VECTOR_MODEL = new FileType(false, false,
      "patch_vector", ".mod", ImodManager.PATCH_VECTOR_MODEL_KEY);
  public static final FileType PATCH_VECTOR_CCC_MODEL = new FileType(false,
      false, "patch_vector_ccc", ".mod", ImodManager.PATCH_VECTOR_CCC_MODEL_KEY);
  public static final FileType COMBINED_VOLUME = new FileType(false, false,
      "sum", ".rec", ImodManager.COMBINED_TOMOGRAM_KEY);
  public static final FileType NAD_TEST_INPUT = new FileType(false, false,
      "test", ".input", ImodManager.TEST_VOLUME_KEY, true);
  public static final FileType TRACK_COMSCRIPT = new FileType(false, true,
      "track", ".com", null);
  public static final FileType CROSS_CORRELATION_COMSCRIPT = new FileType(
      false, true, "xcorr", ".com", null);
  public static final FileType PATCH_TRACKING_COMSCRIPT = new FileType(false,
      true, "xcorr_pt", ".com", null);

  //File types without a name description
  public static final FileType AVERAGED_VOLUMES = FileType
      .getUnamedInstance(ImodManager.AVG_VOL_KEY);
  public static final FileType NAD_TEST_VARYING_ITERATIONS = FileType
      .getUnamedInstance(ImodManager.VARYING_ITERATION_TEST_KEY, true);
  public static final FileType NAD_TEST_VARYING_K = FileType.getUnamedInstance(
      ImodManager.VARYING_K_TEST_KEY, true);
  public static final FileType POSITIONING_SAMPLE = FileType
      .getUnamedInstance(ImodManager.SAMPLE_KEY);
  public static final FileType REFERENCE_VOLUMES = FileType
      .getUnamedInstance(ImodManager.REF_KEY);
  public static final FileType TRIAL_TOMOGRAM = FileType
      .getUnamedInstance(ImodManager.TRIAL_TOMOGRAM_KEY);

  private final String imodManagerKey;
  private final String imodManagerKey2;
  private final boolean usesAxisID;
  private final boolean usesDataset;
  private final String typeString;
  private final String extension;
  private final boolean inSubdirectory;

  private FileType(boolean usesDataset, boolean usesAxisID, String typeString,
      String extension, String imodManagerKey) {
    this.imodManagerKey = imodManagerKey;
    this.imodManagerKey2 = null;
    this.usesAxisID = usesAxisID;
    this.usesDataset = usesDataset;
    this.typeString = typeString;
    this.extension = extension;
    this.inSubdirectory = false;
    //Exlude FileTypes with no name description
    if (hasNameDescription()) {
      namedFileTypeList.add(this);
    }
  }

  private FileType(boolean usesDataset, boolean usesAxisID, String typeString,
      String extension, String imodManagerKey, boolean inSubdirectory) {
    this.imodManagerKey = imodManagerKey;
    this.imodManagerKey2 = null;
    this.usesAxisID = usesAxisID;
    this.usesDataset = usesDataset;
    this.typeString = typeString;
    this.extension = extension;
    this.inSubdirectory = inSubdirectory;
    //Exlude FileTypes with no name description
    if (hasNameDescription()) {
      namedFileTypeList.add(this);
    }
  }

  private FileType(boolean usesDataset, boolean usesAxisID, String typeString,
      String extension, String imodManagerKey, String imodManagerKey2) {
    this.imodManagerKey = imodManagerKey;
    this.imodManagerKey2 = imodManagerKey2;
    this.usesAxisID = usesAxisID;
    this.usesDataset = usesDataset;
    this.typeString = typeString;
    this.extension = extension;
    this.inSubdirectory = false;
    //Exlude FileTypes with no name description
    if (hasNameDescription()) {
      namedFileTypeList.add(this);
    }
  }

  private static FileType getUnamedInstance(String imodManagerKey) {
    return new FileType(false, false, "", "", imodManagerKey);
  }

  private static FileType getUnamedInstance(String imodManagerKey,
      boolean inSubdirectory) {
    return new FileType(false, false, "", "", imodManagerKey, inSubdirectory);
  }

  /**
   * Get FileType instance from its name description.
   * @param usesDataset
   * @param usesAxisID
   * @param typeString
   * @param extension
   * @return
   */
  public static FileType getInstance(boolean usesDataset, boolean usesAxisID,
      String typeString, String extension) {
    Iterator iterator = namedFileTypeList.iterator();
    while (iterator.hasNext()) {
      FileType fileType = (FileType) iterator.next();
      if (fileType.usesDataset == usesDataset
          && fileType.usesAxisID == usesAxisID
          && fileType.typeString.equals(typeString)
          && fileType.extension.equals(extension)) {
        return fileType;
      }
    }
    return null;
  }

  /**
   * Get FileType instance from its name description.  Derives the typeString
   * and the extension.
   * @param manager
   * @param axisID
   * @param usesDataset
   * @param usesAxisID
   * @param fileName
   * @return
   */
  public static FileType getInstance(BaseManager manager, AxisID axisID,
      boolean usesDataset, boolean usesAxisID, String fileName) {
    if (fileName == null) {
      return null;
    }
    String typeString = null;
    int extensionIndex = fileName.lastIndexOf('.');
    String leftSide;
    if (extensionIndex == -1) {
      leftSide = fileName;
    }
    else {
      leftSide = fileName.substring(0, fileName.lastIndexOf('.'));
    }
    if (!usesDataset && !usesAxisID) {
      //Example:  flatten.com
      typeString = leftSide;
    }
    if (manager != null) {
      BaseMetaData metaData = manager.getBaseMetaData();
      String axisIDExtension = "";
      if (usesAxisID) {
        axisIDExtension = correctAxisID(metaData.getAxisType(), axisID)
            .getExtension();
      }
      if (usesDataset) {
        //With the dataset the axis follows the dataset
        //Example:  BBa_erase.fid
        //Strip "BBa" off of BBa_erase (leftside) to get the typeString.
        String dataset = metaData.getName() + axisIDExtension;
        //If dataset + axisID extension is not at the beginning of the file name
        //then it is not a valid file name.
        if (leftSide.indexOf(dataset) == 0) {
          if (dataset.length() == leftSide.length()) {
            typeString = "";
          }
          else if (dataset.length() < leftSide.length()) {
            typeString = leftSide.substring(dataset.length());
          }
        }
      }
      else {
        //Without the dataset the axis follows the left extension
        //Example:  tilta.com
        //String the axisID off of the left side to get the typeString
        if (axisIDExtension.equals("")) {
          typeString = leftSide;
        }
        else {
          //If the axisID extension is not at the end of the leftSide then it is
          //not a valid file name.
          if (leftSide.lastIndexOf(axisIDExtension) == leftSide.length() - 1) {
            typeString = leftSide.substring(0, leftSide.length()
                - axisIDExtension.length());
          }
        }
      }
    }
    if (typeString == null) {
      return null;
    }
    String extension;
    if (extensionIndex == -1) {
      extension = "";
    }
    else {
      extension = fileName.substring(extensionIndex, fileName.length());
    }
    return FileType.getInstance(usesDataset, usesAxisID, typeString, extension);
  }

  public boolean hasNameDescription() {
    return usesAxisID || usesDataset || !extension.equals("")
        || !typeString.equals("");
  }

  public String getExtension() {
    return extension;
  }

  public String getImodManagerKey() {
    return imodManagerKey;
  }

  public String getImodManagerKey2() {
    return imodManagerKey2;
  }

  public File getFile(BaseManager manager, AxisID axisID) {
    if (manager == null || !hasNameDescription()) {
      return null;
    }
    String fileName = getFileName(manager, axisID);
    if (fileName == null || fileName.equals("")) {
      return null;
    }
    //Make the file.  The file may be in a subdirectory below the dataset
    //location.
    String subdirName;
    if (inSubdirectory
        && (subdirName = manager.getFileSubdirectoryName()) != null) {
      File subdir = new File(manager.getPropertyUserDir(), subdirName);
      return new File(subdir, fileName);
    }
    return new File(manager.getPropertyUserDir(), fileName);
  }

  /**
   * Returns the file name.  Assumes that the axisID is ONLY.
   * @param manager
   * @return
   */
  public String getFileName(BaseManager manager) {
    if (manager == null || !hasNameDescription()) {
      return null;
    }
    return getFileName(manager, AxisID.ONLY);
  }

  public String getFileName(BaseManager manager, AxisID axisID) {
    if (manager == null || !hasNameDescription()) {
      return null;
    }
    return getRoot(manager, axisID) + extension;
  }

  /**
   * Returns the file name without the dot or extension.  For example, the root
   * of BBa_fixed.st is "BBa_fixed", the root of tilta.com is "tilta", and the
   * root of tilt.com is "tilt".
   * @param manager
   * @param axisID
   * @return
   */
  public String getRoot(BaseManager manager, AxisID axisID) {
    if (manager == null || !hasNameDescription()) {
      return null;
    }
    if (!usesDataset && !usesAxisID) {
      //Example:  flatten.com
      return typeString;
    }
    if (manager == null) {
      return null;
    }
    BaseMetaData metaData = manager.getBaseMetaData();
    String axisIDExtension = "";
    if (usesAxisID) {
      axisIDExtension = correctAxisID(metaData.getAxisType(), axisID)
          .getExtension();
    }
    if (usesDataset) {
      //With the dataset the axis follows the dataset
      //Example:  BBa_erase.fid
      return metaData.getName() + axisIDExtension + typeString;
    }
    //Without the dataset the axis follows the left extension
    //Example:  tilta.com
    return typeString + axisIDExtension;
  }

  /**
   * Returns the non-generic part of the the left side of the file name.  For
   * example, the type string for BBa_fixed.st is "_fixed", the type string
   * for tilta.com is "tilt", and the type string for tilt.com is "tilt".
   * @return
   */
  public String getTypeString() {
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
    if (axisType == AxisType.DUAL_AXIS
        && (axisID == null || axisID == AxisID.ONLY)) {
      return AxisID.FIRST;
    }
    if (axisType == AxisType.SINGLE_AXIS
        && (axisID == null || axisID == AxisID.FIRST)) {
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
   * @return a description of the file
   */
  public String toString() {
    if (typeString.equals("") && extension.equals("")) {
      return imodManagerKey;
    }
    return "the " + typeString + extension + " file";
  }

  /**
   * @return a description of the file based on the second manager key
   */
  public String toString2() {
    if (imodManagerKey2 == null) {
      return "";
    }
    return imodManagerKey2;
  }

  /**
   * @return namedFileTypeList.iterator()
   */
  static Iterator iterator() {
    return namedFileTypeList.iterator();
  }

  /**
   * Compares the member variables that make a file type unique:  usesDataset,
   * usesAxisID, typeString, and extension.
   * @param fileType
   * @return
   */
  public boolean equals(FileType fileType) {
    return fileType.usesDataset == usesDataset
        && fileType.usesAxisID == usesAxisID
        && fileType.typeString.equals(typeString)
        && fileType.extension.equals(extension);
  }
}
