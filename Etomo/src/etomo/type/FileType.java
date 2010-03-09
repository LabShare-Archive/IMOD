package etomo.type;

import java.io.File;

import etomo.BaseManager;
import etomo.process.ImodManager;

/**
 * <p>Description: Types of files used in Etomo.</p>
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

  public static final FileType FIDUCIAL_3D_MODEL = new FileType(true, true, "",
      ".3dmod", ImodManager.FIDUCIAL_MODEL_KEY);
  public static final FileType ALIGNED_STACK = new FileType(true, true, "",
      ".ali", ImodManager.FINE_ALIGNED_KEY);
  public static final FileType FIDUCIAL_MODEL = new FileType(
      true, true, "", ".fid", null);
  public static final FileType FLATTEN_TOOL_OUTPUT = new FileType(true, false,
      "", ".flat", ImodManager.FLATTEN_TOOL_OUTPUT_KEY);
  public static final FileType ANISOTROPIC_DIFFUSION_OUTPUT = new FileType(
      true, false, "", ".nad", ImodManager.ANISOTROPIC_DIFFUSION_VOLUME_KEY);
  public static final FileType PREALIGNED_STACK = new FileType(true, true, "",
      ".preali", ImodManager.COARSE_ALIGNED_KEY);
  public static final FileType RAW_TILT_ANGLES = new FileType(true, true, "",
      ".rawtlt", null);
  public static final FileType TRIM_VOL_OUTPUT = new FileType(true, false, "",
      ".rec", ImodManager.TRIMMED_VOLUME_KEY);
  public static final FileType SQUEEZE_VOL_OUTPUT = new FileType(true, false,
      "", ".sqz", ImodManager.SQUEEZED_VOLUME_KEY);
  public static final FileType RAW_STACK = new FileType(true, true, "",
      ".st", ImodManager.RAW_STACK_KEY);
  public static final FileType NEWST_OR_BLEND_3D_FIND_OUTPUT = new FileType(
      true, true, "_3dfind", ".ali", ImodManager.FINE_ALIGNED_3D_FIND_KEY);
  public static final FileType FIND_BEADS_3D_OUTPUT_MODEL = new FileType(true,
      true, "_3dfind", ".mod", null);
  public static final FileType TILT_3D_FIND_OUTPUT = new FileType(true, true,
      "_3dfind", ".rec", ImodManager.FULL_VOLUME_3D_FIND_KEY);
  public static final FileType SMOOTHING_ASSESSMENT_OUTPUT_MODEL = new FileType(
      true, true, "_checkflat", ".mod", ImodManager.SMOOTHING_ASSESSMENT_KEY);
  public static final FileType CCD_ERASER_BEADS_INPUT_MODEL = new FileType(
      true, true, "_erase", ".fid", null);
  public static final FileType FIND_BEADS_3D_COMSCRIPT = new FileType(false,
      true, ProcessName.FIND_BEADS_3D.toString(), ".com", null);
  public static final FileType CCD_ERASER_OUTPUT = new FileType(true, true,
      "_fixed", ".st", ImodManager.ERASED_STACK_KEY);
  public static final FileType FLATTEN_WARP_INPUT_MODEL = new FileType(true,
      false, "_flat", ".mod", null);
  public static final FileType FLATTEN_OUTPUT = new FileType(true, false,
      "_flat", ".rec", ImodManager.FLAT_VOLUME_KEY);
  public static final FileType FLATTEN_TOOL_COMSCRIPT = new FileType(true,
      false, "_flatten", ".com", null);
  public static final FileType PATCH_TRACKING_BOUNDARY_MODEL = new FileType(
      true, true, "_ptbound", ".mod", null);
  public static final FileType TRACK_COMSCRIPT = new FileType(false, true,
      ProcessName.TRACK.toString(), ".com", null);
  public static final FileType CROSS_CORRELATION_COMSCRIPT = new FileType(false,
      true, ProcessName.XCORR.toString(), ".com", null);
  public static final FileType PATCH_TRACKING_COMSCRIPT = new FileType(false,
      true, ProcessName.XCORR_PT.toString(), ".com", null);

  private final String imodManagerKey;
  private final boolean usesAxisID;
  private final boolean usesDataset;
  private final String typeString;
  private final String extension;

  private FileType(boolean usesDataset, boolean usesAxisID, String typeString,
      String extension, String imodManagerKey) {
    this.imodManagerKey = imodManagerKey;
    this.usesAxisID = usesAxisID;
    this.usesDataset = usesDataset;
    this.typeString = typeString;
    this.extension = extension;
  }

  public String getExtension() {
    return extension;
  }

  public String getImodManagerKey() {
    return imodManagerKey;
  }

  public File getFile(BaseManager manager, AxisID axisID) {
    if (manager == null) {
      return null;
    }
    String fileName = getFileName(manager, axisID);
    if (fileName == null) {
      return null;
    }
    return new File(manager.getPropertyUserDir(), fileName);
  }

  public String getFileName(BaseManager manager) {
    return getFileName(manager, AxisID.ONLY);
  }

  public String getFileName(BaseManager manager, AxisID axisID) {
    return getRoot(manager, axisID) + extension;
  }

  public String getRoot(BaseManager manager, AxisID axisID) {
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

  public String getTypeString() {
    return typeString;
  }

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
          "AxisType is not set.  AxisType must be set before getting a dataset file name containing the axisID extension.");
    }
    return axisID;
  }

  public String toString() {
    if (imodManagerKey == null) {
      return typeString + extension;
    }
    return imodManagerKey;
  }
}
