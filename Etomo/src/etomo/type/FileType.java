package etomo.type;

import java.io.File;

import etomo.BaseManager;
import etomo.process.ImodManager;
import etomo.util.DatasetFiles;

/**
 * <p>Description: A type of file associated with a process-level panel.</p>
 * 
 * <p>Copyright: Copyright 2008, 2009</p>
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

  //stacks
  public static final FileType CCD_ERASER_INPUT = FileType.getStackInstance(
      ImodManager.RAW_STACK_KEY, true);
  public static final FileType CCD_ERASER_OUTPUT = FileType.getStackInstance(
      ImodManager.ERASED_STACK_KEY, true);
  public static final FileType TRIM_VOL_OUTPUT = FileType.getStackInstance(
      ImodManager.TRIMMED_VOLUME_KEY, false);
  public static final FileType SQUEEZE_VOL_OUTPUT = FileType.getStackInstance(
      ImodManager.SQUEEZED_VOLUME_KEY, false);
  public static final FileType FLATTEN_OUTPUT = FileType.getStackInstance(
      ImodManager.FLAT_VOLUME_KEY, false);
  public static final FileType NEWST_OR_BLEND_3D_FIND_OUTPUT = FileType
      .getStackInstance(ImodManager.FINE_ALIGNED_3D_FIND_KEY, true);
  public static final FileType NEWST_OR_BLEND_OUTPUT = FileType
      .getStackInstance(ImodManager.FINE_ALIGNED_KEY, true);
  public static final FileType TILT_3D_FIND_OUTPUT = FileType.getStackInstance(
      ImodManager.FULL_VOLUME_3D_FIND_KEY, true);

  //models
  public static final FileType CCD_ERASER_BEADS_INPUT_MODEL = FileType
      .getModelInstance();
  public static final FileType FIDUCIAL_3D_MODEL = FileType
      .getModelInstance(ImodManager.FIDUCIAL_MODEL_KEY);
  public static final FileType FIND_BEADS_3D_OUTPUT_MODEL = FileType
      .getModelInstance();
  public static final FileType SMOOTHING_ASSESSMENT_OUTPUT_MODEL= FileType.getModelInstance(ImodManager.SMOOTHING_ASSESSMENT_KEY);

  //comscripts
  public static final FileType TRACK_COMSCRIPT = FileType
      .getComscriptInstance(ProcessName.TRACK);
  public static final FileType FIND_BEADS_3D_COMSCRIPT = FileType
      .getComscriptInstance(ProcessName.FIND_BEADS_3D);

  private final String imodManagerKey;
  private final ProcessName processName;
  private final boolean usesAxisID;
  private final boolean usesDataset;

  private FileType(String imodManagerKey, ProcessName processName,
      boolean usesAxisID, boolean usesDataset) {
    this.imodManagerKey = imodManagerKey;
    this.processName = processName;
    this.usesAxisID = usesAxisID;
    this.usesDataset = usesDataset;
  }

  private static FileType getStackInstance(String imodManagerKey) {
    FileType instance = new FileType(imodManagerKey, null, true, true);
    return instance;
  }

  private static FileType getStackInstance(String imodManagerKey,
      boolean usesAxisID) {
    FileType instance = new FileType(imodManagerKey, null, usesAxisID, true);
    return instance;
  }

  private static FileType getModelInstance() {
    FileType instance = new FileType(null, null, true, true);
    return instance;
  }

  private static FileType getModelInstance(String imodManagerKey) {
    FileType instance = new FileType(imodManagerKey, null, true, true);
    return instance;
  }

  private static FileType getComscriptInstance(ProcessName processName) {
    FileType instance = new FileType(null, processName, true, false);
    return instance;
  }

  public static FileType getInstance(ProcessName processName) {
    if (TRACK_COMSCRIPT.processName == processName) {
      return TRACK_COMSCRIPT;
    }
    if (FIND_BEADS_3D_COMSCRIPT.processName == processName) {
      return FIND_BEADS_3D_COMSCRIPT;
    }
    return null;
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

  public String getFileName(BaseManager manager, AxisID axisID) {
    if (manager == null) {
      return null;
    }
    String axisIDExtension = "";
    if (usesAxisID) {
      axisIDExtension = correctAxisID(manager.getBaseMetaData().getAxisType(),
          axisID).getExtension();
    }
    String extension = getExtension();
    if (extension == null) {
      return null;
    }
    if (usesDataset) {
      //Example:  BBa_erase.fid
      return manager.getBaseMetaData().getName() + axisIDExtension
          + getLeftExtension() + getExtension();
    }
    //Example:  newsta.com
    return getLeftExtension() + axisIDExtension + getExtension();
  }

  public String getLeftExtension() {
    //stacks
    if (this == CCD_ERASER_INPUT) {
      return "";
    }
    if (this == CCD_ERASER_OUTPUT) {
      return "_fixed";
    }
    if (this == FIDUCIAL_3D_MODEL) {
      return "";
    }
    if (this == TRIM_VOL_OUTPUT) {
      return "";
    }
    if (this == SQUEEZE_VOL_OUTPUT) {
      return "";
    }
    if (this == FLATTEN_OUTPUT) {
      return "_flat";
    }
    if (this == NEWST_OR_BLEND_3D_FIND_OUTPUT) {
      return "_3dfind";
    }
    if (this == NEWST_OR_BLEND_OUTPUT) {
      return "";
    }
    if (this == TILT_3D_FIND_OUTPUT) {
      return "_3dfind";
    }
    //models
    if (this == CCD_ERASER_BEADS_INPUT_MODEL) {
      return "_erase";
    }
    if (this == FIDUCIAL_3D_MODEL) {
      return "";
    }
    if (this == FIND_BEADS_3D_OUTPUT_MODEL) {
      return "_3dfind";
    }
    if (this==SMOOTHING_ASSESSMENT_OUTPUT_MODEL) {
      return "_checkflat";
    }
    //comscripts
    if (this == TRACK_COMSCRIPT) {
      return processName.toString();
    }
    if (this == FIND_BEADS_3D_COMSCRIPT) {
      return processName.toString();
    }
    return null;
  }

  public ProcessName getProcessName() {
    return processName;
  }

  private static AxisID correctAxisID(AxisType axisType, AxisID axisID) {
    if (axisType == AxisType.DUAL_AXIS && axisID == AxisID.ONLY) {
      return AxisID.FIRST;
    }
    if (axisType == AxisType.SINGLE_AXIS && axisID == AxisID.FIRST) {
      return AxisID.ONLY;
    }
    if (axisType == null || axisType == AxisType.NOT_SET) {
      throw new IllegalStateException(
          "AxisType is not set.  AxisType must be set before getting a dataset file name containing the axisID extension.");
    }
    return axisID;
  }

  private String getExtension() {
    //stacks
    if (this == CCD_ERASER_INPUT) {
      return DatasetFiles.STACK_EXT;
    }
    if (this == CCD_ERASER_OUTPUT) {
      return DatasetFiles.STACK_EXT;
    }
    if (this == TRIM_VOL_OUTPUT) {
      return ".rec";
    }
    if (this == SQUEEZE_VOL_OUTPUT) {
      return ".sqz";
    }
    if (this == FLATTEN_OUTPUT) {
      return ".rec";
    }
    if (this == NEWST_OR_BLEND_3D_FIND_OUTPUT) {
      return ".ali";
    }
    if (this == NEWST_OR_BLEND_OUTPUT) {
      return ".ali";
    }
    if (this == TILT_3D_FIND_OUTPUT) {
      return ".rec";
    }
    //models
    if (this == CCD_ERASER_BEADS_INPUT_MODEL) {
      return DatasetFiles.FIDUCIAL_MODEL_EXT;
    }
    if (this == FIDUCIAL_3D_MODEL) {
      return ".3dmod";
    }
    if (this == FIND_BEADS_3D_OUTPUT_MODEL) {
      return DatasetFiles.MODEL_EXT;
    }
    if (this==SMOOTHING_ASSESSMENT_OUTPUT_MODEL) {
      return ".mod";
    }
    //comscripts
    if (!this.usesDataset && processName != null) {
      return ".com";
    }
    return null;
  }

  public String toString() {
    if (imodManagerKey == null) {
      return getLeftExtension() + getExtension();
    }
    return imodManagerKey;
  }
}
