package etomo.type;

import java.io.File;

import etomo.BaseManager;
import etomo.process.ImodManager;
import etomo.util.DatasetFiles;

/**
 * <p>Description: A type of file associated with a process-level panel.</p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/06/05 02:05:14  sueh
 * <p> bug# 1219 A class to help other classes specify files without knowing very
 * <p> much about them.
 * <p> </p>
 */
public final class FileType {
  public static final String rcsid = "$Id$";

  public static final FileType TRIM_VOL_OUTPUT = FileType.getOutputInstance(
      ImodManager.TRIMMED_VOLUME_KEY, false);
  public static final FileType SQUEEZE_VOL_OUTPUT = FileType.getOutputInstance(
      ImodManager.SQUEEZED_VOLUME_KEY, false);
  public static final FileType FLATTEN_OUTPUT = FileType.getOutputInstance(
      ImodManager.FLAT_VOLUME_KEY, false);
  public static final FileType NEWST_OR_BLEND_3D_FIND_OUTPUT = FileType
      .getOutputInstance(ImodManager.FINE_ALIGNED_3D_FIND_KEY, true);
  public static final FileType NEWST_OR_BLEND_OUTPUT = FileType
      .getOutputInstance(ImodManager.FINE_ALIGNED_KEY, true);
  public static final FileType TILT_3D_FIND_OUTPUT = FileType
      .getOutputInstance(ImodManager.FULL_VOLUME_3D_FIND_KEY, true);

  public static final FileType CCD_ERASER_BEADS_INPUT_MODEL = FileType
      .getModelInstance();

  public static final FileType FIND_BEADS_3D_OUTPUT_MODEL = FileType
      .getModelInstance();

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

  private static FileType getOutputInstance(String imodManagerKey) {
    FileType instance = new FileType(imodManagerKey, null, true, true);
    return instance;
  }

  private static FileType getOutputInstance(String imodManagerKey,
      boolean usesAxisID) {
    FileType instance = new FileType(imodManagerKey, null, usesAxisID, true);
    return instance;
  }

  private static FileType getModelInstance() {
    FileType instance = new FileType(null, null, true, true);
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
          + getLeftSide() + getExtension();
    }
    //Example:  newsta.com
    return getLeftSide() + axisIDExtension + getExtension();
  }

  public String getLeftSide() {
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
    if (this == CCD_ERASER_BEADS_INPUT_MODEL) {
      return "_erase";
    }
    if (this == FIND_BEADS_3D_OUTPUT_MODEL) {
      return "_3dfind";
    }
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
    //output
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
    //input models
    if (this == CCD_ERASER_BEADS_INPUT_MODEL) {
      return DatasetFiles.FIDUCIAL_MODEL_EXT;
    }
    //output models
    if (this == FIND_BEADS_3D_OUTPUT_MODEL) {
      return DatasetFiles.MODEL_EXT;
    }
    if (!this.usesDataset && processName != null) {
      //comscripts
      return ".com";
    }
    return null;
  }

  public String toString() {
    if (imodManagerKey == null) {
      return getLeftSide() + getExtension();
    }
    return imodManagerKey;
  }
}
