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
 * <p> $Log$ </p>
 */
public final class FileType {
  public static final String rcsid = "$Id$";

  public static final FileType TRIM_VOL_OUTPUT = new FileType(
      ImodManager.TRIMMED_VOLUME_KEY);
  public static final FileType SQUEEZE_VOL_OUTPUT = new FileType(
      ImodManager.SQUEEZED_VOLUME_KEY);
  public static final FileType FLATTEN_OUTPUT = new FileType(
      ImodManager.FLAT_VOLUME_KEY);

  private final String imodManagerKey;

  private FileType(String imodManagerKey) {
    this.imodManagerKey = imodManagerKey;
  }

  public String getImodManagerKey() {
    return imodManagerKey;
  }

  public String getFileName(BaseManager manager) {
    if (manager == null) {
      return null;
    }
    String extension = getExtension();
    if (extension == null) {
      return null;
    }
    return manager.getBaseMetaData().getName() + getExtension();
  }

  public File getFile(BaseManager manager) {
    if (manager == null) {
      return null;
    }
    String fileName = getFileName(manager);
    if (fileName == null) {
      return null;
    }
    return new File(manager.getPropertyUserDir(), fileName);
  }

  public String getExtension() {
    if (this == TRIM_VOL_OUTPUT) {
      return DatasetFiles.TOMO_EXT;
    }
    if (this == SQUEEZE_VOL_OUTPUT) {
      return ".sqz";
    }
    if (this == FLATTEN_OUTPUT) {
      return "_flat.rec";
    }
    return null;
  }

  public String toString() {
    return imodManagerKey;
  }
}
