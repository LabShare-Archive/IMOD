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
public final class ImageFileType {
  public static final String rcsid = "$Id$";

  public static final ImageFileType TRIM_VOL_OUTPUT = new ImageFileType(
      ImodManager.TRIMMED_VOLUME_KEY);
  public static final ImageFileType SQUEEZE_VOL_OUTPUT = new ImageFileType(
      ImodManager.SQUEEZED_VOLUME_KEY);
  public static final ImageFileType FLATTEN_OUTPUT = new ImageFileType(
      ImodManager.FLAT_VOLUME_KEY);

  private final String imodManagerKey;

  private ImageFileType(String imodManagerKey) {
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
