package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

import etomo.type.AxisID;
import etomo.type.ProcessName;
import etomo.util.DatasetFiles;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class TiltLogFileFilter extends FileFilter {
  public static final String rcsid = "$Id$";

  public boolean accept(File f) {
    String name = f.getName();
    //  If this is a file test its extension, all others should return true
    if (!f.isFile()
        || name.equals(ProcessName.TILT.toString() + DatasetFiles.LOG_EXT)
        || name.equals(ProcessName.TILT.toString()
            + AxisID.FIRST.getExtension() + DatasetFiles.LOG_EXT)
        || name.equals(ProcessName.TILT.toString()
            + AxisID.SECOND.getExtension() + DatasetFiles.LOG_EXT)) {
      return true;
    }
    return false;
  }

  public String getDescription() {
    return "Tilt Log (" + ProcessName.TILT.toString() + DatasetFiles.LOG_EXT
        + ")";
  }
}
/**
 * <p> $Log$ </p>
 */
