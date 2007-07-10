package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

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
 * 
 * <p> $Log$
 * <p> Revision 1.1  2007/03/26 23:33:03  sueh
 * <p> bug# 964 Tilt file filter (.tlt).
 * <p> </p>
 */
public class TiltFileFilter extends FileFilter {
  public static final String rcsid = "$Id$";

  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(DatasetFiles.TILT_FILE_EXT)) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "Tilt Angles File (" + DatasetFiles.TILT_FILE_EXT + ")";
  }
}
