package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 * <p>Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log$$ </p>
 */
public class MtfFileFilter extends FileFilter {
  public static final String rcsid = "$$Id$$";

  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(".mtf")) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "MTF File";
  }
}
