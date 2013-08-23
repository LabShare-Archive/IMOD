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
 * <p> $$Log$
 * <p> $Revision 1.1  2004/02/20 23:43:25  sueh
 * <p> $bug# 386 looks for .idf files
 * <p> $$ </p>
 */
public class DistortionFileFilter extends FileFilter {
  public static final String rcsid = "$$Id$$";

  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(".idf")) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Image Distortion Field File";
  }

}
