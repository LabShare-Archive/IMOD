package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;
/**
* <p>Description: File filter for mag gradients correction files (.mgt).</p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class MagGradientFileFilter extends FileFilter {
  public static  final String  rcsid =  "$Id$";
  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(".mgt")) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Mag Gradients Correction File";
  }
}
/**
* <p> $Log$ </p>
*/
