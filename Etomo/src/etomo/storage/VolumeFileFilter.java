package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
public class VolumeFileFilter extends FileFilter implements java.io.FileFilter {
  public static final String rcsid = "$Id:$";

  /**
   * Accept the if it is a directory or has one of the defined extensions (.rec,
   * .mrc).
   */
  public boolean accept(File file) {
    if (!file.isFile()) {
      return true;
    }
    String name = file.getName();
    return name.endsWith(".rec") || name.endsWith(".mrc");
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Volume";
  }
}
