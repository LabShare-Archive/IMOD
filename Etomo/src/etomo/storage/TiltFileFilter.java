package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;
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
* <p> $Log$ </p>
*/
public class TiltFileFilter extends FileFilter {
  public static  final String  rcsid =  "$Id$";
  
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(".tlt")) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "Tilt Angles File";
  }
}
