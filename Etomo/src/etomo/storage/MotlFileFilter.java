package etomo.storage;

import java.io.File;

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
* <p> Revision 1.1  2007/03/01 01:14:47  sueh
* <p> bug# 964 File filter for MOTL files (PEET interface).
* <p> </p>
*/
public final class MotlFileFilter extends javax.swing.filechooser.FileFilter
implements java.io.FileFilter {
  public static  final String  rcsid =  "$Id$";
  
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(".em")) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "MOTL file (.em)";
  }
}
