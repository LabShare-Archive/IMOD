package etomo.storage;

import java.io.File;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.1.2.1  2004/10/15 00:14:38  sueh
* <p> bug# 520 A file filter that selects etomo data files and join data files.
* <p> </p>
*/
public class DataFileFilter extends javax.swing.filechooser.FileFilter implements java.io.FileFilter {
  public static  final String  rcsid =  "$Id$";
  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(".edf") && !f.getAbsolutePath().endsWith(".ejf")) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Etomo data file or Join data file";
  }
}

