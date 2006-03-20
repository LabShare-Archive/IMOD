package etomo.storage;

import java.io.File;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class ChunkComscriptFileFilter extends javax.swing.filechooser.FileFilter implements java.io.FileFilter {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * returns true if a file is a chunk com file
   * File must be in the form:  {rootname}-*.com
   * Example: tilta-001.com
   */
  public boolean accept(File file) {
    if (file.isDirectory()) {
      return true;
    }
    String fileName = file.getName();
    int dashIndex = fileName.indexOf('-');
    if (dashIndex <= 0 || !fileName.endsWith(".com")) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return ".com file for processchunks";
  }
}
/**
* <p> $Log$ </p>
*/