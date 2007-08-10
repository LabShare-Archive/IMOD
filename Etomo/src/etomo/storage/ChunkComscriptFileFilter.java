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
    if (!file.getName().endsWith("-001.com") && 
        !file.getName().endsWith("-001-sync.com")) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "First chunk scripts";
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/04/06 20:51:32  sueh
* <p> bug# 840 Only pick the first chunk .com file, since it always has to be
* <p> there.
* <p>
* <p> Revision 1.1  2006/03/20 17:52:58  sueh
* <p> bug# 835 A file filter for parallel processing .com scripts.
* <p> </p>
*/
