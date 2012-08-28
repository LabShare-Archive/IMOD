package etomo.storage;

import java.io.File;

import etomo.type.DataFileType;

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
* <p> Revision 1.2  2004/11/19 23:29:04  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.1  2004/10/15 00:15:54  sueh
* <p> bug# 520 File filter that recognizes join data files.
* <p> </p>
*/
public class JoinFileFilter extends DataFileFilter {
  public static  final String  rcsid =  "$Id$";
  
  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(DataFileType.JOIN.extension)) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Join data file";
  }
}
