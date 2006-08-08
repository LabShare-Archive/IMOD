package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

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
 * <p> Revision 1.3  2004/12/04 01:26:53  sueh
 * <p> bug# 557 Added.sqz files to the filter.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:29:54  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/28 22:14:02  sueh
 * <p> bug# 520 Allow .flip files.
 * <p>
 * <p> Revision 1.1.2.1  2004/09/21 17:57:00  sueh
 * <p> bug# 520 A file filter for .rec files
 * <p> </p>
 */
public class TomogramFileFilter extends FileFilter implements
    java.io.FileFilter {
  public static final String rcsid = "$Id$";

  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    String filePath = f.getAbsolutePath();
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !filePath.endsWith(".rec") && !filePath.endsWith(".flip")
        && !filePath.endsWith(".sqz") && !filePath.endsWith(".join")) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Tomogram file";
  }
}
