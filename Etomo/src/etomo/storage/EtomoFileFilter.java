package etomo.storage;

import java.io.File;


/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.2  2002/12/10 23:47:31  rickg
 * <p> Added rcsid
 * <p>
 * <p> Revision 1.1  2002/12/09 04:14:08  rickg
 * <p> Initial revision
 * <p>
 */
public class EtomoFileFilter extends javax.swing.filechooser.FileFilter implements java.io.FileFilter {
  public static final String rcsid = "$Id:";

  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(".edf")) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Etomo data file";
  }

}
