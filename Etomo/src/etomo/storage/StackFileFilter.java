package etomo.storage;

import java.io.File;

import javax.swing.filechooser.FileFilter;

import etomo.logic.DatasetTool;

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
 * <p> Revision 1.2.2.1  2003/01/24 18:38:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.2  2002/12/10 23:47:37  rickg
 * <p> Added rcsid
 * <p>
 * <p> Revision 1.1  2002/12/09 04:14:23  rickg
 * <p> Initial revision
 * <p>
 */
public class StackFileFilter extends FileFilter implements java.io.FileFilter {
  public static final String rcsid = "$Id:";

  public StackFileFilter() {
  }

  public boolean accept(File f) {
    // If this is a file test its extension, all others should return true
    if (!f.isFile()) {
      return true;
    }
    String name = f.getName();
    if (!name.endsWith(DatasetTool.STANDARD_DATASET_EXT)
        && !name.endsWith(DatasetTool.ALTERNATE_DATASET_EXT)) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "MRC Image Stack (.mrc files will be renamed to .st)";
  }

}
