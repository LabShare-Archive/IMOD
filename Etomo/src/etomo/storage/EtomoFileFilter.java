package etomo.storage;

import java.io.File;

import etomo.type.DataFileType;

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
 * <p> Revision 3.3  2007/12/26 22:15:39  sueh
 * <p> bug# 1052 Moved ".edf" to DatasetFiles.
 * <p>
 * <p> Revision 3.2  2005/02/16 22:32:06  sueh
 * <p> bug# 604 Prevent EtomoFileFilter from accepting directories.
 * <p>
 * <p> Revision 3.1  2004/11/19 23:28:43  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.0.6.1  2004/10/15 00:15:23  sueh
 * <p> bug# 520 Change class to inherit DataFileFilter so it can be used
 * <p> generically.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/11/03 19:32:10  sueh
 * <p> bug266 MainFrame:
 * <p> getTestParamFilename(): added datasetname.edf as the default when there are no .edf files.
 * <p>
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
public class EtomoFileFilter extends DataFileFilter {
  public static final String rcsid = "$Id:";

  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isDirectory()
        || (f.isFile() && !f.getAbsolutePath().endsWith(DataFileType.RECON.extension))) {
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
