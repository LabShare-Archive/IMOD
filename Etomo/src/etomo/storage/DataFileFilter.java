package etomo.storage;

import java.io.File;

import etomo.EtomoDirector;
import etomo.util.DatasetFiles;

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
 * <p> Revision 1.3  2007/02/21 04:17:30  sueh
 * <p> bug# 964 Adding PEET and parallel processing data files.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:28:24  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.1  2004/10/15 00:14:38  sueh
 * <p> bug# 520 A file filter that selects etomo data files and join data files.
 * <p> </p>
 */
public class DataFileFilter extends javax.swing.filechooser.FileFilter
    implements java.io.FileFilter {
  public static final String rcsid = "$Id$";

  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile()
        && !f.getAbsolutePath().endsWith(".edf")
        && !f.getAbsolutePath().endsWith(".ejf")
        && !f.getAbsolutePath().endsWith(DatasetFiles.PARALLEL_DATA_FILE_EXT)
        && !(f.getAbsolutePath().endsWith(DatasetFiles.PEET_DATA_FILE_EXT) && EtomoDirector
            .getInstance().isNewstuff())) {//TEMP 964
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    //TEMP 964
    if (EtomoDirector.getInstance().isNewstuff()) {
      return "Etomo, Join, Parallel Process, or PEET data file";
    }
    return "Etomo, Join, or Parallel Process data file";
  }
}
