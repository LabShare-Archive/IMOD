package etomo.storage;

import java.io.File;

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
 * <p> Revision 1.7  2007/08/07 00:46:46  sueh
 * <p> bug# 996 Changed getDescription
 * <p>
 * <p> Revision 1.6  2007/07/30 22:39:04  sueh
 * <p> bug# 963 Added DatasetFiles.JOIN_DATA_FILE_EXT.
 * <p>
 * <p> Revision 1.5  2007/05/02 16:34:13  sueh
 * <p> bug# 964 Moved newstuff into mainstream.
 * <p>
 * <p> Revision 1.4  2007/03/03 00:44:44  sueh
 * <p> bug# 964 Removed a mention of PEET that escaped the newstuff setting.
 * <p>
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
    if (f.isFile() && !f.getAbsolutePath().endsWith(DatasetFiles.RECON_DATA_FILE_EXT)
        && !f.getAbsolutePath().endsWith(DatasetFiles.JOIN_DATA_FILE_EXT)
        && !f.getAbsolutePath().endsWith(DatasetFiles.PARALLEL_DATA_FILE_EXT)
        && !f.getAbsolutePath().endsWith(DatasetFiles.PEET_DATA_FILE_EXT)) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Data file (" + ".edf, " + DatasetFiles.JOIN_DATA_FILE_EXT + ", "
        + DatasetFiles.PARALLEL_DATA_FILE_EXT + ", "
        + DatasetFiles.PEET_DATA_FILE_EXT + ")";
  }
}
