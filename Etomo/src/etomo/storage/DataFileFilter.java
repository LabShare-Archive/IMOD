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
 * <p> Revision 1.8  2007/12/26 22:15:27  sueh
 * <p> bug# 1052 Moved ".edf" to DatasetFiles.
 * <p>
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
public class DataFileFilter extends javax.swing.filechooser.FileFilter implements
    java.io.FileFilter {
  public static final String rcsid = "$Id$";

  // turn filesOnly on for building a list of just files, off for a filechooser
  private final boolean filesOnly;

  public DataFileFilter() {
    filesOnly = false;
  }

  public DataFileFilter(final boolean filesOnly) {
    this.filesOnly = filesOnly;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#accept(File)
   */
  public boolean accept(File f) {
    if (filesOnly && !f.isFile()) {
      return false;
    }
    // If this is a file test its extension, all others should return true when filesOnly
    // is off
    if (f.isFile() && !f.getAbsolutePath().endsWith(DataFileType.RECON.extension)
        && !f.getAbsolutePath().endsWith(DataFileType.JOIN.extension)
        && !f.getAbsolutePath().endsWith(DataFileType.PARALLEL.extension)
        && !f.getAbsolutePath().endsWith(DataFileType.PEET.extension)
        && !f.getAbsolutePath().endsWith(DataFileType.SERIAL_SECTIONS.extension)) {
      return false;
    }
    return true;
  }

  /**
   * @see javax.swing.filechooser.FileFilter#getDescription()
   */
  public String getDescription() {
    return "Data file (" + DataFileType.RECON.extension + ", "
        + DataFileType.JOIN.extension + ", " + DataFileType.PARALLEL.extension + ", "
        + DataFileType.PEET.extension + ", " + DataFileType.SERIAL_SECTIONS.extension
        + ")";
  }
}
