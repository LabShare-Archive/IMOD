package etomo.storage;

import java.io.File;

import etomo.type.DataFileType;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.4  2007/06/04 23:07:11  sueh
 * <p> bug# 1005 Substituted DatasetFiles.PEET_DATA_FILE_EXT for ".epe".
 * <p>
 * <p> Revision 1.3  2007/05/16 23:47:41  sueh
 * <p> bug# 964 Added boolean acceptDirectories.
 * <p>
 * <p> Revision 1.2  2007/02/21 04:18:35  sueh
 * <p> bug# 964 Extending DataFileFilter, so it can be used in Interface independent code.
 * <p>
 * <p> Revision 1.1  2007/02/19 21:53:53  sueh
 * <p> bug# 964 File filter for PEET interface files (.epe).
 * <p> </p>
 */
public class PeetFileFilter extends DataFileFilter {
  public static final String rcsid = "$Id$";

  private final boolean acceptDirectories;

  public PeetFileFilter() {
    acceptDirectories = true;
  }

  public PeetFileFilter(boolean acceptDirectories) {
    this.acceptDirectories = acceptDirectories;
  }

  /**
   * returns true if a file is a peet process data file
   * File must be in the form:  {rootname}.epe
   * Example: particles.epe
   */
  public boolean accept(File file) {
    if (file.isDirectory()) {
      return acceptDirectories;
    }
    String fileName = file.getName();
    if (fileName.endsWith(DataFileType.PEET.extension) && fileName.length() > 4) {
      return true;
    }
    return false;
  }

  public String getDescription() {
    return "PEET data file (" + DataFileType.PEET.extension + ")";
  }
}
