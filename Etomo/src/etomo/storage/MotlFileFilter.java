package etomo.storage;

import java.io.File;

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
 * <p> Revision 1.5  2008/02/27 00:19:15  sueh
 * <p> bug# 1089 Only accept .csv files.  PEET no longer supports .em files.
 * <p>
 * <p> Revision 1.4  2008/02/18 23:35:30  sueh
 * <p> bug# 1079 Added .EM and .csv file extensions.
 * <p>
 * <p> Revision 1.3  2008/02/18 21:36:36  sueh
 * <p> bug# 1079 Accepting .csv files.
 * <p>
 * <p> Revision 1.2  2007/06/05 18:10:04  sueh
 * <p> bug# 1011 Accepting any file that ends in .em.
 * <p>
 * <p> Revision 1.1  2007/03/01 01:14:47  sueh
 * <p> bug# 964 File filter for MOTL files (PEET interface).
 * <p> </p>
 */
public final class MotlFileFilter extends javax.swing.filechooser.FileFilter implements
    java.io.FileFilter {
  public static final String rcsid = "$Id$";

  public boolean accept(File f) {
    //  If this is a file test its extension, all others should return true
    if (f.isFile() && !f.getAbsolutePath().endsWith(".csv")) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "MOTL file (.em, .csv)";
  }
}
