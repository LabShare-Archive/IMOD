package etomo.storage;

import java.io.File;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class ChunkComscriptFileFilter extends javax.swing.filechooser.FileFilter
    implements java.io.FileFilter {
  public static final String rcsid = "$Id$";

  /**
   * returns true if a file is a chunk com file
   * File must be in the form:  {rootname}-*.com
   * Example: tilta-001.com
   */
  public boolean accept(File file) {
    if (file.isDirectory()) {
      return true;
    }
    //only match -001.com or -001-sync.com.  At least one 0 is required.  Any
    //number of 0s is valid.
    if (!file.getName().matches("\\S+-0+1.com")
        && !file.getName().matches("\\S+-0+1-sync.com")) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "First chunk scripts";
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.4  2009/02/26 19:55:49  sueh
 * <p> bug# 1185 Allowing a larger string of zeros in the file name.
 * <p>
 * <p> Revision 1.3  2007/08/10 16:56:01  mast
 * <p> The first chunk may be a sync file, so filter for -001-sync.com
 * <p>
 * <p> Revision 1.2  2006/04/06 20:51:32  sueh
 * <p> bug# 840 Only pick the first chunk .com file, since it always has to be
 * <p> there.
 * <p>
 * <p> Revision 1.1  2006/03/20 17:52:58  sueh
 * <p> bug# 835 A file filter for parallel processing .com scripts.
 * <p> </p>
 */
