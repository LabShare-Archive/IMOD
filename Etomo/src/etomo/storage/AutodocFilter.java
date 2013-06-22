package etomo.storage;

import java.io.File;

import etomo.storage.autodoc.AutodocFactory;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */

public class AutodocFilter extends javax.swing.filechooser.FileFilter implements
    java.io.FileFilter {
  public static final String rcsid = "$Id$";

  public boolean accept(File f) {
    if (!f.exists()) {
      System.err.println("Warning: " + f.getAbsolutePath() + " does not exist");
      return false;
    }
    if (f.isFile()) {
      return f.getName().endsWith(AutodocFactory.EXTENSION);
    }
    return true;
  }

  public String getDescription() {
    return "Autodoc file";
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2005/12/23 02:06:08  sueh
 * <p> bug# 675 Added a filter to find autodoc files.
 * <p> </p>
 */
