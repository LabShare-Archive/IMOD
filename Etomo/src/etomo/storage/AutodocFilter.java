package etomo.storage;

import java.io.File;

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

  private String autodocType = null;

  public boolean accept(File f) {
    if (!f.isFile()) {
      return false;
    }
    String name = f.getName();
    if (name.endsWith(".adoc")) {
      if (autodocType == null) {
        return true;
      }
      else {
        if (name.startsWith(autodocType)) {
          return true;
        }
      }
    }
    return false;
  }

  public void setAutodocType(String autodocType) {
    this.autodocType = autodocType;
  }

  public String getDescription() {
    return "Autodoc file";
  }
}
/**
 * <p> $Log$ </p>
 */