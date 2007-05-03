package etomo.storage;

import java.io.File;
import java.io.FileFilter;

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
 * <p> $Log$ </p>
 */

public final class ComFileFilter implements FileFilter {
  public static final String rcsid = "$Id$";

  private final String rootName;

  public ComFileFilter(String rootName) {
    this.rootName = rootName;
  }

  /**
   * returns true if the file is a file and ends in .com
   * This is for automatically deleting files, not looking at a file dialog, so
   * directories should not be included 
   */
  public boolean accept(File file) {
    if (file.isDirectory()) {
      return false;
    }
    String name = file.getName();
    if (!name.startsWith(rootName + "-") || !name.endsWith(".com")) {
      return false;
    }
    return true;
  }

  public String getDescription() {
    return "Comscripts (.com)";
  }
}
