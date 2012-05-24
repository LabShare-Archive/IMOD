package etomo.storage;

import java.io.File;

/**
* <p>Description: Holds a File instance that exists.  Does a one-time search for the file
* in a list of paths.</p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class FileLocation {
  public static final String rcsid = "$Id:$";

  public static final FileLocation LSOF = new FileLocation("lsof", new String[] {
      "/usr/bin", "/usr/sbin" });

  private final String fileName;
  final String[] pathArray;

  private boolean searched = false;
  private File file = null;

  private FileLocation(final String fileName, final String[] pathArray) {
    this.fileName = fileName;
    this.pathArray = pathArray;
  }

  /**
   * @return true if the file exists
   */
  public boolean exists() {
    if (!searched) {
      searchForFile();
    }
    return file != null;
  }

  /**
   * @return the file's absolute path, or null if it doesn't exist
   */
  public String getAbsolutePath() {
    if (!searched) {
      searchForFile();
    }
    if (file == null) {
      return null;
    }
    return file.getAbsolutePath();
  }

  /**
   * Search one time for the file in the paths in pathArray.  If the file isn't found, set
   * the file member variable to null.  Changes searched to true.
   */
  private void searchForFile() {
    if (searched) {
      return;
    }
    searched = true;
    if (pathArray == null) {
      return;
    }
    for (int i = 0; i < pathArray.length; i++) {
      file = new File(pathArray[i], fileName);
      if (file.exists()) {
        return;
      }
    }
    file = null;
  }
}
