package etomo.util;

import java.io.File;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2011</p>
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
public final class FilePath {
  public static final String rcsid = "$Id:$";

  private String drive = null;
  private String[] pathArray = null;
  private String path = null;

  private static boolean debug = false;

  private FilePath(final String absolutePath) {
    path = absolutePath;
    if (path == null) {
      return;
    }
    path = absolutePath.trim();
    if (path.equals("")) {
      return;
    }
    pathArray = path.split("\\s*:\\s*");
    String tempPath = null;
    if (pathArray.length > 1) {
      drive = pathArray[0];
      tempPath = pathArray[1];
    }
    else {
      tempPath = path;
    }
    pathArray = tempPath.split("\\s*" + File.separator + "\\s*");
  }

  /**
   * Returns a relative path going from fromAbsolutePath to toAbsolutePath.  If this is
   * impossible, returns toAbsolutePath.
   * @param fromAbsoluteDirPath
   * @param toAbsolutePath
   * @return
   */
  public static String getRelativePath(final String fromAbsolutePath, final File toFile) {
    FilePath fromPath = new FilePath(fromAbsolutePath);
    if (toFile == null) {
      return null;
    }
    FilePath toPath = new FilePath(toFile.getAbsolutePath());
    return fromPath.getRelativePathTo(toPath);
  }

  /**
   * Returns a relative path going from this to toPath.  If this is impossible, returns
   * the path in toPath.
   * @param toPath
   * @return
   */
  private String getRelativePathTo(final FilePath toPath) {
    // Make sure that the drives are the same
    if (drive != null || toPath.drive != null) {
      if (drive == null || toPath.drive == null || !drive.equals(toPath.drive)) {
        // Paths are too disimilar - return absolute to-path
        return toPath.path;
      }
    }
    if (pathArray == null || pathArray.length == 0) {
      // No from-path - return absolute to-path
      return toPath.path;
    }
    // Make sure that path ends with a separator, and find out if it shares a common
    // starting path with to-path.
    String tempPath = (path + (path.endsWith(File.separator) ? "" : File.separator));
    if (toPath.path.startsWith(tempPath)) {
      // To-path is in the same directory path as path.
      return toPath.path.substring(tempPath.length(), toPath.path.length());
    }
    boolean same = true;
    int strip = -1;
    int goUp = 0;
    for (int i = 0; i < pathArray.length; i++) {
      if (same && pathArray[i].equals(toPath.pathArray[i])) {
        strip = i;
      }
      else {
        same = false;
        goUp++;
      }
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < goUp; i++) {
      buffer.append(".." + File.separator);
    }
    for (int i = strip + 1; i < toPath.pathArray.length; i++) {
      buffer.append(toPath.pathArray[i]);
      if (i < toPath.pathArray.length - 1) {
        buffer.append(File.separator);
      }
    }
    return buffer.toString();
  }

  static void setDebug(final boolean input) {
    debug = input;
  }
}
