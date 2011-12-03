package etomo.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

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

  private static boolean debug = false;

  private String drive = null;
  private List<String> pathArray = null;
  private StringBuffer path = null;

  /**
   * Deep copy constructor.
   */
  private FilePath(final FilePath filePath) {
    drive = filePath.drive;
    if (filePath.pathArray != null) {
      pathArray = new ArrayList<String>(filePath.pathArray.size());
      for (int i = 0; i < filePath.pathArray.size(); i++) {
        pathArray.add(filePath.pathArray.get(i));
      }
    }
    path = filePath.path;
  }

  private FilePath(final String filePath) {
    if (filePath == null) {
      return;
    }
    path = new StringBuffer();
    path.append(filePath.trim());
    if (path.equals("")) {
      return;
    }
    String[] tempArray = path.toString().split(":");
    String tempPath = null;
    if (tempArray.length > 1) {
      drive = tempArray[0];
      tempPath = tempArray[1];
    }
    else {
      tempPath = path.toString();
    }
    tempArray = tempPath.split(File.separator);
    pathArray = new ArrayList(tempArray.length);
    for (int i = 0; i < tempArray.length; i++) {
      pathArray.add(tempArray[i]);
    }
  }

  private void removeLast() {
    if (pathArray == null || pathArray.size() == 0) {
      return;
    }
    int amountToRemove = pathArray.remove(pathArray.size() - 1).length();
    if (path.toString().endsWith(File.separator)) {
      amountToRemove += File.separator.length();
    }
    path.delete(path.length() - amountToRemove, path.length());
  }

  private void removeFirst() {
    if (pathArray == null || pathArray.size() == 0) {
      return;
    }
    int amountToRemove = pathArray.remove(0).length() + File.separator.length();
    if (path.toString().startsWith(File.separator)) {
      amountToRemove += File.separator.length();
    }
    path.delete(0, amountToRemove);
  }

  public static boolean isPath(final String name) {
    if (name == null || name.matches("\\s*")) {
      return false;
    }
    try {
      new File(name).getCanonicalFile();
    }
    catch (IOException e) {
      // Not a valid file name or path
      return false;
    }
    return name.indexOf(File.separator) != -1
        || (Utilities.isWindowsOS() && name.indexOf(":") > 0);
  }

  public static String getFileName(final String path) {
    if (isPath(path)) {
      return new File(path).getName();
    }
    return path;
  }

  /**
   * Replace the filename in the path
   * @param path
   * @param name
   * @return
   */
  public static String replaceName(String path, final String name) {
    if (!isPath(path)) {
      return name;
    }
    String retval = new File(new File(path).getParent(), name).getPath();
    return retval;
  }

  /**
   * Returns a relative path going from fromAbsolutePath to toAbsoluteFile.
   * @param fromAbsoluteDirPath
   * @param toAbsoluteFile
   * @return
   */
  public static String getRelativePath(final String fromAbsolutePath,
      final File toAbsoluteFile) {
    if (fromAbsolutePath == null || fromAbsolutePath.matches("\\s*")) {
      return toAbsoluteFile.getAbsolutePath();
    }
    FilePath fromPath = new FilePath(fromAbsolutePath);
    if (toAbsoluteFile == null) {
      return fromAbsolutePath;
    }
    FilePath toPath = new FilePath(toAbsoluteFile.getAbsolutePath());
    return fromPath.getRelativePathTo(toPath);
  }

  /**
   * Gets a File instance of filePath, if filePath is an absolute path or dir is empty.
   * Otherwise gets a File instance of dir/filePath.
   * @param filePath
   * @return
   */
  public static File getFileFromPath(final String dir, final String filePath) {
    File file = new File(filePath);
    if (file.isAbsolute() || dir == null || dir.matches("\\s*")) {
      return file;
    }
    return new File(dir, filePath);
  }

  /**
   * Make a new relative path from the new root to a file whose relative path is relative
   * to the old root.  If the file's path is absolute, or if newRoot is missing, return
   * filePath as is.
   * @param oldRoot
   * @param newRoot
   * @param filePath
   * @return
   */
  public static String getRerootedRelativePath(final String oldRoot,
      final String newRoot, final String filePath) {
    if ((new File(filePath)).isAbsolute() || newRoot == null || newRoot.matches("\\s*")) {
      return filePath;
    }
    return getRelativePath(newRoot, getAbsoluteFile(oldRoot, filePath));
  }

  static void setDebug(final boolean input) {
    debug = input;
  }

  /**
   * Returns a file with an absolute path going from fromAbsolutePath to filePath.
   * @param fromAbsoluteDirPath - assumes that this is absolute
   * @param filePath
   * @return
   */
  private static File getAbsoluteFile(final String fromAbsolutePath, final String filePath) {
    if (filePath == null || filePath.matches("\\s*")) {
      return new File(fromAbsolutePath);
    }
    FilePath fromPath = new FilePath(fromAbsolutePath);
    FilePath toPath = new FilePath(filePath);
    String output = fromPath.getAbsolutePathTo(toPath);
    return new File(output);
  }

  /**
   * Returns an absolute path going from this to toRelPath.  If this is impossible, returns
   * the absolute path in toRelPath.
   * @param toPath
   * @return
   */
  private String getAbsolutePathTo(final FilePath toRelPath) {
    // Make sure that the drives are the same
    if (drive != null || toRelPath.drive != null) {
      if (drive == null || toRelPath.drive == null || !drive.equals(toRelPath.drive)) {
        // Paths are too disimilar - return absolute to-path
        return new File(toRelPath.path.toString()).getAbsolutePath();
      }
    }
    if (pathArray == null || pathArray.size() == 0) {
      // No from-path - return absolute to-path
      return new File(toRelPath.path.toString()).getAbsolutePath();
    }
    // If the to path doesn't contain ..'s then use path/toRelPath.path.
    if (toRelPath.path.indexOf("..") == -1) {
      return new File(path.toString(), toRelPath.path.toString()).getAbsolutePath();
    }
    // Go up the from path by removing ..'s at the beginning of the to path.
    FilePath tempFromPath = new FilePath(this);
    FilePath tempToPath = new FilePath(toRelPath);
    int i = 0;
    for (i = 0; i < toRelPath.pathArray.size(); i++) {
      if (toRelPath.pathArray.get(i).equals("..")) {
        tempFromPath.removeLast();
        tempToPath.removeFirst();
      }
      else {
        break;
      }
    }
    return new File(tempFromPath.path.toString(), tempToPath.path.toString())
        .getAbsolutePath();
  }

  /**
   * Returns a relative path going from this to toAbsPath.  If this is impossible, returns
   * the absolute path in toPath.
   * @param toAbsPath
   * @return
   */
  private String getRelativePathTo(final FilePath toAbsPath) {
    // Make sure that the drives are the same
    if (drive != null || toAbsPath.drive != null) {
      if (drive == null || toAbsPath.drive == null || !drive.equals(toAbsPath.drive)) {
        // Paths are too disimilar - return absolute to-path
        return toAbsPath.path.toString();
      }
    }
    if (pathArray == null || pathArray.size() == 0) {
      // No from-path - return absolute to-path
      return toAbsPath.path.toString();
    }
    // Make sure that path ends with a separator, and find out if it shares a common
    // starting path with to-path.
    String tempPath = (path + (path.toString().endsWith(File.separator) ? ""
        : File.separator));
    if (toAbsPath.path.toString().startsWith(tempPath)) {
      // To-path is in the same directory path as path.
      return toAbsPath.path.substring(tempPath.length(), toAbsPath.path.length());
    }
    boolean same = true;
    int strip = -1;
    int goUp = 0;
    for (int i = 0; i < pathArray.size(); i++) {
      if (same && pathArray.get(i).equals(toAbsPath.pathArray.get(i))) {
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
    for (int i = strip + 1; i < toAbsPath.pathArray.size(); i++) {
      buffer.append(toAbsPath.pathArray.get(i));
      if (i < toAbsPath.pathArray.size() - 1) {
        buffer.append(File.separator);
      }
    }
    return buffer.toString();
  }
}
