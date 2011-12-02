package etomo.util;

import java.io.File;

import junit.framework.TestCase;

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
public class FilePathTest extends TestCase {
  public static final String rcsid = "$Id:$";

  public void testGetPath() {
    // Absolute paths
    String xyzPathWithADrive = "A:" + File.separator + "x" + File.separator + "y"
        + File.separator + "z";
    String xyzPathWithBDrive = "B:" + File.separator + "x" + File.separator + "y"
        + File.separator + "z";
    String xyzPath = File.separator + "x" + File.separator + "y" + File.separator + "z";
    String xyPath = File.separator + "x" + File.separator + "y";
    String xPath = File.separator + "x";
    String abcPath = File.separator + "a" + File.separator + "b" + File.separator + "c";
    String aPath = File.separator + "a";
    // Relative paths
    String relZPath = "z";
    String relUp1 = "..";
    String relUp3 = ".." + File.separator + ".." + File.separator + "..";
    String drive = "";
    if (Utilities.isWindowsOS()) {
      drive = "C:";
      assertTrue(
          "Should return the absolute to-path when the drives are different",
          FilePath.getRelativePath(xyzPath, new File(xyzPathWithADrive)).equals(
              xyzPathWithADrive));
      assertTrue(
          "Should return the absolute to-path when the drives are different",
          FilePath.getRelativePath(xyzPathWithADrive, new File(xyzPath)).equals(
              new File(xyzPath).getAbsolutePath()));
      assertTrue("Should return the absolute to-path when the drives are different",
          FilePath.getRelativePath(xyzPathWithADrive, new File(xyzPathWithBDrive))
              .equals(xyzPathWithBDrive));
    }
    assertTrue(
        "Should return the absolute to-path when the from-path is empty",
        FilePath.getRelativePath(null, new File(xyzPath)).equals(
            new File(xyzPath).getAbsolutePath()));
    assertTrue(
        "Should return the absolute to-path when the from-path is empty",
        FilePath.getRelativePath("", new File(xyzPath)).equals(
            new File(xyzPath).getAbsolutePath()));
    assertTrue(
        "Should return the absolute to-path when the from-path is empty",
        FilePath.getRelativePath(" ", new File(xyzPath)).equals(
            new File(xyzPath).getAbsolutePath()));
    assertTrue(
        "Should remove the from-path when it is a subset of the to-path, and return a relative path",
        FilePath.getRelativePath(drive + xyPath, new File(drive + xyzPath)).equals(
            relZPath));
    assertTrue(
        "Should go up the from-path where it is not equal",
        FilePath.getRelativePath(drive + xyzPath, new File(drive + abcPath)).equals(
            relUp3 + abcPath));
    assertTrue(
        "Should strip parts from the start of the path that are the same and then go up the remaining from-path where it is not equal",
        FilePath.getRelativePath(drive + xyzPath, new File(drive + xyPath + abcPath))
            .equals(relUp1 + abcPath));
    assertTrue(
        "Should strip parts from only the start of the path that are the same and then go up the remaining from-path where it is not equal",
        FilePath.getRelativePath(drive + xyzPath, new File(drive + abcPath + xyzPath))
            .equals(relUp3 + abcPath + xyzPath));
    assertTrue("Should go up the from-path where it is not equal", FilePath
        .getRelativePath(drive + xPath, new File(drive + aPath)).equals(relUp1 + aPath));
  }
}
