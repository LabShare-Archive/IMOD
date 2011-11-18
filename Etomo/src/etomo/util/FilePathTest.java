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
    /*assertTrue("Should return the to-path when the drives are different", FilePath
     * .getRelativePath(xyzPath, new File(xyzPathWithADrive)) .equals(xyzPathWithADrive));
     * assertTrue("Should return the to-path when the drives are different", FilePath
     * .getRelativePath(xyzPathWithADrive, new File(xyzPath)).equals(xyzPath));
     * assertTrue("Should return the to-path when the drives are different",
     * FilePath.getRelativePath(xyzPathWithADrive, new File(xyzPathWithBDrive))
     * .equals(xyzPathWithBDrive)); */
    if (!Utilities.isWindowsOS()) {
      assertTrue("Should return the to-path when the from-path is empty", FilePath
          .getRelativePath(null, new File(xyzPath)).equals(xyzPath));
      assertTrue("Should return the to-path when the from-path is empty", FilePath
          .getRelativePath("", new File(xyzPath)).equals(xyzPath));
      assertTrue("Should return the to-path when the from-path is empty", FilePath
          .getRelativePath(" ", new File(xyzPath)).equals(xyzPath));
      assertTrue(
          "Should remove the from-path when it is a subset of the to-path, and return a relative path",
          FilePath.getRelativePath(xyPath, new File(xyzPath)).equals(relZPath));
      FilePath.setDebug(true);
      assertTrue("Should go up the from-path where it is not equal", FilePath
          .getRelativePath(xyzPath, new File(abcPath)).equals(relUp3 + abcPath));
      FilePath.setDebug(false);
      assertTrue(
          "Should strip parts from the start of the path that are the same and then go up the remaining from-path where it is not equal",
          FilePath.getRelativePath(xyzPath, new File(xyPath + abcPath)).equals(
              relUp1 + abcPath));
      assertTrue(
          "Should strip parts from only the start of the path that are the same and then go up the remaining from-path where it is not equal",
          FilePath.getRelativePath(xyzPath, new File(abcPath + xyzPath)).equals(
              relUp3 + abcPath + xyzPath));
      assertTrue("Should go up the from-path where it is not equal", FilePath
          .getRelativePath(xPath, new File(aPath)).equals(relUp1 + aPath));
    }
  }
}
