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

  private final String empty = "    ";

  // Relative paths
  private final String relZPath = "z";
  private final String relUp1 = "..";
  private final String relUp3 = ".." + File.separator + ".." + File.separator + "..";

  // Absolute paths
  // Windows
  private final String xyzPathWithADrive = "A:" + File.separator + "x" + File.separator
      + "y" + File.separator + relZPath;
  private final String xyzPathWithBDrive = "B:" + File.separator + "x" + File.separator
      + "y" + File.separator + relZPath;
  // Linux
  private final String xyzPath = File.separator + "x" + File.separator + "y"
      + File.separator + relZPath;
  private final String xyPath = File.separator + "x" + File.separator + "y";
  private final String xPath = File.separator + "x";
  private final String abcPath = File.separator + "a" + File.separator + "b"
      + File.separator + "c";
  private final String aPath = File.separator + "a";

  private final String drive;

  public FilePathTest() {
    if (Utilities.isWindowsOS()) {
      drive = "C:";
    }
    else {
      drive = "";
    }
  }

  public void testIsPath() {
    assertFalse("Null name should return false", FilePath.isPath(null));
    assertFalse("Empty name should return false", FilePath.isPath(""));
    assertFalse("Empty name should return false", FilePath.isPath(empty));
    // Unable to get getCanonicalFile to throw an exception.
    assertFalse("Path with no separator and no drive should return false",
        FilePath.isPath(relZPath));
    assertTrue("Path with a separator and no drive should return true",
        FilePath.isPath(relUp3));
    if (Utilities.isWindowsOS()) {
      assertTrue("Path with a drive and no separator should return true on Windows",
          FilePath.isPath(drive + relZPath));
      assertFalse("Drive separator in the wrong place returns false",
          FilePath.isPath(":" + relZPath));
    }
    assertTrue("Path with separators and a drive should return true",
        FilePath.isPath(xyzPathWithADrive));
  }

  public void testGetFileName() {
    assertNull("Null name should return null", FilePath.getFileName(null));
    assertEquals("Empty name should return empty", FilePath.getFileName(""), "");
    assertEquals("Empty name should return empty", FilePath.getFileName(empty), empty);
    // Unable to get getCanonicalFile to throw an exception.
    assertEquals("Path with no separator and no drive should return passed in parameter",
        FilePath.getFileName(relZPath), relZPath);
    assertEquals("Path with a separator and no drive should return name",
        FilePath.getFileName(relUp3 + File.separator + "c"), "c");
    if (Utilities.isWindowsOS()) {
      assertEquals("Path with a drive and no separator should return name on Windows",
          FilePath.getFileName(drive + relZPath), relZPath);
      assertEquals("Drive separator in the wrong place returns passed in parameter",
          FilePath.getFileName(":" + relZPath), ":" + relZPath);
    }
    assertEquals("Path with separators and a drive should return name",
        FilePath.getFileName(xyzPathWithADrive), relZPath);
  }

  public void testReplaceName() {
    String replace = "r";
    assertEquals("Null name should return passed in name",
        FilePath.replaceName(null, replace), replace);
    assertEquals("Empty name should return passed in name",
        FilePath.replaceName("", replace), replace);
    assertEquals("Empty name should return passed in name",
        FilePath.replaceName(empty, replace), replace);
    // Unable to get getCanonicalFile to throw an exception.
    assertEquals("Path with no separator and no drive should return passed in name",
        FilePath.replaceName(relZPath, replace), replace);
    assertEquals(
        "Path with a separator and no drive should return path with replaced name",
        FilePath.replaceName(relUp3 + File.separator + "c", replace), relUp3
            + File.separator + replace);
    if (Utilities.isWindowsOS()) {
      assertEquals(
          "Path with a drive and no separator should return path with replaced name on Windows",
          FilePath.replaceName(drive + relZPath, replace), drive + replace);
      assertEquals("Drive separator in the wrong place returns passed in name",
          FilePath.replaceName(":" + relZPath, replace), replace);
    }
    assertEquals(
        "Path with separators and a drive should return path with replaced name",
        FilePath.replaceName(xyzPathWithADrive, replace), "A:" + File.separator + "x"
            + File.separator + "y" + File.separator + replace);
  }

  public void testGetRelativePath() {
    if (Utilities.isWindowsOS()) {
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

  public void testBuildAbsoluteFile() {
    assertEquals("'.' should be returned as the absolute path to the current directory",
        new File("").getAbsolutePath(),
        FilePath.buildAbsoluteFile(new File("").getAbsolutePath(), ".").getPath());
    assertEquals("'.' should be returned as the absolute path to the current directory",
        new File("").getAbsolutePath(),
        FilePath.buildAbsoluteFile(new File("").getAbsolutePath(), new File("."))
            .getPath());
    if (!Utilities.isWindowsOS()) {
      assertEquals("Absolute filePath is returned",
          FilePath.buildAbsoluteFile(xPath, xyPath).getPath(), xyPath);
    }
    else {
      assertEquals("Absolute filePath is returned",
          FilePath.buildAbsoluteFile(xyzPathWithADrive, xyzPathWithBDrive).getPath(),
          xyzPathWithBDrive);
    }
    assertEquals("Null dir causes filePath to be returns",
        FilePath.buildAbsoluteFile(null, relZPath).getPath(), relZPath);
    assertEquals("Empty dir causes filePath to be returns",
        FilePath.buildAbsoluteFile("", relZPath).getPath(), relZPath);
    assertEquals("Empty dir causes filePath to be returns",
        FilePath.buildAbsoluteFile(empty, relZPath).getPath(), relZPath);
    if (!Utilities.isWindowsOS()) {
      assertEquals(
          "Relative filePath causes a file created from dir and filePath to be returned",
          FilePath.buildAbsoluteFile(xPath, relZPath).getPath(), xPath + File.separator
              + relZPath);
    }
    else {
      assertEquals(
          "Relative filePath causes a file created from dir and filePath to be returned",
          FilePath.buildAbsoluteFile(xyzPathWithADrive, relZPath).getPath(),
          xyzPathWithADrive + File.separator + relZPath);
    }
    assertEquals(
        "Relative filePath causes a file created from a relative dir and filePath to be returned",
        FilePath.buildAbsoluteFile(xyzPath, relZPath).getPath(), xyzPath + File.separator
            + relZPath);
  }

  public void testGetRerootedRelativePath() {
    if (!Utilities.isWindowsOS()) {
      assertEquals("Absolute filePath is not rerooted",
          FilePath.getRerootedRelativePath(xyzPath, abcPath, xyPath), xyPath);
    }
    else {
      assertEquals(
          "Absolute filePath is not rerooted",
          FilePath.getRerootedRelativePath(xyzPathWithADrive, xyzPathWithBDrive, drive
              + xyzPath), drive + xyzPath);
    }
    assertEquals("Null newRoot means that filePath is not rerooted",
        FilePath.getRerootedRelativePath(xyzPath, null, relZPath), relZPath);
    assertEquals("Empty newRoot means that filePath is not rerooted",
        FilePath.getRerootedRelativePath(xyzPath, "", relZPath), relZPath);
    assertEquals("Empty newRoot means that filePath is not rerooted",
        FilePath.getRerootedRelativePath(xyzPath, empty, relZPath), relZPath);
    assertEquals(
        "Relative filePath is rerooted with a relative path to newRoot (same drive in windows)",
        FilePath.getRerootedRelativePath(drive + xyzPath, drive + abcPath, relZPath),
        relUp3 + xyzPath + File.separator + relZPath);
    assertEquals(
        "..'s should be removed before the elative filePath is rerooted",
        FilePath.getRerootedRelativePath(drive + xyzPath, drive + abcPath, relUp3
            + abcPath + File.separator + relZPath), relZPath);
  }
}
