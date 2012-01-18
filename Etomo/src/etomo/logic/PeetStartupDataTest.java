package etomo.logic;

import java.io.File;

import etomo.process.BaseProcessManager;

import junit.framework.TestCase;

/**
* <p>Description: </p>
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
public class PeetStartupDataTest extends TestCase {
  public static final String rcsid = "$Id:$";

  private static final String TEST_DIR_NAME = "PeetStartupData";
  private static final String BASE_NAME = "test";
  private static final String EXTENSION = ".epe";

  private final File testDir = new File(LogicTests.TEST_ROOT_DIR, TEST_DIR_NAME);

  public PeetStartupDataTest(String name) {
    super(name);
  }

  public void setUp() throws Exception {
    super.setUp();
    testDir.mkdirs();
  }

  protected void tearDown() throws Exception {
    super.tearDown();
  }

  public void testDirectory() {
    PeetStartupData testInstance = new PeetStartupData();
    assertNull("Null is ignored", testInstance.getDirectory());

    testInstance.setDirectory(null);
    assertNull("Null is ignored", testInstance.getDirectory());

    File curDir = new File("");
    testInstance.setDirectory(curDir.getName());
    assertEquals("relative paths are made absolute", testInstance.getDirectory(),
        curDir.getAbsolutePath());

    testInstance.setDirectory(".");
    assertEquals("'.' is turned into a normal absolute path",
        testInstance.getDirectory(), curDir.getAbsolutePath());

    testInstance.setDirectory(curDir.getAbsolutePath());
    assertEquals("absolute paths stay the same", testInstance.getDirectory(),
        curDir.getAbsolutePath());
  }

  public void testCopyFrom() {
    PeetStartupData testInstance = new PeetStartupData();
    assertNull("Null is ignored", testInstance.getCopyFrom());
    assertFalse("Null is ignored", testInstance.isCopyFrom());

    testInstance.setCopyFrom(null);
    assertNull("Null is ignored", testInstance.getCopyFrom());
    assertFalse("Null is ignored", testInstance.isCopyFrom());

    File curDir = new File("");
    testInstance.setCopyFrom(curDir.getName());
    assertEquals("relative paths are made absolute", testInstance.getCopyFrom(),
        curDir.getAbsolutePath());
    assertTrue("true if copy from set", testInstance.isCopyFrom());

    testInstance.setCopyFrom(".");
    assertEquals("'.' is turned into a normal absolute path", testInstance.getCopyFrom(),
        curDir.getAbsolutePath());

    testInstance.setCopyFrom(curDir.getAbsolutePath());
    assertEquals("absolute paths stay the same", testInstance.getCopyFrom(),
        curDir.getAbsolutePath());
  }

  public void testValidate() {
    PeetStartupData testInstance = new PeetStartupData();
    File epeFile = new File(testDir, BASE_NAME + EXTENSION);
    assertNotNull("missing directory is invalid", testInstance.validate());

    testInstance.setDirectory(testDir.getPath());
    assertNotNull("missing base name is invalid", testInstance.validate());

    BaseProcessManager.touch(epeFile.getAbsolutePath(), null);
    testInstance.setBaseName(BASE_NAME + "1");
    assertNotNull("only one .epe file allowed in a directory", testInstance.validate());

    testInstance.setBaseName(BASE_NAME);
    assertNull("The .epe file in a directory may be recreated", testInstance.validate());

    testInstance.setCopyFrom(epeFile.getAbsolutePath());
    assertNotNull("copy from file must be in a different directory",
        testInstance.validate());
  }

  public void testGetParamFile() {
    PeetStartupData testInstance = new PeetStartupData();
    assertNull("return null if directory or base name is null",
        testInstance.getParamFile());

    testInstance.setDirectory(testDir.getAbsolutePath());
    assertNull("return null if directory is null", testInstance.getParamFile());

    PeetStartupData testInstance1 = new PeetStartupData();
    testInstance1.setBaseName(BASE_NAME);
    assertNull("return null if base name is null", testInstance1.getParamFile());

    testInstance.setBaseName(BASE_NAME);
    assertEquals(
        "param file is the base name, plus the peet dataset extension, in directory",
        new File(testDir, BASE_NAME + EXTENSION), testInstance.getParamFile());
  }
}
