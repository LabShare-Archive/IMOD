package etomo.type;

import java.io.File;
import java.io.IOException;
import junit.framework.TestCase;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.process.SystemProgram;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002, 2003, 2004 </p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $Revision$
 *
 * <p> $$Log$
 * <p> $Revision 3.15  2006/05/22 22:51:42  sueh
 * <p> $bug# 577 Placed commands in a String[] rather then a String.
 * <p> $
 * <p> $Revision 3.14  2006/01/03 23:22:19  sueh
 * <p> $bug# 675 Removed the tests involving unreadable or unwriteable files or
 * <p> $directories.
 * <p> $
 * <p> $Revision 3.13  2005/12/23 02:07:03  sueh
 * <p> $bug# 675 Changed EtomoDirector.getCurrentTestManager to
 * <p> $getCurrentManager_test.  EtomoDirectory.getInstance no longer initializes
 * <p> $etomo.
 * <p> $
 * <p> $Revision 3.12  2005/11/10 18:09:29  sueh
 * <p> $bug# 758 Fixed createValidFile() so it deletes and creates the file.
 * <p> $
 * <p> $Revision 3.11  2005/07/29 00:53:20  sueh
 * <p> $bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> $because the current manager changes when the user changes the tab.
 * <p> $Passing the manager where its needed.
 * <p> $
 * <p> $Revision 3.10  2005/04/25 20:51:08  sueh
 * <p> $bug# 615 Passing the axis where a command originates to the message
 * <p> $functions so that the message will be popped up in the correct window.
 * <p> $This requires adding AxisID to many objects.
 * <p> $
 * <p> $Revision 3.9  2005/02/10 18:55:57  sueh
 * <p> $bug# 599 Not running tests on windows which have to do with file
 * <p> $permissions.
 * <p> $
 * <p> $Revision 3.8  2005/02/09 18:41:58  sueh
 * <p> $Adding revision to comment.
 * <p> $
 * <p> $Revision 3.7  2004/11/24 22:14:26  sueh
 * <p> $bug# 520 Getting the root test directory name from TypeTests.
 * <p> $
 * <p> $Revision 3.6  2004/11/23 00:25:16  sueh
 * <p> $bug# 520 Using get and setPropertyUserDir instead of Property.
 * <p> $
 * <p> $Revision 3.5  2004/05/26 17:16:39  sueh
 * <p> $bug# 355 correcting tests
 * <p> $$ </p>
 */

public class ConstMetaDataTest extends TestCase {
  public static final String rcsid = "$$Id$$";
  private MetaData testInst;
  private File testDir;
  private File dummyDir;
  private File dummyDir2;
  private File emptyDir;
  private File emptyDir2;
  private File validFileDir;
  private File dummyFile;
  private File validFile;
  private File validAFile;
  private static final String dummyDirName = new String("ConstMetaData_dummy");
  private static final String dummyDir2Name = new String("ConstMetaData_dummy2");
  private static final String emptyDirName = new String("ConstMetaData_empty");
  private static final String emptyDir2Name = new String("ConstMetaData_empty2");
  private static final String validFileDirName = new String(
      "ConstMetaData_validFile");
  private static final String dummyFileName = new String("dummy");
  private static final String validDatasetName = new String("valid");
  private static final String validFileName = new String(validDatasetName
      + ".st");
  private static final String validAFileName = new String(validDatasetName
      + "a.st");
  private boolean windowsOs = false;
  private final ApplicationManager manager;

  private SystemProgram program;

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();

    //Don't do tests involving file permissions in Windows, since they can't be set.
    String osName = System.getProperty("os.name").toLowerCase();
    windowsOs = osName.indexOf("windows") != -1;
    //create test site
    testInst = new MetaData(manager);
    testDir = new File(TypeTests.TEST_ROOT_DIR, "ConstMetaData");
    if (testDir.isFile()) {
      testDir.delete();
    }
    if (!testDir.exists()) {
      testDir.mkdirs();
    }
    assertTrue(testDir.isDirectory() && testDir.canRead() && testDir.canWrite());

    //make instance of a non-existant directory
    dummyDir = new File(testDir, dummyDirName);
    if (!windowsOs) {
      assertTrue(!dummyDir.exists() || dummyDir.delete());
    }

    //make a second instance of a non-existant directory
    dummyDir2 = new File(testDir, dummyDir2Name);
    assertTrue(!dummyDir2.exists() || dummyDir2.delete());

    //create empty directory
    emptyDir = new File(testDir, emptyDirName);
    if (!emptyDir.exists()) {
      assertTrue(emptyDir.mkdir());
    }
    assertTrue(emptyDir.isDirectory() && emptyDir.canRead()
        && emptyDir.canWrite());

    //create a second empty directory
    emptyDir2 = new File(testDir, emptyDir2Name);
    if (!emptyDir2.exists()) {
      assertTrue(emptyDir2.mkdir());
    }
    assertTrue(emptyDir2.isDirectory() && emptyDir2.canRead()
        && emptyDir2.canWrite());

    //create a directory containing valid files
    validFileDir = new File(testDir, validFileDirName);
    if (!validFileDir.exists()) {
      assertTrue(validFileDir.mkdir());
    }
    assertTrue(validFileDir.isDirectory() && validFileDir.canRead()
        && validFileDir.canWrite());
    //create valid files
    validFile = createValidFile(validFileDir, validFileName);
    validAFile = createValidFile(validFileDir, validAFileName);
  }

  /*
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  protected File createUnreadableFile(File dir, String name) throws IOException {
    File file = new File(dir, name);
    file.delete();
    file.createNewFile();
    assertTrue(file.isFile());
    if (file.canRead()) {
      SystemProgram program = new SystemProgram(manager.getPropertyUserDir(),
          new String[] { "chmod", "244", name }, AxisID.ONLY);
      program.setWorkingDirectory(dir);
      program.run();
    }
    if (!windowsOs) {
      assertFalse(file.canRead());
    }
    return file;
  }

  protected File createValidFile(File dir, String name) throws IOException {
    //create a valid file
    File file = new File(dir, name);
    file.delete();
    file.createNewFile();
    assertTrue(file.isFile() && file.canRead());
    return file;
  }

  /**
   * Constructor for ConstMetaDataTest.
   * @param arg0
   */
  public ConstMetaDataTest(String arg0) {
    super(arg0);
    manager = (ApplicationManager) EtomoDirector.INSTANCE.getCurrentManager();
  }

  /*
   * Class to test for boolean isValid(File, boolean)
   */
  final public void testIsValidFileboolean() throws IOException {
    //Test failures

    //null arg
    assertFalse(ConstMetaData.isValid(null, false));
    //file doesn't exist
    assertFalse(ConstMetaData.isValid(dummyFile, false));
    //Test successes

    //file exists & readable
    assertTrue(ConstMetaData.isValid(validFile, false));
    //file writable
    assertTrue(ConstMetaData.isValid(validFileDir, true));
  }

  /*
   * Class to test for File findValidFile(String, File)
   */
  final public void testFindValidFileStringFile() throws IOException {
    String invalidReason;
    //Test failures

    //null arg
    try {
      testInst.findValidFile(null, validFileDir);
      fail("Should raise an IllegalArgumentException");
    }
    catch (IllegalArgumentException success) {
    }
    try {
      testInst.findValidFile(validFileName, null);
      fail("Should raise an IllegalArgumentException");
    }
    catch (IllegalArgumentException success) {
    }

    //invalid directory
    if (!windowsOs) {
      try {
        testInst.findValidFile(validFileName, dummyDir);
        fail("Should raise an IllegalArgumentException");
      }
      catch (IllegalArgumentException success) {
      }
    }

    //file doesn't exist
    assertNull(testInst.findValidFile(validFileName, emptyDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
        || invalidReason.indexOf(validFileName) == -1
        || invalidReason.indexOf(emptyDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //Test successes

    //file exists & readable
    assertEquals(testInst.findValidFile(validFileName, validFileDir),
        validFileDir);
  }

  /*
   * Class to test for File findValidFile(String, File, File)
   */
  final public void testFindValidFileStringFileFile() throws IOException {
    String invalidReason;
    //Test failures

    //null arg
    try {
      testInst.findValidFile(null, validFileDir, validFileDir);
      fail("Should raise an IllegalArgumentException");
    }
    catch (IllegalArgumentException success) {
    }
    try {
      testInst.findValidFile(validFileName, null, validFileDir);
      fail("Should raise an IllegalArgumentException");
    }
    catch (IllegalArgumentException success) {
    }
    try {
      testInst.findValidFile(validFileName, validFileDir, null);
      fail("Should raise an IllegalArgumentException");
    }
    catch (IllegalArgumentException success) {
    }

    if (!windowsOs) {
      //invalid directory
      try {
        testInst.findValidFile(validFileName, dummyDir, validFileDir);
        fail("Should raise an IllegalArgumentException");
      }
      catch (IllegalArgumentException success) {
      }
    }

    //file doesn't exist
    //check one directory, second one invalid
    if (!windowsOs) {
      assertNull(testInst.findValidFile(validFileName, emptyDir, dummyDir));
      invalidReason = testInst.getInvalidReason();
      if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
          || invalidReason.indexOf(validFileName) == -1
          || invalidReason.indexOf(emptyDirName) == -1) {
        fail("invalidReason =" + invalidReason);
      }
    }
    //check one directory
    assertNull(testInst.findValidFile(validFileName, emptyDir, emptyDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
        || invalidReason.indexOf(validFileName) == -1
        || invalidReason.indexOf(emptyDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //check two directories
    assertNull(testInst.findValidFile(validFileName, emptyDir, emptyDir2));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
        || invalidReason.indexOf(validFileName) == -1
        || invalidReason.indexOf(emptyDir2Name) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //Test successes

    //file exists & readable
    //file found in first directory
    assertEquals(testInst.findValidFile(validFileName, validFileDir, emptyDir),
        validFileDir);
    //file found in second directory
    assertEquals(testInst.findValidFile(validFileName, emptyDir, validFileDir),
        validFileDir);
  }

  /*
   * Class to test for boolean isDatasetNameValid()
   */
  final public void testIsDatasetNameValid() {
    String invalidReason;
    //Test failures

    //invalid directories
    //non-existant directories
    String workingDir = EtomoDirector.INSTANCE.setCurrentPropertyUserDir(
        dummyDir.getAbsolutePath());
    testInst.setBackupDirectory(dummyDir2.getAbsolutePath());
    testInst.setDatasetName(validDatasetName);
    if (!windowsOs) {
      assertFalse(testInst.isDatasetNameValid());
      invalidReason = testInst.getInvalidReason();
      if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
          || invalidReason.indexOf(dummyDirName) == -1
          || invalidReason.indexOf(dummyDir2Name) == -1) {
        fail("invalidReason =" + invalidReason);
      }
    }
    //Test successes
    testInst.setDatasetName(validDatasetName);
    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(
        validFileDir.getAbsolutePath());

    //working dir, single axis
    assertTrue(testInst.isDatasetNameValid());

    //backup dir, dual axis
    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(
        emptyDir.getAbsolutePath());
    testInst.setBackupDirectory(validFileDir.getAbsolutePath());
    testInst.setAxisType(AxisType.DUAL_AXIS);
    assertTrue(testInst.isDatasetNameValid());

    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(workingDir);

  }

  /*
   * Class to test for boolean isValid()
   */
  final public void testIsValid() {
    String invalidReason;
    String workingDir = EtomoDirector.INSTANCE.setCurrentPropertyUserDir(
        validFileDir.getAbsolutePath());
    //Test failures

    //Testing boolean isValid(boolean fromSetupScreen)
    //order of tests:
    //Axis type
    //dataset name
    //pixel
    //fiducial

    //Axis type is always required
    assertFalse(testInst.isValid(true));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
        || invalidReason.toLowerCase().indexOf("axis type") == -1) {
      fail("invalidReason=" + invalidReason);
    }
    assertFalse(testInst.isValid(false));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
        || invalidReason.toLowerCase().indexOf("axis type") == -1) {
      fail("invalidReason=" + invalidReason);
    }

    //Dataset name is always required
    testInst.setAxisType(AxisType.DUAL_AXIS);
    assertFalse(testInst.isValid(true));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
        || invalidReason.toLowerCase().indexOf("dataset name") == -1) {
      fail("invalidReason=" + invalidReason);
    }
    assertFalse(testInst.isValid(false));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
        || invalidReason.toLowerCase().indexOf("dataset name") == -1) {
      fail("invalidReason=" + invalidReason);
    }

    //pixel size must be > 0
    //pixel is only checked for the Setup dialog
    testInst.setDatasetName(validDatasetName);
    testInst.setPixelSize(0);
    assertFalse(testInst.isValid(true));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
        || invalidReason.toLowerCase().indexOf("pixel") == -1
        || invalidReason.indexOf("size") == -1
        || invalidReason.indexOf("zero") == -1) {
      fail("invalidReason=" + invalidReason);
    }
    assertTrue(testInst.isValid(false));

    //fiducial diameter must be > 0
    //fiducial is only checked for the Setup dialog
    testInst.setPixelSize(2.84);
    testInst.setFiducialDiameter(0);
    assertFalse(testInst.isValid(true));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
        || invalidReason.toLowerCase().indexOf("fiducial") == -1
        || invalidReason.indexOf("diameter") == -1) {
      fail("invalidReason=" + invalidReason);
    }
    assertTrue(testInst.isValid(false));

    //Test success

    //working dir, single axis
    testInst.setFiducialDiameter(15);
    assertTrue(testInst.isValid(true));

    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(workingDir);

  }
}