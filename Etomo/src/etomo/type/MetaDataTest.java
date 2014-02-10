package etomo.type;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.logic.DatasetTool;
import etomo.process.SystemProgram;
import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.29  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.28  2009/10/26 15:03:00  sueh
 * <p> bug# 1275 Fixed test bug caused by the removal of the default manager.
 * <p>
 * <p> Revision 3.27  2009/10/23 23:50:17  sueh
 * <p> bug# 1275 No default manager.
 * <p>
 * <p> Revision 3.26  2009/03/17 00:46:15  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.25  2009/02/20 17:07:24  sueh
 * <p> bug# 1180 Letting exceptions by thrown instead of catch so that more
 * <p> information will be available when this fails.
 * <p>
 * <p> Revision 3.24  2009/02/04 23:30:30  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 3.23  2008/12/15 23:02:38  sueh
 * <p> bug# 1161 Made EtomoDirector.getCurrentManager private.  Added a
 * <p> public test version for public access.
 * <p>
 * <p> Revision 3.22  2007/12/26 22:18:31  sueh
 * <p> bug# 1052 Removed print statement.
 * <p>
 * <p> Revision 3.21  2007/12/14 18:51:33  sueh
 * <p> Running system program is not working on windows.
 * <p>
 * <p> Revision 3.20  2007/12/10 22:37:18  sueh
 * <p> bug# 1041 Merged ConstMetaDataTest with MetaDataTest.
 * <p>
 * <p> Revision 3.19  2007/09/07 00:25:21  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.18  2007/07/30 18:53:55  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 3.17  2006/11/18 01:16:49  sueh
 * <p> bug# 956 Temporarily not running problem tests on Windows.
 * <p>
 * <p> Revision 3.16  2006/11/15 20:49:12  sueh
 * <p> bug# 872 testStore_Properties:  throw LogFile exceptions because properties is
 * <p> using LogFile.
 * <p>
 * <p> Revision 3.15  2005/12/23 02:07:20  sueh
 * <p> bug# 675 Changed EtomoDirector.getCurrentTestManager to
 * <p> getCurrentManager_test.  EtomoDirectory.getInstance no longer initializes
 * <p> etomo.
 * <p>
 * <p> Revision 3.14  2005/11/10 18:10:35  sueh
 * <p> bug# 758 Placed the root test directory in a File object in JUnitTests.  It is
 * <p> instanciated once so there won't be a problem if the working directory is
 * <p> changed.  Added a root test directory File object to each of the suites,
 * <p> which is based on the JUnitTests root test directory.
 * <p> bug# 748 calling TestUtilities.getVector instead of checkoutVector.
 * <p>
 * <p> Revision 3.13  2005/07/29 00:53:41  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.12  2004/12/09 04:58:13  sueh
 * <p> bug# 520 Removed unnecessary import.
 * <p>
 * <p> Revision 3.11  2004/12/08 21:31:41  sueh
 * <p> bug# 520 Setting the working directory in TestUtilities.checkoutVector().
 * <p> Also setting the fail message for SystemProcessException in
 * <p> TestUtilities.checkoutVector().
 * <p>
 * <p> Revision 3.10  2004/12/08 03:47:42  sueh
 * <p> bug# 520 improving failure message!!!!!!!!!!!#!@$@!#%#Q$^#%&#$
 * <p>
 * <p> Revision 3.9  2004/12/08 00:13:07  sueh
 * <p> bug# 520 Passing a relative vector to checkoutVector.
 * <p>
 * <p> Revision 3.8  2004/12/07 23:36:26  sueh
 * <p> bug# 520 Changing print statements.
 * <p>
 * <p> Revision 3.7  2004/12/06 23:35:47  sueh
 * <p> bug# 520 Added print statements.
 * <p>
 * <p> Revision 3.6  2004/11/30 18:03:37  sueh
 * <p> bug# 520  Fixing unit tests:  putting setup code in the SetUp() instead of
 * <p> constructor
 * <p>
 * <p> Revision 3.5  2004/11/24 22:15:47  sueh
 * <p> bug# 520 Initializing, creating, and testing testDir in the constructor and
 * <p> using it everywhere in MetaDataTest.
 * <p>
 * <p> Revision 3.4  2004/11/24 01:28:41  sueh
 * <p> bug# 520 MetaDataTest(): Getting the correct file path for error reporting.
 * <p>
 * <p> Revision 3.3  2004/11/23 00:27:38  sueh
 * <p> bug# 520 Using get and setPropertyUserDir instead of Property.  Don't
 * <p> use File.separator with propertyUserDir since it may end in "/".  Construct
 * <p> a new file with originalDirectory as the base directory and get the absolute
 * <p> file.
 * <p>
 * <p> Revision 3.2  2004/06/01 18:56:00  rickg
 * <p> Import fix for javadoc
 * <p>
 * <p> Revision 3.1  2004/04/06 03:24:31  rickg
 * <p> Create MetaDataTest
 * <p> </p>
 */

public class MetaDataTest extends TestCase {
  public static final String rcsid = "$Id$";
  private MetaData testInst;
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
  private static final String validFileDirName = new String("ConstMetaData_validFile");
  private static final String dummyFileName = new String("dummy");
  private static final String validDatasetName = new String("valid");
  private static final String validFileName = new String(validDatasetName
      + DatasetTool.STANDARD_DATASET_EXT);
  private static final String validAFileName = new String(validDatasetName + "a.st");
  private boolean windowsOs = false;
  private SystemProgram program;

  private static final String testDirectory = "MetaData";

  private File testDir;
  private final ApplicationManager manager;

  public MetaDataTest(String test) {
    super(test);
    EtomoDirector.INSTANCE.openTomogram(true, AxisID.ONLY);
    manager = (ApplicationManager) EtomoDirector.INSTANCE.getCurrentManagerForTest();
  }

  protected void setUp() throws Exception {
    super.setUp();
    EtomoDirector etomoDirector = EtomoDirector.INSTANCE;
    testDir = new File(TypeTests.TEST_ROOT_DIR, testDirectory);
    if (!testDir.exists()) {
      assertTrue(testDir.mkdirs());
    }
    assertTrue(testDir.isDirectory() && testDir.canRead() && testDir.canWrite());

    // Don't do tests involving file permissions in Windows, since they can't be set.
    String osName = System.getProperty("os.name").toLowerCase();
    windowsOs = osName.indexOf("windows") != -1;
    // create test site
    testInst = new MetaData(manager, manager.getLogProperties());
    testDir = new File(TypeTests.TEST_ROOT_DIR, "ConstMetaData");
    if (testDir.isFile()) {
      testDir.delete();
    }
    if (!testDir.exists()) {
      testDir.mkdirs();
    }
    assertTrue(testDir.isDirectory() && testDir.canRead() && testDir.canWrite());

    // make instance of a non-existant directory
    dummyDir = new File(testDir, dummyDirName);
    if (!windowsOs) {
      assertTrue(!dummyDir.exists() || dummyDir.delete());
    }

    // make a second instance of a non-existant directory
    dummyDir2 = new File(testDir, dummyDir2Name);
    assertTrue(!dummyDir2.exists() || dummyDir2.delete());

    // create empty directory
    emptyDir = new File(testDir, emptyDirName);
    if (!emptyDir.exists()) {
      assertTrue(emptyDir.mkdir());
    }
    assertTrue(emptyDir.isDirectory() && emptyDir.canRead() && emptyDir.canWrite());

    // create a second empty directory
    emptyDir2 = new File(testDir, emptyDir2Name);
    if (!emptyDir2.exists()) {
      assertTrue(emptyDir2.mkdir());
    }
    assertTrue(emptyDir2.isDirectory() && emptyDir2.canRead() && emptyDir2.canWrite());

    // create a directory containing valid files
    validFileDir = new File(testDir, validFileDirName);
    if (!validFileDir.exists()) {
      assertTrue(validFileDir.mkdir());
    }
    assertTrue(validFileDir.isDirectory() && validFileDir.canRead()
        && validFileDir.canWrite());
    // create valid files
    validFile = createValidFile(validFileDir, validFileName);
    validAFile = createValidFile(validFileDir, validAFileName);
  }

  protected File createUnreadableFile(File dir, String name) throws IOException {
    File file = new File(dir, name);
    file.delete();
    file.createNewFile();
    assertTrue(file.isFile());
    if (file.canRead()) {
      SystemProgram program = new SystemProgram(manager, manager.getPropertyUserDir(),
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
    // create a valid file
    File file = new File(dir, name);
    file.delete();
    file.createNewFile();
    assertTrue(file.isFile() && file.canRead());
    return file;
  }

  /* Class to test for boolean isValid(File, boolean) */
  final public void testIsValidFileboolean() throws IOException {
    // Test failures

    // null arg
    assertFalse(MetaData.isValid(null, false));
    // file doesn't exist
    assertFalse(MetaData.isValid(dummyFile, false));
    // Test successes

    // file exists & readable
    assertTrue(MetaData.isValid(validFile, false));
    // file writable
    assertTrue(MetaData.isValid(validFileDir, true));
  }

  /* Class to test for File findValidFile(String, File) */
  final public void testFindValidFileStringFile() throws IOException {
    String invalidReason;
    // Test failures

    // null arg
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

    // invalid directory
    if (!windowsOs) {
      try {
        testInst.findValidFile(validFileName, dummyDir);
        fail("Should raise an IllegalArgumentException");
      }
      catch (IllegalArgumentException success) {
      }
    }

    // file doesn't exist
    assertNull(testInst.findValidFile(validFileName, emptyDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
        || invalidReason.indexOf(validFileName) == -1
        || invalidReason.indexOf(emptyDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    // Test successes

    // file exists & readable
    assertEquals(testInst.findValidFile(validFileName, validFileDir), validFileDir);
  }

  /* Class to test for File findValidFile(String, File, File) */
  final public void testFindValidFileStringFileFile() throws IOException {
    String invalidReason;
    // Test failures

    // null arg
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
      // invalid directory
      try {
        testInst.findValidFile(validFileName, dummyDir, validFileDir);
        fail("Should raise an IllegalArgumentException");
      }
      catch (IllegalArgumentException success) {
      }
    }

    // file doesn't exist
    // check one directory, second one invalid
    if (!windowsOs) {
      assertNull(testInst.findValidFile(validFileName, emptyDir, dummyDir));
      invalidReason = testInst.getInvalidReason();
      if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
          || invalidReason.indexOf(validFileName) == -1
          || invalidReason.indexOf(emptyDirName) == -1) {
        fail("invalidReason =" + invalidReason);
      }
    }
    // check one directory
    assertNull(testInst.findValidFile(validFileName, emptyDir, emptyDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
        || invalidReason.indexOf(validFileName) == -1
        || invalidReason.indexOf(emptyDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    // check two directories
    assertNull(testInst.findValidFile(validFileName, emptyDir, emptyDir2));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("") || invalidReason.indexOf("exist") == -1
        || invalidReason.indexOf(validFileName) == -1
        || invalidReason.indexOf(emptyDir2Name) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    // Test successes

    // file exists & readable
    // file found in first directory
    assertEquals(testInst.findValidFile(validFileName, validFileDir, emptyDir),
        validFileDir);
    // file found in second directory
    assertEquals(testInst.findValidFile(validFileName, emptyDir, validFileDir),
        validFileDir);
  }

  /* Class to test for boolean isDatasetNameValid() */
  final public void testIsDatasetNameValid() {
    String invalidReason;
    // Test failures

    // invalid directories
    // non-existant directories
    String workingDir = EtomoDirector.INSTANCE.setCurrentPropertyUserDir(dummyDir
        .getAbsolutePath());
    testInst.setBackupDirectory(dummyDir2.getAbsolutePath());
    testInst.setDatasetName(validDatasetName);
    if (!windowsOs) {
      assertFalse(testInst.isDatasetNameValid());
    }
    // Test successes
    testInst.setDatasetName(validDatasetName);
    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(validFileDir.getAbsolutePath());
    // backup dir, dual axis
    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(emptyDir.getAbsolutePath());
    testInst.setBackupDirectory(validFileDir.getAbsolutePath());
    testInst.setAxisType(AxisType.DUAL_AXIS);
    assertTrue(testInst.isDatasetNameValid());
    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(workingDir);
  }

  /* Class to test for boolean isValid() */
  final public void testIsValid() {
    String invalidReason;
    String workingDir = EtomoDirector.INSTANCE.setCurrentPropertyUserDir(validFileDir
        .getAbsolutePath());
    // Test failures

    // Testing boolean isValid(boolean fromSetupScreen)
    // order of tests:
    // Axis type
    // dataset name
    // pixel
    // fiducial

    // Axis type is always required
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

    // Dataset name is always required
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

    // fiducial diameter must be > 0
    // fiducial is only checked for the Setup dialog
    testInst.setPixelSize(2.84);
    testInst.setFiducialDiameter(0);
    assertFalse(testInst.isValid(true));
    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(workingDir);

  }
}