package etomo.type;

import java.io.File;
import java.io.IOException;
import junit.framework.TestCase;

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
 * @version $$$$
 *
 * <p> $$Log$
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
  private File unreadableDir;
  private File unwritableDir;
  private File unreadableFileDir;
  private File validFileDir;
  private File dummyFile;
  private File unreadableFile;
  private File unreadableAFile;
  private File validFile;
  private File validAFile;
  private static final String dummyDirName = new String("ConstMetaData_dummy");
  private static final String dummyDir2Name =
    new String("ConstMetaData_dummy2");
  private static final String emptyDirName = new String("ConstMetaData_empty");
  private static final String emptyDir2Name =
    new String("ConstMetaData_empty2");
  private static final String unreadableDirName =
    new String("ConstMetaData_unrdable");
  private static final String unwritableDirName =
    new String("ConstMetaData_unwrtable");
  private static final String unreadableFileDirName =
    new String("ConstMetaData_unrdableFile");
  private static final String unreadableBFileDirName =
    new String("ConstMetaData_unrdableBFile");
  private static final String validFileDirName =
    new String("ConstMetaData_validFile");
  private static final String dummyFileName = new String("dummy");
  private static final String unreadableDatasetName = new String("unrdable");
  private static final String unreadableFileName =
    new String(unreadableDatasetName + ".st");
  private static final String unreadableAFileName =
    new String(unreadableDatasetName + "a.st");
  private static final String validDatasetName = new String("valid");
  private static final String validFileName =
    new String(validDatasetName + ".st");
  private static final String validAFileName =
    new String(validDatasetName + "a.st");
    
  private SystemProgram program;

  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
   
    //create test site
    testInst = new MetaData();
    testDir = new File(EtomoDirector.getInstance().getCurrentPropertyUserDir(), TypeTests.testRoot);
    if (!testDir.exists()) {
      assertTrue(testDir.mkdirs());
    }
    assertTrue(
      testDir.isDirectory() && testDir.canRead() && testDir.canWrite());

    //make instance of a non-existant directory
    dummyDir = new File(testDir, dummyDirName);
    assertTrue(!dummyDir.exists() || dummyDir.delete());

    //make a second instance of a non-existant directory
    dummyDir2 = new File(testDir, dummyDir2Name);
    assertTrue(!dummyDir2.exists() || dummyDir2.delete());

    //create empty directory
    emptyDir = new File(testDir, emptyDirName);
    if (!emptyDir.exists()) {
      assertTrue(emptyDir.mkdir());
    }
    assertTrue(
      emptyDir.isDirectory() && emptyDir.canRead() && emptyDir.canWrite());

    //create a second empty directory
    emptyDir2 = new File(testDir, emptyDir2Name);
    if (!emptyDir2.exists()) {
      assertTrue(emptyDir2.mkdir());
    }
    assertTrue(
      emptyDir2.isDirectory() && emptyDir2.canRead() && emptyDir2.canWrite());

    //create unreadable directory
    unreadableDir = new File(testDir, unreadableDirName);
    if (!unreadableDir.exists()) {
      assertTrue(unreadableDir.mkdir());
    }
    assertTrue(unreadableDir.isDirectory() && unreadableDir.canWrite());
    
    if (unreadableDir.canRead()) {
      SystemProgram program = new SystemProgram("chmod 244 " + unreadableDirName);
      program.setWorkingDirectory(testDir);
      program.run();
    }
    assertFalse(unreadableDir.canRead());

    //create unwritable directory
    unwritableDir = new File(testDir, unwritableDirName);
    if (!unwritableDir.exists()) {
      assertTrue(unwritableDir.mkdir() && unwritableDir.setReadOnly());
    }
    assertTrue(
      unwritableDir.isDirectory()
        && unwritableDir.canRead()
        && !unwritableDir.canWrite());

    //create a directory containing unreadable files
    unreadableFileDir = new File(testDir, unreadableFileDirName);
    if (!unreadableFileDir.exists()) {
      assertTrue(unreadableFileDir.mkdir());
    }
    assertTrue(
      unreadableFileDir.isDirectory()
        && unreadableFileDir.canRead()
        && unreadableFileDir.canWrite());
    //create unreadable files
    unreadableFile =
      createUnreadableFile(unreadableFileDir, unreadableFileName);
    unreadableAFile =
      createUnreadableFile(unreadableFileDir, unreadableAFileName);

    //create a directory containing valid files
    validFileDir = new File(testDir, validFileDirName);
    if (!validFileDir.exists()) {
      assertTrue(validFileDir.mkdir());
    }
    assertTrue(
      validFileDir.isDirectory()
        && validFileDir.canRead()
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
    System.out.println();
    unreadableFile.delete();
    unreadableAFile.delete();
    validFile.delete();
    validAFile.delete();
    emptyDir.delete();
    emptyDir2.delete();
    unreadableDir.delete();
    unwritableDir.delete();
    unreadableFileDir.delete();
    validFileDir.delete();
  }

  protected File createUnreadableFile(File dir, String name)
    throws IOException {
    File file = new File(dir, name);
    if (!file.exists()) {
      assertTrue(file.createNewFile());
    }
    assertTrue(file.isFile());
    if (file.canRead()) {
      SystemProgram program = new SystemProgram("chmod 244 " + name);
      program.setWorkingDirectory(dir);
      program.run();
    }
    assertFalse(file.canRead());
    return file;
  }

  protected File createValidFile(File dir, String name) throws IOException {
    //create a valid file
    File file = new File(dir, name);
    if (!file.exists()) {
      assertTrue(file.createNewFile());
    }
    assertTrue(file.isFile() && file.canRead());
    return file;
  }

  /**
   * Constructor for ConstMetaDataTest.
   * @param arg0
   */
  public ConstMetaDataTest(String arg0) {
    super(arg0);
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
    //file not readable
    assertFalse(ConstMetaData.isValid(unreadableFile, false));
    //file not writable
    assertFalse(ConstMetaData.isValid(unwritableDir, true));

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
    try {
      testInst.findValidFile(validFileName, dummyDir);
      fail("Should raise an IllegalArgumentException");
    }
    catch (IllegalArgumentException success) {
    }

    //file doesn't exist
    assertNull(testInst.findValidFile(validFileName, emptyDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("exist") == -1
      || invalidReason.indexOf(validFileName) == -1
      || invalidReason.indexOf(emptyDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //file not readable
    assertNull(testInst.findValidFile(unreadableFileName, unreadableFileDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("read") == -1
      || invalidReason.indexOf(unreadableFileName) == -1) {
      fail("invalidReason =" + invalidReason);
    }

    //Test successes

    //file exists & readable
    assertEquals(
      testInst.findValidFile(validFileName, validFileDir),
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

    //invalid directory
    try {
      testInst.findValidFile(validFileName, dummyDir, validFileDir);
      fail("Should raise an IllegalArgumentException");
    }
    catch (IllegalArgumentException success) {
    }

    //file doesn't exist
    //check one directory, second one invalid
    assertNull(testInst.findValidFile(validFileName, emptyDir, dummyDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("exist") == -1
      || invalidReason.indexOf(validFileName) == -1
      || invalidReason.indexOf(emptyDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //check one directory
    assertNull(testInst.findValidFile(validFileName, emptyDir, emptyDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("exist") == -1
      || invalidReason.indexOf(validFileName) == -1
      || invalidReason.indexOf(emptyDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //check two directories
    assertNull(testInst.findValidFile(validFileName, emptyDir, emptyDir2));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("exist") == -1
      || invalidReason.indexOf(validFileName) == -1
      || invalidReason.indexOf(emptyDir2Name) == -1) {
      fail("invalidReason =" + invalidReason);
    }

    //file not readable
    //file found in first directory
    assertNull(
      testInst.findValidFile(
        unreadableFileName,
        unreadableFileDir,
        validFileDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("read") == -1
      || invalidReason.indexOf(unreadableFileName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //file found in second directory
    assertNull(
      testInst.findValidFile(unreadableFileName, emptyDir, unreadableFileDir));
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("read") == -1
      || invalidReason.indexOf(unreadableFileName) == -1) {
      fail("invalidReason =" + invalidReason);
    }

    //Test successes

    //file exists & readable
    //file found in first directory
    assertEquals(
      testInst.findValidFile(validFileName, validFileDir, emptyDir),
      validFileDir);
    //file found in second directory
    assertEquals(
      testInst.findValidFile(validFileName, emptyDir, validFileDir),
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
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    String workingDir = etomoDirector.setCurrentPropertyUserDir(dummyDir.getAbsolutePath());
    testInst.setBackupDirectory(dummyDir2.getAbsolutePath());
    testInst.setDatasetName(validDatasetName);
    assertFalse(testInst.isDatasetNameValid());
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("exist") == -1
      || invalidReason.indexOf(dummyDirName) == -1
      || invalidReason.indexOf(dummyDir2Name) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //problems with the working directory
    //invalid backup directory is ignored while invalid working directory exists
    //unreadable working directory
    etomoDirector.setCurrentPropertyUserDir(unreadableDir.getAbsolutePath());
    testInst.setBackupDirectory(unwritableDir.getAbsolutePath());
    assertFalse(testInst.isDatasetNameValid());
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("read") == -1
      || invalidReason.indexOf(unreadableDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //unwritable working directory
    etomoDirector.setCurrentPropertyUserDir(unwritableDir.getAbsolutePath());
    testInst.setBackupDirectory(unreadableDir.getAbsolutePath());
    assertFalse(testInst.isDatasetNameValid());
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("writ") == -1
      || invalidReason.indexOf(unwritableDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //problems with the backup directory
    //unreadable backup directory
    etomoDirector.setCurrentPropertyUserDir(dummyDir.getAbsolutePath());
    testInst.setBackupDirectory(unreadableDir.getAbsolutePath());
    assertFalse(testInst.isDatasetNameValid());
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("read") == -1
      || invalidReason.indexOf(unreadableDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }
    //unwritable backup directory
    testInst.setBackupDirectory(unwritableDir.getAbsolutePath());
    assertFalse(testInst.isDatasetNameValid());
    invalidReason = testInst.getInvalidReason();
    if (invalidReason.equals("")
      || invalidReason.indexOf("writ") == -1
      || invalidReason.indexOf(unwritableDirName) == -1) {
      fail("invalidReason =" + invalidReason);
    }

    //file not found
    //dual axis
    //file A not found
    testInst.setAxisType(AxisType.DUAL_AXIS);
    testInst.setDatasetName(unreadableDatasetName);
    etomoDirector.setCurrentPropertyUserDir(unreadableFileDir.getAbsolutePath());
    assertFalse(testInst.isDatasetNameValid());
    //single axis
    testInst.setAxisType(AxisType.SINGLE_AXIS);
    testInst.setDatasetName(unreadableDatasetName);
    etomoDirector.setCurrentPropertyUserDir(unreadableFileDir.getAbsolutePath());
    assertFalse(testInst.isDatasetNameValid());
    
    //Test successes
    testInst.setDatasetName(validDatasetName);
    etomoDirector.setCurrentPropertyUserDir(validFileDir.getAbsolutePath());

    //working dir, single axis
    assertTrue(testInst.isDatasetNameValid());

    //backup dir, dual axis
    etomoDirector.setCurrentPropertyUserDir(emptyDir.getAbsolutePath());
    testInst.setBackupDirectory(validFileDir.getAbsolutePath());
    testInst.setAxisType(AxisType.DUAL_AXIS);
    assertTrue(testInst.isDatasetNameValid());

    etomoDirector.setCurrentPropertyUserDir(workingDir);

  }


  /*
   * Class to test for boolean isValid()
   */
  final public void testIsValid() {
    String invalidReason;
    EtomoDirector etomoDirector = EtomoDirector.getInstance();
    String workingDir = etomoDirector.setCurrentPropertyUserDir(validFileDir.getAbsolutePath());
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

    etomoDirector.setCurrentPropertyUserDir(workingDir);

  }
}
