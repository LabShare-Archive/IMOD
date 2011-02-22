package etomo.util;

import java.io.File;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.SystemProcessException;
import etomo.type.AxisID;

import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 3.31  2010/05/21 21:05:28  sueh
 * <p> bug# 1362 Getting test data by calling TestUtilities.copyTestFile instead of
 * <p> getVector.
 * <p>
 * <p> Revision 3.30  2010/02/17 05:05:58  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up
 * <p> messages.
 * <p>
 * <p> Revision 3.29  2009/10/27 15:33:17  sueh
 * <p> bug# 1275
 * <p>
 * <p> Revision 3.28  2009/03/17 22:15:21  sueh
 * <p> testReadBadFilename():  Corrected failure message.
 * <p>
 * <p> Revision 3.27  2009/03/17 00:46:44  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.26  2009/02/20 17:07:48  sueh
 * <p> bug# 1180 Letting exceptions be thrown instead of catching them so that
 * <p> more information will be available when this fails.
 * <p>
 * <p> Revision 3.25  2009/02/13 16:53:50  sueh
 * <p> bug# 1176 In testRead removed the reread test because that doesn't
 * <p> affect the return value anymore.
 * <p>
 * <p> Revision 3.24  2009/02/13 02:40:45  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.
 * <p>
 * <p> Revision 3.23  2008/12/15 23:05:02  sueh
 * <p> bug# 1161 Made EtomoDirector.getCurrentManager private.  Added a
 * <p> public test version for public access.
 * <p>
 * <p> Revision 3.22  2007/09/07 00:31:00  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.21  2006/11/18 01:17:04  sueh
 * <p> bug# 956 Temporarily not running problem tests on Windows.
 * <p>
 * <p> Revision 3.20  2006/11/15 21:37:18  sueh
 * <p> bug# 872 In setup:  create the etomo instance the same way other test classes
 * <p> create it.
 * <p>
 * <p> Revision 3.19  2005/12/23 02:28:06  sueh
 * <p> bug# 675 Changed EtomoDirectory.getCurrentTestManager to
 * <p> getCurrentManager_test.
 * <p>
 * <p> Revision 3.18  2005/11/10 18:19:50  sueh
 * <p> bug# 758 Standardized the way files are created.
 * <p>
 * <p> Revision 3.17  2005/07/29 00:55:35  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.16  2005/06/22 23:39:48  sueh
 * <p> bug# 522 New tests are failing - will fix later.  changed testRead().
 * <p>
 * <p> Revision 3.15  2005/06/21 00:55:45  sueh
 * <p> bug# 522 Used touch to change the last modified date on the test file and
 * <p> check whether MRCHeader will re-read it.
 * <p>
 * <p> Revision 3.14  2005/06/20 17:08:45  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing.  Changed testReadBadFilename() to work with the different
 * <p> way exceptions are being thrown in MRCHeader.read().
 * <p>
 * <p> Revision 3.13  2005/04/25 21:43:32  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.12  2004/12/08 21:32:34  sueh
 * <p> bug# 520 Setting the working directory in TestUtilities.checkoutVector().
 * <p> Also setting the fail message for SystemProcessException in
 * <p> TestUtilities.checkoutVector().
 * <p>
 * <p> Revision 3.11  2004/11/24 22:16:19  sueh
 * <p> bug# 520 Getting the root test directory name from UtilTests.
 * <p>
 * <p> Revision 3.10  2004/11/23 00:37:01  sueh
 * <p> bug# 520 Using get and setPropertyUserDir instead of Property.  Don't
 * <p> use File.separator with propertyUserDir since it may end in "/".  Construct
 * <p> a new file with originalDirectory as the base directory and get the absolute
 * <p> file.
 * <p>
 * <p> Revision 3.9  2004/11/20 00:12:00  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.8.4.1  2004/09/03 21:19:14  sueh
 * <p> bug# 520 getting app mgr from EtomoDirector
 * <p>
 * <p> Revision 3.8  2004/04/06 02:49:03  rickg
 * <p> Use TestUtilities methods
 * <p>
 * <p> Revision 3.7  2004/04/02 18:44:43  rickg
 * <p> Uses TestUtilities class
 * <p>
 * <p> Revision 3.6  2004/02/13 00:08:13  rickg
 * <p> Moved checkouts out of setup and into individual tests.
 * <p>
 * <p> Revision 3.5  2004/02/10 04:53:50  rickg
 * <p> Changed CVS commans to export
 * <p>
 * <p> Revision 3.4  2004/01/27 18:07:05  rickg
 * <p> Unset debug mode for tests, too much cruft
 * <p>
 * <p> Revision 3.3  2004/01/16 20:41:22  rickg
 * <p> Open up the application manager in test mode
 * <p>
 * <p> Revision 3.2  2004/01/16 18:26:38  rickg
 * <p> Added checkout of testHeader to appropriate directories
 * <p>
 * <p> Revision 3.1  2004/01/13 22:36:47  rickg
 * <p> Creates it own ApplicationManager for static function access
 * <p> Added a test that includes a space in the directory name
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:45:05  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/10/03 18:57:38  rickg
 * <p> Initial revision
 * <p> </p>
 */
public class MRCHeaderTest extends TestCase {
  private static final String testDirectory1 = new String("Test");
  private static final String testDirectory2 = new String("With Spaces");
  private static final String headerTestStack = "headerTest.st";
  private final MRCHeader emptyFilename;
  private final MRCHeader badFilename;
  private final MRCHeader mrcHeader;
  private final MRCHeader mrcWithSpaces;
  private final File testDir = new File(UtilTests.TEST_ROOT_DIR, "MRCHeader");
  private final String testDirPath = testDir.getAbsolutePath();
  private final BaseManager manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();

  /**
   * Constructor for MRCHeaderTest.
   * @param arg0
   */
  public MRCHeaderTest(String arg0) {
    super(arg0);
    if (testDir.isFile()) {
      testDir.delete();
    }
    if (!testDir.exists()) {
      testDir.mkdirs();
    }
    emptyFilename = MRCHeader.getInstance(testDirPath, "", AxisID.ONLY);
    badFilename = MRCHeader.getInstance(testDirPath, testDirectory1
        + "/non_existant_image_file", AxisID.ONLY);
    mrcHeader = MRCHeader.getInstance(testDirPath, testDirectory1 + "/headerTest.st",
        AxisID.ONLY);
    mrcWithSpaces = MRCHeader.getInstance(testDirPath, testDirectory2 + "/headerTest.st",
        AxisID.ONLY);
  }

  public void testEmptyFilename() throws InvalidParameterException {
    try {
      assertFalse("Should rise IOException exception", emptyFilename.read(manager));
    }
    catch (IOException success) {
    }
  }

  public void testReadBadFilename() throws InvalidParameterException {
    // First test, should throw an exception because the image stack is not
    // present
    boolean exceptionThrown = false;
    try {
      assertFalse("Did not return false", badFilename.read(manager));
    }
    catch (IOException except) {
    }
  }

  public void testRead() throws IOException, InvalidParameterException,
      SystemProcessException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    //  Create the test directory
    File testDir1 = new File(testDir, testDirectory1);
    if (testDir1.isFile()) {
      testDir1.delete();
    }
    if (!testDir1.exists()) {
      testDir1.mkdirs();
    }

    // Check out the test header stack into the required directories
    TestUtilites.INSTANCE.copyTestFile(testDirPath, testDirectory1, headerTestStack);

    assertTrue(mrcHeader.read(manager));
    assertEquals("Incorrect column count", 512, mrcHeader.getNColumns());
    assertEquals("Incorrect row count", 512, mrcHeader.getNRows());
    assertEquals("Incorrect section count", 1, mrcHeader.getNSections());
    //TEMP test failing - fix later
    /*
     //test: re-read works when the file modify time is later then previous read
     //time
     try {
     Thread.sleep(10);
     }
     catch (InterruptedException e) {
     }
     File file = new File(UtilTests.testRoot + testDirectory1, headerTestStack);
     EtomoDirector.getInstance().getCurrentManager().touch(file);
     try {
     Thread.sleep(1000);
     }
     catch (InterruptedException e) {
     }
     assertTrue(mrcHeader.read());
     */
  }

  public void testWithSpaces() throws IOException, InvalidParameterException,
      SystemProcessException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    //  Create the test directory
    File testDir2 = new File(testDir, testDirectory2);
    if (testDir2.isFile()) {
      testDir2.delete();
    }
    if (!testDir2.exists()) {
      testDir2.mkdirs();
    }
    // Check out the test header stack into the required directories
    TestUtilites.INSTANCE.copyTestFile(testDirPath, testDirectory2, "headerTest.st");

    assertTrue("the file should exist", mrcWithSpaces.read(manager));
    assertEquals("Incorrect column count", 512, mrcWithSpaces.getNColumns());
    assertEquals("Incorrect row count", 512, mrcWithSpaces.getNRows());
    assertEquals("Incorrect section count", 1, mrcWithSpaces.getNSections());
  }

}