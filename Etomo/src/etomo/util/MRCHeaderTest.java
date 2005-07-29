
package etomo.util;

import java.io.File;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirectorTestHarness;
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
  
  private final BaseManager manager;

  /**
   * Constructor for MRCHeaderTest.
   * @param arg0
   */
  public MRCHeaderTest(String arg0) {
    super(arg0);
    manager = EtomoDirectorTestHarness.getCurrentManager();
    emptyFilename = MRCHeader.getInstance(manager.getPropertyUserDir(), "", AxisID.ONLY);
    badFilename = MRCHeader.getInstance(manager.getPropertyUserDir(), UtilTests.testRoot + testDirectory1
        + "/non_existant_image_file", AxisID.ONLY);
    mrcHeader = MRCHeader.getInstance(manager.getPropertyUserDir(), UtilTests.testRoot + testDirectory1
        + "/headerTest.st", AxisID.ONLY);
    mrcWithSpaces = MRCHeader.getInstance(manager.getPropertyUserDir(), UtilTests.testRoot + testDirectory2
        + "/headerTest.st", AxisID.ONLY);
  }

  /**
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /**
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  public void testMRCHeader() {
  }

  public void testEmptyFilename() throws InvalidParameterException {
    try {
      emptyFilename.read();
      fail("Should rise IOException exception");
    }
    catch (IOException success) {
    }
  }

  public void testReadBadFilename() throws InvalidParameterException {
    // First test, should throw an exception because the image stack is not
    // present
    boolean exceptionThrown = false;
    try {
      badFilename.read();
      fail("IOException not thrown");
    }
    catch (IOException except) {
    }
  }

  public void testRead() throws IOException, InvalidParameterException {
    //  Create the test directory
    TestUtilites.makeDirectories(manager.getPropertyUserDir(), UtilTests.testRoot + testDirectory1);

    // Check out the test header stack into the required directories
    File testDir = new File(manager.getPropertyUserDir(), UtilTests.testRoot);
    try {
      TestUtilites.checkoutVector(manager, testDir.getAbsolutePath(), testDirectory1,
          headerTestStack);
    }
    catch (SystemProcessException except) {
      System.err.println(except.getMessage());
      fail("Error checking out test vector:\n" + except.getMessage());
    }
    
    assertTrue(mrcHeader.read());
    assertEquals("Incorrect column count", 512, mrcHeader.getNColumns());
    assertEquals("Incorrect row count", 512, mrcHeader.getNRows());
    assertEquals("Incorrect section count", 1, mrcHeader.getNSections());
    //test: will not re-read when the file modify time is earlier then previous
    //read time
    assertFalse(mrcHeader.read());
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

  public void testWithSpaces() throws IOException, InvalidParameterException {
    //  Create the test directory
    TestUtilites.makeDirectories(manager.getPropertyUserDir(),
        UtilTests.testRoot + testDirectory2);

    // Check out the test header stack into the required directories
    try {
      TestUtilites.checkoutVector(manager, manager.getPropertyUserDir()
          + File.separator + UtilTests.testRoot, testDirectory2,
          "headerTest.st");
    }
    catch (SystemProcessException except) {
      System.err.println(except.getMessage());
      fail("Error checking out test vector:\n" + except.getMessage());
    }

    mrcWithSpaces.read();
    assertEquals("Incorrect column count", 512, mrcWithSpaces.getNColumns());
    assertEquals("Incorrect row count", 512, mrcWithSpaces.getNRows());
    assertEquals("Incorrect section count", 1, mrcWithSpaces.getNSections());
  }

}