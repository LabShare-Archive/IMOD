package etomo.util;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;

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
  private static final String testRoot = new String("JUnitTests/etomo/util/");
  private static final String testDirectory1 = new String("Test");
  private static final String testDirectory2 = new String("With Spaces");

  MRCHeader emptyFilename = new MRCHeader("");
  MRCHeader badFilename =
    new MRCHeader(testRoot + testDirectory1 + "/non_existant_image_file");
  MRCHeader mrcHeader =
    new MRCHeader(testRoot + testDirectory1 + "/headerTest.st");
  MRCHeader mrcWithSpaces =
    new MRCHeader(testRoot + testDirectory2 + "/headerTest.st");

  /**
   * Constructor for MRCHeaderTest.
   * @param arg0
   */
  public MRCHeaderTest(String arg0) {
    super(arg0);
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

  public void testReadBadFilename() {
    // Need an application manger to get the IMOD_DIR environment
    // variable
    String[] args = { "--test" };
    ApplicationManager appManager = new ApplicationManager(args);

    // First test, should throw an exception because the image stack is not
    // present
    boolean exceptionThrown = false;
    try {
      badFilename.read();
    }
    catch (Exception except) {
      exceptionThrown = true;
      assertEquals(
        "Incorrect exception thrown",
        "etomo.util.InvalidParameterException",
        except.getClass().getName());
    }
    finally {
      if (!exceptionThrown) {
        fail("Exception not thrown");
      }
    }
  }

  public void testRead() throws IOException, InvalidParameterException {
    //  Create the test directories
    File dir1 =
      new File(System.getProperty("user.dir"), testRoot + testDirectory1);
    if(! dir1.exists()) {
      assertTrue("Creating test directory 1", dir1.mkdirs());
    }
    // Set the working directory to the current test directory
    String originalDirectory = System.getProperty("user.dir");
    System.setProperty("user.dir", testRoot);

    // Check out the test header stack into the required directories
    TestUtilites.checkoutVector(testDirectory1, "headerTest.st");

    //  Switch back to the original working directory
    System.setProperty("user.dir", originalDirectory);  
    
    mrcHeader.read();
    assertEquals("Incorrect column count", 512, mrcHeader.getNColumns());
    assertEquals("Incorrect row count", 512, mrcHeader.getNRows());
    assertEquals("Incorrect section count", 1, mrcHeader.getNSections());
  }

  public void testWithSpaces() throws IOException, InvalidParameterException {
    File dir2 =
      new File(System.getProperty("user.dir"), testRoot + testDirectory2);
    if(! dir2.exists()) {
      assertTrue("Creating test directory 2", dir2.mkdirs());
    }
    
    // Set the working directory to the current test directory
    String originalDirectory = System.getProperty("user.dir");
    System.setProperty("user.dir", testRoot);

    // Check out the test header stack into the required directories
    TestUtilites.checkoutVector(testDirectory2, "headerTest.st");

    //  Switch back to the original working directory
    System.setProperty("user.dir", originalDirectory);
    
    mrcWithSpaces.read();
    assertEquals("Incorrect column count", 512, mrcWithSpaces.getNColumns());
    assertEquals("Incorrect row count", 512, mrcWithSpaces.getNRows());
    assertEquals("Incorrect section count", 1, mrcWithSpaces.getNSections());
  }

}
