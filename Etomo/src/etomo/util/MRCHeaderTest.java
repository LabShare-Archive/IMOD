package etomo.util;

import java.io.IOException;

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
 * <p> Revision 1.1.2.1  2003/01/24 18:45:05  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/10/03 18:57:38  rickg
 * <p> Initial revision
 * <p> </p>
 */
public class MRCHeaderTest extends TestCase {
  MRCHeader emptyFilename = new MRCHeader("");
  MRCHeader badFilename = new MRCHeader("non_existant_image_file");
  MRCHeader mrcHeader = new MRCHeader("tests/junk.st");

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
      fail("Should rise IOException excpetion");
    }
    catch (IOException success) {
    }
  }

  public void testReadBadFilename() {
    boolean correctException = false;
    //  First test, should throw an exception because the image stack is not present
    try {
      badFilename.read();
    }
    catch (InvalidParameterException except) {
      correctException = true;
    }
    catch (Exception except) {
      correctException = false;
    }
    assertTrue(correctException);

  }

  public void testRead() throws IOException, InvalidParameterException {
    mrcHeader.read();
    assertEquals("Incorrect column count", 512, mrcHeader.getNColumns());
    assertEquals("Incorrect row count", 512, mrcHeader.getNRows());
    assertEquals("Incorrect section count", 1, mrcHeader.getNSections());
  }
}
