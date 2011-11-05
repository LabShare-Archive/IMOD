package etomo.util;

import java.io.File;

import etomo.JUnitTests;
import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * <p>Description: Test suite for etomo.util </p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public class UtilTests {
  public static final String rcsid = "$Id$";

  private static final String TEST_DIR = "etomo/util";
  static final File TEST_ROOT_DIR = new File(JUnitTests.TEST_ROOT_DIR, TEST_DIR);

  public static Test suite() {
    TestSuite suite = new TestSuite("Test:  " + TEST_DIR);
    //$JUnit-BEGIN$
    suite.addTestSuite(FileModifiedFlagTest.class);
    suite.addTestSuite(MRCHeaderTest.class);
    suite.addTestSuite(CircularBufferTest.class);
    suite.addTestSuite(RemotePathTest.class);
    suite.addTestSuite(FilePathTest.class);
    //$JUnit-END$
    return suite;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.11  2006/11/15 21:39:27  sueh
 * <p> bug# 872 changed the test order to put tests with fewer dependencies first
 * <p>
 * <p> Revision 1.10  2006/06/14 00:47:55  sueh
 * <p> bug# 852
 * <p>
 * <p> Revision 1.9  2006/01/04 20:29:22  sueh
 * <p> bug# 675 Removed unnecessary import.
 * <p>
 * <p> Revision 1.8  2006/01/04 00:35:00  sueh
 * <p> bug# 675 Added constructors RemotePathTest, so a single test or all tests
 * <p> could be run.
 * <p>
 * <p> Revision 1.7  2005/12/05 21:41:58  sueh
 * <p> bug# 674 Added UtiltiesTest.
 * <p>
 * <p> Revision 1.6  2005/11/15 21:21:14  sueh
 * <p> bug# 733 fixing IMODBuild
 * <p>
 * <p> Revision 1.5  2005/11/15 00:30:46  sueh
 * <p> bug# 733 fixing IMODBuild
 * <p>
 * <p> Revision 1.4  2005/11/10 18:22:26  sueh
 * <p> bug# 758 Placed the root test directory in a File object in JUnitTests.  It is
 * <p> instanciated once so there won't be a problem if the working directory is
 * <p> changed.  Added a root test directory File object to each of the suites,
 * <p> which is based on the JUnitTests root test directory.
 * <p>
 * <p> Revision 1.3  2005/06/20 17:10:08  sueh
 * <p> bug# 522 Added FileModifiedFlag tests.
 * <p>
 * <p> Revision 1.2  2004/11/24 22:17:49  sueh
 * <p> bug# 520 Adding static test root directory name to be used in tests in this
 * <p> package.
 * <p>
 * <p> Revision 1.1  2004/01/13 23:00:55  rickg
 * <p> Initial revision
 * <p> </p>
 */
