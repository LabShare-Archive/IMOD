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
    suite.addTestSuite(FileModifiedFlagTest.class);
		suite.addTestSuite(MRCHeaderTest.class);
		suite.addTestSuite(CircularBufferTest.class);
    suite.addTestSuite(RemotePathTest.class);
		return suite;
	}
}
/**
 * <p> $Log$
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
