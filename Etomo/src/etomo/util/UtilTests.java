/**
 * <p>Description: Test suite for etomo.util </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
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
 * <p> Revision 1.2  2004/11/24 22:17:49  sueh
 * <p> bug# 520 Adding static test root directory name to be used in tests in this
 * <p> package.
 * <p>
 * <p> Revision 1.1  2004/01/13 23:00:55  rickg
 * <p> Initial revision
 * <p> </p>
 */
package etomo.util;

import junit.framework.Test;
import junit.framework.TestSuite;

public class UtilTests {
	public static final String rcsid = "$Id$";
  static final String testRoot = new String("JUnitTests/etomo/util/");
	public static Test suite() {
		TestSuite suite = new TestSuite("Test for etomo.util");
    suite.addTestSuite(FileModifiedFlagTest.class);
		suite.addTestSuite(MRCHeaderTest.class);
		suite.addTestSuite(CircularBufferTest.class);
		return suite;
	}
}
