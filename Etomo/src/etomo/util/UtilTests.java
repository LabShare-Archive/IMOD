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
 * <p> $Log$ </p>
 */
package etomo.util;

import junit.framework.Test;
import junit.framework.TestSuite;

public class UtilTests {
	public static final String rcsid = "$Id$";
	public static Test suite() {
		TestSuite suite = new TestSuite("Test for etomo.util");
		suite.addTestSuite(MRCHeaderTest.class);
		suite.addTestSuite(CircularBufferTest.class);
		return suite;
	}
}
