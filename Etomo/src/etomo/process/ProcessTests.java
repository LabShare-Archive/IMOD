/*
 * Created on Nov 17, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package etomo.process;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ProcessTests {

  public static Test suite() {
    TestSuite suite = new TestSuite("Test for etomo.process");
    //$JUnit-BEGIN$
    suite.addTestSuite(ImodManagerTest.class);
    //suite.addTest(new ImodManagerTest("testFiducialModel"));
    //$JUnit-END$
    return suite;
  }
}
