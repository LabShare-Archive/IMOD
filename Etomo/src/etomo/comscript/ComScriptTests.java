/*
 * Created on Sep 30, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package etomo.comscript;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class ComScriptTests {

  public static Test suite() {
    TestSuite suite = new TestSuite("Test for etomo.comscript");
    suite.addTestSuite(SetupCombineTest.class);
    suite.addTestSuite(StringListTest.class);
    suite.addTestSuite(NewstParamTest.class);
    //suite.addTest(new NewstParamTest("testParseComScriptCommand"));
    return suite;
  }
}
