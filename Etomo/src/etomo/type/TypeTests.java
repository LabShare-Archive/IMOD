/*
 * Created on Oct 28, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package etomo.type;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class TypeTests {
  static final String testRoot = new String("JUnitTests/etomo/type/");
  
  public static Test suite() {
    TestSuite suite = new TestSuite("Test for etomo.type");
    //$JUnit-BEGIN$
    suite.addTestSuite(ConstMetaDataTest.class);
    suite.addTestSuite(MetaDataTest.class);
    //$JUnit-END$
    return suite;
  }
}
