/*
 * Created on Oct 28, 2003
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package etomo;

import junit.framework.Test;
import junit.framework.TestSuite;

import java.util.Enumeration;

import etomo.type.TypeTests;
import etomo.comscript.ComScriptTests;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class EtomoTests {
  public static Test suite() {
    TestSuite suite = new TestSuite("Test for etomo");
    //$JUnit-BEGIN$

    //$JUnit-END$
    TestSuite testSuite = (TestSuite) TypeTests.suite();
    Enumeration tests = testSuite.tests();
    Object test;
    while (tests.hasMoreElements()) {
      test = tests.nextElement();   
      if (test instanceof Test) {
        suite.addTest((Test) test);
      }
    }
    
    testSuite = (TestSuite) ComScriptTests.suite();
    tests = testSuite.tests();
    while (tests.hasMoreElements()) {
      test = tests.nextElement();   
      if (test instanceof Test) {
        suite.addTest((Test) test);
      }
    }
    
    return suite;
  }
  
  
}
