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
package etomo;

import junit.framework.Test;
import junit.framework.TestSuite;

import java.util.Enumeration;

import etomo.comscript.ComScriptTests;
import etomo.process.ProcessTests;
import etomo.type.TypeTests;
import etomo.util.UtilTests;


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

    testSuite = (TestSuite) ProcessTests.suite();
    tests = testSuite.tests();
    while (tests.hasMoreElements()) {
      test = tests.nextElement();
      if (test instanceof Test) {
        suite.addTest((Test) test);
      }
    }

    testSuite = (TestSuite) UtilTests.suite();
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
