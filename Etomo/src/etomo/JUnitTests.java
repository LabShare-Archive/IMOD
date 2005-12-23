package etomo;

import java.io.File;
import java.util.Enumeration;

import junit.framework.Test;
import junit.framework.TestSuite;
import etomo.comscript.ComScriptTests;
import etomo.process.ProcessTests;
import etomo.type.TypeTests;
import etomo.util.UtilTests;

/**
* <p>Description: Collection of all test suites</p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class JUnitTests {
  public static  final String  rcsid =  "$Id$";
  
  public static final File TEST_ROOT_DIR = new File(System.getProperty("user.dir"), "JUnitTests");
  public static final String[] ETOMO_ARGUMENTS = {"--test", "--headless", "--selftest"};
  
  public static Test suite() {
    TestSuite suite = new TestSuite("Test for etomo");
    
    TestSuite testSuite;
    Enumeration tests;
    Object test;
    
    //$JUnit-BEGIN$
    //$JUnit-END$
    
    testSuite = (TestSuite) TypeTests.suite();
    tests = testSuite.tests();
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
/**
* <p> $Log$
* <p> Revision 1.3  2005/11/15 21:21:28  sueh
* <p> bug# 733 fixing IMODBuild
* <p>
* <p> Revision 1.2  2005/11/15 00:29:11  sueh
* <p> bug# 733 fixing IMODBuild
* <p>
* <p> Revision 1.1  2005/11/10 17:58:18  sueh
* <p> bug# 758 Renamed EtomoTests.
* <p> </p>
*/