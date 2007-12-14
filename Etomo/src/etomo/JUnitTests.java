package etomo;

import java.io.File;
import java.util.Enumeration;

import junit.framework.Test;
import junit.framework.TestSuite;
import etomo.comscript.ComScriptTests;
import etomo.process.ProcessTests;
import etomo.storage.StorageTests;
import etomo.storage.autodoc.AutodocTests;
import etomo.type.TypeTests;
import etomo.ui.UITests;
import etomo.util.UtilTests;
import etomo.util.Utilities;

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
  public static final String rcsid = "$Id$";

  public static final File TEST_ROOT_DIR = new File(System
      .getProperty("user.dir"), "JUnitTests");
  public static final String[] ETOMO_ARGUMENTS = { "--test", "--headless",
      "--selftest" };

  public static Test suite() {
    EtomoDirector.main(ETOMO_ARGUMENTS);
    TestSuite suite = new TestSuite("Test for etomo");

    TestSuite testSuite;
    Enumeration tests;
    Object test;

    //$JUnit-BEGIN$
    //$JUnit-END$

    testSuite = (TestSuite) StorageTests.suite();
    tests = testSuite.tests();
    while (tests.hasMoreElements()) {
      test = tests.nextElement();
      if (test instanceof Test) {
        suite.addTest((Test) test);
      }
    }

    testSuite = (TestSuite) TypeTests.suite();
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
    if (!Utilities.isWindowsOS()) {
    testSuite = (TestSuite) AutodocTests.suite();
    tests = testSuite.tests();
    while (tests.hasMoreElements()) {
      test = tests.nextElement();
      if (test instanceof Test) {
        suite.addTest((Test) test);
      }
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

    testSuite = (TestSuite) UITests.suite();
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
 * <p> Revision 1.9  2007/09/07 00:16:16  sueh
 * <p> bug# 989 Call EtomoDirector.main() as early as possible.
 * <p>
 * <p> Revision 1.8  2007/03/03 00:32:46  sueh
 * <p> bug# 973 Added UITests.
 * <p>
 * <p> Revision 1.7  2006/11/15 18:48:47  sueh
 * <p> bug# 872 Changed the test order to put the classes with fewer dependencies
 * <p> first.
 * <p>
 * <p> Revision 1.6  2006/10/10 05:01:20  sueh
 * <p> bug# 931 Adding StorageTests.
 * <p>
 * <p> Revision 1.5  2006/06/14 00:06:04  sueh
 * <p> bug# 852 Adding tests for Autodoc.
 * <p>
 * <p> Revision 1.4  2005/12/23 02:04:12  sueh
 * <p> bug# 675 This class should not depend on EtomoDirector.
 * <p>
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
