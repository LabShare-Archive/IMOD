package etomo.storage.autodoc;

import java.io.File;

import junit.framework.Test;
import junit.framework.TestSuite;

import etomo.JUnitTests;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class AutodocTests {
  public static final String rcsid = "$Id$";
  
  private static final String TEST_DIR = "etomo/storage/autodoc";
  static final File TEST_ROOT_DIR = new File(JUnitTests.TEST_ROOT_DIR, TEST_DIR);
  
  public static Test suite() {
    TestSuite suite = new TestSuite("Tests:  " + TEST_DIR);
    //$JUnit-BEGIN$
    suite.addTestSuite(AutodocTest.class);
    //suite.addTest(new AutodocTest("testCpu"));
    //$JUnit-END$
    return suite;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/11/15 20:41:45  sueh
* <p> bug# 872 Changed the test order to put the classes with fewer dependencies
* <p> first.
* <p>
* <p> Revision 1.1  2006/06/14 00:23:24  sueh
* <p> bug# 852 Test suite for the autodoc package.
* <p> </p>
*/