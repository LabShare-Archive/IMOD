package etomo.process;

import java.io.File;

import etomo.JUnitTests;
import junit.framework.Test;
import junit.framework.TestSuite;
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
public class ProcessTests {
  public static  final String  rcsid =  "$Id$";
  
  private static final String TEST_DIR = "etomo/process";
  static final File TEST_ROOT_DIR = new File(JUnitTests.TEST_ROOT_DIR, TEST_DIR);
  
  public static Test suite() {
    TestSuite suite = new TestSuite("Tests:  " + TEST_DIR);
    //$JUnit-BEGIN$
    suite.addTestSuite(ImodManagerTest.class);
    suite.addTestSuite(ProcessMessagesTest.class);
    suite.addTestSuite(ImodProcessTest.class);
    //suite.addTest(new ImodManagerTest("testFiducialModel"));
    //$JUnit-END$
    return suite;
  }
}
/**
* <p> $Log$ </p>
*/
