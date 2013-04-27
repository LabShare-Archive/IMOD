package etomo.storage;

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
* 
* <p> $Log$
* <p> Revision 1.3  2006/11/15 20:41:17  sueh
* <p> bug# 872 Changed getParamFileStorableArray to getStorables in the managers.
* <p>
* <p> Revision 1.2  2006/10/16 22:48:39  sueh
* <p> bug# 919  Added JoinInfoFileTest.
* <p>
* <p> Revision 1.1  2006/10/10 05:19:38  sueh
* <p> bug# 931 Test suite for the storage package.
* <p> </p>
*/

public class StorageTests {
  public static final String rcsid = "$Id$";

  private static final String TEST_DIR = "etomo/storage";
  static final File TEST_ROOT_DIR = new File(JUnitTests.TEST_ROOT_DIR, TEST_DIR);

  public static Test suite() {
    TestSuite suite = new TestSuite("Tests:  " + TEST_DIR);
    // $JUnit-BEGIN$
    suite.addTestSuite(LogFileTest.class);
    suite.addTestSuite(ParameterStoreTest.class);
    suite.addTestSuite(JoinInfoFileTest.class);
    suite.addTestSuite(TomogramFileFilterTest.class);
    suite.addTestSuite(DirectiveMapTest.class);
    suite.addTestSuite(DirectiveNameTest.class);
    suite.addTestSuite(ComFileTest.class);
    suite.addTestSuite(DirectiveDescrFileTest.class);
    // suite.addTest(new LogFileTest("testIds"));
    // $JUnit-END$
    return suite;
  }
}
