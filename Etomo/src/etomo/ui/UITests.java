package etomo.ui;

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
* <p> Revision 1.1  2007/03/03 01:09:39  sueh
* <p> bug# 973 Unit test suite for etomo.ui.  A lot of gui class functionality can be
* <p> tested in a headless environment.
* <p> </p>
*/
public final class UITests {
  public static  final String  rcsid =  "$Id$";
  
  private static final String TEST_DIR = "etomo/ui";
  static final File TEST_ROOT_DIR = new File(JUnitTests.TEST_ROOT_DIR, TEST_DIR);
  
  public static Test suite() {
    TestSuite suite = new TestSuite("Tests:  " + TEST_DIR);
    //$JUnit-BEGIN$
    suite.addTestSuite(RadioTextFieldTest.class);
    suite.addTestSuite(SetupDialogExpertTest.class);
    //$JUnit-END$
    return suite;
  }
}
