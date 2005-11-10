package etomo.type;

import java.io.File;

import etomo.JUnitTests;
import junit.framework.Test;
import junit.framework.TestSuite;
/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2005</p>
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
 * <p> Revision 3.5  2005/06/16 20:08:50  sueh
 * <p> bug# 692 ConstEtomoNumberTest.
 * <p>
 * <p> Revision 3.4  2004/12/07 23:45:30  sueh
 * <p> bug# 520 Reinstating metadata tests.
 * <p>
 * <p> Revision 3.3  2004/12/07 22:54:51  sueh
 * <p> bug# 520 Temporarily hidding MetaDataTest.
 * <p>
 * <p> Revision 3.2  2004/04/07 21:04:45  rickg
 * <p> Javadoc fix
 * <p>
 * <p> </p>
*/
public class TypeTests {
  public static  final String  rcsid =  "$Id$";
  
  private static final String TEST_DIR = "etomo/type";
  static final File TEST_ROOT_DIR = new File(JUnitTests.TEST_ROOT_DIR, TEST_DIR);
  
  public static Test suite() {
    TestSuite suite = new TestSuite("Tests:  " + TEST_DIR);
    //$JUnit-BEGIN$
    suite.addTestSuite(ConstEtomoNumberTest.class);
    suite.addTestSuite(ConstMetaDataTest.class);
    suite.addTestSuite(MetaDataTest.class);
    //$JUnit-END$
    return suite;
  }
}
