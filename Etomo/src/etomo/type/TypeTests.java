package etomo.type;

import java.io.File;

import etomo.JUnitTests;
import etomo.util.Utilities;
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
 * <p> Revision 3.13  2009/09/01 03:16:46  sueh
 * <p> bug# 1222 Added property list tests.
 * <p>
 * <p> Revision 3.12  2007/12/14 18:51:42  sueh
 * <p> Running system program is not working on windows.
 * <p>
 * <p> Revision 3.11  2007/12/10 22:39:42  sueh
 * <p> bug# 1041 Merged ConstMetaDataTest with MetaDataTest.
 * <p>
 * <p> Revision 3.10  2007/03/01 01:27:00  sueh
 * <p> bug# 692 Adding tests
 * <p>
 * <p> Revision 3.9  2006/11/15 20:50:10  sueh
 * <p> bug# 872 Changed the test order to put the classes with fewer dependencies
 * <p> first.
 * <p>
 * <p> Revision 3.8  2006/01/26 22:00:03  sueh
 * <p> bug# 401 Turn ProcessResultDisplay into an interface.  Place the
 * <p> functionality into ProcessResultDisplayState.  This allows a greater
 * <p> variety of classes to be ProcessResultDisplay's.
 * <p>
 * <p> Revision 3.7  2006/01/20 21:09:01  sueh
 * <p> bug# 401 Added tests for ProcessResultDisplay
 * <p>
 * <p> Revision 3.6  2005/11/10 18:10:53  sueh
 * <p> bug# 758 Placed the root test directory in a File object in JUnitTests.  It is
 * <p> instanciated once so there won't be a problem if the working directory is
 * <p> changed.  Added a root test directory File object to each of the suites,
 * <p> which is based on the JUnitTests root test directory.
 * <p>
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
  public static final String rcsid = "$Id$";

  private static final String TEST_DIR = "etomo/type";
  static final File TEST_ROOT_DIR = new File(JUnitTests.TEST_ROOT_DIR, TEST_DIR);

  public static Test suite() {
    TestSuite suite = new TestSuite("Tests:  " + TEST_DIR);
    //$JUnit-BEGIN$
    suite.addTestSuite(ConstEtomoNumberTest.class);
    suite.addTestSuite(ProcessResultDisplayStateTest.class);
    if (!Utilities.isWindowsOS()) {
      suite.addTestSuite(MetaDataTest.class);
    }
    suite.addTestSuite(AxisIDTest.class);
    //suite.addTest(new MetaDataTest("testIsValid"));
    suite.addTestSuite(FortranInputStringPropertyListTest.class);
    suite.addTestSuite(EtomoNumberPropertyListTest.class);
    suite.addTestSuite(PanelHeaderStatePropertyListTest.class);
    suite.addTestSuite(FileTypeTest.class);
    suite.addTestSuite(EtomoVersionTest.class);
    //$JUnit-END$
    return suite;
  }
}
