package etomo.uitest;

import java.io.File;
import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * <p>Description: </p>
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

public final class JfcUnitTests {
  public static final String rcsid = "$Id$";

  public static final File TEST_ROOT_DIR = new File(System.getProperty("user.dir"),
      "UITests");

  public static Test suite() throws IOException {
    TestSuite suite = new TestSuite("JfcUnit Tests");
    suite.addTestSuite(TestRunner.class);
    return suite;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2009/01/20 20:48:06  sueh
 * <p> bug# 1102 Changed uitest suite to TestRunner.
 * <p>
 * <p> Revision 1.1  2008/05/30 21:36:43  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.5  2006/01/04 20:22:53  sueh
 * <p> bug# 675 Moved constants that must be shared by non-test objects to an
 * <p> object which doesn't know about junit.  Overwise junit would have to be in
 * <p> the path for compiling and running EtomoDirector.
 * <p>
 * <p> Revision 1.4  2006/01/03 23:20:21  sueh
 * <p> bug# 675 No longer controlling which tests to work on in this class.  Just
 * <p> add the UITest suite.  To avoid having to restart Etomo, will call each test
 * <p> from a script.
 * <p>
 * <p> Revision 1.3  2005/12/30 21:19:01  sueh
 * <p> bug# 675 Class to run jfcunit tests
 * <p>
 * <p> Revision 1.1  2005/12/23 02:03:04  sueh
 * <p> bug# 675 Added a main test suite to use JfcUnit to run the UI.
 * <p> </p>
 */
