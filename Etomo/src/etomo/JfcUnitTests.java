package etomo;

import java.io.File;

import junit.framework.Test;
import junit.framework.TestSuite;
import etomo.ui.UITest;

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

  public static final File TEST_ROOT_DIR = new File(System.getProperty("user.dir"), "JfcUnitTests");
  public static final String[] ETOMO_ARGUMENTS = {"--test", "--selftest"};
  public static final String ETOMO_FILE_ATTRIB_NAME = "etomofile";
  public static final String SECTION_TYPE = "Dialog";
  public static final String SLEEP_ATTRIB_NAME = "sleep";
  public static final String STACK_ATTRIB_NAME = "stack";
  public static final String VECTOR_ATTRIB_NAME = "vector";

  public static Test suite() {
    TestSuite testSuite = new TestSuite("JfcUnit Tests");
    testSuite.addTestSuite(UITest.class);
    return testSuite;
  }
}
/**
 * <p> $Log$ </p>
 */