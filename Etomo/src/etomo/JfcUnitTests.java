package etomo;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import junit.framework.Test;
import junit.framework.TestSuite;
import etomo.type.AxisID;
import etomo.ui.Autodoc;
import etomo.ui.Section;
import etomo.ui.SectionLocation;
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

  public static final File TEST_ROOT_DIR = new File(System.getProperty("user.dir"), "UITests");
  public static final String TEST_SECTION_TYPE = "Test";
  
  public static Test suite() throws IOException {
    TestSuite suite = new TestSuite("JfcUnit Tests");
    //get the uitest.adoc
/*    Autodoc.setTest(true);
    Autodoc autodoc = null;
    try {
      autodoc = Autodoc.getInstance(Autodoc.UITEST, AxisID.ONLY);
    }
    catch (FileNotFoundException e) {
      return suite;
    }
    if (autodoc == null) {
      return suite;
    }
    //add each test based on what's in the autodoc Test sections
    SectionLocation testLoc = autodoc.getSectionLocation(TEST_SECTION_TYPE);
    Section test = autodoc.nextSection(testLoc);
    while (test != null) {
*/      suite.addTest(new UITest("test"/*test.getName()*/)); 
/*      test = autodoc.nextSection(testLoc);
    }
*/    return suite;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2005/12/23 02:03:04  sueh
 * <p> bug# 675 Added a main test suite to use JfcUnit to run the UI.
 * <p> </p>
 */