package etomo.comscript;

/**
* <p>Description: </p>
*
* <p>Copyright: Copyright 2004 </p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $Author$
*
* @version $Revision$
*
* <p> $Log$ </p>
*/
import junit.framework.Test;
import junit.framework.TestSuite;

public class ComScriptTests {
  public static final String rcsid = "$$Id$$";

  public static Test suite() {
    TestSuite suite = new TestSuite("Test for etomo.comscript");
    suite.addTestSuite(SetupCombineTest.class);
    suite.addTestSuite(StringListTest.class);
    suite.addTestSuite(NewstParamTest.class);
    suite.addTestSuite(FortranInputStringTest.class);
    suite.addTestSuite(CombineComscriptStateTest.class);
    //suite.addTest(new NewstParamTest("testParseComScriptCommand"));
    return suite;
  }
}
