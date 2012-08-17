package etomo;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class EtomoTests {
  public static final String rcsid = "$Id:$";

  public static Test suite() {
    TestSuite suite = new TestSuite("etomo tests");
    // $JUnit-BEGIN$
    suite.addTestSuite(ProcessSeriesTest.class);
    // $JUnit-END$
    return suite;
  }
}
