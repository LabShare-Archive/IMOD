/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002-2004</p>
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
 * <p> Revision 3.2  2004/04/07 21:04:45  rickg
 * <p> Javadoc fix
 * <p>
* <p> </p>
 */
package etomo.type;

import junit.framework.Test;
import junit.framework.TestSuite;


public class TypeTests {
  static final String testRoot = new String("JUnitTests/etomo/type/");
  
  public static Test suite() {
    TestSuite suite = new TestSuite("Test for etomo.type");
    //$JUnit-BEGIN$
    suite.addTestSuite(ConstMetaDataTest.class);
    //suite.addTestSuite(MetaDataTest.class);
    //$JUnit-END$
    return suite;
  }
}
