package etomo.type;

import junit.framework.TestCase;

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
* <p> Revision 1.2  2007/03/07 21:08:07  sueh
* <p> bug 692 Added a test.
* <p>
* <p> Revision 1.1  2007/03/01 01:21:07  sueh
* <p> bug# 692 Adding tests
* <p> </p>
*/
public class AxisIDTest extends TestCase {
  public static final String rcsid = "$Id$";

  public void testGetInstance() {
    assertNull("Should return null when passed a null string.", AxisID.getInstance(null));
    assertTrue("Should return ONLY when passed an empty string",
        AxisID.getInstance("") == AxisID.ONLY);
    assertTrue("Should return FIRST when passed 'a'",
        AxisID.getInstance("a") == AxisID.FIRST);
    assertTrue("Should return SECOND when passed 'b'",
        AxisID.getInstance("b") == AxisID.SECOND);
    assertNull("Should return null when passed something it doesn't recognized", AxisID
        .getInstance("c"));
  }

  public void testGetExtension() {
    assertEquals("ONLY returns empty string", AxisID.ONLY.getExtension(), "");
    assertEquals("FIRST returns 'a'", AxisID.FIRST.getExtension(), "a");
    assertEquals("SECOND returns 'b'", AxisID.SECOND.getExtension(), "b");
  }
}
