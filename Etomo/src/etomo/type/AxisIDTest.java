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
* <p> $Log$ </p>
*/
public class AxisIDTest extends TestCase {
  public static  final String  rcsid =  "$Id$";
  
  public void testGetInstance() {
    assertNull("Should return null when passed a null string.",AxisID.getInstance(null));
    assertTrue("Should return ONLY when passed an empty string",AxisID.getInstance("")==AxisID.ONLY);
    assertTrue("Should return FIRST when passed 'a'",AxisID.getInstance("a")==AxisID.FIRST);
    assertTrue("Should return SECOND when passed 'b'",AxisID.getInstance("b")==AxisID.SECOND);
    assertNull("Should return null when passed something it doesn't recognized",AxisID.getInstance("c"));
  }
}
