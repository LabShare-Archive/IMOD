package etomo.type;

import junit.framework.TestCase;

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
public class EtomoVersionTest extends TestCase {
  public static final String rcsid = "$Id:$";

  private final String[] sArray = { "1.8.0.a", "Beta", "2/14/2012", "16:31" };

  public void testSet() {
    String s = sArray[0];
    EtomoVersion ev = EtomoVersion.getDefaultInstance(s);
    assertEquals("Should parse entire string and return parseable string", s,
        ev.toString());
    s = sArray[0] + " " + sArray[1];
    ev = EtomoVersion.getDefaultInstance(s);
    assertEquals("Should parse entire string and return parseable string", s,
        ev.toString());
    s = sArray[0] + " " + sArray[2];
    ev = EtomoVersion.getDefaultInstance(s);
    assertEquals("Should parse entire string and return parseable string", s,
        ev.toString());
    s = sArray[0] + " " + sArray[3];
    ev = EtomoVersion.getDefaultInstance(s);
    assertEquals("Should parse entire string and return parseable string", s,
        ev.toString());
    s = sArray[0] + " " + sArray[1] + " " + sArray[2];
    ev = EtomoVersion.getDefaultInstance(s);
    assertEquals("Should parse entire string and return parseable string", s,
        ev.toString());
    s = sArray[0] + " " + sArray[1] + " " + sArray[3];
    ev = EtomoVersion.getDefaultInstance(s);
    assertEquals("Should parse entire string and return parseable string", s,
        ev.toString());
    s = sArray[0] + " " + sArray[2] + " " + sArray[3];
    ev = EtomoVersion.getDefaultInstance(s);
    assertEquals("Should parse entire string and return parseable string", s,
        ev.toString());
    s = sArray[0] + " " + sArray[1] + " " + sArray[2] + " " + sArray[3];
    ev = EtomoVersion.getDefaultInstance(s);
    assertEquals("Should parse entire string and return parseable string", s,
        ev.toString());
  }

  public void testLt() {
    String s = sArray[0] + " " + sArray[2] + " " + sArray[3];
    EtomoVersion ev = EtomoVersion.getDefaultInstance(s);
    assertTrue("Should ignore the status and date/time",
        ev.equals(EtomoVersion.getDefaultInstance(sArray[0])));
    assertFalse("Should handle a comparison between a number and letter",
        ev.equals("1.8.0.1"));
  }
}
