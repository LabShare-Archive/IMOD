package etomo.process;

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
public class ImodProcessTest extends TestCase {
  public static final String rcsid = "$Id:$";

  public ImodProcessTest(String name) {
    super(name);
  }

  protected void setUp() throws Exception {
    super.setUp();
  }

  protected void tearDown() throws Exception {
    super.tearDown();
  }

  public void testQuickListenerQueue() {
    ImodProcess.QuickListenerQueueTestWrapper queue = new ImodProcess.QuickListenerQueueTestWrapper();
    int numReg = 3;
    assertTrue(
        "Test no longer useful - increase numReg to greater or equal to Stderr.EXPECTED_REGISTRANTS",
        numReg >= queue.getExpectedRegistrants());
    Integer[] reg = new Integer[numReg];
    for (int i = 0; i < numReg; i++) {
      reg[i] = queue.register();
    }
    String[] testValues = new String[] { "a", "b", "c", "d", "e", "f", "g", "h", "i", "j" };
    assertTrue("Not enough test values.", testValues.length >= queue.getPurgeSize());
    for (int i = 0; i < testValues.length; i++) {
      queue.add(testValues[i]);
    }
    String origTestValues = "[a, b, c, d, e, f, g, h, i, j]";
    assertEquals("Quick listener doesn't contain test values.", origTestValues,
        queue.toString());

    assertEquals("Didn't get correct value", "a", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "a", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "b", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "a", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "b", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "c", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        origTestValues, queue.toString());

    assertEquals("Didn't get correct value", "b", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "c", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "d", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "d", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "e", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "f", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        origTestValues, queue.toString());

    assertEquals("Didn't get correct value", "c", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "e", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "f", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "g", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "h", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "i", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        origTestValues, queue.toString());

    assertEquals("Didn't get correct value", "d", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "g", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "h", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "j", queue.getQuickMessage(reg[2]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        origTestValues, queue.toString());

    assertEquals("Didn't get correct value", "e", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "i", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "j", queue.getQuickMessage(reg[1]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        origTestValues, queue.toString());

    assertEquals("Didn't get correct value", "f", queue.getQuickMessage(reg[0]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[1]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    queue.purge();
    String testValuesAfterFirstPurge = "[g, h, i, j]";
    assertEquals("purge should have been done.", testValuesAfterFirstPurge,
        queue.toString());

    assertEquals("Didn't get correct value", "g", queue.getQuickMessage(reg[0]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Queue is too small to purge.", testValuesAfterFirstPurge,
        queue.toString());

    testValues = new String[] { "k", "l", "m", "n", "o", "p","q" };
    assertTrue("Not enough test values.",
        testValues.length >= queue.getPurgeSize() / 2 + 1);
    for (int i = 0; i < testValues.length; i++) {
      queue.add(testValues[i]);
    }
    String newTestValues = "[g, h, i, j, k, l, m, n, o, p, q]";
    assertEquals("Quick listener doesn't contain test values.", newTestValues,
        queue.toString());

    assertEquals("Didn't get correct value", "h", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "k", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "l", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "k", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "l", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "m", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        newTestValues, queue.toString());

    assertEquals("Didn't get correct value", "i", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "m", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "n", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "n", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "o", queue.getQuickMessage(reg[2]));
    assertEquals("Didn't get correct value", "p", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        newTestValues, queue.toString());
    
    assertEquals("Didn't get correct value", "j", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "o", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "p", queue.getQuickMessage(reg[1]));
    assertEquals("Didn't get correct value", "q", queue.getQuickMessage(reg[2]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        newTestValues, queue.toString());
    
    assertEquals("Didn't get correct value", "k", queue.getQuickMessage(reg[0]));
    assertEquals("Didn't get correct value", "q", queue.getQuickMessage(reg[1]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[1]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Not enough values have been read for the purge to be done.",
        newTestValues, queue.toString());
    
    assertEquals("Didn't get correct value", "l", queue.getQuickMessage(reg[0]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[1]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    queue.purge();
    String testValuesAfterSecondPurge = "[m, n, o, p, q]";
    assertEquals("purge should have been done.", testValuesAfterSecondPurge,
        queue.toString());
    
    assertEquals("Didn't get correct value", "m", queue.getQuickMessage(reg[0]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    assertNull("Nothing left to get for this reg", queue.getQuickMessage(reg[2]));
    queue.purge();
    assertEquals("Queue is too small to purge.", testValuesAfterSecondPurge,
        queue.toString());
  }
}
