package etomo.util;

import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002</p>
 * 
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1.2.1  2003/01/24 18:45:05  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/10/03 19:15:51  rickg
 * <p> Initial revision
 * <p> </p>
 */
public class CircularBufferTest extends TestCase {
  CircularBuffer cb;

  /**
   * Constructor for CircularBufferTest.
   * @param arg0
   */
  public CircularBufferTest(String arg0) {
    super(arg0);
  }

  /**
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /**
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  public void testCircularBuffer() {
    int nElements = 10;
    cb = new CircularBuffer(nElements);
    assertEquals("Circular buffer creation", nElements, cb.size());
  }

  public void testSize() {
  }

  public void testPut() {
  }

  public void testGet() {
  }

  public void testSearch() {
  }

}
