package etomo.comscript;

import junit.framework.TestCase;

/*
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
 * <p> </p>
 */
public class StringListTest extends TestCase {
   StringList slEmpty;
   
  /**
   * Constructor for StringListTest.
   * @param arg0
   */
  public StringListTest(String arg0) {
    super(arg0);
  }

  /**
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
    slEmpty = new StringList(0);

  }

  /**
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  /**
   * Test the empty string list
   */
  public void testEmptyString() {
    String empty = slEmpty.toString();

    assertTrue(empty.equals(""));
    assertTrue(slEmpty.getNElements() == 0);

    empty = "    ";
    slEmpty.parseString(empty);
    empty = slEmpty.toString();

  }
  /*
   * Test for void StringList(int)
   */
  public void testStringListI() {

  }

  /*
   * Test for void StringList(StringList)
   */
  public void testStringListStringList() {
  }

  public void testSetNElements() {
  }

  public void testSet() {
  }

  public void testGet() {
  }

  public void testGetNElements() {
  }

  /*
   * Test for String toString()
   */
  public void testToString() {
  }

  public void testParseString() {
  }

}
