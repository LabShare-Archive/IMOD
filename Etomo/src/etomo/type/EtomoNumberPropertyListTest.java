package etomo.type;

import java.util.Properties;

import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.2  2010/04/28 16:23:43  sueh
 * <p> bug# 1344 Removed unnecessary tearDown function override.
 * <p>
 * <p> Revision 1.1  2009/09/01 02:51:11  sueh
 * <p> bug# 1222 Test for EtomoNumberPropertyList.
 * <p> </p>
 */
public class EtomoNumberPropertyListTest extends TestCase {
  public static final String rcsid = "$Id$";

  private final EtomoNumberPropertyList test = new EtomoNumberPropertyList();
  private final Properties props = new Properties();
  private final Object id1 = new Object();
  private final Object id2 = new Object();

  public EtomoNumberPropertyListTest(String arg0) {
    super(arg0);
  }

  protected void setUp() throws Exception {
    super.setUp();
  }

  public void testEtomoNumberPropertyList() {
    assertNotNull("Test should have been created", test);
  }

  public void testAddProperty() {
    test.addProperty(id1, "Test1");
    assertEquals("Should have added an empty element", EtomoNumber.INTEGER_NULL_VALUE,
        test.getDefaultedInt(id1));

    try {
      test.addProperty(id1, "Test2");
      fail("Should throw exception when an id is used more then once");
    }
    catch (IllegalArgumentException e) {
    }
  }

  public void testLoad() {
    //Properties, String
    props.setProperty("Test1", "2");
    props.setProperty("Test2", "4");
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.load(props, "");
    assertEquals("Should have loaded the Test1 property", 2, test.getDefaultedInt(id1));
    assertEquals("Should have loaded the Test2 property", 4, test.getDefaultedInt(id2));

    //EtomoVersion, Properties, String
    test.reset();
    props.remove("Test1");
    props.remove("Test2");
    test.setBackwardCompatibility(id1, EtomoVersion.getDefaultInstance("1.1"),
        "OldestTest");
    test.setBackwardCompatibility(id1, EtomoVersion.getDefaultInstance("1.3"), "OldTest");
    test.setBackwardCompatibility(id2, EtomoVersion.getDefaultInstance("1.2"),
        "OldestTest2");
    props.setProperty("OldestTest", "2");
    props.setProperty("OldTest", "1");
    props.setProperty("OldestTest2", "4");
    test.load(EtomoVersion.getDefaultInstance("1.0"), props, "");
    assertEquals("Should load the oldest value of 2 in the Test1 property", 2, test
        .getDefaultedInt(id1));
    assertEquals("Should load the old value of 4 in the Test2 property", 4, test
        .getDefaultedInt(id2));

    props.setProperty("OldestTest", "1");
    props.setProperty("OldTest", "2");
    test.load(EtomoVersion.getDefaultInstance("1.2"), props, "");
    assertEquals("Should load the old value of 2 in the Test1 property", 2, test
        .getDefaultedInt(id1));
    assertEquals("Should load the old value of 4 in the Test2 property", 4, test
        .getDefaultedInt(id2));

    props.setProperty("OldestTest", "1");
    props.setProperty("OldTest", "5");
  }

  public void testStore() {
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.set(id1, 2);
    test.set(id2, 4);
    test.store(props, "");
    assertTrue("Should have stored the Test1 property", props.getProperty("Test1")
        .equals("2"));
    assertTrue("Should have stored the Test2 property", props.getProperty("Test2")
        .equals("4"));
  }

  public void testGetDefaultedInt() {
    test.addProperty(id1, "Test1");
    try {
      test.getDefaultedInt(id2);
      fail("Should throw exception when the id has not been used");
    }
    catch (IllegalArgumentException e) {
    }

    test.addProperty(id2, "Test2");
    test.set(id1, 2);
    test.set(id2, 4);
    assertEquals("Should get the Test1 property", 2, test.getDefaultedInt(id1));
    assertEquals("Should get the Test2 property", 4, test.getDefaultedInt(id2));
  }

  public void testSet() {
    try {
      test.set(this, 2);
      fail("Should throw exception when asked for a property that doesn't exist");
    }
    catch (IllegalArgumentException e) {
    }

    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.set(id1, 2);
    test.set(id2, 4);
    assertEquals("Should set 2 in the Test1 property", 2, test.getDefaultedInt(id1));
    assertEquals("Should set 4 in the Test2 property", 4, test.getDefaultedInt(id2));
  }

  public void testSetDisplayValue() {
    try {
      test.set(this, 2);
      fail("Should throw exception when asked for a property that doesn't exist");
    }
    catch (IllegalArgumentException e) {
    }

    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.setDisplayValue(id1, 2);
    test.setDisplayValue(id2, 4);
    assertEquals("Should set a default of 2 in the Test1 property", 2, test
        .getDefaultedInt(id1));
    assertEquals("Should set a default of 4 in the Test2 property", 4, test
        .getDefaultedInt(id2));
  }

  public void testSetBackwardCompatibility() {
    EtomoVersion oldestVersion = EtomoVersion.getDefaultInstance("1.3");
    try {
      test.setBackwardCompatibility(id1, oldestVersion, "OldTest1");
      fail("Should throw exception when asked for a property that doesn't exist");
    }
    catch (IllegalArgumentException e) {
    }

    EtomoVersion oldVersion = EtomoVersion.getDefaultInstance("1.4");
    EtomoVersion savedVersion = EtomoVersion.getDefaultInstance("1.4");
    test.addProperty(id1, "Test1");
    test.setBackwardCompatibility(id1, oldestVersion, "OldTest1");
    test.setBackwardCompatibility(id1, oldVersion, "OldTest2");
    props.setProperty("OldTest1", "1");
    props.setProperty("OldTest2", "2");
    test.load(savedVersion, props, "");
    assertEquals("Should get the old value of 2 in the Test1 property", 2, test
        .getDefaultedInt(id1));

    test.load(oldestVersion, props, "");
    assertEquals("Should get the oldest value of 1 in the Test1 property", 1, test
        .getDefaultedInt(id1));
  }

  public void testReset() {
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.set(id1, 2);
    test.set(id2, 4);
    test.reset();
    assertEquals("Should have reset all elements", EtomoNumber.INTEGER_NULL_VALUE, test
        .getDefaultedInt(id1));
    assertEquals("Should have reset all elements", EtomoNumber.INTEGER_NULL_VALUE, test
        .getDefaultedInt(id1));
  }

  public void testEquals() {
    EtomoNumberPropertyList test2 = new EtomoNumberPropertyList();
    assertTrue("Equal when the ids, keys, and values are the same", test.equals(test2));

    test.addProperty(id1, "Test1");
    assertFalse("Not equal when there is a different number of properties", test
        .equals(test2));

    test2.addProperty(id1, "TestA");
    test2.addProperty(id2, "TestB");
    assertFalse("Not equal when there is a different number of properties", test
        .equals(test2));

    test.addProperty(id2, "Test2");
    test.set(id1, 2);
    test.set(id2, 4);
    test2.set(id1, 2);
    test2.set(id2, 4);
    assertTrue("Equal when the ids and values are the same", test.equals(test2));
    assertTrue("Equals should work in both directions", test2.equals(test));

    test2.set(id2, 3);
    assertFalse("Not equal when any value is different", test.equals(test2));

    Object id3 = new Object();
    Object id4 = new Object();
    test.addProperty(id3, "Test3");
    test2.addProperty(id4, "TestC");
    test.set(id3, 6);
    test2.set(id4, 6);
    assertFalse("Not equal when any id is different", test.equals(test2));
  }
}
