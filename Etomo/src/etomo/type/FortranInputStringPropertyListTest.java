package etomo.type;

import java.util.Properties;

import etomo.comscript.FortranInputSyntaxException;
import etomo.storage.LogFile;

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
 * <p> Revision 1.2  2010/04/28 16:29:20  sueh
 * <p> bug# 1344 Removed unnecessary tearDown function override.
 * <p>
 * <p> Revision 1.1  2009/09/01 03:01:05  sueh
 * <p> bug# 1222 Test for FortranInputStringPropertyList.
 * <p> </p>
 */
public class FortranInputStringPropertyListTest extends TestCase {
  public static final String rcsid = "$Id$";

  private final FortranInputStringPropertyList test = new FortranInputStringPropertyList(
      3);
  private final Properties props = new Properties();
  private final Object id1 = new Object();
  private final Object id2 = new Object();

  public FortranInputStringPropertyListTest(String arg0) {
    super(arg0);
  }

  protected void setUp() throws Exception {
    super.setUp();
  }

  public void testFortranInputStringPropertyList() throws FortranInputSyntaxException {
    assertNotNull("Test should have been created", test);

    test.setIntegerType(new boolean[] { true, false, true });
    test.addProperty(id1, "Test1");
    assertFalse("Should call setIntegerType on each property", test.get(id1)
        .isIntegerType(1));
  }

  public void testAddProperty() {
    test.addProperty(id1, "Test1");
    assertTrue("Should have added an empty element", test.get(id1).isNull());

    try {
      test.addProperty(id1, "Test2");
      fail("Should throw exception when an id is used more then once");
    }
    catch (IllegalArgumentException e) {
    }
  }

  public void testLoad() throws LogFile.LockException {
    props.setProperty("Test1", "1,2,3");
    props.setProperty("Test2", "3,4,5");
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.load(props, "");
    assertEquals("Should have loaded the Test1 property", 2, test.get(id1).getInt(1));
    assertEquals("Should have loaded the Test2 property", 4, test.get(id2).getInt(1));
  }

  public void testStore() throws FortranInputSyntaxException {
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.validateAndSet(id1, "1,2,3");
    test.validateAndSet(id2, "3,4,5");
    test.store(props, "");
    assertTrue("Should have stored the Test1 property", props.getProperty("Test1")
        .equals("1.0,2.0,3.0"));
    assertTrue("Should have stored the Test2 property", props.getProperty("Test2")
        .equals("3.0,4.0,5.0"));
  }

  public void testGet() throws FortranInputSyntaxException {
    test.addProperty(id1, "Test1");
    try {
      test.get(id2);
      fail("Should throw exception when the id has not been used");
    }
    catch (IllegalArgumentException e) {
    }

    test.addProperty(id2, "Test2");
    test.validateAndSet(id1, "1,2,3");
    test.validateAndSet(id2, "3,4,5");
    assertEquals("Should get the Test1 property", 2, test.get(id1).getInt(1));
    assertEquals("Should get the Test2 property", 4, test.get(id2).getInt(1));
  }

  public void testValidateAndSet() throws FortranInputSyntaxException {
    try {
      test.validateAndSet(id1, "1,2,3");
      fail("Should throw exception when asked for a property that doesn't exist");
    }
    catch (IllegalArgumentException e) {
    }

    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.validateAndSet(id1, "1,2,3");
    test.validateAndSet(id2, "3,4,5");
    assertEquals("Should set 1,2,3 in the Test1 property", 2, test.get(id1).getInt(1));
    assertEquals("Should set 3,4,5 in the Test2 property", 4, test.get(id2).getInt(1));
  }

  public void testSetIntegerType() throws FortranInputSyntaxException {
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.setIntegerType(new boolean[] { true, false, true });
    assertFalse("Should call setIntegerType on each property", test.get(id1)
        .isIntegerType(1));
    assertFalse("Should call setIntegerType on each property", test.get(id2)
        .isIntegerType(1));
  }

  public void testSetDefault() throws FortranInputSyntaxException {
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.validateAndSet(id1, "1,2,3");
    test.validateAndSet(id2, "3,4,5");
    test.setDefault();
    assertTrue("Should have reset all elements", test.get(id1).isNull());
    assertTrue("Should have reset all elements", test.get(id2).isNull());
  }
}
