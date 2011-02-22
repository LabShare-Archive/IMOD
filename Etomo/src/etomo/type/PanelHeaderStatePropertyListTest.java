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
 * <p> Revision 1.2  2010/04/28 16:32:01  sueh
 * <p> bug# 1344 Removed unnecessary tearDown function override.
 * <p>
 * <p> Revision 1.1  2009/09/01 03:09:51  sueh
 * <p> bug# 1222 Test for PanelHeaderStatePropertyList.
 * <p> </p>
 */
public class PanelHeaderStatePropertyListTest extends TestCase {
  public static final String rcsid = "$Id$";

  private final PanelHeaderStatePropertyList test = new PanelHeaderStatePropertyList();
  private final Properties props = new Properties();
  private final Object id1 = new Object();
  private final Object id2 = new Object();
  private final PanelHeaderState panelHeaderState1 = new PanelHeaderState("Test1");
  private final PanelHeaderState panelHeaderState2 = new PanelHeaderState("Test2");

  public PanelHeaderStatePropertyListTest(String name) {
    super(name);
  }

  protected void setUp() throws Exception {
    super.setUp();
    panelHeaderState1.setMoreLessState("more");
    panelHeaderState2.setOpenCloseState("close");
  }

  public void testPanelHeaderStatePropertyList() {
    assertNotNull("Test should have been created", test);
  }

  public void testAddProperty() {
    test.addProperty(id1, "Test1");
    assertTrue("Should have added an empty element", ((PanelHeaderState) test.get(id1))
        .isNull());

    try {
      test.addProperty(id1, "Test2");
      fail("Should throw exception when an id is used more then once");
    }
    catch (IllegalArgumentException e) {
    }
  }

  public void testStore() {
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.set(id1, panelHeaderState1);
    test.set(id2, panelHeaderState2);
    test.store(props, "");
    assertTrue("Should have stored the Test1 property", props.getProperty(
        "Test1.MoreLess").equals("more"));
    assertTrue("Should have stored the Test2 property", props.getProperty(
        "Test2.OpenClose").equals("close"));
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
    props.setProperty("OldTest1.MoreLess", "less");
    props.setProperty("OldTest2.MoreLess", "more");
    test.load(savedVersion, props, "");
    assertTrue("Should get the old value of more in the Test1 property", test.get(id1)
        .getMoreLessState().equals("more"));

    test.load(oldestVersion, props, "");
    assertTrue("Should get the oldest value of less in the Test1 property", test.get(id1)
        .getMoreLessState().equals("less"));
  }

  public void testLoad() {
    props.setProperty("Test1.MoreLess", "more");
    props.setProperty("Test2.OpenClose", "close");
    test.addProperty(id1, "Test1");
    test.addProperty(id2, "Test2");
    test.load(props, "");
    assertTrue("Should have loaded the Test1 property", test.get(id1).getMoreLessState()
        .equals("more"));
    assertTrue("Should have loaded the Test2 property", test.get(id2).getOpenCloseState()
        .equals("close"));
  }

  public void testGet() {
    test.addProperty(id1, "Test1");
    try {
      test.get(id2);
      fail("Should throw exception when the id doesn't exist");
    }
    catch (IllegalArgumentException e) {
    }

    test.addProperty(id2, "Test2");
    test.set(id1, panelHeaderState1);
    test.set(id2, panelHeaderState2);
    assertTrue("Should have loaded the Test1 property", test.get(id1).getMoreLessState()
        .equals("more"));
    assertTrue("Should have loaded the Test2 property", test.get(id2).getOpenCloseState()
        .equals("close"));
  }
}
