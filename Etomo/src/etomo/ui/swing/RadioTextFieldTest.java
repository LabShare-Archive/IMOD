package etomo.ui.swing;

import javax.swing.ButtonGroup;

import etomo.ui.FieldType;

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
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.2  2007/03/07 21:13:40  sueh
 * <p> bug# 981 reformatted
 * <p>
 * <p> Revision 1.1  2007/03/03 01:06:18  sueh
 * <p> bug# 973 Unit test for RadioTextField.
 * <p> </p>
 */
public final class RadioTextFieldTest extends TestCase {
  public static final String rcsid = "$Id$";

  private static final String LABEL = "Radio Text FieldInterface";

  private final ButtonGroup group;
  private final RadioTextField test;

  public RadioTextFieldTest() {
    super();
    group = new ButtonGroup();
    test = RadioTextField.getInstance(FieldType.STRING, LABEL, group);
  }

  /* Test method for 'etomo.ui.RadioTextField.getInstance(String, ButtonGroup)' */
  public void testGetInstance() {
    assertNotNull("Instance was created", test);
    assertNotNull("Container was created", test.getContainer());
    assertEquals("Label was set", test.getLabel(), LABEL);
    assertEquals("Text wasn't set", test.getText(), "");
    assertFalse("Defaults to unselected", test.isSelected());
    validate();
    RadioTextField sameGroup = RadioTextField.getInstance(FieldType.STRING, "Same Group",
        group);
    String error = sameGroup.validate();
    assertNull(error, error);
    validate();
  }

  /* Test method for 'etomo.ui.RadioTextField.setText(String)' */
  public void testSetText() {
    final String text = "test string";
    test.setText(text);
    assertEquals("Text was set", test.getText(), text);
    validate();
  }

  /* Test method for 'etomo.ui.RadioTextField.setEnabled(boolean)' */
  public void testSetEnabled() {
    test.setEnabled(false);
    validate();
    test.setEnabled(false);
    validate();
  }

  /* Test method for 'etomo.ui.RadioTextField.msgSelected()' */
  public void testMsgSelected() {
    test.msgSelected();
    validate();
  }

  /* Test method for 'etomo.ui.RadioTextField.setSelected(boolean)' */
  public void testSetSelected() {
    test.setSelected(true);
    assertTrue("SetSelected worked", test.isSelected());
    RadioTextField sameGroup = RadioTextField.getInstance(FieldType.STRING, "Same Group",
        group);
    sameGroup.setSelected(true);
    assertTrue("SetSelected worked with multiple buttons in the group",
        sameGroup.isSelected());
    assertFalse("Only one button in the group can be selected", test.isSelected());
  }

  private void validate() {
    String error = test.validate();
    assertNull(error, error);
  }
}
