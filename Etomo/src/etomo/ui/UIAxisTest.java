package etomo.ui;

import java.awt.Component;
import java.io.File;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.text.JTextComponent;

import junit.extensions.jfcunit.JFCTestCase;
import junit.extensions.jfcunit.JFCTestHelper;
import junit.extensions.jfcunit.eventdata.JSpinnerMouseEventData;
import junit.extensions.jfcunit.eventdata.MouseEventData;
import junit.extensions.jfcunit.eventdata.StringEventData;
import junit.extensions.jfcunit.finder.AbstractButtonFinder;
import junit.extensions.jfcunit.finder.ComponentFinder;
import junit.extensions.jfcunit.finder.NamedComponentFinder;

import etomo.storage.autodoc.Attribute;
import etomo.storage.autodoc.Autodoc;
import etomo.storage.autodoc.NameValuePair;
import etomo.storage.autodoc.NameValuePairLocation;
import etomo.storage.autodoc.Section;
import etomo.storage.autodoc.SectionLocation;
import etomo.type.DialogType;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.2  2006/01/12 17:38:22  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.1  2006/01/11 22:45:29  sueh
 * <p> bug# 675 uitest-axis.adoc level test using JfcUnit
 * <p> </p>
 */
final class UIAxisTest {
  public static final String rcsid = "$Id$";

  private final ComponentFinder popupFinder = new ComponentFinder(
      JOptionPane.class);
  private final String dataset;
  private final Autodoc autodoc;
  private final File sourceDir;
  private final SectionLocation sectionLoc;
  private final JFCTestHelper helper;
  private final UITest testCase;
  private final UIField uiField;
  private final double fiducialDiameter;

  private boolean verbose = false;
  private long buttonSleep = 100;
  private long sleep = -1;
  private Section section = null;
  private boolean done = false;
  private boolean newSection = true;
  private NamedComponentFinder finder = null;
  private JPanel panel = null;
  private String currentPopupName = null;
  private AbstractButtonFinder popupButtonFinder = null;
  private NameValuePairLocation pairLoc = null;
  private NameValuePair pair = null;

  UIAxisTest(UITest testCase, String dataset, Autodoc autodoc, File sourceDir,
      JFCTestHelper helper, double fiducialDiameter) {
    this.testCase = testCase;
    this.dataset = dataset;
    this.autodoc = autodoc;
    this.sourceDir = sourceDir;
    this.helper = helper;
    this.fiducialDiameter = fiducialDiameter;
    sectionLoc = autodoc
        .getSectionLocation(UITestConstants.DIALOG_SECTION_TYPE);
    uiField = new UIField();
    popupFinder.setWait(0);
  }

  final boolean isDone() {
    return done;
  }

  /**
   * run the test in the uitest-axis.adoc
   * If takeTurns is false, this function will run the entire test
   * TakeTurns will be set to true for dual axis tomograms.
   * If takeTurns is true, this function will exit each time a section is done
   * or a JButton or JToggleButton is pressed on the current dialog.
   * The state of the class instance must be ready to continue the test from
   * where it left off.
   * @param takeTurns
   */
  final void testAxis(boolean takeTurns) {
    if (done) {
      return;
    }
    //if section is null then this is first time testAxis has been run
    if (section == null) {
      //get the global attributes
      //sleep
      try {
        sleep = Long.parseLong(autodoc.getAttribute(UITest.SLEEP_ATTRIB)
            .getValue());
      }
      catch (NullPointerException e) {
      }
      catch (NumberFormatException e) {
      }
      //sleep.button
      try {
        buttonSleep = Long.parseLong(autodoc.getAttribute(UITest.SLEEP_ATTRIB)
            .getAttribute("button").getValue());
      }
      catch (NullPointerException e) {
      }
      catch (NumberFormatException e) {
      }
      //verbose
      Attribute verboseAttrib = autodoc.getAttribute("verbose");
      if (verboseAttrib != null) {
        verbose = true;
        UIHarness.INSTANCE.setVerbose(true);
      }
      //Since this is the first time testAxis has been run, get the first section
      section = autodoc.nextSection(sectionLoc);
    }
    boolean turnOver = false;
    //process sections
    while (section != null) {
      if (newSection) {
        //start a new section
        startDialog();
        //Since this is a new section, get the first name/value pair
        pairLoc = section.getNameValuePairLocation();
        pair = section.nextNameValuePair(pairLoc);
      }
      //go through name/value pairs in the section
      while (pair != null) {
        if (verbose) {
          System.err.println(pair.getString());
        }
        //set uiField with the current pair
        uiField.set(pair);
        //handle asserts
        if (uiField.isAssertCommand()) {
          assertCommand();
        }
        //handling sleeping
        else if (uiField.isSleepCommand()) {
          sleep(uiField.getValue());
        }
        else {
          //handle popups
          String type = uiField.getFieldType();
          currentPopupName = UIHarness.INSTANCE.getCurrentPopupName();
          if (type.equals(UITestConstants.POPUP_ATTRIB)) {
            closePopup();
          }
          else {
            //if not expecting popup and there is one, fail
            JFCTestCase.assertNull("Not expecting a popup named "
                + currentPopupName, currentPopupName);
            if (type.equals(UITestConstants.BUTTON_ATTRIB)) {
              clickButton();
              turnOver = true;
            }
            //handle fields
            else if (type.equals(UITestConstants.RADIO_BUTTON_ATTRIB)) {
              clickRadioButton();
            }
            else if (type.equals(UITestConstants.TOGGLE_BUTTON_ATTRIB)) {
              clickToggleButton();
              turnOver = true;
            }
            else if (type.equals(UITestConstants.TEXT_FIELD_ATTRIB)) {
              fillInTextField();
            }
            else if (type.equals(UITestConstants.SPINNER_ATTRIB)) {
              spinSpinner();
            }
            else if (type.equals(UITestConstants.CHECK_BOX_ATTRIB)) {
              clickCheckBox();
            }
            else {
              JFCTestCase.fail("Unknown name/value pair format: "
                  + uiField.getString());
            }
          }
        }
        //get the next name/value pair
        pair = section.nextNameValuePair(pairLoc);
        if (takeTurns && turnOver) {
          return;
        }
      }
      //get the next section
      section = autodoc.nextSection(sectionLoc);
      newSection = true;
      if (takeTurns) {
        return;
      }
    }
    //all sections are complete so set done to true
    done = true;
    if (verbose) {
      System.err.println(autodoc.getName() + ":  done with test.");
    }
  }

  /**
   * set the dialog panel to the panel member variable.
   * If the dialog is Setup Tomogram, fill in the dataset name
   */
  private void startDialog() {
    newSection = false;
    if (section == null) {
      return;
    }
    String panelName = section.getName();
    finder = new NamedComponentFinder(JPanel.class, panelName);
    finder.setWait(1);
    panel = (JPanel) finder.find();
    if (verbose) {
      System.err.println(section.getString());
    }
    JFCTestCase.assertNotNull("Unable to get panel: " + panelName, panel);
    if (panelName.equals(DialogType.SETUP.toString())) {
      //assuming that tomogram setup is already displayed
      //fill in the dataset name using data from the uitest.adoc Test section
      finder.setComponentClass(JTextField.class);
      finder.setName(Utilities
          .convertLabelToName(SetupDialog.DATASET_NAME_LABEL));
      JTextField textField = (JTextField) finder.find(panel, 0);
      JFCTestCase.assertNotNull("Unable to find field: "
          + SetupDialog.DATASET_NAME_LABEL, textField);
      helper.sendString(new StringEventData(testCase, textField, dataset));
      //fill in the fiducial diameter from the uitest.adoc Test section
      finder.setComponentClass(JTextField.class);
      finder.setName(Utilities
          .convertLabelToName(SetupDialog.FIDUCIAL_DIAMETER_LABEL));
      textField = (JTextField) finder.find(panel, 0);
      JFCTestCase.assertNotNull("Unable to find field: "
          + SetupDialog.FIDUCIAL_DIAMETER_LABEL, textField);
      helper.sendString(new StringEventData(testCase, textField, String
          .valueOf(fiducialDiameter)));
    }
  }

  private final void fillInTextField() {
    JTextField textField = (JTextField) getField(JTextField.class);
    helper.sendString(new StringEventData(testCase, textField, uiField
        .getValue()));
  }

  private final void spinSpinner() {
    JSpinner spinner = (JSpinner) getField(JSpinner.class);
    int subComponent = 0;
    String direction = uiField.getValue();
    JFCTestCase.assertNotNull("Unknown name/value pair format: "
        + uiField.getString());
    if (direction.equals("up")) {
      subComponent = 1;
    }
    else if (direction.equals("down")) {
      subComponent = 2;
    }
    else {
      JFCTestCase
          .fail("Unknown name/value pair format: " + uiField.getString());
    }
    helper.enterClickAndLeave(new JSpinnerMouseEventData(testCase, spinner,
        subComponent));
  }

  private final void clickRadioButton() {
    if (uiField.getValue() != null) {
      JFCTestCase
          .fail("Unknown name/value pair format: " + uiField.getString());
    }
    JRadioButton radioButton = (JRadioButton) getField(JRadioButton.class);
    helper.enterClickAndLeave(new MouseEventData(testCase, radioButton));
  }

  private final void clickCheckBox() {
    if (uiField.getValue() != null) {
      JFCTestCase
          .fail("Unknown name/value pair format: " + uiField.getString());
    }
    JCheckBox checkBox = (JCheckBox) getField(JCheckBox.class);
    helper.enterClickAndLeave(new MouseEventData(testCase, checkBox));
  }

  private final void clickButton() {
    JButton button = (JButton) getField(JButton.class);
    helper.enterClickAndLeave(new MouseEventData(testCase, button));
    String value = uiField.getValue();
    if (value == null) {
      sleep(buttonSleep);
    }
    else {
      int sleep = Integer.parseInt(value);
      sleep(sleep);
    }
  }

  private final void clickToggleButton() {
    JToggleButton toggleButton = (JToggleButton) getField(JToggleButton.class);
    helper.enterClickAndLeave(new MouseEventData(testCase, toggleButton));
    String value = uiField.getValue();
    if (value == null) {
      sleep(buttonSleep);
    }
    else {
      int sleep = Integer.parseInt(value);
      sleep(sleep);
    }
  }

  private final long getSleep() {
    if (sleep < 0) {
      return testCase.getSleep();
    }
    return sleep;
  }

  private final void buttonSleep(String sleepValue) {
    int sleep = -1;
    if (sleepValue != null) {
      try {
        sleep = Integer.parseInt(sleepValue);
      }
      catch (NumberFormatException e) {
        JFCTestCase.fail("Unknown name/value pair format: "
            + uiField.getString());
      }
    }
    if (sleep < 0) {
      sleep(buttonSleep);
    }
    else {
      sleep(sleep);
    }
  }

  private final void sleep(String sleepValue) {
    int sleep = -1;
    if (sleepValue != null) {
      try {
        sleep = Integer.parseInt(sleepValue);
      }
      catch (NumberFormatException e) {
        JFCTestCase.fail("Unknown name/value pair format: "
            + uiField.getString());
      }
    }
    if (sleep < 0) {
      sleep(getSleep());
    }
    else {
      sleep(sleep);
    }
  }

  private final void closePopup() {
    //get the popup button text to use to close the popup
    //get the timeout
    String[] values = uiField.getValue().split(",");
    JFCTestCase.assertTrue(
        "Popup_button_text is a required value in the popup attribute.",
        values != null && values.length >= 1);
    String buttonText = null;
    long timeout = -1;
    if (values.length == 1) {
      buttonText = values[0];
    }
    else if (values.length == 2) {
      try {
        timeout = Long.parseLong(values[0]);
      }
      catch (NumberFormatException e) {
        JFCTestCase.fail("Unknown name/value pair format: "
            + uiField.getString());
      }
      buttonText = values[1];
      if (timeout <= 0) {
        timeout = getSleep();
      }
    }
    else {
      JFCTestCase
          .fail("Unknown name/value pair format: " + uiField.getString());
    }
    //wait until a popup appears
    int factor = 10;
    long timeoutCount = 0;
    long maxTimeoutCount = timeout < factor ? 1 : timeout / factor;
    while (currentPopupName == null) {
      currentPopupName = UIHarness.INSTANCE.getCurrentPopupName();
      if (++timeoutCount > maxTimeoutCount) {
        break;
      }
      sleep(factor);
    }
    //make sure the correct popup appeared
    String popupName = uiField.getFieldName();
    JFCTestCase.assertNotNull("Expecting a popup.", currentPopupName);
    JFCTestCase.assertEquals("Expecting a popup named " + popupName + ".",
        popupName, currentPopupName);
    //find the popup
    JOptionPane optionPane = (JOptionPane) popupFinder.find();
    JFCTestCase.assertNotNull("Unable to find popup named " + popupName + ".",
        optionPane);
    //find the popup button
    if (popupButtonFinder == null) {
      popupButtonFinder = new AbstractButtonFinder(buttonText);
    }
    else {
      popupButtonFinder.setText(buttonText);
    }
    AbstractButton button = (AbstractButton) popupButtonFinder.find(optionPane,
        0);
    JFCTestCase.assertNotNull("Unable to find the popup button with the label "
        + buttonText + ".", button);
    helper.enterClickAndLeave(new MouseEventData(testCase, button));
  }

  private final void sleep(long time) {
    try {
      Thread.sleep(time);
    }
    catch (InterruptedException e) {
    }
  }

  private final void assertCommand() {
    String type = uiField.getFieldType();
    if (type.equals(UITestConstants.BUTTON_ATTRIB)) {
      assertButton();
    }
    else if (type.equals(UITestConstants.RADIO_BUTTON_ATTRIB)) {
      assertRadioButton();
    }
    else if (type.equals(UITestConstants.TOGGLE_BUTTON_ATTRIB)) {
      assertToggleButton();
    }
    else if (type.equals(UITestConstants.TEXT_FIELD_ATTRIB)) {
      assertTextField();
    }
    else if (type.equals(UITestConstants.SPINNER_ATTRIB)) {
      assertSpinner();
    }
    else if (type.equals(UITestConstants.CHECK_BOX_ATTRIB)) {
      assertCheckBox();
    }
    else {
      JFCTestCase
          .fail("Unknown name/value pair format: " + uiField.getString());
    }
  }

  private final void assertTextField() {
    String value = uiField.getValue();
    JTextField textField = (JTextField) getField(JTextField.class);
    if (assertEnabled(value, textField)) {
      return;
    }
    assertEquals(uiField.getString(), value, textField);
  }

  private final void assertSpinner() {
    String value = uiField.getValue();
    JSpinner spinner = (JSpinner) getField(JSpinner.class);
    if (assertEnabled(value, spinner)) {
      return;
    }
    JFCTestCase.assertEquals(uiField.getString(), new Integer(value),
        (Integer) spinner.getValue());
  }

  private final void assertRadioButton() {
    String value = uiField.getValue();
    JRadioButton radioButton = (JRadioButton) getField(JRadioButton.class);
    if (assertEnabled(value, radioButton)) {
      return;
    }
    assertSelected(value, radioButton);
  }

  private final void assertCheckBox() {
    String value = uiField.getValue();
    JCheckBox checkBox = (JCheckBox) getField(JCheckBox.class);
    if (assertEnabled(value, checkBox)) {
      return;
    }
    assertSelected(value, checkBox);
  }

  private final void assertToggleButton() {
    String value = uiField.getValue();
    JToggleButton toggleButton = (JToggleButton) getField(JToggleButton.class);
    if (assertEnabled(value, toggleButton)) {
      return;
    }
    assertSelected(value, toggleButton);
  }

  private final void assertButton() {
    assertEnabled(uiField.getValue(), (JButton) getField(JButton.class));
  }

  private boolean assertEnabled(String value, Component component) {
    if (!uiField.isAssertEnabled()) {
      return false;
    }
    //assert enabled
    boolean enabled = false;
    if (value != null && value.equals("1")) {
      enabled = true;
    }
    else if (value != null && !value.equals("0")) {
      JFCTestCase
          .fail("Unknown name/value pair format: " + uiField.getString());
    }
    JFCTestCase.assertEquals(uiField.getString() + ":value=" + value, enabled,
        component.isEnabled());
    return true;
  }

  private void assertSelected(String value, AbstractButton button) {
    boolean selected = false;
    if (value != null && value.equals("1")) {
      selected = true;
    }
    JFCTestCase
        .assertEquals(uiField.getString(), selected, button.isSelected());
  }

  private void assertEquals(String assertMessage, String value,
      JTextComponent textComponent) {
    if (value == null) {
      value = "";
    }
    JFCTestCase.assertEquals(assertMessage, value, textComponent.getText());
  }

  private final Component getField(Class fieldClass) {
    JFCTestCase.assertNotNull(finder);//no dialog has found
    String name = uiField.getFieldName();
    finder.setComponentClass(fieldClass);
    finder.setName(name);
    int index = uiField.getFieldIndex();
    Component component = finder.find(panel, index);
    JFCTestCase.assertNotNull(uiField.getString() + ":class=" + fieldClass
        + ",name=" + name + ",index=" + index, component);
    return component;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2006/01/12 17:38:22  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.1  2006/01/11 22:45:29  sueh
 * <p> bug# 675 uitest-axis.adoc level test using JfcUnit
 * <p> </p>
 */