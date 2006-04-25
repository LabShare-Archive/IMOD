package etomo.ui;

import java.awt.Component;
import java.util.HashSet;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.JToggleButton;
import javax.swing.text.JTextComponent;

import junit.extensions.jfcunit.JFCTestHelper;
import junit.extensions.jfcunit.eventdata.JSpinnerMouseEventData;
import junit.extensions.jfcunit.eventdata.JTabbedPaneMouseEventData;
import junit.extensions.jfcunit.eventdata.MouseEventData;
import junit.extensions.jfcunit.eventdata.StringEventData;
import junit.extensions.jfcunit.finder.AbstractButtonFinder;
import junit.extensions.jfcunit.finder.ComponentFinder;
import junit.extensions.jfcunit.finder.NamedComponentFinder;

import etomo.storage.autodoc.Attribute;
import etomo.storage.autodoc.Autodoc;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.ProcessEndState;
import etomo.type.UITestAction;
import etomo.type.UITestField;
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
 * <p> Revision 1.3  2006/04/06 20:33:13  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 1.2  2006/01/12 17:38:22  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.1  2006/01/11 22:45:29  sueh
 * <p> bug# 675 uitest-axis.adoc level test using JfcUnit
 * <p> </p>
 */
final class UIAxisTest {
  public static final String rcsid = "$Id$";

  private static final long DEFAULT_BUTTON_SLEEP = 100;
  private static final long WAIT_SLEEP = 2000;
  private static final String TEST_FROM_ATTRIB = "testfrom";

  private final String dataset;
  private final Autodoc autodoc;
  private final JFCTestHelper helper;
  private final UITest testCase;
  private final double fiducialDiameter;
  private final HashSet dialogsDone = new HashSet();
  private final AxisID axisID;
  private final double startTime;
  private final double duration;
  private final UITestSection section;
  private UITestCommand command = new UITestCommand();
  private final boolean loadedDataFile;

  private boolean verbose = false;
  private long buttonSleep = DEFAULT_BUTTON_SLEEP;
  private long sleep = -1;
  private NamedComponentFinder finder = null;
  private JPanel panel = null;
  private JPanel framePanel = null;
  private String currentPopupName = null;
  private AbstractButtonFinder popupButtonFinder = null;
  private ComponentFinder componentFinder = null;
  private DialogType globalWaitForDialog = null;
  private boolean turnOver = false;
  private boolean retrievedGlobalAttributes = false;
  private DialogType testFromDialog = null;
  private boolean test = true;

  UIAxisTest(UITest testCase, String dataset, Autodoc autodoc,
      JFCTestHelper helper, double fiducialDiameter, AxisID axisID,
      boolean loadedDataFile) {
    this.testCase = testCase;
    this.dataset = dataset;
    this.autodoc = autodoc;
    this.helper = helper;
    this.fiducialDiameter = fiducialDiameter;
    this.axisID = axisID;
    this.loadedDataFile = loadedDataFile;
    startTime = Double.parseDouble(Utilities.getTimestamp());
    testCase.assertTrue(axisID.toString(), "Unable to get timestamp: "
        + startTime, startTime >= 0);
    duration = testCase.getDuration();
    section = new UITestSection(axisID, testCase.getAutodocSourceDir(), autodoc);
  }

  boolean isDone() {
    return section.isDone();
  }

  boolean removeDialog(DialogType dialogType) {
    return dialogsDone.remove(dialogType);
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
  void testAxis() {
    if (section.isDone()) {
      return;
    }
    timeout();
    if (!section.isStarted()) {
      setGlobalAttrib();
    }
    //make sure not in a wait state
    turnOver = false;
    if (globalWaitForDialog != null) {
      if (!lookForDialog(globalWaitForDialog)) {
        return;
      }
      else {
        globalWaitForDialog = null;
      }
    }
    //process sections
    while (!section.isDone()) {
      if (!section.isStarted()) {
        section.next();
      }
      if (!section.isStartedList()
          && test
          && (testFromDialog == null || section.getDialogType() == testFromDialog)
          && (!loadedDataFile || section.getDialogType() != DialogType.SETUP_RECON)) {
        //Since this is a new section, get the first name/value pair
        command = section.nextCommand(command);
        //start a new section
        getDialog();
      }
      //Go through the commands in the section if this dialog should be
      //tested (testfrom and datafile attributes).
      while (test
          && !command.isEmpty()
          && (testFromDialog == null || section.getDialogType() == testFromDialog)
          && (!loadedDataFile || section.getDialogType() != DialogType.SETUP_RECON)) {
        //once testFromDialog has been found, turn off the testfrom.
        testFromDialog = null;
        boolean getNextCommand = true;
        //test the command if the command is recognized (ignores Version)
        if (command.isKnown()) {
          getNextCommand = testCommand(command);//returns true unless waiting
        }
        if (getNextCommand) {
          //get the next command if not waiting
          command = section.nextCommand(command);
        }
        if (turnOver) {
          sleep(UITest.DEFAULT_SLEEP);
          return;
        }
      }
      //get the next section
      if (!section.isDone()) {
        dialogsDone.add(section.getDialogType());
      }
      section.next();
      sleep(WAIT_SLEEP);
      return;
    }
    testCase.assertFalse(section.getInfo(), "Popups where not handled",
        lookForPopup());
  }

  private void timeout() {
    double currentDuration = Double.parseDouble(Utilities.getTimestamp())
        - startTime;
    if (currentDuration > duration) {
      testCase.fail(section.getInfo(), "exceeded maximum duration, duration="
          + duration + ",currentDuration=" + currentDuration);
    }
  }

  private void setGlobalAttrib() {
    //sleep
    try {
      sleep = Long.parseLong(autodoc
          .getAttribute(UITestAction.SLEEP.toString()).getValue());
    }
    catch (NullPointerException e) {
    }
    catch (NumberFormatException e) {
    }
    //sleep.bn
    try {
      buttonSleep = Long.parseLong(autodoc.getAttribute(
          UITestAction.SLEEP.toString()).getAttribute(
          UITestField.BUTTON.toString()).getValue());
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
      section.setVerbose();
    }
    //wait for dialog
    Attribute waitforAttrib = autodoc.getAttribute(UITestAction.WAIT_FOR
        .toString());
    if (waitforAttrib != null) {
      globalWaitForDialog = DialogType.getInstance(waitforAttrib.getValue());
      testCase.assertNotNull(section.getInfo(),
          "Unknown name/value pair format: " + UITestAction.WAIT_FOR.toString()
              + " " + AutodocTokenizer.DEFAULT_DELIMITER, globalWaitForDialog);
      if (verbose) {
        System.err.println(axisID.toString() + ": "
            + UITestAction.WAIT_FOR.toString() + " "
            + AutodocTokenizer.DEFAULT_DELIMITER + " " + globalWaitForDialog);
      }
    }
    //testfrom
    Attribute testFromAttribute = autodoc.getAttribute(TEST_FROM_ATTRIB);
    if (testFromAttribute != null) {
      testFromDialog = DialogType.getInstance(testFromAttribute.getValue());
      if (verbose) {
        System.err.println(axisID.toString() + ": " + TEST_FROM_ATTRIB + " "
            + AutodocTokenizer.DEFAULT_DELIMITER + " " + testFromDialog);
      }
      //testfrom with no value means don't test this axis
      if (testFromDialog == null) {
        if (verbose) {
          System.err.println(autodoc.getName() + ":  done with test.");
        }
        test = false;
        return;
      }
    }
  }

  private boolean testCommand(UITestCommand command) {
    boolean getNextPair = true;//true unless waiting
    UITestAction action = command.getAction();
    //handle asserts
    if (action == UITestAction.ASSERT) {
      testAssert(command);
    }
    //handle copy
    else if (action == UITestAction.COPY) {
      copyFile(command);
    }
    //handle exiting etomo
    else if (action == UITestAction.EXIT) {
      testCase.exit();
    }
    //handle sleeping
    else if (action == UITestAction.SLEEP) {
      sleep(command);
    }
    //handle waitfors
    else if (action == UITestAction.WAIT_FOR) {
      getNextPair = testWaitFor(command);
    }
    else {
      UITestField field = command.getField();
      if (field == UITestField.BUTTON) {
        clickButton(command);
      }
      else if (field == UITestField.CHECK_BOX) {
        clickCheckBox(command);
      }

      else if (field == UITestField.RADIO_BUTTON) {
        clickRadioButton(command);
      }
      else if (field == UITestField.SPINNER) {
        spinSpinner(command);
      }
      else if (field == UITestField.TABBED_PANE) {
        clickTab(command);
      }
      else if (field == UITestField.TEXT_FIELD) {
        fillInTextField(command);
      }
      else {
        testCase.fail(section.getInfo(), "Unknown name/value pair format: "
            + command);
      }
    }
    return getNextPair;
  }

  private void getFrame() {
    if (framePanel != null) {
      return;
    }
    framePanel = (JPanel) getComponent(JPanel.class, MainFrame.NAME);
    if (axisID == AxisID.SECOND) {
      //for the b axis, open the b axis in the second frame
      clickButton(Utilities
          .convertLabelToName(TomogramProcessPanel.BOTH_AXIS_LABEL), false);
      framePanel = null;
      framePanel = (JPanel) getComponent(JPanel.class, SubFrame.NAME);
    }
  }

  /**
   * set the dialog panel to the panel member variable.
   * If the dialog is Setup Tomogram, fill in the dataset name
   */
  private void getDialog() {
    if (framePanel == null) {
      getFrame();
    }
    String panelName = section.getName();
    //get the dialog
    panel = (JPanel) getComponent(JPanel.class, panelName, false);
    if (panelName.equals(DialogType.SETUP_RECON.getStorableName())) {
      if (panel == null) {
        return;
      }
      //assuming that tomogram setup is already displayed
      //fill in the dataset name using data from the uitest.adoc Test section
      JTextField textField = (JTextField) getComponent(JTextField.class,
          Utilities.convertLabelToName(SetupDialog.DATASET_NAME_LABEL));
      helper.sendString(new StringEventData(testCase, textField, dataset));
      //fill in the fiducial diameter from the uitest.adoc Test section
      textField = (JTextField) getComponent(JTextField.class, Utilities
          .convertLabelToName(SetupDialog.FIDUCIAL_DIAMETER_LABEL));
      helper.sendString(new StringEventData(testCase, textField, String
          .valueOf(fiducialDiameter)));
    }
    else if (panel == null) {
      //need to click the process button to get the dialog to come up
      clickButton(panelName);
      panel = (JPanel) getComponent(JPanel.class, panelName);
    }
    if (axisID != AxisID.SECOND) {
      testCase.moveSubFrame();
    }
  }

  private void fillInTextField(UITestCommand command) {
    JTextField textField = (JTextField) getComponent(command, JTextField.class);
    textField.setText(command.getValue());
  }

  private void spinSpinner(UITestCommand command) {
    JSpinner spinner = (JSpinner) getComponent(command, JSpinner.class);
    int subComponent = 0;
    String direction = command.getValue();
    testCase.assertNotNull(section.getInfo(),
        "Unknown name/value pair format: " + command, direction);
    if (direction.equals("up")) {
      subComponent = 1;
    }
    else if (direction.equals("down")) {
      subComponent = 2;
    }
    else {
      testCase.fail(section.getInfo(), "Unknown name/value pair format: "
          + command);
    }
    helper.enterClickAndLeave(new JSpinnerMouseEventData(testCase, spinner,
        subComponent));
  }

  private void clickRadioButton(UITestCommand command) {
    testCase.assertNull(section.getInfo(), "Unknown name/value pair format: "
        + command, command.getValue());
    JRadioButton radioButton = (JRadioButton) getComponent(command,
        JRadioButton.class);
    helper.enterClickAndLeave(new MouseEventData(testCase, radioButton));
  }

  private void clickTab(UITestCommand command) {
    testCase.assertNull(section.getInfo(), "Unknown name/value pair format: "
        + command, command.getValue());
    JTabbedPane tabbedPane = (JTabbedPane) getTabbedPane(command);
    helper.enterClickAndLeave(new JTabbedPaneMouseEventData(testCase,
        tabbedPane, command.getFieldIndex(), 1));
  }

  private void clickCheckBox(UITestCommand command) {
    testCase.assertNull(section.getInfo(), "Unknown name/value pair format: "
        + command, command.getValue());
    JCheckBox checkBox = (JCheckBox) getComponent(command, JCheckBox.class);
    helper.enterClickAndLeave(new MouseEventData(testCase, checkBox));
  }

  private void clickButton(UITestCommand command) {
    AbstractButton button = (AbstractButton) getButton(command);
    helper.enterClickAndLeave(new MouseEventData(testCase, button));
    String value = command.getValue();
    if (value == null) {
      sleep(buttonSleep);
    }
    else {
      int sleep = Integer.parseInt(value);
      sleep(sleep);
    }
    UIHarness.INSTANCE.toFront(axisID);
  }

  private void clickButton(String name) {
    clickButton(name, true);
  }

  private void clickButton(String name, boolean required) {
    AbstractButton button = (AbstractButton) getButton(name, required);
    if (button != null) {
      helper.enterClickAndLeave(new MouseEventData(testCase, button));
    }
    sleep(buttonSleep);
    UIHarness.INSTANCE.toFront(axisID);
  }

  private long getSleep() {
    if (sleep < 0) {
      return testCase.getSleep();
    }
    return sleep;
  }

  private void sleep(UITestCommand command) {
    String sleepValue = command.getValue();
    int sleep = -1;
    if (sleepValue != null) {
      try {
        sleep = Integer.parseInt(sleepValue);
      }
      catch (NumberFormatException e) {
        testCase.fail(section.getInfo(), "Unknown name/value pair format "
            + command);
      }
    }
    if (sleep < 0) {
      sleep(getSleep());
    }
    else {
      sleep(sleep);
    }
  }

  private boolean lookForDialog(UITestCommand command) {
    DialogType dialogType = command.getDialogType();
    testCase.assertNotNull(section.getInfo(), "Unknown name/value pair format "
        + command, dialogType);
    return lookForDialog(dialogType);
  }

  /**
   * Waits for a dialog to be finished.  Returns false if it is not finished.
   * @param dialogType
   * @return
   */
  private boolean lookForDialog(DialogType dialogType) {
    testCase.assertNotNull(section.getInfo(), "Invalid waitfor format",
        dialogType);
    AxisID otherAxisID;
    if (axisID == AxisID.SECOND) {
      otherAxisID = AxisID.FIRST;
    }
    else
      otherAxisID = AxisID.SECOND;
    if (!testCase.removeDialog(otherAxisID, dialogType)) {
      turnOver = true;
      sleep(WAIT_SLEEP);
      return false;
    }
    return true;
  }

  /**
   * Check whether any process has ended.  Returns false if the process has not
   * ended.  Asserts that the process has ended with the end state specified in
   * command.value.
   * @param command
   * @return
   */
  private boolean lookForProcessEnd(UITestCommand command) {
    ProcessEndState endState = command.getProcessEndState();
    if (endState == null) {
      testCase.fail(section.getInfo(), "Invalid waitfor process format: "
          + command);
    }
    JButton killProcessButton = (JButton) getComponent(JButton.class, Utilities
        .convertLabelToName(AxisProcessPanel.KILL_BUTTON_LABEL));
    if (killProcessButton.isEnabled()) {
      turnOver = true;
      sleep(WAIT_SLEEP);
      return false;
    }
    JProgressBar progressBar = (JProgressBar) getComponent(JProgressBar.class,
        ProgressPanel.NAME);
    testCase.assertTrue(section.getInfo(), command + ": process is not "
        + endState, progressBar.getString().equals(endState.toString()));
    return true;
  }

  private boolean lookForPopup() {
    return getPopup() != null;
  }

  /**
   * Check whether a popup has popped up.  Returns false if the popup has not
   * popped up.  Attempts to close the pop up with the button specified in
   * command.value.
   * @param command
   * @return
   */
  private boolean lookForPopup(UITestCommand command) {
    JOptionPane optionPane = getPopup(command);
    if (optionPane == null) {
      turnOver = true;
      sleep(WAIT_SLEEP);
      return false;
    }
    AbstractButton button = (AbstractButton) getButton(optionPane, command
        .getValue());
    helper.enterClickAndLeave(new MouseEventData(testCase, button));
    return true;
  }

  private void sleep(long time) {
    try {
      Thread.sleep(time);
    }
    catch (InterruptedException e) {
    }
  }

  private void copyFile(UITestCommand command) {
    command.setVariable(UITest.DATASET_ATTRIB, testCase.getDataset());
    command.setVariable("axis", axisID.getExtension());
    String value = command.getValue();
    testCase.copyFile(value, testCase.getCurrentSourceDir());
  }

  private boolean testWaitFor(UITestCommand command) {
    UITestField field = command.getField();
    if (field == null) {
      return lookForDialog(command);
    }
    if (field == UITestField.PROCESS) {
      return lookForProcessEnd(command);
    }
    if (field == UITestField.POPUP) {
      return lookForPopup(command);
    }
    else {
      testCase.fail(null, "Unknown name/value pair format: " + command);
      return false;
    }
  }

  private void testAssert(UITestCommand command) {
    UITestField field = command.getField();
    if (field == UITestField.BUTTON) {
      assertButton(command);
    }
    else if (field == UITestField.RADIO_BUTTON) {
      assertRadioButton(command);
    }
    else if (field == UITestField.TEXT_FIELD) {
      assertTextField(command);
    }
    else if (field == UITestField.SPINNER) {
      assertSpinner(command);
    }
    else if (field == UITestField.CHECK_BOX) {
      assertCheckBox(command);
    }
    else {
      testCase.fail(section.getInfo(), "Unknown name/value pair format: "
          + command);
    }
  }

  private void assertTextField(UITestCommand command) {
    JTextField textField = (JTextField) getComponent(command, JTextField.class);
    if (assertEnabled(command, textField)) {
      return;
    }
    assertEquals(command, textField);
  }

  private void assertSpinner(UITestCommand command) {
    JSpinner spinner = (JSpinner) getComponent(command, JSpinner.class);
    if (assertEnabled(command, spinner)) {
      return;
    }
    testCase.assertEquals(section.getInfo(), command.toString(), new Integer(
        command.getValue()), (Integer) spinner.getValue());
  }

  private void assertRadioButton(UITestCommand command) {
    JRadioButton radioButton = (JRadioButton) getComponent(command,
        JRadioButton.class);
    if (assertEnabled(command, radioButton)) {
      return;
    }
    assertSelected(command, radioButton);
  }

  private void assertCheckBox(UITestCommand command) {
    JCheckBox checkBox = (JCheckBox) getComponent(command, JCheckBox.class);
    if (assertEnabled(command, checkBox)) {
      return;
    }
    assertSelected(command, checkBox);
  }

  private void assertToggleButton(UITestCommand command) {
    JToggleButton toggleButton = (JToggleButton) getComponent(command,
        JToggleButton.class);
    if (assertEnabled(command, toggleButton)) {
      return;
    }
    assertSelected(command, toggleButton);
  }

  private void assertButton(UITestCommand command) {
    assertEnabled(command, (JButton) getButton(command));
  }

  private boolean assertEnabled(UITestCommand command, Component component) {
    if (!command.isEnabled()) {
      return false;
    }
    String value = command.getValue();
    //assert enabled
    boolean enabled = false;
    if (value != null && value.equals("1")) {
      enabled = true;
    }
    else if (value != null && !value.equals("0")) {
      testCase.fail(section.getInfo(), "Unknown name/value pair format: "
          + command);
    }
    testCase.assertEquals(section.getInfo(), command + ":value=" + value,
        enabled, component.isEnabled());
    return true;
  }

  private void assertSelected(UITestCommand command, AbstractButton button) {
    boolean selected = false;
    String value = command.getValue();
    if (value != null && value.equals("1")) {
      selected = true;
    }
    testCase.assertEquals(section.getInfo(), command.toString(), selected,
        button.isSelected());
  }

  private void assertEquals(UITestCommand command, JTextComponent textComponent) {
    String value = command.getValue();
    if (value == null) {
      value = "";
    }
    testCase.assertEquals(section.getInfo(), command.toString(), value,
        textComponent.getText());
  }

  private AbstractButton getButton(UITestCommand command) {
    AbstractButton button = (AbstractButton) getComponent(command,
        JButton.class, false);
    if (button == null) {
      button = (AbstractButton) getComponent(command, JToggleButton.class);
    }
    return button;
  }

  private AbstractButton getButton(String name) {
    return getButton(name, true);
  }

  private AbstractButton getButton(String name, boolean required) {
    AbstractButton button = (AbstractButton) getComponent(JButton.class, name,
        false);
    if (button == null) {
      button = (AbstractButton) getComponent(JToggleButton.class, name,
          required);
    }
    return button;
  }

  private JOptionPane getPopup() {
    JOptionPane optionPane = (JOptionPane) getComponentFinder(JOptionPane.class)
        .find();
    return optionPane;
  }

  private NamedComponentFinder getNamedComponentFinder(Class componentClass,
      String name) {
    if (finder == null) {
      finder = new NamedComponentFinder(componentClass, name);
      finder.setWait(1);
      finder.setOperation(NamedComponentFinder.OP_EQUALS);
    }
    else {
      finder.setComponentClass(componentClass);
      finder.setName(name);
    }
    return finder;
  }

  private AbstractButtonFinder getAbstractButtonFinder(String buttonText) {
    if (popupButtonFinder == null) {
      popupButtonFinder = new AbstractButtonFinder(buttonText);
      popupButtonFinder.setWait(1);
    }
    else {
      popupButtonFinder.setText(buttonText);
    }
    return popupButtonFinder;
  }

  private ComponentFinder getComponentFinder(Class componentClass) {
    if (componentFinder == null) {
      componentFinder = new ComponentFinder(componentClass);
      componentFinder.setWait(1);
    }
    else {
      componentFinder.setComponentClass(componentClass);
    }
    return componentFinder = new ComponentFinder(JOptionPane.class);
  }

  private JOptionPane getPopup(UITestCommand command) {
    String name = command.getFieldName();
    JOptionPane optionPane = (JOptionPane) getNamedComponentFinder(
        JOptionPane.class, name).find();
    return optionPane;
  }

  private Component getComponent(UITestCommand command, Class fieldClass) {
    return getComponent(command, fieldClass, true);
  }

  /**
   * Finds components described by command in the panel.
   * @param command
   * @param fieldClass
   * @param required
   * @return
   */
  private Component getComponent(UITestCommand command, Class componentClass,
      boolean required) {
    String name = command.getFieldName();
    int index = command.getFieldIndex();
    Component component;
    finder = getNamedComponentFinder(componentClass, name);
    if (panel == null) {
      component = finder.find(index);
    }
    else {
      component = finder.find(panel, index);
    }
    if (required) {
      testCase.assertNotNull(section.getInfo(), command + ":class="
          + componentClass + ",name=" + name + ",index=" + index, component);
    }
    return component;
  }

  private Component getComponent(Class componentClass, String name) {
    return getComponent(componentClass, name, true);
  }

  /**
   * Finds components in the framePanel at index 0.
   * @param fieldClass
   * @param name
   * @param required
   * @return
   */
  private Component getComponent(Class componentClass, String name,
      boolean required) {
    Component component;
    finder = getNamedComponentFinder(componentClass, name);
    if (framePanel == null) {
      component = finder.find();
    }
    else {
      component = finder.find(framePanel, 0);
    }
    if (required) {
      testCase.assertNotNull(section.getInfo(), "componentClass="
          + componentClass + ",name=" + name, component);
    }
    return component;
  }

  private Component getTabbedPane(UITestCommand command) {
    String name = command.getFieldName();
    Class componentClass = JTabbedPane.class;
    Component component;
    finder = getNamedComponentFinder(componentClass, name);
    if (panel == null) {
      component = finder.find(0);
    }
    else {
      component = finder.find(panel, 0);
    }
    testCase.assertNotNull(section.getInfo(), command + ":class="
        + componentClass + ",name=" + name + "index=" + 0, component);
    return component;
  }

  private AbstractButton getButton(JOptionPane optionPane, String buttonText) {
    AbstractButton button = (AbstractButton) getAbstractButtonFinder(buttonText)
        .find(optionPane, 0);
    testCase.assertNotNull(section.getInfo(), "buttonText=" + buttonText,
        button);
    return button;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2006/04/06 20:33:13  sueh
 * <p> bug# 808 Moved the function convertLabelToName from UIUtilities to
 * <p> util.Utilities.
 * <p>
 * <p> Revision 1.2  2006/01/12 17:38:22  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.1  2006/01/11 22:45:29  sueh
 * <p> bug# 675 uitest-axis.adoc level test using JfcUnit
 * <p> </p>
 */