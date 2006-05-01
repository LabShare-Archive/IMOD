package etomo.ui;

import java.awt.Component;
import java.util.ArrayList;
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
import junit.extensions.jfcunit.finder.AbstractButtonFinder;
import junit.extensions.jfcunit.finder.ComponentFinder;
import junit.extensions.jfcunit.finder.NamedComponentFinder;

import etomo.storage.autodoc.AdocCommand;
import etomo.storage.autodoc.AdocCommandFactory;
import etomo.storage.autodoc.AdocCommandReader;
import etomo.storage.autodoc.Autodoc;
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
 * <p> Revision 1.2  2006/05/01 21:20:31  sueh
 * <p> bug# 787 Removed fiducial diameter, added set.  Handling all variables in
 * <p> UITestAxisSectionCommand.
 * <p>
 * <p> Revision 1.1  2006/04/28 21:07:53  sueh
 * <p> bug# 787 Renamed UIAxisTest to UITestAxis.  Using
 * <p> AdocCommandReader to read the global name/value pairs.  Added mini-
 * <p> buttons.
 * <p>
 * <p> Revision 1.4  2006/04/25 19:35:33  sueh
 * <p> bug# 787 Added testfrom and global waitfor.  Added adoc, copy, exit, tab
 * <p> (tp), waitfor, waitfor.popup, waitfor.process.  Added variables $dataset
 * <p> and $axis.  Removed toggle button.  Removed popup and sleep
 * <p> associated with popups.  Moved the functionality to handle getting
 * <p> secondary autodocs, sections, and pairs to AdocCommandReader.
 * <p>
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
final class UITestAxis implements AdocCommandFactory {
  public static final String rcsid = "$Id$";

  private static final long BUTTON_SLEEP = 100;
  private static final long WAIT_SLEEP = 2000;
  private static final String TEST_FROM_ATTRIB = "testfrom";

  private final Autodoc autodoc;
  private final JFCTestHelper helper;
  private final UITest testCase;
  private final HashSet finishedDialogs = new HashSet();
  private final AxisID axisID;
  private final double startTime;
  private final double duration;
  private final AdocCommandReader reader;
  private final boolean loadedDataFile;
  private final ArrayList variables;

  private boolean verbose = false;
  private long sleep = -1;
  private NamedComponentFinder finder = null;
  private JPanel panel = null;
  private JPanel frame = null;
  private String currentPopupName = null;
  private AbstractButtonFinder popupButtonFinder = null;
  private ComponentFinder componentFinder = null;
  private DialogType globalWaitForDialog = null;
  private boolean turnOver = false;
  private boolean retrievedGlobalAttributes = false;
  private DialogType testFromDialog = null;
  private boolean test = true;
  private boolean stopped = false;
  private UITestAxisSectionCommand command = null;

  UITestAxis(UITest testCase, Autodoc autodoc,
      JFCTestHelper helper, AxisID axisID,
      boolean loadedDataFile, ArrayList variables) {
    this.testCase = testCase;
    this.autodoc = autodoc;
    this.helper = helper;
    this.axisID = axisID;
    this.loadedDataFile = loadedDataFile;
    this.variables = variables;
    startTime = Double.parseDouble(Utilities.getTimestamp());
    testCase.assertTrue(axisID.toString(), "Unable to get timestamp: "
        + startTime, startTime >= 0);
    duration = testCase.getDuration();
    reader = new AdocCommandReader(autodoc,
        UITestAxisSectionCommand.SECTION_TYPE);
    reader.setAxisID(axisID);
    reader.setSecondaryAutodocSourceDir(testCase.getAutodocSourceDir());
  }

  boolean isDone() {
    return reader.isDone();
  }

  /**
   * Returns true if able to remove the dialogType from the finishedDialogs
   * list.  If the test is done and it hasn't been stopped, then it assumes that
   * the dialogType is on the list and return true.
   * @param dialogType
   * @return
   */
  boolean removeFinishedDialog(DialogType dialogType) {
    if (reader.isDone() && !stopped) {
      return true;
    }
    return finishedDialogs.remove(dialogType);
  }

  /**
   * run the test in the uitest-axis.adoc
   * If takeTurns is false, this function will run the entire test
   * TakeTurns will be set to true for dual axis tomograms.
   * If takeTurns is true, this function will exit each time a reader is done
   * or a JButton or JToggleButton is pressed on the current dialog.
   * The state of the class instance must be ready to continue the test from
   * where it left off.
   * @param takeTurns
   */
  void testAxis() {
    if (reader.isDone()) {
      return;
    }
    timeout();
    if (!reader.isReadingSections()) {
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
    while (!reader.isDone()) {
      if (!reader.isReadingSections()) {
        reader.nextSection();
      }
      if (!reader.isReadingCommands()
          && test
          && (testFromDialog == null || testFromDialog.equals(reader.getName()))
          && (!loadedDataFile || !DialogType.SETUP_RECON.equals(reader
              .getName()))) {
        //Since this is a new reader, get the first name/value pair
        command = (UITestAxisSectionCommand) reader.nextCommand(command, this);
        //start a new reader
        getDialog();
      }
      //Go through the commands in the reader if this dialog should be
      //tested (testfrom and datafile attributes).
      while (test
          && !command.isEmpty()
          && (testFromDialog == null || testFromDialog.equals(reader.getName()))
          && (!loadedDataFile || !DialogType.SETUP_RECON.equals(reader
              .getName()))) {
        //once testFromDialog has been found, turn off the testfrom.
        testFromDialog = null;
        boolean getNextCommand = true;
        //test the command if the command is recognized (ignores Version)
        if (command.isKnown()) {
          getNextCommand = testCommand(command);//returns true unless waiting
        }
        if (getNextCommand) {
          //get the next command if not waiting
          command = (UITestAxisSectionCommand) reader
              .nextCommand(command, this);
        }
        if (turnOver) {
          sleep(UITest.DEFAULT_SLEEP);
          return;
        }
      }
      //get the next reader
      if (!reader.isDone()) {
        finishedDialogs.add(DialogType.getInstance(reader.getName()));
      }
      reader.nextSection();
      sleep(WAIT_SLEEP);
      return;
    }
    testCase.assertFalse(reader.getInfo(), "Popups where not handled",
        lookForPopup());
  }

  public AdocCommand newAdocCommand() {
    return new UITestAxisSectionCommand(variables);
  }

  private void timeout() {
    double currentDuration = Double.parseDouble(Utilities.getTimestamp())
        - startTime;
    if (currentDuration > duration) {
      testCase.fail(reader.getInfo(), "exceeded maximum duration, duration="
          + duration + ",currentDuration=" + currentDuration);
    }
  }

  private void setGlobalAttrib() {
    if (reader.isReadingSections()) {
      return;
    }
    UITestAxisCommandFactory factory = new UITestAxisCommandFactory();
    UITestAxisCommand command = (UITestAxisCommand) reader.nextCommand(null,
        factory);
    while (!command.isEmpty()) {
      if (command.isKnown()) {
        UITestAction action = command.getAction();
        if (action == UITestAction.SLEEP) {
          try {
            sleep = Long.parseLong(command.getValue());
          }
          catch (NullPointerException e) {
          }
          catch (NumberFormatException e) {
          }
        }
        else if (action == UITestAction.VERBOSE) {
          verbose = true;
          UIHarness.INSTANCE.setVerbose(true);
          reader.setVerbose();
        }
        else if (action == UITestAction.WAIT_FOR) {
          globalWaitForDialog = command.getDialogType();
          testCase.assertNotNull(reader.getInfo(),
              "Unknown name/value pair format: ", globalWaitForDialog);
        }
        else if (action == UITestAction.TEST_FROM) {
          testFromDialog = command.getDialogType();
          if (testFromDialog == null) {
            test = false;
          }
        }
      }
      command = (UITestAxisCommand) reader.nextCommand(command, factory);
    }
  }

  private boolean testCommand(UITestAxisSectionCommand command) {
    boolean getCommand = true;//true unless waiting
    UITestAction action = command.getAction();
    //handle asserts
    if (action == UITestAction.ASSERT) {
      testAssert(command);
    }
    //handle copy
    else if (action == UITestAction.COPY) {
      copyFile(command);
    }
    //handle stop
    else if (action == UITestAction.STOP) {
      stop();
    }
    //handle sleeping
    else if (action == UITestAction.SLEEP) {
      sleep(command);
    }
    //handle waitfors
    else if (action == UITestAction.WAIT_FOR) {
      getCommand = testWaitFor(command);
    }
    else {
      UITestField field = command.getType();
      if (field == UITestField.BUTTON) {
        clickButton(command);
      }
      else if (field == UITestField.CHECK_BOX) {
        clickCheckBox(command);
      }
      else if (field == UITestField.MINI_BUTTON) {
        clickMiniButton(command);
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
        testCase.fail(reader.getInfo(), "Unknown name/value pair format: "
            + command);
      }
    }
    return getCommand;
  }

  private void getFrame() {
    if (frame != null) {
      return;
    }
    frame = (JPanel) getComponent(JPanel.class, MainFrame.NAME);
    if (axisID == AxisID.SECOND) {
      //for the b axis, open the b axis in the second frame
      clickButton(Utilities
          .convertLabelToName(TomogramProcessPanel.BOTH_AXIS_LABEL), false);
      frame = null;
      frame = (JPanel) getComponent(JPanel.class, SubFrame.NAME);
    }
  }

  /**
   * set the dialog panel to the panel member variable.
   * If the dialog is Setup Tomogram, fill in the dataset name
   */
  private void getDialog() {
    if (frame == null) {
      getFrame();
    }
    String panelName = reader.getName();
    //get the dialog
    panel = (JPanel) getComponent(JPanel.class, panelName, false);
    if (panelName.equals(DialogType.SETUP_RECON.getStorableName())) {
      if (panel == null) {
        return;
      }
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

  private void fillInTextField(UITestAxisSectionCommand command) {
    JTextField textField = (JTextField) getComponent(command, JTextField.class);
    textField.setText(command.getValue());
  }

  private void spinSpinner(UITestAxisSectionCommand command) {
    JSpinner spinner = (JSpinner) getComponent(command, JSpinner.class);
    int subComponent = 0;
    String direction = command.getValue();
    testCase.assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, direction);
    if (direction.equals("up")) {
      subComponent = 1;
    }
    else if (direction.equals("down")) {
      subComponent = 2;
    }
    else {
      testCase.fail(reader.getInfo(), "Unknown name/value pair format: "
          + command);
    }
    helper.enterClickAndLeave(new JSpinnerMouseEventData(testCase, spinner,
        subComponent));
  }

  private void clickRadioButton(UITestAxisSectionCommand command) {
    testCase.assertNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, command.getValue());
    JRadioButton radioButton = (JRadioButton) getComponent(command,
        JRadioButton.class);
    helper.enterClickAndLeave(new MouseEventData(testCase, radioButton));
  }

  private void clickTab(UITestAxisSectionCommand command) {
    testCase.assertNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, command.getValue());
    JTabbedPane tabbedPane = (JTabbedPane) getTabbedPane(command);
    helper.enterClickAndLeave(new JTabbedPaneMouseEventData(testCase,
        tabbedPane, command.getIndex(), 1));
  }

  private void clickCheckBox(UITestAxisSectionCommand command) {
    testCase.assertNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, command.getValue());
    JCheckBox checkBox = (JCheckBox) getComponent(command, JCheckBox.class);
    String value = command.getValue();
    if (value == null) {
      helper.enterClickAndLeave(new MouseEventData(testCase, checkBox));
      return;
    }
    //if the value is set, make the checkbox match the value
    boolean check = false;
    if (value.equals("1")) {
      check = true;
    }
    if (checkBox.isSelected() != check) {
      helper.enterClickAndLeave(new MouseEventData(testCase, checkBox));
    }
  }

  private void clickButton(UITestAxisSectionCommand command) {
    AbstractButton button = (AbstractButton) getButton(command);
    String value = command.getValue();
    if (value == null || button instanceof JButton) {
      helper.enterClickAndLeave(new MouseEventData(testCase, button));
    }
    else {
      //if the value is set, make the checkbox match the value
      boolean select = false;
      if (value.equals("1")) {
        select = true;
      }
      if (button.isSelected() != select) {
        helper.enterClickAndLeave(new MouseEventData(testCase, button));
      }
    }
    sleep(BUTTON_SLEEP);
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
    sleep(BUTTON_SLEEP);
    UIHarness.INSTANCE.toFront(axisID);
  }

  private void clickMiniButton(UITestAxisSectionCommand command) {
    JButton button = (JButton) getComponent(command, JButton.class, false);
    if (button == null) {
      //mini-button may be on the parallel processing panel, so look for it in
      //the frame
      button = (JButton) getComponent(frame, command, JButton.class);
    }
    String value = command.getValue();
    if (value == null) {
      helper.enterClickAndLeave(new MouseEventData(testCase, button));
    }
    else {
      //if the value is set, make the checkbox match the value
      boolean expand = false;
      if (value.equals("1")) {
        expand = true;
      }
      if (ExpandButton.isExpanded(button) != expand) {
        helper.enterClickAndLeave(new MouseEventData(testCase, button));
      }
    }
    sleep(BUTTON_SLEEP);
    UIHarness.INSTANCE.toFront(axisID);
  }

  private long getSleep() {
    if (sleep < 0) {
      return testCase.getSleep();
    }
    return sleep;
  }

  private void sleep(UITestAxisSectionCommand command) {
    String sleepValue = command.getValue();
    int sleep = -1;
    if (sleepValue != null) {
      try {
        sleep = Integer.parseInt(sleepValue);
      }
      catch (NumberFormatException e) {
        testCase.fail(reader.getInfo(), "Unknown name/value pair format "
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

  private boolean lookForDialog(UITestAxisSectionCommand command) {
    DialogType dialogType = command.getDialogType();
    testCase.assertNotNull(reader.getInfo(), "Unknown name/value pair format "
        + command, dialogType);
    return lookForDialog(dialogType);
  }

  /**
   * Waits for a dialog to be finished.  Returns false if it is not finished.
   * @param dialogType
   * @return
   */
  private boolean lookForDialog(DialogType dialogType) {
    testCase.assertNotNull(reader.getInfo(), "Invalid waitfor format",
        dialogType);
    AxisID otherAxisID;
    if (axisID == AxisID.SECOND) {
      otherAxisID = AxisID.FIRST;
    }
    else
      otherAxisID = AxisID.SECOND;
    if (!testCase.removeDialog(otherAxisID, dialogType)) {
      if (testCase.isStopped(otherAxisID)) {
        stop();
      }
      turnOver = true;
      sleep(WAIT_SLEEP);
      return false;
    }
    return true;
  }

  private void stop() {
    reader.setDone();
    stopped = true;
  }

  boolean isStopped() {
    return stopped;
  }

  /**
   * Check whether any process has ended.  Returns false if the process has not
   * ended.  Asserts that the process has ended with the end state specified in
   * command.value.
   * @param command
   * @return
   */
  private boolean lookForProcessEnd(UITestAxisSectionCommand command) {
    ProcessEndState endState = command.getProcessEndState();
    if (endState == null) {
      testCase.fail(reader.getInfo(), "Invalid waitfor process format: "
          + command);
    }
    JButton killProcessButton = (JButton) getComponent(JButton.class, Utilities
        .convertLabelToName(AxisProcessPanel.KILL_BUTTON_LABEL));
    if (killProcessButton.isEnabled()) {
      turnOver = true;
      sleep(WAIT_SLEEP);
      return false;
    }
    //let process bar catch up with kill process button
    sleep(BUTTON_SLEEP);
    //ignore the quick stops between processes
    if (killProcessButton.isEnabled()) {
      turnOver = true;
      sleep(WAIT_SLEEP);
      return false;
    }
    JProgressBar progressBar = (JProgressBar) getComponent(JProgressBar.class,
        ProgressPanel.NAME);
    testCase.assertTrue(reader.getInfo(), command + ": process is not "
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
  private boolean lookForPopup(UITestAxisSectionCommand command) {
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

  private void copyFile(UITestAxisSectionCommand command) {
    testCase.copyFile(command);
  }

  private boolean testWaitFor(UITestAxisSectionCommand command) {
    UITestField field = command.getType();
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

private void testAssert(UITestAxisSectionCommand command) {
    UITestField field = command.getType();
    if (field == UITestField.BUTTON) {
      assertButton(command);
    }
    else if (field == UITestField.CHECK_BOX) {
      assertCheckBox(command);
    }
    else if (field == UITestField.MINI_BUTTON) {
      assertMiniButton(command);
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
    else {
      testCase.fail(reader.getInfo(), "Unknown name/value pair format: "
          + command);
    }
  }  private void assertTextField(UITestAxisSectionCommand command) {
    JTextField textField = (JTextField) getComponent(command, JTextField.class);
    if (command.isEnabled()) {
      assertEnabled(command, textField);
      return;
    }
    assertEquals(command, textField);
  }

  private void assertSpinner(UITestAxisSectionCommand command) {
    JSpinner spinner = (JSpinner) getComponent(command, JSpinner.class);
    if (command.isEnabled()) {
      assertEnabled(command, spinner);
      return;
    }
    testCase.assertEquals(reader.getInfo(), command.toString(), new Integer(
        command.getValue()), (Integer) spinner.getValue());
  }

  private void assertRadioButton(UITestAxisSectionCommand command) {
    JRadioButton radioButton = (JRadioButton) getComponent(command,
        JRadioButton.class);
    if (command.isEnabled()) {
      assertEnabled(command, radioButton);
      return;
    }
    assertSelected(command, radioButton);
  }

  private void assertCheckBox(UITestAxisSectionCommand command) {
    JCheckBox checkBox = (JCheckBox) getComponent(command, JCheckBox.class);
    if (command.isEnabled()) {
      assertEnabled(command, checkBox);
      return;
    }
    assertSelected(command, checkBox);
  }

  private void assertButton(UITestAxisSectionCommand command) {
    AbstractButton button = (AbstractButton) getComponent(command,
        AbstractButton.class);
    if (command.isEnabled()) {
      assertEnabled(command, button);
      return;
    }
    assertSelected(command, button);
  }

  private void assertMiniButton(UITestAxisSectionCommand command) {
    JButton button = (JButton) getComponent(command, JButton.class);
    if (command.isEnabled()) {
      assertEnabled(command, button);
      return;
    }
    //assert expanded
    boolean expanded = false;
    String value = command.getValue();
    if (value != null && value.equals("1")) {
      expanded = true;
    }
    testCase.assertEquals(reader.getInfo(), command.toString(), expanded,
        ExpandButton.isExpanded(button));
  }

  private void assertEnabled(UITestAxisSectionCommand command,
      Component component) {
    testCase.assertTrue(reader.getInfo(), "bad call to assertEnabled", command
        .isEnabled());
    String value = command.getValue();
    boolean enabled = false;
    if (value != null && value.equals("1")) {
      enabled = true;
    }
    //assert enabled
    testCase.assertEquals(reader.getInfo(), command + ":value=" + value,
        enabled, component.isEnabled());
  }

  private void assertSelected(UITestAxisSectionCommand command,
      AbstractButton button) {
    boolean selected = false;
    String value = command.getValue();
    if (value != null && value.equals("1")) {
      selected = true;
    }
    testCase.assertEquals(reader.getInfo(), command.toString(), selected,
        button.isSelected());
  }

  private void assertEquals(UITestAxisSectionCommand command,
      JTextComponent textComponent) {
    String value = command.getValue();
    if (value == null) {
      value = "";
    }
    testCase.assertEquals(reader.getInfo(), command.toString(), value,
        textComponent.getText());
  }

  private AbstractButton getButton(UITestAxisSectionCommand command) {
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

  private JOptionPane getPopup(UITestAxisSectionCommand command) {
    String name = command.getName();
    JOptionPane optionPane = (JOptionPane) getNamedComponentFinder(
        JOptionPane.class, name).find();
    return optionPane;
  }

  private Component getComponent(UITestAxisSectionCommand command,
      Class fieldClass) {
    return getComponent(command, fieldClass, true);
  }

  /**
   * Finds components described by command in the panel.
   * @param command
   * @param fieldClass
   * @param required
   * @return
   */
  private Component getComponent(UITestAxisSectionCommand command,
      Class componentClass, boolean required) {
    return getComponent(panel, command, componentClass, required);
  }

  private Component getComponent(JPanel panel,
      UITestAxisSectionCommand command, Class componentClass) {
    return getComponent(panel, command, componentClass, true);
  }

  /**
   * Finds components described by command
   * @param panel
   * @param command
   * @param componentClass
   * @param required
   * @return
   */
  private Component getComponent(JPanel panel,
      UITestAxisSectionCommand command, Class componentClass, boolean required) {
    String name = command.getName();
    int index = command.getIndex();
    Component component;
    finder = getNamedComponentFinder(componentClass, name);
    if (panel == null) {
      component = finder.find(index);
    }
    else {
      component = finder.find(panel, index);
    }
    if (required) {
      testCase.assertNotNull(reader.getInfo(), command + ":class="
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
    if (frame == null) {
      component = finder.find();
    }
    else {
      component = finder.find(frame, 0);
    }
    if (required) {
      testCase.assertNotNull(reader.getInfo(), "componentClass="
          + componentClass + ",name=" + name, component);
    }
    return component;
  }

  private Component getTabbedPane(UITestAxisSectionCommand command) {
    String name = command.getName();
    Class componentClass = JTabbedPane.class;
    Component component;
    finder = getNamedComponentFinder(componentClass, name);
    if (panel == null) {
      component = finder.find(0);
    }
    else {
      component = finder.find(panel, 0);
    }
    testCase.assertNotNull(reader.getInfo(), command + ":class="
        + componentClass + ",name=" + name + "index=" + 0, component);
    return component;
  }

  private AbstractButton getButton(JOptionPane optionPane, String buttonText) {
    AbstractButton button = (AbstractButton) getAbstractButtonFinder(buttonText)
        .find(optionPane, 0);
    testCase
        .assertNotNull(reader.getInfo(), "buttonText=" + buttonText, button);
    return button;
  }

  private static final class UITestAxisCommandFactory implements
      AdocCommandFactory {
    public AdocCommand newAdocCommand() {
      return new UITestAxisCommand();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2006/05/01 21:20:31  sueh
 * <p> bug# 787 Removed fiducial diameter, added set.  Handling all variables in
 * <p> UITestAxisSectionCommand.
 * <p>
 * <p> Revision 1.1  2006/04/28 21:07:53  sueh
 * <p> bug# 787 Renamed UIAxisTest to UITestAxis.  Using
 * <p> AdocCommandReader to read the global name/value pairs.  Added mini-
 * <p> buttons.
 * <p>
 * <p> Revision 1.4  2006/04/25 19:35:33  sueh
 * <p> bug# 787 Added testfrom and global waitfor.  Added adoc, copy, exit, tab
 * <p> (tp), waitfor, waitfor.popup, waitfor.process.  Added variables $dataset
 * <p> and $axis.  Removed toggle button.  Removed popup and sleep
 * <p> associated with popups.  Moved the functionality to handle getting
 * <p> secondary autodocs, sections, and pairs to AdocCommandReader.
 * <p>
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