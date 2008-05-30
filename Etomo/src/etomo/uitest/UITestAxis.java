package etomo.uitest;

import java.awt.Component;
import java.io.File;
import java.util.ArrayList;
import java.util.HashSet;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
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

import etomo.process.UncaughtException;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.UITestAction;
import etomo.type.UITestField;
import etomo.ui.AxisProcessPanel;
import etomo.ui.ExpandButton;
import etomo.ui.MainFrame;
import etomo.ui.ProgressPanel;
import etomo.ui.SubFrame;
import etomo.ui.TomogramProcessPanel;
import etomo.ui.UIHarness;
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
 * <p> Revision 1.1  2008/05/30 21:52:59  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.13  2008/01/22 18:04:30  sueh
 * <p> bug# 1068 Increase wait time in finders.
 * <p>
 * <p> Revision 1.12  2007/03/21 19:48:25  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.  Moved AdocCommand
 * <p> classes out of the autodoc package.
 * <p>
 * <p> Revision 1.11  2006/10/24 23:35:04  sueh
 * <p> bug# 947 Added waitfor.process.process_name =
 * <p>
 * <p> Revision 1.10  2006/10/11 10:12:46  sueh
 * <p> bug# 938 Making ThreadGroup dependent on UncaughtException instead of
 * <p> UITest, so that it does not require JfcUnit to compile.
 * <p>
 * <p> Revision 1.9  2006/10/10 05:26:10  sueh
 * <p> bug# 931 Added assert file exists functionality.  Failing on an uncaught
 * <p> exception.
 * <p>
 * <p> Revision 1.8  2006/08/28 18:27:16  sueh
 * <p> bug# 923 Changed the uitest source attribute to filedir.  Global filedir is an
 * <p> absolute file path.
 * <p>
 * <p> Revision 1.7  2006/08/18 23:25:44  sueh
 * <p> bug# 852 clickCheckBox:  remove assert that prevents cb. from having a value
 * <p>
 * <p> Revision 1.6  2006/08/08 18:20:15  sueh
 * <p> bug# 852 Changing the Dialog adoc command to mean the function
 * <p> location.  Added the function command, to run a function section.
 * <p>
 * <p> Revision 1.5  2006/06/27 22:35:54  sueh
 * <p> bug# 852 Implementing callFunction().
 * <p>
 * <p> Revision 1.4  2006/06/14 00:42:29  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.  Renamed the section level command
 * <p> objects to make them less generic.
 * <p>
 * <p> Revision 1.3  2006/05/01 21:24:02  sueh
 * <p> bug# 787 removed commented out code
 * <p>
 * <p> Revision 1.2  2006/05/01 21:20:31  sueh
 * <p> bug# 787 Removed fiducial diameter, added set.  Handling all variables in
 * <p> UITestAxisDialogCommand.
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
final class UITestAxis implements UITestCommandFactory {
  public static final String rcsid = "$Id$";

  private static final long BUTTON_SLEEP = 100;
  private static final long WAIT_SLEEP = 2000;
  private static final String TEST_FROM_ATTRIB = "testfrom";

  private final ReadOnlyAutodoc autodoc;
  private final JFCTestHelper helper;
  private final UITest testCase;
  private final HashSet finishedDialogs = new HashSet();
  private final AxisID axisID;
  private final double startTime;
  private final double duration;
  private final CommandReader reader;
  private final boolean loadedDataFile;
  private final ArrayList variables;
  private final InterfaceSection interfaceSection;

  private boolean verbose = false;
  private long sleep = -1;
  private NamedComponentFinder finder = null;
  private JComponent panel = null;
  private JPanel frame = null;
  private String currentPopupName = null;
  private AbstractButtonFinder popupButtonFinder = null;
  private ComponentFinder componentFinder = null;
  private String globalWaitForDialog = null;
  private boolean turnOver = false;
  private boolean retrievedGlobalAttributes = false;
  private String testFromDialog = null;
  private boolean test = true;
  private boolean stopped = false;
  private DialogSectionCommand command = null;

  UITestAxis(UITest testCase, ReadOnlyAutodoc autodoc, JFCTestHelper helper,
      AxisID axisID, boolean loadedDataFile, ArrayList variables,
      InterfaceSection interfaceSection) {
    this.testCase = testCase;
    this.autodoc = autodoc;
    this.helper = helper;
    this.axisID = axisID;
    this.loadedDataFile = loadedDataFile;
    this.variables = variables;
    this.interfaceSection = interfaceSection;
    startTime = Double.parseDouble(Utilities.getTimestamp());
    testCase.assertTrue(axisID.toString(), "Unable to get timestamp: "
        + startTime, startTime >= 0);
    duration = testCase.getDuration();
    reader = new CommandReader(autodoc, UITestAction.DIALOG.toString());
    reader.setAxisID(axisID);
    reader.setFunctionLocationSourceDir(UITest.SOURCE_DIR);
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
  boolean removeFinishedDialog(String dialogSectionName) {
    if (reader.isDone() && !stopped) {
      return true;
    }
    return finishedDialogs.remove(dialogSectionName);
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
    if (isUncaughtException()) {
      return;
    }
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
    while (!isUncaughtException() && !reader.isDone()) {
      if (!reader.isReadingSections()) {
        reader.nextSection();
      }
      if (!reader.isReadingStatements()
          && test
          && (testFromDialog == null || testFromDialog.equals(reader.getName()))
          && (!loadedDataFile || !interfaceSection.isDefaultDialog(reader
              .getName()))) {
        //Since this is a new reader, get the first name/value pair
        command = (DialogSectionCommand) reader.nextCommand(command, this);
        //start a new reader
        getDialog();
      }
      //Go through the commands in the reader if this dialog should be
      //tested (testfrom and datafile attributes).
      while (!isUncaughtException()
          && test
          && !command.isEmpty()
          && (testFromDialog == null || testFromDialog.equals(reader.getName()))
          && (!loadedDataFile || !interfaceSection.isDefaultDialog(reader
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
          command = (DialogSectionCommand) reader.nextCommand(command, this);
        }
        if (turnOver) {
          sleep(UITest.DEFAULT_SLEEP);
          return;
        }
      }
      //get the next reader
      if (!reader.isDone()) {
        finishedDialogs.add(reader.getName());
      }
      reader.nextSection();
      sleep(WAIT_SLEEP);
      return;
    }

    testCase.assertFalse(reader.getInfo(), "Popups where not handled",
        lookForPopup());
  }

  public UITestCommand newCommand() {
    return new DialogSectionCommand(variables);
  }

  private void timeout() {
    double currentDuration = Double.parseDouble(Utilities.getTimestamp())
        - startTime;
    if (currentDuration > duration) {
      testCase.fail(reader.getInfo(), "exceeded maximum duration, duration="
          + duration + ",currentDuration=" + currentDuration);
    }
  }

  private boolean isUncaughtException() {
    String message;
    if ((message = UncaughtException.INSTANCE.getThrowableString()) != null) {
      String info = null;
      if (reader != null) {
        info = reader.getInfo();
      }
      String commandString = null;
      if (command != null) {
        commandString = command.toString();
      }
      testCase.fail(info, command.toString(), message);
      return true;
    }
    return false;
  }

  private void setGlobalAttrib() {
    if (reader.isReadingSections()) {
      return;
    }
    CommandFactory factory = new CommandFactory();
    TestAutodocCommand command = (TestAutodocCommand) reader.nextCommand(null,
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
          globalWaitForDialog = command.getValue();
          testCase.assertNotNull(reader.getInfo(),
              "Unknown name/value pair format: ", globalWaitForDialog);
        }
        else if (action == UITestAction.TEST_FROM) {
          testFromDialog = command.getValue();
          if (testFromDialog == null) {
            test = false;
          }
        }
      }
      command = (TestAutodocCommand) reader.nextCommand(command, factory);
    }
  }

  private boolean testCommand(DialogSectionCommand command) {
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
    frame = (JPanel) getComponent(JPanel.class, MainFrame.NAME, true, false);
    if (axisID == AxisID.SECOND) {
      //for the b axis, open the b axis in the second frame
      clickButton(Utilities
          .convertLabelToName(TomogramProcessPanel.BOTH_AXIS_LABEL), false);
      frame = null;
      frame = (JPanel) getComponent(JPanel.class, SubFrame.NAME, true, false);
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
    UITestField navigationField = interfaceSection.getNavigationField();
    if (navigationField == UITestField.BUTTON) {
      panel = (JPanel) getComponent(JPanel.class, panelName, false, false);
    }
    else if (navigationField == UITestField.TABBED_PANE) {
      panel = (JTabbedPane) getComponent(JTabbedPane.class, panelName,
          interfaceSection.getIndex(panelName), false, false);
    }
    else {
      testCase.fail(panelName, "Unknown Interface section navigation value, "
          + navigationField);
    }
    //Button navigation interfaces have a default dialog which does not have a
    //button associated with it, so the default dialog of a button navigation
    //cannot be brought up.  The default dialog for tabbed navigation can
    //be brought up because the default tab is always there.
    if (navigationField == UITestField.BUTTON
        && interfaceSection.isDefaultDialog(panelName)) {
      if (panel == null) {
        return;
      }
    }
    else if (panel == null) {
      //need to click the process button to get the dialog to come up
      if (navigationField == UITestField.BUTTON) {
        clickButton(panelName);
        panel = (JPanel) getComponent(JPanel.class, panelName, true, false);
      }
      else if (navigationField == UITestField.TABBED_PANE) {
        clickTab(panelName);
        panel = (JTabbedPane) getComponent(JTabbedPane.class, panelName,
            interfaceSection.getIndex(panelName), true, false);
      }
    }
    if (axisID != AxisID.SECOND) {
      testCase.moveSubFrame();
    }
  }

  private void fillInTextField(DialogSectionCommand command) {
    JTextField textField = (JTextField) getComponent(command, JTextField.class,
        true);
    textField.setText(command.getValue());
  }

  private void spinSpinner(DialogSectionCommand command) {
    JSpinner spinner = (JSpinner) getComponent(command, JSpinner.class, true);
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

  private void clickRadioButton(DialogSectionCommand command) {
    testCase.assertNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, command.getValue());
    JRadioButton radioButton = (JRadioButton) getComponent(command,
        JRadioButton.class, true);
    helper.enterClickAndLeave(new MouseEventData(testCase, radioButton));
  }

  private void clickTab(String name) {
    JTabbedPane tabbedPane = (JTabbedPane) getTabbedPane(name);
    if (tabbedPane != null) {
      helper.enterClickAndLeave(new JTabbedPaneMouseEventData(testCase,
          tabbedPane, 0, 1));
    }
    sleep(BUTTON_SLEEP);
    UIHarness.INSTANCE.toFront(axisID);
  }

  private void clickTab(DialogSectionCommand command) {
    testCase.assertNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, command.getValue());
    JTabbedPane tabbedPane = (JTabbedPane) getTabbedPane(command);
    helper.enterClickAndLeave(new JTabbedPaneMouseEventData(testCase,
        tabbedPane, command.getIndex(), 1));
  }

  /**
   * 
   * @param name
   * @return
   */
  private Component getTabbedPane(String name) {
    Class componentClass = JTabbedPane.class;
    Component component;
    finder = getNamedComponentFinder(componentClass, name);
    if (panel == null) {
      component = finder.find(0);
    }
    else {
      component = finder.find(panel, 0);
    }
    return component;
  }

  private Component getTabbedPane(DialogSectionCommand command) {
    String name = command.getName();
    Component component = getTabbedPane(name);
    testCase.assertNotNull(reader.getInfo(), command + ":class="
        + JTabbedPane.class + ",name=" + name + "index=" + 0, component);
    return component;
  }

  private void clickCheckBox(DialogSectionCommand command) {
    JCheckBox checkBox = (JCheckBox) getComponent(command, JCheckBox.class,
        true);
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

  private void clickButton(DialogSectionCommand command) {
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

  private void clickMiniButton(DialogSectionCommand command) {
    JButton button = (JButton) getComponent(command, JButton.class, false);
    if (button == null) {
      //mini-button may be on the parallel processing panel, so look for it in
      //the frame
      button = (JButton) getComponent(frame, command, JButton.class, true);
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

  private void sleep(DialogSectionCommand command) {
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

  private boolean lookForDialog(DialogSectionCommand command) {
    String waitforSectionName = command.getValue();
    testCase.assertNotNull(reader.getInfo(), "Unknown name/value pair format "
        + command, waitforSectionName);
    return lookForDialog(waitforSectionName);
  }

  /**
   * Waits for a dialog to be finished.  Returns false if it is not finished.
   * @param dialogType
   * @return
   */
  private boolean lookForDialog(String waitforSectionName) {
    testCase.assertNotNull(reader.getInfo(), "Invalid waitfor format",
        waitforSectionName);
    AxisID otherAxisID;
    if (axisID == AxisID.SECOND) {
      otherAxisID = AxisID.FIRST;
    }
    else
      otherAxisID = AxisID.SECOND;
    if (!testCase.removeDialog(otherAxisID, waitforSectionName)) {
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
   * This function will only be reliable when used to find the last process in a
   * sequence or a single process.  This is because there is a delay between
   * checking the kill process button and looking at the process
   * bar, so there is no way to know if the next process has already started.
   * @param command
   * @return
   */
  private boolean lookForProcessEnd(DialogSectionCommand command) {
    ProcessEndState endState = command.getProcessEndState();
    if (endState == null) {
      testCase.fail(reader.getInfo(), "Invalid waitfor process format: "
          + command);
    }
    ProcessName processName = command.getProcessName();
    JButton killProcessButton;
    if (processName == null) {
      //If the command process name is null, get the kill process button no
      //matter what its name is.
      killProcessButton = (JButton) getComponent(JButton.class, Utilities
          .convertLabelToName(AxisProcessPanel.KILL_BUTTON_LABEL), true, true);
    }
    else {
      //If the command process name is set, get the kill process button only if
      //the name matches the process.
      //This is not required since there may be multiple processes.
      killProcessButton = (JButton) getComponent(JButton.class, Utilities
          .convertLabelToName(AxisProcessPanel.KILL_BUTTON_LABEL, processName),
          false, false);
    }
    if (killProcessButton == null || killProcessButton.isEnabled()) {
      turnOver = true;
      sleep(WAIT_SLEEP);
      return false;
    }
    //Let process bar catch up with kill process button.
    sleep(BUTTON_SLEEP);
    /*
     //ignore the quick stops between processes
     if (killProcessButton.isEnabled()) {
     turnOver = true;
     sleep(WAIT_SLEEP);
     return false;
     }*/
    JProgressBar progressBar = (JProgressBar) getComponent(JProgressBar.class,
        ProgressPanel.NAME, false, false);
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
  private boolean lookForPopup(DialogSectionCommand command) {
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

  private void copyFile(DialogSectionCommand command) {
    testCase.copyFile(command);
  }

  private boolean testWaitFor(DialogSectionCommand command) {
    UITestField field = command.getType();
    if (field == UITestField.DIALOG) {
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

  private void testAssert(DialogSectionCommand command) {
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
    else if (field == UITestField.FILE) {
      assertFileExists(command);
    }
    else {
      testCase.fail(reader.getInfo(), "Unknown name/value pair format: "
          + command);
    }
  }

  private void assertTextField(DialogSectionCommand command) {
    JTextField textField = (JTextField) getComponent(command, JTextField.class,
        true);
    if (command.getTest() == UITestTest.ENABLED) {
      assertEnabled(command, textField);
      return;
    }
    assertEquals(command, textField);
  }

  private void assertFileExists(DialogSectionCommand command) {
    sleep(1000);
    File file = new File(System.getProperty("user.dir"), command.getValue());
    testCase.assertTrue(reader.getInfo(), command.toString(), file.exists());
  }

  private void assertSpinner(DialogSectionCommand command) {
    JSpinner spinner = (JSpinner) getComponent(command, JSpinner.class, true);
    if (command.getTest() == UITestTest.ENABLED) {
      assertEnabled(command, spinner);
      return;
    }
    testCase.assertEquals(reader.getInfo(), command.toString(), new Integer(
        command.getValue()), (Integer) spinner.getValue());
  }

  private void assertRadioButton(DialogSectionCommand command) {
    JRadioButton radioButton = (JRadioButton) getComponent(command,
        JRadioButton.class, true);
    if (command.getTest() == UITestTest.ENABLED) {
      assertEnabled(command, radioButton);
      return;
    }
    assertSelected(command, radioButton);
  }

  private void assertCheckBox(DialogSectionCommand command) {
    JCheckBox checkBox = (JCheckBox) getComponent(command, JCheckBox.class,
        true);
    if (command.getTest() == UITestTest.ENABLED) {
      assertEnabled(command, checkBox);
      return;
    }
    assertSelected(command, checkBox);
  }

  private void assertButton(DialogSectionCommand command) {
    AbstractButton button = (AbstractButton) getComponent(command,
        AbstractButton.class, true);
    if (command.getTest() == UITestTest.ENABLED) {
      assertEnabled(command, button);
      return;
    }
    assertSelected(command, button);
  }

  private void assertMiniButton(DialogSectionCommand command) {
    JButton button = (JButton) getComponent(command, JButton.class, true);
    if (command.getTest() == UITestTest.ENABLED) {
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

  private void assertEnabled(DialogSectionCommand command, Component component) {
    testCase.assertTrue(reader.getInfo(), "bad call to assertEnabled", command
        .getTest() == UITestTest.ENABLED);
    String value = command.getValue();
    boolean enabled = false;
    if (value != null && value.equals("1")) {
      enabled = true;
    }
    //assert enabled
    testCase.assertEquals(reader.getInfo(), command + ":value=" + value,
        enabled, component.isEnabled());
  }

  private void assertSelected(DialogSectionCommand command,
      AbstractButton button) {
    boolean selected = false;
    String value = command.getValue();
    if (value != null && value.equals("1")) {
      selected = true;
    }
    testCase.assertEquals(reader.getInfo(), command.toString(), selected,
        button.isSelected());
  }

  private void assertEquals(DialogSectionCommand command,
      JTextComponent textComponent) {
    String value = command.getValue();
    if (value == null) {
      value = "";
    }
    testCase.assertEquals(reader.getInfo(), command.toString(), value,
        textComponent.getText());
  }

  private AbstractButton getButton(DialogSectionCommand command) {
    AbstractButton button = (AbstractButton) getComponent(command,
        JButton.class, false);
    if (button == null) {
      button = (AbstractButton) getComponent(command, JToggleButton.class, true);
    }
    return button;
  }

  private void callback(DialogSectionCommand command) {
    command.getCallbackClassEnum().getCallbackClass().callback(command);
  }

  private AbstractButton getButton(String name) {
    return getButton(name, true);
  }

  private AbstractButton getButton(String name, boolean required) {
    AbstractButton button = (AbstractButton) getComponent(JButton.class, name,
        false, false);
    if (button == null) {
      button = (AbstractButton) getComponent(JToggleButton.class, name,
          required, false);
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
    return getNamedComponentFinder(componentClass, name,
        NamedComponentFinder.OP_EQUALS);
  }

  private NamedComponentFinder getNamedComponentFinder(Class componentClass,
      String name, int operation) {
    if (finder == null) {
      finder = new NamedComponentFinder(componentClass, name);
      finder.setWait(2);
      finder.setOperation(operation);
    }
    else {
      finder.setComponentClass(componentClass);
      finder.setName(name);
      finder.setOperation(operation);
    }
    return finder;
  }

  private AbstractButtonFinder getAbstractButtonFinder(String buttonText) {
    if (popupButtonFinder == null) {
      popupButtonFinder = new AbstractButtonFinder(buttonText);
      popupButtonFinder.setWait(2);
    }
    else {
      popupButtonFinder.setText(buttonText);
    }
    return popupButtonFinder;
  }

  private ComponentFinder getComponentFinder(Class componentClass) {
    if (componentFinder == null) {
      componentFinder = new ComponentFinder(componentClass);
      componentFinder.setWait(2);
    }
    else {
      componentFinder.setComponentClass(componentClass);
    }
    return componentFinder = new ComponentFinder(JOptionPane.class);
  }

  private JOptionPane getPopup(DialogSectionCommand command) {
    String name = command.getName();
    JOptionPane optionPane = (JOptionPane) getNamedComponentFinder(
        JOptionPane.class, name).find();
    return optionPane;
  }

  /**
   * Finds components described by command in the panel.
   * @param command
   * @param fieldClass
   * @param required
   * @return
   */
  private Component getComponent(DialogSectionCommand command,
      Class componentClass, boolean required) {
    return getComponent(panel, command, componentClass, required);
  }

  /**
   * Finds components described by command
   * @param panel
   * @param command
   * @param componentClass
   * @param required
   * @return
   */
  private Component getComponent(JComponent panel,
      DialogSectionCommand command, Class componentClass, boolean required) {
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

  /**
   * Finds components in the framePanel at index 0.
   * @param fieldClass
   * @param name
   * @param required
   * @param startsWith
   * @return
   */
  private Component getComponent(Class componentClass, String name,
      boolean required, boolean startsWith) {
    Component component;
    int operation = startsWith ? NamedComponentFinder.OP_CONTAINS
        : NamedComponentFinder.OP_EQUALS;
    finder = getNamedComponentFinder(componentClass, name, operation);
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

  /**
   * Finds components in the framePanel at index 0.
   * @param fieldClass
   * @param name
   * @param required
   * @param startsWith
   * @return
   */
  private Component getComponent(Class componentClass, String name, int index,
      boolean required, boolean startsWith) {
    Component component;
    int operation = startsWith ? NamedComponentFinder.OP_CONTAINS
        : NamedComponentFinder.OP_EQUALS;
    finder = getNamedComponentFinder(componentClass, name, operation);
    if (frame == null) {
      component = finder.find();
    }
    else {
      component = finder.find(frame, index);
    }
    if (required) {
      testCase.assertNotNull(reader.getInfo(), "componentClass="
          + componentClass + ",name=" + name, component);
    }
    return component;
  }

  private AbstractButton getButton(JOptionPane optionPane, String buttonText) {
    AbstractButton button = (AbstractButton) getAbstractButtonFinder(buttonText)
        .find(optionPane, 0);
    testCase
        .assertNotNull(reader.getInfo(), "buttonText=" + buttonText, button);
    return button;
  }

  private static final class CommandFactory implements UITestCommandFactory {
    public UITestCommand newCommand() {
      return new TestAutodocCommand();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2008/05/30 21:52:59  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.13  2008/01/22 18:04:30  sueh
 * <p> bug# 1068 Increase wait time in finders.
 * <p>
 * <p> Revision 1.12  2007/03/21 19:48:25  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.  Moved AdocCommand
 * <p> classes out of the autodoc package.
 * <p>
 * <p> Revision 1.11  2006/10/24 23:35:04  sueh
 * <p> bug# 947 Added waitfor.process.process_name =
 * <p>
 * <p> Revision 1.10  2006/10/11 10:12:46  sueh
 * <p> bug# 938 Making ThreadGroup dependent on UncaughtException instead of
 * <p> UITest, so that it does not require JfcUnit to compile.
 * <p>
 * <p> Revision 1.9  2006/10/10 05:26:10  sueh
 * <p> bug# 931 Added assert file exists functionality.  Failing on an uncaught
 * <p> exception.
 * <p>
 * <p> Revision 1.8  2006/08/28 18:27:16  sueh
 * <p> bug# 923 Changed the uitest source attribute to filedir.  Global filedir is an
 * <p> absolute file path.
 * <p>
 * <p> Revision 1.7  2006/08/18 23:25:44  sueh
 * <p> bug# 852 clickCheckBox:  remove assert that prevents cb. from having a value
 * <p>
 * <p> Revision 1.6  2006/08/08 18:20:15  sueh
 * <p> bug# 852 Changing the Dialog adoc command to mean the function
 * <p> location.  Added the function command, to run a function section.
 * <p>
 * <p> Revision 1.5  2006/06/27 22:35:54  sueh
 * <p> bug# 852 Implementing callFunction().
 * <p>
 * <p> Revision 1.4  2006/06/14 00:42:29  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.  Renamed the section level command
 * <p> objects to make them less generic.
 * <p>
 * <p> Revision 1.3  2006/05/01 21:24:02  sueh
 * <p> bug# 787 removed commented out code
 * <p>
 * <p> Revision 1.2  2006/05/01 21:20:31  sueh
 * <p> bug# 787 Removed fiducial diameter, added set.  Handling all variables in
 * <p> UITestAxisDialogCommand.
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
