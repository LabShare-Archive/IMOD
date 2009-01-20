package etomo.uitest;

import java.awt.Component;
import java.awt.Container;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.text.JTextComponent;

import junit.extensions.jfcunit.JFCTestHelper;
import junit.extensions.jfcunit.eventdata.JSpinnerMouseEventData;
import junit.extensions.jfcunit.eventdata.JTabbedPaneMouseEventData;
import junit.extensions.jfcunit.eventdata.MouseEventData;
import junit.extensions.jfcunit.finder.AbstractButtonFinder;
import junit.extensions.jfcunit.finder.NamedComponentFinder;
import junit.framework.Assert;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.UITestActionType;
import etomo.type.UITestFieldType;
import etomo.type.UITestSubjectType;
import etomo.ui.AxisProcessPanel;
import etomo.ui.ProgressPanel;
import etomo.util.Utilities;

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
 * <p> $Log$ </p>
 */
final class AutodocTester extends Assert implements VariableList {
  public static final String rcsid = "$Id$";

  private final ReadOnlyAutodoc autodoc;
  private final JFCTestHelper helper;
  private final TestRunner testRunner;
  private final AxisID axisID;
  private final String sectionType;
  private final String sectionName;
  private final File sourceDir;
  private final VariableList parentVariableList;
  private final ReadOnlySection subsection;
  private final AutodocTester parentTester;

  private Container currentPanel = null;
  private CommandReader reader = null;
  private boolean wait = false;
  private ReadOnlyAutodoc functionAutodoc = null;
  private String functionSectionType = null;
  private AutodocTester childTester = null;
  private Map globalVariableMap = null;
  private Map variableMap = null;
  private NamedComponentFinder finder = null;
  private AbstractButtonFinder buttonFinder = null;
  private Command command = null;
  private String skipToDialogSection = null;
  private boolean debug = false;
  private Set completedDialogSections = null;
  private boolean frameOpen = false;

  /**
   * Tests all the sections of sectionType in order.  This is assumed to be a
   * top-level autodoc.
   * 
   * Autodoc Testers:
   * These top level testers are associated with one autodoc (other top level
   * testers and function testers may use the same autodoc).  They are uniquely
   * identified by the axisID that is passed to them.  They attempt to test
   * every section in the autodoc that matches sectionType.  Autodoc testers
   * cannot create child function testers until they have used the set.adoc
   * command to set an autodoc and section type for function calls.  Autodoc
   * testers have no parent.  They can have a function tester or a subsection
   * tester as a child.
   * @param testRunner
   * @param helper
   * @param autodoc
   * @param sourceDir
   * @param sectionType
   * @param axisID
   * @return
   */
  static AutodocTester getAutodocTester(TestRunner testRunner,
      JFCTestHelper helper, ReadOnlyAutodoc autodoc, File sourceDir,
      String sectionType, AxisID axisID, VariableList parentVariableList) {
    AutodocTester tester = new AutodocTester(testRunner, helper, autodoc,
        sourceDir, sectionType, null, null, axisID, parentVariableList, null);
    tester.globalVariableMap = new HashMap();
    tester.globalVariableMap.put("axis", axisID.getExtension());
    tester.completedDialogSections = new HashSet();
    return tester;
  }

  /**
   * Tests one section.  Sets the function autodoc and section type for function
   * calls from the parent.
   * 
   * Function Testers:
   * These testers are associated with one section.  This section must have a
   * unique type and name in its autodoc.  The autodoc containing the section
   * may be different from the parent autodoc, but doesn't have to be.  Function
   * testers may create child function testers, which will have the same autodoc
   * and section type for function calls as the parent, unless set.adoc is used
   * to change them.  Function testers can have any kind of tester as a parent.
   * They can have a function tester or a subsection tester as a child.
   * @param autodoc
   * @param sectionType
   * @param sectionName
   * @param parentTester
   * @return
   */
  static AutodocTester getFunctionTester(ReadOnlyAutodoc autodoc,
      String sectionType, String sectionName, AutodocTester parentTester) {
    AutodocTester tester = new AutodocTester(parentTester.testRunner,
        parentTester.helper, autodoc, parentTester.sourceDir, sectionType,
        sectionName, null, parentTester.axisID, parentTester, parentTester);
    //Set up the function tester to run more functions.
    tester.functionAutodoc = autodoc;
    tester.functionSectionType = sectionType;
    tester.currentPanel = parentTester.currentPanel;
    tester.frameOpen = parentTester.frameOpen;
    return tester;
  }

  /**
   * Tests one subsection.  Sets the function autodoc and section type for
   * function calls from the parent.  Does not pass the section type into the
   * constructor since the subsection does not have to be found.
   * 
   * Subsection Testers:
   * These testers are associated with one subsection.  This subsection is
   * passed to the tester, so it doesn't have to be unique.  If the subsection
   * tester did not get an autodoc and section type for calling functions from
   * its parent, it must use the set.adoc command to set them.  Subsection
   * testers can have an autodoc tester or a function tester as a parent.  They
   * can have a function tester as a child.
   * @param subsection
   * @param parentTester
   * @return
   */
  static AutodocTester getSubsectionTester(ReadOnlySection subsection,
      AutodocTester parentTester) {
    AutodocTester tester = new AutodocTester(parentTester.testRunner,
        parentTester.helper, parentTester.autodoc, parentTester.sourceDir,
        null, null, subsection, parentTester.axisID, parentTester, parentTester);
    tester.functionAutodoc = parentTester.functionAutodoc;
    tester.functionSectionType = parentTester.functionSectionType;
    tester.currentPanel = parentTester.currentPanel;
    tester.frameOpen = parentTester.frameOpen;
    return tester;
  }

  /**
   * @param testRunner
   * @param helper
   * @param autodoc
   * @param sourceDir
   * @param sectionType
   * @param sectionName
   * @param subsection
   * @param axisID
   * @param parentVariableList
   * @param parentTester
   */
  private AutodocTester(TestRunner testRunner, JFCTestHelper helper,
      ReadOnlyAutodoc autodoc, File sourceDir, String sectionType,
      String sectionName, ReadOnlySection subsection, AxisID axisID,
      VariableList parentVariableList, AutodocTester parentTester) {
    this.testRunner = testRunner;
    this.helper = helper;
    this.autodoc = autodoc;
    this.axisID = axisID;
    this.sourceDir = sourceDir;
    this.sectionType = sectionType;
    this.sectionName = sectionName;
    this.subsection = subsection;
    this.parentVariableList = parentVariableList;
    this.parentTester = parentTester;
  }

  /**
   * A tester is done when it has not childTester and its reader is done.
   * @return
   */
  boolean isDone() {
    if (reader == null || wait || childTester != null) {
      return false;
    }
    return reader.isDone();
  }

  /**
   * Exit from the current scope.
   */
  private void setReturn() {
    if (sectionType == null || sectionName != null) {
      //Either a function tester or a subsection test so just set
      //reader.done=true to leave the scope.
      reader.setDone();
    }
    else {
      //This is an autodoc tester so make testUntilWait() call nextSection().
      command = null;
    }
  }

  /**
   * Exit from all scopes and end the test.  Make sure that isDone() returns
   * true.
   */
  private void setEnd() {
    reader.setDone();
    wait = false;
    if (childTester != null) {
      //The childTester is active, so this call will have originated from it.
      //Set the childTester to null so that isDone() will return true.
      childTester = null;
    }
    if (parentTester != null) {
      //This is either a function tester or a subsection tester so must tell
      //parent to end.
      parentTester.setEnd();
    }
  }

  void setDebug() {
    debug = true;
  }

  /**
   * A tester is in wait state if the wait member variable is true, unless it
   * has a childTester; in that case the wait state is dependent on the child
   * tester's wait state.
   * @return
   */
  private boolean isWait() {
    if (childTester != null) {
      return childTester.isWait();
    }
    return wait;
  }

  /**
   * Processes commands until a wait state is encountered or the test is done.
   * 
   * When an open.frame command exists for this tester:
   * The first time this function is run it attempts to open the frame.  If this
   * fails, it goes into a wait state and will continue to try to open the frame
   * each time it runs until it succeeds.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   * @throws LogFile.FileException
   */
  void testUntilWait() throws FileNotFoundException, IOException,
      LogFile.ReadException, LogFile.FileException {
    //If this is a top level tester and the frame hasn't been opened, try to
    //open the frame associated with this tester.
    if (!frameOpen) {
      frameOpen = openFrame();
      if (!frameOpen) {
        //Wait until frame can be opened.
        wait = true;
        try {
          Thread.sleep(1);
        }
        catch (InterruptedException e) {
        }
        return;
      }
      else {
        wait = false;
      }
    }
    if (reader == null) {
      //first time running testUntilWait.
      nextSection();
    }
    if (reader.isDone()) {
      return;
    }
    gotoFrame();
    while (!reader.isDone()) {
      do {
        //Don't get a new command until the current function is completed.
        if (childTester != null) {
          childTester.testUntilWait();
          //SetEnd() could have set the childTester to null.
          if (childTester != null) {
            if (childTester.isDone()) {
              childTester = null;
            }
            else if (childTester.isWait()) {
              return;
            }
          }
        }
        //Before getting a new command, wait for the current command if it is a
        //wait command.
        if (!wait) {
          command = reader.nextCommand(command);
        }
        if (command != null && command.isKnown()) {
          executeCommand(command);
          if (wait) {
            return;
          }
        }
      } while (!reader.isDone() && command != null
          && skipToDialogSection == null);
      nextSection();
    }
  }

  /**
   * Get the next section or starts the reader (which gets the first section).
   * For Autodoc testers it also removes section-level
   * scope.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   * @throws LogFile.FileException
   */
  private void nextSection() throws FileNotFoundException, IOException,
      LogFile.ReadException, LogFile.FileException {
    if (debug) {
      System.out.println("sectionType=" + sectionType + ",sectionName="
          + sectionName);
      System.out.println("nextSection:functionAutodoc="
          + functionAutodoc.getName() + ",functionSectionType="
          + functionSectionType);
    }
    do {
      //Get the next section
      if (reader == null) {
        if (sectionType == null) {
          //Gets a reader for the subsection that was passed in
          reader = CommandReader.getSubsectionReader(autodoc, subsection,
              axisID, this);
        }
        else if (sectionName == null) {
          //Opens the first section to be read.
          reader = CommandReader.getAutodocReader(autodoc, sectionType, axisID,
              this);
          if (skipToDialogSection != null
              && skipToDialogSection.equals(reader.getSectionName())) {
            //Found the starting dialog - stop skipping
            skipToDialogSection = null;
          }
        }
        else {
          //open function section
          reader = CommandReader.getSectionReader(autodoc, sectionType,
              sectionName, axisID, this);
          assertFalse("called function that doesn't exist - " + sectionType
              + " = " + sectionName + " - in " + autodoc.getName(), reader
              .isDone());
        }
      }
      else if (!reader.isDone()) {
        if (completedDialogSections != null) {
          //add to list of completed (and skipped) dialog sections
          completedDialogSections.add(reader.getSectionName());
        }
        reader.nextSection();
        if (skipToDialogSection != null
            && skipToDialogSection.equals(reader.getSectionName())) {
          //Found the section to skip to - stop skipping
          skipToDialogSection = null;
        }
      }
      if (reader.isDone()) {
        return;
      }
      //If section type is not null and section name is null then this is an
      //autodoc tester.  This means that
      //it tests multiple sections in a Dialog autodoc, each with it's own scope.
      //It also means that each section is associated with a different dialog as
      //described in the interface section.
      if (sectionType != null && sectionName == null) {
        //Remove section level scope
        //function scope
        functionAutodoc = null;
        functionSectionType = null;
        //Variable scope
        if (variableMap != null && variableMap.size() > 0) {
          variableMap.clear();
        }
      }
    } while (skipToDialogSection != null);
  }

  /**
   * Execute required command to get to the frame panel for the current axis.
   * Called every time testUntilWait is called.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  private void gotoFrame() throws FileNotFoundException, IOException,
      LogFile.ReadException, LogFile.FileException {
    executeCommand(testRunner.getInterfaceSection().getGotoFrameCommand(axisID));
  }

  /**
   * Execute required command to open the frame for the current axis.
   * Called the first time testUntilWait is called.  To open the secondary frame,
   * must first goto the primary frame.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   * @return true if the frame is open
   */
  private boolean openFrame() throws FileNotFoundException, IOException,
      LogFile.ReadException, LogFile.FileException {
    Command frameCommand = testRunner.getInterfaceSection()
        .getOpenFrameCommand(axisID);
    //if frameCommand is null, then it is not necessary to open the frame, so
    //return true
    if (frameCommand == null) {
      return true;
    }
    executeCommand(testRunner.getInterfaceSection().getGotoFrameCommand(
        AxisID.ONLY));
    return executeCommand(frameCommand);
  }

  /**
   * Execute optional command to open a dialog called dialogName.  Called when "open.dialog ="
   * executed.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  private void openInterface() throws FileNotFoundException, IOException,
      LogFile.ReadException, LogFile.FileException {
    executeCommand(testRunner.getInterfaceSection().getOpenInterfaceCommand());
  }

  /**
   * Execute optional command to open a dialog called dialogName.  Called when "open.dialog ="
   * executed.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  private void openDialog(String dialogName) throws FileNotFoundException,
      IOException, LogFile.ReadException, LogFile.FileException {
    executeCommand(testRunner.getInterfaceSection().getOpenDialogCommand(
        dialogName));
  }

  /**
   * Executes an action command.  Returns without doing anything for  null
   * command.  If the action command read a field command, it will call
   * executeFieldCommand.
   * @param command
   * @return false if an optional command fails, otherwise return true or fail an assert
   */
  private boolean executeCommand(Command command) throws FileNotFoundException,
      IOException, LogFile.FileException, LogFile.ReadException {
    if (command == null || !command.isKnown()) {
      return true;
    }
    UITestActionType actionType = command.getActionType();
    UITestSubjectType subjectType = command.getSubjectType();
    UITestModifierType modifierType = command.getModifierType();
    Subject subject = command.getSubject();
    Command subcommand = command.getSubcommand();
    String subjectName = null;
    if (subject != null) {
      subjectName = subject.getName();
    }
    Field field = command.getField();
    String value = command.getValue();
    //Field
    if (actionType == null) {
      executeField(command);
    }
    //ASSERT
    else if (actionType == UITestActionType.ASSERT) {
      //assert.file
      if (subjectType == UITestSubjectType.FILE) {
        File file = new File(System.getProperty("user.dir"), value);
        //assert.exists.file = file_name
        if (modifierType == UITestModifierType.EXISTS) {
          assertTrue("file does not exist - " + value + " (" + command + ")",
              file.exists());
        }
        //assert.not-exists.file = file_name
        else if (modifierType == UITestModifierType.NOT_EXISTS) {
          assertFalse("file exists - " + value + " (" + command + ")", file
              .exists());
        }
        else {
          fail("unexpected command (" + command + ")");
        }
      }
      //assert.field = value
      else if (field != null) {
        executeField(command);
      }
      else {
        fail("unexpected command (" + command + ")");
      }
    }
    //COPY
    else if (actionType == UITestActionType.COPY) {
      assertTrue("only the always modifier is allowed with this actionType ("
          + command + ")", modifierType == null
          || modifierType == UITestModifierType.ALWAYS);
      //copy.file = file_name
      assertEquals("can only copy a file", subjectType, UITestSubjectType.FILE);
      testRunner.copyFile(value, modifierType == UITestModifierType.ALWAYS);
    }
    //END
    else if (actionType == UITestActionType.END) {
      setEnd();
    }
    //IF
    else if (actionType == UITestActionType.IF) {
      //[[if = variable]]
      if (command.isSubsection()) {
        assertNull("modifier not used with this actionType (" + command + ")",
            modifierType);
        assertFalse("illegal section name - " + value + " (" + command + ")",
            value.startsWith("="));
        if (isVariableSet(value)) {
          childTester = AutodocTester.getSubsectionTester(command
              .getSubsection(), this);
        }
      }
      //if.var
      else if (subjectType == UITestSubjectType.VAR) {
        boolean variableSet = isVariableSet(subjectName);
        if (modifierType == null) {
          if (variableSet) {
            //if.var.action
            if (subcommand != null) {
              executeCommand(subcommand);
            }
            //if.var.field
            else {
              executeField(command);
            }
          }
        }
        //if.not.var
        else if (modifierType == UITestModifierType.NOT) {
          if (!variableSet) {
            //if.not.var.action
            if (subcommand != null) {
              executeCommand(subcommand);
            }
            //if.not.var.field
            else {
              executeField(command);
            }
          }
        }
        else {
          String variableValue = getVariableValue(subjectName);
          boolean comparisonTrue = false;
          //if.equals.var.variable_name.... = variable_value
          if (modifierType == UITestModifierType.EQUALS) {
            comparisonTrue = variableValue.equals(value);
          }
          //if.not-equals.var.variable_name.... = variable_value
          else if (modifierType == UITestModifierType.NOT_EQUALS) {
            comparisonTrue = !variableValue.equals(value);
          }
          else {
            fail("unexpected command (" + command + ")");
          }
          assertNotNull(
              "subcommand required in an if.comparison.var command - ("
                  + command + ")", subcommand);
          assertEquals(
              "only return subcommand is allowed in an if.comparison.var command - ("
                  + command + ")", subcommand.getActionType(),
              UITestActionType.RETURN);
          if (comparisonTrue) {
            executeCommand(subcommand);
          }
        }
      }
      else {
        fail("unexpected command (" + command + ")");
      }
    }
    //GOTO
    else if (actionType == UITestActionType.GOTO) {
      assertNull("modifier not used with this actionType (" + command + ")",
          modifierType);
      //goto.frame
      if (subjectType == UITestSubjectType.FRAME) {
        executeField(command);
      }
      else {
        fail("unexpected command (" + command + ")");
      }
    }
    //OPEN
    else if (actionType == UITestActionType.OPEN) {
      assertNull("modifier not used with this actionType (" + command + ")",
          modifierType);
      //[[open = interface]]
      if (command.isSubsection()) {
        assertTrue("the open legal value for an open subsection is interface",
            value.equals(UITestSubjectType.INTERFACE.toString()));
        assertFalse("illegal section name - " + value + " (" + command + ")",
            value.startsWith("="));
        childTester = AutodocTester.getSubsectionTester(
            command.getSubsection(), this);
      }
      //open.dialog
      else if (subjectType == UITestSubjectType.DIALOG) {
        assertNotNull("dialog name is required (" + command + ")", subjectName);
        //open.dialog
        if (field == null) {
          //refers to the interface section
          openDialog(subjectName);
        }
        //open.dialog.field
        else {
          //probably a command from the interface section
          executeField(command);
        }
      }
      //open.frame
      else if (subjectType == UITestSubjectType.FRAME) {
        assertNull("subject name is not use in this command (" + command + ")",
            subjectName);
        //This is an optional command because the buttons that are used to open
        //the frame are not always available.
        if (!executeField(command, false)) {
          return false;
        }
      }
      //open.interface
      else if (subjectType == UITestSubjectType.INTERFACE) {
        //probably a command from the interface section
        openInterface();
      }
      else {
        fail("unexpected command (" + command.toString() + ")");
      }
    }
    //RETURN
    else if (actionType == UITestActionType.RETURN) {
      setReturn();
    }
    //RUN
    else if (actionType == UITestActionType.RUN) {
      //run.function.section_name
      if (subjectType == UITestSubjectType.FUNCTION) {
        if (debug) {
          System.out.println("run function " + command
              + ",functionSectionType=" + functionSectionType);
        }
        assertNull("modifier not used with this actionType (" + command + ")",
            modifierType);
        assertNotNull("missing section name (" + command + ")", subjectName);
        assertNotNull("missing function autodoc - run set.adoc (" + command
            + ")", functionAutodoc);
        assertNotNull("missing function section type - run set.adoc ("
            + command + ")", functionSectionType);
        childTester = AutodocTester.getFunctionTester(functionAutodoc,
            functionSectionType, subjectName, this);
        if (debug) {
          childTester.setDebug();
        }
      }
    }
    //SET
    else if (actionType == UITestActionType.SET) {
      assertNull("modifier not used with this actionType (" + command + ")",
          modifierType);
      //set.adoc.section_type
      if (subjectType == UITestSubjectType.ADOC) {
        assertNotNull("missing section type (" + command + ")", subjectName);
        functionSectionType = subjectName;
        //set.adoc.section_type =
        if (value == null) {
          functionAutodoc = autodoc;
        }
        //set.adoc.section_type = autodoc_name
        else {
          functionAutodoc = AutodocFactory.getInstance(sourceDir, value,
              AxisID.ONLY);
        }
      }
      //set.var.variable_name
      else if (subjectType == UITestSubjectType.VAR) {
        assertNotNull("missing variable name (" + command + ")", subjectName);
        if (variableMap == null) {
          variableMap = new HashMap();
        }
        variableMap.put(subjectName, value);
      }
      else {
        fail("unexpected command (" + command.toString() + ")");
      }
    }
    //SKIPTO
    else if (actionType == UITestActionType.SKIPTO) {
      assertNull("modifier not used with this actionType (" + command + ")",
          modifierType);
      //skipto.dialog.dialog_section_name
      if (subjectType == UITestSubjectType.DIALOG) {
        assertNotNull("dialog name is missing (" + command + ")", subjectName);
        skipToDialogSection = subjectName;
      }
      else {
        fail("unexpected command (" + command.toString() + ")");
      }
    }
    //SLEEP
    else if (actionType == UITestActionType.SLEEP) {
      assertNull("modifier not used with this actionType (" + command + ")",
          modifierType);
      assertNull("subject not used with this actionType (" + command + ")",
          subjectType);
      assertNull("field not used with this actionType (" + command + ")", field);
      EtomoNumber interval = new EtomoNumber();
      //sleep = interval
      if (value != null) {
        interval.set(value);
      }
      //sleep =
      else {
        interval.set(1000);
      }
      try {
        Thread.sleep(interval.getInt());
      }
      catch (InterruptedException e) {
      }
    }
    //WAIT
    else if (actionType == UITestActionType.WAIT) {
      //Take naps during the wait to avoid driving the load up
      try {
        Thread.sleep(5);
      }
      catch (InterruptedException e) {
      }
      assertNull("modifier not used with this actionType (" + command + ")",
          modifierType);
      //wait.file-chooser.file_chooser_title = chosen_file
      if (subjectType == UITestSubjectType.FILE_CHOOSER) {
        setupNamedComponentFinder(JFileChooser.class, subjectName);
        JFileChooser fileChooser = (JFileChooser) finder.find();
        File file = new File(getVariableValue(UITestSubjectType.TESTDIR
            .toString()), value);
        fileChooser.setSelectedFile(file);
        fileChooser.approveSelection();
      }
      //wait.popup.popup_title = dismiss_button_label
      else if (subjectType == UITestSubjectType.POPUP) {
        assertNotNull("popup name is required (" + command + ")", subjectName);
        assertNotNull("button to close popup is required (" + command + ")",
            value);
        if (!wait) {
          wait = true;
        }
        //Try to get the pop up immediately
        setupNamedComponentFinder(JOptionPane.class, subjectName);
        Container popup = (Container) finder.find();
        //If popup hasn't popped up, keep waiting.
        if (popup == null) {
          return true;
        }
        //close popup
        setupAbstractButtonFinder(value);
        AbstractButton button = (AbstractButton) buttonFinder.find(popup, 0);
        assertNotNull("unable to find button to close popup - " + value + " ("
            + command + ")", button);
        helper.enterClickAndLeave(new MouseEventData(testRunner, button));
        wait = false;
        if (wait) {
          return true;
        }
      }
      //wait.process.process_title = process_bar_end_text
      else if (subjectType == UITestSubjectType.PROCESS) {
        assertNotNull("process name is required (" + command + ")", subjectName);
        assertNotNull("end state is required (" + command + ")", value);
        if (!wait) {
          wait = true;
          return true;
        }
        //Already waited at least once - now see whether the process is done.
        //Waiting for anything but a single process or the last process in a
        //series will not work.
        //Get the kill process button
        setupNamedComponentFinder(JButton.class, Utilities
            .convertLabelToName(AxisProcessPanel.KILL_BUTTON_LABEL));
        JButton killButton = (JButton) finder.find(currentPanel, 0);
        assertNotNull("can't find kill button (" + command + ")", killButton);
        //Get the progress bar label
        setupNamedComponentFinder(JLabel.class, ProgressPanel.LABEL_NAME);
        JLabel progressBarLabel = (JLabel) finder.find(currentPanel, 0);
        assertNotNull("can't find progress bar label (" + command + ")",
            progressBarLabel);
        //Get the progress bar
        setupNamedComponentFinder(JProgressBar.class, ProgressPanel.NAME);
        JProgressBar progressBar = (JProgressBar) finder.find(currentPanel, 0);
        assertNotNull("can't find progress bar label (" + command + ")",
            progressBar);
        //Decide if the process is still running
        if (killButton.isEnabled()) {
          return true;
        }
        //The killButton turns on and off in between processes.  Avoid exiting
        //in that case.
        try {
          Thread.sleep(1000);
        }
        catch (InterruptedException e) {
        }
        if (killButton.isEnabled()) {
          return true;
        }
        try {
          Thread.sleep(1000);
        }
        catch (InterruptedException e) {
        }
        if (killButton.isEnabled()) {
          return true;
        }
        //Decide if this is the right process         
        if (!Utilities.convertLabelToName(progressBarLabel.getText()).equals(
            subjectName)) {
          return true;
        }
        //The right process is done
        wait = false;
        //Check the end_state
        assertEquals("process ended with the wrong state -" + value + " ("
            + command + ")", progressBar.getString(), value);
      }
      //wait.test
      else if (subjectType == UITestSubjectType.TEST) {
        if (testRunner.isDialogSectionComplete(subject.getAxisID(), value)) {
          wait = false;
        }
        else {
          if (testRunner.isDone(subject.getAxisID())) {
            setEnd();
          }
          else {
            wait = true;
          }
        }
      }
      else {
        fail("unexpected command (" + command.toString() + ")");
      }
    }
    else {
      fail("unexpected command (" + command.toString() + ")");
    }
    return true;
  }

  boolean isDialogSectionComplete(String dialogSection) {
    assertNotNull("this function should be called in only top level testers",
        completedDialogSections);
    return completedDialogSections.contains(dialogSection);
  }

  AxisID getAxisID() {
    return axisID;
  }

  /**
   * Attempts to find variableName in variableMap.  If it can't, if calls
   * parentVariableList.getVariableValue().
   * @param variableName
   * @return variableValue
   */
  public String getVariableValue(String variableName) {
    if (variableMap != null && variableMap.containsKey(variableName)) {
      return (String) variableMap.get(variableName);
    }
    if (globalVariableMap != null
        && globalVariableMap.containsKey(variableName)) {
      return (String) globalVariableMap.get(variableName);
    }
    return parentVariableList.getVariableValue(variableName);
  }

  /**
   * Checks to see if variableMap contains the key variableName.  If it doesn't
   * calls the parent doesVariableExist.
   * @param variableName
   * @return variableExists
   */
  public boolean isVariableSet(String variableName) {
    if (variableMap != null && variableMap.containsKey(variableName)) {
      return true;
    }
    if (globalVariableMap != null
        && globalVariableMap.containsKey(variableName)) {
      return true;
    }
    return parentVariableList.isVariableSet(variableName);
  }

  public String toString() {
    if (sectionName == null) {
      return autodoc.getName() + "," + sectionType;
    }
    return autodoc.getName() + "," + sectionType + "," + sectionName;
  }

  /**
   * Handle assert.enabled/disabled.  Checks Component.isEnabled.
   * @param component
   * @param command
   */
  private void assertEnabled(Component component, Command command) {
    Container container;
    assertNull("assert.enabled/disabled command does not use a value ("
        + command + ")", command.getValue());
    UITestModifierType modifierType = command.getModifierType();
    if (modifierType == UITestModifierType.ENABLED) {
      assertTrue("component is not enabled (" + command + ")", component
          .isEnabled());
    }
    else if (modifierType == UITestModifierType.DISABLED) {
      assertFalse("component is enabled (" + command + ")", component
          .isEnabled());
    }
    else {
      fail("unexpected command (" + command + ")");
    }
  }

  /**
   * Handle assert.enabled/disabled.  Checks Component.isEnabled and
   * JTextComponent.isEditable.
   * @param textComponent
   * @param command
   */
  private void assertEnabled(JTextComponent textComponent, Command command) {
    assertNull("assert.enabled/disabled command does not use a value ("
        + command + ")", command.getValue());
    UITestModifierType modifierType = command.getModifierType();
    //also need to check whether the text field is editable (treat the
    //same as enabled)
    assertNull("assert.enabled/disabled command does not use a value ("
        + command + ")", command.getValue());
    if (modifierType == UITestModifierType.ENABLED) {
      assertTrue("textComponent is not enabled (" + command + ")",
          textComponent.isEnabled());
      assertTrue("textComponent is not editable (" + command + ")",
          textComponent.isEditable());
    }
    else if (modifierType == UITestModifierType.DISABLED) {
      assertFalse("textComponent is enabled or editable (" + command + ")",
          textComponent.isEnabled() && textComponent.isEditable());
    }
    else {
      fail("unexpected command (" + command + ")");
    }
  }

  private void executeField(Command command) {
    executeField(command, true);
  }

  /**
   * Executes the field part of a command.  Also handles asserting field states.
   * @param command
   * @return false if failed and failIfNotFound is false, otherwise return true if a failure is not asserted
   */
  private boolean executeField(Command command, boolean failIfNotFound) {
    UITestActionType actionType = command.getActionType();
    UITestModifierType modifierType = command.getModifierType();
    Field field = command.getField();
    UITestFieldType fieldType = field.getFieldType();
    String name = field.getName();
    int index = field.getIndex();
    String value = command.getValue();
    assertNotNull("missing field (" + command + ")", field);
    boolean assertAction = false;
    if (actionType == UITestActionType.ASSERT && command.getSubject() == null) {
      //If the action type is assert, and it is not associated with a subject,
      //then this is an assert field command.
      assertAction = true;
    }
    //BUTTON
    if (fieldType == UITestFieldType.BUTTON) {
      setupNamedComponentFinder(AbstractButton.class, name);
      AbstractButton button = (AbstractButton) finder.find(currentPanel, index);
      if (button == null) {
        if (failIfNotFound) {
          fail("can't find button - " + name + " (" + command + ")");
        }
        else {
          return false;
        }
      }
      //bn.button_name =
      if (!assertAction) {
        assertNull("value not valid in a button command (" + command + ")",
            value);
        helper.enterClickAndLeave(new MouseEventData(testRunner, button, 1));
      }
      //assert.bn.button_name
      else {
        //assert.enabled.bn.button_name
        //assert.disabled.bn.button_name
        if (modifierType != null) {
          assertEnabled(button, command);
        }
        //assert.enabled.bn.button_name = button_state
        else {
          assertNotNull("value is required in an assert.bn command (" + command
              + ")", value);
          assertEquals("button state is not equal to value - " + value + " ("
              + command + ")", button.isSelected(), convertToBoolean(value));
        }
      }
    }
    //CHECK BOX
    else if (fieldType == UITestFieldType.CHECK_BOX) {
      setupNamedComponentFinder(JCheckBox.class, name);
      JCheckBox checkBox = (JCheckBox) finder.find(currentPanel, index);
      if (checkBox == null) {
        if (failIfNotFound) {
          fail("can't find checkbox - " + name + " (" + command + ")");
        }
        else {
          return false;
        }
      }
      //cb.check_box_name
      if (!assertAction) {
        //if value is present,only click on check box to get it to match value
        if (value == null || checkBox.isSelected() != convertToBoolean(value)) {
          helper
              .enterClickAndLeave(new MouseEventData(testRunner, checkBox, 1));
        }
      }
      //assert.cb.check_box_label
      else {
        //assert.enabled.cb.check_box_name
        //assert.disabled.cb.check_box_name
        if (modifierType != null) {
          assertEnabled(checkBox, command);
        }
        //assert.cb.check_box_name = check_box_state
        else {
          assertNotNull("value is required in an assert.cb command (" + command
              + ")", value);
          assertEquals("check box state is not equal to value - "
              + checkBox.getText() + "," + value + " (" + command + ")",
              checkBox.isSelected(), convertToBoolean(value));
        }
      }
    }
    //MENU_ITEM
    else if (fieldType == UITestFieldType.MENU_ITEM) {
      setupNamedComponentFinder(JMenuItem.class, name);
      AbstractButton menuItem = (AbstractButton) finder.find();
      if (menuItem == null) {
        if (failIfNotFound) {
          fail("can't find menu item - " + name + " (" + command + ")");
        }
        else {
          return false;
        }
      }
      //mn.menu_item_label
      if (!assertAction) {
        //if value is present,only click on mini-button when it matches value
        helper.enterClickAndLeave(new MouseEventData(testRunner, menuItem));
        //wait for menu to open
        try {
          Thread.sleep(1);
        }
        catch (InterruptedException e) {
        }
      }
      else {
        //assert.enabled.mn.menu_item_label
        //assert.disabled.mn.menu_item_label
        assertNotNull("modifier is required", modifierType);
        assertEnabled(menuItem, command);
      }
    }
    //MINI BUTTON
    else if (fieldType == UITestFieldType.MINI_BUTTON) {
      setupNamedComponentFinder(AbstractButton.class, name);
      AbstractButton miniButton = (AbstractButton) finder.find(currentPanel,
          index);
      if (miniButton == null) {
        if (failIfNotFound) {
          fail("can't find button - " + name + " (" + command + ")");
        }
        else {
          return false;
        }
      }
      //mb.title_with_mini_button
      if (!assertAction) {
        //if value is present,only click on mini-button when it matches value
        //mb.title_with_mini_button =
        //mb.title_with_mini_button = current_label
        if (value == null
            || miniButton.getText().equals("<html><b>" + value + "</b>")) {
          helper.enterClickAndLeave(new MouseEventData(testRunner, miniButton));
        }
      }
      //assert.mb.title_with_mini_button
      else {
        //assert.enabled.mb.title_with_mini_button
        //assert.disabled.mb.title_with_mini_button
        if (modifierType != null) {
          assertEnabled(miniButton, command);
        }
        //assert.mb.title_with_mini_button = current_label
        else {
          assertNotNull("value is required in an assert.mb command (" + command
              + ")", value);
          assertTrue("mini-button label is not equal to value - "
              + miniButton.getText() + "," + value + " (" + command + ")",
              miniButton.getText().equals(value));
        }
      }
    }
    //PANEL
    //pnl.panel_title
    else if (fieldType == UITestFieldType.PANEL) {
      assertNull("value not valid in a panel command (" + command + ")", value);
      assertFalse("cannot assert a panel (" + command + ")", assertAction);
      setupNamedComponentFinder(JPanel.class, name);
      currentPanel = (Container) finder.find();
      if (currentPanel == null) {
        if (failIfNotFound) {
          fail("panel not found - " + name + " (" + command + ")");
        }
        else {
          return false;
        }
      }
    }
    //RADIO BUTTON
    else if (fieldType == UITestFieldType.RADIO_BUTTON) {
      setupNamedComponentFinder(JRadioButton.class, name);
      JRadioButton radioButton = (JRadioButton) finder
          .find(currentPanel, index);
      if (radioButton == null) {
        if (failIfNotFound) {
          fail("can't find radio button - " + name + " (" + command + ")");
        }
        else {
          return false;
        }
      }
      //rb.radio_button_label
      if (!assertAction) {
        assertNull("value not valid in a radio command (" + command + ")",
            value);
        helper.enterClickAndLeave(new MouseEventData(testRunner, radioButton));
      }
      //assert.rb.radio_button_label
      else {
        //assert.enabled.rb.radio_button_label
        //assert.disabled.rb.radio_button_label
        if (modifierType != null) {
          assertEnabled(radioButton, command);
        }
        //assert.rb.radio_button_label = radio_button_state
        else {
          assertNotNull("value is required in an assert.rb command (" + command
              + ")", value);
          assertEquals("radio button state is not equal to value - "
              + radioButton.getText() + "," + value + " (" + command + ")",
              radioButton.isSelected(), convertToBoolean(value));
        }
      }
    }
    //SPINNER
    else if (fieldType == UITestFieldType.SPINNER) {
      setupNamedComponentFinder(JSpinner.class, name);
      JSpinner spinner = (JSpinner) finder.find(currentPanel, index);
      if (spinner == null) {
        if (failIfNotFound) {
          fail("spinner not found - " + name + " (" + command + ")");
        }
        else {
          return false;
        }
      }
      EtomoNumber nValue = new EtomoNumber();
      nValue.set(value);
      //sp.spinner_label = integer_value|up|down
      if (!assertAction) {
        if (nValue.isValid()) {
          spinner.setValue(nValue.getNumber());
        }
        else {
          boolean spinnerControl = convertToSpinnerControl(value);
          helper.enterClickAndLeave(new JSpinnerMouseEventData(testRunner,
              spinner,
              spinnerControl ? JSpinnerMouseEventData.UP_ARROW_SUBCOMPONENT
                  : JSpinnerMouseEventData.DOWN_ARROW_SUBCOMPONENT, 1));
        }
      }
      //assert.sp.spinner_label
      else {
        //assert.enabled.sp.spinner_label
        //assert.disabled.sp.spinner_label
        if (modifierType != null) {
          assertEnabled(spinner, command);
        }
        //assert.sp.spinner_label = integer_value
        else {
          if (value != null) {
            assertTrue("field text is not equal to value - "
                + spinner.getValue() + "," + value + " (" + command + ")",
                ((Number) spinner.getValue()).intValue() == nValue.getInt());
          }
          else {
            nValue.set((Number) spinner.getValue());
            assertTrue("field text is not empty - " + spinner.getValue() + ","
                + value + " (" + command + ")", nValue.isNull());
          }
        }
      }
    }
    //TAB
    //tb.tabbed_panel_title.index_of_tab
    else if (fieldType == UITestFieldType.TAB) {
      //find the tabbed panel and click on the tab
      assertNull("value not valid in a tab command (" + command + ")", value);
      assertFalse("cannot assert a tab (" + command + ")", assertAction);
      setupNamedComponentFinder(JTabbedPane.class, name);
      JTabbedPane tabbedPane = (JTabbedPane) finder.find(currentPanel, 0);
      if (tabbedPane == null) {
        if (failIfNotFound) {
          fail("can't find tab - " + name + " (" + field + ")");
        }
        else {
          return false;
        }
      }
      helper.enterClickAndLeave(new JTabbedPaneMouseEventData(testRunner,
          tabbedPane, index, 1));
    }
    //TEXT FIELD
    else if (fieldType == UITestFieldType.TEXT_FIELD) {
      setupNamedComponentFinder(JTextField.class, name);
      JTextComponent textField = (JTextComponent) finder.find(currentPanel,
          index);
      if (textField == null) {
        if (failIfNotFound) {
          fail("text field not found - " + name + " (" + command + ")");
        }
        else {
          return false;
        }
      }
      //tf.text_field_label
      if (!assertAction) {
        textField.setText(value);
      }
      //assert.tf.text_field_label
      else {
        //assert.enabled.tf.text_field_label
        //assert.disabled.tf.text_field_label
        if (modifierType != null) {
          assertEnabled(textField, command);
        }
        //assert.tf.text_field_label
        else {
          //assert.tf.text_field_label = value
          if (value != null) {
            assertTrue("field text is not equal to value - "
                + textField.getText() + "," + value + " (" + command + ")",
                textField.getText().equals(value));
          }
          //assert.tf.text_field_label =
          else {
            assertTrue("field text is not empty - " + textField.getText() + ","
                + value + " (" + command + ")", textField.getText().equals(""));
          }
        }
      }
    }
    else {
      fail("unexpected command (" + command + ")");
    }
    return true;
  }

  /**
   * Converts all possible boolean strings to boolean.
   * @param input
   * @return
   */
  private boolean convertToBoolean(String input) {
    if (input.equalsIgnoreCase("t") || input.equalsIgnoreCase("true")
        || input.equalsIgnoreCase("y") || input.equalsIgnoreCase("yes")
        || input.equalsIgnoreCase("on")) {
      return true;
    }
    if (input.equalsIgnoreCase("f") || input.equalsIgnoreCase("false")
        || input.equalsIgnoreCase("n") || input.equalsIgnoreCase("no")
        || input.equalsIgnoreCase("off")) {
      return false;
    }
    EtomoNumber number = new EtomoNumber();
    number.set(input);
    if (number.isValid()) {
      if (number.equals(1)) {
        return true;
      }
      if (number.equals(0)) {
        return false;
      }
    }
    fail("boolean value is required - " + input + " (" + command + ")");
    return false;
  }

  /**
   * Converts all possible spinner control strings to boolean where 1 equals up
   * and 0 equals down.
   * @param input
   * @return
   */
  private boolean convertToSpinnerControl(String input) {
    if (input.equalsIgnoreCase("up")) {
      return true;
    }
    if (input.equalsIgnoreCase("down")) {
      return false;
    }
    fail("allowabled strings:  up, down - " + input + " (" + command + ")");
    return false;
  }

  /**
   * Creates or reuses the named component finder.  Updates the component class
   * and name.  Sets the wait to 2.  Sets the operation to OP_EQUALS.
   * @param componentoClass
   * @param fieldName
   */
  private void setupNamedComponentFinder(Class componentoClass, String name) {
    if (finder == null) {
      finder = new NamedComponentFinder(componentoClass, name);
      finder.setWait(2);
      finder.setOperation(NamedComponentFinder.OP_EQUALS);
    }
    else {
      finder.setComponentClass(componentoClass);
      finder.setName(name);
    }
  }

  /**
   * Creates or reuses the abstract button finder.  Updates the button label.
   * Sets the wait to 2.  Sets the operation to OP_EQUALS.
   * @param buttonText
   */
  private void setupAbstractButtonFinder(String buttonLabel) {
    if (buttonFinder == null) {
      buttonFinder = new AbstractButtonFinder(buttonLabel);
      buttonFinder.setWait(2);
      buttonFinder.setOperation(NamedComponentFinder.OP_EQUALS);
    }
    else {
      buttonFinder.setText(buttonLabel);
    }
  }
}
