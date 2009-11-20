package etomo.uitest;

import java.awt.Component;
import java.awt.Container;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.AbstractButton;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
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
import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.ManagerKey;
import etomo.process.SystemProgram;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.EtomoNumber;
import etomo.type.UITestActionType;
import etomo.type.UITestFieldType;
import etomo.type.UITestSubjectType;
import etomo.ui.AxisProcessPanel;
import etomo.ui.ProgressPanel;
import etomo.ui.UIHarness;
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
 * <p> $Log$
 * <p> Revision 1.17  2009/10/29 20:03:24  sueh
 * <p> bug# 1280 Fix file chooser handler, which was crashing instead of waiting
 * <p> of the file chooser wasn't available.
 * <p>
 * <p> Revision 1.16  2009/10/23 19:48:43  sueh
 * <p> bug# 1275 In executeCommand implemented open.interface.field.
 * <p>
 * <p> Revision 1.15  2009/10/08 20:59:53  sueh
 * <p> bug# 1277 In executeField changed the name of minibuttons to mb.name.
 * <p>
 * <p> Revision 1.14  2009/09/28 18:37:28  sueh
 * <p> bug# 1235 In executeCommand, for wait.file-chooser, make sure that
 * <p> value isn't null and handle a fully qualified chosen_file.
 * <p>
 * <p> Revision 1.13  2009/09/22 21:04:34  sueh
 * <p> bug# 1259 Changed assert.equals.com to assert.same.file.
 * <p>
 * <p> Revision 1.12  2009/09/20 21:34:47  sueh
 * <p> bug# 1268 In executeCommand, fixed bug in ASSERT.
 * <p>
 * <p> Revision 1.11  2009/09/11 22:41:36  sueh
 * <p> bug# 1259 comparing com files by sorting them first.
 * <p>
 * <p> Revision 1.10  2009/09/06 01:40:21  sueh
 * <p> bug# 1259 Fixed a uitest bug.
 * <p>
 * <p> Revision 1.9  2009/09/01 03:18:33  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.8  2009/07/13 20:19:42  sueh
 * <p> Increased sleep for wait.process.  Ubuntu may have had a problem with
 * <p> checking the process bar too soon after the kill button is disabled.
 * <p>
 * <p> Revision 1.7  2009/06/10 17:26:41  sueh
 * <p> bug# 1202, bug # 1216 Added if.not-exists.field.subcommand to handle
 * <p> RAPTOR's not existing on all computers.
 * <p>
 * <p> Revision 1.6  2009/03/17 00:46:33  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.5  2009/03/02 20:58:00  sueh
 * <p> bug# 1102 Added assert.ge.tf and assert.le.tf.  Added compare
 * <p> (JTextComponent, Command) to test these commands.
 * <p>
 * <p> Revision 1.4  2009/02/20 18:14:56  sueh
 * <p> bug# 1102 Do a quick sleep after finding a panel to avoid unreliable
 * <p> behavior.
 * <p>
 * <p> Revision 1.3  2009/02/04 23:37:15  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.2  2009/01/28 00:57:50  sueh
 * <p> bug# 1102 In nextSection, moving subframe out of the the way.  In executeCommand handling the ifnot subsection and if.enabled.field.subcommand.  In executeField handling if.enabled.field.subcommand.  Changed assertEnabled to enabled to handle if.enabled.field.subcommand.
 * <p>
 * <p> Revision 1.1  2009/01/20 20:44:46  sueh
 * <p> bug# 1102 Tester of autodocs, functions, and subsections.
 * <p> </p>
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
   * Passes a variable up to the enclosing scope, if it exists, and exits the
   * current scope.
   * @param variableName
   */
  private void setReturn(final String variableName) {
    if (variableMap != null && variableMap.containsKey(variableName)) {
      parentVariableList.setVariable(variableName, variableMap
          .get(variableName));
    }
    setReturn();
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
      LogFile.LockException {
    //System.out.println("testUntilWait:axisID="+axisID);
    //If this is a top level tester and the frame hasn't been opened, try to
    //open the frame associated with this tester.
    if (!frameOpen) {
      frameOpen = openFrame();
      if (!frameOpen) {
        //Wait until frame can be opened.
        wait = true;
        try {
          Thread.sleep(1000);
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
      LogFile.LockException {
    boolean isAutodocTester = sectionType != null && sectionName == null;
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
      }
      if (reader.isDone()) {
        return;
      }
      //If this is a top level autodoc, try to turn off skipping
      if (skipToDialogSection != null && isAutodocTester
          && skipToDialogSection.equals(reader.getSectionName())) {
        //Found the section to skip to - stop skipping
        skipToDialogSection = null;
      }
      //If section type is not null and section name is null then this is an
      //autodoc tester.  This means that
      //it tests multiple sections in a Dialog autodoc, each with it's own scope.
      //It also means that each section is associated with a different dialog as
      //described in the interface section.
      if (isAutodocTester) {
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
    if (axisID == AxisID.SECOND && isAutodocTester) {
      UIHarness.INSTANCE.moveSubFrame();
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
      }
    }
  }

  /**
   * Execute required command to get to the frame panel for the current axis.
   * Called every time testUntilWait is called.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  private void gotoFrame() throws FileNotFoundException, IOException,
      LogFile.LockException {
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
      LogFile.LockException {
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
   * Execute optional command to open the interface.  Called when "open.interface"
   * is executed.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  private void openInterface() throws FileNotFoundException, IOException,
      LogFile.LockException {
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
      IOException, LogFile.LockException {
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
      IOException, LogFile.LockException {
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
    //FieldInterface
    if (actionType == null) {
      executeField(command);
    }
    //ASSERT
    else if (actionType == UITestActionType.ASSERT) {
      //assert.file
      if (subjectType == UITestSubjectType.FILE) {
        File file = new File(System.getProperty("user.dir"), value);
        //assert.contains.file =
        if (modifierType == UITestModifierType.CONTAINS) {
          assertFileContainsString(command.getValue(0), command.getValue(1));
        }
        //assert.exists.file = file_name
        else if (modifierType == UITestModifierType.EXISTS) {
          assertTrue("file does not exist - " + value + " (" + command + ")",
              file.exists());
        }
        //assert.not-exists.file = file_name
        else if (modifierType == UITestModifierType.NOT_EXISTS) {
          assertFalse("file exists - " + value + " (" + command + ")", file
              .exists());
        }
        //assert.same.file = file_name
        else if (modifierType == UITestModifierType.SAME) {
          assertSameFile(command.getValue(0), command.getValue(1));
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
      assertEquals("can only copy a file", UITestSubjectType.FILE, subjectType);
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
        if (isVariableSet(value, axisID)) {
          childTester = AutodocTester.getSubsectionTester(command
              .getSubsection(), this);
        }
      }
      //if.var
      else if (subjectType == UITestSubjectType.VAR) {
        boolean variableSet = isVariableSet(subjectName, axisID);
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
          String variableValue = getVariableValue(subjectName, axisID);
          assertNotNull("subcommand required in an if.comparison command - ("
              + command + ")", subcommand);
          //if.equals.var.variable_name.subcommand = variable_value
          if (modifierType == UITestModifierType.EQUALS) {
            if (variableValue.equals(value)) {
              executeCommand(subcommand);
            }
          }
          //if.not-equals.var.variable_name.subcommand = variable_value
          else if (modifierType == UITestModifierType.NOT_EQUALS) {
            if (!variableValue.equals(value)) {
              executeCommand(subcommand);
            }
          }
        }
      }
      //if.enabled.field
      //if.disabled.field
      //if.not-exists.field
      else if (subjectType == null) {
        //if.enabled.field.subcommand
        if (modifierType == UITestModifierType.ENABLED) {
          executeField(command);
        }
        //if.disabled.field.subcommand
        else if (modifierType == UITestModifierType.DISABLED) {
          executeField(command);
        }
        else if (modifierType == UITestModifierType.NOT_EXISTS) {
          executeField(command);
        }
        else {
          fail("unexpected command (" + command + ")");
        }
      }
    }
    //IFNOT
    else if (actionType == UITestActionType.IFNOT) {
      //[[ifnot = variable]]
      if (command.isSubsection()) {
        assertNull("modifier not used with this actionType (" + command + ")",
            modifierType);
        assertFalse("illegal section name - " + value + " (" + command + ")",
            value.startsWith("="));
        if (!isVariableSet(value, axisID)) {
          childTester = AutodocTester.getSubsectionTester(command
              .getSubsection(), this);
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
        if (field == null) {
          //call from the test autodoc to the open.interface command in the
          //interface section
          openInterface();
        }
        else {
          //open.interface.field
          executeField(command);
        }
      }
      else {
        fail("unexpected command (" + command.toString() + ")");
      }
    }
    //RETURN
    else if (actionType == UITestActionType.RETURN) {
      //return.var.variable_name
      if (subjectType == UITestSubjectType.VAR) {
        assertNotNull("missing variable name (" + command + ")", subjectName);
        setReturn(subjectName);
      }
      else {
        //return
        setReturn();
      }
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
    //SAVE
    else if (actionType == UITestActionType.SAVE) {
      //save
      assertNull("subject not used with this actionType (" + command + ")",
          subject);
      assertNull("modifier not used with this actionType (" + command + ")",
          modifierType);
      assertNull("field not used with this actionType (" + command + ")", field);
      assertNull("value not used with this actionType (" + command + ")", value);
      try {
        Thread.sleep(20);
      }
      catch (InterruptedException e) {
      }
      UIHarness.INSTANCE.save(axisID);
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
          ManagerKey managerKey = null;
          BaseManager manager = EtomoDirector.INSTANCE
              .getCurrentManagerForTest();
          if (manager != null) {
            managerKey = manager.getManagerKey();
          }
          functionAutodoc = AutodocFactory.getInstance(sourceDir, value,
              AxisID.ONLY, managerKey);
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
      assertNotNull("value is required (" + command + ")", value);
      //wait.file-chooser.file_chooser_title = chosen_file
      if (subjectType == UITestSubjectType.FILE_CHOOSER) {
        setupNamedComponentFinder(JFileChooser.class, subjectName);
        JFileChooser fileChooser = (JFileChooser) finder.find();
        if (fileChooser == null) {
          wait = true;
        }
        else {
          wait = false;
          File file;
          if (value.startsWith(File.separator)) {
            file = new File(value);
          }
          else {
            file = new File(getVariableValue(UITestSubjectType.TESTDIR
                .toString(), axisID), value);
          }
          fileChooser.setSelectedFile(file);
          try {
            Thread.sleep(1);
          }
          catch (InterruptedException e) {
          }
          /*String buttonName = "Open";
          setupAbstractButtonFinder(buttonName);
          AbstractButton button = (AbstractButton) buttonFinder.find(fileChooser, 0);
          assertNotNull("unable to find button to accept file or directory - " + buttonName + " ("
              + command + ")", button);
          System.out.println("button="+button);
          helper.enterClickAndLeave(new MouseEventData(testRunner, button));*/
          fileChooser.approveSelection();
        }
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
        setupNamedComponentFinder(JButton.class, UITestFieldType.BUTTON
            .toString()
            + AutodocTokenizer.SEPARATOR_CHAR
            + Utilities.convertLabelToName(AxisProcessPanel.KILL_BUTTON_LABEL));
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
          Thread.sleep(1500);
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
            + command + ")", value, progressBar.getString());
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
    else if (!actionType.isNoOp()) {
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

  public void setVariable(String variableName, Object variableValue) {
    if (variableMap == null) {
      variableMap = new HashMap();
    }
    variableMap.put(variableName, variableValue);
  }

  /**
   * Attempts to find variableName in variableMap.  It checks the global map.
   * If the variable isn't there, is calls this function in the parent variable
   * list.
   * @param variableName
   * @return variableValue
   */
  public String getVariableValue(String variableName, AxisID axisID) {
    if (variableMap != null && variableMap.containsKey(variableName)) {
      return (String) variableMap.get(variableName);
    }
    if (globalVariableMap != null
        && globalVariableMap.containsKey(variableName)) {
      return (String) globalVariableMap.get(variableName);
    }
    return parentVariableList.getVariableValue(variableName, axisID);
  }

  /**
   * Checks to see if variableMap contains the key variableName.  If it doesn't
   * calls the parent doesVariableExist.
   * @param variableName
   * @return variableExists
   */
  public boolean isVariableSet(String variableName, AxisID axisID) {
    if (variableMap != null && variableMap.containsKey(variableName)) {
      return true;
    }
    if (globalVariableMap != null
        && globalVariableMap.containsKey(variableName)) {
      return true;
    }
    return parentVariableList.isVariableSet(variableName, axisID);
  }

  public String toString() {
    if (sectionName == null) {
      return autodoc.getName() + "," + sectionType;
    }
    return autodoc.getName() + "," + sectionType + "," + sectionName;
  }

  /**
   * Handle the situation when a field cannot be found.
   * @param command
   * @param throwExceptionIfNotFound
   * @return
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.LockException
   */
  private boolean notExistsField(Command command,
      boolean throwExceptionIfNotFound) throws FileNotFoundException,
      IOException, LogFile.LockException {
    //if.not-exists.field.subcommand
    if (command.getActionType() == UITestActionType.IF
        && command.getModifierType() == UITestModifierType.NOT_EXISTS) {
      assertNotNull("missing subcommand (" + command + ")", command
          .getSubcommand());
      executeCommand(command.getSubcommand());
    }
    else if (throwExceptionIfNotFound) {
      fail("can't find field - " + command.getField().getName() + " ("
          + command + ")");
    }
    else {
      return false;
    }
    return true;
  }

  /**
   * Handle assert.enabled/disabled and if.enabled/disabled.  Checks
   * Component.isEnabled.
   * @param component
   * @param command
   */
  private void enabled(Component component, Command command)
      throws FileNotFoundException, IOException, LogFile.LockException {
    UITestActionType actionType = command.getActionType();
    UITestModifierType modifierType = command.getModifierType();
    boolean enabled = false;
    if (modifierType == UITestModifierType.ENABLED) {
      enabled = true;
    }
    else if (modifierType == UITestModifierType.DISABLED) {
      enabled = false;
    }
    else {
      fail("unexpected command (" + command + ")");
    }
    //assert.enabled.field
    //assert.disabled.field
    if (actionType == UITestActionType.ASSERT) {
      assertNull("assert.enabled/disabled command does not use a value ("
          + command + ")", command.getValue());
      assertEquals("component is not enabled/disabled (" + command + ")",
          enabled, component.isEnabled());
    }
    //if.enabled.field.subcommand
    //if.disabled.field.subcommand
    else if (actionType == UITestActionType.IF) {
      if (component.isEnabled() == enabled) {
        executeCommand(command.getSubcommand());
      }
    }
    else {
      fail("unexpected command (" + command + ")");
    }
  }

  /**
   * Handle assert.enabled/disabled and if.enabled/disabled.  Checks
   * Component.isEnabled and JTextComponent.isEditable.
   * @param textComponent
   * @param command
   */
  private void enabled(JTextComponent textComponent, Command command)
      throws FileNotFoundException, IOException, LogFile.LockException {
    assertNull("assert.enabled/disabled command does not use a value ("
        + command + ")", command.getValue());
    UITestActionType actionType = command.getActionType();
    UITestModifierType modifierType = command.getModifierType();
    boolean enabled = false;
    if (modifierType == UITestModifierType.ENABLED) {
      enabled = true;
    }
    else if (modifierType == UITestModifierType.DISABLED) {
      enabled = false;
    }
    else {
      fail("unexpected command (" + command + ")");
    }
    //assert.enabled.field
    if (actionType == UITestActionType.ASSERT) {
      assertNull("assert.enabled/disabled command does not use a value ("
          + command + ")", command.getValue());
      if (enabled) {
        assertTrue(
            "component is not enabled or not editable (" + command + ")",
            textComponent.isEnabled() && textComponent.isEditable());
      }
      //assert.disabled.field
      else {
        assertTrue("component is not disabled (" + command + ")",
            !textComponent.isEnabled() || !textComponent.isEditable());
      }
    }
    //if.enabled.field.subcommand
    else if (actionType == UITestActionType.IF) {
      if (enabled && textComponent.isEnabled() && textComponent.isEditable()) {
        executeCommand(command.getSubcommand());
      }
      //if.disabled.field.subcommand
      else if (!enabled
          && (!textComponent.isEnabled() || !textComponent.isEditable())) {
        executeCommand(command.getSubcommand());
      }
    }
    else {
      fail("unexpected command (" + command + ")");
    }
  }

  /**
   * Compares files.  Sorts the files and them compares them line by line.
   * The sorting is necessary because the order of a command may change
   * Returns null if file is equals with uitestDataDir/storedFileName.
   * Otherwise returns an error message.  Fails if the files are not the same.
   * @param file
   * @param storedFileName
   * @throws LogFile.LockException
   * @throws FileNotFoundException
   * @throws IOException
   */
  private void assertSameFile(String fileName, String compareToFileName)
      throws LogFile.LockException, FileNotFoundException, IOException {
    //Create dataset file from fileName
    if (fileName == null) {
      //One or more of the files are missing.
      fail("Missing fileName.  (" + command + ")\n");
    }
    File file = new File(System.getProperty("user.dir"), fileName);
    assertTrue(
        file.getAbsolutePath() + " does not exist.  (" + command + ")\n", file
            .exists());
    assertTrue(file.getAbsolutePath() + " is not a file.  (" + command + ")\n",
        file.isFile());

    //Create stored file from either compareToFileName or fileName.
    File storedFile;
    if (compareToFileName == null || compareToFileName.matches("\\s*")) {
      storedFile = new File(testRunner.getUitestDataDir(), fileName);
    }
    else {
      storedFile = new File(testRunner.getUitestDataDir(), compareToFileName);
    }
    assertTrue(storedFile.getAbsolutePath() + " does not exist.  (" + command
        + ")\n", storedFile.exists());
    assertTrue(storedFile.getAbsolutePath() + " is not a file.  (" + command
        + ")\n", storedFile.isFile());
    //Sort files because order of parameters can vary inside a command.
    SystemProgram sortFile = new SystemProgram(System.getProperty("user.dir"),
        new String[] { "sort", file.getAbsolutePath() }, AxisID.ONLY, null);
    sortFile.run();
    String[] stdOut = sortFile.getStdOutput();
    stdOut = stripCommentsAndBlankLines(stdOut);
    SystemProgram sortStoredFile = new SystemProgram(System
        .getProperty("user.dir"), new String[] { "sort",
        storedFile.getAbsolutePath() }, AxisID.ONLY, null);
    sortStoredFile.run();
    String[] storedStdOut = sortStoredFile.getStdOutput();
    storedStdOut = stripCommentsAndBlankLines(storedStdOut);
    if (stdOut == null && storedStdOut == null) {
      //Both files contains no comparable lines so they are the same.
      return;
    }
    assertTrue("One file has commands(s) and the other does not(\n"
        + file.getAbsolutePath() + ",\n" + storedFile.getAbsolutePath()
        + ").  (" + command + ")\n", stdOut != null && storedStdOut != null);
    //Compare lengths
    if (stdOut.length != storedStdOut.length) {
      assertTrue("Command(s) have unequal lengths:  " + file.getAbsolutePath()
          + ": " + stdOut.length + ",\n " + storedFile.getAbsolutePath() + ": "
          + storedStdOut.length + ".  (" + command + ")\n",
          stdOut.length == storedStdOut.length);
    }
    //Compare lines.
    for (int i = 0; i < stdOut.length; i++) {
      if (!stdOut[i].trim().equals(storedStdOut[i].trim())) {
        //Found an unequal line that is not a comment.
        fail("Unequal lines:" + file.getAbsolutePath() + ":\n" + stdOut[i]
            + "\n" + storedFile.getAbsolutePath() + ":\n" + storedStdOut[i]
            + ".\n(" + command + ")\n");
      }
    }
  }

  /**
   * Looks for a string in a file.  Fails if the file does not exist or the 
   * string is not found.
   * @param fileName
   * @param stargetString
   * @throws LogFile.LockException
   * @throws FileNotFoundException
   * @throws IOException
   */
  private void assertFileContainsString(String fileName, String targetString)
      throws LogFile.LockException, FileNotFoundException, IOException {
    //Create dataset file from fileName
    if (fileName == null) {
      //One or more of the files are missing.
      fail("Missing fileName.  (" + command + ")\n");
    }
    File file = new File(System.getProperty("user.dir"), fileName);
    assertTrue(
        file.getAbsolutePath() + " does not exist.  (" + command + ")\n", file
            .exists());
    assertTrue(file.getAbsolutePath() + " is not a file.  (" + command + ")\n",
        file.isFile());
    assertFalse("Target string is empty.  (" + command + ")\n",
        targetString == null || targetString.matches("\\s*"));
    //Compare lines.
    LogFile logFile = LogFile.getInstance(file, null);
    LogFile.ReaderId readerId = logFile.openReader();
    assertFalse("Unable to read " + fileName + "(" + command + ")\n", readerId
        .isEmpty());
    String line;
    while ((line = logFile.readLine(readerId)) != null) {
      if (line.indexOf(targetString) != -1) {
        logFile.closeReader(readerId);
        return;
      }
    }
    logFile.closeReader(readerId);
    fail(targetString + "\nnot found in " + fileName + "(" + command + ")\n");
  }

  String[] stripCommentsAndBlankLines(String[] stringArray) {
    if (stringArray == null || stringArray.length == 0) {
      return stringArray;
    }
    List strippedList = new ArrayList();
    for (int i = 0; i < stringArray.length; i++) {
      String line = stringArray[i].trim();
      if (line.length() > 0 && !line.startsWith("#")) {
        strippedList.add(line);
      }
    }
    if (strippedList.size() == 0) {
      return new String[0];
    }
    else if (strippedList.size() == 1) {
      return new String[] { (String) strippedList.get(0) };
    }
    else {
      return (String[]) strippedList.toArray(new String[strippedList.size()]);
    }
  }

  /**
   * Handle assert.ge/le.  Checks text field value against command value.
   * @param textComponent
   * @param command
   */
  private void compare(JTextComponent textComponent, Command command)
      throws FileNotFoundException, IOException, LogFile.LockException {
    assertNotNull("assert.ge/le command must use a value (" + command + ")",
        command.getValue());
    UITestModifierType modifierType = command.getModifierType();
    EtomoNumber fieldValue = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    fieldValue.set(textComponent.getText());
    EtomoNumber commandValue = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    commandValue.set(command.getValue());
    assertTrue("only numeric comparisons are valid (" + command + ")",
        fieldValue.isValid() && commandValue.isValid());
    //assert.ge.field
    if (modifierType == UITestModifierType.GE) {
      assertTrue(
          "the value of this field is not greater or equal to the value if this command ("
              + command + ")", fieldValue.ge(commandValue));
    }
    else if (modifierType == UITestModifierType.LE) {
      assertTrue(
          "the value of this field is not less then or equal to the value if this command ("
              + command + ")", fieldValue.le(commandValue));
    }
    else {
      fail("unknown modifier - " + textComponent.getText() + ","
          + command.getValue() + " (" + command + ")");
    }
  }

  private void executeField(Command command) throws FileNotFoundException,
      IOException, LogFile.LockException {
    executeField(command, true);
  }

  /**
   * Executes the field part of a command.  Also handles asserting field states.
   * Return false if the field cannot be found and failIfNotFound is false;
   * unless the command is an if.not-exists command, otherwise return true if a failure is not asserted
   * @param command
   * @param throwExceptionIfNotFound
   * @return boolean
   */
  private boolean executeField(Command command, boolean throwExceptionIfNotFound)
      throws FileNotFoundException, IOException, LogFile.LockException {
    UITestActionType actionType = command.getActionType();
    UITestModifierType modifierType = command.getModifierType();
    Field field = command.getField();
    UITestFieldType fieldType = field.getFieldType();
    String name = field.getName();
    int index = field.getIndex();
    String value = command.getValue();
    assertNotNull("missing field (" + command + ")", field);
    boolean fieldActionSet = false;
    if (actionType != null && command.getSubject() == null) {
      //If the action type is assert or if, and it is not associated with a subject,
      //then this is an assert field command or an if field command.
      //assert.field
      fieldActionSet = true;
    }
    //BUTTON
    if (fieldType == UITestFieldType.BUTTON) {
      try {
        Thread.sleep(1);
      }
      catch (InterruptedException e) {
      }
      setupNamedComponentFinder(AbstractButton.class, UITestFieldType.BUTTON
          .toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      AbstractButton button = (AbstractButton) finder.find(currentPanel, index);
      if (button == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //bn.button_name =
      if (!fieldActionSet) {
        assertNull("value not valid in a button command (" + command + ")",
            value);
        helper.enterClickAndLeave(new MouseEventData(testRunner, button, 1));
        try {
          Thread.sleep(1);
        }
        catch (InterruptedException e) {
        }
      }
      //if.not-exists
      else if (actionType == UITestActionType.IF
          && modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      //assert
      //if
      else {
        if (modifierType != null) {
          enabled(button, command);
        }
        //assert.bn.button_name = button_state
        else {
          assertEquals("only assert doesn't require a modifier",
              UITestActionType.ASSERT, actionType);
          assertNotNull("value is required in an assert.bn command (" + command
              + ")", value);
          assertEquals("button state is not equal to value - " + value + " ("
              + command + ")", convertToBoolean(value), button.isSelected());
        }
      }
    }
    //CHECK BOX
    else if (fieldType == UITestFieldType.CHECK_BOX) {
      setupNamedComponentFinder(JCheckBox.class, UITestFieldType.CHECK_BOX
          .toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      JCheckBox checkBox = (JCheckBox) finder.find(currentPanel, index);
      if (checkBox == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //cb.check_box_name
      if (!fieldActionSet) {
        //if value is present,only click on check box to get it to match value
        if (value == null || checkBox.isSelected() != convertToBoolean(value)) {
          helper
              .enterClickAndLeave(new MouseEventData(testRunner, checkBox, 1));
          try {
            Thread.sleep(2);
          }
          catch (InterruptedException e) {
          }
        }
      }
      //if.not-exists
      else if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      //assert
      //if
      else {
        if (modifierType != null) {
          enabled(checkBox, command);
        }
        //assert.cb.check_box_name = check_box_state
        else {
          assertEquals("only assert doesn't require a modifier",
              UITestActionType.ASSERT, actionType);
          assertNotNull("value is required in an assert.cb command (" + command
              + ")", value);
          assertEquals("check box state is not equal to value - "
              + checkBox.getText() + "," + value + " (" + command + ")",
              convertToBoolean(value), checkBox.isSelected());
        }
      }
    }
    //COMBO BOX
    else if (fieldType == UITestFieldType.COMBO_BOX) {
      setupNamedComponentFinder(JComboBox.class, UITestFieldType.COMBO_BOX
          .toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      JComboBox comboBox = (JComboBox) finder.find(currentPanel, index);
      if (comboBox == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //cbb.combo_box_label
      if (!fieldActionSet) {
        comboBox.addItem(value);
        comboBox.setSelectedItem(value);
      }
      //if.not-exists
      else if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      //assert
      //if
      else {
        if (modifierType != null) {
          if (modifierType == UITestModifierType.ENABLED
              || modifierType == UITestModifierType.DISABLED) {
            //assert.enabled
            //assert.disabled
            enabled(comboBox, command);
          }
          else {
            fail("unknown action/modifier - " + comboBox.getSelectedItem()
                + "," + value + " (" + command + ")");
          }
        }
        //assert.cbb.combo_box_label
        else {
          //assert.cbb.combo_box_label = value
          if (value != null) {
            assertTrue("combo box selected text is not equal to value - "
                + comboBox.getSelectedItem() + "," + value + " (" + command
                + ")", ((String) comboBox.getSelectedItem()).equals(value));
          }
          //assert.cbb.combo_box_label =
          else {
            assertEquals("combo box selected text is not empty - "
                + comboBox.getSelectedItem() + "," + value + " (" + command
                + ")", null, comboBox.getSelectedItem());
          }
        }
      }
    }
    //MENU_ITEM
    else if (fieldType == UITestFieldType.MENU_ITEM) {
      setupNamedComponentFinder(JMenuItem.class, UITestFieldType.MENU_ITEM
          .toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      JMenuItem menuItem = (JMenuItem) finder.find();
      if (menuItem == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //mn.menu_item_label
      if (!fieldActionSet) {
        //if value is present, only click on menu item when it matches value
        helper.enterClickAndLeave(new MouseEventData(testRunner, menuItem));
        //wait for menu to open
        try {
          Thread.sleep(1000);
        }
        catch (InterruptedException e) {
        }
      }
      //if.not-exists
      else if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      //assert
      //if
      else {
        assertNotNull("modifier is required", modifierType);
        enabled(menuItem, command);
      }
    }
    //MINI BUTTON
    else if (fieldType == UITestFieldType.MINI_BUTTON) {
      //The name for a mini-button is actually mb.name.
      setupNamedComponentFinder(AbstractButton.class,
          UITestFieldType.MINI_BUTTON.toString()
              + AutodocTokenizer.SEPARATOR_CHAR + name);
      AbstractButton miniButton = (AbstractButton) finder.find(currentPanel,
          index);
      if (miniButton == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //mb.title_with_mini_button
      if (!fieldActionSet) {
        //if value is present,only click on mini-button when it matches value
        //mb.title_with_mini_button =
        //mb.title_with_mini_button = current_label
        if (value == null
            || miniButton.getText().equals("<html><b>" + value + "</b>")) {
          helper.enterClickAndLeave(new MouseEventData(testRunner, miniButton));
          try {
            Thread.sleep(1);
          }
          catch (InterruptedException e) {
          }
        }
      }
      //if.not-exists
      else if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      //assert
      //if
      else {
        if (modifierType != null) {
          enabled(miniButton, command);
        }
        //assert.mb.title_with_mini_button = current_label
        else {
          assertEquals("only assert doesn't require a modifier",
              UITestActionType.ASSERT, actionType);
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
      assertFalse("cannot assert or if a panel (" + command + ")",
          fieldActionSet);
      setupNamedComponentFinder(JPanel.class, UITestFieldType.PANEL.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      currentPanel = (Container) finder.find();
      if (currentPanel == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //if.not-exists
      if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      else {
        try {
          Thread.sleep(1000);
        }
        catch (InterruptedException e) {
        }
      }
    }
    //RADIO BUTTON
    else if (fieldType == UITestFieldType.RADIO_BUTTON) {
      setupNamedComponentFinder(JRadioButton.class,
          UITestFieldType.RADIO_BUTTON.toString()
              + AutodocTokenizer.SEPARATOR_CHAR + name);
      JRadioButton radioButton = (JRadioButton) finder
          .find(currentPanel, index);
      if (radioButton == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //rb.radio_button_label
      if (!fieldActionSet) {
        assertNull("value not valid in a radio command (" + command + ")",
            value);
        helper.enterClickAndLeave(new MouseEventData(testRunner, radioButton));
        try {
          Thread.sleep(1);
        }
        catch (InterruptedException e) {
        }
      }
      //if.not-exists
      else if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      //assert
      //if
      else {
        if (modifierType != null) {
          enabled(radioButton, command);
        }
        //assert.rb.radio_button_label = radio_button_state
        else {
          assertEquals("only assert doesn't require a modifier",
              UITestActionType.ASSERT, actionType);
          assertNotNull("value is required in an assert.rb command (" + command
              + ")", value);
          assertEquals("radio button state is not equal to value - "
              + radioButton.getText() + "," + value + " (" + command + ")",
              convertToBoolean(value), radioButton.isSelected());
        }
      }
    }
    //SPINNER
    else if (fieldType == UITestFieldType.SPINNER) {
      setupNamedComponentFinder(JSpinner.class, UITestFieldType.SPINNER
          .toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      JSpinner spinner = (JSpinner) finder.find(currentPanel, index);
      if (spinner == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      EtomoNumber nValue = new EtomoNumber();
      nValue.set(value);
      //sp.spinner_label = integer_value|up|down
      if (!fieldActionSet) {
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
      //if.not-exists
      else if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      //assert
      //if
      else {
        if (modifierType != null) {
          enabled(spinner, command);
        }
        //assert.sp.spinner_label = integer_value
        else {
          assertEquals("only assert doesn't require a modifier",
              UITestActionType.ASSERT, actionType);
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
      assertFalse("not action can be associated with a tab (" + command + ")",
          fieldActionSet);
      setupNamedComponentFinder(JTabbedPane.class, UITestFieldType.TAB
          .toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      JTabbedPane tabbedPane = (JTabbedPane) finder.find(currentPanel, 0);
      if (tabbedPane == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //if.not-exists
      if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      helper.enterClickAndLeave(new JTabbedPaneMouseEventData(testRunner,
          tabbedPane, index, 1));
      try {
        Thread.sleep(1);
      }
      catch (InterruptedException e) {
      }
    }
    //TEXT FIELD
    else if (fieldType == UITestFieldType.TEXT_FIELD) {
      setupNamedComponentFinder(JTextField.class, UITestFieldType.TEXT_FIELD
          .toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      JTextComponent textField = (JTextComponent) finder.find(currentPanel,
          index);
      if (textField == null) {
        return notExistsField(command, throwExceptionIfNotFound);
      }
      //tf.text_field_label
      if (!fieldActionSet) {
        textField.setText(value);
      }
      //if.not-exists
      else if (actionType == UITestActionType.IF
          || modifierType == UITestModifierType.NOT_EXISTS) {
        //The field exists so the "if" test failed - nothing to do.
        return true;
      }
      //assert
      //if
      else {
        if (modifierType != null) {
          if (modifierType == UITestModifierType.ENABLED
              || modifierType == UITestModifierType.DISABLED) {
            enabled(textField, command);
          }
          //assert.ge
          //assert.le
          else if (actionType == UITestActionType.ASSERT
              && (modifierType == UITestModifierType.GE || modifierType == UITestModifierType.LE)) {
            compare(textField, command);
          }
          else {
            fail("unknown action/modifier - " + textField.getText() + ","
                + value + " (" + command + ")");
          }
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
