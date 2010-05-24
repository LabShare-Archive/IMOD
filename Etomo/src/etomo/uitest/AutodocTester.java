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
 * <p> Revision 1.28  2010/05/21 17:24:55  sueh
 * <p> bug# 1322 In executeCommand removed a print that repeated endlessly.
 * <p> In formatApplication excreased the sleep.
 * <p>
 * <p> Revision 1.27  2010/05/18 13:51:51  sueh
 * <p> bug# 1322 Printing the current progress bar label when the --names
 * <p> parameter is used.
 * <p>
 * <p> Revision 1.26  2010/05/16 00:41:50  sueh
 * <p> bug# 1371 Reformatting one time when a field is not found.
 * <p>
 * <p> Revision 1.24  2010/05/10 20:42:22  sueh
 * <p> bug# 1358 In executeCommand doing more sleeping for wait.process to avoid
 * <p>being fooled when kill button is disabled for a second.
 * <p>
 * $Log$
 * Revision 1.28  2010/05/21 17:24:55  sueh
 * bug# 1322 In executeCommand removed a print that repeated endlessly.
 * In formatApplication excreased the sleep.
 *
 * Revision 1.27  2010/05/18 13:51:51  sueh
 * bug# 1322 Printing the current progress bar label when the --names
 * parameter is used.
 *
 * Revision 1.26  2010/05/16 00:41:50  sueh
 * bug# 1371 Reformatting one time when a field is not found.
 *
 * Revision 1.25  2010/05/15 17:41:17  sueh
 * bug# 1358 For Windows using a 1 second wait after a button is pressed
 * instead of a 1 millisecond wait.
 * <p>
 * Revision 1.24 2010/05/10 20:42:22 sueh
 * <p>
 * bug# 1358 In executeCommand doing more sleeping for wait.process to avoid
 * being fooled when kill button is disabled for a second.
 * <p>
 * <p>
 * Revision 1.23 2010/04/29 01:36:30 sueh
 * <p>
 * <p> Revision 1.23  2010/04/29 01:36:30  sueh
 * <p> bug# 1356 In assertFileContainsString handle wildcards in the targetString.
 * <p>
 * <p> Revision 1.22  2010/03/03 05:11:20  sueh
 * <p> bug# 1311 Implemented set.debug.
 * <p>
 * <p> Revision 1.21  2010/02/24 02:57:43  sueh
 * <p> bug# 1301 Fixed null pointer exception in ifEnabled.
 * <p>
 * <p> Revision 1.20  2010/02/23 00:23:52  sueh
 * <p> bug# 1301 Fixed bug in assertFileContainsString.
 * <p>
 * <p> Revision 1.19  2010/02/17 05:04:02  sueh
 * <p> bug# 1301 Pulled enableField and ifField out of executeField.
 * <p>
 * <p> Revision 1.18  2009/11/20 17:43:01  sueh
 * <p> bug# 1282 Changed areFilesSame to assertSameFile and added the ability to compare files with different names.  Added assertFileContains.
 * <p>
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
  private boolean interfaceOpen = false;

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
  static AutodocTester getAutodocTester(final TestRunner testRunner,
      final JFCTestHelper helper, final ReadOnlyAutodoc autodoc,
      final File sourceDir, final String sectionType, final AxisID axisID,
      final VariableList parentVariableList) {
    AutodocTester tester = new AutodocTester(testRunner, helper, autodoc,
        sourceDir, sectionType, null, null, axisID, parentVariableList, null);
    tester.globalVariableMap = new HashMap();
    tester.globalVariableMap.put("axis", axisID.getExtension());
    tester.completedDialogSections = new HashSet();
    //openInterface must only run once
    if (axisID == AxisID.SECOND) {
      tester.interfaceOpen = true;
    }
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
  static AutodocTester getFunctionTester(final ReadOnlyAutodoc autodoc,
      final String sectionType, final String sectionName,
      final AutodocTester parentTester) {
    AutodocTester tester = new AutodocTester(parentTester.testRunner,
        parentTester.helper, autodoc, parentTester.sourceDir, sectionType,
        sectionName, null, parentTester.axisID, parentTester, parentTester);
    //Set up the function tester to run more functions.
    tester.functionAutodoc = autodoc;
    tester.functionSectionType = sectionType;
    tester.currentPanel = parentTester.currentPanel;
    //openInterface must only run once
    tester.interfaceOpen = true;
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
  static AutodocTester getSubsectionTester(final ReadOnlySection subsection,
      final AutodocTester parentTester, boolean interfaceOpen) {
    AutodocTester tester = new AutodocTester(parentTester.testRunner,
        parentTester.helper, parentTester.autodoc, parentTester.sourceDir,
        null, null, subsection, parentTester.axisID, parentTester, parentTester);
    tester.functionAutodoc = parentTester.functionAutodoc;
    tester.functionSectionType = parentTester.functionSectionType;
    tester.currentPanel = parentTester.currentPanel;
    //openInterface must only run once
    tester.interfaceOpen = interfaceOpen;
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
  private AutodocTester(final TestRunner testRunner,
      final JFCTestHelper helper, final ReadOnlyAutodoc autodoc,
      final File sourceDir, final String sectionType, final String sectionName,
      final ReadOnlySection subsection, final AxisID axisID,
      final VariableList parentVariableList, final AutodocTester parentTester) {
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
    boolean openingInterface = false;
    if (!interfaceOpen) {
      //OpeningInterface is set to true if the open interface subsection is
      //going to be run.
      openingInterface = openInterface() && childTester != null;
    }
    if (reader == null) {
      //first time running testUntilWait.
      nextSection();
    }
    if (reader.isDone()) {
      return;
    }
    if (interfaceOpen) {
      gotoFrame();
    }
    while (!reader.isDone()) {
      do {
        //Don't get a new command until the current function is completed.
        if (childTester != null) {
          childTester.testUntilWait();
          if (openingInterface && (childTester == null || childTester.isDone())) {
            //Successfully opened the interface.
            openingInterface = false;
            interfaceOpen = true;
            gotoFrame();
          }
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
      System.err.println("sectionType=" + sectionType + ",sectionName="
          + sectionName);
      System.err.println("nextSection:functionAutodoc="
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
   * Execute the optional subsection to open the interface (open = interface).
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   * @return true if the function completed
   */
  private boolean openInterface() throws FileNotFoundException, IOException,
      LogFile.LockException {
    if (parentTester != null || axisID == AxisID.SECOND) {
      //Only the first top-level tester is allowed to open the interface.
      return false;
    }
    executeCommand(testRunner.getInterfaceSection().getOpenInterfaceCommand());
    return true;
  }

  /**
   * Execute optional command to open a dialog called dialogName.  Called when "open.dialog ="
   * executed.
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  private void openDialog(final String dialogName)
      throws FileNotFoundException, IOException, LogFile.LockException {
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
  private boolean executeCommand(final Command command)
      throws FileNotFoundException, IOException, LogFile.LockException {
    if (command == null || !command.isKnown()) {
      return true;
    }
    BaseManager manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();
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
          assertSameFile(manager, command.getValue(0), command.getValue(1));
        }
        else {
          fail("unexpected command (" + command + ")");
        }
      }
      //assert.field = value
      else if (field != null) {
        assertField(command);
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
      testRunner.copyFile(command.getValue(0), command.getValue(1),
          modifierType == UITestModifierType.ALWAYS);
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
              .getSubsection(), this, interfaceOpen);
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
      //if.exists.field
      //if.not-exists.field
      else if (subjectType == null) {
        ifField(command);
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
              .getSubsection(), this, interfaceOpen);
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
            command.getSubsection(), this, interfaceOpen);
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
          System.err.println("run function " + command
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
          functionAutodoc = AutodocFactory.getInstance(manager, sourceDir,
              value, AxisID.ONLY);
        }
      }
      //set.debug
      else if (subjectType == UITestSubjectType.DEBUG) {
        debug = convertToBoolean(value);
        testRunner.setDebug(debug);
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
        try {
          Thread.sleep(500);
        }
        catch (InterruptedException e) {
        }
        if (killButton.isEnabled()) {
          return true;
        }
        //Decide if this is the right process
        String progressBarName = Utilities.convertLabelToName(progressBarLabel
            .getText());
        /*if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
         System.err.println("progressBarName=" + progressBarName);
         }*/
        if (!progressBarName.equals(subjectName)) {
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

  boolean isDialogSectionComplete(final String dialogSection) {
    assertNotNull("this function should be called in only top level testers",
        completedDialogSections);
    return completedDialogSections.contains(dialogSection);
  }

  AxisID getAxisID() {
    return axisID;
  }

  public void setVariable(final String variableName, final Object variableValue) {
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
  public String getVariableValue(final String variableName, final AxisID axisID) {
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
  public boolean isVariableSet(final String variableName, final AxisID axisID) {
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
   * Handle if.exists.subcommand and if.not-exists.subcommand.  Fails if the
   * action is not "if".
   * @param command
   * @return
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.LockException
   */
  private void ifExists(final Component component, final Command command)
      throws FileNotFoundException, IOException, LogFile.LockException {
    if (command.getActionType() == UITestActionType.IF) {
      //if.exists.field.subcommand
      if (command.getModifierType() == UITestModifierType.EXISTS) {
        assertNotNull("missing subcommand (" + command + ")", command
            .getSubcommand());
        if (component != null) {
          executeCommand(command.getSubcommand());
        }
      }
      //if.not-exists.field.subcommand
      else if (command.getModifierType() == UITestModifierType.NOT_EXISTS) {
        assertNotNull("missing subcommand (" + command + ")", command
            .getSubcommand());
        if (component == null) {
          executeCommand(command.getSubcommand());
        }
      }
      else {
        fail("unknown command" + " (" + command + ")");
      }
    }
    else {
      fail("unknown command" + " (" + command + ")");
    }
  }

  /**
   * Handle if.enabled/disabled.  Checks Component.isEnabled.  Fails if the
   * action isn't "if".
   * @param component
   * @param command
   */
  private void ifEnabled(final Component component, final Command command)
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
    //if.enabled.field.subcommand
    //if.disabled.field.subcommand
    assertNotNull("cannot find component (" + command + ")", component);
    if (actionType == UITestActionType.IF) {
      if (component.isEnabled() == enabled) {
        executeCommand(command.getSubcommand());
      }
    }
    else {
      fail("unexpected command (" + command + ")");
    }
  }

  /**
   * Handle assert.enabled/disabled.  Checks Component.isEnabled.  Fails if the
   * action isn't "assert".
   * @param component
   * @param command
   */
  private void assertEnabled(final Component component, final Command command)
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
    else {
      fail("unexpected command (" + command + ")");
    }
  }

  /**
   * Handle if.enabled/disabled.  Checks Component.isEnabled and
   * JTextComponent.isEditable.  Fails if the action isn't "if".
   * @param textComponent
   * @param command
   */
  private void ifEnabled(final JTextComponent textComponent,
      final Command command) throws FileNotFoundException, IOException,
      LogFile.LockException {
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
    //if.enabled.field.subcommand
    if (actionType == UITestActionType.IF) {
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
   * Handle assert.enabled/disabledd.  Checks Component.isEnabled and
   * JTextComponent.isEditable.  Fails if actions isn't "assert".
   * @param textComponent
   * @param command
   */
  private void assertEnabled(final JTextComponent textComponent,
      final Command command) throws FileNotFoundException, IOException,
      LogFile.LockException {
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
  private void assertSameFile(final BaseManager manager, final String fileName,
      final String compareToFileName) throws LogFile.LockException,
      FileNotFoundException, IOException {
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
    if (debug) {
      System.err.println("assertSameFile:file.getAbsolutePath()="
          + file.getAbsolutePath());
    }
    SystemProgram sortFile = new SystemProgram(manager, System
        .getProperty("user.dir"),
        new String[] { "sort", file.getAbsolutePath() }, AxisID.ONLY);
    sortFile.run();
    String[] stdOut = sortFile.getStdOutput();
    stdOut = stripCommentsAndBlankLines(stdOut);
    if (debug) {
      System.err.println("assertSameFile:storedFile.getAbsolutePath()="
          + storedFile.getAbsolutePath());
    }
    SystemProgram sortStoredFile = new SystemProgram(manager, System
        .getProperty("user.dir"), new String[] { "sort",
        storedFile.getAbsolutePath() }, AxisID.ONLY);
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
  private void assertFileContainsString(final String fileName,
      final String targetString) throws LogFile.LockException,
      FileNotFoundException, IOException {
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
    LogFile logFile = LogFile.getInstance(file);
    LogFile.ReaderId readerId = logFile.openReader();
    assertFalse("Unable to read " + fileName + "(" + command + ")\n", readerId
        .isEmpty());
    String line;
    //Break up the target string based on the wildcard character.
    String[] targetArray = targetString.split("\\*");
    //See if one of the lines matches the target array.
    while ((line = logFile.readLine(readerId)) != null) {
      int fromIndex = 0;
      boolean foundAMatch = false;
      //Look for each part of the targetArray in order along a single line.  The
      //matched areas may or may not be contiguious, since the wildcard matches
      //0 or more characters.
      for (int i = 0; i < targetArray.length; i++) {
        if (fromIndex >= line.length()) {
          //Haven't completed the match and there nothing left to match on this
          //line.
          break;
        }
        //Attempt to match with the current part of the target array.
        int index = line.indexOf(targetArray[i], fromIndex);
        if (index == -1) {
          //Part of the target array didn't match.
          break;
        }
        if (i == targetArray.length - 1) {
          //The last part of the target array matched.
          foundAMatch = true;
        }
        else {
          //Move start index past the current match (* matches 0 or more)
          fromIndex = index + targetArray[i].length();
        }
      }
      if (foundAMatch) {
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
  private void assertComparison(final JTextComponent textComponent,
      final Command command) throws FileNotFoundException, IOException,
      LogFile.LockException {
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

  private AbstractButton findButton(UITestFieldType fieldType, String name,
      int index) {
    try {
      Thread.sleep(1);
    }
    catch (InterruptedException e) {
    }
    setupNamedComponentFinder(AbstractButton.class, fieldType.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (debug) {
      finder.setDebug(true);
    }
    return (AbstractButton) finder.find(currentPanel, index);
  }

  private JCheckBox findCheckBox(String name, int index) {
    setupNamedComponentFinder(JCheckBox.class, UITestFieldType.CHECK_BOX
        .toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    return (JCheckBox) finder.find(currentPanel, index);
  }

  private JComboBox findComboBox(String name, int index) {
    setupNamedComponentFinder(JComboBox.class, UITestFieldType.COMBO_BOX
        .toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    return (JComboBox) finder.find(currentPanel, index);
  }

  private JMenuItem findMenuItem(String name, int index) {
    setupNamedComponentFinder(JMenuItem.class, UITestFieldType.MENU_ITEM
        .toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    return (JMenuItem) finder.find();
  }

  private void findContainer(String name) {
    setupNamedComponentFinder(JPanel.class, UITestFieldType.PANEL.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    currentPanel = (Container) finder.find();
  }

  private JRadioButton findRadioButton(String name, int index) {
    setupNamedComponentFinder(JRadioButton.class, UITestFieldType.RADIO_BUTTON
        .toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    return (JRadioButton) finder.find(currentPanel, index);
  }

  private JSpinner findSpinner(String name, int index) {
    setupNamedComponentFinder(JSpinner.class, UITestFieldType.SPINNER
        .toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    return (JSpinner) finder.find(currentPanel, index);
  }

  private JTabbedPane findTabbedPane(String name) {
    setupNamedComponentFinder(JTabbedPane.class, UITestFieldType.TAB.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    return (JTabbedPane) finder.find(currentPanel, 0);
  }

  private JTextComponent findTextField(String name, int index) {
    setupNamedComponentFinder(JTextField.class, UITestFieldType.TEXT_FIELD
        .toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    return (JTextComponent) finder.find(currentPanel, index);
  }

  /**
   * Executes the field part of a command.  Does not handle assert or if.
   * @param command
   * @param throwExceptionIfNotFound
   */
  private void executeField(final Command command)
      throws FileNotFoundException, IOException, LogFile.LockException {
    UITestModifierType modifierType = command.getModifierType();
    Field field = command.getField();
    UITestFieldType fieldType = field.getFieldType();
    String name = field.getName();
    int index = field.getIndex();
    boolean formatted = false;
    String value = command.getValue();
    assertNotNull("missing field (" + command + ")", field);
    //BUTTON
    if (fieldType == UITestFieldType.BUTTON) {
      AbstractButton button = null;
      while (button == null) {
        button = findButton(UITestFieldType.BUTTON, name, index);
        if (button == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      //bn.button_name =
      assertNull("value not valid in a button command (" + command + ")", value);
      MouseEventData mouseEventData = new MouseEventData(testRunner, button, 1);
      assertTrue("prepareComponent failed (" + command + ")", mouseEventData
          .prepareComponent());
      helper.enterClickAndLeave(mouseEventData);
      try {
        if (Utilities.isWindowsOS()) {
          Thread.sleep(1000);
        }
        else {
          Thread.sleep(1);
        }
      }
      catch (InterruptedException e) {
      }
    }
    //CHECK BOX
    else if (fieldType == UITestFieldType.CHECK_BOX) {
      JCheckBox checkBox = null;
      while (checkBox == null) {
        checkBox = findCheckBox(name, index);
        if (checkBox == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      //cb.check_box_name
      //if value is present,only click on check box to get it to match value
      if (value == null || checkBox.isSelected() != convertToBoolean(value)) {
        helper.enterClickAndLeave(new MouseEventData(testRunner, checkBox, 1));
        try {
          Thread.sleep(2);
        }
        catch (InterruptedException e) {
        }
      }
    }
    //COMBO BOX
    else if (fieldType == UITestFieldType.COMBO_BOX) {
      JComboBox comboBox = null;
      while (comboBox == null) {
        comboBox = findComboBox(name, index);
        if (comboBox == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      //cbb.combo_box_label
      comboBox.addItem(value);
      comboBox.setSelectedItem(value);
    }
    //MENU_ITEM
    else if (fieldType == UITestFieldType.MENU_ITEM) {
      JMenuItem menuItem = null;
      while (menuItem == null) {
        menuItem = findMenuItem(name, index);
        if (menuItem == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      //mn.menu_item_label
      //if value is present, only click on menu item when it matches value
      helper.enterClickAndLeave(new MouseEventData(testRunner, menuItem));
      //wait for menu to open
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
      }
    }
    //MINI BUTTON
    else if (fieldType == UITestFieldType.MINI_BUTTON) {
      AbstractButton miniButton = null;
      while (miniButton == null) {
        miniButton = findButton(UITestFieldType.MINI_BUTTON, name, index);
        if (miniButton == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      //mb.title_with_mini_button
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
    //PANEL
    //pnl.panel_title
    else if (fieldType == UITestFieldType.PANEL) {
      assertNull("value not valid in a panel command (" + command + ")", value);
      findContainer(name);
      if (currentPanel == null) {
        fail("can't find field - " + command.getField().getName() + " ("
            + command + ")");
      }
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
      }
    }
    //RADIO BUTTON
    else if (fieldType == UITestFieldType.RADIO_BUTTON) {
      JRadioButton radioButton = null;
      while (radioButton == null) {
        radioButton = findRadioButton(name, index);
        if (radioButton == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      //rb.radio_button_label
      assertNull("value not valid in a radio command (" + command + ")", value);
      helper.enterClickAndLeave(new MouseEventData(testRunner, radioButton));
      try {
        Thread.sleep(1);
      }
      catch (InterruptedException e) {
      }
    }
    //SPINNER
    else if (fieldType == UITestFieldType.SPINNER) {
      JSpinner spinner = null;
      while (spinner == null) {
        spinner = findSpinner(name, index);
        if (spinner == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      EtomoNumber nValue = new EtomoNumber();
      nValue.set(value);
      //sp.spinner_label = integer_value|up|down
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
    //TAB
    //tb.tabbed_panel_title.index_of_tab
    else if (fieldType == UITestFieldType.TAB) {
      //find the tabbed panel and click on the tab
      assertNull("value not valid in a tab command (" + command + ")", value);
      JTabbedPane tabbedPane = null;
      while (tabbedPane == null) {
        tabbedPane = findTabbedPane(name);
        if (tabbedPane == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
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
      JTextComponent textField = null;
      while (textField == null) {
        textField = findTextField(name, index);
        if (textField == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      //tf.text_field_label
      textField.setText(value);
    }
    else {
      fail("unexpected command (" + command + ")");
    }
    return;
  }

  /**
   * Call UIHarness.pack.
   */
  private void formatApplication() {
    UIHarness.INSTANCE.pack(axisID, EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    try {
      Thread.sleep(200);
    }
    catch (InterruptedException e) {
    }
  }

  /**
   * Runs assert.field actions.
   * @param command
   * @param throwExceptionIfNotFound
   */
  private void assertField(final Command command) throws FileNotFoundException,
      IOException, LogFile.LockException {
    UITestModifierType modifierType = command.getModifierType();
    Field field = command.getField();
    UITestFieldType fieldType = field.getFieldType();
    String name = field.getName();
    int index = field.getIndex();
    boolean formatted = false;
    String value = command.getValue();
    assertNotNull("missing field (" + command + ")", field);
    assertEquals("function can only handle assert field commands (" + command
        + ")", UITestActionType.ASSERT, command.getActionType());
    assertNull("function can only handle assert field commands (" + command
        + ")", command.getSubject());
    //assert.field
    //BUTTON
    if (fieldType == UITestFieldType.BUTTON) {
      AbstractButton button = null;
      while (button == null) {
        button = findButton(UITestFieldType.BUTTON, name, index);
        if (button == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      if (modifierType != null) {
        //assert.enabled.field
        //assert.disabled.field
        assertEnabled(button, command);
      }
      //assert.bn.button_name = button_state
      else {
        assertNotNull("value is required in an assert.bn command (" + command
            + ")", value);
        assertEquals("button state is not equal to value - " + value + " ("
            + command + ")", convertToBoolean(value), button.isSelected());
      }
    }
    //CHECK BOX
    else if (fieldType == UITestFieldType.CHECK_BOX) {
      setupNamedComponentFinder(JCheckBox.class, UITestFieldType.CHECK_BOX
          .toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name);
      JCheckBox checkBox = null;
      while (checkBox == null) {
        checkBox = (JCheckBox) finder.find(currentPanel, index);
        if (checkBox == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      if (modifierType != null) {
        //assert.enabled.field
        //assert.disabled.field
        assertEnabled(checkBox, command);
      }
      //assert.cb.check_box_name = check_box_state
      else {
        assertNotNull("value is required in an assert.cb command (" + command
            + ")", value);
        assertEquals("check box state is not equal to value - "
            + checkBox.getText() + "," + value + " (" + command + ")",
            convertToBoolean(value), checkBox.isSelected());
      }
    }
    //COMBO BOX
    else if (fieldType == UITestFieldType.COMBO_BOX) {
      JComboBox comboBox = null;
      comboBox = findComboBox(name, index);
      while (comboBox == null) {
        if (comboBox == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      if (modifierType != null) {
        if (modifierType == UITestModifierType.ENABLED
            || modifierType == UITestModifierType.DISABLED) {
          //assert.enabled.field
          //assert.disabled.field
          assertEnabled(comboBox, command);
        }
        else {
          fail("unknown action/modifier - " + comboBox.getSelectedItem() + ","
              + value + " (" + command + ")");
        }
      }
      //assert.cbb.combo_box_label
      else {
        //assert.cbb.combo_box_label = value
        if (value != null) {
          assertTrue(
              "combo box selected text is not equal to value - "
                  + comboBox.getSelectedItem() + "," + value + " (" + command
                  + ")", ((String) comboBox.getSelectedItem()).equals(value));
        }
        //assert.cbb.combo_box_label =
        else {
          assertEquals(
              "combo box selected text is not empty - "
                  + comboBox.getSelectedItem() + "," + value + " (" + command
                  + ")", null, comboBox.getSelectedItem());
        }
      }
    }
    //MENU_ITEM
    else if (fieldType == UITestFieldType.MENU_ITEM) {
      JMenuItem menuItem = null;
      while (menuItem == null) {
        menuItem = findMenuItem(name, index);
        if (menuItem == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      assertNotNull("modifier is required", modifierType);
      //assert.enabled.field
      //assert.disabled.field
      assertEnabled(menuItem, command);
    }
    //MINI BUTTON
    else if (fieldType == UITestFieldType.MINI_BUTTON) {
      AbstractButton miniButton = null;
      while (miniButton == null) {
        miniButton = findButton(UITestFieldType.MINI_BUTTON, name, index);
        if (miniButton == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      if (modifierType != null) {
        //assert.enabled.field
        //assert.disabled.field
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
    //RADIO BUTTON
    else if (fieldType == UITestFieldType.RADIO_BUTTON) {
      JRadioButton radioButton = null;
      while (radioButton == null) {
        radioButton = findRadioButton(name, index);
        if (radioButton == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      if (modifierType != null) {
        //assert.enabled.field
        //assert.disabled.field
        assertEnabled(radioButton, command);
      }
      //assert.rb.radio_button_label = radio_button_state
      else {
        assertNotNull("value is required in an assert.rb command (" + command
            + ")", value);
        assertEquals("radio button state is not equal to value - "
            + radioButton.getText() + "," + value + " (" + command + ")",
            convertToBoolean(value), radioButton.isSelected());
      }
    }
    //SPINNER
    else if (fieldType == UITestFieldType.SPINNER) {
      JSpinner spinner = null;
      while (spinner == null) {
        spinner = findSpinner(name, index);
        if (spinner == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      if (modifierType != null) {
        //assert.enabled.field
        //assert.disabled.field
        assertEnabled(spinner, command);
      }
      //assert.sp.spinner_label = integer_value
      else {
        EtomoNumber nValue = new EtomoNumber();
        nValue.set(value);
        if (value != null) {
          assertTrue("field text is not equal to value - " + spinner.getValue()
              + "," + value + " (" + command + ")", ((Number) spinner
              .getValue()).intValue() == nValue.getInt());
        }
        else {
          nValue.set((Number) spinner.getValue());
          assertTrue("field text is not empty - " + spinner.getValue() + ","
              + value + " (" + command + ")", nValue.isNull());
        }
      }
    }
    //TEXT FIELD
    else if (fieldType == UITestFieldType.TEXT_FIELD) {
      JTextComponent textField = null;
      while (textField == null) {
        textField = findTextField(name, index);
        if (textField == null) {
          if (!formatted) {
            formatApplication();
            formatted = true;
          }
          else {
            fail("can't find field - " + command.getField().getName() + " ("
                + command + ")");
            return;
          }
        }
      }
      if (modifierType != null) {
        if (modifierType == UITestModifierType.ENABLED
            || modifierType == UITestModifierType.DISABLED) {
          //assert.enabled.field
          //assert.disabled.field
          assertEnabled(textField, command);
        }
        //assert.ge
        //assert.le
        else if (modifierType == UITestModifierType.GE
            || modifierType == UITestModifierType.LE) {
          assertComparison(textField, command);
        }
        else {
          fail("unknown action/modifier - " + textField.getText() + "," + value
              + " (" + command + ")");
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
    else {
      fail("unexpected command (" + command + ")");
    }
  }

  /**
   * Runs if.field actions.
   * @param command
   * @param throwExceptionIfNotFound
   */
  private void ifField(final Command command) throws FileNotFoundException,
      IOException, LogFile.LockException {
    UITestModifierType modifierType = command.getModifierType();
    Field field = command.getField();
    UITestFieldType fieldType = field.getFieldType();
    String name = field.getName();
    int index = field.getIndex();
    String value = command.getValue();
    assertNotNull("missing field (" + command + ")", field);
    assertEquals("function can only handle assert field commands (" + command
        + ")", UITestActionType.IF, command.getActionType());
    assertNull("function can only handle if field commands (" + command + ")",
        command.getSubject());
    //if.field
    //BUTTON
    if (fieldType == UITestFieldType.BUTTON) {
      AbstractButton button = findButton(UITestFieldType.BUTTON, name, index);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(button, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        //if.enabled
        //if.disabled
        ifEnabled(button, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //CHECK BOX
    else if (fieldType == UITestFieldType.CHECK_BOX) {
      JCheckBox checkBox = findCheckBox(name, index);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(checkBox, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        //if.enabled
        //if.disabled
        ifEnabled(checkBox, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //COMBO BOX
    else if (fieldType == UITestFieldType.COMBO_BOX) {
      JComboBox comboBox = findComboBox(name, index);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(comboBox, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        //if.enabled
        //if.disabled
        ifEnabled(comboBox, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //MENU_ITEM
    else if (fieldType == UITestFieldType.MENU_ITEM) {
      JMenuItem menuItem = findMenuItem(name, index);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(menuItem, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        //if.enabled
        //if.disabled
        ifEnabled(menuItem, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //MINI BUTTON
    else if (fieldType == UITestFieldType.MINI_BUTTON) {
      AbstractButton miniButton = findButton(UITestFieldType.MINI_BUTTON, name,
          index);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(miniButton, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        //if.enabled
        //if.disabled
        ifEnabled(miniButton, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //PANEL
    //pnl.panel_title
    else if (fieldType == UITestFieldType.PANEL) {
      assertNull("value not valid in a panel command (" + command + ")", value);
      findContainer(name);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(currentPanel, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //RADIO BUTTON
    else if (fieldType == UITestFieldType.RADIO_BUTTON) {
      JRadioButton radioButton = findRadioButton(name, index);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(radioButton, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        //if.enabled
        //if.disabled
        ifEnabled(radioButton, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //SPINNER
    else if (fieldType == UITestFieldType.SPINNER) {
      JSpinner spinner = findSpinner(name, index);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(spinner, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        //if.enabled
        //if.disabled
        ifEnabled(spinner, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //TAB
    //tb.tabbed_panel_title.index_of_tab
    else if (fieldType == UITestFieldType.TAB) {
      //find the tabbed panel and click on the tab
      assertNull("value not valid in a tab command (" + command + ")", value);
      JTabbedPane tabbedPane = findTabbedPane(name);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(tabbedPane, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    //TEXT FIELD
    else if (fieldType == UITestFieldType.TEXT_FIELD) {
      JTextComponent textField = findTextField(name, index);
      //if.exists
      //if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(textField, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        //if.enabled
        //if.disabled
        ifEnabled(textField, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    else {
      fail("unexpected command (" + command + ")");
    }
    return;
  }

  /**
   * Converts all possible boolean strings to boolean.
   * @param input
   * @return
   */
  private boolean convertToBoolean(final String input) {
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
  private boolean convertToSpinnerControl(final String input) {
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
  private void setupNamedComponentFinder(final Class componentoClass,
      final String name) {
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
  private void setupAbstractButtonFinder(final String buttonLabel) {
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
