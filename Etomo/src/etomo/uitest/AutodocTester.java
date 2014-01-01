package etomo.uitest;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.KeyEvent;
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
import junit.extensions.jfcunit.eventdata.KeyEventData;
import junit.extensions.jfcunit.eventdata.MouseEventData;
import junit.extensions.jfcunit.finder.AbstractButtonFinder;
import junit.extensions.jfcunit.finder.ComponentFinder;
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
import etomo.type.ProcessEndState;
import etomo.type.UITestActionType;
import etomo.type.UITestFieldType;
import etomo.type.UITestSubjectType;
import etomo.ui.swing.AxisProcessPanel;
import etomo.ui.swing.ProgressPanel;
import etomo.ui.swing.UIHarness;
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
 * <p> Revision 1.55  2011/07/14 21:21:26  sueh
 * <p> Increasing waiting in formatApplication.
 * <p>
 * <p> Revision 1.54  2011/07/12 03:33:27  sueh
 * <p> In executeCommand handled a file chooser in Windows.
 * <p>
 * <p> Revision 1.53  2011/07/08 18:34:01  sueh
 * <p> In formatApplication, Increasing format time.
 * <p>
 * <p> Revision 1.52  2011/06/29 03:13:46  sueh
 * <p> In formatApplication increasing format wait time.
 * <p>
 * <p> Revision 1.51  2011/06/28 02:33:02  sueh
 * <p> Allow an extra reformat because the advanced button could not be found in a UITest.
 * <p>
 * <p> Revision 1.50  2011/05/26 02:42:35  sueh
 * <p> First format isn't always working - allow a maximum of two.
 * <p>
 * <p> Revision 1.49  2011/05/25 14:20:12  sueh
 * <p> Increasing wait after formatting application.
 * <p>
 * <p> Revision 1.48  2011/05/24 23:07:31  sueh
 * <p> Bug# 1478 In assertField, fixed combo box comparison.
 * <p>
 * <p> Revision 1.47  2011/05/24 15:49:39  sueh
 * <p> FormatApplication was not being called in all cases - fixed.
 * <p>
 * <p> Revision 1.46  2011/05/23 16:08:00  sueh
 * <p> Trying to get formatApplication to solve the missing Advanced button problem more often.  Increased
 * <p> the wait after formatting.  Made it easy to change the number of formats done.
 * <p>
 * <p> Revision 1.45  2011/05/20 21:49:56  sueh
 * <p> Trying to get formatApplication to solve the missing Advanced button problem more often.
 * <p>
 * <p> Revision 1.44  2011/05/13 03:47:14  sueh
 * <p> Increase formatApplication sleep.
 * <p>
 * <p> Revision 1.43  2011/02/22 21:51:07  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.42  2010/11/13 16:08:08  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.41  2010/11/10 20:56:59  sueh
 * <p> In executeCommand slowing down assert.exists.file.  In Windows it
 * <p> may have run too fast to avoid file locks.
 * <p>
 * <p> Revision 1.40  2010/11/09 21:54:53  sueh
 * <p> In executeCommand slowing down assert.exists.file.  In Windows it
 * <p> may have run too fast to avoid file locks.
 * <p>
 * <p> Revision 1.39  2010/11/09 01:33:07  sueh
 * <p> In executeCommand slowing down assert.not-exists.file.  In Windows it
 * <p> may have run too fast to avoid file locks.
 * <p>
 * <p> Revision 1.38  2010/06/12 00:50:36  sueh
 * <p> bug# 1383 Try a REDRAW_WAIT of 3.
 * <p>
 * <p> Revision 1.37  2010/06/10 21:58:45  sueh
 * <p> bug# 1383 Added REDRAW_WAIT.  Using it with fields that may require a
 * <p> panel to be redrawn.
 * <p>
 * <p> Revision 1.36  2010/06/09 21:27:48  sueh
 * <p> bug# 1383 In executeField increased the button sleep to 3 milliseconds.
 * <p>
 * <p> Revision 1.35  2010/06/08 19:04:33  sueh
 * <p> bug# 1383 Adding format action.  Changed formatApplication sleep to a quarter of a second.  Increased the checkbox and radio button sleeps to
 * <p> 3 milliseconds.
 * <p>
 * <p> Revision 1.34  2010/06/08 16:36:55  sueh
 * <p> In formatApplication increased sleep.
 * <p>
 * <p> Revision 1.33  2010/06/07 14:54:24  sueh
 * <p> In formatApplication increased sleep.
 * <p>
 * <p> Revision 1.32  2010/06/07 13:47:23  sueh
 * <p> In formatApplication increased sleep.
 * <p>
 * <p> Revision 1.31  2010/06/02 17:09:03  sueh
 * <p> In formatApplication, increasing wait.
 * <p>
 * <p> Revision 1.30  2010/05/28 19:10:02  sueh
 * <p> In formatApplication added a System.err.print so that I can see if it is helping.
 * <p>
 * <p> Revision 1.29  2010/05/24 17:09:50  sueh
 * <p> In formatApplication increased sleep.
 * <p>
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
 * Revision 1.55  2011/07/14 21:21:26  sueh
 * Increasing waiting in formatApplication.
 *
 * Revision 1.54  2011/07/12 03:33:27  sueh
 * In executeCommand handled a file chooser in Windows.
 *
 * Revision 1.53  2011/07/08 18:34:01  sueh
 * In formatApplication, Increasing format time.
 *
 * Revision 1.52  2011/06/29 03:13:46  sueh
 * In formatApplication increasing format wait time.
 *
 * Revision 1.51  2011/06/28 02:33:02  sueh
 * Allow an extra reformat because the advanced button could not be found in a UITest.
 *
 * Revision 1.50  2011/05/26 02:42:35  sueh
 * First format isn't always working - allow a maximum of two.
 *
 * Revision 1.49  2011/05/25 14:20:12  sueh
 * Increasing wait after formatting application.
 *
 * Revision 1.48  2011/05/24 23:07:31  sueh
 * Bug# 1478 In assertField, fixed combo box comparison.
 *
 * Revision 1.47  2011/05/24 15:49:39  sueh
 * FormatApplication was not being called in all cases - fixed.
 *
 * Revision 1.46  2011/05/23 16:08:00  sueh
 * Trying to get formatApplication to solve the missing Advanced button problem more often.  Increased
 * the wait after formatting.  Made it easy to change the number of formats done.
 *
 * Revision 1.45  2011/05/20 21:49:56  sueh
 * Trying to get formatApplication to solve the missing Advanced button problem more often.
 *
 * Revision 1.44  2011/05/13 03:47:14  sueh
 * Increase formatApplication sleep.
 *
 * Revision 1.43  2011/02/22 21:51:07  sueh
 * bug# 1437 Reformatting.
 *
 * Revision 1.42  2010/11/13 16:08:08  sueh
 * bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 *
 * Revision 1.41  2010/11/10 20:56:59  sueh
 * In executeCommand slowing down assert.exists.file.  In Windows it
 * may have run too fast to avoid file locks.
 *
 * Revision 1.40  2010/11/09 21:54:53  sueh
 * In executeCommand slowing down assert.exists.file.  In Windows it
 * may have run too fast to avoid file locks.
 *
 * Revision 1.39  2010/11/09 01:33:07  sueh
 * In executeCommand slowing down assert.not-exists.file.  In Windows it
 * may have run too fast to avoid file locks.
 *
 * Revision 1.38  2010/06/12 00:50:36  sueh
 * bug# 1383 Try a REDRAW_WAIT of 3.
 *
 * Revision 1.37  2010/06/10 21:58:45  sueh
 * bug# 1383 Added REDRAW_WAIT.  Using it with fields that may require a
 * panel to be redrawn.
 *
 * Revision 1.36  2010/06/09 21:27:48  sueh
 * bug# 1383 In executeField increased the button sleep to 3 milliseconds.
 *
 * Revision 1.35  2010/06/08 19:04:33  sueh
 * bug# 1383 Adding format action.  Changed formatApplication sleep to a quarter of a second.  Increased the checkbox and radio button sleeps to
 * 3 milliseconds.
 *
 * Revision 1.34  2010/06/08 16:36:55  sueh
 * In formatApplication increased sleep.
 *
 * Revision 1.33  2010/06/07 14:54:24  sueh
 * In formatApplication increased sleep.
 *
 * Revision 1.32  2010/06/07 13:47:23  sueh
 * In formatApplication increased sleep.
 *
 * Revision 1.31  2010/06/02 17:09:03  sueh
 * In formatApplication, increasing wait.
 *
 * Revision 1.30  2010/05/28 19:10:02  sueh
 * In formatApplication added a System.err.print so that I can see if it is helping.
 *
 * Revision 1.29  2010/05/24 17:09:50  sueh
 * In formatApplication increased sleep.
 *
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

  private static final int REDRAW_WAIT = 100;
  private static final int MAX_FORMAT = 5;
  private static final int FORMAT_WAIT = 700;
  private static final int MAX_FRAME_WAIT = 2;
  private static final int PROCESS_CHECK_MAX = 10;

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
  private NamedComponentFinder namedFinder = null;
  private ComponentFinder finder = null;
  private AbstractButtonFinder buttonFinder = null;
  private Command command = null;
  private String skipToDialogSection = null;
  private boolean debug = false;
  private Set completedDialogSections = null;
  private boolean interfaceOpen = false;
  private int frameWait = 0;
  private boolean tryAgain = false;
  private int processCheck = 0;

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
      final JFCTestHelper helper, final ReadOnlyAutodoc autodoc, final File sourceDir,
      final String sectionType, final AxisID axisID, final VariableList parentVariableList) {
    AutodocTester tester = new AutodocTester(testRunner, helper, autodoc, sourceDir,
        sectionType, null, null, axisID, parentVariableList, null);
    tester.globalVariableMap = new HashMap();
    tester.globalVariableMap.put("axis", axisID.getExtension());
    tester.completedDialogSections = new HashSet();
    // openInterface must only run once
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
      final String sectionType, final String sectionName, final AutodocTester parentTester) {
    AutodocTester tester = new AutodocTester(parentTester.testRunner,
        parentTester.helper, autodoc, parentTester.sourceDir, sectionType, sectionName,
        null, parentTester.axisID, parentTester, parentTester);
    // Set up the function tester to run more functions.
    tester.functionAutodoc = autodoc;
    tester.functionSectionType = sectionType;
    tester.currentPanel = parentTester.currentPanel;
    // openInterface must only run once
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
        parentTester.helper, parentTester.autodoc, parentTester.sourceDir, null, null,
        subsection, parentTester.axisID, parentTester, parentTester);
    tester.functionAutodoc = parentTester.functionAutodoc;
    tester.functionSectionType = parentTester.functionSectionType;
    tester.currentPanel = parentTester.currentPanel;
    // openInterface must only run once
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
  private AutodocTester(final TestRunner testRunner, final JFCTestHelper helper,
      final ReadOnlyAutodoc autodoc, final File sourceDir, final String sectionType,
      final String sectionName, final ReadOnlySection subsection, final AxisID axisID,
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
      parentVariableList.setVariable(variableName, variableMap.get(variableName));
    }
    setReturn();
  }

  /**
   * Exit from the current scope.
   */
  private void setReturn() {
    if (sectionType == null || sectionName != null) {
      // Either a function tester or a subsection test so just set
      // reader.done=true to leave the scope.
      reader.setDone();
    }
    else {
      // This is an autodoc tester so make testUntilWait() call nextSection().
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
      // The childTester is active, so this call will have originated from it.
      // Set the childTester to null so that isDone() will return true.
      childTester = null;
    }
    if (parentTester != null) {
      // This is either a function tester or a subsection tester so must tell
      // parent to end.
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
  void testUntilWait() throws FileNotFoundException, IOException, LogFile.LockException {
    boolean openingInterface = false;
    if (!interfaceOpen) {
      // OpeningInterface is set to true if the open interface subsection is
      // going to be run.
      openingInterface = openInterface() && childTester != null;
    }
    if (reader == null) {
      // first time running testUntilWait.
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
        // Don't get a new command until the current function is completed.
        if (childTester != null) {
          childTester.testUntilWait();
          if (openingInterface && (childTester == null || childTester.isDone())) {
            // Successfully opened the interface.
            openingInterface = false;
            interfaceOpen = true;
            gotoFrame();
          }
          // SetEnd() could have set the childTester to null.
          if (childTester != null) {
            if (childTester.isDone()) {
              childTester = null;
            }
            else if (childTester.isWait()) {
              return;
            }
          }
        }
        // Before getting a new command, wait for the current command if it is a
        // wait command.
        if (!wait && !tryAgain) {
          command = reader.nextCommand(command);
        }
        if (command != null && command.isKnown()) {
          executeCommand(command);
          if (wait) {
            return;
          }
        }
      } while (!reader.isDone() && command != null && skipToDialogSection == null);
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
      System.err.println("sectionType=" + sectionType + ",sectionName=" + sectionName);
      System.err.println("nextSection:functionAutodoc=" + functionAutodoc.getName()
          + ",functionSectionType=" + functionSectionType);
    }
    do {
      // Get the next section
      if (reader == null) {
        if (sectionType == null) {
          // Gets a reader for the subsection that was passed in
          reader = CommandReader.getSubsectionReader(autodoc, subsection, axisID, this);
        }
        else if (sectionName == null) {
          // Opens the first section to be read.
          reader = CommandReader.getAutodocReader(autodoc, sectionType, axisID, this);
          if (skipToDialogSection != null
              && skipToDialogSection.equals(reader.getSectionName())) {
            // Found the starting dialog - stop skipping
            skipToDialogSection = null;
          }
        }
        else {
          // open function section
          reader = CommandReader.getSectionReader(autodoc, sectionType, sectionName,
              axisID, this);
          assertFalse("called function that doesn't exist - " + sectionType + " = "
              + sectionName + " - in " + autodoc.getName(), reader.isDone());
        }
      }
      else if (!reader.isDone()) {
        if (completedDialogSections != null) {
          // add to list of completed (and skipped) dialog sections
          completedDialogSections.add(reader.getSectionName());
        }
        reader.nextSection();
      }
      if (reader.isDone()) {
        return;
      }
      // If this is a top level autodoc, try to turn off skipping
      if (skipToDialogSection != null && isAutodocTester
          && skipToDialogSection.equals(reader.getSectionName())) {
        // Found the section to skip to - stop skipping
        skipToDialogSection = null;
      }
      // If section type is not null and section name is null then this is an
      // autodoc tester. This means that
      // it tests multiple sections in a Dialog autodoc, each with it's own scope.
      // It also means that each section is associated with a different dialog as
      // described in the interface section.
      if (isAutodocTester) {
        // Remove section level scope
        // function scope
        functionAutodoc = null;
        functionSectionType = null;
        // Variable scope
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
      // Only the first top-level tester is allowed to open the interface.
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
  private void openDialog(final String dialogName) throws FileNotFoundException,
      IOException, LogFile.LockException {
    executeCommand(testRunner.getInterfaceSection().getOpenDialogCommand(dialogName));
  }

  /**
   * Executes an action command.  Returns without doing anything for  null
   * command.  If the action command read a field command, it will call
   * executeFieldCommand.
   * @param command
   * @return false if an optional command fails, otherwise return true or fail an assert
   */
  private void executeCommand(final Command command) throws FileNotFoundException,
      IOException, LogFile.LockException {
    if (command == null || !command.isKnown()) {
      return;
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
    // FieldInterface
    if (actionType == null) {
      executeField(command);
    }
    // ASSERT
    else if (actionType == UITestActionType.ASSERT) {
      // assert.file
      if (subjectType == UITestSubjectType.FILE) {
        File file = new File(System.getProperty("user.dir"), value);
        // assert.contains.file =
        if (modifierType == UITestModifierType.CONTAINS) {
          assertFileContainsString(command.getValue(0), command.getValue(1));
        }
        // assert.exists.file = file_name
        else if (modifierType == UITestModifierType.EXISTS) {
          try {
            Thread.sleep(2);
          }
          catch (InterruptedException e) {
          }
          assertTrue("file does not exist - " + value + " (" + command + ")",
              file.exists());
        }
        // assert.not-exists.file = file_name
        else if (modifierType == UITestModifierType.NOT_EXISTS) {
          try {
            Thread.sleep(1);
          }
          catch (InterruptedException e) {
          }
          assertFalse("file exists - " + value + " (" + command + ")", file.exists());
        }
        // assert.same.file = file_name
        else if (modifierType == UITestModifierType.SAME) {
          assertSameFile(manager, command.getValue(0), command.getValue(1));
        }
        else {
          fail("unexpected command (" + command + ")");
        }
      }
      // assert.field = value
      else if (field != null) {
        assertField(command);
      }
      else {
        fail("unexpected command (" + command + ")");
      }
    }
    // COPY
    else if (actionType == UITestActionType.COPY) {
      assertTrue("only the always modifier is allowed with this actionType (" + command
          + ")", modifierType == null || modifierType == UITestModifierType.ALWAYS);
      // copy.file = file_name
      assertEquals("can only copy a file", UITestSubjectType.FILE, subjectType);
      testRunner.copyFile(command.getValue(0), command.getValue(1),
          modifierType == UITestModifierType.ALWAYS);
    }
    // END
    else if (actionType == UITestActionType.END) {
      setEnd();
    }
    // FORMAT
    else if (actionType == UITestActionType.FORMAT) {
      formatApplication();
      // format.field
      if (field != null) {
        executeField(command);
      }
    }
    // IF
    else if (actionType == UITestActionType.IF) {
      // [[if = variable]]
      if (command.isSubsection()) {
        assertNull("modifier not used with this actionType (" + command + ")",
            modifierType);
        assertFalse("illegal section name - " + value + " (" + command + ")",
            value.startsWith("="));
        if (isVariableSet(value, axisID)) {
          childTester = AutodocTester.getSubsectionTester(command.getSubsection(), this,
              interfaceOpen);
        }
      }
      // if.var
      else if (subjectType == UITestSubjectType.VAR) {
        boolean variableSet = isVariableSet(subjectName, axisID);
        if (modifierType == null) {
          if (variableSet) {
            // if.var.action
            if (subcommand != null) {
              executeCommand(subcommand);
            }
            // if.var.field
            else {
              executeField(command);
            }
          }
        }
        // if.not.var
        else if (modifierType == UITestModifierType.NOT) {
          if (!variableSet) {
            // if.not.var.action
            if (subcommand != null) {
              executeCommand(subcommand);
            }
            // if.not.var.field
            else {
              executeField(command);
            }
          }
        }
        else {
          String variableValue = getVariableValue(subjectName, axisID);
          assertNotNull("subcommand required in an if.comparison command - (" + command
              + ")", subcommand);
          // if.equals.var.variable_name.subcommand = variable_value
          if (modifierType == UITestModifierType.EQUALS) {
            if (variableValue.equals(value)) {
              executeCommand(subcommand);
            }
          }
          // if.not-equals.var.variable_name.subcommand = variable_value
          else if (modifierType == UITestModifierType.NOT_EQUALS) {
            if (!variableValue.equals(value)) {
              executeCommand(subcommand);
            }
          }
        }
      }
      // if.wait.process.subcommand
      else if (subjectType == UITestSubjectType.PROCESS) {
        assertEquals("modifier must be wait (" + command + ")", modifierType,
            UITestModifierType.WAIT);
        assertNotNull("process name is required (" + command + ")", subjectName);
        assertNotNull("end state is required (" + command + ")", value);
        // Get the kill process button
        setupNamedComponentFinder(JButton.class,
            UITestFieldType.BUTTON.toString() + AutodocTokenizer.SEPARATOR_CHAR
                + Utilities.convertLabelToName(AxisProcessPanel.KILL_BUTTON_LABEL));
        JButton killButton = (JButton) namedFinder.find(currentPanel, 0);
        assertNotNull("can't find kill button (" + command + ")", killButton);
        // Get the progress bar label
        setupNamedComponentFinder(JLabel.class, ProgressPanel.LABEL_NAME);
        JLabel progressBarLabel = (JLabel) namedFinder.find(currentPanel, 0);
        assertNotNull("can't find progress bar label (" + command + ")", progressBarLabel);
        // Get the progress bar
        setupNamedComponentFinder(JProgressBar.class, ProgressPanel.NAME);
        JProgressBar progressBar = (JProgressBar) namedFinder.find(currentPanel, 0);
        assertNotNull("can't find progress bar label (" + command + ")", progressBar);
        wait = true;
        // Decide if this is the right process
        String progressBarName = Utilities.convertLabelToName(progressBarLabel.getText());
        if (!progressBarName.equals(subjectName)) {
          return;
        }
        String progressString = progressBar.getString();
        if (progressString != null && progressString.indexOf(value) != -1) {
          // Found the process and state - run the subcommand
          wait = false;
          if (subcommand != null) {
            executeCommand(subcommand);
          }
        }
        else if (!killButton.isEnabled() && isProcessDone(killButton)) {
          // If the process is really done, the state was not found, so do not run the
          // subcommand.
          wait = false;
        }
        return;
      }
      // if.enabled.field
      // if.disabled.field
      // if.exists.field
      // if.not-exists.field
      else if (subjectType == null) {
        ifField(command);
      }
    }
    // IFNOT
    else if (actionType == UITestActionType.IFNOT) {
      // [[ifnot = variable]]
      if (command.isSubsection()) {
        assertNull("modifier not used with this actionType (" + command + ")",
            modifierType);
        assertFalse("illegal section name - " + value + " (" + command + ")",
            value.startsWith("="));
        if (!isVariableSet(value, axisID)) {
          childTester = AutodocTester.getSubsectionTester(command.getSubsection(), this,
              interfaceOpen);
        }
      }
      else {
        fail("unexpected command (" + command + ")");
      }
    }
    // GOTO
    else if (actionType == UITestActionType.GOTO) {
      assertNull("modifier not used with this actionType (" + command + ")", modifierType);
      // goto.frame
      if (subjectType == UITestSubjectType.FRAME) {
        executeField(command);
      }
      else {
        fail("unexpected command (" + command + ")");
      }
    }
    // OPEN
    else if (actionType == UITestActionType.OPEN) {
      assertNull("modifier not used with this actionType (" + command + ")", modifierType);
      // [[open = interface]]
      if (command.isSubsection()) {
        assertTrue("the open legal value for an open subsection is interface",
            value.equals(UITestSubjectType.INTERFACE.toString()));
        assertFalse("illegal section name - " + value + " (" + command + ")",
            value.startsWith("="));
        childTester = AutodocTester.getSubsectionTester(command.getSubsection(), this,
            interfaceOpen);
      }
      // open.dialog
      else if (subjectType == UITestSubjectType.DIALOG) {
        assertNotNull("dialog name is required (" + command + ")", subjectName);
        // open.dialog
        if (field == null) {
          // refers to the interface section
          openDialog(subjectName);
        }
        // open.dialog.field
        else {
          // probably a command from the interface section
          executeField(command);
        }
      }
    }
    // RETURN
    else if (actionType == UITestActionType.RETURN) {
      // return.var.variable_name
      if (subjectType == UITestSubjectType.VAR) {
        assertNotNull("missing variable name (" + command + ")", subjectName);
        setReturn(subjectName);
      }
      else {
        // return
        setReturn();
      }
    }
    // RUN
    else if (actionType == UITestActionType.RUN) {
      // run.function.section_name
      if (subjectType == UITestSubjectType.FUNCTION) {
        if (debug) {
          System.err.println("run function " + command + ",functionSectionType="
              + functionSectionType);
        }
        assertNull("modifier not used with this actionType (" + command + ")",
            modifierType);
        assertNotNull("missing section name (" + command + ")", subjectName);
        assertNotNull("missing function autodoc - run set.adoc (" + command + ")",
            functionAutodoc);
        assertNotNull("missing function section type - run set.adoc (" + command + ")",
            functionSectionType);
        childTester = AutodocTester.getFunctionTester(functionAutodoc,
            functionSectionType, subjectName, this);
        if (debug) {
          childTester.setDebug();
        }
      }
    }
    // SAVE
    else if (actionType == UITestActionType.SAVE) {
      // save
      assertNull("subject not used with this actionType (" + command + ")", subject);
      assertNull("modifier not used with this actionType (" + command + ")", modifierType);
      assertNull("field not used with this actionType (" + command + ")", field);
      assertNull("value not used with this actionType (" + command + ")", value);
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
      }
      UIHarness.INSTANCE.save(axisID);
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
      }
    }
    // SET
    else if (actionType == UITestActionType.SET) {
      assertNull("modifier not used with this actionType (" + command + ")", modifierType);
      // set.adoc.section_type
      if (subjectType == UITestSubjectType.ADOC) {
        assertNotNull("missing section type (" + command + ")", subjectName);
        functionSectionType = subjectName;
        // set.adoc.section_type =
        if (value == null) {
          functionAutodoc = autodoc;
        }
        // set.adoc.section_type = autodoc_name
        else {
          functionAutodoc = AutodocFactory.getTestInstance(manager, sourceDir, value,
              AxisID.ONLY);
        }
      }
      // set.debug
      else if (subjectType == UITestSubjectType.DEBUG) {
        debug = convertToBoolean(value);
        testRunner.setDebug(debug);
      }
      // set.index
      else if (subjectType == UITestSubjectType.INDEX) {
        executeField(command);
      }
      // set.var.variable_name
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
    // SKIPTO
    else if (actionType == UITestActionType.SKIPTO) {
      assertNull("modifier not used with this actionType (" + command + ")", modifierType);
      // skipto.dialog.dialog_section_name
      if (subjectType == UITestSubjectType.DIALOG) {
        assertNotNull("dialog name is missing (" + command + ")", subjectName);
        skipToDialogSection = subjectName;
      }
      else {
        fail("unexpected command (" + command.toString() + ")");
      }
    }
    // SLEEP
    else if (actionType == UITestActionType.SLEEP) {
      assertNull("modifier not used with this actionType (" + command + ")", modifierType);
      assertNull("subject not used with this actionType (" + command + ")", subjectType);
      assertNull("field not used with this actionType (" + command + ")", field);
      EtomoNumber interval = new EtomoNumber();
      // sleep = interval
      if (value != null) {
        interval.set(value);
      }
      // sleep =
      else {
        interval.set(1000);
      }
      try {
        Thread.sleep(interval.getInt());
      }
      catch (InterruptedException e) {
      }
    }
    // TOUCH
    else if (actionType == UITestActionType.TOUCH) {
      assertTrue("only the always modifier is allowed with this actionType (" + command
          + ")", modifierType == null);
      // touch.file = file_name
      if (subjectType == UITestSubjectType.FILE) {
        touchFile(command.getValue(0));
      }
      else if (subjectType == UITestSubjectType.DIR) {
        touchDir(command.getValue(0));
      }
      else {
        fail("can only touch a file or a directory");
      }
    }
    // UNSET
    else if (actionType == UITestActionType.SET) {
      // unset.var.variable_name
      if (subjectType == UITestSubjectType.VAR) {
        assertNotNull("missing variable name (" + command + ")", subjectName);
        if (variableMap != null) {
          variableMap.remove(subjectName);
        }
      }
      else {
        fail("unexpected command (" + command.toString() + ")");
      }
    }
    // WAIT
    else if (actionType == UITestActionType.WAIT) {
      assertNotNull("value is required (" + command + ")", value);
      assertNull("modifier not used with this actionType (" + command + ")", modifierType);
      // Take naps during the wait to avoid driving the load up
      try {
        Thread.sleep(5);
      }
      catch (InterruptedException e) {
      }
      // wait.file-chooser.file_chooser_title = chosen_file
      if (subjectType == UITestSubjectType.FILE_CHOOSER) {
        setupComponentFinder(JFileChooser.class);
        JFileChooser fileChooser = (JFileChooser) finder.find();
        if (fileChooser == null) {
          wait = true;
        }
        else {
          // Make sure its the right fileChooser
          String fileChooserName = fileChooser.getName();
          if (fileChooserName == null || !fileChooserName.equals(subjectName)) {
            // Close incorrect fileChooser and fail
            helper.sendKeyAction(new KeyEventData(testRunner, fileChooser,
                KeyEvent.VK_ESCAPE));
            fail("wrong file chooser - " + fileChooserName + " (" + command + ")");
          }
          wait = false;
          File file;
          if (value.startsWith(File.separator)
              || (Utilities.isWindowsOS() && value.charAt(1) == ':')) {
            file = new File(value);
          }
          else {
            file = new File(
                getVariableValue(UITestSubjectType.TESTDIR.toString(), axisID), value);
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
      // wait.popup.popup_title = dismiss_button_label
      else if (subjectType == UITestSubjectType.POPUP) {
        assertNotNull("popup name is required (" + command + ")", subjectName);
        assertNotNull("button to close popup is required (" + command + ")", value);
        if (!wait) {
          wait = true;
        }
        // Try to get the pop up immediately
        setupComponentFinder(JOptionPane.class);
        Container popup = (Container) finder.find();
        // If popup hasn't popped up, keep waiting.
        if (popup == null) {
          return;
        }
        // Make sure its the right popup
        String popupName = popup.getName();
        if (popupName == null || !popupName.equals(subjectName)) {
          // Close incorrect popup and fail
          helper.sendKeyAction(new KeyEventData(testRunner, popup, KeyEvent.VK_ESCAPE));
          fail("wrong popup - " + popupName + " (" + command + ")");
        }
        // close popup
        setupAbstractButtonFinder(value);
        AbstractButton button = (AbstractButton) buttonFinder.find(popup, 0);
        assertNotNull("unable to find button to close popup - " + value + " (" + command
            + ")", button);
        helper.enterClickAndLeave(new MouseEventData(testRunner, button));
        try {
          Thread.sleep(REDRAW_WAIT);
        }
        catch (InterruptedException e) {
        }
        wait = false;
        if (wait) {
          return;
        }
      }
      // wait.process
      else if (subjectType == UITestSubjectType.PROCESS) {
        assertNotNull("process name is required (" + command + ")", subjectName);
        assertNotNull("end state is required (" + command + ")", value);
        // Get the kill process button
        setupNamedComponentFinder(JButton.class,
            UITestFieldType.BUTTON.toString() + AutodocTokenizer.SEPARATOR_CHAR
                + Utilities.convertLabelToName(AxisProcessPanel.KILL_BUTTON_LABEL));
        JButton killButton = (JButton) namedFinder.find(currentPanel, 0);
        assertNotNull("can't find kill button (" + command + ")", killButton);
        // Get the progress bar label
        setupNamedComponentFinder(JLabel.class, ProgressPanel.LABEL_NAME);
        JLabel progressBarLabel = (JLabel) namedFinder.find(currentPanel, 0);
        assertNotNull("can't find progress bar label (" + command + ")", progressBarLabel);
        // Get the progress bar
        setupNamedComponentFinder(JProgressBar.class, ProgressPanel.NAME);
        JProgressBar progressBar = (JProgressBar) namedFinder.find(currentPanel, 0);
        assertNotNull("can't find progress bar label (" + command + ")", progressBar);
        if (!wait) {
          wait = true;
          return;
        }
        // Already waited at least once - now see whether the process is done.
        // Waiting for anything but a single process or the last process in a
        // series will not work.

        // Decide if the process is still running
        if (killButton.isEnabled()) {
          return;
        }
        // The killButton turns on and off in between processes. Avoid exiting
        // in that case.
        if (!isProcessDone(killButton)) {
          return;
        }
        // Decide if this is the right process
        String progressBarName = Utilities.convertLabelToName(progressBarLabel.getText());
        if (!progressBarName.equals(subjectName)) {
          return;
        }
        String progressString = progressBar.getString();
        if (!isEndProgressString(progressString, value)) {
          // A final progress string is either the command, or "killed", "paused", etc.
          return;
        }
        // The right process is done
        wait = false;
        // Check the end_state
        assertFalse(
            "process ended with the wrong state -" + value + " (" + command + ")",
            progressString.indexOf(value) == -1);
      }
      // wait.test
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
    // WRITE
    // write.file
    else if (actionType == UITestActionType.WRITE) {
      appendStringToFile(command.getValue(0), command.getValue(1));
    }
    else if (!actionType.isNoOp()) {
      fail("unexpected command (" + command.toString() + ")");
    }
    return;
  }

  /**
   * Returns true if the killButton stays disabled.
   * @param killButton
   * @return
   */
  private boolean isProcessDone(final JButton killButton) {
    if (killButton == null) {
      return true;
    }
    // The killButton turns on and off in between processes. Avoid exiting
    // in that case.
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    if (killButton.isEnabled()) {
      return false;
    }
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    if (killButton.isEnabled()) {
      return false;
    }
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    if (killButton.isEnabled()) {
      return false;
    }
    return true;
  }

  /**
   * Returns true if progressString contains an end string like "done" or "killed".
   * @param progressString
   * @param expectedString
   * @param modifierType
   * @return
   */
  private boolean isEndProgressString(final String progressString,
      final String expectedString) {
    if (ProcessEndState.isValid(progressString)) {
      return true;
    }
    if (progressString == null) {
      return false;
    }
    return progressString.indexOf(expectedString) != -1;
  }

  /**
   * touch a file in the working directory.
   * @param fileName
   */
  private void touchFile(final String fileName) {
    if (fileName == null || fileName.matches("\\s*")) {
      return;
    }
    touch(new File(System.getProperty("user.dir"), fileName));
  }

  /**
   * touch a directory in the working directory if it exists, otherwise create it.
   * @param fileName
   */
  private void touchDir(final String dirName) {
    if (dirName == null || dirName.matches("\\s*")) {
      return;
    }
    File dir = new File(System.getProperty("user.dir"), dirName);
    if (dir.exists()) {
      touch(dir);
    }
    else {
      dir.mkdirs();
    }
  }

  private void touch(final File file) {
    SystemProgram program = new SystemProgram(null, System.getProperty("user.dir"),
        new String[] { "python", BaseManager.getIMODBinPath() + "b3dtouch",
            file.getAbsolutePath() }, AxisID.ONLY);
    program.run();
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
    if (globalVariableMap != null && globalVariableMap.containsKey(variableName)) {
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
    if (globalVariableMap != null && globalVariableMap.containsKey(variableName)) {
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
      // if.exists.field.subcommand
      if (command.getModifierType() == UITestModifierType.EXISTS) {
        assertNotNull("missing subcommand (" + command + ")", command.getSubcommand());
        if (component != null) {
          executeCommand(command.getSubcommand());
        }
      }
      // if.not-exists.field.subcommand
      else if (command.getModifierType() == UITestModifierType.NOT_EXISTS) {
        assertNotNull("missing subcommand (" + command + ")", command.getSubcommand());
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
    // if.enabled.field.subcommand
    // if.disabled.field.subcommand
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
    // assert.enabled.field
    // assert.disabled.field
    if (actionType == UITestActionType.ASSERT) {
      assertNull(
          "assert.enabled/disabled command does not use a value (" + command + ")",
          command.getValue());
      assertEquals("component is not enabled/disabled (" + command + ")", enabled,
          component.isEnabled());
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
  private void ifEnabled(final JTextComponent textComponent, final Command command)
      throws FileNotFoundException, IOException, LogFile.LockException {
    assertNull("assert.enabled/disabled command does not use a value (" + command + ")",
        command.getValue());
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
    // if.enabled.field.subcommand
    if (actionType == UITestActionType.IF) {
      if (enabled && textComponent.isEnabled() && textComponent.isEditable()) {
        executeCommand(command.getSubcommand());
      }
      // if.disabled.field.subcommand
      else if (!enabled && (!textComponent.isEnabled() || !textComponent.isEditable())) {
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
  private void assertEnabled(final JTextComponent textComponent, final Command command)
      throws FileNotFoundException, IOException, LogFile.LockException {
    assertNull("assert.enabled/disabled command does not use a value (" + command + ")",
        command.getValue());
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
    // assert.enabled.field
    if (actionType == UITestActionType.ASSERT) {
      assertNull(
          "assert.enabled/disabled command does not use a value (" + command + ")",
          command.getValue());
      if (enabled) {
        assertTrue("component is not enabled or not editable (" + command + ")",
            textComponent.isEnabled() && textComponent.isEditable());
      }
      // assert.disabled.field
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
    // Create dataset file from fileName
    if (fileName == null) {
      // One or more of the files are missing.
      fail("Missing fileName.  (" + command + ")\n");
    }
    File file = new File(System.getProperty("user.dir"), fileName);
    assertTrue(file.getAbsolutePath() + " does not exist.  (" + command + ")\n",
        file.exists());
    assertTrue(file.getAbsolutePath() + " is not a file.  (" + command + ")\n",
        file.isFile());

    // Create stored file from either compareToFileName or fileName.
    File storedFile;
    if (compareToFileName == null || compareToFileName.matches("\\s*")) {
      storedFile = new File(testRunner.getUitestDataDir(), fileName);
    }
    else {
      storedFile = new File(testRunner.getUitestDataDir(), compareToFileName);
    }
    assertTrue(storedFile.getAbsolutePath() + " does not exist.  (" + command + ")\n",
        storedFile.exists());
    assertTrue(storedFile.getAbsolutePath() + " is not a file.  (" + command + ")\n",
        storedFile.isFile());
    // Sort files because order of parameters can vary inside a command.
    if (debug) {
      System.err.println("assertSameFile:file.getAbsolutePath()="
          + file.getAbsolutePath());
    }
    String command;
    if (Utilities.isWindowsOS()) {
      command = "C:/cygwin/bin/sort";
    }
    else {
      command = "sort";
    }
    SystemProgram sortFile = new SystemProgram(manager, System.getProperty("user.dir"),
        new String[] { command, file.getAbsolutePath() }, AxisID.ONLY);
    sortFile.run();
    String[] stdOut = sortFile.getStdOutput();
    stdOut = stripCommentsAndBlankLines(stdOut);
    if (debug) {
      System.err.println("assertSameFile:storedFile.getAbsolutePath()="
          + storedFile.getAbsolutePath());
    }
    SystemProgram sortStoredFile = new SystemProgram(manager,
        System.getProperty("user.dir"), new String[] { command,
            storedFile.getAbsolutePath() }, AxisID.ONLY);
    sortStoredFile.run();
    String[] storedStdOut = sortStoredFile.getStdOutput();
    storedStdOut = stripCommentsAndBlankLines(storedStdOut);
    if (stdOut == null && storedStdOut == null) {
      // Both files contains no comparable lines so they are the same.
      return;
    }
    assertTrue(
        "One file has commands(s) and the other does not(\n" + file.getAbsolutePath()
            + ",\n" + storedFile.getAbsolutePath() + ").  (" + command + ")\n",
        stdOut != null && storedStdOut != null);
    // Compare lengths
    if (stdOut.length != storedStdOut.length) {
      assertTrue("Command(s) have unequal lengths:  " + file.getAbsolutePath() + ": "
          + stdOut.length + ",\n " + storedFile.getAbsolutePath() + ": "
          + storedStdOut.length + ".  (" + command + ")\n",
          stdOut.length == storedStdOut.length);
    }
    // Compare lines.
    for (int i = 0; i < stdOut.length; i++) {
      String[] line = stdOut[i].trim().split("\\s+");
      String[] storedLine = storedStdOut[i].trim().split("\\s+");
      for (int j = 0; j < line.length; j++) {
        if (j >= storedLine.length) {
          fail("Unequal line lengths:" + file.getAbsolutePath() + ":\n" + stdOut[i]
              + "\n" + storedFile.getAbsolutePath() + ":\n" + storedStdOut[i] + ".\n("
              + command + ")\n");
        }
        else if (!line[j].equals(storedLine[j])) {
          // Found an unequal line that is not a comment.
          fail("Unequal lines:" + file.getAbsolutePath() + ":\n" + stdOut[i] + "\n"
              + storedFile.getAbsolutePath() + ":\n" + storedStdOut[i] + ".\n(" + command
              + ")\n");
        }
      }
    }
  }

  /**
   * Appends a new line and writeString to the file called fileName.  Fails if the file
   * does not exist or the  .
   * @param fileName
   * @param writeString
   * @throws LogFile.LockException
   * @throws IOException
   */
  private void appendStringToFile(final String fileName, final String writeString)
      throws LogFile.LockException, IOException {
    // Create dataset file from fileName
    if (fileName == null) {
      // One or more of the files are missing.
      fail("Missing fileName.  (" + command + ")\n");
    }
    File file = new File(System.getProperty("user.dir"), fileName);
    assertTrue(file.getAbsolutePath() + " does not exist.  (" + command + ")\n",
        file.exists());
    assertTrue(file.getAbsolutePath() + " is not a file.  (" + command + ")\n",
        file.isFile());
    assertFalse("Target string is empty.  (" + command + ")\n", writeString == null
        || writeString.matches(""));
    // write string
    LogFile logFile = LogFile.getInstance(file);
    LogFile.WriterId writerId = logFile.openWriter(true);
    assertFalse("Unable to write to " + fileName + "(" + command + ")\n",
        writerId.isEmpty());
    logFile.newLine(writerId);
    logFile.write(writeString, writerId);
    logFile.closeWriter(writerId);
  }

  /**
   * Looks for a string in a file.  Fails if the file does not exist or the 
   * string is not found.
   * @param fileName
   * @param targetString
   * @throws LogFile.LockException
   * @throws FileNotFoundException
   * @throws IOException
   */
  private void assertFileContainsString(final String fileName, final String targetString)
      throws LogFile.LockException, FileNotFoundException, IOException {
    // Create dataset file from fileName
    if (fileName == null) {
      // One or more of the files are missing.
      fail("Missing fileName.  (" + command + ")\n");
    }
    File file = new File(System.getProperty("user.dir"), fileName);
    assertTrue(file.getAbsolutePath() + " does not exist.  (" + command + ")\n",
        file.exists());
    assertTrue(file.getAbsolutePath() + " is not a file.  (" + command + ")\n",
        file.isFile());
    assertFalse("Target string is empty.  (" + command + ")\n", targetString == null
        || targetString.matches("\\s*"));
    // Compare lines.
    LogFile logFile = LogFile.getInstance(file);
    LogFile.ReaderId readerId = logFile.openReader();
    assertFalse("Unable to read " + fileName + "(" + command + ")\n", readerId.isEmpty());
    String line;
    // Break up the target string based on the wildcard character.
    String[] targetArray = targetString.split("\\*");
    // See if one of the lines matches the target array.
    while ((line = logFile.readLine(readerId)) != null) {
      int fromIndex = 0;
      boolean foundAMatch = false;
      // Look for each part of the targetArray in order along a single line. The
      // matched areas may or may not be contiguious, since the wildcard matches
      // 0 or more characters.
      for (int i = 0; i < targetArray.length; i++) {
        if (fromIndex >= line.length()) {
          // Haven't completed the match and there nothing left to match on this
          // line.
          break;
        }
        // Attempt to match with the current part of the target array.
        int index = line.indexOf(targetArray[i], fromIndex);
        if (index == -1) {
          // Part of the target array didn't match.
          break;
        }
        if (i == targetArray.length - 1) {
          // The last part of the target array matched.
          foundAMatch = true;
        }
        else {
          // Move start index past the current match (* matches 0 or more)
          fromIndex = index + targetArray[i].length();
        }
      }
      if (foundAMatch) {
        logFile.closeRead(readerId);
        return;
      }
    }
    logFile.closeRead(readerId);
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
  private void assertComparison(final JTextComponent textComponent, final Command command)
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
    // assert.ge.field
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
      fail("unknown modifier - " + textComponent.getText() + "," + command.getValue()
          + " (" + command + ")");
    }
  }

  private AbstractButton findButton(final UITestFieldType fieldType, final String name,
      final int index) {
    int formatted = 0;
    try {
      Thread.sleep(1);
    }
    catch (InterruptedException e) {
    }
    setupNamedComponentFinder(AbstractButton.class, fieldType.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (debug) {
      namedFinder.setDebug(true);
    }
    AbstractButton button = null;
    while (button == null && formatted < MAX_FORMAT) {
      button = (AbstractButton) namedFinder.find(currentPanel, index);
      if (button == null) {
        formatApplication();
        formatted++;
      }
    }
    return button;
  }

  private JCheckBox findCheckBox(final String name, final int index) {
    int formatted = 0;
    setupNamedComponentFinder(JCheckBox.class, UITestFieldType.CHECK_BOX.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    JCheckBox checkBox = null;
    while (checkBox == null && formatted < MAX_FORMAT) {
      checkBox = (JCheckBox) namedFinder.find(currentPanel, index);
      if (checkBox == null) {
        formatApplication();
        formatted++;
      }
    }
    return checkBox;
  }

  private JComboBox findComboBox(final String name, final int index) {
    int formatted = 0;
    setupNamedComponentFinder(JComboBox.class, UITestFieldType.COMBO_BOX.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    JComboBox comboBox = null;
    while (comboBox == null && formatted < MAX_FORMAT) {
      comboBox = (JComboBox) namedFinder.find(currentPanel, index);
      if (comboBox == null) {
        formatApplication();
        formatted++;
      }
    }
    return comboBox;
  }

  private JMenuItem findMenuItem(final String name, final int index) {
    int formatted = 0;
    setupNamedComponentFinder(JMenuItem.class, UITestFieldType.MENU_ITEM.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    JMenuItem menuItem = null;
    while (menuItem == null && formatted < MAX_FORMAT) {
      menuItem = (JMenuItem) namedFinder.find();
      if (menuItem == null) {
        formatApplication();
        formatted++;

      }
    }
    return menuItem;
  }

  private void findContainer(String name) {
    setupNamedComponentFinder(JPanel.class, UITestFieldType.PANEL.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    currentPanel = (Container) namedFinder.find();
  }

  private JRadioButton findRadioButton(final String name, final int index) {
    int formatted = 0;
    setupNamedComponentFinder(JRadioButton.class, UITestFieldType.RADIO_BUTTON.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    JRadioButton radioButton = null;
    while (radioButton == null && formatted < MAX_FORMAT) {
      radioButton = (JRadioButton) namedFinder.find(currentPanel, index);
      if (radioButton == null) {
        formatApplication();
        formatted++;
      }
    }
    return radioButton;
  }

  private JSpinner findSpinner(final String name, final int index) {
    int formatted = 0;
    setupNamedComponentFinder(JSpinner.class, UITestFieldType.SPINNER.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    JSpinner spinner = null;
    while (spinner == null && formatted < MAX_FORMAT) {
      spinner = (JSpinner) namedFinder.find(currentPanel, index);
      if (spinner == null) {
        formatApplication();
        formatted++;
      }
    }
    return spinner;
  }

  private JTabbedPane findTabbedPane(final String name) {
    int formatted = 0;
    setupNamedComponentFinder(JTabbedPane.class, UITestFieldType.TAB.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    JTabbedPane tabbedPane = null;
    while (tabbedPane == null && formatted < MAX_FORMAT) {
      tabbedPane = (JTabbedPane) namedFinder.find(currentPanel, 0);
      if (tabbedPane == null) {
        formatApplication();
        formatted++;
      }
    }
    return tabbedPane;
  }

  private JTextComponent findTextField(final String name, final int index) {
    int formatted = 0;
    setupNamedComponentFinder(JTextField.class, UITestFieldType.TEXT_FIELD.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    JTextComponent textField = null;
    while (textField == null && formatted < MAX_FORMAT) {
      textField = (JTextComponent) namedFinder.find(currentPanel, index);
      if (textField == null) {
        formatApplication();
        formatted++;
      }
    }
    return textField;
  }

  /**
   * Executes the field part of a command.  Does not handle assert or if.
   * @param command
   * @param throwExceptionIfNotFound
   */
  private void executeField(final Command command) throws FileNotFoundException,
      IOException, LogFile.LockException {
    UITestModifierType modifierType = command.getModifierType();
    Field field = command.getField();
    UITestFieldType fieldType = field.getFieldType();
    String name = field.getName();
    int index = field.getIndex();
    String value = command.getValue();
    assertNotNull("missing field (" + command + ")", field);
    // PANEL
    // pnl.panel_title
    if (fieldType == UITestFieldType.PANEL) {
      assertNull("value not valid in a panel command (" + command + ")", value);
      findContainer(name);
      if (currentPanel == null) {
        if (frameWait > MAX_FRAME_WAIT) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
        }
        else {
          tryAgain = true;
          frameWait++;
          System.err.println("Wait " + frameWait + " for " + name);
        }
      }
      else {
        tryAgain = false;
        frameWait = 0;
      }
      try {
        Thread.sleep(1000);
      }
      catch (InterruptedException e) {
      }
    }
    else {
      tryAgain = false;
      frameWait = 0;
      // BUTTON
      if (fieldType == UITestFieldType.BUTTON) {
        AbstractButton button = findButton(UITestFieldType.BUTTON, name, index);
        if (button == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        // bn.button_name =
        assertNull("value not valid in a button command (" + command + ")", value);
        MouseEventData mouseEventData = new MouseEventData(testRunner, button, 1);
        if (!mouseEventData.prepareComponent()) {
          formatApplication();
        }
        helper.enterClickAndLeave(mouseEventData);
        try {
          if (Utilities.isWindowsOS()) {
            Thread.sleep(1000);
          }
          else {
            Thread.sleep(REDRAW_WAIT);
          }
        }
        catch (InterruptedException e) {
        }
      }
      // CHECK BOX
      else if (fieldType == UITestFieldType.CHECK_BOX) {
        JCheckBox checkBox = findCheckBox(name, index);
        if (checkBox == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        // cb.check_box_name
        // if value is present,only click on check box to get it to match value
        if (value == null || checkBox.isSelected() != convertToBoolean(value)) {
          helper.enterClickAndLeave(new MouseEventData(testRunner, checkBox, 1));
          try {
            Thread.sleep(REDRAW_WAIT);
          }
          catch (InterruptedException e) {
          }
        }
      }
      // COMBO BOX
      else if (fieldType == UITestFieldType.COMBO_BOX) {
        JComboBox comboBox = findComboBox(name, index);
        if (comboBox == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        if (command.getActionType() == UITestActionType.SET
            && command.getSubjectType() == UITestSubjectType.INDEX) {
          // set.index.cbb.combo_box_label
          EtomoNumber nValue = new EtomoNumber();
          nValue.set(value);
          assertTrue("value isn't a valid index - " + command.getField().getName() + " ("
              + command + ")", nValue.isValid());
          comboBox.setSelectedIndex(nValue.getInt());
        }
        else {
          // cbb.combo_box_label
          comboBox.addItem(value);
          comboBox.setSelectedItem(value);
        }
      }
      // MENU_ITEM
      else if (fieldType == UITestFieldType.MENU_ITEM) {
        JMenuItem menuItem = findMenuItem(name, index);
        if (menuItem == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        // mn.menu_item_label
        // if value is present, only click on menu item when it matches value
        helper.enterClickAndLeave(new MouseEventData(testRunner, menuItem));
        // wait for menu to open
        try {
          Thread.sleep(1000);
        }
        catch (InterruptedException e) {
        }
      }
      // MINI BUTTON
      else if (fieldType == UITestFieldType.MINI_BUTTON) {
        AbstractButton miniButton = findButton(UITestFieldType.MINI_BUTTON, name, index);
        if (miniButton == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        // mb.title_with_mini_button
        // if value is present,only click on mini-button when it matches value
        // mb.title_with_mini_button =
        // mb.title_with_mini_button = current_label
        if (value == null
            || miniButton.getText().equals("<html><b><center>" + value + "</center></b>")) {
          helper.enterClickAndLeave(new MouseEventData(testRunner, miniButton));
          try {
            Thread.sleep(REDRAW_WAIT);
          }
          catch (InterruptedException e) {
          }
        }
      }
      // RADIO BUTTON
      else if (fieldType == UITestFieldType.RADIO_BUTTON) {
        JRadioButton radioButton = findRadioButton(name, index);
        if (radioButton == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        // rb.radio_button_label
        assertNull("value not valid in a radio command (" + command + ")", value);
        helper.enterClickAndLeave(new MouseEventData(testRunner, radioButton));
        try {
          Thread.sleep(REDRAW_WAIT);
        }
        catch (InterruptedException e) {
        }
      }
      // SPINNER
      else if (fieldType == UITestFieldType.SPINNER) {
        JSpinner spinner = findSpinner(name, index);
        if (spinner == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        EtomoNumber nValue = new EtomoNumber();
        nValue.set(value);
        // sp.spinner_label = integer_value|up|down
        if (nValue.isValid()) {
          spinner.setValue(nValue.getNumber());
        }
        else {
          boolean spinnerControl = convertToSpinnerControl(value);
          helper.enterClickAndLeave(new JSpinnerMouseEventData(testRunner, spinner,
              spinnerControl ? JSpinnerMouseEventData.UP_ARROW_SUBCOMPONENT
                  : JSpinnerMouseEventData.DOWN_ARROW_SUBCOMPONENT, 1));
        }
      }
      // TAB
      // tb.tabbed_panel_title.index_of_tab
      else if (fieldType == UITestFieldType.TAB) {
        // find the tabbed panel and click on the tab
        assertNull("value not valid in a tab command (" + command + ")", value);
        JTabbedPane tabbedPane = findTabbedPane(name);
        if (tabbedPane == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        helper.enterClickAndLeave(new JTabbedPaneMouseEventData(testRunner, tabbedPane,
            index, 1));
        try {
          Thread.sleep(REDRAW_WAIT);
        }
        catch (InterruptedException e) {
        }
      }
      // TEXT FIELD
      else if (fieldType == UITestFieldType.TEXT_FIELD) {
        JTextComponent textField = findTextField(name, index);
        if (textField == null) {
          fail("can't find field - " + command.getField().getName() + " (" + command
              + ")");
          return;
        }
        // tf.text_field_label
        textField.setText(value);
      }
      else {
        fail("unexpected command (" + command + ")");
      }
      return;
    }
  }

  /**
   * Call UIHarness.pack.
   */
  private void formatApplication() {
    System.err.println("Formatting application");
    UIHarness.INSTANCE.pack(axisID, EtomoDirector.INSTANCE.getCurrentManagerForTest());
    try {
      Thread.sleep(FORMAT_WAIT);
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
    String value = command.getValue();
    assertNotNull("missing field (" + command + ")", field);
    assertEquals("function can only handle assert field commands (" + command + ")",
        UITestActionType.ASSERT, command.getActionType());
    assertNull("function can only handle assert field commands (" + command + ")",
        command.getSubject());
    // assert.field
    // BUTTON
    if (fieldType == UITestFieldType.BUTTON) {
      AbstractButton button = findButton(UITestFieldType.BUTTON, name, index);
      if (button == null) {
        fail("can't find field - " + command.getField().getName() + " (" + command + ")");
        return;
      }
      if (modifierType != null) {
        // assert.enabled.field
        // assert.disabled.field
        assertEnabled(button, command);
      }
      // assert.bn.button_name = button_state
      else {
        assertNotNull("value is required in an assert.bn command (" + command + ")",
            value);
        assertEquals("button state is not equal to value - " + value + " (" + command
            + ")", convertToBoolean(value), button.isSelected());
      }
    }
    // CHECK BOX
    else if (fieldType == UITestFieldType.CHECK_BOX) {
      JCheckBox checkBox = findCheckBox(name, index);
      if (checkBox == null) {
        fail("can't find field - " + command.getField().getName() + " (" + command + ")");
        return;
      }
      if (modifierType != null) {
        // assert.enabled.field
        // assert.disabled.field
        assertEnabled(checkBox, command);
      }
      // assert.cb.check_box_name = check_box_state
      else {
        assertNotNull("value is required in an assert.cb command (" + command + ")",
            value);
        assertEquals("check box state is not equal to value - " + checkBox.getText()
            + "," + value + " (" + command + ")", convertToBoolean(value),
            checkBox.isSelected());
      }
    }
    // COMBO BOX
    else if (fieldType == UITestFieldType.COMBO_BOX) {
      JComboBox comboBox = findComboBox(name, index);
      if (comboBox == null) {
        fail("can't find field - " + command.getField().getName() + " (" + command + ")");
        return;
      }
      if (modifierType != null) {
        if (modifierType == UITestModifierType.ENABLED
            || modifierType == UITestModifierType.DISABLED) {
          // assert.enabled.field
          // assert.disabled.field
          assertEnabled(comboBox, command);
        }
        else {
          fail("unknown action/modifier - " + comboBox.getSelectedItem() + "," + value
              + " (" + command + ")");
        }
      }
      // assert.cbb.combo_box_label
      else {
        // assert.cbb.combo_box_label = value
        if (value != null) {
          assertTrue(
              "combo box selected text is not equal to value - "
                  + comboBox.getSelectedItem() + "," + value + " (" + command + ")",
              comboBox.getSelectedItem().toString().equals(value));
        }
        // assert.cbb.combo_box_label =
        else {
          assertEquals(
              "combo box selected text is not empty - " + comboBox.getSelectedItem()
                  + "," + value + " (" + command + ")", null, comboBox.getSelectedItem());
        }
      }
    }
    // MENU_ITEM
    else if (fieldType == UITestFieldType.MENU_ITEM) {
      JMenuItem menuItem = findMenuItem(name, index);
      if (menuItem == null) {
        fail("can't find field - " + command.getField().getName() + " (" + command + ")");
        return;
      }
      assertNotNull("modifier is required", modifierType);
      // assert.enabled.field
      // assert.disabled.field
      assertEnabled(menuItem, command);
    }
    // MINI BUTTON
    else if (fieldType == UITestFieldType.MINI_BUTTON) {
      AbstractButton miniButton = findButton(UITestFieldType.MINI_BUTTON, name, index);
      if (miniButton == null) {
        fail("can't find field - " + command.getField().getName() + " (" + command + ")");
        return;
      }
      if (modifierType != null) {
        // assert.enabled.field
        // assert.disabled.field
        assertEnabled(miniButton, command);
      }
      // assert.mb.title_with_mini_button = current_label
      else {
        assertNotNull("value is required in an assert.mb command (" + command + ")",
            value);
        assertTrue("mini-button label is not equal to value - " + miniButton.getText()
            + "," + value + " (" + command + ")", miniButton.getText().equals(value));
      }
    }
    // RADIO BUTTON
    else if (fieldType == UITestFieldType.RADIO_BUTTON) {
      JRadioButton radioButton = findRadioButton(name, index);
      if (radioButton == null) {
        fail("can't find field - " + command.getField().getName() + " (" + command + ")");
        return;
      }
      if (modifierType != null) {
        // assert.enabled.field
        // assert.disabled.field
        assertEnabled(radioButton, command);
      }
      // assert.rb.radio_button_label = radio_button_state
      else {
        assertNotNull("value is required in an assert.rb command (" + command + ")",
            value);
        assertEquals(
            "radio button state is not equal to value - " + radioButton.getText() + ","
                + value + " (" + command + ")", convertToBoolean(value),
            radioButton.isSelected());
      }
    }
    // SPINNER
    else if (fieldType == UITestFieldType.SPINNER) {
      JSpinner spinner = findSpinner(name, index);
      if (spinner == null) {
        fail("can't find field - " + command.getField().getName() + " (" + command + ")");
        return;
      }
      if (modifierType != null) {
        // assert.enabled.field
        // assert.disabled.field
        assertEnabled(spinner, command);
      }
      // assert.sp.spinner_label = integer_value
      else {
        EtomoNumber nValue = new EtomoNumber();
        nValue.set(value);
        if (value != null) {
          assertTrue("field text is not equal to value - " + spinner.getValue() + ","
              + value + " (" + command + ")",
              ((Number) spinner.getValue()).intValue() == nValue.getInt());
        }
        else {
          nValue.set((Number) spinner.getValue());
          assertTrue("field text is not empty - " + spinner.getValue() + "," + value
              + " (" + command + ")", nValue.isNull());
        }
      }
    }
    // TEXT FIELD
    else if (fieldType == UITestFieldType.TEXT_FIELD) {
      JTextComponent textField = findTextField(name, index);
      if (textField == null) {
        fail("can't find field - " + command.getField().getName() + " (" + command + ")");
        return;
      }
      if (modifierType != null) {
        if (modifierType == UITestModifierType.ENABLED
            || modifierType == UITestModifierType.DISABLED) {
          // assert.enabled.field
          // assert.disabled.field
          assertEnabled(textField, command);
        }
        // assert.ge
        // assert.le
        else if (modifierType == UITestModifierType.GE
            || modifierType == UITestModifierType.LE) {
          assertComparison(textField, command);
        }
        else {
          fail("unknown action/modifier - " + textField.getText() + "," + value + " ("
              + command + ")");
        }
      }
      // assert.tf.text_field_label
      else {
        // assert.tf.text_field_label = value
        if (value != null) {
          assertTrue(
              "field text is not equal to value: " + value + " - " + textField.getName()
                  + ":" + textField.getText() + "," + " (" + command + ")", textField
                  .getText().equals(value));
        }
        // assert.tf.text_field_label =
        else {
          assertTrue("field text is not empty - " + textField.getText() + "," + value
              + " (" + command + ")", textField.getText().equals(""));
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
  private void ifField(final Command command) throws FileNotFoundException, IOException,
      LogFile.LockException {
    UITestModifierType modifierType = command.getModifierType();
    Field field = command.getField();
    UITestFieldType fieldType = field.getFieldType();
    String name = field.getName();
    int index = field.getIndex();
    String value = command.getValue();
    assertNotNull("missing field (" + command + ")", field);
    assertEquals("function can only handle assert field commands (" + command + ")",
        UITestActionType.IF, command.getActionType());
    assertNull("function can only handle if field commands (" + command + ")",
        command.getSubject());
    // if.field
    // BUTTON
    if (fieldType == UITestFieldType.BUTTON) {
      AbstractButton button = findButton(UITestFieldType.BUTTON, name, index);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(button, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        // if.enabled
        // if.disabled
        ifEnabled(button, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // CHECK BOX
    else if (fieldType == UITestFieldType.CHECK_BOX) {
      JCheckBox checkBox = findCheckBox(name, index);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(checkBox, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        // if.enabled
        // if.disabled
        ifEnabled(checkBox, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // COMBO BOX
    else if (fieldType == UITestFieldType.COMBO_BOX) {
      JComboBox comboBox = findComboBox(name, index);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(comboBox, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        // if.enabled
        // if.disabled
        ifEnabled(comboBox, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // MENU_ITEM
    else if (fieldType == UITestFieldType.MENU_ITEM) {
      JMenuItem menuItem = findMenuItem(name, index);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(menuItem, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        // if.enabled
        // if.disabled
        ifEnabled(menuItem, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // MINI BUTTON
    else if (fieldType == UITestFieldType.MINI_BUTTON) {
      AbstractButton miniButton = findButton(UITestFieldType.MINI_BUTTON, name, index);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(miniButton, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        // if.enabled
        // if.disabled
        ifEnabled(miniButton, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // PANEL
    // pnl.panel_title
    else if (fieldType == UITestFieldType.PANEL) {
      assertNull("value not valid in a panel command (" + command + ")", value);
      findContainer(name);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(currentPanel, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // RADIO BUTTON
    else if (fieldType == UITestFieldType.RADIO_BUTTON) {
      JRadioButton radioButton = findRadioButton(name, index);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(radioButton, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        // if.enabled
        // if.disabled
        ifEnabled(radioButton, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // SPINNER
    else if (fieldType == UITestFieldType.SPINNER) {
      JSpinner spinner = findSpinner(name, index);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(spinner, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        // if.enabled
        // if.disabled
        ifEnabled(spinner, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // TAB
    // tb.tabbed_panel_title.index_of_tab
    else if (fieldType == UITestFieldType.TAB) {
      // find the tabbed panel and click on the tab
      assertNull("value not valid in a tab command (" + command + ")", value);
      JTabbedPane tabbedPane = findTabbedPane(name);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(tabbedPane, command);
      }
      else {
        fail("invalid field command" + " (" + command + ")");
      }
    }
    // TEXT FIELD
    else if (fieldType == UITestFieldType.TEXT_FIELD) {
      JTextComponent textField = findTextField(name, index);
      // if.exists
      // if.not-exists
      if (modifierType == UITestModifierType.EXISTS
          || modifierType == UITestModifierType.NOT_EXISTS) {
        ifExists(textField, command);
      }
      else if (modifierType == UITestModifierType.ENABLED
          || modifierType == UITestModifierType.DISABLED) {
        // if.enabled
        // if.disabled
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
  private void setupNamedComponentFinder(final Class componentoClass, final String name) {
    if (namedFinder == null) {
      namedFinder = new NamedComponentFinder(componentoClass, name);
      namedFinder.setWait(2);
      namedFinder.setOperation(NamedComponentFinder.OP_EQUALS);
    }
    else {
      namedFinder.setComponentClass(componentoClass);
      namedFinder.setName(name);
    }
  }

  /**
   * Creates or reuses the named component finder.  Updates the component class
   * and name.  Sets the wait to 2.  Sets the operation to OP_EQUALS.
   * @param componentoClass
   * @param fieldName
   */
  private void setupComponentFinder(final Class componentoClass) {
    if (finder == null) {
      finder = new ComponentFinder(componentoClass);
      finder.setWait(2);
      finder.setOperation(NamedComponentFinder.OP_EQUALS);
    }
    else {
      finder.setComponentClass(componentoClass);
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
