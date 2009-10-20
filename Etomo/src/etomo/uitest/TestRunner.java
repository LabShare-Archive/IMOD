package etomo.uitest;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.extensions.jfcunit.JFCTestCase;
import junit.extensions.jfcunit.JFCTestHelper;

import etomo.Arguments;
import etomo.EtomoDirector;
import etomo.ManagerKey;
import etomo.process.SystemProgram;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.UITestActionType;
import etomo.type.UITestSubjectType;
import etomo.util.EnvironmentVariable;
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
 * <p> Revision 1.7  2009/09/22 21:06:09  sueh
 * <p> bug# 1259 Handling copy.file and set.interface in Test Sections.
 * <p>
 * <p> Revision 1.6  2009/09/02 22:46:47  sueh
 * <p> bug# 1254 Added commented out --names argument to the parameter list
 * <p> for convenience.
 * <p>
 * <p> Revision 1.5  2009/09/01 03:18:33  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.4  2009/03/17 00:46:33  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.3  2009/03/03 20:46:40  sueh
 * <p> bug# 1102 Added comments.
 * <p>
 * <p> Revision 1.2  2009/02/04 23:37:32  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.1  2009/01/20 20:49:35  sueh
 * <p> bug# 1102 Class to run a test defined by a Test section in uitest.adoc.
 * <p> </p>
 */
public final class TestRunner extends JFCTestCase implements VariableList {
  public static final String rcsid = "$Id$";

  private static final String SOURCE_ENV_VAR = "IMOD_UITEST_SOURCE";

  private final Map variableMap = new HashMap();
  private final Map variableMapA = new HashMap();
  private final Map variableMapB = new HashMap();
  private final List parameterList = new ArrayList();
  private final Map autodocTesterMap = new HashMap();
  private File uitestDataDir;

  private JFCTestHelper helper = null;
  private EtomoDirector etomo = null;
  private File testDir = null;
  private boolean keepDatasetDir = false;
  private InterfaceSection interfaceSection = null;

  public TestRunner() {
  }

  protected void setUp() throws Exception {
    super.setUp();
    helper = new JFCTestHelper();
  }

  protected void tearDown() throws Exception {
    if (etomo != null) {
      etomo.setTestDone(true);
      etomo.exitProgram(AxisID.ONLY);
      etomo = null;
    }
    JFCTestHelper.cleanUp(this);
    super.tearDown();
  }

  /**
   * Run one test described by a Test section in uitest.adoc
   * @throws FileNotFoundException
   * @throws IOException
   * @throws LogFile.ReadException
   */
  public void test() throws FileNotFoundException, IOException,
      LogFile.LockException {
    //Add the default arguments to the parameter list.
    parameterList.add(Arguments.SELFTEST_TAG);
    parameterList.add(Arguments.TEST_TAG);
    //parameterList.add(Arguments.NAMES_TAG);
    //parameterList.add(Arguments.DEBUG_TAG);
    //get uitest.adoc
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(AutodocFactory.UITEST,
        AxisID.ONLY, null);
    if (autodoc == null) {
      fail("Missing autodoc: " + AutodocFactory.UITEST);
      return;
    }
    //Get the Test section specified by the environment variable
    //IMOD_TEST_SECTION.
    String testName = EnvironmentVariable.INSTANCE.getValue(null,
        "IMOD_TEST_SECTION", AxisID.ONLY, null);
    System.err.println("Testing " + testName);
    if (testName == null || testName.matches("\\*+")) {
      fail("$IMOD_TEST_SECTION has not been set");
    }
    //Read the commands in the Test section in order.
    CommandReader testSectionReader = CommandReader.getSectionReader(autodoc,
        SectionType.TEST.toString(), testName, AxisID.ONLY, this);
    List autodocTesterList = new ArrayList();
    //The best order for these attributes to be set is TEST_DIR, then ADOC, then
    //DATASET.
    Command command = null;
    while (!testSectionReader.isDone()) {
      command = testSectionReader.nextCommand(command);
      if (command != null && command.isKnown()) {
        assertNull("field not needed in Test section commands (" + command
            + ")", command.getField());
        UITestActionType actionType = command.getActionType();
        UITestSubjectType subjectType = command.getSubjectType();
        Subject subject = command.getSubject();
        String value = command.getValue();
        //SET
        if (actionType == UITestActionType.SET) {
          if (subjectType == UITestSubjectType.ADOC) {
            //Create an autodoc tester for each autodoc in the Test section.  The
            //they appear is the order in which they start running.
            File dir = Utilities.getExistingDir(SOURCE_ENV_VAR, AxisID.ONLY,
                null);
            System.err.println(SOURCE_ENV_VAR + ": " + dir.getAbsolutePath());
            AxisID axisID = subject.getAxisID();
            //Get the sectionType
            String sectionType = subject.getName();
            assertNotNull(
                "set.adoc command must specify section types to run in autodoc ("
                    + command + ")", sectionType);
            //Create an autodoc tester for this autodoc.
            AutodocTester autodocTester = AutodocTester.getAutodocTester(this,
                helper, AutodocFactory.getInstance(dir, value, AxisID.ONLY,
                    null), dir, sectionType, axisID, this);
            autodocTesterList.add(autodocTester);
            //Assuming that AxisID can be a unique key for the testers
            autodocTesterMap.put(axisID, autodocTester);
          }
          else if (subjectType == UITestSubjectType.PARAM) {
            assertNotNull("missing paramter - " + value + " (" + command + ")",
                value);
            parameterList.add(value);
          }
          //set.dataset
          else if (subjectType == UITestSubjectType.DATASET) {
            executeDatasetSection(autodoc, value);
          }
          //set.testdir
          else if (subjectType == UITestSubjectType.TESTDIR) {
            setTestDir(command, testSectionReader);
          }
          else if (subjectType == UITestSubjectType.VAR) {
            variableMap.put(subject.getName(), value);
          }
          else if (subjectType == UITestSubjectType.INTERFACE) {
            String subjectName = command.getSubject().getName();
            assertNotNull("set.interface command requires a subject name ("
                + command + ")", subjectName);
            interfaceSection = new InterfaceSection(subjectName);
          }
          else {
            fail("unexpected command (" + command.toString() + ")");
          }
        }
        //COPY
        else if (actionType == UITestActionType.COPY
            && subjectType == UITestSubjectType.FILE) {
          //Only copy axes that are specified in the Test section in the
          //set.adoc commands.
          if (autodocTesterMap.containsKey(subject.getAxisID())) {
            copyFile(value, false);
          }
        }
        else {
          fail("unexpected command (" + command.toString() + ")");
        }
      }
    }
    //If there was no testdir entry in the Test section, then set the test
    //directory from the name of the Test section.
    assertNotNull("testdir is required in Test sections", testDir);
    //Run Etomo
    EtomoDirector.main((String[]) parameterList
        .toArray(new String[parameterList.size()]));
    etomo = EtomoDirector.INSTANCE;
    //Run each autodoc, taking turns
    boolean testing = true;
    //Loop until each autodoc is done.
    while (testing) {
      testing = false;
      int index = 0;
      //Test each autodoc in turn, until each experiences a wait, or is done.
      while (index < autodocTesterList.size()) {
        AutodocTester tester = (AutodocTester) autodocTesterList.get(index++);
        if (!tester.isDone()) {
          testing = true;
          tester.testUntilWait();
        }
      }
    }
  }

  boolean isDone(AxisID axisID) {
    AutodocTester tester = (AutodocTester) autodocTesterMap.get(axisID);
    assertNotNull("no tester has this axisID - " + axisID, tester);
    return tester.isDone();
  }

  boolean isDialogSectionComplete(AxisID axisID, String dialogSection) {
    AutodocTester tester = (AutodocTester) autodocTesterMap.get(axisID);
    assertNotNull("no tester has this axisID - " + axisID, tester);
    return tester.isDialogSectionComplete(dialogSection);
  }

  InterfaceSection getInterfaceSection() {
    return interfaceSection;
  }

  private void executeDatasetSection(ReadOnlyAutodoc autodoc, String sectionName) {
    assertNotNull("sectionName is required", sectionName);
    variableMap.put(UITestSubjectType.DATASET.toString(), sectionName);
    CommandReader datasetSectionReader = CommandReader
        .getSectionReader(autodoc, SectionType.DATASET.toString(), sectionName,
            AxisID.ONLY, this);
    //The directory where dataset files are stored is specified by the section
    //name.
    uitestDataDir = new File(Utilities.getExistingDir("IMOD_UITEST_DATA",
        AxisID.ONLY, null), sectionName);
    System.err.println("uitestDataDir="+uitestDataDir);
    Command command = null;
    while (!datasetSectionReader.isDone()) {
      command = datasetSectionReader.nextCommand(command);
      if (command != null && command.isKnown()) {
        assertNull("field not needed in dataset section commands (" + command
            + ")", command.getField());
        UITestActionType actionType = command.getActionType();
        UITestSubjectType subjectType = command.getSubjectType();
        Subject subject = command.getSubject();
        UITestModifierType modifierType = command.getModifierType();
        String value = command.getValue();
        //COPY
        if (actionType == UITestActionType.COPY
            && subjectType == UITestSubjectType.FILE) {
          //Only copy axes that are specified in the Test section in the
          //set.adoc commands.
          if (autodocTesterMap.containsKey(subject.getAxisID())) {
            copyFile(value, false);
          }
        }
        //SET
        else if (actionType == UITestActionType.SET) {
          if (subjectType == UITestSubjectType.DATASET) {
            //Override the implicit setting of the dataset variable for a single
            //axis dataset.
            if (modifierType == UITestModifierType.SINGLE) {
              if (autodocTesterMap.containsKey(AxisID.ONLY)) {
                variableMap.remove(UITestSubjectType.DATASET.toString());
                variableMap.put(UITestSubjectType.DATASET.toString(), value);
              }
            }
            else {
              fail("unexpected command (" + command + ")");
            }
          }
          else if (subjectType == UITestSubjectType.INTERFACE) {
            String subjectName = command.getSubject().getName();
            assertNotNull("set.interface command requires a subject name ("
                + command + ")", subjectName);
            interfaceSection = new InterfaceSection(subjectName);
          }
          else if (subjectType == UITestSubjectType.VAR) {
            AxisID axisID = subject.getAxisID();
            String subjectName = subject.getName();
            if (axisID == AxisID.FIRST) {
              variableMapA.put(subjectName, value);
            }
            else if (axisID == AxisID.SECOND) {
              variableMapB.put(subjectName, value);
            }
            else {
              variableMap.put(subjectName, value);
            }
          }
          else {
            fail("unexpected command (" + command.toString() + ")");
          }
        }
        else {
          fail("unexpected command (" + command.toString() + ")");
        }
      }
    }
    System.err.println("dataset="
        + variableMap.get(UITestSubjectType.DATASET.toString()));
  }

  public void setVariable(String variableName, Object variableValue) {
    variableMap.put(variableName, variableValue);
  }

  /**
   * Get a variables value.  First try to get it from the axis level variable
   * map.  If the variable is not in that map, get it from the global map.
   * @param variableName
   * @param axisID
   * @return
   */
  public String getVariableValue(String variableName, AxisID axisID) {
    if ((axisID == AxisID.FIRST || axisID == AxisID.ONLY)
        && variableMapA.containsKey(variableName)) {
      return (String) variableMapA.get(variableName);
    }
    if (axisID == AxisID.SECOND && variableMapB.containsKey(variableName)) {
      return (String) variableMapB.get(variableName);
    }
    return (String) variableMap.get(variableName);
  }

  /**
   * First check the axis level variables.  If nothing is found, check the
   * global variables.
   * @param variableName
   * @param axisID
   * @return
   */
  public boolean isVariableSet(String variableName, AxisID axisID) {
    if (axisID == AxisID.FIRST || axisID == AxisID.ONLY) {
      if (variableMapA.containsKey(variableName)) {
        return true;
      }
    }
    else if (axisID == AxisID.SECOND) {
      if (variableMapB.containsKey(variableName)) {
        return true;
      }
    }
    return variableMap.containsKey(variableName);
  }

  /**
   * Get the test directory for the current section.  If modifier is not set to
   * keep, clean the test directory by deleting it and then recreating it.
   * Makes the test directory the working directory.  Also sets the keep boolean,
   * which keeps other files from being overridden in the dataset directory.
   * @return
   */
  private void setTestDir(Command command, CommandReader reader) {
    JfcUnitTests.TEST_ROOT_DIR.mkdirs();
    //Get the test directory name
    String testDirName = command.getValue();
    assertNotNull("Section name cannot be null", testDirName);
    //make the test directory path
    testDir = new File(JfcUnitTests.TEST_ROOT_DIR, testDirName);
    variableMap.put(UITestSubjectType.TESTDIR.toString(), testDir
        .getAbsolutePath());
    keepDatasetDir = command.getModifierType() == UITestModifierType.KEEP;
    if (keepDatasetDir) {
      variableMap.put("keep-dataset-dir", "");
    }
    if (!testDir.exists()) {
      testDir.mkdirs();
    }
    else if (!keepDatasetDir) {
      //clean the test directory by deleting it
      SystemProgram remove = new SystemProgram(System.getProperty("user.dir"),
          new String[] { "rm", "-fr", testDir.getAbsolutePath() }, AxisID.ONLY,
          null);
      remove.run();
      //make the test directory
      testDir.mkdir();
    }
    //make the test directory the working directory
    System.setProperty("user.dir", testDir.getAbsolutePath());
  }

  /**
   * copy a file from dataDir to the working directory.  If keepDatasetDir is
   * true, only copy if file is not in the working directory.  Dataset must be
   * set before a copy can be done.
   * @param attrib
   * @param sourceDir
   */
  void copyFile(String fileName, boolean always) {
    assertNotNull(variableMap.get(UITestSubjectType.DATASET.toString()));
    File file = new File(uitestDataDir, fileName);
    if (!file.exists() || file.isDirectory()) {
      throw new IllegalStateException(
          "file must exist and must not be a directory: "
              + file.getAbsolutePath());
    }
    //If keepDatasetDir is true, then only copy if the file does not exist in
    //the dataset directory (the directory being tested in).
    if (keepDatasetDir && !always) {
      File targetFile = new File(System.getProperty("user.dir"), file.getName());
      if (targetFile.exists()) {
        return;
      }
    }
    SystemProgram copy = new SystemProgram(System.getProperty("user.dir"),
        new String[] { "cp", file.getAbsolutePath(), "." }, AxisID.ONLY, null);
    copy.run();
  }

  File getUitestDataDir() {
    return uitestDataDir;
  }

  ManagerKey getManagerKey() {
    return etomo.getCurrentManagerForTest().getManagerKey();
  }
}
