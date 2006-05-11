package etomo.ui;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

import etomo.EtomoDirector;
import etomo.JfcUnitTests;
import etomo.process.SystemProgram;
import etomo.storage.autodoc.AdocCommand;
import etomo.storage.autodoc.AdocCommandFactory;
import etomo.storage.autodoc.AdocCommandReader;
import etomo.storage.autodoc.Autodoc;
import etomo.storage.autodoc.NameValuePair;
import etomo.storage.autodoc.Section;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.UITestAction;
import etomo.util.Utilities;
import junit.extensions.jfcunit.JFCTestCase;
import junit.extensions.jfcunit.JFCTestHelper;

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
 */
public final class UITest extends JFCTestCase implements AdocCommandFactory {
  public static final String rcsid = "$Id$";

  static final File TEST_REPOSITORY = Utilities.getExistingDir(
      "IMOD_UITEST_SOURCE", AxisID.ONLY);
  static final long DEFAULT_SLEEP = 1000;
  static final String DATASET_ATTRIB = "dataset";

  private static final String[] ARGS = new String[] { "--selftest", "--test" };

  private AdocCommandReader reader = null;
  private File testDir = null;
  private Autodoc autodocA = null;
  private Autodoc autodocB = null;
  private JFCTestHelper helper = null;
  private EtomoDirector etomo = null;
  private long sleep = DEFAULT_SLEEP;
  private String dataset = null;
  private boolean keep = false;
  private UITestAxis axisAUITest = null;
  private UITestAxis axisBUITest = null;
  private long duration = 15 * 60;
  private File currentSourceDir = null;
  private File autodocSourceDir = null;
  private AxisID axisIDA = null;
  private AxisID axisIDB = null;
  private String dataFileName = null;
  private ArrayList variablesA = null;
  private ArrayList variablesB = null;

  protected void setUp() throws Exception {
    super.setUp();
    helper = new JFCTestHelper();
    setHelper(helper);
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
   * run all the tests in uitest.adoc
   * @throws IOException
   */
  public void test() throws IOException {
    String testName = Utilities.getEnvironmentVariable(null,
        "IMOD_TEST_SECTION", AxisID.ONLY);
    System.err.println("test " + testName + ":");
    //get the uitest.adoc
    Autodoc.setTest(true);
    Autodoc autodoc = null;
    try {
      autodoc = Autodoc.getInstance(Autodoc.UITEST, AxisID.ONLY);
    }
    catch (FileNotFoundException e) {
      return;
    }
    catch (IllegalStateException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    if (autodoc == null) {
      return;
    }
    reader = new AdocCommandReader(autodoc, UITestSectionCommand.SECTION_TYPE);
    UITestCommandFactory factory = new UITestCommandFactory();
    UITestCommand command = (UITestCommand) reader.nextCommand(null, factory);
    //process commands in the global section
    while (!command.isEmpty()) {
      if (command.isKnown()) {
        UITestAction action = command.getAction();
        if (action == UITestAction.SLEEP) {
          setSleep(command);
        }
        else if (action == UITestAction.SOURCE) {
          autodocSourceDir = getRelativeDir(command, TEST_REPOSITORY, false);
        }
        else if (action == UITestAction.TEST_DIR) {
          testDir = getRelativeDir(command, JfcUnitTests.TEST_ROOT_DIR, true);
        }
      }
      command = (UITestCommand) reader.nextCommand(command, factory);
    }
    //set default directories
    if (autodocSourceDir == null) {
      autodocSourceDir = TEST_REPOSITORY;
    }
    if (testDir == null) {
      JfcUnitTests.TEST_ROOT_DIR.mkdirs();
      testDir = JfcUnitTests.TEST_ROOT_DIR;
    }
    //process section specified by testName
    reader.setSection(testName);
    if (reader.isDone()) {
      return;
    }
    processSection();
    runAxisLevelTests();
  }

  private void setSleep(UITestCommand command) {
    String value = command.getValue();
    assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, value);
    try {
      sleep = Long.parseLong(value);
    }
    catch (NumberFormatException e) {
      fail(reader.getInfo(), "value must be numeric: " + command);
    }
  }

  void moveSubFrame() {
    UIHarness.INSTANCE.moveSubFrame();
  }

  long getSleep() {
    return sleep;
  }

  String getDataset() {
    return dataset;
  }

  public AdocCommand newAdocCommand() {
    return new UITestSectionCommand();
  }

  private void setDuration(UITestSectionCommand command) {
    String[] durationArray = command.getValue().split(":");
    if (durationArray.length == 4) {
      duration = Long.parseLong(durationArray[3]);
    }
    if (!durationArray[2].equals("")) {
      duration += Long.parseLong(durationArray[2]) * 60;
    }
    if (!durationArray[1].equals("")) {
      duration += Long.parseLong(durationArray[1]) * 60 * 60;
    }
    if (!durationArray[0].equals("")) {
      duration += Long.parseLong(durationArray[1]) * 60 * 60 * 24;
    }
  }

  private String getRequiredValue(UITestSectionCommand command) {
    String value = command.getValue();
    assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, value);
    return value;
  }

  private void setVariable(UITestSectionCommand command) {
    AxisID axisID = command.getAxisID();
    if (axisID == null || axisID == AxisID.ONLY || axisID == AxisID.FIRST) {
      variablesA = setVariable(command, variablesA);
    }
    if (axisID == null || axisID == AxisID.SECOND) {
      variablesB = setVariable(command, variablesB);
    }
  }

  private ArrayList setVariable(UITestSectionCommand command,
      ArrayList variables) {
    if (variables == null) {
      variables = new ArrayList();
    }
    variables.add(command.getVariable());
    return variables;
  }

  private void setDataset(UITestSectionCommand command) {
    String value = command.getValue();
    assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, value);
    dataset = value;
    setVariable(command);
  }

  /**
   * Performs all commands and open all autodocs found in the section
   * @throws FileNotFoundException
   * @throws IOException
   */
  private void processSection() throws FileNotFoundException, IOException {
    UITestSectionCommand command = (UITestSectionCommand) reader.nextCommand(
        null, this);
    boolean datasetDirCommand = false;
    while (!command.isEmpty()) {
      if (command.isKnown()) {
        UITestAction action = command.getAction();
        if (action == UITestAction.ADOC) {
          setAutodoc(command);
        }
        else if (action == UITestAction.COPY) {
          if (!datasetDirCommand) {
            datasetDirCommand = true;
            setDatasetDir(null, false);
          }
          copyFile(command);
        }
        else if (action == UITestAction.DATA_FILE) {
          dataFileName = getRequiredValue(command);
        }
        else if (action == UITestAction.DATASET) {
          setDataset(command);
        }
        else if (action == UITestAction.DATASET_DIR) {
          if (!datasetDirCommand) {
            datasetDirCommand = true;
            setDatasetDir(command);
          }
        }
        else if (action == UITestAction.DURATION) {
          setDuration(command);
        }
        else if (action == UITestAction.SOURCE) {
          //Get a source directory relative to the test repository
          currentSourceDir = getRelativeDir(command, TEST_REPOSITORY, false);
        }
        else if (action == UITestAction.SET) {
          setVariable(command);
        }
      }
      command = (UITestSectionCommand) reader.nextCommand(command, this);
    }
    assertTrue(reader.getInfo(), UITestAction.ADOC + " is required.",
        autodocA != null || autodocB != null);
    if (!datasetDirCommand) {
      setDatasetDir(null, false);
    }
  }

  File getCurrentSourceDir() {
    return currentSourceDir;
  }

  long getDuration() {
    return duration;
  }

  File getAutodocSourceDir() {
    return autodocSourceDir;
  }

  /**
   * runs tests with uitest-axis autodocs for A and/or B
   * @param test
   * @param sourceDir
   */
  private void runAxisLevelTests() {
    if (autodocA == null && autodocB == null) {
      return;
    }
    //run Etomo
    if (dataFileName == null) {
      etomo = EtomoDirector.createInstance_test(ARGS);
    }
    else {
      File dataFile = new File(new File(testDir, reader.getName()),
          dataFileName);
      String[] args = new String[ARGS.length + 1];
      for (int i = 0; i < ARGS.length; i++) {
        args[i] = ARGS[i];
      }
      args[ARGS.length] = dataFile.getAbsolutePath();
      etomo = EtomoDirector.createInstance_test(args);
    }
    //Create the UITestAxis instances
    if (autodocA != null) {
      axisAUITest = new UITestAxis(this, autodocA, helper, axisIDA,
          dataFileName != null, variablesA);
    }
    if (autodocB != null) {
      axisBUITest = new UITestAxis(this, autodocB, helper, axisIDB,
          dataFileName != null, variablesB);
    }
    //Test the axis or axes, taking turns if there are two
    boolean testingA = axisAUITest != null;
    boolean testingB = axisBUITest != null;
    while ((testingA && !axisAUITest.isDone())
        || (testingB && !axisBUITest.isDone())) {
      if (testingA && !axisAUITest.isDone()) {
        axisAUITest.testAxis();
      }
      if (testingB && !axisBUITest.isDone()) {
        axisBUITest.testAxis();
      }
    }
  }

  /**
   * Returns true if the axis test is null.
   * Returns true if the axis test is done and not stopped.
   * Otherwise returns true if able to remove the dialog from the finished
   * dialog list.
   * @param axisID
   * @param dialogType
   * @return
   */
  boolean removeDialog(AxisID axisID, DialogType dialogType) {
    UITestAxis axisUITest;
    if (axisID == AxisID.SECOND) {
      axisUITest = axisBUITest;
    }
    else {
      axisUITest = axisAUITest;
    }
    if (axisUITest == null) {
      return true;
    }
    return axisUITest.removeFinishedDialog(dialogType);
  }

  /**
   * returns true if the axis test exists and is stopped
   * @param axisID
   * @return
   */
  boolean isStopped(AxisID axisID) {
    UITestAxis axisUITest;
    if (axisID == AxisID.SECOND) {
      axisUITest = axisBUITest;
    }
    else {
      axisUITest = axisAUITest;
    }
    if (axisUITest == null) {
      return false;
    }
    return axisUITest.isStopped();
  }

  /**
   * copy a file from sourceDir to the working directory
   * @param attrib
   * @param sourceDir
   */
  void copyFile(AdocCommand command) {
    String fileName = command.getValue();
    assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, fileName);
    File file = new File(currentSourceDir, fileName);
    if (!file.exists() || file.isDirectory()) {
      throw new IllegalStateException("bad dataset file: "
          + file.getAbsolutePath());
    }
    if (keep) {
      File targetFile = new File(System.getProperty("user.dir"), file.getName());
      if (targetFile.exists()) {
        return;
      }
    }
    SystemProgram copy = new SystemProgram(System.getProperty("user.dir"),
        new String[] { "cp", file.getAbsolutePath(), "." }, AxisID.ONLY);
    copy.run();
  }

  /**
   * Create a new autodoc and set it to autodocA or B, if the autodocA(B) is not
   * already set. 
   * @param adocAttrib
   * @param currentSourceDir
   * @throws FileNotFoundException
   * @throws IOException
   */
  private void setAutodoc(UITestSectionCommand command)
      throws FileNotFoundException, IOException {
    String value = command.getValue();
    assertNotNull(null, "Unknown name/value pair format: " + command, value);
    setVariable(command);
    //look for adoc = autodoc
    AxisID axisID = command.getAxisID();
    if (axisID == AxisID.SECOND) {
      if (autodocB != null) {
        return;
      }
      axisIDB = axisID;
      autodocB = Autodoc.getUITestAxisInstance_test(autodocSourceDir, value,
          AxisID.ONLY);
      return;
    }
    if (autodocA != null) {
      return;
    }
    axisIDA = axisID;
    autodocA = Autodoc.getUITestAxisInstance_test(autodocSourceDir, value,
        AxisID.ONLY);
  }

  /**
   * Get the datasetDir for the current section.  If the keep is false 
   * then clean the datasetDir by deleting
   * it and then recreating it.  Set it be the working directory.
   * @param datasetDir
   * @return
   */
  private void setDatasetDir(String dirName, boolean keep) {
    File datasetDir = null;
    //Get the directory name
    if (dirName == null) {
      dirName = reader.getName();
      assertNotNull(reader.getInfo(), "Invalid section", dirName);
    }
    //make the directory path
    datasetDir = new File(testDir, dirName);
    if (!datasetDir.exists()) {
      datasetDir.mkdirs();
    }
    //if keep, do not clean the directory
    if (!keep) {
      //clean the directory by deleting it
      SystemProgram remove = new SystemProgram(System.getProperty("user.dir"),
          new String[] { "rm", "-fr", datasetDir.getAbsolutePath() },
          AxisID.ONLY);
      remove.run();
      //make the directory
      datasetDir.mkdir();
    }
    //make the directory the working directory
    System.setProperty("user.dir", datasetDir.getAbsolutePath());
  }

  private void setDatasetDir(UITestSectionCommand command) {
    File datasetDir = null;
    //Get the directory name
    String dirName = command.getValue();
    boolean keep = command.isKeep();
    setDatasetDir(dirName, keep);
  }

  /**
   * Create a directory or directory path in rootDir using a command value.
   * If the value does not exist, return rootDir.  Make
   * directories if makeDirs is true and the directory does not exist.
   * This is an optional attribute:  return rootDir if the attribute is not found.
   * @param autodoc
   * @param attribName
   * @param rootDir
   * @param makeDirs - if true, make the directories
   * @return
   */
  private File getRelativeDir(AdocCommand command, File rootDir,
      boolean makeDirs) {
    //Make the root directories on the root directory since it could be returned
    if (makeDirs && !rootDir.exists()) {
      rootDir.mkdirs();
    }
    //Get the directory name
    String dirName = command.getValue();
    assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, dirName);
    //make the directories
    File dir = new File(rootDir, dirName);
    if (makeDirs && !dir.exists()) {
      dir.mkdirs();
    }
    return dir;
  }

  /**
   * Create a directory or directory path in rootDir using an attribute value.
   * If the attribute value does not exist, return originalDir.  Make
   * directories if makeDirs is true and the directory does not exist.
   * @param section
   * @param attribName
   * @param rootDir
   * @return the directory
   */
  private File getRelativeDir(NameValuePair pair, File rootDir, boolean makeDirs) {
    //Get the directory name
    String dirName = pair.getValue();
    assertNotNull(null, "Unknown name/value pair format: " + pair.getString(),
        dirName);
    //make the directories
    File dir = new File(rootDir, dirName);
    if (makeDirs && !dir.exists()) {
      dir.mkdirs();
    }
    return dir;
  }

  /**
   * Create a File in rootDir using the section name.  The section name is a
   * required field so this function should never return rootDir.
   * @param section
   * @param rootDir
   * @param makeDirs
   * @return
   */
  private File getRequiredRelativeDir(Section section, File rootDir,
      boolean makeDirs) {
    //Get the directory name
    String dirName = section.getName();
    //make the directories
    File dir = new File(rootDir, dirName);
    if (makeDirs && !dir.exists()) {
      dir.mkdirs();
    }
    return dir;
  }

  public void assertTrue(String sectionInfo, String message, boolean condition) {
    if (sectionInfo != null) {
      message = sectionInfo + ": " + message;
    }
    assertTrue(message, condition);
  }

  public void fail(String sectionInfo, String message) {
    if (sectionInfo != null) {
      message = sectionInfo + ": " + message;
    }
    fail(message);
  }

  public void assertNotNull(String sectionInfo, String message, Object object) {
    if (sectionInfo != null) {
      message = sectionInfo + ": " + message;
    }
    assertNotNull(message, object);
  }

  public void assertFalse(String sectionInfo, String message, boolean condition) {
    if (sectionInfo != null) {
      message = sectionInfo + ": " + message;
    }
    assertFalse(message, condition);
  }

  public void assertNull(String sectionInfo, String message, Object object) {
    if (sectionInfo != null) {
      message = sectionInfo + ": " + message;
    }
    assertNull(message, object);
  }

  public void assertEquals(String sectionInfo, String message, Object expected,
      Object actual) {
    if (sectionInfo != null) {
      message = sectionInfo + ": " + message;
    }
    assertEquals(message, expected, actual);
  }

  public void assertEquals(String sectionInfo, String message,
      boolean expected, boolean actual) {
    if (sectionInfo != null) {
      message = sectionInfo + ": " + message;
    }
    assertEquals(message, expected, actual);
  }

  private static final class UITestCommandFactory implements AdocCommandFactory {
    public AdocCommand newAdocCommand() {
      return new UITestCommand();
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.12  2006/05/01 21:19:43  sueh
 * <p> bug# 787 Removed fiducial diameter, added set.
 * <p>
 * <p> Revision 1.11  2006/04/28 21:06:48  sueh
 * <p> bug# 787 Simplify code by using AdocCommandReader instead of
 * <p> keyed searches and NameValuePair.
 * <p>
 * <p> Revision 1.10  2006/04/25 19:38:45  sueh
 * <p> bug# 787 Added duration and datafile.  Added removeDialog() to remove
 * <p> a done dialog, when checking for a completed dialog.
 * <p>
 * <p> Revision 1.9  2006/01/13 20:17:05  sueh
 * <p> bug# 675 Added datasetdir.keep to avoid copying the dataset files.
 * <p>
 * <p> Revision 1.8  2006/01/12 22:18:18  sueh
 * <p> bug# 675 removed unnecessary function enterClickAndLeave().
 * <p>
 * <p> Revision 1.7  2006/01/12 17:38:44  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.6  2006/01/11 23:18:40  sueh
 * <p> bug# 675 Running one test instead of all tests.  Enforcing uitest.adoc
 * <p> syntax rules.  Added fiducial diameter.
 * <p>
 * <p> Revision 1.5  2006/01/04 20:28:12  sueh
 * <p> bug# 675 Moved constants that must be shared by non-test objects to an
 * <p> object which doesn't know about junit.  Overwise junit would have to be in
 * <p> the path for compiling and running EtomoDirector.
 * <p>
 * <p> Revision 1.4  2006/01/04 00:08:44  sueh
 * <p> bug# 675 Make test() run one test described in uitest.adoc.
 * <p>
 * <p> Revision 1.3  2005/12/30 21:19:57  sueh
 * <p> bug# 675 class to run a jfcunit test
 * <p>
 * <p> Revision 1.1  2005/12/23 02:25:03  sueh
 * <p> bug# 675 A class to generically run Etomo through the UI.
 * <p> </p>
 */