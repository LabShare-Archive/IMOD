package etomo.ui;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

import etomo.EtomoDirector;
import etomo.JfcUnitTests;
import etomo.process.SystemProgram;
import etomo.storage.AdocCommand;
import etomo.storage.AdocCommandFactory;
import etomo.storage.AdocCommandReader;
import etomo.storage.LogFile;
import etomo.storage.UITestCommand;
import etomo.storage.UITestTestCommand;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlyStatement;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.DialogType;
import etomo.type.UITestAction;
import etomo.ui.UIHarness;
import etomo.util.EnvironmentVariable;
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

  static final File SOURCE_DIR = Utilities.getExistingDir("IMOD_UITEST_SOURCE",
      AxisID.ONLY);
  private static final File DATA_DIR = Utilities.getExistingDir(
      "IMOD_UITEST_DATA", AxisID.ONLY);
  private static final String[] ARGS = new String[] { "--selftest", "--test" };
  static final long DEFAULT_SLEEP = 1000;

  private static Throwable uncaughtException = null;

  private AdocCommandReader reader = null;
  private File datasetDir = null;
  private ReadOnlyAutodoc autodocA = null;
  private ReadOnlyAutodoc autodocB = null;
  private JFCTestHelper helper = null;
  private EtomoDirector etomo = null;
  private long sleep = DEFAULT_SLEEP;
  private String dataset = null;
  private boolean keep = false;
  private UITestAxis axisAUITest = null;
  private UITestAxis axisBUITest = null;
  private long duration = 15 * 60;//test duration in seconds
  private File dataDir = null;
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
  public void test() throws IOException,LogFile.ReadException {
    String testName = EnvironmentVariable.INSTANCE.getValue(null,
        "IMOD_TEST_SECTION", AxisID.ONLY);
    System.err.println("test " + testName + ":");
    if (testName == null || testName.matches("\\*+")) {
      fail("$IMOD_TEST_SECTION has not been set");
    }
    //get the uitest.adoc
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.UITEST, AxisID.ONLY);
    }
    catch (FileNotFoundException e) {
      fail(e.getMessage());
      return;
    }
    catch (IllegalStateException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    if (autodoc == null) {
      fail("Missing autodoc");
      return;
    }
    reader = new AdocCommandReader(autodoc, UITestTestCommand.SECTION_TYPE);
    UITestCommandFactory factory = new UITestCommandFactory();
    UITestCommand command = (UITestCommand) reader.nextCommand(null, factory);
    //process commands in the global section
    while (!command.isEmpty()) {
      if (command.isKnown()) {
        UITestAction action = command.getAction();
        if (action == UITestAction.SLEEP) {
          setSleep(command);
        }
      }
      command = (UITestCommand) reader.nextCommand(command, factory);
    }
    //process section specified by testName
    reader.setSection(testName);
    if (reader.isDone()) {
      return;
    }
    processSection();
    UIHarness.INSTANCE.setLog(true);
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
    return new UITestTestCommand();
  }

  private void setDuration(UITestTestCommand command) {
    String[] durationArray = command.getValue().split(":");
    if (durationArray.length == 4 && !durationArray[3].equals("")) {
      duration = Long.parseLong(durationArray[3]);
    }
    else {
      duration = 0;
    }
    if (!durationArray[2].equals("")) {
      duration += Long.parseLong(durationArray[2]) * 60;
    }
    if (!durationArray[1].equals("")) {
      duration += Long.parseLong(durationArray[1]) * 60 * 60;
    }
    if (!durationArray[0].equals("")) {
      duration += Long.parseLong(durationArray[0]) * 60 * 60 * 24;
    }
  }

  private String getRequiredValue(UITestTestCommand command) {
    String value = command.getValue();
    assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, value);
    return value;
  }

  private void setVariable(UITestTestCommand command) {
    AxisID axisID = command.getAxisID();
    if (axisID == null || axisID == AxisID.ONLY || axisID == AxisID.FIRST) {
      variablesA = setVariable(command, variablesA);
    }
    if (axisID == null || axisID == AxisID.SECOND) {
      variablesB = setVariable(command, variablesB);
    }
  }

  private ArrayList setVariable(UITestTestCommand command, ArrayList variables) {
    if (variables == null) {
      variables = new ArrayList();
    }
    variables.add(command.getVariable());
    return variables;
  }

  private void setDataset(UITestTestCommand command) {
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
  private void processSection() throws FileNotFoundException, IOException,LogFile.ReadException {
    UITestTestCommand command = (UITestTestCommand) reader.nextCommand(null,
        this);
    while (!command.isEmpty()) {
      if (command.isKnown()) {
        UITestAction action = command.getAction();
        if (action == UITestAction.ADOC) {
          setAutodoc(command);
        }
        else if (action == UITestAction.COPY) {
          if (datasetDir == null) {
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
          if (datasetDir == null) {
            setDatasetDir(command);
          }
        }
        else if (action == UITestAction.DURATION) {
          setDuration(command);
        }
        else if (action == UITestAction.DATA_DIR) {
          //Get a file directory relative to the base file dir
          dataDir = getRelativeDir(command, DATA_DIR, false);
        }
        else if (action == UITestAction.SET) {
          setVariable(command);
        }
      }
      command = (UITestTestCommand) reader.nextCommand(command, this);
    }
    assertTrue(reader.getInfo(), UITestAction.ADOC + " is required.",
        autodocA != null || autodocB != null);
    if (datasetDir == null) {
      setDatasetDir(null, false);
    }
  }

  long getDuration() {
    return duration;
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
      EtomoDirector.main(ARGS);
    }else {
      File dataFile = new File(System.getProperty("user.dir"), dataFileName);
      String[] args = new String[ARGS.length + 1];
      for (int i = 0; i < ARGS.length; i++) {
        args[i] = ARGS[i];
      }
      args[ARGS.length] = dataFile.getAbsolutePath();
      EtomoDirector.main(args);
    }
    etomo = EtomoDirector.INSTANCE;
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
   * copy a file from dataDir to the working directory
   * @param attrib
   * @param sourceDir
   */
  void copyFile(AdocCommand command) {
    assertNotNull(command.toString()
        + ":  The dataDir must be set before a copy can be done.");
    String fileName = command.getValue();
    assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, fileName);
    File file = new File(dataDir, fileName);
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
  private void setAutodoc(UITestTestCommand command)
      throws FileNotFoundException, IOException,LogFile.ReadException {
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
      autodocB = AutodocFactory.getInstance(SOURCE_DIR, value,
          AxisID.ONLY);
      return;
    }
    if (autodocA != null) {
      return;
    }
    axisIDA = axisID;
    autodocA = AutodocFactory.getInstance(SOURCE_DIR, value,
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
    assertNull("The dataset directory can only be set once:dirName=" + dirName
        + ",datasetDir=", datasetDir);
    JfcUnitTests.TEST_ROOT_DIR.mkdirs();
    //Get the directory name
    if (dirName == null) {
      dirName = reader.getName();
      assertNotNull(reader.getInfo(), "Invalid section", dirName);
    }
    //make the directory path
    datasetDir = new File(JfcUnitTests.TEST_ROOT_DIR, dirName);
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

  private void setDatasetDir(UITestTestCommand command) {
    File datasetDir = null;
    //Get the directory name
    String dirName = command.getValue();
    boolean keep = command.isKeep();
    setDatasetDir(dirName, keep);
  }

  private File getAbsoluteDir(AdocCommand command, boolean makeDirs) {
    //Get the directory name
    String dirName = command.getValue();
    assertNotNull(reader.getInfo(), "Unknown name/value pair format: "
        + command, dirName);
    //make the directories
    File dir = new File(dirName);
    assertTrue(reader.getInfo(), dir.isAbsolute());
    if (makeDirs && !dir.exists()) {
      dir.mkdirs();
    }
    return dir;
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
  private File getRelativeDir(ReadOnlyStatement pair, File rootDir, boolean makeDirs) {
    //Get the directory name
    String dirName = pair.getRightSide();
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
  private File getRequiredRelativeDir(ReadOnlySection section, File rootDir,
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

  public void fail(String sectionInfo, String command, String message) {
    StringBuffer buffer = new StringBuffer();
    if (sectionInfo != null) {
      buffer.append(sectionInfo + ": ");
    }
    if (command != null) {
      buffer.append(command + ":\n");
    }
    buffer.append(message);
    fail(message);
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

  public void assertNull(String message, File file) {
    if (file == null) {
      return;
    }
    fail(message + file.getAbsolutePath());
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
 * <p> Revision 1.25  2007/04/09 21:23:53  sueh
 * <p> Bug# 964 Changed NameValuePair to Statement and child classes.
 * <p>
 * <p> Revision 1.24  2007/03/21 19:48:12  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.  Moved AdocCommand
 * <p> classes out of the autodoc package.
 * <p>
 * <p> Revision 1.23  2007/03/05 21:29:27  sueh
 * <p> bug# 964 Stop controlling autodoc instances, except for the standard ones.
 * <p>
 * <p> Revision 1.22  2007/03/01 01:45:51  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 1.21  2006/10/24 23:34:49  sueh
 * <p> bug# 948 Changed filedir to datadir.  Removed testdir.
 * <p>
 * <p> Revision 1.20  2006/10/11 10:12:33  sueh
 * <p> bug# 938 Making ThreadGroup independent of UITest, so that it does not require
 * <p> JfcUnit to compile.
 * <p>
 * <p> Revision 1.19  2006/10/10 05:25:25  sueh
 * <p> bug# 931 Added uncaughtException to handle uncaught exceptions in Etomo
 * <p> during UITest.
 * <p>
 * <p> Revision 1.18  2006/08/28 18:27:04  sueh
 * <p> bug# 923 Changed the source attribute to filedir.  Global filedir is an absolute file
 * <p> path.
 * <p>
 * <p> Revision 1.17  2006/06/30 16:29:54  sueh
 * <p> bug# 883 Added EnvironmentVariable, a class to get and store environment
 * <p> variables.
 * <p>
 * <p> Revision 1.16  2006/06/14 00:41:33  sueh
 * <p> bug# 852 Moved classes to the autodoc package that parse an autodoc or find
 * <p> attributes specific to a type of autdoc.
 * <p>
 * <p> Revision 1.15  2006/06/06 18:16:19  sueh
 * <p> bug# 766 Fail when test name is not set.  Setting log in UIHarness.
 * <p>
 * <p> Revision 1.14  2006/06/06 17:21:45  sueh
 * <p> bug# 766 Add fails when autodoc is not found
 * <p>
 * <p> Revision 1.13  2006/05/11 19:33:00  sueh
 * <p> Removed unnessary strings from ARGS.
 * <p>
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
