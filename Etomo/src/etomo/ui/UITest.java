package etomo.ui;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.EtomoDirector;
import etomo.JfcUnitTests;
import etomo.process.SystemProgram;
import etomo.storage.autodoc.Attribute;
import etomo.storage.autodoc.Autodoc;
import etomo.storage.autodoc.NameValuePair;
import etomo.storage.autodoc.NameValuePairLocation;
import etomo.storage.autodoc.Section;
import etomo.storage.autodoc.SectionLocation;
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
public final class UITest extends JFCTestCase {
  public static final String rcsid = "$Id$";

  static final File TEST_REPOSITORY = Utilities.getExistingDir(
      "IMOD_UITEST_SOURCE", AxisID.ONLY);
  static final long DEFAULT_SLEEP = 1000;
  static final String DATASET_ATTRIB = "dataset";

  private static final String TEST_SECTION_TYPE = "Test";
  private static final String SOURCE_DIR_ATTRIB = "source";
  private static final String[] ARGS = new String[] { "--selftest", "--test"/* , "--names"*/};
  private static final String FIDUCIAL_DIAMETER_ATTRIB = "fiducial-diameter";
  private static final String ADOC_ATTRIB = "adoc";
  private static final String COPY_ATTRIB = "copy";
  private static final String DATASET_DIR_ATTRIB = "datasetdir";
  private static final String KEEP_ATTRIB = "keep";
  private static final String DURATION_ATTRIB = "duration";
  private static final String DATAFILE_ATTRIB = "datafile";

  private File testDir = null;
  private Autodoc autodocA = null;
  private Autodoc autodocB = null;
  private JFCTestHelper helper = null;
  private EtomoDirector etomo = null;
  private long sleep = DEFAULT_SLEEP;
  private double fiducialDiameter = 0;
  private String dataset = null;
  private boolean keep = false;
  private UIAxisTest axisAUITest = null;
  private UIAxisTest axisBUITest = null;
  private long duration = 0;
  private File currentSourceDir = null;
  private File autodocSourceDir = null;
  private AxisID axisIDA = null;
  private AxisID axisIDB = null;
  private String dataFileName = null;

  protected void setUp() throws Exception {
    super.setUp();
    helper = new JFCTestHelper();
    setHelper(helper);
  }

  protected void tearDown() throws Exception {
    if (etomo != null) {
      exit();
    }
    JFCTestHelper.cleanUp(this);
    super.tearDown();
  }

  void exit() {
    etomo.setTestDone(true);
    etomo.exitProgram(AxisID.ONLY);
    etomo = null;
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
    //get single entries
    testDir = getRelativeDir(autodoc, "testdir", JfcUnitTests.TEST_ROOT_DIR,
        true);
    autodocSourceDir = getRelativeDir(autodoc, SOURCE_DIR_ATTRIB,
        TEST_REPOSITORY, false);
    setSleep(autodoc);
    //run each test specified in the Test sections
    SectionLocation testLoc = autodoc.getSectionLocation(TEST_SECTION_TYPE);
    Section test = autodoc.getSection(TEST_SECTION_TYPE, testName);
    if (test != null) {
      processSection(test, autodocSourceDir);
      runAxisLevelTests(test, autodocSourceDir);
      test = autodoc.nextSection(testLoc);
    }
  }

  /**
   * set required field fiducial diameter
   * @param autodoc
   */
  private void setFiducialDiameter(Section test) {
    Attribute fiducialDiameterAttrib = test
        .getAttribute(FIDUCIAL_DIAMETER_ATTRIB);
    assertNotNull(null, "In uitest.adoc:  the " + FIDUCIAL_DIAMETER_ATTRIB
        + " attribute is required.", fiducialDiameterAttrib);
    String value = fiducialDiameterAttrib.getValue();
    assertNotNull(null, "In uitest.adoc:  the " + FIDUCIAL_DIAMETER_ATTRIB
        + " attribute must have a value.", value);
    try {
      fiducialDiameter = Double.parseDouble(value);
    }
    catch (NumberFormatException e) {
      fail(null, "In uitest.adoc:  the " + FIDUCIAL_DIAMETER_ATTRIB
          + " value must be numeric.");
    }
  }

  private void setSleep(Autodoc autodoc) {
    Attribute sleepAttrib = autodoc.getAttribute(UITestAction.SLEEP.toString());
    if (sleepAttrib == null) {
      return;
    }
    String value = sleepAttrib.getValue();
    if (value == null) {
      fail(null,
          "In uitest.adoc:  the global sleep attribute must have a value.");
    }
    try {
      sleep = Long.parseLong(value);
    }
    catch (NumberFormatException e) {
      fail(null, "In uitest.adoc:  the global sleep value must be numeric.");
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

  /**
   * Performs all commands and open all autodocs found in the section
   * @param test
   * @param sourceDir
   * @throws FileNotFoundException
   * @throws IOException
   */
  private void processSection(Section test, File sourceDir)
      throws FileNotFoundException, IOException {
    autodocA = null;
    autodocB = null;
    dataset = null;
    fiducialDiameter = 0;
    //clean the dataset directory and make it the working directory
    cleanDatasetDir(test);
    //set parameters
    setDataset(test);
    setFiducialDiameter(test);
    currentSourceDir = sourceDir;
    NameValuePairLocation pairLoc = test.getNameValuePairLocation();
    NameValuePair pair = test.nextNameValuePair(pairLoc);
    while (pair != null) {
      if (pair.equalsName(ADOC_ATTRIB, 0)) {
        setAutodoc(pair, currentSourceDir);
      }
      else if (pair.equalsName(COPY_ATTRIB, 0)) {
        //copy a file from the current source directory to the working directory
        copyFile(pair.getValue(), currentSourceDir);
      }
      else if (pair.equalsName(SOURCE_DIR_ATTRIB, 0)) {
        //Get a source directory relative to the test repository
        currentSourceDir = getRelativeDir(pair, TEST_REPOSITORY, false);
      }
      else if (pair.equalsName(DURATION_ATTRIB, 0)) {
        String[] durationArray = pair.getValue().split(":");
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
      else if (pair.equalsName(DATAFILE_ATTRIB, 0)) {
        dataFileName = pair.getValue();
      }
      pair = test.nextNameValuePair(pairLoc);
    }
    if (autodocA == null && autodocB == null) {
      fail(null, "In uitest.adoc:  at least one " + ADOC_ATTRIB
          + " attribute is required.");
    }
    if (duration <= 0) {
      fail(null, "In uitest.adoc:  the " + DURATION_ATTRIB
          + " attribute is missing, 0, or caused an overflow: " + duration);
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
  private void runAxisLevelTests(Section test, File sourceDir) {
    if (autodocA == null && autodocB == null) {
      return;
    }
    //run Etomo
    if (dataFileName == null) {
      etomo = EtomoDirector.createInstance_test(ARGS);
    }
    else {
      File dataFile = new File(new File(testDir, test.getName()), dataFileName);
      String[] args = new String[ARGS.length + 1];
      for (int i = 0; i < ARGS.length; i++) {
        args[i] = ARGS[i];
      }
      args[ARGS.length] = dataFile.getAbsolutePath();
      etomo = EtomoDirector.createInstance_test(args);
    }
    //Create the UIAxisTest instances
    if (autodocA != null) {
      axisAUITest = new UIAxisTest(this, dataset, autodocA, helper,
          fiducialDiameter, axisIDA, dataFileName != null);
    }
    if (autodocB != null) {
      axisBUITest = new UIAxisTest(this, dataset, autodocB, helper,
          fiducialDiameter, axisIDB, dataFileName != null);
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

  boolean removeDialog(AxisID axisID, DialogType dialogType) {
    if (axisID == AxisID.SECOND) {
      if (axisBUITest == null || axisBUITest.isDone()) {
        return true;
      }
      return axisBUITest.removeDialog(dialogType);
    }
    if (axisAUITest == null || axisAUITest.isDone()) {
      return true;
    }
    return axisAUITest.removeDialog(dialogType);
  }

  /**
   * copy a file from sourceDir to the working directory
   * @param attrib
   * @param sourceDir
   */
  void copyFile(String fileName, File sourceDir) {
    assertNotNull("In uitest.adoc:  the " + COPY_ATTRIB
        + " attribute must have a value.", fileName);
    File file = new File(sourceDir, fileName);
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
  private void setAutodoc(NameValuePair adocPair, File sourceDir)
      throws FileNotFoundException, IOException {
    String value = adocPair.getValue();
    assertNotNull(null, "Unknown name/value pair format: "
        + adocPair.getString(), value);
    //look for adoc = autodoc
    String axisName = adocPair.getName(1);
    if (axisName == null) {
      axisIDA = AxisID.ONLY;
      //Set autodocA if it is not already set
      if (autodocA != null) {
        return;
      }
      autodocA = Autodoc.getUITestAxisInstance_test(sourceDir, value,
          AxisID.ONLY);
      return;
    }
    //look for adoc.a = autodoc
    if (axisName.equals(AxisID.FIRST.getExtension())) {
      axisIDA = AxisID.FIRST;
      //Set autodocA if it is not already set
      if (autodocA != null) {
        return;
      }
      autodocA = Autodoc.getUITestAxisInstance_test(sourceDir, value,
          AxisID.ONLY);
      return;
    }
    //look for adoc.b = autodoc
    else if (axisName.equals(AxisID.SECOND.getExtension())) {
      axisIDB = AxisID.SECOND;
      //Set autodocB if it is not already set
      if (autodocB != null) {
        return;
      }
      autodocB = Autodoc.getUITestAxisInstance_test(sourceDir, value,
          AxisID.ONLY);
      return;
    }
  }

  private void setDataset(Section section) {
    Attribute datasetAttrib = section.getAttribute(DATASET_ATTRIB);
    if (datasetAttrib == null) {
      //default value
      dataset = section.getName();
      return;
    }
    String value = datasetAttrib.getValue();
    assertNotNull(null, "In uitest.adoc:  the " + DATASET_ATTRIB
        + " attribute must have a value.", value);
    dataset = value;
  }

  /**
   * Get the datasetDir for the current section.  If the keep attribute isn't
   * found then clean the datasetDir by deleting
   * it and then recreating it.  Set it be the working directory.
   * @param datasetDir
   * @return
   */
  private void cleanDatasetDir(Section section) {
    File datasetDir = null;
    //Get the directory name
    String dirName = null;
    Attribute dirAttrib = section.getAttribute(DATASET_DIR_ATTRIB);
    if (dirAttrib == null) {
      //use the default directory if the datasetdir atttribute isn't found
      datasetDir = getRequiredRelativeDir(section, testDir, true);
    }
    else {
      //get directory name
      dirName = dirAttrib.getValue();
      if (dirName == null) {
        //get optional keep attribute
        Attribute keepAttrib = dirAttrib.getAttribute(KEEP_ATTRIB);
        if (keepAttrib == null) {
          fail(null, DATASET_DIR_ATTRIB
              + " attribute can only be followed by a " + KEEP_ATTRIB
              + " attribute.");
        }
        keep = true;
        //get directory name
        dirName = keepAttrib.getValue();
      }
      assertNotNull(null, "In uitest.adoc:  the " + DATASET_DIR_ATTRIB
          + " attribute must have a value.", dirName);
      //make the directory path
      datasetDir = new File(testDir, dirName);
      if (!datasetDir.exists()) {
        datasetDir.mkdirs();
      }
    }
    //if keep attribute was found, do not clean the directory
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

  /**
   * Create a directory or directory path in rootDir using an attribute value.
   * If the attribute value does not exist, return rootDir.  Make
   * directories if makeDirs is true and the directory does not exist.
   * This is an optional attribute:  return rootDir if the attribute is not found.
   * @param autodoc
   * @param attribName
   * @param rootDir
   * @param makeDirs - if true, make the directories
   * @return
   */
  private File getRelativeDir(Autodoc autodoc, String attribName, File rootDir,
      boolean makeDirs) {
    //Make the root directories on the root directory since it could be returned
    if (makeDirs && !rootDir.exists()) {
      rootDir.mkdirs();
    }
    //Get the directory name
    String dirName = null;
    Attribute dirAttrib = autodoc.getAttribute(attribName);
    if (dirAttrib == null) {
      //default value
      return rootDir;
    }
    dirName = dirAttrib.getValue();
    assertNotNull(null, "In uitest.adoc:  the " + attribName
        + " attribute must have a value.", dirName);
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

  public void assertEquals(String sectionInfo, String message, boolean expected,
      boolean actual) {
    if (sectionInfo != null) {
      message = sectionInfo + ": " + message;
    }
    assertEquals(message, expected, actual);
  }
}
/**
 * <p> $Log$
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