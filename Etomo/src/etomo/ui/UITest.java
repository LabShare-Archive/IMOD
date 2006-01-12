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
public class UITest extends JFCTestCase {
  public static final String rcsid = "$Id$";

  static final File TEST_REPOSITORY = Utilities.getExistingDir(
      "IMOD_UITEST_SOURCE", AxisID.ONLY);
  static final String SLEEP_ATTRIB = "sleep";

  private static final String TEST_SECTION_TYPE = "Test";
  private static final String SOURCE_DIR_ATTRIB = "source";
  private static final String[] ARGS = new String[] { "--selftest", "--test" };
  private static final String FIDUCIAL_DIAMETER_ATTRIB = "fiducial-diameter";
  private static final String ADOC_ATTRIB = "adoc";
  private static final String COPY_ATTRIB = "copy";
  private static final String DATASET_ATTRIB = "dataset";

  private File testDir = null;
  private Autodoc autodocA = null;
  private Autodoc autodocB = null;
  private JFCTestHelper helper = null;
  private EtomoDirector etomo = null;
  private long sleep = 1000;
  private double fiducialDiameter = 0;
  private String dataset = null;

  protected void setUp() throws Exception {
    super.setUp();
    helper = new JFCTestHelper();
    setHelper(helper);
  }

  protected void tearDown() throws Exception {
    JFCTestHelper.cleanUp(this);
    if (etomo != null) {
      etomo.exitProgram(AxisID.ONLY);
    }
    super.tearDown();
  }

  /**
   * run all the tests in uitest.adoc
   * @throws IOException
   */
  public final void test() throws IOException {
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
    File sourceDir = getRelativeDir(autodoc, SOURCE_DIR_ATTRIB,
        TEST_REPOSITORY, false);
    setSleep(autodoc);
    //run each test specified in the Test sections
    SectionLocation testLoc = autodoc.getSectionLocation(TEST_SECTION_TYPE);
    Section test = autodoc.getSection(TEST_SECTION_TYPE, testName);
    if (test != null) {
      processSection(test, sourceDir);
      runAxisLevelTests(test, sourceDir);
      test = autodoc.nextSection(testLoc);
    }
  }

  /**
   * set required field fiducial diameter
   * @param autodoc
   */
  private final void setFiducialDiameter(Section test) {
    Attribute fiducialDiameterAttrib = test
        .getAttribute(FIDUCIAL_DIAMETER_ATTRIB);
    JFCTestCase.assertNotNull("In uitest.adoc:  the "
        + FIDUCIAL_DIAMETER_ATTRIB + " attribute is required.",
        fiducialDiameterAttrib);
    String value = fiducialDiameterAttrib.getValue();
    JFCTestCase.assertNotNull("In uitest.adoc:  the "
        + FIDUCIAL_DIAMETER_ATTRIB + " attribute must have a value.", value);
    try {
      fiducialDiameter = Double.parseDouble(value);
    }
    catch (NumberFormatException e) {
      JFCTestCase.fail("In uitest.adoc:  the " + FIDUCIAL_DIAMETER_ATTRIB
          + " value must be numeric.");
    }
  }

  private final void setSleep(Autodoc autodoc) {
    Attribute sleepAttrib = autodoc.getAttribute(SLEEP_ATTRIB);
    if (sleepAttrib == null) {
      return;
    }
    String value = sleepAttrib.getValue();
    if (value == null) {
      JFCTestCase
          .fail("In uitest.adoc:  the global sleep attribute must have a value.");
    }
    try {
      sleep = Long.parseLong(value);
    }
    catch (NumberFormatException e) {
      JFCTestCase
          .fail("In uitest.adoc:  the global sleep value must be numeric.");
    }
  }

  final long getSleep() {
    return sleep;
  }

  /**
   * Performs all commands and open all autodocs found in the section
   * @param test
   * @param sourceDir
   * @throws FileNotFoundException
   * @throws IOException
   */
  private final void processSection(Section test, File sourceDir)
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
    File currentSourceDir = sourceDir;
    NameValuePairLocation pairLoc = test.getNameValuePairLocation();
    NameValuePair pair = test.nextNameValuePair(pairLoc);
    while (pair != null) {
      if (pair.equalsName(ADOC_ATTRIB, 0)) {
        setAutodoc(pair, currentSourceDir);
      }
      else if (pair.equalsName(COPY_ATTRIB, 0)) {
        //copy a file from the current source directory to the working directory
        copyFile(pair, currentSourceDir);
      }
      else if (pair.equalsName(SOURCE_DIR_ATTRIB, 0)) {
        //Get a source directory relative to the test repository
        currentSourceDir = getRelativeDir(pair, TEST_REPOSITORY, false);
      }
      pair = test.nextNameValuePair(pairLoc);
    }
    if (autodocA == null && autodocB == null) {
      JFCTestCase.fail("In uitest.adoc:  at least one " + ADOC_ATTRIB
          + " attribute is required.");
    }
  }

  /**
   * runs tests with uitest-axis autodocs for A and/or B
   * @param test
   * @param sourceDir
   */
  private final void runAxisLevelTests(Section test, File sourceDir) {
    if (autodocA == null && autodocB == null) {
      return;
    }
    //run Etomo
    etomo = EtomoDirector.createInstance_test(ARGS);
    //Create the UIAxisTest instances
    UIAxisTest axisAUITest = null;
    UIAxisTest axisBUITest = null;
    if (autodocA != null) {
      axisAUITest = new UIAxisTest(this, dataset, autodocA, sourceDir, helper,
          fiducialDiameter);
    }
    if (autodocB != null) {
      axisBUITest = new UIAxisTest(this, dataset, autodocB, sourceDir, helper,
          fiducialDiameter);
    }
    //Test the axis or axes, taking turns if there are two
    boolean testingA = axisAUITest != null;
    boolean testingB = axisBUITest != null;
    while ((testingA && !axisAUITest.isDone())
        || (testingB && !axisBUITest.isDone())) {
      if (testingA && !axisAUITest.isDone()) {
        axisAUITest.testAxis(testingB);
      }
      if (testingB && !axisBUITest.isDone()) {
        axisBUITest.testAxis(testingA);
      }
    }
  }

  /**
   * copy a file from sourceDir to the working directory
   * @param attrib
   * @param sourceDir
   */
  private final void copyFile(NameValuePair pair, File sourceDir) {
    String value = pair.getValue();
    JFCTestCase.assertNotNull("In uitest.adoc:  the " + COPY_ATTRIB
        + " attribute must have a value.", value);
    File file = new File(sourceDir, value);
    if (!file.exists() || file.isDirectory()) {
      throw new IllegalStateException("bad dataset file: "
          + file.getAbsolutePath());
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
  private final void setAutodoc(NameValuePair adocPair, File sourceDir)
      throws FileNotFoundException, IOException {
    String value = adocPair.getValue();
    JFCTestCase.assertNotNull("Unknown name/value pair format: "
        + adocPair.getString(), value);
    //look for adoc = autodoc
    String axisName = adocPair.getName(1);
    if (axisName == null) {
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
      //Set autodocB if it is not already set
      if (autodocB != null) {
        return;
      }
      autodocB = Autodoc.getUITestAxisInstance_test(sourceDir, value,
          AxisID.ONLY);
      return;
    }
  }

  private final void setDataset(Section section) {
    Attribute datasetAttrib = section.getAttribute(DATASET_ATTRIB);
    if (datasetAttrib == null) {
      //default value
      dataset = section.getName();
      return;
    }
    String value = datasetAttrib.getValue();
    JFCTestCase.assertNotNull("In uitest.adoc:  the " + DATASET_ATTRIB
        + " attribute must have a value.", value);
    dataset = value;
  }

  /**
   * Create datasetDir for the current section, clean the datasetDir by deleting
   * it and then recreate it and set it be the working directory.
   * @param datasetDir
   * @return
   */
  private final void cleanDatasetDir(Section section) {
    File datasetDir = getRequiredRelativeDir(section, "datasetdir", testDir,
        true);
    SystemProgram remove = new SystemProgram(System.getProperty("user.dir"),
        new String[] { "rm", "-fr", datasetDir.getAbsolutePath() }, AxisID.ONLY);
    remove.run();
    datasetDir.mkdir();
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
  private final File getRelativeDir(Autodoc autodoc, String attribName,
      File rootDir, boolean makeDirs) {
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
    JFCTestCase.assertNotNull("In uitest.adoc:  the " + attribName
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
   * Make directories if makeDirs is true and the directory does not exist.
   * If the attribute value does not exist return the result of
   * getRequiredRelativeDir(SectionFile,makeDirs).
   * @param section
   * @param attribName
   * @param rootDir
   * @param required
   * @param makeDirs - if true, make the directories
   * @return
   */
  private final File getRequiredRelativeDir(Section section, String attribName,
      File rootDir, boolean makeDirs) {
    //Get the directory name
    String dirName = null;
    Attribute dirAttrib = section.getAttribute(attribName);
    if (dirAttrib == null) {
      //default value
      return getRequiredRelativeDir(section, rootDir, makeDirs);
    }
    dirName = dirAttrib.getValue();
    JFCTestCase.assertNotNull("In uitest.adoc:  the " + attribName
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
  private final File getRelativeDir(NameValuePair pair, File rootDir,
      boolean makeDirs) {
    //Get the directory name
    String dirName = pair.getValue();
    JFCTestCase.assertNotNull("Unknown name/value pair format: "
        + pair.getString(), dirName);
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
  private final File getRequiredRelativeDir(Section section, File rootDir,
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
}
/**
 * <p> $Log$
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