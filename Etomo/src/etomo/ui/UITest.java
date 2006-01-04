package etomo.ui;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.EtomoDirector;
import etomo.JfcUnitTests;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.util.Utilities;
import junit.extensions.jfcunit.JFCTestCase;

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
  private static final String SECTION_NAME_ENV = "IMOD_TEST_SECTION";

  private static final String ADOC_ATTRIB = "adoc";
  private static final String COPY_ATTRIB = "copy";
  private static final String SOURCE_DIR_ATTRIB = "source";
  private static final String TEST_DIR_ATTRIB = "testdir";
  private static final String DATASET_ATTRIB = "dataset";
  private static final String DATASET_DIR_ATTRIB = "datasetdir";
  private static final String[] ARGS = new String[] { "--selftest", "--test" };

  private File testDir = null;
  private Autodoc autodocA = null;
  private Autodoc autodocB = null;

  /**
   * run all the tests in uitest.adoc
   * @throws IOException
   */
  public final void test() throws IOException {
    String testName = Utilities.getEnvironmentVariable(null, SECTION_NAME_ENV, AxisID.ONLY);
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
    testDir = getRelativeDir(autodoc, TEST_DIR_ATTRIB,
        JfcUnitTests.TEST_ROOT_DIR, true);
    File sourceDir = getRelativeDir(autodoc, SOURCE_DIR_ATTRIB,
        TEST_REPOSITORY, false);
    //run each test specified in the Test sections
    SectionLocation testLoc = autodoc
        .getSectionLocation(UITestConstants.TEST_SECTION_TYPE);
    Section test = autodoc.getSection(UITestConstants.TEST_SECTION_TYPE, testName);
    if (test != null) {
      runAxisLevelTests(processTestSection(test, sourceDir), sourceDir);
      test = autodoc.nextSection(testLoc);
    }
  }

  /**
   * Performs all commands and open all autodocs found in the section
   * @param test
   * @param sourceDir
   * @return the dataset name
   * @throws FileNotFoundException
   * @throws IOException
   */
  private final String processTestSection(Section test, File sourceDir)
      throws FileNotFoundException, IOException {
    autodocA = null;
    autodocB = null;
    //clean the dataset directory and make it the working directory
    File datasetDir = cleanDatasetDir(test);
    File currentSourceDir = sourceDir;
    String dataset = getDataset(test);
    AttributeLocation attribLoc = test.getAttributeLocation();
    Attribute attrib = test.nextAttribute(attribLoc);
    while (attrib != null) {
      if (attrib.equalsName(ADOC_ATTRIB)) {
        setAutodoc(attrib, currentSourceDir);
      }
      else if (attrib.equalsName(COPY_ATTRIB)) {
        //copy a file from the current source directory to the working directory
        copyFile(attrib, currentSourceDir);
      }
      else if (attrib.equalsName(SOURCE_DIR_ATTRIB)) {
        //Get a source directory relative to the test repository
        currentSourceDir = getRelativeDir(attrib, TEST_REPOSITORY,
            currentSourceDir, false);
      }
      attrib = test.nextAttribute(attribLoc);
    }
    return dataset;
  }

  /**
   * runs tests with uitest-axis autodocs for A and/or B
   * @param args
   * @param sourceDir
   */
  private final void runAxisLevelTests(String dataset, File sourceDir) {
    if (autodocA == null && autodocB == null) {
      return;
    }
    EtomoDirector etomo = EtomoDirector.createInstance_test(ARGS);
    AxisUITest axisAUITest = null;
    AxisUITest axisBUITest = null;
    if (autodocA != null) {
      axisAUITest = new AxisUITest(dataset, autodocA, sourceDir);
    }
    if (autodocB != null) {
      axisBUITest = new AxisUITest(dataset, autodocB, sourceDir);
    }
    boolean testingA = axisAUITest != null;
    boolean testingB = axisBUITest != null;
    while ((testingA && !axisAUITest.isDone())
        || (testingB && !axisBUITest.isDone())) {
      if (testingA && !axisAUITest.isDone()) {
        axisAUITest.test(testingB);
      }
      if (testingB && !axisBUITest.isDone()) {
        axisBUITest.test(testingA);
      }
    }
  }

  /**
   * copy a file from sourceDir to the working directory
   * @param attrib
   * @param sourceDir
   */
  private final void copyFile(Attribute attrib, File sourceDir) {
    String value = attrib.getUnformattedValue();
    if (value == null) {
      return;
    }
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
  private final void setAutodoc(Attribute adocAttrib, File sourceDir)
      throws FileNotFoundException, IOException {
    //look for adoc =
    String value = adocAttrib.getUnformattedValue();
    if (value != null) {
      //Set autodocA if it is not already set
      if (autodocA != null) {
        return;
      }
      autodocA = Autodoc.getUITestAxisInstance_test(sourceDir, value,
          AxisID.ONLY);
      return;
    }
    //look for adoc.a =
    Attribute axisAttrib = adocAttrib.getAttribute(AxisID.FIRST.getExtension());
    if (axisAttrib != null) {
      value = axisAttrib.getUnformattedValue();
      if (value != null) {
        //Set autodocA if it is not already set
        if (autodocA != null) {
          return;
        }
        autodocA = Autodoc.getUITestAxisInstance_test(sourceDir, value,
            AxisID.ONLY);
        return;
      }
    }
    //look for adoc.b =
    axisAttrib = adocAttrib.getAttribute(AxisID.SECOND.getExtension());
    if (axisAttrib != null) {
      value = axisAttrib.getUnformattedValue();
      //Set autodocB if value is not null and autodocB is not already set
      if (value != null && autodocB == null) {
        autodocB = Autodoc.getUITestAxisInstance_test(sourceDir, value,
            AxisID.ONLY);
      }
    }
  }

  private final String getDataset(Section section) {
    try {
      String value = section.getAttribute(DATASET_ATTRIB).getUnformattedValue();
      if (value != null) {
        return value;
      }
    }
    catch (NullPointerException e) {
    }
    return section.getName();
  }

  /**
   * Create datasetDir for the current section, clean the datasetDir by deleting
   * it and then recreate it and set it be the working directory.
   * If datasetDir is already set, this function has no effect.
   * @param datasetDir
   * @return
   */
  private final File cleanDatasetDir(Section section) {
    File datasetDir = getRequiredRelativeDir(section, DATASET_DIR_ATTRIB,
        testDir, true);
    SystemProgram remove = new SystemProgram(System.getProperty("user.dir"),
        new String[] { "rm", "-fr", datasetDir.getAbsolutePath() }, AxisID.ONLY);
    remove.run();
    datasetDir.mkdir();
    System.setProperty("user.dir", datasetDir.getAbsolutePath());
    return datasetDir;
  }

  /**
   * Create a directory or directory path in rootDir using an attribute value.
   * If the attribute value does not exist, return rootDir.  Make
   * directories if makeDirs is true and the directory does not exist.
   * Return rootDir if the attribute is not found.
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
    try {
      dirName = autodoc.getAttribute(attribName).getUnformattedValue();
    }
    catch (NullPointerException e) {
      return rootDir;
    }
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
    try {
      dirName = section.getAttribute(attribName).getUnformattedValue();
    }
    catch (NullPointerException e) {
      //Get an alternative directory
      return getRequiredRelativeDir(section, rootDir, makeDirs);
    }
    if (dirName == null) {
      //Get an alternative directory
      return getRequiredRelativeDir(section, rootDir, makeDirs);
    }
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
   * @param originalDir
   * @return the directory
   */
  private final File getRelativeDir(Attribute attrib, File rootDir,
      File originalDir, boolean makeDirs) {
    if (makeDirs && !originalDir.exists()) {
      originalDir.mkdirs();
    }
    //Get the directory name
    String dirName = null;
    try {
      dirName = attrib.getUnformattedValue();
    }
    catch (NullPointerException e) {
      return originalDir;
    }
    if (dirName == null) {
      return originalDir;
    }
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