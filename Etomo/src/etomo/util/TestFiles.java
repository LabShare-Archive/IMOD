package etomo.util;

import java.io.File;
import java.io.IOException;

import etomo.type.AxisID;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $$Author$$
 *
 * @version $$Revision$$
 *
 * <p> $$Log$
 * <p> $Revision 1.1  2004/05/03 18:05:13  sueh
 * <p> $param testing proof of concept - object to retrieve testing files
 * <p> $$ </p>
 */
public class TestFiles {
  public static final String rcsid = "$$Id$$";
  
  public static final String SELF_TEST_DIR_ENV_VAR = "IMOD_SELF_TEST_DIR";
  public static final String TEST_DIR = "SelfTestDir";
  public static final String COMSCRIPT_DIR = "ComscriptDir";
  public static final String OLD_VERSION_DIR = "OldVersions";
  public static final String NEW_VERSION_DIR = "NewVersions";
  public static final String TEST_TYPE_LOAD_DIR = "testTypeLoadDir";
  public static final String TEST_TYPE_SET_DIR = "testTypeSetDir";
  public static final int TOTAL_TESTS = 2;
  public static final int TEST_TYPE_LOAD = 0;
  public static final int TEST_TYPE_SET = 1;
  public static final int OLD_VERSION = 0;
  public static final int NEW_VERSION = 1;
  
  private static TestFiles one = null;
  
  private File testDir;
  private File comscriptDir;
  private File oldVersionDir;
  private File newVersionDir;
  private File userTestDir;
  private File testTypeLoadDir;
  private File testTypeSetDir;
  
  public static TestFiles getTestFiles() {
    if (one == null) {
      one = new TestFiles();
    }
    return one;
  }

  private TestFiles() {
    testDir = new File(Utilities.getEnvironmentVariable(SELF_TEST_DIR_ENV_VAR), TEST_DIR);
    comscriptDir = new File(testDir, COMSCRIPT_DIR);
    oldVersionDir = new File(comscriptDir, OLD_VERSION_DIR);
    newVersionDir = new File(comscriptDir, NEW_VERSION_DIR);
    userTestDir = new File(System.getProperty("user.dir"), TEST_DIR);
    testTypeLoadDir = new File(userTestDir, TEST_TYPE_LOAD_DIR);
    testTypeSetDir = new File(userTestDir, TEST_TYPE_SET_DIR);
    if (!userTestDir.exists()) {
      userTestDir.mkdir();
    }
    if (!testTypeLoadDir.exists()) {
      testTypeLoadDir.mkdir();
    }
    if (!testTypeSetDir.exists()) {
      testTypeSetDir.exists();
    }
  }
  
  public File getComscript(
    String scriptName,
    AxisID axisID,
    int testType,
    int version,
    boolean overWrite) {
    File versionDir = null;
    if (version == OLD_VERSION) {
      versionDir = oldVersionDir;
    }
    else if (version == NEW_VERSION) {
      versionDir = newVersionDir;
    }
    else {
      return null;
    }
    if (!versionDir.exists()) {
      return null;
    }
    
    File testTypeDir = null;
    if (testType == TEST_TYPE_LOAD) {
      testTypeDir = testTypeLoadDir;
    }
    else if (testType == TEST_TYPE_SET) {
      testTypeDir = testTypeSetDir;
    }
    else {
      return null;
    }
    String command = scriptName + axisID.getExtension() + ".com";
    File comscriptUserTestFile = new File(userTestDir, command);
    if (overWrite) {
      File comscriptTestFile = new File(versionDir, command);
      if (!comscriptTestFile.exists()) {
        return null;
      }
      try {
        Utilities.copyFile(comscriptTestFile, comscriptUserTestFile);
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    return comscriptUserTestFile;
  }

}
