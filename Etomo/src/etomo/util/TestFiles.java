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
 * <p> $$Log$$ </p>
 */
public class TestFiles {
  public static final String rcsid = "$$Id$$";
  
  public static final String SELF_TEST_DIR_ENV_VAR = "IMOD_SELF_TEST_DIR";
  public static final String TEST_DIR = "SelfTestDir";
  public static final String COMSCRIPT_DIR = "ComscriptDir";
  public static final String OLD_VERSION_DIR = "OldVersions";
  public static final String NEW_VERSION_DIR = "NewVersions";
  
  private static TestFiles one = null;
  
  private File testDir;
  private File comscriptDir;
  private File oldVersionDir;
  private File newVersionDir;
  private File userTestDir;
  
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
  }
  
  public File getComscript(
    String scriptName,
    AxisID axisID,
    int version,
    boolean fromTest) {
    File versionDir = null;
    if (version == 0) {
      versionDir = oldVersionDir;
    }
    else if (version == 1) {
      versionDir = newVersionDir;
    }
    else {
      return null;
    }
    if (!versionDir.exists()) {
      return null;
    }
    
    String command = scriptName + axisID.getExtension() + ".com";
    File comscriptUserTestFile = new File(userTestDir, command);
    if (fromTest) {
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
