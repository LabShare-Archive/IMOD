/**
 * <p>Description: This class prodives utility functions for working unit,
 * functional and integration testing.</p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.23  2009/10/23 23:52:07  sueh
 * <p> bug# 1275 No default manager.
 * <p>
 * <p> Revision 1.22  2009/03/17 00:46:43  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.21  2009/02/23 21:38:01  sueh
 * <p> bug# 1180 Ported from 3.13.
 * <p>
 * <p> Revision 1.18.4.3  2009/02/23 21:32:09  sueh
 * <p> bug# 1180 Checking for "has disappeared" error message.  Error doesn't
 * <p> cause a non-zero return value.
 * <p> Revision 1.20  2009/02/20 18:06:26  sueh
 * <p> bug# 1180 Added more diagnositic info to checkVector.  Trying the bad directory deletion for both export commands.
 * <p>
 * <p> Revision 1.19  2009/02/19 21:38:29  sueh
 * <p> bug# 1180 Ported change from IMOD_3-13, revision 1.18.4.1.
 * <p>
 * <p> Revision 1.18.4.1  2009/02/19 21:29:27  sueh
 * <p> bug# 1180 In checkoutVector try checking out with the current version
 * <p> tag before checking out the latest checkins.
 * <p>
 * <p> Revision 1.18  2007/09/07 00:31:26  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.17  2007/06/11 21:20:48  sueh
 * <p> Removed print statements.
 * <p>
 * <p> Revision 1.16  2007/03/13 19:44:40  sueh
 * <p> bug# 964 Got the test autodoc tests working.  The tests checkout location
 * <p> should be "imod" because "IMOD" is an alias.  Using the alias caused the "-d" option in the "cvs export" command behave incorrectly.
 * <p>
 * <p> Revision 1.15  2007/03/08 22:07:27  sueh
 * <p> bug# 964 Allow vectors to be checked out from IMOD/Etomo/tests as well as the
 * <p> original vector location.
 * <p>
 * <p> Revision 1.14  2007/03/05 21:30:16  sueh
 * <p> bug# 964 Returning target file when checking out vector.
 * <p>
 * <p> Revision 1.13  2006/06/30 16:30:23  sueh
 * <p> bug# 883 Added EnvironmentVariable, a class to get and store environment
 * <p> variables.
 * <p>
 * <p> Revision 1.12  2005/11/10 22:22:02  sueh
 * <p> bug# 748 Removed print statements.
 * <p>
 * <p> Revision 1.11  2005/11/10 18:21:38  sueh
 * <p> bug# 748 Added getVector, a function that tries to get the test vector from
 * <p> the workspace directory tree before checking them out.
 * <p>
 * <p> Revision 1.10  2005/07/29 00:56:26  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.9  2005/04/25 21:44:13  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.8  2005/02/10 18:57:35  sueh
 * <p> bug# 599 String.matches() isn't handling the Windows file separator "\",
 * <p> since its an escape charactor.  Using String.indexOf() instead.
 * <p>
 * <p> Revision 1.7  2004/12/08 21:33:31  sueh
 * <p> bug# 520 Setting the working directory in TestUtilities.checkoutVector().
 * <p> Also setting the fail message for SystemProcessException in
 * <p> TestUtilities.checkoutVector().  Corrected deletion of vector when vector
 * <p> already exists.
 * <p>
 * <p> Revision 1.6  2004/12/07 23:36:34  sueh
 * <p> bug# 520 Changing print statements.
 * <p>
 * <p> Revision 1.5  2004/12/06 23:37:05  sueh
 * <p> bug# 520 Adding print statements.
 * <p>
 * <p> Revision 1.4  2004/11/24 01:30:24  sueh
 * <p> bug# 520 Getting working directory from EtomoDirector instead of
 * <p> property.  makeDirectories:  making sure to get a valid path for
 * <p> badDirectory.
 * <p>
 * <p> Revision 1.3  2004/04/06 22:56:56  rickg
 * <p> Workaround for buggy cvs exports
 * <p>
 * <p> Revision 1.2  2004/04/06 02:48:13  rickg
 * <p> Added makeDirectories method
 * <p>
 * <p> Revision 1.1  2004/04/02 18:44:26  rickg
 * <p> Initial revision
 * <p> </p>
 */

package etomo.util;

import java.io.File;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.SystemProcessException;
import etomo.process.SystemProgram;
import etomo.type.AxisID;
import etomo.type.EtomoVersion;
import etomo.type.ImodVersion;

public class TestUtilites {
  public static final String rcsid = "$Id$";

  private static final String VECTOR_LOCATION = "ImodTests/EtomoTests/vectors/";
  private static final String TESTS_LOCATION_WORKSPACE = "Etomo/tests/";
  private static final String TESTS_LOCATION_CHECKOUT = "imod/"
      + TESTS_LOCATION_WORKSPACE;

  /**
   * Make all of the directories on on the specified path if necessary.  If the
   * path begins with the systems separator then it is an absolute path, if not
   * it is relative to the current directory specified by propertyUserDir from
   * EtomoDirector. 
   * @param newDirectory
   */
  public static void makeDirectories(String propertyUserDir, String newDirectory)
      throws IOException {
    //  Create the test directories
    File directory;
    if (newDirectory.startsWith(File.separator)) {
      directory = new File(newDirectory);
    }
    else {
      directory = new File(propertyUserDir, newDirectory);
    }
    if (!directory.exists()) {
      if (!directory.mkdirs()) {
        throw new IOException("Creating directory: "
            + directory.getAbsolutePath());
      }
    }
  }

  public static File getVector(BaseManager manager, String testRootDirName,
      String testDirName, String vectorName) throws SystemProcessException,
      InvalidParameterException {
    return getVector(manager, testRootDirName, testDirName, vectorName, false);
  }

  public static File getVector(BaseManager manager, String testRootDirName,
      String testDirName, String vectorName, boolean fromTestsDirectory)
      throws SystemProcessException, InvalidParameterException {
    String checkoutLocation;
    String workspaceLocation;
    if (fromTestsDirectory) {
      checkoutLocation = TESTS_LOCATION_CHECKOUT;
      workspaceLocation = TESTS_LOCATION_WORKSPACE;
    }
    else {
      checkoutLocation = VECTOR_LOCATION;
      workspaceLocation = VECTOR_LOCATION;
    }
    //check vector
    if (vectorName.indexOf(File.separator) != -1) {
      throw new InvalidParameterException(
          "vector can not contain path separators");
    }
    File testRootDir = new File(testRootDirName);
    File testDir = new File(testRootDir, testDirName);
    File target = new File(testDir, vectorName);
    //save time by looking for already checked out files in workspace directory
    String homeDirName = EnvironmentVariable.INSTANCE.getValue(manager, null,
        "HOME", AxisID.ONLY);
    if (homeDirName != null && !homeDirName.matches("\\s*+")) {
      File homeDir = new File(EnvironmentVariable.INSTANCE.getValue(manager,
          null, "HOME", AxisID.ONLY));
      if (homeDir.exists() && homeDir.canRead()) {
        File vector = new File(new File(homeDir, "workspace/"
            + workspaceLocation), vectorName);
        //delete target
        if (target.exists() && !target.delete()) {
          throw new SystemProcessException("Cannot delete target: "
              + target.getAbsolutePath());
        }
        if (vector.exists()) {
          //copy vector to target
          String[] copyCommand = new String[3];
          copyCommand[0] = "cp";
          copyCommand[1] = vector.getAbsolutePath();
          copyCommand[2] = testDir.getAbsolutePath();
          SystemProgram copy = new SystemProgram(manager,
              manager == null ? null : manager.getPropertyUserDir(),
              copyCommand, AxisID.ONLY);
          copy.setDebug(true);
          copy.run();
          if (target.exists()) {
            return target;
          }
        }
      }
    }
    return checkoutVector(manager, testRootDir, testDir, target,
        checkoutLocation);
  }

  /**
   * Check out the specified test vector into the specified directory. Note the
   * cvs export cannot handle a full path as an argument to -d.  The directory
   * must reside in the current directory.
   * Setting the working directory just before running cvs
   * @param workingDirName - Name of the directory containing the dirName directory.
   * @param dirName - Directory name with no path.
   * @param vector - File to be added to the dirName directory.
   */
  private static File checkoutVector(BaseManager manager, File testRootDir,
      File testDir, File target, String checkoutLocation)
      throws SystemProcessException, InvalidParameterException {
    System.out.println("target=" + target);
    System.out.println("testDir=" + testDir);
    System.out.println("checkoutLocation=" + checkoutLocation);
    //set working directory
    String originalDirName = EtomoDirector.INSTANCE
        .setCurrentPropertyUserDir(testRootDir.getAbsolutePath());
    //delete existing target
    if (target.exists() && !target.delete()) {
      //unable to delete - reset working directory and throw exception
      EtomoDirector.INSTANCE.setCurrentPropertyUserDir(originalDirName);
      throw new SystemProcessException("Cannot delete target: "
          + target.getAbsolutePath());
    }
    //Check using the version number in ImodVersion.
    EtomoVersion version = EtomoVersion
        .getDefaultInstance(ImodVersion.CURRENT_VERSION);
    String[] cvsCommand = new String[7];
    cvsCommand[0] = "cvs";
    cvsCommand[1] = "export";
    cvsCommand[2] = "-r";
    cvsCommand[3] = "IMOD_" + version.get(0) + "-" + version.get(1);
    cvsCommand[4] = "-d";
    cvsCommand[5] = testDir.getName();
    cvsCommand[6] = checkoutLocation + target.getName();
    SystemProgram cvs = new SystemProgram(manager,
        manager.getPropertyUserDir(), cvsCommand, AxisID.ONLY);
    cvs.setDebug(true);
    cvs.run();
    //make sure that the file system is up to date
    String[] command = new String[2];
    command[0] = "ls";
    command[1] = checkoutLocation;
    SystemProgram systemProgram = new SystemProgram(manager, manager
        .getPropertyUserDir(), command, AxisID.ONLY);
    systemProgram.run();
    if ((cvs.getExitValue() > 0 && cvs.getStdErrorString().indexOf(
        "no such tag") != -1)
        || (!(new File(checkoutLocation + target.getName())).exists()
            && cvs.getStdErrorString() != null && cvs.getStdErrorString()
            .indexOf("has disappeared") != -1)) {
      // NOTE: some version of cvs (1.11.2) have bug that results in a checkout
      // (CVS directory is created) instead of an export when using the -d flag
      // This is a work around to handle that case
      //This also seems to happen with a -r tag.
      //The mysterious "has disappeared" warning turned up, so I need to check
      //for it when the target was not checked out.
      File badDirectory = new File(testDir, "CVS");
      if (badDirectory.exists()) {
        String[] rmCommand = new String[3];
        rmCommand[0] = "rm";
        rmCommand[1] = "-rf";
        rmCommand[2] = badDirectory.getAbsolutePath();
        SystemProgram rm = new SystemProgram(manager, manager
            .getPropertyUserDir(), rmCommand, AxisID.ONLY);
        rm.run();
      }

      //If checking out with the version number fails, then the tag was not
      //created.  Checkout the latest stuff.
      cvsCommand = new String[7];
      cvsCommand[0] = "cvs";
      cvsCommand[1] = "export";
      cvsCommand[2] = "-D";
      cvsCommand[3] = "today";
      cvsCommand[4] = "-d";
      cvsCommand[5] = testDir.getName();
      cvsCommand[6] = checkoutLocation + target.getName();
      cvs = new SystemProgram(manager, manager.getPropertyUserDir(),
          cvsCommand, AxisID.ONLY);
      cvs.setDebug(true);
      cvs.run();
    }
    for (int i = 0; i < cvsCommand.length; i++) {
      System.err.print(cvsCommand[i] + " ");
    }
    System.err.println();
    if (cvs.getExitValue() > 0) {
      //report error
      String message = cvs.getStdErrorString()
          + "\nCVSROOT="
          + EnvironmentVariable.INSTANCE.getValue(manager, manager
              .getPropertyUserDir(), "CVSROOT", AxisID.ONLY)
          + ",manager.getPropertyUserDir()=" + manager.getPropertyUserDir()
          + ",testRootDir=" + testRootDir.getAbsolutePath() + "\ntestDir="
          + testDir.getAbsolutePath() + ",target=" + target.getAbsolutePath()
          + ",working dir=" + System.getProperty("user.dir");
      EtomoDirector.INSTANCE.setCurrentPropertyUserDir(originalDirName);
      throw new SystemProcessException(message);
    }
    // NOTE: some version of cvs (1.11.2) have bug that results in a checkout
    // (CVS directory is created) instead of an export when using the -d flag
    // This is a work around to handle that case
    File badDirectory = new File(testDir, "CVS");
    if (badDirectory.exists()) {
      String[] rmCommand = new String[3];
      rmCommand[0] = "rm";
      rmCommand[1] = "-rf";
      rmCommand[2] = badDirectory.getAbsolutePath();
      SystemProgram rm = new SystemProgram(manager, manager
          .getPropertyUserDir(), rmCommand, AxisID.ONLY);
      rm.run();
    }
    //reset working directory
    EtomoDirector.INSTANCE.setCurrentPropertyUserDir(originalDirName);
    return target;
  }
}