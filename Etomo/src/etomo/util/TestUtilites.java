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
 * <p> Revision 1.26  2011/02/22 21:54:38  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.25  2010/05/21 21:06:43  sueh
 * <p> bug# 1362 Rewrote to get test data from a directory under Etomo which
 * <p> would be as up to date as the checked out Etomo directory would be.
 * <p>
 * <p> Revision 1.24  2010/02/17 05:05:58  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up
 * <p> messages.
 * <p>
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

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.SystemProcessException;
import etomo.process.SystemProgram;
import etomo.type.AxisID;

public final class TestUtilites {
  public static final String rcsid = "$Id$";

  public static final TestUtilites INSTANCE = new TestUtilites();

  private File unitTestData = null;
  private boolean unitTestDataSet = false;

  private TestUtilites() {
  }

  /**
   * Attempts to set unitTestData, if the attempt hasn't been made before.
   * Synchronized.
   */
  private void initUnitTestData() {
    if (unitTestDataSet) {
      return;
    }
    synchronized (this) {
      if (unitTestDataSet) {
        return;
      }
      unitTestDataSet = true;
      unitTestData = new File("unitTestData");
      if (!unitTestData.exists() || !unitTestData.isDirectory()) {
        BaseManager manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();
        // If IMOD_UNIT_TEST_DATA is not set, assume that this is the build unit test.
        // In that case unitTestData should be located here:
        // $IMOD_DIR/../Etomo/unitTestData.
        String imodDirName = EnvironmentVariable.INSTANCE.getValue(manager,
            manager == null ? null : manager.getPropertyUserDir(), "IMOD_DIR",
            AxisID.ONLY);
        if (imodDirName != null && !imodDirName.matches("\\s*")) {
          unitTestData = new File(
              new File(new File(imodDirName).getParentFile(), "Etomo"), "unitTestData");
        }
      }
    }
  }

  public File getUnitTestData() {
    initUnitTestData();
    return unitTestData;
  }

  /**
   * Deletes the existing file in testDir.  Copies a new one from unitTestData.
   * Returns the copied file if it exists, otherwise returns null.
   * @param testRootAbsolutePath
   * @param testDirName
   * @param testFileName
   * @return
   * @throws SystemProcessException
   */
  public File copyTestFile(String testRootAbsolutePath, String testDirName,
      String testFileName) throws SystemProcessException {
    initUnitTestData();
    File unitTestDataFile = new File(unitTestData, testFileName);
    File testDir = new File(testRootAbsolutePath, testDirName);
    File testDirFile = new File(testDir, testFileName);
    if (unitTestDataFile.exists()) {
      // delete existing test dir file
      if (testDirFile.exists() && !testDirFile.delete()) {
        throw new SystemProcessException("Cannot delete testDirFile: "
            + testDirFile.getAbsolutePath());
      }
      // copy data file from unitTestData to test directory
      String[] copyCommand = new String[4];
      copyCommand[0] = "python";
      copyCommand[1] = BaseManager.getIMODBinPath() + "b3dcopy";
      copyCommand[2] = unitTestDataFile.getAbsolutePath();
      copyCommand[3] = testDir.getAbsolutePath();
      BaseManager manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();
      SystemProgram copy = new SystemProgram(manager, manager == null ? null
          : manager.getPropertyUserDir(), copyCommand, AxisID.ONLY);
      copy.run();
      return testDirFile;
    }
    return null;
  }
}