package etomo.storage;

import java.io.File;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.BaseProcessManager;
import etomo.type.AxisID;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;
import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.9  2010/02/17 04:49:31  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.8  2009/10/23 22:24:33  sueh
 * <p> bug# 1275 No default manager.
 * <p>
 * <p> Revision 1.7  2009/03/17 00:44:42  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.6  2009/02/04 23:29:40  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.5  2008/12/15 23:02:11  sueh
 * <p> bug# 1161 Made EtomoDirector.getCurrentManager private.  Added a
 * <p> public test version for public access.
 * <p>
 * <p> Revision 1.4  2008/01/31 20:22:05  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.3  2007/09/07 00:19:52  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.2  2006/11/18 01:15:53  sueh
 * <p> bug# 956 Temporarily not running problem bugs on Windows.
 * <p>
 * <p> Revision 1.1  2006/10/16 22:45:21  sueh
 * <p> bug# 919  File to test JoinInfoFile.
 * <p> </p>
 */
public class JoinInfoFileTest extends TestCase {
  public static final String rcsid = "$Id$";

  private static final File testDir = new File(StorageTests.TEST_ROOT_DIR, "JoinInfoFile");

  public void testGetInverted() throws LogFile.LockException, IOException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    testDir.mkdirs();
    EtomoDirector.INSTANCE.openJoin(true, AxisID.ONLY);
    BaseManager manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();
    LogFile infoFile = LogFile.getInstance(testDir.getAbsolutePath(), DatasetFiles
        .getJoinInfoName(manager));
    infoFile.delete();
    JoinInfoFile test = JoinInfoFile.getTestInstance(infoFile);
    assertNull("Should return null when there is no file", test.getInverted(manager, 0));
    BaseProcessManager.touch(infoFile.getAbsolutePath(), manager);
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    assertNull("Should return null when the file is empty", test.getInverted(manager, 0));
    LogFile.WriterId writerId = infoFile.openWriter();
    infoFile.newLine(writerId);
    assertNull("Should return null when the second line doesn't exist", test.getInverted(
        manager, 0));
    infoFile.write("0 1 0", writerId);
    infoFile.closeWriter(writerId);
    assertEquals("First number should be 0", test.getInverted(manager, 0).getInt(), 0);
    assertEquals("Second number should be 1", test.getInverted(manager, 1).getInt(), 1);
    assertEquals("Third number should be 0", test.getInverted(manager, 2).getInt(), 0);
    assertNull("Should return null for an index error", test.getInverted(manager, 4));
  }
}
