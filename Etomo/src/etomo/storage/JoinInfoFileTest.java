package etomo.storage;

import java.io.File;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
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
  
  private static final File testDir = new File(StorageTests.TEST_ROOT_DIR,
  "JoinInfoFile");

  public void testGetInverted() throws LogFile.LockException,IOException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    testDir.mkdirs();
    BaseManager manager = EtomoDirector.INSTANCE.getCurrentManagerForTest();
    LogFile infoFile = LogFile.getInstance(testDir.getAbsolutePath(),
        DatasetFiles.getJoinInfoName(manager));
    infoFile.delete();
    JoinInfoFile test =  JoinInfoFile.getTestInstance(infoFile);
    assertNull("Should return null when there is no file", test
        .getInverted(0));
    EtomoDirector.INSTANCE.getCurrentManagerForTest().touch(
        infoFile.getAbsolutePath());
    try {
      Thread.sleep(500);
    }
    catch (InterruptedException e) {
    }
    assertNull("Should return null when the file is empty", test
        .getInverted(0));
    LogFile.WriterId writerId = infoFile.openWriter();
    infoFile.newLine(writerId);
    assertNull("Should return null when the second line doesn't exist",
        test.getInverted(0));
    infoFile.write("0 1 0", writerId);
    infoFile.closeWriter(writerId);
    assertEquals("First number should be 0", test.getInverted(0)
        .getInt(), 0);
    assertEquals("Second number should be 1", test.getInverted(1)
        .getInt(), 1);
    assertEquals("Third number should be 0", test.getInverted(2)
        .getInt(), 0);
    assertNull("Should return null for an index error", test
        .getInverted(4));
  }
}
