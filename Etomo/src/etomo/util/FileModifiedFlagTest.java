package etomo.util;

import java.io.File;
import java.io.IOException;

import junit.framework.TestCase;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class FileModifiedFlagTest extends TestCase {
  public static final String rcsid = "$Id$";

  File testDir;
  File testFile;

  protected void setUp() throws Exception {
    super.setUp();
    testDir = new File(UtilTests.TEST_ROOT_DIR, "FileModifiedFlag");
    if (!testDir.exists()) {
      testDir.mkdirs();
    }
    testFile = new File(testDir, "testfile");
  }

  public final void testfileModifiedFlag() {
    FileModifiedFlag fileModifiedFlag = new FileModifiedFlag(testFile);
  }

  //TODO
  public final void testIsModifiedSinceLastRead() throws IOException {
  }

  //TODO
  public final void testSetReadingNow() throws IOException {
  }
}
/**
* <p> $Log$
* <p> Revision 1.15  2010/04/28 16:50:46  sueh
* <p> bug# 1344 Removed unnecessary tearDown override.
* <p>
* <p> Revision 1.14  2006/06/14 00:44:42  sueh
* <p> bug# 852 Removed selfTestInvariants() because there isn't much to test.
* <p>
* <p> Revision 1.13  2005/11/10 18:18:13  sueh
* <p> bug# 758 Placed the root test directory in a File object in JUnitTests.  It is
* <p> instanciated once so there won't be a problem if the working directory is
* <p> changed.  Added a root test directory File object to each of the suites,
* <p> which is based on the JUnitTests root test directory.
* <p>
* <p> Revision 1.12  2005/07/01 21:27:26  sueh
* <p> Removed an unnecessary import.
* <p>
* <p> Revision 1.11  2005/06/22 23:41:27  sueh
* <p> bug# 522 New tests are failing - will fix later.  changed
* <p> testIsModifiedSinceLastRead().
* <p>
* <p> Revision 1.10  2005/06/22 23:39:27  sueh
* <p> bug# 522 New tests are failing - will fix later.  changed
* <p> testIsModifiedSinceLastRead().
* <p>
* <p> Revision 1.9  2005/06/22 23:30:58  sueh
* <p> bug# 522 fixing build problems
* <p>
* <p> Revision 1.8  2005/06/22 23:14:36  sueh
* <p> bug# 522 fixing build problems
* <p>
* <p> Revision 1.7  2005/06/22 22:47:46  sueh
* <p> bug# 522 fixing build problems
* <p>
* <p> Revision 1.6  2005/06/22 22:34:12  sueh
* <p> bug# 522 fixing build problems
* <p>
* <p> Revision 1.5  2005/06/22 22:13:46  sueh
* <p> bug# 522 fixing build problems
* <p>
* <p> Revision 1.4  2005/06/22 22:05:42  sueh
* <p> bug# 522 fixing build problems
* <p>
* <p> Revision 1.3  2005/06/22 21:47:39  sueh
* <p> *** empty log message ***
* <p>
* <p> Revision 1.2  2005/06/21 00:51:40  sueh
* <p> bug# 522 Changed testIsModifiedSinceLastRead() to use touch to change
* <p> the last modified date on the file instead of deleting and adding file.
* <p>
* <p> Revision 1.1  2005/06/20 17:04:51  sueh
* <p> bug# 522
* <p> </p>
*/
