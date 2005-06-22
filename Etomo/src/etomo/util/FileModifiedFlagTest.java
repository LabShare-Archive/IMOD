package etomo.util;

import java.io.File;
import java.io.IOException;

import etomo.EtomoDirector;

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
  public static  final String  rcsid =  "$Id$";
  
  File testDir;
  File testFile;
  
  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
  }

  /*
   * @see TestCase#tearDown()
   */
  protected void tearDown() throws Exception {
    super.tearDown();
  }

  /**
   * Constructor for FileModifiedFlagTest.
   * @param arg0
   */
  public FileModifiedFlagTest(String arg0) {
    super(arg0);
    testDir = new File(UtilTests.testRoot, "FileModifiedFlag");
    testDir.mkdirs();
    testFile = new File(testDir, "testfile");
  }
  
  public final void testfileModifiedFlag() {
    FileModifiedFlag fileModifiedFlag = new FileModifiedFlag(testFile);
    fileModifiedFlag.selfTestInvariants();
  }
  
  public final void testIsModifiedSinceLastRead() throws IOException {
    FileModifiedFlag fileModifiedFlag = new FileModifiedFlag(testFile);
    testFile.createNewFile();
    //test: returns true when never read
    assertTrue(fileModifiedFlag.isModifiedSinceLastRead());
    //test: returns false when file hasn't changed after setReadingNow() call
    fileModifiedFlag.setReadingNow();
    assertFalse(fileModifiedFlag.isModifiedSinceLastRead());
    //test: return true when file has changed after setReadingNow() call
    try {
      Thread.sleep(10);
    }
    catch (InterruptedException e) {
    }
    System.err.println("testFile="+testFile.getAbsolutePath());
    System.err.println("testFile last modified="+testFile.lastModified());
    EtomoDirector.getInstance().getCurrentManager().touch(testFile);
    try {
      Thread.sleep(1000);
    }
    catch (InterruptedException e) {
    }
    assertTrue(fileModifiedFlag.isModifiedSinceLastRead());
  }
  
  public final void testSetReadingNow() throws IOException {
    FileModifiedFlag fileModifiedFlag = new FileModifiedFlag(testFile);
    testFile.createNewFile();
    long fileLastModified = testFile.lastModified();
    fileModifiedFlag.setReadingNow();
    assertEquals(fileModifiedFlag.getLastModified(), fileLastModified);
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2005/06/21 00:51:40  sueh
* <p> bug# 522 Changed testIsModifiedSinceLastRead() to use touch to change
* <p> the last modified date on the file instead of deleting and adding file.
* <p>
* <p> Revision 1.1  2005/06/20 17:04:51  sueh
* <p> bug# 522
* <p> </p>
*/