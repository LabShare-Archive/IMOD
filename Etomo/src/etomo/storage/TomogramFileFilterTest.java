package etomo.storage;

import java.io.File;

import etomo.EtomoDirector;
import etomo.process.BaseProcessManager;

import junit.framework.TestCase;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/10/29 19:53:53  sueh
 * <p> bug# 1280 Unit tests for TomogramFileFilter.
 * <p> </p>
 */
public class TomogramFileFilterTest extends TestCase {
  private static final File testDir = new File(StorageTests.TEST_ROOT_DIR,
      "TomogramFileFilter");
  private static final TomogramFileFilter tomogramFileFilter = new TomogramFileFilter();

  public void testDirectory() {
    File dir = new File(testDir, "testDirectory");
    BaseProcessManager.mkdir(dir.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    assertTrue("Should accept all directories", tomogramFileFilter.accept(dir));
  }

  public void testDefaultExtensions() {
    File file = new File(testDir, "testDefaultExtensions" + ".rec");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    assertTrue("Should accept all directories", tomogramFileFilter.accept(file));

    file = new File(testDir, "testDefaultExtensions" + ".flip");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    assertTrue("Should accept all directories", tomogramFileFilter.accept(file));

    file = new File(testDir, "testDefaultExtensions" + ".sqz");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    assertTrue("Should accept all directories", tomogramFileFilter.accept(file));

    file = new File(testDir, "testDefaultExtensions" + ".join");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    assertTrue("Should accept all directories", tomogramFileFilter.accept(file));

    file = new File(testDir, "testDefaultExtensions" + ".dummy");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    assertFalse("Should not accept unknown extension", tomogramFileFilter.accept(file));
  }

  public void testNewExtension() {
    File file = new File(testDir, "testNewExtension" + ".dummy");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    tomogramFileFilter.addExtension(file);
    assertTrue("Should accept unknown extension after its been added", tomogramFileFilter
        .accept(file));
  }

  public void testAllowAll() {
    File file = new File(testDir, "testAllowAll");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    assertFalse("Should not accept without an extension", tomogramFileFilter.accept(file));
    tomogramFileFilter.addExtension(file);
    assertTrue("Should accept without an extension once its been added",
        tomogramFileFilter.accept(file));

    file = new File(testDir, "testAllowAll" + ".dummy");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    assertTrue("Should accept all files once a file without an extension has been added",
        tomogramFileFilter.accept(file));
  }

  public void testAddExtension() {
    File file = new File(testDir, "testAddExtension" + ".dummy");
    BaseProcessManager.touch(file.getAbsolutePath(), EtomoDirector.INSTANCE
        .getCurrentManagerForTest());
    tomogramFileFilter.addExtension(file);
    tomogramFileFilter.addExtension(file);
    assertEquals("Should only add new extension once", tomogramFileFilter
        .getExtraExtensionListSize(), 1);
  }
}
