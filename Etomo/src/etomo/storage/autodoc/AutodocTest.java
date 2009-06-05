package etomo.storage.autodoc;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.process.SystemProcessException;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.util.InvalidParameterException;
import etomo.util.TestUtilites;
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
 */
public final class AutodocTest extends TestCase {
  public static final String rcsid = "$Id$";

  private static final String TEST_DIR_NAME = "Autodoc";

  private BaseManager manager;

  public AutodocTest(String test) {
    super(test);
  }

  public void setUp() throws Exception {
    super.setUp();
    File testDir = new File(AutodocTests.TEST_ROOT_DIR, TEST_DIR_NAME);
    testDir.mkdirs();
    manager = (BaseManager) EtomoDirector.INSTANCE.getCurrentManagerForTest();
    Autodoc.resetAbsoluteDir();
  }

  //Vectors

  public void testSimple() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    if (Utilities.isWindowsOS()) {
      return;
    }
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "simple.adoc"), manager.getManagerKey());
    //only use with getTestInstance
    //autodoc.runInternalTest(AutodocFactory.InternalTestType.PARSER,false,false);
    //only use with getInstance
    //autodoc.printStoredData();
    assertTrue(autodoc != null && !autodoc.isError());
  }

  //Standard autodocs

  public void testBeadtrack() throws FileNotFoundException, IOException,
      LogFile.LockException {
    //TEMP
    if (Utilities.isWindowsOS()) {
      return;
    }
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.BEADTRACK, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testCcderaser() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.CCDERASER, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testCombineFft() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.COMBINE_FFT, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testCorrSearch3d() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.CORR_SEARCH_3D, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testCtfPhaseFlip() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.CTF_PHASE_FLIP, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testCtfPlotter() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.CTF_PLOTTER, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testFlattenWarp() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.FLATTEN_WARP, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testWarpVol() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.WARP_VOL, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testDensmatch() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.DENS_MATCH, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testMtfFilter() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.MTF_FILTER, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testNewstack() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.NEWSTACK, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testSolvematch() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.SOLVEMATCH, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testTiltalign() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.TILTALIGN, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testTiltxcorr() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.TILTXCORR, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  public void testXfjointomo() throws FileNotFoundException, IOException,
      LogFile.LockException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
        AutodocFactory.XFJOINTOMO, AxisID.ONLY, manager.getManagerKey());
    assertFalse(autodoc.isError());
  }

  //cpu adoc

  public void testCpu() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    if (Utilities.isWindowsOS()) {
      return;
    }
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "cpu.adoc"), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  //matlab param

  public void testMatlabParamFile() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    if (Utilities.isWindowsOS()) {
      return;
    }
    ReadOnlyAutodoc autodoc = AutodocFactory.getMatlabInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "master.prm"), manager.getManagerKey());
    //only use with getTestInstance
    //autodoc.runInternalTest(AutodocFactory.INSTANCE.InternalTestType.PARSER,false,false);
    //only use with getInstance
    //autodoc.printStoredData();
    assertTrue(autodoc != null && !autodoc.isError());
  }

  //uitest autodocs

  public void testUitest() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "uitest.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testTests() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "tests.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  //Reconstruction autodocs

  public void testRecon() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "recon.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testSetupRecon() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "setup-recon.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testPreProc() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "pre-proc.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testCoarseAlign() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "coarse-align.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testFidModel() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "fid-model.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testFineAlign() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "fine-align.adoc", true), manager.getManagerKey());
    //only use with getTestInstance
    //autodoc.runInternalTest(AutodocFactory.InternalTestType.PARSER,false,false);
    //only use with getInstance
    //autodoc.printStoredData();
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testTomoPos() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "tomo-pos.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testStack() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "stack.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testTomoGen() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "tomo-gen.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testCombine() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "combine.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testPostProc() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "post-proc.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testCleanUp() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "clean-up.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  //Join autodocs

  public void testJoin() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "join.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testJoinSetup() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "join-setup.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testJoinAlign() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "join-align.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testJoinModel() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "join-model.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }

  public void testJoinRejoin() throws LogFile.LockException, IOException,
      SystemProcessException, InvalidParameterException {
    ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(TestUtilites
        .getVector(manager, AutodocTests.TEST_ROOT_DIR.getAbsolutePath(),
            TEST_DIR_NAME, "join-rejoin.adoc", true), manager.getManagerKey());
    assertTrue(autodoc != null && !autodoc.isError());
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.23  2009/03/17 00:46:02  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.22  2009/02/05 17:38:38  sueh
 * <p> bug# 1102 Removed tests on autodocs that had been deleted from the tests directory.  Added the rest of the tests on autodocs supported by Etomo.
 * <p>
 * <p> Revision 1.21  2009/02/04 23:30:00  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.20  2008/12/15 23:02:30  sueh
 * <p> bug# 1161 Made EtomoDirector.getCurrentManager private.  Added a
 * <p> public test version for public access.
 * <p>
 * <p> Revision 1.19  2008/01/31 20:25:07  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.18  2007/12/14 18:31:06  sueh
 * <p> file problems on windows
 * <p>
 * <p> Revision 1.17  2007/09/07 00:24:48  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.16  2007/08/01 22:44:50  sueh
 * <p> bug# 985 Removed unnecessary prints.
 * <p>
 * <p> Revision 1.15  2007/03/21 18:15:58  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.14  2007/03/13 19:41:52  sueh
 * <p> bug# 964 Got the test autodoc tests working.
 * <p>
 * <p> Revision 1.13  2007/03/09 22:05:20  sueh
 * <p> bug# 964 Hiding test that aren't working temporarily
 * <p>
 * <p> Revision 1.12  2007/03/08 21:55:59  sueh
 * <p> bug# 964 Testing directly from the IMOD/Etomo/tests directory.  Adding more
 * <p> tests of .adoc files.
 * <p>
 * <p> Revision 1.11  2007/03/07 21:06:41  sueh
 * <p> bug# 964 Fixed printing.  Made internal tests runnable from unit tests.
 * <p>
 * <p> Revision 1.10  2007/03/05 21:28:55  sueh
 * <p> bug# 964 Added tests for cpu.adoc and master.prm.
 * <p>
 * <p> Revision 1.9  2007/03/01 01:19:05  sueh
 * <p> bug# 964 Added LogFile to PrimativeTokenizer.
 * <p>
 * <p> Revision 1.8  2006/11/18 01:16:36  sueh
 * <p> bug# 956 Temporarily not running problem tests on Windows.
 * <p>
 * <p> Revision 1.7  2006/11/16 23:42:46  sueh
 * <p> bug# 872 Set autodoc test dir to null to avoid changes made by previous test
 * <p> classes.
 * <p>
 * <p> Revision 1.6  2006/06/15 18:46:12  sueh
 * <p> bug# 876 Removed unnecessary commented out functions.
 * <p>
 * <p> Revision 1.5  2006/06/15 17:55:14  sueh
 * <p> bug# 876 Remove test against cpu.adoc because cpu.adoc is optional.
 * <p>
 * <p> Revision 1.4  2006/06/15 16:19:02  sueh
 * <p> bug# 876 testCpu():  cpu.adoc is optional so catch FileNotFoundException.
 * <p>
 * <p> Revision 1.3  2006/06/14 16:26:04  sueh
 * <p> bug# 852 Fixed problem with tests
 * <p>
 * <p> Revision 1.2  2006/06/14 00:31:25  sueh
 * <p> bug# 852 Added a test for setup-recon.adoc.
 * <p>
 * <p> Revision 1.1  2006/06/14 00:23:02  sueh
 * <p> bug# 852 Tests for Autodoc.  Parses autodocs and then checks the parser for an
 * <p> error.
 * <p> </p>
 */
