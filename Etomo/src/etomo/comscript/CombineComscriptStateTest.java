package etomo.comscript;

import etomo.ApplicationManagerTestHarness;
import etomo.ui.TomogramCombinationDialog;
import junit.framework.TestCase;

/**
 * @author sueh
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class CombineComscriptStateTest extends TestCase {
  ApplicationManagerTestHarness testHarness = null;
  ComScriptManager comScriptManager = null;
  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
    String[] args = {"BB.edf", "--test", "--selfTest"};
    testHarness = 
        ApplicationManagerTestHarness.getApplicationManagerTestHarness(args);
    assertNotNull(testHarness);
    comScriptManager = testHarness.getComScriptManager();
  }

  final public void testStartCommand() {
    //load combine.com
    testHarness.openTomogramCombinationDialog();
    //update combine.com
    CombineComscriptState combineStateOut = 
        testHarness.runUpdateCombineComscriptState(
        CombineComscriptState.MATCHVOL1_INDEX);
    assertNotNull(combineStateOut);
    //reload combine.com into a new instance
    CombineComscriptState combineStateIn = 
        comScriptManager.getCombineComscript();
    //compare instances
    if (!combineStateOut.equals(combineStateIn)) {
      fail(combineStateOut.getNotEqualsReason());
    }
  }
  
  final public void testEndCommand() {
    //load combine.com
    testHarness.openTomogramCombinationDialog();
    //test endCommand
    TomogramCombinationDialog combinationDialog = 
        testHarness.getTomogramCombinationDialog();
    boolean runVolcombine = combinationDialog.isRunVolcombine();
    //change endCommand
    combinationDialog.setRunVolcombine(!runVolcombine);
    //update combine.com
    CombineComscriptState combineStateOut = 
        testHarness.runUpdateCombineComscriptState(
        CombineComscriptState.PATCHCORR_INDEX);
    assertNotNull(combineStateOut);
    //reload combine.com into a new instance
    CombineComscriptState combineStateIn = 
        comScriptManager.getCombineComscript();
    //compare instances
    if (!combineStateOut.equals(combineStateIn)) {
      fail(combineStateOut.getNotEqualsReason());
    }
  }

}
