package etomo.comscript;

import etomo.ApplicationManagerTestHarness;
import etomo.ui.TomogramCombinationDialog;
import junit.framework.TestCase;

/**
* <p>Description: Uses functions in ApplicationManager and ComScriptManager to 
* test the saving, changing, and loading of start and end commmands.</p>
*
* <p>Copyright: Copyright 2004 </p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $Author$
*
* @version $Revision$
*
* <p> $Log$
* <p> Revision 1.2  2004/08/20 21:37:14  sueh
* <p> bug# 508 added log
* <p> </p>
*/
public class CombineComscriptStateTest extends TestCase {
  public static final String rcsid = "$$Id$$";
  ApplicationManagerTestHarness testHarness = null;
  ComScriptManager comScriptManager = null;
  /*
   * @see TestCase#setUp()
   */
  protected void setUp() throws Exception {
    super.setUp();
    String[] args = 
        {"/scratch/bebop/sueh/jUnit/BB.edf", "--test", "--selfTest"};
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
