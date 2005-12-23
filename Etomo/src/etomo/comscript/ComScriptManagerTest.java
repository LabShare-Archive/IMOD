/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003</p>
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
* <p> Revision 1.4  2005/11/10 17:58:45  sueh
* <p> bug# 758 Remove unnecessary class EtomoDirectorTestHarness.
* <p>
* <p> Revision 1.3  2005/07/29 00:43:30  sueh
* <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
* <p> because the current manager changes when the user changes the tab.
* <p> Passing the manager where its needed.
* <p>
* <p> Revision 1.2  2004/11/19 22:42:48  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.4.2  2004/10/08 15:45:57  sueh
* <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
* <p> member variables non-static.
* <p>
* <p> Revision 1.1.4.1  2004/09/03 21:08:11  sueh
* <p> bug# 520 getting app mgr from EtomoDirector
* <p>
* <p> Revision 1.1  2004/06/14 23:35:08  rickg
* <p> Bug #383  Initial revision
* <p> </p>
 */
package etomo.comscript;

import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.AxisType;
import junit.framework.TestCase;


public class ComScriptManagerTest extends TestCase {

  // FIXME this needs to really test the method, not just exercise it.
  
  public void testUseTemplate() {
    //  Need an application manger to get the IMOD_DIR environment
    // variable
    BaseManager manager = EtomoDirector.getInstance().getCurrentManager_test();
    System.out.println(EtomoDirector.getInstance().getIMODDirectory().getAbsolutePath());
    ComScriptManager comScriptManager = manager.getComScriptManager();
    try {
      comScriptManager.useTemplate("mtffilter", "datasetName", AxisType.SINGLE_AXIS, AxisID.ONLY);
      comScriptManager.useTemplate("eraser", "datasetName", AxisType.DUAL_AXIS, AxisID.SECOND);
      comScriptManager.useTemplate("volcombine", "datasetName", AxisType.DUAL_AXIS, AxisID.ONLY);
      comScriptManager.useTemplate("solvematch", "datasetName", AxisType.DUAL_AXIS, AxisID.ONLY);
    }
    catch (BadComScriptException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    catch (IOException e) {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

}
