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
* <p> $Log$ </p>
 */
package etomo.comscript;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.AxisType;
import junit.framework.TestCase;


public class ComScriptManagerTest extends TestCase {

  // FIXME this needs to really test the method, not just exercise it.
  
  public void testUseTemplate() {
    //  Need an application manger to get the IMOD_DIR environment
    // variable
    String[] args = {"--test"};
    ApplicationManager appManager = new ApplicationManager(args);
    System.out.println(ApplicationManager.getIMODDirectory().getAbsolutePath());
    ComScriptManager comScriptManager = appManager.getComScriptManager();
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
