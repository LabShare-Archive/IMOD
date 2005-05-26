package etomo;

import etomo.type.AxisID;
import etomo.type.BaseMetaData;
import etomo.type.MetaData;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
* <p> Revision 1.2  2005/04/25 20:34:58  sueh
* <p> bug# 615 Passing the axis where the command originated to the message
* <p> functions so that the message will be popped up in the correct window.
* <p> This requires adding AxisID to many objects.
* <p>
* <p> Revision 1.1  2005/01/21 22:17:36  sueh
* <p> bug# 509 bug# 591  Implements Controller.  Manages a group of controller
* <p> classes, including ApplicationManager and MetaData.
* <p> </p>
*/
public class ReconstructionController implements Controller {
  public static  final String  rcsid =  "$Id$";
  
  private ApplicationManager manager;
  private MetaData metaData;
  
  ReconstructionController(String paramFileName, AxisID axisID) {
    metaData = new MetaData();
    manager = new ApplicationManager(paramFileName, metaData, axisID);
  }
  
  public BaseManager getManager() {
    return manager;
  }
  // RJG: Changed to return meta data from manager instead of unset
  // default metaData object
  public BaseMetaData getMetaData() {
    return manager.getMetaData();
  }
  
  MetaData getReconstructionMetaData() {
    return metaData;
  }
}
