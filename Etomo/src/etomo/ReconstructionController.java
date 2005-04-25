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
  
  public BaseMetaData getMetaData() {
    return metaData;
  }
  
  MetaData getReconstructionMetaData() {
    return metaData;
  }
}
