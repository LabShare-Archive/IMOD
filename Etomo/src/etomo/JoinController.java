package etomo;

import etomo.type.AxisID;
import etomo.type.BaseMetaData;
import etomo.type.JoinMetaData;

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
* <p> Revision 1.1  2005/01/21 22:13:39  sueh
* <p> bug# 509 bug# 591  Implements Controller.  Manages a group of controller
* <p> classes, including JoinManager and JoinMetaData.
* <p> </p>
*/
public class JoinController implements Controller {
  public static  final String  rcsid =  "$Id$";
  
  private JoinManager manager;
  private JoinMetaData metaData;
  
  JoinController(String paramFileName, AxisID axisID) {
    metaData = new JoinMetaData();
    manager = new JoinManager(paramFileName, metaData, axisID);
  }
  
  public BaseManager getManager() {
    return manager;
  }
  
  public BaseMetaData getMetaData() {
    return metaData;
  }
}
