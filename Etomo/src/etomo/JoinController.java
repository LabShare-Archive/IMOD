package etomo;

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
* <p> $Log$ </p>
*/
public class JoinController implements Controller {
  public static  final String  rcsid =  "$Id$";
  
  private JoinManager manager;
  private JoinMetaData metaData;
  
  JoinController(String paramFileName) {
    metaData = new JoinMetaData();
    manager = new JoinManager(paramFileName, metaData);
  }
  
  public BaseManager getManager() {
    return manager;
  }
  
  public BaseMetaData getMetaData() {
    return metaData;
  }
}
