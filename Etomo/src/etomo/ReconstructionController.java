package etomo;

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
* <p> $Log$ </p>
*/
public class ReconstructionController implements Controller {
  public static  final String  rcsid =  "$Id$";
  
  private ApplicationManager manager;
  private MetaData metaData;
  
  ReconstructionController(String paramFileName) {
    metaData = new MetaData();
    manager = new ApplicationManager(paramFileName, metaData);
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
