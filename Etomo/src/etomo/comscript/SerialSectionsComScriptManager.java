package etomo.comscript;

import etomo.SerialSectionsManager;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class SerialSectionsComScriptManager extends BaseComScriptManager {
  public static  final String  rcsid =  "$Id:$";
  
  private final SerialSectionsManager manager;
  
  public SerialSectionsComScriptManager(SerialSectionsManager manager) {
    super(manager);
    this.manager = manager;
  }
}
