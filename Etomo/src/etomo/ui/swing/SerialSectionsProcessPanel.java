package etomo.ui.swing;

import etomo.BaseManager;
import etomo.type.AxisID;

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
public final class SerialSectionsProcessPanel extends AxisProcessPanel {
  public static  final String  rcsid =  "$Id:$";
  
  SerialSectionsProcessPanel(BaseManager manager) {
    super(AxisID.ONLY, manager, false);
    createProcessControlPanel();
    initializePanels();
  }
}
