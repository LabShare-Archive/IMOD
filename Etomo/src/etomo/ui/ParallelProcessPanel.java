package etomo.ui;

import java.awt.event.ActionEvent;

import etomo.ParallelManager;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public final class ParallelProcessPanel extends AxisProcessPanel {
  public static  final String  rcsid =  "$Id$";
  
  ParallelProcessPanel(ParallelManager manager) {
    super(AxisID.ONLY, manager);
    createProcessControlPanel();
    initializePanels();
  }
  
  protected void buttonKillAction(ActionEvent event) {
    manager.kill(axisID);
  }
  
  void showBothAxis() {
  }
}
/**
* <p> $Log$ </p>
*/
