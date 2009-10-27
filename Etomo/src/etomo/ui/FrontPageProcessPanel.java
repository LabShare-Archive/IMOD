package etomo.ui;

import java.awt.event.ActionEvent;

import etomo.FrontPageManager;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2008</p>
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
final class FrontPageProcessPanel  extends AxisProcessPanel{
  public static  final String  rcsid =  "$Id$";
  
  FrontPageProcessPanel(FrontPageManager manager) {
    super(AxisID.ONLY, manager);
    initializePanels();
  }
  
  protected void buttonKillAction(ActionEvent event) {
    manager.kill(axisID);
  }
  
  void showBothAxis() {
  }
  
  protected void createProcessControlPanel() {
    showBothAxis();
  }
}
