package etomo.ui;

import java.awt.Color;
import java.awt.event.ActionEvent;

import etomo.BaseManager;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
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
public class PeetProcessPanel extends AxisProcessPanel {
  public static  final String  rcsid =  "$Id$";
  
  protected void buttonKillAction(ActionEvent event) {
    manager.kill(axisID);
  }
  
  PeetProcessPanel(BaseManager manager) {
    super(AxisID.ONLY, manager);
    createProcessControlPanel();
    initializePanels();
  }
  
  void showBothAxis() {
    setBackground(Colors.getBackgroundPeet());
  }
  
  private void setBackground(Color color) {
    panelRoot.setBackground(color);
    outerStatusPanel.setBackground(color);
    innerStatusPanel.setBackground(color);
    parallelStatusPanel.setBackground(color);
    panelDialog.setBackground(color);
    panelProcessSelect.setBackground(color);
    //axisButtonPanel.setBackground(color);
    progressPanel.setBackground(color);
  }
}
