package etomo.ui;

import java.awt.Color;
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
    super(AxisID.ONLY, manager,true);
    createProcessControlPanel();
    initializePanels();
  }
  
  protected void buttonKillAction(ActionEvent event) {
    manager.kill(axisID);
  }
  
  void showBothAxis() {
    setBackground(Colors.getBackgroundParallel());
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
  
  protected void createProcessControlPanel() {
    super.createProcessControlPanel();
    showBothAxis();
  }
}
/**
* <p> $Log$
* <p> Revision 1.3  2006/04/07 23:32:39  sueh
* <p> bug# 846 Changing the background colors for java 1.5.
* <p>
* <p> Revision 1.2  2006/04/06 23:34:45  sueh
* <p> bug# 844 Added a color for the generic parallel processing window.
* <p>
* <p> Revision 1.1  2006/03/20 18:05:57  sueh
* <p> bug# 835 Added an AxisProcessPanel for ParallelManager.
* <p> </p>
*/
