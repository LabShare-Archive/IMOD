package etomo.ui;

import java.awt.Color;
import java.awt.event.ActionEvent;

import etomo.ToolsManager;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2010/02/17 04:57:21  sueh
 * <p> bug# 1301 Process panel for the ToolsManager.
 * <p> </p>
 */
public final class ToolsProcessPanel extends AxisProcessPanel {
  public static final String rcsid = "$Id$";

  ToolsProcessPanel(ToolsManager manager) {
    super(AxisID.ONLY, manager,true);
    createProcessControlPanel();
    initializePanels();
  }

  protected void buttonKillAction(ActionEvent event) {
    manager.kill(axisID);
  }

  void showBothAxis() {
    setBackground(Colors.getBackgroundTools());
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
