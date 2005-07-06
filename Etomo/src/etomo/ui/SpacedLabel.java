package etomo.ui;

import java.awt.Container;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
final class SpacedLabel {
  public static  final String  rcsid =  "$Id$";
  
  private JLabel label = null;
  private JPanel labelPanel = null;
  private JPanel yAxisPanel = null;
  
  
  SpacedLabel(String label) {
    label = label.trim();
    this.label = new JLabel(label);
    //panels
    yAxisPanel = new JPanel();
    yAxisPanel.setLayout(new BoxLayout(yAxisPanel, BoxLayout.Y_AXIS));
    labelPanel = new JPanel();
    labelPanel.setLayout(new BoxLayout(labelPanel, BoxLayout.X_AXIS));
    //labelPanel
    labelPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    labelPanel.add(this.label);
    labelPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    //yPanel
    yAxisPanel.add(labelPanel);
    yAxisPanel.add(Box.createRigidArea(FixedDim.x0_y5));
  }
  
  final void setToolTipText(String toolTipText) {
    label.setToolTipText(toolTipText);
    labelPanel.setToolTipText(toolTipText);
    yAxisPanel.setToolTipText(toolTipText);
  }
  
  final Container getContainer() {
    if (yAxisPanel != null) {
      return yAxisPanel;
    }
    return labelPanel;
  }

  final void setVisible(boolean visible) {
    getContainer().setVisible(visible);
  }
  
  final void setAlignmentX(float alignmentX) {
    if (label != null) {
      label.setAlignmentX(alignmentX);
    }
    if (labelPanel != null) {
      labelPanel.setAlignmentX(alignmentX);
    }
    if (yAxisPanel != null) {
      yAxisPanel.setAlignmentX(alignmentX);
    }
  }
}
/**
* <p> $Log$ </p>
*/