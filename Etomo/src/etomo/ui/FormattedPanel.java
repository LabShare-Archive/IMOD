package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.LayoutManager;

import javax.swing.AbstractButton;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.border.Border;

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
* <p> $Log$
* <p> Revision 1.1.2.1  2004/10/13 23:08:10  sueh
* <p> bug# 520 A panel which sizes its buttons and can set alignment for its
* <p> components.
* <p> </p>
*/
public class FormattedPanel {
  public static  final String  rcsid =  "$Id$";
  
  JPanel panel;
  
  private static final Dimension multiLineButtonDim = UIParameters.getButtonDimension();
  
  private float alignmentX = Float.NaN;
  
  FormattedPanel() {
    panel = new JPanel();
  }
  
  public Component add(Component comp) {
    return panel.add(comp);
  }
  
  public Component add(JComponent comp) {
    if (!Float.isNaN(alignmentX)) {
      comp.setAlignmentX(alignmentX);
    }
    return panel.add(comp);
  }
  
  Component add(SpacedPanel spacedPanel) {
    return panel.add(spacedPanel.getContainer());
  }
  
  Component add(DoubleSpacedPanel doubleSpacedPanel) {
    return panel.add(doubleSpacedPanel.getContainer());
  }
  
  Component add(LabeledTextField field) {
    if (!Float.isNaN(alignmentX)) {
      field.setAlignmentX(alignmentX);
    }
    return panel.add(field.getContainer());
  }
  
  Component add(LabeledSpinner spinner) {
    if (!Float.isNaN(alignmentX)) {
      spinner.setAlignmentX(alignmentX);
    }
    return panel.add(spinner.getContainer());
  }

  
  Component addMultiLineButton(AbstractButton button) {
    button.setPreferredSize(multiLineButtonDim);
    button.setMaximumSize(multiLineButtonDim);
    button.setMinimumSize(multiLineButtonDim);
    if (!Float.isNaN(alignmentX)) {
      button.setAlignmentX(alignmentX);
    }
    return panel.add(button);
  }
  
  void setComponentAlignmentX(float alignmentX) {
    this.alignmentX = alignmentX;
  }
  
  void resetComponentAlignmentX() {
    alignmentX = Float.NaN;
  }
  
  Container getContainer() {
    return panel;
  }
  
  public void removeAll() {
    panel.removeAll();
  }
  
  public void setLayout(LayoutManager mgr) {
    panel.setLayout(mgr);
  }
  
  public void setBorder(Border border) {
    panel.setBorder(border);
  }
}
