package etomo.ui;

import java.awt.event.ActionListener;

import javax.swing.JCheckBox;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class NamedCheckBox {
  public static  final String  rcsid =  "$Id$";
  
  private JCheckBox checkBox;
  
  NamedCheckBox(String label) {
    checkBox = new JCheckBox(label);
    checkBox.setName(label);
  }
  
  JCheckBox getContainer() {
    return checkBox;
  }
  
  void addActionListener(ActionListener actionListener) {
    checkBox.addActionListener(actionListener);
  }
  
  void setSelected(boolean selected) {
    checkBox.setSelected(selected);
  }
  
  boolean isSelected() {
    return checkBox.isSelected();
  }
  
  String getActionCommand() {
    return checkBox.getActionCommand();
  }
  
  void setToolTipText(String text) {
    checkBox.setToolTipText(text);
  }
  
  void setEnabled(boolean enabled) {
    checkBox.setEnabled(enabled);
  }
}
/**
* <p> $Log$ </p>
*/
