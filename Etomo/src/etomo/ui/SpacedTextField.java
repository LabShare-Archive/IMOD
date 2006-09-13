package etomo.ui;

import java.awt.Container;
import java.awt.event.KeyListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import etomo.EtomoDirector;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.ConstEtomoNumber;
import etomo.type.UITestField;
import etomo.util.Utilities;

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
final class SpacedTextField {
  public static  final String  rcsid =  "$Id$";
  
  private JTextField textField = new JTextField();
  private JLabel label = null;
  private JPanel fieldPanel = null;
  private JPanel yAxisPanel = null;
  
  
  SpacedTextField(String label) {
    //set name
    String name = Utilities.convertLabelToName(label);
    textField.setName(name);
    if (EtomoDirector.getInstance().isPrintNames()) {
      System.out.println(UITestField.TEXT_FIELD.toString()
          + AutodocTokenizer.SEPARATOR_CHAR + name + ' '
          + AutodocTokenizer.DEFAULT_DELIMITER + ' ');
    }
    label = label.trim();
    this.label = new JLabel(label);
    //panels
    yAxisPanel = new JPanel();
    yAxisPanel.setLayout(new BoxLayout(yAxisPanel, BoxLayout.Y_AXIS));
    fieldPanel = new JPanel();
    fieldPanel.setLayout(new BoxLayout(fieldPanel, BoxLayout.X_AXIS));
    //fieldPanel
    fieldPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    fieldPanel.add(this.label);
    fieldPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    fieldPanel.add(textField);
    fieldPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    //yPanel
    yAxisPanel.add(fieldPanel);
    yAxisPanel.add(Box.createRigidArea(FixedDim.x0_y5));
  }
  
  final void setToolTipText(String toolTipText) {
    textField.setToolTipText(toolTipText);
    label.setToolTipText(toolTipText);
    fieldPanel.setToolTipText(toolTipText);
    yAxisPanel.setToolTipText(toolTipText);
  }
  
  final Container getContainer() {
    if (yAxisPanel != null) {
      return yAxisPanel;
    }
    return fieldPanel;
  }
  
  void setText(ConstEtomoNumber number) {
    textField.setText(number.toString());
  }
  
  final void setText(String text) {
    textField.setText(text);
  }

  final void setText(int value) {
    textField.setText(String.valueOf(value));
  }

  final void setText(float value) {
    textField.setText(String.valueOf(value));
  }
  
  final void setText(long value) {
    textField.setText(String.valueOf(value));
  }
  
  final void setText(double value) {
    textField.setText(String.valueOf(value));
  }
  
  final String getLabel() {
    return label.getText();
  }

  final String getText() {
    return textField.getText();
  }
  
  final void setVisible(boolean visible) {
    getContainer().setVisible(visible);
  }
  
  final void setAlignmentX(float alignmentX) {
    textField.setAlignmentX(alignmentX);
    if (label != null) {
      label.setAlignmentX(alignmentX);
    }
    if (fieldPanel != null) {
      fieldPanel.setAlignmentX(alignmentX);
    }
    if (yAxisPanel != null) {
      yAxisPanel.setAlignmentX(alignmentX);
    }
  }
  
  public void addKeyListener(KeyListener listener) {
    textField.addKeyListener(listener);
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2006/04/25 19:21:25  sueh
* <p> bug# 787 Named the text field.
* <p>
* <p> Revision 1.1  2005/07/06 23:50:12  sueh
* <p> bug# 437 Class to encapsulate rigid areas within a labeled text field.
* <p> Uses two panels to hold the label, text field, and x and y rigid areas.
* <p> Important for basic displays where there are a lot of advanced fields and
* <p> many rigid areas between them.
* <p> </p>
*/