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
import etomo.type.UITestFieldType;
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
  public static final String rcsid = "$Id$";

  private JTextField textField = new JTextField();
  private JLabel label = null;
  private JPanel fieldPanel = null;
  private JPanel yAxisPanel = null;

  SpacedTextField(String label) {
    //set name
    String name = Utilities.convertLabelToName(label);
    textField.setName(UITestFieldType.TEXT_FIELD.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(textField.getName() + ' '
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

  final void setToolTipText(String text) {
    String tooltip = TooltipFormatter.INSTANCE.format(text);
    textField.setToolTipText(tooltip);
    label.setToolTipText(tooltip);
    fieldPanel.setToolTipText(tooltip);
    yAxisPanel.setToolTipText(tooltip);
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
 * <p> Revision 1.9  2009/01/20 20:29:22  sueh
 * <p> bug# 1102 Changed UITestField to UITestFieldType.
 * <p>
 * <p> Revision 1.8  2008/05/30 22:33:49  sueh
 * <p> bug# 1102 Isolating the etomo.uitest package so it is not need for
 * <p> running EtomoDirector.
 * <p>
 * <p> Revision 1.7  2008/05/30 21:34:07  sueh
 * <p> bug# 1102 Moved uitest classes to etomo.uitest.
 * <p>
 * <p> Revision 1.6  2007/12/26 22:31:54  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.5  2007/09/07 00:28:51  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.4  2007/03/01 01:44:01  sueh
 * <p> bug# 964 Formatting tooltip.
 * <p>
 * <p> Revision 1.3  2006/09/13 23:56:29  sueh
 * <p> bug# 920 Added setText(ConstEtomoNumber)
 * <p>
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
