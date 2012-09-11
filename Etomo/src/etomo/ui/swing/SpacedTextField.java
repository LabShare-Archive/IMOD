package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.KeyListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import etomo.EtomoDirector;
import etomo.logic.FieldValidator;
import etomo.storage.autodoc.AutodocTokenizer;
import etomo.type.ConstEtomoNumber;
import etomo.type.UITestFieldType;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
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
final class SpacedTextField implements UIComponent {
  public static final String rcsid = "$Id$";

  private final JTextField textField = new JTextField();
  private final JPanel fieldPanel = new JPanel();
  private final JPanel yAxisPanel = new JPanel();

  private final JLabel label;
  private final FieldType fieldType;

  SpacedTextField(final FieldType fieldType, String label) {
    this.fieldType = fieldType;
    // set name
    String name = Utilities.convertLabelToName(label);
    textField.setName(UITestFieldType.TEXT_FIELD.toString()
        + AutodocTokenizer.SEPARATOR_CHAR + name);
    if (EtomoDirector.INSTANCE.getArguments().isPrintNames()) {
      System.out.println(textField.getName() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER
          + ' ');
    }
    label = label.trim();
    this.label = new JLabel(label);
    // panels
    yAxisPanel.setLayout(new BoxLayout(yAxisPanel, BoxLayout.Y_AXIS));
    fieldPanel.setLayout(new BoxLayout(fieldPanel, BoxLayout.X_AXIS));
    // fieldPanel
    fieldPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    fieldPanel.add(this.label);
    fieldPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    fieldPanel.add(textField);
    fieldPanel.add(Box.createRigidArea(FixedDim.x5_y0));
    // yPanel
    yAxisPanel.add(fieldPanel);
    yAxisPanel.add(Box.createRigidArea(FixedDim.x0_y5));
  }

  void setToolTipText(String text) {
    String tooltip = TooltipFormatter.INSTANCE.format(text);
    textField.setToolTipText(tooltip);
    label.setToolTipText(tooltip);
    fieldPanel.setToolTipText(tooltip);
    yAxisPanel.setToolTipText(tooltip);
  }

  Container getContainer() {
    if (yAxisPanel != null) {
      return yAxisPanel;
    }
    return fieldPanel;
  }

  public Component getComponent() {
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

  final void setText(double value) {
    textField.setText(String.valueOf(value));
  }

  final void setText(long value) {
    textField.setText(String.valueOf(value));
  }

  final String getLabel() {
    return label.getText();
  }

  final String getText(final boolean doValidation) throws FieldValidationFailedException {
    String text = textField.getText();
    if (doValidation && textField.isEnabled() && textField.isVisible()
        && fieldType.validationType.canValidate) {
      text = FieldValidator.validateText(text, fieldType, this, getQuotedLabel());
    }
    return text;
  }

  String getQuotedLabel() {
    return Utilities.quoteLabel(label.getText());
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.10  2009/11/20 17:36:01  sueh
 * <p> bug# 1282 Added prefixes to all of the field names, so that the fields that
 * <p> are actually abstract buttons (radio buttons, etc) won't be activated by a
 * <p> "bn." field command.
 * <p>
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
