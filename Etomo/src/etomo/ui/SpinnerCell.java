package etomo.ui;

import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JFormattedTextField;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.plaf.ColorUIResource;

import etomo.type.EtomoNumber;

/**
 * <p>Description: Table cell with an integer JSpinner</p>
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
class SpinnerCell extends InputCell {
  public static final String rcsid = "$Id$";

  private JSpinner spinner = null;
  private EtomoNumber disabledValue = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
  private EtomoNumber savedValue = new EtomoNumber(EtomoNumber.INTEGER_TYPE);

  SpinnerCell(int min, int max) {
    super();
    spinner = new JSpinner(new SpinnerNumberModel(min, min, max, 1));
    setBackground();
    setFont();
    setForeground();
    spinner.setBorder(BorderFactory.createEtchedBorder());
    getTextField().setHorizontalAlignment(JTextField.LEFT);
  }

  void setEnabled(boolean enabled) {
    super.setEnabled(enabled);
    if (!disabledValue.isNull()) {
      if (enabled) {
        if (disabledValue.equals((Number) spinner.getValue())
            && !savedValue.isNull()) {
          spinner.setValue(savedValue.getNumber());
        }
      }
      else {
        savedValue.set((Number) spinner.getValue());
        spinner.setValue(disabledValue.getNumber());
      }
    }
  }

  void setDisabledValue(int disabledValue) {
    this.disabledValue.set(disabledValue);
  }

  protected final Component getComponent() {
    return spinner;
  }

  private final JFormattedTextField getTextField() {
    return ((JSpinner.DefaultEditor) spinner.getEditor()).getTextField();
  }

  final void setValue(int value) {
    savedValue.set(value);
    if (enabled || disabledValue.isNull()) {
      spinner.setValue(new Integer(value));
    }
  }

  final void setValue(String value) {
    setValue(new EtomoNumber(EtomoNumber.INTEGER_TYPE).set(value).getInteger());
  }

  final int getValue() {
    return ((Integer) spinner.getValue()).intValue();
  }

  protected final void setBackground(ColorUIResource color) {
    getTextField().setBackground(color);
  }

  protected final void setForeground() {
    JFormattedTextField textField = getTextField();
    if (error) {
      textField.setForeground(errorForeground);
      textField.setDisabledTextColor(errorForeground);
    }
    else {
      textField.setForeground(foreground);
      textField.setDisabledTextColor(foreground);
    }
  }
}
/**
 * <p> $Log$ </p>
 */