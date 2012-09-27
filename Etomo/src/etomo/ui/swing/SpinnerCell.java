package etomo.ui.swing;

import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JFormattedTextField;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ColorUIResource;

import etomo.type.EtomoNumber;
import etomo.type.UITestFieldType;

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

  private final EtomoNumber disabledValue;
  private final EtomoNumber savedValue;
  private JSpinner spinner = null;
  private final EtomoNumber.Type type;
  private boolean enabled = true;

  public String toString() {
    return getTextField().getText();
  }

  static SpinnerCell getIntInstance(int min, int max) {
    return new SpinnerCell(min, max);
  }

  /**
   * The buttons should still work
   */
  void setEditable(boolean editable) {
    getTextField().setEditable(editable);
  }

  /**
   * disable - the buttons shouldn't work
   */
  public void setEnabled(boolean enabled) {
    this.enabled = enabled;
    getComponent().setEnabled(enabled);
    if (!disabledValue.isNull()) {
      if (enabled) {
        if (disabledValue.equals((Number) spinner.getValue()) && !savedValue.isNull()) {
          spinner.setValue(savedValue.getNumber());
        }
      }
      else {
        savedValue.set((Number) spinner.getValue());
        if (savedValue.equals(0)) {
          savedValue.set(1);
        }
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

  final void setValue(int value) {
    setValue(new EtomoNumber(type).set(value).getNumber());
  }

  final void setValue(String value) {
    setValue(new EtomoNumber(type).set(value).getNumber());
  }

  UITestFieldType getFieldType() {
    return UITestFieldType.SPINNER;
  }

  final int getIntValue() {
    return ((Integer) spinner.getValue()).intValue();
  }

  final String getStringValue() {
    if (type == EtomoNumber.Type.INTEGER) {
      return String.valueOf(getIntValue());
    }
    return null;
  }

  final int getWidth() {
    return spinner.getWidth();
  }

  final void addChangeListener(ChangeListener changeListener) {
    spinner.addChangeListener(changeListener);
  }

  final void removeChangeListener(ChangeListener changeListener) {
    spinner.removeChangeListener(changeListener);
  }

  void setToolTipText(String text) {
    String tooltip = TooltipFormatter.INSTANCE.format(text);
    spinner.setToolTipText(tooltip);
    getTextField().setToolTipText(tooltip);
  }

  /**
   * This probably doesn't work.  Should use something like
   * UIUtilities.highlightJTextComponents.  Will need to control the background
   * color more carefully.
   */
  protected final void setBackground(ColorUIResource color) {
    getTextField().setBackground(color);
  }

  protected final void setForeground() {
    JFormattedTextField textField = getTextField();
    textField.setForeground(Colors.CELL_FOREGROUND);
    textField.setDisabledTextColor(Colors.CELL_FOREGROUND);
  }

  private SpinnerCell(int min, int max) {
    super();
    type = EtomoNumber.Type.INTEGER;
    disabledValue = new EtomoNumber();
    savedValue = new EtomoNumber();
    spinner = new JSpinner(new SpinnerNumberModel(min, min, max, 1));
    spinner.setBorder(BorderFactory.createEtchedBorder());
    getTextField().setHorizontalAlignment(JTextField.LEFT);
    setBackground();
    setForeground();
    setFont();
  }

  private final JFormattedTextField getTextField() {
    return ((JSpinner.DefaultEditor) spinner.getEditor()).getTextField();
  }

  private final void setValue(Number value) {
    savedValue.set(value);
    if (enabled || disabledValue.isNull()) {
      spinner.setValue(value);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.14  2009/01/20 20:29:48  sueh
 * <p> bug# 1102 Added getFieldType.
 * <p>
 * <p> Revision 1.13  2007/07/12 21:44:27  sueh
 * <p> bug# 993 In setEnabled, when using savedValue and disabledValue,
 * <p> make sure that savedValue can't be 0.
 * <p>
 * <p> Revision 1.12  2007/04/02 21:53:11  sueh
 * <p> bug# 964 Implementing Cell interface.
 * <p>
 * <p> Revision 1.11  2007/03/27 19:32:23  sueh
 * <p> bug# 964 Changed InputCell.setEnabled() to setEditable.
 * <p>
 * <p> Revision 1.10  2007/03/01 01:44:26  sueh
 * <p> bug# 964 Moved colors from InputCell to Colors.
 * <p>
 * <p> Revision 1.9  2007/02/09 00:53:24  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.8  2007/02/05 23:45:23  sueh
 * <p> bug# 962 Added getIntInstance, getIntValue, getLongInstance, getLongValue,
 * <p> getStringValue, and removeChangeListener.
 * <p>
 * <p> Revision 1.7  2006/10/16 22:53:13  sueh
 * <p> bug# 919  Added setToolTipText().
 * <p>
 * <p> Revision 1.6  2005/09/13 00:02:47  sueh
 * <p> bug# 532 Fixed bug in setEnabled() by return if nothing has to be done.
 * <p>
 * <p> Revision 1.5  2005/08/04 20:19:46  sueh
 * <p> bug# 532 added getWidth().
 * <p>
 * <p> Revision 1.4  2005/07/29 19:48:02  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.3  2005/07/19 22:35:48  sueh
 * <p> bug# 532 changing the look of inUse == false to greyed out text.
 * <p> Changing the look of error == true to red background.
 * <p>
 * <p> Revision 1.2  2005/07/01 23:06:30  sueh
 * <p> bug# 619 added addChangeListener
 * <p>
 * <p> Revision 1.1  2005/07/01 21:24:32  sueh
 * <p> bug# 619 A writable table cell that extends InputCell and contains a
 * <p> JSpinner.  Only contains integers.
 * <p> </p>
 */
