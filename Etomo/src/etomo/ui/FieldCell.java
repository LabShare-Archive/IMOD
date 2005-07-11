package etomo.ui;

import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JTextField;

import etomo.type.EtomoNumber;

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
 * <p> Revision 1.4  2005/07/01 23:03:33  sueh
 * <p> bug# 619 added getIntValue
 * <p>
 * <p> Revision 1.3  2005/07/01 21:16:07  sueh
 * <p> bug# 619 Pulled an ancestor class (InputCell) out of FieldCell because we
 * <p>  need several typs of input cells.  Added setHideValue().  Changed
 * <p> setText() and getText() to setValue() and getValue() to standardize all
 * <p> input cell, one of which has a label (CheckBoxCell).
 * <p>
 * <p> Revision 1.2  2004/11/19 23:53:44  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.2  2004/10/13 23:06:57  sueh
 * <p> bug# 520 Changed the add() functions.  No longer relaying on the Table
 * <p> interface because a FieldCell instance could be added to different panels.
 * <p> The Add() functions remember what panel they where added to.  The
 * <p> remove() requires no parameters.
 * <p>
 * <p> Revision 1.1.2.1  2004/10/01 19:53:42  sueh
 * <p> bug# 520 A text field designed designed to be used with a gridbag layout.
 * <p> It can be used with any ui object with implements Table.  It has three
 * <p> state variables: isUse (lightens foreground when not in use), enabled
 * <p> darkens background when not enabled), and highlighted (uses table
 * <p> selection color for background when highlighted, setting it darker when
 * <p> field is disabled).
 * <p> </p>
 */
class FieldCell extends InputCell {
  public static final String rcsid = "$Id$";

  private JTextField textField = new JTextField();
  private String hiddenValue = null;
  private boolean hideValue = false;

  FieldCell() {
    super();
    setBackground();
    setForeground();
    setFont();
    textField.setBorder(BorderFactory.createEtchedBorder());
  }

  void setHideValue(boolean hideValue) {
    if (this.hideValue == hideValue) {
      return;
    }
    this.hideValue = hideValue;
    if (hiddenValue == null) {
      return;
    }
    if (hideValue) {
      hiddenValue = textField.getText();
      textField.setText("");
    }
    else {
      textField.setText(hiddenValue);
    }
  }

  protected final Component getComponent() {
    return textField;
  }

  final void setValue(String value) {
    hiddenValue = value;
    if (!hideValue) {
      textField.setText(value);
    }
  }

  final void setValue(int value) {
    setValue(new Integer(value).toString());
  }

  final void setValue(double value) {
    setValue(new Double(value).toString());
  }
  
  final void setValue(long value) {
    setValue(new Long(value).toString());
  }

  final void setValueEmpty() {
    setValue("");
  }

  final String getValue() {
    return textField.getText();
  }
  
  final int getIntValue() {
    try {
      return Integer.parseInt(textField.getText());
    }
    catch (NumberFormatException e) {
      return EtomoNumber.INTEGER_NULL_VALUE;
    }
  }
  
  final long getLongValue() {
    try {
      return new Long(textField.getText()).longValue();
    }
    catch (NumberFormatException e) {
      return 0;
    }
  }

  protected final void setForeground() {
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