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
 * <p> Revision 1.10  2005/12/14 20:55:14  sueh
 * <p> bug# 784 Added setToolTipText().
 * <p>
 * <p> Revision 1.9  2005/09/01 18:00:33  sueh
 * <p> bug# 532 Added setValue(void) to empty a field.  Removed
 * <p> setValueEmpty().
 * <p>
 * <p> Revision 1.8  2005/08/27 22:36:43  sueh
 * <p> bug# 532 Added isEmpty(), which returns true for a field that is empty or
 * <p> only contains whitespace.
 * <p>
 * <p> Revision 1.7  2005/08/04 20:10:05  sueh
 * <p> bug# 532 added getWidth().
 * <p>
 * <p> Revision 1.6  2005/07/19 22:31:38  sueh
 * <p> bug# 532 changing the look of inUse == false to greyed out text.
 * <p> Changing the look of error == true to red background.
 * <p>
 * <p> Revision 1.5  2005/07/11 22:57:04  sueh
 * <p> bug# 619 Don't assume that a blank value is 0, return a null value from
 * <p> getIntValue().
 * <p>
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
final class FieldCell extends InputCell {
  public static final String rcsid = "$Id$";

  private final JTextField textField = new JTextField();
  
  private String hiddenValue = null;
  private boolean hideValue = false;
  private boolean range = false;
  private int endValue = EtomoNumber.INTEGER_NULL_VALUE;

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
  
  boolean isEmpty() {
    String value = textField.getText();
    return value == null || value.matches("\\s*");
  }

  protected final Component getComponent() {
    return textField;
  }
  
  private void setValue(String value, boolean range) {
    this.range = range;
    hiddenValue = value;
    if (!hideValue) {
      textField.setText(value);
    }
  }

  void setValue(String value) {
    setValue(value, false);
  }
  
  void setRangeValue(int start, int end) {
    endValue = end;
    setValue(new Integer(start).toString() + " - " + new Integer(end).toString(), true);
  }
  
  void setValue() {
    setValue("", false);
  }

  void setValue(int value) {
    setValue(new Integer(value).toString(), false);
  }

  void setValue(double value) {
    setValue(new Double(value).toString(), false);
  }
  
  void setValue(long value) {
    setValue(new Long(value).toString(), false);
  }
  
  int getEndValue() {
    if (!range) {
      throw new IllegalStateException("range not in use");
    }
    return endValue;
  }

  String getValue() {
    return textField.getText();
  }
  
  int getIntValue() {
    try {
      return Integer.parseInt(textField.getText());
    }
    catch (NumberFormatException e) {
      return EtomoNumber.INTEGER_NULL_VALUE;
    }
  }
  
  long getLongValue() {
    try {
      return new Long(textField.getText()).longValue();
    }
    catch (NumberFormatException e) {
      return 0;
    }
  }

  protected void setForeground() {
    if (inUse) {
      textField.setForeground(foreground);
      textField.setDisabledTextColor(foreground);
    }
    else {
      textField.setForeground(notInUseForeground);
      textField.setDisabledTextColor(notInUseForeground);
    }
  }
  
  int getWidth() {
    return textField.getWidth();
        //+ textField.getBorder().getBorderInsets(textField).right / 2;
  }
  
  int getRightBorder() {
    return textField.getBorder().getBorderInsets(textField).right;
  }
  
  void setToolTipText(String toolTipText) {
    textField.setToolTipText(toolTipText);
  }
}