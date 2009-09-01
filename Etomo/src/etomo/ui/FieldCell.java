package etomo.ui;

import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JTextField;

import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoNumber;
import etomo.type.ParsedArray;
import etomo.type.ParsedElementType;
import etomo.type.UITestFieldType;

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
 * <p> Revision 1.27  2009/01/20 20:01:53  sueh
 * <p> bug# 1102 Added getFieldType so that the ancestor class can name itself
 * <p> and print out the name.
 * <p>
 * <p> Revision 1.26  2008/09/30 20:59:24  sueh
 * <p> bug# 1113 Added toString.
 * <p>
 * <p> Revision 1.25  2008/08/21 00:07:22  sueh
 * <p> bug# 1132 Added isEnabled.
 * <p>
 * <p> Revision 1.24  2008/06/20 20:05:50  sueh
 * <p> bug# 1119 For clarity added _NUMBER to MATLAB and NON_MATLAB.
 * <p>
 * <p> Revision 1.23  2008/04/02 02:26:03  sueh
 * <p> bug# 1097 Added ParsedElementType to work with ParsedArray correctly.
 * <p>
 * <p> Revision 1.22  2007/09/27 20:50:20  sueh
 * <p> bug# 1044 Added getIneditableInstance(String value) to create an instance with
 * <p> a value set.  Useful for debugging.
 * <p>
 * <p> Revision 1.21  2007/07/31 20:42:20  sueh
 * <p> bug# 1028 Added getParsedArray().
 * <p>
 * <p> Revision 1.20  2007/07/18 23:21:07  sueh
 * <p> bug# 1022 Added getEtomoNumber() and
 * <p> getEtomoNumber(EtomoNumber.Type).
 * <p>
 * <p> Revision 1.19  2007/04/09 21:13:09  sueh
 * <p> bug# 964 Added clearExpandableValues().  Clears contractedValue and
 * <p> expandedValue, and sets the displayed value to nothing.
 * <p>
 * <p> Revision 1.18  2007/04/02 21:49:28  sueh
 * <p> bug# 964 Added FieldCell.editable to make instances of FieldCell that can't be
 * <p> edited.  This allows FieldCell.setEditable and setEnabled to be called without
 * <p> checking whether a field should be editable.
 * <p>
 * <p> Revision 1.17  2007/03/27 19:30:59  sueh
 * <p> bug# 964 Changed InputCell.setEnabled() to setEditable.  Added setEnabled().
 * <p>
 * <p> Revision 1.16  2007/03/26 18:38:34  sueh
 * <p> bug# 964 Prevented getContractedValue and getExpandedValue from returning null.
 * <p>
 * <p> Revision 1.15  2007/03/20 23:10:15  sueh
 * <p> bug# 964 Added fixedValues, to prevent the field from being enabled.  Used by
 * <p> getExpandableInstance().
 * <p>
 * <p> Revision 1.14  2007/03/01 01:34:18  sueh
 * <p> bug# 964 Made InputCell colors constant and moved them to Colors.  Added
 * <p> setExpandableValues, getContractedValue, and getExpandedValue.
 * <p>
 * <p> Revision 1.13  2007/02/09 00:49:04  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.12  2007/02/05 23:36:06  sueh
 * <p> bug# 962 Added getFloatValue and setValue(ConstEtomoNumber).
 * <p>
 * <p> Revision 1.11  2006/03/21 19:38:00  sueh
 * <p> bug# 807 Add the ability to set and retrieve a range of integers.  Added
 * <p> range, endValue, getEndValue(), setRangeValue().
 * <p>
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

  private final JTextField textField;
  private final boolean editable;
  private final ParsedElementType parsedElementType;

  private String hiddenValue = null;
  private boolean hideValue = false;
  private boolean range = false;
  private int endValue = EtomoNumber.INTEGER_NULL_VALUE;
  private String contractedValue = null;
  private String expandedValue = null;
  private boolean fixedValues = false;
  private boolean inUse = true;

  private FieldCell(boolean editable, ParsedElementType parsedElementType) {
    this.editable = editable;
    this.parsedElementType = parsedElementType;
    //construction
    textField = new JTextField();
    //field
    textField.setBorder(BorderFactory.createEtchedBorder());
    //color
    setBackground();
    setForeground();
    setFont();
  }

  public String toString() {
    return textField.getText();
  }

  static FieldCell getEditableInstance() {
    return new FieldCell(true, ParsedElementType.NON_MATLAB_NUMBER);
  }

  static FieldCell getEditableMatlabInstance() {
    return new FieldCell(true, ParsedElementType.MATLAB_NUMBER);
  }

  static FieldCell getIneditableInstance() {
    FieldCell instance = new FieldCell(false,
        ParsedElementType.NON_MATLAB_NUMBER);
    instance.setEditable(false);
    return instance;
  }

  static FieldCell getIneditableInstance(String value) {
    FieldCell instance = new FieldCell(false,
        ParsedElementType.NON_MATLAB_NUMBER);
    instance.setEditable(false);
    instance.setValue(value);
    return instance;
  }

  static FieldCell getExpandableInstance() {
    FieldCell instance = new FieldCell(false,
        ParsedElementType.NON_MATLAB_NUMBER);
    instance.setEditable(false);
    instance.fixedValues = true;
    return instance;
  }

  public void setEnabled(boolean enable) {
    setEditable(enable);
    if (enable) {
      setForeground();
    }
    else {
      textField.setForeground(Colors.CELL_DISABLED_FOREGROUND);
      textField.setDisabledTextColor(Colors.CELL_DISABLED_FOREGROUND);
    }
  }

  public boolean isEnabled() {
    return isEditable();
  }

  void setEditable(boolean editable) {
    //if this is not an editable instance, it can't be made editable
    if (!this.editable && editable) {
      return;
    }
    if (fixedValues && editable) {
      return;
    }
    super.setEditable(editable);
  }

  void setInUse(final boolean inUse) {
    this.inUse = inUse;
    setForeground();
  }

  void clearExpandableValues() {
    contractedValue = null;
    expandedValue = null;
    setValue();
  }

  /**
   * Sets contracted value and expanded value.
   * @param contractedValue
   * @param expandedValue
   */
  void setExpandableValues(final String contractedValue,
      final String expandedValue) {
    this.contractedValue = contractedValue;
    this.expandedValue = expandedValue;
  }

  void setHideValue(final boolean hideValue) {
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

  Component getComponent() {
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

  void setValue(ConstEtomoNumber value) {
    setValue(value.toString(), false);
  }

  void setRangeValue(int start, int end) {
    endValue = end;
    setValue(new Integer(start).toString() + " - "
        + new Integer(end).toString(), true);
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

  UITestFieldType getFieldType() {
    return UITestFieldType.TEXT_FIELD;
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

  float getFloatValue() {
    try {
      return Float.parseFloat(textField.getText());
    }
    catch (NumberFormatException e) {
      return EtomoNumber.FLOAT_NULL_VALUE;
    }
  }

  ConstEtomoNumber getEtomoNumber() {
    EtomoNumber number = new EtomoNumber();
    number.set(textField.getText());
    return number;
  }

  ParsedArray getParsedArray() {
    ParsedArray array = ParsedArray.getInstance(parsedElementType);
    array.setRawString(textField.getText());
    return array;
  }

  ConstEtomoNumber getEtomoNumber(EtomoNumber.Type type) {
    EtomoNumber number = new EtomoNumber(type);
    number.set(textField.getText());
    return number;
  }

  long getLongValue() {

    try {
      return new Long(textField.getText()).longValue();
    }
    catch (NumberFormatException e) {
      return 0;
    }
  }

  void setForeground() {
    if (inUse) {
      textField.setForeground(Colors.CELL_FOREGROUND);
      textField.setDisabledTextColor(Colors.CELL_FOREGROUND);
    }
    else {
      textField.setForeground(Colors.CELL_NOT_IN_USE_FOREGROUND);
      textField.setDisabledTextColor(Colors.CELL_NOT_IN_USE_FOREGROUND);
    }
  }

  int getWidth() {
    return textField.getWidth();
  }

  int getRightBorder() {
    return textField.getBorder().getBorderInsets(textField).right;
  }

  void setToolTipText(String text) {
    textField.setToolTipText(TooltipFormatter.INSTANCE.format(text));
  }

  void expand(boolean expanded) {
    if (expanded && expandedValue != null) {
      setValue(expandedValue);
    }
    else if (!expanded && contractedValue != null) {
      setValue(contractedValue);
    }
  }

  boolean equals(String comp) {
    return getValue().equals(comp);
  }

  String getContractedValue() {
    if (contractedValue == null) {
      return "";
    }
    return contractedValue;
  }

  String getExpandedValue() {
    if (expandedValue == null) {
      return "";
    }
    return expandedValue;
  }
}