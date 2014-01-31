package etomo.storage;

import etomo.Arguments.DebugLevel;
import etomo.EtomoDirector;
import etomo.comscript.FortranInputString;
import etomo.storage.Directive.BooleanValue;
import etomo.storage.Directive.NumericPairValue;
import etomo.storage.Directive.NumericValue;
import etomo.storage.Directive.StringValue;
import etomo.storage.Directive.Value;
import etomo.storage.Directive.ValueFactory;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstStringParameter;
import etomo.type.EtomoNumber;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class DirectiveValues {
  final DirectiveValueType valueType;

  private Value defaultValue = null;
  private Value value = null;
  private DebugLevel debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

  DirectiveValues(final DirectiveValueType valueType) {
    this.valueType = valueType;
  }

  public Value getValue() {
    return value;
  }

  public Value getDefaultValue() {
    return defaultValue;
  }

  public boolean isSet() {
    return value != null;
  }

  /**
   * Returns true if any of the values when changed from their coresponding default
   * values.  If an A or B value has not been set, compare the value to non-axis default
   * value.
   * @return
   */
  public boolean isChanged() {
    return isChanged(value, defaultValue);
  }

  /**
   * Assumes that the two values have the same type.
   * @param value
   * @param defaultValue
   * @return
   */
  public boolean equals(Value value1, Value value2) {
    if (debug.isExtra()) {
      System.err.println("equals:value1:" + value1 + ",value2:" + value2);
      value1.setDebug(debug);
    }
    if (value1 == null && value2 == null) {
      return true;
    }
    if (value1 == null) {
      // Value2 is not null - swap them to do the comparison.
      value1 = value2;
      value2 = null;
    }
    if (valueType == DirectiveValueType.BOOLEAN) {
      return ((BooleanValue) value1).equals((BooleanValue) value2);
    }
    if (valueType == DirectiveValueType.FLOATING_POINT
        || valueType == DirectiveValueType.INTEGER) {
      return ((NumericValue) value1).equals((NumericValue) value2);
    }
    if (valueType == DirectiveValueType.FLOATING_POINT_PAIR
        || valueType == DirectiveValueType.INTEGER_PAIR) {
      return ((NumericPairValue) value1).equals((NumericPairValue) value2);
    }
    if (valueType == DirectiveValueType.LIST || valueType == DirectiveValueType.STRING) {
      return ((StringValue) value1).equals((StringValue) value2);
    }
    return false;
  }

  /**
   * Returns false if value is null, because that means it hasn't been set.  Otherwise
   * returns !equals.
   * @param value
   * @param defaultValue
   * @return
   */
  private boolean isChanged(Value value, Value defaultValue) {
    if (debug.isExtra()) {
      System.err.println("isChanged:value:" + value + ",defaultValue:" + defaultValue);
    }
    if (value == null) {
      return false;
    }
    if (debug.isExtra()) {
      value.setDebug(debug);
    }
    return !equals(value, defaultValue);
  }

  public void setDebug(final DebugLevel input) {
    debug = input;
  }

  public void resetDebug() {
    debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();
  }

  void setDefaultValue(final boolean input) {
    createDefaultValue();
    defaultValue.set(input);
  }

  void setDefaultValue(final String input) {
    if (input == null || input.matches("\\ss*")) {
      defaultValue = null;
    }
    else {
      createDefaultValue();
      defaultValue.set(input);
    }
  }

  void setDefaultValue(final int input) {
    if (input == EtomoNumber.INTEGER_NULL_VALUE) {
      defaultValue = null;
    }
    else {
      createDefaultValue();
      defaultValue.set(input);
    }
  }

  void setDefaultValue(final ConstEtomoNumber input) {
    if (input.isNull()) {
      defaultValue = null;
    }
    else {
      createDefaultValue();
      defaultValue.set(input);
    }
  }

  void setValue(final boolean input) {
    createValue();
    value.set(input);
  }

  void setValue(final ConstEtomoNumber input) {
    if (input == null || input.isNull()) {
      value = null;
    }
    else {
      createValue();
      value.set(input);
    }
  }

  void setValue(final ConstStringParameter input) {
    if (input == null || input.isEmpty()) {
      value = null;
    }
    else {
      createValue();
      value.set(input);
    }
  }

  void setValue(final double input) {
    if (input == EtomoNumber.DOUBLE_NULL_VALUE) {
      value = null;
    }
    else {
      createValue();
      value.set(input);
    }
  }

  void setValue(final double[] input) {
    if (input == null) {
      value = null;
    }
    else {
      createValue();
      value.set(input);
    }
  }

  void setValue(final FortranInputString input) {
    if (input == null) {
      value = null;
    }
    else {
      createValue();
      value.set(input);
    }
  }

  void setValue(final int input) {
    if (input == EtomoNumber.INTEGER_NULL_VALUE) {
      value = null;
    }
    createValue();
    value.set(input);
  }

  void setValue(final String input) {
    if (input == null || input.matches("\\s*")) {
      value = null;
    }
    else {
      createValue();
      value.set(input);
    }
  }

  private void createDefaultValue() {
    if (defaultValue == null) {
      defaultValue = ValueFactory.getValue(valueType);
    }
  }

  private void createValue() {
    if (value == null) {
      value = ValueFactory.getValue(valueType);
    }
  }
}