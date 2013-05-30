package etomo.storage;

import etomo.EtomoDirector;
import etomo.storage.Directive.BooleanValue;
import etomo.storage.Directive.NumericPairValue;
import etomo.storage.Directive.NumericValue;
import etomo.storage.Directive.StringValue;
import etomo.storage.Directive.Value;
import etomo.storage.Directive.ValueFactory;
import etomo.type.AxisID;
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
  private Value defaultValueA = null;
  private Value defaultValueB = null;
  private Value value = null;
  private Value valueA = null;
  private Value valueB = null;
  private int debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();

  DirectiveValues(final DirectiveValueType valueType) {
    this.valueType = valueType;
  }

  public Value getValue(final AxisID axisID) {
    if (axisID == null) {
      return value;
    }
    else if (axisID == AxisID.SECOND) {
      return valueB;
    }
    else if (axisID == AxisID.FIRST) {
      return valueA;
    }
    return null;
  }

  public Value getDefaultValue(final AxisID axisID) {
    if (axisID == null) {
      return defaultValue;
    }
    else if (axisID == AxisID.SECOND) {
      return defaultValueB;
    }
    else if (axisID == AxisID.FIRST) {
      return defaultValueA;
    }
    return null;
  }

  public boolean isSet(final AxisID axisID) {
    if (axisID == null) {
      return value != null;
    }
    else if (axisID == AxisID.FIRST) {
      return valueA != null;
    }
    else if (axisID == AxisID.SECOND) {
      return valueB != null;
    }
    return false;
  }

  /**
   * Returns true if any of the values when changed from their coresponding default
   * values.  If an A or B value has not been set, compare the value to non-axis default
   * value.
   * @return
   */
  public boolean isChanged() {
    if (value != null) {
      if (isChanged(value, defaultValue)) {
        if (debug > 1) {
          System.out.println("A:value:" + value + ",defaultValue:" + defaultValue);
        }
        return true;
      }
    }
    if (valueA != null) {
      if (defaultValueA != null) {
        if (isChanged(valueA, defaultValueA)) {
          if (debug > 1) {
            System.out.println("B");
          }
          return true;
        }
      }
      else if (isChanged(valueA, defaultValue)) {
        if (debug > 1) {
          System.out.println("C");
        }
        return true;
      }
    }
    if (valueB != null) {
      if (defaultValueB != null) {
        if (isChanged(valueB, defaultValueB)) {
          if (debug > 1) {
            System.out.println("D");
          }
          return true;
        }
      }
      else if (isChanged(valueB, defaultValue)) {
        if (debug > 1) {
          System.out.println("E");
        }
        return true;
      }
    }
    if (debug > 1) {
      System.out.println("F");
    }
    return false;
  }

  /**
   * Returns true if any of the values when changed from their coresponding default
   * values.  If an A or B default value has not been set, compare the value to non-axis
   * default value.
   * @return
   */
  public boolean isChanged(final AxisID axisID) {
    if (axisID == null) {
      if (value != null) {
        if (isChanged(value, defaultValue)) {
          return true;
        }
      }
    }
    else if (axisID == AxisID.FIRST) {
      if (valueA != null) {
        if (defaultValueA != null) {
          if (isChanged(valueA, defaultValueA)) {
            return true;
          }
        }
        else if (isChanged(valueA, defaultValue)) {
          return true;
        }
      }
    }
    else if (axisID == AxisID.SECOND) {
      if (valueB != null) {
        if (defaultValueB != null) {
          if (isChanged(valueB, defaultValueB)) {
            return true;
          }
        }
        else if (isChanged(valueB, defaultValue)) {
          return true;
        }
      }
    }
    return false;
  }

  /**
   * Assumes that the two values have the same type.
   * @param value
   * @param defaultValue
   * @return
   */
  public boolean equals(Value value1, Value value2) {
    if (debug >= 2) {
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
    if (debug >= 2) {
      System.err.println("isChanged:value:" + value + ",defaultValue:" + defaultValue);
    }
    if (value == null) {
      return false;
    }
    if (debug >= 2) {
      value.setDebug(2);
    }
    return !equals(value, defaultValue);
  }

  public void setDebug(final int input) {
    debug = input;
  }

  public void resetDebug() {
    debug = EtomoDirector.INSTANCE.getArguments().getDebugLevel();
  }

  void setDefaultValue(final AxisID axisID, final boolean input) {
    createDefaultValue(axisID);
    if (axisID == null) {
      defaultValue.set(input);
    }
    else if (axisID == AxisID.SECOND) {
      defaultValueB.set(input);
    }
    else {
      defaultValueA.set(input);
    }
  }

  void setDefaultValue(final AxisID axisID, final String input) {
    if (input == null || input.matches("\\ss*")) {
      if (axisID == null) {
        defaultValue = null;
      }
      else if (axisID == AxisID.SECOND) {
        defaultValueB = null;
      }
      else {
        defaultValueA = null;
      }
    }
    else {
      createDefaultValue(axisID);
      if (axisID == null) {
        defaultValue.set(input);
      }
      else if (axisID == AxisID.SECOND) {
        defaultValueB.set(input);
      }
      else {
        defaultValueA.set(input);
      }
    }
  }

  void setDefaultValue(final int input) {
    if (input == EtomoNumber.INTEGER_NULL_VALUE) {
      defaultValue = null;
    }
    else {
      createDefaultValue(null);
      defaultValue.set(input);
    }
  }

  void setValue(final AxisID axisID, final boolean input) {
    createValue(axisID);
    if (axisID == null) {
      value.set(input);
    }
    else if (axisID == AxisID.SECOND) {
      valueB.set(input);
    }
    else {
      valueA.set(input);
    }
  }

  void setValue(final AxisID axisID, final ConstEtomoNumber input) {
    if (input == null || input.isNull()) {
      if (axisID == null) {
        value = null;
      }
      else if (axisID == AxisID.SECOND) {
        valueB = null;
      }
      else {
        valueA = null;
      }
    }
    else {
      createValue(axisID);
      if (axisID == null) {
        value.set(input);
      }
      else if (axisID == AxisID.SECOND) {
        valueB.set(input);
      }
      else {
        valueA.set(input);
      }
    }
  }

  void setValue(final AxisID axisID, final String input) {
    if (input == null || input.matches("\\s*")) {
      if (axisID == null) {
        value = null;
      }
      else if (axisID == AxisID.SECOND) {
        valueB = null;
      }
      else {
        valueA = null;
      }
    }
    else {
      createValue(axisID);
      if (axisID == null) {
        value.set(input);
      }
      else if (axisID == AxisID.SECOND) {
        valueB.set(input);
      }
      else {
        valueA.set(input);
      }
    }
  }

  void setValue(final boolean input) {
    createValue(null);
    value.set(input);
  }

  void setValue(final ConstEtomoNumber input) {
    if (input == null || input.isNull()) {
      value = null;
    }
    else {
      createValue(null);
      value.set(input);
    }
  }

  void setValue(final ConstStringParameter input) {
    if (input == null || input.isEmpty()) {
      value = null;
    }
    else {
      createValue(null);
      value.set(input);
    }
  }

  void setValue(final double input) {
    if (input == EtomoNumber.DOUBLE_NULL_VALUE) {
      value = null;
    }
    else {
      createValue(null);
      value.set(input);
    }
  }

  void setValue(final double[] input) {
    if (input == null) {
      value = null;
    }
    else {
      createValue(null);
      value.set(input);
    }
  }

  void setValue(final int input) {
    if (input == EtomoNumber.INTEGER_NULL_VALUE) {
      value = null;
    }
    createValue(null);
    value.set(input);
  }

  void setValue(final String input) {
    if (input == null || input.matches("\\s*")) {
      value = null;
    }
    else {
      createValue(null);
      value.set(input);
    }
  }

  private void createDefaultValue(final AxisID axisID) {
    if (axisID == null) {
      if (defaultValue == null) {
        defaultValue = ValueFactory.getValue(valueType);
      }
    }
    else if (axisID == AxisID.SECOND) {
      if (defaultValueB == null) {
        defaultValueB = ValueFactory.getValue(valueType);
      }
    }
    else {
      if (defaultValueA == null) {
        defaultValueA = ValueFactory.getValue(valueType);
      }
    }
  }

  private void createValue(final AxisID axisID) {
    if (axisID == null) {
      if (value == null) {
        value = ValueFactory.getValue(valueType);
      }
    }
    else if (axisID == AxisID.SECOND) {
      if (valueB == null) {
        valueB = ValueFactory.getValue(valueType);
      }
    }
    else {
      if (valueA == null) {
        valueA = ValueFactory.getValue(valueType);
      }
    }
  }
}