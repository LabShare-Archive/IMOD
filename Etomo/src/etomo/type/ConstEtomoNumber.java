package etomo.type;

import java.util.Properties;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.storage.Storable;

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
* <p> Revision 1.4  2004/11/30 00:35:03  sueh
* <p> bug# 556 Making isValid() error message clearer.
* <p>
* <p> Revision 1.3  2004/11/24 01:04:01  sueh
* <p> bug# 520 Allow class to display its own error message when required
* <p> (isValue).
* <p>
* <p> Revision 1.2  2004/11/19 23:33:29  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.4  2004/11/19 19:38:23  sueh
* <p> bug# 520 Made getValue() private.
* <p>
* <p> Revision 1.1.2.3  2004/11/19 03:02:57  sueh
* <p> bug# 520 Added a default displayDefault.  If not overriden it will affect how
* <p> get, toString, and equals fuctions work.  The default value is used only if
* <p> displayDefault is true.  DisplayDefault can be overriden by a parameter.
* <p> Added getValue to simplify choosing the first non-null value to work with.
* <p>
* <p> Revision 1.1.2.2  2004/11/19 00:04:05  sueh
* <p> bug# 520 changed the equals functions so that they work on the same
* <p> principle as the get functions, since they will be comparing values that
* <p> came from get.  If value is null, compare resetValue.  Added a useDefault
* <p> boolean:  if value and resetValue are null, compare defaultValue.
* <p>
* <p> Revision 1.1.2.1  2004/11/16 02:26:06  sueh
* <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
* <p> EtomoLong with EtomoNumber.  EtomoNumber acts a simple numeric
* <p> type which handles null values, defaults, and recommended values.
* <p> EtomoNumber stores its values in Number variables and is created with a
* <p> required type parameter to keep track of its numeric type.
* <p> </p>
*/
public abstract class ConstEtomoNumber implements Storable {
  public static  final String  rcsid =  "$Id$";
  
  public static final int DOUBLE_TYPE = -1;
  public static final int FLOAT_TYPE = -2;
  public static final int INTEGER_TYPE = -3;
  public static final int LONG_TYPE = -4;
  
  private static final double doubleNullValue = Double.NaN;
  private static final float floatNullValue = Float.NaN;
  private static final int integerNullValue = Integer.MIN_VALUE;
  private static final long longNullValue = Long.MIN_VALUE; 
  
  protected int type;
  protected String name;
  protected String description = null;
  protected String invalidReason = null;
  protected boolean displayDefault = false;
  protected Number value;
  protected Number defaultValue;
  protected Number resetValue;
  protected Number ceilingValue;

  protected ConstEtomoNumber(int type) {
    this.type = type;
    name = super.toString();
    description = name;
    initialize();
  }
  
  protected ConstEtomoNumber(int type, String name) {
    this.type = type;
    this.name = name;
    description = name;
    initialize();
  }
  
  protected ConstEtomoNumber(int type, int initialValue) {
    this.type = type;
    name = super.toString();
    description = name;
    initialize(initialValue);
  }
  
  protected ConstEtomoNumber(ConstEtomoNumber that) {
    type = that.type;
    name = that.name;
    description = that.description;
    value = newNumber(that.value);
    defaultValue = newNumber(that.defaultValue);
    resetValue = newNumber(that.resetValue);
    ceilingValue = newNumber(that.ceilingValue);
  }
  
  public String getDescription() {
    return description;
  }
  
  public boolean isValid() {
    return invalidReason == null;
  }
  
  public boolean isValid(boolean displayErrorMessage, String errorTitle) {
    boolean valid = invalidReason == null;
    if (!valid && displayErrorMessage) {
      BaseManager manager = EtomoDirector.getInstance().getCurrentManager();
      if (manager != null) {
        manager.getMainPanel().openMessageDialog(description + ": " + invalidReason, errorTitle);
      }
    }
    return valid;
  }
  
  public String getInvalidReason() {
    return invalidReason;
  }
  
  public String classInfoString() {
    return getClass().getName() + "[" + paramString() + "]";
  }
  
  protected String paramString() {
    return ",\ntype=" + type + ",\nname=" + name + ",\ndescription=" + description
        + ",\ninvalidReason=" + invalidReason + ",\nvalue="
        + value + ",\ndefaultValue=" + defaultValue + ",\nresetValue=" + resetValue
        + ",\nceilingValue=" + ceilingValue;
  }
  
  public ConstEtomoNumber setCeiling(int ceilingValue) {
    this.ceilingValue = newNumber(ceilingValue);
    return this;
  }
  
  public ConstEtomoNumber setDefault(int defaultValue) {
    this.defaultValue = newNumber(defaultValue);
    return this;
  }
  
  public ConstEtomoNumber setDefault(String defaultValueString) {
    StringBuffer invalidBuffer = new StringBuffer();
    Number defaultValue = newNumber(defaultValueString, invalidBuffer);
    if (invalidBuffer.length() == 0 && !isNull(defaultValue)) {
      this.defaultValue = defaultValue;
    }
    return this;
  }
  
  public ConstEtomoNumber setDisplayDefault(boolean displayDefault) {
    this.displayDefault = displayDefault;
    return this;
  }
  
  /**
   * Set the value will be used if the user does not set a value or there is no
   * value to load.  Also used in reset().
   * @param resetValue
   * @return
   */
  public void setResetValue(int resetValue) {
    this.resetValue = newNumber(resetValue);
  }
  
  /**
   * Set the value will be used if the user does not set a value or there is no
   * value to load.  Also used in reset().
   * @param resetValue
   * @return
   */
  public void setResetValue(double resetValue) {
    this.resetValue = newNumber(resetValue);
  }
  
  public void setDescription(String description) {
    if (description != null) {
      this.description = description;
    }
    else {
      name = description;
    }
  }
  
  public void store(Properties props) {
    props.setProperty(name, value.toString());
  }
  
  public void store(Properties props, String prepend) {
    props.setProperty(prepend + "." + name, value.toString());
  }
  
  public void remove(Properties props, String prepend) {
    props.remove(prepend + "." + name);
  }
  
  public String toString() {
    return toString(displayDefault);
  }
  
  public String toString(boolean displayDefault) {
    Number value = getValue(displayDefault);
    if (isNull(value)) {
      return "";
    }
    return value.toString();
  }
  
  public int getInteger() {
    return getInteger(displayDefault);
  }
  
  public int getInteger(boolean displayDefault) {
    return getValue(displayDefault).intValue();
  }
  
  public long getLong() {
    return getValue().longValue();
  }
  
  public Number getNumber() {
    return getNumber(displayDefault);
  }
  
  public Number getNumber(boolean displayDefault) {
    return newNumber(getValue(displayDefault));
  }

  public ConstEtomoNumber getNegation() {
    EtomoNumber that = new EtomoNumber(this);
    that.value = newNumberMultiplied(value, -1);
    that.resetValue = newNumberMultiplied(resetValue, -1);
    that.defaultValue = newNumberMultiplied(defaultValue, -1);
    return that;
  }

  public boolean isSetAndNotDefault() {
    return !isNull() && (isNull(defaultValue) || !value.equals(defaultValue));
  }

  public boolean isNull() {
    return isNull(value);
  }

  /**
   * compare two EtomoNumbers, comparing resetValue if value is null.
   * @param that
   * @return
   */
  public boolean equals(ConstEtomoNumber that) {
    return equals(getValue(), that.getValue());
  }

  public boolean equals(int value) {
    return equals(getValue(), value);
  }

  public boolean equals(Number value) {
    return equals(getValue(), value);
  }

  public boolean equals(String value) {
    return equals(getValue(), newNumber(value, new StringBuffer()));
  }

  private void initialize() {
    value = newNumber();
    defaultValue = newNumber();
    resetValue = newNumber();
    ceilingValue = newNumber();
  }
  
  private void initialize(int initialValue) {
    value = newNumber(initialValue);
    defaultValue = newNumber();
    resetValue = newNumber();
    ceilingValue = newNumber();
  }
  
  private Number getValue() {
    return getValue(displayDefault);
  }
  
  private Number getValue(boolean displayDefault) {
    if (!isNull()) {
      return value;
    }
    if (!isNull(resetValue)) {
      return resetValue;
    }
    if (displayDefault && !isNull(defaultValue)) {
      return defaultValue;
    }
    return value;
  }
  
  protected Number newNumber() {
    switch (type) {
      case DOUBLE_TYPE:
        return new Double(doubleNullValue);
      case FLOAT_TYPE:
        return new Float(floatNullValue);
      case INTEGER_TYPE:
        return new Integer(integerNullValue);
      case LONG_TYPE:
        return new Long(longNullValue);
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
  protected Number newNumber(Number value) {
    switch (type) {
      case DOUBLE_TYPE:
        return new Double(value.doubleValue());
      case FLOAT_TYPE:
        return new Float(value.floatValue());
      case INTEGER_TYPE:
        return new Integer(value.intValue());
      case LONG_TYPE:
        return new Long(value.longValue());
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
  protected Number newNumber(String value, StringBuffer invalidBuffer) {
    if (!value.matches("\\S+")) {
      return newNumber();
    }
    try {
      switch (type) {
      case DOUBLE_TYPE:
        return new Double(value);
      case FLOAT_TYPE:
        return new Float(value);
      case INTEGER_TYPE:
        return new Integer(value);
      case LONG_TYPE:
        return new Long(value);
      default:
        throw new IllegalStateException("type=" + type);
      }
    }
    catch (NumberFormatException e) {
      e.printStackTrace();
      invalidBuffer.append("Invalid number:  " + value);
      return newNumber();
    }
  }
  
  protected Number newNumber(int value) {
    switch (type) {
      case DOUBLE_TYPE:
        return new Double(new Integer(value).doubleValue());
      case FLOAT_TYPE:
        return new Float(new Integer(value).floatValue());
      case INTEGER_TYPE:
        return new Integer(value);
      case LONG_TYPE:
        return new Long(new Integer(value).longValue());
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
  protected Number newNumber(double value) {
    switch (type) {
      case DOUBLE_TYPE:
        return new Double(value);
      case FLOAT_TYPE:
        return new Float(new Double(value).floatValue());
      case INTEGER_TYPE:
        return new Integer(new Double(value).intValue());
      case LONG_TYPE:
        return new Long(new Double(value).intValue());
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
  private Number newNumberMultiplied(Number number, int multiple) {
    if (isNull(number)) {
      return number;
    }
    switch (type) {
    case DOUBLE_TYPE:
      return new Double(number.doubleValue() * multiple);
    case FLOAT_TYPE:
      return new Float(number.floatValue() * multiple);
    case INTEGER_TYPE:
      return new Integer(number.intValue() * multiple);
    case LONG_TYPE:
      return new Long(number.longValue() * multiple);
    default:
      throw new IllegalStateException("type=" + type);
    }
  }
  
  protected boolean isNull(Number value) {
    switch (type) {
      case DOUBLE_TYPE:
        return Double.isNaN(value.doubleValue());
      case FLOAT_TYPE:
        return Float.isNaN(value.floatValue());
      case INTEGER_TYPE:
        return value.intValue() == integerNullValue;
      case LONG_TYPE:
        return value.longValue() == longNullValue;
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
  protected boolean isNull(int value) {
    switch (type) {
      case DOUBLE_TYPE:
        return Double.isNaN(value);
      case FLOAT_TYPE:
        return Float.isNaN(value);
      case INTEGER_TYPE:
        return value == integerNullValue;
      case LONG_TYPE:
        return value == longNullValue;
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
  protected boolean gt(Number number, Number compValue) {
    if (isNull(number) || isNull(compValue)) {
      return false;
    }
    switch (type) {
      case DOUBLE_TYPE:
        return number.doubleValue() > compValue.doubleValue();
      case FLOAT_TYPE:
        return number.floatValue() > compValue.floatValue();
      case INTEGER_TYPE:
        return number.intValue() > compValue.intValue();
      case LONG_TYPE:
        return number.longValue() > compValue.longValue();
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
  protected boolean equals(Number number, Number compValue) {
    if (isNull(number) && isNull(compValue)) {
      return true;
    }
    if (isNull(number) || isNull(compValue)) {
      return false;
    }
    switch (type) {
      case DOUBLE_TYPE:
        return number.doubleValue() == compValue.doubleValue();
      case FLOAT_TYPE:
        return number.floatValue() == compValue.floatValue();
      case INTEGER_TYPE:
        return number.intValue() == compValue.intValue();
      case LONG_TYPE:
        return number.longValue() == compValue.longValue();
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
  protected boolean equals(Number number, int compValue) {
    if (isNull(number) && isNull(compValue)) {
      return true;
    }
    if (isNull(number) || isNull(compValue)) {
      return false;
    }
    switch (type) {
      case DOUBLE_TYPE:
        return number.doubleValue() == compValue;
      case FLOAT_TYPE:
        return number.floatValue() == compValue;
      case INTEGER_TYPE:
        return number.intValue() == compValue;
      case LONG_TYPE:
        return number.longValue() == compValue;
      default:
        throw new IllegalStateException("type=" + type);
    }
  }
  
}
