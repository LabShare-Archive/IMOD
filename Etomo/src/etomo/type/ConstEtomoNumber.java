package etomo.type;

import java.util.Properties;
import java.util.Vector;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.comscript.ComScriptCommand;
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
 * <p> Revision 1.8  2005/01/10 23:26:46  sueh
 * <p> bug# 578 Standardized class so that every use of value goes through
 * <p> getValue().  GetValue() tries to find a non-null value by looking first at
 * <p> value, then resetValue, and then defaultValue (if displayDefault is set).
 * <p> Replacing isNull() with isSet().  IsSet() does not use getValue(), since it is
 * <p> querying whether the value was set (by set(), resetValue() with a non-null
 * <p> resetValue, or with an initialValue).  Added a new isNull() that uses
 * <p> getValue().
 * <p>
 * <p> Revision 1.7  2005/01/06 18:16:29  sueh
 * <p> bug# 578 Make integer null value static public.
 * <p>
 * <p> Revision 1.6  2004/12/29 00:05:29  sueh
 * <p> bug# 567 Added update(ComScriptCommand) to update value where the
 * <p> keyword in ComScriptCommand equals name.  Added validValues:  a list
 * <p> of valid values.  Added validate() to check validValues when value is
 * <p> changed.
 * <p>
 * <p> Revision 1.5  2004/12/16 02:27:18  sueh
 * <p> bug# 564 Remove recommendedValue.  Use resetValue instead.  Added
 * <p> is().
 * <p>
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
  public static final String rcsid = "$Id$";

  public static final int DOUBLE_TYPE = -1;
  public static final int FLOAT_TYPE = -2;
  public static final int INTEGER_TYPE = -3;
  public static final int LONG_TYPE = -4;

  private static final double doubleNullValue = Double.NaN;
  private static final float floatNullValue = Float.NaN;
  public static final int INTEGER_NULL_VALUE = Integer.MIN_VALUE;
  private static final long longNullValue = Long.MIN_VALUE;

  protected int type;
  protected String name;
  protected String description = null;
  protected String invalidReason = null;
  protected EtomoVersion originalVersion = null;
  protected boolean displayDefault = false;
  protected Number value;
  protected Number defaultValue;
  protected Number resetValue;
  protected Number ceilingValue;
  protected Vector validValues = null;

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
  
  protected ConstEtomoNumber(int type, String name, int initialValue) {
    this.type = type;
    this.name = name;
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
    if (that.validValues != null && that.validValues.size() == 0) {
      validValues = new Vector(that.validValues.size());
      for (int i = 0; i < that.validValues.size(); i++) {
        validValues.add(newNumber((Number) that.validValues.get(i)));
      }
    }
  }

  public String getDescription() {
    return description;
  }

  public String getName() {
    return name;
  }
  
  public int getResetInteger() {
    return resetValue.intValue();
  }

  /**
   * Validate value against validValues
   * Sets invalidReason if invalid
   */
  protected void validate() {
    if (!isSet() || invalidReason != null || validValues == null) {
      return;
    }
    boolean valid = false;
    for (int i = 0; i < validValues.size(); i++) {
      if (equals(value, (Number) validValues.get(i))) {
        valid = true;
      }
    }
    if (!valid) {
      StringBuffer invalidBuffer = new StringBuffer();
      invalidReason = "Invalid value:  " + toString(value)
          + "\nValid values are " + toString(validValues);
    }
  }

  public boolean isValid() {
    return invalidReason == null;
  }

  public boolean isValid(boolean displayErrorMessage, String errorTitle) {
    boolean valid = invalidReason == null;
    if (!valid && displayErrorMessage) {
      BaseManager manager = EtomoDirector.getInstance().getCurrentManager();
      if (manager != null) {
        manager.getMainPanel().openMessageDialog(
            description + ": " + invalidReason, errorTitle);
      }
    }
    return valid;
  }

  public String getInvalidReason() {
    return description + ": " + invalidReason;
  }

  public String classInfoString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",\ntype=" + type + ",\nname=" + name + ",\ndescription="
        + description + ",\ninvalidReason=" + invalidReason + ",\nvalue="
        + value + ",\ndefaultValue=" + defaultValue + ",\nresetValue="
        + resetValue + ",\nceilingValue=" + ceilingValue;
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

  public ConstEtomoNumber setValidValues(int[] validValues) {
    if (validValues == null || validValues.length == 0) {
      return this;
    }
    this.validValues = new Vector(validValues.length);
    for (int i = 0; i < validValues.length; i++) {
      this.validValues.add(this.newNumber(validValues[i]));
    }
    validate();
    return this;
  }
  
  public ConstEtomoNumber setOriginalVersion(String originalVersion) {
    this.originalVersion.set(originalVersion);
    return this;
  }
  
  public boolean inVersion(EtomoVersion version) {
    if (originalVersion.isNull() || originalVersion.isNull()) {
      return false;
    }
    if (originalVersion.earlierOrEqualTo(version)) {
      return true;
    }
    return false;
  }

  public void store(Properties props) {
    props.setProperty(name, toString(value));
  }

  public void store(Properties props, String prepend) {
    props.setProperty(prepend + "." + name, toString(value));
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
    return toString(value);
  }

  public int getInteger() {
    return getInteger(displayDefault);
  }

  public int getInteger(boolean displayDefault) {
    return getValue(displayDefault).intValue();
  }
  
  public boolean is() {
    if (isNull() || equals(0)) {
      return false;
    }
    return true;
  }

  public long getLong() {
    return getValue().longValue();
  }

  public double getDouble() {
    return getValue().doubleValue();
  }

  public Number getNumber() {
    return getNumber(displayDefault);
  }

  public Number getNumber(boolean displayDefault) {
    return newNumber(getValue(displayDefault));
  }
  
  public ConstEtomoNumber update(ComScriptCommand scriptCommand) {
    Number value = getValue();
    if (isNull(value) || (!isNull(defaultValue) && equals(value, defaultValue))) {
      scriptCommand.deleteKey(name);
    }
    else {
      scriptCommand.setValue(name, toString());
    }
    return this;
  }

  public ConstEtomoNumber getNegation() {
    EtomoNumber that = new EtomoNumber(this);
    that.value = newNumberMultiplied(value, -1);
    that.resetValue = newNumberMultiplied(resetValue, -1);
    that.defaultValue = newNumberMultiplied(defaultValue, -1);
    if (validValues == null || validValues.size() == 0) {
      return that;
    }
    that.validValues = new Vector(validValues.size());
    for (int i = 0; i < validValues.size(); i++) {
      that.validValues
          .add(newNumberMultiplied((Number) validValues.get(i), -1));
    }
    return that;
  }

  public boolean isSetAndNotDefault() {
    return isSet() && (isNull(defaultValue) || !value.equals(defaultValue));
  }

  /**
   * 
   * @return true if the instance value has been set using a set function
   * or by setting initial value.  Ignores resetValue and defaultValue.
   */
  public boolean isSet() {
    return !isNull(value);
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
  
  public boolean isNull() {
    return isNull(getValue());
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
    originalVersion = new EtomoVersion();
  }

  private void initialize(int initialValue) {
    value = newNumber(initialValue);
    defaultValue = newNumber();
    resetValue = newNumber();
    ceilingValue = newNumber();
    originalVersion = new EtomoVersion();
  }

  /**
   * Gets the correct value, taking resetValue and defaultValue
   * and displayDefault into account.  Should be used every time the value is
   * looked at, except isSet().
   * @param displayDefault
   * @return
   */
  protected Number getValue() {
    return getValue(displayDefault);
  }

  /**
   * Gets the correct value, taking resetValue and defaultValue
   * and displayDefault into account.  Should be used every time the value is
   * looked at, except isSet().
   * @param displayDefault
   * @return
   */
  protected Number getValue(boolean displayDefault) {
    if (isSet()) {
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
  
  protected String toString(Number value) {
    return value.toString();
  }
  
  protected String toString(Vector numberVector) {
    if (numberVector == null || numberVector.size() == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(toString((Number) numberVector.get(0)));
    for (int i = 1; i < numberVector.size(); i++) {
      buffer.append("," + toString((Number) numberVector.get(0)));
    }
    return buffer.toString();
  }
  
  protected Number newNumber() {
    switch (type) {
    case DOUBLE_TYPE:
      return new Double(doubleNullValue);
    case FLOAT_TYPE:
      return new Float(floatNullValue);
    case INTEGER_TYPE:
      return new Integer(INTEGER_NULL_VALUE);
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
      return value.intValue() == INTEGER_NULL_VALUE;
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
      return value == INTEGER_NULL_VALUE;
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