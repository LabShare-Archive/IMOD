package etomo.type;

import java.util.Properties;
import java.util.Vector;

import etomo.storage.Storable;
import etomo.ui.UIHarness;

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
 * <p> Revision 1.18  2005/05/10 02:20:16  sueh
 * <p> bug# 658 corrected comment
 * <p>
 * <p> Revision 1.17  2005/05/10 02:18:25  sueh
 * <p> bug# 658 Setting invalidReason with functions resetInvalidReason and
 * <p> addInvalidReason.  Change validate() to setInvalidReason().  Make
 * <p> validate() a public function like isValid(), except that it throws an
 * <p> exception.  The preventNullValue member variable is unnecessary because
 * <p> just setting displayValue has the same effect - remove it.  Add
 * <p> nullIsValid member variable (default true).  NullIsValid set to false
 * <p> doesn't prevent current value from being null, so a display value isn't
 * <p> necessary.  But, isValid() or validate() will fail in this situation.
 * <p> Added recommended value.  This value will only appear in invalidReason
 * <p> messages as a suggestion.  Removed useDisplayValue, since turning
 * <p> displayValue on and off is confusing.  Fixed a bug in toString(Vector).
 * <p> To create a required field:
 * <p> Call setNullIsValid(false)
 * <p> If possible call setRecommendedValue()
 * <p> Call isValid() or validate() to validate the field
 * <p>
 * <p> Revision 1.16  2005/04/25 20:50:29  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.15  2005/03/08 01:59:56  sueh
 * <p> bug# 533 Making the long null value public.
 * <p>
 * <p> Revision 1.14  2005/01/25 22:48:00  sueh
 * <p> Added is(Number value) to convert value to a boolean.
 * <p>
 * <p> Revision 1.13  2005/01/25 21:56:30  sueh
 * <p> Changing resetValue to displayValue.  Removing empty functionality.  Removing
 * <p> displayDefault functionality.  Adding boolean useDisplayValue, but not
 * <p> adding function to set it, because it is equivalent to setting
 * <p> displayValue to null.  An inheriting class might want to turn off
 * <p> useDisplayValue to prevent displayValue from being used on the screen.
 * <p> Store only the currentValue.  Moving defaultValue to inherit class
 * <p> ScriptParameter.
 * <p>
 * <p> Revision 1.12  2005/01/22 04:11:06  sueh
 * <p> bug# 509, bug# 591  Ignore empty in comparisons.  Change validate() to
 * <p> ignore empty when looking at validValues.  Also in validate() fail on null
 * <p> or empty if preventNullValue is on.
 * <p>
 * <p> Revision 1.11  2005/01/21 23:04:12  sueh
 * <p> bug# 509 bug# 591  Added isUpdateCommand() in place of
 * <p> isSetAndNotDefault() as a standard why to decide if a parameter should
 * <p> be placed in a comscript.  Changed the name of value to currentValue.
 * <p> Added an empty value.  The currentValue is set to empty value when
 * <p> set(String) receives an empty or whitespace filled string.  The distinguishes
 * <p> it from a currentValue that was never set or set incorrectly.  THe only
 * <p> function which doesn't treate empty the same as null is getValue().  This
 * <p> allows EtomoNumber to display a blank field, even when the resetValue is
 * <p> in use.  However, since store() and updateCommand(), and
 * <p> isUpdateCommand() treate empty just like a null, the blank field isn't
 * <p> remembered.  This prevents the comscript, .edf, .ejf files from having lots
 * <p> of empty fields in them.  This means that the reset value will show when
 * <p> they are reloaded.  If a blank numeric field must be remembered and reload
 * <p> blank, write an alternate way to save it, treating empty like a regular value.
 * <p>
 * <p> Revision 1.10  2005/01/14 23:02:34  sueh
 * <p> Removing originalVersion because it is not being used.
 * <p>
 * <p> Revision 1.9  2005/01/10 23:48:08  sueh
 * <p> bug# 578 Changing getValue() to protected, so it can be used by
 * <p> EtomoState.
 * <p>
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
  public static final long LONG_NULL_VALUE = Long.MIN_VALUE;

  protected int type;
  protected String name;
  protected String description = null;
  protected StringBuffer invalidReason = null;
  protected Number currentValue = null;
  protected Number displayValue = null;
  protected Number ceilingValue = null;
  protected Vector validValues = null;
  protected boolean nullIsValid = true;

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

  protected ConstEtomoNumber(ConstEtomoNumber that) {
    type = that.type;
    name = that.name;
    description = that.description;
    currentValue = newNumber(that.currentValue);
    displayValue = newNumber(that.displayValue);
    ceilingValue = newNumber(that.ceilingValue);
    nullIsValid = that.nullIsValid;
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
  
  public int getDisplayInteger() {
    return displayValue.intValue();
  }

  /**
   * If validValues has been set, look for currentValue in validValues. Set
   * invalidReasion if  currentValue is not found.  Null is ignored.
   * Set invalidReason if currentValue is null and nullIsValid is false.
   */
  protected void setInvalidReason() {
    //Pass when there are no validation settings
    if (nullIsValid && validValues == null) {
      return;
    }
    //Catch illegal null values
    if (isNull(currentValue)) {
      if (nullIsValid) {
        return;
      }
      addInvalidReason("This field cannot be empty.");
    }
    //Validate against validValues
    else if (validValues != null) {
      for (int i = 0; i < validValues.size(); i++) {
        if (equals(currentValue, (Number) validValues.get(i))) {
          return;
        }
      }
      addInvalidReason(toString(currentValue) + " is not a valid value.");
      StringBuffer invalidBuffer = new StringBuffer();
    }
    //Pass all other cases
    else {
      return;
    }
    //Add recommendations to invalidReason
    if (validValues != null) {
      addInvalidReason("Valid values are " + toString(validValues) + ".");
    }
  }
  
  /**
   * Returns ceilingValue if value > ceilingValue.
   * Otherwise returns value.
   * Ignores null and empty
   * @param value
   * @return
   */
  protected Number applyCeilingValue(Number value) {
    if (value != null && !isNull(ceilingValue) && !isNull(value)
        && gt(currentValue, ceilingValue)) {
      return ceilingValue;
    }
    return value;
  }

  /**
   * If invalidReason is set, return true
   * @return
   */
  public boolean isValid() {
    return invalidReason == null;
  }
  
  /**
   * If invalidReason is set, throw an exception
   * @throws InvalidEtomoNumberException
   */
  public void validate() throws InvalidEtomoNumberException {
    if (invalidReason != null) {
      throw new InvalidEtomoNumberException(invalidReason.toString());
    }
  }
  
  /**
   * If invalidReason is set, display an error message and throw an exception
   * @param errorTitle
   * @param axisID
   * @throws InvalidEtomoNumberException
   */
  public void validate(String errorTitle, String description, AxisID axisID)
      throws InvalidEtomoNumberException {
    if (!isValid(true, errorTitle, description, axisID)) {
      throw new InvalidEtomoNumberException(invalidReason.toString());
    }
  }
  
  /**
   * If invalidReason is set, display an error message and return true
   * @param displayErrorMessage
   * @param errorTitle
   * @param axisID
   * @return
   */
  public boolean isValid(boolean displayErrorMessage, String errorTitle, AxisID axisID) {
    return isValid(displayErrorMessage, errorTitle, null, axisID);
  }

  /**
   * If invalidReason is set, display an error message and return true
   * @param displayErrorMessage
   * @param errorTitle
   * @param description
   * @param axisID
   * @return
   */
  public boolean isValid(boolean displayErrorMessage, String errorTitle,
      String description, AxisID axisID) {
    if (invalidReason != null && displayErrorMessage) {
      if (description == null) {
        UIHarness.INSTANCE.openMessageDialog(this.description + ": "
            + invalidReason, errorTitle, axisID);
      }
      else {
        description = description.trim();
        if (description.endsWith(":")) {
          UIHarness.INSTANCE.openMessageDialog(description + "  " + invalidReason,
              errorTitle, axisID);
        }
        else {
          UIHarness.INSTANCE.openMessageDialog(description + ":  "
              + invalidReason, errorTitle, axisID);
        }
      }
      return false;
    }
    return true;
  }

  public String getInvalidReason() {
    return description + ": " + invalidReason;
  }

  public String classInfoString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return ",\ntype=" + type + ",\nname=" + name + ",\ndescription="
        + description + ",\ninvalidReason=" + invalidReason + ",\ncurrentValue="
        + currentValue + ",\ndisplayValue="
        + displayValue + ",\nceilingValue=" + ceilingValue
        + ",\nnullIsValid=" + nullIsValid;
  }

  public ConstEtomoNumber setCeiling(int ceilingValue) {
    this.ceilingValue = newNumber(ceilingValue);
    return this;
  }

  /**
   * Set the value will be used if the user does not set a value or there is no
   * value to load.  Also used in reset().
   * @param resetValue
   * @return
   */
  public ConstEtomoNumber setDisplayValue(int displayValue) {
    this.displayValue = newNumber(displayValue);
    return this;
  }
  
  public ConstEtomoNumber setDisplayValue(boolean displayValue) {
    this.displayValue = newNumber(displayValue);
    return this;
  }
  
  protected ConstEtomoNumber setDisplayValue(Number displayValue) {
    this.displayValue = newNumber(displayValue);
    return this;
  }
  
  /**
   * Set the value will be used if the user does not set a value or there is no
   * value to load.  Also used in reset().
   * @param resetValue
   * @return
   */
  public void setDisplayValue(double displayValue) {
    if (type != DOUBLE_TYPE) {
      throw new IllegalStateException("Cannot use a double displayValue with any type but double.");
    }
    this.displayValue = newNumber(displayValue);
  }
  
  public void setDisplayValue(long displayValue) {
    if (type != DOUBLE_TYPE && type != LONG_TYPE) {
      throw new IllegalStateException("Cannot use a long displayValue with any type but long or double.");
    }
    this.displayValue = newNumber(displayValue);
  }

  public void setDescription(String description) {
    if (description != null) {
      this.description = description;
    }
    else {
      this.description = name;
    }
  }

  public ConstEtomoNumber setNullIsValid(boolean nullIsValid) {
    this.nullIsValid = nullIsValid;
    return this;
  }
  
  public ConstEtomoNumber setValidValues(int[] validValues) {
    if (validValues == null || validValues.length == 0) {
      return this;
    }
    this.validValues = new Vector(validValues.length);
    for (int i = 0; i < validValues.length; i++) {
      this.validValues.add(this.newNumber(validValues[i]));
    }
    return this;
  }
  
  public void store(Properties props) {
    if (isNull(currentValue)) {
      remove(props);
      return;
    }
    props.setProperty(name, toString(currentValue));
  }

  public void store(Properties props, String prepend) {
    if (isNull(currentValue)) {
      remove(props, prepend);
      return;
    }
    props.setProperty(prepend + "." + name, toString(currentValue));
  }

  public void remove(Properties props) {
    props.remove(name);
  }
  
  public void remove(Properties props, String prepend) {
    props.remove(prepend + "." + name);
  }
  
  public String toString() {
    return toString(getValue());
  }

  public int getInteger() {
    return getValue().intValue();
  }
  
  public boolean is() {
    if (isNull() || equals(0)) {
      return false;
    }
    return true;
  }
  
  protected boolean is(Number value) {
    if (isNull(value) || equals(value, 0)) {
      return false;
    }
    return true;
  }
  
  public boolean isPositive() {
    return gt(getValue(), newNumber(0));
  }
  
  public boolean isNegative() {
    return lt(getValue(), newNumber(0));
  }

  public long getLong() {
    return getValue().longValue();
  }

  public double getDouble() {
    return getValue().doubleValue();
  }

  public Number getNumber() {
    return newNumber(getValue());
  }

  /**
   * Returns true if getValue() equals that.getValue().
   * @param that
   * @return
   */
  public boolean equals(ConstEtomoNumber that) {
    return equals(getValue(), that.getValue());
  }

  public boolean equals(int value) {
    return equals(getValue(), value);
  }
  
  /**
   * Returns true if getValue() is null.
   * @return
  */
  public boolean isNull() {
    return isNull(getValue());
  }
  
  public boolean equals(Number value) {
    return equals(getValue(), value);
  }

  public boolean equals(String value) {
    return equals(getValue(), newNumber(value, new StringBuffer()));
  }

  protected void initialize() {
    initialize(newNumber());
  }
  
  protected void initialize(Number displayValue) {
    ceilingValue = newNumber();
    this.displayValue = newNumber(displayValue);
    currentValue = newNumber();
  }

  /**
   * Gets the correct value.  Used with public toString(), is(), and get...().
   * Also used with isNull() and equals().
   * If currentValue is not null or useDisplayValue is false, return currentValue.
   * Otherwise, if displayValue is not null, return displayValue.
   * Otherwise, return null;
   * @param displayDefault
   * @return
   */
  protected Number getValue() {
    if (!isNull(currentValue)) {
      return currentValue;
    }
    if (!isNull(displayValue)) {
      return displayValue;
    }
    return currentValue;
  }
  
  protected String toString(Number value) {
    if (isNull(value)) {
      return "";
    }
    return value.toString();
  }
  
  protected String toString(Vector numberVector) {
    if (numberVector == null || numberVector.size() == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(toString((Number) numberVector.get(0)));
    for (int i = 1; i < numberVector.size(); i++) {
      buffer.append("," + toString((Number) numberVector.get(1)));
    }
    return buffer.toString();
  }
  
  protected void resetInvalidReason() {
    invalidReason = null;
  }
  
  protected void addInvalidReason(String message) {
    if (invalidReason == null) {
      invalidReason = new StringBuffer(message);
    }
    else {
      invalidReason.append("\n" + message);
    }
  }
  
  protected Number newNumber() {
    if (displayValue != null) {
      return newNumber(displayValue);
    }
    switch (type) {
    case DOUBLE_TYPE:
      return new Double(doubleNullValue);
    case FLOAT_TYPE:
      return new Float(floatNullValue);
    case INTEGER_TYPE:
      return new Integer(INTEGER_NULL_VALUE);
    case LONG_TYPE:
      return new Long(LONG_NULL_VALUE);
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

  /**
   * Override this class to display numbers as descriptive character strings.
   * @param value
   * @param invalidBuffer
   * @return
   */
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
      invalidBuffer.append(value + " is not a valid number.");
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
  
  protected Number newNumber(boolean value) {
    if (value) {
      return newNumber(1);
    }
    return newNumber(0);
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

  protected Number newNumberMultiplied(Number number, int multiple) {
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
      return value.longValue() == LONG_NULL_VALUE;
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
      return value == LONG_NULL_VALUE;
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
  
  protected boolean lt(Number number, Number compValue) {
    if (isNull(number) || isNull(compValue)) {
      return false;
    }
    switch (type) {
    case DOUBLE_TYPE:
      return number.doubleValue() < compValue.doubleValue();
    case FLOAT_TYPE:
      return number.floatValue() < compValue.floatValue();
    case INTEGER_TYPE:
      return number.intValue() < compValue.intValue();
    case LONG_TYPE:
      return number.longValue() < compValue.longValue();
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