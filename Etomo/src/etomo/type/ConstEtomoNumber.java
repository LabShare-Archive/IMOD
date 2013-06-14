package etomo.type;

import java.util.Properties;
import java.util.Vector;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import etomo.storage.Storable;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 * 
 * <p>DisplayValue verses defaultValue:</p>
 * 
 * <p>The display value is the value returned when the instance is null.  Use
 * the display value to prevent an instance from returning the null value or a
 * blank string.  The display value does not affect the result of the isNull()
 * function.<p>
 * 
 * <p>The default value is just an extra value that ConstEtomoNumber can store.
 * "Get" functions with the parameter "boolean defaultIfNull" are convenience
 * functions which return the default value when the instance is null.  Use the
 * default value as a way to store the default of the instance in one place.</p>
 * 
 * <p>ScriptParameter uses the default value to decide whether an instance needs
 * to be placed in a script.</p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
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
 * <p> Revision 1.68  2011/02/22 05:32:22  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.67  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.66  2009/12/01 15:19:30  sueh
 * <p> bug# 1282 In newNumber(String, StringBuffer) mentioned the type in the
 * <p> error message.
 * <p>
 * <p> Revision 1.65  2009/09/05 00:09:10  sueh
 * <p> bug# 1256 Moved toStringWithLeadingZeros to the nad param because it
 * <p> is only being used there.
 * <p>
 * <p> Revision 1.64  2009/09/02 22:44:55  sueh
 * <p> bug# 1254 Getting rid of excessive debug prints.
 * <p>
 * <p> Revision 1.63  2009/09/01 02:33:26  sueh
 * <p> bug# 1222 Fixed comparisons between different types.
 * <p>
 * <p> Revision 1.62  2009/03/17 00:46:15  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.61  2009/03/02 20:57:01  sueh
 * <p> bug# 1102 Added le(ConstEtomoNumber) and ge(ConstEtomoNumber).
 * <p>
 * <p> Revision 1.60  2008/11/20 01:34:04  sueh
 * <p> bug# 1149 Added getDefaultedInt.
 * <p>
 * <p> Revision 1.59  2008/08/21 00:02:30  sueh
 * <p> bug# 1132 Added multiply().
 * <p>
 * <p> Revision 1.58  2008/04/15 21:01:20  sueh
 * <p> bug# 1105 Changed setDefault(Integer) to setDefault(ConstEtomoNumber).
 * <p>
 * <p> Revision 1.57  2008/04/02 01:59:51  sueh
 * <p> bug# 1097 Added le(int) and newNumberNegate.
 * <p>
 * <p> Revision 1.56  2007/12/13 01:11:54  sueh
 * <p> bug# 1056 Added startArray, to convert integers to/from strings.  Added
 * <p> equalsNameIgnoreCase, to compare to the name.
 * <p>
 * <p> Revision 1.55  2007/11/06 19:35:30  sueh
 * <p> bug# 1047 Added toStringWithLeadingZeros to control that appearance of the
 * <p> number in string form.
 * <p>
 * <p> Revision 1.54  2007/08/21 21:51:56  sueh
 * <p> bug# 771 Added equals(long).
 * <p>
 * <p> Revision 1.53  2007/05/18 23:52:33  sueh
 * <p> bug# 987 Made CpuAdoc thread-safe.  Added minNice.
 * <p>
 * <p> Revision 1.52  2007/05/11 15:50:33  sueh
 * <p> bug# 964 Added lt(ConstEtomoNumber) which returns true if this is less
 * <p> then the parameter.  Added plus(Number,Number) which adds the two
 * <p> parameters together.
 * <p>
 * <p> Revision 1.51  2007/04/26 02:46:04  sueh
 * <p> bug# 964 Added getDefaultValue(), resetDefault(), and setDefault(Integer).
 * <p>
 * <p> Revision 1.50  2007/04/19 21:37:42  sueh
 * <p> bug# 964 Added isDefaultNull() to tell if a defaulted value is null.
 * <p>
 * <p> Revision 1.49  2007/04/13 21:48:36  sueh
 * <p> bug# 964 Added getDefaultedNumber().
 * <p>
 * <p> Revision 1.48  2007/04/13 19:52:46  sueh
 * <p> bug# 964 Added getDefaultedBoolean
 * <p>
 * <p> Revision 1.47  2007/03/30 23:40:34  sueh
 * <p> bug# 964 Made ConstEtomoNumber(Type) work correctly when parameter is null.
 * <p>
 * <p> Revision 1.46  2007/03/26 18:37:07  sueh
 * <p> bug# 964 Changed getDouble(boolean defaultIfNull) to getDefaultDouble() so that
 * <p> the functionality will be remembered and used.
 * <p>
 * <p> Revision 1.45  2007/02/06 19:47:45  sueh
 * <p> bug# 962 Fixed failure in unit test.
 * <p>
 * <p> Revision 1.44  2007/02/05 23:10:02  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.43  2006/11/15 20:43:19  sueh
 * <p> bug# 872 Fixed bug - store and remove are not handling a null prepend.
 * <p>
 * <p> Revision 1.42  2006/10/17 20:04:25  sueh
 * <p> bug# 939  Added defaultValue, getDouble(boolean), getValue(boolean),
 * <p> isDefault(Number), setDefault(int), and useDefaultAsDisplayValue().  Placed the
 * <p> code from initialize(Number) into initialize().
 * <p>
 * <p> Revision 1.41  2006/09/13 23:31:30  sueh
 * <p> bug# 920 Added getFloat().
 * <p>
 * <p> Revision 1.40  2006/08/29 20:04:53  sueh
 * <p> bug# 924 Added static store for storing optional member variables.
 * <p>
 * <p> Revision 1.39  2006/07/19 15:22:44  sueh
 * <p> bug# 903 Added get(int) and get(ConstEtomoNumber).
 * <p>
 * <p> Revision 1.38  2006/06/27 18:30:45  sueh
 * <p> bug# 692 Removed todo comment
 * <p>
 * <p> Revision 1.37  2006/06/22 21:13:58  sueh
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 1.36  2006/06/21 16:50:11  sueh
 * <p> bug# 692 Fix null pointer exception in getInvalidReason().  Add
 * <p> resetInvalidReason() call to setValidFloor().
 * <p>
 * <p> Revision 1.35  2006/06/09 21:53:32  sueh
 * <p> bug# 692 Added resetInvalidReason() calls to setValidValues() and
 * <p> setNullIsValid().  Added debug, which can be set  for an instance and used to
 * <p> debug validation.
 * <p>
 * <p> Revision 1.34  2006/06/08 19:46:21  sueh
 * <p> bug# 692 Changed the float and double null constants to capitals
 * <p>
 * <p> Revision 1.33  2006/06/07 23:50:18  sueh
 * <p> bug# 692 Changed selfTest to internalTest.  Changed setTestCopy to
 * <p> internalTestDeepCopy and stopped calling it from the copy constructor.
 * <p>
 * <p> Revision 1.32  2006/04/06 20:09:48  sueh
 * <p> bug# 808 Fixed a bug: equals(constEtomoNumber) wasn't handling a null
 * <p> ConstEtomoNumber.
 * <p>
 * <p> Revision 1.31  2006/01/27 18:39:06  sueh
 * <p> bug# 801 Added isInt() to get the type of the number.
 * <p>
 * <p> Revision 1.30  2005/10/27 00:31:01  sueh
 * <p> bug# 725 Added newNumber(float) and validateInputType(float).
 * <p>
 * <p> Revision 1.29  2005/07/29 19:46:14  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.28  2005/07/26 23:00:07  sueh
 * <p> bug# 692
 * <p>
 * <p> Revision 1.27  2005/07/21 22:00:51  sueh
 * <p> bug# 532 Added validFloor.
 * <p>
 * <p> Revision 1.26  2005/06/21 16:32:03  sueh
 * <p> bug# 692 Added getType().
 * <p>
 * <p> Revision 1.25  2005/06/20 16:53:11  sueh
 * <p> bug# 692 Moved selftest convenience variable to util.Utilities.  Change
 * <p> validateCopy() to selfTestCopy() because it is just testing the correctness
 * <p> of ConstEtomoNumber code.
 * <p>
 * <p> Revision 1.24  2005/06/16 21:20:31  sueh
 * <p> bug# 692 Fixed validateCopy().
 * <p>
 * <p> Revision 1.23  2005/06/16 20:00:00  sueh
 * <p> bug# 692 Making self test variables boolean instead of EtomoBoolean2 to
 * <p> avoid test problems.  Added validation functions.
 * <p>
 * <p> Revision 1.22  2005/06/14 23:06:34  sueh
 * <p> bug# 687 Fixed toString(Vector), which was only returning the first two
 * <p> elements and repeating the second element.
 * <p>
 * <p> Revision 1.21  2005/06/13 23:36:16  sueh
 * <p> bug# 583 Fixed a bug in newNumber(double) where it was setting a long
 * <p> null value to the int null value.
 * <p>
 * <p> Revision 1.20  2005/06/10 23:18:06  sueh
 * <p> bug# 583 Added member variables:  floorValue, selfTest.  Add functions:
 * <p> isNamed, runSelfTest, selfTest, setFloor.
 * <p>
 * <p> Revision 1.19  2005/05/12 01:27:39  sueh
 * <p> bug# 658 Removed recommendedValue, since it isn't practical to get to
 * <p> information required to set it.  Improved invalidReason messages.  Allow
 * <p> isValid() and validate() to get the field description from the caller.
 * <p>
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

  // null values
  public static final double DOUBLE_NULL_VALUE = Double.NaN;
  public static final float FLOAT_NULL_VALUE = Float.NaN;
  public static final int INTEGER_NULL_VALUE = Integer.MIN_VALUE;
  public static final long LONG_NULL_VALUE = Long.MIN_VALUE;
  // null for types not currently supported
  private static final short shortNullValue = Short.MIN_VALUE;
  private static final byte byteNullValue = Byte.MIN_VALUE;

  // type
  final Type type;// defaults to integer, can't be changed once it is set
  // name and description
  final String name;// defaults to Object.toString(), can't be changed once it is set
  String description;// optional, defaults to name
  // value of the instance
  Number currentValue;// optional, defaults to newNumber()
  // value to display when currentValue isNull()
  Number displayValue;// optional, defaults to newNumber()
  // numbers that can modify currentValue
  Number ceilingValue;// optional, defaults to newNumber()
  Number floorValue;// optional, defaults to newNumber()

  // validatation numbers. Use isValid() or validate() to find out if
  // currentValue is valid.
  // validValues overrides validFloor
  boolean nullIsValid = true;// optional
  // set of valid values
  Vector validValues = null;// optional
  // number below validFloor are invalid
  private Number validFloor;// optional, defaults to newNumber()
  Number defaultValue;
  String[] stringArray = null;

  // internal validation result
  StringBuffer invalidReason = null;
  private boolean debug = false;
  private boolean valueAltered = false;

  /**
   * Construct a ConstEtomoNumber with type = INTEGER_TYPE
   *
   */
  ConstEtomoNumber() {
    type = Type.INTEGER;
    name = super.toString();
    description = name;
    initialize();
  }

  /**
   * Construct a ConstEtomoNumber with type = INTEGER_TYPE
   * @param name the name of the instance
   */
  ConstEtomoNumber(String name) {
    type = Type.INTEGER;
    this.name = name;
    description = name;
    initialize();
  }

  ConstEtomoNumber(Type type) {
    if (type == null) {
      this.type = Type.INTEGER;
    }
    else {
      this.type = type;
    }
    name = super.toString();
    description = name;
    initialize();
  }

  ConstEtomoNumber(Type type, String name) {
    this.type = type;
    this.name = name;
    description = name;
    initialize();
  }

  /**
   * Makes a deep copy of instance.
   * Returns empty instance when instance is null
   * @param that
   */
  ConstEtomoNumber(ConstEtomoNumber instance) {
    if (instance == null) {
      type = Type.INTEGER;
      name = super.toString();
      description = name;
      initialize();
      return;
    }
    type = instance.type;
    // OK to assign Strings because they are immutable
    name = instance.name;
    description = instance.description;
    // OK to assign Numbers because they are immutable
    currentValue = instance.currentValue;
    displayValue = instance.displayValue;
    ceilingValue = instance.ceilingValue;
    floorValue = instance.floorValue;
    validFloor = instance.validFloor;
    nullIsValid = instance.nullIsValid;
    defaultValue = instance.defaultValue;
    if (instance.validValues != null && instance.validValues.size() > 0) {
      validValues = new Vector(instance.validValues.size());
      for (int i = 0; i < instance.validValues.size(); i++) {
        validValues.add(newNumber((Number) instance.validValues.get(i)));
      }
    }
    if (instance.invalidReason != null) {
      invalidReason = new StringBuffer(instance.invalidReason.toString());
    }
  }

  public static boolean isNull(int value) {
    return value == INTEGER_NULL_VALUE;
  }

  public String getDescription() {
    return description;
  }

  public String getName() {
    return name;
  }

  boolean isDebug() {
    return debug;
  }

  public int getDisplayInteger() {
    return displayValue.intValue();
  }

  /**
   * If validValues has been set, look for currentValue in validValues. Set
   * invalidReasion if  currentValue is not found.  Null is ignored.
   * Set invalidReason if currentValue is null and nullIsValid is false.
   */
  void setInvalidReason() {
    // Pass when there are no validation settings
    if (nullIsValid && validValues == null && isNull(validFloor)) {
      return;
    }
    // Catch illegal null values
    if (isNull(currentValue)) {
      if (nullIsValid) {
        return;
      }
      addInvalidReason("This field cannot be empty.");
    }
    // Validate against validValues, overrides validFloor
    else if (validValues != null) {
      for (int i = 0; i < validValues.size(); i++) {
        if (equals(currentValue, (Number) validValues.get(i))) {
          return;
        }
      }
      addInvalidReason(toString(currentValue) + " is not a valid value.");
      addInvalidReason("Valid values are " + toString(validValues) + ".");
      return;
    }
    // If validValues is not set, validate against validFloor
    else if (!isNull(validFloor)) {
      if (ge(currentValue, validFloor)) {
        return;
      }
      addInvalidReason(toString(currentValue) + " is not a valid value.");
      addInvalidReason("Valid values are greater or equal to " + toString(validFloor)
          + ".");
    }
  }

  /**
   * Returns ceilingValue if value > ceilingValue.
   * Otherwise returns value.
   * Ignores null
   * @param value
   * @return
   */
  Number applyCeilingValue(Number value) {
    if (value != null && !isNull(ceilingValue) && !isNull(value)
        && gt(value, ceilingValue)) {
      valueAltered = true;
      return newNumber(ceilingValue);
    }
    return value;
  }

  /**
   * Returns floorValue if value < floorValue.  Otherwise returns values.
   * Ignores null.
   * @param value
   * @return
   */
  Number applyFloorValue(Number value) {
    if (value != null && !isNull(floorValue) && !isNull(value) && lt(value, floorValue)) {
      valueAltered = true;
      return newNumber(floorValue);
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
   * Returns true if the value that was set by another class has been altered internally
   * by this class (or EtomoNumber).  This functionality currently ignores alteration
   * because of an unparsable value.  When the ceiling and floor functionality cause the
   * externally set value to change, this function will return true.
   * @return true if the value that was set has been altered internally
   */
  public boolean isValueAltered() {
    return valueAltered;
  }

  /**
   * If invalidReason is set, display an error message and return true
   * @param displayErrorMessage
   * @param errorTitle
   * @param description
   * @return error message if invalidReason is set, null if valid
   */
  public String validate(String description) {
    if (invalidReason != null) {
      if (description == null) {
        return this.description + ": " + invalidReason;
      }
      description = description.trim();
      if (description.endsWith(":")) {
        return description + "  " + invalidReason;

      }
      return description + ":  " + invalidReason;
    }
    return null;
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  public String getInvalidReason() {
    if (invalidReason == null) {
      return "";
    }
    return invalidReason.toString();
  }

  public String classInfoString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  String paramString() {
    StringBuffer buffer = new StringBuffer(",\ntype=" + type + ",\nname=" + name
        + ",\ndescription=" + description + ",\ninvalidReason=" + invalidReason
        + ",\ncurrentValue=" + currentValue + ",\ndisplayValue=" + displayValue
        + ",\nceilingValue=" + ceilingValue + ",\nfloorValue=" + floorValue
        + ",\nnullIsValid=" + nullIsValid + ",\nvalidValues=");
    if (validValues == null) {
      buffer.append("null");
    }
    else {
      buffer.append(validValues.toString());
    }
    return buffer.toString();
  }

  /**
   * Sets ceiling value.  Ceiling value is applied when the value is set; if the
   * value being set is more then the ceiling value, the value is set equals to
   * the ceiling value.
   * @param ceilingValue
   * @return
   */
  public ConstEtomoNumber setCeiling(int ceilingValue) {
    this.ceilingValue = newNumber(ceilingValue);
    validateFloorAndCeiling();
    currentValue = applyCeilingValue(currentValue);
    return this;
  }

  /**
   * Sets floor value.  Floor value is applied when the value is set; if the
   * value being set is less then the floor value, the value is set equals to
   * the floor value.
   * @param floorValue
   * @return
   */
  public ConstEtomoNumber setFloor(int floorValue) {
    this.floorValue = newNumber(floorValue);
    validateFloorAndCeiling();
    currentValue = applyFloorValue(currentValue);
    return this;
  }

  void internalTest() {
    if (!Utilities.isSelfTest()) {
      return;
    }
    // Name should never be null.
    if (name == null) {
      throw new IllegalStateException("name cannot be null.");
    }
    // currentValue should never be null
    if (currentValue == null) {
      throw new IllegalStateException("currentValue cannot be null.");
    }
    // displayValue should never be null
    if (displayValue == null) {
      throw new IllegalStateException("displayValue cannot be null.");
    }
    // ceilingValue should never be null
    if (ceilingValue == null) {
      throw new IllegalStateException("ceilingValue cannot be null.");
    }
    // floorValue should never be null
    if (floorValue == null) {
      throw new IllegalStateException("floorValue cannot be null.");
    }
    // validFloor should never be null
    if (validFloor == null) {
      throw new IllegalStateException("validFloor cannot be null.");
    }
    // defaultValue should never be null
    if (defaultValue == null) {
      throw new IllegalStateException("defaultValue cannot be null.");
    }
    // Type should be either double, integer, or long.
    if (type != Type.DOUBLE && type != Type.INTEGER && type != Type.LONG) {
      throw new IllegalStateException("type is not valid.  type=" + type);
    }
    // Type constants must be unique.
    if (Type.DOUBLE == Type.INTEGER || Type.DOUBLE == Type.LONG
        || Type.INTEGER == Type.LONG) {
      throw new IllegalStateException("Type constants are the same.\nDOUBLE_TYPE="
          + Type.DOUBLE + ",INTEGER_TYPE=" + Type.INTEGER + ",LONG_TYPE=" + Type.LONG);
    }
    // All members of type Number must be created with the current type
    // The type should be set only once.
    if (type == Type.DOUBLE) {
      if (!(currentValue instanceof Double)) {
        throw new IllegalStateException(
            "currentValue doesn't match the current type.  currentValue.getClass()="
                + currentValue.getClass() + ",type=" + type);
      }
      if (!(displayValue instanceof Double)) {
        throw new IllegalStateException(
            "displayValue doesn't match the current type.  displayValue.getClass()="
                + displayValue.getClass() + ",type=" + type);
      }
      if (!(ceilingValue instanceof Double)) {
        throw new IllegalStateException(
            "ceilingValue doesn't match the current type.  ceilingValue.getClass()="
                + ceilingValue.getClass() + ",type=" + type);
      }
      if (!(floorValue instanceof Double)) {
        throw new IllegalStateException(
            "floorValue doesn't match the current type.  floorValue.getClass()="
                + floorValue.getClass() + ",type=" + type);
      }
      if (!(validFloor instanceof Double)) {
        throw new IllegalStateException(
            "validFloor doesn't match the current type.  validFloor.getClass()="
                + validFloor.getClass() + ",type=" + type);
      }
      if (!(defaultValue instanceof Double)) {
        throw new IllegalStateException(
            "defaultValue doesn't match the current type.  defaultValue.getClass()="
                + defaultValue.getClass() + ",type=" + type);
      }
    }
    if (type == Type.INTEGER) {
      if (!(currentValue instanceof Integer)) {
        throw new IllegalStateException(
            "currentValue doesn't match the current type.  currentValue.getClass()="
                + currentValue.getClass() + ",type=" + type);
      }
      if (!(displayValue instanceof Integer)) {
        throw new IllegalStateException(
            "displayValue doesn't match the current type.  displayValue.getClass()="
                + displayValue.getClass() + ",type=" + type);
      }
      if (!(ceilingValue instanceof Integer)) {
        throw new IllegalStateException(
            "ceilingValue doesn't match the current type.  ceilingValue.getClass()="
                + ceilingValue.getClass() + ",type=" + type);
      }
      if (!(floorValue instanceof Integer)) {
        throw new IllegalStateException(
            "floorValue doesn't match the current type.  floorValue.getClass()="
                + floorValue.getClass() + ",type=" + type);
      }
      if (!(validFloor instanceof Integer)) {
        throw new IllegalStateException(
            "validFloor doesn't match the current type.  validFloor.getClass()="
                + validFloor.getClass() + ",type=" + type);
      }
      if (!(defaultValue instanceof Integer)) {
        throw new IllegalStateException(
            "defaultValue doesn't match the current type.  defaultValue.getClass()="
                + defaultValue.getClass() + ",type=" + type);
      }
    }
    if (type == Type.LONG) {
      if (!(currentValue instanceof Long)) {
        throw new IllegalStateException(
            "currentValue doesn't match the current type.  currentValue.getClass()="
                + currentValue.getClass() + ",type=" + type);
      }
      if (!(displayValue instanceof Long)) {
        throw new IllegalStateException(
            "displayValue doesn't match the current type.  displayValue.getClass()="
                + displayValue.getClass() + ",type=" + type);
      }
      if (!(ceilingValue instanceof Long)) {
        throw new IllegalStateException(
            "ceilingValue doesn't match the current type.  ceilingValue.getClass()="
                + ceilingValue.getClass() + ",type=" + type);
      }
      if (!(floorValue instanceof Long)) {
        throw new IllegalStateException(
            "floorValue doesn't match the current type.  floorValue.getClass()="
                + floorValue.getClass() + ",type=" + type);
      }
      if (!(validFloor instanceof Long)) {
        throw new IllegalStateException(
            "validFloor doesn't match the current type.  validFloor.getClass()="
                + validFloor.getClass() + ",type=" + type);
      }
      if (!(defaultValue instanceof Long)) {
        throw new IllegalStateException(
            "defaultValue doesn't match the current type.  defaultValue.getClass()="
                + defaultValue.getClass() + ",type=" + type);
      }
    }
    // floorValue <= ceilingValue
    validateFloorAndCeiling();
    // valid validValues
    if (validValues != null) {
      for (int i = 0; i < validValues.size(); i++) {
        if (isNull((Number) validValues.get(i))) {
          throw new IllegalStateException("ValidValues elements cannot be null.");
        }
      }
    }
    // test deep copy on assignment
    if (currentValue == displayValue) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  currentValue, displayValue");
    }
    if (currentValue == ceilingValue) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  currentValue, ceilingValue");
    }
    if (currentValue == floorValue) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  currentValue, floorValue.");
    }
    if (currentValue == validFloor) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  currentValue, validFloor.");
    }
    if (displayValue == ceilingValue) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  displayValue, ceilingValue");
    }
    if (displayValue == floorValue) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  displayValue, floorValue.");
    }
    if (displayValue == validFloor) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  displayValue, validFloor.");
    }
    if (ceilingValue == floorValue) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  ceilingValue, floorValue.");
    }
    if (ceilingValue == validFloor) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  ceilingValue, validFloor.");
    }
    if (floorValue == validFloor) {
      throw new IllegalStateException(
          "newNumber() was not use in an assignment:  floorValue, validFloor.");
    }
  }

  /**
   * Set the value will be returned if the user does not set a value or there is no
   * value to load.
   * @param displayValue
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

  ConstEtomoNumber setDisplayValue(Number displayValue) {
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
    this.displayValue = newNumber(displayValue);
  }

  public void setDisplayValue(long displayValue) {
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
    resetState();
    this.nullIsValid = nullIsValid;
    setInvalidReason();
    return this;
  }

  /**
   * Set a list of non-null valid values
   * A null param or an empty list causes this.validValues to be set to null.
   * @param validValues
   * @return
   */
  public ConstEtomoNumber setValidValues(int[] validValues) {
    resetState();
    if (validValues == null || validValues.length == 0) {
      this.validValues = null;
    }
    else {
      this.validValues = new Vector(validValues.length);
      for (int i = 0; i < validValues.length; i++) {
        int validValue = validValues[i];
        if (!isNull(validValue)) {
          this.validValues.add(this.newNumber(validValues[i]));
        }
      }
    }
    setInvalidReason();
    return this;
  }

  /**
   * Sets valid floor.  Valid floor does not change a value being set.  It
   * causes validation to fail if the current value is less then the valid
   * floor.
   * @param validFloor
   * @return
   */
  public ConstEtomoNumber setValidFloor(int validFloor) {
    resetState();
    this.validFloor = newNumber(validFloor);
    setInvalidReason();
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
    if (prepend == null || prepend.matches("\\s*")) {
      store(props);
    }
    else {
      props.setProperty(prepend + "." + name, toString(currentValue));
    }
  }

  public static void store(EtomoNumber etomoNumber, String name, Properties props,
      String prepend) {
    if (etomoNumber == null) {
      props.remove(prepend + '.' + name);
      return;
    }
    etomoNumber.store(props, prepend);
  }

  public void remove(Properties props) {
    props.remove(name);
  }

  public void remove(Properties props, String prepend) {
    if (prepend == null || prepend.matches("\\s*")) {
      remove(props);
    }
    else {
      props.remove(prepend + "." + name);
    }
  }

  /**
   * Sets an instance with a string array so numbers can be
   * translated to and from strings.  No effect if the type isn't integer.
   * Use with setValidValues and setDisplayValue to create a boolean that can be
   * null.
   * @param input - used to set stringArray
   */
  public void setStrings(String[] input) {
    if (type != Type.INTEGER) {
      return;
    }
    if (input == null || input.length == 0) {
      stringArray = null;
    }
    stringArray = new String[input.length];
    for (int i = 0; i < input.length; i++) {
      stringArray[i] = input[i];
    }
  }

  /**
   * Must show the string version of getValue();
   */
  public String toString() {
    return toString(getValue());
  }

  /**
   * Add leading zeros to the number up to maxZeros.  If the size of the number
   * (or the size of the number left of the decimal if the type is float or
   * double) is greater or equal to maxZeros then nothing is done.
   * Examples:
   * If the number is 1 and maxZeros is 3, returns 001
   * If the number is .08 and maxZeros is 1, return 0.08
   * @param maxZeros
   * @return
   
   public String toStringWithLeadingZeros(int maxZeros) {
   String wholeNumbers = toString();
   int decimalPlace = wholeNumbers.indexOf('.');
   if (decimalPlace != -1) {
   wholeNumbers = wholeNumbers.substring(0, decimalPlace);
   }
   int length = wholeNumbers.length();
   if (length > maxZeros || length == maxZeros) {
   return toString();
   }
   StringBuffer retval = new StringBuffer();
   for (int i = 0; i < maxZeros - length; i++) {
   retval.append("0");
   }
   retval.append(toString());
   return retval.toString();
   }*/

  /**
   * If default is set and isNull() is true, defaultValue will be returned, even
   * if displayValue is set.  If defaultValue is not set, or isNull() is false,
   * then it works the same as setValue().
   * @return
   */
  public String toDefaultedString() {
    return toString(getDefaultedValue());
  }

  public Number getDefaultValue() {
    return defaultValue;
  }

  public int getInt() {
    return getValue().intValue();
  }

  public boolean is() {
    if (isNull() || equals(0)) {
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

  /**
   * If default is set and isNull() is true, defaultValue will be returned, even
   * if displayValue is set.  If defaultValue is not set, or isNull() is false,
   * then it works the same as setValue().
   * @return
   */
  public double getDefaultedDouble() {
    return getDefaultedValue().doubleValue();
  }

  public int getDefaultedInt() {
    return getDefaultedValue().intValue();
  }

  public ConstEtomoNumber setDefault(int input) {
    defaultValue = newNumber(input);
    return this;
  }

  public void setDefault(ConstEtomoNumber input) {
    if (input == null) {
      defaultValue = newNumber();
    }
    else {
      defaultValue = input.getNumber();
    }
  }

  public ConstEtomoNumber setDefault(boolean defaultValue) {
    this.defaultValue = newNumber(defaultValue);
    return this;
  }

  /**
   * Returns true if currentValue is not null and is equal to defaultValue.
   * This function is not effected by displayValue.
   * @return
   */
  public boolean isDefault() {
    return isDefault(currentValue);
  }

  public boolean isDefaultSet() {
    return !isNull(defaultValue);
  }

  /**
   * Returns true if defaultValue is not null and value is equal to
   * defaultValue.
   * @return
   */
  boolean isDefault(Number value) {
    if (isNull(defaultValue)) {
      return false;
    }
    return equals(value, defaultValue);
  }

  public ConstEtomoNumber useDefaultAsDisplayValue() {
    return setDisplayValue(defaultValue);
  }

  public Number getNumber() {
    return newNumber(getValue());
  }

  public boolean equalsNameIgnoreCase(String input) {
    return name.compareToIgnoreCase(input) == 0;
  }

  /**
   * Returns true if getValue() equals that.getValue().
   * @param that
   * @return
   */
  public boolean equals(ConstEtomoNumber that) {
    if (that == null) {
      return false;
    }
    return equals(getValue(), that.getValue());
  }

  public boolean gt(int value) {
    return gt(getValue(), value);
  }

  public boolean lt(int value) {
    return lt(getValue(), value);
  }

  public boolean ge(long value) {
    return ge(getValue(), value);
  }

  public boolean le(int value) {
    Number v = newNumber(value);
    return lt(getValue(), v) || equals(getValue(), v);
  }

  public boolean le(ConstEtomoNumber value) {
    return le(getValue(), value.getNumber());
  }

  public boolean ge(ConstEtomoNumber value) {
    return ge(getValue(), value.getNumber());
  }

  public boolean gt(ConstEtomoNumber etomoNumber) {
    if (etomoNumber == null) {
      return false;
    }
    return gt(getValue(), etomoNumber.getValue());
  }

  public boolean lt(ConstEtomoNumber etomoNumber) {
    if (etomoNumber == null) {
      return false;
    }
    return lt(getValue(), etomoNumber.getValue());
  }

  public boolean equals(int value) {
    return equals(getValue(), value);
  }

  public boolean equals(long value) {
    return equals(getValue(), value);
  }

  public boolean equals(double value) {
    return equals(getValue(), value);
  }

  /**
   * Returns true if currentValue is null.  IsNull() does not use getValue() and
   * ignores displayValue, so it shows whether the instance has been explicitely
   * set.
   * @return
   */
  public boolean isNull() {
    return isNull(currentValue);
  }

  public boolean equals(Number value) {
    return equals(getValue(), value);
  }

  public boolean equals(String value) {
    return equals(getValue(), newNumber(value, new StringBuffer()));
  }

  public boolean isNamed(String name) {
    return this.name.equals(name);
  }

  public final boolean isInt() {
    return type == Type.INTEGER;
  }

  private void initialize() {
    ceilingValue = newNumber();
    floorValue = newNumber();
    this.displayValue = newNumber();
    currentValue = newNumber();
    validFloor = newNumber();
    defaultValue = newNumber();
  }

  /**
   * If the currentValue is not null, returns it.  If the currentValue is null,
   * returns the displayValue.  So, if the displayValue is null also, it returns
   * null.
   * @return
   */
  Number getValue() {
    if (!isNull(currentValue)) {
      return currentValue;
    }
    return displayValue;
  }

  public boolean isDefaultedNull() {
    return isNull(getDefaultedValue());
  }

  /**
   * If default is set and isNull() is true, defaultValue will be returned, even
   * if displayValue is set.  If defaultValue is not set, or isNull() is false,
   * then it works the same as setValue().
   * @return
   */
  Number getDefaultedValue() {
    if (isDefaultSet() && isNull()) {
      return defaultValue;
    }
    return getValue();
  }

  public Number getDefaultedNumber() {
    return newNumber(getDefaultedValue());
  }

  public Number getNegatedDefaultedNumber() {
    return negate(getDefaultedValue());
  }

  public boolean getDefaultedBoolean() {
    Number value = getDefaultedValue();
    if (isNull(value) || equals(value, newNumber(0))) {
      return false;
    }
    return true;
  }

  String toString(Number value) {
    if (isNull(value)) {
      return "";
    }
    if (type == Type.INTEGER && stringArray != null) {
      int index = value.intValue();
      if (index > 0 && index < stringArray.length) {
        return stringArray[index];
      }
    }
    return value.toString();
  }

  private String toString(Vector numberVector) {
    if (numberVector == null || numberVector.size() == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(toString((Number) numberVector.get(0)));
    for (int i = 1; i < numberVector.size(); i++) {
      buffer.append("," + toString((Number) numberVector.get(i)));
    }
    return buffer.toString();
  }

  void resetState() {
    invalidReason = null;
    valueAltered = false;
  }

  void addInvalidReason(String message) {
    if (message == null) {
      return;
    }
    if (invalidReason == null) {
      invalidReason = new StringBuffer(message);
    }
    else {
      invalidReason.append("\n" + message);
    }
  }

  Number newNumber() {
    if (type == Type.DOUBLE) {
      return new Double(DOUBLE_NULL_VALUE);
    }
    if (type == Type.INTEGER) {
      return new Integer(INTEGER_NULL_VALUE);
    }
    if (type == Type.LONG) {
      return new Long(LONG_NULL_VALUE);
    }
    throw new IllegalStateException("type=" + type);
  }

  /**
   * Creates a new number based on the type member variable.
   * @param value
   * @return
   */
  Number newNumber(Number value) {
    if (value == null || isNull(value)) {
      return newNumber();
    }
    if (type == Type.DOUBLE) {
      return new Double(value.doubleValue());
    }
    if (type == Type.INTEGER) {
      return new Integer(value.intValue());
    }
    if (type == Type.LONG) {
      return new Long(value.longValue());
    }
    throw new IllegalStateException("type=" + type);
  }

  /**
   * Override this class to display numbers as descriptive character strings.
   * @param value
   * @param invalidBuffer
   * @return
   */
  Number newNumber(String value, StringBuffer invalidBuffer) {
    if (value == null) {
      return newNumber();
    }
    value = value.trim();
    if (value.equals("")) {
      return newNumber();
    }
    try {
      if (type == Type.DOUBLE) {
        return new Double(value);
      }
      if (type == Type.INTEGER) {
        return new Integer(value);
      }
      if (type == Type.LONG) {
        return new Long(value);
      }
      throw new IllegalStateException("type=" + type);
    }
    catch (NumberFormatException e) {
      invalidBuffer.append(value + " is not a valid " + type + ".  " + e.getMessage());
      // TODO should the invalid string be stored somewhere?
      return newNumber();
    }
  }

  Number newNumber(int value) {
    if (type == Type.DOUBLE) {
      return new Double(value);
    }
    if (type == Type.INTEGER) {
      return new Integer(value);
    }
    if (type == Type.LONG) {
      return new Long(value);
    }
    throw new IllegalStateException("type=" + type);
  }

  Number newNumber(boolean value) {
    if (value) {
      return newNumber(1);
    }
    return newNumber(0);
  }

  Number newNumber(double value) {
    if (Double.isNaN(value)) {
      return newNumber();
    }
    if (type == Type.DOUBLE) {
      return new Double(value);
    }
    if (type == Type.INTEGER) {
      return new Integer((int) value);
    }
    if (type == Type.LONG) {
      return new Long((long) value);
    }
    throw new IllegalStateException("type=" + type);
  }

  Number newNumber(long value) {
    if (value == LONG_NULL_VALUE) {
      return newNumber();
    }
    if (type == Type.DOUBLE) {
      return new Double(value);
    }
    if (type == Type.INTEGER) {
      return new Integer((int) value);
    }
    if (type == Type.LONG) {
      return new Long(value);
    }
    throw new IllegalStateException("type=" + type);
  }

  Number newNumber(float value) {
    if (value == LONG_NULL_VALUE) {
      return newNumber();
    }
    if (type == Type.DOUBLE) {
      return new Double(value);
    }
    if (type == Type.INTEGER) {
      return new Integer((int) value);
    }
    if (type == Type.LONG) {
      return new Long((long) value);
    }
    throw new IllegalStateException("type=" + type);
  }

  boolean isNull(Number number) {
    if (number == null) {
      return true;
    }
    if (number instanceof Byte) {
      return number.byteValue() == byteNullValue;
    }
    if (number instanceof Double) {
      return Double.isNaN(number.doubleValue());
    }
    if (number instanceof Float) {
      return Float.isNaN(number.floatValue());
    }
    if (number instanceof Integer || number instanceof AtomicInteger) {
      return isNull(number.intValue());
    }
    if (number instanceof Long || number instanceof AtomicLong) {
      return number.longValue() == LONG_NULL_VALUE;
    }
    if (number instanceof Short) {
      return number.shortValue() == shortNullValue;
    }
    throw new IllegalStateException("Unknown type.  value.getClass()="
        + number.getClass());
  }

  boolean gt(Number number1, Number number2) {
    return compare(Comparison.GT, number1, number2);
  }

  boolean ge(Number number1, Number number2) {
    return compare(Comparison.GE, number1, number2);
  }

  boolean lt(Number number1, Number number2) {
    return compare(Comparison.LT, number1, number2);
  }

  boolean le(Number number1, Number number2) {
    return compare(Comparison.LE, number1, number2);
  }

  boolean equals(Number number1, Number number2) {
    return compare(Comparison.EQUALS, number1, number2);
  }

  Number add(Number number1, Number number2) {
    return doMath(Operator.ADD, number1, number2);
  }

  /**
   * If one of the numbers is null, the result is null.
   * @param number1
   * @param number2
   * @return
   */
  Number multiply(Number number1, Number number2) {
    return doMath(Operator.MULTIPLY, number1, number2);
  }

  /**
   * If one of the numbers is null, the result is null.
   * @param number1
   * @param number2
   * @return
   */
  Number divideBy(Number number1, Number number2) {
    return doMath(Operator.DIVIDE_BY, number1, number2);
  }

  Number negate(Number number) {
    return doMath(Operator.MULTIPLY, number, -1);
  }

  Type getType() {
    return type;
  }

  /**
   * Validation for floor and ceiling
   *
   */
  private void validateFloorAndCeiling() {
    // if floorValue and ceilingValue are both used, then floorValue must be less
    // then or equal to ceilingValue.
    if (!isNull(ceilingValue) && !isNull(floorValue) && gt(floorValue, ceilingValue)) {
      throw new IllegalStateException(
          "FloorValue cannot be greater then ceilingValue.\nfloorValue=" + floorValue
              + ", ceilingValue=" + ceilingValue);
    }
  }

  /**
   * Check that a deep copy was done and that all data was copied.
   * Use self test on this because, if one copy works, they all should work.
   * @param original
   */
  void internalTestDeepCopy(ConstEtomoNumber original) {
    if (!Utilities.isSelfTest()) {
      return;
    }
    if (original == null) {
      return;
    }
    // deep copy test
    // ok for name to be the same instance because it is final
    if (this == original) {
      throw new IllegalStateException("Incorrect copy: was not a deep copy");
    }
    // equality test
    if (type != original.type
        || !name.equals(original.name)
        || !description.equals(original.description)
        || !equals(currentValue, original.currentValue)
        || !equals(displayValue, original.displayValue)
        || !equals(ceilingValue, original.ceilingValue)
        || !equals(floorValue, original.floorValue)
        || !equals(validFloor, original.validFloor)
        || !equals(defaultValue, original.defaultValue)
        || (validValues != null && original.validValues == null)
        || (validValues == null && original.validValues != null)
        || (validValues != null && original.validValues != null && !validValues
            .equals(original.validValues))
        || nullIsValid != original.nullIsValid
        || (invalidReason != null && original.invalidReason == null)
        || (invalidReason == null && original.invalidReason != null)
        || (invalidReason != null && original.invalidReason != null && !invalidReason
            .toString().equals(original.invalidReason.toString()))) {
      throw new IllegalStateException("Incorrect copy: this=" + this.classInfoString()
          + ",original=" + original.classInfoString());
    }
  }

  private boolean compare(final Comparison comparison, final Number number1,
      final Number number2) {
    if (comparison == Comparison.EQUALS && isNull(number1) && isNull(number2)) {
      return true;
    }
    if (isNull(number1) || isNull(number2)) {
      return false;
    }
    // Avoid casting double to/from float because it causes floating point errors
    if (number1 instanceof Double) {
      if (number2 instanceof Double) {
        return compare(comparison, number1.doubleValue(), number2.doubleValue());
      }
      if (number2 instanceof Float) {
        return compare(comparison, number1.doubleValue(), number2.floatValue());
      }
      return compare(comparison, number1.doubleValue(), number2.longValue());
    }
    if (number1 instanceof Float) {
      if (number2 instanceof Double) {
        return compare(comparison, number1.floatValue(), number2.doubleValue());
      }
      if (number2 instanceof Float) {
        return compare(comparison, number1.floatValue(), number2.floatValue());
      }
      return compare(comparison, number1.floatValue(), number2.longValue());
    }
    if (number2 instanceof Double) {
      return compare(comparison, number1.longValue(), number2.doubleValue());
    }
    if (number2 instanceof Float) {
      return compare(comparison, number1.longValue(), number2.floatValue());
    }
    return compare(comparison, number1.longValue(), number2.longValue());
  }

  private Number doMath(final Operator operator, final Number number1,
      final Number number2) {
    boolean null1 = false;
    boolean null2 = false;
    if ((null1 = isNull(number1)) || (null2 = isNull(number2))) {
      if (operator == Operator.MULTIPLY || operator == Operator.DIVIDE_BY) {
        return newNumber();
      }
      if (operator == Operator.ADD) {
        if (null1 && null2) {
          return newNumber();
        }
        if (null1) {
          return newNumber(number2);
        }
        if (null2) {
          return newNumber(number1);
        }
      }
    }
    // Avoid casting double to/from float because it causes floating point errors
    if (number1 instanceof Double) {
      if (number2 instanceof Double) {
        return doMath(operator, number1.doubleValue(), number2.doubleValue());
      }
      if (number2 instanceof Float) {
        return doMath(operator, number1.doubleValue(), number2.floatValue());
      }
      return doMath(operator, number1.doubleValue(), number2.longValue());
    }
    if (number1 instanceof Float) {
      if (number2 instanceof Double) {
        return doMath(operator, number1.floatValue(), number2.doubleValue());
      }
      if (number2 instanceof Float) {
        return doMath(operator, number1.floatValue(), number2.floatValue());
      }
      return doMath(operator, number1.floatValue(), number2.longValue());
    }
    if (number2 instanceof Double) {
      return doMath(operator, number1.longValue(), number2.doubleValue());
    }
    if (number2 instanceof Float) {
      return doMath(operator, number1.longValue(), number2.floatValue());
    }
    return doMath(operator, number1.longValue(), number2.longValue());
  }

  private Number doMath(final Operator operator, final double number1,
      final double number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private Number doMath(final Operator operator, final double number1, final float number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private Number doMath(final Operator operator, final double number1, final long number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private Number doMath(final Operator operator, final float number1, final double number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private Number doMath(final Operator operator, final float number1, final float number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private Number doMath(final Operator operator, final float number1, final long number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private Number doMath(final Operator operator, final long number1, final double number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private Number doMath(final Operator operator, final long number1, final float number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private Number doMath(final Operator operator, final long number1, final long number2) {
    if (operator == Operator.ADD) {
      return newNumber(number1 + number2);
    }
    if (operator == Operator.MULTIPLY) {
      return newNumber(number1 * number2);
    }
    if (operator == Operator.DIVIDE_BY) {
      return newNumber(number1 / number2);
    }
    return newNumber();
  }

  private boolean compare(final Comparison comparison, final double number1,
      final double number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  private boolean compare(final Comparison comparison, final double number1,
      final float number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  private boolean compare(final Comparison comparison, final double number1,
      final long number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  private boolean compare(final Comparison comparison, final float number1,
      final double number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  private boolean compare(final Comparison comparison, final float number1,
      final float number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  private boolean compare(final Comparison comparison, final float number1,
      final long number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  private boolean compare(final Comparison comparison, final long number1,
      final double number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  private boolean compare(final Comparison comparison, final long number1,
      final float number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  private boolean compare(final Comparison comparison, final long number1,
      final long number2) {
    if (comparison == Comparison.GT) {
      return number1 > number2;
    }
    if (comparison == Comparison.GE) {
      return number1 >= number2;
    }
    if (comparison == Comparison.LT) {
      return number1 < number2;
    }
    if (comparison == Comparison.LE) {
      return number1 <= number2;
    }
    if (comparison == Comparison.EQUALS) {
      return number1 == number2;
    }
    return false;
  }

  public static final class Type {
    public static final Type DOUBLE = new Type();
    public static final Type INTEGER = new Type();
    public static final Type LONG = new Type();

    public static Type getDefault() {
      return INTEGER;
    }

    public String toString() {
      if (this == DOUBLE) {
        return "Double";
      }
      if (this == INTEGER) {
        return "Integer";
      }
      if (this == LONG) {
        return "Long";
      }
      return "Unknown Type";
    }
  }

  private static final class Comparison {
    private static final Comparison GT = new Comparison();
    private static final Comparison GE = new Comparison();
    private static final Comparison LT = new Comparison();
    private static final Comparison LE = new Comparison();
    private static final Comparison EQUALS = new Comparison();

    private Comparison() {
    }
  }

  private static final class Operator {
    private static final Operator ADD = new Operator();
    private static final Operator MULTIPLY = new Operator();
    private static final Operator DIVIDE_BY = new Operator();

    private Operator() {
    }
  }
}