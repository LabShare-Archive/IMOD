package etomo.type;

import java.util.Properties;

import etomo.comscript.FortranInputString;

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
 * <p> Revision 1.25  2011/02/22 05:38:26  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.24  2010/03/09 01:41:00  sueh
 * <p> bug# 1323 In load(Properties,String) handling a prepend which ends with ".".
 * <p>
 * <p> Revision 1.23  2009/09/22 21:01:36  sueh
 * <p> bug# 1259 Commented multiply.
 * <p>
 * <p> Revision 1.22  2009/09/01 02:48:05  sueh
 * <p> bug# 1222 In load functions improved checking for an empty prepend.
 * <p>
 * <p> Revision 1.21  2008/10/27 18:38:15  sueh
 * <p> bug# 1141 Added debug only print statements.
 * <p>
 * <p> Revision 1.20  2008/08/21 00:02:42  sueh
 * <p> bug# 1132 Added multiply.
 * <p>
 * <p> Revision 1.19  2008/04/02 02:00:29  sueh
 * <p> bug# 1097 Changed plus to add.
 * <p>
 * <p> Revision 1.18  2007/12/13 01:12:42  sueh
 * <p> bug# 1056 Added startArray, to convert integers to/from strings.  Added loadWithAlternateKey.
 * <p>
 * <p> Revision 1.17  2007/11/06 19:36:41  sueh
 * <p> bug# 1047 Added loadIfPresent() and load with a default.
 * <p>
 * <p> Revision 1.16  2007/05/11 15:58:10  sueh
 * <p> bug# 964 Added plus(ConstEtomoNumber) which adds the parametes to
 * <p> currentValue.
 * <p>
 * <p> Revision 1.15  2007/02/05 23:25:53  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.14  2006/11/15 20:45:59  sueh
 * <p> bug# 872 Fixed bug - load are not handling a null prepend.
 * <p>
 * <p> Revision 1.13  2006/08/29 20:06:30  sueh
 * <p> bug# 924 Added a static load, for loading into an optional parameter
 * <p>
 * <p> Revision 1.12  2005/10/27 00:32:51  sueh
 * <p> bug# 725 Added set(float).
 * <p>
 * <p> Revision 1.11  2005/06/16 20:07:16  sueh
 * <p> bug# 692 Make integer the default type.  Move validation to
 * <p> ConstEtomoNumber.
 * <p>
 * <p> Revision 1.10  2005/06/11 02:33:55  sueh
 * <p> bug# 583 Added member variable:  floor.  Simplified load functions.
 * <p>
 * <p> Revision 1.9  2005/05/10 02:56:21  sueh
 * <p> bug# 658 Removed preventNullValue because just setting displayValue
 * <p> does the same thing.  Use resetInvalidReason() and addInvalidReason()
 * <p> instead of setting invalidReason directly.  Add set(FortranInputString, int)
 * <p> to set currentValue to the value of an element of FortranInputString.
 * <p>
 * <p> Revision 1.8  2005/01/25 23:52:46  sueh
 * <p> Pulling the part of EtomoNumber which handles reading and writing to
 * <p> scripts out of this class and placing it in ScriptParameter, which inherits
 * <p> this class.  Moving defaultValue to ScriptParameter.  Changing reset so that
 * <p> is always sets currentValue to null, unless preventNullValues is true.
 * <p>
 * <p> Revision 1.7  2005/01/22 04:12:44  sueh
 * <p> bug# 509, bug# 591  In set(long) and set(double), and setResetValue(double)
 * <p> through an exception if the type of the instance is not compatible.
 * <p>
 * <p> Revision 1.6  2005/01/21 23:26:02  sueh
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
 * <p> Removed initialValue from constructor.  Added functionality to optionally
 * <p> prevent the setting of null values (changed initialize() and newNumber().
 * <p>
 * <p> Revision 1.5  2005/01/14 23:04:35  sueh
 * <p> Changing the name of set(ComScriptCommand) to parse.  Handle missing
 * <p> keyword by setting value to null.
 * <p>
 * <p> Revision 1.4  2005/01/10 23:31:00  sueh
 * <p> bug# 578 Standardized class so that every use of value goes through
 * <p> getValue().  GetValue() tries to find a non-null value by looking first at
 * <p> value, then resetValue, and then defaultValue (if displayDefault is set).
 * <p> Replacing isNull() with isSet().  IsSet() does not use getValue(), since it is
 * <p> querying whether the value was set (by set(), resetValue() with a non-null
 * <p> resetValue, or with an initialValue).  Added a new isNull() that uses
 * <p> getValue().  Also added is() to ConstEtomoNumber.  Add added
 * <p> toString(Number) and toString(Vector) to ConstEtomoNumber.
 * <p> toString(Number) was created to be overidden by EtomoState.  In this
 * <p> class it wraps Number.toString().
 * <p>
 * <p> Revision 1.3  2004/12/29 00:07:40  sueh
 * <p> bug# 567 Added set(ComScriptCommand) to get the value in
 * <p> ComScriptCommand value where thekeyword in ComScriptCommand
 * <p> equals name.  Calling validate() when value is changed.  Added
 * <p> set(double).
 * <p>
 * <p> Revision 1.2  2004/11/19 23:34:47  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.2  2004/11/19 03:04:52  sueh
 * <p> bug# 520 In set(Object) renamed a local variable for clarity.
 * <p>
 * <p> Revision 1.1.2.1  2004/11/16 02:27:39  sueh
 * <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
 * <p> EtomoLong with EtomoNumber.  EtomoNumber acts a simple numeric
 * <p> type which handles null values, defaults, and recommended values.
 * <p> EtomoNumber stores its values in Number variables and is created with a
 * <p> required type parameter to keep track of its numeric type.
 * <p> </p>
 */
public class EtomoNumber extends ConstEtomoNumber {
  public static final String rcsid = "$Id$";

  /**
   * Construct an EtomoNumber with type = INTEGER_TYPE
   *
   */
  public EtomoNumber() {
    super();
  }

  /**
   * Construct a ConstEtomoNumber with type = INTEGER_TYPE
   * @param name the name of the instance
   */
  public EtomoNumber(String name) {
    super(name);
  }

  public EtomoNumber(Type type) {
    super(type);
  }

  public EtomoNumber(Type type, String name) {
    super(type, name);
  }

  public EtomoNumber(ConstEtomoNumber that) {
    super(that);
  }

  public void loadWithAlternateKey(Properties props, String prepend, String key) {
    if (props == null) {
      reset();
    }
    else if ((prepend == null || prepend.matches("\\s*"))
        && (key == null || key.matches("\\s*"))) {
      load(props);
    }
    else if (prepend == null || prepend.matches("\\s*")) {
      set(props.getProperty(key));
    }
    else if (key == null || key.matches("\\s*")) {
      load(props, prepend);
    }
    else {
      set(props.getProperty(prepend + "." + key));
    }
  }

  public void load(Properties props) {
    set(props.getProperty(name));
  }

  private String createKey(String prepend, final String key) {
    if (prepend == null || prepend.matches("\\s*")) {
      return key;
    }
    if (prepend.endsWith(".")) {
      return prepend + key;
    }
    return prepend + "." + key;
  }

  void loadFromOtherKey(final Properties props, final String prepend, final String key) {
    if (props == null) {
      reset();
    }
    else {
      set(props.getProperty(createKey(prepend, key)));
    }
  }

  public void load(final Properties props, final String prepend) {
    if (prepend == null || prepend.matches("\\s*")) {
      load(props);
    }
    else if (prepend.endsWith(".")) {
      set(props.getProperty(prepend + name));
    }
    else {
      set(props.getProperty(prepend + "." + name));
    }
  }

  public void load(Properties props, String prepend, int defaultValue) {
    if (loadIfPresent(props, prepend)) {
      return;
    }
    set(defaultValue);
  }
  
  public void load(Properties props, String prepend, boolean defaultValue) {
    if (loadIfPresent(props, prepend)) {
      return;
    }
    set(defaultValue);
  }

  public boolean isKeyPresent(Properties props, String prepend) {
    if (props.getProperty(prepend == null || prepend.matches("\\s*") ? name : prepend
        + "." + name) == null) {
      return false;
    }
    return true;
  }

  public boolean loadIfPresent(Properties props, String prepend) {
    if (props.getProperty(prepend == null || prepend.matches("\\s*") ? name : prepend
        + "." + name) == null) {
      return false;
    }
    load(props, prepend);
    return true;
  }

  /**
   * Attempt to get the property specified by prepend and name.  If it doesn't
   * exist, return null.  If it does, set it in instance (create instance
   * if it doesn't exist).
   * @param etomoNumber
   * @param type
   * @param name
   * @param props
   * @param prepend
   * @return etomoNumber
   */
  public static EtomoNumber load(EtomoNumber instance, Type type, String name,
      Properties props, String prepend) {
    String value = props.getProperty(prepend == null || prepend.matches("\\s*") ? name
        : prepend + "." + name);
    if (value == null) {
      return null;
    }
    if (instance == null) {
      instance = new EtomoNumber(type, name);
    }
    instance.set(value);
    return instance;
  }

  /**
   * Attempt to get the property specified by prepend and name.  If it doesn't
   * exist, return null.  If it does, set it in instance (create instance
   * if it doesn't exist).
   * @param etomoNumber
   * @param name
   * @param props
   * @param prepend
   * @return etomoNumber
   */
  public static EtomoNumber load(EtomoNumber instance, String name, Properties props,
      String prepend) {
    String value = props.getProperty(prepend == null || prepend.matches("\\s*") ? name
        : prepend + "." + name);
    if (value == null) {
      return null;
    }
    if (instance == null) {
      instance = new EtomoNumber(name);
    }
    instance.set(value);
    return instance;
  }

  /**
   * Converts a string to a Number of the correct type.  If the string is
   * empty, currentValue will be set to newNumber().  If the string is not a 
   * valid number of the correct type, currentValue will be set to newNumber().
   * If ceilingValue is not null and the new value exceeds ceilingValue,
   * currentValue will be set to ceilingValue.
   * @param value
   * @return
   */
  public EtomoNumber set(String value) {
    if (isDebug()) {
      System.out.println("value=" + value);
    }
    resetState();
    if (value == null || value.matches("\\s*")) {
      currentValue = newNumber();
    }
    else {
      StringBuffer invalidBuffer = new StringBuffer();
      currentValue = newNumber(value, invalidBuffer);
      if (isDebug()) {
        System.out.println("currentValue=" + currentValue + ",invalidBuffer="
            + invalidBuffer);
      }
      if (invalidBuffer.length() > 0) {
        if (type == Type.INTEGER && stringArray != null) {
          for (int i = 0; i < stringArray.length; i++) {
            if (value.compareToIgnoreCase(stringArray[i]) == 0) {
              currentValue = newNumber(i);
              resetState();
            }
          }
        }
        addInvalidReason(invalidBuffer.toString());
        currentValue = newNumber();
      }
    }
    currentValue = applyCeilingValue(applyFloorValue(currentValue));
    setInvalidReason();
    return this;
  }

  public void setToDefault() {
    set(defaultValue);
  }

  public EtomoNumber set(Number value) {
    resetState();
    currentValue = applyCeilingValue(applyFloorValue(value));
    setInvalidReason();
    return this;
  }

  /**
   * set currentValue from number.getValue().  Take the currentValueSet value
   * from number.
   * @param number
   * @return
   */
  public ConstEtomoNumber set(ConstEtomoNumber number) {
    if (number == null) {
      reset();
    }
    else {
      set(number.getValue());
    }
    return this;
  }

  public void add(ConstEtomoNumber number) {
    if (number == null) {
      return;
    }
    else {
      set(add(getValue(), number.getValue()));
    }
  }

  public void add(int i) {
    set(add(getValue(), newNumber(i)));
  }

  /**
   * Multiply the current value by i and store the result as the current value.
   * @param i
   */
  public void multiply(int i) {
    set(multiply(getValue(), newNumber(i)));
  }
  
  /**
   * Divide the current value by i and store the result as the current value.
   * @param i
   */
  public void divideBy(int i) {
    set(divideBy(getValue(), newNumber(i)));
  }

  public EtomoNumber set(int value) {
    return set(newNumber(value));
  }

  public ConstEtomoNumber set(boolean value) {
    return set(newNumber(value));
  }

  public EtomoNumber set(long value) {
    return set(newNumber(value));
  }

  public EtomoNumber set(double value) {
    return set(newNumber(value));
  }

  public EtomoNumber set(FortranInputString fortranInputString, int index) {
    if (fortranInputString.isEmpty(index) || fortranInputString.isDefault(index)) {
      return set(newNumber());
    }
    if (fortranInputString.isIntegerType(index)) {
      return set(newNumber(fortranInputString.getInt(index)));
    }
    return set(newNumber(fortranInputString.getDouble(index)));
  }

  /**
   * Set currentValue to resetValue.  Sets currentValueSet to false.
   * @return
   */
  public EtomoNumber reset() {
    resetState();
    currentValue = applyCeilingValue(applyFloorValue(newNumber()));
    setInvalidReason();
    return this;
  }

}
