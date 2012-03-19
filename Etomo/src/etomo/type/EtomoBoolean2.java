package etomo.type;

import java.util.HashMap;
import java.util.Properties;

import etomo.comscript.ComScriptCommand;
import etomo.comscript.InvalidParameterException;

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
 * <p> Revision 1.23  2011/02/22 05:38:14  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.22  2010/03/18 22:43:20  sueh
 * <p> bug# 1323 In load(EtomoBoolean2,String,Properties,String fix the way the
 * <p> key is derived from prepend and name.
 * <p>
 * <p> Revision 1.21  2010/03/09 01:40:29  sueh
 * <p> bug# 1323 In load(EtomoBoolean2,String,Properties,String) handling a
 * <p> prepend which ends with ".".
 * <p>
 * <p> Revision 1.20  2009/09/22 21:00:58  sueh
 * <p> bug# 1259 No longer unnecessarily returning this from updateComScript.
 * <p>
 * <p> Revision 1.19  2007/12/13 01:12:17  sueh
 * <p> bug# 1056 Reformatted.
 * <p>
 * <p> Revision 1.18  2007/02/05 23:25:40  sueh
 * <p> bug# 962 Added static load().
 * <p>
 * <p> Revision 1.17  2006/09/19 22:33:41  sueh
 * <p> bug# 920 Fixed getInstance(EtomoBoolean2, String, Properties, String).
 * <p> Props.getProperty was passing the wrong key.
 * <p>
 * <p> Revision 1.16  2006/09/13 23:37:14  sueh
 * <p> bug# 920 Added static equals(EtomoBoolean2, EtomoBoolean2) to handle
 * <p> optional instances.  Fixed getInstance(EtomoBoolean2, String, Properties, String)
 * <p> to not create an instance if the value is not found in properties.
 * <p>
 * <p> Revision 1.15  2006/06/09 16:57:11  sueh
 * <p> bug#  869 Added static functions store and getInstance to handle a variable
 * <p> which could be null.
 * <p>
 * <p> Revision 1.14  2006/04/06 20:11:37  sueh
 * <p> bug# 808 Not using equals(value, int); equals(value, Number) handles more
 * <p> cases.
 * <p>
 * <p> Revision 1.13  2005/09/21 16:14:53  sueh
 * <p> bug# 532 Add equals(boolean).
 * <p>
 * <p> Revision 1.12  2005/06/16 20:06:08  sueh
 * <p> bug# 692 EtomoBoolean2 is supposed to be a boolean which allows nulls.
 * <p> It should have nullIsValid = true.
 * <p>
 * <p> Revision 1.11  2005/06/14 22:03:10  sueh
 * <p> bug# 681 Fixed toString(Number).  It was always returning true.  Fixed
 * <p> parse().  It was not overriding
 * <p> ScriptParameter.parse(ComScriptCommand) because it another
 * <p> parameter.  Also fixed it so that it handled shortName.
 * <p>
 * <p> Revision 1.10  2005/06/11 02:32:12  sueh
 * <p> Removed an unnecessary function, is(Number) in ConstEtomoNumber.  It
 * <p> did the same thing as equals(Number).
 * <p>
 * <p> Revision 1.9  2005/06/06 16:50:57  sueh
 * <p> bug# 671 Added EtomoBoolean2() because an instance of this class
 * <p> doesn't need to be named if it is not saved to a file.
 * <p>
 * <p> Revision 1.8  2005/05/17 19:18:25  sueh
 * <p> bug# 658 Passing a HashMap of required values from the autodoc to the
 * <p> super constructor.
 * <p>
 * <p> Revision 1.7  2005/05/10 02:42:01  sueh
 * <p> bug# 658 Made EtomoBoolean2 capable of handle any two values.
 * <p> Prevent setting displayAsInteger to false if the instance doesn't have an
 * <p> integer equivalent, such as "true" and "false".  Added setOn() and
 * <p> setOff().  Removed addToScript() because it is confusing and it is only
 * <p> used in one place.  Changed setInScript() to updateComScript().
 * <p> Changed validate() to setInvalidReason().
 * <p>
 * <p> Revision 1.6  2005/03/08 02:00:30  sueh
 * <p> bug# 533 Changed updateAsInteger to more general displayAsInteger.
 * <p>
 * <p> Revision 1.5  2005/03/04 00:16:10  sueh
 * <p> bug# 533 Added setUpdateAsInteger().  Fixed a bug in newNumber() that
 * <p> caused a stack overflow.
 * <p>
 * <p> Revision 1.4  2005/01/25 23:50:52  sueh
 * <p> Switching from inheriting EtomoNumber to inheritying ScriptParameters.
 * <p> Overriding isUseInScript(), getValueFromScript(), and addToScript().
 * <p> Changing update to setInScript().
 * <p>
 * <p> Revision 1.3  2005/01/21 23:23:26  sueh
 * <p> bug# 509 bug# 591  Moved the prevention of null values to
 * <p> ConstEtomoNumber.  No longer need to override initialize() functions or
 * <p> newNumber().
 * <p>
 * <p> Revision 1.2  2005/01/14 23:03:18  sueh
 * <p> Overriding initialize(), parse(), and update().
 * <p>
 * <p> Revision 1.1  2005/01/13 19:03:01  sueh
 * <p> Inherits EtomoNumber to create a boolean which does not allow nulls and
 * <p> can read/write itself  to/from .edf and .com files.
 * <p> </p>
 */
public class EtomoBoolean2 extends ScriptParameter {
  public static final String rcsid = "$Id$";

  public static final int DEFAULT_FALSE_VALUE = 0;
  public static final int DEFAULT_TRUE_VALUE = 1;

  private static final String defaultFalseString = "false";
  private static final String defaultFalseStrings[] = { "f", "no" };
  private static final String defaultTrueString = "true";
  private static final String defaultTrueStrings[] = { "t", "yes" };

  private int falseValue = DEFAULT_FALSE_VALUE;
  private int trueValue = DEFAULT_TRUE_VALUE;
  private String falseString = defaultFalseString;
  private String trueString = defaultTrueString;
  private String[] falseStrings = defaultFalseStrings;
  private String[] trueStrings = defaultTrueStrings;

  private boolean displayAsInteger = false;

  public EtomoBoolean2() {
    super(EtomoNumber.Type.INTEGER);
    setValidValues(new int[] { falseValue, trueValue });
    setDisplayValue(falseValue);
  }

  public EtomoBoolean2(String name) {
    super(EtomoNumber.Type.INTEGER, name);
    setValidValues(new int[] { falseValue, trueValue });
    setDisplayValue(falseValue);
  }

  public EtomoBoolean2(String name, HashMap requiredMap) {
    super(EtomoNumber.Type.INTEGER, name, requiredMap);
    setValidValues(new int[] { falseValue, trueValue });
    setDisplayValue(falseValue);
  }

  public EtomoBoolean2(String name, int onValue, int offValue) {
    super(EtomoNumber.Type.INTEGER, name);
    trueValue = onValue;
    falseValue = offValue;
    falseString = null;
    trueString = null;
    falseStrings = null;
    trueStrings = null;
    setValidValues(new int[] { falseValue, trueValue });
    setDisplayValue(falseValue);
  }

  public static void store(EtomoBoolean2 instance, Properties props, String prepend,
      String name) {
    if (instance == null) {
      props.remove(prepend + "." + name);
    }
    else {
      instance.store(props, prepend);
    }
  }

  /**
   * Attempt to get the property specified by prepend and name.  If it doesn't
   * exist, return null.  If it does, set it in instance (create instance
   * if it doesn't exist).  Return the instance.
   * @param etomoBoolean2
   * @param name
   * @param props
   * @param prepend
   * @return etomoBoolean2
   */
  public static EtomoBoolean2 load(EtomoBoolean2 instance, String name, Properties props,
      String prepend) {
    String key = null;
    if (prepend == null || prepend.matches("\\s*")) {
      key = name;
    }
    else if (prepend.endsWith(".")) {
      key = prepend + name;
    }
    else {
      key = prepend + '.' + name;
    }
    String value = props.getProperty(key);
    if (value == null) {
      return null;
    }
    if (instance == null) {
      instance = new EtomoBoolean2(name);
    }
    instance.set(value);
    return instance;
  }

  public static boolean equals(EtomoBoolean2 instance1, EtomoBoolean2 instance2) {
    if (instance1 == instance2) {
      return true;
    }
    if (instance1 == null) {
      return false;
    }
    return instance1.equals(instance2);
  }

  public static EtomoBoolean2 set(EtomoBoolean2 instance, ConstEtomoNumber value,
      String name) {
    if (instance == null && value != null) {
      instance = new EtomoBoolean2(name);
    }
    if (instance == null) {
      return null;
    }
    instance.set(value);
    return instance;
  }

  public static EtomoBoolean2 set(EtomoBoolean2 instance, boolean value, String name) {
    if (instance == null) {
      instance = new EtomoBoolean2(name);
    }
    instance.set(value);
    return instance;
  }

  /**
   * If instance exists, load it and return it.
   * If instance doesn't exist, only create and return it if the value in props
   * exists.  If there is no value in props with the key prepend + '.' + key,
   * then return null
   * @param instance
   * @param key
   * @param props
   * @param prepend
   * @return instance or null
   */
  public static EtomoBoolean2 getInstance(EtomoBoolean2 instance, String key,
      Properties props, String prepend) {
    if (instance != null) {
      instance.load(props, prepend);
      return instance;
    }
    String value = props.getProperty(prepend + '.' + key);
    if (value == null) {
      return null;
    }
    if (instance == null) {
      instance = new EtomoBoolean2(key);
    }
    instance.set(value);
    return instance;
  }

  public static EtomoBoolean2 getInstance(EtomoBoolean2 instance, String key,
      boolean value) {
    if (instance == null) {
      instance = new EtomoBoolean2(key);
    }
    instance.set(value);
    return instance;
  }

  /**
   * Override isNull() to prevent EtomoBoolean2 from being null.  Checks the
   * display value, which is always set, when checking for null.  So it never
   * tests as null.  This could be overriden by setting the display value to
   * null.
   */
  public boolean isNull() {
    return isNull(getValue());
  }

  /**
   * To prevent values not in validValues from being used:
   * Override validate().  Call super.validate(). Throw an exception when
   * invalidReason is set.
   */
  protected void setInvalidReason() {
    super.setInvalidReason();
    if (invalidReason != null) {
      throw new IllegalArgumentException(invalidReason.toString());
    }
  }

  /**
   * return false if null or 0
   * otherwise return true
   */
  protected String toString(Number value) {
    if (displayAsInteger) {
      return super.toString(value);
    }
    if (equals(value, newNumber(trueValue))) {
      return trueString;
    }
    return falseString;
  }

  public ConstEtomoNumber setDisplayAsInteger(boolean displayAsInteger) {
    if (!displayAsInteger && (trueString == null || falseString == null)) {
      throw new IllegalStateException("Must display " + name
          + "as an integer, since it has no string equivalent.");
    }
    this.displayAsInteger = displayAsInteger;
    return this;
  }

  /**
   * Override parse(ComScriptCommand) to handle a boolean being true when
   * it has no value in the script.
   */
  public ConstEtomoNumber parse(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    boolean nameInScript = scriptCommand.hasKeyword(name);
    if (!nameInScript && (shortName == null || !scriptCommand.hasKeyword(shortName))) {
      return set(falseValue);
    }
    String scriptValue;
    if (nameInScript) {
      scriptValue = scriptCommand.getValue(name);
    }
    else {
      scriptValue = scriptCommand.getValue(shortName);
    }
    if (scriptValue == null || scriptValue.matches("\\s*")) {
      return set(trueValue);
    }
    return set(scriptValue);
  }

  /**
   * Override update(ComScriptCommand) to handle writing the boolean as an
   * integer in the script (use super.toString()).  Also handle writing the
   * boolean as a parameter without a value.
   */
  public void updateComScript(ComScriptCommand scriptCommand) {
    if (!isUseInScript()) {
      return;
    }
    if (!displayAsInteger && !is()) {
      scriptCommand.deleteKey(name);
    }
    else if (displayAsInteger) {
      scriptCommand.setValue(name, super.toString());
    }
    else {
      scriptCommand.setValue(name, "");
    }
  }

  public ConstEtomoNumber setOn() {
    return set(trueValue);
  }

  public ConstEtomoNumber setOff() {
    return set(falseValue);
  }

  public boolean isUseInScript() {
    return true;
  }

  public boolean equals(boolean value) {
    if (value) {
      return equals(trueValue);
    }
    return equals(falseValue);
  }

  /**
   * To convert from strings such as "false"
   * Override newNumber(String, StringBuffer) and convert from a trimmed, case
   * adjusted character strings to values.
   * Then call super.newNumber(String, StringBuffer) to handle strings such as
   * "0".
   */
  protected Number newNumber(String value, StringBuffer invalidBuffer) {
    //Convert from character string to integer value
    String trimmedValue = value.trim().toLowerCase();
    if (falseString != null && trimmedValue.equals(falseString)) {
      return newNumber(falseValue);
    }
    if (trueString != null && trimmedValue.equals(trueString)) {
      return newNumber(trueValue);
    }
    if (falseStrings != null) {
      for (int i = 0; i < falseStrings.length; i++) {
        if (trimmedValue.equals(falseStrings[i])) {
          return newNumber(falseValue);
        }
      }
    }
    if (trueStrings != null) {
      for (int i = 0; i < trueStrings.length; i++) {
        if (trimmedValue.equals(trueStrings[i])) {
          return newNumber(trueValue);
        }
      }
    }
    return super.newNumber(value, invalidBuffer);
  }

}
