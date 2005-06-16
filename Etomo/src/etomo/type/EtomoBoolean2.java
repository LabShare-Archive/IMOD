package etomo.type;

import java.util.HashMap;

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
* <p> can't read/write itself  to/from .edf and .com files.
* <p> </p>
*/
public class EtomoBoolean2 extends ScriptParameter {
  public static  final String  rcsid =  "$Id$";
  
  public static final int DEFAULT_FALSE_VALUE = 0;
  public static final int DEFAULT_TRUE_VALUE = 1;
  
  private static final String defaultFalseString = "false";
  private static final String defaultFalseStrings[] = {"f", "no"};
  private static final String defaultTrueString = "true";
  private static final String defaultTrueStrings[] = {"t","yes"};
  
  private int falseValue = DEFAULT_FALSE_VALUE;
  private int trueValue = DEFAULT_TRUE_VALUE;
  private String falseString = defaultFalseString;
  private String trueString = defaultTrueString;
  private String[] falseStrings = defaultFalseStrings;
  private String[] trueStrings = defaultTrueStrings;
  
  private boolean displayAsInteger = false;

  public EtomoBoolean2() {
    super(EtomoNumber.INTEGER_TYPE);
    setValidValues(new int[] { falseValue, trueValue });
    setDisplayValue(falseValue);
  }
  
  public EtomoBoolean2(String name) {
    super(EtomoNumber.INTEGER_TYPE, name);
    setValidValues(new int[] { falseValue, trueValue });
    setDisplayValue(falseValue);
  }
  
  public EtomoBoolean2(String name, HashMap requiredMap) {
    super(EtomoNumber.INTEGER_TYPE, name, requiredMap);
    setValidValues(new int[] { falseValue, trueValue });
    setDisplayValue(falseValue);
  }
  
  public EtomoBoolean2(String name, int onValue, int offValue) {
    super(EtomoNumber.INTEGER_TYPE, name);
    trueValue = onValue;
    falseValue = offValue;
    falseString = null;
    trueString = null;
    falseStrings = null;
    trueStrings = null;
    setValidValues(new int[] { falseValue, trueValue });
    setDisplayValue(falseValue);
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
    if (equals(value, trueValue)) {
      return trueString;
    }
    return falseString;
  }
  
  public ConstEtomoNumber setDisplayAsInteger(boolean displayAsInteger) {
    if (!displayAsInteger && (trueString == null || falseString == null)) {
      throw new IllegalStateException("Must display " + name + "as an integer, since it has no string equivalent.");
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
  public ConstEtomoNumber updateComScript(ComScriptCommand scriptCommand) {
    if (!isUseInScript()) {
      return this;
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
    return this;
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
