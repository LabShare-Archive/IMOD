package etomo.type;

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
* <p> Revision 1.1  2005/01/13 19:03:01  sueh
* <p> Inherits EtomoNumber to create a boolean which does not allow nulls and
* <p> can't read/write itself  to/from .edf and .com files.
* <p> </p>
*/
public class EtomoBoolean2 extends EtomoNumber {
  public static  final String  rcsid =  "$Id$";
  
  public static final int FALSE_VALUE = 0;
  public static final int TRUE_VALUE = 1;
  public static final int booleanNullValue = FALSE_VALUE;
  
  private static final String falseString = "false";
  private static final String falseStrings[] = {"f", "no"};
  private static final String trueString = "true";
  private static final String trueStrings[] = {"t","yes"};
  private boolean updateAsInteger = false;

  public EtomoBoolean2(String name) {
    super(EtomoNumber.INTEGER_TYPE, name);
    setValidValues(new int[] { FALSE_VALUE, TRUE_VALUE });
    //Don't force it have defaults
    defaultValue = new Integer(INTEGER_NULL_VALUE);
  }
  
  /**
   * Have to override newNumber() to prevent it from setting null.  This means
   * that we also must override initialize() to set defaultValue and 
   * ceilingValue to null to prevent them from being used unless the user sets
   * them.
   */
  protected void initialize() {
    value = newNumber();
    defaultValue = super.newNumber();
    resetValue = newNumber();
    ceilingValue = super.newNumber();
  }
  
  /**
   * Have to override newNumber() to prevent it from setting null.  This means
   * that we also must override initialize(int) to set defaultValue and 
   * ceilingValue to null to prevent them from being used unless the user sets
   * them.
   */
  protected void initialize(int initialValue) {
    value = newNumber(initialValue);
    defaultValue = super.newNumber();
    resetValue = newNumber();
    ceilingValue = super.newNumber();
  }
  
  /**
   * Allow setting a boolean value
   * @param value
   * @return
   */
  public ConstEtomoNumber set(boolean value) {
    if (value) {
      return set(TRUE_VALUE);
    }
    return set(FALSE_VALUE);
  }
  
  /**
   * To prevent values not in validValues from being used:
   * Override validate().  Call super.validate(). Throw an exception when
   * invalidReason is set.
   * To prevent nulls:
   * Also throw an exception when value is null.
   */
  protected void validate() {
    super.validate();
    if (invalidReason != null) {
      throw new IllegalArgumentException(invalidReason);
    }
    if (value.intValue() == INTEGER_NULL_VALUE) {
      throw new IllegalArgumentException(name + " cannot be null.");
    }
  }
  
  /**
   * To use a meaningful string such as "true" instead of using the number in
   * string form:
   * Override toString(Number).  Return the meaningful string if the value is
   * recognized
   */
  protected String toString(Number value) {
    int intValue = value.intValue();
    switch (intValue) {
    case FALSE_VALUE:
      return falseString;
    case TRUE_VALUE:
      return trueString;
    default:
      return super.toString(value);
    }
  }

  /**
   * Override parse(ComScriptCommand) to handle a boolean being true when
   * it has no value in the script.
   */
  public ConstEtomoNumber parse(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    if (scriptCommand.hasKeyword(name)) {
      String scriptValue = scriptCommand.getValue(name);
      if (scriptValue == null || scriptValue.matches("\\s*")) {
        return set(TRUE_VALUE);
      }
    }
    return super.parse(scriptCommand);
  }
  
  /**
   * Override update(ComScriptCommand) to handle writing the boolean as an
   * integer in the script (use super.toString()).  Also handle writing the
   * boolean as a parameter without a value.
   */
  public ConstEtomoNumber update(ComScriptCommand scriptCommand) {
    if (!updateAsInteger && !is()) {
      scriptCommand.deleteKey(name);
    }
    else if (updateAsInteger) {
      scriptCommand.setValue(name, super.toString());
    }
    else {
      scriptCommand.setValue(name, "");
    }
    return this;
  }
  
  /**
   * To convert from strings such as "false"
   * Override newNumber(String, StringBuffer) and convert from a trimmed, case
   * adjusted character strings to values.
   * Then call super.newNumber(String, StringBuffer) to handle strings such as
   * "0".
   */
  protected Number newNumber(String value, StringBuffer invalidBuffer) {
    if (value != null && !value.matches("\\s*")) {
      //Convert from character string to integer value
      String trimmedValue = value.trim().toLowerCase();
      if (trimmedValue.equals(falseString)) {
        return newNumber(FALSE_VALUE);
      }
      if (trimmedValue.equals(trueString)) {
        return newNumber(TRUE_VALUE);
      }
      for (int i = 0; i < falseStrings.length; i++) {
        if (trimmedValue.equals(falseStrings[i])) {
          return newNumber(FALSE_VALUE);
        }
      }
      for (int i = 0; i < trueStrings.length; i++) {
        if (trimmedValue.equals(trueStrings[i])) {
          return newNumber(TRUE_VALUE);
        }
      }
    }
    return newNumber(value, invalidBuffer);
  }
  
  /**
   * To prevent null values:
   * Override newNumber() by calling newNumber(int) with a const.
   */
  protected Number newNumber() {
    return newNumber(booleanNullValue);
  }

}
