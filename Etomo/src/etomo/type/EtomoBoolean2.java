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
  
  public static final int FALSE_VALUE = 0;
  public static final int TRUE_VALUE = 1;
  
  private static final String falseString = "false";
  private static final String falseStrings[] = {"f", "no"};
  private static final String trueString = "true";
  private static final String trueStrings[] = {"t","yes"};
  private boolean updateAsInteger = false;

  public EtomoBoolean2(String name) {
    super(EtomoNumber.INTEGER_TYPE, name, true, new Integer(FALSE_VALUE));
    setValidValues(new int[] { FALSE_VALUE, TRUE_VALUE });
    setUseScreenDisplayValue(false);
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
  }
  
  /**
   * return false if null or 0
   * otherwise return true
   */
  protected String toString(Number value) {
    if (is(value)) {
      return trueString;
    }
    return falseString;
  }

  /**
   * Override parse(ComScriptCommand) to handle a boolean being true when
   * it has no value in the script.
   */
  public ConstEtomoNumber parse(ComScriptCommand scriptCommand, String keyword)
      throws InvalidParameterException {
    if (!scriptCommand.hasKeyword(keyword)) {
      return set(FALSE_VALUE);
    }
    String scriptValue = scriptCommand.getValue(keyword);
    if (scriptValue == null || scriptValue.matches("\\s*")) {
      return set(TRUE_VALUE);
    }
    return set(scriptValue);
  }
  
  /**
   * Override update(ComScriptCommand) to handle writing the boolean as an
   * integer in the script (use super.toString()).  Also handle writing the
   * boolean as a parameter without a value.
   */
  public ConstEtomoNumber setInScript(ComScriptCommand scriptCommand) {
    if (!isUseInScript()) {
      return this;
    }
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
  
  public ConstEtomoNumber addToScript(StringBuffer optionBuffer, boolean force) {
    if (!force && !isUseInScript()) {
      return this;
    }
    if (updateAsInteger) {
      optionBuffer.append(super.toString(getValueForScript()));
    }
    else {
      optionBuffer.append(toString(getValueForScript()));
    }
    return this;
  }
  
  public boolean isUseInScript() {
    return true;
  }
  
  protected Number getValueForScript() {
    return currentValue;
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
    return newNumber(value, invalidBuffer);
  }

}
