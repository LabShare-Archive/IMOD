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
* <p> Revision 1.2  2005/01/14 23:03:18  sueh
* <p> Overriding initialize(), parse(), and update().
* <p>
* <p> Revision 1.1  2005/01/13 19:03:01  sueh
* <p> Inherits EtomoNumber to create a boolean which does not allow nulls and
* <p> can't read/write itself  to/from .edf and .com files.
* <p> </p>
*/
public class EtomoBoolean2 extends EtomoNumber {
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
    if (is()) {
      return trueString;
    }
    return falseString;
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
