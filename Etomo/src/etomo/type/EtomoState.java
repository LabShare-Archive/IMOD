package etomo.type;

import java.util.Properties;

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
* <p> Revision 1.6  2005/10/12 21:26:08  sueh
* <p> EtomoState():  The default type for EtomoNumber is integer, so the type
* <p> doesn't have to be sent to super().
* <p>
* <p> Revision 1.5  2005/06/11 02:38:25  sueh
* <p> Overrode store functions.  Added isResultSet().
* <p>
* <p> Revision 1.4  2005/05/10 02:58:30  sueh
* <p> bug# 658 Changed ConstEtomoNumber.validate() to
* <p> ConstEtomoNumber.setInvalidReason.
* <p>
* <p> Revision 1.3  2005/01/21 23:27:42  sueh
* <p> bug# 509 bug# 591  Changed the integer values for true and false to 1 and 0
* <p> so more functionality can be handled by EtomoNumber.  Stopped overriding
* <p> set(boolean).  Simplified is().
* <p>
* <p> Revision 1.2  2005/01/10 23:48:59  sueh
* <p> bug# 578 Removed defaultValue and displayDefault setting in constructor
* <p> since these are unnecessary.
* <p>
* <p> Revision 1.1  2005/01/10 23:36:03  sueh
* <p> bug# 578 Class to handle state with four values:  null, no result, false, and
* <p> true.  It inherits EtomoNumber and uses validValues to maintain a strict
* <p> set; invalid values cause a runtime error.  It overides toString(Number)
* <p> and newNumber(String, StringBuffer) to translate its valid values to
* <p> meaningful strings.  It overides is() to make its true value return true and
* <p> its other values return false.
* <p> </p>
*/

public class EtomoState extends EtomoNumber {
  public static final String  rcsid =  "$Id$";
  
  public static final int NO_RESULT_VALUE = -1;
  public static final int FALSE_VALUE = 0;
  public static final int TRUE_VALUE = 1;
  public static final String NO_RESULT_STRING = "no result";
  
  private static final String nullString = "null";
  private static final String falseString = "false";
  private static final String trueString = "true";

  public EtomoState(String name) {
    super(name);
    setValidValues(new int[] { NO_RESULT_VALUE, FALSE_VALUE, TRUE_VALUE });
  }
  
  public EtomoState() {
    super();
    setValidValues(new int[] { NO_RESULT_VALUE, FALSE_VALUE, TRUE_VALUE });
  }
  
  /**
   * overide validate() to throw an exception
   */
  protected void setInvalidReason() {
    super.setInvalidReason();
    if (invalidReason != null) {
      throw new IllegalArgumentException(invalidReason.toString());
    }
  }
  
  /**
   * Convert to true/false
   * @return
   */
  public boolean is() {
    int intValue = getValue().intValue();
    if (intValue == NO_RESULT_VALUE) {
      return false;
    }
    return super.is();
  }
  
  public boolean isResultSet() {
    return currentValue.intValue() != NO_RESULT_VALUE;
  }
  
  protected String toString(Number value) {
    int intValue = value.intValue();
    switch (intValue) {
    case INTEGER_NULL_VALUE:
      return nullString;
    case NO_RESULT_VALUE:
      return NO_RESULT_STRING;
    case FALSE_VALUE:
      return falseString;
    case TRUE_VALUE:
      return trueString;
    default:
      return super.toString(value);
    }
  }
  
  /**
   * Overide newNumber(String, StringBuffer) to convert from a character string
   * to the value
   */
  protected Number newNumber(String value, StringBuffer invalidBuffer) {
    if (value != null && !value.matches("\\s*")) {
      //Convert from character string to integer value
      String trimmedValue = value.trim().toLowerCase();
      if (trimmedValue.equals(nullString)) {
        return newNumber(INTEGER_NULL_VALUE);
      }
      if (trimmedValue.equals(NO_RESULT_STRING)) {
        return newNumber(NO_RESULT_VALUE);
      }
      if (trimmedValue.equals(falseString)) {
        return newNumber(FALSE_VALUE);
      }
      if (trimmedValue.equals(trueString)) {
        return newNumber(TRUE_VALUE);
      }
    }
    return newNumber(value, invalidBuffer);
  }
  
  public void store(Properties props) {
    props.setProperty(name, toString(currentValue));
  }

  public void store(Properties props, String prepend) {
    props.setProperty(prepend + "." + name, toString(currentValue));
  }

}
