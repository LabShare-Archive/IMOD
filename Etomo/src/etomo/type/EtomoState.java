package etomo.type;

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
* <p> $Log$ </p>
*/

public class EtomoState extends EtomoNumber {
  public static final String  rcsid =  "$Id$";
  
  public static final int NO_RESULT_VALUE = -1;
  public static final int FALSE_VALUE = -2;
  public static final int TRUE_VALUE = -3;
  
  private static final String nullString = "null";
  private static final String noResultString = "no result";
  private static final String falseString = "false";
  private static final String trueString = "true";

  public EtomoState(String name) {
    super(EtomoNumber.INTEGER_TYPE, name);
    setValidValues(new int[] { NO_RESULT_VALUE, FALSE_VALUE, TRUE_VALUE });
    setDefault(FALSE_VALUE);
    setDisplayDefault(true);
  }
  
  /**
   * allow setting a boolean value
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
   * overide validate() to throw an exception
   */
  protected void validate() {
    super.validate();
    if (invalidReason != null) {
      throw new IllegalArgumentException(invalidReason);
    }
  }
  
  /**
   * Convert to true/false
   * @return
   */
  public boolean is() {
    int intValue = getValue(displayDefault).intValue();
    if (intValue == TRUE_VALUE) {
      return true;
    }
    if (isNull(intValue) || intValue == NO_RESULT_VALUE || intValue == FALSE_VALUE) {
      return false;
    }
    return super.is();
  }
  
  protected String toString(Number value) {
    int intValue = value.intValue();
    switch (intValue) {
    case INTEGER_NULL_VALUE:
      return nullString;
    case NO_RESULT_VALUE:
      return noResultString;
    case FALSE_VALUE:
      return falseString;
    case TRUE_VALUE:
      return trueString;
    default:
      return super.toString(value);
    }
  }
  
  /**
   * Overide newNumber() to convert from a character string to the value
   */
  protected Number newNumber(String value, StringBuffer invalidBuffer) {
    if (value != null && !value.matches("\\s*")) {
      //Convert from character string to integer value
      String trimmedValue = value.trim().toLowerCase();
      if (trimmedValue.equals(nullString)) {
        return newNumber(INTEGER_NULL_VALUE);
      }
      if (trimmedValue.equals(noResultString)) {
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

}
