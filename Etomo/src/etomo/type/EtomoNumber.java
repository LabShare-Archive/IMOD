package etomo.type;

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
  public static  final String  rcsid =  "$Id$";
  
  public EtomoNumber(int type) {
    super(type);
  }
  
  public EtomoNumber(int type, String name) {
    super(type, name);
  }
  
  public EtomoNumber(ConstEtomoNumber that) {
    super(that);
  }
  
  public EtomoNumber(int type, String name, boolean preventNullValue, Number resetValue) {
    super(type, name, preventNullValue, resetValue);
  }
  
  public void load(Properties props) {
    String stringValue = props.getProperty(name);
    if (stringValue == null) {
      reset();
    }
    else {
      set(stringValue);
    }
  }
  
  public void load(Properties props, String prepend) {
    String stringValue = props.getProperty(prepend + "." + name);
    if (stringValue == null) {
      reset();
    }
    else {
      set(stringValue);
    }
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
    invalidReason = null;
    if (value == null) {
      currentValue = newNumber();
    }
    else if (value.matches("\\s*")){
      currentValue = newEmptyNumber();
    }
    else {
      StringBuffer invalidBuffer = new StringBuffer();
      currentValue = newNumber(value, invalidBuffer);
      if (invalidBuffer.length() > 0) {
        invalidReason = invalidBuffer.toString();
        currentValue = newNumber();
      }
    }
    currentValue = applyCeilingValue(currentValue);
    validate();
    return this;
  }
  
  public ConstEtomoNumber parse(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    return parse(scriptCommand, name);
  }
  
  /**
   * Parse scriptCommand for keyword.  If keyword is not found, call reset().
   * If keyword is found, call set with the string value found in scriptCommand.
   * @param scriptCommand
   * @param keyword
   * @return
   * @throws InvalidParameterException
   */
  public ConstEtomoNumber parse(ComScriptCommand scriptCommand, String keyword)
      throws InvalidParameterException {
    if (!scriptCommand.hasKeyword(keyword)) {
      reset();
    }
    return set(scriptCommand.getValue(keyword));
  }
  
  public EtomoNumber set(Number value) {
    invalidReason = null;
    currentValue = applyCeilingValue(value);
    validate();
    return this;
  }
  
  /**
   * set currentValue from number.getValue().  Take the currentValueSet value
   * from number.
   * @param number
   * @return
   */
  public ConstEtomoNumber set(ConstEtomoNumber number) {
    set(number.getValue());
    return this;
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
  
  /**
   * Set currentValue to resetValue.  Sets currentValueSet to false.
   * @return
   */
  public EtomoNumber reset() {
    currentValue = applyCeilingValue(newNumber(resetValue));
    validate();
    return this;
  }
  
}
