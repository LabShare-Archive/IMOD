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
  
  public EtomoNumber(int type, int initialValue) {
    super(type, initialValue);
  }
  
  public EtomoNumber(int type, String name, int initialValue) {
    super(type, name, initialValue);
  }
  
  public EtomoNumber(ConstEtomoNumber that) {
    super(that);
  }
  
  public void load(Properties props) {
    set(props.getProperty(name, toString(resetValue)));
  }
  public void load(Properties props, String prepend) {
    set(props.getProperty(prepend + "." + name, toString(resetValue)));
  }
  
  public EtomoNumber set(String value) {
    invalidReason = null;
    if (value == null || value.matches("\\s*")) {
      this.value = newNumber();
    }
    else {
      StringBuffer invalidBuffer = new StringBuffer();
      this.value = newNumber(value, invalidBuffer);
      if (invalidBuffer.length() > 0) {
        invalidReason = invalidBuffer.toString();
        this.value = newNumber();
      }
    }
    if (!isNull(ceilingValue) && isSet() && gt(this.value, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    validate();
    return this;
  }
  
  public ConstEtomoNumber parse(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    if (!scriptCommand.hasKeyword(name)) {
      value = newNumber();
      return this;
    }
    return set(scriptCommand.getValue(name));
  }
  
  public EtomoNumber set(Number value) {
    invalidReason = null;
    if (value == null) {
      this.value = newNumber();
    }
    else if (!isNull(ceilingValue) && !isNull(value) && gt(value, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    else {
      this.value = newNumber(value);
    }
    validate();
    return this;
  }
  
  public EtomoNumber set(int value) {
    invalidReason = null;
    this.value = newNumber(value);
    if (!isNull(ceilingValue) && isSet() && gt(this.value, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    validate();
    return this;
  }
  
  public EtomoNumber set(long value) {
    invalidReason = null;
    this.value = newNumber(value);
    if (!isNull(ceilingValue) && isSet() && gt(this.value, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    validate();
    return this;
  }
  
  public EtomoNumber set(double value) {
    invalidReason = null;
    this.value = newNumber(value);
    if (!isNull(ceilingValue) && isSet() && gt(this.value, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    validate();
    return this;
  }
  
  public EtomoNumber reset() {
    value = newNumber(resetValue);
    validate();
    return this;
  }
  
}
