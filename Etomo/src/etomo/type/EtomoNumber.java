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
  
  public EtomoNumber(ConstEtomoNumber that) {
    super(that);
  }
  
  public void load(Properties props) {
    set(props.getProperty(name, resetValue.toString()));
  }
  public void load(Properties props, String prepend) {
    set(props.getProperty(prepend + "." + name, resetValue.toString()));
  }
  
  public EtomoNumber set(String value) {
    invalidReason = null;
    if (value == null || !value.matches("\\S+")) {
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
    if (!isNull(ceilingValue) && !isNull() && gt(this.value, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    validate();
    return this;
  }
  
  public EtomoNumber set(ComScriptCommand scriptCommand)
      throws InvalidParameterException {
    return set(scriptCommand.getValue(name));
  }
  
  public EtomoNumber set(Object value) {
    Number newValue = (Number) value;
    invalidReason = null;
    if (value == null) {
      this.value = newNumber();
    }
    else if (!isNull(ceilingValue) && !isNull(newValue) && gt(newValue, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    else {
      this.value = newNumber(newValue);
    }
    validate();
    return this;
  }
  
  public EtomoNumber set(int value) {
    invalidReason = null;
    this.value = newNumber(value);
    if (!isNull(ceilingValue) && !isNull() && gt(this.value, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    validate();
    return this;
  }
  
  public EtomoNumber set(long value) {
    invalidReason = null;
    this.value = newNumber(value);
    if (!isNull(ceilingValue) && !isNull() && gt(this.value, ceilingValue)) {
      this.value = newNumber(ceilingValue);
    }
    validate();
    return this;
  }
  
  public EtomoNumber set(double value) {
    invalidReason = null;
    this.value = newNumber(value);
    if (!isNull(ceilingValue) && !isNull() && gt(this.value, ceilingValue)) {
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
