package etomo.type;

import java.util.Properties;

import etomo.comscript.ComScriptCommand;
import etomo.storage.Storable;

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
* <p> Revision 1.2  2004/12/16 02:27:28  sueh
* <p> bug# 564 Remove recommendedValue.  Use resetValue instead.  Added
* <p> is().
* <p>
* <p> Revision 1.1  2004/12/14 21:43:15  sueh
* <p> bug# 564 A three state boolean (null, true, false).
* <p> </p>
*/
public abstract class ConstEtomoBoolean implements Storable {
  public static  final String  rcsid =  "$Id$";
  
  public static final int UPDATE_AS_KEYWORD = -1;
  public static final int UPDATE_AS_INTEGER = -2;
  private static final int nullValue = Integer.MIN_VALUE;
  
  protected String name;
  protected String description = null;
  protected boolean displayDefault = false;
  protected int value;
  protected int defaultValue;
  protected int resetValue;
  protected boolean useBackwardCompatibleValue;
  protected int backwardCompatibleValue;
  private int updateAs = UPDATE_AS_KEYWORD;
  
  public abstract void load(Properties props);
  public abstract void load(Properties props, String prepend);

  
  protected ConstEtomoBoolean() {
    name = super.toString();
    description = name;
    initialize();
  }
  
  protected ConstEtomoBoolean(String name) {
    this.name = name;
    description = name;
    initialize();
  }
  
  public String getDescription() {
    return description;
  }
  
  public String classInfoString() {
    return getClass().getName() + "[" + paramString() + "]";
  }
  
  protected String paramString() {
    return ",\nname=" + name + ",\ndescription=" + description + ",\nvalue="
        + value + ",\ndefaultValue=" + defaultValue + ",\nresetValue=" + resetValue;
  }
  
  public ConstEtomoBoolean setDefault(boolean defaultValue) {
    this.defaultValue = toInteger(defaultValue);
    return this;
  }
  
  public ConstEtomoBoolean setDisplayDefault(boolean displayDefault) {
    this.displayDefault = displayDefault;
    return this;
  }
  
  /**
   * Set the value will be used if the user does not set a value or there is no
   * value to load.  Also used in reset().
   * @param resetValue
   * @return
   */
  public ConstEtomoBoolean setResetValue(boolean resetValue) {
    this.resetValue = toInteger(resetValue);
    return this;
  }
  
  /**
   * Set the value that will be used if the variable cannot be loaded
   * Overrides resetValue in load().
   * @param value
   * @return
   */
  public ConstEtomoBoolean setBackwardCompatibleValue(ConstEtomoBoolean value) {
    useBackwardCompatibleValue = true;
    backwardCompatibleValue = value.toInteger();
    return this;
  }
  
  /**
   * Set updateAs, which changes how the object is stored in ComScriptCommand.
   * UPDATE_AS_KEYWORD (default) stores the name only if the object is true.
   * UPDATE_AS_INTEGER stores the name and 0 or 1.
   * @param updateAs
   * @return
   */
  public ConstEtomoBoolean setUpdateAs(int updateAs) {
    this.updateAs = updateAs;
    return this;
  }
  
  public void setDescription(String description) {
    if (description != null) {
      this.description = description;
    }
    else {
      name = description;
    }
  }
  
  public void store(Properties props) {
    props.setProperty(name, toString(value));
  }
  
  public void store(Properties props, String prepend) {
    props.setProperty(prepend + "." + name, toString(value));
  }
  
  public void remove(Properties props, String prepend) {
    props.remove(prepend + "." + name);
  }
  
  public String toString() {
    return toString(displayDefault);
  }
  
  public String toString(boolean displayDefault) {
    return toString(getValue(displayDefault));
  }

  public boolean isSetAndNotDefault() {
    return !isNull() && (isNull(defaultValue) || value != defaultValue);
  }
  
  public boolean is() {
    if (isNull() || value == 0) {
      return false;
    }
     return true;
  }
  
  /**
   * update name and value in ComScriptCommand
   * @param scriptCommand
   * @return
   */
  public ConstEtomoBoolean update(ComScriptCommand scriptCommand) {
    if (value == 0 || isNull()) {
      switch (updateAs) {
      case UPDATE_AS_KEYWORD:
        scriptCommand.deleteKey(name);
        break;
      case UPDATE_AS_INTEGER:
        scriptCommand.setValue(name, "0");
        break;
      }
    }
    else {
      switch (updateAs) {
      case UPDATE_AS_KEYWORD:
        scriptCommand.setValue(name, "");
        break;
      case UPDATE_AS_INTEGER:
        scriptCommand.setValue(name, "1");
        break;
      }
    }
    return this;
  }

  public boolean isNull() {
    return isNull(value);
  }

  /**
   * compare two EtomoBooleans, comparing resetValue if value is null.
   * @param that
   * @return
   */
  public boolean equals(ConstEtomoBoolean that) {
    return equals(getValue(), that.getValue());
  }

  public boolean equals(boolean value) {
    return equals(getValue(), toInteger(value));
  }

  public boolean equals(String value) {
    return equals(getValue(), toInteger(value));
  }

  private void initialize() {
    value = Integer.MIN_VALUE;
    defaultValue = Integer.MIN_VALUE;
    resetValue = Integer.MIN_VALUE;
    useBackwardCompatibleValue = false;
    backwardCompatibleValue = Integer.MIN_VALUE;
  }
  
  private void initialize(boolean initialValue) {
    value = toInteger(initialValue);
    defaultValue = Integer.MIN_VALUE;
    resetValue = Integer.MIN_VALUE;
    useBackwardCompatibleValue = false;
    backwardCompatibleValue = Integer.MIN_VALUE;
  }
  
  private int getValue() {
    return getValue(displayDefault);
  }
  
  private int getValue(boolean displayDefault) {
    if (!isNull()) {
      return value;
    }
    if (!isNull(resetValue)) {
      return resetValue;
    }
    if (displayDefault && !isNull(defaultValue)) {
      return defaultValue;
    }
    return value;
  }
  
  protected int toInteger(boolean value) {
    if (value) {
      return 1;
    }
    return 0;
  }
  
  private int toInteger() {
    return value;
  }
  
  protected int toInteger(String value) {
    value = value.toLowerCase();
    if (value == null || value.matches("\\s*") || value.equals("null")) {
      return Integer.MIN_VALUE;
    }
    if (value.equals("true") || value.equals("t") || value.equals("yes")) {
      return 1;
    }
    if (value.equals("false") || value.equals("f") || value.equals("no")) {
      return 0;
    }
    long longValue;
    try {
      longValue = Long.parseLong(value);
    }
    catch (NumberFormatException eLong) {
      double doubleValue;
      try {
        doubleValue = Double.parseDouble(value);
      }
      catch (NumberFormatException eDouble) {
        return Integer.MIN_VALUE;
      }
      //convert floating point value
      if (doubleValue == 0) {
        return 0;
      }
      return 1;
    }
    //convert integer value
    if (longValue == 0) {
      return 0;
    }
    return 1;
  }
  
  protected String toString(int value) {
    if (isNull(value)) {
      return "null";
    }
    if (value == 0) {
      return "false";
    }
    return "true";
  }
    
  protected boolean isNull(int value) {
    return value == nullValue;
  }
  
  protected boolean equals(int value, int compValue) {
    if (isNull(value) && isNull(compValue)) {
      return true;
    }
    if (isNull(value) || isNull(compValue)) {
      return false;
    }
    return value == compValue;
  }

}
