/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright(c) 2002, 2003, 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.5  2004/04/29 20:23:12  sueh
 * <p> $bug# 427 corrected Double.NaN comparison.
 * <p> $
 * <p> $Revision 1.4  2004/04/27 00:52:29  sueh
 * <p> $bug# 427 adding update required parameter - vector of strings
 * <p> $
 * <p> $Revision 1.3  2004/04/26 21:18:44  sueh
 * <p> $bug# 427 add updateParameterStrings() for Vectors
 * <p> $containing strings
 * <p> $
 * <p> $Revision 1.2  2004/03/29 20:53:36  sueh
 * <p> $bug# 418 avoid updating a null FortranInputString parameter
 * <p> $
 * <p> $Revision 1.1  2004/03/25 00:48:11  sueh
 * <p> $bug# 409, bug# 418 Utility functions for Param objects
 * <p> $$ </p>
 */
package etomo.comscript;

import java.util.Vector;

public class ParamUtilities {
  public static final String rcsid = "$$Id$$";

  public static String getString(double value) {
    if (Double.isNaN(value)) {
      return new String();
    }
    return String.valueOf(value);
  }
  
  public static String[] getStrings(double[] values) {
    String[] strings = new String[values.length <= 0 ? 1 : values.length];
    if (values.length == 0) {
      strings[0] = new String();
    }
    for (int i = 0; i < values.length; i++) {
      strings[i] = getString(values[i]);
    }
    return strings;
  }


  public static double getDouble(String value) {
    if (value == null || !value.matches("\\S+")) {
      return Double.NaN;
    }
    return Double.parseDouble(value);
  }

  public static void set(String value, FortranInputString target)
    throws FortranInputSyntaxException {
    if (target == null) {
      throw new NullPointerException();
    }
    if (value == null) {
      target.setDefault();
    }
    else {
      target.validateAndSet(value);
    }
  }
  
  public static void set(String value, FortranInputString target, int index) {
    if (target == null) {
      throw new NullPointerException();
    }
    if (value == null) {
      target.setDefault(index);
    }
    else if (!value.matches("\\S+")) {
      target.setDefault(index);
    }
    else {
      target.set(index, Double.parseDouble(value));
    }
  }
  
  public static void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    String value)
    throws BadComScriptException {
    updateParameter(scriptCommand, key, value, false);
  }
  public static void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    String value,
    boolean required)
    throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (value != null && value.length() > 0) {
      scriptCommand.setValue(key, value);
    }
    else {
      scriptCommand.deleteKey(key);
      if (required) {
        throw new BadComScriptException(
          "MTF Filter:  Missing parameter value, " + key + ".");
      }
    }
  }

  public static void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    double value) {
    if (key == null) {
      throw new NullPointerException();
    }
    if (!Double.isNaN(value)) {
      scriptCommand.setValue(key, Double.toString(value));
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }

  public static void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    FortranInputString value) {
    if (key == null) {
      throw new NullPointerException();
    }
    if (value != null && value.valuesSet() && !value.isDefault()) {
      scriptCommand.setValue(key, value.toString());
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }
  
  public static void updateParameterStrings(
    ComScriptCommand scriptCommand,
    String key,
    Vector strings)
    throws BadComScriptException {
    updateParameterStrings(scriptCommand, key, strings, false);
  }
  public static void updateParameterStrings(
    ComScriptCommand scriptCommand,
    String key,
    Vector strings,
    boolean required)
    throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (strings == null || strings.size() == 0) {
      scriptCommand.deleteKeyAll(key);
      if (required) {
        throw new BadComScriptException(
          "MTF Filter:  Missing parameter value, " + key + ".");
      }
      return;
    }
    scriptCommand.setValues(
      key,
      (String[]) strings.toArray(new String[strings.size()]));
  }

  public static void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    double[] values)
    throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (values == null || values.length == 0) {
      scriptCommand.deleteKeyAll(key);
      return;
    }
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < values.length; i++) {
      buffer.append(String.valueOf(values[i]));
      if (i < values.length - 1) {
        buffer.append(",");
      }
    }
    scriptCommand.setValue(key, buffer.toString());
  }
  
  public static void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    boolean set) {
    if (key == null) {
      throw new NullPointerException();
    }
    if (set) {
      scriptCommand.setValue(key, "");
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }

}
