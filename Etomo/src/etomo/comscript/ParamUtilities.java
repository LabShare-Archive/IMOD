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
 * <p> $$Log$$ </p>
 */
package etomo.comscript;

public class ParamUtilities {
  public static final String rcsid = "$$Id$$";

  public static String getString(double value) {
    if (Double.isNaN(value)) {
      return new String();
    }
    return String.valueOf(value);
  }

  public static double setDouble(String value) {
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
    target.validateAndSet(value);
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
    double value)
    throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (value != Double.NaN) {
      scriptCommand.setValue(key, Double.toString(value));
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }

  public static void updateParameter(
    ComScriptCommand scriptCommand,
    String key,
    FortranInputString value)
    throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (!value.isDefault()) {
      scriptCommand.setValue(key, value.toString());
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }

}
