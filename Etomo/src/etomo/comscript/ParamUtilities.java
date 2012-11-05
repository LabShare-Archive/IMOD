/**
 * <p>Description: A collection of static utility functions to work with 
 * Com script parameter objects.  There are four classes of functions:
 * 
 * 1. setParamIfPresent returns value of the specified parameter from the com
 *    script if it is present in the comscript, otherwise it returns the "not
 *    present value" specified in the argument list.  The purpose of these 
 *    functions is to assist in reading com scripts into parameter objects
 * 
 * 2. updateScriptParameter updates the ComScript object with specified
 *    parameter, deleting the keyword if the specified parameter equals the
 *    default value.  The purpose of these function is to assist in writing
 *    out new com scripts </p>
 * 
 * 3. valueOf returns the String representation of the parameter handling the
 *    not present or default case correctly
 * 
 * 4. parseType parses a string into the specfied type returning the correct not
 *    present or default value if the String is null or empty or white space
 *   
 * <p>Copyright: Copyright(c) 2002, 2003, 2004</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $$Author$$
 * 
 * @version $$Revision$$
 * 
 * <p> $$Log$
 * <p> $Revision 1.17  2007/03/03 00:36:12  sueh
 * <p> $bug# 973 In
 * <p> $updateScriptParameter(ComScriptCommand,String,FortranInputString) if the
 * <p> $FortranInputString is not active, the parameter is deleted in the
 * <p> $ComScriptCommand.
 * <p> $
 * <p> $Revision 1.16  2006/05/16 21:27:06  sueh
 * <p> $bug# 856 Changed setParamIfPresent(ComScriptCommand, String, StringList)
 * <p> $to create stringList if it is null.  Returns stringList.
 * <p> $
 * <p> $Revision 1.15  2005/05/09 23:10:29  sueh
 * <p> $bug# 658 Reformatted.
 * <p> $
 * <p> $Revision 1.14  2004/12/30 17:57:27  sueh
 * <p> $bug# 567 Fixed valueOf(FortranInputString[]) - for loop was wrong.
 * <p> $
 * <p> $Revision 1.13  2004/12/29 23:32:51  sueh
 * <p> $bug# 567 Adding the FortranInputString to parse(String...) and
 * <p> $parse(StringList...).
 * <p> $
 * <p> $Revision 1.12  2004/12/28 23:46:02  sueh
 * <p> $bug# 567 Add functions to handle StringList, and FortranInputString[].
 * <p> $
 * <p> $Revision 1.11  2004/06/25 18:06:58  sueh
 * <p> $bug# 484 adding function to return integer value or default
 * <p> $
 * <p> $Revision 1.10  2004/06/24 20:21:12  sueh
 * <p> $bug# 482 added an isEmpty() function for Strings
 * <p> $
 * <p> $Revision 1.9  2004/06/14 23:23:34  rickg
 * <p> $Bug #383  Changed interface to make it clearer which direction information
 * <p> $ is moving and what the likely use of each class of calls is.
 * <p> $$
 * <p> Revision 1.8  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p> 
 * <p> Revision 1.7  2004/06/09 21:18:12  rickg
 * <p> Changed upateParameter method to updateScriptParameter
 * <p> 
 * <p> Revision 1.6  2004/05/03 18:01:48  sueh
 * <p> bug# 418 adding more functions, fixing bugs.
 * <p> 
 * <p> Revision 1.5  2004/04/29 20:23:12  sueh
 * <p> bug# 427 corrected Double.NaN comparison.
 * <p> 
 * <p> Revision 1.4  2004/04/27 00:52:29  sueh
 * <p> bug# 427 adding update required parameter - vector of strings
 * <p> 
 * <p> Revision 1.3  2004/04/26 21:18:44  sueh
 * <p> bug# 427 add updateParameterStrings() for Vectors
 * <p> containing strings
 * <p> 
 * <p> Revision 1.2  2004/03/29 20:53:36  sueh
 * <p> bug# 418 avoid updating a null FortranInputString parameter
 * <p> 
 * <p> Revision 1.1  2004/03/25 00:48:11  sueh
 * <p> bug# 409, bug# 418 Utility functions for Param objects
 * </p>
 */
package etomo.comscript;

import java.util.Vector;

public class ParamUtilities {

  public static final int INT_NOT_SET = Integer.MIN_VALUE;
  private static final String zeroOrMoreWhiteSpace = "\\s*";

  /**
   * 
   * @param value
   * @return false if value is null, has a zero length, or contains only
   *         whitespace
   */
  public static boolean isEmpty(String value) {
    if (value == null || !value.matches("\\S+")) {
      return true;
    }
    return false;
  }

  /**
   * Return the string representation of the int value or an empty string if the
   * value is the not present value
   * @param value
   * @return
   */
  public static String valueOf(int value) {
    if (value == Integer.MIN_VALUE) {
      return new String();
    }
    return String.valueOf(value);
  }

  /**
   * Return the string representation of the float value or an empty string if
   * the value is the not present value
   * @param value
   * @return
   */
  public static String valueOf(float value) {
    if (Float.isNaN(value)) {
      return new String();
    }
    return String.valueOf(value);
  }

  /**
   * Return the string representation of the double value or an empty string if
   * the value is the not present value
   * @param value
   * @return
   */
  public static String valueOf(double value) {
    if (Double.isNaN(value)) {
      return new String();
    }
    return String.valueOf(value);
  }

  public static String[] valueOf(double[] values) {
    String[] strings = new String[values.length <= 0 ? 1 : values.length];
    if (values.length == 0) {
      strings[0] = new String();
    }
    for (int i = 0; i < values.length; i++) {
      strings[i] = valueOf(values[i]);
    }
    return strings;
  }

  public static String valueOf(FortranInputString[] valueArray) {
    if (valueArray == null || valueArray.length == 0) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(valueArray[0].toString(true));
    for (int i = 1; i < valueArray.length; i++) {
      buffer.append(" " + valueArray[i].toString(true));
    }
    return buffer.toString();
  }

  /**
   * Parse an integer value from a string, returning the default value if the
   * string is white space
   * @param value
   * @return
   */
  public static int parseInt(String value) {
    if (value == null || !value.matches("\\S+")) {
      return Integer.MIN_VALUE;
    }
    return Integer.parseInt(value);
  }

  /**
   * Parse a FortranInputString array from a string.  Use StringList to split
   * the string at whitespace.
   * @param value
   * @param integerType
   * @return
   * @throws FortranInputSyntaxException
   */
  public static FortranInputString[] parse(String value, boolean integerType,
      int inputStringSize) throws FortranInputSyntaxException {
    if (value == null || value.matches(zeroOrMoreWhiteSpace)) {
      return null;
    }
    StringList stringList = new StringList();
    stringList.parseString(value);
    boolean[] integerTypeArray = new boolean[inputStringSize];
    for (int i = 0; i < integerTypeArray.length; i++) {
      integerTypeArray[i] = integerType;
    }
    return parse(stringList, integerTypeArray, inputStringSize);
  }

  /**
   * Parse a FortranInputString array from a StringList.
   * @param stringList
   * @param size
   * @param integerType
   * @return
   * @throws FortranInputSyntaxException
   */
  public static FortranInputString[] parse(StringList stringList, boolean[] integerType,
      int inputStringSize) throws FortranInputSyntaxException {
    if (stringList != null && stringList.getNElements() > 0) {
      int stringListSize = stringList.getNElements();
      FortranInputString[] inputStringArray = new FortranInputString[stringListSize];
      for (int i = 0; i < stringListSize; i++) {
        inputStringArray[i] = new FortranInputString(inputStringSize);
        inputStringArray[i].setIntegerType(integerType);
        inputStringArray[i].validateAndSet(stringList.get(i));
      }
      return inputStringArray;
    }
    return null;
  }

  /**
   * Returns value or defaultValue if value isn't set
   * @param value
   * @param defaultValue
   * @return
   */
  public static int get(int value, int defaultValue) {
    if (value == INT_NOT_SET) {
      return defaultValue;
    }
    return value;
  }

  /**
   * Parse a double value from a string returning the default value the string
   * is white space
   * @param value
   * @return
   */
  public static double parseDouble(String value) {
    if (value == null || !value.matches("\\S+")) {
      return Double.NaN;
    }
    return Double.parseDouble(value);
  }

  /**
   * Sets a FortranInputString from a string
   * @param value
   * @param target
   * @throws FortranInputSyntaxException
   */
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

  public static FortranInputString[] setParamIfPresent(ComScriptCommand scriptCommand,
      String keyword, int size, boolean[] integerType) throws InvalidParameterException,
      FortranInputSyntaxException {
    if (scriptCommand.hasKeyword(keyword)) {
      return parse(new StringList(scriptCommand.getValues(keyword)), integerType, size);
    }
    return null;
  }

  /**
   * Return the boolean parameter if it is present in the com script command
   * object, otherwise return the notPresentValue.
   * @param scriptCommand
   * @param keyword
   * @param notPresentValue
   * @return
   * @throws InvalidParameterException
   */
  public static boolean setParamIfPresent(ComScriptCommand scriptCommand, String keyword,
      boolean notPresentValue) throws InvalidParameterException {

    return (scriptCommand.hasKeyword(keyword)) ? true : notPresentValue;
  }

  /**
   * Return the string parameter if it is present in the com script command
   * object, otherwise return the notPresentValue
   * @param scriptCommand
   * @param keyword
   * @param notPresentValue
   * @return
   * @throws InvalidParameterException
   */
  public static String setParamIfPresent(ComScriptCommand scriptCommand, String keyword,
      String notPresentValue) throws InvalidParameterException {

    return (scriptCommand.hasKeyword(keyword)) ? scriptCommand.getValue(keyword)
        : notPresentValue;
  }

  /**
   * Return the int parameter if it is present in the com script command
   * object, otherwise return the notPresentValue
   * @param scriptCommand
   * @param keyword
   * @param notPresentValue
   * @return
   * @throws InvalidParameterException NumberFormatException 
   */
  public static int setParamIfPresent(ComScriptCommand scriptCommand, String keyword,
      int notPresentValue) throws InvalidParameterException, NumberFormatException {

    return (scriptCommand.hasKeyword(keyword)) ? Integer.parseInt(scriptCommand
        .getValue(keyword)) : notPresentValue;
  }

  /**
   * Return the double parameter if it is present in the com script command
   * object, otherwise return the notPresentValue
   * @param scriptCommand
   * @param keyword
   * @param notPresentValue
   * @return
   * @throws InvalidParameterException NumberFormatException 
   */
  public static double setParamIfPresent(ComScriptCommand scriptCommand, String keyword,
      double notPresentValue) throws InvalidParameterException, NumberFormatException {

    return (scriptCommand.hasKeyword(keyword)) ? Double.parseDouble(scriptCommand
        .getValue(keyword)) : notPresentValue;
  }

  /**
   * Set the FortranInputString parameter if it is present in the com script
   * command object
   * @param scriptCommand
   * @param keyword
   * @param parameter
   * @throws FortranInputSyntaxException, InvalidParameterException
   */
  public static void setParamIfPresent(ComScriptCommand scriptCommand, String keyword,
      FortranInputString fisParameter) throws FortranInputSyntaxException,
      InvalidParameterException {

    if (scriptCommand.hasKeyword(keyword)) {
      fisParameter.validateAndSet(scriptCommand.getValue(keyword));
    }
  }

  /**
   * Set the StringList parameter if it is present in the com script
   * command object
   * @param scriptCommand
   * @param keyword
   * @param parameter
   * @throws FortranInputSyntaxException, InvalidParameterException
   */
  public static StringList setParamIfPresent(ComScriptCommand scriptCommand,
      String keyword, StringList stringList) throws InvalidParameterException {

    if (scriptCommand.hasKeyword(keyword)) {
      if (stringList == null) {
        stringList = new StringList();
      }
      stringList.parseString(scriptCommand.getValue(keyword));
    }
    return stringList;
  }

  /**
   * Update the specified com script parameter with the supplied value
   * @param scriptCommand
   * @param key
   * @param value
   * @throws BadComScriptException
   */
  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
      String value) throws BadComScriptException {

    updateScriptParameter(scriptCommand, key, value, false);
  }

  /**
   * 
   * @param scriptCommand
   * @param key
   * @param value
   * @param required
   * @throws BadComScriptException
   */
  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
      String value, boolean required) throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (key.matches(zeroOrMoreWhiteSpace)) {
      throw new IllegalArgumentException();
    }
    if (value != null && !value.matches(zeroOrMoreWhiteSpace)) {
      scriptCommand.setValue(key, value);
    }
    else {
      scriptCommand.deleteKey(key);
      if (required) {
        throw new BadComScriptException(scriptCommand.getCommand()
            + " missing required parameter: " + key + ".");
      }
    }
  }

  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
      StringList value) throws BadComScriptException {
    if (key == null) {
      throw new NullPointerException();
    }
    if (key.matches(zeroOrMoreWhiteSpace)) {
      throw new IllegalArgumentException();
    }
    if (value != null && value.getNElements() > 0) {
      scriptCommand.setValue(key, value.toString());
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }

  /**
   * 
   * @param scriptCommand
   * @param key
   * @param value
   */
  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
      int value) {

    if (key == null) {
      throw new NullPointerException();
    }
    if (!(value == Integer.MIN_VALUE)) {
      scriptCommand.setValue(key, Integer.toString(value));
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }

  /**
   * @param scriptCommand
   * @param key
   * @param value
   */
  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
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

  /**
   * 
   * @param scriptCommand
   * @param key
   * @param value
   */
  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
      FortranInputString value) {
    if (key == null) {
      throw new NullPointerException();
    }
    if (!value.isActive()) {
      scriptCommand.deleteKey(key);
      return;
    }
    if (value != null && value.valuesSet() && !value.isDefault()) {
      scriptCommand.setValue(key, value.toString());
    }
    else {
      scriptCommand.deleteKey(key);
    }
  }

  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
      FortranInputString[] value) {
    if (key == null) {
      throw new NullPointerException();
    }
    if (key.matches(zeroOrMoreWhiteSpace)) {
      throw new IllegalArgumentException();
    }
    if (value == null || value.length == 0) {
      scriptCommand.deleteKey(key);
    }
    else {
      StringBuffer buffer = new StringBuffer(value[0].toString());
      for (int i = 1; i < value.length; i++) {
        buffer.append(" " + value[i].toString());
      }
      scriptCommand.setValue(key, buffer.toString());
    }
  }

  /**
   * 
   * @param scriptCommand
   * @param key
   * @param values
   * @throws BadComScriptException
   */
  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
      double[] values) throws BadComScriptException {

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

  /**
   * 
   * @param scriptCommand
   * @param key
   * @param set
   */
  public static void updateScriptParameter(ComScriptCommand scriptCommand, String key,
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

  /**
   * 
   * @param scriptCommand
   * @param key
   * @param strings
   * @throws BadComScriptException
   */
  public static void updateScriptParameterStrings(ComScriptCommand scriptCommand,
      String key, Vector strings) throws BadComScriptException {

    updateScriptParameterStrings(scriptCommand, key, strings, false);
  }

  /**
   * 
   * @param scriptCommand
   * @param key
   * @param strings
   * @param required
   * @throws BadComScriptException
   */
  public static void updateScriptParameterStrings(ComScriptCommand scriptCommand,
      String key, Vector strings, boolean required) throws BadComScriptException {

    if (key == null) {
      throw new NullPointerException();
    }
    if (strings == null || strings.size() == 0) {
      scriptCommand.deleteKeyAll(key);
      if (required) {
        throw new BadComScriptException(scriptCommand.getCommand()
            + " missing required parameter: " + key + ".");
      }
      return;
    }
    scriptCommand.setValues(key, (String[]) strings.toArray(new String[strings.size()]));
  }
}