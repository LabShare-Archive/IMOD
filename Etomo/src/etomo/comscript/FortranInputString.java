package etomo.comscript;

import java.util.Properties;

import etomo.type.ConstEtomoNumber;

/**
 * <p>Description: The FortranInputString class models the multiple parameter
 * FORTRAN input formatting present in the IMOD utilities.  It also allows for
 * range and type validation of the parameters.</p>
 *
 * <p> Implementation details:  Double.NaN is used internaly to represent 
 * David's remaining default input specifier /.  Double.NEGATIVE_INFINITY is
 * used to represent an uninitialized value.
 * 
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.23  2011/06/01 01:12:35  sueh
 * <p> Bug# 1471 In validateAndSet(String) throwing FortranInputSyntaxException instead of
 * <p> NumberFormatException.
 * <p>
 * <p> Revision 3.22  2011/05/24 23:10:45  sueh
 * <p> Bug# 1471 Added a new, private validateAndSet which also takes a divider character.  Allowing both
 * <p> comma and space dividers without calling setdivider.
 * <p>
 * <p> Revision 3.21  2011/05/20 03:51:05  sueh
 * <p> Bug# 1471 In validateAndSet catch NumberFormatException and throw FortranInputSyntaxException.
 * <p>
 * <p> Revision 3.20  2011/04/04 16:46:53  sueh
 * <p> bug# 1416 Added toString.
 * <p>
 * <p> Revision 3.19  2011/02/22 03:14:03  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 3.18  2010/03/03 04:52:10  sueh
 * <p> bug# 1311 Reformatted.
 * <p>
 * <p> Revision 3.17  2009/06/05 01:48:23  sueh
 * <p> bug# 1219 Added isNull functions to deal with the two ways this class is
 * <p> empty.  In set(int,String) changing the value to default when the input is
 * <p> empty.
 * <p>
 * <p> Revision 3.16  2007/08/16 16:28:46  sueh
 * <p> bug# 1035 Added propertiesKey for saving to a Properties object.  Added
 * <p> functions load(Properties, String) and makePrepend.
 * <p>
 * <p> Revision 3.15  2007/03/03 00:34:40  sueh
 * <p> bug# 973 Added active and isEmpty().
 * <p>
 * <p> Revision 3.14  2006/09/05 17:34:24  sueh
 * <p> bug# 917 Added option to change the default divider (',').
 * <p>
 * <p> Revision 3.13  2006/08/25 22:51:07  sueh
 * <p> bug# 918 Added convenience functions
 * <p> updateScriptParameter(ComScriptCommand) and
 * <p> validateAndSet(ComScriptCommand).
 * <p>
 * <p> Revision 3.12  2005/05/09 22:55:50  sueh
 * <p> bug# 658 Added FortranInputString(double[]).  Added
 * <p> double[] getDouble().  Added new set functions.  Added
 * <p> isEmpty(int index) and isIntegerType(int index).
 * <p>
 * <p> Revision 3.11  2004/12/30 17:54:14  sueh
 * <p> bug# 567 Using Double.NaN as the null value everywhere.
 * <p>
 * <p> Revision 3.10  2004/12/28 23:41:48  sueh
 * <p> bug# 567 Add size() to return nParams.
 * <p>
 * <p> Revision 3.9  2004/03/13 02:25:47  sueh
 * <p> bug# 373 Added toString(boolean defaultIsBlank) - turns "/" into ""
 * <p>
 * <p> Revision 3.8  2004/03/12 20:58:19  sueh
 * <p> bug# 373 Added setDefault().
 * <p>
 * <p> Revision 3.7  2004/03/12 20:01:21  sueh
 * <p> bug# 412 added setDefault(int), toString(int)
 * <p>
 * <p> Revision 3.6  2004/03/12 17:30:33  sueh
 * <p> bug# 412 fixed null pointer bug, added valueSet(int) and isDefault(int)
 * <p>
 * <p> Revision 3.5  2004/03/11 23:28:44  rickg
 * <p> Added isDefault method
 * <p>
 * <p> Revision 3.4  2004/02/26 17:58:30  sueh
 * <p> bug# 404 fixing typo
 * <p>
 * <p> Revision 3.3  2004/02/13 01:03:25  rickg
 * <p> Renamed valuesSet method
 * <p>
 * <p> Revision 3.2  2004/02/12 04:42:01  rickg
 * <p> Removed diagnostic printing
 * <p>
 * <p> Revision 3.1  2004/02/12 04:35:29  rickg
 * <p> Major parsing change to match PIP structure
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.1  2003/03/20 17:22:45  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.1.2.1  2003/01/24 18:33:42  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class FortranInputString {
  public static final String rcsid = "$Id$";

  static final char DEFAULT_DIVIDER = ',';

  private int nParams;
  private boolean[] isInteger;
  private double[] minimum;
  private double[] maximum;
  private Double[] value;
  private String key = null;
  private char divider = DEFAULT_DIVIDER;
  private boolean active = true;
  private String propertiesKey = null;

  /**
   * Create a FortranInputString with nParams parameters.
   * @param nParams the number of parameters
   */
  public FortranInputString(int nParams) {
    initialize(nParams);
  }

  public FortranInputString(String key, int nParams) {
    this.key = key;
    initialize(nParams);
  }

  /**
   * Copy constructor
   */
  public FortranInputString(FortranInputString src) {
    nParams = src.nParams;
    value = new Double[nParams];
    minimum = new double[nParams];
    maximum = new double[nParams];
    isInteger = new boolean[nParams];
    for (int i = 0; i < nParams; i++) {
      value[i] = src.value[i];
      minimum[i] = src.minimum[i];
      maximum[i] = src.maximum[i];
      isInteger[i] = src.isInteger[i];
    }
  }

  public FortranInputString(double[] values) {
    if (values == null) {
      initialize(0);
      return;
    }
    initialize(value.length);
    for (int i = 0; i < value.length; i++) {
      set(i, values[i]);
    }
  }

  /**
   * 
   * @param props
   * @param group
   * @return an instance of FortranInputString loaded from props.
   * @throws FortranInputSyntaxException
   */
  public static FortranInputString getInstance(Properties props, String group)
      throws FortranInputSyntaxException {
    String list = props.getProperty(group);
    return getInstance(list);
  }

  public static FortranInputString getInstance(String list)
      throws FortranInputSyntaxException {
    if (list == null || list.length() == 0) {
      return new FortranInputString(0);
    }
    int nParams = list.split(",").length;
    FortranInputString instance = new FortranInputString(nParams);
    instance.validateAndSet(list);
    return instance;
  }

  public static FortranInputString getInstance(double[] list) {
    if (list == null) {
      return new FortranInputString(0);
    }
    FortranInputString instance = new FortranInputString(list.length);
    for (int i = 0; i < list.length; i++) {
      instance.set(i, list[i]);
    }
    return instance;
  }

  /**
   * Loads from props, does not set nParams.
   * @param props
   * @param group
   */
  public void load(Properties props, String group) {
    String list = props.getProperty(makePrepend(group));
    try {
      validateAndSet(list);
    }
    catch (FortranInputSyntaxException e) {
      reset();
    }
  }

  private String makePrepend(String group) {
    if (propertiesKey != null) {
      if (group == null || group.matches("\\s*")) {
        group = propertiesKey;
      }
      else {
        group = group + "." + propertiesKey;
      }
    }
    return group;
  }

  public void store(Properties props, String group) {
    props.setProperty(makePrepend(group), toString());
  }

  void reset() {
    value = new Double[nParams];
    for (int i = 0; i < nParams; i++) {
      value[i] = new Double(Double.NEGATIVE_INFINITY);
    }
  }

  public void setActive(boolean active) {
    this.active = active;
  }

  public boolean isActive() {
    return active;
  }

  private void initialize(int nParams) {
    this.nParams = nParams;
    minimum = new double[nParams];
    maximum = new double[nParams];
    isInteger = new boolean[nParams];
    value = new Double[nParams];
    for (int i = 0; i < nParams; i++) {
      minimum[i] = -1 * Double.MAX_VALUE;
      maximum[i] = Double.MAX_VALUE;
      isInteger[i] = false;
      value[i] = new Double(Double.NEGATIVE_INFINITY);
    }
  }

  /**
   * Set the valid range for all of the values.
   * @param min the minimum valid value for the parameters.
   * @param max the maximum valid value for the parameters.
   */
  public void setRange(double min, double max) {
    for (int i = 0; i < minimum.length; i++) {
      minimum[i] = min;
    }
    for (int i = 0; i < maximum.length; i++) {
      maximum[i] = max;
    }
  }

  public int size() {
    return nParams;
  }

  public void updateScriptParameter(ComScriptCommand scriptCommand) {
    ParamUtilities.updateScriptParameter(scriptCommand, key, this);
  }

  void setDivider(char divider) {
    this.divider = divider;
  }

  void resetDivider() {
    divider = DEFAULT_DIVIDER;
  }

  public void validateAndSet(ComScriptCommand scriptCommand)
      throws FortranInputSyntaxException, InvalidParameterException {
    if (key == null) {
      throw new IllegalStateException("key == null");
    }
    if (scriptCommand.hasKeyword(key)) {
      validateAndSet(scriptCommand.getValue(key));
    }
  }

  /**
   * Allow two different dividers (the default and a space) without calling setDivider.
   * @param newValues
   * @throws FortranInputSyntaxException
   */
  public void validateAndSet(final String newValues) throws FortranInputSyntaxException {
    try {
      validateAndSet(newValues, divider);
    }
    catch (NumberFormatException dividerException) {
      try {
        // Didn't work, try an alternative divider
        char origDivider = divider;
        if (divider == DEFAULT_DIVIDER) {
          validateAndSet(newValues, ' ');
        }
        else if (divider == ' ') {
          validateAndSet(newValues, DEFAULT_DIVIDER);
        }
        else {
          // Unfamiliar divider, throw a FortranInputSyntaxException so the problem will
          // be
          // handled with a pop up.
          throw new FortranInputSyntaxException(dividerException.getMessage());
        }
      }
      catch (NumberFormatException e) {
        // Error happened with the alternative divider, throw a
        // FortranInputSyntaxException
        // so the problem will be handled with a pop up.
        throw new FortranInputSyntaxException(e.getMessage());
      }
    }
  }

  /**
   * Set the String representation of the parameters and validate it against the
   * specified rules.
   */
  private void validateAndSet(String newValues, final char dividerParam)
      throws FortranInputSyntaxException {
    if (newValues == null) {
      newValues = "";
    }
    // Handle a simple default string
    if (newValues.equals("/")) {
      for (int i = 0; i < value.length; i++) {
        value[i] = new Double(Double.NaN);
      }
      return;
    }
    // Walk through the newValues string parsing the values
    Double[] tempValue = new Double[value.length];
    for (int i = 0; i < value.length; i++) {
      tempValue[i] = new Double(Double.NaN);
    }
    int idxValue = 0;// current index of tempValue
    int idxStart = 0;// current index of newValues
    while (idxStart < newValues.length()) {
      int idxDelim = newValues.indexOf(dividerParam, idxStart);
      if (idxDelim != -1) {
        String currentToken = newValues.substring(idxStart, idxDelim);

        // A default value
        if (currentToken.length() == 0) {
          tempValue[idxValue] = new Double(Double.NaN);
        }
        else {
          tempValue[idxValue] = new Double(currentToken);
          rangeCheck(tempValue[idxValue].doubleValue(), idxValue, newValues);
        }
        idxValue++;
        idxStart = idxDelim + 1;
      }
      // This should be the last value
      else {
        String currentToken = newValues.substring(idxStart);
        if (currentToken.endsWith("/")) {
          tempValue[idxValue] = new Double(currentToken.substring(0,
              currentToken.length() - 1));
          rangeCheck(tempValue[idxValue].doubleValue(), idxValue, newValues);
          idxValue++;
          while (idxValue < nParams) {
            tempValue[idxValue] = new Double(Double.NaN);
            idxValue++;
          }
        }
        else {
          tempValue[idxValue] = new Double(newValues.substring(idxStart));
        }
        break;
      }
    }
    value = tempValue;
  }

  /**
   * Get a specific value as a double
   */
  public double getDouble(int index) {
    return value[index].doubleValue();
  }

  /**
   * @return get input string as doubles
   */
  public double[] getDouble() {
    double[] array = new double[nParams];
    for (int i = 0; i < nParams; i++) {
      array[i] = value[i].doubleValue();
    }
    return array;
  }

  /**
   * Get a specific value as an integer
   */
  public int getInt(int index) {
    return value[index].intValue();
  }

  /**
   * Set the value of given parameter
   */
  public void set(int index, double newValue) {
    value[index] = new Double(newValue);
  }

  public void set(int index, String newValue) {
    if (newValue == null || newValue.matches("\\s*")) {
      setDefault(index);
    }
    else {
      value[index] = new Double(String.valueOf(newValue));
    }
  }

  public void set(int index, ConstEtomoNumber newValue) {
    value[index] = new Double(newValue.getDouble());
  }

  public void set(int index, FortranInputString newValue) {
    value[index] = new Double(newValue.getDouble(index));
  }

  public void set(FortranInputString newValue) {
    for (int i = 0; i < nParams; i++) {
      value[i] = new Double(newValue.getDouble(i));
    }
  }

  public void setPropertiesKey(String input) {
    propertiesKey = input;
  }

  public void setDefault() {
    for (int i = 0; i < nParams; i++) {
      value[i] = new Double(Double.NaN);
    }
  }

  public void setDefault(int index) {
    value[index] = new Double(Double.NaN);
  }

  /**
   * Return the string representation of the parameters
   */
  public String toString() {
    if (value[0] == null) {
      return "Unitialized!";
    }

    // Walk backwards from the end of the array collecting any default value
    // into /

    boolean trailingDefault = false;
    int idxValue = value.length - 1;
    while (idxValue >= 0 && value[idxValue].isNaN()) {
      trailingDefault = true;
      idxValue--;
    }
    StringBuffer buffer = new StringBuffer();
    if (trailingDefault) {
      buffer.append("/");
    }
    while (idxValue >= 0) {
      if (!value[idxValue].isNaN()) {
        if (isInteger[idxValue]) {
          buffer.insert(0, String.valueOf(value[idxValue].intValue()));
        }
        else {
          buffer.insert(0, String.valueOf(value[idxValue]));
        }
      }
      if (idxValue != 0) {
        buffer.insert(0, divider);
      }
      idxValue--;
    }
    return buffer.toString();
  }

  public String toString(boolean defaultIsBlank) {
    if (!defaultIsBlank) {
      return toString();
    }
    if (isDefault()) {
      return "";
    }
    if (isNull()) {
      return "";
    }
    if (isEmpty()) {
      return "";
    }
    String string = toString();
    if (string.equals("/")) {
      return "";
    }
    return string;
  }

  public String toString(int index, boolean defaultIsBlank) {
    if (!defaultIsBlank) {
      return toString(index);
    }
    String string = toString(index);
    if (string.equals("Uninitialized!")) {
      return "";
    }
    return string;
  }

  public String toString(int index) {
    if (!valueSet(index)) {
      return "Uninitialized!";
    }
    if (isDefault(index)) {
      return "";
    }
    if (isInteger[index]) {
      return String.valueOf(value[index].intValue());
    }
    else {
      return String.valueOf(value[index]);
    }
  }

  /**
   * Sets all the elements to either integer or not integer
   * @param isInteger
   */
  public void setIntegerType(boolean isInteger) {
    for (int i = 0; i < this.isInteger.length; i++) {
      this.isInteger[i] = isInteger;
    }
  }

  /**
   * Set the specified element to be an integer.
   */
  public void setIntegerType(int index, boolean isInteger) {
    this.isInteger[index] = isInteger;
  }

  /**
   * Set the integer state for all values.
   * @param isIntArray a boolean array specifying the integer state for each
   * value.
   */
  public void setIntegerType(boolean[] isIntArray) {
    for (int i = 0; i < isIntArray.length; i++) {
      isInteger[i] = isIntArray[i];
    }
  }

  /**
   * Are any of the values unset
   * @return
   */
  public boolean valuesSet() {
    for (int i = 0; i < nParams; i++) {
      if (value[i].isInfinite()) {
        return false;
      }
    }
    return true;
  }

  public boolean valueSet(int index) {
    return value[index] != null && !value[index].isInfinite();
  }

  /**
   * Are all of the values set to their defaults
   * @return
   */
  public boolean isDefault() {
    for (int i = 0; i < nParams; i++) {
      if (!value[i].isNaN()) {
        return false;
      }
    }
    return true;
  }

  /**
   * 
   * @param index
   * @return is value defaulted at index
   */
  public boolean isDefault(int index) {
    if (value[index] == null) {
      throw new NullPointerException("value[" + index + "]");
    }
    return value[index].isNaN();
  }

  /**
   * 
   * @param index
   * @return is value not initialized at index
   */
  public boolean isEmpty(int index) {
    if (value[index] == null) {
      throw new NullPointerException("value[" + index + "]");
    }
    return value[index].isInfinite();
  }
  
  /**
   * Compares this.value to input.value.  The two arrays can be different sizes; the
   * smaller array is equal if the larger contains nulls for the outsized portion of the
   * array.
   * @param input
   * @return
   */
  public boolean equals(final FortranInputString input) {
    if (input == null) {
      return isNull();
    }
    int max = Math.max(nParams, input.nParams);
    for (int i = 0; i < max; i++) {
      if (!equals(i, input)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Compares this.value[index] to input.value[index].  Handles an out-of-range index by
   * comparing to null.  Null and isNull() are considered equal.
   * @param index
   * @param input
   * @return
   */
  public boolean equals(final int index, final FortranInputString input) {
    Double thisElement = index < nParams ? value[index] : null;
    Double inputElement = index < input.nParams ? input.value[index] : null;
    // handle out of range and null values
    if ((thisElement == null || isNull(index))
        && (inputElement == null || input.isNull(index))) {
      return true;
    }
    if (thisElement == null || isNull(index) || inputElement == null
        || input.isNull(index)) {
      return false;
    }
    // compare
    if (isInteger[index] && input.isInteger[index]) {
      return thisElement.intValue() == inputElement.intValue();
    }
    if (isInteger[index] && !input.isInteger[index]) {
      return thisElement.intValue() == inputElement.doubleValue();
    }
    if (!isInteger[index] && input.isInteger[index]) {
      return thisElement.doubleValue() == inputElement.intValue();
    }
    if (!isInteger[index] && !input.isInteger[index]) {
      return thisElement.doubleValue() == inputElement.doubleValue();
    }
    return false;
  }

  /**
   * Checks for both NaN and infinity.  Returns true for range indices.
   * @return true if the element is null or contains infinity or NaN
   */
  public boolean isNull(int index) {
    if (index >= nParams) {
      return true;
    }
    if (value[index] == null) {
      return true;
    }
    Double v = value[index];
    return v.isInfinite() || v.isNaN();
  }

  /**
   * Checks for both NaN and infinity.
   * @return true unless an element of value does not contain infinity or NaN
   */
  public boolean isNull() {
    for (int i = 0; i < nParams; i++) {
      Double element = value[i];
      if (!element.isInfinite() && !element.isNaN()) {
        return false;
      }
    }
    return true;
  }

  /**
   * @return false if any value is set
   */
  public boolean isEmpty() {
    for (int i = 0; i < nParams; i++) {
      if (!value[i].isInfinite()) {
        return false;
      }
    }
    return true;
  }

  /**
   * 
   * @param index
   * @return is value treated as an integer at index
   */
  public boolean isIntegerType(int index) {
    if (value[index] == null) {
      throw new NullPointerException("value[" + index + "]");
    }
    return isInteger[index];
  }

  /**
   * Compare given value range specified for index.
   * @param value
   * @param index
   * @param newValues
   * @throws FortranInputSyntaxException
   */
  private void rangeCheck(double value, int index, String newValues)
      throws FortranInputSyntaxException {
    if (value < minimum[index]) {
      String message = key == null ? "" : key + ": "
          + "Value below minimum.  Acceptable range: [" + String.valueOf(minimum[index])
          + "," + String.valueOf(maximum[index]) + "] got " + String.valueOf(value);
      throw (new FortranInputSyntaxException(message, newValues));
    }
    if (value > maximum[index]) {
      String message = key == null ? "" : key + ": "
          + "Value above maximum.  Acceptable range: [" + String.valueOf(minimum[index])
          + "," + String.valueOf(maximum[index]) + "] got " + String.valueOf(value);
      throw (new FortranInputSyntaxException(message, newValues));
    }
  }

}
