package etomo.comscript;

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
  public static final String rcsid = 
  "$Id$";

  int nParams;

  boolean[] isInteger;

  double[] minimum;

  double[] maximum;

  Double[] value;

  /**
   * Create a FortranInputString with nParams parameters.
   * @param nParams the number of parameters
   */
  public FortranInputString(int nParams) {
    this.nParams = nParams;
    value = new Double[nParams];
    minimum = new double[nParams];
    maximum = new double[nParams];
    isInteger = new boolean[nParams];
    for (int i = 0; i < nParams; i++) {
      minimum[i] = -1 * Double.MAX_VALUE;
      maximum[i] = Double.MAX_VALUE;
      isInteger[i] = false;
      value[i] = new Double(Double.NEGATIVE_INFINITY);
    }
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

  /**
   * Set the String representation of the parameters and validate it against the
   * specified rules.
   */
  public void validateAndSet(String newValues)
  throws FortranInputSyntaxException {

    //  Handle a simple default string
    if (newValues.equals("/")) {
      for (int i = 0; i < value.length; i++) {
        value[i] = new Double(Double.NaN);
      }
      return;
    }

    // Walk through the newValues string parsing the values
    Double[] tempValue = new Double[value.length];
    for (int i = 0; i < value.length; i++) {
      tempValue[i] = new Double(Double.NEGATIVE_INFINITY);
    }
    int idxValue = 0;
    int idxStart = 0;
    while (idxStart < newValues.length()) {
      int idxDelim = newValues.indexOf(',', idxStart);
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
        idxStart = idxDelim + 1;
        idxValue++;
      }
      //  This should be the last value
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
    return;
  }

  /**
   * Get a specific value as a double
   */
  public double getDouble(int index) {
    return value[index].doubleValue();
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
  
  public void setDefault() {
    for(int i=0; i <nParams; i++) {
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
        buffer.insert(0, ',');
      }
      idxValue--;
    }
    return buffer.toString();
  }

  public String toString(boolean defaultIsBlank) {
    if (!defaultIsBlank) {
      return toString();
    }
    String string = toString();
    if (string.equals("/")) {
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
  public boolean valuesSet(){
    for(int i=0; i <nParams; i++) {
      if(value[i].isInfinite()){
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
    for(int i=0; i <nParams; i++) {
      if(! value[i].isNaN()){
        return false;
      }
    }
    return true;
  }
  
  public boolean isDefault(int index) {
    if (value[index] == null) {
      throw new NullPointerException("value[" + index + "]");
    }
    return value[index].isNaN();
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
      String message = 
      "Value below minimum.  Acceptable range: ["
      + String.valueOf(minimum[index])
      + ","
      + String.valueOf(maximum[index])
      + "] got "
      + String.valueOf(value);
      throw (new FortranInputSyntaxException(message, newValues));
    }
    if (value > maximum[index]) {
      String message = 
      "Value above maximum.  Acceptable range: ["
      + String.valueOf(minimum[index])
      + ","
      + String.valueOf(maximum[index])
      + "] got "
      + String.valueOf(value);
      throw (new FortranInputSyntaxException(message, newValues));
    }
  }
  
 
}
