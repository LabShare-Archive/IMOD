package etomo.comscript;

/**
 * <p>Description: The FortranInputString class models the multiple parameter
 * FORTRAN input formatting present in the IMOD utilities.  It also allows for
 * range and type validation of the parameters.</p>
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

    // separate the sting into comma separated tokens and check for the right
    // number of parameters or the default specifier
    String[] tokens = newValues.split(",");

    if (tokens.length < nParams) {
      String lastToken = tokens[tokens.length - 1];
      if (lastToken.endsWith("/")) {
        tokens[tokens.length - 1] =
          lastToken.substring(0, lastToken.length() - 1);
      }
      else {
        String message =
          "Incorrect number of parameters.  Expected "
            + String.valueOf(nParams)
            + " got "
            + String.valueOf(tokens.length);

        throw (new FortranInputSyntaxException(message, newValues));

      }
    }

    // validate the range of each value if it is not a default
    // TODO walking the data twice is not the most efficient but we don't
    // want to change the state of the object if the input data is invalid
    for (int i = 0; i < tokens.length; i++) {
      double test = Double.parseDouble(tokens[i]);

      if (test < minimum[i]) {
        String message =
          "Value below minimum.  Acceptable range: ["
            + String.valueOf(minimum[i])
            + ","
            + String.valueOf(maximum[i])
            + "] got "
            + String.valueOf(test);
        throw (new FortranInputSyntaxException(message, newValues));
      }
      if (test > maximum[i]) {
        String message =
          "Value above maximum.  Acceptable range: ["
            + String.valueOf(minimum[i])
            + ","
            + String.valueOf(maximum[i])
            + "] got "
            + String.valueOf(test);
        throw (new FortranInputSyntaxException(message, newValues));
      }
    }

    // parse the tokens into the value array,
    // filling in any default values
    for (int i = 0; i < tokens.length; i++) {
      value[i] = new Double(tokens[i]);
    }
    for (int i = tokens.length; i < nParams; i++) {
      value[i] = new Double(Double.NaN);
    }
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

  /**
   * Return the string representation of the parameters
   */
  public String toString() {

    if (value[0] == null) {
      return "Unitialized!";
    }

    if (value[0].isNaN()) {
      return "/";
    }

    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < value.length; i++) {
      if (isInteger[i]) {
        buffer.append(String.valueOf(value[i].intValue()));
      }
      else {
        buffer.append(String.valueOf(value[i]));
      }
      if (i < value.length - 1) {
        if (value[i + 1].isNaN()) {
          buffer.append("/");
          return buffer.toString();
        }
        else {
          buffer.append(",");
        }
      }

    }
    return buffer.toString();
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
}
