package etomo.type;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2008/04/02 02:19:24  sueh
 * <p> bug# 1097 Added a type because the type changes how an empty
 * <p> element displays itself.
 * <p> </p>
 */
public final class ParsedElementType {
  public static final ParsedElementType NON_MATLAB = new ParsedElementType();
  public static final ParsedElementType MATLAB = new ParsedElementType();
  static final ParsedElementType NON_MATLAB_ARRAY = new ParsedElementType();
  static final ParsedElementType MATLAB_ARRAY = new ParsedElementType();
  static final ParsedElementType STRING = new ParsedElementType();

  public String toString() {
    if (this == NON_MATLAB) {
      return "Non Matlab";
    }
    if (this == NON_MATLAB_ARRAY) {
      return "Non Matlab Array";
    }
    if (this == MATLAB) {
      return "Matlab";
    }
    if (this == MATLAB_ARRAY) {
      return "Matlab Array";
    }
    if (this == STRING) {
      return "String";
    }
    return "Unknown ParsedElementType";
  }

  boolean isArray() {
    if (this == NON_MATLAB_ARRAY || this == MATLAB_ARRAY) {
      return true;
    }
    return false;
  }

  boolean isMatlab() {
    if (this == MATLAB || this == MATLAB_ARRAY) {
      return true;
    }
    return false;
  }

  ParsedElementType toArrayInstance() {
    if (this == NON_MATLAB_ARRAY || this == MATLAB_ARRAY || this == STRING) {
      return this;
    }
    if (this == NON_MATLAB) {
      return NON_MATLAB_ARRAY;
    }
    if (this == MATLAB) {
      return MATLAB_ARRAY;
    }
    return NON_MATLAB_ARRAY;
  }
}
