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
 * <p> Revision 1.5  2009/09/05 00:32:32  sueh
 * <p> bug# 1256 Removed ParsedIteratorDescriptor.
 * <p>
 * <p> Revision 1.4  2008/09/10 21:03:49  sueh
 * <p> bug# 1135 ParsedArrayDescriptor now has its own type.  This is because a ParsedNumber which is of type MATLAB_ARRAY_DESCRIPTOR must never show a "NaN" value.
 * <p>
 * <p> Revision 1.3  2008/06/20 20:01:23  sueh
 * <p> bug# 1119 Added NON_MATLAB_ITERATOR_ARRAY, which is the only type
 * <p> to use with ParsedIteratorDescriptor.  For clarity _NUMBER to MATLAB and
 * <p> NON_MATLAB.
 * <p>
 * <p> Revision 1.2  2008/04/15 21:26:53  sueh
 * <p> bug# 1105 Added array types.
 * <p>
 * <p> Revision 1.1  2008/04/02 02:19:24  sueh
 * <p> bug# 1097 Added a type because the type changes how an empty
 * <p> element displays itself.
 * <p> </p>
 */
public final class ParsedElementType {
  public static final ParsedElementType NON_MATLAB_NUMBER = new ParsedElementType();
  public static final ParsedElementType MATLAB_NUMBER = new ParsedElementType();
  static final ParsedElementType NON_MATLAB_ARRAY = new ParsedElementType();
  public static final ParsedElementType MATLAB_ARRAY = new ParsedElementType();
  public static final ParsedElementType MATLAB_ARRAY_DESCRIPTOR = new ParsedElementType();
  static final ParsedElementType STRING = new ParsedElementType();

  public String toString() {
    if (this == NON_MATLAB_NUMBER) {
      return "Non Matlab Number";
    }
    if (this == NON_MATLAB_ARRAY) {
      return "Non Matlab Array";
    }
    if (this == MATLAB_NUMBER) {
      return "Matlab Number";
    }
    if (this == MATLAB_ARRAY) {
      return "Matlab Array";
    }
    if (this == MATLAB_ARRAY_DESCRIPTOR) {
      return "Matlab Array Descriptor";
    }
    if (this == STRING) {
      return "String";
    }
    return "Unknown ParsedElementType";
  }

  boolean isArray() {
    if (this == NON_MATLAB_ARRAY || this == MATLAB_ARRAY
        || this == MATLAB_ARRAY_DESCRIPTOR) {
      return true;
    }
    return false;
  }

  boolean isMatlab() {
    if (this == MATLAB_NUMBER || this == MATLAB_ARRAY || this == MATLAB_ARRAY_DESCRIPTOR) {
      return true;
    }
    return false;
  }

  ParsedElementType toArrayInstance() {
    if (this == NON_MATLAB_ARRAY || this == MATLAB_ARRAY
        || this == MATLAB_ARRAY_DESCRIPTOR || this == STRING) {
      //Already an array or has no array equivalent
      return this;
    }
    if (this == NON_MATLAB_NUMBER) {
      return NON_MATLAB_ARRAY;
    }
    if (this == MATLAB_NUMBER) {
      return MATLAB_ARRAY;
    }
    return NON_MATLAB_ARRAY;
  }
}
