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
 * @version $Revision$`
 * 
 * <p> $Log$
 * <p> Revision 1.6  2008/09/10 21:04:25  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.
 * <p> arsedElementList will no longer create an empty element, so null returns
 * <p> will happen.
 * <p>
 * <p> Revision 1.5  2008/06/20 20:01:52  sueh
 * <p> bug# 1119 NON_MATLAB_ITERATOR_ARRAY is the only type to use with
 * <p> ParsedIteratorDescriptor.
 * <p>
 * <p> Revision 1.4  2008/04/15 21:28:05  sueh
 * <p> bug# 1105 Simplified setting the default.  Added debug and default to
 * <p> constructor.  Move setDebug() to child classes.  Moved generic descriptor
 * <p> code to ParsedDescriptor.
 * <p>
 * <p> Revision 1.3  2008/04/08 23:59:20  sueh
 * <p> bug# 1105 Changed the array used in getParsedNumberExpandedArray
 * <p> to a ParsedElementList because it always holds ParsedNumbers.
 * <p>
 * <p> Revision 1.2  2008/04/02 02:20:10  sueh
 * <p> bug# 1097 Moved functionality out of ParsedDescriptor and into child
 * <p> classes.  This is because the child class are now less similar to each
 * <p> other.
 * <p>
 * <p> Revision 1.1  2007/11/06 19:49:43  sueh
 * <p> bug# 1047 Class to parsed iterator descriptors such as 4-9 and 5-2.
 * <p> </p>
 */
final class ParsedIteratorDescriptor/* extends ParsedDescriptor */{
  public static final String rcsid = "$Id$";
/*
  static final Character DIVIDER_SYMBOL = new Character('-');

  private boolean debug = false;

  private ParsedIteratorDescriptor(EtomoNumber.Type etomoNumberType,
      boolean debug, EtomoNumber defaultValue) {
    super(ParsedElementType.NON_MATLAB_ITERATOR_ARRAY, etomoNumberType, debug,
        defaultValue);
  }

  static ParsedIteratorDescriptor getInstance(boolean debug,
      EtomoNumber defaultValue) {
    //Iterator descriptors must only contain whole numbers.
    return new ParsedIteratorDescriptor(EtomoNumber.Type.INTEGER, debug,
        defaultValue);
  }

  public void setDebug(boolean input) {
    debug = input;
    descriptor.setDebug(input);
    for (int i = 0; i < size(); i++) {
      ParsedElement element = getElement(i);
      if (element != null) {
        element.setDebug(input);
      }
    }
  }

  boolean isDebug() {
    return debug;
  }

  Character getDividerSymbol() {
    return DIVIDER_SYMBOL;
  }*/
}
