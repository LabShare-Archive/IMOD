package etomo.type;

/**
 * <p>Description: A description of an array.  A correct array descriptor
 * consists of the start, increment, and end values or the start and end values
 * (increment is assumed to be 1).  The increment may be negative which means
 * that the start value should be >= to the end value.  If the increment is
 * positive, the start value should be <= to the end value.  If the increment is
 * zero, the array described is [start, end].  If the descriptor contains only
 * one value, the array described is [value].  Values after the first three
 * values are ignored.
 * 
 * The descriptor member variable should only contain ParsedNumbers.  The string
 * representation of an array descriptor uses ":" to divide the values and does
 * not use open and close symbols such as square brackets.
 * 
 * The array descriptor is only used as an element of a ParsedArray.</p>
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
 * <p> Revision 1.12  2007/07/31 20:40:02  sueh
 * <p> bug# 1028 added ge(int).
 * <p>
 * <p> Revision 1.11  2007/07/31 16:30:00  sueh
 * <p> bug# 1033 In getArray(List) include the last number in the arrray, if it is one of the
 * <p> number specified by the start and increment numbers.
 * <p>
 * <p> Revision 1.10  2007/07/24 04:04:33  sueh
 * <p> bug# 1030 Fixed getArray(List); it was handling the last number incorrectly.
 * <p>
 * <p> Revision 1.9  2007/05/11 16:02:06  sueh
 * <p> bug# 964 Added getArray(List), which converts an array descriptor into
 * <p> an array.
 * <p>
 * <p> Revision 1.8  2007/05/03 21:08:59  sueh
 * <p> bug# 964 Fixed bug in hasParsedNumberSyntax().
 * <p>
 * <p> Revision 1.7  2007/04/26 02:47:06  sueh
 * <p> bug# 964 Fixed problems with defaultValue.  Added ParsedArray.compact
 * <p> when empty array elements should not be displayed (lstThresholds).
 * <p>
 * <p> Revision 1.6  2007/04/19 21:42:42  sueh
 * <p> bug# 964 Added boolean compact.  A compact instance ignores all the empty
 * <p> numbers when writing to the .prm file.
 * <p>
 * <p> Revision 1.5  2007/04/13 21:51:03  sueh
 * <p> bug# 964 Not returning ConstEtomoNumber from ParsedElement, because it
 * <p> must be returned with a getDefaulted... function to be accurate.
 * <p> GetReferenceVolume is returning ParsedElement instead.
 * <p>
 * <p> Revision 1.4  2007/04/13 20:14:25  sueh
 * <p> bug# 964 Added setRawString(String), which parses a list of numbers separated
 * <p> by :'s.
 * <p>
 * <p> Revision 1.3  2007/04/09 21:00:29  sueh
 * <p> bug# 964 Added parsing.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:54:24  sueh
 * <p> bug# 964 Added isCollection().
 * <p>
 * <p> Revision 1.1  2007/03/30 23:41:13  sueh
 * <p> bug# 964 Class to parse Matlab array descriptors.
 * <p> </p>
 */

public final class ParsedArrayDescriptor extends ParsedDescriptor {
  public static final String rcsid = "$Id$";

  static final Character DIVIDER_SYMBOL = new Character(':');

  /**
   * When compact is true, only place non-empty elements in the parsable string.
   */
  private final boolean compact;

  ParsedArrayDescriptor(EtomoNumber.Type etomoNumberType, boolean compact) {
    super(etomoNumberType);
    this.compact = compact;
  }

  boolean isCompact() {
    return compact;
  }

  Character getDividerSymbol() {
    return DIVIDER_SYMBOL;
  }
  
  boolean isIteratorDescriptor() {
    return false;
  }
  
  int getIncrement(ParsedNumber first,ParsedNumber last) {
    return 1;
  }
}
