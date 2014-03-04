package etomo.type;

import java.io.IOException;

import etomo.ui.swing.Token;
import etomo.util.PrimativeTokenizer;

/**
 * <p>Description: A description of an array.  A correct array descriptor
 * consists of the start, increment, and end values or the start and end values.
 * The increment is optional and is assumed to be 1.  If the increment isn't
 * found while parsing, a 1 increment is added, because j:k is identical to
 * j:1:k.  The increment may be negative which means
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
 * <H4>Matlab Variable Syntax</H4>
 * 
 * <H5>Array descriptor</H5><UL>
 * <LI>Delimiters: none
 * <LI>Divider: ":"
 * <LI>Empty descriptor:<UL>
 *   <LI>With 0 elements: illegal
 *   <LI>With 2 elements (j:k): empty if j > k
 *   <LI>With 3 elements (j:i:k): empty if<UL>
 *     <LI>i == 0 or 
 *     <LI>(i > 0 and j > k) or 
 *     <LI>(i < 0 and j < k)</UL></UL>
 * <LI>Elements: numbers
 * <LI>Number of elements: 2, or 3<UL>
 *   <LI>2 elements (j:k): [j,j+1,...,k]
 *   <LI>3 elements (j:i:k): [j,j+i,j+2i,...,k]</UL>
 * <LI>Empty element: With 2 elements, i is assumed to be 1.</UL>
 * 
 * @see ParsedList
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
 * <p> Revision 1.20  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.19  2008/09/10 21:01:26  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.
 * <p> ParsedElementList will no longer create an empty element, so null returns
 * <p> will happen.  Handle lstThreshold with more flexibility.  Moved the special
 * <p> functionality for Phi, etc to MatlabParam.
 * <p>
 * <p> Revision 1.18  2008/08/21 00:05:13  sueh
 * <p> bug# 1132 Fixed deriveLimit, which wasn't returning anything when it
 * <p> didn't have to negate the referenceLimit.  Added getIncrement to return 1
 * <p> when increment is empty or 0.  Added getStart, and getEnd to return 0
 * <p> when start or end is empty.  Using these functions where applicable.
 * <p>
 * <p> Revision 1.17  2008/06/20 19:58:54  sueh
 * <p> bug# 1119 ParsedArrayDescriptor can be either Matlab or non-Matlab now, so I need to explicitly choose an iterator array when I need one.
 * <p>
 * <p> Revision 1.16  2008/04/15 21:22:15  sueh
 * <p> bug# 1105 Simplified setting the default.  Added debug and default to
 * <p> constructor.  Move setDebug() to child classes.  Moved generic descriptor
 * <p> code to ParsedDescriptor.
 * <p>
 * <p> Revision 1.15  2008/04/08 23:57:17  sueh
 * <p> bug# 1105 Changed the array used in getParsedNumberExpandedArray
 * <p> to a ParsedElementList because it always holds ParsedNumbers.  Fixed a
 * <p> bug in getParsedNumberExpandedArray where an EtomoNumber rather
 * <p> then a ParsedNumber was being returned.
 * <p>
 * <p> Revision 1.14  2008/04/02 02:04:25  sueh
 * <p> bug# 1097 Matching Mallab's syntax.  Making non-Matlab syntax the
 * <p> default in the ParsedElements classes.  This is because matlab uses
 * <p> "NaN", which is unhealthy for Etomo and IMOD.  Made
 * <p> ParsedArrayDescriptor to know more about being an array descriptor.
 * <p> Moved functionality out of super class.
 * <p>
 * <p> Revision 1.13  2007/11/06 19:39:34  sueh
 * <p> bug# 1047 Moved most of the code to parent class ParsedDescriptor so that
 * <p> ParsedIteratorDescriptor.
 * <p>
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
  private static final int START_INDEX = 0;
  private static final int INCREMENT_INDEX = 1;
  private static final int END_INDEX = 2;
  private static final int NO_INCREMENT_SIZE = 2;
  private static final int MAX_SIZE = 3;

  private boolean debug = false;

  ParsedArrayDescriptor(final EtomoNumber.Type etomoNumberType, boolean debug,
      EtomoNumber defaultValue, final String descr) {
    super(ParsedElementType.MATLAB_ARRAY_DESCRIPTOR, etomoNumberType, debug,
        defaultValue, descr);
    setDebug(debug);
  }

  public static ParsedArrayDescriptor getInstance(final EtomoNumber.Type etomoNumberType,
      final String descr) {
    return new ParsedArrayDescriptor(etomoNumberType, false, null, descr);
  }

  public void setRawStringEnd(final String input) {
    setRawString(END_INDEX, input);
  }

  public void setRawStringStart(final String input) {
    setRawString(START_INDEX, input);
  }

  Character getDividerSymbol() {
    return DIVIDER_SYMBOL;
  }

  public void setRawStringIncrement(final String input) {
    setRawString(INCREMENT_INDEX, input);
  }

  public String getRawStringEnd() {
    ParsedElement element = descriptor.get(END_INDEX);
    if (element != null) {
      return element.getRawString();
    }
    return "";
  }

  String getRawStringStart() {
    ParsedElement element = descriptor.get(START_INDEX);
    if (element != null) {
      return element.getRawString();
    }
    return "";
  }

  public String getRawStringIncrement() {
    ParsedElement element = descriptor.get(INCREMENT_INDEX);
    if (element != null) {
      return element.getRawString();
    }
    return "";
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

  /**
   * No effect.  The array size is always three.
   */
  public void setMinArraySize(int input) {
  }

  /**
   * Set number at index if index between 0 and 2.
   */
  void setRawString(final int index, double number) {
    if (index < 0) {
      return;
    }
    if (index > END_INDEX) {
      new IllegalStateException("Unable to add element " + index + 1 + ".  No more then "
          + END_INDEX + 1 + " elements are allowed in an array descriptor.")
          .printStackTrace();
      return;
    }
    super.setRawString(index, number);
  }

  boolean isDebug() {
    return debug;
  }

  /**
   * Set string at index if index between 0 and 2.
   */
  void setRawString(final int index, String string) {
    if (index < 0) {
      return;
    }
    if (index > END_INDEX) {
      fail("Unable to add element " + index + 1 + ".  No more then " + END_INDEX + 1
          + " elements are allowed in an array descriptor.");
      return;
    }
    super.setRawString(index, string);
  }

  public ParsedElement getStart() {
    int size = descriptor.size();
    if (size > 0) {
      return descriptor.get(START_INDEX);
    }
    return null;
  }

  public ParsedElement getEnd() {
    int size = descriptor.size();
    if (size == NO_INCREMENT_SIZE) {
      return descriptor.get(INCREMENT_INDEX);
    }
    if (size == MAX_SIZE) {
      return descriptor.get(END_INDEX);
    }
    return null;
  }

  public String validate() {
    for (int i = 0; i < descriptor.size(); i++) {
      ParsedElement element = descriptor.get(i);
      String errorMessage = null;
      if (element != null) {
        errorMessage = element.validate();
      }
      if (errorMessage != null) {
        return errorMessage;
      }
    }
    int size = descriptor.size();
    if (size < NO_INCREMENT_SIZE || size > MAX_SIZE) {
      return "Array descriptors can contain either " + NO_INCREMENT_SIZE + " or "
          + MAX_SIZE + " elements.";
    }
    ParsedElement start = descriptor.get(START_INDEX);
    ParsedElement end;
    if (size > NO_INCREMENT_SIZE) {
      end = descriptor.get(END_INDEX);
    }
    else {
      end = descriptor.get(INCREMENT_INDEX);
    }
    if (start == null || start.isEmpty() || end == null || end.isEmpty()) {
      return "Array descriptors must contain at least a start and an end.";
    }
    return super.validate();
  }

  /**
   * Parse the array descriptor.  Also figures out if this actually is an array
   * descriptor.  If the descriptor only contains two elements move the second
   * one; treat it as the end element rather then the increment element.
   */
  Token parse(Token token, PrimativeTokenizer tokenizer) {
    clear();
    if (token == null) {
      return null;
    }
    try {
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      boolean dividerFound = true;
      // loop until the end of the array descriptor. Whitespace is not allowed in
      // an array descriptor.
      while (dividerFound && !isFailed() && token != null && !token.is(Token.Type.EOL)
          && !token.is(Token.Type.EOF) && !token.is(Token.Type.WHITESPACE)
          && !token.equals(Token.Type.SYMBOL, ParsedList.CLOSE_SYMBOL.charValue())
          && !token.equals(Token.Type.SYMBOL, ParsedArray.CLOSE_SYMBOL.charValue())) {
        // parse an element
        token = parseElement(token, tokenizer);
        // Find the divider.
        dividerFound = false;
        if (token != null && token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
          // This confirms that this is a descriptor.
          setDividerParsed();
          dividerFound = true;
          token = tokenizer.next();
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    // If there are 2 elements, then assume that the increment is one and put the
    // second element in the end slot.
    if (descriptor.size() == END_INDEX) {
      ParsedElement increment = getElement(INCREMENT_INDEX);
      if (increment != null) {
        setRawString(END_INDEX, increment.getRawString());
        increment.setRawString("1");
      }
    }
    return token;
  }

  /**
   * Append an array of non-empty ParsedNumbers described by this.descriptor.
   * Construct parsedNumberExpandedArray if it is null.
   * @param parsedNumberExpandedArray the array to be added to and returned
   * @return parsedNumberExpandedArray
   */
  ParsedElementList getParsedNumberExpandedArray(
      ParsedElementList parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ParsedElementList(getType(), getEtomoNumberType(),
          debug, getDefault(), descr);
    }
    ParsedNumber start = (ParsedNumber) descriptor.get(START_INDEX);
    ParsedNumber end = (ParsedNumber) descriptor.get(END_INDEX);
    if (start == null || start.isEmpty() || end == null || end.isEmpty()) {
      // Invalid descriptor.
      return parsedNumberExpandedArray;
    }
    EtomoNumber increment = new EtomoNumber(getEtomoNumberType());
    ParsedElement element = descriptor.get(INCREMENT_INDEX);
    if (element != null) {
      increment.set(element.getRawNumber());
    }
    // A missing increment is the same as an increment equals to 1.
    if (increment.isNull()) {
      increment.set(1);
    }
    if (increment.isNegative() && start.lt(end)) {
      // Empty descriptor
      return parsedNumberExpandedArray;
    }
    if (!increment.isNegative() && end.lt(start)) {
      // Empty descriptor
      return parsedNumberExpandedArray;
    }
    // Add the first element to the array.
    parsedNumberExpandedArray.add(start);
    if (start.equals(end)) {
      // Descriptor with one element.
      return parsedNumberExpandedArray;
    }
    // Add elements to the expanded array.
    EtomoNumber current = new EtomoNumber(getEtomoNumberType());
    current.set(start.getRawNumber());
    current.add(increment);
    EtomoNumber last = new EtomoNumber(getEtomoNumberType());
    last.set(end.getRawNumber());
    boolean increasing = !increment.isNegative();
    while ((increasing && current.lt(last)) || (!increasing && current.gt(last))) {
      ParsedNumber parsedCurrent = ParsedNumber.getInstance(getType(),
          getEtomoNumberType(), debug, getDefault(), descr);
      parsedCurrent.setRawString(current.getNumber());
      parsedNumberExpandedArray.add(parsedCurrent);
      current = new EtomoNumber(current);
      current.add(increment);
    }
    // Add the last element to the array.
    parsedNumberExpandedArray.add(end);
    return parsedNumberExpandedArray;
  }

  int size() {
    return descriptor.size();
  }

  /**
   * Return a two or three element array descriptor.  Returns two elements if
   * increment is not set.
   * @param parsable - true when creating a parsable string
   */
  String getString(final boolean parsable) {
    if (descriptor.isEmpty()) {
      return "";
    }
    ParsedElement start = descriptor.get(START_INDEX);
    ParsedElement increment = descriptor.get(INCREMENT_INDEX);
    ParsedElement end = descriptor.get(END_INDEX);
    String startString = "";
    String incrementString = "";
    String endString = "";
    if (parsable) {
      if (start != null) {
        startString = start.getParsableString();
      }
      if (increment != null) {
        incrementString = increment.getParsableString();
      }
      if (end != null) {
        endString = end.getParsableString();
      }
    }
    else {
      if (start != null) {
        startString = start.getRawString();
      }
      if (increment != null) {
        incrementString = increment.getRawString();
      }
      if (end != null) {
        endString = end.getRawString();
      }
    }
    StringBuffer buffer = new StringBuffer();
    buffer.append(startString + DIVIDER_SYMBOL.toString());
    // Never use an empty or zero increment element.
    if (increment != null && !increment.isEmpty()) {
      buffer.append(incrementString + DIVIDER_SYMBOL.toString());
    }
    buffer.append(endString);
    return buffer.toString();
  }
}
