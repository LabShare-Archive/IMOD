package etomo.type;

import java.io.IOException;

import etomo.BaseManager;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.swing.Token;
import etomo.ui.swing.UIHarness;
import etomo.util.PrimativeTokenizer;

/**
 * <p>Description: Parsable number used with other Parsed... objects.  Can
 * read and write Matlab syntax.</p>
 * 
 * @see ParsedList
 * 
 * <H4>Matlab Variable Syntax</H4>
 * 
 * <H5>Number</H5><UL>
 * <LI>Delimiters: [] or '' (optional - cannot be used inside a regular array)
 * <LI>Empty number: [], '', or NaN</UL>
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
 * <p> Revision 1.25  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.24  2010/05/20 23:51:30  sueh
 * <p> bug# 1368 Added setDefault(boolean).
 * <p>
 * <p> Revision 1.23  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.22  2009/09/20 21:31:09  sueh
 * <p> bug# 1268 Added getEtomoNumber.
 * <p>
 * <p> Revision 1.21  2009/03/17 00:46:15  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.20  2009/01/13 19:36:47  sueh
 * <p> bug# 1170 Added setFloor.
 * <p>
 * <p> Revision 1.19  2008/09/10 21:09:27  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.
 * <p> arsedElementList will no longer create an empty element, so null returns
 * <p> will happen.  Do not create an empty ParsedNumber just to return the
 * <p> correct empty value; just return null.
 * <p>
 * <p> Revision 1.18  2008/08/22 17:51:22  sueh
 * <p> bug# 1136 Made setRawString(Number) public.
 * <p>
 * <p> Revision 1.17  2008/06/20 20:02:26  sueh
 * <p> bug# 1119 For clarity added _NUMBER to MATLAB and NON_MATLAB.
 * <p>
 * <p> Revision 1.16  2008/04/15 21:28:58  sueh
 * <p> bug# 1105 Simplified setting the default.  Added debug and default to
 * <p> constructor.  Move setDebug() to child classes.
 * <p>
 * <p> Revision 1.15  2008/04/09 00:01:21  sueh
 * <p> bug# 1105 Changed the array used in getParsedNumberExpandedArray
 * <p> to a ParsedElementList because it always holds ParsedNumbers.  Fixed
 * <p> parsing; it was eating up the delimiter whitespace before it could be
 * <p> recognized.
 * <p>
 * <p> Revision 1.14  2008/04/02 02:22:31  sueh
 * <p> bug# 1097 Matching Matlab's syntax.  This simplifies many of the
 * <p> ParsedElement classes because there where too many special cases
 * <p> before.  Now it just follows Matlab's syntax instead of trying to imitate
 * <p> exactly how a field happened to be entered by hand in the .prm file.
 * <p> ParsedNumber will use "NaN" for its parsable string when it is using Matlab
 * <p> syntax.
 * <p>
 * <p> Revision 1.13  2007/11/06 19:50:38  sueh
 * <p> bug# 1047 Made class compatible with ParsedIteratorDescriptor.
 * <p>
 * <p> Revision 1.12  2007/07/31 20:41:12  sueh
 * <p> bug# 1028 added ge(int).
 * <p>
 * <p> Revision 1.11  2007/07/31 16:30:28  sueh
 * <p> bug# 1033 Added ge(ParsedNumber) and le(ParsedNumber).
 * <p>
 * <p> Revision 1.10  2007/07/24 04:05:00  sueh
 * <p> bug# 1030 added equals(ParsedNumber).
 * <p>
 * <p> Revision 1.9  2007/05/11 16:04:21  sueh
 * <p> bug# 964 Added getArray(List), which adds itself to the list, if it is not
 * <p> empty.
 * <p>
 * <p> Revision 1.8  2007/04/26 02:47:50  sueh
 * <p> bug# 964 Fixed problems with defaultValue.  Added ParsedArray.compact
 * <p> when empty array elements should not be displayed (lstThresholds).
 * <p>
 * <p> Revision 1.7  2007/04/19 21:56:22  sueh
 * <p> bug# 964 Added getRawString(int), moveElement(int,int), setRawString(float),
 * <p> setRawString(int,float), setRawString(int,String), and setRawString(String,String).
 * <p>
 * <p> Revision 1.6  2007/04/13 21:51:43  sueh
 * <p> bug# 964 Not returning ConstEtomoNumber from ParsedElement, because it
 * <p> must be returned with a getDefaulted... function to be accurate.
 * <p> GetReferenceVolume is returning ParsedElement instead.
 * <p>
 * <p> Revision 1.5  2007/04/13 20:27:50  sueh
 * <p> bug# 964 Added getRawBoolean.  Changed setRawNumber to setRawString to
 * <p> standardize function names.
 * <p>
 * <p> Revision 1.4  2007/04/11 22:17:36  sueh
 * <p> bug# 964 Made functions required by MatlabParamFile public.
 * <p>
 * <p> Revision 1.3  2007/04/09 21:10:01  sueh
 * <p> bug# 964 Added parsing.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:56:48  sueh
 * <p> bug# 964 Added isCollection().
 * <p>
 * <p> Revision 1.1  2007/03/30 23:46:45  sueh
 * <p> bug# 964 Parses a Matlab number.
 * <p> </p>
 */
public final class ParsedNumber extends ParsedElement {
  public static final String rcsid = "$Id$";

  private final EtomoNumber rawNumber;
  private final EtomoNumber.Type etomoNumberType;

  private final StringBuffer NON_ELEMENT_SYMBOLS;
  private final ParsedElementType type;

  private EtomoNumber defaultValue = null;
  private boolean debug = false;

  private ParsedNumber(ParsedElementType type, EtomoNumber.Type etomoNumberType,
      boolean debug, EtomoNumber defaultValue, final String descr) {
    super(descr);
    this.etomoNumberType = etomoNumberType;
    this.type = type;
    rawNumber = new EtomoNumber(etomoNumberType);
    rawNumber.setDefault(defaultValue);
    NON_ELEMENT_SYMBOLS = new StringBuffer(ParsedList.OPEN_SYMBOL.toString()
        + ParsedList.CLOSE_SYMBOL.toString() + ParsedArray.OPEN_SYMBOL.toString()
        + ParsedArray.CLOSE_SYMBOL.toString() + ParsedQuotedString.DELIMITER_SYMBOL
        + ParsedList.DIVIDER_SYMBOL.toString() + ParsedArray.DIVIDER_SYMBOL.toString()
        + ParsedDescriptor.getDividerSymbol(type).toString());
    setDebug(debug);
  }

  public static ParsedNumber getMatlabInstance(final String descr) {
    return new ParsedNumber(ParsedElementType.MATLAB_NUMBER, null, false, null, descr);
  }

  public static ParsedNumber getMatlabInstance(EtomoNumber.Type etomoNumberType,
      final String descr) {
    return new ParsedNumber(ParsedElementType.MATLAB_NUMBER, etomoNumberType, false,
        null, descr);
  }

  static ParsedNumber getInstance(ParsedElementType type,
      EtomoNumber.Type etomoNumberType, boolean debug, EtomoNumber defaultValue,
      final String descr) {
    return new ParsedNumber(type, etomoNumberType, debug, defaultValue, descr);
  }

  public void parse(ReadOnlyAttribute attribute) {
    rawNumber.reset();
    resetFailed();
    if (attribute == null) {
      setMissingAttribute();
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(attribute.getValue());
    try {
      parse(tokenizer.next(), tokenizer);
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  public String toString() {
    return "[rawNumber:" + rawNumber + "]";
  }

  /**
   * No effect because this is not a collection.
   */
  public void setMinArraySize(int input) {
  }

  public boolean getRawBoolean() {
    return rawNumber.getDefaultedBoolean();
  }

  /**
   * When an index is passed, treat ParsedNumber as an array of 1.  Return an
   * empty ParsedNumber for any other index.
   */
  public ParsedElement getElement(int index) {
    if (index == 0) {
      return this;
    }
    return null;
  }

  /**
   * When an index is passed, treat ParsedNumber as an array of 1
   */
  public String getRawString(int index) {
    if (index == 0) {
      return getRawString();
    }
    return "";
  }

  public Number getRawNumber() {
    return rawNumber.getDefaultedNumber();
  }

  public ConstEtomoNumber getEtomoNumber() {
    return rawNumber;
  }

  public Number getNegatedRawNumber() {
    return rawNumber.getNegatedDefaultedNumber();
  }

  public void setDebug(final boolean input) {
    debug = input;
    rawNumber.setDebug(input);
  }

  public String getRawString() {
    return rawNumber.toDefaultedString();
  }

  public void clear() {
    rawNumber.reset();
  }

  /**
   * return the raw number.  If the type is double, return it
   * as an int or long if there is no decimal value
   */
  public String getParsableString() {
    if (rawNumber.isDefaultedNull()) {
      if (type.isMatlab()) {
        // Empty strings cannot be parsed by MatLab. If this instance is a
        // Matlab syntax instance, return NaN (unless it is an array descriptor
        // because they can't contain NaN).
        if (type == ParsedElementType.MATLAB_ARRAY_DESCRIPTOR) {
          return "";
        }
        return "NaN";
      }
      else {
        return "";
      }
    }
    Number number = rawNumber.getDefaultedNumber();
    // Remove unnecessary decimal points.
    if (etomoNumberType == EtomoNumber.Type.DOUBLE) {
      double doubleNumber = number.doubleValue();
      if (Math.round(doubleNumber) == doubleNumber) {
        return new Integer(number.intValue()).toString();
      }
    }
    return number.toString();
  }

  public void setRawString(boolean bool) {
    rawNumber.set(bool);
  }

  public void setElement(ParsedElement element) {
    if (element != null) {
      String rawString = element.getRawString();
      if (!rawString.equals("NaN")) {
        rawNumber.set(rawString);
        return;
      }
    }
    rawNumber.reset();
  }

  public boolean isEmpty() {
    return rawNumber.isNull();
  }

  public void moveElement(int fromIndex, int toIndex) {
  }

  public void setRawString(String number) {
    if (!number.equals("NaN")) {
      rawNumber.set(number);
    }
    else {
      rawNumber.reset();
    }
    setFailed(!rawNumber.isValid(), rawNumber.getInvalidReason());
  }

  void setRawString(BaseManager manager, String number, String fieldDescription) {
    if (!number.equals("NaN")) {
      rawNumber.set(number);
    }
    else {
      rawNumber.reset();
    }
    if (fieldDescription != null) {
      String errorMessage = rawNumber.validate(fieldDescription);
      if (errorMessage != null) {
        UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, "Entry Error");
      }
    }
  }

  public String validate() {
    if (!rawNumber.isValid()) {
      String invalidReason = rawNumber.getInvalidReason();
      if (invalidReason == null) {
        return (descr != null ? descr : "") + ": Unable to parse.";
      }
      return (descr != null ? descr : "") + ": " + invalidReason;
    }
    return getFailedMessage();
  }

  void setRawString(double number) {
    rawNumber.set(number);
  }

  boolean equals(ParsedNumber input) {
    return rawNumber.equals(input.rawNumber);
  }

  void setRawString(int index, double number) {
    if (index != 0) {
      return;
    }
    rawNumber.set(number);
  }

  void setRawString(int index, String string) {
    if (index != 0) {
      return;
    }
    setRawString(string);
  }

  boolean isPositive() {
    return rawNumber.isPositive();
  }

  boolean isNegative() {
    return rawNumber.isNegative();
  }

  boolean le(ParsedNumber element) {
    return rawNumber.lt(element.rawNumber) || rawNumber.equals(element.rawNumber);
  }

  boolean lt(ParsedNumber element) {
    return rawNumber.lt(element.rawNumber);
  }

  boolean ge(int number) {
    return rawNumber.gt(number) || rawNumber.equals(number);
  }

  public boolean equals(int number) {
    return rawNumber.equals(number);
  }

  boolean ge(ParsedNumber element) {
    return rawNumber.gt(element.rawNumber) || rawNumber.equals(element.rawNumber);
  }

  boolean gt(ParsedNumber element) {
    return rawNumber.gt(element.rawNumber);
  }

  public void setRawString(Number number) {
    rawNumber.set(number);
  }

  void plus(ConstEtomoNumber number) {
    rawNumber.add(number);
  }

  public void setDefault(int input) {
    rawNumber.setDefault(input);
  }

  public void setDefault(boolean input) {
    rawNumber.setDefault(input);
  }

  void setDefault(EtomoNumber input) {
    rawNumber.setDefault(input);
  }

  public void setFloor(int input) {
    rawNumber.setFloor(input);
  }

  void removeElement(int index) {
    if (index == 0) {
      rawNumber.reset();
    }
  }

  /**
   * If rawNumber is not null append this to parsedNumberExpandedArray.  Create
   * parsedNumberExpandedArray if parsedNumberExpandedArray == null.
   * @param parsedNumberExpandedArray
   * @return parsedNumberExpandedArray
   */
  ParsedElementList getParsedNumberExpandedArray(
      ParsedElementList parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ParsedElementList(type, etomoNumberType, debug,
          defaultValue, descr);
    }
    if (rawNumber.isNull()) {
      return parsedNumberExpandedArray;
    }
    parsedNumberExpandedArray.add(this);
    return parsedNumberExpandedArray;
  }

  /**
   * parse the number including delimiters
   * @return the token that is current when the array is parsed
   */
  Token parse(Token token, PrimativeTokenizer tokenizer) {
    rawNumber.reset();
    resetFailed();
    if (token == null) {
      return token;
    }
    try {
      Character closeSymbol = null;
      if (!type.isArray()) {
        if (token.is(Token.Type.WHITESPACE)) {
          token = tokenizer.next();
        }
        if (token == null) {
          return token;
        }
        // If the number is not in an array, it may still have delimiters
        // (either [] or ''). Find opening delimiter.
        if (token.equals(Token.Type.SYMBOL, ParsedArray.OPEN_SYMBOL.charValue())) {
          closeSymbol = ParsedArray.CLOSE_SYMBOL;
          token = tokenizer.next();
        }
        else if (token.equals(Token.Type.SYMBOL,
            ParsedQuotedString.DELIMITER_SYMBOL.charValue())) {
          closeSymbol = ParsedQuotedString.DELIMITER_SYMBOL;
          token = tokenizer.next();
        }
      }
      // Remove any whitespace before the element.
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      token = parseElement(token, tokenizer);
      if (debug) {
        System.out.println("ParsedNumber.parse:rawNumber=" + rawNumber);
      }
      if (isFailed()) {
        return token;
      }
      // Find closing delimiter
      if (closeSymbol != null) {
        if (token.is(Token.Type.WHITESPACE)) {
          token = tokenizer.next();
        }
        if (token == null) {
          fail("End of value.  Closing delimiter, " + closeSymbol + ", was not found.");
          return token;
        }
        // If the number is not in an array, it may have delimiters
        // (either [] or '').
        if (token.equals(Token.Type.SYMBOL, closeSymbol)) {
          token = tokenizer.next();
        }
        else {
          fail("Closing delimiter, " + closeSymbol + ", was not found.");
          return token;
        }
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    return token;
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    if (token == null) {
      return null;
    }
    // Loop until whitespace, EOL, EOF, or a recognized symbol is found; that
    // should be the end of the number.
    StringBuffer buffer = new StringBuffer();
    while (!isFailed()
        && token != null
        && !token.is(Token.Type.WHITESPACE)
        && !token.is(Token.Type.EOL)
        && !token.is(Token.Type.EOF)
        && (!token.is(Token.Type.SYMBOL) || NON_ELEMENT_SYMBOLS.toString().indexOf(
            token.getChar()) == -1)) {
      // build the number
      buffer.append(token.getValue());
      if (debug) {
        System.out.println("ParsedNumber.parseElement:buffer=" + buffer);
      }
      try {
        token = tokenizer.next();
      }
      catch (IOException e) {
        e.printStackTrace();
        fail(e.getMessage());
      }
    }
    String string = buffer.toString();
    if (!string.equals("NaN")) {
      rawNumber.set(string);
    }
    else {
      rawNumber.reset();
    }
    return token;
  }

  boolean isDescriptor() {
    return false;
  }

  boolean isCollection() {
    return false;
  }

  boolean isDefaultedEmpty() {
    return rawNumber.isDefaultedNull();
  }

  int size() {
    return 1;
  }
}
