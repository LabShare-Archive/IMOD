package etomo.type;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.Token;
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

  private final boolean inArray;

  private ParsedNumber(ParsedElementType type,
      EtomoNumber.Type etomoNumberType, boolean inArray) {
    this.etomoNumberType = etomoNumberType;
    this.type = type;
    this.inArray = inArray;
    rawNumber = new EtomoNumber(etomoNumberType);
    NON_ELEMENT_SYMBOLS = new StringBuffer(ParsedList.OPEN_SYMBOL.toString()
        + ParsedList.CLOSE_SYMBOL.toString()
        + ParsedArray.OPEN_SYMBOL.toString()
        + ParsedArray.CLOSE_SYMBOL.toString()
        + ParsedQuotedString.DELIMITER_SYMBOL
        + ParsedList.DIVIDER_SYMBOL.toString()
        + ParsedArray.DIVIDER_SYMBOL.toString()
        + ParsedDescriptor.getDividerSymbol(type).toString());
  }

  public static ParsedNumber getInstance(ParsedElementType type,
      EtomoNumber.Type etomoNumberType) {
    return new ParsedNumber(type, etomoNumberType, false);
  }

  public static ParsedNumber getArrayInstance(ParsedElementType type,
      EtomoNumber.Type etomoNumberType) {
    return new ParsedNumber(type, etomoNumberType, true);
  }

  public static ParsedNumber getMatlabInstance() {
    return new ParsedNumber(ParsedElementType.MATLAB, null, false);
  }

  public static ParsedNumber getMatlabInstance(EtomoNumber.Type etomoNumberType) {
    return new ParsedNumber(ParsedElementType.MATLAB, etomoNumberType, false);
  }

  public void parse(ReadOnlyAttribute attribute) {
    rawNumber.reset();
    resetFailed();
    if (attribute == null) {
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

  public boolean getRawBoolean() {
    return rawNumber.getDefaultedBoolean();
  }

  /**
   * When an index is passed, treat ParsedNumber as an array of 1
   */
  public ParsedElement getElement(int index) {
    if (index == 0) {
      return this;
    }
    return ParsedEmptyElement.getInstance(type);
  }

  /**
   * When an index is passed, treat ParsedNumber as an array of 1
   */
  public String getRawString(int index) {
    if (index == 0) {
      return getRawString();
    }
    return ParsedEmptyElement.getInstance(type).getRawString();
  }

  public Number getRawNumber() {
    return rawNumber.getDefaultedNumber();
  }

  public Number getNegatedRawNumber() {
    return rawNumber.getNegatedDefaultedNumber();
  }

  public String getRawString() {
    return rawNumber.toDefaultedString();
  }

  public void clear() {
    rawNumber.reset();
  }

  /**
   * return the raw number.  If the type is floating point or double, return it
   * as an int or long if there is no decimal value
   */
  public String getParsableString() {
    if (rawNumber.isDefaultedNull()) {
      if (type == ParsedElementType.MATLAB) {
        //Empty strings cannot be parsed by MatLab.  If this instance is a
        //Matlab syntax instance, return NaN.
        return "NaN";
      }
      else {
        return "";
      }
    }
    Number number = rawNumber.getDefaultedNumber();
    //Remove unnecessary decimal points.
    if (etomoNumberType == EtomoNumber.Type.FLOAT) {
      float floatNumber = number.floatValue();
      if (Math.round(floatNumber) == floatNumber) {
        return new Integer(number.intValue()).toString();
      }
    }
    else if (etomoNumberType == EtomoNumber.Type.DOUBLE) {
      double doubleNumber = number.doubleValue();
      if (Math.round(doubleNumber) == doubleNumber) {
        return new Long(number.longValue()).toString();
      }
    }
    return number.toString();
  }

  public void setRawString(boolean bool) {
    rawNumber.set(bool);
  }

  public void setElement(ParsedElement element) {
    rawNumber.set(element.getRawString());
  }

  public boolean isEmpty() {
    return rawNumber.isNull();
  }

  public void moveElement(int fromIndex, int toIndex) {
  }

  public void setRawString(String number) {
    rawNumber.set(number);
    setFailed(!rawNumber.isValid(), rawNumber.getInvalidReason());
  }

  void setRawString(String number, String fieldDescription) {
    rawNumber.set(number);
    if (fieldDescription != null) {
      rawNumber.isValid("Entry Error", fieldDescription);
    }
  }

  String validate() {
    if (!rawNumber.isValid()) {
      return rawNumber.getInvalidReason();
    }
    return getFailedMessage();
  }

  void setRawString(float number) {
    rawNumber.set(number);
  }

  boolean equals(ParsedNumber input) {
    return rawNumber.equals(input.rawNumber);
  }

  void setRawString(int index, float number) {
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

  boolean equals(int number) {
    return rawNumber.equals(number);
  }

  boolean isPositive() {
    return rawNumber.isPositive();
  }

  boolean isNegative() {
    return rawNumber.isNegative();
  }

  boolean le(ParsedNumber element) {
    return rawNumber.lt(element.rawNumber)
        || rawNumber.equals(element.rawNumber);
  }

  boolean lt(ParsedNumber element) {
    return rawNumber.lt(element.rawNumber);
  }

  boolean ge(int number) {
    return rawNumber.gt(number) || rawNumber.equals(number);
  }

  boolean ge(ParsedNumber element) {
    return rawNumber.gt(element.rawNumber)
        || rawNumber.equals(element.rawNumber);
  }

  boolean gt(ParsedNumber element) {
    return rawNumber.gt(element.rawNumber);
  }

  void setRawString(Number number) {
    rawNumber.set(number);
  }

  void plus(ConstEtomoNumber number) {
    rawNumber.add(number);
  }

  /**
   * Set defaultValue using defaultValueArray[numberIndex].
   */
  void setDefaultValue(int numberIndex, Integer[] defaultValueArray) {
    if (defaultValueArray == null || numberIndex < 0
        || defaultValueArray.length <= numberIndex) {
      rawNumber.resetDefault();
    }
    else {
      rawNumber.setDefault(defaultValueArray[numberIndex]);
    }
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
  List getParsedNumberExpandedArray(List parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ArrayList();
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
      if (!inArray) {
        if (token.is(Token.Type.WHITESPACE)) {
          token = tokenizer.next();
        }
        if (token == null) {
          return token;
        }
        //If the number is not in an array, it may have delimiters
        //(either [] or '').  Find opening delimiter
        if (token
            .equals(Token.Type.SYMBOL, ParsedArray.OPEN_SYMBOL.charValue())) {
          closeSymbol = ParsedArray.CLOSE_SYMBOL;
          token = tokenizer.next();
        }
        else if (token.equals(Token.Type.SYMBOL,
            ParsedQuotedString.DELIMITER_SYMBOL.charValue())) {
          closeSymbol = ParsedQuotedString.DELIMITER_SYMBOL;
          token = tokenizer.next();
        }
      }
      //Remove any whitespace before the element.
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      token = parseElement(token, tokenizer);
      if (isFailed()) {
        return token;
      }
      //Find closing delimiter
      if (closeSymbol != null) {
        if (token.is(Token.Type.WHITESPACE)) {
          token = tokenizer.next();
        }
        if (token == null) {
          fail("End of value.  Closing delimiter, " + closeSymbol
              + ", was not found.");
          return token;
        }
        //If the number is not in an array, it may have delimiters
        //(either [] or '').
        if (token.equals(Token.Type.SYMBOL, closeSymbol)) {
          token = tokenizer.next();
        }
        else {
          fail("Closing delimiter, " + closeSymbol + ", was not found.");
          return token;
        }
      }
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
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
    //Loop until whitespace, EOL, EOF, or a recognized symbol is found; that
    //should be the end of the number.
    StringBuffer buffer = new StringBuffer();
    while (!isFailed()
        && token != null
        && !token.is(Token.Type.WHITESPACE)
        && !token.is(Token.Type.EOL)
        && !token.is(Token.Type.EOF)
        && (!token.is(Token.Type.SYMBOL) || NON_ELEMENT_SYMBOLS.toString()
            .indexOf(token.getChar()) == -1)) {
      //build the number
      buffer.append(token.getValue());
      try {
        token = tokenizer.next();
      }
      catch (IOException e) {
        e.printStackTrace();
        fail(e.getMessage());
      }
    }
    rawNumber.setDebug(isDebug());
    rawNumber.set(buffer.toString());
    return token;
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
