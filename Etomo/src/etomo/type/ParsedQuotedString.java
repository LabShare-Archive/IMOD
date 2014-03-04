package etomo.type;

import java.io.IOException;

import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.swing.Token;
import etomo.util.PrimativeTokenizer;

/**
 * <p>Description: Matlab string.  Allows whitespace.</p>
 * 
 * @see ParsedList
 * 
 * <H4>Matlab Variable Syntax</H4>
 * 
 * <H5>String</H5><UL>
 * <LI>Delimiters: ''  (required)
 * <LI>Empty string: ''</UL>
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
 * <p> Revision 1.15  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.14  2008/09/10 21:33:07  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.
 * <p> arsedElementList will no longer create an empty element, so null returns
 * <p> will happen.  Do not create an empty ParsedQuotedString just to return the
 * <p> correct empty value; just return null.
 * <p>
 * <p> Revision 1.13  2008/04/15 21:29:23  sueh
 * <p> bug# 1105 Added debug and to constructor.  Move setDebug() to child
 * <p> classes.
 * <p>
 * <p> Revision 1.12  2008/04/09 00:01:33  sueh
 * <p> bug# 1105 Changed the array used in getParsedNumberExpandedArray
 * <p> to a ParsedElementList because it always holds ParsedNumbers.
 * <p>
 * <p> Revision 1.11  2008/04/02 02:24:19  sueh
 * <p> bug# 1097 Added ParsedElementType.  Got rid of OPEN and
 * <p> CLOSE_SYMBOL (just using DELIMITER_SYMBOL).
 * <p>
 * <p> Revision 1.10  2007/11/06 19:50:50  sueh
 * <p> bug# 1047 added validate.
 * <p>
 * <p> Revision 1.9  2007/07/31 20:41:23  sueh
 * <p> bug# 1028 added ge(int).
 * <p>
 * <p> Revision 1.8  2007/05/11 16:04:36  sueh
 * <p> bug# 964 Added getArray(List), which does nothing.
 * <p>
 * <p> Revision 1.7  2007/04/26 02:47:59  sueh
 * <p> bug# 964 Fixed problems with defaultValue.  Added ParsedArray.compact
 * <p> when empty array elements should not be displayed (lstThresholds).
 * <p>
 * <p> Revision 1.6  2007/04/19 21:57:16  sueh
 * <p> bug# 964 Added getRawString(int), moveElement(int,int), moveElement(int,int),
 * <p> setRawString(int,float), and setRawString(int,String).
 * <p>
 * <p> Revision 1.5  2007/04/13 21:51:53  sueh
 * <p> bug# 964 Not returning ConstEtomoNumber from ParsedElement, because it
 * <p> must be returned with a getDefaulted... function to be accurate.
 * <p> GetReferenceVolume is returning ParsedElement instead.
 * <p>
 * <p> Revision 1.4  2007/04/13 20:31:50  sueh
 * <p> bug# 964 Changed functionality:  An empty string should return ''.  This works with the requirements for alignedBaseName.
 * <p>
 * <p> Revision 1.3  2007/04/09 21:10:23  sueh
 * <p> bug# 964 Added parsing from a tokenizer.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:59:36  sueh
 * <p> bug# 964 Added isCollection().
 * <p>
 * <p> Revision 1.1  2007/03/30 23:47:04  sueh
 * <p> bug# 964 Parses a Matlab quoted string.
 * <p> </p>
 */
public final class ParsedQuotedString extends ParsedElement {
  public static final String rcsid = "$Id$";

  static final Character DELIMITER_SYMBOL = new Character('\'');
  private static final ParsedElementType type = ParsedElementType.STRING;

  private String rawString = "";
  private boolean debug = false;

  private ParsedQuotedString(boolean debug, final String descr) {
    super(descr);
    setDebug(debug);
  }

  public static ParsedQuotedString getInstance(final String descr) {
    return new ParsedQuotedString(false, descr);
  }

  static ParsedQuotedString getInstance(boolean debug, final String descr) {
    return new ParsedQuotedString(debug, descr);
  }

  public void setDebug(final boolean input) {
    debug = input;
  }

  /**
   * This is a quoted string only if starts with a quote (ignoring whitespace).
   * @param attribute
   * @return
   */
  public static boolean isQuotedString(final ReadOnlyAttribute attribute) {
    if (attribute == null) {
      return false;
    }
    return isQuotedString(attribute.getValue());
  }

  /**
   * This is a quoted string only if starts with a quote (ignoring whitespace).
   * @param attribute
   * @return
   */
  public static boolean isQuotedString(final String value) {
    if (value == null) {
      return false;
    }
    return value.trim().charAt(0) == DELIMITER_SYMBOL.charValue();
  }

  public void parse(ReadOnlyAttribute attribute) {
    rawString = "";
    resetFailed();
    if (attribute == null) {
      setMissingAttribute();
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(attribute.getValue());
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    try {
      token = tokenizer.next();
      parse(token, tokenizer);
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
  }

  public String getRawString() {
    return rawString;
  }

  public boolean isEmpty() {
    return rawString == null || rawString.equals("");
  }

  public String getRawString(int index) {
    if (index == 0) {
      return rawString;
    }
    return new ParsedQuotedString(debug, descr).getRawString();
  }

  void setRawString(int index, String string) {
    if (index != 0) {
      return;
    }
    rawString = string;
  }

  public Number getRawNumber() {
    EtomoNumber etomoNumber = new EtomoNumber();
    etomoNumber.set(rawString);
    return etomoNumber.getNumber();
  }

  public String getParsableString() {
    StringBuffer buffer = new StringBuffer(DELIMITER_SYMBOL.toString());
    buffer.append(getRawString());
    buffer.append(DELIMITER_SYMBOL.toString());
    return buffer.toString();
  }

  public void setElement(ParsedElement element) {

    if (element != null) {
      rawString = element.getRawString();
    }
    else {
      rawString = "";
    }
  }

  public void setRawString(String string) {
    rawString = string;
  }

  public String toString() {
    return "[rawString:" + rawString + "]";
  }

  boolean ge(int number) {
    return false;
  }

  public boolean equals(int number) {
    return false;
  }

  void moveElement(int fromIndex, int toIndex) {
  }

  public void clear() {
    rawString = "";
  }

  ParsedElement getElement(int index) {
    if (index == 0) {
      return this;
    }
    return null;
  }

  void setRawString(int index, double number) {
    if (index > 0) {
      return;
    }
    rawString = String.valueOf(number);
  }

  public String validate() {
    if (isFailed()) {
      return getFailedMessage();
    }
    return null;
  }

  Token parse(Token token, PrimativeTokenizer tokenizer) {
    if (token == null) {
      return token;
    }
    try {
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      if (token == null || !token.equals(Token.Type.SYMBOL, DELIMITER_SYMBOL.charValue())) {
        fail("Missing delimiter: '" + DELIMITER_SYMBOL + "'");
        return token;
      }
      token = tokenizer.next();
      // everything within DELIMITER symbols is part of the rawString.
      token = parseElement(token, tokenizer);
      if (isFailed()) {
        return token;
      }
      if (token == null || !token.equals(Token.Type.SYMBOL, DELIMITER_SYMBOL.charValue())) {
        fail("Missing delimiter: '" + DELIMITER_SYMBOL + "'");
        return token;
      }
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(e.getMessage());
    }
    return token;
  }

  /**
   * Append non-null ParsedNumbers to parsedNumberExpandedArray.  Create
   * parsedNumberExpandedArray if parsedNumberExpandedArray == null.
   * @param parsedNumberExpandedArray
   * @return parsedNumberExpandedArray
   */
  ParsedElementList getParsedNumberExpandedArray(
      ParsedElementList parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ParsedElementList(type, null, debug, null, descr);
    }
    return parsedNumberExpandedArray;
  }

  boolean isDescriptor() {
    return false;
  }

  boolean isCollection() {
    return false;
  }

  boolean isDefaultedEmpty() {
    return isEmpty();
  }

  public void setDefault(int input) {
  }

  void setDefault(EtomoNumber input) {
  }

  public void setMinArraySize(int input) {
  }

  void removeElement(int index) {
    if (index == 0) {
      rawString = "";
    }
  }

  int size() {
    return 1;
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    rawString = "";
    resetFailed();
    if (token == null) {
      return null;
    }
    // Loop until DELIMITER_SYMBOL, EOL, EOF is found; that
    // should be the end of the string.
    StringBuffer buffer = new StringBuffer();
    while (!isFailed() && token != null
        && !token.equals(Token.Type.SYMBOL, DELIMITER_SYMBOL.charValue())
        && !token.is(Token.Type.EOL) && !token.is(Token.Type.EOF)) {
      // build the string
      buffer.append(token.getValue());
      try {
        token = tokenizer.next();
      }
      catch (IOException e) {
        e.printStackTrace();
        fail(e.getMessage());
      }
    }
    rawString = buffer.toString();
    return token;
  }
}
