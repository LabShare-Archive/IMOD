package etomo.type;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.Token;
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

  public ParsedQuotedString() {
  }

  /**
   * This is a quoted string only if starts with a quote (ignoring whitespace).
   * @param attribute
   * @return
   */
  public static boolean isQuotedString(ReadOnlyAttribute attribute) {
    if (attribute == null) {
      return false;
    }
    String value = attribute.getValue();
    if (value == null) {
      return false;
    }
    if (value.trim().charAt(0) == DELIMITER_SYMBOL.charValue()) {
      return true;
    }
    return false;
  }

  public void parse(ReadOnlyAttribute attribute) {
    rawString = "";
    resetFailed();
    if (attribute == null) {
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
    return ParsedEmptyElement.getInstance(type).getRawString();
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
    rawString = element.getRawString();
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

  void moveElement(int fromIndex, int toIndex) {
  }

  public void clear() {
    rawString = "";
  }

  ParsedElement getElement(int index) {
    if (index == 0) {
      return this;
    }
    return ParsedEmptyElement.getInstance(type);
  }

  void setRawString(int index, float number) {
    if (index > 0) {
      return;
    }
    rawString = String.valueOf(number);
  }

  String validate() {
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
      if (token == null
          || !token.equals(Token.Type.SYMBOL, DELIMITER_SYMBOL.charValue())) {
        fail(DELIMITER_SYMBOL + " not found.");
        return token;
      }
      token = tokenizer.next();
      //everything within DELIMITER symbols is part of the rawString.
      token = parseElement(token, tokenizer);
      if (isFailed()) {
        return token;
      }
      if (token == null
          || !token.equals(Token.Type.SYMBOL, DELIMITER_SYMBOL.charValue())) {
        fail(DELIMITER_SYMBOL + " not found.");
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
  List getParsedNumberExpandedArray(List parsedNumberExpandedArray) {
    if (parsedNumberExpandedArray == null) {
      parsedNumberExpandedArray = new ArrayList();
    }
    return parsedNumberExpandedArray;
  }

  boolean isCollection() {
    return false;
  }

  boolean isDefaultedEmpty() {
    return isEmpty();
  }

  void setDefaultValue(int numberIndex, Integer[] defaultValueArray) {
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
    //Loop until DELIMITER_SYMBOL, EOL, EOF is found; that
    //should be the end of the string.
    StringBuffer buffer = new StringBuffer();
    while (!isFailed() && token != null
        && !token.equals(Token.Type.SYMBOL, DELIMITER_SYMBOL.charValue())
        && !token.is(Token.Type.EOL) && !token.is(Token.Type.EOF)) {
      //build the string
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
