package etomo.type;

import java.io.IOException;

import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.Token;
import etomo.util.PrimativeTokenizer;

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
 * <p> Revision 1.2  2007/03/31 02:59:36  sueh
 * <p> bug# 964 Added isCollection().
 * <p>
 * <p> Revision 1.1  2007/03/30 23:47:04  sueh
 * <p> bug# 964 Parses a Matlab quoted string.
 * <p> </p>
 */
public final class ParsedQuotedString extends ParsedElement {
  public static final String rcsid = "$Id$";

  private static final Character OPEN_SYMBOL = new Character('\'');
  private static final Character CLOSE_SYMBOL = OPEN_SYMBOL;

  private String rawString = "";
  private boolean valid = true;

  public ParsedQuotedString() {
  }

  public static ParsedQuotedString getInstance(ReadOnlyAttribute attribute) {
    ParsedQuotedString instance = new ParsedQuotedString();
    instance.parse(attribute);
    return instance;
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
    if (value.trim().charAt(0) == OPEN_SYMBOL.charValue()) {
      return true;
    }
    return false;
  }

  public void parse(ReadOnlyAttribute attribute) {
    rawString = "";
    valid = true;
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
      valid = false;
    }
  }

  public String getRawString() {
    return rawString;
  }

  public ConstEtomoNumber getRawNumber() {
    EtomoNumber number = new EtomoNumber();
    number.set(rawString);
    return number;
  }

  public String getParsableString() {
    if (rawString.equals("")) {
      return "";
    }
    StringBuffer buffer = new StringBuffer(OPEN_SYMBOL.toString());
    buffer.append(getRawString());
    buffer.append(CLOSE_SYMBOL.toString());
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

  Token parse(Token token, PrimativeTokenizer tokenizer) {
    if (token == null) {
      return token;
    }
    try {
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      if (token == null
          || !token.equals(Token.Type.SYMBOL, OPEN_SYMBOL.charValue())) {
        valid = false;
        return token;
      }
      token = tokenizer.next();
      //everything within OPEN and CLose symbols ("'" and "'") is part of the
      //rawString.
      token = parseElement(token, tokenizer);
      if (!valid) {
        return token;
      }
      if (token == null
          || !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
        valid = false;
        return token;
      }
      token = tokenizer.next();
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
    return token;
  }

  boolean isCollection() {
    return false;
  }

  ParsedElement getElement(int index) {
    if (index == 0) {
      return this;
    }
    return null;
  }

  int size() {
    return 1;
  }

  void fail() {
    valid = false;
  }

  boolean isEmpty() {
    return rawString == null || rawString.equals("");
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    rawString = "";
    valid = true;
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //Loop until CLOSE_SYMBOL, EOL, EOF is found; that
    //should be the end of the string.
    StringBuffer buffer = new StringBuffer();
    while (valid && token != null
        && !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())
        && !token.is(Token.Type.EOL) && !token.is(Token.Type.EOF)) {
      //build the number
      buffer.append(token.getValue());
      try {
        token = tokenizer.next();
      }
      catch (IOException e) {
        e.printStackTrace();
        fail();
      }
    }
    rawString = buffer.toString();
    return token;
  }
}
