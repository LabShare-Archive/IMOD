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

  private EtomoNumber.Type etomoNumberType = null;
  private Integer defaultValue = null;
  private boolean valid = true;

  private static final StringBuffer SYMBOL_STRING = new StringBuffer(
      ParsedList.OPEN_SYMBOL.toString() + ParsedList.CLOSE_SYMBOL.toString()
          + ParsedList.DIVIDER_SYMBOL.toString()
          + ParsedArray.OPEN_SYMBOL.toString()
          + ParsedArray.CLOSE_SYMBOL.toString()
          + ParsedArray.DIVIDER_SYMBOL.toString()
          + ParsedArrayDescriptor.DIVIDER_SYMBOL.toString());

  public ParsedNumber() {
    this(null, null);
  }

  public ParsedNumber(EtomoNumber.Type etomoNumberType) {
    this(etomoNumberType, null);
  }

  ParsedNumber(EtomoNumber.Type etomoNumberType, Integer defaultValue) {
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = defaultValue;
    rawNumber = new EtomoNumber(etomoNumberType);
    if (defaultValue != null) {
      rawNumber.setDefault(defaultValue.intValue());
    }
  }

  public void parse(ReadOnlyAttribute attribute) {
    rawNumber.reset();
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
      fail();
    }
  }

  public String toString() {
    return "[rawNumber:" + rawNumber + "]";
  }
  
  public boolean getRawBoolean() {
    if (defaultValue == null) {
      return rawNumber.is();
    }
    return rawNumber.getDefaultedBoolean();
  }
  
  public String getRawString() {
    if (defaultValue == null) {
      return rawNumber.toString();
    }
    return rawNumber.toDefaultedString();
  }
  
  public void clear() {
    rawNumber.reset();
  }
  
  public String getParsableString() {
    return getRawString();
  }

  public void setRawString(String number) {
    rawNumber.set(number);
  }
  
  public void setRawString(boolean bool) {
    rawNumber.set(bool);
  }

  public void setElement(ParsedElement element) {
    rawNumber.set(element.getRawString());
  }

  Token parse(Token token, PrimativeTokenizer tokenizer) {
    rawNumber.reset();
    valid = true;
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //Loop until whitespace, EOL, EOF, or a recognized symbol is found; that
    //should be the end of the number.
    StringBuffer buffer = new StringBuffer();
    while (valid
        && token != null
        && !token.is(Token.Type.WHITESPACE)
        && !token.is(Token.Type.EOL)
        && !token.is(Token.Type.EOF)
        && !(token.is(Token.Type.SYMBOL) && SYMBOL_STRING.toString().indexOf(
            token.getChar()) != -1)) {
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
    rawNumber.set(buffer.toString());
    return token;
  }

  ConstEtomoNumber getRawNumber() {
    return rawNumber;
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
    return rawNumber.isNull();
  }
}
