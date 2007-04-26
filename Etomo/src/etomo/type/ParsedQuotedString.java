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

  private static final Character OPEN_SYMBOL = new Character('\'');
  private static final Character CLOSE_SYMBOL = OPEN_SYMBOL;

  private String rawString = "";
  private boolean valid = true;
  private boolean debug = false;

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
  
  public String getRawString(int index) {
    if (index == 0) {
      return rawString;
    }
    return ParsedElementList.EmptyParsedElement.INSTANCE.getRawString();
  }
  
  public void setRawString (int index ,String string){
    if (index!=0) {
      return;
    }
    rawString=string;
  }

  public Number getRawNumber() {
    EtomoNumber etomoNumber = new EtomoNumber();
    etomoNumber.set(rawString);
    return etomoNumber.getNumber();
  }

  public String getParsableString() {
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
  
  public void moveElement(int fromIndex, int toIndex) {
  }
  
  public void clear() {
    rawString = "";
  }
  
  public ParsedElement getElement(int index) {
    if (index == 0) {
      return this;
    }
    return ParsedElementList.EmptyParsedElement.INSTANCE;
  }
  
  public void setRawString(int index,float number) {
    if (index>0) {
      return;
    }
    rawString=String.valueOf(number);
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
  
  boolean hasParsedNumberSyntax() {
    return false;
  }

  boolean isCollection() {
    return false;
  }
  
  boolean isDefaultedEmpty() {
    return isEmpty();
  }
  
  void setDefaultValue(int numberIndex,Integer[] defaultValueArray) {}
  
  void setDebug(boolean debug) {
    this.debug = debug;
  }
  
  void removeElement(int index) {
    if (index==0) {
      rawString="";
    }
  }

  int size() {
    return 1;
  }

  void fail() {
    valid = false;
  }

  public boolean isEmpty() {
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
