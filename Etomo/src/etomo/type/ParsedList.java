package etomo.type;

import java.io.IOException;

import etomo.storage.LogFile;
import etomo.storage.autodoc.ReadOnlyAttribute;
import etomo.ui.Token;
import etomo.util.PrimativeTokenizer;

/**
 * <p>Description: For Matlab.  An expandable array of elements that implement ParsedData.
 * The array does not have to be fully populated.
 * Use parse and toString functions to load and retrieve parsable data.
 * The add, set, and get functions refer to raw data.
 * ListParser has two types.  When the STRING type is set, ListParser only parses
 * quoted strings.  When the NUMERIC type is set, ListParser only parses numbers and arrays.
 * </p>
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
 * <p> Revision 1.3  2007/04/09 21:09:46  sueh
 * <p> bug# 964 Added missing tokenizer.next() call to parseList.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:56:33  sueh
 * <p> bug# 964 Removed getRawString(int) because it is not in use.
 * <p>
 * <p> Revision 1.1  2007/03/30 23:46:15  sueh
 * <p> bug# 964 Parses a Matlab list.
 * <p> </p>
 */
public final class ParsedList {
  public static final String rcsid = "$Id$";

  static final Character OPEN_SYMBOL = new Character('{');
  static final Character CLOSE_SYMBOL = new Character('}');
  static final Character DIVIDER_SYMBOL = new Character(',');

  private final Type type;
  private final ParsedElementList list = new ParsedElementList();

  private EtomoNumber.Type etomoNumberType = null;
  private Integer defaultValue = null;
  private boolean valid = true;

  private ParsedList(Type type) {
    this.type = type;
  }

  private ParsedList(Type type, EtomoNumber.Type etomoNumberType) {
    this.type = type;
    this.etomoNumberType = etomoNumberType;
  }

  private ParsedList(Type type, EtomoNumber.Type etomoNumberType,
      int defaultValue) {
    this.type = type;
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = new Integer(defaultValue);
  }

  public static ParsedList getNumericInstance(EtomoNumber.Type etomoNumberType) {
    return new ParsedList(Type.NUMERIC, etomoNumberType);
  }

  public static ParsedList getNumericInstance(EtomoNumber.Type etomoNumberType,
      int defaultValue) {
    return new ParsedList(Type.NUMERIC, etomoNumberType, defaultValue);
  }

  public static ParsedList getNumericInstance(ReadOnlyAttribute attribute) {
    ParsedList instance = new ParsedList(Type.NUMERIC);
    instance.parse(attribute);
    return instance;
  }

  public static ParsedList getNumericInstance(ReadOnlyAttribute attribute,
      EtomoNumber.Type etomoNumberType) {
    ParsedList instance = new ParsedList(Type.NUMERIC, etomoNumberType);
    instance.parse(attribute);
    return instance;
  }

  public static ParsedList getNumericInstance(ReadOnlyAttribute attribute,
      EtomoNumber.Type etomoNumberType, int defaultValue) {
    ParsedList instance = new ParsedList(Type.NUMERIC, etomoNumberType,
        defaultValue);
    instance.parse(attribute);
    return instance;
  }

  public static ParsedList getStringInstance() {
    return new ParsedList(Type.STRING);
  }

  public static ParsedList getStringInstance(ReadOnlyAttribute attribute) {
    ParsedList instance = new ParsedList(Type.STRING);
    instance.parse(attribute);
    return instance;
  }

  /**
   * This is a list only if starts with "{" (ignoring whitespace).
   * @param attribute
   * @return
   */
  public static boolean isList(ReadOnlyAttribute attribute) {
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

  public int size() {
    return list.size();
  }
  
  public String toString() {
    return "[list:"+list+"]";
  }

  public ParsedElement getElement(int index) {
    return list.get(index);
  }
  
  public String getRawString(int index) {
    return list.get(index).getRawString();
  }

  public void addElement(ParsedElement element) {
    list.add(element);
  }

  public String getParsableString() {
    StringBuffer buffer = new StringBuffer(OPEN_SYMBOL.toString());
    for (int i = 0; i < list.size(); i++) {
      if (i > 0) {
        buffer.append(DIVIDER_SYMBOL.toString() + " ");
      }
      buffer.append(list.get(i).getParsableString());
    }
    buffer.append(CLOSE_SYMBOL);
    return buffer.toString();
  }

  /**
   * Parse attribute value and set the list member variable.  May set the valid
   * member variable.
   * @param attribute
   * @param etomoNumberType
   */
  public void parse(ReadOnlyAttribute attribute) {
    list.clear();
    valid = true;
    if (attribute == null) {
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(attribute.getValue());
    Token token = null;
    try {
      token = tokenizer.next();
      if (token == null) {
        return;
      }
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      if (token == null
          || !token.equals(Token.Type.SYMBOL, OPEN_SYMBOL.charValue())) {
        valid = false;
        return;
      }
      token = tokenizer.next();
      //remove any whitespace before the first element
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      token = parseList(token, tokenizer);
      if (!valid) {
        return;
      }
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      //if the close symbol wasn't found, fail
      if (token == null
          || !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
        valid = false;
        return;
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
  }
  
  private PrimativeTokenizer createTokenizer(String value) {
    PrimativeTokenizer tokenizer = new PrimativeTokenizer(value);
    try {
      tokenizer.initialize();
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
      valid = false;
    }
    return tokenizer;
  }

  private Token parseList(Token token, PrimativeTokenizer tokenizer) {
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //loop until a divider isn't found, this should be the end of the list
    while (dividerFound && valid) {
      try {
        //parse an element.
        token = parseElement(token, tokenizer);
        //remove any whitespace
        if (token != null && token.is(Token.Type.WHITESPACE)) {
          token = tokenizer.next();
        }
        dividerFound = false;
        //if the divider symbol is found, continue parsing elements
        if (token != null
            && token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
          dividerFound = true;
          token = tokenizer.next();
          //remove whitespace after divider
          if (token != null && token.is(Token.Type.WHITESPACE)) {
            token = tokenizer.next();
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        valid = false;
        return token;
      }
    }
    return token;
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    //end of list
    if (token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
      //This probably doesn't matter because its only necessary to keep track of
      //location of existing elements in a lightly populated list.  Its not really
      //necessary to track the size of the list.  But this does preserve the
      //the original string:
      //{} means no elements, {,} means two elements.  {'tomo',,} means three elements
      if (list.size() > 0) {
        list.addEmptyElement();
      }
      return token;
    }
    //found empty element
    if (token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
      list.addEmptyElement();
      return token;
    }
    //may have found an element
    ParsedElement element;
    if (type == Type.STRING) {
      element = new ParsedQuotedString();
    }
    else if (ParsedArray.isArray(token)) {
      element = new ParsedArray(etomoNumberType, defaultValue);
    }
    else {
      element = new ParsedNumber(etomoNumberType, defaultValue);
    }
    //parse the token
    token = element.parse(token, tokenizer);
    list.add(element);
    return token;
  }

  private static final class Type {
    private static final Type NUMERIC = new Type();
    private static final Type STRING = new Type();

    private Type() {
    }
  }
}
