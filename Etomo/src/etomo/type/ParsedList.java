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
 * <p> $Log$ </p>
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
  
  public static ParsedList getNumericInstance(EtomoNumber.Type etomoNumberType,int defaultValue) {
    return new ParsedList(Type.NUMERIC, etomoNumberType,defaultValue);
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

  public ParsedElement getElement(int index) {
    return list.get(index);
  }
  
  public void addElement(ParsedElement element) {
    list.add(element);
  }
  
  public String getParsableString() {
    StringBuffer buffer= new StringBuffer(OPEN_SYMBOL.toString());
    for (int i =0;i<list.size();i++) {
      if (i>0) {
        buffer.append(DIVIDER_SYMBOL.toString()+" ");
      }
      buffer.append(list.get(i).getParsableString());
    }
    buffer.append(CLOSE_SYMBOL);
    return buffer.toString();
  }
  
  String getRawString(int index) {
    return list.get(index).getRawString();
  }
  
  /**
   * Parse attribute value and set the list member variable.  May set the valid
   * member variable.
   * @param attribute
   * @param etomoNumberType
   */
  private void parse(ReadOnlyAttribute attribute) {
    list.clear();
    if (attribute == null) {
      return;
    }
    String value = attribute.getValue();
    PrimativeTokenizer tokenizer = createTokenizer(value);
    StringBuffer buffer = new StringBuffer();
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
          || !token.equals(Token.Type.SYMBOL, OPEN_SYMBOL
              .charValue())) {
        fail(value);
        return;
      }
      token = tokenizer.next();
      token = parseList(token, tokenizer, value);
      if (!valid) {
        return;
      }
      if (token == null
          || !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL
              .charValue())) {
        fail(value);
        return;
      }
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(value);
    }
  }

  private Token parseList(Token token, PrimativeTokenizer tokenizer,
      String value) {
    if (token == null) {
      return null;
    }
    while (valid
        && token != null
        && !token.is(Token.Type.EOL)
        && !token.is(Token.Type.EOF)
        && !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL
            .charValue())) {
      try {
        if (token.is(Token.Type.WHITESPACE)) {
          token = tokenizer.next();
        }
        token = parseElement(token, tokenizer);
        if (token.is(Token.Type.WHITESPACE)) {
          token = tokenizer.next();
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        fail(value);
        return token;
      }
      if (!token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL
          .charValue())
          && !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL
              .charValue())) {
        fail(value);
      }
    }
    return token;
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    //end of list
    if (token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL
        .charValue())) {
      //This probably doesn't matter because its only necessary to keep track of
      //location of existing elements in a lightly populated array.  Its not really
      //necessary to track the size of the array.  But this does preserve the
      //the original string:
      //{} means no elements, {,} means two elements.  {'tomo',,} means three elements
      if (list.size() > 0) {
        list.addEmptyElement();
      }
      return token;
    }
    //found empty element
    if (token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL
        .charValue())) {
      list.addEmptyElement();
      return token;
    }
    //find element
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
    while (!token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL
        .charValue())
        && !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL
            .charValue())) {
      token = element.parse(token, tokenizer);
      list.add(element);
    }
    return token;
  }

  private void fail(String value) {
    valid = false;
    ParsedString string = new ParsedString();
    string.setRawString(value);
    list.add(string);
  }

  private PrimativeTokenizer createTokenizer(String value) {
    PrimativeTokenizer tokenizer = new PrimativeTokenizer(value);
    StringBuffer buffer = new StringBuffer();
    Token token = null;
    boolean firstToken = true;
    try {
      tokenizer.initialize();
    }
    catch (IOException e) {
      e.printStackTrace();
      fail(value);
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
      fail(value);
    }
    return tokenizer;
  }

  private static final class Type {
    private static final Type NUMERIC = new Type();
    private static final Type STRING = new Type();

    private Type() {
    }
  }
}
