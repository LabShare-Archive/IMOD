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
 * <p> Revision 1.2  2007/03/31 02:54:01  sueh
 * <p> bug# 964 In getRawString(), using a "," when some of the elements are empty -
 * <p> so that the location of the existing elements isn't lost.  Using ParsedElement.isCollection()
 * <p> to decide whether to use "," in other cases:  using a comma when one of the
 * <p> elements is an array descriptor.
 * <p>
 * <p> Revision 1.1  2007/03/30 23:40:59  sueh
 * <p> bug# 964 Class to parse Matlab arrays.
 * <p> </p>
 */
public final class ParsedArray extends ParsedElement {
  public static final String rcsid = "$Id$";

  static final Character OPEN_SYMBOL = new Character('[');
  static final Character CLOSE_SYMBOL = new Character(']');
  static final Character DIVIDER_SYMBOL = new Character(',');

  private final ParsedElementList array = new ParsedElementList();

  private EtomoNumber.Type etomoNumberType = null;
  private Integer defaultValue = null;
  private boolean valid = true;

  public ParsedArray() {
  }

  public ParsedArray(EtomoNumber.Type etomoNumberType) {
    this.etomoNumberType = etomoNumberType;
  }

  public ParsedArray(EtomoNumber.Type etomoNumberType, int defaultValue) {
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = new Integer(defaultValue);
  }

  public ParsedArray(EtomoNumber.Type etomoNumberType, Integer defaultValue) {
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = defaultValue;
  }

  public static ParsedArray getInstance(ReadOnlyAttribute attribute) {
    ParsedArray instance = new ParsedArray();
    instance.parse(attribute);
    return instance;
  }

  /**
   * This is a array only if starts with "[" (ignoring whitespace).
   * @param attribute
   * @return
   */
  static boolean isArray(Token token) {
    if (token == null) {
      return false;
    }
    if (token.equals(Token.Type.SYMBOL, OPEN_SYMBOL.charValue())) {
      return true;
    }
    return false;
  }
  
  public void parse(ReadOnlyAttribute attribute) {
    array.clear();
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

  public String getRawString(int index) {
    return array.get(index).getRawString();
  }

  public ConstEtomoNumber getRawNumber(int index) {
    return array.get(index).getRawNumber();
  }

  public ConstEtomoNumber getRawNumber() {
    return getRawNumber(0);
  }

  public void setRawNumber(int index, String number) {
    ParsedNumber element = new ParsedNumber(etomoNumberType, defaultValue);
    element.setRawNumber(number);
    array.set(index, element);
  }

  /**
   * Clear the list member variable, and add each element of the ParsedElement
   * to the list.
   * @param ParsedArray
   */
  public void setElementArray(ParsedElement elementArray) {
    array.clear();
    for (int i = 0; i < elementArray.size(); i++) {
      array.add(elementArray.getElement(i));
    }
  }

  public int size() {
    return array.size();
  }

  ParsedElement getElement(int index) {
    return array.get(index);
  }
  
  void fail() {
    valid =false;
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
      //remove any whitespace before the first element
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      token = parseArray(token, tokenizer);
      if (!valid) {
        return token;
      }
      if (token != null && token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
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

  /**
   * Asking for a raw string from a collection returns a semi-raw string.  The
   * elements are separated by a divider in the same way a parsable string would be,
   * but the "[" and "]" are not added.
   */
  String getRawString() {
    StringBuffer buffer = new StringBuffer();
    //only use spaces as dividers when there are no empty elements and no array
    //descriptors
    boolean elementIsCollection = false;
    boolean emptyElements =  false;
    boolean empty = true;//set to false when a non-empty element is found
    for (int i = 0; i < array.size(); i++) {
      if (i > 0) {
        //use the divider symbol if there are missing elements or complex elements
        if (emptyElements || elementIsCollection) {
          buffer.append(DIVIDER_SYMBOL + " ");
        }
        else {
          buffer.append(" ");
        }
      }
      ParsedElement element = array.get(i);
      elementIsCollection = element.isCollection();
      emptyElements = element.isEmpty();
      if (!emptyElements) {
        empty = false;
      }
      buffer.append(element.getRawString());
    }
    if (empty) {
      return "";
    }
    return buffer.toString();
  }
  
  /**
   * @return true if the size of the list is null.  Empty elements in the list
   * do not affect this result
   */
  boolean isEmpty() {
    return size()==0;
  }

  boolean isCollection() {
    return true;
  }

  /**
   * Returns [] if the list is empty.  Returns a list surrounded by brackets if
   * there are multiple elements.
   */
  public String getParsableString() {
    StringBuffer buffer = new StringBuffer(OPEN_SYMBOL.toString());
    buffer.append(getRawString());
    buffer.append(CLOSE_SYMBOL.toString());
    return buffer.toString();
  }
  
  public String toString() {
    return "[array:"+array+"]";
  }

  private Token parseArray(Token token, PrimativeTokenizer tokenizer) {
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //loop until the end of the array
    //can't just check dividerFound, because whitespace can act as a divider,
    //but isn't always the divider
    while (dividerFound && valid && token != null && !token.is(Token.Type.EOL)
        && !token.is(Token.Type.EOF)
        && !token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
      try {
        //parse an element
        token = parseElement(token, tokenizer);
        //whitespace may be used as a divider, but it may not always be the divider -
        //it could just be whitespace at the end of the array or whitespace
        //before the divider
        dividerFound = false;
        if (token != null && token.is(Token.Type.WHITESPACE)) {
          dividerFound = true;
          token = tokenizer.next();
        }
        if (token != null
            && token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
          dividerFound = true;
          token = tokenizer.next();
          //A "," is being used as a divider character so remove whitespace
          //after divider
          if (token != null && token.is(Token.Type.WHITESPACE)) {
            token = tokenizer.next();
          }
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        valid = false;
      }
    }
    return token;
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    //end of list
    if (token.equals(Token.Type.SYMBOL, CLOSE_SYMBOL.charValue())) {
      //This probably doesn't matter because its only necessary to keep track of
      //location of existing elements in a lightly populated array.  Its not really
      //necessary to track the size of the array.  But this does preserve the
      //the original string:
      //[] means no elements, [,] means two elements.  [1,,] means three elements
      if (array.size() > 0) {
        array.addEmptyElement();
      }
      return token;
    }
    //found empty element
    if (token.is(Token.Type.WHITESPACE)
        || token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
      array.addEmptyElement();
      return token;
    }
    //may have found an element
    //Array descriptors don't have their own open and close symbols, so they
    //look like numbers until to you get to the first divider (":").
    ParsedArrayDescriptor element= new ParsedArrayDescriptor(etomoNumberType, defaultValue);
    token=element.parse(token,tokenizer);
    if (element.size()==0) {
      //There's nothing there, so its an empty element
      array.addEmptyElement();
    }
    if (element.size()==1) {
      //If there is only one number, then there was no ":" and its really a ParsedNumber.
      array.add(element.getElement(0));
    }
    else {
      array.add(element);
    }
    return token;
  }
}
