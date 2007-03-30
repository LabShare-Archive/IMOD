package etomo.type;

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
 * <p> $Log$ </p>
 */
public final class ParsedArray extends ParsedElement {
  public static final String rcsid = "$Id$";

  static final Character OPEN_SYMBOL = new Character('[');
  static final Character CLOSE_SYMBOL = new Character(']');
  static final Character DIVIDER_SYMBOL = new Character(',');

  private final ParsedElementList list = new ParsedElementList();

  private EtomoNumber.Type etomoNumberType = null;
  private Integer defaultValue = null;

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
  
  public void parse(ReadOnlyAttribute attribute) {}

  public String getRawString(int index) {
    return list.get(index).getRawString();
  }
  
  public void setRawNumber(int index,String number) {
    ParsedNumber element = new ParsedNumber(etomoNumberType,defaultValue);
    element.setRawNumber(number);
    list.set(index,element);
  }

  /**
   * Clear the list member variable, and add each element of the ParsedElement
   * to the list.
   * @param ParsedArray
   */
  public void setElementArray(ParsedElement elementArray) {
    list.clear();
    for (int i = 0; i < elementArray.size(); i++) {
      list.add(elementArray.getElement(i));
    }
  }
  
  public boolean isEmpty() {
    return list.size() == 0;
  }
  
  Token parse(Token token, PrimativeTokenizer tokenizer) {return null;}
  
  int size() {
    return list.size();
  }
  
  ParsedElement getElement(int index) {
    return list.get(index);
  }
  
  /**
   * Asking for a raw string from a collection returns a semi-raw string.  The
   * elements are separated by a divider in the same way a parsable string would be,
   * but the "[" and "]" are not added.
   */
  String getRawString() {
    StringBuffer buffer= new StringBuffer();
    boolean elementIsCollection = false;
    for (int i = 0;i <list.size();i++) {
      if (i>0) {
        if (elementIsCollection) {
          buffer.append(DIVIDER_SYMBOL+" ");
        }
        else {
          buffer.append(" ");
        }
      }
      buffer.append(list.get(i).getRawString());
    }
    return buffer.toString();
  }
  
  String getParsableString() {
    StringBuffer buffer= new StringBuffer(OPEN_SYMBOL.toString());
    buffer.append(getRawString());
    buffer.append(CLOSE_SYMBOL);
    return buffer.toString();
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
}
