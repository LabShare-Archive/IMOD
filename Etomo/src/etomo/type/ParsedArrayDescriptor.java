package etomo.type;

import java.io.IOException;

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
 * <p> Revision 1.4  2007/04/13 20:14:25  sueh
 * <p> bug# 964 Added setRawString(String), which parses a list of numbers separated
 * <p> by :'s.
 * <p>
 * <p> Revision 1.3  2007/04/09 21:00:29  sueh
 * <p> bug# 964 Added parsing.
 * <p>
 * <p> Revision 1.2  2007/03/31 02:54:24  sueh
 * <p> bug# 964 Added isCollection().
 * <p>
 * <p> Revision 1.1  2007/03/30 23:41:13  sueh
 * <p> bug# 964 Class to parse Matlab array descriptors.
 * <p> </p>
 */

final class ParsedArrayDescriptor extends ParsedElement {
  public static final String rcsid = "$Id$";

  static final Character DIVIDER_SYMBOL = new Character(':');

  private final ParsedElementList descriptor = new ParsedElementList();

  private EtomoNumber.Type etomoNumberType = null;
  private Integer defaultValue = null;
  private boolean valid = true;

  ParsedArrayDescriptor(EtomoNumber.Type etomoNumberType, Integer defaultValue) {
    this.etomoNumberType = etomoNumberType;
    this.defaultValue = defaultValue;
  }

  public String toString() {
    return "[descriptor:" + descriptor + "]";
  }

  int size() {
    return descriptor.size();
  }

  void fail() {
    valid = false;
  }

  public boolean isEmpty() {
    return size() == 0;
  }

  Token parse(Token token, PrimativeTokenizer tokenizer) {
    descriptor.clear();
    valid = true;
    if (token == null) {
      return null;
    }
    boolean dividerFound = true;
    //loop until a divider isn't found, this should be the end of the list
    while (dividerFound && valid) {
      try {
        //parse an element.
        token = parseElement(token, tokenizer);
        //whitespace is not allowed in a file descriptor and it may be used as
        //an array divider, so it shouldn't be removed
        dividerFound = false;
        //if the divider symbol is found, continue parsing elements
        if (token != null
            && token.equals(Token.Type.SYMBOL, DIVIDER_SYMBOL.charValue())) {
          dividerFound = true;
          token = tokenizer.next();
        }
      }
      catch (IOException e) {
        e.printStackTrace();
        fail();
      }
    }
    return token;
  }

  ParsedElement getElement(int index) {
    return descriptor.get(index);
  }

  public String getRawString() {
    StringBuffer buffer = new StringBuffer();
    for (int i = 0; i < descriptor.size(); i++) {
      if (i > 0) {
        buffer.append(DIVIDER_SYMBOL.charValue());
      }
      buffer.append(descriptor.get(i).getRawString());
    }
    return buffer.toString();
  }
  
  /**
   * input is a collection, since it is not indexed, so it is a semi-raw string
   * - it has :'s
   */
  public void setRawString(String input) {
    descriptor.clear();
    valid = true;
    if (input == null) {
      return;
    }
    PrimativeTokenizer tokenizer = createTokenizer(input);
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
    parse(token,tokenizer);
  }

  public Number getRawNumber() {
    return descriptor.get(0).getRawNumber();
  }

  boolean isCollection() {
    return true;
  }

  String getParsableString() {
    return getRawString();
  }

  private Token parseElement(Token token, PrimativeTokenizer tokenizer) {
    //parse a number
    ParsedNumber element = new ParsedNumber(etomoNumberType, defaultValue);
    token = element.parse(token, tokenizer);
    descriptor.add(element);
    return token;
  }
}
