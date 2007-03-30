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
 * <p> $Log$ </p>
 */
final class ParsedString extends ParsedElement {
  public static final String rcsid = "$Id$";

  private static final String SYMBOL_STRING = ParsedList.OPEN_SYMBOL.toString()
      + ParsedList.CLOSE_SYMBOL.toString()
      + ParsedList.DIVIDER_SYMBOL.toString()
      + ParsedArray.OPEN_SYMBOL.toString()
      + ParsedArray.CLOSE_SYMBOL.toString()
      + ParsedArray.DIVIDER_SYMBOL.toString()
      + ParsedArrayDescriptor.DIVIDER_SYMBOL.toString();

  private String string = "";
  private boolean valid = false;

  void setRawString(String string) {
    valid = false;
    if (string == null || string.matches("\\s*")) {
      string = "";
    }
    else {
      this.string = string;
    }
  }

  String getRawString() {
    return string;
  }
  
  String getParsableString() {
    return getRawString();
  }

  ParsedElement getElement(int index) {
    if (index == 0) {
      return this;
    }
    return null;
  }

  Token parse(Token token, final PrimativeTokenizer tokenizer) {
    string = "";
    valid = true;
    if (token == null) {
      return null;
    }
    try {
      //remove whitespace
      if (token.is(Token.Type.WHITESPACE)) {
        token = tokenizer.next();
      }
      StringBuffer buffer = new StringBuffer();
      //collect tokens until the end of the line, whitespace, or a recognized symbol is found
      while (token != null
          && !token.is(Token.Type.EOL)
          && !token.is(Token.Type.EOF)
          && !(token.is(Token.Type.SYMBOL) && SYMBOL_STRING.indexOf(token
              .getValue()) != -1) && !token.is(Token.Type.WHITESPACE)) {
        buffer.append(token.getValue());
        token = tokenizer.next();
      }
      string = buffer.toString();
    }
    catch (IOException e) {
      e.printStackTrace();
      valid = false;
    }
    return token;
  }

  int size() {
    return 1;
  }
}
