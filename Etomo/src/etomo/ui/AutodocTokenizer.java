package etomo.ui;

import java.io.File;
//import java.io.StreamTokenizer;
//import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;

/**
* <p>Description:</p>
*
* <p>Copyright: Copyright © 2002, 2003</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$
* <p> $Revision 1.1  2003/12/22 23:49:28  sueh
* <p> $bug# 372 creates tokens for AutodocParser
* <p> $$ </p>
*/
public class AutodocTokenizer {
  public static final String rcsid =
    "$$Id$$";

  public static final String COMMENT_CHAR = "#";
  public static final String SEPARATOR_CHAR = ".";
  public static final String OPEN_CHAR = "[";
  public static final String CLOSE_CHAR = "]";
  public static final String defaultDelimiter = "=";
  public static final String VERSION_KEYWORD = "version";
  public static final String PIP_KEYWORD = "pip";
  public static final String DELIMITER_KEYWORD = "keyvaluedelimiter";
  private String delimiterString = defaultDelimiter;
  private PrimativeTokenizer primative = null;
  private File file = null;
  private Token primativeToken;
  private Token token = new Token();
  boolean nextTokenFound = false;
  private Token nextToken = new Token();

  AutodocTokenizer(File file) {
    this.file = file;
    primative = new PrimativeTokenizer(file);
  }

  public void initialize() throws FileNotFoundException, IOException {
    primative.initialize();
    primativeToken = primative.next();
  }

  public Token getToken() {
    return token;
  }

  public void setDelimiter(String delimiter) {
    delimiterString = delimiter;
  }

  public String getDelimiter() {
    return delimiterString;
  }

  public Token next() throws IOException {
    //System.out.println("AutodocTokenizer.next");
    boolean found = false;
    token.reset();
    StringBuffer valueBuffer = null;
    boolean wordFound = false;
    StringBuffer wordBuffer = null;

    if (nextTokenFound) {
      found = true;
      token.set(nextToken);
      nextToken.reset();
      nextTokenFound = false;
      primativeToken = primative.next();
    }
    while (!found) {
      if (primativeToken.is(Token.EOF)
        || primativeToken.is(Token.EOL)
        || primativeToken.is(Token.WHITESPACE)) {
        token.set(primativeToken);
        found = true;
      }
      else if (primativeToken.equals(Token.SYMBOL, COMMENT_CHAR)) {
        token.set(Token.COMMENT, COMMENT_CHAR);
        found = true;
      }
      else if (primativeToken.equals(Token.SYMBOL, SEPARATOR_CHAR)) {
        token.set(Token.SEPARATOR, SEPARATOR_CHAR);
        found = true;
      }
      else if (primativeToken.equals(Token.SYMBOL, OPEN_CHAR)) {
        token.set(Token.OPEN, OPEN_CHAR);
        found = true;
      }
      else if (primativeToken.equals(Token.SYMBOL, CLOSE_CHAR)) {
        token.set(Token.CLOSE, CLOSE_CHAR);
        found = true;
      }
      else if (primativeToken.equals(Token.SYMBOL, delimiterString)) {
        token.set(Token.DELIMITER, delimiterString);
        found = true;
      }
      else if (
        primativeToken.is(Token.SYMBOL)
          && delimiterString.indexOf(primativeToken.getValue()) == 0) {
        valueBuffer = new StringBuffer().append(primativeToken.getValue());
        int index = 1;
        while (primative.next().is(Token.SYMBOL)
          && index <= delimiterString.length()
          && delimiterString.indexOf(primative.getToken().getValue()) == index) {
          valueBuffer.append(primative.getToken().getValue());
          index++;
        }
        if (index == delimiterString.length()) {
          token.set(Token.DELIMITER, delimiterString);
          found = true;
        }
      }
      if (!found) {
        if (!wordFound) {
          wordFound = true;
          if (valueBuffer == null) {
            wordBuffer = new StringBuffer().append(primativeToken.getValue());
          }
          else {
            wordBuffer =
              new StringBuffer().append(valueBuffer).append(
                primative.getToken().getValue());
          }
        }
        else {
          wordBuffer.append(primativeToken.getValue());
        }
      }

      if (found) {
        if (wordFound) {
          nextTokenFound = true;
          nextToken.set(token);
          if (valueBuffer != null) {
            nextToken.set(valueBuffer.toString());
          }
          token.set(Token.WORD, wordBuffer.toString());
          wordBuffer = null;
          wordFound = false;
        }
      }
      if (!nextTokenFound
        && (token.is(Token.DELIMITER) || delimiterString.length() == 1)) {
        primativeToken = primative.next();
      }
      else {
        primativeToken = primative.getToken();
      }
    }

    if (token.is(Token.WORD)) {
      if (token.equals(VERSION_KEYWORD)
        || token.equals(PIP_KEYWORD)
        || token.equals(DELIMITER_KEYWORD)) {
        token.set(Token.KEYWORD);
      }
    }
    return token;
  }

  public void testPrimativeTokenizer(boolean tokens) throws IOException {
    primative.initialize();
    Token token;
    do {
      token = primative.next();
      if (tokens) {
        System.out.println(token.toString());
      }
      else if (token.is(Token.EOL)) {
        System.out.println();
      }
      else if (!token.is(Token.EOF)) {
        System.out.print(token.getValue());
      }
    }
    while (!token.is(Token.EOF));
  }

  public void testStreamTokenizer(boolean tokens) throws IOException {
    primative.testStreamTokenizer(tokens);
  }

}
