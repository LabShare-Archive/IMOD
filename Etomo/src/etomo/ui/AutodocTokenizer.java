package etomo.ui;

import java.io.File;
//import java.io.StreamTokenizer;
//import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.IllegalStateException;

/**
* <p>Description:
* Creates the tokens required for autodoc functionality.  It can recognize the
* following tokens:  EOF, EOL, WHITESPACE, COMMENT, SEPARATOR, OPEN, CLOSE,
* DELIMITER, WORD, and KEYWORD.  It is not case sensitive, but it does preserve
* original case and whitespace.
* 
* To Use:
* construct with a file.
* call initialize().
* call next() to get the next token, until the end of file is reached.
* 
* Testing:
* Do not call initialize() when testing.
* Call test() to test this class.
* Call testPrimativeTokenizer() to test the PrimativeTokenizer.
* Call testStreamTokenizer() to test the StreamTokenizer.
*
* Current token characters and strings:
* COMMENT:  #
* SEPARATOR:  .
* OPEN:  [
* CLOSE:  ]
* Default DELIMITER:  =
* BREAK:  ^
* Keywords:  Version, Pip, KeyValueDelimiter
* 
* Tokenizing Rules:
* 
* Currently the COMMENT, SEPARATOR, OPEN, CLOSE, and BREAK symbols must be single
* character.
* 
* The delimiter may contain multiple characters.  It may not contain symbols
* used by other tokens.
* 
* COMMENT, SEPARATOR, OPEN, CLOSE, DELIMITER, and BREAK should not contain
* alphanumeric or whitespace characters.
* 
* WHITESPACE, EOL, and EOF are defined in PrimativeTokenizer.
* 
* A WORD is the longest possible set of characters that are alphanumeric and
* unmatched symbols, and do not match the current delimiter string.
* 
* A KEYWORD is a word that completely matches a keyword constant.  Embedded
* strings matching keyword constants are ignored.
</p>
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
* <p> $Revision 1.5  2005/02/15 19:53:12  sueh
* <p> $bug# 602 Tokenizing BREAK.  "^" is now a matched character.
* <p> $
* <p> $Revision 1.4  2003/12/31 17:48:07  sueh
* <p> $bug# 372 change doc
* <p> $
* <p> $Revision 1.3  2003/12/31 01:28:30  sueh
* <p> $bug# 372 simplified next() function, fixed delimiter bugs,
* <p> $added doc
* <p> $
* <p> $Revision 1.2  2003/12/23 21:33:08  sueh
* <p> $bug# 372 Reformating.
* <p> $
* <p> $Revision 1.1  2003/12/22 23:49:28  sueh
* <p> $bug# 372 creates tokens for AutodocParser
* <p> $$ </p>
*/
public class AutodocTokenizer {
  public static final String rcsid =
    "$$Id$$";

  public static final char COMMENT_CHAR = '#';
  public static final char SEPARATOR_CHAR = '.';
  public static final char OPEN_CHAR = '[';
  public static final char CLOSE_CHAR = ']';
  public static final char BREAK_CHAR = '^';
  public static final String defaultDelimiter = "=";
  public static final String VERSION_KEYWORD = "Version";
  public static final String PIP_KEYWORD = "Pip";
  public static final String DELIMITER_KEYWORD = "KeyValueDelimiter";
  private String delimiterString = defaultDelimiter;
  private StringBuffer restrictedSymbols =
    new StringBuffer(COMMENT_CHAR + SEPARATOR_CHAR + OPEN_CHAR + CLOSE_CHAR + BREAK_CHAR);
  private PrimativeTokenizer primative = null;
  private File file = null;
  private Token primativeToken = null;
  private Token autodocToken = null;
  private Token token = new Token();
  private Token nextToken = new Token();
  private boolean useNextToken = false;
  private StringBuffer wordBuffer = null;

  AutodocTokenizer(File file) {
    this.file = file;
    primative = new PrimativeTokenizer(file);
  }

  public void initialize() throws FileNotFoundException, IOException {
    primative.initialize();
  }

  public Token getToken() {
    return autodocToken;
  }
  public String getDelimiterString() {
    return delimiterString;
  }

  public boolean setDelimiterString(String delimiterString) {
    if (delimiterString == null) {
      return false;
    }
    char character;
    String symbols = primative.getSymbols();
    for (int i = 0; i < delimiterString.length(); i++) {
      character = delimiterString.charAt(i);
      if (restrictedSymbols.toString().indexOf(character) != -1) {
        return false;
      }
      if (symbols.indexOf(character) == -1) {
        return false;
      }
    }
    this.delimiterString = delimiterString;
    return true;
  }

  public Token next() throws IOException {
    if (useNextToken) {
      useNextToken = false;
      autodocToken = new Token(nextToken);
      return autodocToken;
    }
    primativeToken = primative.next();
    autodocToken = new Token(findToken());
    return autodocToken;
  }
  
  
  public void test(boolean tokens) throws IOException {
    initialize();
    Token token = null;
    do {
      token = next();
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

  public void testPrimativeTokenizer(boolean tokens) throws IOException {
    primative.test(tokens);
  }

  public void testStreamTokenizer(boolean tokens) throws IOException {
    primative.testStreamTokenizer(tokens);
  }


  private Token findToken() throws IOException {
    boolean buildingWord = false;
    do {
      if (findSimpleToken()) {
        if (buildingWord) {
          buildingWord = false;
          makeWord();
          return token;
        }
        return token;
      }
      if (primativeToken.is(Token.ALPHANUM)) {
        buildingWord = true;
        buildWord();
        primativeToken = primative.next();
      }
      else {
        if (findDelimiter()) {
          if (buildingWord) {
            buildingWord = false;
            makeWord();
            return token;
          }
          return token;
        }
        buildingWord = true;
      }
    }
    while (buildingWord);
    throw new IllegalStateException();
  }

  private boolean findSimpleToken() {
    if (primativeToken.is(Token.EOF)
      || primativeToken.is(Token.EOL)
      || primativeToken.is(Token.WHITESPACE)) {
      token.set(primativeToken);
      return true;
    }
    else if (primativeToken.equals(Token.SYMBOL, COMMENT_CHAR)) {
      token.set(Token.COMMENT, COMMENT_CHAR);
      return true;
    }
    else if (primativeToken.equals(Token.SYMBOL, SEPARATOR_CHAR)) {
      token.set(Token.SEPARATOR, SEPARATOR_CHAR);
      return true;
    }
    else if (primativeToken.equals(Token.SYMBOL, OPEN_CHAR)) {
      token.set(Token.OPEN, OPEN_CHAR);
      return true;
    }
    else if (primativeToken.equals(Token.SYMBOL, CLOSE_CHAR)) {
      token.set(Token.CLOSE, CLOSE_CHAR);
      return true;
    }
    else if (primativeToken.equals(Token.SYMBOL, BREAK_CHAR)) {
      token.set(Token.BREAK, BREAK_CHAR);
      return true;
    }
    else if (primativeToken.equals(Token.SYMBOL, delimiterString)) {
      //Found a one character DELIMITER.
      token.set(Token.DELIMITER, delimiterString);
      return true;
    }
    return false;
  }

  private boolean findDelimiter() throws IOException {
    int length = delimiterString.length();
    int index = 0;
    StringBuffer delimiterBuffer = null;
    boolean found = false;
    while (!found
      && primativeToken.is(Token.SYMBOL) && index < length
      && delimiterString.indexOf(primativeToken.getValue(), index) == index) {
      //Build a buffer containing only symbols that could be part of the delimiter
      if (delimiterBuffer == null) {
        delimiterBuffer = new StringBuffer(primativeToken.getValue());
      }
      else {
        delimiterBuffer.append(primativeToken.getValue());
      }
      if (index == length - 1) {
        //set found - do not get next primative token
        found = true;
      }
      else {
        //haven't check the entire delimiter string - get next primative token
        index++;
        primativeToken = primative.next();
      }
    }
    if (found) {
      token.set(Token.DELIMITER, delimiterBuffer);
      return true;
    }
    //delimiter contains only symbols not found in the findSimpleTokens()
    //delimiter failed - must be a word
    if (delimiterBuffer == null) {
      //never went into while loop - stale primative token
      buildWord();
      primativeToken = primative.next();
    }
    else {
      buildWord(delimiterBuffer);
    }
    return false;
  }

  private void buildWord() {
    if (wordBuffer == null) {
      wordBuffer = new StringBuffer(primativeToken.getValue());
    }
    else {
      wordBuffer.append(primativeToken.getValue());
    }
  }

  private void buildWord(StringBuffer buffer) {
    if (wordBuffer == null) {
      wordBuffer = new StringBuffer(buffer.toString());
    }
    else {
      wordBuffer.append(buffer);
    }
  }

  private void makeWord() {
    //the entire word is found when the next token is found
    nextToken.set(token);
    useNextToken = true;
    token.set(Token.WORD, wordBuffer);
    wordBuffer = null;
    findKeyword();
  }

  private void findKeyword() {
    if (token.is(Token.WORD)) {
      if (token.equals(VERSION_KEYWORD)
        || token.equals(PIP_KEYWORD)
        || token.equals(DELIMITER_KEYWORD)) {
        token.set(Token.KEYWORD); }
    }
  }

}
