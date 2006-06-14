package etomo.storage.autodoc;

import java.io.File;
//import java.io.StreamTokenizer;
//import java.io.FileReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.IllegalStateException;

import etomo.ui.Token;
import etomo.util.PrimativeTokenizer;

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
 * <p>Copyright: Copyright 2002 - 2006</p>
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
 * <p> $Revision 1.4  2006/06/14 00:32:28  sueh
 * <p> $bug# 852 Added some comments.  Simplified findDelimiter().
 * <p> $
 * <p> $Revision 1.3  2006/05/01 21:17:15  sueh
 * <p> $bug# 854
 * <p> $
 * <p> $Revision 1.2  2006/04/06 20:08:58  sueh
 * <p> $Moved PrimativeTokenizer to util.
 * <p> $
 * <p> $Revision 1.1  2006/01/12 17:02:49  sueh
 * <p> $bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p> $
 * <p> $Revision 1.7  2005/11/10 18:16:10  sueh
 * <p> $bug# 733 Changed defaultDelimiter to DEFAULT_DELIMITER
 * <p> $
 * <p> $Revision 1.6  2005/02/21 23:03:22  sueh
 * <p> $update comment.
 * <p> $
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
public final class AutodocTokenizer {
  public static final String rcsid = "$$Id$$";

  //special characters
  public static final char COMMENT_CHAR = '#';
  public static final char SEPARATOR_CHAR = '.';
  public static final char OPEN_CHAR = '[';
  public static final char CLOSE_CHAR = ']';
  public static final char BREAK_CHAR = '^';
  public static final String DEFAULT_DELIMITER = "=";
  //keywords - keywords may not contain special characters
  public static final String VERSION_KEYWORD = "Version";
  public static final String PIP_KEYWORD = "Pip";
  public static final String DELIMITER_KEYWORD = "KeyValueDelimiter";

  private String delimiterString = DEFAULT_DELIMITER;
  private StringBuffer restrictedSymbols = new StringBuffer(COMMENT_CHAR
      + SEPARATOR_CHAR + OPEN_CHAR + CLOSE_CHAR + BREAK_CHAR);
  private PrimativeTokenizer primativeTokenizer = null;
  private File file = null;
  private Token primativeToken = null;
  private Token autodocToken = null;
  private Token token = new Token();
  private Token nextToken = new Token();
  private boolean useNextToken = false;
  private StringBuffer wordBuffer = null;

  AutodocTokenizer(File file) {
    this.file = file;
    primativeTokenizer = new PrimativeTokenizer(file);
  }

  void initialize() throws FileNotFoundException, IOException {
    primativeTokenizer.initialize();
  }

  Token getToken() {
    return autodocToken;
  }

  String getDelimiterString() {
    return delimiterString;
  }

  boolean setDelimiterString(String delimiterString) {
    if (delimiterString == null) {
      return false;
    }
    char character;
    String symbols = primativeTokenizer.getSymbols();
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

  Token next() throws IOException {
    if (useNextToken) {
      useNextToken = false;
      autodocToken = new Token(nextToken);
      return autodocToken;
    }
    primativeToken = primativeTokenizer.next();
    autodocToken = new Token(findToken());
    return autodocToken;
  }

  void test(boolean tokens) throws IOException {
    initialize();
    Token token = null;
    do {
      token = next();
      if (tokens) {
        System.out.println(token.toString());
      }
      else if (token.is(Token.Type.EOL)) {
        System.out.println();
      }
      else if (!token.is(Token.Type.EOF)) {
        System.out.print(token.getValue());
      }
    } while (!token.is(Token.Type.EOF));
  }

  void testPrimativeTokenizer(boolean tokens) throws IOException {
    primativeTokenizer.test(tokens);
  }

  void testStreamTokenizer(boolean tokens) throws IOException {
    primativeTokenizer.testStreamTokenizer(tokens);
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
      if (primativeToken.is(Token.Type.ALPHANUM)) {
        buildingWord = true;
        buildWord();
        primativeToken = primativeTokenizer.next();
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
        //Don't have to call buildWord() here, because findDelimiter handles
        //building a word when it fails.
        buildingWord = true;
      }
    } while (buildingWord);
    throw new IllegalStateException();
  }

  /**
   * Recognizes primative tokens that are also used by autodoc.
   * Makes one-character tokens out of primative SYMBOL tokens.
   * @return
   */
  private boolean findSimpleToken() {
    if (primativeToken.is(Token.Type.EOF) || primativeToken.is(Token.Type.EOL)
        || primativeToken.is(Token.Type.WHITESPACE)) {
      token.copy(primativeToken);
    }
    else if (primativeToken.equals(Token.Type.SYMBOL, COMMENT_CHAR)) {
      token.set(Token.Type.COMMENT, COMMENT_CHAR);
    }
    else if (primativeToken.equals(Token.Type.SYMBOL, SEPARATOR_CHAR)) {
      token.set(Token.Type.SEPARATOR, SEPARATOR_CHAR);
    }
    else if (primativeToken.equals(Token.Type.SYMBOL, OPEN_CHAR)) {
      token.set(Token.Type.OPEN, OPEN_CHAR);
    }
    else if (primativeToken.equals(Token.Type.SYMBOL, CLOSE_CHAR)) {
      token.set(Token.Type.CLOSE, CLOSE_CHAR);
    }
    else if (primativeToken.equals(Token.Type.SYMBOL, BREAK_CHAR)) {
      token.set(Token.Type.BREAK, BREAK_CHAR);
    }
    else if (primativeToken.equals(Token.Type.SYMBOL, delimiterString)) {
      //Found a one character DELIMITER.
      token.set(Token.Type.DELIMITER, delimiterString);
    }
    else {
      return false;
    }
    return true;
  }

  /**
   * Tries to build a multi-character delimiter
   * Assumes that findSimpleToken() has already been called
   * The delimiter string can only contain symbols that are not found by
   * findSimpleToken().
   * @return
   * @throws IOException
   */
  private boolean findDelimiter() throws IOException {
    int length = delimiterString.length();
    int index = 0;
    StringBuffer delimiterBuffer = null;
    boolean success = false;
    char symbol = primativeToken.getChar();
    //attempt to build a delimiter that matches delimiterString
    while (!success && primativeToken.is(Token.Type.SYMBOL) && index < length
        && delimiterString.charAt(index) == symbol) {
      if (delimiterBuffer == null) {
        delimiterBuffer = new StringBuffer();
      }
      delimiterBuffer.append(symbol);
      if (index == length - 1) {
        //found the whole delimiterString - succeed
        success = true;
      }
      else {
        //haven't matched the entire delimiter string - get next primative token
        index++;
        primativeToken = primativeTokenizer.next();
      }
      symbol = primativeToken.getChar();
    }
    if (success) {
      token.set(Token.Type.DELIMITER, delimiterBuffer);
      return true;
    }
    //delimiter match failed - build a word
    if (delimiterBuffer == null) {
      //never went into delimiter string recongnition loop - build a word from
      //the current primativeToken
      buildWord();
      primativeToken = primativeTokenizer.next();
    }
    else {
      buildWord(delimiterBuffer);
    }
    return false;
  }

  /**
   * Start or add to a word.
   */
  private void buildWord() {
    if (wordBuffer == null) {
      wordBuffer = new StringBuffer(primativeToken.getValue());
    }
    else {
      wordBuffer.append(primativeToken.getValue());
    }
  }

  /**
   * Start or add to a word
   */
  private void buildWord(StringBuffer buffer) {
    if (wordBuffer == null) {
      wordBuffer = new StringBuffer(buffer.toString());
    }
    else {
      wordBuffer.append(buffer);
    }
  }

  /**
   * Make a WORD token out of wordBuffer.
   * Calls findKeyword().
   */
  private void makeWord() {
    //The entire word was found - the current token will have to wait until
    //the next time next() is called.
    nextToken.copy(token);
    useNextToken = true;
    //Make the WORD token
    token.set(Token.Type.WORD, wordBuffer);
    wordBuffer = null;
    //Convert the token to a KEYWORD token if necessary
    findKeyword();
  }

  /**
   * Converts a WORD token to a KEYWORD token
   */
  private void findKeyword() {
    if (token.is(Token.Type.WORD)) {
      if (token.equals(VERSION_KEYWORD) || token.equals(PIP_KEYWORD)
          || token.equals(DELIMITER_KEYWORD)) {
        token.set(Token.Type.KEYWORD);
      }
    }
  }

}
