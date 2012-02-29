package etomo.util;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.io.StringReader; //import java.lang.IllegalArgumentException;
import java.io.IOException; //import java.lang.IllegalStateException;
import java.io.StreamTokenizer;
import java.io.FileNotFoundException;

import etomo.storage.LogFile;
import etomo.ui.swing.Token;

/**
 * <p>Description:
 * Creates the following primative tokens: EOL, EOF, ALPHANUM (the largest
 * possible alphanumeric string), SYMBOL (a character matching one of the
 * following: !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~).  Everything else is called
 * WHITESPACE and returned as the largest possible string.
 * 
 * To Use:
 * construct with a file.
 * call initialize().
 * call next() to get the next token, until the end of file is reached.
 * 
 * Testing:
 * Do not call initialize() when testing.
 * Call test() to test this class.
 * Call testStreamTokenizer() to test the StreamTokenizer.
 * 
 * 
 * Possible Upgrades:
 * The StreamTokenizer could be initialized differently and/or the set of symbols
 * could be overridden.
 * 
 * New functions:
 * initialize(StreamTokenizer)
 * initialize(StreamTokenizer, String symbols)
 * initialize(String symbols)
 * 
 * </p>
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
 * <p> $Revision 1.15  2010/11/13 16:08:59  sueh
 * <p> $bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p> $
 * <p> $Revision 1.14  2009/09/05 00:06:14  sueh
 * <p> $bug# 1256 Added separateAlphabeticAndNumeric() to divide ALPHANUM
 * <p> $tokens into ALPHABETIC and NUMERIC tokens without changing the next()
 * <p> $function.
 * <p> $
 * <p> $Revision 1.13  2009/02/04 23:38:24  sueh
 * <p> $bug# 1158 Changed id and exception classes in LogFile.
 * <p> $
 * <p> $Revision 1.12  2007/06/07 21:34:12  sueh
 * <p> $bug# 1012 Passing debug in constructor.  In initializeStreamTokenizer
 * <p> $catching FileNotFoundException so that the file can be closed.
 * <p> $
 * <p> $Revision 1.11  2007/05/14 22:29:26  sueh
 * <p> $bug# 964 Handling \r\n.
 * <p> $
 * <p> $Revision 1.10  2007/05/14 22:26:59  sueh
 * <p> $bug# 964 Handling \r\n.
 * <p> $
 * <p> $Revision 1.9  2007/05/14 17:35:34  sueh
 * <p> $bug# 964 The "\r" is not being recognized as part of the EOL by Java
 * <p> $StreamTokenizer.  Alternatively the peetPrm.adoc has line endings with
 * <p> $"\r\r\n" when in Reggae, but that doesn't seem to be true.  Handing this
 * <p> $by rolling any "\r" found before an EOL token into the EOL token.
 * <p> $
 * <p> $Revision 1.8  2007/04/09 22:01:10  sueh
 * <p> $bug# 964 InitializeStreamTokenizer:  handling a null string be creating a string
 * <p> $reader on an empty string.
 * <p> $
 * <p> $Revision 1.7  2007/04/09 21:26:07  sueh
 * <p> $bug# 964 Made class final.
 * <p> $
 * <p> $Revision 1.6  2007/03/23 20:45:55  sueh
 * <p> $bug# 964 In initializeStreamTokenizer:  handling NullPointerException.
 * <p> $
 * <p> $Revision 1.5  2007/03/08 22:06:35  sueh
 * <p> $bug# 964 Improved the StreamTokenizer test.  Prevent infinite loop by checking
 * <p> $for the private StreamTokenizer.TT_NOTHING value.
 * <p> $
 * <p> $Revision 1.4  2007/03/01 01:47:50  sueh
 * <p> $bug# 964 Using LogFile instead of file, since some autodoc will be writeable.
 * <p> $
 * <p> $Revision 1.3  2006/06/14 00:45:24  sueh
 * <p> $bug# 852 Renamed Token.set(Token) copy() to make it clear that it is doing a
 * <p> $deep copy.
 * <p> $
 * <p> $Revision 1.2  2006/05/01 21:22:20  sueh
 * <p> $bug# 854
 * <p> $
 * <p> $Revision 1.1  2006/04/06 20:34:40  sueh
 * <p> $Moved PrimativeTokenizer to util.
 * <p> $
 * <p> $Revision 1.5  2006/01/12 17:18:26  sueh
 * <p> $bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p> $
 * <p> $Revision 1.4  2006/01/11 22:25:28  sueh
 * <p> $bug# 675 Added the ability to tokenize a String.
 * <p> $
 * <p> $Revision 1.3  2003/12/31 01:29:48  sueh
 * <p> $bug# 372 added doc
 * <p> $
 * <p> $Revision 1.2  2003/12/23 21:34:17  sueh
 * <p> $bug# 372 Reformatting.  Fixing test function.
 * <p> $
 * <p> $Revision 1.1  2003/12/22 23:50:52  sueh
 * <p> $bug# 372 creates primative tokens
 * <p> $$ </p>
 */
public final class PrimativeTokenizer {
  public static final String rcsid = "$$Id$$";

  private static final String RETURN = "\r";

  private final boolean separateAlphabeticAndNumeric;

  private LogFile file = null;
  private LogFile.ReadingId readingId = null;
  private String string = null;
  private StreamTokenizer tokenizer = null;
  private String symbols = new String("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~");
  private String digits = new String("0123456789");
  private String letters = new String(
      "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
  private Token token = new Token();
  private boolean nextTokenFound = false;
  private Token nextToken = new Token();
  private Reader reader = null;
  private boolean fileClosed = false;
  private int streamTokenizerNothingValue;
  private boolean debug = false;
  private String valueBeingBrokenUp = null;
  private int valueIndex = -1;

  public PrimativeTokenizer(LogFile file, boolean debug) {
    this.file = file;
    this.debug = debug;
    separateAlphabeticAndNumeric = false;
  }

  public PrimativeTokenizer(String string) {
    this.string = string;
    separateAlphabeticAndNumeric = false;
  }

  public static PrimativeTokenizer getNumericInstance(String string) {
    return new PrimativeTokenizer(string, true);
  }

  private PrimativeTokenizer(String string, boolean separateAlphabeticAndNumeric) {
    this.string = string;
    this.separateAlphabeticAndNumeric = separateAlphabeticAndNumeric;
  }

  /**
   * @return the current token
   */
  public Token getToken() {
    return token;
  }

  /**
   * Must be called before the first call to next().
   * 
   * @throws FileNotFoundException
   * @throws IOException
   */
  public void initialize() throws FileNotFoundException, IOException,
      LogFile.LockException {
    initializeStreamTokenizer();
    nextToken();
  }

  private void initializeStreamTokenizer() throws FileNotFoundException,
      LogFile.LockException {
    try {
      if (file != null) {
        File readingFile = new File(file.getAbsolutePath());
        readingId = file.openForReading();
        reader = new FileReader(readingFile);
      }
      else if (string != null) {
        reader = new StringReader(string);
      }
      else {
        reader = new StringReader("");
      }
      tokenizer = new StreamTokenizer(reader);
      streamTokenizerNothingValue = tokenizer.ttype;
      tokenizer.resetSyntax();
      tokenizer.wordChars('a', 'z');
      tokenizer.wordChars('A', 'Z');
      tokenizer.wordChars('0', '9');
      tokenizer.eolIsSignificant(true);
    }
    catch (FileNotFoundException e) {
      if (readingId != null && !readingId.isEmpty()) {
        file.closeRead(readingId);
      }
      throw e;
    }
  }

  /**
   * Tokenizes a file.
   * @return the next token found.
   * @throws IOException
   */
  public Token next() throws IOException {
    if (valueBeingBrokenUp == null) {
      if (fileClosed) {
        return token;
      }
      boolean found = false;
      token.reset();
      boolean whitespaceFound = false;
      StringBuffer whitespaceBuffer = null;
      boolean returnFound = false;

      if (nextTokenFound) {
        found = true;
        token.copy(nextToken);
        nextToken.reset();
        nextTokenFound = false;
        nextToken();
      }
      while (!found) {
        if (tokenizer.ttype == StreamTokenizer.TT_EOF) {
          token.set(Token.Type.EOF);
          if (debug) {
            System.out.println(token);
          }
          found = true;
          returnFound = false;
          if (file != null) {
            closeFile();
          }
        }
        else if (tokenizer.ttype == StreamTokenizer.TT_EOL) {
          token.set(Token.Type.EOL);
          if (debug) {
            System.out.println(token);
          }
          found = true;
          //If "\r" was found before an EOL, roll it into the EOL.  This is
          //necessary because StreamTokenizer is not working according to its
          //definition:  It is supposed to return both "\n" and "\r\n" as EOL, but
          //it does not do this for "\r\n".  If this bug is fixed, then "\r\r\n"
          //will appear as an EOL, but this is OK because this is character string
          //that usually means that there was an error in transfering the file
          //between Windows and Linux.
          if (returnFound && whitespaceFound) {
            returnFound = false;
            whitespaceBuffer.deleteCharAt(whitespaceBuffer.length() - 1);
            if (whitespaceBuffer.length() == 0) {
              whitespaceFound = false;
              whitespaceBuffer = null;
            }
          }
        }
        else if (tokenizer.ttype == StreamTokenizer.TT_WORD) {
          token.set(Token.Type.ALPHANUM, tokenizer.sval);
          if (debug && !separateAlphabeticAndNumeric) {
            System.out.println(token);
          }
          found = true;
          returnFound = false;
        }
        else if (symbols.indexOf(tokenizer.ttype) != -1) {
          token.set(Token.Type.SYMBOL, (char) tokenizer.ttype);
          if (debug) {
            System.out.println(token);
          }
          found = true;
          returnFound = false;
        }
        else {
          if (RETURN.indexOf(tokenizer.ttype) != -1) {
            returnFound = true;
          }
          else {
            returnFound = false;
          }
          if (!whitespaceFound) {
            whitespaceFound = true;
            whitespaceBuffer = new StringBuffer().append((char) tokenizer.ttype);
          }
          else {
            whitespaceBuffer.append((char) tokenizer.ttype);
          }
        }
        if (found) {
          if (whitespaceFound) {
            nextTokenFound = true;
            nextToken.copy(token);
            token.set(Token.Type.WHITESPACE, whitespaceBuffer);
            if (debug) {
              System.out.println(token);
            }
            whitespaceBuffer = null;
            whitespaceFound = false;
          }
        }
        if (!nextTokenFound) {
          nextToken();
        }
      }
    }
    if (separateAlphabeticAndNumeric) {
      token = separateAlphabeticAndNumeric(token);
    }
    return token;
  }

  /**
   * Take an ALPHANUM token and break it up.  Called multiple times.
   * ValueBeingBrokenUp and valueIndex are preserve outside the function and
   * managed inside the function.
   * @return
   */
  private Token separateAlphabeticAndNumeric(Token token) {
    if (valueBeingBrokenUp == null && !token.is(Token.Type.ALPHANUM)) {
      //Only ALPHANUM tokens are broken up.
      return token;
    }
    if (valueBeingBrokenUp == null) {
      //Start breaking up a token.
      valueBeingBrokenUp = token.getValue();
      valueIndex = 0;
    }
    //Grab an as large as possible NUMERIC or ALPHABETIC token starting at
    //valueIndex.
    Token newToken = new Token();
    for (int i = valueIndex; i < valueBeingBrokenUp.length(); i++) {
      char ch = valueBeingBrokenUp.charAt(i);
      if (digits.indexOf(ch) != -1) {
        //A NUMERIC token.
        if (newToken.is(Token.Type.NULL)) {
          //Start the NUMERIC token.
          newToken.set(Token.Type.NUMERIC);
        }
        else if (newToken.is(Token.Type.ALPHABETIC)) {
          //Found the end of the NUMERIC token.  Add the value to the new token
          //and return.
          newToken.set(valueBeingBrokenUp.substring(valueIndex, i));
          if (debug) {
            System.out.println(newToken);
          }
          return newToken;
        }
      }
      else if (letters.indexOf(ch) != -1) {
        //An ALPHABETIC token
        if (newToken.is(Token.Type.NULL)) {
          //Start the ALPHABETIC token.
          newToken.set(Token.Type.ALPHABETIC);
        }
        else if (newToken.is(Token.Type.NUMERIC)) {
          //Found the end of the ALPHABETIC token.  Add the value to the new
          //token and return.
          newToken.set(valueBeingBrokenUp.substring(valueIndex, i));
          if (debug) {
            System.out.println(newToken);
          }
          return newToken;
        }
      }
    }
    //Found the end of the last token in valueBeingBrokenUp.  Add the value to
    //the new token and reset valueBeingBrokenUp and valueIndex.
    newToken.set(valueBeingBrokenUp.substring(valueIndex, valueBeingBrokenUp.length()));
    if (debug) {
      System.out.println(newToken);
    }
    valueBeingBrokenUp = null;
    valueIndex = -1;
    return newToken;
  }

  public String getSymbols() {
    return symbols;
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  /**
   * Tests this object.  Prints result to System.out.
   * @param tokens If true, prints each token.  If false, prints the text.
   * @throws IOException
   */
  public void test(boolean tokens) throws IOException, LogFile.LockException {
    initialize();
    Token token;
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

  /**
   * Tests the StreamTokenizer.  Prints result to System.out.
   * @param tokens If true, prints each token.  If false, prints the text.
   * @throws IOException
   */
  public void testStreamTokenizer(boolean tokens, boolean details) throws IOException,
      LogFile.LockException {
    initializeStreamTokenizer();
    do {
      nextToken();
      if (tokens) {
        System.out.print(tokenizer.toString());
        if (details) {
          TokenType tokenType = TokenType.getInstance(tokenizer.ttype);
          if (tokenType == null) {
            System.out.println(", " + ((char) tokenizer.ttype) + ",sval="
                + tokenizer.sval + ",nval=" + tokenizer.nval);
          }
          else {
            System.out.println(", " + TokenType.getInstance(tokenizer.ttype) + ",sval="
                + tokenizer.sval + ",nval=" + tokenizer.nval);
          }
        }
        else {
          System.out.println();
        }
      }
      else {
        if (tokenizer.ttype == StreamTokenizer.TT_EOL) {
          System.out.println();
        }
        else if (tokenizer.ttype != StreamTokenizer.TT_EOF) {
          if (tokenizer.ttype == StreamTokenizer.TT_WORD) {
            System.out.print(tokenizer.sval);
          }
          else {
            System.out.print((char) tokenizer.ttype);
          }
        }
      }
    } while (tokenizer.ttype != StreamTokenizer.TT_EOF
        && tokenizer.ttype != streamTokenizerNothingValue);
    if (file != null) {
      closeFile();
    }
    System.out.println();
  }

  private void nextToken() throws IOException {
    if (file != null && fileClosed) {
      return;
    }
    tokenizer.nextToken();
  }

  private void closeFile() throws IOException {
    if (file != null && readingId != null && !readingId.isEmpty()) {
      fileClosed = true;
      reader.close();
      file.closeRead(readingId);
      readingId = null;
    }
  }

  private static final class TokenType {
    private final String name;

    private static final TokenType EOF = new TokenType("TT_EOF");
    private static final TokenType EOL = new TokenType("TT_EOL");
    private static final TokenType NUMBER = new TokenType("TT_NUMBER");
    private static final TokenType WORD = new TokenType("TT_WORD");
    private static final TokenType NOTHING = new TokenType("TT_NOTHING");

    private TokenType(String name) {
      this.name = name;
    }

    private static TokenType getInstance(int ttype) {
      switch (ttype) {
      case StreamTokenizer.TT_EOF:
        return EOF;
      case StreamTokenizer.TT_EOL:
        return EOL;
      case StreamTokenizer.TT_NUMBER:
        return NUMBER;
      case StreamTokenizer.TT_WORD:
        return WORD;
      case -4://StreamTokenizer.TT_NOTHING
        return NOTHING;
      default:
        return null;
      }
    }

    public String toString() {
      return name;
    }
  }
}
