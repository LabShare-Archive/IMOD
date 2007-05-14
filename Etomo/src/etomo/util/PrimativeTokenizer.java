package etomo.util;

import java.io.File;
import java.io.FileReader;
import java.io.Reader;
import java.io.StringReader;
//import java.lang.IllegalArgumentException;
import java.io.IOException;
//import java.lang.IllegalStateException;
import java.io.StreamTokenizer;
import java.io.FileNotFoundException;

import etomo.storage.LogFile;
import etomo.ui.Token;

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

  private LogFile file = null;
  private long readId = LogFile.NO_ID;
  private String string = null;
  private StreamTokenizer tokenizer = null;
  private String symbols = new String("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~");
  private Token token = new Token();
  private boolean nextTokenFound = false;
  private Token nextToken = new Token();
  private Reader reader = null;
  private boolean fileClosed = false;
  private int streamTokenizerNothingValue;
  private boolean debug = false;

  public PrimativeTokenizer(LogFile file) {
    this.file = file;
  }

  public PrimativeTokenizer(String string) {
    this.string = string;
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
      LogFile.ReadException {
    initializeStreamTokenizer();
    nextToken();
  }

  private void initializeStreamTokenizer() throws FileNotFoundException,
      LogFile.ReadException {
    if (file != null) {
      File readingFile = new File(file.getAbsolutePath());
      readId = file.openForReading();
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

  /**
   * Tokenizes a file.
   * @return the next token found.
   * @throws IOException
   */
  public Token next() throws IOException {
    if (fileClosed) {
      return token;
    }
    boolean found = false;
    token.reset();
    boolean whitespaceFound = false;
    boolean returnFound = false;
    boolean prevReturnFound = false;
    StringBuffer whitespaceBuffer = null;

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
        found = true;
        if (file != null) {
          closeFile();
        }
      }
      else if (tokenizer.ttype == StreamTokenizer.TT_EOL) {
        token.set(Token.Type.EOL);
        found = true;
        //If "\r" was found before an EOL, roll it into the EOL.  This is
        //necessary because StreamTokenizer is not working according to its
        //definition:  It is supposed to return both "\n" and "\r\n" as EOL, but
        //it is only doing this for "\n".  If this bug is fixed, then "\r\r\n"
        //will appear as an EOL, but this is OK because this is character string
        //usually means that there was an error in transfering the file because
        //Windows and Linux.
        if (prevReturnFound) {
          prevReturnFound=false;
        }
      }
      else if (tokenizer.ttype == StreamTokenizer.TT_WORD) {
        token.set(Token.Type.ALPHANUM, tokenizer.sval);
        found = true;
      }
      else if (symbols.indexOf(tokenizer.ttype) != -1) {
        token.set(Token.Type.SYMBOL, (char) tokenizer.ttype);
        found = true;
      }
      else {
        //Find out if the current character is "\r".
        if (RETURN.indexOf(tokenizer.ttype) != -1) {
          returnFound = true;
        }
        if (!whitespaceFound) {
          whitespaceFound = true;
          whitespaceBuffer = new StringBuffer();
        }
        //If the previous character was "\r", add it to the whitespace, if it
        //wasn't rolled into the EOL token.
        if (prevReturnFound) {
          whitespaceBuffer.append(RETURN);
        }
        //Don't add the "\r" character to the whitespace until we can see 
        //whether the next token is an EOL.
        if (!returnFound) {
          whitespaceBuffer.append((char) tokenizer.ttype);
        }
      }
      if (found) {
        if (whitespaceFound) {
          nextTokenFound = true;
          nextToken.copy(token);
          token.set(Token.Type.WHITESPACE, whitespaceBuffer);
          whitespaceBuffer = null;
          whitespaceFound = false;
        }
      }
      if (!nextTokenFound) {
        nextToken();
      }
      //Remember whether the current character was "\r".
      prevReturnFound = returnFound;
    }
    if (debug) {
      System.out.println("token=" + token);
    }
    return token;
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
  public void test(boolean tokens) throws IOException, LogFile.ReadException {
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
  public void testStreamTokenizer(boolean tokens, boolean details)
      throws IOException, LogFile.ReadException {
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
            System.out.println(", " + TokenType.getInstance(tokenizer.ttype)
                + ",sval=" + tokenizer.sval + ",nval=" + tokenizer.nval);
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
    if (file != null) {
      fileClosed = true;
      reader.close();
      file.closeForReading(readId);
      readId = LogFile.NO_ID;
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
