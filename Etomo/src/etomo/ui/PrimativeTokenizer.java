package etomo.ui;

import java.io.File;
import java.io.FileReader;
//import java.lang.IllegalArgumentException;
import java.io.IOException;
//import java.lang.IllegalStateException;
import java.io.StreamTokenizer;
import java.io.FileNotFoundException;


/**
* <p>Description:</p>
*
* <p>Copyright: Copyright © 2002, 2003</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* Univeristy of Colorado</p>
*
* @author $$Author$$
*
* @version $$Revision$$
*
* <p> $$Log$$ </p>
*/
public class PrimativeTokenizer {
  public static final String rcsid = "$$Id$$";
  
  private File file;
  private StreamTokenizer tokenizer = null;
  public static final String SYMBOLS = new String("!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~");
  Token token = new Token();
  boolean nextTokenFound = false;
  Token nextToken = new Token();

  PrimativeTokenizer(File file) {
    this.file = file;
  }
    
  public final Token getToken() {
    return token;
  }
    
  public void initialize() throws FileNotFoundException, IOException {
    tokenizer = null;
    FileReader reader = new FileReader(file);
    tokenizer = new StreamTokenizer(reader);
    tokenizer.resetSyntax();
    tokenizer.wordChars('a', 'z');
    tokenizer.wordChars('A', 'Z');
    tokenizer.wordChars('0', '9');
    tokenizer.eolIsSignificant(true);
    tokenizer.nextToken();
  }

  public Token next() throws IOException {
    //System.out.println("in Primative.next");
    boolean found = false;
    token.reset();
    boolean whitespaceFound = false;
    StringBuffer whitespaceBuffer = null;
    
    if (nextTokenFound) {
      found = true;
      token.set(nextToken);
      nextToken.reset();
      nextTokenFound = false;
      tokenizer.nextToken();
    }
    while (!found) {
      if (tokenizer.ttype == StreamTokenizer.TT_EOF) {
        token.set(Token.EOF);
        found = true;
      }
      else if (tokenizer.ttype == StreamTokenizer.TT_EOL) {
        token.set(Token.EOL);
        found = true;
      }
      else if (tokenizer.ttype == StreamTokenizer.TT_WORD) {
        token.set(Token.ALPHANUM, tokenizer.sval);
        found = true;
      }
      else if (SYMBOLS.indexOf(tokenizer.ttype) != -1) {
        token.set(Token.SYMBOL, (char) tokenizer.ttype);
        found = true;
      }
      else {
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
          nextToken.set(token);
          token.set(Token.WHITESPACE, whitespaceBuffer);
          whitespaceBuffer = null;
          whitespaceFound = false;
        }
      }
      if (!nextTokenFound) {     
        tokenizer.nextToken();
      }
    }
    return token;
  }
  
  public void testStreamTokenizer(boolean tokens) throws IOException {
    initialize();
    System.out.println("in Primative.testStreamTokenizer");
    if (tokens) {
      System.out.println("TT_EOL=" + StreamTokenizer.TT_EOL + ",TT_WORD=" + StreamTokenizer.TT_WORD + ",TT_NUMBER=" + StreamTokenizer.TT_NUMBER);
    }
    while (tokenizer.ttype != StreamTokenizer.TT_EOF) {
      if (tokens) {
        System.out.println("ttype=" + tokenizer.ttype + ",sval=" + tokenizer.sval + ",nval=" + tokenizer.nval);
      }
      if (tokenizer.ttype == StreamTokenizer.TT_EOL) {
        if (!tokens) {
          System.out.println();
        }
      }
      else {
        if (tokenizer.ttype == StreamTokenizer.TT_WORD) {
          if (!tokens) {
            System.out.print(tokenizer.sval);
          }
        }
        else {
          if (!tokens) {
            System.out.print((char) tokenizer.ttype);
          }
        }
      }
      tokenizer.nextToken();
    }
  }

}
