package etomo.ui;

/**
* <p>Description:
* A class to encapsulate a token type and value.  Provides tools for comparing
* tokens, saving tokens with a key, and for values made of multiple tokens.
* 
* Token type:
* Token types are integers. Use typeToString() to get name of the type.
* Unknown token types are allowed.  See Possible Upgrades.
* 
* Token value:
* Tokens with a NULL, EOF, and EOL type always have a null token value.
* 
* Comparing tokens:
* Tokens can be compared with an is(int type) function and various equals()
* functions.  All token value comparisons are done using the result of the
* static function getKey(String value).
* 
* Saving tokens with a key:
* The getKey() functions are public and can be used to create standard keys for
* saving and retrieving tokens.
* 
* Values made up of multiple tokens:
* Link list:
* This class can be used to make a linked list of tokens.  The next token can be
* set.  A token can be removed from the list (see dropFromList()).  The next
* token can be retrieved.
* Values:
* Values and keys made of multiple tokens can be retrieved.  See
* getValue(boolean includeNext) and getKey(boolean includeNext).  When
* retrieving a string made of multiple tokens, one space with be appended to the
* string for each null value.
* 
* Inheritance:
* This class is not designed to be inherited.
* 
* 
* 
* Possible Upgrades:
* This class could be upgraded to allow the addition or an
* Vector of new token types.  The existing token numbers are all negative, so
* they wouldn't have to change.
* 
* New fields:
* private Vector typeList = null;
* 
* Function changes:
* Token(Vector)
* typeToString()
* set(Token)
* 
* Drawbacks:
* The static function typeToString(int type) could not return the new types.
*
* </p>
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
* <p> $Revision 1.3  2003/12/31 01:32:01  sueh
* <p> $bug# 372 added doc, added link list connectors
* <p> $
* <p> $Revision 1.2  2003/12/23 21:34:43  sueh
* <p> $bug# 372 Reformatting.
* <p> $
* <p> $Revision 1.1  2003/12/22 23:51:49  sueh
* <p> $bug# 372 encapsulates token type and value.  Handles
* <p> $comparisons.
* <p> $$ </p>
*/
public class Token {
  public static final String rcsid =
    "$$Id$$";

  public static final int NULL = -1;
  public static final int EOF = -2;
  public static final int EOL = -3;
  public static final int ALPHANUM = -4;
  public static final int SYMBOL = -5;
  public static final int WHITESPACE = -6;
  public static final int COMMENT = -7;
  public static final int SEPARATOR = -8;
  public static final int OPEN = -9;
  public static final int CLOSE = -10;
  public static final int DELIMITER = -11;
  public static final int WORD = -12;
  public static final int KEYWORD = -13;

  private int type = NULL;
  private String value = null;
  private String key = null;

  private Token next = null;
  private Token previous = null;

  /**
   * Converts a string into a standard key for saving and retrieving tokens.
   * This function is unnecessary when using Token.equals() functions because it
   * is used internally.
   * 
   * Converts values to lower case.
   * 
   * @param token value
   * @return a key
   */
  public static final String getKey(String value) {
    return value.toLowerCase();
  }

  public Token() {

  }

  /**
   * Makes a deep copy of a token.
   * 
   * @param token
   */
  public Token(Token token) {
    set(token);
  }

  public final int getType() {
    return type;
  }

  /**
   * @return the name of the type
   */
  public final String typeToString() {
    return typeToString(type);
  }

  /**
   * Static function that converts and integer into the name of the 
   * corresponding type.
   * 
   * @param type
   * @return
   */
  public static final String typeToString(int type) {
    if (type == NULL) {
      return "NULL";
    }
    else if (type == EOF) {
      return "EOF";
    }
    else if (type == EOL) {
      return "EOL";
    }
    else if (type == ALPHANUM) {
      return "ALPHANUM";
    }
    else if (type == SYMBOL) {
      return "SYMBOL";
    }
    else if (type == WHITESPACE) {
      return "WHITESPACE";
    }
    else if (type == COMMENT) {
      return "COMMENT";
    }
    else if (type == SEPARATOR) {
      return "SEPARATOR";
    }
    else if (type == OPEN) {
      return "OPEN";
    }
    else if (type == CLOSE) {
      return "CLOSE";
    }
    else if (type == DELIMITER) {
      return "DELIMITER";
    }
    else if (type == WORD) {
      return "WORD";
    }
    else if (type == KEYWORD) {
      return "KEYWORD";
    }
    return "UNKNOWN";
  }

  public final String getValue() {
    return value;
  }

  /**
   * When includeNext is true, returns a string containing all values in the
   * token link list concatenated together.  Null values are converted to ' '.
   * 
   * @param includeNext
   * @return
   */
  public final String getValue(boolean includeNext) {
    if (!includeNext || next == null) {
      return value;
    }
    StringBuffer buffer = new StringBuffer(value);
    Token token = this.next;
    while (token != null) {
      if (token.value == null) {
        buffer.append(' ');
      }
      else {
        buffer.append(token.value);
      }
      token = token.next;
    }
    return buffer.toString();
  }

  /**
   * @return the standard key associated with the value of this token.
   */
  public final String getKey() {
    return key;
  }

  /**
   * When includeNext is true, returns a string containing all keys in the token
   * link list concatenated together.  Null keys are converted to ' '.
   * 
   * @param includeNext
   * @return
   */
  public final String getKey(boolean includeNext) {
    if (!includeNext || next == null) {
      return key;
    }
    StringBuffer buffer = new StringBuffer(key);
    Token token = this.next;
    while (token != null) {
      if (token.key == null) {
        buffer.append(' ');
      }
      else {
        buffer.append(token.key);
      }
      token = token.next;
    }
    return buffer.toString();
  }

  /**
   * Sets the type of the token to NULL.
   */
  public final void reset() {
    type = NULL;
    value = null;
    key = null;
  }

  /**
   * Makes a deep copy of a token.
   * @param token
   */
  public final void set(Token token) {
    type = token.type;
    if (token.value == null) {
      value = null;
      key = null;
    }
    else {
      value = new String(token.value);
      key = new String(token.key);
    }

  }

  /**
   * Sets the type and value of the token.
   * @param type
   * @param value
   */
  public final void set(int type, String value) {
    this.type = type;
    set(value);
  }

  /**
   * Sets the type and value of the token.
   * @param type
   * @param value
   */
  public final void set(int type, char value) {
    this.type = type;
    set(new StringBuffer().append(value).toString());
  }

  /**
   * Sets the type and value of the token.
   * @param type
   * @param valueBuffer
   */
  public final void set(int type, StringBuffer valueBuffer) {
    set(type, valueBuffer.toString());
  }

  /**
   * Sets the type of the token.  Does not reset the value, unless the type is
   * NULL, EOF, or EOL.
   * @param type
   */
  public final void set(int type) {
    this.type = type;
    set();
  }

  /**
   * Sets the value of the token.
   * @param value
   */
  public final void set(String value) {
    this.value = new String(value);
    key = getKey(this.value);
    set();
  }

  private void set() {
    if (type == NULL || type == EOF || type == EOL) {
      value = null;
      key = null;
    }
  }

  /**
   * @param type
   * @return true if type equals the token type
   */
  public final boolean is(int type) {
    return this.type == type;
  }

  /**
   * @param token
   * @return true if the type and key of this token are the same
   */
  public final boolean equals(Token token) {
    return type == token.type && equals(token.value);
  }

  /**
   * @return true if the type and key of this token are the same as the type and
   * getKey(value).
   */
  public final boolean equals(int type, String value) {
    return this.type == type && equals(value);
  }

  /**
   * @return true if the type and key of this token are the same as the type and
   * getKey(value).
   */
  public final boolean equals(int type, char value) {
    if (this.type != type) {
      return false;
    }
    if (this.value == null) {
      return false;
    }
    return key.length() == 1 && key.charAt(0) == value;
  }

  /**
  * @return true if the key of this token are the same as getKey(value).
  */
  public final boolean equals(String value) {
    if (this.value == value) {
      return true;
    }
    if (this.value == null || value == null) {
      return false;
    }
    if (key.equals(getKey(value))) {
      return true;
    }
    return false;
  }

  public final String toString() {
    if (value == null) {
      return "(" + typeToString() + ")";
    }
    return "(" + typeToString() + "," + value + ")";
  }

  /**
   * Sets the next token in the link list.
   * @param token
   * @return the next token
   */
  public final Token setNext(Token token) {
    next = token;
    if (token != null) {
      token.previous = this;
    }
    return token;
  }

  /**
   * @return the next token
   */
  public final Token next() {
    return next;
  }

  /**
   * Drops the token from the link list.  
   * @return The previous token on the list, if it exists.  If not, returns the
   * next token on the list.
   */
  public final Token dropFromList() {
    Token list = null;
    if (previous == null) {
      list = next;
    }
    else {
      list = previous;
    }
    if (previous != null) {
      previous.next = next;
    }
    if (next != null) {
      next.previous = previous;
    }
    previous = null;
    next = null;
    return list;
  }
}
