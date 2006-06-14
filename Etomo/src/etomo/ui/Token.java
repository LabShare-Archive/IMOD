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
 * <p> $Revision 1.10  2006/05/01 21:19:02  sueh
 * <p> $bug# 854
 * <p> $
 * <p> $Revision 1.9  2006/01/12 17:20:14  sueh
 * <p> $bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p> $
 * <p> $Revision 1.8  2006/01/11 21:44:03  sueh
 * <p> $bug# 675 Moved value formatting to Token.  Use
 * <p> $Token.getFormattedValues(boolean format) to get a value that has
 * <p> $formatting strings.  If format is true then use the formatting strings,
 * <p> $otherwise string them.
 * <p> $
 * <p> $Revision 1.7  2006/01/03 23:59:21  sueh
 * <p> $bug# 675 Added information to toString().  Renamed the original toString()
 * <p> $to getString().
 * <p> $
 * <p> $Revision 1.6  2005/12/23 02:23:48  sueh
 * <p> $bug# 675 Changed static getKey(String) to convertToKey() for clarity.
 * <p> $Removed unnecessary parameter String includeNext in getKey().
 * <p> $
 * <p> $Revision 1.5  2005/02/15 20:41:08  sueh
 * <p> $bug# 602 Added BREAKK and INDENT types.  Added length(), which
 * <p> $returns the length of the value.  Added numberOf(char searchChar, int
 * <p> $fromIndex), which returns the number of contiguous searchChars starting
 * <p> $from fromIndex in the value.  Added split(), which splits the token into
 * <p> $two tokens.
 * <p> $
 * <p> $Revision 1.4  2003/12/31 17:48:42  sueh
 * <p> $bug# 372 change doc
 * <p> $
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
public final class Token {
  public static final String rcsid = "$$Id$$";
  
  private Type type = Type.NULL;
  private String value = null;
  private String key = null;

  private Token next = null;
  private Token previous = null;
  private boolean active = true;

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "type=" + type + ",value=" + value /*+ ",key="
     + key + ",\nnext=" + next + ",\nprevious="
     + previous + ",\n" + super.toString()*/;
  }

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
  public static String convertToKey(String value) {
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
    copy(token);
  }

  public Type getType() {
    return type;
  }

  public String getValue() {
    return value;
  }

  public char getChar() {
    if (value == null) {
      return ' ';
    }
    return value.charAt(0);
  }

  /**
   * Returns a string containing all values in the
   * token link list concatenated together.  Null values are converted to ' '.
   * 
   * @param includeNext
   * @return
   */
  public String getValues() {
    Token token = this;
    StringBuffer buffer = new StringBuffer();
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
   * Returns a string containing all values in the
   * token link list concatenated together.  Null values are converted to ' '.
   * Formats with break and indent if format is true, otherwise ignores them.
   * @param format
   * @return
   */
  public String getFormattedValues(boolean format) {
    Token token = this;
    StringBuffer buffer = new StringBuffer();
    boolean breakFound = false;
    while (token != null) {
      //treat inactive BREAK tokens as regular characters; they have no effect on the format
      if (token.is(Type.BREAK) && token.active) {
        //use break only when formatting
        if (format) {
          buffer.append("\n");
          breakFound = true;
        }
      }
      else if (token.is(Type.WHITESPACE)) {
        if (token.next != null && token.next.is(Type.BREAK)
            && token.next.active) {
          //ignore whitespace preceding break whether formatting or not
        }
        else if (breakFound) {
          breakFound = false;
          //use whitespace after break only when formatting
          if (format) {
            buffer.append(token.value);
          }
        }
        else {
          buffer.append(token.value);
        }
      }
      else {
        breakFound = false;
        if (token.value == null) {
          buffer.append(' ');
        }
        else {
          buffer.append(token.value);
        }
      }
      token = token.next;
    }
    return buffer.toString();
  }

  /**
   * Returns a string containing all keys in the token
   * link list concatenated together Null keys are converted to ' '.
   * 
   * @param includeNext
   * @return
   */
  public String getKey() {
    StringBuffer buffer = new StringBuffer();
    if (key == null) {
      buffer.append(' ');
    }
    else {
      buffer.append(key);
    }
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
   * Finds the number of contiguous searchChars, start from fromIndex.
   * @param searchChar
   * @param fromIndex
   * @return
   */
  public int numberOf(char searchChar, int fromIndex) {
    if (value == null) {
      System.out.println("valid is null");
      return 0;
    }
    boolean found = false;
    int numberFound = 0;
    for (int i = 0; i < value.length(); i++) {
      if (!found) {
        if (value.charAt(i) == searchChar) {
          found = true;
          numberFound++;
        }
      }
      else {
        if (value.charAt(i) == searchChar) {
          numberFound++;
        }
        else {
          return numberFound;
        }
      }
    }
    return numberFound;
  }

  /**
   * Returns the number of characters in the value.
   * @return
   */
  public int length() {
    if (value == null) {
      return 0;
    }
    return value.length();
  }

  /**
   * Split off a new token from this token.  Set the new token type to the type
   * parameter.  Set the new token value to a substring of the value in this
   * token, starting from startIndex and going for size characters.  Remove this substring from
   * this token.
   * @param type
   * @param startIndex
   * @param size
   */
  public Token split(Type type, int startIndex, int size) {
    if (value == null || value.length() <= startIndex + size) {
      throw new IndexOutOfBoundsException("startIndex + size, " + startIndex
          + size + ", must be less then value.length," + value.length() + ".");
    }
    Token newToken = new Token();
    newToken.set(type, value.substring(startIndex, startIndex + size));
    value = value.substring(size);
    if (next != null || previous != null) {
      newToken.next = this;
      if (previous != null) {
        newToken.previous = previous;
      }
      previous = newToken;
    }
    return newToken;
  }

  /**
   * Sets the type of the token to NULL.
   */
  public void reset() {
    type = Type.NULL;
    active = true;
    value = null;
    key = null;
  }

  /**
   * Makes a deep copy of a token.
   * @param token
   */
  public void copy(Token token) {
    type = token.type;
    active = token.active;
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
  public void set(Type type, String value) {
    setType(type);
    set(value);
  }

  /**
   * Sets the type and value of the token.
   * @param type
   * @param value
   */
  public void set(Type type, char value) {
    setType(type);
    set(new StringBuffer().append(value).toString());
  }

  /**
   * Sets the type and value of the token.
   * @param type
   * @param valueBuffer
   */
  public void set(Type type, StringBuffer valueBuffer) {
    set(type, valueBuffer.toString());
  }

  /**
   * Sets the type of the token.  Does not reset the value, unless the type is
   * NULL, EOF, or EOL.
   * @param type
   */
  public void set(Type type) {
    setType(type);
    if (type == Type.NULL || type == Type.EOF || type == Type.EOL) {
      value = null;
      key = null;
    }
  }

  /**
   * Sets the value of the token.
   * @param value
   */
  public void set(String value) {
    if (type == Type.NULL || type == Type.EOF || type == Type.EOL) {
      value = null;
      key = null;
    }
    else {
      this.value = new String(value);
      key = convertToKey(this.value);
    }
  }

  private void setType(Type type) {
    this.type = type;
    if (type == Type.BREAK) {
      active = false;
    }
  }
  
  public void setActive(boolean active) {
    this.active = active;
  }

  /**
   * @param type
   * @return true if type equals the token type
   */
  public boolean is(Type type) {
    return this.type == type;
  }

  /**
   * @param token
   * @return true if the type and key of this token are the same
   */
  public boolean equals(Token token) {
    return type == token.type && equals(token.value);
  }

  /**
   * @return true if the type and key of this token are the same as the type and
   * getKey(value).
   */
  public boolean equals(Type type, String value) {
    return this.type == type && equals(value);
  }

  /**
   * @return true if the type and key of this token are the same as the type and
   * getKey(value).
   */
  public boolean equals(Type type, char value) {
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
  public boolean equals(String value) {
    if (this.value == value) {
      return true;
    }
    if (this.value == null || value == null) {
      return false;
    }
    if (key.equals(convertToKey(value))) {
      return true;
    }
    return false;
  }

  public String getString() {
    if (value == null) {
      return "(" + type + ")";
    }
    return "(" + type + "," + value + ")";
  }

  /**
   * Sets the next token in the link list.
   * @param token
   * @return the next token
   */
  public Token setNext(Token token) {
    if (token == this) {
      return token;
    }
    next = token;
    if (token != null) {
      token.previous = this;
    }
    return token;
  }

  /**
   * @return the next token
   */
  public Token next() {
    return next;
  }

  /**
   * Drops the token from the link list.  
   * @return The previous token on the list, if it exists.  If not, returns the
   * next token on the list.
   */
  public Token dropFromList() {
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
  
  public static final class Type {
    public static final Type NULL = new Type();
    public static final Type EOF = new Type();
    public static final Type EOL = new Type();
    public static final Type ALPHANUM = new Type();
    public static final Type SYMBOL = new Type();
    public static final Type WHITESPACE = new Type();
    public static final Type COMMENT = new Type();
    public static final Type SEPARATOR = new Type();
    public static final Type OPEN = new Type();
    public static final Type CLOSE = new Type();
    public static final Type DELIMITER = new Type();
    public static final Type WORD = new Type();
    public static final Type KEYWORD = new Type();
    public static final Type BREAK = new Type();
    
    public String toString() {
   
      if (this == NULL) {
        return "NULL";
      }
      else if (this == EOF) {
        return "EOF";
      }
      else if (this == EOL) {
        return "EOL";
      }
      else if (this == ALPHANUM) {
        return "ALPHANUM";
      }
      else if (this == SYMBOL) {
        return "SYMBOL";
      }
      else if (this == WHITESPACE) {
        return "WHITESPACE";
      }
      else if (this == COMMENT) {
        return "COMMENT";
      }
      else if (this == SEPARATOR) {
        return "SEPARATOR";
      }
      else if (this == OPEN) {
        return "OPEN";
      }
      else if (this == CLOSE) {
        return "CLOSE";
      }
      else if (this == DELIMITER) {
        return "DELIMITER";
      }
      else if (this == WORD) {
        return "WORD";
      }
      else if (this == KEYWORD) {
        return "KEYWORD";
      }
      else if (this == BREAK) {
        return "BREAK";
      }
      return "UNKNOWN";
    }
  }
}
