package etomo.ui;

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
public class Token {
  public static final String rcsid = "$$Id$$";
  
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

  int type = NULL;
  String value = null;
  String comparableValue = null;
  
  public Token() {
    
  }
  
  public Token(Token token) {
    set(token);
  }
  
  public final int getType() {
    return type;
  }
  
  public final String typeToString() {
    return typeToString(type);
  }
  
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
  
  public final void reset() {
    type = NULL;
    value = null;
    comparableValue = null;
  }
  
  public final void set(Token token) {
    type = token.type;
    if (token.value == null) {
      value = null;
      comparableValue = null;
    }
    else {
      value = new String(token.value);
      comparableValue = new String(token.comparableValue);
    }

  }

  public final void set(int type, String value) {
    this.type = type;
    set(value);
  }
  
  public final void set(int type, char cValue) {
    this.type = type;
    set(new StringBuffer().append(cValue).toString());
  }
  
  public final void set(int type, StringBuffer valueBuffer) {
    set(type, valueBuffer.toString());
  }
  
  public final void set(int type) {
    this.type = type;
    set();
  }
  
  public final void set(String value) {
    this.value = new String(value);
    comparableValue = this.value.toLowerCase();
    set();
  }
  
  private void set() {
    if (type == NULL || type == EOF || type == EOL) {
      value = null;
      comparableValue = null;
    }
  }
  
  public final boolean is(int type) {
    return this.type == type;
  }
  
  public final boolean equals(Token token) {
    return type == token.type && valueEquals(token.value);
  }
  
  public final boolean equals(int type, String value) {
    return this.type == type && valueEquals(value);
  }
  
  public final boolean valueEquals(String value) {
    if (this.value == value) {
      return true;
    }
    if (this.value == null || value == null) {
      return false;
    }
    if (comparableValue.equals(value.toLowerCase())) {
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
  
}
