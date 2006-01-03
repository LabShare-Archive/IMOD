package etomo.ui;

import etomo.type.EtomoNumber;

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
* <p> $Revision 1.7  2005/12/23 02:08:37  sueh
* <p> $bug# 675 Encapsulated the list of attributes into AttributeList.  There is a
* <p> $list of attributes in three classes.
* <p> $
* <p> $Revision 1.6  2005/11/10 18:11:42  sueh
* <p> $bug# 733 Added getAttribute(int name), which translates name into a string
* <p> $and calls getAttribute(String).
* <p> $
* <p> $Revision 1.5  2005/09/21 16:22:56  sueh
* <p> $bug# 532 Added getUnformattedValue(EtomoNumber)
* <p> $
* <p> $Revision 1.4  2005/05/17 19:31:59  sueh
* <p> $bug# 372 Reducing the visibility of functions and member variables.
* <p> $Removing unused function getValue().
* <p> $
* <p> $Revision 1.3  2005/02/15 19:30:44  sueh
* <p> $bug# 602 Added getUnformattedValue() and getFormattedValue() to get the
* <p> $value either ignoring or using the BREAK and INDENT tokens.
* <p> $
* <p> $Revision 1.2  2004/01/01 00:42:45  sueh
* <p> $bug# 372 correcting interface
* <p> $
* <p> $Revision 1.1  2003/12/31 01:22:02  sueh
* <p> $bug# 372 holds attribute data
* <p> $$ </p>
*/

public class Attribute implements AttributeCollection {
  public static final String rcsid = "$$Id$$";
  
  private String key = null; //required
  private Token name = null; //required
  private Token value = null; //optional
  private AttributeList attributeList = null;//optional
  
  static String getKey(Token name) {
    if (name == null) {
      return null;
    }
    return name.getKey();
  }
 
  static String getKey(String name) {
    if (name == null) {
      return null;
    }
    return Token.convertToKey(name);
  }
  
  boolean equalsName(String name) {
    if (name == null) {
      return false;
    }
    return key.equals(Token.convertToKey(name));
  }
 
  Attribute(Token name) {
    this.name = name;
    key = name.getKey();
  }
  
  public AttributeCollection addAttribute(Token name) {
    if (attributeList == null) {
      attributeList = new AttributeList();
    }
    return attributeList.addAttribute(name);
  }
  
  void setValue(Token value) {
    this.value = value;
  }
  
  public Attribute getAttributeByIndex(int index) {
    if (attributeList == null) {
      return null;
    }
    return attributeList.getAttribute(index);
  }
  
  public Attribute getAttribute(int name) {
    if (attributeList == null) {
      return null;
    }
    return attributeList.getAttribute(String.valueOf(name));
  }

  public Attribute getAttribute(String name) {
    if (attributeList == null) {
      return null;
    }
    return attributeList.getAttribute(name);
  }

  final void print() {
    print(0);
  }
  
  final void print(int level) {
    Token token = null;
    if (level == 0) {
      System.out.print("Attribute: ");
    }
    else {
      for (int i = 0; i < level; i++) {
        System.out.print("  ");
      }
    }
    System.out.print(key + ":(");
    System.out.print(name.getValue(true));
    if (value != null) {
      System.out.print("," + value.getValue(true));
    }
    if (attributeList != null) {
      attributeList.print(level);
    }
    System.out.print(")");
  }

  String getKey() {
    return key;
  }
  
  String getName() {
    return name.getValue(true);
  }
  
  public void getUnformattedValue(EtomoNumber unformattedValue) {
    unformattedValue.set(getUnformattedValue());
  }
 
  public String getUnformattedValue() {
    if (value == null) {
      return null;
    }
    Token token = value;
    StringBuffer buffer = new StringBuffer();
    while (token != null) {
      if (!token.is(Token.BREAK) && !token.is(Token.INDENT)) {
        String tokenValue = token.getValue();
        if (tokenValue == null) {
          buffer.append(' ');
        }
        else {
          buffer.append(tokenValue);
        }
      }
      token = token.next();
    }
    return buffer.toString();
  }
  
  String getFormattedValue() {
    if (value == null) {
      return null;
    }
    Token token = value;
    StringBuffer buffer = new StringBuffer();
    while (token != null) {
      if (token.is(Token.BREAK)) {
        buffer.append("\n");
      }
      else {
        String tokenValue = token.getValue();
        if (tokenValue == null) {
          buffer.append(' ');
        }
        else {
          buffer.append(tokenValue);
        }
      }
      token = token.next();
    }
    return buffer.toString();
  }
 
  public int hashCode() {
    return key.hashCode();
  }
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return /*"key=" + key +*/ ",name=" + name + ",value="
        + value + ",\nattributeList=" + attributeList/* + ",\n" + super.toString()*/;
  }
}
