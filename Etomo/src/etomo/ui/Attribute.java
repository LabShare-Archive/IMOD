package etomo.ui;

import java.util.Vector;

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
* <p> $Revision 1.8  2006/01/03 23:23:26  sueh
* <p> $bug# 675 Added equalsName().
* <p> $
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

public class Attribute implements WriteOnlyAttributeMap {
  public static final String rcsid = "$$Id$$";
  
  private final WriteOnlyAttributeMap parent;
  private final WriteOnlyNameValuePairList nameValuePairList;
  private final Token name;
  
  private String key = null; //required

  private Vector values = null; //optional
  private AttributeMap attributeMap = null;//optional
  
  Attribute(WriteOnlyAttributeMap parent, WriteOnlyNameValuePairList nameValuePairList, Token name) {
    this.parent = parent;
    this.nameValuePairList = nameValuePairList;
    this.name = name;
    key = name.getKey();
  }
  
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
  
  public WriteOnlyAttributeMap addAttribute(Token name) {
    if (attributeMap == null) {
      attributeMap = new AttributeMap(this, nameValuePairList);
    }
    return attributeMap.addAttribute(name);
  }
  
  synchronized void setValue(Token value) {
    if (values == null) {
      values = new Vector();
    }
    values.add(value);
    //add the full name (name1.name2.name3...) and value to a sequential list
    nameValuePairList.addNameValuePair(this, values.size() - 1);
  }
  
  public Attribute getAttributeByIndex(int index) {
    if (attributeMap == null) {
      return null;
    }
    return attributeMap.getAttribute(index);
  }
  
  public Attribute getAttribute(int name) {
    if (attributeMap == null) {
      return null;
    }
    return attributeMap.getAttribute(String.valueOf(name));
  }

  public Attribute getAttribute(String name) {
    if (attributeMap == null) {
      return null;
    }
    return attributeMap.getAttribute(name);
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
    System.out.print(name.getValues());
    if (values != null) {
      for (int i = 0; i < values.size(); i++) {
        System.out.print("," + ((Token) values.get(i)).getValues());
      }
    }
    if (attributeMap != null) {
      attributeMap.print(level);
    }
    System.out.print(")");
  }

  final String getKey() {
    return key;
  }
  
  final WriteOnlyAttributeMap getParent() {
    return parent;
  }
  
  final Token getNameToken() {
    return name;
  }
  
  final String getName() {
    return name.getValues();
  }
  
  public final void getValue(EtomoNumber value) {
    value.set(getValue());
  }
  
  final Token getValueToken(int index) {
    if (values == null) {
      return null;
    }
    return (Token) values.get(index);
  }
 
  public final String getValue() {
    if (values == null || values.size() == 0) {
      return null;
    }
    Token value = (Token) values.get(0);
    if (value == null) {
      return null;
    }
    return value.getFormattedValues(false);
  }
  
  String getFormattedValue() {
    if (values == null) {
      return null;
    }
    Token token = (Token) values.get(0);
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
    return /*"key=" + key +*/ ",name=" + name + ",\nvalues="
        + values + ",\nattributeMap=" + attributeMap/* + ",\n" + super.toString()*/;
  }
}
