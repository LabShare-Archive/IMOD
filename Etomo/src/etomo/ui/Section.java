package etomo.ui;

import java.util.HashMap;
import java.util.Collection;
import java.util.Iterator;

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
* <p> $Revision 1.2  2004/01/01 00:46:28  sueh
* <p> $bug# 372 returning null instead of empty section when
* <p> $section is not found
* <p> $
* <p> $Revision 1.1  2003/12/31 01:30:21  sueh
* <p> $bug# storage for autodoc sections
* <p> $$ </p>
*/

public class Section implements AttributeCollection {
  public static final String rcsid = "$$Id$$";
  
  private String key = null; //required
  private Token type = null; //required
  private Token name = null; //required
  private HashMap attributeMap = null; //optional
  
  final static String getKey(Token type, Token name) {
    if (type == null && name == null) {
      return null;
    }
    if (type == null) {
      return name.getKey(true);
    }
    if (name == null) {
      return type.getKey(true);
    }
    return type.getKey(true) + name.getKey(true);
  }
  
  final static String getKey(String type, String name) {
    if (type == null && name == null) {
      return null;
    }
    if (type == null) {
      return Token.getKey(name);
    }
    if (name == null) {
      return Token.getKey(type);
    }
    return Token.getKey(type) + Token.getKey(name);
  }
  
  Section(Token type, Token name) {
    key = Section.getKey(type, name);
    this.type = type;
    this.name = name;
  }
  
  public AttributeCollection addAttribute(Token name) {
    if (attributeMap == null) {
      attributeMap = new HashMap();
    }
    Attribute existingAttribute = null;
    String key = Attribute.getKey(name);
    existingAttribute = (Attribute) attributeMap.get(key);
    if (existingAttribute == null) {
      Attribute newAttribute = new Attribute(name);
      attributeMap.put(key, newAttribute);
      return newAttribute;
    }
    return existingAttribute;
  }
  
  boolean equalsType(String type) {
    if (type == null) {
      return false;
    }
    return this.type.getKey(true).equals(Token.getKey(type));
  }

  String getKey() {
    return key;
  }
  
  String getName() {
    return name.getValue(true);
  }
  
  public int hashCode() {
    return key.hashCode();
  }
  
  public Attribute getAttribute(String name) {
    if (attributeMap == null) {
      return null;
    }
    String key = Attribute.getKey(name);
    Attribute attribute = (Attribute) attributeMap.get(key);
    return attribute;
  }

  void print() {
    Token token = null;
    System.out.print("Section: " + key + ":(");
    System.out.print(type.getValue(true));
    System.out.print(",");
    System.out.print(name.getValue(true));
    System.out.println(")");
    Attribute element = null;
    if (attributeMap != null) {
      Collection collection = attributeMap.values();
      Iterator iterator = collection.iterator();
      while (iterator.hasNext()) {
        element = (Attribute) iterator.next();
        element.print();
      }
    }
  }
}
