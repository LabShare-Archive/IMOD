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
* <p> $$Log$$ </p>
*/

public class Section implements AttributeInterface {
  public static final String rcsid = "$$Id$$";
  
  private String key = null;
  private Token type = null;
  private Token name = null;
  private HashMap attributeMap = null;
  
  public final static String getKey(Token type, Token name) {
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
  
  public final static String getKey(String type, String name) {
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
  
  public Section() {
  }
  
  public Section(Token type, Token name) {
    key = Section.getKey(type, name);
    this.type = type;
    this.name = name;
  }
  
  public AttributeInterface addAttribute(Token name) {
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
  
  public boolean equalsType(String type) {
    if (this.type == null || type == null) {
      return false;
    }
    return this.type.getKey(true).equals(Token.getKey(type));
  }

  public boolean isNull() {
    return key == null;
  }

  public String getKey() {
    return key;
  }
  
  public String getName() {
    if (name == null) {
      return null;
    }
    return name.getValue(true);
  }
  
  public int hashCode() {
    if (key == null) {
      return new String().hashCode();
    }
    return key.hashCode();
  }
  
  public Attribute getAttribute(String name) {
    if (attributeMap == null) {
      return new Attribute();
    }
    String key = Attribute.getKey(name);
    Attribute attribute = (Attribute) attributeMap.get(key);
    if (attribute == null) {
      return new Attribute();
    }
    return attribute;
  }

  public void print() {
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
