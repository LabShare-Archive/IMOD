package etomo.ui;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class AttributeList {
  public static  final String  rcsid =  "$Id$";
  
  private HashMap attributeMap = null;
  private Vector attributeList = null;
  
  public AttributeCollection addAttribute(Token name) {
    if (attributeMap == null) {
      attributeMap = new HashMap();
      attributeList = new Vector();
    }
    Attribute existingAttribute = null;
    String key = Attribute.getKey(name);
    existingAttribute = (Attribute) attributeMap.get(key);
    Attribute newAttribute = new Attribute(name);
    if (!attributeMap.containsKey(key)) {
      attributeMap.put(key, newAttribute);
    }
    attributeList.add(newAttribute);
    return newAttribute;
  }
  
  public Attribute getAttributeByIndex(int index) {
    if (attributeList == null || attributeList.size() <= index) {
      return null;
    }
    return (Attribute) attributeList.get(index);
  }
  
  public Attribute getAttribute(int name) {
    return getAttribute(String.valueOf(name));
  }
  
  public Attribute getAttribute(String name) {
    if (attributeMap == null) {
      return null;
    }
    String key = Attribute.getKey(name);
    Attribute attribute = (Attribute) attributeMap.get(key);
    return attribute;
  }
  
  public final AttributeLocation getAttributeLocation() {
    if (attributeList == null || attributeList.size() == 0) {
      return null;
    }
    Attribute attrib = null;
    return new AttributeLocation(0);
  }
  
  public final Attribute nextAttribute(AttributeLocation location) {
    if (location == null || attributeList == null) {
      return null;
    }
    int index = location.getIndex();
    if (index >= attributeList.size()) {
      return null;
    }
    location.increment();
    return (Attribute) attributeList.get(index);
  }
  
  final void print() {
    print(0);
  }
  
  final void print(int level) {
    System.out.print(")");
    if (attributeMap != null) {
      Attribute attribute = null;
      Collection collection = attributeMap.values();
      Iterator iterator = collection.iterator();
      if (iterator.hasNext()) {
        System.out.println(":");
        while (iterator.hasNext()) {
          attribute = (Attribute) iterator.next();
          attribute.print(level+1);
        }
      }
    }
    else {
      System.out.println();
    }
  }
  
  protected String paramString() {
    StringBuffer buffer = new StringBuffer();
    if (attributeMap != null) {
      buffer.append(attributeMap.toString());
    }
    return buffer.toString();
  }
}
/**
* <p> $Log$ </p>
*/