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
final class AttributeList {
  public static final String rcsid = "$Id$";

  private HashMap attributeMap = null;
  private Vector attributeList = null;

  final AttributeCollection addAttribute(Token name) {
    Attribute attribute = null;
    if (attributeMap == null) {
      attributeMap = new HashMap();
      attributeList = new Vector();
    }
    String key = Attribute.getKey(name);
    attribute = (Attribute) attributeMap.get(key);
    if (attribute == null) {
      attribute = new Attribute(name);
      attributeMap.put(key, attribute);
    }
    attributeList.add(attribute);
    return attribute;
  }

  final Attribute getAttribute(int index) {
    if (attributeList == null || attributeList.size() <= index) {
      return null;
    }
    return (Attribute) attributeList.get(index);
  }

  final Attribute getAttribute(String name) {
    if (attributeMap == null) {
      return null;
    }
    String key = Attribute.getKey(name);
    Attribute attribute = (Attribute) attributeMap.get(key);
    return attribute;
  }

  final AttributeLocation getAttributeLocation(String name) {
    if (attributeList == null || attributeList.size() == 0) {
      return null;
    }
    Attribute attrib = null;
    for (int i = 0; i < attributeList.size(); i++) {
      attrib = (Attribute) attributeList.get(i);
      if (attrib.equalsName(name)) {
        return new AttributeLocation(name, i);
      }
    }
    return null;
  }

  final AttributeLocation getAttributeLocation() {
    if (attributeList == null || attributeList.size() == 0) {
      return null;
    }
    Attribute attrib = null;
    return new AttributeLocation(0);
  }

  final Attribute nextAttribute(AttributeLocation location) {
    if (location == null) {
      return null;
    }
    Attribute attrib = null;
    for (int i = location.getIndex(); i < attributeList.size(); i++) {
      attrib = (Attribute) attributeList.get(i);
      String name = location.getName();
      if (name == null || attrib.equalsName(name)) {
        location.setIndex(i + 1);
        return attrib;
      }
    }
    return null;
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
          attribute.print(level + 1);
        }
      }
    }
    else {
      System.out.println();
    }
  }
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "attributeMap=" + attributeMap/* + ",\nattributeList=" + attributeList
        + ",\n" + super.toString()*/;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2005/12/23 02:10:48  sueh
 * <p> bug# 675 Encapsulated the list of attributes into AttributeList.  There is a
 * <p> list of attributes in three classes.  Added Vector storage,
 * <p> getAttributeLocation and nextAttribute to get an ordered list of attributes.
 * <p> Saving the first duplicate attribute instead of the last.
 * <p> </p>
 */