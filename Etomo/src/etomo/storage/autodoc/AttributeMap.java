package etomo.storage.autodoc;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import etomo.ui.Token;

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
final class AttributeMap {
  public static  final String  rcsid =  "$Id$";
  
  private final WriteOnlyAttributeMap parent;
  private final WriteOnlyNameValuePairList nameValuePairList;

  private HashMap attributeMap = null;
  private Vector attributeList = null;

  AttributeMap(WriteOnlyAttributeMap parent, WriteOnlyNameValuePairList nameValuePairList) {
    this.nameValuePairList = nameValuePairList;
    this.parent = parent;
  }

  WriteOnlyAttributeMap addAttribute(Token name) {
    Attribute attribute = null;
    if (attributeMap == null) {
      attributeMap = new HashMap();
      attributeList = new Vector();
    }
    String key = Attribute.getKey(name);
    attribute = (Attribute) attributeMap.get(key);
    if (attribute == null) {
      attribute = new Attribute(parent, nameValuePairList, name);
      attributeMap.put(key, attribute);
    }
    attributeList.add(attribute);
    return attribute;
  }

  Attribute getAttribute(int index) {
    if (attributeList == null || attributeList.size() <= index) {
      return null;
    }
    return (Attribute) attributeList.get(index);
  }

  Attribute getAttribute(String name) {
    if (attributeMap == null) {
      return null;
    }
    String key = Attribute.getKey(name);
    Attribute attribute = (Attribute) attributeMap.get(key);
    return attribute;
  }

  void print() {
    print(0);
  }

  void print(int level) {
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
* <p> Revision 1.1  2006/01/11 21:53:22  sueh
* <p> bug# 675 Replaced AttributeList with AttributeMap.  The sequential
* <p> functionality is taken care off by a Vector of NameValuePair's.
* <p> </p>
*/
/**
 * <p> Old Log: AttributeList.java
 * <p> Revision 1.3  2006/01/03 23:24:52  sueh
 * <p> bug# 675 Added getAttributeLocation(String) to get the first attribute with
 * <p> a specific name in a section.
 * <p>
 * <p> Revision 1.1  2005/12/23 02:10:48  sueh
 * <p> bug# 675 Encapsulated the list of attributes into AttributeList.  There is a
 * <p> list of attributes in three classes.  Added Vector storage,
 * <p> getAttributeLocation and nextAttribute to get an ordered list of attributes.
 * <p> Saving the first duplicate attribute instead of the last.
 * <p> </p>
 */