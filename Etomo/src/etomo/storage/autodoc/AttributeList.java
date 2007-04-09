package etomo.storage.autodoc;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import etomo.ui.Token;

/**
* <p>Description: A list of Attribute instances.  Each attribute contains an AttributeList
* instance called children. Each Autodoc and Section instance can also contain
* an AttributeList instance.</p>
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
  public static  final String  rcsid =  "$Id$";
  
  private final WriteOnlyAttributeList parent;
  //private final WriteOnlyNameValuePairList nameValuePairList;

  private Map map = null;
  private List list = null;

  AttributeList(WriteOnlyAttributeList parent/*, WriteOnlyNameValuePairList nameValuePairList*/) {
    //this.nameValuePairList = nameValuePairList;
    this.parent = parent;
  }

  WriteOnlyAttributeList addAttribute(Token name) {
    Attribute attribute = null;
    if (map == null) {
      map = new HashMap();
      list = new Vector();
    }
    String key = Attribute.getKey(name);
    attribute = (Attribute) map.get(key);
    if (attribute == null) {
      attribute = new Attribute(parent, /*nameValuePairList, */name);
      map.put(key, attribute);
    }
    list.add(attribute);
    return attribute;
  }

  Attribute getAttribute(int index) {
    if (list == null || list.size() <= index) {
      return null;
    }
    return (Attribute) list.get(index);
  }

  Attribute getAttribute(String name) {
    if (map == null) {
      return null;
    }
    String key = Attribute.getKey(name);
    Attribute attribute = (Attribute) map.get(key);
    return attribute;
  }

  void print(int level) {
    if (map != null) {
      Attribute attribute = null;
      Collection collection = map.values();
      Iterator iterator = collection.iterator();
      if (iterator.hasNext()) {
        while (iterator.hasNext()) {
          attribute = (Attribute) iterator.next();
          attribute.print(level);
        }
      }
    }
  }
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "map=" + map;
  }
}
/**
* <p> $Log$
* <p> Revision 1.2  2007/03/07 21:04:52  sueh
* <p> bug# 964 Fixed printing.
* <p>
* <p> Revision 1.1  2006/01/12 17:01:34  sueh
* <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
* <p>
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