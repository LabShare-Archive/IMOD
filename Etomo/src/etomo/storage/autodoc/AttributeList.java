package etomo.storage.autodoc;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import etomo.ui.swing.Token;

/**
 * <p>Description: The Autodoc and each Section contain an AttributeList which holds
 * the attributes with the first attribute in each of their name/value pairs.
 * If the name of a name/value pair contains multiple attributes, then each
 * attribute, except for the last one, will also contain an AttributeList called
 * children.  So Autodocs and sections each contain a tree structure of Attributes.</p>
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
final class AttributeList implements ReadOnlyAttributeList {
  public static final String rcsid = "$Id$";

  private final WriteOnlyAttributeList parent;

  /**
   * map contains Attributes.  Each Attribute instance stands for 0 or more
   * occurrences of a name in this attribute list.  Attributes are never removed,
   * but the number of occurrences they contain can be reduced to 0.
   */
  private final Map<String, Attribute> map = new HashMap<String, Attribute>();
  private final List<Attribute> list = new ArrayList<Attribute>();

  AttributeList(WriteOnlyAttributeList parent) {
    this.parent = parent;
  }

  /**
   * Adds a new attribute, or increments an existing one
   * @param name
   * @return
   */
  WriteOnlyAttributeList addAttribute(Token name) {
    String key = Attribute.getKey(name);
    Attribute attribute = map.get(Attribute.getKey(name));
    if (attribute == null) {
      attribute = new Attribute(parent, name);
      map.put(key, attribute);
      list.add(attribute);
    }
    else {
      // add another occurrence of this attribute
      attribute.add();
    }
    return attribute;
  }

  Attribute getAttribute(String name) {
    if (map == null) {
      return null;
    }
    Attribute attribute = map.get(Attribute.getKey(name));
    if (attribute == null || !attribute.exists()) {
      // if !exists(), then all occurrences of this attribute have been removed
      return null;
    }
    return attribute;
  }

  /**
   * @return An iterator for the list of attributes.
   */
  public ReadOnlyAttributeIterator iterator() {
    return new ReadOnlyAttributeIterator(list);
  }

  /**
   * Returns the first attribute which exists.
   * @return
   */
  ReadOnlyAttribute getFirstAttribute() {
    for (int i = 0; i < list.size(); i++) {
      Attribute attribute = list.get(i);
      if (attribute.exists()) {
        return attribute;
      }
    }
    return null;
  }

  void print(int level) {
    if (map != null) {
      Attribute attribute = null;
      Collection collection = map.values();
      Iterator iterator = collection.iterator();
      if (iterator.hasNext()) {
        while (iterator.hasNext()) {
          // This bypasses the exists() check, but Attribute.print() also checks exists()
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
 * <p> Revision 1.3  2009/01/20 19:31:49  sueh
 * <p> bug# 1102 Added list and getFirstAttribute.
 * <p>
 * <p> Revision 1.2  2007/04/11 21:49:56  sueh
 * <p> bug# 964 Removed list because it was not being used.  In addAttribute, when an
 * <p> attribute already exists, increment Attribute.occurrences by calling
 * <p> Attribute.add().  In getAttribute, return null is Attribute.exists() returns false.
 * <p>
 * <p> Revision 1.1  2007/04/09 20:16:15  sueh
 * <p> bug# 964 Moved the value to the associated name/value pair.  Changed
 * <p> the Vector member variable from values to nameValuePairList.  Associated the
 * <p> last attribute in each name/value pair with the name value pair.  This is the
 * <p> attribute which used to contain the value.  The name/value pair also contained
 * <p> the value; so it was duplicated.  This made it difficult to add a value to an
 * <p> existing attribute.  GetValue() gets the value from the associated name/value
 * <p> pair.  Also removed the old nameValuePairList member variable, because it
 * <p> wasn't being used for anything.  Changed AttributeMap to AttributeList.
 * <p>
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
