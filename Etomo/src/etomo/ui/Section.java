package etomo.ui;

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
* <p> $Revision 1.5  2005/12/23 02:20:08  sueh
* <p> $bug# 675 Renamed getFirstSectionLocation to getSectionLocation.
* <p> $Removed getSection(sectionLocation).  Changed nextSection so it gets the
* <p> $current section and increments.  Encapsulated the attribute list into
* <p> $AttributeList.  Added getAttributeLocation and nextAttribute.
* <p> $
* <p> $Revision 1.4  2005/12/01 00:25:16  sueh
* <p> $bug# 775 Made getName() public.
* <p> $
* <p> $Revision 1.3  2005/05/17 19:40:17  sueh
* <p> $bug# 372 Reducing the visibility of functions.
* <p> $
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
  private AttributeList attributeList = null;//optional
  
  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "key=" + key + ",type=" + type + ",name="
        + name + ",\nattributeList=" + attributeList + ",\n" + super.toString();
  }
  
  final static String getKey(Token type, Token name) {
    if (type == null && name == null) {
      return null;
    }
    if (type == null) {
      return name.getKey();
    }
    if (name == null) {
      return type.getKey();
    }
    return type.getKey() + name.getKey();
  }
  
  final static String getKey(String type, String name) {
    if (type == null && name == null) {
      return null;
    }
    if (type == null) {
      return Token.convertToKey(name);
    }
    if (name == null) {
      return Token.convertToKey(type);
    }
    return Token.convertToKey(type) + Token.convertToKey(name);
  }
  
  Section(Token type, Token name) {
    key = Section.getKey(type, name);
    this.type = type;
    this.name = name;
  }
  
  public AttributeCollection addAttribute(Token name) {
    if (attributeList == null) {
      attributeList = new AttributeList();
    }
    return attributeList.addAttribute(name);
  }
  
  boolean equalsType(String type) {
    if (type == null) {
      return false;
    }
    return this.type.getKey().equals(Token.convertToKey(type));
  }

  String getKey() {
    return key;
  }
  
  public final String getName() {
    if (name == null) {
      throw new IllegalStateException("name is required");
    }
    return name.getValue(true);
  }
  
  public int hashCode() {
    return key.hashCode();
  }
  
  public Attribute getAttribute(String name) {
    if (attributeList == null) {
      return null;
    }
    return attributeList.getAttribute(name);
  }
  
  public final AttributeLocation getAttributeLocation() {
    if (attributeList == null) {
      return null;
    }
    return attributeList.getAttributeLocation();
  }
  
  public final AttributeLocation getAttributeLocation(String name) {
    if (attributeList == null) {
      return null;
    }
    return attributeList.getAttributeLocation(name);
  }
  
  public final Attribute nextAttribute(AttributeLocation location) {
    if (attributeList == null) {
      return null;
    }
    return attributeList.nextAttribute(location);
  }

  void print() {
    Token token = null;
    System.out.print("Section: " + key + ":(");
    System.out.print(type.getValue(true));
    System.out.print(",");
    System.out.print(name.getValue(true));
    System.out.println(")");
    Attribute element = null;
    if (attributeList != null) {
      attributeList.print();
    }
  }
}
