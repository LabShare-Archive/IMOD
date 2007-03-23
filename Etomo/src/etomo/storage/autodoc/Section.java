package etomo.storage.autodoc;

import java.util.Vector;

import etomo.storage.LogFile;
import etomo.ui.Token;

/**
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright 2002 - 2006</p>
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
 * <p> $Revision 1.13  2007/03/21 19:41:40  sueh
 * <p> $bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> $
 * <p> $Revision 1.12  2007/03/15 21:46:59  sueh
 * <p> $bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
 * <p> $unless the Attribute needs to be modified.
 * <p> $
 * <p> $Revision 1.11  2007/03/08 22:02:03  sueh
 * <p> $bug# 964 Save name/value pairs in the parser instead of saving them from the
 * <p> $Attribute.  This is necessary because the name/value pair must be placed in the
 * <p> $autodoc or section as soon as they are found to preserve the original order of the
 * <p> $autodoc file.
 * <p> $
 * <p> $Revision 1.10  2007/03/07 21:07:15  sueh
 * <p> $bug# 964 Fixed printing.
 * <p> $
 * <p> $Revision 1.9  2007/03/01 01:20:38  sueh
 * <p> $bug# 964 Added addComment and addEmptyLine.
 * <p> $
 * <p> $Revision 1.8  2006/06/21 17:40:20  sueh
 * <p> $bug# 852 Returning Section from addSection().
 * <p> $
 * <p> $Revision 1.7  2006/06/16 17:48:25  sueh
 * <p> $bug# 852 Added addSection().
 * <p> $
 * <p> $Revision 1.6  2006/06/15 18:47:27  sueh
 * <p> $bug# 852 Added getTypeToken() and getNameToken().
 * <p> $
 * <p> $Revision 1.5  2006/06/14 21:24:32  sueh
 * <p> $bug# 852 Added isAttribute().
 * <p> $
 * <p> $Revision 1.4  2006/06/14 00:33:58  sueh
 * <p> $bug# 852 Added function isGlobal so that it is possible to tell whether an attribute
 * <p> $is global or part of a section.
 * <p> $
 * <p> $Revision 1.3  2006/05/01 21:17:36  sueh
 * <p> $bug# 854
 * <p> $
 * <p> $Revision 1.2  2006/04/25 18:55:17  sueh
 * <p> $bug# 787 Implemented ReadOnlyNameValuePairList so that name/value
 * <p> $pairs can be read from either a global or section area using the same
 * <p> $code.
 * <p> $
 * <p> $Revision 1.1  2006/01/12 17:03:24  sueh
 * <p> $bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p> $
 * <p> $Revision 1.7  2006/01/11 22:38:40  sueh
 * <p> $bug# 675 Implementing WriteOnlyAttributeMap and
 * <p> $WriteOnlyNameValuePairList
 * <p> $
 * <p> $Revision 1.6  2006/01/03 23:45:32  sueh
 * <p> $bug# 675 Added getAttributeLocation(String).
 * <p> $
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

final class Section extends WriteOnlyNameValuePairList implements
    ReadOnlySection {
  public static final String rcsid = "$$Id$$";

  private final Vector nameValuePairList = new Vector();

  private String key = null; //required
  private Token type = null; //required
  private Token name = null; //required
  private AttributeMap attributeMap = null;//optional
  private boolean subsection = false;
  private final WriteOnlyNameValuePairList parent;

  boolean isGlobal() {
    return false;
  }

  boolean isAttribute() {
    return false;
  }
  
  void setCurrentDelimiter(Token newDelimiter) {
    parent.setCurrentDelimiter(newDelimiter);
  }
  
  String getCurrentDelimiter() {
    return parent.getCurrentDelimiter();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "key=" + key + ",type=" + type + ",name=" + name
        + ",\nattributeMap=" + attributeMap + ",\n" + super.toString();
  }

  public String getString() {
    return AutodocTokenizer.OPEN_CHAR + type.getValues() + ' '
        + AutodocTokenizer.DEFAULT_DELIMITER + ' ' + name.getValues()
        + AutodocTokenizer.CLOSE_CHAR;
  }

  public static String getKey(Token type, Token name) {
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

  public static String getKey(String type, String name) {
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

  Section(Token type, Token name,WriteOnlyNameValuePairList parent) {
    key = Section.getKey(type, name);
    this.type = type;
    this.name = name;
    this.parent=parent;
  }

  WriteOnlyAttributeMap addAttribute(Token name) {
    if (attributeMap == null) {
      attributeMap = new AttributeMap(this, this);
    }
    return attributeMap.addAttribute(name);
  }

  boolean equalsType(String type) {
    if (type == null) {
      return false;
    }
    return this.type.getKey().equals(Token.convertToKey(type));
  }

  Token getTypeToken() {
    if (type == null) {
      throw new IllegalStateException("type is required");
    }
    return type;
  }

  String getKey() {
    return key;
  }

  public String getName() {
    if (name == null) {
      throw new IllegalStateException("name is required");
    }
    return name.getValues();
  }

  Token getNameToken() {
    if (name == null) {
      throw new IllegalStateException("name is required");
    }
    return name;
  }

  public int hashCode() {
    return key.hashCode();
  }

  public ReadOnlyAttribute getAttribute(String name) {
    if (attributeMap == null) {
      return null;
    }
    return attributeMap.getAttribute(name);
  }

  void write(LogFile file, long writeId) throws LogFile.WriteException {
    //write section header
    file.write(AutodocTokenizer.OPEN_CHAR, writeId);
    if (subsection) {
      file.write(AutodocTokenizer.OPEN_CHAR, writeId);
    }
    type.write(file, writeId);
    file.write(' ' + parent.getCurrentDelimiter() + ' ', writeId);
    name.write(file, writeId);
    file.write(AutodocTokenizer.CLOSE_CHAR, writeId);
    if (subsection) {
      file.write(AutodocTokenizer.CLOSE_CHAR, writeId);
    }
    file.newLine(writeId);
    for (int i = 0; i < nameValuePairList.size(); i++) {
      ((NameValuePair) nameValuePairList.get(i)).write(file, writeId);
    }
    //if subsection, write subsection footer
    if (subsection) {
      file.write(""+AutodocTokenizer.OPEN_CHAR + AutodocTokenizer.OPEN_CHAR
      + AutodocTokenizer.CLOSE_CHAR + AutodocTokenizer.CLOSE_CHAR, writeId);
      file.newLine(writeId);
    }
  }

  NameValuePair addNameValuePair() {
    NameValuePair pair = NameValuePair.getNameValuePairInstance(this);
    nameValuePairList.add(pair);
    return pair;
  }

  Section addSection(Token type, Token name) {
    Section section = new Section(type, name,this);
    section.subsection = true;
    nameValuePairList.add(NameValuePair.getSubsectionInstance(section,this));
    return section;
  }

  void addComment(Token comment) {
    nameValuePairList.add(NameValuePair.getCommentInstance(comment,this));
  }
  
  void addDelimiterChange(Token newDelimiter) {
    nameValuePairList.add(NameValuePair.getDelimiterChangeInstance(newDelimiter,this));
  }

  void addEmptyLine() {
    nameValuePairList.add(NameValuePair.getEmptyLineInstance(this));
  }

  public NameValuePairLocation getNameValuePairLocation() {
    return new NameValuePairLocation();
  }

  public NameValuePair nextNameValuePair(NameValuePairLocation location) {
    if (location == null || location.isOutOfRange(nameValuePairList)) {
      return null;
    }
    NameValuePair pair = (NameValuePair) nameValuePairList.get(location
        .getIndex());
    location.increment();
    return pair;
  }

  void print(int level) {
    if (level > 0) {
      Autodoc.printIndent(level);
      System.out.print("[");
    }
    else {
      System.out.println();
    }
    System.out.print("[" + type.getValues() + " = " + name.getValues() + "]");
    if (level > 0) {
      System.out.println("]");
    }
    else {
      System.out.println();
    }
    //name value pair list
    Autodoc.printIndent(level);
    System.out.println("LIST:");
    if (nameValuePairList != null) {
      NameValuePair nameValuePair = null;
      for (int i = 0; i < nameValuePairList.size(); i++) {
        nameValuePair = (NameValuePair) nameValuePairList.get(i);
        nameValuePair.print(level);
      }
    }
    Autodoc.printIndent(level);
    System.out.println("MAP:");
    if (attributeMap != null) {
      attributeMap.print(level);
    }
  }
}