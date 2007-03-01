package etomo.storage.autodoc;

import java.util.ArrayList;
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
public final class NameValuePair {
  public static final String rcsid = "$Id$";

  private final Vector names = new Vector();
  
  private  Token value=null;
  private boolean isPair=true;
  private boolean isSection = false;
  private boolean isComment=false;
  private boolean isEmptyLine=false;
  
  private Section section = null;
  private Token comment=null;
  
  static NameValuePair getNameValuePairInstance(Attribute attrib, Token value) {
    return new NameValuePair(attrib, value);
  }
  
  static NameValuePair getSubsectionInstance(Section subsection) {
    return new NameValuePair(subsection);
  }
  
  static NameValuePair getCommentInstance(Token comment) {
    return new NameValuePair(comment);
  }
  
  static NameValuePair getEmptyLineInstance() {
    return new NameValuePair();
  }

  private NameValuePair(Attribute attrib, Token value) {
    this.value = value;
    //set names
    ArrayList list = new ArrayList();
    while (attrib != null) {
      list.add(attrib.getNameToken());
      WriteOnlyAttributeMap parent = attrib.getParent();
      if (parent instanceof Attribute) {
        attrib = (Attribute) parent;
      }
      else {
        attrib = null;
      }
    }
    for (int i = list.size() - 1; i >= 0; i--) {
      names.add(list.get(i));
    }
  }

  private NameValuePair(Section section) {
    names.add(section.getTypeToken());
    value = section.getNameToken();
    isSection = true;
    this.section = section;
  }
  
  private NameValuePair(Token comment){
    isComment=true;
    this.comment=comment;
  }
  
  private NameValuePair(){
    isEmptyLine=true;
  }
  
  boolean isSection() {
    return isSection;
  }

  public int levels() {
    return names.size();
  }

  public boolean equalsName(String name, int index) {
    if (name == null || index >= names.size()) {
      return false;
    }
    String key = ((Token) names.get(index)).getKey();
    return key.equals(Token.convertToKey(name));
  }

  public String getName(int index) {
    if (index >= names.size()) {
      return null;
    }
    return ((Token) names.get(index)).getValues();
  }

  public String getValue() {
    if (value == null) {
      return null;
    }
    return value.getFormattedValues(false);
  }

  public String getString() {
    StringBuffer buffer = new StringBuffer();
    if (names.size() >= 1) {
      buffer.append(((Token) names.get(0)).getValues());
    }
    for (int i = 1; i < names.size(); i++) {
      buffer.append(AutodocTokenizer.SEPARATOR_CHAR);
      buffer.append(((Token) names.get(i)).getValues());
    }
    buffer.append(" " + AutodocTokenizer.DEFAULT_DELIMITER + " ");
    if (value != null) {
      buffer.append(value.getFormattedValues(false));
    }
    return buffer.toString();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "names=" + names + ",\nvalue=" + value + "," + super.toString();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2006/06/27 22:32:19  sueh
 * <p> bug# 852 Added isSection().
 * <p>
 * <p> Revision 1.2  2006/06/15 18:46:56  sueh
 * <p> bug# 852 Added NameValuePair(Section), so sub-sections can be stored in
 * <p> order.
 * <p>
 * <p> Revision 1.1  2006/01/12 17:03:00  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 1.1  2006/01/11 21:47:38  sueh
 * <p> bug# 675 Class to represent an attribute as a single line in an autodoc,
 * <p> rather then a tree of attributes.  This is useful for stepping through
 * <p> attributes in the same order as they are in the autodoc file.
 * <p> </p>
 */
