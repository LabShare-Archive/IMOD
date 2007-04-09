package etomo.storage.autodoc;

import java.util.Vector;

import etomo.storage.LogFile;
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
final class NameValuePair extends Statement {
  public static final String rcsid = "$Id$";

  private static final Type TYPE = Statement.Type.NAME_VALUE_PAIR;

  /**
   * The name is made of attributes
   */
  private final Vector name = new Vector();
  private final WriteOnlyStatementList parent;

  private Token value = null;
  private Token newDelimiter = null;

  public NameValuePair(WriteOnlyStatementList parent) {
    this.parent = parent;
  }

  static NameValuePair getDelimiterChangeInstance(Token newDelimiter,
      WriteOnlyStatementList parent) {
    NameValuePair pair = new NameValuePair(parent);
    pair.newDelimiter = newDelimiter;
    return pair;
  }
  
  public Statement.Type getType(){
    return TYPE;
  }
  
  public int sizeLeftSide() {
    return name.size();
  }
  
  public String getLeftSide(int index) {
    if (index <0||index>=name.size()) {
      return null;
    }
    return ((Attribute)name.get(index)).getName();
  }
  
  public String getRightSide() {
    if (value==null) {
      return null;
    }
    return value.getValues();
  }
  
  /**
   * Get something equivalent to the original statement.  No gaurenteed to be
   * exactly the same
   */
  public String getString() {
    StringBuffer buffer = new StringBuffer();
    if (name.size() >= 1) {
      buffer.append(((Attribute) name.get(0)).getName());
    }
    for (int i = 1; i < name.size(); i++) {
      buffer.append(AutodocTokenizer.SEPARATOR_CHAR);
      buffer.append(((Attribute) name.get(i)).getName());
    }
    buffer.append(" " + AutodocTokenizer.DEFAULT_DELIMITER + " ");
    if (value != null) {
      buffer.append(value.getValues());
    }
    return buffer.toString();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "name=" + name + ",\nvalue=" + value + "," + super.toString();
  }

  void write(LogFile file, long writeId) throws LogFile.WriteException {
    for (int i = 0; i < name.size(); i++) {
      ((Attribute) name.get(i)).write(file, writeId);
      if (i < name.size() - 1) {
        file.write(AutodocTokenizer.SEPARATOR_CHAR, writeId);
      }
    }
    file.write(' ' + parent.getCurrentDelimiter() + ' ', writeId);
    if (value != null) {
      value.write(file, writeId);
      file.newLine(writeId);
    }
    if (newDelimiter != null) {
      parent.setCurrentDelimiter(newDelimiter);
    }
  }

  void print(int level) {
    Autodoc.printIndent(level);
    for (int i = 0; i < name.size(); i++) {
      System.out.print(((Token) name.get(i)).getValues());
      if (i < name.size() - 1) {
        System.out.print('.');
      }
    }
    System.out.print(' ' + parent.getCurrentDelimiter() + ' ');
    if (value == null) {
      System.out.println();
    }
    else {
      System.out.println(value.getValues());
    }
    if (newDelimiter != null) {
      parent.setCurrentDelimiter(newDelimiter);
      return;
    }
  }

  /**
   * each attribute is added as it is found
   * @param attribute
   */
  void addAttribute(Attribute attribute) {
    name.add(attribute);
  }

  /**
   * The value is added after all the attributes are added.  This name/value pair
   * to the last attribute added.
   * @param value
   */
  void addValue(Token value) {
    this.value = value;
    ((Attribute) name.get(name.size() - 1)).addNameValuePair(this);
  }

  void setValue(Token value) {
    this.value = value;
  }

  public String getValue() {
    if (value == null) {
      return null;
    }
    return value.getValues();
  }

  public Token getTokenValue() {
    return value;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.8  2007/03/23 20:33:57  sueh
 * <p> bug# 964 Adding a Type which represents the change in delimiter.  Added write(),
 * <p> to write out an autodoc to a file.
 * <p>
 * <p> Revision 1.7  2007/03/21 19:39:20  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Moved the Type inner class to a separate file so that it could be accessed by
 * <p> classes outside the package.
 * <p>
 * <p> Revision 1.6  2007/03/08 21:58:58  sueh
 * <p> bug# 964 Added Type.  Building Type.NAME_VALUE_PAIR instances piecemeal
 * <p> so that the parser can put them together.
 * <p>
 * <p> Revision 1.5  2007/03/07 21:07:05  sueh
 * <p> bug# 964 Fixed printing.
 * <p>
 * <p> Revision 1.4  2007/03/01 01:20:15  sueh
 * <p> bug# 964 Using static getInstance functions.  Added comment and empty line
 * <p> settings.
 * <p>
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
