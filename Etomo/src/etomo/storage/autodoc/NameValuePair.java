package etomo.storage.autodoc;

import java.io.IOException;
import java.util.Vector;

import etomo.storage.LogFile;
import etomo.ui.swing.Token;

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

  public NameValuePair(WriteOnlyStatementList parent, Statement previousStatement) {
    super(previousStatement);
    this.parent = parent;
  }

  public Statement.Type getType() {
    return TYPE;
  }

  public int sizeLeftSide() {
    return name.size();
  }

  public String getLeftSide() {
    StringBuffer buffer = new StringBuffer();
    int size = sizeLeftSide();
    for (int i = 0; i < size; i++) {
      buffer.append((i > 0 ? AutodocTokenizer.SEPARATOR_CHAR : "") + getLeftSide(i));
    }
    return buffer.toString();
  }

  public String getLeftSide(int index) {
    if (index < 0 || index >= name.size()) {
      return null;
    }
    return ((Attribute) name.get(index)).getName();
  }

  public String getRightSide() {
    if (value == null) {
      return null;
    }
    return value.getValues();
  }

  public ReadOnlySection getSubsection() {
    return null;
  }

  public String toString() {
    return getString();
  }

  /**
   * Get something equivalent to the original statement.  Not guarenteed to be
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

  void setDelimiterChange(Token newDelimiter) {
    this.newDelimiter = newDelimiter;
  }

  /**
   * Remove an occurrence from each attribute in the name.  Remove this instance
   * from the last attribute.  Remove the instance from the Statement link list.
   * @return Statement.previous
   */
  WritableStatement remove() {
    Attribute attribute;
    for (int i = 0; i < name.size(); i++) {
      attribute = (Attribute) name.get(i);
      attribute.remove();
      if (i == name.size() - 1) {
        attribute.removeNameValuePair(this);
      }
    }
    return super.remove();
  }

  void write(LogFile file, LogFile.WriterId writerId) throws LogFile.LockException,
      IOException {
    for (int i = 0; i < name.size(); i++) {
      ((Attribute) name.get(i)).write(file, writerId);
      if (i < name.size() - 1) {
        file.write(AutodocTokenizer.SEPARATOR_CHAR, writerId);
      }
    }
    file.write(' ' + parent.getCurrentDelimiter() + ' ', writerId);
    if (value != null) {
      value.write(file, writerId);
      file.newLine(writerId);
    }
    if (newDelimiter != null) {
      parent.setCurrentDelimiter(newDelimiter);
    }
  }

  void print(int level) {
    Autodoc.printIndent(level);
    for (int i = 0; i < name.size(); i++) {
      System.out.print(((Attribute) name.get(i)).getValue());
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
 * <p> Revision 1.13  2010/11/13 16:05:36  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.12  2009/02/04 23:30:00  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.11  2009/01/20 19:35:33  sueh
 * <p> bug# 1102 Added getSubsection and toString.
 * <p>
 * <p> Revision 1.10  2007/04/11 22:04:52  sueh
 * <p> bug# 964 Added a link list to Statement so that groups of statements could be
 * <p> removed.  Added the parameter Statement previousStatement to the Statement
 * <p> constructor.  Removed getDelimiterChangeInstance and added
 * <p> setDelimiterChange.  Added remove(), which overrides Statement.remove() and
 * <p> calls Attribute.remove() for each attribute in the name.
 * <p>
 * <p> Revision 1.9  2007/04/09 20:33:37  sueh
 * <p> bug# 964 Change NameValuePair to an abstract class called Statement and
 * <p> child classes representing name/value pair, comment, empty line, and
 * <p> subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p> Saving Attribute instance in name instead of strings so as not to create
 * <p> duplications.
 * <p>
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
