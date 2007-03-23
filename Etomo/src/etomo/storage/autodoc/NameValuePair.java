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
final class NameValuePair implements ReadOnlyNameValuePair {
  public static final String rcsid = "$Id$";

  private final Vector name = new Vector();
  private final NameValuePairType type;
  private final WriteOnlyNameValuePairList parent;

  private Token value = null;
  private Section subsection = null;

  private NameValuePair(NameValuePairType type,
      WriteOnlyNameValuePairList parent) {
    this.type = type;
    this.parent = parent;
  }

  private NameValuePair(NameValuePairType type, Token value,
      WriteOnlyNameValuePairList parent) {
    this(type, parent);
    this.value = value;
  }

  private NameValuePair(NameValuePairType type, Section subsection,
      WriteOnlyNameValuePairList parent) {
    this(type, parent);
    name.add(subsection.getTypeToken());
    value = subsection.getNameToken();
    this.subsection = subsection;
  }

  static NameValuePair getNameValuePairInstance(
      WriteOnlyNameValuePairList parent) {
    return new NameValuePair(NameValuePairType.NAME_VALUE_PAIR, parent);
  }

  static NameValuePair getSubsectionInstance(Section subsection,
      WriteOnlyNameValuePairList parent) {
    return new NameValuePair(NameValuePairType.SUBSECTION, subsection, parent);
  }

  static NameValuePair getDelimiterChangeInstance(Token newDelimiter,
      WriteOnlyNameValuePairList parent) {
    return new NameValuePair(NameValuePairType.DELIMITER_CHANGE, newDelimiter,
        parent);
  }

  static NameValuePair getCommentInstance(Token comment,
      WriteOnlyNameValuePairList parent) {
    return new NameValuePair(NameValuePairType.COMMENT, comment, parent);
  }

  static NameValuePair getEmptyLineInstance(WriteOnlyNameValuePairList parent) {
    return new NameValuePair(NameValuePairType.EMPTY_LINE, parent);
  }

  void write(LogFile file, long writeId) throws LogFile.WriteException {
    if (type == NameValuePairType.DELIMITER_CHANGE) {
      parent.setCurrentDelimiter(value);
      return;
    }
    if (type == NameValuePairType.EMPTY_LINE) {
      file.newLine(writeId);
      return;
    }
    if (type == NameValuePairType.COMMENT) {
      file.write(AutodocTokenizer.COMMENT_CHAR, writeId);
      value.write(file, writeId);
      file.newLine(writeId);
      return;
    }
    if (type == NameValuePairType.SUBSECTION) {
      subsection.write(file, writeId);
    }
    if (type == NameValuePairType.NAME_VALUE_PAIR) {
      for (int i = 0; i < name.size(); i++) {
        ((Token) name.get(i)).write(file, writeId);
        if (i < name.size() - 1) {
          file.write(AutodocTokenizer.SEPARATOR_CHAR, writeId);
        }
      }
      file.write(' '+parent.getCurrentDelimiter()+' ',writeId);
      if (value!=null) {
        value.write(file,writeId);
        file.newLine(writeId);
      }
    }
  }

  void print(int level) {
    Autodoc.printIndent(level);
    if (type == NameValuePairType.DELIMITER_CHANGE) {
      parent.setCurrentDelimiter(value);
      return;
    }
    if (type == NameValuePairType.EMPTY_LINE) {
      System.out.println("<empty-line>");
      return;
    }
    if (type == NameValuePairType.COMMENT) {
      System.out.println("<comment> " + value.getValues());
      return;
    }
    if (type == NameValuePairType.SUBSECTION) {
      subsection.print(level);
      return;
    }
    if (type == NameValuePairType.NAME_VALUE_PAIR) {
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
    }
  }

  public int numAttributes() {
    return name.size();
  }

  void addAttribute(Token attribute) {
    name.add(attribute);
  }

  void addValue(Token value) {
    this.value = value;
  }

  public String getAttribute(int index) {
    if (index >= name.size()) {
      return null;
    }
    return ((Token) name.get(index)).getValues();
  }

  public String getValue() {
    if (value == null) {
      return null;
    }
    return value.getValues();
  }

  public NameValuePairType getNameValuePairType() {
    return type;
  }

  public String getString() {
    StringBuffer buffer = new StringBuffer();
    if (name.size() >= 1) {
      buffer.append(((Token) name.get(0)).getValues());
    }
    for (int i = 1; i < name.size(); i++) {
      buffer.append(AutodocTokenizer.SEPARATOR_CHAR);
      buffer.append(((Token) name.get(i)).getValues());
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
}
/**
 * <p> $Log$
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
