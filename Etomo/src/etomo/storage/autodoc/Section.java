package etomo.storage.autodoc;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import etomo.storage.LogFile;
import etomo.ui.swing.Token;

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
 * @notthreadsafe
 * 
 * @notthreadsafe
 *
 * <p> $$Log$
 * <p> $Revision 1.20  2010/11/13 16:05:36  sueh
 * <p> $bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p> $
 * <p> $Revision 1.19  2009/02/04 23:30:00  sueh
 * <p> $bug# 1158 Changed id and exceptions classes in LogFile.
 * <p> $
 * <p> $Revision 1.18  2009/01/20 19:39:51  sueh
 * <p> $bug# 1102 Added sectionList and subSectionMap.  Added getSection, getSectionLocation, nextSection, and setDebug.
 * <p> $
 * <p> $Revision 1.17  2008/10/27 18:36:11  sueh
 * <p> $bug# 1141 Added debug.
 * <p> $
 * <p> $Revision 1.16  2007/04/11 22:06:10  sueh
 * <p> $bug# 964 Added a link list to Statement so that groups of statements could be
 * <p> $removed.  Added the parameter Statement previousStatement to the Statement
 * <p> $constructor.
 * <p> $
 * <p> $Revision 1.15  2007/04/09 20:50:23  sueh
 * <p> $bug# 964 Changed NameValuePair to an abstract class called Statement and
 * <p> $child classes representing name/value pair, comment, empty line, and
 * <p> $subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> $Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p> $Saving Attribute instance in name instead of strings so as not to create
 * <p> $duplications.
 * <p> $
 * <p> $Revision 1.14  2007/03/23 20:36:30  sueh
 * <p> $bug# 964 Adding a Type which represents the change in delimiter.  Added write(),
 * <p> $to write out an autodoc to a file.
 * <p> $
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

final class Section extends WriteOnlyStatementList implements ReadOnlySection {
  public static final String rcsid = "$$Id$$";

  private final List statementList = new ArrayList();
  private final String key;
  private final Token type;
  private final Token name;
  private final AttributeList attributeList;
  private final List sectionList = new ArrayList();
  private final HashMap subSectionMap = new HashMap();

  private boolean subsection = false;
  private final WriteOnlyStatementList parent;
  private boolean debug = false;

  Section(Token type, Token name, WriteOnlyStatementList parent) {
    key = Section.getKey(type, name);
    this.type = type;
    this.name = name;
    this.parent = parent;
    attributeList = new AttributeList(this);
  }

  public String toString() {
    return getClass().getName() + "[key=" + key + ",type=" + type + ",name=" + name
        + ",\nattributeList=" + attributeList + "]";
  }

  public String getString() {
    StringBuffer buffer = new StringBuffer();
    buffer.append(AutodocTokenizer.OPEN_CHAR);
    if (subsection) {
      buffer.append(AutodocTokenizer.OPEN_CHAR);
    }
    buffer.append(type.getValues() + ' ' + AutodocTokenizer.DEFAULT_DELIMITER + ' '
        + name.getValues() + AutodocTokenizer.CLOSE_CHAR);
    if (subsection) {
      buffer.append(AutodocTokenizer.CLOSE_CHAR);
    }
    return buffer.toString();
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

  public StatementLocation getStatementLocation() {
    return new StatementLocation();
  }

  public Statement nextStatement(StatementLocation location) {
    if (location == null || location.isOutOfRange(statementList)) {
      return null;
    }
    Statement statement = (Statement) statementList.get(location.getIndex());
    location.increment();
    return statement;
  }

  public String getType() {
    if (type == null) {
      throw new IllegalStateException("type is required");
    }
    return type.getValues();
  }

  public String getName() {
    if (name == null) {
      throw new IllegalStateException("name is required");
    }
    return name.getValues();
  }

  boolean isGlobal() {
    return false;
  }

  boolean isAttribute() {
    return false;
  }

  void setCurrentDelimiter(Token newDelimiter) {
    parent.setCurrentDelimiter(newDelimiter);
  }

  public void setDebug() {
    debug = true;
  }

  String getCurrentDelimiter() {
    return parent.getCurrentDelimiter();
  }

  WriteOnlyAttributeList addAttribute(Token name) {
    return attributeList.addAttribute(name);
  }

  boolean equalsType(String type) {
    if (type == null) {
      // For a section location with a null type, all sections are returned.
      return true;
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
    return attributeList.getAttribute(name);
  }

  void write(LogFile file, LogFile.WriterId writerId) throws LogFile.LockException,
      IOException {
    // write section header
    file.write(AutodocTokenizer.OPEN_CHAR, writerId);
    if (subsection) {
      file.write(AutodocTokenizer.OPEN_CHAR, writerId);
    }
    type.write(file, writerId);
    file.write(' ' + parent.getCurrentDelimiter() + ' ', writerId);
    name.write(file, writerId);
    file.write(AutodocTokenizer.CLOSE_CHAR, writerId);
    if (subsection) {
      file.write(AutodocTokenizer.CLOSE_CHAR, writerId);
    }
    file.newLine(writerId);
    for (int i = 0; i < statementList.size(); i++) {
      ((Statement) statementList.get(i)).write(file, writerId);
    }
    // if subsection, write subsection footer
    if (subsection) {
      file.write("" + AutodocTokenizer.OPEN_CHAR + AutodocTokenizer.OPEN_CHAR
          + AutodocTokenizer.CLOSE_CHAR + AutodocTokenizer.CLOSE_CHAR, writerId);
      file.newLine(writerId);
    }
  }

  NameValuePair addNameValuePair() {
    NameValuePair pair = new NameValuePair(this, getMostRecentStatement());
    statementList.add(pair);
    return pair;
  }

  /**
   * Adds a subsection to a section.
   */
  Section addSection(Token type, Token name) {
    Section section = new Section(type, name, this);
    section.subsection = true;
    statementList.add(new Subsection(section, this, getMostRecentStatement()));
    sectionList.add(section);
    subSectionMap.put(section.getKey(), section);
    return section;
  }

  /**
   * Sets a SectionLocation index to the first section with the type the same as
   * the type parameter.  Returns the SectionLocation index.
   */
  public SectionLocation getSectionLocation(String type) {
    Section section = null;
    for (int i = 0; i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      if (section.equalsType(type)) {
        return new SectionLocation(type, i);
      }
    }
    return null;
  }

  /**
   * Starts with the section that location is pointing to returns the first
   * section which the same type as location.  Increments location.
   */
  public ReadOnlySection nextSection(SectionLocation location) {
    if (location == null) {
      return null;
    }
    Section section = null;
    for (int i = location.getIndex(); i < sectionList.size(); i++) {
      section = (Section) sectionList.get(i);
      if (section.equalsType(location.getType())) {
        location.setIndex(i + 1);
        return section;
      }
    }
    return null;
  }

  /**
   * Gets a subsection
   * @return
   */
  public ReadOnlySection getSection(String subSectionType, String subSectionName) {
    return (ReadOnlySection) subSectionMap.get(Section.getKey(subSectionType,
        subSectionName));
  }

  void addComment(Token comment) {
    statementList.add(new Comment(comment, this, getMostRecentStatement()));
  }

  void addEmptyLine() {
    statementList.add(new EmptyLine(this, getMostRecentStatement()));
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
    // name value pair list
    Autodoc.printIndent(level);
    System.out.println("Statements:");
    for (int i = 0; i < statementList.size(); i++) {
      Statement statement = (Statement) statementList.get(i);
      statement.print(level);
    }
    Autodoc.printIndent(level);
    System.out.println("Attributes:");
    attributeList.print(level);
  }

  private Statement getMostRecentStatement() {
    if (statementList.size() == 0) {
      return null;
    }
    return (Statement) statementList.get(statementList.size() - 1);
  }
}