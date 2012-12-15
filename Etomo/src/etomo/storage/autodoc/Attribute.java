package etomo.storage.autodoc;

import java.io.IOException;
import java.util.Vector;

import etomo.storage.LogFile;
import etomo.ui.swing.Token;

/**
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright 2002, 2003</p>
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
 * <p> $Revision 1.15  2010/11/13 16:05:36  sueh
 * <p> $bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p> $
 * <p> $Revision 1.14  2009/03/09 17:26:02  sueh
 * <p> $bug# 1199 Returning the last nameValuePair (duplicate attribute) all
 * <p> $functions that get the value, except print() which prints them all.
 * <p> $
 * <p> $Revision 1.13  2009/02/04 23:30:00  sueh
 * <p> $bug# 1158 Changed id and exceptions classes in LogFile.
 * <p> $
 * <p> $Revision 1.12  2009/01/20 19:31:16  sueh
 * <p> $bug# 1102 Added getFirstAttribute.
 * <p> $
 * <p> $Revision 1.11  2007/04/11 21:47:10  sueh
 * <p> $bug# 964 Allow removal of an occurrence of an attribute.  To allow the removal
 * <p> $of attributes, I added boolean occurrences.  When occurrences is 0, then
 * <p> $attribute will not print, and exists() will return false.  This allows the removal of
 * <p> $the attribute without changing the tree structure.  Attributes are removed by
 * <p> $NameValuePairs.
 * <p> $
 * <p> $Revision 1.10  2007/04/09 20:15:46  sueh
 * <p> $bug# 964 Moved the value to the associated name/value pair.  Changed
 * <p> $the Vector member variable from values to nameValuePairList.  Associated the
 * <p> $last attribute in each name/value pair with the name value pair.  This is the
 * <p> $attribute which used to contain the value.  The name/value pair also contained
 * <p> $the value; so it was duplicated.  This made it difficult to add a value to an
 * <p> $existing attribute.  GetValue() gets the value from the associated name/value
 * <p> $pair.  Also removed the old nameValuePairList member variable, because it
 * <p> $wasn't being used for anything.
 * <p> $
 * <p> $Revision 1.9  2007/03/23 20:28:58  sueh
 * <p> $bug# 964 Added getMultiLineValue().
 * <p> $
 * <p> $Revision 1.8  2007/03/15 21:43:59  sueh
 * <p> $bug# 964 Added changeValue() which overwrites the first token with the String
 * <p> $parameter and removes the rest of the token link list.
 * <p> $
 * <p> $Revision 1.7  2007/03/08 21:45:14  sueh
 * <p> $bug# 964 Save name/value pairs in the parser.  Fixing printing.
 * <p> $
 * <p> $Revision 1.6  2007/03/07 21:04:36  sueh
 * <p> $bug# 964 Fixed printing.
 * <p> $
 * <p> $Revision 1.5  2007/03/01 01:15:21  sueh
 * <p> $bug# 964 Added comments.
 * <p> $
 * <p> $Revision 1.4  2006/06/14 21:19:33  sueh
 * <p> $bug# 852 Added isBase() and isAttribute()
 * <p> $
 * <p> $Revision 1.3  2006/06/14 00:13:27  sueh
 * <p> $bug# 852 Added function isGlobal so that it is possible to tell whether an attribute
 * <p> $is global or part of a section.
 * <p> $
 * <p> $Revision 1.2  2006/05/01 21:16:12  sueh
 * <p> $bug# 854
 * <p> $
 * <p> $Revision 1.1  2006/01/12 17:01:17  sueh
 * <p> $bug# 798 Moved the autodoc classes to etomo.storage.
 * <p> $
 * <p> $Revision 1.9  2006/01/11 21:49:57  sueh
 * <p> $bug# 675 Moved value formatting to Token.  Use
 * <p> $Token.getFormattedValues(boolean format) to get a value that has
 * <p> $formatting strings.  If format is true then use the formatting strings,
 * <p> $otherwise strip them.  Added references to the parent of the attribute and
 * <p> $the list of name/value pairs the attribute is in.
 * <p> $
 * <p> $Revision 1.8  2006/01/03 23:23:26  sueh
 * <p> $bug# 675 Added equalsName().
 * <p> $
 * <p> $Revision 1.7  2005/12/23 02:08:37  sueh
 * <p> $bug# 675 Encapsulated the list of attributes into AttributeList.  There is a
 * <p> $list of attributes in three classes.
 * <p> $
 * <p> $Revision 1.6  2005/11/10 18:11:42  sueh
 * <p> $bug# 733 Added getAttribute(int name), which translates name into a string
 * <p> $and calls getAttribute(String).
 * <p> $
 * <p> $Revision 1.5  2005/09/21 16:22:56  sueh
 * <p> $bug# 532 Added getUnformattedValue(EtomoNumber)
 * <p> $
 * <p> $Revision 1.4  2005/05/17 19:31:59  sueh
 * <p> $bug# 372 Reducing the visibility of functions and member variables.
 * <p> $Removing unused function getValue().
 * <p> $
 * <p> $Revision 1.3  2005/02/15 19:30:44  sueh
 * <p> $bug# 602 Added getUnformattedValue() and getFormattedValue() to get the
 * <p> $value either ignoring or using the BREAK and INDENT tokens.
 * <p> $
 * <p> $Revision 1.2  2004/01/01 00:42:45  sueh
 * <p> $bug# 372 correcting interface
 * <p> $
 * <p> $Revision 1.1  2003/12/31 01:22:02  sueh
 * <p> $bug# 372 holds attribute data
 * <p> $$ </p>
 */

final class Attribute extends WriteOnlyAttributeList implements WritableAttribute {
  public static final String rcsid = "$$Id$$";

  private final WriteOnlyAttributeList parent;
  private final Token name;
  private final String key;

  /**
   * An attribute can occur more then once.  When occurrences is less then 1,
   * this attribute no longer exists.
   */
  private int occurrences = 1;

  /**
   * nameValuePairList will be instantiated if the attribute is the last attribute
   * in the name of at least one name/value pair.  The value of the attribute is
   * retrieved through the name/value pair.
   */
  private Vector nameValuePairList = null;
  /**
   * children will be instantiated if the attribute is the not the last attribute
   * in the name of at least one name/value pair.
   */
  private AttributeList children = null;

  Attribute(WriteOnlyAttributeList parent,
  /* WriteOnlyNameValuePairList nameValuePairList, */Token name) {
    this.parent = parent;
    // this.nameValuePairList = nameValuePairList;
    this.name = name;
    key = name.getKey();
  }

  /**
   * Global attributes are not in sections
   */
  boolean isGlobal() {
    return parent.isGlobal();
  }

  /**
   * First attribute in name value pair - parent is a section or an autodoc
   * @return
   */
  boolean isBase() {
    return !parent.isAttribute();
  }

  boolean isAttribute() {
    return true;
  }

  void add() {
    occurrences++;
  }

  void remove() {
    occurrences--;
  }

  boolean exists() {
    return occurrences >= 1;
  }

  static String getKey(Token name) {
    if (name == null) {
      return null;
    }
    return name.getKey();
  }

  static String getKey(String name) {
    if (name == null) {
      return null;
    }
    return Token.convertToKey(name);
  }

  /*boolean equalsName(String name) { if (name == null) { return false; } return
   * key.equals(Token.convertToKey(name)); } */
  WriteOnlyAttributeList addAttribute(Token name) {
    if (children == null) {
      children = new AttributeList(this/* , nameValuePairList */);
    }
    return children.addAttribute(name);
  }

  synchronized void addNameValuePair(NameValuePair nameValuePair) {
    if (nameValuePairList == null) {
      // complete construction before assigning to keep the unsynchronized
      // functions from seeing a partially constructed instance.
      Vector vector = new Vector();
      nameValuePairList = vector;
    }
    nameValuePairList.add(nameValuePair);
  }

  public synchronized void setValue(String newValue) {
    if (nameValuePairList == null) {
      // This attribute is never the last attribute in a name/value pair.
      // Therefore there is no value to change
      // To add a value to this attribute you would have to create the name/value pair
      // where this attribute is the last attribute in the name.
      return;
    }
    // current we can only modify the last name/value pair found
    NameValuePair nameValuePair = getNameValuePair();
    Token value = new Token();
    value.set(Token.Type.ANYTHING, newValue);
    nameValuePair.setValue(value);
  }

  public Attribute getAttribute(int name) {
    if (children == null) {
      return null;
    }
    return children.getAttribute(String.valueOf(name));
  }

  public ReadOnlyAttribute getAttribute(String name) {
    if (children == null) {
      return null;
    }
    return children.getAttribute(name);
  }

  /**
   * Gets the first attribute in children with an occurrences value of at least
   * one.
   */
  public ReadOnlyAttribute getFirstAttribute() {
    if (children == null) {
      return null;
    }
    return children.getFirstAttribute();
  }

  public void write(LogFile file, LogFile.WriterId writerId)
      throws LogFile.LockException, IOException {
    if (!exists()) {
      return;
    }
    name.write(file, writerId);
  }

  void print(int level) {
    if (exists()) {
      Autodoc.printIndent(level);
      if (nameValuePairList == null || nameValuePairList.size() == 0) {
        System.out.println(name.getValues() + ".");
      }
      else {
        System.out.print(name.getValues() + " = ");
        Token value = ((NameValuePair) nameValuePairList.get(0)).getTokenValue();
        if (value == null) {
          System.out.println("null");
        }
        else {
          System.out.println(value.getValues());
        }
        if (nameValuePairList.size() > 1) {
          for (int i = 1; i < nameValuePairList.size(); i++) {
            Autodoc.printIndent(level + 1);
            System.out.print(" = ");
            value = ((NameValuePair) nameValuePairList.get(i)).getTokenValue();
            if (value == null) {
              System.out.println("null");
            }
            else {
              System.out.println(value.getValues());
            }
          }
        }
      }
    }
    if (children != null) {
      children.print(level + 1);
    }
  }

  WriteOnlyAttributeList getParent() {
    return parent;
  }

  Token getNameToken() {
    return name;
  }

  public String getName() {
    return name.getValues();
  }

  public ReadOnlyAttributeList getChildren() {
    return children;
  }

  public String getMultiLineValue() {
    if (nameValuePairList == null || nameValuePairList.size() == 0) {
      return null;
    }
    Token value = getNameValuePair().getTokenValue();
    if (value == null) {
      return null;
    }
    return value.getMultiLineValues();
  }

  /**
   * Gets the value from the first nameValuePair in the nameValuePairList.
   */
  public String getValue() {
    if (nameValuePairList == null || nameValuePairList.size() == 0) {
      return null;
    }
    Token value = getNameValuePair().getTokenValue();
    if (value == null) {
      return null;
    }
    return value.getValues();
  }

  void removeNameValuePair(NameValuePair pair) {
    nameValuePairList.remove(pair);
  }

  /**
   * Gets the last nameValuePair in the nameValuePairList.
   */
  NameValuePair getNameValuePair() {
    if (nameValuePairList == null || nameValuePairList.size() == 0) {
      return null;
    }
    return (NameValuePair) nameValuePairList.get(nameValuePairList.size() - 1);
  }

  public Token getValueToken() {
    if (nameValuePairList == null || nameValuePairList.size() == 0) {
      return null;
    }
    Token value = getNameValuePair().getTokenValue();
    if (value == null) {
      return null;
    }
    return value;
  }

  public int hashCode() {
    return key.hashCode();
  }

  public String toString() {
    return getClass().getName() + "[" + ",name=" + name + ",\nchildren=" + children + "]";
  }
}
