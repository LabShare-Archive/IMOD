package etomo.storage.autodoc;

import etomo.storage.LogFile;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.1  2007/04/09 20:50:44  sueh
 * <p> bug# 964 Changed NameValuePair to an abstract class called Statement and
 * <p> child classes representing name/value pair, comment, empty line, and
 * <p> subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p> Saving Attribute instance in name instead of strings so as not to create
 * <p> duplications.
 * <p> </p>
 */
public abstract class Statement extends WritableStatement {
  public static final String rcsid = "$Id$";

  private Statement previous = null;
  private Statement next = null;

  public Statement(Statement previousStatement) {
    if (previousStatement != null) {
      //set up link list
      previous = previousStatement;
      previous.next = this;
    }
  }
  
  public String toString() {
    return getType().toString();
  }

  abstract void write(LogFile file, long writeId) throws LogFile.WriteException;

  abstract void print(int level);

  /**
   * Update the previous.next and next.previous to remove this instance from the
   * link list.
   * @return previous
   */
  WritableStatement remove() {
    //remove this instance from the link list
    if (previous != null) {
      previous.next = next;
    }
    if (next != null) {
      next.previous = previous;
    }
    return previous;
  }

  public static final class Type {
    public static final Type NAME_VALUE_PAIR = new Type("NAME_VALUE_PAIR");
    public static final Type SUBSECTION = new Type("SUBSECTION");
    public static final Type COMMENT = new Type("COMMENT");
    public static final Type EMPTY_LINE = new Type("EMPTY_LINE");

    private final String string;

    private Type(String string) {
      this.string = string;
    }

    public String toString() {
      return string;
    }
  }
}
