package etomo.storage.autodoc;

import java.io.IOException;

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
 * <p> Revision 1.4  2009/02/04 23:30:00  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.3  2009/01/20 19:41:03  sueh
 * <p> bug# 1102 Added getSubsection.
 * <p>
 * <p> Revision 1.2  2007/04/11 22:09:16  sueh
 * <p> bug# 964 Added a link list to Statement so that groups of statements could be
 * <p> removed.  Added the parameter Statement previousStatement to the Statement
 * <p> constructor.
 * <p>
 * <p> Revision 1.1  2007/04/09 20:52:42  sueh
 * <p> bug# 964 Changed NameValuePair to an abstract class called Statement and
 * <p> child classes representing name/value pair, comment, empty line, and
 * <p> subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p> Saving Attribute instance in name instead of strings so as not to create
 * <p> duplications.
 * <p> </p>
 */
public final class Subsection extends Statement {
  public static final String rcsid = "$Id$";

  private static final Type TYPE = Statement.Type.SUBSECTION;

  private final WriteOnlyStatementList parent;
  private final Section subsection;

  Subsection(Section subsection, WriteOnlyStatementList parent,
      Statement previousStatement) {
    super(previousStatement);
    this.parent = parent;
    this.subsection = subsection;
  }

  public Statement.Type getType() {
    return TYPE;
  }

  public int sizeLeftSide() {
    return 1;
  }

  public String getString() {
    return subsection.getString();
  }

  public String getLeftSide() {
    return subsection.getTypeToken().getValues();
  }

  public String getLeftSide(int index) {
    if (index > 0) {
      return null;
    }
    return subsection.getTypeToken().getValues();
  }

  public String getRightSide() {
    return subsection.getName();
  }

  public ReadOnlySection getSubsection() {
    return subsection;
  }

  void write(LogFile file, LogFile.WriterId writerId) throws LogFile.LockException,
      IOException {
    subsection.write(file, writerId);
  }

  void print(int level) {
    Autodoc.printIndent(level);
    subsection.print(level);
  }
}
