package etomo.storage.autodoc;

import java.io.IOException;

import etomo.storage.LogFile;
import etomo.ui.swing.Token;

/**
 * <p>Description: Represents a comment in an autodoc.</p>
 * 
 * <p>Copyright: Copyright 2006 - 2010</p>
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
 * <p> Revision 1.6  2010/11/13 16:05:36  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.5  2010/02/17 04:49:43  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.4  2009/02/04 23:30:00  sueh
 * <p> bug# 1158 Changed id and exceptions classes in LogFile.
 * <p>
 * <p> Revision 1.3  2009/01/20 19:34:46  sueh
 * <p> bug# 1102 Added getSubsection.
 * <p>
 * <p> Revision 1.2  2007/04/11 22:01:52  sueh
 * <p> bug# 964 Added a link list to Statement so that groups of statements could be
 * <p> removed.  Added the parameter Statement previousStatement to the Statement
 * <p> constructor.
 * <p>
 * <p> Revision 1.1  2007/04/09 20:32:12  sueh
 * <p> bug# 964 Change NameValuePair to an abstract class called Statement and
 * <p> child classes representing name/value pair, comment, empty line, and
 * <p> subsection.  Made delimiter change an attribute of the name/value pair class.
 * <p> Added ReadOnlyStatement to provide a public interface for Statement classes.
 * <p> </p>
 */
final class Comment extends Statement {
  public static final String rcsid = "$Id$";

  private static final Type TYPE = Statement.Type.COMMENT;

  private final WriteOnlyStatementList parent;
  private final Token comment;

  Comment(Token comment, WriteOnlyStatementList parent, Statement previousStatement) {
    super(previousStatement);
    this.parent = parent;
    this.comment = comment;
  }

  public Statement.Type getType() {
    return TYPE;
  }

  public String getString() {
    return AutodocTokenizer.COMMENT_CHAR + " " + comment.getValues();
  }

  public int sizeLeftSide() {
    return 0;
  }

  public String getLeftSide() {
    return null;
  }

  public String getLeftSide(int index) {
    return null;
  }

  public String getRightSide() {
    return comment.getValues();
  }

  void write(LogFile file, LogFile.WriterId writerId) throws LogFile.LockException,
      IOException {
    file.write(AutodocTokenizer.COMMENT_CHAR, writerId);
    comment.write(file, writerId);
    file.newLine(writerId);
    return;
  }

  public ReadOnlySection getSubsection() {
    return null;
  }

  void print(int level) {
    System.out.println("<comment> " + comment.getValues());
    return;
  }
}
