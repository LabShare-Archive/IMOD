package etomo.storage.autodoc;

import etomo.storage.LogFile;
import etomo.ui.Token;

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
 * <p> $Log$ </p>
 */
final class Comment extends Statement {
  public static final String rcsid = "$Id$";

  private static final Type TYPE = Statement.Type.COMMENT;

  private final WriteOnlyStatementList parent;
  private final Token comment;

  Comment(Token comment, WriteOnlyStatementList parent) {
    this.parent = parent;
    this.comment = comment;
  }
  
  public Statement.Type getType(){
    return TYPE;
  }
  
  public String getString() {
    return AutodocTokenizer.COMMENT_CHAR+" "+comment.getValues();
  }
  
  public int sizeLeftSide() {
    return 0;
  }
  
  public String getLeftSide(int index) {
    return null;
  }
  
  public String getRightSide() {
    return comment.getValues();
  }

  void write(LogFile file, long writeId) throws LogFile.WriteException {
    file.write(AutodocTokenizer.COMMENT_CHAR, writeId);
    comment.write(file, writeId);
    file.newLine(writeId);
    return;
  }

  void print(int level) {
    System.out.println("<comment> " + comment.getValues());
    return;
  }
}
