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
 * <p> $Log$ </p>
 */
final class EmptyLine extends Statement {
  public static final String rcsid = "$Id$";

  private static final Type TYPE = Statement.Type.EMPTY_LINE;
  
  private final WriteOnlyStatementList parent;
  
  EmptyLine(WriteOnlyStatementList parent){
    this.parent=parent;
  }
  
  public Statement.Type getType(){
    return TYPE;
  }
  
  public String getString() {
    return "";
  }
  
  public int sizeLeftSide() {
    return 0;
  }
  
  public String getLeftSide(int index) {
    return null;
  }
  
  public String getRightSide() {
    return "";
  }

  void write(LogFile file, long writeId) throws LogFile.WriteException {
    file.newLine(writeId);
  }

  void print(int level) {
    Autodoc.printIndent(level);
    System.out.println("<empty-line>");
  }
}
