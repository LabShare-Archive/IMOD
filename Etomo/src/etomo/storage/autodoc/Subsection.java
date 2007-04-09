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
final class Subsection extends Statement {
  public static final String rcsid = "$Id$";

  private static final Type TYPE = Statement.Type.SUBSECTION;

  private final WriteOnlyStatementList parent;
  private final Section subsection;

  Subsection(Section subsection, WriteOnlyStatementList parent) {
    this.parent = parent;
    this.subsection = subsection;
  }
  
  public Statement.Type getType(){
    return TYPE;
  }
  
  public int sizeLeftSide() {
    return 1;
  }
  
  public String getString() {
    return subsection.getString();
  }
  
  public String getLeftSide(int index) {
    if (index >0) {
      return null;
    }
    return subsection.getTypeToken().getValues();
  }
  
  public String getRightSide() {
    return subsection.getName();
  }

  void write(LogFile file, long writeId) throws LogFile.WriteException {
    subsection.write(file, writeId);
  }

  void print(int level) {
    Autodoc.printIndent(level);
    subsection.print(level);
  }
}
