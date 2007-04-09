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
public abstract class Statement implements ReadOnlyStatement{
  public static  final String  rcsid =  "$Id$";
  
  abstract void write(LogFile file, long writeId) throws LogFile.WriteException;
  abstract void print(int level);
  
   public static final class Type{
    public static final Type NAME_VALUE_PAIR = new Type();
    public static final Type SUBSECTION = new Type();
    public static final Type COMMENT=new Type();
    public static final Type EMPTY_LINE=new Type();
   
   private Type() {
   }
  }
}
