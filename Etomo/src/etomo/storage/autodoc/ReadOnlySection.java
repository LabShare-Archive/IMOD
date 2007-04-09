package etomo.storage.autodoc;
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
* <p> Revision 1.1  2007/03/21 19:41:12  sueh
* <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
* <p> </p>
*/

public interface ReadOnlySection extends ReadOnlyStatementList{
  public static  final String  rcsid =  "$Id$";
  
  public ReadOnlyAttribute getAttribute(String name);
}
