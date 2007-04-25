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
* <p> Revision 1.1  2007/03/23 20:37:00  sueh
* <p> bug# 964 An interface which can be used to modify an attribute.
* <p> </p>
*/
public interface WritableAttribute extends ReadOnlyAttribute{
  public static  final String  rcsid =  "$Id$";
  
  public void setValue(String newValue);
}
