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
* <p> $Log$ </p>
*/
public interface ReadOnlyAttribute {
  public static  final String  rcsid =  "$Id$";
  
  public String getValue();
  public ReadOnlyAttribute getAttribute(String name);
  public Attribute getAttribute(int name);
}
