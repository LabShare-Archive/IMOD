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
* <p> Revision 1.1  2007/03/15 21:46:36  sueh
* <p> bug# 964 Added ReadOnlyAttribute, which is used as an interface for Attribute,
* <p> unless the Attribute needs to be modified.
* <p> </p>
*/
public interface ReadOnlyAttribute {
  public static  final String  rcsid =  "$Id$";
  
  public String getValue();
  public String getMultiLineValue();
  public ReadOnlyAttribute getAttribute(String name);
  public Attribute getAttribute(int name);
}
