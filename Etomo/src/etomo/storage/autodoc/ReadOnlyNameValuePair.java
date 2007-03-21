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
public interface ReadOnlyNameValuePair {
  public static  final String  rcsid =  "$Id$";
  
  public NameValuePairType getNameValuePairType();
  public String getString();
  public int numAttributes();
  public String getAttribute(int index);
  public String getValue();
}
