package etomo.storage.autodoc;
/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2006</p>
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
public interface ReadOnlyNameValuePairList {
  public static  final String  rcsid =  "$Id$";
  
  public String getString();
  public NameValuePairLocation getNameValuePairLocation();
  public NameValuePair nextNameValuePair(NameValuePairLocation location);
  public String getName();
}
/**
* <p> $Log$ </p>
*/