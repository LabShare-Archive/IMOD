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
public final class NameValuePairType {
  public static  final String  rcsid =  "$Id$";
  
  public static final NameValuePairType NAME_VALUE_PAIR = new NameValuePairType();
  public static final NameValuePairType SUBSECTION = new NameValuePairType();
  public static final NameValuePairType COMMENT=new NameValuePairType();
  public static final NameValuePairType EMPTY_LINE=new NameValuePairType();
 
 private NameValuePairType() {
 }
}
