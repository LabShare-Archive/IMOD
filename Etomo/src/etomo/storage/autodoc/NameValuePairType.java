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
* <p> Revision 1.1  2007/03/21 19:40:14  sueh
* <p> bug# 964 Moved the NameValuePair.Type inner class to a separate file so that it
* <p> could be accessed by classes outside the package.
* <p> </p>
*/
public final class NameValuePairType {
  public static  final String  rcsid =  "$Id$";
  
  public static final NameValuePairType NAME_VALUE_PAIR = new NameValuePairType();
  public static final NameValuePairType SUBSECTION = new NameValuePairType();
  public static final NameValuePairType COMMENT=new NameValuePairType();
  public static final NameValuePairType EMPTY_LINE=new NameValuePairType();
  public static final NameValuePairType DELIMITER_CHANGE = new NameValuePairType();
 
 private NameValuePairType() {
 }
}
