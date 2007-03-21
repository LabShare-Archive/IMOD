package etomo.storage;

import etomo.storage.autodoc.ReadOnlyNameValuePair;
import etomo.type.UITestAction;

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
*/
public interface AdocCommand {
  public static  final String  rcsid =  "$Id$";
  
  public void reset();
  public void set(ReadOnlyNameValuePair pair);
  public UITestAction getAction();
  public boolean isFunctionLocation();
  public boolean isFunction();
  public String getValue();
}
/**
* <p> $Log$
* <p> Revision 1.3  2006/08/08 17:13:41  sueh
* <p> bug# 852 Adding isFunctionLocation() and isFunction().  Removing
* <p> isSecondaryAutodoc().
* <p>
* <p> Revision 1.2  2006/05/01 21:15:49  sueh
* <p> bug# 787
* <p>
* <p> Revision 1.1  2006/04/28 20:51:51  sueh
* <p> bug# 787 Interface for commands read by the AdocCommandReader.
* <p> </p>
*/