package etomo.storage.autodoc;

import etomo.type.UITestAction;

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
*/
public interface AdocCommand {
  public static  final String  rcsid =  "$Id$";
  
  public void reset();
  public void set(NameValuePair pair);
  public UITestAction getAction();
  public boolean isSecondaryAutodoc();
  public String getValue();
}
/**
* <p> $Log$ </p>
*/