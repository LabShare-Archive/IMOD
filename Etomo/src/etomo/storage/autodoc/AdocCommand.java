package etomo.storage.autodoc;

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
  public void set(NameValuePair pair);
  public UITestAction getAction();
  public boolean isSecondaryAutodoc();
  public String getValue();
}
/**
* <p> $Log$
* <p> Revision 1.1  2006/04/28 20:51:51  sueh
* <p> bug# 787 Interface for commands read by the AdocCommandReader.
* <p> </p>
*/