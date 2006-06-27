package etomo.util;

import etomo.storage.autodoc.AdocCommand;

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
public interface CallbackClass {
  public static final String rcsid = "$Id$";
  
  public void callback(AdocCommand command);
}
/**
* 
* <p> $Log$ </p>
*/