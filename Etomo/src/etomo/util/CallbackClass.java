package etomo.util;

import etomo.storage.AdocCommand;

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
* <p> $Log$
* <p> Revision 1.1  2006/06/27 22:36:59  sueh
* <p> bug# 852 An interface to allow selected methods on the implementing class to be
* <p> called generically.
* <p> </p>
*/