package etomo.ui;

import etomo.type.Run3dmodMenuOptions;

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
public interface Run3dmodDeferredCommand {
  public static  final String  rcsid =  "$Id$";
  
  public void action(Run3dmodMenuOptions menuOptions);
}
