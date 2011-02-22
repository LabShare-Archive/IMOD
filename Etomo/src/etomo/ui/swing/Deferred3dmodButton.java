package etomo.ui.swing;

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
* <p> $Log$
* <p> Revision 1.1  2010/11/13 16:07:34  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 1.1  2008/05/13 23:01:36  sueh
* <p> bug# 847 Interface for Run3dmodButton.  Gives access to the button's
* <p> right click menu actions.
* <p>
* <p> Revision 1.1  2008/05/07 00:25:37  sueh
* <p> bug#847 Running deferred 3dmods by using the button that usually calls
* <p> them.  This avoids having to duplicate the calls and having a
* <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
* <p> button be passed to the function that starts the process.  The interface is
* <p> used to pass the real run 3dmod buttons to functions running other
* <p> processes so that the 3dmod process can be run later.
* <p> </p>
*/
public interface Deferred3dmodButton {
  public static final String rcsid = "$Id$";

  public void action(Run3dmodMenuOptions menuOptions);
}
