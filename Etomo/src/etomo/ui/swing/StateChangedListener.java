package etomo.ui.swing;

import java.util.EventListener;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2011</p>
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

public interface StateChangedListener extends EventListener {
  public static final String rcsid = "$Id$";

  public void stateChanged(StateChangedEvent event);
}
