package etomo.ui.swing;

import java.util.EventObject;

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
class StateChangedEvent extends EventObject {
  public static final String rcsid = "$Id$";

  private final TiltPanel leadSource;

  StateChangedEvent(final TiltPanel leadSource) {
    super(leadSource);
    this.leadSource = leadSource;
  }

  TiltPanel getLeadSource() {
    return leadSource;
  }
}
