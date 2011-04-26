package etomo.ui.swing;

import javax.swing.text.Document;

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
interface StateChangeSource {
  public static final String rcsid = "$Id$";

  public boolean getState();

  public void setReporter(StateChangedReporter reporter);

  public boolean equals(Document document);
}
