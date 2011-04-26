package etomo.ui.swing;

import javax.swing.event.DocumentListener;

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

interface StateChangeDocumentSource extends StateChangeSource {
  public static final String rcsid = "$Id$";

  public void addDocumentListener(DocumentListener listener);
}
