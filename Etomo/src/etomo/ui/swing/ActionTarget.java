package etomo.ui.swing;

import java.io.File;

/**
* <p>Description: For performing function triggered by buttons, checkboxes, radio buttons,
* etc.  Passed parameter values may be null.</p>
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
interface ActionTarget {
  public final String rcsid = "$Id:$";

  public void setTargetFile(File file);
}
