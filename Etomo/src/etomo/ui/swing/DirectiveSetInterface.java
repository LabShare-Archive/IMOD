package etomo.ui.swing;

import java.awt.Component;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2013</p>
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
interface DirectiveSetInterface {
  public static final String rcsid = "$Id:$";

  boolean isVisible();

  Component getComponent();

  boolean isInclude();

  void updateDisplay();
}
