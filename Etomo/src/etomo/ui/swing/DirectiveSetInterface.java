package etomo.ui.swing;

import java.awt.Component;

import etomo.storage.Directive;

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

  Component getComponent();

  boolean isInclude();

  /**
   * @return true if instance is visible when this function is complete.
   */
  boolean msgControlChanged(boolean includeChange, boolean expandChange);

  boolean isDifferentFromCheckpoint(boolean checkInclude);

  /**
   * Return a directive containing the current state of the directive set
   * @return
   */
  Directive getState();

  void checkpoint();
}
