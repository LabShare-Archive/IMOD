package etomo.ui.swing;

import etomo.DirectiveEditorManager;
import etomo.type.AxisID;

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
final class DirectiveEditorProcessPanel extends AxisProcessPanel{
  public static final String rcsid = "$Id:$";

  DirectiveEditorProcessPanel(DirectiveEditorManager manager) {
    super(AxisID.ONLY, manager, true);
    createProcessControlPanel();
    showBothAxis();
    initializePanels();
  }

  void showBothAxis() {
    setBackground(Colors.getBackgroundTools());
  }
}
