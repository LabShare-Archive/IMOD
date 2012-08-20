package etomo.ui.swing;

import etomo.ToolsManager;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2010</p>
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
 * <p> Revision 1.2  2010/07/02 03:20:10  sueh
 * <p> bug# 1388 Constructing super class with popupChunkWarnings equal to true.
 * <p>
 * <p> Revision 1.1  2010/02/17 04:57:21  sueh
 * <p> bug# 1301 Process panel for the ToolsManager.
 * <p> </p>
 */
public final class ToolsProcessPanel extends AxisProcessPanel {
  public static final String rcsid = "$Id$";

  ToolsProcessPanel(ToolsManager manager) {
    super(AxisID.ONLY, manager, true);
    createProcessControlPanel();
    showBothAxis();
    initializePanels();
  }

  void showBothAxis() {
    setBackground(Colors.getBackgroundTools());
  }
}
