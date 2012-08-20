package etomo.ui.swing;

import etomo.JoinManager;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
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
* <p> Revision 1.7  2010/07/02 03:18:45  sueh
* <p> bug# 1388 Constructing super class with popupChunkWarnings equal to true.
* <p>
* <p> Revision 1.6  2006/04/07 23:32:30  sueh
* <p> bug# 846 Changing the background colors for java 1.5.
* <p>
* <p> Revision 1.5  2006/04/06 23:34:22  sueh
* <p> bug# 844 Added a color for the join window.
* <p>
* <p> Revision 1.4  2005/09/22 21:04:29  sueh
* <p> bug# 532 Added the BaseManager to AxisProcessPanel.  So do not need
* <p> the join manager in the child class.
* <p>
* <p> Revision 1.3  2005/04/16 01:55:45  sueh
* <p> bug# 615 Add empty showBothAxis() to implement abstract function.
* <p>
* <p> Revision 1.2  2004/11/19 23:56:48  sueh
* <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
* <p>
* <p> Revision 1.1.2.4  2004/10/11 02:14:13  sueh
* <p> bug# 520 moved responsibility for the manager member variable to child
* <p> classes.  Used abstract functions to use this variable in the base class.
* <p> This is more reliable and doesn't require casting.
* <p>
* <p> Revision 1.1.2.3  2004/09/29 19:34:29  sueh
* <p> bug# 520 updated comment
* <p>
* <p> Revision 1.1.2.2  2004/09/15 22:36:45  sueh
* <p> bug# 520 calling createPrcoessControlPanel and initializePanels in
* <p> constructor
* <p>
* <p> Revision 1.1.2.1  2004/09/08 20:06:09  sueh
* <p> bug# 520 AxisProcessPanel for Join
* <p> </p>
*/

public class JoinProcessPanel extends AxisProcessPanel {
  public static final String rcsid = "$Id$";

  /**
   * @param joinManager
   * @param axis
   */
  public JoinProcessPanel(JoinManager joinManager, AxisID axis) {
    super(axis, joinManager, true);
    createProcessControlPanel();
    showBothAxis();
    initializePanels();
  }

  void showBothAxis() {
    setBackground(Colors.getBackgroundJoin());
  }
}
