package etomo.ui.swing;

import etomo.BaseManager;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2006</p>
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
* <p> Revision 1.1  2010/11/13 16:07:35  sueh
* <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
* <p>
* <p> Revision 1.3  2010/07/02 03:19:33  sueh
* <p> bug# 1388 Constructing super class with popupChunkWarnings equal to false.
* <p>
* <p> Revision 1.2  2007/02/19 22:14:55  sueh
* <p> bug# 964 Overriding createProcessControlPanel
* <p>
* <p> Revision 1.1  2007/02/19 22:03:37  sueh
* <p> bug# 964 Process panel for PEET interface.
* <p> </p>
*/
public class PeetProcessPanel extends AxisProcessPanel {
  public static final String rcsid = "$Id$";

  PeetProcessPanel(BaseManager manager) {
    super(AxisID.ONLY, manager, false);
    createProcessControlPanel();
    initializePanels();
  }
}
