package etomo.ui.swing;

import java.awt.event.ActionEvent;

import etomo.FrontPageManager;
import etomo.type.AxisID;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2008</p>
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
* <p> Revision 1.2  2010/07/02 03:18:35  sueh
* <p> bug# 1388 Constructing super class with popupChunkWarnings equal to true.
* <p>
* <p> Revision 1.1  2009/10/27 20:42:18  sueh
* <p> bug# 1275 Required class for FrontPageManager.
* <p> </p>
*/
final class FrontPageProcessPanel  extends AxisProcessPanel{
  public static  final String  rcsid =  "$Id$";
  
  FrontPageProcessPanel(FrontPageManager manager) {
    super(AxisID.ONLY, manager,true);
    initializePanels();
  }
  
  protected void buttonKillAction(ActionEvent event) {
    manager.kill(axisID);
  }
  
  void showBothAxis() {
  }
  
  protected void createProcessControlPanel() {
    showBothAxis();
  }
}
