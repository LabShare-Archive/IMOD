package etomo.ui;

import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;

import etomo.ApplicationManager;
import etomo.type.AxisID;
import etomo.type.DialogType;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class CleanUpDialog extends ProcessDialog implements ContextMenu {
  public static  final String  rcsid =  "$Id$";
  
  private CleanupPanel cleanupPanel;
  
  public CleanUpDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.ONLY, DialogType.CLEAN_UP);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    cleanupPanel = new CleanupPanel(applicationManager);
    rootPanel.add(cleanupPanel.getContainer());
    addExitButtons();
    
    btnAdvanced.setVisible(false);
    btnExecute.setText("Done");


    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    // Set the default advanced dialog state
    updateAdvanced();
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    new ContextPopup(rootPanel, mouseEvent, "Cleaning Up", ContextPopup.TOMO_GUIDE);
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    applicationManager.packMainWindow(axisID);
  }
  
  //
  //  Action function overides for buttons
  //
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneCleanUp();
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneCleanUp();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneCleanUp();
  }

}
/**
 * <p> $Log$
 * <p> Revision 1.2  2005/04/16 01:53:59  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 1.1  2005/03/24 17:49:51  sueh
 * <p> bug# 621 Moved the clean up panel in post processing to a separate
 * <p> dialog.
 * <p> </p>
*/ 
