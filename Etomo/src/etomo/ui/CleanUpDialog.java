package etomo.ui;

import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import javax.swing.Box;
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
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    
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
    applicationManager.packMainWindow();
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
 * <p> $Log$ </p>
*/ 
