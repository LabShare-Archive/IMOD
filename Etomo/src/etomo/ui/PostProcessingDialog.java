package etomo.ui;

import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.comscript.TrimvolParam;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/10/30 21:05:06  rickg
 * <p> Bug# 340 Added context menu
 * <p>
 * <p> Revision 2.4  2003/04/17 23:07:20  rickg
 * <p> Added cleanup panel
 * <p>
 * <p> Revision 2.3  2003/04/16 00:15:01  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 2.2  2003/04/14 23:57:34  rickg
 * <p> Trimvol management changes
 * <p>
 * <p> Revision 2.1  2003/04/10 23:43:23  rickg
 * <p> Added trimvol panel
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.5.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.5  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.4  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.3  2002/10/17 22:39:55  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class PostProcessingDialog
  extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private TrimvolPanel trimvolPanel;
  private CleanupPanel cleanupPanel;

  public PostProcessingDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.ONLY);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));

    trimvolPanel = new TrimvolPanel(applicationManager);
    rootPanel.add(trimvolPanel.getContainer());

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
   * Set the trimvol panel values with the specified parameters
   * @param trimvolParam
   */
  public void setTrimvolParams(TrimvolParam trimvolParam) {
    trimvolPanel.setParameters(trimvolParam);
  }

  /**
   * Get the trimvol parameter values from the panel 
   * @param trimvolParam
   */
  public void getTrimvolParams(TrimvolParam trimvolParam) {
    trimvolPanel.getParameters(trimvolParam);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Trimvol"};
    String[] manPage = { "trimvol.html" };

    //    ContextPopup contextPopup =
    new ContextPopup(
      rootPanel,
      mouseEvent,
      "POST-PROCESSING",
      manPagelabel,
      manPage);
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
    applicationManager.donePostProcessing();
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.donePostProcessing();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.donePostProcessing();
  }
}
