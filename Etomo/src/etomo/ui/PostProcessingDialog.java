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
public class PostProcessingDialog extends ProcessDialog {
  public static final String rcsid =
    "$Id$";

  private TrimvolPanel trimvolPanel;

  public PostProcessingDialog(ApplicationManager appManager) {
    super(appManager, AxisID.ONLY);
    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    trimvolPanel = new TrimvolPanel(applicationManager);
    rootPanel.add(trimvolPanel.getContainer());
    
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(panelExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

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
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    applicationManager.packMainWindow();
  }
  //
  //  Action function overides for buttons
  //
  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
  }
}
