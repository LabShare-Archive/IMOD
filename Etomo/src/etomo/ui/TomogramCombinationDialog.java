package etomo.ui;

import java.awt.event.*;
import javax.swing.*;

import etomo.ApplicationManager;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;
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
 * <p> Revision 2.2  2003/02/24 23:48:18  rickg
 * <p> Panel layout for combination dialog
 * <p>
 * <p> Revision 2.1  2003/01/29 15:23:50  rickg
 * <p> Fixed button action logic
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.6.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.6  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.5  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.4  2002/10/17 22:40:16  rickg
 * <p> Added fileset name to window title
 * <p> this reference removed applicationManager messages
 * <p>
 * <p> Revision 1.3  2002/10/08 23:55:39  rickg
 * <p> createCombineScript call now include a reference to this
 * <p> added NoumberFormatException
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class TomogramCombinationDialog extends ProcessDialog {
  public static final String rcsid =
    "$Id$";
  SetupCombinePanel panelSetupCombine;
  InitialCombinePanel panelInitialCombine;
  FinalCombinePanel panelFinalCombine;

  private JTabbedPane tabbedPane = new JTabbedPane();

  public TomogramCombinationDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.FIRST);

    //  Nothing in advanced catagory
    buttonAdvanced.setEnabled(false);

    //  Instantiate the tab pane contents
    panelSetupCombine = new SetupCombinePanel(applicationManager);
    panelInitialCombine = new InitialCombinePanel(applicationManager);
    panelFinalCombine = new FinalCombinePanel(applicationManager);

    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));

    //  Construct the main panel for this dialog panel
    tabbedPane.add("Setup", panelSetupCombine.getContainer());
    tabbedPane.add("Initial Match", panelInitialCombine.getContainer());
    tabbedPane.add("Final Match", panelFinalCombine.getContainer());

    rootPanel.setBorder(new BeveledBorder("Tomogram Combination").getBorder());
    rootPanel.add(tabbedPane);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(panelExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    // Set the default advanced dialog state
    updateAdvanced();
  }

  public void showPane(String paneName) {
    tabbedPane.setSelectedIndex(tabbedPane.indexOfTab(paneName));
  }
  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    applicationManager.packMainWindow();
  }

  /**
   * Set the setupcombine parameters of the UI from the the ConstCombineParams
   * object
   * @param combineParams
   */
  public void setCombineParams(ConstCombineParams combineParams) {
    panelSetupCombine.setParameters(combineParams);
  }

  /**
   * Get the the setupcombine parameters of the UI returning them in the 
   * modified CombineParams object
   * @param combineParams
   * @throws NumberFormatException
   */
  public void getCombineParams(CombineParams combineParams)
    throws NumberFormatException {
    panelSetupCombine.getParameters(combineParams);
  }

  /**
   * Set the matchorwarp parameters of the UI from the the ConstMatchorwarp
   * object
   * @param matchorwarpParams
   */
  public void setMatchorwarpParams(ConstMatchorwarpParam matchorwarpParams) {
    panelFinalCombine.setMatchorwarpParams(matchorwarpParams);
  }

  /**
   * Get the the matchorwarp parameters of the UI returning them in the 
   * modified MatchorwarpParam object  * 
   * @param matchorwarpParams
   * @throws NumberFormatException
   */
  public void getMatchorwarpParams(MatchorwarpParam matchorwarpParams)
    throws NumberFormatException {
    panelFinalCombine.getMatchorwarpParams(matchorwarpParams);
  }

  //  Action function overides for buttons
  public void buttonCancelAction(ActionEvent event) {
    super.buttonCancelAction(event);
    applicationManager.doneTomogramCombinationDialog();
  }

  public void buttonPostponeAction(ActionEvent event) {
    super.buttonPostponeAction(event);
    applicationManager.doneTomogramCombinationDialog();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    applicationManager.doneTomogramCombinationDialog();
  }

}