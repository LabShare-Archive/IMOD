package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JTabbedPane;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.ConstSolvematchmodParam;
import etomo.comscript.ConstSolvematchshiftParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.SolvematchmodParam;
import etomo.comscript.SolvematchshiftParam;
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
 * <p> Revision 3.3  2004/03/05 18:23:18  sueh
 * <p> bug# 250 changed getCombineParams(int, CombineParams) - handle
 * <p> Final tab
 * <p>
 * <p> Revision 3.2  2004/03/02 00:03:11  sueh
 * <p> bug# 250 getCombineParams(fromTab, combineParams) -
 * <p> get CombineParams from a tab
 * <p>
 * <p> Revision 3.1  2004/02/27 20:08:31  sueh
 * <p> bug# 250 added getUseMatchingModels()
 * <p> added setUseMatchingModels()
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.13  2003/11/05 19:56:58  rickg
 * <p> Bug# 300 Selecting matching models on setup patch now
 * <p> selects matching models on initial page
 * <p>
 * <p> Revision 2.12  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.11  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.10  2003/07/28 22:43:21  rickg
 * <p> Added isCombinePanelEnable method and state var
 * <p>
 * <p> Revision 2.9  2003/03/20 17:45:07  rickg
 * <p> Added right button context menu
 * <p>
 * <p> Revision 2.8  2003/03/18 23:45:28  rickg
 * <p> Added combine tab enabling and solvematchmod parameter
 * <p> pass through
 * <p>
 * <p> Revision 2.7  2003/03/18 00:32:33  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.6  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.5  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.4  2003/03/06 01:19:17  rickg
 * <p> Combine changes in progress
 * <p>
 * <p> Revision 2.3  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
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
public class TomogramCombinationDialog
  extends ProcessDialog
  implements ContextMenu {
  public static final String rcsid =
    "$Id$";
  public static final int NO_TAB = -1;
  public static final int SETUP_TAB = 0;
  public static final int INITIAL_TAB = 1;
  public static final int FINAL_TAB = 2;
  private SetupCombinePanel pnlSetup;
  private InitialCombinePanel pnlInitial;
  private FinalCombinePanel pnlFinal;
  private boolean combinePanelEnabled;

  private JTabbedPane tabbedPane = new JTabbedPane();

  public TomogramCombinationDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.FIRST);

    // Instantiate the tab pane contents
    // The initial combine panel needs to be instatiated before the setup panel
    // because the setup panel calls the initial combine panel 
    pnlInitial = new InitialCombinePanel(applicationManager);
    pnlSetup = new SetupCombinePanel(applicationManager, pnlInitial);
    pnlFinal = new FinalCombinePanel(applicationManager);

    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));

    //  Construct the main panel for this dialog panel
    tabbedPane.add("Setup", pnlSetup.getContainer());
    tabbedPane.add("Initial Match", pnlInitial.getContainer());
    tabbedPane.add("Final Match", pnlFinal.getContainer());

    rootPanel.setBorder(new BeveledBorder("Tomogram Combination").getBorder());
    JLabel zWarning =
      new JLabel("For all 3D parameters Z represents the depth domain");
    zWarning.setAlignmentX(Component.CENTER_ALIGNMENT);
    rootPanel.add(zWarning);
    rootPanel.add(tabbedPane);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(Box.createVerticalGlue());
    btnExecute.setText("Done");
    rootPanel.add(pnlExitButtons);
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));

    // Set the default advanced dialog state
    updateAdvanced(isAdvanced);
  }

  public void showPane(String paneName) {
    tabbedPane.setSelectedIndex(tabbedPane.indexOfTab(paneName));
  }

  /**
   * Set the setupcombine parameters of the UI from the the ConstCombineParams
   * object
   * @param combineParams
   */
  public void setCombineParams(ConstCombineParams combineParams) {
    pnlSetup.setParameters(combineParams);
  }

  /**
   * Get the the setupcombine parameters of the UI returning them in the 
   * modified CombineParams object
   * @param combineParams
   * @throws NumberFormatException
   */
  public void getCombineParams(CombineParams combineParams)
    throws NumberFormatException {
    getCombineParams(SETUP_TAB, combineParams);
  }
  
  /**
   * Get the the setupcombine parameters of the UI returning them in the 
   * modified CombineParams object
   * @param combineParams
   * @throws NumberFormatException
   */
  public void getCombineParams(int fromTab, CombineParams combineParams)
    throws NumberFormatException {
    if (fromTab == SETUP_TAB) {
      pnlSetup.getParameters(combineParams);
      return;
    }
    if (fromTab == INITIAL_TAB) {
      pnlInitial.getCombineParameters(combineParams);
    }
    if (fromTab == FINAL_TAB) {
      pnlFinal.getCombineParameters(combineParams);
    }
  }

  /**
   * Set the solvematchshift parameters of the UI from the the
   * ConstSolvematchshiftParams object
   * @param solvematchshiftParams
   */
  public void setSolvematchshiftParams(ConstSolvematchshiftParam solvematchshiftParams) {
    pnlInitial.setSolvematchshiftParams(solvematchshiftParams);
  }

  /**
   * Get the the patchcrawl3d parameters of the UI returning them in the 
   * modified SolvematchshiftParam object
   * @param solvematchshiftParams
   * @throws NumberFormatException
   */
  public void getSolvematchshiftParams(SolvematchshiftParam solvematchshiftParams)
    throws NumberFormatException {
    pnlInitial.getSolvematchshiftParams(solvematchshiftParams);
  }

  /**
   * Set the solvematchmod parameters of the UI from the the
   * ConstSolvematchmodParams object
   * @param solvematchmodParams
   */
  public void setSolvematchmodParams(ConstSolvematchmodParam solvematchmodParams) {
    pnlInitial.setSolvematchmodParams(solvematchmodParams);
  }

  /**
   * Get the the patchcrawl3d parameters of the UI returning them in the 
   * modified SolvematchmodParam object
   * @param solvematchmodParams
   * @throws NumberFormatException
   */
  public void getSolvematchmodParams(SolvematchmodParam solvematchmodParams)
    throws NumberFormatException {
    pnlInitial.getSolvematchmodParams(solvematchmodParams);
  }

  /**
   * Set the patchcrawl3D parameters of the UI from the the
   * ConstPatchcrawl3DParam object
   * @param patchcrawl3DParams
   */
  public void setPatchcrawl3DParams(ConstPatchcrawl3DParam patchcrawl3DParams) {
    pnlFinal.setPatchcrawl3DParams(patchcrawl3DParams);
  }

  /**
   * Get the the patchcrawl3d parameters of the UI returning them in the 
   * modified Patchcrawl3DParam object
   * @param patchcrawl3DParams
   * @throws NumberFormatException
   */
  public void getPatchcrawl3DParams(Patchcrawl3DParam patchcrawl3DParams)
    throws NumberFormatException {
    pnlFinal.getPatchcrawl3DParams(patchcrawl3DParams);
  }

  /**
   * Set the matchorwarp parameters of the UI from the the ConstMatchorwarp
   * object
   * @param matchorwarpParams
   */
  public void setMatchorwarpParams(ConstMatchorwarpParam matchorwarpParams) {
    pnlFinal.setMatchorwarpParams(matchorwarpParams);
  }
  
  /**
   * get Use matching model from a tab
   * @param tab
   */
  public boolean getUseMatchingModels(int tab) {
    if (tab == SETUP_TAB) {
      return pnlSetup.getUseMatchingModels();
    }
    if (tab == INITIAL_TAB) {
      return pnlInitial.getUseMatchingModels();
    }
    return pnlSetup.getUseMatchingModels();
  }
  
  public void setUseMatchingModels(int tab, boolean setting) {
    if (tab == SETUP_TAB) {
      pnlSetup.setUseMatchingModels(setting);
    }
    else if (tab == INITIAL_TAB) {
      pnlInitial.setUseMatchingModels(setting);
    }
  }

  /**
   * Set the state of the initial and final match to the specified state.
   * @param state
   */
  public void enableCombineTabs(boolean state) {
    combinePanelEnabled = state;
    tabbedPane.setEnabledAt(tabbedPane.indexOfTab("Initial Match"), state);
    tabbedPane.setEnabledAt(tabbedPane.indexOfTab("Final Match"), state);
  }

  /**
   * Return true if the initial and final match panels are enabled. 
   * @return The state of the initial and final match panels
   */
  public boolean isCombinePanelEnabled() {
    return combinePanelEnabled;
  }

  /**
   * Get the the matchorwarp parameters of the UI returning them in the 
   * modified MatchorwarpParam object  * 
   * @param matchorwarpParams
   * @throws NumberFormatException
   */
  public void getMatchorwarpParams(MatchorwarpParam matchorwarpParams)
    throws NumberFormatException {
    pnlFinal.getMatchorwarpParams(matchorwarpParams);
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

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced(isAdvanced);
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced(boolean isAdvanced) {
    pnlInitial.setAdvanced(isAdvanced);
    pnlFinal.setAdvanced(isAdvanced);
    applicationManager.packMainWindow();
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel =
      { "Solvematch", "Matchshifts", "Patchcrawl3d", "Matchorwarp" };
    String[] manPage =
      {
        "solvematch.html",
        "matchshifts.html",
        "patchcrawl3d.html",
        "matchorwarp.html" };
    String[] logFileLabel =
      {
        "Solvematchshift.log",
        "Solvematchmod.log",
        "Patchcorr.log",
        "Matchorwarp.log" };
    ContextPopup contextPopup =
      new ContextPopup(
        rootPanel,
        mouseEvent,
        "TOMOGRAM COMBINATION",
        manPagelabel,
        manPage,
        logFileLabel,
        logFileLabel);
  }
}