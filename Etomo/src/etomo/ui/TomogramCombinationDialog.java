package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.ConstSetParam;
import etomo.comscript.ConstSolvematchParam;
import etomo.comscript.SetParam;

import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.SolvematchParam;
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
 * <p> Revision 3.15  2004/11/30 00:36:02  sueh
 * <p> bug# 556 Adding get and set params for volcombine.
 * <p>
 * <p> Revision 3.14  2004/11/20 00:06:29  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.13.2.1  2004/10/11 02:18:46  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.13  2004/08/31 17:43:34  sueh
 * <p> Bug# 542 Adding function to turn on warning label in Setup tab.
 * <p>
 * <p> Revision 3.12  2004/08/19 02:56:21  sueh
 * <p> bug# 508 Added a way to set the don't run volcombine checkbox
 * <p> Added:
 * <p> setRunVolcombine(boolean runVolcombine)
 * <p>
 * <p> Revision 3.11  2004/07/21 00:20:05  sueh
 * <p> bug# 507 added isRunVolcombine()
 * <p>
 * <p> Revision 3.10  2004/06/25 23:25:28  sueh
 * <p> bug# 485 made Syncronize public
 * <p>
 * <p> Revision 3.9  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.8  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.7  2004/05/11 21:38:42  sueh
 * <p> bug# 302 simplifying logic in synchronize function
 * <p>
 * <p> Revision 3.6  2004/05/11 21:01:21  sueh
 * <p> bug# 302 adding field set ids
 * <p> standardizing synchronization
 * <p> removing unnecessary getCombineParams function
 * <p> removing unnecessary set field functions
 * <p> adding synchronizatin functions
 * <p>
 * <p> Revision 3.5  2004/05/03 22:27:28  sueh
 * <p> bug# 416 adding get and set BinBy2 functions.
 * <p>
 * <p> Revision 3.4  2004/03/15 20:33:55  rickg
 * <p> button variable name changes to btn...
 * <p>
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
  public static final String lblSetup = "Setup";
  public static final String lblInitial = "Initial Match";
  public static final String lblFinal = "Final Match";
  public static final int ALL_FIELDS = 10;
  public static final int MATCHING_MODEL_FIELDS = 11;
  public static final int PATCH_REGION_MODEL_FIELDS = 12;
  private SetupCombinePanel pnlSetup;
  private InitialCombinePanel pnlInitial;
  private FinalCombinePanel pnlFinal;
  private boolean combinePanelEnabled;

  private JTabbedPane tabbedPane = new JTabbedPane();
  /**
   * This is the index of the last tab to keep track of what to sync from when
   * switching tabs
   */
  private int idxLastTab;

  public TomogramCombinationDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.FIRST);

    // Instantiate the tab pane contents
    pnlSetup = new SetupCombinePanel(this, applicationManager);
    pnlInitial = new InitialCombinePanel(this,  applicationManager);
    pnlFinal = new FinalCombinePanel(this,  applicationManager);

    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    
    //  Construct the main panel for this dialog panel
    tabbedPane.add(lblSetup, pnlSetup.getContainer());
    tabbedPane.add(lblInitial, pnlInitial.getContainer());
    tabbedPane.add(lblFinal, pnlFinal.getContainer());

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

    TabChangeListener tabChangeListener = new TabChangeListener(this);
    tabbedPane.addChangeListener(tabChangeListener);
    
    // Set the default advanced dialog state
    updateAdvanced(isAdvanced);
    
    idxLastTab = tabbedPane.getSelectedIndex();
  }
  
  /**
   * Show the specified tab pane
   * @param paneName
   */
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
   * assumes synchronize is done
   * @param combineParams
   * @throws NumberFormatException
   */
  public void getCombineParams(CombineParams combineParams)
    throws NumberFormatException {
      pnlSetup.getParameters(combineParams);
  }

  /**
   * Set the solvematch parameters of the UI from the the
   * ConstSolvematchParams object
   * @param solvematchshiftParams
   */
  public void setSolvematchParams(ConstSolvematchParam solvematchParams) {
    pnlInitial.setSolvematchParams(solvematchParams);
  }
  
  /**
   * Get the the solvematch parameters of the UI returning them in the 
   * modified SolvematchParam object
   * @param solvematchParams
   * @throws NumberFormatException
   */
  public void getSolvematchParams(SolvematchParam solvematchParams)
    throws NumberFormatException {
    pnlInitial.getSolvematchParams(solvematchParams);
  }
  
  /**
   * Set the patchcrawl3D parameters of the UI from the the
   * ConstPatchcrawl3DParam object
   * @param patchcrawl3DParams
   */
  public void setPatchcrawl3DParams(ConstPatchcrawl3DParam patchcrawl3DParams) {
    pnlFinal.setPatchcrawl3DParams(patchcrawl3DParams);
  }
  
  public void setVolcombineParams(ConstSetParam setParam) {
    pnlFinal.setVolcombineParams(setParam);
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
  
  public void getVolcombineParams(SetParam setParam) {
    pnlFinal.getVolcombineParams(setParam);
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
   * synchronizes setup panel to/from initial and final panels
   * @param currentTab
   * @param copyFromCurrentTab True when synchronizing data from the current tab
   * to the other tab(s).  False when copying data into the current tab (when
   * running combine on the setup tab).
   */
  public void synchronize(String tabTitle,
    boolean copyFromCurrentTab,
    int fieldSet) {
    if (tabTitle.equals(lblSetup)) {
      if (copyFromCurrentTab) {
        synchronize(pnlSetup, pnlInitial, fieldSet);
        synchronize(pnlSetup, pnlFinal, fieldSet);
        return;
      }
      synchronize(pnlInitial, pnlSetup, fieldSet);
      synchronize(pnlFinal, pnlSetup, fieldSet);
      return;
    }
    if (tabTitle.equals(lblInitial)) {
      if (copyFromCurrentTab) {
        synchronize(pnlInitial, pnlSetup, fieldSet);
        return;
      }
      synchronize(pnlSetup, pnlInitial, fieldSet);
      return;      
    }
    if (tabTitle.equals(lblFinal)) {
      if (copyFromCurrentTab) {
        synchronize(pnlFinal, pnlSetup, fieldSet);
        return;
      }
      synchronize(pnlSetup, pnlFinal, fieldSet);
      return;      
    }
  }
  
  /**
   * Initial combine fields synchronization method
   * @param fromPanel
   * @param toPanel
   * @param fieldSet
   */
  private void synchronize(
    InitialCombineFields fromPanel,
    InitialCombineFields toPanel,
    int fieldSet) {
    if (fieldSet == ALL_FIELDS) {
      toPanel.setSurfacesOrModels(fromPanel.getSurfacesOrModels());
      toPanel.setBinBy2(fromPanel.isBinBy2());
      toPanel.setFiducialMatchListA(fromPanel.getFiducialMatchListA());
      toPanel.setFiducialMatchListB(fromPanel.getFiducialMatchListB());
      return;
    }
    if (fieldSet == MATCHING_MODEL_FIELDS) {
      toPanel.setSurfacesOrModels(fromPanel.getSurfacesOrModels());
      toPanel.setBinBy2(fromPanel.isBinBy2());
      return;
    }
  }

  /**
   * Final combine fields synchronization method
   * @param fromPanel
   * @param toPanel
   * @param fieldSet
   */
  private void synchronize(
    FinalCombineFields fromPanel,
    FinalCombineFields toPanel,
    int fieldSet) {
    if (fieldSet == ALL_FIELDS) {
      toPanel.setUsePatchRegionModel(fromPanel.isUsePatchRegionModel());
      toPanel.setXMin(fromPanel.getXMin());
      toPanel.setXMax(fromPanel.getXMax());
      toPanel.setYMin(fromPanel.getYMin());
      toPanel.setYMax(fromPanel.getYMax());
      toPanel.setZMin(fromPanel.getZMin());
      toPanel.setZMax(fromPanel.getZMax());
      return;
    }
    if (fieldSet == PATCH_REGION_MODEL_FIELDS) {
      toPanel.setUsePatchRegionModel(fromPanel.isUsePatchRegionModel());
      return;
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
  
  public boolean isRunVolcombine() {
    return pnlFinal.isRunVolcombine();
  }
  
  public void setRunVolcombine(boolean runVolcombine) {
    pnlFinal.setRunVolcombine(runVolcombine);
  }
  
  public void setBinningWarning(boolean binningWarning) {
    pnlSetup.setBinningWarning(binningWarning);
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
    synchronize(tabbedPane.getTitleAt(idxLastTab), true, ALL_FIELDS);
    applicationManager.doneTomogramCombinationDialog();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    synchronize(tabbedPane.getTitleAt(idxLastTab), true, ALL_FIELDS);
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
        "Solvematch.log",
        "Patchcorr.log",
        "Matchorwarp.log" };
    ContextPopup contextPopup =
      new ContextPopup(
        rootPanel,
        mouseEvent,
        "TOMOGRAM COMBINATION", ContextPopup.TOMO_GUIDE, 
        manPagelabel,
        manPage,
        logFileLabel,
        logFileLabel, applicationManager);
  }
  
  /**
   * Handle tab state changes 
   * @param event
   */
  void tabStateChange(ChangeEvent event){
    synchronize(tabbedPane.getTitleAt(idxLastTab), true, ALL_FIELDS);
    //  Set the last tab index to current tab so that we are ready for tab
    // change
    idxLastTab = tabbedPane.getSelectedIndex();
  }
  
  /**
   * Connect tab state changes to the appropriate dialog method
   */
  class TabChangeListener implements ChangeListener {
    TomogramCombinationDialog adaptee;
    public TabChangeListener(TomogramCombinationDialog dialog) {
      adaptee = dialog;
    }
    
     public void stateChanged(ChangeEvent event) {
       adaptee.tabStateChange(event);
     }
  }
}