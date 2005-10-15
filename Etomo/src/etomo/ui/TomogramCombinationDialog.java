package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
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
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.SetParam;
import etomo.comscript.SplitcombineParam;

import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.SolvematchParam;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessName;
import etomo.type.ReconScreenState;

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
 * <p> Revision 3.28  2005/10/13 22:36:57  sueh
 * <p> bug# 532 parallel process check box and no volcombine check box are on
 * <p> both setup and final now.  Getting the text for no volcombine from final.
 * <p> Getting the text for parallel process from Tomo Gen dialog.  In
 * <p> synchronize() always copying all fields.
 * <p>
 * <p> Revision 3.27  2005/09/29 19:12:38  sueh
 * <p> bug# 532 Added functionality to set the advanced/basic button to match
 * <p> the panel advanced/basic buttons, when all of the panel buttons have the
 * <p> same state.
 * <p>
 * <p> Revision 3.26  2005/09/22 21:33:11  sueh
 * <p> bug# 532 Moved the parallel process panel to AxisProcessPanel.
 * <p>
 * <p> Revision 3.25  2005/09/21 17:06:30  sueh
 * <p> bug# 532 Removed all resume functionality from the dialogs.  Removed
 * <p> resume().  Removed getParallelProgressDisplay() because the parallel
 * <p> panel can be gotten from the manager.
 * <p>
 * <p> Revision 3.24  2005/09/16 21:20:50  sueh
 * <p> bug# 532 Changed ParallelDialog.resetParallelPanel() to
 * <p> resetParallelProgressDisplay() because ParallelDialog is generic.
 * <p>
 * <p> Revision 3.23  2005/09/16 20:57:40  sueh
 * <p> bug# 532 Moved call to resetParallelPanel() to
 * <p> ApplicationManager.processchunks().  Added resetParallelPanel() to
 * <p> ParallelDialog.  Added isParallelProcessSelected().
 * <p>
 * <p> Revision 3.22  2005/09/16 18:18:21  sueh
 * <p> bug# 532 Added parallelPanel.  Add parallelPanelContainer; which is a
 * <p> JPanel to keep parallelPanel in the same place, even though it can be
 * <p> removed and re-added.  Added functions getParallelProgressDisplay,
 * <p> getParameters(MetaData), getParameters(ParallelParam).  Added
 * <p> resetParallelPanel(), which is used when the user returns to the
 * <p> Combine dialog; parallelPanel may have been placed on another dialog,
 * <p> so it has to be added again.  Added resume() and
 * <p> setParameters(ConstMetaData).  Added updateParallelProcess(), which
 * <p> displays/hides the parallel process panel depending on whether the
 * <p> parallel process checkbox on the final tab has been checked.
 * <p>
 * <p> Revision 3.21  2005/08/04 20:17:17  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.20  2005/04/21 20:55:02  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.19  2005/04/16 02:05:04  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.18  2005/01/14 03:11:19  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.17  2004/12/03 20:23:31  sueh
 * <p> bug# 556 Support older versions of volcombine.com.  Added enableReductionFactor() to
 * <p> disable ReductionFactor when the set param is missing or invalid.
 * <p>
 * <p> Revision 3.16  2004/12/02 20:42:31  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
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
  implements ContextMenu, ParallelDialog {
  public static final String rcsid =
    "$Id$";
  public static final String lblSetup = "Setup";
  public static final String lblInitial = "Initial Match";
  public static final String lblFinal = "Final Match";
  public static final int ALL_FIELDS = 10;
  private SetupCombinePanel pnlSetup;
  private InitialCombinePanel pnlInitial;
  private FinalCombinePanel pnlFinal;
  private boolean combinePanelEnabled;
  private JPanel parallelPanelContainer = new JPanel();

  private JTabbedPane tabbedPane = new JTabbedPane();
  final String parallelProcessCheckBoxText;
  /**
   * This is the index of the last tab to keep track of what to sync from when
   * switching tabs
   */
  private int idxLastTab;

  public TomogramCombinationDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.FIRST, DialogType.TOMOGRAM_COMBINATION);

    ConstEtomoNumber maxCPUs = ParallelPanel.getMaxCPUs(AxisID.ONLY,
        ProcessName.VOLCOMBINE);
    if (maxCPUs != null && !maxCPUs.isNull()) {
      parallelProcessCheckBoxText = ParallelPanel.TITLE
          + ParallelPanel.MAX_CPUS_STRING + maxCPUs.toString();
    }
    else {
      parallelProcessCheckBoxText = ParallelPanel.TITLE;
    }
    // Instantiate the tab pane contents
    pnlSetup = new SetupCombinePanel(this, applicationManager);
    pnlInitial = new InitialCombinePanel(this,  applicationManager);
    pnlFinal = new FinalCombinePanel(this,  applicationManager);

    fixRootPanel(rootSize);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.add(parallelPanelContainer);
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
    addExitButtons();
    btnExecute.setText("Done");

    TabChangeListener tabChangeListener = new TabChangeListener(this);
    tabbedPane.addChangeListener(tabChangeListener);
    
    // Set the default advanced dialog state
    updateAdvanced(isAdvanced);
    
    idxLastTab = tabbedPane.getSelectedIndex();
    setVisible(lblSetup);
  }
  
  
  static final JCheckBox getParallelProcessCheckBox() {
    ConstEtomoNumber maxCPUs = ParallelPanel.getMaxCPUs(AxisID.ONLY,
        ProcessName.VOLCOMBINE);
    JCheckBox parallelProcessCheckBox;
    if (maxCPUs != null && !maxCPUs.isNull()) {
      parallelProcessCheckBox = new JCheckBox(ParallelPanel.TITLE
          + ParallelPanel.MAX_CPUS_STRING + maxCPUs.toString());
    }
    else {
      parallelProcessCheckBox = new JCheckBox(ParallelPanel.TITLE);
    }
    return parallelProcessCheckBox;
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
  
  public final void getParameters(ParallelParam param) {
    ProcesschunksParam processchunksParam = (ProcesschunksParam) param;
    pnlFinal.getParameters(processchunksParam);
  }
  
  public final void setParameters(ReconScreenState screenState) {
    pnlSetup.setParameters(screenState);
    pnlInitial.setParameters(screenState);
    pnlFinal.setParameters(screenState);
  }
  
  public final void getParameters(ReconScreenState screenState) {
    pnlSetup.getParameters(screenState);
    pnlInitial.getParameters(screenState);
    pnlFinal.getParameters(screenState);
  }
  
  public final void getParameters(MetaData metaData) {
    synchronize(lblSetup, false);
    pnlSetup.getParameters(metaData);
  }
  
  public final void getParameters(SplitcombineParam param) {
    pnlSetup.setParameters(param);
  }
  
  public final void setParameters(ConstMetaData metaData) {
    pnlSetup.setParameters(metaData);
    synchronize(lblSetup, true);
    updateParallelProcess();
  }
  
  final void updateParallelProcess() {
    applicationManager.setParallelDialog(axisID, this);
  }
  
  public final boolean isParallel() {
    synchronize(lblFinal, false);
    return pnlSetup.isParallel();
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
  
  public void enableReductionFactor(boolean enable) {
    pnlFinal.enableReductionFactor(enable);
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
  public void synchronize(String tabTitle, boolean copyFromCurrentTab) {
    if (tabTitle.equals(lblSetup)) {
      if (copyFromCurrentTab) {
        synchronize(pnlSetup, pnlInitial);
        synchronize(pnlSetup, pnlFinal);
      }
      else {
        synchronize(pnlInitial, pnlSetup);
        synchronize(pnlFinal, pnlSetup);
      }
    }
    else if (tabTitle.equals(lblInitial)) {
      if (copyFromCurrentTab) {
        synchronize(pnlInitial, pnlSetup);
      }
      else {
        synchronize(pnlSetup, pnlInitial);
      }
      ;
    }
    else if (tabTitle.equals(lblFinal)) {
      if (copyFromCurrentTab) {
        synchronize(pnlFinal, pnlSetup);
      }
      else {
        synchronize(pnlSetup, pnlFinal);
      }
    }
  }
  
  /**
   * Initial combine fields synchronization method
   * @param fromPanel
   * @param toPanel
   * @param fieldSet
   */
  private void synchronize(InitialCombineFields fromPanel,
      InitialCombineFields toPanel) {
    toPanel.setSurfacesOrModels(fromPanel.getSurfacesOrModels());
    toPanel.setBinBy2(fromPanel.isBinBy2());
    toPanel.setFiducialMatchListA(fromPanel.getFiducialMatchListA());
    toPanel.setFiducialMatchListB(fromPanel.getFiducialMatchListB());
  }

  /**
   * Final combine fields synchronization method
   * @param fromPanel
   * @param toPanel
   * @param fieldSet
   */
  private void synchronize(FinalCombineFields fromPanel,
      FinalCombineFields toPanel) {
    toPanel.setUsePatchRegionModel(fromPanel.isUsePatchRegionModel());
    toPanel.setXMin(fromPanel.getXMin());
    toPanel.setXMax(fromPanel.getXMax());
    toPanel.setYMin(fromPanel.getYMin());
    toPanel.setYMax(fromPanel.getYMax());
    toPanel.setZMin(fromPanel.getZMin());
    toPanel.setZMax(fromPanel.getZMax());
    toPanel.setParallel(fromPanel.isParallel());
    toPanel.setNoVolcombine(fromPanel.isNoVolcombine());
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
    synchronize(tabbedPane.getTitleAt(idxLastTab), true);
    applicationManager.doneTomogramCombinationDialog();
  }

  public void buttonExecuteAction(ActionEvent event) {
    super.buttonExecuteAction(event);
    synchronize(tabbedPane.getTitleAt(idxLastTab), true);
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
    UIHarness.INSTANCE.pack(axisID, applicationManager);
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
  void tabStateChange(ChangeEvent event) {
    int idxNewTab = tabbedPane.getSelectedIndex();
    synchronize(tabbedPane.getTitleAt(idxLastTab), true);
    setVisible(tabbedPane.getTitleAt(idxNewTab));
    //  Set the last tab index to current tab so that we are ready for tab
    // change
    idxLastTab = tabbedPane.getSelectedIndex();
  }
  
  final void setVisible(String showTabTitle) {
    if (showTabTitle.equals(lblSetup)) {
      pnlSetup.setVisible(true);
      pnlInitial.setVisible(false);
      pnlFinal.setVisible(false);
    }
    else if (showTabTitle.equals(lblInitial)) {
      pnlSetup.setVisible(false);
      pnlInitial.setVisible(true);
      pnlFinal.setVisible(false);
    }
    else if (showTabTitle.equals(lblFinal)) {
      pnlSetup.setVisible(false);
      pnlInitial.setVisible(false);
      pnlFinal.setVisible(true);
    }
    UIHarness.INSTANCE.pack(AxisID.ONLY, applicationManager);
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