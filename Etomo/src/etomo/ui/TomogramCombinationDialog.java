package etomo.ui;

import java.awt.Component;
import java.awt.event.MouseEvent;

import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.ConstSetParam;
import etomo.comscript.ConstSolvematchParam;
import etomo.comscript.MatchvolParam;
import etomo.comscript.SetParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.SolvematchParam;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.CombineProcessType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MatchMode;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
import etomo.type.ReconScreenState;
import etomo.type.TomogramState;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002 - 2006</p>
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
 * <p> Revision 3.62  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.61  2008/10/16 22:31:44  sueh
 * <p> bug# 1141 Removed fixRootPanel because it doesn't do anything.
 * <p>
 * <p> Revision 3.60  2008/09/30 22:45:32  sueh
 * <p> bug# 1113 Reformatted
 * <p>
 * <p> Revision 3.59  2008/07/19 01:12:27  sueh
 * <p> bug# 1125 Making it easier to access CpuAdoc by not passing the
 * <p> manager to it; all it needs is the current directory.
 * <p>
 * <p> Revision 3.58  2008/05/13 23:07:39  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.57  2007/12/26 22:36:50  sueh
 * <p> bug# 1052 Return true when done() completes successfully.
 * <p>
 * <p> Revision 3.56  2007/12/10 22:48:37  sueh
 * <p> bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * <p> getParameters because it is required and has been added to the
 * <p> ProcesschunksParam constructor.  Removed getParameters
 * <p> ProcesschunksParam) because it is empty.
 * <p>
 * <p> Revision 3.55  2007/08/21 21:55:14  sueh
 * <p> bug# 771 In show() passing !isChanged() to pnlSetup.show().
 * <p>
 * <p> Revision 3.54  2007/08/08 15:04:36  sueh
 * <p> bug# 834 Fixed the parallel processing check box label.
 * <p>
 * <p> Revision 3.53  2007/07/17 21:44:59  sueh
 * <p> bug# 1018 Getting cpu.adoc information from CpuAdoc.
 * <p>
 * <p> Revision 3.52  2006/10/13 22:30:55  sueh
 * <p> bug# 927 Added ltfLowFromBothRadius to final tab.
 * <p>
 * <p> Revision 3.51  2006/10/10 05:24:43  sueh
 * <p> bug# 931 Allowing UITest to manipulate the tabs by changing to
 * <p> etomo.ui.TabbedPane.
 * <p>
 * <p> Revision 3.50  2006/09/19 22:37:43  sueh
 * <p> bug# 928 Added updatePatchVectorModelDisplay().
 * <p>
 * <p> Revision 3.49  2006/09/13 23:59:12  sueh
 * <p> bug# 921 Added getMatchMode
 * <p>
 * <p> Revision 3.48  2006/09/05 17:41:47  sueh
 * <p> bug# 917 Added matchvol params to initial combine tab.
 * <p>
 * <p> Revision 3.47  2006/07/28 22:24:44  sueh
 * <p> bug# 910 Done():  moved call to synchronize() to synchronizeFromCurrentTab(),
 * <p> where it can be call from ApplicationManager.saveTomogramCombinationDialog.
 * <p> This allows the dialog to be synchronized on exit as well as when changing
 * <p> dialogs.
 * <p>
 * <p> Revision 3.46  2006/07/28 19:59:16  sueh
 * <p> bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p> usingParallelProcessing because isParallel is too similar to a standard get
 * <p> function.
 * <p>
 * <p> Revision 3.45  2006/07/21 23:50:56  sueh
 * <p> bug# 892 Calling show() in setup combination
 * <p>
 * <p> Revision 3.44  2006/07/18 20:57:16  sueh
 * <p> bug# 904 Added setZMax() and setZMin().
 * <p>
 * <p> Revision 3.43  2006/07/04 20:42:14  sueh
 * <p> bug# 898 Don't remove action listeners unless the done dialog function
 * <p> succeeds.
 * <p>
 * <p> Revision 3.42  2006/06/30 20:03:54  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog done() functions,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.  Set displayed to
 * <p> false after the done dialog function is called.
 * <p>
 * <p> Revision 3.41  2006/06/21 15:55:04  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 3.40  2006/06/09 17:08:51  sueh
 * <p> bug# 869 Not checking script creation status in this class.  Removed
 * <p> combineScriptsCreated.  Calling pnlSetup.updateDisplay() in updateDisplay().
 * <p> Removed  setCombineState().
 * <p>
 * <p> Revision 3.39  2006/05/16 21:38:35  sueh
 * <p> bug# 856 Changed TomogramCombinationDialog.isUpToDate() to isChanged().
 * <p> IsChanged() looks at match direction and the use corresponding list checkbox
 * <p> and also looks at whether the scripts exist.
 * <p>
 * <p> Revision 3.38  2006/03/20 18:07:26  sueh
 * <p> bug# 835 Changed the interface ParallelDialog to AbstractParallelDialog.
 * <p>
 * <p> Revision 3.37  2006/03/16 02:01:31  sueh
 * <p> bug# 828 Added isUpToDate() which compares the dialog match mode
 * <p> against the script match mode.  Added scriptMatchMode.  Turning off
 * <p> synchronization with isUpToDate() is false.  Disable initial and final
 * <p> tabs when isUpToDate() is false.
 * <p>
 * <p> Revision 3.36  2006/02/06 21:22:17  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.35  2006/01/31 21:01:08  sueh
 * <p> bug# 521 Managing the restart buttons and the combine button in
 * <p> ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.34  2006/01/26 22:08:37  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.33  2006/01/03 23:59:36  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.
 * <p>
 * <p> Revision 3.32  2005/11/21 20:47:02  sueh
 * <p> bug# 772 Disabling the parallel process checkbox when the cpu.adoc is
 * <p> missing.  Copy parallel process checkbox's enabled setting from the
 * <p> Setup to the Final tab.
 * <p>
 * <p> Revision 3.31  2005/11/14 22:21:34  sueh
 * <p> Removed extra ;'s.
 * <p>
 * <p> Revision 3.30  2005/10/28 18:57:03  sueh
 * <p> bug# 746 don't add temp directory to splitcombine
 * <p>
 * <p> Revision 3.29  2005/10/15 00:36:46  sueh
 * <p> bug# 532 Standardized is and set parallel processing checkbox functions
 * <p> to setParallel() and isParallel().
 * <p>
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
public final class TomogramCombinationDialog extends ProcessDialog implements
    ContextMenu, AbstractParallelDialog {
  public static final String rcsid = "$Id$";

  private static final int SETUP_INDEX = 0;
  private static final int INITIAL_INDEX = 1;
  private static final int FINAL_INDEX = 2;
  public static final String lblSetup = "Setup";
  public static final String lblInitial = "Initial Match";
  public static final String lblFinal = "Final Match";
  public static final int ALL_FIELDS = 10;
  private SetupCombinePanel pnlSetup;
  private InitialCombinePanel pnlInitial;
  private FinalCombinePanel pnlFinal;
  private boolean combinePanelEnabled;
  private JPanel parallelPanelContainer = new JPanel();

  private TabbedPane tabbedPane = new TabbedPane();
  final String parallelProcessCheckBoxText;
  private boolean constructed = false;

  /**
   * This is the index of the last tab to keep track of what to sync from when
   * switching tabs
   */
  private int idxLastTab;

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]\n";
  }

  protected String paramString() {
    return "pnlSetup=" + pnlSetup + ",\npnlInitial=" + pnlInitial
        + ",\npnlFinal=" + pnlFinal + ",\ncombinePanelEnabled="
        + combinePanelEnabled + ",\nparallelProcessCheckBoxText="
        + parallelProcessCheckBoxText + ",\nidxLastTab=" + idxLastTab;
  }

  public TomogramCombinationDialog(ApplicationManager appMgr) {
    super(appMgr, AxisID.FIRST, DialogType.TOMOGRAM_COMBINATION);
    ConstEtomoNumber maxCPUs = CpuAdoc.getInstance(AxisID.ONLY,
        appMgr.getPropertyUserDir(), appMgr.getManagerKey()).getMaxVolcombine();
    if (maxCPUs != null && !maxCPUs.isNull()) {
      parallelProcessCheckBoxText = ParallelPanel.FIELD_LABEL
          + ParallelPanel.MAX_CPUS_STRING + maxCPUs.toString();
    }
    else {
      parallelProcessCheckBoxText = ParallelPanel.FIELD_LABEL;
    }
    // Instantiate the tab pane contents
    pnlSetup = new SetupCombinePanel(this, applicationManager, dialogType);
    pnlInitial = new InitialCombinePanel(this, applicationManager, dialogType,btnAdvanced);
    pnlFinal = new FinalCombinePanel(this, applicationManager, dialogType,btnAdvanced);

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.add(parallelPanelContainer);
    //  Construct the main panel for this dialog panel
    tabbedPane.add(lblSetup, pnlSetup.getContainer());
    tabbedPane.add(lblInitial, pnlInitial.getContainer());
    tabbedPane.add(lblFinal, pnlFinal.getContainer());

    rootPanel.setBorder(new BeveledBorder("Tomogram Combination").getBorder());
    JLabel zWarning = new JLabel(
        "For all 3D parameters Z represents the depth domain");
    zWarning.setAlignmentX(Component.CENTER_ALIGNMENT);
    rootPanel.add(zWarning);
    rootPanel.add(tabbedPane);
    addExitButtons();
    btnExecute.setText("Done");

    TabChangeListener tabChangeListener = new TabChangeListener(this);
    tabbedPane.addChangeListener(tabChangeListener);

    // Set the default advanced dialog state
    updateAdvanced();

    idxLastTab = tabbedPane.getSelectedIndex();
    setVisible(lblSetup);
    constructed = true;
    pnlSetup.setDeferred3dmodButtons();
    pnlInitial.setDeferred3dmodButtons();
    updateDisplay();
  }

  public static ProcessResultDisplay getCreateCombineDisplay() {
    return SetupCombinePanel
        .getCreateCombineDisplay(DialogType.TOMOGRAM_COMBINATION);
  }

  public static ProcessResultDisplay getCombineDisplay() {
    return SetupCombinePanel.getCombineDisplay(DialogType.TOMOGRAM_COMBINATION);
  }

  public static ProcessResultDisplay getRestartCombineDisplay() {
    return InitialCombinePanel
        .getRestartCombineDisplay(DialogType.TOMOGRAM_COMBINATION);
  }

  public static ProcessResultDisplay getRestartMatchvol1Display() {
    return InitialCombinePanel
        .getRestartMatchvol1Display(DialogType.TOMOGRAM_COMBINATION);
  }

  public static ProcessResultDisplay getRestartPatchcorrDisplay() {
    return FinalCombinePanel
        .getRestartPatchcorrDisplay(DialogType.TOMOGRAM_COMBINATION);
  }

  public static ProcessResultDisplay getRestartMatchorwarpDisplay() {
    return FinalCombinePanel
        .getRestartMatchorwarpDisplay(DialogType.TOMOGRAM_COMBINATION);
  }

  public static ProcessResultDisplay getRestartVolcombineDisplay() {
    return FinalCombinePanel
        .getRestartVolcombineDisplay(DialogType.TOMOGRAM_COMBINATION);
  }

  /**
   * Set the setupcombine parameters of the UI from the the ConstCombineParams
   * object
   * @param combineParams
   */
  public void setCombineParams(ConstCombineParams combineParams) {
    pnlSetup.setParameters(combineParams);
  }

  public void setZMin(String zMin) {
    pnlSetup.setZMin(zMin);
  }

  public void setZMax(String zMax) {
    pnlSetup.setZMax(zMax);
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

  Run3dmodButton getImodCombinedButton() {
    return pnlFinal.getImodCombinedButton();
  }

  /**
   * Set the solvematch parameters of the UI from the the
   * ConstSolvematchParams object
   * @param solvematchshiftParams
   */
  public void setSolvematchParams(ConstSolvematchParam solvematchParams) {
    pnlInitial.setSolvematchParams(solvematchParams);
  }

  public void setParameters(MatchvolParam param) {
    pnlInitial.setParameters(param);
  }

  public void setParameters(ReconScreenState screenState) {
    pnlSetup.setParameters(screenState);
    pnlInitial.setParameters(screenState);
    pnlFinal.setParameters(screenState);
  }

  public void getParameters(ReconScreenState screenState) {
    pnlSetup.getParameters(screenState);
    pnlFinal.getParameters(screenState);
  }

  /**
   * Show the specified tab pane
   * @param paneName
   */
  public void showPane(CombineProcessType combineProcessType) {
    if (combineProcessType == null) {
      return;
    }
    if (combineProcessType == CombineProcessType.SOLVEMATCH
        || combineProcessType == CombineProcessType.MATCHVOL1) {
      tabbedPane.setSelectedIndex(INITIAL_INDEX);
    }
    else {
      tabbedPane.setSelectedIndex(FINAL_INDEX);
    }
  }

  public void getParameters(MetaData metaData) {
    synchronize(lblSetup, false);
    pnlSetup.getParameters(metaData);
  }

  public void show() {
    pnlSetup.show(!isChanged(applicationManager.getState()));
    setDisplayed(true);
  }

  public void setParameters(ConstMetaData metaData) {
    pnlSetup.setParameters(metaData);
    synchronize(lblSetup, true);
    updateParallelProcess();
  }

  void updateParallelProcess() {
    applicationManager.setParallelDialog(axisID, usingParallelProcessing());
  }

  public boolean usingParallelProcessing() {
    synchronize(lblFinal, false);
    return pnlSetup.usingParallelProcessing();
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

  public void getParameters(MatchvolParam param) throws NumberFormatException {
    pnlInitial.getParameters(param);
  }

  /**
   * Set the patchcrawl3D parameters of the UI from the the
   * ConstPatchcrawl3DParam object
   * @param patchcrawl3DParams
   */
  public void setPatchcrawl3DParams(ConstPatchcrawl3DParam patchcrawl3DParams) {
    pnlFinal.setPatchcrawl3DParams(patchcrawl3DParams);
  }

  public void setReductionFactorParams(ConstSetParam setParam) {
    pnlFinal.setReductionFactorParams(setParam);
  }

  public void setLowFromBothRadiusParams(ConstSetParam setParam) {
    pnlFinal.setLowFromBothRadiusParams(setParam);
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

  public void getReductionFactorParam(SetParam setParam) {
    pnlFinal.getReductionFactorParam(setParam);
  }

  public void getLowFromBothRadiusParam(SetParam setParam) {
    pnlFinal.getLowFromBothRadiusParam(setParam);
  }

  public void enableReductionFactor(boolean enable) {
    pnlFinal.enableReductionFactor(enable);
  }

  public void enableLowFromBothRadius(boolean enable) {
    pnlFinal.enableLowFromBothRadius(enable);
  }

  MatchMode getMatchMode() {
    return pnlSetup.getMatchMode();
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
   * @param copyFromTab True when synchronizing data from the specified tab
   * to the other tab(s).  False when copying data into the current tab (when
   * running combine on the setup tab).
   */
  public void synchronize(String tabTitle, boolean copyFromTab) {
    if (tabTitle.equals(lblSetup)) {
      if (copyFromTab) {
        synchronize(pnlSetup, pnlInitial);
        synchronize(pnlSetup, pnlFinal);
      }
      else {
        synchronize(pnlInitial, pnlSetup);
        synchronize(pnlFinal, pnlSetup);
      }
    }
    else if (tabTitle.equals(lblInitial)) {
      if (copyFromTab) {
        synchronize(pnlInitial, pnlSetup);
      }
      else {
        synchronize(pnlSetup, pnlInitial);
      }
    }
    else if (tabTitle.equals(lblFinal)) {
      if (copyFromTab) {
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
    if (!fromPanel.isEnabled() || !toPanel.isEnabled()) {
      return;
    }
    toPanel.setSurfacesOrModels(fromPanel.getSurfacesOrModels());
    toPanel.setBinBy2(fromPanel.isBinBy2());
    toPanel.setFiducialMatchListA(fromPanel.getFiducialMatchListA());
    toPanel.setFiducialMatchListB(fromPanel.getFiducialMatchListB());
    toPanel.setUseCorrespondingPoints(fromPanel.isUseCorrespondingPoints());
    toPanel.setUseList(fromPanel.getUseList());
    toPanel.setMatchMode(fromPanel.getMatchMode());
  }

  /**
   * Final combine fields synchronization method
   * @param fromPanel
   * @param toPanel
   * @param fieldSet
   */
  private void synchronize(FinalCombineFields fromPanel,
      FinalCombineFields toPanel) {
    if (!fromPanel.isEnabled() || !toPanel.isEnabled()) {
      return;
    }
    toPanel.setUsePatchRegionModel(fromPanel.isUsePatchRegionModel());
    toPanel.setXMin(fromPanel.getXMin());
    toPanel.setXMax(fromPanel.getXMax());
    toPanel.setYMin(fromPanel.getYMin());
    toPanel.setYMax(fromPanel.getYMax());
    toPanel.setZMin(fromPanel.getZMin());
    toPanel.setZMax(fromPanel.getZMax());
    toPanel.setParallel(fromPanel.isParallel());
    toPanel.setParallelEnabled(fromPanel.isParallelEnabled());
    toPanel.setNoVolcombine(fromPanel.isNoVolcombine());
  }

  public boolean isChanged(TomogramState state) {
    return pnlSetup.isChanged(state);
  }

  public void updateDisplay() {
    if (!constructed) {
      return;
    }
    boolean enableTabs = !isChanged(applicationManager.getState());
    tabbedPane.setEnabledAt(INITIAL_INDEX, enableTabs);
    tabbedPane.setEnabledAt(FINAL_INDEX, enableTabs);
    pnlSetup.updateDisplay(enableTabs);
  }

  public void updatePatchVectorModelDisplay() {
    pnlFinal.updatePatchVectorModelDisplay();
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

  public void synchronizeFromCurrentTab() {
    synchronize(tabbedPane.getTitleAt(idxLastTab), true);
  }

  public void done() {
    applicationManager.doneTomogramCombinationDialog();
      setDisplayed(false);
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    pnlInitial.updateAdvanced(isAdvanced());
    pnlFinal.updateAdvanced(isAdvanced());
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Solvematch", "Matchshifts", "Patchcrawl3d",
        "Matchorwarp" };
    String[] manPage = { "solvematch.html", "matchshifts.html",
        "patchcrawl3d.html", "matchorwarp.html" };
    String[] logFileLabel = { "Solvematch.log", "Patchcorr.log",
        "Matchorwarp.log" };
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
        "TOMOGRAM COMBINATION", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFileLabel, applicationManager, axisID);
  }

  public boolean isTabEnabled(String tabLabel) {
    if (tabLabel.equals(lblSetup)) {
      return tabbedPane.isEnabledAt(SETUP_INDEX);
    }
    if (tabLabel.equals(lblInitial)) {
      return tabbedPane.isEnabledAt(INITIAL_INDEX);
    }
    if (tabLabel.equals(lblFinal)) {
      return tabbedPane.isEnabledAt(FINAL_INDEX);
    }
    throw new IllegalArgumentException("tabLabel=" + tabLabel);
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

  void setVisible(String showTabTitle) {
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