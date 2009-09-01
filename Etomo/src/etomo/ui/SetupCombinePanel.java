package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.CombinePatchSize;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.FiducialMatch;
import etomo.type.MatchMode;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.util.DatasetFiles;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;

/**
 * <p>
 * Description:
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 3.60  2009/03/17 00:46:24  sueh
 * bug# 1186 Pass managerKey to everything that pops up a dialog.
 *
 * Revision 3.59  2009/02/13 02:34:57  sueh
 * bug# 1176 Checking return value of MRCHeader.read.
 *
 * Revision 3.58  2009/01/20 20:25:29  sueh
 * bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 *
 * Revision 3.57  2008/07/19 01:12:03  sueh
 * bug# 1125 Making it easier to access CpuAdoc by not passing the
 * manager to it; all it needs is the current directory.
 *
 * Revision 3.56  2008/05/28 02:51:22  sueh
 * bug# 1111 Add a dialogType parameter to the ProcessSeries
 * constructor.  DialogType must be passed to any function that constructs
 * a ProcessSeries instance.
 *
 * Revision 3.55  2008/05/13 23:07:20  sueh
 * bug# 847 Adding a right click menu for deferred 3dmods to some
 * process buttons.
 *
 * Revision 3.54  2008/05/03 00:56:57  sueh
 * bug# 847 Passing null for ProcessSeries to process funtions.
 *
 * Revision 3.53  2008/02/29 20:52:58  sueh
 * bug# 1092 Running updateDisplay after create is done to pick up all changes.  In isChanged fixed return value calculation (parenthesis where in the the wrong places).
 *
 * Revision 3.52  2008/01/25 22:28:18  sueh
 * bug# 1070 In setParameters don't use parallel processing unless the cpu.adoc or
 * IMOD_PROCESSORS has been set by the user.
 *
 * Revision 3.51  2007/12/10 22:46:03  sueh
 * bug# 1041 Formatted.
 *
 * Revision 3.50  2007/08/21 21:54:18  sueh
 * bug# 771 Added JLabel lTomogramSizeWarning.  Added
 * isTomogramSizeChanged, resetXandY, and updateTomogramSizeWarning.
 *
 * Revision 3.49  2007/07/17 21:44:24  sueh
 * bug# 1018 Adding cpu.adoc information from CpuAdoc.
 *
 * Revision 3.48  2007/03/07 21:13:52  sueh
 * bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * because it is getting more complicated.
 *
 * Revision 3.47  2007/02/09 00:52:55  sueh
 * bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * classes.
 *
 * Revision 3.46  2006/11/07 23:02:50  sueh
 * bug# 954 Added bug for  cbNoVolcombine
 *
 * Revision 3.45  2006/09/13 23:48:40  sueh
 * bug# 921
 *
 * Revision 3.44  2006/07/28 19:58:50  sueh
 * bug# 868 Changed AbstractParallelDialog.isParallel to
 * usingParallelProcessing because isParallel is too similar to a standard get
 * function.
 *
 * Revision 3.43  2006/07/28 17:43:40  sueh
 * bug# 909 Changed TomogramState.combineScriptsCreated to an EtomoState so
 * it will show when it has not been set.  Deleting old versions of combine scripts
 * create and match mode in the .edf.  Getting data from the old versions of
 * combine scripts create and match mode with the new ones aren't available.
 *
 * Revision 3.42  2006/07/21 23:50:10  sueh
 * bug# 892 Added show().
 *
 * Revision 3.41  2006/07/20 17:21:33  sueh
 * bug# 848 Made UIParameters a singleton.
 *
 * Revision 3.40  2006/07/19 15:24:41  sueh
 * bug# 903 Getting ZMin and Max labels from ConstCombineParams.
 *
 * Revision 3.39  2006/06/21 15:54:41  sueh
 * bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 *
 * Revision 3.38  2006/06/09 17:04:07  sueh
 * bug# 869 Getting script creation mode from TomogramState.  Removed
 * scriptMatchMode.  Added updateDisplay().  Removed setCombineState().
 * IsChanged():  checking TomogramState.combineScriptsCreated and
 * CombineMatchMode.  Removed updateStartCombine().
 *
 * Revision 3.37  2006/05/16 21:37:35  sueh
 * bug# 856 Added useCorrespondingPoints and useList.  Added isChanged(),
 * which looks at useCorrespondingPoints.
 *
 * Revision 3.36  2006/05/12 17:22:46  sueh
 * bug# 861 unset patch region model when checkbox is not checked
 *
 * Revision 3.35  2006/04/28 21:04:47  sueh
 * bug# 787 PanelHeader:  Removed the member variable title, which was
 * not used.
 *
 * Revision 3.34  2006/03/27 21:06:15  sueh
 * bug# 836 Added DialogType to PanelHeader get instances functions so
 * that the buttons in PanelHeader could save themselves.
 *
 * Revision 3.33  2006/03/16 01:58:53  sueh
 * bug# 828 Added isEnabled() - always returns true.  Added
 * getScreenMatchMode() - returns the state of rbBtoA with a MatchMode.
 *
 * Revision 3.32  2006/02/06 21:21:55  sueh
 * bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 *
 * Revision 3.31  2006/01/31 21:00:35  sueh
 * bug# 521 Managing the combine button in ProcessResultDisplayFactory.
 *
 * Revision 3.30  2006/01/26 22:08:25  sueh
 * bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * the buttons turned on each they are run, unless the process fails or is
 * killed.
 *
 * Revision 3.29  2006/01/03 23:46:10  sueh
 * bug# 675 Converted JCheckBox's to CheckBox.  Converted JRadioButton's
 * toRadioButton.
 *
 * Revision 3.28  2005/12/14 20:58:39  sueh
 * bug# 784 Added tool tips.
 *
 * Revision 3.27  2005/12/13 02:28:49  sueh
 * bug# 773 Getting default parallel processing checkbox setting from
 * metadata.defaultParallel.
 *
 * Revision 3.26  2005/11/21 20:46:51  sueh
 * bug# 772 Disabling the parallel process checkbox when the cpu.adoc is
 * missing.  Copy parallel process checkbox's enabled setting from the
 * Setup to the Final tab.
 *
 * Revision 3.25  2005/11/14 22:20:04  sueh
 * bug# 762 Made buttonAction() protected.
 *
 * Revision 3.24  2005/10/28 18:56:48  sueh
 * bug# 746 don't add temp directory to splitcombine
 *
 * Revision 3.23  2005/10/15 00:36:33  sueh
 * bug# 532 Standardized is and set parallel processing checkbox functions
 * to setParallel() and isParallel().
 *
 * Revision 3.22  2005/10/13 22:36:14  sueh
 * bug# 532 parallel process check box and no volcombine check box are on
 * both setup and final now.  Getting the text for no volcombine from final.
 * Getting the text for parallel process from Tomo Gen dialog.
 *
 * Revision 3.21  2005/09/29 19:10:49  sueh
 * bug# 532 Add panel headers to all of the sections in Combine.  Hide the
 * sections in the tabs that are not visible so that the visible tab can become
 * small.  Added an expand() function to each tab to handle the
 * expand/contract requests of the panel header buttons.  Added set and get
 * parameters for ReconScreenState to set and get the state of the panel
 * headers.
 *
 * Revision 3.20  2005/09/16 18:11:40  sueh
 * bug# 532 Added setParameters(SplitcombineParam).
 *
 * Revision 3.19  2005/08/11 23:59:39  sueh
 * bug# 711  Get rid of duplicate code by running the 3dmods from a private
 * function called run3dmod(String, Run3dmodMenuOptions).  It can be
 * called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and the
 * action function.
 *
 * Revision 3.18  2005/08/10 20:47:01  sueh
 * bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * of MultiLineButton.
 *
 * Revision 3.17  2005/08/09 20:53:08  sueh
 * bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * MultiLineButton from JButton.
 *
 * Revision 3.16  2004/12/02 20:42:08  sueh
 * bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * the join guide.  Need to specify the guide to anchor.
 *
 * Revision 3.15  2004/11/20 00:04:03  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 3.14.2.1  2004/10/11 02:17:32  sueh
 * bug# 520 Passed the manager to the ContextPopup object in order to get
 * the propertyUserDir.
 *
 * Revision 3.14  2004/08/31 17:41:03  sueh
 * Bug# 542 Adding binning warning label to X, Y, and Z min and max
 * fields.  Adding function to turn on warning label.
 *
 * Revision 3.13  2004/06/25 23:24:52  sueh
 * bug# 485 fixed warning
 *
 * Revision 3.12  2004/06/17 21:48:18  sueh
 * bug# 474 UIUtilities.setButtonSizeAll() causes this, called
 * UIUtilities.setButtonSize(AbstractButton), since there is only
 * one button on the panel in this case.
 *
 * Revision 3.11  2004/06/15 21:37:16  rickg
 * Bug #383 Correct synchronization of solvematch sub-panel
 *
 * Revision 3.10  2004/06/14 23:39:53  rickg
 * Bug #383 Transitioned to using solvematch
 *
 * Revision 3.9  2004/06/13 17:03:23  rickg
 * Solvematch mid change
 *
 * Revision 3.8  2004/05/11 21:48:05  sueh
 * bug# 302 enabling/disabling matching models and patch region
 * in Setup during sync
 *
 * Revision 3.7  2004/05/11 20:54:41  sueh
 * bug# 302 adding InitialCombineValues and FinalCombineValues interface
 * standardizing synchronization
 *
 * Revision 3.6  2004/05/05 22:26:01  sueh
 * bug# 416 moving binned by 2 checkbox to above matching models
 * button
 *
 * Revision 3.5  2004/05/03 22:26:52  sueh
 * bug# 416 Adding Bin By 2 checkbox.  Passing tab identifier to
 * imodMatchingModel so that checkbox settings can be copied between
 * Setup and Initial tabs when the Matching Models button is pressed.
 *
 * Revision 3.4  2004/03/06 00:29:32  sueh
 * bug# 318 add maxZMax - getParameters, setParameters, not displayed, used to
 * validate ZMax in CombineParams
 *
 * Revision 3.3  2004/03/05 18:20:59  sueh
 * bug# 250 change setUseMatchingModels() - set to Both Sides when
 * Use Model is turned off, this is the default in the com script creation
 *
 * Revision 3.2  2004/02/27 20:02:46  sueh
 * bug# 250 added getUseMatchingModels()
 * added setUseMatchingModels()
 * change updateUseFiducialModel() - stop updating initial panel
 *
 * Revision 3.1  2004/01/30 22:45:18  sueh
 * bug# 356 Changing buttons with html labels to
 * MultiLineButton and MultiLineToggleButton
 *
 * Revision 3.0  2003/11/07 23:19:01  rickg
 * Version 1.0.0
 *
 * Revision 2.18  2003/11/05 21:09:18  rickg
 * Bug# 175 Swap patch parameter x and y min and max if match to state changes.
 *
 * Revision 2.17  2003/11/05 19:56:58  rickg
 * Bug# 300 Selecting matching models on setup patch now
 * selects matching models on initial page
 *
 * Revision 2.16  2003/11/05 19:40:47  rickg
 * Bug# 351 added warning label regarding creating scripts
 *
 * Revision 2.15  2003/10/30 01:43:44  rickg
 * Bug# 338 Remapped context menu entries
 *
 * Revision 2.14  2003/10/29 20:57:38  rickg
 * Bug# 297 Tooltips
 *
 * Revision 2.13  2003/10/21 23:42:54  rickg
 * Changed imod buttons to non multiline
 *
 * Revision 2.12  2003/10/20 18:53:30  rickg
 * Changed patch region and start combine buttons to MultilineToggles
 * Logic for checking if com scripts exist.
 * Revision 2.11 2003/10/18 00:53:22 rickg
 * Added multiline toggle button for matching models Updated matching model
 * button state in setParameters method
 * 
 * <p>
 * Revision 2.10 2003/10/15 22:48:03 rickg
 * <p>
 * Added create matching models button
 * <p>
 * Label changes
 * <p>
 * Button size changes
 * <p>
 * <p>
 * Revision 2.9 2003/10/15 16:56:56 rickg
 * <p>
 * Bug# 294 Label changes
 * <p>
 * <p>
 * Revision 2.8 2003/09/25 23:29:28 rickg
 * <p>
 * Bug #246 fixed to the appropriate combine method in the app manager
 * <p>
 * <p>
 * Revision 2.7 2003/09/08 05:49:07 rickg
 * <p>
 * Method name change for opening the complete volume
 * <p>
 * <p>
 * Revision 2.6 2003/06/05 04:43:20 rickg
 * <p>
 * Added create patch region model button
 * <p>
 * <p>
 * Revision 2.5 2003/04/28 23:25:25 rickg
 * <p>
 * Changed visible imod references to 3dmod
 * <p>
 * <p>
 * Revision 2.4 2003/03/20 17:46:21 rickg
 * <p>
 * Added right button context menu
 * <p>
 * <p>
 * Revision 2.3 2003/03/18 00:32:33 rickg
 * <p>
 * combine development in progress
 * <p>
 * <p>
 * Revision 2.2 2003/02/24 23:49:36 rickg
 * <p>
 * Panel layout for combination dialog
 * <p>
 * Changed borders
 * <p>
 * <p>
 * Revision 2.1 2003/01/29 20:42:55 rickg
 * <p>
 * Swtiched checkbox to jcheckbox
 * <p>
 * <p>
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * <p>
 * Single window merge to main branch
 * <p>
 * <p>
 * Revision 1.6.2.1 2003/01/24 18:43:37 rickg
 * <p>
 * Single window GUI layout initial revision
 * <p>
 * <p>
 * Revision 1.6 2003/01/08 21:40:46 rickg
 * <p>
 * Added transferfid.log to the context menu
 * <p>
 * <p>
 * Revision 1.5 2002/11/14 21:18:37 rickg
 * <p>
 * Added anchors into the tomoguide
 * <p>
 * <p>
 * Revision 1.4 2002/11/14 04:41:11 rickg
 * <p>
 * Fined matchorwarp man page entry
 * <p>
 * <p>
 * Revision 1.3 2002/10/09 00:06:28 rickg
 * <p>
 * Added getting and setting of patch boundary parameters
 * <p>
 * <p>
 * Revision 1.2 2002/10/07 22:31:18 rickg
 * <p>
 * removed unused imports
 * <p>
 * reformat after emacs trashed it
 * <p>
 * <p>
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * <p>
 * Initial CVS entry, basic functionality not including combining
 * <p>
 * </p>
 */
public final class SetupCombinePanel implements ContextMenu,
    InitialCombineFields, FinalCombineFields, Run3dmodButtonContainer,
    Expandable {
  public static final String rcsid = "$Id$";

  private static final String TOMOGRAM_SIZE_CHECK_STRING = " - check min and max values";
  private static final String TOMOGRAM_SIZE_CHANGED_STRING = "THE TOMOGRAM HAS CHANGED";
  private static final String TOMOGRAM_SIZE_UNEQUAL_STRING = "A AND B AREN'T THE SAME SIZE IN X AND Y";

  private TomogramCombinationDialog tomogramCombinationDialog;
  private ApplicationManager applicationManager;

  boolean matchBtoA;

  private EtomoPanel pnlRoot = new EtomoPanel();
  private BeveledBorder brdrContent = new BeveledBorder(
      "Combination Parameters");

  private EtomoPanel pnlToSelector = new EtomoPanel();
  private ButtonGroup bgToSelector = new ButtonGroup();
  private JPanel pnlRBToSelector = new JPanel();
  private JLabel lblEffectWarning = new JLabel(
      "You must create new combine scripts for some changes in these parameters to take effect.");
  private RadioButton rbBtoA = new RadioButton("Match the B tomogram to A");
  private RadioButton rbAtoB = new RadioButton("Match the A tomogram to B");

  private SolvematchPanel pnlSolvematch;

  private EtomoPanel pnlPatchParams = new EtomoPanel();
  private JPanel pnlPatchParamsBody = new JPanel();
  private RadioButton rbSmallPatch = new RadioButton("Small patches");
  private RadioButton rbMediumPatch = new RadioButton("Medium patches");
  private RadioButton rbLargePatch = new RadioButton("Large patches");
  private ButtonGroup bgPatchSize = new ButtonGroup();
  private JPanel pnlPatchRegionModel = new JPanel();
  private CheckBox cbPatchRegionModel = new CheckBox("Use patch region model");
  private Run3dmodButton btnPatchRegionModel = Run3dmodButton.get3dmodInstance(
      "Create/Edit Patch Region Model", this);

  private JPanel pnlPatchRegion = new JPanel();
  private final EtomoPanel pnlVolcombineControls = new EtomoPanel();
  private final JPanel pnlVolcombineControlsBody = new JPanel();
  private LabeledTextField ltfXMin = new LabeledTextField("X axis min: ");
  private LabeledTextField ltfXMax = new LabeledTextField("X axis max: ");
  private LabeledTextField ltfYMin = new LabeledTextField("Y axis min: ");
  private LabeledTextField ltfYMax = new LabeledTextField("Y axis max: ");
  private LabeledTextField ltfZMin = new LabeledTextField(
      ConstCombineParams.PATCH_Z_MIN_LABEL + ": ");
  private LabeledTextField ltfZMax = new LabeledTextField(
      ConstCombineParams.PATCH_Z_MAX_LABEL + ": ");
  private int maxZMax = 0;

  private EtomoPanel pnlTempDirectory = new EtomoPanel();
  private JPanel pnlTempDirectoryBody = new JPanel();
  private LabeledTextField ltfTempDirectory = new LabeledTextField(
      "Temporary directory: ");
  private CheckBox cbManualCleanup = new CheckBox("Manual cleanup");

  private JPanel pnlButton = new JPanel();
  private Run3dmodButton btnImodVolumeA = Run3dmodButton.get3dmodInstance(
      "3dmod Volume A", this);
  private Run3dmodButton btnImodVolumeB = Run3dmodButton.get3dmodInstance(
      "3dmod Volume B", this);
  private final MultiLineButton btnCreate;
  private final Run3dmodButton btnCombine;
  private JLabel binningWarning = new JLabel();
  private final PanelHeader toSelectorHeader;
  private final PanelHeader patchParamsHeader;
  private final PanelHeader tempDirectoryHeader;
  private final PanelHeader volcombineHeader;
  private final CheckBox cbNoVolcombine = new CheckBox(
      FinalCombinePanel.NO_VOLCOMBINE_TITLE);
  private final CheckBox cbParallelProcess;
  private final SetupCombineActionListener actionListener;
  private final JLabel lTomogramSizeWarning = new JLabel();
  private final MultiLineButton btnDefaults = new MultiLineButton("Defaults");
  private final DialogType dialogType;

  /**
   * Default constructor
   */
  public SetupCombinePanel(TomogramCombinationDialog parent,
      ApplicationManager appMgr, DialogType dialogType) {
    tomogramCombinationDialog = parent;
    applicationManager = appMgr;
    this.dialogType = dialogType;
    btnCreate = (MultiLineButton) appMgr.getProcessResultDisplayFactory(
        AxisID.ONLY).getCreateCombine();
    btnCombine = (Run3dmodButton) appMgr.getProcessResultDisplayFactory(
        AxisID.ONLY).getCombine();
    btnCombine.setContainer(this);
    //  Create the matching direction selector panel
    lblEffectWarning.setAlignmentX(Component.CENTER_ALIGNMENT);
    rbAtoB.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbBtoA.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgToSelector.add(rbAtoB.getAbstractButton());
    bgToSelector.add(rbBtoA.getAbstractButton());
    pnlToSelector.setBorder(BorderFactory.createEtchedBorder());
    toSelectorHeader = PanelHeader.getInstance(
        "Tomogram Matching Relationship", this, dialogType);
    pnlToSelector.setLayout(new BoxLayout(pnlToSelector, BoxLayout.Y_AXIS));
    pnlRBToSelector.setLayout(new BoxLayout(pnlRBToSelector, BoxLayout.Y_AXIS));
    pnlRBToSelector.add(rbBtoA.getComponent());
    pnlRBToSelector.add(rbAtoB.getComponent());
    pnlToSelector.add(toSelectorHeader);
    pnlToSelector.add(pnlRBToSelector);
    pnlToSelector.add(Box.createHorizontalGlue());

    // Create the solvematch panel
    pnlSolvematch = SolvematchPanel.getInstance(tomogramCombinationDialog,
        TomogramCombinationDialog.lblSetup, appMgr,
        ReconScreenState.COMBINE_SETUP_SOLVEMATCH_HEADER_GROUP, dialogType);
    // pnlSolvematch.visibleResidual(false);

    //  Create the patch parmeters panel
    rbSmallPatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbMediumPatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbLargePatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgPatchSize.add(rbSmallPatch.getAbstractButton());
    bgPatchSize.add(rbMediumPatch.getAbstractButton());
    bgPatchSize.add(rbLargePatch.getAbstractButton());
    pnlPatchParamsBody.setLayout(new BoxLayout(pnlPatchParamsBody,
        BoxLayout.Y_AXIS));
    pnlPatchParamsBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchParamsBody.add(lTomogramSizeWarning);
    lTomogramSizeWarning.setForeground(ProcessControlPanel.colorNotStarted);
    lTomogramSizeWarning.setVisible(false);
    lTomogramSizeWarning.setAlignmentX(Component.CENTER_ALIGNMENT);
    JPanel pnlPatchRB = new JPanel();
    pnlPatchRB.setLayout(new BoxLayout(pnlPatchRB, BoxLayout.Y_AXIS));
    pnlPatchRB.add(rbSmallPatch.getComponent());
    pnlPatchRB.add(rbMediumPatch.getComponent());
    pnlPatchRB.add(rbLargePatch.getComponent());

    pnlPatchRegionModel.setLayout(new BoxLayout(pnlPatchRegionModel,
        BoxLayout.X_AXIS));
    pnlPatchRegionModel.add(pnlPatchRB);
    pnlPatchRegionModel.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlPatchRegionModel.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlPatchRegionModel.add(cbPatchRegionModel);
    pnlPatchRegionModel.add(btnPatchRegionModel.getComponent());
    pnlPatchParamsBody.add(pnlPatchRegionModel);
    binningWarning.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlPatchParamsBody.add(binningWarning);
    btnPatchRegionModel.setSize();

    //  Patch boundary
    pnlPatchRegion.setLayout(new GridLayout(2, 3, 10, 10));
    pnlPatchRegion.add(ltfXMin.getContainer());
    pnlPatchRegion.add(ltfYMin.getContainer());
    pnlPatchRegion.add(ltfZMin.getContainer());
    pnlPatchRegion.add(ltfXMax.getContainer());
    pnlPatchRegion.add(ltfYMax.getContainer());
    pnlPatchRegion.add(ltfZMax.getContainer());
    JPanel pnlDefaults = new JPanel();
    pnlDefaults.setLayout(new BoxLayout(pnlDefaults, BoxLayout.X_AXIS));
    pnlDefaults.add(pnlPatchRegion);
    pnlDefaults.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlDefaults.add(btnDefaults.getComponent());
    btnDefaults.setSize();
    pnlPatchParamsBody.add(pnlDefaults);
    pnlPatchParams.setBorder(BorderFactory.createEtchedBorder());
    patchParamsHeader = PanelHeader.getInstance(
        "Patch Parameters for Refining Alignment", this, dialogType);
    pnlPatchParams.setLayout(new BoxLayout(pnlPatchParams, BoxLayout.Y_AXIS));
    pnlPatchParams.add(patchParamsHeader);
    pnlPatchParams.add(pnlPatchParamsBody);

    //  Create the volcombine controls panel
    pnlVolcombineControls.setLayout(new BoxLayout(pnlVolcombineControls,
        BoxLayout.Y_AXIS));
    pnlVolcombineControls.setBorder(BorderFactory.createEtchedBorder());
    volcombineHeader = PanelHeader.getInstance("Volcombine Controls", this,
        dialogType);
    pnlVolcombineControls.add(volcombineHeader);
    pnlVolcombineControlsBody.setLayout(new BoxLayout(
        pnlVolcombineControlsBody, BoxLayout.Y_AXIS));
    cbParallelProcess = new CheckBox(
        tomogramCombinationDialog.parallelProcessCheckBoxText);
    pnlVolcombineControlsBody.add(cbParallelProcess);
    pnlVolcombineControlsBody.add(cbNoVolcombine);
    UIUtilities.alignComponentsX(pnlVolcombineControlsBody,
        Component.CENTER_ALIGNMENT);
    pnlVolcombineControls.add(pnlVolcombineControlsBody);

    //  Create the temporary storage panel
    pnlTempDirectoryBody.setLayout(new BoxLayout(pnlTempDirectoryBody,
        BoxLayout.Y_AXIS));
    pnlTempDirectoryBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlTempDirectoryBody.add(ltfTempDirectory.getContainer());
    pnlTempDirectoryBody.add(cbManualCleanup);

    pnlTempDirectory.setBorder(BorderFactory.createEtchedBorder());
    tempDirectoryHeader = PanelHeader.getInstance("Intermediate Data Storage",
        this, dialogType);
    pnlTempDirectory
        .setLayout(new BoxLayout(pnlTempDirectory, BoxLayout.Y_AXIS));
    pnlTempDirectory.add(tempDirectoryHeader);
    pnlTempDirectory.add(pnlTempDirectoryBody);

    //  Bind the buttons to the action listener
    actionListener = new SetupCombineActionListener(this);
    btnPatchRegionModel.addActionListener(actionListener);
    btnImodVolumeA.addActionListener(actionListener);
    btnImodVolumeB.addActionListener(actionListener);
    btnCreate.addActionListener(actionListener);
    btnCombine.addActionListener(actionListener);
    btnDefaults.addActionListener(actionListener);
    cbParallelProcess.addActionListener(actionListener);

    //  Bind the radio buttons to the action listener
    RBMatchToListener rbMatchToListener = new RBMatchToListener(this);
    rbAtoB.addActionListener(rbMatchToListener);
    rbBtoA.addActionListener(rbMatchToListener);

    // Bind the patch region model check box to its action listener
    CBPatchListener cbPatchListener = new CBPatchListener(this);
    cbPatchRegionModel.addActionListener(cbPatchListener);

    //  Button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodVolumeA.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodVolumeB.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnCreate.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnCombine.getComponent());
    pnlButton.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlButton, UIParameters.INSTANCE
        .getButtonDimension());

    pnlToSelector.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(brdrContent.getBorder());

    pnlRoot.add(lblEffectWarning);
    pnlRoot.add(pnlToSelector);
    pnlRoot.add(pnlSolvematch.getContainer());
    pnlRoot.add(pnlPatchParams);
    pnlRoot.add(pnlVolcombineControls);
    pnlRoot.add(pnlTempDirectory);
    pnlRoot.add(Box.createVerticalGlue());
    UIUtilities.addWithYSpace(pnlRoot, pnlButton);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);
    pnlSolvematch.updateUseFiducialModel();
    updatePatchRegionModel();
    //updateStartCombine();
    setToolTipText();
  }

  static ProcessResultDisplay getCreateCombineDisplay(DialogType dialogType) {
    return MultiLineButton.getToggleButtonInstance("Create Combine Scripts",
        dialogType);
  }

  static ProcessResultDisplay getCombineDisplay(DialogType dialogType) {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Start Combine",
        dialogType);
  }

  public Container getContainer() {
    return pnlRoot;
  }

  void show(boolean enableCombine) {
    pnlSolvematch.show();
    updateTomogramSizeWarning(enableCombine);
  }

  void setDeferred3dmodButtons() {
    btnCombine.setDeferred3dmodButton(tomogramCombinationDialog
        .getImodCombinedButton());
    pnlSolvematch.setDeferred3dmodButtons();
  }

  private boolean isTomogramSizeChanged() {
    AxisID toAxisID = matchBtoA ? AxisID.FIRST : AxisID.SECOND;
    return !applicationManager.getState().getTomogramSize(toAxisID).equals(
        applicationManager.getTomogramSize(toAxisID));
  }

  private void updateTomogramSizeWarning(boolean enableCombine) {
    boolean changed = isTomogramSizeChanged();
    boolean different = false;
    if (!enableCombine) {
      AxisID toAxisID = matchBtoA ? AxisID.FIRST : AxisID.SECOND;
      MRCHeader toMrcHeader = MRCHeader.getInstance(applicationManager
          .getPropertyUserDir(), DatasetFiles.getTomogramName(
          applicationManager, toAxisID), AxisID.ONLY, applicationManager.getManagerKey());
      AxisID fromAxisID = matchBtoA ? AxisID.SECOND : AxisID.FIRST;
      MRCHeader fromMrcHeader = MRCHeader.getInstance(applicationManager
          .getPropertyUserDir(), DatasetFiles.getTomogramName(
          applicationManager, fromAxisID), AxisID.ONLY, applicationManager.getManagerKey());
      try {
        if (toMrcHeader.read()) {
          fromMrcHeader.read();
          if (toMrcHeader.getNColumns() != fromMrcHeader.getNSections()
              || toMrcHeader.getNSections() != fromMrcHeader.getNColumns()) {
            different = true;
          }
        }
      }
      catch (InvalidParameterException e) {
        e.printStackTrace();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    if (!changed && !different) {
      lTomogramSizeWarning.setVisible(false);
      return;
    }
    StringBuffer buffer = new StringBuffer();
    if (changed) {
      buffer.append(TOMOGRAM_SIZE_CHANGED_STRING);
      if (different) {
        buffer.append(" & ");
      }
    }
    if (different) {
      buffer.append(TOMOGRAM_SIZE_UNEQUAL_STRING);
    }
    buffer.append(TOMOGRAM_SIZE_CHECK_STRING);
    lTomogramSizeWarning.setText(buffer.toString());
    lTomogramSizeWarning.setVisible(true);
  }

  ProcessResultDisplay getCombineResultDisplay() {
    return btnCombine;
  }

  void getParameters(MetaData metaData) {
    metaData.setCombineVolcombineParallel(cbParallelProcess.isSelected());
  }

  void updateDisplay(boolean enableCombine) {
    btnCombine.setEnabled(enableCombine);
    updateTomogramSizeWarning(enableCombine);
  }

  void setParameters(ConstMetaData metaData) {
    CpuAdoc cpuAdoc = CpuAdoc.getInstance(AxisID.ONLY, applicationManager
        .getPropertyUserDir(), applicationManager.getManagerKey());
    //Parallel processing is optional in tomogram reconstruction, so only use it
    //if the user set it up.
    boolean validAutodoc = cpuAdoc.isAvailable();
    ConstEtomoNumber combineVolcombineParallel = metaData
        .getCombineVolcombineParallel();
    cbParallelProcess.setEnabled(validAutodoc);
    if (combineVolcombineParallel == null) {
      cbParallelProcess.setSelected(validAutodoc
          && metaData.getDefaultParallel().is());
    }
    else {
      cbParallelProcess.setSelected(validAutodoc
          && combineVolcombineParallel.is());
    }
  }

  public void setNoVolcombine(boolean noVolcombine) {
    cbNoVolcombine.setSelected(noVolcombine);
  }

  public boolean isNoVolcombine() {
    return cbNoVolcombine.isSelected();
  }

  public void setParallel(boolean parallel) {
    cbParallelProcess.setSelected(parallel);
  }

  public void setParallelEnabled(boolean parallelEnabled) {
    cbParallelProcess.setEnabled(parallelEnabled);
  }

  public boolean usingParallelProcessing() {
    return cbParallelProcess.isEnabled() && cbParallelProcess.isSelected();
  }

  public boolean isParallel() {
    return cbParallelProcess.isSelected();
  }

  public boolean isParallelEnabled() {
    return cbParallelProcess.isEnabled();
  }

  public boolean isUseCorrespondingPoints() {
    return pnlSolvematch.isUseCorrespondingPoints();
  }

  public void setUseCorrespondingPoints(boolean use) {
    pnlSolvematch.setUseCorrespondingPoints(use);
  }

  void getParameters(ReconScreenState screenState) {
    toSelectorHeader.getState(screenState
        .getCombineSetupToSelectorHeaderState());
    pnlSolvematch.getHeader().getState(
        screenState.getCombineSetupSolvematchHeaderState());
    patchParamsHeader.getState(screenState
        .getCombineSetupPatchcorrHeaderState());
    volcombineHeader.getState(screenState
        .getCombineSetupVolcombineHeaderState());
    tempDirectoryHeader.getState(screenState
        .getCombineSetupTempDirHeaderState());
  }

  void setParameters(ReconScreenState screenState) {
    toSelectorHeader.setState(screenState
        .getCombineSetupToSelectorHeaderState());
    pnlSolvematch.getHeader().setState(
        screenState.getCombineSetupSolvematchHeaderState());
    patchParamsHeader.setState(screenState
        .getCombineSetupPatchcorrHeaderState());
    volcombineHeader.setState(screenState
        .getCombineSetupVolcombineHeaderState());
    tempDirectoryHeader.setState(screenState
        .getCombineSetupTempDirHeaderState());
    btnCreate.setButtonState(screenState.getButtonState(btnCreate
        .getButtonStateKey()));
    btnCombine.setButtonState(screenState.getButtonState(btnCombine
        .getButtonStateKey()));
  }

  public boolean isEnabled() {
    return true;
  }

  public MatchMode getMatchMode() {
    if (rbBtoA.isSelected()) {
      return MatchMode.B_TO_A;
    }
    return MatchMode.A_TO_B;
  }

  public void setMatchMode(MatchMode matchMode) {
    if (matchMode == null) {
      return;
    }
    setBtoA(matchMode);
  }

  void setVisible(boolean visible) {
    lblEffectWarning.setVisible(visible);
    pnlToSelector.setVisible(visible);
    pnlSolvematch.setVisible(visible);
    pnlPatchParams.setVisible(visible);
    pnlVolcombineControls.setVisible(visible);
    pnlTempDirectory.setVisible(visible);
  }
  
  public void expand(final GlobalExpandButton button) {
  }

  public void expand(ExpandButton button) {
    if (toSelectorHeader.equalsOpenClose(button)) {
      pnlRBToSelector.setVisible(button.isExpanded());
    }
    else if (patchParamsHeader.equalsOpenClose(button)) {
      pnlPatchParamsBody.setVisible(button.isExpanded());
    }
    else if (volcombineHeader.equalsOpenClose(button)) {
      pnlVolcombineControlsBody.setVisible(button.isExpanded());
    }
    else if (tempDirectoryHeader.equalsOpenClose(button)) {
      pnlTempDirectoryBody.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(AxisID.ONLY, applicationManager);
  }

  private void setBtoA(MatchMode matchMode) {
    if (matchMode == null || matchMode == MatchMode.B_TO_A) {
      rbBtoA.setSelected(true);
      matchBtoA = true;
    }
    else {
      rbAtoB.setSelected(true);
      matchBtoA = false;
    }
  }

  /**
   * Set the parameters of the panel using the combineParams object
   * @param combineParams
   */
  public void setParameters(ConstCombineParams combineParams) {
    MatchMode matchMode = combineParams.getMatchMode();
    setBtoA(matchMode);
    pnlSolvematch.setParameters(combineParams);

    if (combineParams.getPatchSize() == CombinePatchSize.SMALL) {
      rbSmallPatch.setSelected(true);
    }
    if (combineParams.getPatchSize() == CombinePatchSize.MEDIUM) {
      rbMediumPatch.setSelected(true);
    }
    if (combineParams.getPatchSize() == CombinePatchSize.LARGE) {
      rbLargePatch.setSelected(true);
    }
    cbPatchRegionModel.setSelected(combineParams.usePatchRegionModel());
    ltfXMin.setText(combineParams.getPatchXMin());
    ltfXMax.setText(combineParams.getPatchXMax());
    ltfYMin.setText(combineParams.getPatchYMin());
    ltfYMax.setText(combineParams.getPatchYMax());
    ltfZMin.setText(combineParams.getPatchZMin());
    ltfZMax.setText(combineParams.getPatchZMax());
    maxZMax = combineParams.getMaxPatchZMax();

    ltfTempDirectory.setText(combineParams.getTempDirectory());
    cbManualCleanup.setSelected(combineParams.getManualCleanup());

    pnlSolvematch.updateUseFiducialModel();
    updatePatchRegionModel();
    //updateStartCombine();
  }

  /**
   * Get the cobineParams from the panel
   * @param combineParams
   * @throws NumberFormatException
   */
  public void getParameters(CombineParams combineParams)
      throws NumberFormatException {
    String badParameter = "unknown";
    try {
      combineParams.setMatchMode(rbBtoA.isSelected());
      pnlSolvematch.getParameters(combineParams);

      if (rbSmallPatch.isSelected()) {
        combineParams.setPatchSize(CombinePatchSize.SMALL);
      }
      if (rbMediumPatch.isSelected()) {
        combineParams.setPatchSize(CombinePatchSize.MEDIUM);
      }
      if (rbLargePatch.isSelected()) {
        combineParams.setPatchSize(CombinePatchSize.LARGE);
      }

      if (cbPatchRegionModel.isSelected()) {
        combineParams.setDefaultPatchRegionModel();
      }
      else {
        combineParams.setPatchRegionModel("");
      }

      badParameter = ltfXMin.getLabel();
      combineParams.setPatchXMin(Integer.parseInt(ltfXMin.getText()));
      badParameter = ltfXMax.getLabel();
      combineParams.setPatchXMax(Integer.parseInt(ltfXMax.getText()));
      badParameter = ltfYMin.getLabel();
      combineParams.setPatchYMin(Integer.parseInt(ltfYMin.getText()));
      badParameter = ltfYMax.getLabel();
      combineParams.setPatchYMax(Integer.parseInt(ltfYMax.getText()));
      badParameter = ltfZMin.getLabel();
      combineParams.setPatchZMin(ltfZMin.getText());
      badParameter = ltfZMax.getLabel();
      combineParams.setPatchZMax(ltfZMax.getText());
      combineParams.setMaxPatchZMax(maxZMax);
      badParameter = "unknown";

      combineParams.setTempDirectory(ltfTempDirectory.getText());

      combineParams.setManualCleanup(cbManualCleanup.isSelected());
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }

  }

  public void setUsePatchRegionModel(boolean usePatchRegionModel) {
    cbPatchRegionModel.setSelected(usePatchRegionModel);
    updatePatchRegionModel();
  }

  public boolean isUsePatchRegionModel() {
    return cbPatchRegionModel.isSelected();
  }

  public void setXMin(String xMin) {
    ltfXMin.setText(xMin);
  }

  public String getXMin() {
    return ltfXMin.getText();
  }

  public void setXMax(String xMax) {
    ltfXMax.setText(xMax);
  }

  public String getXMax() {
    return ltfXMax.getText();
  }

  public void setYMin(String yMin) {
    ltfYMin.setText(yMin);
  }

  public String getYMin() {
    return ltfYMin.getText();
  }

  public void setYMax(String yMax) {
    ltfYMax.setText(yMax);
  }

  public String getYMax() {
    return ltfYMax.getText();
  }

  public void setZMin(String zMin) {
    ltfZMin.setText(zMin);
  }

  public String getZMin() {
    return ltfZMin.getText();
  }

  ProcessResultDisplay getCombineProcessResultDisplay() {
    return btnCombine;
  }

  public void setZMax(String zMax) {
    ltfZMax.setText(zMax);
  }

  public String getZMax() {
    return ltfZMax.getText();
  }

  // InitialiCombineFields interface pass-thru
  public FiducialMatch getSurfacesOrModels() {
    return pnlSolvematch.getSurfacesOrModels();
  }

  public void setSurfacesOrModels(FiducialMatch state) {
    pnlSolvematch.setSurfacesOrModels(state);
  }

  public void setBinningWarning(boolean binningWarning) {
    if (binningWarning) {
      this.binningWarning
          .setText("WARNING:  Coordinates must be selected from an unbinned 3dmod");
    }
    else {
      this.binningWarning.setText("");
    }
  }

  public boolean isBinBy2() {
    return pnlSolvematch.isBinBy2();
  }

  public void setBinBy2(boolean state) {
    pnlSolvematch.setBinBy2(state);
  }

  public void setFiducialMatchListA(String fiducialMatchListA) {
    pnlSolvematch.setFiducialMatchListA(fiducialMatchListA);
  }

  public void setUseList(String useList) {
    pnlSolvematch.setUseList(useList);
  }

  public String getUseList() {
    return pnlSolvematch.getUseList();
  }

  public String getFiducialMatchListA() {
    return pnlSolvematch.getFiducialMatchListA();
  }

  public void setFiducialMatchListB(String fiducialMatchListB) {
    pnlSolvematch.setFiducialMatchListB(fiducialMatchListB);
  }

  public String getFiducialMatchListB() {
    return pnlSolvematch.getFiducialMatchListB();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  private void buttonAction(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    //  Synchronize this panel with the others
    tomogramCombinationDialog.synchronize(TomogramCombinationDialog.lblSetup,
        true);
    if (command.equals(btnCreate.getActionCommand())) {
      updateTomogramSizeWarning(applicationManager
          .createCombineScripts(btnCreate));
      tomogramCombinationDialog.updateDisplay();
    }
    else if (command.equals(btnCombine.getActionCommand())) {
      applicationManager.combine(btnCombine, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType);
    }
    else if (command.equals(cbParallelProcess.getActionCommand())) {
      tomogramCombinationDialog.updateParallelProcess();
    }
    else if (command.equals(btnDefaults.getActionCommand())) {
      resetXandY();
    }
    else if (command.equals(btnPatchRegionModel.getActionCommand())) {
      applicationManager.imodPatchRegionModel(run3dmodMenuOptions);
    }
    else if (command.equals(btnImodVolumeA.getActionCommand())) {
      applicationManager.imodFullVolume(AxisID.FIRST, run3dmodMenuOptions);
    }
    else if (command.equals(btnImodVolumeB.getActionCommand())) {
      applicationManager.imodFullVolume(AxisID.SECOND, run3dmodMenuOptions);
    }
  }

  private void resetXandY() {
    AxisID toAxisID = matchBtoA ? AxisID.FIRST : AxisID.SECOND;
    MRCHeader mrcHeader = MRCHeader.getInstance(applicationManager
        .getPropertyUserDir(), DatasetFiles.getTomogramName(applicationManager,
        toAxisID), AxisID.ONLY, applicationManager.getManagerKey());
    try {
      if (!mrcHeader.read()) {
        return;
      }
      int xyborder = CombineParams.getXYBorder(mrcHeader);
      ltfXMin.setText(xyborder);
      ltfXMax.setText(mrcHeader.getNColumns() - xyborder);
      ltfYMin.setText(xyborder);
      ltfYMax.setText(mrcHeader.getNSections() - xyborder);
      tomogramCombinationDialog.synchronizeFromCurrentTab();
    }
    catch (IOException e) {
    }
    catch (InvalidParameterException e) {
    }
  }

  /**
   * Manage radio button action events
   * 
   * @param event
   */
  protected void rbMatchToAction(ActionEvent event) {
    updateMatchTo();
    tomogramCombinationDialog.updateDisplay();
  }

  boolean isChanged(TomogramState state) {
    if (!state.getCombineScriptsCreated().is()) {
      return true;
    }
    MatchMode scriptMatchMode = state.getCombineMatchMode();
    return scriptMatchMode == null
        || (scriptMatchMode == MatchMode.A_TO_B && !rbAtoB.isSelected())
        || (scriptMatchMode == MatchMode.B_TO_A && !rbBtoA.isSelected())
        || isTomogramSizeChanged();
  }

  private void updateMatchTo() {
    //  Swap the X and Y values if the matching state changes 
    if ((matchBtoA && rbAtoB.isSelected())
        || (!matchBtoA && rbBtoA.isSelected())) {
      String temp = ltfXMin.getText();
      ltfXMin.setText(ltfYMin.getText());
      ltfYMin.setText(temp);

      temp = ltfXMax.getText();
      ltfXMax.setText(ltfYMax.getText());
      ltfYMax.setText(temp);
    }

    if (rbAtoB.isSelected()) {
      matchBtoA = false;
    }
    else {
      matchBtoA = true;
    }
  }

  /**
   * Manage patch region check box actions
   * @param event
   */
  protected void cbPatchRegionAction(ActionEvent event) {
    updatePatchRegionModel();
  }

  /**
   * Enable/disable the patch region model button
   */
  private void updatePatchRegionModel() {
    btnPatchRegionModel.setEnabled(cbPatchRegionModel.isSelected());
  }

  /**
   * Enable/disable the start combine button w.r.t. the existence of the scripts
   
   private void updateStartCombine() {
   btnCombine.setEnabled(applicationManager.combineScriptsExist());
   }*/

  /**
   * Right mouse btn context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Solvematch", "Matchshifts", "Patchcrawl3d",
        "Matchorwarp" };
    String[] manPage = { "solvematch.html", "matchshifts.html",
        "patchcrawl3d.html", "matchorwarp.html" };
    String[] logFileLabel = { "Transferfid", "Solvematch", "Patchcorr",
        "Matchorwarp", "Volcombine" };
    String[] logFile = { "transferfid.log", "solvematch.log", "patchcorr.log",
        "matchorwarp.log", "volcombine.log" };

    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent,
        "TOMOGRAM COMBINATION", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager, AxisID.ONLY);
  }

  //	Button action listener
  private final class SetupCombineActionListener implements ActionListener {
    private final SetupCombinePanel adaptee;

    private SetupCombineActionListener(final SetupCombinePanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null, null);
    }
  }

  class RBMatchToListener implements ActionListener {

    SetupCombinePanel adaptee;

    public RBMatchToListener(SetupCombinePanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.rbMatchToAction(event);
    }
  }

  class CBPatchListener implements ActionListener {

    SetupCombinePanel adaptee;

    public CBPatchListener(SetupCombinePanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.cbPatchRegionAction(event);
    }
  }

  /**
   * Initialize the tooltip text
   */
  private void setToolTipText() {
    String text;
    rbBtoA
        .setToolTipText("Transform the B tomogram into the same orientation as the A tomogram.");
    rbAtoB
        .setToolTipText("Transform the A tomogram into the same orientation as the B tomogram.");
    rbSmallPatch
        .setToolTipText("Use small patches for refining the alignment with correlation - "
            + "appropriate for feature-rich tomogram from binned CCD camera images "
            + "or from film.");
    rbMediumPatch
        .setToolTipText("Use medium patches for refining the alignment with correlation - "
            + "appropriate for feature-rich tomogram from unbinned CCD camera "
            + "images.");
    rbLargePatch
        .setToolTipText("Use large patches for refining the alignment with correlation - may be "
            + "needed for tomogram with sparse features.");
    cbPatchRegionModel
        .setToolTipText("Use a model with contours around the areas where patches should be "
            + "correlated to prevent bad patches outside those areas.");
    btnPatchRegionModel
        .setToolTipText("Open the volume being matched to and create the patch region model.");
    ltfXMin
        .setToolTipText("Minimum X coordinate for left edge of correlation patches.");
    ltfXMax
        .setToolTipText("Maximum X coordinate for right edge of correlation patches.");
    ltfYMin
        .setToolTipText("Minimum Y coordinate for upper edge of correlation patches.");
    ltfYMax
        .setToolTipText("Maximum Y coordinate for lower edge of correlation patches.");
    ltfZMin
        .setToolTipText("Minimum Z coordinate for top edge of correlation patches.");
    ltfZMax
        .setToolTipText("Maximum Z coordinate for bottom edge of correlation patches.");
    ltfTempDirectory
        .setToolTipText("Specify a directory on local disk (e.g., /usr/tmp, or /scratch/myarea) "
            + "to avoid writing temporary files over a network.");
    cbManualCleanup
        .setToolTipText("If using a temporary directory, select this option if you will want to "
            + "examine the *.mat file that will be left in it.");
    btnImodVolumeA.setToolTipText("Display tomogram from axis A");
    btnImodVolumeB.setToolTipText("Display tomogram from axis B");
    btnCreate
        .setToolTipText("Run setupcombine to create the com scripts for combining, using the "
            + "current parameters.");
    btnCombine
        .setToolTipText("Start running the combine operation from the beginning.");
    cbNoVolcombine
        .setToolTipText("Stop after running Matchorwarp.  Use the \"Restart at Volcombine\" button to continue.");
    cbParallelProcess
        .setToolTipText(FinalCombinePanel.VOLCOMBINE_PARALLEL_PROCESSING_TOOL_TIP);
  }
}