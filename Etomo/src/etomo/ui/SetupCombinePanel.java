package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.type.AxisID;
import etomo.type.CombinePatchSize;
import etomo.type.FiducialMatch;
import etomo.type.Run3dmodMenuOption;

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
public class SetupCombinePanel
  implements ContextMenu, InitialCombineFields, FinalCombineFields, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  private TomogramCombinationDialog tomogramCombinationDialog;
  private ApplicationManager applicationManager;

  boolean matchBtoA;

  private JPanel pnlRoot = new JPanel();
  private BeveledBorder brdrContent = new BeveledBorder(
    "Combination Parameters");

  private JPanel pnlToSelector = new JPanel();
  private ButtonGroup bgToSelector = new ButtonGroup();
  private JPanel pnlRBToSelector = new JPanel();
  private JLabel lblEffectWarning = new JLabel(
    "You must create new combine scripts for some changes in these parameters to take effect.");
  private JRadioButton rbBtoA = new JRadioButton("Match the B tomogram to A");
  private JRadioButton rbAtoB = new JRadioButton("Match the A tomogram to B");

  private SolvematchPanel pnlSolvematch;

  private JPanel pnlPatchParams = new JPanel();
  private JRadioButton rbSmallPatch = new JRadioButton("Small patches");
  private JRadioButton rbMediumPatch = new JRadioButton("Medium patches");
  private JRadioButton rbLargePatch = new JRadioButton("Large patches");
  private ButtonGroup bgPatchSize = new ButtonGroup();
  private JPanel pnlPatchRegionModel = new JPanel();
  private JCheckBox cbPatchRegionModel = new JCheckBox("Use patch region model");
  private Run3dmodButton btnPatchRegionModel = new Run3dmodButton(
    "<html><b>Create/Edit Patch Region Model</b>", this);

  private JPanel pnlPatchRegion = new JPanel();
  private LabeledTextField ltfXMin = new LabeledTextField("X axis min: ");
  private LabeledTextField ltfXMax = new LabeledTextField("X axis max: ");
  private LabeledTextField ltfYMin = new LabeledTextField("Y axis min: ");
  private LabeledTextField ltfYMax = new LabeledTextField("Y axis max: ");
  private LabeledTextField ltfZMin = new LabeledTextField("Z axis min: ");
  private LabeledTextField ltfZMax = new LabeledTextField("Z axis max: ");
  private int maxZMax = 0;

  private JPanel pnlTempDirectory = new JPanel();
  private LabeledTextField ltfTempDirectory = new LabeledTextField(
    "Temporary directory: ");
  private JCheckBox cbManualCleanup = new JCheckBox("Manual cleanup");

  private JPanel pnlButton = new JPanel();
  private Run3dmodButton btnImodVolumeA = new Run3dmodButton(
    "<html><b>3dmod Volume A</b>", this);
  private Run3dmodButton btnImodVolumeB = new Run3dmodButton(
    "<html><b>3dmod Volume B</b>", this);
  private MultiLineButton btnCreate = MultiLineButton.getToggleButtonInstance(
    "<html><b>Create Combine Scripts</b>");
  private MultiLineButton btnCombine = MultiLineButton.getToggleButtonInstance(
    "<html><b>Start Combine</b>");
  private JLabel binningWarning = new JLabel();

  /**
   * Default constructor
   */
  public SetupCombinePanel(TomogramCombinationDialog parent,
    ApplicationManager appMgr) {

    tomogramCombinationDialog = parent;
    applicationManager = appMgr;

    //  Create the matching direction selector panel
    lblEffectWarning.setAlignmentX(Component.CENTER_ALIGNMENT);
    rbAtoB.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbBtoA.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgToSelector.add(rbAtoB);
    bgToSelector.add(rbBtoA);
    pnlToSelector.setBorder(new EtchedBorder("Tomogram Matching Relationship")
      .getBorder());
    pnlToSelector.setLayout(new BoxLayout(pnlToSelector, BoxLayout.X_AXIS));
    pnlRBToSelector.setLayout(new BoxLayout(pnlRBToSelector, BoxLayout.Y_AXIS));
    pnlRBToSelector.add(rbBtoA);
    pnlRBToSelector.add(rbAtoB);
    pnlToSelector.add(pnlRBToSelector);
    pnlToSelector.add(Box.createHorizontalGlue());

    // Create the solvematch panel
    pnlSolvematch = new SolvematchPanel(tomogramCombinationDialog,
      TomogramCombinationDialog.lblSetup, appMgr);
    pnlSolvematch.visibleResidual(false);

    //  Create the patch parmeters panel
    rbSmallPatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbMediumPatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbLargePatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgPatchSize.add(rbSmallPatch);
    bgPatchSize.add(rbMediumPatch);
    bgPatchSize.add(rbLargePatch);
    pnlPatchParams.setBorder(new EtchedBorder(
      "Patch Parameters for Refining Alignment").getBorder());
    pnlPatchParams.setLayout(new BoxLayout(pnlPatchParams, BoxLayout.Y_AXIS));
    JPanel pnlPatchRB = new JPanel();
    pnlPatchRB.setLayout(new BoxLayout(pnlPatchRB, BoxLayout.Y_AXIS));
    pnlPatchRB.add(rbSmallPatch);
    pnlPatchRB.add(rbMediumPatch);
    pnlPatchRB.add(rbLargePatch);

    pnlPatchRegionModel.setLayout(new BoxLayout(pnlPatchRegionModel,
      BoxLayout.X_AXIS));
    pnlPatchRegionModel.add(pnlPatchRB);
    pnlPatchRegionModel.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlPatchRegionModel.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlPatchRegionModel.add(cbPatchRegionModel);
    pnlPatchRegionModel.add(btnPatchRegionModel.getComponent());
    pnlPatchParams.add(pnlPatchRegionModel);
    pnlPatchParams.add(Box.createRigidArea(FixedDim.x0_y10));
    binningWarning.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlPatchParams.add(binningWarning);
    btnPatchRegionModel.setSize();

    //  Patch boundary
    pnlPatchRegion.setLayout(new GridLayout(2, 3, 10, 10));
    pnlPatchRegion.add(ltfXMin.getContainer());
    pnlPatchRegion.add(ltfYMin.getContainer());
    pnlPatchRegion.add(ltfZMin.getContainer());
    pnlPatchRegion.add(ltfXMax.getContainer());
    pnlPatchRegion.add(ltfYMax.getContainer());
    pnlPatchRegion.add(ltfZMax.getContainer());
    pnlPatchParams.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlPatchParams.add(pnlPatchRegion);

    //  Create the temporary storage panel
    pnlTempDirectory.setBorder(new EtchedBorder("Intermediate Data Storage")
      .getBorder());
    pnlTempDirectory
      .setLayout(new BoxLayout(pnlTempDirectory, BoxLayout.Y_AXIS));
    pnlTempDirectory.add(ltfTempDirectory.getContainer());
    pnlTempDirectory.add(cbManualCleanup);

    //  Bind the buttons to the action listener
    SetupCombineActionListener actionListener = new SetupCombineActionListener(
      this);
    btnPatchRegionModel.addActionListener(actionListener);
    btnImodVolumeA.addActionListener(actionListener);
    btnImodVolumeB.addActionListener(actionListener);
    btnCreate.addActionListener(actionListener);
    btnCombine.addActionListener(actionListener);

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
    UIUtilities.setButtonSizeAll(pnlButton, UIParameters.getButtonDimension());

    pnlToSelector.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(brdrContent.getBorder());

    UIUtilities.addWithSpace(pnlRoot, lblEffectWarning, FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlRoot, pnlToSelector, FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlRoot, pnlSolvematch.getContainer(),
      FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlRoot, pnlPatchParams, FixedDim.x0_y10);
    UIUtilities.addWithSpace(pnlRoot, pnlTempDirectory, FixedDim.x0_y10);
    pnlRoot.add(Box.createVerticalGlue());
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y20));
    UIUtilities.addWithYSpace(pnlRoot, pnlButton);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);
    pnlSolvematch.updateUseFiducialModel();
    updatePatchRegionModel();
    updateStartCombine();
    setToolTipText();
  }

  public Container getContainer() {
    return pnlRoot;
  }

  /**
   * Set the parameters of the panel using the combineParams object
   * @param combineParams
   */
  public void setParameters(ConstCombineParams combineParams) {

    if (combineParams.getMatchBtoA()) {
      rbBtoA.setSelected(true);
      matchBtoA = true;
    }
    else {
      rbAtoB.setSelected(true);
      matchBtoA = false;
    }

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
    updateStartCombine();
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

      combineParams.setMatchBtoA(rbBtoA.isSelected());
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

      badParameter = ltfXMin.getLabel();
      combineParams.setPatchXMin(Integer.parseInt(ltfXMin.getText()));
      badParameter = ltfXMax.getLabel();
      combineParams.setPatchXMax(Integer.parseInt(ltfXMax.getText()));
      badParameter = ltfYMin.getLabel();
      combineParams.setPatchYMin(Integer.parseInt(ltfYMin.getText()));
      badParameter = ltfYMax.getLabel();
      combineParams.setPatchYMax(Integer.parseInt(ltfYMax.getText()));
      badParameter = ltfZMin.getLabel();
      combineParams.setPatchZMin(Integer.parseInt(ltfZMin.getText()));
      badParameter = ltfZMax.getLabel();
      combineParams.setPatchZMax(Integer.parseInt(ltfZMax.getText()));
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
      this.binningWarning.setText(
        "WARNING:  Coordinates must be selected from an unbinned 3dmod");
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

  public String getFiducialMatchListA() {
    return pnlSolvematch.getFiducialMatchListA();
  }

  public void setFiducialMatchListB(String fiducialMatchListB) {
    pnlSolvematch.setFiducialMatchListB(fiducialMatchListB);
  }

  public String getFiducialMatchListB() {
    return pnlSolvematch.getFiducialMatchListB();
  }

  public void run3dmod(Run3dmodButton button, Run3dmodMenuOption menuOption) {
    if (button.equals(btnPatchRegionModel)) {
      applicationManager.imodPatchRegionModel(menuOption);
    }
    else if (button.equals(btnImodVolumeA)) {
      applicationManager.imodFullVolume(AxisID.FIRST, menuOption);
    }
    else if (button.equals(btnImodVolumeB)) {
      applicationManager.imodFullVolume(AxisID.SECOND, menuOption);
    }
  }
  //  Action functions for setup panel buttons
  private void buttonAction(ActionEvent event) {
    //  Synchronize this panel with the others
    tomogramCombinationDialog.synchronize(TomogramCombinationDialog.lblSetup,
      true, TomogramCombinationDialog.ALL_FIELDS);

    String command = event.getActionCommand();
    if (command.equals(btnPatchRegionModel.getActionCommand())) {
      applicationManager.imodPatchRegionModel(Run3dmodMenuOption.NONE);
    }
    if (command.equals(btnImodVolumeA.getActionCommand())) {
      applicationManager.imodFullVolume(AxisID.FIRST, Run3dmodMenuOption.NONE);
    }
    if (command.equals(btnImodVolumeB.getActionCommand())) {
      applicationManager.imodFullVolume(AxisID.SECOND, Run3dmodMenuOption.NONE);
    }
    if (command.equals(btnCreate.getActionCommand())) {
      applicationManager.createCombineScripts();
    }
    if (command.equals(btnCombine.getActionCommand())) {
      applicationManager.combine();
    }
    //  Check the combine scripts state and set the start button accordingly
    updateStartCombine();
  }

  /**
   * Manage radio button action events
   * 
   * @param event
   */
  private void rbMatchToAction(ActionEvent event) {
    updateMatchTo();
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
  private void cbPatchRegionAction(ActionEvent event) {
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
   */
  private void updateStartCombine() {
    btnCombine.setEnabled(applicationManager.combineScriptsExist());
  }

  /**
   * Right mouse btn context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = {"Solvematch", "Matchshifts", "Patchcrawl3d",
        "Matchorwarp"};
    String[] manPage = {"solvematch.html", "matchshifts.html",
        "patchcrawl3d.html", "matchorwarp.html"};
    String[] logFileLabel = {"Transferfid", "Solvematch", "Patchcorr",
        "Matchorwarp", "Volcombine"};
    String[] logFile = {"transferfid.log", "solvematch.log", "patchcorr.log",
        "matchorwarp.log", "volcombine.log"};

    ContextPopup contextPopup = new ContextPopup(pnlRoot, mouseEvent,
      "TOMOGRAM COMBINATION", ContextPopup.TOMO_GUIDE, manPagelabel, manPage, logFileLabel, logFile, applicationManager);
  }
  //	Button action listener
  class SetupCombineActionListener implements ActionListener {

    SetupCombinePanel adaptee;

    public SetupCombineActionListener(SetupCombinePanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
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
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "Transform the B tomogram into the same orientation as the A tomogram.";
    rbBtoA.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Transform the A tomogram into the same orientation as the B tomogram.";
    rbAtoB.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Use small patches for refining the alignment with correlation - "
      + "appropriate for feature-rich tomogram from binned CCD camera images "
      + "or from film.";
    rbSmallPatch.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Use medium patches for refining the alignment with correlation - "
      + "appropriate for feature-rich tomogram from unbinned CCD camera "
      + "images.";
    rbMediumPatch.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Use large patches for refining the alignment with correlation - may be "
      + "needed for tomogram with sparse features.";
    rbLargePatch.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Use a model with contours around the areas where patches should be "
      + "correlated to prevent bad patches outside those areas.";
    cbPatchRegionModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Open the volume being matched to and create the patch region model.";
    btnPatchRegionModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Minimum X coordinate for left edge of correlation patches.";
    ltfXMin.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum X coordinate for right edge of correlation patches.";
    ltfXMax.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Minimum Y coordinate for upper edge of correlation patches.";
    ltfYMin.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum Y coordinate for lower edge of correlation patches.";
    ltfYMax.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Minimum Z coordinate for top edge of correlation patches.";
    ltfZMin.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum Z coordinate for bottom edge of correlation patches.";
    ltfZMax.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Specify a directory on local disk (e.g., /usr/tmp, or /scratch/myarea) "
      + "to avoid writing temporary files over a network.";
    ltfTempDirectory.setToolTipText(tooltipFormatter.setText(text).format());

    text = "If using a temporary directory, select this option if you will want to "
      + "examine the *.mat file that will be left in it.";
    cbManualCleanup.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Display tomogram from axis A";
    btnImodVolumeA.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Display tomogram from axis B";
    btnImodVolumeB.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Run setupcombine to create the com scripts for combining, using the "
      + "current parameters.";
    btnCreate.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Start running the combine operation from the beginning.";
    btnCombine.setToolTipText(tooltipFormatter.setText(text).format());

  }
}