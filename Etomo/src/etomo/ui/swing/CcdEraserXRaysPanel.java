/**
 * <p>Description: Panel to modify the CCD eraser parameters</p>
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
 */

package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.ConstCCDEraserParam;
import etomo.comscript.MakecomfileParam;
import etomo.comscript.TomodataplotsParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.FileType;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

final class CcdEraserXRaysPanel implements ContextMenu, Run3dmodButtonContainer,
    CcdEraserDisplay, Expandable {
  public static final String rcsid = "$Id$";

  static final String ERASE_LABEL = "Create Fixed Stack";
  static final String USE_FIXED_STACK_LABEL = "Use Fixed Stack";

  private final JPanel pnlCCDEraser = new JPanel();
  private final JPanel pnlManualReplacement = new JPanel();
  private final CheckBox cbXrayReplacement = new CheckBox("Automatic x-ray replacement");
  private final LabeledTextField ltfPeakCriterion = new LabeledTextField(
      FieldType.FLOATING_POINT, "Peak criterion:");
  private final LabeledTextField ltfDiffCriterion = new LabeledTextField(
      FieldType.FLOATING_POINT, "Difference criterion:");
  private final LabeledTextField ltfGrowCriterion = new LabeledTextField(
      FieldType.FLOATING_POINT, "Grow criterion:");
  private final LabeledTextField ltfEdgeExclusion = new LabeledTextField(
      FieldType.INTEGER, "Edge exclusion:");
  private final LabeledTextField ltfMaximumRadius = new LabeledTextField(
      FieldType.FLOATING_POINT, "Maximum radius:");
  private final LabeledTextField ltfAnnulusWidth = new LabeledTextField(
      FieldType.FLOATING_POINT, "Annulus width:");
  private final LabeledTextField ltfScanRegionSize = new LabeledTextField(
      FieldType.INTEGER, "XY scan size:");
  private final LabeledTextField ltfScanCriterion = new LabeledTextField(
      FieldType.FLOATING_POINT, "Scan criterion:");
  private final Run3dmodButton btnFindXRays;
  private final Run3dmodButton btnViewXRayModel = Run3dmodButton.get3dmodInstance(
      "View X-ray Model", this);
  private final CheckBox cbManualReplacement = new CheckBox("Manual replacement");
  private final LabeledTextField ltfGlobalReplacementList = new LabeledTextField(
      FieldType.INTEGER_LIST, "All section replacement list: ");
  private final LabeledTextField ltfLocalReplacementList = new LabeledTextField(
      FieldType.INTEGER_LIST, "Line replacement list: ");
  private final LabeledTextField ltfBoundaryReplacementList = new LabeledTextField(
      FieldType.INTEGER_LIST, "Boundary replacement list: ");
  private final Run3dmodButton btnCreateModel = Run3dmodButton.get3dmodInstance(
      "Create Manual Replacement Model", this);
  private final LabeledTextField ltfBorderPixels = new LabeledTextField(
      FieldType.INTEGER, "Border pixels: ");
  private final LabeledTextField ltfPolynomialOrder = new LabeledTextField(
      FieldType.INTEGER, "Polynomial order: ");
  private final CheckBox cbIncludeAdjacentPoints = new CheckBox("Include adjacent points");
  private final Run3dmodButton btnViewErased = Run3dmodButton.get3dmodInstance(
      "View Fixed Stack", this);
  private final MultiLineButton btnClipStatsRaw = new MultiLineButton(
      "Show Min/Max for Raw Stack");
  private final MultiLineButton btnClipStatsFixed = new MultiLineButton(
      "Show Min/Max for Fixed Stack");
  private final LabeledTextField ltfGiantCriterion = new LabeledTextField(
      FieldType.FLOATING_POINT, "Extra-large peak criterion:");
  private final LabeledTextField ltfBigDiffCriterion = new LabeledTextField(
      FieldType.FLOATING_POINT, "Extra-large difference criterion:");
  private final LabeledTextField ltfExtraLargeRadius = new LabeledTextField(
      FieldType.FLOATING_POINT, "Maximum radius of extra-large peak:");
  private final PanelHeader phManualReplacement;
  private final JPanel pnlManualReplacementBody = new JPanel();
  private final JPanel pnlManualButtons = new JPanel();

  private final ApplicationManager applicationManager;
  private final AxisID axisID;
  private final Run3dmodButton btnErase;
  private final MultiLineButton btnReplaceRawStack;
  private final DialogType dialogType;
  private final CCDEraserXRaysActionListener ccdEraserActionListener;

  private CcdEraserXRaysPanel(final ApplicationManager appMgr, final AxisID id,
      final DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    applicationManager = appMgr;
    axisID = id;
    this.dialogType = dialogType;
    globalAdvancedButton.register(this);
    phManualReplacement = PanelHeader.getAdvancedBasicOnlyInstance(
        "Manual Pixel Region Replacement", this, DialogType.PRE_PROCESSING,
        globalAdvancedButton, true);
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    btnErase = (Run3dmodButton) displayFactory.getCreateFixedStack();
    btnErase.setContainer(this);
    btnErase.setDeferred3dmodButton(btnViewErased);
    btnFindXRays = (Run3dmodButton) displayFactory.getFindXRays();
    btnFindXRays.setContainer(this);
    btnFindXRays.setDeferred3dmodButton(btnViewXRayModel);
    btnReplaceRawStack = (MultiLineButton) displayFactory.getUseFixedStack();
    setToolTipText();

    JPanel pnlManualReplacementCheckBox = new JPanel();
    EtomoPanel pnlXRayReplacement = new EtomoPanel();
    pnlXRayReplacement.setLayout(new BoxLayout(pnlXRayReplacement, BoxLayout.Y_AXIS));
    pnlXRayReplacement.setBorder(new EtchedBorder("Automatic X-ray Replacement")
        .getBorder());

    UIUtilities.addWithYSpace(pnlXRayReplacement, cbXrayReplacement);
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfPeakCriterion.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfDiffCriterion.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfMaximumRadius.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfGiantCriterion.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfBigDiffCriterion.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfExtraLargeRadius.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfGrowCriterion.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfEdgeExclusion.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfAnnulusWidth.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfScanRegionSize.getContainer());
    UIUtilities.addWithYSpace(pnlXRayReplacement, ltfScanCriterion.getContainer());

    JPanel pnlXRayButtons = new JPanel();
    pnlXRayButtons.setLayout(new BoxLayout(pnlXRayButtons, BoxLayout.X_AXIS));
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayButtons.add(btnFindXRays.getComponent());
    pnlXRayButtons.add(Box.createHorizontalGlue());
    pnlXRayButtons.add(btnViewXRayModel.getComponent());
    pnlXRayButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlXRayButtons,
        UIParameters.INSTANCE.getButtonDimension());

    UIUtilities.addWithYSpace(pnlXRayReplacement, pnlXRayButtons);

    pnlManualReplacement.setLayout(new BoxLayout(pnlManualReplacement, BoxLayout.Y_AXIS));
    pnlManualReplacement.setBorder(BorderFactory.createEtchedBorder());
    pnlManualReplacement.add(phManualReplacement.getContainer());
    pnlManualReplacement.add(pnlManualReplacementBody);

    pnlManualReplacementBody.setLayout(new BoxLayout(pnlManualReplacementBody,
        BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlManualReplacementBody, pnlManualReplacementCheckBox);
    UIUtilities.addWithYSpace(pnlManualReplacementBody,
        ltfGlobalReplacementList.getContainer());
    UIUtilities.addWithYSpace(pnlManualReplacementBody,
        ltfLocalReplacementList.getContainer());
    UIUtilities.addWithYSpace(pnlManualReplacementBody,
        ltfBoundaryReplacementList.getContainer());

    pnlManualButtons.setLayout(new BoxLayout(pnlManualButtons, BoxLayout.X_AXIS));
    pnlManualButtons.add(Box.createHorizontalGlue());
    pnlManualButtons.add(btnCreateModel.getComponent());
    pnlManualButtons.add(Box.createHorizontalGlue());
    UIUtilities.setButtonSizeAll(pnlManualButtons,
        UIParameters.INSTANCE.getButtonDimension());
    UIUtilities.addWithYSpace(pnlManualReplacementBody, pnlManualButtons);

    pnlManualReplacementCheckBox.setLayout(new BoxLayout(pnlManualReplacementCheckBox,
        BoxLayout.X_AXIS));
    pnlManualReplacementCheckBox.add(cbManualReplacement);
    pnlManualReplacementCheckBox.add(Box.createHorizontalGlue());

    pnlCCDEraser.setLayout(new BoxLayout(pnlCCDEraser, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlCCDEraser, pnlXRayReplacement);
    UIUtilities.addWithYSpace(pnlCCDEraser, pnlManualReplacement);
    UIUtilities.addWithYSpace(pnlCCDEraser, ltfBorderPixels.getContainer());
    UIUtilities.addWithYSpace(pnlCCDEraser, ltfPolynomialOrder.getContainer());
    UIUtilities.addWithYSpace(pnlCCDEraser, cbIncludeAdjacentPoints);

    pnlCCDEraser.add(Box.createRigidArea(FixedDim.x0_y5));
    btnClipStatsRaw.setSize();
    btnClipStatsFixed.setSize();
    JPanel pnlEraseButtons = new JPanel();
    pnlEraseButtons.setLayout(new BoxLayout(pnlEraseButtons, BoxLayout.Y_AXIS));
    JPanel pnlErase = new JPanel();
    pnlErase.setLayout(new BoxLayout(pnlErase, BoxLayout.X_AXIS));
    pnlErase.add(Box.createHorizontalGlue());
    pnlErase.add(btnErase.getComponent());
    pnlErase.add(Box.createHorizontalGlue());
    pnlErase.add(btnViewErased.getComponent());
    pnlErase.add(Box.createHorizontalGlue());
    pnlErase.add(btnReplaceRawStack.getComponent());
    pnlErase.add(Box.createHorizontalGlue());
    JPanel pnlClipStats = new JPanel();
    pnlClipStats.setLayout(new BoxLayout(pnlClipStats, BoxLayout.X_AXIS));
    pnlClipStats.add(Box.createHorizontalGlue());
    pnlClipStats.add(btnClipStatsRaw.getComponent());
    pnlClipStats.add(Box.createHorizontalGlue());
    pnlClipStats.add(btnClipStatsFixed.getComponent());
    pnlClipStats.add(Box.createHorizontalGlue());
    pnlEraseButtons.add(pnlErase);
    pnlEraseButtons.add(pnlClipStats);
    UIUtilities.setButtonSizeAll(pnlErase, UIParameters.INSTANCE.getButtonDimension());
    UIUtilities
        .setButtonSizeAll(pnlClipStats, UIParameters.INSTANCE.getButtonDimension());

    UIUtilities.addWithYSpace(pnlCCDEraser, pnlEraseButtons);

    // Left align all of the compenents in each panel and center align the
    // panel
    UIUtilities.alignComponentsX(pnlXRayReplacement, Component.LEFT_ALIGNMENT);
    UIUtilities.alignComponentsX(pnlCCDEraser, Component.LEFT_ALIGNMENT);
    pnlCCDEraser.setAlignmentX(Component.CENTER_ALIGNMENT);

    enableXRayReplacement();
    enableManualReplacement();
    updateManualReplacementAdvanced(globalAdvancedButton.isExpanded());

    ccdEraserActionListener = new CCDEraserXRaysActionListener(this);
  }

  static CcdEraserXRaysPanel getInstance(final ApplicationManager appMgr,
      final AxisID id, final DialogType dialogType,
      GlobalExpandButton globalAdvancedButton) {
    CcdEraserXRaysPanel instance = new CcdEraserXRaysPanel(appMgr, id, dialogType,
        globalAdvancedButton);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    // Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlCCDEraser.addMouseListener(mouseAdapter);

    btnFindXRays.addActionListener(ccdEraserActionListener);
    btnViewXRayModel.addActionListener(ccdEraserActionListener);
    btnCreateModel.addActionListener(ccdEraserActionListener);
    btnErase.addActionListener(ccdEraserActionListener);
    btnViewErased.addActionListener(ccdEraserActionListener);
    btnReplaceRawStack.addActionListener(ccdEraserActionListener);
    cbXrayReplacement.addActionListener(ccdEraserActionListener);
    cbManualReplacement.addActionListener(ccdEraserActionListener);
    btnClipStatsRaw.addActionListener(ccdEraserActionListener);
    btnClipStatsFixed.addActionListener(ccdEraserActionListener);
  }

  /**
   * Set the fields
   * @param ccdEraserParams
   */
  void setParameters(final ConstCCDEraserParam ccdEraserParams) {
    cbXrayReplacement.setSelected(ccdEraserParams.isFindPeaks());
    ltfPeakCriterion.setText(ccdEraserParams.getPeakCriterion());
    ltfDiffCriterion.setText(ccdEraserParams.getDiffCriterion());
    ltfGrowCriterion.setText(ccdEraserParams.getGrowCriterion());
    ltfScanCriterion.setText(ccdEraserParams.getScanCriterion());
    ltfMaximumRadius.setText(ccdEraserParams.getMaximumRadius());
    ltfAnnulusWidth.setText(ccdEraserParams.getAnnulusWidth());
    ltfScanRegionSize.setText(ccdEraserParams.getXyScanSize());
    ltfEdgeExclusion.setText(ccdEraserParams.getEdgeExclusion());
    cbManualReplacement.setSelected(!ccdEraserParams.getModelFile().equals(""));
    ltfGlobalReplacementList.setText(ccdEraserParams.getGlobalReplacementList());
    ltfLocalReplacementList.setText(ccdEraserParams.getlocalReplacementList());
    ltfBoundaryReplacementList.setText(ccdEraserParams.getBoundaryReplacementList());
    ltfBorderPixels.setText(ccdEraserParams.getBorderPixels());
    ltfPolynomialOrder.setText(ccdEraserParams.getPolynomialOrder());
    cbIncludeAdjacentPoints.setSelected(ccdEraserParams.getIncludeAdjacentPoints());
    ltfGiantCriterion.setText(ccdEraserParams.getGiantCriterion());
    ltfBigDiffCriterion.setText(ccdEraserParams.getBigDiffCriterion());
    ltfExtraLargeRadius.setText(ccdEraserParams.getExtraLargeRadius());
    enableXRayReplacement();
    enableManualReplacement();
  }

  void done() {
    btnFindXRays.removeActionListener(ccdEraserActionListener);
    btnErase.removeActionListener(ccdEraserActionListener);
    btnReplaceRawStack.removeActionListener(ccdEraserActionListener);
  }

  void setParameters(BaseScreenState screenState) {
    phManualReplacement.setButtonStates(screenState);
  }

  public void getParameters(BaseScreenState screenState) {
    phManualReplacement.getButtonStates(screenState);
  }

  public boolean getParameters(final CCDEraserParam ccdEraserParams,
      final boolean doValidation) {
    try {
      ccdEraserParams.setFindPeaks(cbXrayReplacement.isSelected());
      ccdEraserParams.setPeakCriterion(ltfPeakCriterion.getText(doValidation));
      ccdEraserParams.setDiffCriterion(ltfDiffCriterion.getText(doValidation));
      ccdEraserParams.setGrowCriterion(ltfGrowCriterion.getText(doValidation));
      ccdEraserParams.setScanCriterion(ltfScanCriterion.getText(doValidation));
      ccdEraserParams.setMaximumRadius(ltfMaximumRadius.getText(doValidation));
      ccdEraserParams.setAnnulusWidth(ltfAnnulusWidth.getText(doValidation));
      ccdEraserParams.setXyScanSize(ltfScanRegionSize.getText(doValidation));
      ccdEraserParams.setEdgeExclusion(ltfEdgeExclusion.getText(doValidation));
      ccdEraserParams.setGlobalReplacementList(ltfGlobalReplacementList
          .getText(doValidation));
      ccdEraserParams.setLocalReplacementList(ltfLocalReplacementList
          .getText(doValidation));
      ccdEraserParams.setBoundaryReplacementList(ltfBoundaryReplacementList
          .getText(doValidation));
      ccdEraserParams.setBorderPixels(ltfBorderPixels.getText(doValidation));
      ccdEraserParams.setPolynomialOrder(ltfPolynomialOrder.getText(doValidation));
      ccdEraserParams.setIncludeAdjacentPoints(cbIncludeAdjacentPoints.isSelected());
      ccdEraserParams.setGiantCriterion(ltfGiantCriterion.getText(doValidation));
      ccdEraserParams.setBigDiffCriterion(ltfBigDiffCriterion.getText(doValidation));
      ccdEraserParams.setExtraLargeRadius(ltfExtraLargeRadius.getText(doValidation));
      if (cbManualReplacement.isSelected()) {
        ccdEraserParams.setModelFile(applicationManager.getMetaData().getDatasetName()
            + axisID.getExtension() + ".erase");
      }
      else {
        ccdEraserParams.setModelFile("");
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public boolean getParameters(final MakecomfileParam param, final boolean doValidation) {
    return true;
  }

  /**
   * Return the container of panel
   * @return
   */
  JPanel getContainer() {
    return pnlCCDEraser;
  }

  /**
   * Makes the advanced components visible or invisible
   * @param state
   */
  void updateAdvanced(final boolean state) {
    cbXrayReplacement.setVisible(state);
    ltfGrowCriterion.setVisible(state);
    ltfEdgeExclusion.setVisible(state);
    ltfAnnulusWidth.setVisible(state);
    ltfScanRegionSize.setVisible(state);
    ltfScanCriterion.setVisible(state);
    ltfBorderPixels.setVisible(state);
    ltfPolynomialOrder.setVisible(state);
    cbIncludeAdjacentPoints.setVisible(state);
    ltfGiantCriterion.setVisible(state);
    ltfExtraLargeRadius.setVisible(state);
  }

  void updateManualReplacementAdvanced(final boolean advanced) {
    pnlManualReplacementBody.setVisible(advanced);
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public void expand(GlobalExpandButton button) {
    updateAdvanced(button.isExpanded());
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public void expand(final ExpandButton button) {
    if (phManualReplacement != null && phManualReplacement.equalsAdvancedBasic(button)) {
      updateManualReplacementAdvanced(button.isExpanded());
    }
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from CCDEraserActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  private void buttonAction(String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnFindXRays.getActionCommand())) {
      applicationManager.findXrays(axisID, btnFindXRays, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType, this);
    }
    else if (command.equals(btnErase.getActionCommand())) {
      applicationManager.preEraser(axisID, btnErase, null, deferred3dmodButton,
          run3dmodMenuOptions, dialogType, this);
    }
    else if (command.equals(btnReplaceRawStack.getActionCommand())) {
      applicationManager.replaceRawStack(axisID, btnReplaceRawStack, dialogType);
    }
    else if (command.equals(cbXrayReplacement.getActionCommand())) {
      enableXRayReplacement();
    }
    else if (command.equals(cbManualReplacement.getActionCommand())) {
      enableManualReplacement();
    }
    else if (command.equals(btnViewXRayModel.getActionCommand())) {
      applicationManager.imodXrayModel(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnCreateModel.getActionCommand())) {
      applicationManager.imodManualErase(axisID, run3dmodMenuOptions, dialogType);
    }
    else if (command.equals(btnViewErased.getActionCommand())) {
      applicationManager.imodErasedStack(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnClipStatsRaw.getActionCommand())) {
      applicationManager.clipStats(axisID, FileType.RAW_STACK, null, dialogType,
          TomodataplotsParam.Task.MIN_MAX);
    }
    else if (command.equals(btnClipStatsFixed.getActionCommand())) {
      applicationManager.clipStats(axisID, FileType.FIXED_XRAYS_STACK, null, dialogType,
          TomodataplotsParam.Task.FIXED_MIN_MAX);
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    String[] label = { "CCDEraser" };
    String[] manPage = { "ccderaser.html" };

    String[] logFileLabel = { "Eraser" };
    String[] logFile = new String[1];
    logFile[0] = "eraser" + axisID.getExtension() + ".log";

    TomodataplotsParam.Task[] graph = { TomodataplotsParam.Task.MIN_MAX,
        TomodataplotsParam.Task.FIXED_MIN_MAX };
    ContextPopup contextPopup = new ContextPopup(pnlCCDEraser, mouseEvent,
        "PRE-PROCESSING", ContextPopup.TOMO_GUIDE, label, manPage, logFileLabel, logFile,
        graph, applicationManager, axisID);
  }

  private void enableXRayReplacement() {
    boolean state = cbXrayReplacement.isSelected();
    ltfPeakCriterion.setEnabled(state);
    ltfDiffCriterion.setEnabled(state);
    ltfGrowCriterion.setEnabled(state);
    ltfEdgeExclusion.setEnabled(state);
    ltfMaximumRadius.setEnabled(state);
    ltfAnnulusWidth.setEnabled(state);
    ltfScanRegionSize.setEnabled(state);
    ltfScanCriterion.setEnabled(state);
    btnFindXRays.setEnabled(state);
    btnViewXRayModel.setEnabled(state);
    ltfGiantCriterion.setEnabled(state);
    ltfBigDiffCriterion.setEnabled(state);
    ltfExtraLargeRadius.setEnabled(state);
  }

  private void enableManualReplacement() {
    boolean state = cbManualReplacement.isSelected();
    ltfGlobalReplacementList.setEnabled(state);
    ltfLocalReplacementList.setEnabled(state);
    ltfBoundaryReplacementList.setEnabled(state);
    btnCreateModel.setEnabled(state);
  }

  /**
   * Tooltip string initialization
   */
  private void setToolTipText() {
    String text;
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(applicationManager, AutodocFactory.CCDERASER,
          axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
    cbXrayReplacement.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.FIND_PEAKS_KEY));
    ltfPeakCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.PEAK_CRITERION_KEY));
    ltfDiffCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.DIFF_CRITERION_KEY));
    ltfGrowCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.GROW_CRITERION_KEY));
    ltfScanCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.SCAN_CRITERION_KEY));
    ltfMaximumRadius.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.MAXIMUM_RADIUS_KEY));
    ltfAnnulusWidth.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.ANNULUS_WIDTH_KEY));
    ltfScanRegionSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.X_Y_SCAN_SIZE_KEY));
    ltfEdgeExclusion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.EDGE_EXCLUSION_WIDTH_KEY));
    ltfLocalReplacementList.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.LINE_OBJECTS_KEY));
    ltfBoundaryReplacementList.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.BOUNDARY_OBJECTS_KEY));
    cbManualReplacement
        .setToolTipText("Use a manually created model to specify regions and lines to replace.");
    ltfGlobalReplacementList.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.ALL_SECTION_OBJECTS_KEY));
    ltfBorderPixels.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.BORDER_SIZE_KEY));
    ltfPolynomialOrder.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.POLYNOMIAL_ORDER_KEY));
    cbIncludeAdjacentPoints
        .setToolTipText("Include pixels adjacent to the patch being replaced in the pixels "
            + "being fit.");
    btnFindXRays.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.TRIAL_MODE_KEY));
    ltfGiantCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.GIANT_CRITERION_KEY));
    ltfBigDiffCriterion.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.BIG_DIFF_CRITERION_KEY));
    ltfExtraLargeRadius.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        CCDEraserParam.EXTRA_LARGE_RADIUS_KEY));
    btnViewXRayModel.setToolTipText("View the x-ray model on the raw stack in 3dmod.");
    btnCreateModel.setToolTipText("Create a manual replacement model using 3dmod.");
    btnErase
        .setToolTipText("Run ccderaser, erasing the raw stack and writing the modified stack "
            + "to the output file specified (the default is *_fixed.st).  "
            + "NOTE: subsequent processing uses the "
            + "raw stack filename, therefore for ccderaser to have an effect on "
            + "your data you must commit the raw stack when you are satisfied with"
            + " your ccderaser output stack.");
    btnViewErased.setToolTipText("View the erased stack in 3dmod.");
    btnReplaceRawStack
        .setToolTipText("Use the raw stack with the output from ccderaser.  "
            + "NOTE: subsequent processing uses the "
            + "raw stack filename, therefore for ccderaser to have an effect on "
            + "your data you must commit the raw stack when you are satisfied with"
            + " your ccderaser output stack.");
    btnClipStatsRaw
        .setToolTipText("Run clip stats on the raw stack.  Prints information "
            + "about each section");
    btnClipStatsFixed.setToolTipText("Run clip stats on the stack created by the "
        + ERASE_LABEL + " button.  Prints information about each section.");
  }

  // Action listener
  private final class CCDEraserXRaysActionListener implements ActionListener {

    private final CcdEraserXRaysPanel adaptee;

    private CCDEraserXRaysActionListener(final CcdEraserXRaysPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null, null);
    }
  }
}

/**
 * <p> $Log$
 * <p> Revision 1.3  2011/02/21 17:50:15  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 04:56:37  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.7  2010/04/28 16:35:59  sueh
 * <p> bug# 1344 In buttonAction fixed the output file passed to clipStats:
 * <p> changed it from CCD_ERASER_OUTPUT to FIXED_XRAYS_STACK.
 * <p>
 * <p> Revision 3.6  2010/03/12 04:10:40  sueh
 * <p> bug# 1325 Made the use fixed stack button label available to the package.
 * <p>
 * <p> Revision 3.5  2010/03/09 22:06:57  sueh
 * <p> bug# 1325 Changed FileType.CCD_ERASER_INPUT to
 * <p> FileType.RAW_STACK.
 * <p>
 * <p> Revision 3.4  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.3  2009/12/11 20:52:33  sueh
 * <p> bug# 1291 Rename and rearrange clip stats buttons.
 * <p>
 * <p> Revision 3.2  2009/12/11 17:28:52  sueh
 * <p> bug# 1291 Added btnClipStatsFixed and btnClipStatsRaw.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.1  2009/06/16 22:52:44  sueh
 * <p> bug# 1221 Panel for erasing x-rays.  Runs ccderaser in several different
 * <p> ways.
 * <p>
 * <p> Revision 3.33  2009/06/11 16:51:04  sueh
 * <p> bug# 1221 Sending the process panel to the process function in the
 * <p> manager wrapped in a ProcessDisplay interface.  Implemented
 * <p> CCDEraserDisplay.
 * <p>
 * <p> Revision 3.32  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.31  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 3.30  2009/01/20 19:49:02  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.29  2008/05/28 02:49:26  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 3.28  2008/05/13 23:00:05  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.27  2008/05/06 23:56:33  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.
 * <p>
 * <p> Revision 3.26  2008/05/03 00:48:40  sueh
 * <p> bug# 847 Gave process buttons right-click menu functionality.
 * <p>
 * <p> Revision 3.25  2007/12/13 21:54:25  sueh
 * <p> bug# 1057 Added boundaryReplacementList.
 * <p>
 * <p> Revision 3.24  2007/09/10 20:42:06  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.23  2007/03/21 19:45:01  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.22  2007/03/01 01:27:27  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 3.21  2007/02/09 00:47:31  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.20  2006/07/20 17:19:39  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 3.19  2006/06/21 15:50:08  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 3.18  2006/02/06 21:20:38  sueh
 * <p> bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 * <p>
 * <p> Revision 3.17  2006/01/26 22:03:43  sueh
 * <p> bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * <p> the buttons turned on each they are run, unless the process fails or is
 * <p> killed.
 * <p>
 * <p> Revision 3.16  2006/01/12 17:07:44  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.15  2006/01/03 23:30:01  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox
 * <p>
 * <p> Revision 3.14  2005/10/27 00:33:54  sueh
 * <p> bug# 725 Calling preEraser instead eraser so that the B stack can be
 * <p> processed.
 * <p>
 * <p> Revision 3.13  2005/08/27 22:35:26  sueh
 * <p> bug# 532 Changed Autodoc.get() to getInstance().
 * <p>
 * <p> Revision 3.12  2005/08/11 23:45:28  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * <p> private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * <p> be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * <p> the action function.
 * <p>
 * <p> Revision 3.11  2005/08/10 20:40:19  sueh
 * <p> bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * <p> of MultiLineButton.
 * <p>
 * <p> Revision 3.10  2005/08/09 20:13:08  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.
 * <p>
 * <p> Revision 3.9  2005/04/25 20:53:48  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.8  2005/02/22 20:57:49  sueh
 * <p> bug# 600 Converting tooltips to autodoc.
 * <p>
 * <p> Revision 3.7  2004/12/02 20:37:08  sueh
 * <p> bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * <p> the join guide.  Need to specify the guide to anchor.
 * <p>
 * <p> Revision 3.6  2004/11/19 23:49:22  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.5.4.2  2004/10/11 02:10:20  sueh
 * <p> bug# 520 Passed the manager to the ContextPopup object in order to get
 * <p> the propertyUserDir.
 * <p>
 * <p> Revision 3.5.4.1  2004/09/07 17:58:36  sueh
 * <p> bug# 520 getting dataset name from metadata
 * <p>
 * <p> Revision 3.5  2004/06/25 00:34:01  sueh
 * <p> bug# 467 Removing outerRadius, adding annulusWidth.
 * <p> Making maximumRadius a basic field.
 * <p>
 * <p> Revision 3.4  2004/04/21 17:06:17  rickg
 * <p> Bug #424 simplified panel layout using UIUtilities
 * <p>
 * <p> Revision 3.3  2004/01/30 22:44:47  sueh
 * <p> bug# 356 Changing buttons with html labels to
 * <p> MultiLineButton and MultiLineToggleButton
 * <p>
 * <p> Revision 3.2  2004/01/29 22:33:01  rickg
 * <p> Tooltip text change
 * <p>
 * <p> Revision 3.1  2003/11/10 07:36:24  rickg
 * <p> Task tags moved to bugzilla, reformat
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.14  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.13  2003/10/28 23:35:48  rickg
 * <p> Bug# 336 Context menu label capitalization
 * <p>
 * <p> Revision 2.12  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.11  2003/10/13 20:26:52  sueh
 * <p> bug270
 * <p> added and changed tooltips
 * <p>
 * <p> Revision 2.10  2003/10/13 17:00:19  sueh
 * <p> bug269
 * <p> UI Changes
 * <p> CCDEraserPanel
 * <p> changed button names
 * <p>
 * <p> Revision 2.9  2003/09/23 23:58:42  sueh
 * <p> bug#237 moved XrayReplacement to Advanced
 * <p>
 * <p> Revision 2.8  2003/09/09 17:14:09  rickg
 * <p> Changed replace text to commit
 * <p>
 * <p> Revision 2.7  2003/08/06 21:56:44  rickg
 * <p> Switched stateful buttons to JToggleButton
 * <p>
 * <p> Revision 2.6  2003/07/30 21:53:44  rickg
 * <p> Use new tooltip formatting class
 * <p>
 * <p> Revision 2.5  2003/07/25 23:02:47  rickg
 * <p> Moved polynomial order, border pixels and inclide adjacent to
 * <p> the global section
 * <p> Corrected spelling mistakes
 * <p>
 * <p> Revision 2.4  2003/07/22 22:17:54  rickg
 * <p> Erase button name change
 * <p> Correct setup of manual replacement parameters
 * <p>
 * <p> Revision 2.3  2003/07/11 23:14:08  rickg
 * <p> Add parameter set and get for new eraser mode
 * <p>
 * <p> Revision 2.2  2003/07/08 20:49:43  rickg
 * <p> Restructure panel for new ccderaser
 * <p>
 * <p> Revision 2.1  2003/05/08 04:26:51  rickg
 * <p> Centered checkbox
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
