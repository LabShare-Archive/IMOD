package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.AutoAlignmentController;
import etomo.BaseManager;
import etomo.comscript.MidasParam;
import etomo.comscript.XfalignParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AutoAlignmentMetaData;
import etomo.type.AxisID;
import etomo.type.EtomoAutodoc;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.SharedConstants;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class AutoAlignmentPanel implements Run3dmodButtonContainer {
  public static final String rcsid = "$Id:$";

  private final SpacedPanel pnlRoot = SpacedPanel.getFocusableInstance();
  private final SpacedPanel pnlParameters = SpacedPanel.getInstance();
  private final LabeledTextField ltfSigmaLowFrequency = new LabeledTextField(
      FieldType.FLOATING_POINT, "Sigma for low-frequency filter: ");
  private final LabeledTextField ltfCutoffHighFrequency = new LabeledTextField(
      FieldType.FLOATING_POINT, "Cutoff for high-frequency filter: ");
  private final LabeledTextField ltfSigmaHighFrequency = new LabeledTextField(
      FieldType.FLOATING_POINT, "Sigma for high-frequency filter: ");

  private final SpacedPanel pnlButtons = SpacedPanel.getInstance();
  private final MultiLineButton btnInitialAutoAlignment = new MultiLineButton(
      "Initial Auto Alignment");
  private final MultiLineButton btnMidas = new MultiLineButton("Midas");
  private final MultiLineButton btnRefineAutoAlignment = new MultiLineButton(
      "Refine with Auto Alignment");
  private final MultiLineButton btnRevertToMidas = new MultiLineButton(
      "Revert Auto Alignment to Midas");
  private final MultiLineButton btnRevertToEmpty = new MultiLineButton(
      "Revert to No Transforms");
  private final LabeledSpinner spReduceByBinning = LabeledSpinner.getDefaultedInstance(
      "Binning in search: ", 2, 1, 50, 1, 1);
  private final LabeledTextField ltfSkipSectionsFrom1 = new LabeledTextField(
      FieldType.INTEGER_LIST, "Sections to skip: ");
  private final CheckBox cbPreCrossCorrelation = new CheckBox(
      "Find initial shifts with cross-correlation");
  private final LabeledTextField ltfEdgeToIgnore = new LabeledTextField(
      FieldType.FLOATING_POINT, "Fraction to ignore on edges: ");
  private final Spinner spMidasBinning = Spinner.getLabeledInstance("Binning in Midas: ",
      1, 1, 8);

  private final BaseManager manager;
  private final boolean joinInterface;
  private final TransformChooserPanel tcAlign;
  private final CheckBox cbFindWarping;
  private final LabeledTextField ltfWarpPatchSizeX;
  private final LabeledTextField ltfWarpPatchSizeY;
  private final CheckBox cbBoundaryModel;
  private final Run3dmodButton btnBoundaryModel;
  private final LabeledTextField ltfShiftLimitsForWarpX;
  private final LabeledTextField ltfShiftLimitsForWarpY;
  private final CheckBox cbSobelFilter;

  private AutoAlignmentController controller = null;

  private AutoAlignmentPanel(final BaseManager manager, final boolean joinInterface) {
    this.manager = manager;
    this.joinInterface = joinInterface;
    if (joinInterface) {
      tcAlign = TransformChooserPanel.getJoinAlignInstance();
      cbFindWarping = null;
      ltfWarpPatchSizeX = null;
      ltfWarpPatchSizeY = null;
      cbBoundaryModel = null;
      btnBoundaryModel = null;
      ltfShiftLimitsForWarpX = null;
      ltfShiftLimitsForWarpY = null;
      cbSobelFilter = null;
    }
    else {
      tcAlign = TransformChooserPanel.getSerialSectionsInstance();
      cbFindWarping = new CheckBox("Find warping transformations");
      ltfWarpPatchSizeX = new LabeledTextField(FieldType.INTEGER,
          "Correlation patch size in X: ");
      ltfWarpPatchSizeY = new LabeledTextField(FieldType.INTEGER, " Y: ");
      cbBoundaryModel = new CheckBox("Use boundary model:");
      btnBoundaryModel = Run3dmodButton.get3dmodInstance("Create/View Boundary Model",
          this);
      ltfShiftLimitsForWarpX = new LabeledTextField(FieldType.INTEGER,
          "Limits to shifts in X: ");
      ltfShiftLimitsForWarpY = new LabeledTextField(FieldType.INTEGER, " Y: ");
      cbSobelFilter = new CheckBox("Apply Sobel filter");
    }
  }

  static AutoAlignmentPanel getJoinInstance(final BaseManager manager) {
    AutoAlignmentPanel instance = new AutoAlignmentPanel(manager, true);
    instance.createPanel(true);
    instance.setTooltips();
    return instance;
  }

  static AutoAlignmentPanel getSerialSectionsInstance(final BaseManager manager) {
    AutoAlignmentPanel instance = new AutoAlignmentPanel(manager, false);
    instance.createPanel(false);
    instance.setTooltips();
    return instance;
  }

  private void createPanel(final boolean joinConfiguration) {
    // init
    ltfSigmaLowFrequency.setText("0.0");
    ltfCutoffHighFrequency.setText("0.35");
    ltfSigmaHighFrequency.setText("0.05");
    ltfEdgeToIgnore.setText("0.05");
    // panels
    JPanel pnlPreCrossCorrelation = new JPanel();
    SpacedPanel pnlLeftButtons = SpacedPanel.getInstance();
    SpacedPanel pnlRightButtons = SpacedPanel.getInstance();
    JPanel pnlWarping = null;
    JPanel pnlFindWarping = null;
    JPanel pnlWarpPatchSize = null;
    JPanel pnlBoundaryModel = null;
    JPanel pnlShiftLimitsForWarp = null;
    JPanel pnlSobelFilter = null;
    if (cbSobelFilter != null) {
      pnlSobelFilter = new JPanel();
    }
    if (cbFindWarping != null) {
      pnlWarping = new JPanel();
      pnlFindWarping = new JPanel();
      pnlWarpPatchSize = new JPanel();
      pnlBoundaryModel = new JPanel();
      pnlShiftLimitsForWarp = new JPanel();
    }
    // init
    if (joinConfiguration) {
      spReduceByBinning.setVisible(false);
      ltfSkipSectionsFrom1.setVisible(false);
      cbPreCrossCorrelation.setVisible(false);
      ltfEdgeToIgnore.setVisible(false);
      spMidasBinning.setVisible(false);
    }
    if (cbFindWarping != null) {
      btnBoundaryModel.setSize();
    }
    btnInitialAutoAlignment.setSize();
    btnMidas.setSize();
    btnRefineAutoAlignment.setSize();
    btnRevertToMidas.setSize();
    btnRevertToEmpty.setSize();
    // root
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.add(pnlParameters.getContainer());
    pnlRoot.add(spMidasBinning.getContainer());
    pnlRoot.add(pnlButtons.getContainer());
    // parameters
    pnlParameters.setBoxLayout(BoxLayout.Y_AXIS);
    pnlParameters.setBorder(new EtchedBorder("Auto Alignment Parameters").getBorder());
    pnlParameters.add(ltfSigmaLowFrequency);
    pnlParameters.add(ltfCutoffHighFrequency);
    pnlParameters.add(ltfSigmaHighFrequency);
    if (pnlSobelFilter != null) {
      pnlParameters.add(pnlSobelFilter);
    }
    pnlParameters.add(pnlPreCrossCorrelation);
    pnlParameters.add(tcAlign.getComponent());
    if (pnlWarping != null) {
      pnlParameters.add(pnlWarping);
    }
    pnlParameters.add(ltfSkipSectionsFrom1.getContainer());
    pnlParameters.add(ltfEdgeToIgnore);
    pnlParameters.add(spReduceByBinning.getContainer());
    // SobelFilter panel
    if (pnlSobelFilter != null) {
      pnlSobelFilter.setLayout(new BoxLayout(pnlSobelFilter, BoxLayout.X_AXIS));
      pnlSobelFilter.add(cbSobelFilter);
      pnlSobelFilter.add(Box.createHorizontalGlue());
    }
    // warping panel
    if (pnlWarping != null) {
      pnlWarping.setLayout(new BoxLayout(pnlWarping, BoxLayout.Y_AXIS));
      pnlWarping.setBorder(new EtchedBorder("Warping").getBorder());
      pnlWarping.add(pnlFindWarping);
      pnlWarping.add(Box.createRigidArea(FixedDim.x0_y3));
      pnlWarping.add(pnlWarpPatchSize);
      pnlWarping.add(Box.createRigidArea(FixedDim.x0_y3));
      pnlWarping.add(pnlBoundaryModel);
      pnlWarping.add(Box.createRigidArea(FixedDim.x0_y3));
      pnlWarping.add(pnlShiftLimitsForWarp);
      // FindWarping panel
      pnlFindWarping.setLayout(new BoxLayout(pnlFindWarping, BoxLayout.X_AXIS));
      pnlFindWarping.add(cbFindWarping);
      pnlFindWarping.add(Box.createHorizontalGlue());
      // WarpPatchSize panel
      pnlWarpPatchSize.setLayout(new BoxLayout(pnlWarpPatchSize, BoxLayout.X_AXIS));
      pnlWarpPatchSize.add(ltfWarpPatchSizeX.getComponent());
      pnlWarpPatchSize.add(ltfWarpPatchSizeY.getComponent());
      // BoundaryModel panel
      pnlBoundaryModel.setLayout(new BoxLayout(pnlBoundaryModel, BoxLayout.X_AXIS));
      pnlBoundaryModel.add(cbBoundaryModel);
      pnlBoundaryModel.add(btnBoundaryModel.getComponent());
      // ShiftLimitsForWarp panel
      pnlShiftLimitsForWarp.setLayout(new BoxLayout(pnlShiftLimitsForWarp,
          BoxLayout.X_AXIS));
      pnlShiftLimitsForWarp.add(ltfShiftLimitsForWarpX.getComponent());
      pnlShiftLimitsForWarp.add(ltfShiftLimitsForWarpY.getComponent());
    }
    // pre cross correlation
    pnlPreCrossCorrelation.setLayout(new BoxLayout(pnlPreCrossCorrelation,
        BoxLayout.X_AXIS));
    pnlPreCrossCorrelation.add(cbPreCrossCorrelation);
    pnlPreCrossCorrelation.add(Box.createHorizontalGlue());
    // buttons
    pnlButtons.setBoxLayout(BoxLayout.X_AXIS);
    pnlButtons.add(pnlLeftButtons);
    pnlButtons.add(pnlRightButtons);
    // left buttons
    pnlLeftButtons.setBoxLayout(BoxLayout.Y_AXIS);
    pnlLeftButtons.add(btnInitialAutoAlignment);
    pnlLeftButtons.add(btnMidas);
    pnlLeftButtons.add(btnRefineAutoAlignment);
    // right buttons
    pnlRightButtons.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRightButtons.add(btnRevertToMidas);
    pnlRightButtons.add(btnRevertToEmpty);
    // display
    updateDisplay();
  }

  /**
   * Sets the controller and adds listeners
   * @param input
   */
  public void setController(final AutoAlignmentController input) {
    controller = input;
    addListeners();
  }

  private void addListeners() {
    ActionListener listener = new AutoAlignmentActionListener(this);
    if (cbFindWarping != null) {
      cbFindWarping.addActionListener(listener);
      cbBoundaryModel.addActionListener(listener);
      btnBoundaryModel.addActionListener(listener);
    }
    btnInitialAutoAlignment.addActionListener(listener);
    btnMidas.addActionListener(listener);
    btnRefineAutoAlignment.addActionListener(listener);
    btnRevertToMidas.addActionListener(listener);
    btnRevertToEmpty.addActionListener(listener);
    if (!joinInterface) {
      tcAlign.addSearchListener(listener);
    }
  }

  Component getRootComponent() {
    return pnlRoot.getContainer();
  }

  boolean getParameters(final AutoAlignmentMetaData metaData, final boolean doValidation) {
    try {
      metaData.setSigmaLowFrequency(ltfSigmaLowFrequency.getText(doValidation));
      metaData.setSigmaLowFrequencyEnabled(ltfSigmaLowFrequency.isEnabled());
      metaData.setCutoffHighFrequency(ltfCutoffHighFrequency.getText(doValidation));
      metaData.setCutoffHighFrequencyEnabled(ltfCutoffHighFrequency.isEnabled());
      metaData.setSigmaHighFrequency(ltfSigmaHighFrequency.getText(doValidation));
      metaData.setSigmaHighFrequencyEnabled(ltfSigmaHighFrequency.isEnabled());
      metaData.setAlignTransform(tcAlign.getTransform());
      if (cbFindWarping != null) {
        metaData.setFindWarping(cbFindWarping.isSelected());
        metaData.setWarpPatchSizeX(ltfWarpPatchSizeX.getText());
        metaData.setWarpPatchSizeY(ltfWarpPatchSizeY.getText());
        metaData.setBoundaryModel(cbBoundaryModel.isSelected());
        metaData.setShiftLimitsForWarpX(ltfShiftLimitsForWarpX.getText());
        metaData.setShiftLimitsForWarpY(ltfShiftLimitsForWarpY.getText());
      }
      metaData.setPreCrossCorrelation(cbPreCrossCorrelation.isSelected());
      metaData.setSkipSectionsFrom1(ltfSkipSectionsFrom1.getText(doValidation));
      metaData.setEdgeToIgnore(ltfEdgeToIgnore.getText(doValidation));
      metaData.setReduceByBinning(spReduceByBinning.getValue());
      metaData.setMidasBinning(spMidasBinning.getValue());
      if (cbSobelFilter != null) {
        metaData.setSobelFilter(cbSobelFilter.isSelected());
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void setParameters(final AutoAlignmentMetaData metaData) {
    if (!metaData.isSigmaLowFrequencyNull()) {
      ltfSigmaLowFrequency.setText(metaData.getSigmaLowFrequency().toString());
    }
    if (!metaData.isCutoffHighFrequencyNull()) {
      ltfCutoffHighFrequency.setText(metaData.getCutoffHighFrequency().toString());
    }
    if (!metaData.isSigmaHighFrequencyNull()) {
      ltfSigmaHighFrequency.setText(metaData.getSigmaHighFrequency().toString());
    }
    tcAlign.setTransform(metaData.getAlignTransform());
    if (cbFindWarping != null) {
      cbFindWarping.setSelected(metaData.isFindWarping());
      ltfWarpPatchSizeX.setText(metaData.getWarpPatchSizeX());
      ltfWarpPatchSizeY.setText(metaData.getWarpPatchSizeY());
      cbBoundaryModel.setSelected(metaData.isBoundaryModel());
      ltfShiftLimitsForWarpX.setText(metaData.getShiftLimitsForWarpX());
      ltfShiftLimitsForWarpY.setText(metaData.getShiftLimitsForWarpY());
    }
    cbPreCrossCorrelation.setSelected(metaData.isPreCrossCorrelation());
    ltfSkipSectionsFrom1.setText(metaData.getSkipSectionsFrom1());
    if (!metaData.isEdgeToIgnoreNull()) {
      ltfEdgeToIgnore.setText(metaData.getEdgeToIgnore());
    }
    if (!metaData.isReduceByBinningNull()) {
      spReduceByBinning.setValue(metaData.getReduceByBinning());
    }
    if (!metaData.isMidasBinningNull()) {
      spMidasBinning.setValue(metaData.getMidasBinning());
    }
    if (cbSobelFilter != null) {
      cbSobelFilter.setSelected(metaData.isSobelFilter());
    }
    updateDisplay();
  }

  public boolean getParameters(final XfalignParam param, final boolean doValidation) {
    try {
      if (cbPreCrossCorrelation.isVisible()) {
        param.setPreCrossCorrelation(cbPreCrossCorrelation.isSelected());
      }
      else {
        param.setPreCrossCorrelation(false);
      }
      if (ltfSkipSectionsFrom1.isVisible()) {
        param.setSkipSectionsFrom1(ltfSkipSectionsFrom1.getText(doValidation));
      }
      else {
        param.resetSkipSectionsFrom1();
      }
      if (ltfEdgeToIgnore.isVisible() && ltfEdgeToIgnore.isEnabled()) {
        param.setEdgeToIgnore(ltfEdgeToIgnore.getText(doValidation));
      }
      else {
        param.resetEdgeToIgnore();
      }
      if (spReduceByBinning.isVisible() && spReduceByBinning.isEnabled()) {
        param.setReduceByBinning(spReduceByBinning.getValue());
      }
      else {
        param.resetReduceByBinning();
      }
      if (cbFindWarping != null) {
        if (cbFindWarping.isSelected()) {
          param.setWarpPatchSize(ltfWarpPatchSizeX.getText(doValidation),
              ltfWarpPatchSizeY.getText(doValidation));
          param.setBoundaryModel(cbBoundaryModel.isSelected());
          param.setShiftLimitsForWarp(ltfShiftLimitsForWarpX.getText(doValidation),
              ltfShiftLimitsForWarpY.getText(doValidation));
        }
        else {
          param.resetWarpPatchSize();
          param.resetBoundaryModel();
          param.resetShiftLimitsForWarp();
        }
      }
      if (cbSobelFilter != null && cbSobelFilter.isEnabled()) {
        param.setSobelFilter(cbSobelFilter.isSelected());
      }
      else {
        param.resetSobelFilter();
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public void getParameters(final MidasParam param) {
    param.setBinning(spMidasBinning.getValue());
  }

  /**
   * checking if panel is equal to meta data.  Set useDefault to match how 
   * useDefault is used in setMetaData()
   * @param metaData
   * @return
   */
  boolean equals(final AutoAlignmentMetaData metaData) {
    if (!metaData.getSigmaLowFrequency().equals(ltfSigmaLowFrequency.getText())) {
      return false;
    }
    if (!metaData.getCutoffHighFrequency().equals(ltfCutoffHighFrequency.getText())) {
      return false;
    }
    if (!metaData.getSigmaHighFrequency().equals(ltfSigmaHighFrequency.getText())) {
      return false;
    }
    if (tcAlign.getTransform() != metaData.getAlignTransform()) {
      return false;
    }
    if (cbFindWarping != null) {
      if (cbFindWarping.isSelected() != metaData.isFindWarping()) {
        return false;
      }
      if (!metaData.getWarpPatchSizeX().equals(ltfWarpPatchSizeX.getText())) {
        return false;
      }
      if (!metaData.getWarpPatchSizeY().equals(ltfWarpPatchSizeY.getText())) {
        return false;
      }
      if (cbBoundaryModel.isSelected() != metaData.isBoundaryModel()) {
        return false;
      }
      if (!metaData.getShiftLimitsForWarpX().equals(ltfShiftLimitsForWarpX.getText())) {
        return false;
      }
      if (!metaData.getShiftLimitsForWarpY().equals(ltfShiftLimitsForWarpY.getText())) {
        return false;
      }
      if (cbSobelFilter != null && cbSobelFilter.isSelected() != metaData.isSobelFilter()) {
        return false;
      }
    }
    return true;
  }

  public void msgProcessChange(final boolean processEnded) {
    btnMidas.setEnabled(processEnded);
    btnRevertToMidas.setEnabled(processEnded);
    btnRevertToEmpty.setEnabled(processEnded);
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), run3dmodMenuOptions);
  }

  private void action(final String command, final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnInitialAutoAlignment.getActionCommand())) {
      msgProcessChange(false);
      controller.xfalignInitial(null, joinInterface);
    }
    else if (command.equals(btnMidas.getActionCommand())) {
      controller.midasSample(btnMidas.getQuotedLabel());
    }
    else if (command.equals(btnRefineAutoAlignment.getActionCommand())) {
      msgProcessChange(false);
      controller.xfalignRefine(null, joinInterface,
          btnRefineAutoAlignment.getQuotedLabel());
    }
    else if (command.equals(btnRevertToMidas.getActionCommand())) {
      controller.revertXfFileToMidas();
    }
    else if (command.equals(btnRevertToEmpty.getActionCommand())) {
      controller.revertXfFileToEmpty();
    }
    else if (command.equals(tcAlign.getSearchActionCommand())) {
      updateDisplay();
    }
    else if (cbFindWarping != null) {
      if (command.equals(cbFindWarping.getActionCommand())) {
        updateDisplay();
      }
      else if (command.equals(cbBoundaryModel.getActionCommand())) {
        updateDisplay();
      }
      else if (command.equals(btnBoundaryModel.getActionCommand())) {
        controller.imodBoundaryModel(run3dmodMenuOptions);
      }
    }
  }

  private void updateDisplay() {
    boolean search = tcAlign.isSearch();
    ltfSigmaLowFrequency.setEnabled(search);
    ltfCutoffHighFrequency.setEnabled(search);
    ltfSigmaHighFrequency.setEnabled(search);
    if (cbSobelFilter != null) {
      cbSobelFilter.setEnabled(search);
    }
    if (cbFindWarping != null) {
      boolean findWarping = cbFindWarping.isSelected();
      ltfWarpPatchSizeX.setEnabled(findWarping);
      ltfWarpPatchSizeY.setEnabled(findWarping);
      cbBoundaryModel.setEnabled(findWarping);
      btnBoundaryModel.setEnabled(findWarping && cbBoundaryModel.isSelected());
      ltfShiftLimitsForWarpX.setEnabled(findWarping);
      ltfShiftLimitsForWarpY.setEnabled(findWarping);
    }
    ltfEdgeToIgnore.setEnabled(search);
    spReduceByBinning.setEnabled(search);
  }

  private void setTooltips() {
    ltfSigmaLowFrequency
        .setToolTipText("Sigma of an inverted gaussian for filtering out low frequencies "
            + "before searching for transformation.  Filter is applied to binned image.");
    ltfCutoffHighFrequency
        .setToolTipText("Starting radius of a gaussian for filtering out high frequencies "
            + "before searching for transformation.  Filter is applied to binned image.");
    ltfSigmaHighFrequency
        .setToolTipText("Sigma of gaussian for filtering out high frequencies before "
            + "searching for transformation.  Filter is applied to binned image.");
    btnInitialAutoAlignment
        .setToolTipText("OPTIONAL:  Run xfalign.  Find preliminary translational "
            + "alignments with tiltxcorr rather then using an existing .xf file.");
    btnMidas
        .setToolTipText("Open Midas to check the output of the auto alignment and to make "
            + "transformations by hand.");
    btnRefineAutoAlignment
        .setToolTipText("OPTIONAL:  Run xfalign using preliminary alignments created by "
            + "the most recent use of Midas or xfalign.");
    btnRevertToMidas
        .setToolTipText("Use to ignore xfalign changes.  Returns transformations to the "
            + "state created by the most recent save done in Midas.");
    btnRevertToEmpty.setToolTipText("Use to remove all transformations.");
    spMidasBinning.setToolTipText(SharedConstants.MIDAS_BINNING_TOOLTIP);
    if (cbSobelFilter != null) {
      cbSobelFilter
          .setToolTipText("Apply edge-detecting Sobel filter after image reduction and "
              + "filtering, if any.");
    }
    if (cbFindWarping != null) {
      cbFindWarping
          .setToolTipText("Align with non-linear warping by cross-correlating overlapping "
              + "patches.");
      String text = "Size of patches to correlate in X and Y, in unbinned pixels.";
      ltfWarpPatchSizeX.setToolTipText(text);
      ltfWarpPatchSizeY.setToolTipText(text);
      cbBoundaryModel.setToolTipText("Use model with contours around areas where "
          + "patches should be correlated.");
      btnBoundaryModel.setToolTipText("Open 3dmod to draw or see contours around areas "
          + "to use for correlation.");
      text = "Maximum pixels of shift for each patch.  If both fields are blank there "
          + "are no limits; otherwise there must be a value in both fields.";
      ltfShiftLimitsForWarpX.setToolTipText(text);
      ltfShiftLimitsForWarpY.setToolTipText(text);
    }
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.XFALIGN, AxisID.ONLY);
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
    if (autodoc != null) {
      cbPreCrossCorrelation
          .setToolTipText("Use cross-correlation to find initial translations; needed if "
              + "shifts are large.  This checkbox has no effect when refining with auto "
              + "alignment");
      ltfEdgeToIgnore.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          XfalignParam.EDGE_TO_IGNORE_KEY));
      spReduceByBinning.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          XfalignParam.REDUCE_BY_BINNING_KEY));
      ltfSkipSectionsFrom1
          .setToolTipText(EtomoAutodoc
              .getTooltip(autodoc, XfalignParam.SKIP_SECTIONS_KEY)
              + "  Also sets the "
              + XfalignParam.SECTIONS_NUMBERED_FROM_ONE_KEY
              + " option:  "
              + EtomoAutodoc.getTooltip(autodoc,
                  XfalignParam.SECTIONS_NUMBERED_FROM_ONE_KEY));
    }
  }

  private static final class AutoAlignmentActionListener implements ActionListener {
    private final AutoAlignmentPanel panel;

    private AutoAlignmentActionListener(final AutoAlignmentPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action(event.getActionCommand(), null);
    }
  }
}
