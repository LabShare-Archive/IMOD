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
 */
package etomo.ui.swing;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.filechooser.FileFilter;

import etomo.ApplicationManager;
import etomo.logic.ConfigTool;
import etomo.logic.DatasetTool;
import etomo.storage.DirectiveFileCollection;
import etomo.storage.MagGradientFileFilter;
import etomo.storage.StackFileFilter;
import etomo.storage.DistortionFileFilter;
import etomo.type.AxisID;
import etomo.type.DataFileType;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.StringProperty;
import etomo.type.TiltAngleSpec;
import etomo.type.UserConfiguration;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.ui.SetupReconInterface;

final class SetupDialog extends ProcessDialog implements ContextMenu,
    Run3dmodButtonContainer, Expandable, SetupReconInterface {
  public static final String rcsid = "$Id$";

  static final String DATASET_NAME_LABEL = "Dataset name: ";
  static final String FIDUCIAL_DIAMETER_LABEL = "Fiducial diameter (nm): ";
  static final String AXIS_TYPE_LABEL = "Axis Type";
  static final String FRAME_TYPE_LABEL = "Frame Type";
  static final String SINGLE_AXIS_LABEL = "Single axis";
  static final String MONTAGE_LABEL = "Montage";
  static final String SINGLE_FRAME_LABEL = "Single frame";
  private final String BACKUP_DIRECTORY_LABEL = "Backup directory: ";
  private final int BINNING_DEFAULT = 1;
  private static final String TWODIR_LABEL_1 = "Series was bidirectional from ";
  private static final String TWODIR_LABEL_2 = " degrees";

  private final JPanel pnlDataParameters = new JPanel();
  // Dataset GUI objects
  private final JPanel pnlDataset = new JPanel();
  private final ImageIcon iconFolder = new ImageIcon(
      ClassLoader.getSystemResource("images/openFile.gif"));

  private final FileTextField2 ftfDataset = FileTextField2.getInstance(
      applicationManager, DATASET_NAME_LABEL);

  private final FileTextField ftfBackupDirectory = new FileTextField(
      BACKUP_DIRECTORY_LABEL);

  // Data type GUI objects
  private final JPanel pnlPerAxisInfo = new JPanel();
  private final EtomoPanel pnlAxisInfoA = new EtomoPanel();
  private final EtomoPanel pnlDataType = new EtomoPanel();
  private final EtomoPanel pnlAxisType = new EtomoPanel();
  private final RadioButton rbSingleAxis = new RadioButton(SINGLE_AXIS_LABEL);
  private final RadioButton rbDualAxis = new RadioButton("Dual axis");

  private final EtomoPanel pnlViewType = new EtomoPanel();
  private final RadioButton rbSingleView = new RadioButton(SINGLE_FRAME_LABEL);
  private final RadioButton rbMontage = new RadioButton(MONTAGE_LABEL);

  private final Run3dmodButton btnViewRawStackA = Run3dmodButton.get3dmodInstance(
      "View Raw Image Stack", this);
  private Run3dmodButton btnViewRawStackB = Run3dmodButton.get3dmodInstance(
      "View Raw Image Stack", this);

  // Image parameter objects
  private final JPanel pnlImageParams = new JPanel();
  private final MultiLineButton btnScanHeader = new MultiLineButton("Scan Header");
  private final JPanel pnlImageRows = new JPanel();

  private final JPanel pnlStackInfo = new JPanel();
  private final LabeledTextField ltfPixelSize = new LabeledTextField(
      FieldType.FLOATING_POINT, "Pixel size (nm): ");
  private final LabeledTextField ltfFiducialDiameter = new LabeledTextField(
      FieldType.FLOATING_POINT, FIDUCIAL_DIAMETER_LABEL);
  private final LabeledTextField ltfImageRotation = new LabeledTextField(
      FieldType.FLOATING_POINT, "Image rotation (degrees): ");

  private final JPanel pnlDistortionInfo = new JPanel();
  private final FileTextField ftfDistortionFile = new FileTextField(
      "Image distortion field file: ");
  private final LabeledSpinner spnBinning = LabeledSpinner.getInstance("Binning: ",
      BINNING_DEFAULT, 1, 50, 1);

  private final JPanel pnlMagGradientInfo = new JPanel();
  private final FileTextField ftfMagGradientFile = new FileTextField(
      "Mag gradients correction: ");
  private final CheckBox cbParallelProcess = new CheckBox("Parallel Processing");
  private final CheckBox cbGpuProcessing = new CheckBox("Graphics card processing");

  // Tilt angle GUI objects
  // private TiltAnglePanel tiltAnglesA = new TiltAnglePanel();
  private final LabeledTextField ltfExcludeListA = new LabeledTextField(
      FieldType.INTEGER_LIST, "Exclude views: ");
  private final JPanel pnlAdjustedFocusA = new JPanel();
  private final CheckBox cbAdjustedFocusA = new CheckBox(
      "Focus was adjusted between montage frames");

  private final BeveledBorder borderAxisInfoB = new BeveledBorder("Axis B: ");
  // private TiltAnglePanel tiltAnglesB = new TiltAnglePanel();
  private final LabeledTextField ltfExcludeListB = new LabeledTextField(
      FieldType.INTEGER_LIST, "Exclude views: ");
  private final JPanel pnlAdjustedFocusB = new JPanel();
  private final CheckBox cbAdjustedFocusB = new CheckBox(
      "Focus was adjusted between montage frames");
  private final CheckTextField ctfTwodir = CheckTextField.getInstance(
      FieldType.FLOATING_POINT, TWODIR_LABEL_1);
  private final CheckTextField ctfBtwodir = CheckTextField.getInstance(
      FieldType.FLOATING_POINT, TWODIR_LABEL_1);
  private final JLabel lTwodir = new JLabel(TWODIR_LABEL_2);
  private final JLabel lBtwodir = new JLabel(TWODIR_LABEL_2);

  private final SetupDialogExpert expert;
  private final boolean calibrationAvailable;
  private final SetupDialogActionListener listener;
  private final TemplatePanel templatePanel;

  private DirectiveFileCollection directiveFileCollection = null;

  // Construct the setup dialog
  private SetupDialog(final SetupDialogExpert expert, final ApplicationManager manager,
      final AxisID axisID, final DialogType dialogType, boolean calibrationAvailable) {
    super(manager, axisID, dialogType);
    this.expert = expert;
    this.calibrationAvailable = calibrationAvailable;
    listener = new SetupDialogActionListener(expert);
    templatePanel = TemplatePanel.getInstance(manager, axisID, listener, "Templates",
        null);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    ftfDataset.setFileSelectionMode(FileChooser.FILES_ONLY);
    ftfDataset.setOrigin(expert.getDatasetDir());
    ftfDataset.setAbsolutePath(true);
    ftfDataset.setFileFilter(new StackFileFilter());
    createDatasetPanel();
    createDataTypePanel();
    createPerAxisInfoPanel();
    // Relabel the postpone button
    btnPostpone.setText("Use Existing Coms");
    btnExecute.setText("Create Com Scripts");

    if (calibrationAvailable) {
      // There are no advanced settings for this dialog, remove the advanced
      // button
      pnlExitButtons.remove(btnAdvanced.getComponent());
    }

    // Add the panes to the dialog box
    rootPanel.add(pnlDataParameters);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(Box.createRigidArea(FixedDim.x0_y10));
    rootPanel.add(pnlPerAxisInfo);
    rootPanel.add(Box.createVerticalGlue());
    addExitButtons();
    UIUtilities.alignComponentsX(rootPanel, Component.CENTER_ALIGNMENT);

    // Resize the standard panel buttons
    UIUtilities.setButtonSizeAll(pnlExitButtons,
        UIParameters.INSTANCE.getButtonDimension());
    if (!calibrationAvailable) {
      updateAdvanced(btnAdvanced.isExpanded());
      btnAdvanced.register(this);
    }
    // Calcute the necessary window size
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  static SetupDialog getInstance(final SetupDialogExpert expert,
      final ApplicationManager manger, final AxisID axisID, final DialogType dialogType,
      boolean calibrationAvailable) {
    SetupDialog instance = new SetupDialog(expert, manger, axisID, dialogType,
        calibrationAvailable);
    instance.addListeners();
    return instance;
  }

  /**
   * Right mouse button context menu
   **/
  public void popUpContextMenu(final MouseEvent mouseEvent) {
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent, "INITIAL STEPS",
        applicationManager, axisID);
  }

  public void done() {
    if (applicationManager
        .doneSetupDialog(expert.getExitState() == DialogExitState.EXECUTE)) {
      setDisplayed(false);
    }
  }

  public boolean buttonExecuteAction() {
    String sDataset = ftfDataset.getText();
    StringProperty standardizedFilePath = new StringProperty();
    if (DatasetTool.standardizeExtension(applicationManager, expert.getAxisType(),
        ftfDataset.getFile(), standardizedFilePath)) {
      // file name changed or there was a problem with the rename
      ftfDataset.setText(standardizedFilePath.toString());
    }
    if (sDataset.indexOf(File.separator) != -1) {
      if (!DatasetTool.validateDatasetName(applicationManager, null, AxisID.ONLY,
          ftfDataset.getFile(), DataFileType.RECON, expert.getAxisType())) {
        return false;
      }
    }
    else {
      String datasetName = ftfDataset.getText();
      if (!DatasetTool.validateDatasetName(applicationManager, null, AxisID.ONLY,
          new File(expert.getPropertyUserDir()), datasetName, DataFileType.RECON,
          expert.getAxisType(),
          !datasetName.endsWith(FileType.RAW_STACK.getExtension(applicationManager)))) {
        return false;
      }
    }
    return super.buttonExecuteAction();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (btnViewRawStackA == button) {
      expert.viewRawStack(AxisID.FIRST, run3dmodMenuOptions);
    }
    else if (btnViewRawStackB == button) {
      expert.viewRawStack(AxisID.SECOND, run3dmodMenuOptions);
    }
  }

  public DirectiveFileCollection getDirectiveFileCollection() {
    return templatePanel.getDirectiveFileCollection();
  }

  void setParameters(final UserConfiguration userConfig) {
    templatePanel.setParameters(userConfig);
  }

  private void resetRadioButtons(final RadioButton rb1, final RadioButton rb2) {
    if (rb1.isCheckpointValue()) {
      rb1.setSelected(true);
    }
    else if (rb2.isCheckpointValue()) {
      rb2.setSelected(true);
    }
    else {
      rb1.setSelected(false);
      rb2.setSelected(false);
    }
  }

  void checkpoint() {
    ftfDataset.checkpoint();
    ftfBackupDirectory.checkpoint();
    rbSingleAxis.checkpoint();
    rbDualAxis.checkpoint();
    rbSingleView.checkpoint();
    rbMontage.checkpoint();
    ltfPixelSize.checkpoint();
    ltfFiducialDiameter.checkpoint();
    ltfImageRotation.checkpoint();
    ftfDistortionFile.checkpoint();
    spnBinning.checkpoint();
    ftfMagGradientFile.checkpoint();
    cbParallelProcess.checkpoint();
    cbGpuProcessing.checkpoint();
    ltfExcludeListA.checkpoint();
    cbAdjustedFocusA.checkpoint();
    ltfExcludeListB.checkpoint();
    cbAdjustedFocusB.checkpoint();
    ctfTwodir.checkpoint();
    ctfBtwodir.checkpoint();
  }

  void updateTemplateValues() {
    DirectiveFileCollection directiveFileCollection = templatePanel
        .getDirectiveFileCollection();
    // Handle dual differently because the dual is the default.
    if (directiveFileCollection.containsDual()) {
      if (directiveFileCollection.isDual()) {
        rbDualAxis.setSelected(true);
      }
      else {
        rbSingleAxis.setSelected(true);
      }
    }
    else {
      resetRadioButtons(rbDualAxis, rbSingleAxis);
    }
    if (directiveFileCollection.containsMontage()) {
      if (directiveFileCollection.isMontage()) {
        rbMontage.setSelected(true);
      }
      else {
        rbSingleView.setSelected(true);
      }
    }
    else {
      resetRadioButtons(rbSingleView, rbMontage);
    }
    if (directiveFileCollection.containsPixel()) {
      ltfPixelSize.setText(directiveFileCollection.getPixelSize(false));
    }
    else {
      ltfPixelSize.resetToCheckpoint();
    }
    if (directiveFileCollection.isTwodir(AxisID.FIRST)) {
      ctfTwodir.setSelected(true);
      ctfTwodir.setText(directiveFileCollection.getTwodir(AxisID.FIRST, false));
    }
    else {
      ctfTwodir.resetToCheckpoint();
    }
    if (directiveFileCollection.isTwodir(AxisID.SECOND)) {
      ctfBtwodir.setSelected(true);
      ctfBtwodir.setText(directiveFileCollection.getTwodir(AxisID.SECOND, false));
    }
    else {
      ctfBtwodir.resetToCheckpoint();
    }
    if (directiveFileCollection.containsGold()) {
      ltfFiducialDiameter.setText(directiveFileCollection.getFiducialDiameter(false));
    }
    else {
      ltfFiducialDiameter.resetToCheckpoint();
    }
    if (directiveFileCollection.containsRotation()) {
      ltfImageRotation.setText(directiveFileCollection.getImageRotation(AxisID.FIRST,
          false));
    }
    else {
      ltfImageRotation.resetToCheckpoint();
    }
    expert.updateTiltAnglePanelTemplateValues(directiveFileCollection);
    if (directiveFileCollection.containsDistort()) {
      ftfDistortionFile.setText(directiveFileCollection.getDistortionFile());
    }
    else {
      ftfDistortionFile.resetToCheckpoint();
    }
    if (directiveFileCollection.containsBinning()) {
      spnBinning.setValue(directiveFileCollection.getIntBinning(BINNING_DEFAULT));
    }
    else {
      spnBinning.resetToCheckpoint();
    }
    if (directiveFileCollection.containsGradient()) {
      ftfMagGradientFile.setText(directiveFileCollection.getMagGradientFile());
    }
    else {
      ftfMagGradientFile.resetToCheckpoint();
    }
    if (directiveFileCollection.containsFocus(AxisID.FIRST)) {
      cbAdjustedFocusA.setSelected(directiveFileCollection
          .isAdjustedFocusSelected(AxisID.FIRST));
    }
    else {
      cbAdjustedFocusA.resetToCheckpoint();
    }
    if (directiveFileCollection.containsFocus(AxisID.SECOND)) {
      cbAdjustedFocusB.setSelected(directiveFileCollection
          .isAdjustedFocusSelected(AxisID.SECOND));
    }
    else {
      cbAdjustedFocusB.resetToCheckpoint();
    }
  }

  private void viewRawStackA() {
    action(btnViewRawStackA, null);
  }

  private void viewRawStackB() {
    action(btnViewRawStackB, null);
  }

  void setDataset(final String input) {
    ftfDataset.setText(input);
  }

  void setParallelProcess(final boolean input) {
    cbParallelProcess.setSelected(input);
  }

  void setParallelProcessEnabled(final boolean input) {
    cbParallelProcess.setEnabled(input);
  }

  void setGpuProcessingEnabled(final boolean input) {
    cbGpuProcessing.setEnabled(input);
  }

  void setGpuProcessing(final boolean input) {
    cbGpuProcessing.setSelected(input);
  }

  void setBackupDirectory(final String input) {
    ftfBackupDirectory.setText(input);
  }

  void setDistortionFile(final String input) {
    ftfDistortionFile.setText(input);
  }

  void setMagGradientFile(final String input) {
    ftfMagGradientFile.setText(input);
  }

  void setAdjustedFocus(final AxisID axisID, final boolean input) {
    if (axisID == AxisID.SECOND) {
      cbAdjustedFocusB.setSelected(input);
    }
    else {
      cbAdjustedFocusA.setSelected(input);
    }
  }

  void setAxisTypeTooltip(final String tooltip) {
    pnlAxisType.setToolTipText(tooltip);
    rbSingleAxis.setToolTipText(tooltip);
    rbDualAxis.setToolTipText(tooltip);
  }

  void setDistortionFileTooltip(final String tooltip) {
    ftfDistortionFile.setFieldToolTipText(tooltip);
    ftfDistortionFile.setButtonToolTipText(tooltip);
  }

  void setViewTypeTooltip(final String tooltip) {
    pnlViewType.setToolTipText(tooltip);
    rbSingleView.setToolTipText(tooltip);
    rbMontage.setToolTipText(tooltip);
  }

  void setPixelSizeTooltip(final String tooltip) {
    ltfPixelSize.setToolTipText(tooltip);
  }

  void setFiducialDiameterTooltip(final String tooltip) {
    ltfFiducialDiameter.setToolTipText(tooltip);
  }

  void setImageRotationTooltip(final String tooltip) {
    ltfImageRotation.setToolTipText(tooltip);
  }

  void setBinningTooltip(final String tooltip) {
    spnBinning.setToolTipText(tooltip);
  }

  void setViewRawStackTooltip(final String tooltip) {
    btnViewRawStackA.setToolTipText(tooltip);
    btnViewRawStackB.setToolTipText(tooltip);
  }

  void setAdjustedFocusTooltip(final String tooltip) {
    cbAdjustedFocusA.setToolTipText(tooltip);
    cbAdjustedFocusB.setToolTipText(tooltip);
  }

  void setExcludeListTooltip(final String tooltip) {
    ltfExcludeListA.setToolTipText(tooltip);
    ltfExcludeListB.setToolTipText(tooltip);
  }

  void setTwodirTooltip(final String tooltip) {
    ctfTwodir.setToolTipText(tooltip);
    ctfBtwodir.setToolTipText(tooltip);
  }

  void setPostponeTooltip(final String tooltip) {
    btnPostpone.setToolTipText(tooltip);
  }

  void setExecuteTooltip(final String tooltip) {
    btnExecute.setToolTipText(tooltip);
  }

  void setParallelProcessTooltip(final String tooltip) {
    cbParallelProcess.setToolTipText(tooltip);
  }

  void setGpuProcessingTooltip(final String tooltip) {
    cbGpuProcessing.setToolTipText(tooltip);
  }

  void setMagGradientFileTooltip(final String tooltip) {
    ftfMagGradientFile.setFieldToolTipText(tooltip);
    ftfMagGradientFile.setButtonToolTipText(tooltip);
  }

  void setSingleAxis(final boolean input) {
    rbSingleAxis.setSelected(input);
  }

  public boolean isSingleAxisSelected() {
    return rbSingleAxis.isSelected();
  }

  public boolean isSingleViewSelected() {
    return rbSingleView.isSelected();
  }

  public boolean isDualAxisSelected() {
    return rbDualAxis.isSelected();
  }

  void setDualAxis(final boolean input) {
    rbDualAxis.setSelected(input);
  }

  public void setPixelSize(final double input) {
    ltfPixelSize.setText(input);
  }

  void setFiducialDiameter(final double input) {
    ltfFiducialDiameter.setText(input);
  }

  public void setImageRotation(final String input) {
    ltfImageRotation.setText(input);
  }

  void setImageRotation(final double input) {
    ltfImageRotation.setText(input);
  }

  public void setBinning(final int input) {
    spnBinning.setValue(input);
  }

  public String getBinning() {
    return spnBinning.getValue().toString();
  }

  public String getExcludeList(final AxisID axisID, final boolean doValidation)
      throws FieldValidationFailedException {
    if (axisID == AxisID.SECOND) {
      return ltfExcludeListB.getText(doValidation);
    }
    return ltfExcludeListA.getText(doValidation);
  }

  void setExcludeList(final AxisID axisID, final String input) {
    if (axisID == AxisID.SECOND) {
      ltfExcludeListB.setText(input);
    }
    else {
      ltfExcludeListA.setText(input);
    }
  }

  public String getTwodir(AxisID axisID, boolean doValidation)
      throws FieldValidationFailedException {
    if (axisID == AxisID.SECOND) {
      return ctfBtwodir.getText(doValidation);
    }
    return ctfTwodir.getText(doValidation);
  }

  public boolean isTwodir(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return ctfBtwodir.isSelected();
    }
    return ctfTwodir.isSelected();
  }

  void setTwodir(final AxisID axisID, final boolean selected) {
    if (axisID == AxisID.SECOND) {
      ctfBtwodir.setSelected(selected);
    }
    else {
      ctfTwodir.setSelected(selected);
    }
  }

  void setTwodir(final AxisID axisID, final String input) {
    if (axisID == AxisID.SECOND) {
      ctfBtwodir.setText(input);
    }
    else {
      ctfTwodir.setText(input);
    }
  }

  public void setTwodir(final AxisID axisID, final double input) {
    if (axisID == AxisID.SECOND) {
      ctfBtwodir.setSelected(true);
      ctfBtwodir.setText(Double.toString(input));
    }
    else {
      ctfTwodir.setSelected(true);
      ctfTwodir.setText(Double.toString(input));
    }
  }

  void setExcludeListEnabled(final AxisID axisID, final boolean enable) {
    if (axisID == AxisID.SECOND) {
      ltfExcludeListB.setEnabled(enable);
    }
    else {
      ltfExcludeListA.setEnabled(enable);
    }
  }

  void setTwodirEnabled(final AxisID axisID, final boolean enable) {
    if (axisID == AxisID.SECOND) {
      ctfBtwodir.setCheckBoxEnabled(enable);
      lBtwodir.setEnabled(enable);
    }
    else {
      ctfTwodir.setCheckBoxEnabled(enable);
      lTwodir.setEnabled(enable);
    }
  }

  void setViewRawStackEnabled(final AxisID axisID, final boolean enable) {
    if (axisID == AxisID.SECOND) {
      btnViewRawStackB.setEnabled(enable);
    }
    else {
      btnViewRawStackA.setEnabled(enable);
    }
  }

  void setSingleView(final boolean input) {
    rbSingleView.setSelected(input);
  }

  void setMontage(final boolean input) {
    rbMontage.setSelected(input);
  }

  public String getDataset() {
    return ftfDataset.getText();
  }

  public boolean getTiltAngleFields(final AxisID axisID,
      final TiltAngleSpec tiltAngleSpec, final boolean doValidation) {
    return expert.getTiltAngleFields(axisID, tiltAngleSpec, doValidation);
  }

  public String getBackupDirectory() {
    return ftfBackupDirectory.getText();
  }

  public String getDistortionFile() {
    return ftfDistortionFile.getText();
  }

  public String getMagGradientFile() {
    return ftfMagGradientFile.getText();
  }

  public boolean isParallelProcessSelected(final String propertyUserDir) {
    return cbParallelProcess.isSelected();
  }

  public boolean isGpuProcessingSelected(final String propertyUserDir) {
    return cbGpuProcessing.isSelected();
  }

  public boolean isAdjustedFocusSelected(final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return cbAdjustedFocusB.isSelected();
    }
    return cbAdjustedFocusA.isSelected();
  }

  public String getPixelSize(final boolean doValidation)
      throws FieldValidationFailedException {
    return ltfPixelSize.getText(doValidation);
  }

  public boolean validateTiltAngle(final AxisID axisID, final String errorTitle) {
    return expert.validateTiltAngle(axisID, errorTitle);
  }

  public String getFiducialDiameter(final boolean doValidation)
      throws FieldValidationFailedException {
    return ltfFiducialDiameter.getText(doValidation);
  }

  /**
   * @param axisID has no effect
   */
  public String getImageRotation(final AxisID axisID, final boolean doValidation)
      throws FieldValidationFailedException {
    return ltfImageRotation.getText(doValidation);
  }

  boolean equalsSingleAxisActionCommand(final String actionCommand) {
    return actionCommand.equals(rbSingleAxis.getActionCommand());
  }

  boolean equalsDualAxisActionCommand(final String actionCommand) {
    return actionCommand.equals(rbDualAxis.getActionCommand());
  }

  boolean equalsSingleViewActionCommand(final String actionCommand) {
    return actionCommand.equals(rbSingleView.getActionCommand());
  }

  void setAdjustedFocusEnabled(final AxisID axisID, final boolean enable) {
    if (axisID == AxisID.SECOND) {
      cbAdjustedFocusB.setEnabled(enable);
    }
    else {
      cbAdjustedFocusA.setEnabled(enable);
    }
  }

  boolean equalsMontageActionCommand(final String actionCommand) {
    return actionCommand.equals(rbMontage.getActionCommand());
  }

  boolean equalsScanHeaderActionCommand(final String actionCommand) {
    return actionCommand.equals(btnScanHeader.getActionCommand());
  }

  boolean equalsTemplateActionCommand(final String actionCommand) {
    return templatePanel.equalsActionCommand(actionCommand);
  }

  public void expand(GlobalExpandButton button) {
    updateAdvanced(button.isExpanded());
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public final void expand(final ExpandButton button) {
  }

  void updateAdvanced(final boolean advanced) {
    if (calibrationAvailable || pnlDistortionInfo == null) {
      return;
    }
    ftfDistortionFile.setVisible(advanced);
    spnBinning.setVisible(advanced);
    pnlMagGradientInfo.setVisible(advanced);
  }

  void setMagGradientInfoVisible(final boolean visible) {

  }

  File getFile(String dir, FileFilter fileFilter, int selectionMode) {
    JFileChooser chooser = new FileChooser(new File(dir));
    if (fileFilter != null) {
      chooser.setFileFilter(fileFilter);
    }
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(selectionMode);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      return chooser.getSelectedFile();
    }
    return null;
  }

  void backupDirectoryAction() {
    try {
      File file = getFile(expert.getCurrentBackupDirectory(), null,
          JFileChooser.DIRECTORIES_ONLY);
      if (file != null) {
        ftfBackupDirectory.setText(file.getCanonicalPath());
      }
    }
    catch (Exception excep) {
      excep.printStackTrace();
    }
  }

  void distortionFileAction() {
    try {
      File file = getFile(
          ConfigTool.getDistortionDir(applicationManager, ftfDistortionFile.getFile()),
          new DistortionFileFilter(), JFileChooser.FILES_ONLY);
      if (file != null) {
        ftfDistortionFile.setText(file.getAbsolutePath());
      }
    }
    catch (Exception excep) {
      excep.printStackTrace();
    }
  }

  /**
   * Lets the user choose the mag gradients correction file.
   * @param event
   */
  void magGradientFileAction() {
    try {
      File file = getFile(expert.getCurrentMagGradientDir(), new MagGradientFileFilter(),
          JFileChooser.FILES_ONLY);
      if (file != null) {
        ftfMagGradientFile.setText(file.getAbsolutePath());
      }
    }
    catch (Exception excep) {
      excep.printStackTrace();
    }
  }

  void setDatasetTooltip(final String fieldTooltip, final String buttonTooltip) {
    ftfDataset.setFieldToolTipText(fieldTooltip);
    ftfDataset.setButtonToolTipText(buttonTooltip);
  }

  void setBackupDirectoryTooltip(final String fieldTooltip, final String buttonTooltip) {
    ftfBackupDirectory.setFieldToolTipText(fieldTooltip);
    ftfBackupDirectory.setButtonToolTipText(buttonTooltip);
  }

  void setScanHeaderTooltip(String tooltip) {
    btnScanHeader.setToolTipText(tooltip);
  }

  private void addListeners() {
    // Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    ftfBackupDirectory.addActionListener(new BackupDirectoryActionListener(this));
    ftfDistortionFile.addActionListener(new DistortionFileActionListener(this));
    ftfMagGradientFile.addActionListener(new MagGradientFileActionListener(this));
    btnViewRawStackA.addActionListener(new ViewRawStackAActionListener(this));
    btnViewRawStackB.addActionListener(new ViewRawStackBActionListener(this));
    rbSingleAxis.addActionListener(listener);
    rbDualAxis.addActionListener(listener);
    rbSingleView.addActionListener(listener);
    rbMontage.addActionListener(listener);
    btnScanHeader.addActionListener(listener);
  }

  private void createDatasetPanel() {
    pnlDataset.setLayout(new BoxLayout(pnlDataset, BoxLayout.X_AXIS));

    // Bind the buttons to their adapters

    // Add the GUI objects to the pnl
    pnlDataset.add(Box.createRigidArea(FixedDim.x5_y0));

    pnlDataset.add(ftfDataset.getRootPanel());
    pnlDataset.add(Box.createRigidArea(FixedDim.x10_y0));

    pnlDataset.add(ftfBackupDirectory.getContainer());
    pnlDataset.add(Box.createRigidArea(FixedDim.x5_y0));
  }

  private void createDataTypePanel() {
    JPanel pnlRow2 = new JPanel();
    // init
    ftfDistortionFile.setTextPreferredWidth(505);
    ftfMagGradientFile.setTextPreferredWidth(505);

    // Datatype subpnls: DataSource AxisType Viewtype
    Dimension dimDataTypePref = new Dimension(
        (int) (150 * UIParameters.INSTANCE.getFontSizeAdjustment()),
        (int) (80 * UIParameters.INSTANCE.getFontSizeAdjustment()));

    ButtonGroup bgAxisType = new ButtonGroup();
    bgAxisType.add(rbSingleAxis.getAbstractButton());
    bgAxisType.add(rbDualAxis.getAbstractButton());
    pnlAxisType.setLayout(new BoxLayout(pnlAxisType, BoxLayout.Y_AXIS));
    pnlAxisType.setPreferredSize(dimDataTypePref);
    pnlAxisType.setBorder(new EtchedBorder(AXIS_TYPE_LABEL).getBorder());
    pnlAxisType.add(rbSingleAxis.getComponent());
    pnlAxisType.add(rbDualAxis.getComponent());
    ButtonGroup bgViewType = new ButtonGroup();
    bgViewType.add(rbSingleView.getAbstractButton());
    bgViewType.add(rbMontage.getAbstractButton());
    pnlViewType.setLayout(new BoxLayout(pnlViewType, BoxLayout.Y_AXIS));
    pnlViewType.setPreferredSize(dimDataTypePref);
    pnlViewType.setBorder(new EtchedBorder(FRAME_TYPE_LABEL).getBorder());
    pnlViewType.add(rbSingleView.getComponent());
    pnlViewType.add(rbMontage.getComponent());

    // Datatype panel
    pnlDataType.setLayout(new BoxLayout(pnlDataType, BoxLayout.X_AXIS));
    pnlDataType.setBorder(new EtchedBorder("Data Type").getBorder());
    pnlDataType.add(pnlAxisType);
    pnlDataType.add(Box.createHorizontalGlue());
    pnlDataType.add(pnlViewType);
    pnlDataType.add(Box.createHorizontalGlue());

    // Pixel & Alignment panel
    ltfPixelSize.setColumns(8);
    ltfFiducialDiameter.setColumns(5);
    ltfImageRotation.setColumns(5);
    btnScanHeader.setSize();
    spnBinning.setTextMaxmimumSize(UIParameters.INSTANCE.getSpinnerDimension());

    pnlStackInfo.setLayout(new BoxLayout(pnlStackInfo, BoxLayout.X_AXIS));
    btnScanHeader.setAlignmentY(Component.CENTER_ALIGNMENT);
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlStackInfo.add(Box.createHorizontalGlue());
    pnlStackInfo.add(btnScanHeader.getComponent());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(ltfPixelSize.getContainer());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(Box.createHorizontalGlue());
    pnlStackInfo.add(ltfFiducialDiameter.getContainer());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(Box.createHorizontalGlue());
    pnlStackInfo.add(ltfImageRotation.getContainer());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlStackInfo.add(Box.createHorizontalGlue());
    pnlStackInfo.add(Box.createRigidArea(FixedDim.x5_y0));

    pnlDistortionInfo.setLayout(new BoxLayout(pnlDistortionInfo, BoxLayout.X_AXIS));
    pnlDistortionInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlDistortionInfo.add(ftfDistortionFile.getContainer());
    pnlDistortionInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlDistortionInfo.add(spnBinning.getContainer());
    pnlDistortionInfo.add(Box.createRigidArea(FixedDim.x5_y0));

    pnlMagGradientInfo.setLayout(new BoxLayout(pnlMagGradientInfo, BoxLayout.X_AXIS));
    pnlMagGradientInfo.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlMagGradientInfo.add(ftfMagGradientFile.getContainer());
    pnlMagGradientInfo.add(Box.createRigidArea(FixedDim.x119_y0));

    JPanel pnlParallelProcess = new JPanel();
    pnlParallelProcess.setLayout(new BoxLayout(pnlParallelProcess, BoxLayout.X_AXIS));
    pnlParallelProcess.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlParallelProcess.add(cbParallelProcess);
    JPanel pnlGpuProcessing = new JPanel();

    pnlGpuProcessing.setLayout(new BoxLayout(pnlGpuProcessing, BoxLayout.X_AXIS));
    pnlGpuProcessing.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlGpuProcessing.add(cbGpuProcessing);

    pnlImageRows.setLayout(new BoxLayout(pnlImageRows, BoxLayout.Y_AXIS));
    pnlImageRows.add(pnlStackInfo);
    pnlImageRows.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlImageRows.add(pnlParallelProcess);
    pnlImageRows.add(pnlGpuProcessing);
    pnlImageRows.add(pnlDistortionInfo);
    pnlImageRows.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlImageRows.add(pnlMagGradientInfo);
    UIUtilities.alignComponentsX(pnlImageRows, Component.LEFT_ALIGNMENT);

    pnlImageRows.setAlignmentY(Component.CENTER_ALIGNMENT);
    pnlImageParams.setLayout(new BoxLayout(pnlImageParams, BoxLayout.X_AXIS));

    pnlImageParams.add(pnlImageRows);
    pnlImageParams.add(Box.createHorizontalGlue());
    pnlImageParams.add(Box.createRigidArea(FixedDim.x5_y0));

    pnlRow2.setLayout(new BoxLayout(pnlRow2, BoxLayout.X_AXIS));
    pnlRow2.add(templatePanel.getComponent());
    pnlRow2.add(Box.createRigidArea(FixedDim.x2_y0));
    pnlRow2.add(pnlDataType);

    // Create Data Parameters panel
    pnlDataParameters.setLayout(new BoxLayout(pnlDataParameters, BoxLayout.Y_AXIS));
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlDataset);
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlRow2);
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlDataParameters.add(pnlImageParams);
    pnlDataParameters.add(Box.createRigidArea(FixedDim.x0_y10));
  }

  private void createPerAxisInfoPanel() {
    // constructors
    JPanel pnlTwodir = new JPanel();
    JPanel pnlBtwodir = new JPanel();
    // init
    ltfFiducialDiameter.setRequired(true);
    btnViewRawStackA.setSize();
    btnViewRawStackB.setSize();
    ctfTwodir.setColumns();
    ctfTwodir.setRequired(true);
    ctfBtwodir.setColumns();
    ctfBtwodir.setRequired(true);
    // Tilt angle specification panels
    pnlAxisInfoA.setBorder(new BeveledBorder("Axis A: ").getBorder());
    pnlAxisInfoA.setLayout(new BoxLayout(pnlAxisInfoA, BoxLayout.Y_AXIS));
    ltfExcludeListA.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnViewRawStackA.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlAxisInfoA.add(expert.getTiltAnglesPanelExpert(AxisID.FIRST).getComponent());
    pnlAxisInfoA.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAxisInfoA.add(pnlTwodir);
    pnlAxisInfoA.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlAxisInfoA.add(ltfExcludeListA.getContainer());
    pnlAxisInfoA.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlAxisInfoA.add(btnViewRawStackA.getComponent());
    // Add adjusted focus checkbox
    pnlAdjustedFocusA.setLayout(new BoxLayout(pnlAdjustedFocusA, BoxLayout.X_AXIS));
    pnlAdjustedFocusA.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlAdjustedFocusA.add(cbAdjustedFocusA);
    pnlAdjustedFocusA.add(Box.createHorizontalGlue());
    cbAdjustedFocusA.setAlignmentX(Component.RIGHT_ALIGNMENT);
    cbAdjustedFocusA.setEnabled(false);
    pnlAxisInfoA.add(pnlAdjustedFocusA);

    EtomoPanel pnlAxisInfoB = new EtomoPanel();
    pnlAxisInfoB.setBorder(borderAxisInfoB.getBorder());
    pnlAxisInfoB.setLayout(new BoxLayout(pnlAxisInfoB, BoxLayout.Y_AXIS));
    ltfExcludeListB.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnViewRawStackB.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlAxisInfoB.add(expert.getTiltAnglesPanelExpert(AxisID.SECOND).getComponent());
    pnlAxisInfoB.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlAxisInfoB.add(pnlBtwodir);
    pnlAxisInfoB.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlAxisInfoB.add(ltfExcludeListB.getContainer());
    pnlAxisInfoB.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlAxisInfoB.add(btnViewRawStackB.getComponent());
    cbAdjustedFocusB.setAlignmentX(Component.RIGHT_ALIGNMENT);
    // Add adjusted focus checkbox
    pnlAdjustedFocusB.setLayout(new BoxLayout(pnlAdjustedFocusB, BoxLayout.X_AXIS));
    pnlAdjustedFocusB.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlAdjustedFocusB.add(cbAdjustedFocusB);
    pnlAdjustedFocusB.add(Box.createHorizontalGlue());
    cbAdjustedFocusB.setAlignmentX(Component.RIGHT_ALIGNMENT);
    cbAdjustedFocusB.setEnabled(false);
    pnlAxisInfoB.add(pnlAdjustedFocusB);

    pnlPerAxisInfo.setLayout(new BoxLayout(pnlPerAxisInfo, BoxLayout.X_AXIS));
    pnlPerAxisInfo.add(pnlAxisInfoA);
    pnlPerAxisInfo.add(pnlAxisInfoB);
    // twodir
    pnlTwodir.setLayout(new BoxLayout(pnlTwodir, BoxLayout.X_AXIS));
    pnlTwodir.add(ctfTwodir.getComponent());
    pnlTwodir.add(lTwodir);
    pnlBtwodir.setLayout(new BoxLayout(pnlBtwodir, BoxLayout.X_AXIS));
    pnlBtwodir.add(ctfBtwodir.getComponent());
    pnlBtwodir.add(lBtwodir);
  }

  private static final class BackupDirectoryActionListener implements ActionListener {

    private final SetupDialog adaptee;

    private BackupDirectoryActionListener(final SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.backupDirectoryAction();
    }
  }

  private static final class DistortionFileActionListener implements ActionListener {

    private final SetupDialog adaptee;

    private DistortionFileActionListener(final SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.distortionFileAction();
    }
  }

  private static final class MagGradientFileActionListener implements ActionListener {

    private SetupDialog adaptee;

    private MagGradientFileActionListener(final SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.magGradientFileAction();
    }
  }

  private static final class ViewRawStackAActionListener implements ActionListener {

    private SetupDialog adaptee;

    private ViewRawStackAActionListener(final SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.viewRawStackA();
    }
  }

  private static final class ViewRawStackBActionListener implements ActionListener {

    private SetupDialog adaptee;

    private ViewRawStackBActionListener(final SetupDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.viewRawStackB();
    }
  }

  private static final class SetupDialogActionListener implements TemplateActionListener {

    private final SetupDialogExpert adaptee;

    private SetupDialogActionListener(final SetupDialogExpert adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand());
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2011/05/05 01:29:29  sueh
 * <p> bug# 1396  Popping up an error message and failing when the dataset directory ends in a space.
 * <p>
 * <p> Revision 1.2  2011/02/22 19:29:22  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.71  2010/04/08 18:03:25  sueh
 * <p> bug# 1348 Passing image rotation as a string.
 * <p>
 * <p> Revision 3.70  2010/03/27 05:08:59  sueh
 * <p> bug# 1333 Added GPU checkbox.
 * <p>
 * <p> Revision 3.69  2009/11/20 17:35:02  sueh
 * <p> bug# 1282 Naming all the file choosers by constructing a FileChooser
 * <p> instance instead of a JFileChooser instance.
 * <p>
 * <p> Revision 3.68  2009/09/23 23:25:26  sueh
 * <p> bug# 1270 Implemented expand.  Changed setAdvanced to updateAdvanced.
 * <p>
 * <p> Revision 3.67  2009/09/20 21:33:58  sueh
 * <p> bug# 1268 Added a default value to LabeledSpinner.
 * <p>
 * <p> Revision 3.66  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.65  2009/02/12 00:03:20  sueh
 * <p> bug# 1152 Shared Single frame label.
 * <p>
 * <p> Revision 3.64  2009/01/20 20:27:11  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.  Changed icon buttons to type SimpleButton and
 * <p> naming them.
 * <p>
 * <p> Revision 3.63  2008/12/01 22:30:40  sueh
 * <p> bug# 1131 Made Montage labels available to package.
 * <p>
 * <p> Revision 3.62  2008/09/10 21:36:08  sueh
 * <p> Handled a null pointer exception in DatasetAction.
 * <p>
 * <p> Revision 3.61  2008/05/03 00:57:05  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.60  2007/12/26 22:28:06  sueh
 * <p> bug# 1052 Turned SetupDialog into an extremely thin GUI.  Moved decisions and
 * <p> knowledge to SetupDialogExpert.
 * <p>
 * <p> Revision 3.59  2007/09/07 00:28:43  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 3.58  2007/08/08 15:04:14  sueh
 * <p> bug# 834 Using UserConfiguration to initialize fields.
 * <p>
 * <p> Revision 3.57  2007/07/17 21:44:35  sueh
 * <p> bug# 1018 Adding cpu.adoc information from CpuAdoc.
 * <p>
 * <p> Revision 3.56  2007/03/07 21:14:04  sueh
 * <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * <p> because it is getting more complicated.
 * <p>
 * <p> Revision 3.55  2007/02/09 00:53:04  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.54  2006/11/07 23:09:00  sueh
 * <p> bug# 954 Added tooltips
 * <p>
 * <p> Revision 3.53  2006/09/21 16:41:08  sueh
 * <p> bug# 680 Changed MRCHeader.x/y/zPixelSize to EtomoNumber.
 * <p>
 * <p> Revision 3.52  2006/07/29 14:36:02  mast
 * <p> Put binning spinner into advanced along with distortion file field
 * <p>
 * <p> Revision 3.51  2006/07/21 22:12:48  sueh
 * <p> bug# 901 Putting the distortion and mag gradient fields into advanced when the
 * <p> distortion directory isn't available.
 * <p>
 * <p> Revision 3.50  2006/07/21 19:17:31  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 3.49  2006/07/20 17:21:47  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 3.48  2006/06/30 20:03:25  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog done() functions,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.  Set displayed to
 * <p> false after the done dialog function is called.
 * <p>
 * <p> Revision 3.47  2006/06/21 15:54:53  sueh
 * <p> bug# 581 Passing manager and axis to ContextPopup, so that imodqtassist can
 * <p> be run.
 * <p>
 * <p> Revision 3.46  2006/04/25 19:20:46  sueh
 * <p> bug# 787 Changed DialogType.SETUP to SETUP_RECON.
 * <p>
 * <p> Revision 3.45  2006/01/11 22:39:53  sueh
 * <p> bug# 675 Made the labels of the dataset name field and fiducial diameter
 * <p> field shareable
 * <p>
 * <p> Revision 3.44  2006/01/03 23:53:29  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.  Converted JRadioButton's
 * <p> toRadioButton.
 * <p>
 * <p> Revision 3.43  2005/12/23 02:22:15  sueh
 * <p> bug# 675 Changed single axis and montage radio buttons to RadioButton.
 * <p>
 * <p> Revision 3.42  2005/12/16 01:47:12  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 3.41  2005/12/14 20:58:50  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 3.40  2005/12/13 02:30:24  sueh
 * <p> bug# 773 Added cbParallelProcess.
 * <p>
 * <p> Revision 3.39  2005/12/05 21:40:12  sueh
 * <p> bug# 674 In btnScanHeaderAction() rounding xPixelSize to 6 significant
 * <p> digits.
 * <p>
 * <p> Revision 3.38  2005/11/14 22:20:45  sueh
 * <p> bug# 762 Made btnDatasetAction() and stateChanged() protected.
 * <p>
 * <p> Revision 3.37  2005/10/27 00:35:39  sueh
 * <p> bug# 725 Setting metadata.bStackProcessed.
 * <p>
 * <p> Revision 3.36  2005/08/22 22:10:27  sueh
 * <p> bug# 714 For dataset name and backup directory, open file chooser in the
 * <p> director in Etomo opened.
 * <p>
 * <p> Revision 3.35  2005/08/12 00:00:10  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.
 * <p>
 * <p> Revision 3.34  2005/08/10 20:47:31  sueh
 * <p> bug# 711 Moved button sizing to MultiLineButton.  SetSize() sets the
 * <p> standard button size.
 * <p>
 * <p> Revision 3.33  2005/08/09 20:53:31  sueh
 * <p> bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * <p> Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * <p> MultiLineButton from JButton.
 * <p>
 * <p> Revision 3.32  2005/08/04 20:17:02  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 3.31  2005/07/29 00:54:40  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.30  2005/06/20 16:58:00  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in btnScanHeaderAction().
 * <p>
 * <p> Revision 3.29  2005/04/26 17:41:39  sueh
 * <p> bug# 615 Change the name of the UIHarness member variable to
 * <p> uiHarness.
 * <p>
 * <p> Revision 3.28  2005/04/25 21:38:48  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.  All Frame functions will use UIHarness as a
 * <p> public interface.
 * <p>
 * <p> Revision 3.27  2005/04/21 20:47:00  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 3.26  2005/04/16 02:04:12  sueh
 * <p> bug# 615 Moved the adding of exit buttons to the base class.
 * <p>
 * <p> Revision 3.25  2005/03/10 18:50:22  sueh
 * <p> bug# 533 Removed new stuff option from montaging functionality.
 * <p>
 * <p> Revision 3.24  2005/03/02 23:15:56  sueh
 * <p> bug# 533 In setup dialog, put the Scan Header button with the header fields.
 * <p>
 * <p> Revision 3.23  2005/03/02 20:26:56  sueh
 * <p> bug# 533 Temporary:  enabling montaging option when
 * <p> EtomoDirector.newstuff is true.  Bug# 533 Added adjustedFocus.  Only
 * <p> used with montaging and mag gradient correction.
 * <p>
 * <p> Revision 3.22  2005/03/02 00:13:29  sueh
 * <p> bug# 611 Added mag gradients correction file.  Bug# 533 Enabled montaging.
 * <p>
 * <p> Revision 3.21  2005/02/15 21:06:43  sueh
 * <p> bug# 603 Removed SectionType radio buttons (single or serial sections)
 * <p> because serial sections are handled by the join interface.
 * <p>
 * <p> Revision 3.20  2005/02/07 22:55:44  sueh
 * <p> bug# 594 Added setDatasetString() to return the value of the dataset field.
 * <p>
 * <p> Revision 3.19  2005/01/14 03:11:06  sueh
 * <p> bug# 511 Added DialogType to super constructor.
 * <p>
 * <p> Revision 3.18  2004/11/20 00:04:14  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.17.2.3  2004/10/11 02:18:09  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 3.17.2.2  2004/10/08 16:41:27  sueh
 * <p> bug# 520 Since EtomoDirector is a singleton, made all functions and
 * <p> member variables non-static.
 * <p>
 * <p> Revision 3.17.2.1  2004/09/15 22:47:53  sueh
 * <p> bug# 520 call openMessageDialog in mainPanel instead of mainFrame.
 * <p>
 * <p> Revision 3.17  2004/08/20 23:07:58  sueh
 * <p> bug# 515 add fields names for error messages about tilt angles
 * <p> Changed:
 * <p> getFields()
 * <p>
 * <p> Revision 3.16  2004/08/20 22:56:19  sueh
 * <p> bug# 515 catching exceptions on numeric fields
 * <p> Changed:
 * <p> getFields()
 * <p>
 * <p> Revision 3.15  2004/07/12 17:39:08  sueh
 * <p> bug# 492 add getDataset() to return a MetaData with the
 * <p> minumum fields needed to run 3dmod
 * <p>
 * <p> Revision 3.14  2004/06/17 18:49:38  sueh
 * <p> bug# 472
 * <p>
 * <p> Revision 3.13  2004/04/26 03:17:54  rickg
 * <p> Normalized button size
 * <p>
 * <p> Revision 3.12  2004/04/06 04:13:28  rickg
 * <p> Updated imageRotation to store axis separately
 * <p>
 * <p> Revision 3.11  2004/03/24 03:02:45  rickg
 * <p> Changed spinner size to only specify spinner region.  The
 * <p> panel and label should be handled automatically
 * <p>
 * <p> Revision 3.10  2004/03/16 00:55:55  rickg
 * <p> Bug# 411 re-layout setup page
 * <p> Add tooltips for image distortion and binning
 * <p>
 * <p> Revision 3.9  2004/03/15 23:14:10  sueh
 * <p> progress button names changed to "btn"
 * <p>
 * <p> Revision 3.8  2004/03/11 01:13:09  sueh
 * <p> bug# 386 retrieved binning from MRCHeader
 * <p>
 * <p> Revision 3.7  2004/03/10 00:43:14  sueh
 * <p> bug# 408 opening distortion file chooser in $IMOD_CALIB_DIR/Distortion, if
 * <p> possible
 * <p>
 * <p> Revision 3.6  2004/02/23 18:22:13  sueh
 * <p> bug# 386 Make distortion file optional
 * <p>
 * <p> Revision 3.5  2004/02/21 00:31:22  sueh
 * <p> bug# 386 validate distortion file
 * <p>
 * <p> Revision 3.4  2004/02/20 23:51:01  sueh
 * <p> bug# 386 added distortionFile chooser and binning spinner
 * <p>
 * <p> Revision 3.3  2004/02/08 18:34:40  sueh
 * <p> bug# 169 Calling imodPreview instead of imodRawStack.
 * <p>
 * <p> Revision 3.2  2003/12/08 22:33:51  sueh
 * <p> bug# 169 adding ViewRawStack button for axis A and B
 * <p>
 * <p> Revision 3.1  2003/11/10 18:50:47  sueh
 * <p> bug332 isValid(): Added call to
 * <p> TiltAngleDialogPanel.getErrorMessage() for Axis' A and B.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.16  2003/11/06 22:45:27  sueh
 * <p> cleaning up task tags and prints
 * <p>
 * <p> Revision 2.15  2003/10/24 00:34:28  sueh
 * <p> Bug271 Prevent dataset name from being "."
 * <p>
 * <p> Revision 2.14  2003/10/23 23:07:44  sueh
 * <p> bug271 added isValid() to contain all SetupDialog validation
 * <p>
 * <p> Revision 2.13  2003/10/23 22:08:28  sueh
 * <p> Bug322 changed labels and a tooltip.
 * <p>
 * <p> Revision 2.12  2003/10/10 22:56:59  sueh
 * <p> bug265
 * <p> changed file.pathSeparator (???:???) to file.separator (???/???)
 * <p>
 * <p> Revision 2.11  2003/10/09 20:27:43  sueh
 * <p> bug264
 * <p> UI Changes
 * <p>
 * <p> Revision 2.10  2003/10/08 22:03:21  sueh
 * <p> Bug263
 * <p> UI Changes
 * <p> Removed data source from Setup dialog.  Removed setDataSource() from MetaData.
 * <p> DataSource is always the default (CCD) in ConstMetaData
 * <p> Grayed out ViewType.
 * <p>
 * <p> Revision 2.9  2003/10/08 21:11:41  sueh
 * <p> bug262
 * <p> UI Change
 * <p> Changed View Type radio button choice
 * <p> from Single View to Single Frame on the Setup dialog.
 * <p>
 * <p> Revision 2.8  2003/10/08 19:12:50  sueh
 * <p> bug261 change changes on the screen:
 * <p> projection -> view
 * <p> raw stack data -> raw image stack
 * <p>
 * <p> Revision 2.7  2003/06/03 23:28:26  rickg
 * <p> Fixed font size ltf at 5 columns for text boxes
 * <p>
 * <p> Revision 2.6  2003/05/20 21:32:54  rickg
 * <p> Added scan header button
 * <p>
 * <p> Revision 2.5  2003/05/12 01:32:25  rickg
 * <p> Working directory calculation works both unix and windows now
 * <p>
 * <p> Revision 2.4  2003/05/07 23:36:12  rickg
 * <p> Updated Data Source labels and tooltips
 * <p>
 * <p> Revision 2.3  2003/05/07 17:47:46  rickg
 * <p> System property user.dir now defines the working directory
 * <p> Added method to get working directy name from current dataset
 * <p> Fixed some tooltips
 * <p>
 * <p> Revision 2.2  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.14.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.14  2003/01/06 20:43:26  rickg
 * <p> Fixed direct entry of fileset name, working directory
 * <p> is taken from app manager
 * <p>
 * <p> Revision 1.13  2003/01/06 20:18:33  rickg
 * <p> Changed edit boxes to LabeledTextFields
 * <p>
 * <p> Revision 1.12  2002/12/19 17:45:22  rickg
 * <p> Implemented advanced dialog state processing
 * <p> including:
 * <p> default advanced state set on start up
 * <p> advanced button management now handled by
 * <p> super class
 * <p>
 * <p> Revision 1.11  2002/12/19 00:30:26  rickg
 * <p> app manager and root pane moved to super class
 * <p>
 * <p> Revision 1.10  2002/12/09 04:17:07  rickg
 * <p> Added stack file filter to open dialog
 * <p>
 * <p> Revision 1.9  2002/11/19 02:34:24  rickg
 * <p> Tooltip spelling correction
 * <p>
 * <p> Revision 1.8  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.7  2002/11/14 04:21:47  rickg
 * <p> HTMLPage and ContextPopup now work with URLS
 * <p>
 * <p> Revision 1.6  2002/10/24 21:12:29  rickg
 * <p> Got folder icon working
 * <p>
 * <p> Revision 1.5  2002/10/24 19:54:52  rickg
 * <p> Moved fileset specification to after axis type specification
 * <p>
 * <p> Revision 1.4  2002/10/22 23:26:22  rickg
 * <p> Merged directory and fileset name to a single UI entity
 * <p>
 * <p> Revision 1.3  2002/10/17 22:40:55  rickg
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
