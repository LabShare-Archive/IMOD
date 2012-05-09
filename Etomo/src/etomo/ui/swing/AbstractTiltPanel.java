package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import etomo.ApplicationManager;
import etomo.ProcessingMethodMediator;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.logic.TomogramTool;
import etomo.storage.CpuAdoc;
import etomo.storage.LogFile;
import etomo.storage.Network;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.MetaData;
import etomo.type.PanelId;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.type.ViewType;
import etomo.util.InvalidParameterException;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.9  2011/06/28 21:49:38  sueh
 * <p> Bug# 1480 Change gpuAvailable to gpusAvailable - non-local GPUs.  Added localGpuAvailable.  Calling
 * <p> updateDisplay from msgMethodChanged so that the display is up to date before the method is
 * <p> changed.  Corrected enabling the GPU checkbox in updateDisplay.
 * <p>
 * <p> Revision 1.8  2011/05/11 01:35:39  sueh
 * <p> bug# 1483 In updateDisplay fixed the boolean value used to call TrialTiltPanel.setResume.
 * <p>
 * <p> Revision 1.7  2011/05/03 03:05:18  sueh
 * <p> bug# 1416 Placed the field change listeners in this class.  Checkpoint from the tilt param (tilt_for_sirt.com).
 * <p> Added fieldChangeAction.  Added overrideable functions isBackProjection and isSirt.
 * <p>
 * <p> Revision 1.6  2011/04/25 23:21:20  sueh
 * <p> bug# 1416 Moved responsibility of when to checkpoint to child class.  Added addStateChangedReporter.
 * <p> Added a numeric type to checkpointed fields that contain a number.
 * <p>
 * <p> Revision 1.5  2011/04/04 17:16:53  sueh
 * <p> bug# 1416 Removed enabled, getEnabled, setEnabled.  Added method, pnlButton, pnlSlicesInY,
 * <p> resume,trialPanel, checkpoint, isChanged, msgTiltComLoaded, msgTiltComSaved, setMethod, setVisible,
 * <p> update.  Modified radialPanel, constructor, action, addListeners, createPanel, done,
 * <p> get3dmodTomogramButton, getParameters, getPRocessingMethod, getTiltButton, initialPanel, setParameters,
 * <p> setTiltButtonTooltip, setToolTipText, updateAdvanced, updateDisplay.
 * <p>
 * <p> Revision 1.4  2011/02/10 04:31:50  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.3  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.2  2010/12/05 04:48:30  sueh
 * <p> bug# 1416 Moved radial filter fields to RadialPanel.  Added setEnabled
 * <p> boolean) to enable/disable all the fields in the panel.  Added booleans for
 * <p> remember the state so enabling all the fields will be down correctly.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:35  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.10  2010/05/27 22:09:06  sueh
 * <p> bug# 1377 Added updateUseGPU and updateParallelProcess calls
 * <p> everywhere these checkboxes are modified.
 * <p>
 * <p> Revision 3.9  2010/04/09 03:01:05  sueh
 * <p> bug# 1352 Passing the ProcessResultDisplay via parameter instead of retrieving it with a function so that it always be passed.
 * <p>
 * <p> Revision 3.8  2010/03/27 04:53:59  sueh
 * <p> bug# 1333 Save parallel processing according the panel ID.  Initialize GPU
 * <p> from default GPU.
 * <p>
 * <p> Revision 3.7  2010/03/12 04:09:06  sueh
 * <p> bug# 1325 Changed the logarithm fields.
 * <p>
 * <p> Revision 3.6  2010/03/05 04:11:07  sueh
 * <p> bug# 1319 Changed all SpacedTextField variables to LabeledTextField to
 * <p> line up the fields better.
 * <p>
 * <p> Revision 3.5  2010/03/05 04:01:37  sueh
 * <p> bug# 1319 Convert ltfLogOffset to ctfLog.  Added linear scale for when
 * <p> the log is turned off.
 * <p>
 * <p> Revision 3.4  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.3  2010/01/11 23:58:46  sueh
 * <p> bug# 1299 Added GPU checkbox.
 * <p>
 * <p> Revision 3.2  2009/09/22 23:43:07  sueh
 * <p> bug# 1269 Moved setEnabledTiltParameters to abstract tilt panel so it can be use by tilt 3dfind.
 * <p>
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
abstract class AbstractTiltPanel implements Expandable, TrialTiltParent,
    Run3dmodButtonContainer, TiltDisplay, ProcessInterface {
  public static final String rcsid = "$Id$";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  // Keep components with listeners private.
  private final Run3dmodButton btn3dmodTomogram = Run3dmodButton.get3dmodInstance(
      "View Tomogram In 3dmod", this);
  private final ActionListener actionListener = new TiltActionListener(this);
  private final JPanel pnlBody = new JPanel();
  private final CheckTextField ctfLog = CheckTextField.getNumericInstance(
      "Take logarithm of densities with offset: ", EtomoNumber.Type.DOUBLE);
  private final LabeledTextField ltfTomoWidth = new LabeledTextField(
      "Tomogram width in X: ");
  final LabeledTextField ltfTomoThickness = LabeledTextField.getNumericInstance(
      "Tomogram thickness in Z: ", EtomoNumber.Type.INTEGER);
  private final LabeledTextField ltfXAxisTilt = LabeledTextField.getNumericInstance(
      "X axis tilt: ", EtomoNumber.Type.DOUBLE);
  private final LabeledTextField ltfTiltAngleOffset = LabeledTextField
      .getNumericInstance("Tilt angle offset: ", EtomoNumber.Type.DOUBLE);
  private final LabeledTextField ltfExtraExcludeList = new LabeledTextField(
      "Extra views to exclude: ");
  private final LabeledTextField ltfLogDensityScaleFactor = LabeledTextField
      .getNumericInstance("Logarithm density scaling factor: ", EtomoNumber.Type.DOUBLE);
  private final LabeledTextField ltfLogDensityScaleOffset = LabeledTextField
      .getNumericInstance(" Offset: ", EtomoNumber.Type.DOUBLE);
  private final LabeledTextField ltfLinearDensityScaleFactor = LabeledTextField
      .getNumericInstance("Linear density scaling factor: ", EtomoNumber.Type.DOUBLE);
  private final LabeledTextField ltfLinearDensityScaleOffset = LabeledTextField
      .getNumericInstance(" Offset: ", EtomoNumber.Type.DOUBLE);
  final LabeledTextField ltfZShift = LabeledTextField.getNumericInstance(" Z shift: ",
      EtomoNumber.Type.DOUBLE);
  private final LabeledTextField ltfXShift = new LabeledTextField("X shift: ");
  private final CheckBox cbUseLocalAlignment = new CheckBox("Use local alignments");
  private final CheckBox cbUseZFactors = new CheckBox("Use Z factors");
  private final LabeledTextField ltfTomoHeight = new LabeledTextField(
      "Tomogram height in Y: ");
  private final LabeledTextField ltfYShift = new LabeledTextField(" Y shift: ");
  private final RadialPanel radialPanel;
  /**
   * cbParallelProcess: Call mediator.msgChangedMethod when
   * cbParallelProcess's value is changed.
   */
  private final CheckBox cbParallelProcess = new CheckBox(ParallelPanel.FIELD_LABEL);
  /**
   * cbUseGpu: Enable/disable cbUseGpu by changing gpuEnabled and calling
   * updateDisplay.  Call mediator.msgChangedMethod when cbUseGpu's value is
   * changed.
   */
  private final CheckBox cbUseGpu = new CheckBox(
      "Use the GPU:  Maximum number of GPUs recommended is 3");
  private final SpacedPanel trialPanel = SpacedPanel.getInstance();
  private final SpacedPanel pnlButton = SpacedPanel.getInstance(true);

  private final PanelHeader header;
  final ApplicationManager manager;
  final AxisID axisID;
  final DialogType dialogType;
  private final TrialTiltPanel trialTiltPanel;
  // Keep components with listeners private.
  private final Run3dmodButton btnTilt;
  private final MultiLineButton btnDeleteStack;
  private final PanelId panelId;
  final ProcessingMethodMediator mediator;
  private final boolean listenForFieldChanges;

  private boolean gpusAvailable = false;
  private boolean nonLocalHostGpusAvailable = false;
  private boolean localGpuAvailable = true;
  private boolean gpuEnabled = true;
  private boolean madeZFactors = false;
  private boolean newstFiducialessAlignment = false;
  private boolean usedLocalAlignments = false;
  private boolean processingMethodLocked = false;

  abstract void tiltAction(ProcessResultDisplay processResultDisplay,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions, ProcessingMethod tiltProcessingMethod);

  abstract void imodTomogramAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions);

  // backward compatibility functionality - if the metadata binning is missing
  // get binning from newst
  AbstractTiltPanel(final ApplicationManager manager, final AxisID axisID,
      final DialogType dialogType, final GlobalExpandButton globalAdvancedButton,
      final PanelId panelId, final boolean listenForFieldChanges) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    this.panelId = panelId;
    this.listenForFieldChanges = listenForFieldChanges;
    radialPanel = RadialPanel.getInstance(manager, axisID, panelId);
    mediator = manager.getProcessingMethodMediator(axisID);
    header = PanelHeader.getAdvancedBasicInstance("Tilt", this, dialogType,
        globalAdvancedButton);
    trialTiltPanel = TrialTiltPanel.getInstance(manager, axisID, dialogType, this);
    ProcessResultDisplayFactory displayFactory = manager
        .getProcessResultDisplayFactory(axisID);
    btnTilt = (Run3dmodButton) displayFactory.getTilt(dialogType);
    btnDeleteStack = (MultiLineButton) displayFactory.getDeleteAlignedStack();
  }

  final void initializePanel() {
    btnTilt.setSize();
    btnTilt.setContainer(this);
    btnTilt.setDeferred3dmodButton(btn3dmodTomogram);
    btn3dmodTomogram.setSize();
    btnDeleteStack.setSize();
    ConstEtomoNumber maxCPUs = CpuAdoc.INSTANCE.getMaxTilt(manager, axisID,
        manager.getPropertyUserDir());
    if (maxCPUs != null && !maxCPUs.isNull()) {
      cbParallelProcess.setText(ParallelPanel.FIELD_LABEL + ParallelPanel.MAX_CPUS_STRING
          + maxCPUs.toString());
    }
  }

  void createPanel() {
    // Initialize
    initializePanel();
    ltfLinearDensityScaleFactor.setText(TiltParam.LINEAR_SCALE_FACTOR_DEFAULT);
    ltfLinearDensityScaleOffset.setText(TiltParam.LINEAR_SCALE_OFFSET_DEFAULT);
    ltfTomoWidth.setPreferredWidth(163);
    ltfTomoHeight.setPreferredWidth(159);
    // local panels
    JPanel pnlLogDensity = new JPanel();
    JPanel pnlLinearDensity = new JPanel();
    JPanel pnlCheckBox = new JPanel();
    JPanel pnlX = new JPanel();
    JPanel pnlY = new JPanel();
    JPanel pnlZ = new JPanel();
    // Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(BorderFactory.createEtchedBorder());
    pnlRoot.add(header);
    pnlRoot.add(pnlBody);
    UIUtilities.alignComponentsX(pnlRoot.getContainer(), Component.LEFT_ALIGNMENT);
    // Body panel
    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlBody.add(cbParallelProcess);
    pnlBody.add(cbUseGpu);
    pnlBody.add(ctfLog.getRootComponent());
    pnlBody.add(pnlLogDensity);
    pnlBody.add(pnlLinearDensity);
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlBody.add(pnlX);
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlBody.add(pnlY);
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y3));
    pnlBody.add(pnlZ);
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlBody.add(ltfXAxisTilt.getContainer());
    pnlBody.add(ltfTiltAngleOffset.getContainer());
    pnlBody.add(radialPanel.getRoot());
    pnlBody.add(ltfExtraExcludeList.getContainer());
    pnlBody.add(pnlCheckBox);
    pnlBody.add(trialPanel.getContainer());
    pnlBody.add(pnlButton.getContainer());
    UIUtilities.alignComponentsX(pnlBody, Component.LEFT_ALIGNMENT);
    // Log density panel
    pnlLogDensity.setLayout(new BoxLayout(pnlLogDensity, BoxLayout.X_AXIS));
    pnlLogDensity.add(ltfLogDensityScaleFactor.getContainer());
    pnlLogDensity.add(ltfLogDensityScaleOffset.getContainer());
    UIUtilities.alignComponentsX(pnlLogDensity, Component.LEFT_ALIGNMENT);
    // Linear density panel
    pnlLinearDensity.setLayout(new BoxLayout(pnlLinearDensity, BoxLayout.X_AXIS));
    pnlLinearDensity.add(ltfLinearDensityScaleFactor.getContainer());
    pnlLinearDensity.add(ltfLinearDensityScaleOffset.getContainer());
    UIUtilities.alignComponentsX(pnlLinearDensity, Component.LEFT_ALIGNMENT);
    // X panel
    pnlX.setLayout(new BoxLayout(pnlX, BoxLayout.X_AXIS));
    pnlX.add(ltfTomoWidth.getContainer());
    pnlX.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlX.add(ltfXShift.getContainer());
    // Y panel
    pnlY.setLayout(new BoxLayout(pnlY, BoxLayout.X_AXIS));
    pnlY.add(ltfTomoHeight.getContainer());
    pnlY.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlY.add(ltfYShift.getContainer());
    // Z panel
    pnlZ.setLayout(new BoxLayout(pnlZ, BoxLayout.X_AXIS));
    pnlZ.add(ltfTomoThickness.getContainer());
    pnlZ.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlZ.add(ltfZShift.getContainer());
    // Check box panel
    pnlCheckBox.setLayout(new BoxLayout(pnlCheckBox, BoxLayout.Y_AXIS));
    pnlCheckBox.add(cbUseLocalAlignment);
    pnlCheckBox.add(cbUseZFactors);
    UIUtilities.alignComponentsX(pnlCheckBox, Component.LEFT_ALIGNMENT);
    // Trial panel
    trialPanel.setBoxLayout(BoxLayout.X_AXIS);
    trialPanel.add(trialTiltPanel.getComponent());
    trialPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    // Button panel
    pnlButton.setBoxLayout(BoxLayout.X_AXIS);
    pnlButton.add(btnTilt);
    pnlButton.add(btn3dmodTomogram);
    pnlButton.add(btnDeleteStack);
    pnlButton.alignComponentsX(Component.LEFT_ALIGNMENT);
  }

  final SpacedPanel getRootPanel() {
    return pnlRoot;
  }

  final PanelId getPanelId() {
    return panelId;
  }

  final Component getTiltButton() {
    return btnTilt.getComponent();
  }

  final Component get3dmodTomogramButton() {
    return btn3dmodTomogram.getComponent();
  }

  final Component getParallelProcessCheckBox() {
    return cbParallelProcess;
  }

  final Component getUseGpuCheckBox() {
    return cbUseGpu;
  }

  final void setTiltButtonTooltip(String tooltip) {
    btnTilt.setToolTipText(tooltip);
  }

  void addListeners() {
    btnTilt.addActionListener(actionListener);
    btn3dmodTomogram.addActionListener(actionListener);
    btnDeleteStack.addActionListener(actionListener);
    cbParallelProcess.addActionListener(actionListener);
    cbUseGpu.addActionListener(actionListener);
    ctfLog.addActionListener(actionListener);
    // If the child class has field listeners, it must set listenForFieldChanges to true.
    // Otherwise changes in these fields will be ignored.
    if (listenForFieldChanges) {
      cbUseLocalAlignment.addActionListener(actionListener);
      cbUseZFactors.addActionListener(actionListener);
      DocumentListener documentListener = new TiltDocumentListener(this);
      ctfLog.addDocumentListener(documentListener);
      ltfLogDensityScaleFactor.addDocumentListener(documentListener);
      ltfLogDensityScaleOffset.addDocumentListener(documentListener);
      ltfLinearDensityScaleFactor.addDocumentListener(documentListener);
      ltfLinearDensityScaleOffset.addDocumentListener(documentListener);
      ltfTomoThickness.addDocumentListener(documentListener);
      ltfZShift.addDocumentListener(documentListener);
      ltfXAxisTilt.addDocumentListener(documentListener);
      ltfTiltAngleOffset.addDocumentListener(documentListener);
      ltfExtraExcludeList.addDocumentListener(documentListener);
    }
  }

  Component getRoot() {
    return pnlRoot.getContainer();
  }

  public boolean allowTiltComSave() {
    return true;
  }

  /**
   * Checks the fields that are listened to when listenForFieldChanges is true.  Returns
   * true if any of them are different from their checkpoint or haven't been checkpointed.
   * It is unnecessary to check whether a field is active, because a disabled or invisible
   * field returns false.
   * @return
   */
  boolean isDifferentFromCheckpoint() {
    if (ctfLog.isDifferentFromCheckpoint()
        || ltfLogDensityScaleFactor.isDifferentFromCheckpoint()
        || ltfLogDensityScaleOffset.isDifferentFromCheckpoint()
        || ltfLinearDensityScaleFactor.isDifferentFromCheckpoint()
        || ltfLinearDensityScaleOffset.isDifferentFromCheckpoint()
        || ltfTomoThickness.isDifferentFromCheckpoint()
        || ltfZShift.isDifferentFromCheckpoint()
        || ltfXAxisTilt.isDifferentFromCheckpoint()
        || ltfTiltAngleOffset.isDifferentFromCheckpoint()
        || ltfExtraExcludeList.isDifferentFromCheckpoint()
        || cbUseLocalAlignment.isDifferentFromCheckpoint()
        || cbUseZFactors.isDifferentFromCheckpoint()) {
      return true;
    }
    return false;
  }

  public final void expand(final GlobalExpandButton button) {
  }

  public final void expand(final ExpandButton button) {
    if (header != null) {
      if (header.equalsOpenClose(button)) {
        pnlBody.setVisible(button.isExpanded());
      }
      else if (header.equalsAdvancedBasic(button)) {
        updateAdvanced(button.isExpanded());
      }
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  final void msgMethodChanged() {
    setVisible(header.isAdvanced());
    updateDisplay();
    mediator.setMethod(this, getProcessingMethod());
  }

  void updateAdvanced(final boolean advanced) {
    setVisible(advanced);
  }

  /**
   * Call fields' and panels' setVisible functions based on advanced and the current
   * method.
   * @param advanced
   */
  void setVisible(final boolean advanced) {
    boolean backProjection = isBackProjection();
    cbParallelProcess.setVisible(backProjection);
    ltfLogDensityScaleOffset.setVisible(advanced);
    ltfLogDensityScaleFactor.setVisible(advanced);
    ltfLinearDensityScaleOffset.setVisible(advanced);
    ltfLinearDensityScaleFactor.setVisible(advanced);
    ltfTomoWidth.setVisible(advanced && backProjection);
    ltfTomoHeight.setVisible(advanced && backProjection);
    ltfYShift.setVisible(advanced && backProjection);
    ltfXShift.setVisible(advanced && backProjection);
    // Z shift is not always an advanced field.
    ltfTiltAngleOffset.setVisible(advanced);
    radialPanel.setVisible(backProjection);
    ltfExtraExcludeList.setVisible(advanced);
    trialPanel.setVisible(backProjection);
    trialTiltPanel.setVisible(advanced);
    pnlButton.setVisible(backProjection);
  }

  final void done() {
    btnTilt.removeActionListener(actionListener);
    btnDeleteStack.removeActionListener(actionListener);
    trialTiltPanel.done();
    mediator.deregister(this);
  }

  boolean isResume() {
    return false;
  }

  boolean isBackProjection() {
    return true;
  }

  boolean isSirt() {
    return false;
  }

  void updateDisplay() {
    boolean backProjection = isBackProjection();
    boolean resume = isResume();
    btn3dmodTomogram.setEnabled(backProjection || !resume);
    ctfLog.setEnabled(backProjection || !resume);
    cbParallelProcess.setEnabled(!processingMethodLocked);
    ltfTomoWidth.setEnabled(backProjection || !resume);
    ltfTomoThickness.setEnabled(backProjection || !resume);
    ltfXAxisTilt.setEnabled(backProjection || !resume);
    ltfTiltAngleOffset.setEnabled(backProjection || !resume);
    ltfExtraExcludeList.setEnabled(backProjection || !resume);

    boolean logIsSelected = ctfLog.isSelected();
    ltfLogDensityScaleFactor.setEnabled(logIsSelected && (backProjection || !resume));
    ltfLogDensityScaleOffset.setEnabled(logIsSelected && (backProjection || !resume));
    ltfLinearDensityScaleFactor.setEnabled(!logIsSelected && (backProjection || !resume));
    ltfLinearDensityScaleOffset.setEnabled(!logIsSelected && (backProjection || !resume));

    ltfTomoHeight.setEnabled(backProjection || !resume);
    ltfYShift.setEnabled(backProjection || !resume);
    ltfZShift.setEnabled(backProjection || !resume);
    ltfXShift.setEnabled(backProjection || !resume);
    radialPanel.setEnabled(backProjection || !resume);

    cbUseLocalAlignment.setEnabled(usedLocalAlignments && !newstFiducialessAlignment
        && (backProjection || !resume));
    cbUseZFactors.setEnabled(madeZFactors && !newstFiducialessAlignment
        && (backProjection || !resume));
    // A local GPU installed on this computer means that GPU processing is available
    // with or without parallel processing. Non-local GPU(s) installed in the network mean
    // that GPU processing is available with parallel processing.
    cbUseGpu.setEnabled((((gpusAvailable || localGpuAvailable) && (cbParallelProcess
        .isSelected() || isSirt())) || (localGpuAvailable
        && !cbParallelProcess.isSelected() && !isSirt()))
        && gpuEnabled && !processingMethodLocked);
    trialTiltPanel.setResume(!backProjection && resume);
    btnTilt.setEnabled(backProjection || !resume);
    btnDeleteStack.setEnabled(backProjection || !resume);
  }

  final boolean isParallelProcess() {
    return cbParallelProcess.isSelected();
  }

  final boolean isZShiftSet() {
    return ltfZShift.getText().matches("\\S+");
  }

  final boolean isUseLocalAlignment() {
    return cbUseLocalAlignment.isSelected();
  }

  void setState(TomogramState state, ConstMetaData metaData) {
    // madeZFactors
    if (!state.getMadeZFactors(axisID).isNull()) {
      madeZFactors = state.getMadeZFactors(axisID).is();
    }
    else {
      madeZFactors = state.getBackwardCompatibleMadeZFactors(axisID);
    }
    // newstFiducialessAlignment
    if (!state.getNewstFiducialessAlignment(axisID).isNull()) {
      newstFiducialessAlignment = state.getNewstFiducialessAlignment(axisID).is();
    }
    else {
      newstFiducialessAlignment = metaData.isFiducialessAlignment(axisID);
    }
    // usedLocalAlignments
    if (!state.getUsedLocalAlignments(axisID).isNull()) {
      usedLocalAlignments = state.getUsedLocalAlignments(axisID).is();
    }
    else {
      usedLocalAlignments = state.getBackwardCompatibleUsedLocalAlignments(axisID);
    }
    updateDisplay();
  }

  boolean isUseZFactors() {
    return cbUseZFactors.isSelected();
  }

  void getParameters(final MetaData metaData) throws FortranInputSyntaxException {
    metaData.setTiltParallel(axisID, panelId, isParallelProcess());
    trialTiltPanel.getParameters(metaData);
    metaData.setGenLog(axisID, ctfLog.getText());
    metaData.setGenScaleFactorLog(axisID, ltfLogDensityScaleFactor.getText());
    metaData.setGenScaleOffsetLog(axisID, ltfLogDensityScaleOffset.getText());
    metaData.setGenScaleFactorLinear(axisID, ltfLinearDensityScaleFactor.getText());
    metaData.setGenScaleOffsetLinear(axisID, ltfLinearDensityScaleOffset.getText());
  }

  final void getParameters(final ReconScreenState screenState) {
    header.getState(screenState.getTomoGenTiltHeaderState());
    trialTiltPanel.getParameters(screenState);
  }

  final void setParameters(final ConstMetaData metaData) {
    // Parallel processing is optional in tomogram reconstruction, so only use it
    // if the user set it up.
    boolean validAutodoc = Network.isParallelProcessingEnabled(manager, axisID,
        manager.getPropertyUserDir());
    // Use GPU
    gpusAvailable = Network.isNonLocalOnlyGpuProcessingEnabled(manager, axisID,
        manager.getPropertyUserDir());
    nonLocalHostGpusAvailable = Network.isNonLocalHostGpuProcessingEnabled(manager,
        axisID, manager.getPropertyUserDir());
    localGpuAvailable = Network.isLocalHostGpuProcessingEnabled(manager, axisID,
        manager.getPropertyUserDir());
    cbUseGpu.setSelected(metaData.getDefaultGpuProcessing().is());
    // updateUseGpu();
    // Parallel processing
    cbParallelProcess.setEnabled(validAutodoc);
    ConstEtomoNumber tiltParallel = metaData.getTiltParallel(axisID, panelId);
    // If only a local GPU is available and the Use GPU checkbox defaults to on, do not
    // select the parallel processing checkbox.
    if (nonLocalHostGpusAvailable || !cbUseGpu.isSelected()) {
      if (tiltParallel == null) {
        cbParallelProcess.setSelected(validAutodoc && metaData.getDefaultParallel().is());
      }
      else {
        cbParallelProcess.setSelected(validAutodoc && tiltParallel.is());
      }
    }
    trialTiltPanel.setParameters(metaData);
    ctfLog.setText(metaData.getGenLog(axisID));
    ltfLogDensityScaleFactor.setText(metaData.getGenScaleFactorLog(axisID));
    ltfLogDensityScaleOffset.setText(metaData.getGenScaleOffsetLog(axisID));
    if (metaData.isGenScaleFactorLinearSet(axisID)) {
      ltfLinearDensityScaleFactor.setText(metaData.getGenScaleFactorLinear(axisID));
    }
    if (metaData.isGenScaleOffsetLinearSet(axisID)) {
      ltfLinearDensityScaleOffset.setText(metaData.getGenScaleOffsetLinear(axisID));
    }
    updateDisplay();
    mediator.setMethod(this, getProcessingMethod());
  }

  public void disableGpu(final boolean disable) {
    gpuEnabled = !disable;
    updateDisplay();
  }

  public void lockProcessingMethod(final boolean lock) {
    processingMethodLocked = lock;
    updateDisplay();

  }

  public ProcessingMethod getProcessingMethod() {
    // SIRT must use parallel processing.
    boolean parallelProcess = (cbParallelProcess.isEnabled() && cbParallelProcess
        .isSelected()) || isSirt();
    if (parallelProcess) {
      if (cbUseGpu.isEnabled() && cbUseGpu.isSelected()) {
        return ProcessingMethod.PP_GPU;
      }
      return ProcessingMethod.PP_CPU;
    }
    if (cbUseGpu.isEnabled() && cbUseGpu.isSelected()) {
      return ProcessingMethod.LOCAL_GPU;
    }
    return ProcessingMethod.LOCAL_CPU;
  }

  void registerProcessingMethodMediator() {
    mediator.register(this);
  }

  /**
   * Checkpoint fields that are listened to when listenForFieldChanges is true. 
   * @param tiltParam
   */
  void checkpoint(final ConstTiltParam tiltParam) {
    ctfLog.checkpoint(tiltParam.hasLogOffset(), tiltParam.getLogShift());
    ltfLogDensityScaleFactor.checkpoint(tiltParam.getScaleCoeff());
    ltfLogDensityScaleOffset.checkpoint(tiltParam.getScaleFLevel());
    ltfLinearDensityScaleFactor.checkpoint(tiltParam.getScaleCoeff());
    ltfLinearDensityScaleOffset.checkpoint(tiltParam.getScaleFLevel());
    ltfTomoThickness.checkpoint(tiltParam.getThickness());
    ltfZShift.checkpoint(tiltParam.getZShift());
    ltfXAxisTilt.checkpoint(tiltParam.getXAxisTilt());
    ltfTiltAngleOffset.checkpoint(tiltParam.getTiltAngleOffset());
    ltfExtraExcludeList.checkpoint(tiltParam.getExcludeList2());
    cbUseLocalAlignment.checkpoint(tiltParam.hasLocalAlignFile());
    cbUseZFactors.checkpoint(tiltParam.hasZFactorFileName());
  }

  /**
   * Set the UI parameters with the specified tiltParam values
   * WARNING: be sure the setNewstParam is called first so the binning value for
   * the stack is known.  The thickness, first and last slice, width and x,y,z
   * offsets are scaled so that they are represented to the user in unbinned
   * dimensions.
   * @param tiltParam
   * @param initialize - true when the dialog is first created for the dataset
   */
  void setParameters(final ConstTiltParam tiltParam, boolean initialize) {
    if (tiltParam.hasWidth()) {
      ltfTomoWidth.setText(tiltParam.getWidth());
    }
    if (tiltParam.hasThickness()) {
      ltfTomoThickness.setText(tiltParam.getThickness());
    }
    if (tiltParam.hasXShift()) {
      ltfXShift.setText(tiltParam.getXShift());
    }
    if (tiltParam.hasZShift()) {
      ltfZShift.setText(tiltParam.getZShift());
    }
    if (tiltParam.hasSlice()) {
      long[] yHeightAndShift = TomogramTool.getYHeightAndShift(manager, axisID,
          tiltParam.getIdxSliceStart(), tiltParam.getIdxSliceStop());
      if (yHeightAndShift != null && yHeightAndShift.length == 2) {
        ltfTomoHeight.setText(yHeightAndShift[0]);
        ltfYShift.setText(yHeightAndShift[1]);
      }
    }
    if (tiltParam.hasXAxisTilt()) {
      ltfXAxisTilt.setText(tiltParam.getXAxisTilt());
    }
    if (tiltParam.hasTiltAngleOffset()) {
      ltfTiltAngleOffset.setText(tiltParam.getTiltAngleOffset());
    }
    radialPanel.setParameters(tiltParam);
    ctfLog.setSelected(tiltParam.hasLogOffset());
    boolean log = tiltParam.hasLogOffset();
    // If initialize is true, get defaults from tilt.com
    if (log || initialize) {
      ctfLog.setText(tiltParam.getLogShift());
    }
    if ((log || initialize) && tiltParam.hasScale()) {
      ltfLogDensityScaleOffset.setText(tiltParam.getScaleFLevel());
      ltfLogDensityScaleFactor.setText(tiltParam.getScaleCoeff());
    }
    if (!log && tiltParam.hasScale()) {
      ltfLinearDensityScaleOffset.setText(tiltParam.getScaleFLevel());
      ltfLinearDensityScaleFactor.setText(tiltParam.getScaleCoeff());
    }
    if (initialize && log) {
      EtomoNumber logScale = new EtomoNumber(EtomoNumber.Type.FLOAT);
      logScale.set(ltfLogDensityScaleFactor.getText());
      if (log && !logScale.isNull() && logScale.isValid()) {
        ltfLinearDensityScaleFactor
            .setText(Math.round(logScale.getFloat() / 5000. * 10.) / 10.);
      }
    }
    if (!initialize) {
      // During initialization the value should coming from setup
      cbUseGpu.setSelected(tiltParam.isUseGpu());
      // updateUseGpu();
    }
    MetaData metaData = manager.getMetaData();
    cbUseLocalAlignment.setSelected(metaData.getUseLocalAlignments(axisID));
    cbUseZFactors.setSelected(metaData.getUseZFactors(axisID).is());
    ltfExtraExcludeList.setText(tiltParam.getExcludeList2());
    updateDisplay();
    mediator.setMethod(this, getProcessingMethod());
  }

  final void setParameters(final ReconScreenState screenState) {
    header.setState(screenState.getTomoGenTiltHeaderState());
    btnTilt.setButtonState(screenState.getButtonState(btnTilt.getButtonStateKey()));
    btnDeleteStack.setButtonState(screenState.getButtonState(btnDeleteStack
        .getButtonStateKey()));
  }

  public boolean getParameters(final SplittiltParam param) {
    ParallelPanel parallelPanel = manager.getMainPanel().getParallelPanel(axisID);
    if (parallelPanel == null) {
      return false;
    }
    ConstEtomoNumber numMachines = param.setNumMachines(parallelPanel.getCPUsSelected());
    if (!numMachines.isValid()) {
      if (numMachines.equals(0)) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            parallelPanel.getNoCpusSelectedErrorMessage(), "Unable to run splittilt",
            axisID);
        return false;
      }
      else {
        UIHarness.INSTANCE.openMessageDialog(manager,
            parallelPanel.getCPUsSelectedLabel() + " " + numMachines.getInvalidReason(),
            "Unable to run splittilt", axisID);
        return false;
      }
    }
    return true;
  }

  /**
   * Get the tilt parameters from the requested axis panel
   */
  public boolean getParameters(final TiltParam tiltParam) throws NumberFormatException,
      InvalidParameterException, IOException {
    if (isResume()) {
      return true;
    }
    radialPanel.getParameters(tiltParam);
    String badParameter = "";
    try {
      badParameter = "IMAGEBINNED";
      tiltParam.setImageBinned();
      // Do not manage full image size. It is coming from copytomocoms.
      if (ltfTomoWidth.getText().matches("\\S+")) {
        badParameter = ltfTomoWidth.getLabel();
        tiltParam.setWidth(Integer.parseInt(ltfTomoWidth.getText()));
      }
      else {
        tiltParam.resetWidth();
      }

      // set Z Shift
      if (isZShiftSet()) {
        badParameter = ltfZShift.getLabel();
        tiltParam.setZShift(ltfZShift.getText());
      }
      else {
        tiltParam.resetZShift();
      }
      // set X Shift
      if (ltfXShift.getText().matches("\\S+")) {
        badParameter = ltfXShift.getLabel();
        tiltParam.setXShift(Float.parseFloat(ltfXShift.getText()));
      }
      else if (isZShiftSet()) {
        tiltParam.setXShift(0);
        ltfXShift.setText(0);
      }
      else {
        tiltParam.resetXShift();
      }

      ConstEtomoNumber startingSlice = TomogramTool.getYStartingSlice(manager, axisID,
          ltfTomoHeight.getText(), ltfYShift.getText(), ltfTomoHeight.getQuotedLabel(),
          ltfYShift.getQuotedLabel());
      if (startingSlice == null) {
        return false;
      }
      if (startingSlice.isNull()) {
        tiltParam.resetIdxSlice();
      }
      else {
        ConstEtomoNumber endingSlice = TomogramTool.getYEndingSlice(manager, axisID,
            startingSlice, ltfTomoHeight.getText(), ltfTomoHeight.getQuotedLabel());
        if (endingSlice == null) {
          return false;
        }
        if (endingSlice.isNull()) {
          tiltParam.resetIdxSlice();
        }
        else {
          tiltParam.setIdxSliceStart(startingSlice.getLong());
          tiltParam.setIdxSliceStop(endingSlice.getLong());
        }
      }

      if (ltfTomoThickness.getText().matches("\\S+")) {
        badParameter = ltfTomoThickness.getLabel();
        tiltParam.setThickness(ltfTomoThickness.getText());
      }
      else {
        tiltParam.resetThickness();
      }

      if (ltfXAxisTilt.getText().matches("\\S+")) {
        badParameter = ltfXAxisTilt.getLabel();
        tiltParam.setXAxisTilt(ltfXAxisTilt.getText());
      }
      else {
        tiltParam.resetXAxisTilt();
      }

      if (ltfTiltAngleOffset.getText().matches("\\S+")) {
        badParameter = ltfTiltAngleOffset.getLabel();
        tiltParam.setTiltAngleOffset(ltfTiltAngleOffset.getText());
      }
      else {
        tiltParam.resetTiltAngleOffset();
      }

      if (ltfLogDensityScaleOffset.isEnabled()
          && (ltfLogDensityScaleOffset.getText().matches("\\S+") || ltfLogDensityScaleFactor
              .getText().matches("\\S+"))) {
        badParameter = ltfLogDensityScaleFactor.getLabel();
        tiltParam.setScaleCoeff(Float.parseFloat(ltfLogDensityScaleFactor.getText()));
        badParameter = ltfLogDensityScaleOffset.getLabel();
        tiltParam.setScaleFLevel(Float.parseFloat(ltfLogDensityScaleOffset.getText()));
      }
      else if (ltfLinearDensityScaleOffset.isEnabled()
          && (ltfLinearDensityScaleOffset.getText().matches("\\S+") || ltfLinearDensityScaleFactor
              .getText().matches("\\S+"))) {
        badParameter = ltfLinearDensityScaleFactor.getLabel();
        tiltParam.setScaleCoeff(Float.parseFloat(ltfLinearDensityScaleFactor.getText()));
        badParameter = ltfLinearDensityScaleOffset.getLabel();
        tiltParam.setScaleFLevel(Float.parseFloat(ltfLinearDensityScaleOffset.getText()));
      }
      else {
        tiltParam.resetScale();
      }

      if (ctfLog.isSelected() && ctfLog.getText().matches("\\S+")) {
        badParameter = ctfLog.getLabel();
        tiltParam.setLogShift(Float.parseFloat(ctfLog.getText()));
      }
      else {
        tiltParam.setLogShift(Float.NaN);
      }

      MetaData metaData = manager.getMetaData();
      if (isUseLocalAlignment() && cbUseLocalAlignment.isEnabled()) {
        tiltParam.setLocalAlignFile(metaData.getDatasetName() + axisID.getExtension()
            + "local.xf");
      }
      else {
        tiltParam.setLocalAlignFile("");
      }
      metaData.setUseLocalAlignments(axisID, isUseLocalAlignment());
      // TiltParam.fiducialess is based on whether final alignment was run
      // fiducialess.
      // newstFiducialessAlignment
      boolean newstFiducialessAlignment = false;
      TomogramState state = manager.getState();
      if (!state.getNewstFiducialessAlignment(axisID).isNull()) {
        newstFiducialessAlignment = state.getNewstFiducialessAlignment(axisID).is();
      }
      else {
        newstFiducialessAlignment = metaData.isFiducialessAlignment(axisID);
      }
      tiltParam.setFiducialess(newstFiducialessAlignment);

      tiltParam.setUseZFactors(isUseZFactors() && cbUseZFactors.isEnabled());
      metaData.setUseZFactors(axisID, isUseZFactors());
      tiltParam.setExcludeList2(ltfExtraExcludeList.getText());
      badParameter = TiltParam.SUBSETSTART_KEY;
      if (metaData.getViewType() == ViewType.MONTAGE) {
        tiltParam.setMontageSubsetStart();
      }
      else if (!tiltParam.setSubsetStart()) {
        return false;
      }
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
    catch (IOException e) {
      e.printStackTrace();
      throw new IOException(badParameter + ":  " + e.getMessage());
    }
    tiltParam.setUseGpu(cbUseGpu.isEnabled() && cbUseGpu.isSelected());
    return true;
  }

  public final void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  void fieldChangeAction() {
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  final void action(final String command, final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnTilt.getActionCommand())) {
      tiltAction(btnTilt, deferred3dmodButton, run3dmodMenuOptions,
          mediator.getRunMethodForProcessInterface(getProcessingMethod()));
    }
    else if (command.equals(btnDeleteStack.getActionCommand())) {
      manager.deleteIntermediateImageStacks(axisID, btnDeleteStack);
    }
    else if (command.equals(cbParallelProcess.getActionCommand())) {
      msgParallelProcessChanged();
      mediator.setMethod(this, getProcessingMethod());
    }
    else if (command.equals(btn3dmodTomogram.getActionCommand())) {
      imodTomogramAction(deferred3dmodButton, run3dmodMenuOptions);
    }
    else if (command.equals(cbUseGpu.getActionCommand())) {
      msgUseGpuChanged();
      mediator.setMethod(this, getProcessingMethod());
    }
    else if (command.equals(ctfLog.getActionCommand())) {
      updateDisplay();
      fieldChangeAction();
    }
    else if (command.equals(cbUseLocalAlignment.getActionCommand())
        || command.equals(cbUseZFactors.getActionCommand())) {
      fieldChangeAction();
    }
    else {
      updateDisplay();
    }
  }

  /**
   * If there is a only a local GPU available, turn off parallel processing when Use GPU
   * is selected.
   */
  private void msgUseGpuChanged() {
    if (!cbUseGpu.isSelected()) {
      return;
    }
    if (!nonLocalHostGpusAvailable && cbParallelProcess.isSelected()) {
      cbParallelProcess.setSelected(false);
    }
  }

  /**
   * If there is a only a local GPU available, turn off Use GPU when parallel processing
   * is selected.
   */
  private void msgParallelProcessChanged() {
    if (!cbParallelProcess.isSelected()) {
      return;
    }
    if (!nonLocalHostGpusAvailable && cbUseGpu.isSelected()) {
      cbUseGpu.setSelected(false);
    }
  }

  private void documentAction() {
    fieldChangeAction();
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;

    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.TILT, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    cbParallelProcess
        .setToolTipText("Check to distribute the tilt process across multiple computers.");
    cbUseGpu.setToolTipText("Check to run the tilt process on the graphics card.");
    ltfTomoThickness
        .setToolTipText("Thickness, in unbinned pixels, along the z-axis of the "
            + "reconstructed volume.");
    ltfTomoHeight
        .setToolTipText("This entry specifies the Y extent of the output tomogram, in "
            + "unbinned pixels; the default is the height of the aligned stack.");
    ltfYShift
        .setToolTipText("Amount to shift the reconstructed region in Y, in unbinned pixels.  "
            + "A positive value will shift the region upward and reconstruct an area lower "
            + "in Y.");
    ltfTomoWidth
        .setToolTipText("This entry specifies the width, in unbinned pixels, of the output "
            + "image; the default is the width of the input image.");
    ltfXShift
        .setToolTipText("Amount, in unbinned pixels, to shift the reconstructed slices in "
            + "X before output.  A positive value will shift the slice to the right, and "
            + "the output will contain the left part of the whole potentially "
            + "reconstructable area.");
    ltfZShift
        .setToolTipText("Amount, in unbinned pixels, to shift the reconstructed slices in "
            + "Z before output.  A positive value will shift the slice upward.");
    ltfXAxisTilt.setToolTipText(TomogramGenerationDialog.X_AXIS_TILT_TOOLTIP);
    ltfTiltAngleOffset
        .setToolTipText("Offset in degrees to apply to the tilt angles; a positive offset will "
            + "rotate the reconstructed slices counterclockwise.");
    ltfLogDensityScaleOffset
        .setToolTipText("Amount to add to reconstructed density values before multiplying by"
            + " the scale factor and outputting the values.");
    ltfLogDensityScaleFactor
        .setToolTipText("Amount to multiply reconstructed density values by, after adding the "
            + "offset value.");
    ltfLinearDensityScaleOffset
        .setToolTipText("Amount to add to reconstructed density values before multiplying by"
            + " the scale factor and outputting the values.");
    ltfLinearDensityScaleFactor
        .setToolTipText("Amount to multiply reconstructed density values by, after adding the "
            + "offset value.");
    ctfLog
        .setToolTipText("This parameter allows one to generate a reconstruction using the "
            + "logarithm of the densities in the input file, with the value "
            + "specified added before taking the logarithm.  If no parameter is "
            + "specified the logarithm of the input data is not taken.");
    cbUseLocalAlignment
        .setToolTipText("Select this checkbox to use local alignments.  You must have "
            + "created the local alignments in the Fine Alignment step");
    btnTilt
        .setToolTipText("Compute the tomogram from the full aligned stack.  This runs "
            + "the tilt.com script.");
    btn3dmodTomogram.setToolTipText("View the reconstructed volume in 3dmod.");
    btnDeleteStack.setToolTipText("Delete the aligned stack for this axis.  Once the "
        + "tomogram is calculated this intermediate file is not used and "
        + "can be deleted to free up disk space.");
    cbUseZFactors
        .setToolTipText("Use the file containing factors for adjusting the backprojection position "
            + "in each image as a function of Z height in the output slice (.zfac file).  "
            + "These factors are necessary when input images have been transformed to "
            + "correct for an apparent specimen stretch.  "
            + "If this box is not checked, "
            + "Z factors in a local alignment file will not be applied.");
    ltfExtraExcludeList
        .setToolTipText("List of views to exclude from the reconstruction, in "
            + "addition to the ones excluded from fine alignment.");
  }

  private static final class TiltActionListener implements ActionListener {
    private final AbstractTiltPanel adaptee;

    private TiltActionListener(final AbstractTiltPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }

  private static final class TiltDocumentListener implements DocumentListener {
    private final AbstractTiltPanel adaptee;

    private TiltDocumentListener(final AbstractTiltPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void changedUpdate(final DocumentEvent event) {
      adaptee.documentAction();
    }

    public void insertUpdate(final DocumentEvent event) {
      adaptee.documentAction();
    }

    public void removeUpdate(final DocumentEvent event) {
      adaptee.documentAction();
    }
  }
}
