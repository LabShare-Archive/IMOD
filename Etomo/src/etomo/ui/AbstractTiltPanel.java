package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.storage.CpuAdoc;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
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
 * <p> Revision 3.1  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p> </p>
 */
abstract class AbstractTiltPanel implements Expandable, TrialTiltParent,
    Run3dmodButtonContainer, TiltDisplay {
  public static final String rcsid = "$Id$";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  //Keep components with listeners private.
  private final Run3dmodButton btn3dmodTomogram = Run3dmodButton
      .get3dmodInstance("View Tomogram In 3dmod", this);
  private final ActionListener actionListener = new TiltActionListener(this);
  private final JPanel pnlBody = new JPanel();
  private final SpacedTextField ltfLogOffset = new SpacedTextField(
      "Log offset: ");
  private final CheckBox cbParallelProcess = new CheckBox(
      ParallelPanel.FIELD_LABEL);
  private final SpacedTextField ltfTomoWidth = new SpacedTextField(
      "Tomogram width in X: ");
  final LabeledTextField ltfTomoThickness = new LabeledTextField(
      "Tomogram thickness in Z: ");
  private final SpacedTextField ltfSliceIncr = new SpacedTextField(
      "Slice step in Y: ");
  private final SpacedTextField ltfXAxisTilt = new SpacedTextField(
      "X axis tilt: ");
  private final SpacedTextField ltfTiltAngleOffset = new SpacedTextField(
      "Tilt angle offset: ");
  private final SpacedTextField ltfExtraExcludeList = new SpacedTextField(
      "Extra views to exclude: ");
  private final SpacedTextField ltfDensityScale = new SpacedTextField(
      "Output density scaling factor: ");
  private final SpacedTextField ltfDensityOffset = new SpacedTextField(
      "Offset: ");
  private final SpacedTextField ltfSliceStart = new SpacedTextField(
      "First slice: ");
  private final SpacedTextField ltfSliceStop = new SpacedTextField(
      "Last slice: ");
  private final SpacedLabel lblInY = new SpacedLabel(" in Y");
  final LabeledTextField ltfZShift = new LabeledTextField("Z shift: ");
  private final SpacedTextField ltfXShift = new SpacedTextField("X shift:");
  private final SpacedTextField ltfRadialMax = new SpacedTextField(
      "Radial filter cutoff: ");
  private final SpacedTextField ltfRadialFallOff = new SpacedTextField(
      "Falloff: ");
  private final CheckBox cbUseLocalAlignment = new CheckBox(
      "Use local alignments");
  private final CheckBox cbUseZFactors = new CheckBox("Use Z factors");

  private final PanelHeader header;
  final ApplicationManager manager;
  final AxisID axisID;
  final DialogType dialogType;
  private final TrialTiltPanel trialTiltPanel;
  //Keep components with listeners private.
  private final Run3dmodButton btnTilt;
  private final MultiLineButton btnDeleteStack;
  private final TiltParent parent;

  abstract Run3dmodButton getTiltButton(ApplicationManager manager,
      AxisID axisID);

  abstract void tiltAction(final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions);

  abstract void imodTomogramAction(
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions);

  //backward compatibility functionality - if the metadata binning is missing
  //get binning from newst
  AbstractTiltPanel(ApplicationManager manager, AxisID axisID,
      DialogType dialogType, TiltParent parent,
      GlobalExpandButton globalAdvancedButton) {
    this.manager = manager;
    this.axisID = axisID;
    this.dialogType = dialogType;
    this.parent = parent;
    header = PanelHeader.getAdvancedBasicInstance("Tilt", this, dialogType,
        globalAdvancedButton);
    trialTiltPanel = TrialTiltPanel.getInstance(manager, axisID, dialogType,
        this);
    ProcessResultDisplayFactory displayFactory = manager
        .getProcessResultDisplayFactory(axisID);
    btnTilt = getTiltButton(manager, axisID);
    btnDeleteStack = (MultiLineButton) displayFactory.getDeleteAlignedStack();
  }

  final void initializePanel() {
    btnTilt.setSize();
    btnTilt.setContainer(this);
    btnTilt.setDeferred3dmodButton(btn3dmodTomogram);
    btn3dmodTomogram.setSize();
    btnDeleteStack.setSize();
    ConstEtomoNumber maxCPUs = CpuAdoc.getInstance(axisID,
        manager.getPropertyUserDir(), manager.getManagerKey()).getMaxTilt();
    if (maxCPUs != null && !maxCPUs.isNull()) {
      cbParallelProcess.setText(ParallelPanel.FIELD_LABEL
          + ParallelPanel.MAX_CPUS_STRING + maxCPUs.toString());
    }
  }

  void createPanel() {
    //Initialize
    initializePanel();
    //local panels
    JPanel pnlDensity = new JPanel();
    JPanel pnlSlicesInY = new JPanel();
    JPanel pnlOffset = new JPanel();
    JPanel pnlRadial = new JPanel();
    JPanel pnlCheckBox = new JPanel();
    SpacedPanel trialPanel = SpacedPanel.getInstance();
    SpacedPanel pnlButton = SpacedPanel.getInstance(true);
    //Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(BorderFactory.createEtchedBorder());
    pnlRoot.add(header);
    pnlRoot.add(pnlBody);
    UIUtilities.alignComponentsX(pnlRoot.getContainer(),
        Component.LEFT_ALIGNMENT);
    //Body panel
    pnlBody.setLayout(new BoxLayout(pnlBody, BoxLayout.Y_AXIS));
    pnlBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlBody.add(cbParallelProcess);
    pnlBody.add(ltfLogOffset.getContainer());
    pnlBody.add(pnlDensity);
    pnlBody.add(ltfTomoWidth.getContainer());
    pnlBody.add(ltfTomoThickness.getContainer());
    pnlBody.add(pnlSlicesInY);
    pnlBody.add(ltfSliceIncr.getContainer());
    pnlBody.add(pnlOffset);
    pnlBody.add(ltfXAxisTilt.getContainer());
    pnlBody.add(ltfTiltAngleOffset.getContainer());
    pnlBody.add(pnlRadial);
    pnlBody.add(ltfExtraExcludeList.getContainer());
    pnlBody.add(pnlCheckBox);
    pnlBody.add(trialPanel.getContainer());
    pnlBody.add(pnlButton.getContainer());
    UIUtilities.alignComponentsX(pnlBody, Component.LEFT_ALIGNMENT);
    //Density panel
    pnlDensity.setLayout(new BoxLayout(pnlDensity, BoxLayout.X_AXIS));
    pnlDensity.add(ltfDensityScale.getContainer());
    pnlDensity.add(ltfDensityOffset.getContainer());
    UIUtilities.alignComponentsX(pnlDensity, Component.LEFT_ALIGNMENT);
    //Slices in Y panel
    pnlSlicesInY.setLayout(new BoxLayout(pnlSlicesInY, BoxLayout.X_AXIS));
    pnlSlicesInY.add(ltfSliceStart.getContainer());
    pnlSlicesInY.add(ltfSliceStop.getContainer());
    pnlSlicesInY.add(lblInY.getContainer());
    UIUtilities.alignComponentsX(pnlSlicesInY, Component.LEFT_ALIGNMENT);
    //Offset panel
    pnlOffset.setLayout(new BoxLayout(pnlOffset, BoxLayout.X_AXIS));
    pnlOffset.add(ltfXShift.getContainer());
    pnlOffset.add(ltfZShift.getContainer());
    UIUtilities.alignComponentsX(pnlOffset, Component.LEFT_ALIGNMENT);
    //Radial panel
    pnlRadial.setLayout(new BoxLayout(pnlRadial, BoxLayout.X_AXIS));
    pnlRadial.add(ltfRadialMax.getContainer());
    pnlRadial.add(ltfRadialFallOff.getContainer());
    UIUtilities.alignComponentsX(pnlRadial, Component.LEFT_ALIGNMENT);
    //Check box panel
    pnlCheckBox.setLayout(new BoxLayout(pnlCheckBox, BoxLayout.Y_AXIS));
    pnlCheckBox.add(cbUseLocalAlignment);
    pnlCheckBox.add(cbUseZFactors);
    UIUtilities.alignComponentsX(pnlCheckBox, Component.LEFT_ALIGNMENT);
    //Trial panel
    trialPanel.setBoxLayout(BoxLayout.X_AXIS);
    trialPanel.add(trialTiltPanel.getComponent());
    trialPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //Button panel
    pnlButton.setBoxLayout(BoxLayout.X_AXIS);
    pnlButton.add(btnTilt);
    pnlButton.add(btn3dmodTomogram);
    pnlButton.add(btnDeleteStack);
    pnlButton.alignComponentsX(Component.LEFT_ALIGNMENT);
  }

  final SpacedPanel getRootPanel() {
    return pnlRoot;
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

  final ProcessResultDisplay getTiltProcessResultDisplay() {
    return btnTilt;
  }

  final void setTiltButtonTooltip(String tooltip) {
    btnTilt.setToolTipText(tooltip);
  }

  final void addListeners() {
    btnTilt.addActionListener(actionListener);
    btn3dmodTomogram.addActionListener(actionListener);
    btnDeleteStack.addActionListener(actionListener);
    cbParallelProcess.addActionListener(actionListener);
  }

  final Component getComponent() {
    return pnlRoot.getContainer();
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

  void updateAdvanced(final boolean advanced) {
    ltfLogOffset.setVisible(advanced);
    ltfDensityOffset.setVisible(advanced);
    ltfDensityScale.setVisible(advanced);
    ltfTomoWidth.setVisible(advanced);
    //ltfTomoThickness
    ltfSliceStart.setVisible(advanced);
    ltfSliceStop.setVisible(advanced);
    lblInY.setVisible(advanced);
    ltfSliceIncr.setVisible(advanced);
    ltfXShift.setVisible(advanced);
    //Z shift is not always an advanced field.
    //ltfZShift
    //ltfXAxisTilt
    ltfTiltAngleOffset.setVisible(advanced);
    //ltfRadialMax
    //ltfRadialFallOff
    ltfExtraExcludeList.setVisible(advanced);
    //cbUseLocalAlignment
    //cbUseZFactors
    //cbParallelProcess
    trialTiltPanel.setVisible(advanced);
    //btnTilt
    //btn3dmodTomogram
    //btnDeleteStacks
  }

  final void done() {
    btnTilt.removeActionListener(actionListener);
    btnDeleteStack.removeActionListener(actionListener);
    trialTiltPanel.done();
  }

  public final void updateParallelProcess() {
    parent.updateParallelProcess();
  }

  public final boolean isParallelProcess() {
    return cbParallelProcess.isSelected();
  }

  public final boolean usingParallelProcessing() {
    return cbParallelProcess.isEnabled() && cbParallelProcess.isSelected();
  }

  final void setParallelProcess(final boolean select) {
    cbParallelProcess.setSelected(select);
  }

  final boolean isZShiftSet() {
    return ltfZShift.getText().matches("\\S+");
  }

  final boolean isUseLocalAlignment() {
    return cbUseLocalAlignment.isSelected();
  }

  void setEnabledTiltParameters(TomogramState state, ConstMetaData metaData) {
    boolean madeZFactors = false;
    boolean newstFiducialessAlignment = false;
    boolean usedLocalAlignments = false;
    // madeZFactors
    if (!state.getMadeZFactors(axisID).isNull()) {
      madeZFactors = state.getMadeZFactors(axisID).is();
    }
    else {
      madeZFactors = state.getBackwardCompatibleMadeZFactors(axisID);
    }
    // newstFiducialessAlignment
    if (!state.getNewstFiducialessAlignment(axisID).isNull()) {
      newstFiducialessAlignment = state.getNewstFiducialessAlignment(axisID)
          .is();
    }
    else {
      newstFiducialessAlignment = metaData.isFiducialessAlignment(axisID);
    }
    // usedLocalAlignments
    if (!state.getUsedLocalAlignments(axisID).isNull()) {
      usedLocalAlignments = state.getUsedLocalAlignments(axisID).is();
    }
    else {
      usedLocalAlignments = state
          .getBackwardCompatibleUsedLocalAlignments(axisID);
    }
    // enable parameters
    cbUseZFactors.setEnabled(madeZFactors && !newstFiducialessAlignment);
    cbUseLocalAlignment.setEnabled(usedLocalAlignments
        && !newstFiducialessAlignment);
  }

  boolean isUseZFactors() {
    return cbUseZFactors.isSelected();
  }

  final void getParameters(final MetaData metaData)
      throws FortranInputSyntaxException {
    metaData.setTomoGenTiltParallel(axisID, isParallelProcess());
    trialTiltPanel.getParameters(metaData);
  }

  final void getParameters(final ReconScreenState screenState) {
    header.getState(screenState.getTomoGenTiltHeaderState());
    trialTiltPanel.getParameters(screenState);
  }

  final void setParameters(final ConstMetaData metaData) {
    CpuAdoc cpuAdoc = CpuAdoc.getInstance(AxisID.ONLY, manager
        .getPropertyUserDir(), manager.getManagerKey());
    //Parallel processing is optional in tomogram reconstruction, so only use it
    //if the user set it up.
    boolean validAutodoc = cpuAdoc.isAvailable();
    cbParallelProcess.setEnabled(validAutodoc);
    ConstEtomoNumber tomoGenTiltParallel = metaData
        .getTomoGenTiltParallel(axisID);
    if (tomoGenTiltParallel == null) {
      setParallelProcess(validAutodoc && metaData.getDefaultParallel().is());
    }
    else {
      setParallelProcess(validAutodoc && tomoGenTiltParallel.is());
    }
    trialTiltPanel.setParameters(metaData);
    updateParallelProcess();
  }

  /**
   * Set the UI parameters with the specified tiltParam values
   * WARNING: be sure the setNewstParam is called first so the binning value for
   * the stack is known.  The thickness, first and last slice, width and x,y,z
   * offsets are scaled so that they are represented to the user in unbinned
   * dimensions.
   * @param tiltParam
   * @param initialize - used by child classes - has no effect in this class
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
      ltfSliceStart.setText(tiltParam.getIdxSliceStart());
      ltfSliceStop.setText(tiltParam.getIdxSliceStop());
    }
    if (tiltParam.hasSliceIncr()) {
      ltfSliceIncr.setText(tiltParam.getIncrSlice());
    }
    if (tiltParam.hasXAxisTilt()) {
      ltfXAxisTilt.setText(tiltParam.getXAxisTilt());
    }
    if (tiltParam.hasTiltAngleOffset()) {
      ltfTiltAngleOffset.setText(tiltParam.getTiltAngleOffset());
    }
    if (tiltParam.hasRadialWeightingFunction()) {
      ltfRadialMax.setText(tiltParam.getRadialBandwidth());
      ltfRadialFallOff.setText(tiltParam.getRadialFalloff());
    }
    if (tiltParam.hasScale()) {
      ltfDensityOffset.setText(tiltParam.getScaleFLevel());
      ltfDensityScale.setText(tiltParam.getScaleCoeff());
    }
    if (tiltParam.hasLogOffset()) {
      ltfLogOffset.setText(tiltParam.getLogShift());
    }
    MetaData metaData = manager.getMetaData();
    cbUseLocalAlignment.setSelected(metaData.getUseLocalAlignments(axisID));
    cbUseZFactors.setSelected(metaData.getUseZFactors(axisID).is());
    ltfExtraExcludeList.setText(tiltParam.getExcludeList2());
  }

  final void setParameters(final ReconScreenState screenState) {
    header.setState(screenState.getTomoGenTiltHeaderState());
    btnTilt.setButtonState(screenState.getButtonState(btnTilt
        .getButtonStateKey()));
    btnDeleteStack.setButtonState(screenState.getButtonState(btnDeleteStack
        .getButtonStateKey()));
  }

  public boolean getParameters(final SplittiltParam param) {
    ParallelPanel parallelPanel = manager.getMainPanel().getParallelPanel(
        axisID);
    if (parallelPanel == null) {
      return false;
    }
    ConstEtomoNumber numMachines = param.setNumMachines(parallelPanel
        .getCPUsSelected());
    if (!numMachines.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(parallelPanel.getCPUsSelectedLabel()
          + " " + numMachines.getInvalidReason(), "Unable to run splittilt",
          axisID, manager.getManagerKey());
      return false;
    }
    return true;
  }

  /**
   * Get the tilt parameters from the requested axis panel
   */
  public boolean getParameters(final TiltParam tiltParam)
      throws NumberFormatException, InvalidParameterException, IOException {
    String badParameter = "";
    try {
      badParameter = "IMAGEBINNED";
      tiltParam.setImageBinned();
      //Do not manage full image size.  It is coming from copytomocoms.      
      if (ltfTomoWidth.getText().matches("\\S+")) {
        badParameter = ltfTomoWidth.getLabel();
        tiltParam.setWidth(Integer.parseInt(ltfTomoWidth.getText()));
      }
      else {
        tiltParam.resetWidth();
      }
      //set Z Shift
      if (isZShiftSet()) {
        badParameter = ltfZShift.getLabel();

        tiltParam.setZShift(ltfZShift.getText());
      }
      else {
        tiltParam.resetZShift();
      }

      //set X Shift
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

      boolean sliceRangeSpecified = false;
      if (ltfSliceStart.getText().matches("\\S+")
          && ltfSliceStop.getText().matches("\\S+")) {
        badParameter = ltfSliceStart.getLabel();
        tiltParam.setIdxSliceStart(Integer.parseInt(ltfSliceStart.getText()));
        badParameter = ltfSliceStop.getLabel();
        tiltParam.setIdxSliceStop(Integer.parseInt(ltfSliceStop.getText()));
        sliceRangeSpecified = true;
      }
      else if (ltfSliceStart.getText().matches("^\\s*$")
          && ltfSliceStop.getText().matches("^\\s*$")) {
        tiltParam.resetIdxSlice();
      }
      else {
        throw (new InvalidParameterException(
            "You must supply both the first and last slices if you want to specify either."));
      }
      if (ltfSliceIncr.getText().matches("\\S+")) {
        if (sliceRangeSpecified) {
          badParameter = ltfSliceIncr.getLabel();
          tiltParam.setIncrSlice(Integer.parseInt(ltfSliceIncr.getText()));
        }
        else {
          throw (new InvalidParameterException(
              "You must supply both the first and last slices to specify the slice step."));
        }
      }
      else {
        tiltParam.resetIncrSlice();
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
        tiltParam.setTiltAngleOffset(Float.parseFloat(ltfTiltAngleOffset
            .getText()));
      }
      else {
        tiltParam.resetTiltAngleOffset();
      }

      if (ltfRadialMax.getText().matches("\\S+")
          || ltfRadialFallOff.getText().matches("\\S+")) {
        badParameter = ltfRadialMax.getLabel();
        tiltParam.setRadialBandwidth(Float.parseFloat(ltfRadialMax.getText()));
        badParameter = ltfRadialFallOff.getLabel();
        tiltParam
            .setRadialFalloff(Float.parseFloat(ltfRadialFallOff.getText()));
      }
      else {
        tiltParam.resetRadialFilter();
      }

      if (ltfDensityOffset.getText().matches("\\S+")
          || ltfDensityScale.getText().matches("\\S+")) {
        badParameter = ltfDensityScale.getLabel();
        tiltParam.setScaleCoeff(Float.parseFloat(ltfDensityScale.getText()));
        badParameter = ltfDensityOffset.getLabel();
        tiltParam.setScaleFLevel(Float.parseFloat(ltfDensityOffset.getText()));
      }
      else {
        tiltParam.resetScale();
      }

      if (ltfLogOffset.getText().matches("\\S+")) {
        badParameter = ltfLogOffset.getLabel();
        tiltParam.setLogShift(Float.parseFloat(ltfLogOffset.getText()));
      }
      else {
        tiltParam.setLogShift(Float.NaN);
      }

      MetaData metaData = manager.getMetaData();
      if (isUseLocalAlignment() && cbUseLocalAlignment.isEnabled()) {
        tiltParam.setLocalAlignFile(metaData.getDatasetName()
            + axisID.getExtension() + "local.xf");
      }
      else {
        tiltParam.setLocalAlignFile("");
      }
      metaData.setUseLocalAlignments(axisID, isUseLocalAlignment());
      //TiltParam.fiducialess is based on whether final alignment was run
      //fiducialess.
      // newstFiducialessAlignment
      boolean newstFiducialessAlignment = false;
      TomogramState state = manager.getState();
      if (!state.getNewstFiducialessAlignment(axisID).isNull()) {
        newstFiducialessAlignment = state.getNewstFiducialessAlignment(axisID)
            .is();
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
    return true;
  }

  public final void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
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
  final void action(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnTilt.getActionCommand())) {
      tiltAction(deferred3dmodButton, run3dmodMenuOptions);
    }
    else if (command.equals(btnDeleteStack.getActionCommand())) {
      manager.deleteAlignedStacks(axisID, btnDeleteStack);
    }
    else if (command.equals(cbParallelProcess.getActionCommand())) {
      updateParallelProcess();
    }
    else if (command.equals(btn3dmodTomogram.getActionCommand())) {
      imodTomogramAction(deferred3dmodButton, run3dmodMenuOptions);
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;

    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.MTF_FILTER, axisID,
          manager.getManagerKey());
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
    ltfTomoThickness
        .setToolTipText("Thickness, in pixels, along the z-axis of the reconstructed volume.");
    ltfSliceStart
        .setToolTipText("The first slice in the Y dimension to include in the reconstructed "
            + " volume.  Slices are numbered from 0, a last slice must also "
            + "be specified.");
    ltfSliceStop
        .setToolTipText("The last slice in the Y dimension to include in the reconstructed "
            + " volume.  Slices are numbered from 0, a first slice must also "
            + "be specified.");
    ltfSliceIncr
        .setToolTipText("Step between slices in the Y dimension.  A first and last slice must "
            + "also be entered. Default is 1.");
    ltfTomoWidth
        .setToolTipText("This entry specifies the width of the output image; the default is the "
            + "width of the input image.");
    ltfXShift
        .setToolTipText("Amount to shift the reconstructed slices in X before output.  A "
            + "positive value will shift the slice to the right, and the "
            + "output will contain the left part of the whole potentially "
            + "reconstructable area.");
    ltfZShift
        .setToolTipText("Amount to shift the reconstructed slices in Z before output.  A "
            + "positive value will shift the slice upward.");
    ltfXAxisTilt.setToolTipText(TomogramGenerationDialog.X_AXIS_TILT_TOOLTIP);
    ltfTiltAngleOffset
        .setToolTipText("Offset in degrees to apply to the tilt angles; a positive offset will "
            + "rotate the reconstructed slices counterclockwise.");
    ltfRadialMax
        .setToolTipText("The spatial frequency at which to switch from the R-weighted radial "
            + "filter to a Gaussian falloff.  Frequency is in cycles/pixel and "
            + "ranges from 0-0.5.  Both a cutoff and a falloff must be entered.");
    ltfRadialFallOff
        .setToolTipText("The sigma value of a Gaussian which determines how fast the radial "
            + "filter falls off at spatial frequencies above the cutoff frequency."
            + "  Frequency is in cycles/pixel and ranges from 0-0.5.  Both a "
            + "cutoff and a falloff must be entered ");
    ltfDensityOffset
        .setToolTipText("Amount to add to reconstructed density values before multiplying by"
            + " the scale factor and outputting the values.");
    ltfDensityScale
        .setToolTipText("Amount to multiply reconstructed density values by, after adding the "
            + "offset value.");
    ltfLogOffset
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
    btnDeleteStack
        .setToolTipText("Delete the aligned stack for this axis.  Once the "
            + "tomogram is calculated this intermediate file is not used and can be "
            + "" + "deleted to free up disk space.");
    cbUseZFactors
        .setToolTipText("Use the file containing factors for adjusting the backprojection position "
            + "in each image as a function of Z height in the output slice (.zfac file).  "
            + "These factors are necessary when input images have been transformed to "
            + "correct for an apparent specimen stretch.  "
            + "If this box is not checked, "
            + "Z factors in a local alignment file will not be applied.");
    ltfExtraExcludeList
        .setToolTipText("List of views to exclude from the reconstruction, in addition to the ones"
            + "excluded from fine alignment.");
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
}
