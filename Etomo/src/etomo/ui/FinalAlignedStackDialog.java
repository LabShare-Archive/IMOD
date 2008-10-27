package etomo.ui;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.comscript.CtfPhaseFlipParam;
import etomo.comscript.CtfPlotterParam;
import etomo.comscript.NewstParam;
import etomo.storage.LogFile;
import etomo.storage.MtfFileFilter;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstStringParameter;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.PanelHeaderState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.ViewType;
import etomo.util.DatasetFiles;

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
 * <p> Revision 1.2  2008/10/27 20:38:44  sueh
 * <p> bug# 1141 Added CTF Correction.
 * <p>
 * <p> Revision 1.1  2008/10/16 21:24:25  sueh
 * <p> bug# 1141 Dialog for running newst (full align) and filtering
 * <p> </p>
 */
public final class FinalAlignedStackDialog extends ProcessDialog implements
    ContextMenu, FiducialessParams, Expandable, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  static final String SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL = "Size to output";
  private static final DialogType DIALOG_TYPE = DialogType.FINAL_ALIGNED_STACK;
  public static final String CTF_CORRECTION_LABEL = "Correct CTF";

  private final JPanel pnlFinalAlignedStack = new JPanel();

  // Fiducialess parameters
  private final CheckBox cbFiducialess = new CheckBox("Fiducialless alignment");
  private final LabeledTextField ltfRotation = new LabeledTextField(
      "Tilt axis rotation: ");

  // Newst/Newstack objects
  private final CheckBox cbUseLinearInterpolation = new CheckBox(
      "Use linear interpolation");
  private final LabeledSpinner spinBinning;

  //  Aligned stack buttons
  private final Run3dmodButton btnNewst;
  private final Run3dmodButton btn3dmodFull = Run3dmodButton.get3dmodInstance(
      "View Full Aligned Stack", this);

  // MTF Filter objects
  private final LabeledTextField ltfLowPassRadiusSigma = new LabeledTextField(
      "Low pass (cutoff,sigma): ");
  private final ImageIcon iconFolder = new ImageIcon(ClassLoader
      .getSystemResource("images/openFile.gif"));
  private final LabeledTextField ltfMtfFile = new LabeledTextField("MTF file: ");
  private final JButton btnMtfFile = new JButton(iconFolder);
  private final LabeledTextField ltfMaximumInverse = new LabeledTextField(
      "Maximum Inverse: ");
  private final LabeledTextField ltfInverseRolloffRadiusSigma = new LabeledTextField(
      "Rolloff (radius,sigma): ");
  private final Run3dmodButton btnFilter;
  private final Run3dmodButton btnViewFilter = Run3dmodButton.get3dmodInstance(
      "View Filtered Stack", this);
  private final MultiLineButton btnUseFilter;
  private final SpacedTextField ltfStartingAndEndingZ = new SpacedTextField(
      "Starting and ending views: ");

  //headers should not go into garbage collection
  private final PanelHeader newstHeader;
  private final PanelHeader filterHeader = PanelHeader
      .getAdvancedBasicInstance("2D Filtering (optional)", this, DIALOG_TYPE);
  //panels that are changed in setAdvanced()
  private final SpacedPanel inverseParamsPanel;
  private final JPanel filterBodyPanel;
  private final SpacedPanel newstBodyPanel;
  private final LabeledTextField ltfSizeToOutputInXandY = new LabeledTextField(
      SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + " (X,Y - unbinned): ");

  //backward compatibility functionality - if the metadata binning is missing
  //get binning from newst

  private final ReconScreenState screenState;
  private final ButtonListener finalAlignedStackListener = new ButtonListener(
      this);
  private final FinalAlignedStackExpert expert;

  //ctf correction
  private final PanelHeader ctfCorrectionHeader = PanelHeader
      .getAdvancedBasicInstance("CTF Correction", this, DIALOG_TYPE);
  private final SpacedPanel ctfCorrectionBodyPanel = SpacedPanel
      .getInstance(true);
  private final FileTextField ftfConfigFile = new FileTextField("Config file: ");
  private final LabeledTextField ltfVoltage = new LabeledTextField("Voltage: ");
  private final LabeledTextField ltfSphericalAberration = new LabeledTextField(
      "Spherical Aberration: ");
  private final LabeledTextField ltfAmplitudeContrast = new LabeledTextField(
      "Amplitude contrast: ");
  private final LabeledTextField ltfExpectedDefocus = new LabeledTextField(
      "Expected defocus: ");
  private final LabeledTextField ltfInterpolationWidth = new LabeledTextField(
      "Interpolation width: ");
  private final CheckBox cbParallelProcess = new CheckBox(
      ParallelPanel.FIELD_LABEL);
  private final LabeledTextField ltfDefocusTol = new LabeledTextField(
      "Defocus tolerance: ");
  private final MultiLineButton btnCtfPlotter = new MultiLineButton(
      "Run Ctf Plotter");
  private final Run3dmodButton btnCtfCorrection;
  private final Run3dmodButton btnImodCtfCorrection = Run3dmodButton
      .get3dmodInstance("View CTF Correction", this);
  private final MultiLineButton btnUseCtfCorrection;
  private final CheckBox cbUseExpectedDefocus = new CheckBox(
      "Use expected defocus instead of ctfplotter output");

  private boolean trialTilt = false;

  private FinalAlignedStackDialog(ApplicationManager appMgr,
      FinalAlignedStackExpert expert, AxisID axisID) {
    super(appMgr, axisID, DIALOG_TYPE);
    this.expert = expert;
    screenState = appMgr.getScreenState(axisID);
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    btnNewst = (Run3dmodButton) displayFactory.getFullAlignedStack();
    btnNewst.setContainer(this);
    btnNewst.setDeferred3dmodButton(btn3dmodFull);
    btnFilter = (Run3dmodButton) displayFactory.getFilter();
    btnFilter.setContainer(this);
    btnFilter.setDeferred3dmodButton(btnViewFilter);
    btnUseFilter = (MultiLineButton) displayFactory.getUseFilteredStack();
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");
    // Layout the main panel (and sub panels) and add it to the root panel
    pnlFinalAlignedStack.setBorder(new BeveledBorder("Final Aligned Stack")
        .getBorder());
    pnlFinalAlignedStack.setLayout(new BoxLayout(pnlFinalAlignedStack,
        BoxLayout.Y_AXIS));
    filterBodyPanel = new JPanel();
    inverseParamsPanel = SpacedPanel.getInstance(true);
    newstBodyPanel = SpacedPanel.getInstance();
    //headers
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      newstHeader = PanelHeader.getAdvancedBasicInstance("Blendmont", this,
          dialogType);
    }
    else {
      newstHeader = PanelHeader.getAdvancedBasicInstance("Newstack", this,
          dialogType);
    }
    btnCtfCorrection = (Run3dmodButton) displayFactory.getCtfCorrection();
    btnCtfCorrection.setContainer(this);
    btnCtfCorrection.setDeferred3dmodButton(btnImodCtfCorrection);
    btnUseCtfCorrection = (MultiLineButton) displayFactory
        .getUseCtfCorrection();
    //field instantiation
    SpinnerNumberModel integerModel = new SpinnerNumberModel(1, 1, 8, 1);
    spinBinning = new LabeledSpinner("Aligned image stack binning ",
        integerModel);
    UIUtilities.addWithYSpace(pnlFinalAlignedStack, layoutNewstPanel());
    UIUtilities.addWithYSpace(pnlFinalAlignedStack, layoutCtfCorrectionPanel());
    UIUtilities.addWithYSpace(pnlFinalAlignedStack, layoutFilterPanel());
    UIUtilities.alignComponentsX(pnlFinalAlignedStack,
        Component.CENTER_ALIGNMENT);

    rootPanel.add(pnlFinalAlignedStack);
    addExitButtons();

    updateFiducialess();
    // Set the default advanced dialog state
    updateAdvanced();
    setToolTipText();
  }

  public static FinalAlignedStackDialog getInstance(ApplicationManager appMgr,
      FinalAlignedStackExpert expert, AxisID axisID) {
    FinalAlignedStackDialog instance = new FinalAlignedStackDialog(appMgr,
        expert, axisID);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    // Bind the buttons to the action listener
    btnNewst.addActionListener(finalAlignedStackListener);
    btn3dmodFull.addActionListener(finalAlignedStackListener);
    btnFilter.addActionListener(finalAlignedStackListener);
    btnViewFilter.addActionListener(finalAlignedStackListener);
    btnUseFilter.addActionListener(finalAlignedStackListener);
    btnMtfFile.addActionListener(new MtfFileActionListener(this));
    ltfStartingAndEndingZ
        .addKeyListener(new StartingAndEndingZKeyListener(this));
    cbFiducialess.addActionListener(finalAlignedStackListener);
    cbParallelProcess.addActionListener(finalAlignedStackListener);
    cbUseExpectedDefocus.addActionListener(finalAlignedStackListener);
    ftfConfigFile.addActionListener(finalAlignedStackListener);
    btnCtfPlotter.addActionListener(finalAlignedStackListener);
    btnCtfCorrection.addActionListener(finalAlignedStackListener);
    btnImodCtfCorrection.addActionListener(finalAlignedStackListener);
    btnUseCtfCorrection.addActionListener(finalAlignedStackListener);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
  }

  public static ProcessResultDisplay getFullAlignedStackDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance(
        "Create Full Aligned Stack", DIALOG_TYPE);
  }

  public static ProcessResultDisplay getFilterDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Filter", DIALOG_TYPE);
  }

  public static ProcessResultDisplay getUseFilteredStackDisplay() {
    return MultiLineButton.getToggleButtonInstance("Use Filtered Stack",
        DIALOG_TYPE);
  }

  public static ProcessResultDisplay getCtfCorrectionDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance(CTF_CORRECTION_LABEL,
        DIALOG_TYPE);
  }

  public static ProcessResultDisplay getUseCtfCorrectionDisplay() {
    return MultiLineButton.getToggleButtonInstance("Use CTF Correction",
        DIALOG_TYPE);
  }

  void setFilterButtonEnabled(boolean enable) {
    btnFilter.setEnabled(enable);
  }

  void setFilterButtonState(ReconScreenState screenState) {
    btnFilter.setButtonState(screenState.getButtonState(btnFilter
        .getButtonStateKey()));
  }

  void setCtfCorrectionButtonState(ReconScreenState screenState) {
    btnCtfCorrection.setButtonState(screenState.getButtonState(btnCtfCorrection
        .getButtonStateKey()));
  }

  void setViewFilterButtonEnabled(boolean enable) {
    btnViewFilter.setEnabled(enable);
  }

  void setVoltage(ConstEtomoNumber input) {
    ltfVoltage.setText(input);
  }

  File getConfigFile() {
    return ftfConfigFile.getFile();
  }

  void setConfigFile(ConstStringParameter input) {
    ftfConfigFile.setText(input);
  }

  void setSphericalAberration(ConstEtomoNumber input) {
    ltfSphericalAberration.setText(input);
  }

  void setAmplitudeContrast(ConstEtomoNumber input) {
    ltfAmplitudeContrast.setText(input);
  }

  void setDefocusTol(ConstEtomoNumber input) {
    ltfDefocusTol.setText(input);
  }

  void setExpectedDefocus(ConstEtomoNumber input) {
    ltfExpectedDefocus.setText(input);
  }

  String getDefocusTol() {
    return ltfDefocusTol.getText();
  }

  String getExpectedDefocus() {
    return ltfExpectedDefocus.getText();
  }

  void setUseFilterEnabled(boolean enable) {
    btnUseFilter.setEnabled(enable);
  }

  void setFiducialessAlignment(boolean state) {
    cbFiducialess.setSelected(state);
    updateFiducialess();
  }

  public boolean isFiducialess() {
    return cbFiducialess.isSelected();
  }

  void setImageRotation(float tiltAxisAngle) {
    ltfRotation.setText(tiltAxisAngle);
  }

  void setInterpolationWidth(ConstEtomoNumber input) {
    ltfInterpolationWidth.setText(input);
  }

  void setInverseRolloffRadiusSigma(String inverseRolloffRadiusSigma) {
    ltfInverseRolloffRadiusSigma.setText(inverseRolloffRadiusSigma);
  }

  void setLowPassRadiusSigma(String lowPassRadiusSigma) {
    ltfLowPassRadiusSigma.setText(lowPassRadiusSigma);
  }

  void setNewstHeaderState(PanelHeaderState state) {
    newstHeader.setState(state);
  }

  void setFilterHeaderState(PanelHeaderState state) {
    filterHeader.setState(state);
  }

  void setCtfCorrectionHeaderState(PanelHeaderState state) {
    ctfCorrectionHeader.setState(state);
  }

  public float getImageRotation() throws NumberFormatException {
    return Float.parseFloat(ltfRotation.getText());
  }

  String getInterpolationWidth() {
    return ltfInterpolationWidth.getText();
  }

  String getInverseRolloffRadiusSigma() {
    return ltfInverseRolloffRadiusSigma.getText();
  }

  String getMaximumInverse() {
    return ltfMaximumInverse.getText();
  }

  String getLowPassRadiusSigma() {
    return ltfLowPassRadiusSigma.getText();
  }

  String getMtfFile() {
    return ltfMtfFile.getText();
  }

  String getStartingAndEndingZ() {
    return ltfStartingAndEndingZ.getText();
  }

  void setUseExpectedDefocus(boolean input) {
    cbUseExpectedDefocus.setSelected(input);
  }

  void setUseFilterButtonState(ReconScreenState screenState) {
    btnUseFilter.setButtonState(screenState.getButtonState(btnUseFilter
        .getButtonStateKey()));
  }

  void setUseCtfCorrectionButtonState(ReconScreenState screenState) {
    btnUseCtfCorrection.setButtonState(screenState
        .getButtonState(btnUseCtfCorrection.getButtonStateKey()));
  }

  String getVoltage() {
    return ltfVoltage.getText();
  }

  String getSphericalAberration() {
    return ltfSphericalAberration.getText();
  }

  String getAmplitudeContrast() {
    return ltfAmplitudeContrast.getText();
  }

  int getBinning() {
    return ((Integer) spinBinning.getValue()).intValue();
  }

  void getFilterHeaderState(PanelHeaderState state) {
    filterHeader.getState(state);
  }

  void getCtfCorrectionHeaderState(PanelHeaderState state) {
    ctfCorrectionHeader.getState(state);
  }

  void getNewstHeaderState(PanelHeaderState state) {
    newstHeader.getState(state);
  }

  void setNewstButtonState(ReconScreenState screenState) {
    btnNewst.setButtonState(screenState.getButtonState(btnNewst
        .getButtonStateKey()));
  }

  void setParallelProcessEnabled(boolean enable) {
    cbParallelProcess.setEnabled(enable);
  }

  void setStartingAndEndingZ(String startingAndEndingZ) {
    ltfStartingAndEndingZ.setText(startingAndEndingZ);
  }

  void setMaximumInverse(String maximumInverse) {
    ltfMaximumInverse.setText(maximumInverse);
  }

  void setMtfFile(String mtfFile) {
    ltfMtfFile.setText(mtfFile);
  }

  void setParallelProcess(boolean select) {
    cbParallelProcess.setSelected(select);
  }

  public void expand(ExpandButton button) {
    if (filterHeader != null) {
      if (filterHeader.equalsOpenClose(button)) {
        filterBodyPanel.setVisible(button.isExpanded());
      }
      else if (filterHeader.equalsAdvancedBasic(button)) {
        updateAdvancedFilter(button.isExpanded());
      }
    }
    if (newstHeader != null) {
      if (newstHeader.equalsOpenClose(button)) {
        newstBodyPanel.setVisible(button.isExpanded());
      }
      else if (newstHeader.equalsAdvancedBasic(button)) {
        updateAdvancedNewst(button.isExpanded());
      }
    }
    if (ctfCorrectionHeader != null) {
      if (ctfCorrectionHeader.equalsOpenClose(button)) {
        ctfCorrectionBodyPanel.setVisible(button.isExpanded());
      }
      else if (ctfCorrectionHeader.equalsAdvancedBasic(button)) {
        updateAdvancedCtfCorrection(button.isExpanded());
      }
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  private void updateAdvancedFilter(boolean advanced) {
    ltfStartingAndEndingZ.setVisible(advanced);
    inverseParamsPanel.setVisible(advanced);
  }

  private void updateAdvancedCtfCorrection(boolean advanced) {
    ltfAmplitudeContrast.setVisible(advanced);
    ltfDefocusTol.setVisible(advanced);
  }

  private void updateAdvancedNewst(boolean advanced) {
    ltfSizeToOutputInXandY.setVisible(advanced);
  }

  void setAdvanced() {
    boolean headerAdvanced = filterHeader.isAdvanced();
    if (headerAdvanced != newstHeader.isAdvanced()) {
      return;
    }
    if (headerAdvanced != isAdvanced) {
      super.setAdvanced(headerAdvanced);
    }
  }

  void setBinning(int binning) {
    spinBinning.setValue(binning);
  }

  void setBinning(ConstEtomoNumber binning) {
    spinBinning.setValue(binning);
  }

  void setUseLinearInterpolation(boolean select) {
    cbUseLinearInterpolation.setSelected(select);
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    filterHeader.setAdvanced(isAdvanced);
    newstHeader.setAdvanced(isAdvanced);
    ctfCorrectionHeader.setAdvanced(isAdvanced);

    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public boolean usingParallelProcessing() {
    return cbParallelProcess.isEnabled() && cbParallelProcess.isSelected();
  }

  boolean isParallelProcess() {
    return cbParallelProcess.isSelected();
  }

  boolean isUseExpectedDefocus() {
    return cbUseExpectedDefocus.isSelected();
  }

  String getSizeToOutputInXandY() {
    return ltfSizeToOutputInXandY.getText();
  }

  void setSizeToOutputInXandY(String input) {
    ltfSizeToOutputInXandY.setText(input);
  }

  boolean isUseLinearInterpolation() {
    return cbUseLinearInterpolation.isSelected();
  }

  private JPanel layoutCtfCorrectionPanel() {
    //panel
    JPanel ctfCorrectionMainPanel = new JPanel();
    ctfCorrectionMainPanel.setLayout(new BoxLayout(ctfCorrectionMainPanel,
        BoxLayout.Y_AXIS));
    ctfCorrectionMainPanel.setBorder(BorderFactory.createEtchedBorder());
    ctfCorrectionMainPanel.add(ctfCorrectionHeader.getContainer());
    ctfCorrectionMainPanel.add(ctfCorrectionBodyPanel.getContainer());
    //body
    ctfCorrectionBodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    ctfCorrectionBodyPanel.add(ltfVoltage);
    ctfCorrectionBodyPanel.add(ltfSphericalAberration);
    ctfCorrectionBodyPanel.add(ltfAmplitudeContrast);
    //ctf plotter
    SpacedPanel ctfPlotterPanel = SpacedPanel.getInstance();
    ctfPlotterPanel.setBoxLayout(BoxLayout.Y_AXIS);
    ctfPlotterPanel.setBorder(new EtchedBorder("CTF Plotter").getBorder());
    ctfPlotterPanel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    ctfCorrectionBodyPanel.add(ctfPlotterPanel);
    ctfPlotterPanel.add(ftfConfigFile);
    ctfPlotterPanel.add(ltfExpectedDefocus);
    ctfPlotterPanel.add(btnCtfPlotter);
    //ctf phase flip
    SpacedPanel ctfCorrectionPanel = SpacedPanel.getInstance();
    ctfCorrectionPanel.setBoxLayout(BoxLayout.Y_AXIS);
    ctfCorrectionPanel
        .setBorder(new EtchedBorder("CTF Phase Flip").getBorder());
    ctfCorrectionBodyPanel.add(ctfCorrectionPanel);
    //use expected defocus
    JPanel useExpectedDefocusPanel = new JPanel();
    useExpectedDefocusPanel.setLayout(new BoxLayout(useExpectedDefocusPanel,
        BoxLayout.X_AXIS));
    ctfCorrectionPanel.add(useExpectedDefocusPanel);
    useExpectedDefocusPanel.add(Box.createHorizontalGlue());
    useExpectedDefocusPanel.add(cbUseExpectedDefocus);

    ctfCorrectionPanel.add(cbParallelProcess);
    ctfCorrectionPanel.add(ltfInterpolationWidth);
    ctfCorrectionPanel.add(ltfDefocusTol);
    //buttons
    SpacedPanel buttonPanel = SpacedPanel.getInstance();
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    ctfCorrectionPanel.add(buttonPanel);
    buttonPanel.addHorizontalGlue();
    buttonPanel.add(btnCtfCorrection);
    buttonPanel.add(btnImodCtfCorrection);
    buttonPanel.add(btnUseCtfCorrection);
    buttonPanel.addHorizontalGlue();
    //size buttons
    btnCtfPlotter.setSize();
    btnCtfCorrection.setSize();
    btnImodCtfCorrection.setSize();
    btnUseCtfCorrection.setSize();
    //init
    ctfCorrectionHeader.setOpen(false);
    return ctfCorrectionMainPanel;
  }

  /**
   * Layout the newstack panel
   */
  private JPanel layoutNewstPanel() {
    //panels
    JPanel newstPanel = new JPanel();
    newstPanel.setLayout(new BoxLayout(newstPanel, BoxLayout.Y_AXIS));
    newstPanel.setBorder(BorderFactory.createEtchedBorder());
    newstBodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
    //buttonPanel
    buttonPanel.add(Box.createHorizontalGlue());
    buttonPanel.add(btnNewst.getComponent());
    buttonPanel.add(Box.createHorizontalGlue());
    buttonPanel.add(btn3dmodFull.getComponent());
    buttonPanel.add(Box.createHorizontalGlue());
    //newstBodyPanel
    newstBodyPanel.add(cbUseLinearInterpolation);
    newstBodyPanel.add(spinBinning);
    newstBodyPanel.add(cbFiducialess);
    newstBodyPanel.add(ltfRotation);
    newstBodyPanel.add(ltfSizeToOutputInXandY);
    newstBodyPanel.add(buttonPanel);
    newstBodyPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //newstPanel
    newstPanel.add(newstHeader.getContainer());
    newstPanel.add(newstBodyPanel.getContainer());
    UIUtilities.alignComponentsX(newstPanel, Component.LEFT_ALIGNMENT);
    //configure
    btnNewst.setSize();
    btn3dmodFull.setSize();
    return newstPanel;
  }

  /**
   * Layout the MTF filter panel
   *
   */
  private JPanel layoutFilterPanel() {
    //panels
    JPanel filterPanel = new JPanel();
    filterPanel.setLayout(new BoxLayout(filterPanel, BoxLayout.Y_AXIS));
    filterPanel.setBorder(BorderFactory.createEtchedBorder());
    filterBodyPanel.setLayout(new BoxLayout(filterBodyPanel, BoxLayout.Y_AXIS));
    inverseParamsPanel.setBoxLayout(BoxLayout.Y_AXIS);
    inverseParamsPanel.setBorder(new EtchedBorder(
        "Inverse Filtering Parameters: ").getBorder());
    SpacedPanel mtfFilePanel = SpacedPanel.getInstance();
    mtfFilePanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel inversePanel = SpacedPanel.getInstance();
    inversePanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel buttonPanel = SpacedPanel.getInstance(true);
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    //buttonPanel
    btnFilter.setSize();
    buttonPanel.add(btnFilter);
    btnViewFilter.setSize();
    buttonPanel.add(btnViewFilter);
    btnUseFilter.setSize();
    buttonPanel.add(btnUseFilter);
    //inversePanel
    inversePanel.add(ltfMaximumInverse);
    inversePanel.add(ltfInverseRolloffRadiusSigma);
    //mtfFilePanel
    mtfFilePanel.add(ltfMtfFile);
    mtfFilePanel.add(btnMtfFile);
    //inverseParamsPanel
    inverseParamsPanel.add(mtfFilePanel);
    inverseParamsPanel.add(inversePanel);
    //filterBodyPanel
    filterBodyPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    filterBodyPanel.add(ltfStartingAndEndingZ.getContainer());
    filterBodyPanel.add(ltfLowPassRadiusSigma.getContainer());
    filterBodyPanel.add(inverseParamsPanel.getContainer());
    filterBodyPanel.add(buttonPanel.getContainer());
    //filterPanel
    filterPanel.add(filterHeader.getContainer());
    filterPanel.add(filterBodyPanel);
    //configure
    btnFilter.setSize();
    btnViewFilter.setSize();
    btnUseFilter.setSize();
    return filterPanel;
  }

  void btnMtfFileAction(ActionEvent event) {
    //Open up the file chooser in the $IMOD_CALIB_DIR/Camera, if available,
    //otherwise open in the working directory
    String currentMtfDirectory = ltfMtfFile.getText();
    if (currentMtfDirectory.equals("")) {
      File calibrationDir = EtomoDirector.INSTANCE.getIMODCalibDirectory();
      File cameraDir = new File(calibrationDir.getAbsolutePath(), "Camera");
      if (cameraDir.exists()) {
        currentMtfDirectory = cameraDir.getAbsolutePath();
      }
      else {
        currentMtfDirectory = applicationManager.getPropertyUserDir();
      }
    }
    JFileChooser chooser = new JFileChooser(new File(currentMtfDirectory));
    MtfFileFilter mtfFileFilter = new MtfFileFilter();
    chooser.setFileFilter(mtfFileFilter);
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File mtfFile = chooser.getSelectedFile();
      try {
        ltfMtfFile.setText(mtfFile.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String alignManpageLabel;
    String alignManpage;
    String alignLogfileLabel;
    String alignLogfile;
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      alignManpageLabel = "Blendmont";
      alignManpage = "blendmont";
      alignLogfileLabel = "Blend";
      alignLogfile = "blend";
    }
    else {
      alignManpageLabel = "Newstack";
      alignManpage = "newstack";
      alignLogfileLabel = "Newst";
      alignLogfile = "newst";
    }
    String[] manPagelabel = { alignManpageLabel, "Ctfplotter", "Ctfphaseflip",
        "3dmod" };
    String[] manPage = { alignManpage + ".html", "ctfplotter.html",
        "ctfphaseflip.html", "3dmod.html" };
    String[] logFileLabel = { alignLogfileLabel, "Ctfplotter" };
    String[] logFile = new String[2];
    logFile[0] = alignLogfile + axisID.getExtension() + ".log";
    logFile[1] = "ctfplotter" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
        "FINAL ALIGNED STACK", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager, axisID);
  }

  public void startingAndEndingZKeyReleased(KeyEvent event) {
    expert.enableUseFilter();
  }

  void updateFiducialess() {
    ltfRotation.setEnabled(cbFiducialess.isSelected());
  }

  boolean done() {
    if (expert.doneDialog()) {
      btnNewst.removeActionListener(finalAlignedStackListener);
      btnUseFilter.removeActionListener(finalAlignedStackListener);
      btnFilter.removeActionListener(finalAlignedStackListener);
      setDisplayed(false);
      return true;
    }
    return false;
  }

  private void chooseConfigFile(FileTextField fileTextField) {
    JFileChooser chooser = new JFileChooser(expert.getConfigDir());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      fileTextField.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }

  void updateCtfPlotter() {
    boolean enable = !cbUseExpectedDefocus.isSelected();
    ftfConfigFile.setEnabled(enable);
    btnCtfPlotter.setEnabled(enable);
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
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
  void buttonAction(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnNewst.getActionCommand())) {
      expert.newst(btnNewst, null, deferred3dmodButton, run3dmodMenuOptions);
    }
    else if (command.equals(btnFilter.getActionCommand())) {
      expert.mtffilter(btnFilter, null, deferred3dmodButton,
          run3dmodMenuOptions);
    }
    else if (command.equals(btnUseFilter.getActionCommand())) {
      expert.useMtfFilter(btnUseFilter);
    }
    else if (command.equals(cbFiducialess.getActionCommand())) {
      updateFiducialess();
    }
    else if (command.equals(cbParallelProcess.getActionCommand())) {
      expert.updateParallelProcess();
    }
    else if (command.equals(btn3dmodFull.getActionCommand())) {
      applicationManager.imodFineAlign(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnViewFilter.getActionCommand())) {
      applicationManager.imodMTFFilter(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(ftfConfigFile.getActionCommand())) {
      chooseConfigFile(ftfConfigFile);
    }
    else if (command.equals(cbUseExpectedDefocus.getActionCommand())) {
      updateCtfPlotter();
    }
    else if (command.equals(btnCtfPlotter.getActionCommand())) {
      expert.ctfPlotter(btnCtfPlotter);
    }
    else if (command.equals(btnCtfCorrection.getActionCommand())) {
      expert.ctfCorrection(btnCtfCorrection, null, deferred3dmodButton,
          run3dmodMenuOptions);
    }
    else if (command.equals(btnImodCtfCorrection.getActionCommand())) {
      applicationManager.imodCtfCorrection(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnUseCtfCorrection.getActionCommand())) {
      expert.useCtfCorrection(btnUseCtfCorrection);
    }
  }

  private static final class ButtonListener implements ActionListener {
    private final FinalAlignedStackDialog adaptee;

    private ButtonListener(final FinalAlignedStackDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null, null);
    }
  }

  private static final class MtfFileActionListener implements ActionListener {
    private final FinalAlignedStackDialog adaptee;

    private MtfFileActionListener(final FinalAlignedStackDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnMtfFileAction(event);
    }
  }

  private static final class StartingAndEndingZKeyListener implements
      KeyListener {
    private final FinalAlignedStackDialog adaptee;

    private StartingAndEndingZKeyListener(final FinalAlignedStackDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void keyReleased(final KeyEvent event) {
      adaptee.startingAndEndingZKeyReleased(event);
    }

    public void keyPressed(final KeyEvent event) {
    }

    public void keyTyped(final KeyEvent event) {
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.MTF_FILTER, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    cbUseLinearInterpolation
        .setToolTipText("Make aligned stack with linear instead of cubic interpolation to "
            + "reduce noise.");
    btnNewst
        .setToolTipText("Generate the complete aligned stack for input into the tilt process."
            + "  This runs the newst.com script.");
    btn3dmodFull.setToolTipText("Open the complete aligned stack in 3dmod");
    if (autodoc != null) {
      ltfStartingAndEndingZ.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          "StartingAndEndingZ"));
      ltfLowPassRadiusSigma.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          "LowPassRadiusSigma"));
      String text = EtomoAutodoc.getTooltip(autodoc, "MtfFile");
      if (text != null) {
        ltfMtfFile.setToolTipText(text);
        btnMtfFile.setToolTipText(text);
      }
      ltfMaximumInverse.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          "MaximumInverse"));
      ltfInverseRolloffRadiusSigma.setToolTipText(EtomoAutodoc.getTooltip(
          autodoc, "InverseRolloffRadiusSigma"));
    }
    btnFilter.setToolTipText("Run mtffilter on the full aligned stack.");
    btnViewFilter
        .setToolTipText("View the results of running mtffilter on the full aligned stack.");
    btnUseFilter
        .setToolTipText("Use the results of running mtffilter as the new full aligned stack.");
    cbFiducialess.setToolTipText("Use cross-correlation alignment only.");
    ltfRotation
        .setToolTipText("Rotation angle of tilt axis for generating aligned stack from "
            + "cross-correlation alignment only.");
    spinBinning
        .setToolTipText("Set the binning for the aligned image stack and tomogram.  With a "
            + "binned tomogram, all of the thickness, position, and size parameters"
            + " below are still entered in unbinned pixels.");
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.NEWSTACK, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    if (autodoc != null) {
      ltfSizeToOutputInXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          NewstParam.SIZE_TO_OUTPUT_IN_X_AND_Y));
    }
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.CTF_PLOTTER, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    if (autodoc != null) {
      ftfConfigFile.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          "ConfigFile"));
      ltfVoltage.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.VOLTAGE_OPTION));
      ltfSphericalAberration.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.SPHERICAL_ABERRATION_OPTION));
      ltfAmplitudeContrast.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.AMPLITUDE_CONTRAST_OPTION));
      ltfExpectedDefocus.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPlotterParam.EXPECTED_DEFOCUS_OPTION));
    }
    btnCtfPlotter.setToolTipText("Run ctfplotter");
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.CTF_PHASE_FLIP,
          axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    if (autodoc != null) {
      ltfInterpolationWidth.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.INTERPOLATION_WIDTH_OPTION));
      ltfDefocusTol.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.DEFOCUS_TOL_OPTION));
    }
    cbUseExpectedDefocus
        .setToolTipText("Instead of using the CTF plotter output ("
            + DatasetFiles.CTF_PLOTTER_EXT
            + ") use a one line file containing the expected defocus ("
            + DatasetFiles.SIMPLE_DEFOCUS_EXT + ").  Etomo will create the "
            + DatasetFiles.SIMPLE_DEFOCUS_EXT
            + " file when this checkbox is checked.");
    cbParallelProcess.setToolTipText("Run " + ProcessName.SPLIT_CORRECTION
        + " and use parallel procossing when running "
        + ProcessName.CTF_CORRECTION + "." + DatasetFiles.COMSCRIPT_EXT + ".");
    btnCtfCorrection.setToolTipText("Run " + ProcessName.CTF_CORRECTION + "."
        + DatasetFiles.COMSCRIPT_EXT + ", which calls "
        + CtfPhaseFlipParam.COMMAND + ".");
    btnImodCtfCorrection.setToolTipText("Open CTF corrected stack ("
        + DatasetFiles.CTF_CORRECTION_EXT + ").");
    btnUseCtfCorrection.setToolTipText("Replace full aligned stack ("
        + DatasetFiles.FULL_ALIGNED_EXT + ") with CTF corrected stack ("
        + DatasetFiles.CTF_CORRECTION_EXT + ").");
  }
}
