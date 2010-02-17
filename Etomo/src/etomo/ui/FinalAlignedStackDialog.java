package etomo.ui;

import java.awt.Component;
import java.awt.Container;
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
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstFindBeads3dParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.CtfPhaseFlipParam;
import etomo.comscript.CtfPlotterParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;
import etomo.storage.LogFile;
import etomo.storage.MtfFileFilter;
import etomo.storage.Network;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.ConstStringParameter;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.MetaData;
import etomo.type.PanelHeaderState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
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
 * <p> Revision 1.24  2010/01/11 23:59:00  sueh
 * <p> bug# 1299 Removed responsibility anything other then cpu.adoc from
 * <p> CpuAdoc.  Placed responsibility for information about the network in the
 * <p> Network class.
 * <p>
 * <p> Revision 1.23  2009/12/19 01:14:34  sueh
 * <p> bug# 1294 Fixed broken right click popup menu.
 * <p>
 * <p> Revision 1.22  2009/11/20 17:11:47  sueh
 * <p> bug# 1282 Naming all the file choosers by constructing a FileChooser
 * <p> instance instead of a JFileChooser instance.  Added isMenuSaveEnabled to
 * <p> allow a save function to have the same limits as the save menu option.
 * <p>
 * <p> Revision 1.21  2009/10/19 21:07:03  sueh
 * <p> bug# 1263 Calling updateParallelProcess from changeTab.  In
 * <p> usingParallelProcessing take the current tab into account.
 * <p>
 * <p> Revision 1.20  2009/10/19 16:28:56  sueh
 * <p> bug# 1253 Added invertTiltAngles.
 * <p>
 * <p> Revision 1.19  2009/09/22 23:54:54  sueh
 * <p> bug# 1269 Added setEnabledTiltParameters.
 * <p>
 * <p> Revision 1.18  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.17  2009/06/16 22:53:53  sueh
 * <p> bug# 1221 Factored out newst and ccderaser.  Move some of the expert
 * <p> functionality back into the dialog because it doesn't seem to be compatible
 * <p> with process panels.
 * <p>
 * <p> Revision 1.16  2009/06/12 19:49:14  sueh
 * <p> bug# 1221 Factored running newst, making it independent of the
 * <p> final aligned dialog and expert.
 * <p>
 * <p> Revision 1.15  2009/06/10 22:16:55  sueh
 * <p> bug# 1221 Factoring Newstack and blendmont into NewstackPanel.
 * <p>
 * <p> Revision 1.14  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.13  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.12  2009/01/26 23:10:57  sueh
 * <p> bug# 1173 Correct tag for tomogram doc
 * <p>
 * <p> Revision 1.11  2009/01/26 22:42:55  sueh
 * <p> bug# 1173 Added mouse listener to tabbed panel.  Added tooltips.
 * <p>
 * <p> Revision 1.10  2009/01/20 20:04:05  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.  Changed JButtons to SimpleButtons to they they can
 * <p> name themselves.
 * <p>
 * <p> Revision 1.9  2008/12/02 21:39:10  sueh
 * <p> bug# 1157 Added "(pixels)" to the final stack fiducial diameter.
 * <p>
 * <p> Revision 1.8  2008/12/02 21:21:51  sueh
 * <p> bug# 1157 Changed better radius to fiducial diameter.
 * <p>
 * <p> Revision 1.7  2008/11/20 01:44:06  sueh
 * <p> bug# 1147 Added Erase Gold tab.  Added polynomialOrder and
 * <p> betterRadius.  Added buttons to run xfmodel and ccderaser.  Bug# 1153
 * <p> Removed listeners on dialog done from buttons that are created by the
 * <p> display result factory.
 * <p>
 * <p> Revision 1.6  2008/11/11 23:51:07  sueh
 * <p> bug# 1149 Changed the tab names.
 * <p>
 * <p> Revision 1.5  2008/11/11 00:18:12  sueh
 * <p> bug# 1147 Coverted newst, ctfcorrection, and mtffilter panels to tabs.
 * <p> Removed the open/close part of the panel headers.
 * <p>
 * <p> Revision 1.4  2008/11/07 21:26:14  sueh
 * <p> bug# 1146 Added ctfcorrection log to right-click menu.  Also add mtffilter
 * <p> options.
 * <p>
 * <p> Revision 1.3  2008/10/27 23:20:27  sueh
 * <p> bug# 1141 Changed the names of the ctfplotter button and the
 * <p> ctfcorrection button.
 * <p>
 * <p> Revision 1.2  2008/10/27 20:38:44  sueh
 * <p> bug# 1141 Added CTF Correction.
 * <p>
 * <p> Revision 1.1  2008/10/16 21:24:25  sueh
 * <p> bug# 1141 Dialog for running newst (full align) and filtering
 * <p> </p>
 */
public final class FinalAlignedStackDialog extends ProcessDialog implements
    ContextMenu, Expandable, Run3dmodButtonContainer, EraseGoldParent {
  public static final String rcsid = "$Id$";

  private static final String MTF_FILE_LABEL = "MTF file: ";

  private static final DialogType DIALOG_TYPE = DialogType.FINAL_ALIGNED_STACK;
  public static final String CTF_CORRECTION_LABEL = "Correct CTF";

  private final NewstackOrBlendmontPanel newstackOrBlendmontPanel;
  private final EtomoPanel pnlFinalAlignedStack = new EtomoPanel();

  // MTF Filter objects
  private final LabeledTextField ltfLowPassRadiusSigma = new LabeledTextField(
      "Low pass (cutoff,sigma): ");
  private final ImageIcon iconFolder = new ImageIcon(ClassLoader
      .getSystemResource("images/openFile.gif"));
  private final LabeledTextField ltfMtfFile = new LabeledTextField(
      MTF_FILE_LABEL);
  private final SimpleButton btnMtfFile = new SimpleButton(iconFolder);
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
  private final PanelHeader filterHeader = PanelHeader
      .getAdvancedBasicOnlyInstance("2D Filtering (optional)", this,
          DIALOG_TYPE, btnAdvanced);
  //panels that are changed in setAdvanced()
  private final SpacedPanel inverseParamsPanel;
  private final JPanel filterBodyPanel;

  //backward compatibility functionality - if the metadata binning is missing
  //get binning from newst

  private final ReconScreenState screenState;
  private final ButtonListener finalAlignedStackListener = new ButtonListener(
      this);
  private final FinalAlignedStackExpert expert;

  //ctf correction
  private final PanelHeader ctfCorrectionHeader = PanelHeader
      .getAdvancedBasicOnlyInstance("CTF Correction", this, DIALOG_TYPE,
          btnAdvanced);
  private final SpacedPanel ctfCorrectionBodyPanel = SpacedPanel
      .getInstance(true);
  private final FileTextField ftfConfigFile = new FileTextField("Config file: ");
  private final LabeledTextField ltfVoltage = new LabeledTextField("Voltage: ");
  private final LabeledTextField ltfSphericalAberration = new LabeledTextField(
      "Spherical Aberration: ");
  private final CheckBox cbInvertTiltAngles = new CheckBox(
      "Invert sign of tilt angles");
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

  private final TabbedPane tabbedPane = new TabbedPane();
  private final EtomoPanel ctfCorrectionMainPanel = new EtomoPanel();
  private final JPanel filterPanel = new JPanel();

  private final EraseGoldPanel eraseGoldPanel;

  private boolean trialTilt = false;
  private Tab curTab = Tab.DEFAULT;

  private FinalAlignedStackDialog(ApplicationManager appMgr,
      FinalAlignedStackExpert expert, AxisID axisID, Tab curTab) {
    super(appMgr, axisID, DIALOG_TYPE);
    this.expert = expert;
    this.curTab = curTab;
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    eraseGoldPanel = EraseGoldPanel.getInstance(appMgr, axisID, dialogType,
        this, btnAdvanced);
    if (appMgr.getMetaData().getViewType() == ViewType.MONTAGE) {
      newstackOrBlendmontPanel = BlendmontPanel.getInstance(appMgr, axisID,
          DIALOG_TYPE, btnAdvanced);
    }
    else {
      newstackOrBlendmontPanel = NewstackPanel.getInstance(appMgr, axisID,
          DIALOG_TYPE, btnAdvanced);
    }
    screenState = appMgr.getScreenState(axisID);
    btnFilter = (Run3dmodButton) displayFactory.getFilter();
    btnFilter.setContainer(this);
    btnFilter.setDeferred3dmodButton(btnViewFilter);
    btnUseFilter = (MultiLineButton) displayFactory.getUseFilteredStack();
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");
    btnMtfFile.setName(MTF_FILE_LABEL);
    // Layout the main panel (and sub panels) and add it to the root panel
    rootPanel.setBorder(new BeveledBorder("Final Aligned Stack").getBorder());
    filterBodyPanel = new JPanel();
    inverseParamsPanel = SpacedPanel.getInstance(true);

    btnCtfCorrection = (Run3dmodButton) displayFactory.getCtfCorrection();
    btnCtfCorrection.setContainer(this);
    btnCtfCorrection.setDeferred3dmodButton(btnImodCtfCorrection);
    btnUseCtfCorrection = (MultiLineButton) displayFactory
        .getUseCtfCorrection();
    //field instantiation
    layoutNewstPanel();
    layoutCtfCorrectionPanel();
    layoutCcdEraser();
    layoutFilterPanel();
    rootPanel.add(tabbedPane);
    addExitButtons();

    // Set the default advanced dialog state
    updateAdvanced();
    setToolTipText();
  }

  public static FinalAlignedStackDialog getInstance(ApplicationManager appMgr,
      FinalAlignedStackExpert expert, AxisID axisID, Tab curTab) {
    FinalAlignedStackDialog instance = new FinalAlignedStackDialog(appMgr,
        expert, axisID, curTab);
    instance.addListeners();
    instance.tabbedPane.setSelectedIndex(curTab.toInt());
    return instance;
  }

  private void addListeners() {
    // Bind the buttons to the action listener
    btnFilter.addActionListener(finalAlignedStackListener);
    btnViewFilter.addActionListener(finalAlignedStackListener);
    btnUseFilter.addActionListener(finalAlignedStackListener);
    btnMtfFile.addActionListener(new MtfFileActionListener(this));
    ltfStartingAndEndingZ
        .addKeyListener(new StartingAndEndingZKeyListener(this));
    cbUseExpectedDefocus.addActionListener(finalAlignedStackListener);
    ftfConfigFile.addActionListener(finalAlignedStackListener);
    btnCtfPlotter.addActionListener(finalAlignedStackListener);
    btnCtfCorrection.addActionListener(finalAlignedStackListener);
    btnImodCtfCorrection.addActionListener(finalAlignedStackListener);
    btnUseCtfCorrection.addActionListener(finalAlignedStackListener);
    tabbedPane.addChangeListener(new TabChangeListener(this));
    cbParallelProcess.addActionListener(finalAlignedStackListener);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    tabbedPane.addMouseListener(mouseAdapter);
  }

  public static String getTilt3dFindButtonLabel() {
    return Tilt3dFindPanel.TILT_3D_FIND_LABEL;
  }

  public static String getReprojectModelButtonLabel() {
    return ReprojectModelPanel.REPROJECT_MODEL_LABEL;
  }

  public static ProcessResultDisplay getFullAlignedStackDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance(
        NewstackOrBlendmontPanel.RUN_BUTTON_LABEL, DIALOG_TYPE);
  }

  public static ProcessResultDisplay getXfModelDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance(
        "Transform Fiducial Model", DIALOG_TYPE);
  }

  public static ProcessResultDisplay getTilt3dFindButton() {
    return Tilt3dFindPanel.getTilt3dFindButton(DIALOG_TYPE);
  }

  public static ProcessResultDisplay getFindBeads3dButton() {
    return FindBeads3dPanel.getFindBeads3dButton(DIALOG_TYPE);
  }

  public static ProcessResultDisplay getReprojectModelDisplay() {
    return ReprojectModelPanel.getReprojectModelDisplay(DIALOG_TYPE);
  }

  public static ProcessResultDisplay getCcdEraserButton() {
    return Run3dmodButton.getDeferredToggle3dmodInstance(
        CcdEraserBeadsPanel.CCD_ERASER_LABEL, DIALOG_TYPE);
  }

  public static ProcessResultDisplay getUseCcdEraserDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Use Erased Stack",
        DIALOG_TYPE);
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

  void setInvertTiltAngles(boolean input) {
    cbInvertTiltAngles.setSelected(input);
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

  public void setEnabledTiltParameters(TomogramState state,
      ConstMetaData metaData) {
    eraseGoldPanel.setEnabledTiltParameters(state, metaData);
  }

  String getExpectedDefocus() {
    return ltfExpectedDefocus.getText();
  }

  void setUseFilterEnabled(boolean enable) {
    btnUseFilter.setEnabled(enable);
  }

  void setInterpolationWidth(ConstEtomoNumber input) {
    ltfInterpolationWidth.setText(input);
  }

  void setFiducialessAlignment(boolean input) {
    newstackOrBlendmontPanel.setFiducialessAlignment(input);
  }

  void setImageRotation(float input) {
    newstackOrBlendmontPanel.setImageRotation(input);
  }

  void setInverseRolloffRadiusSigma(String inverseRolloffRadiusSigma) {
    ltfInverseRolloffRadiusSigma.setText(inverseRolloffRadiusSigma);
  }

  void setLowPassRadiusSigma(String lowPassRadiusSigma) {
    ltfLowPassRadiusSigma.setText(lowPassRadiusSigma);
  }

  void setFilterHeaderState(PanelHeaderState state) {
    filterHeader.setState(state);
  }

  void setCtfCorrectionHeaderState(PanelHeaderState state) {
    ctfCorrectionHeader.setState(state);
  }

  Tab getCurTab() {
    return curTab;
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

  boolean getInvertTiltAngles() {
    return cbInvertTiltAngles.isSelected();
  }

  /**
   * The Metadata values that are from the setup dialog should not be overrided
   * by this dialog unless the Metadata values are empty.
   * @param metaData
   * @throws FortranInputSyntaxException
   */
  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    metaData.setFinalStackCtfCorrectionParallel(axisID, isParallelProcess());
    newstackOrBlendmontPanel.getParameters(metaData);
    eraseGoldPanel.getParameters(metaData);
  }

  BlendmontDisplay getBlendmontDisplay() {
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      return (BlendmontDisplay) newstackOrBlendmontPanel;
    }
    return null;
  }

  BlendmontDisplay getBlendmont3dFindDisplay() {
    return eraseGoldPanel.getBlendmont3dFindDisplay();
  }

  NewstackDisplay getNewstackDisplay() {
    if (applicationManager.getMetaData().getViewType() != ViewType.MONTAGE) {
      return (NewstackDisplay) newstackOrBlendmontPanel;
    }
    return null;
  }

  NewstackDisplay getNewstack3dFindDisplay() {
    return eraseGoldPanel.getNewstack3dFindDisplay();
  }

  TiltDisplay getTilt3dFindDisplay() {
    return eraseGoldPanel.getTilt3dFindDisplay();
  }

  FindBeads3dDisplay getFindBeads3dDisplay() {
    return eraseGoldPanel.getFindBeads3dDisplay();
  }

  CcdEraserDisplay getCcdEraserBeadsDisplay() {
    return eraseGoldPanel.getCcdEraserBeadsDisplay();
  }

  FiducialessParams getFiducialessParams() {
    return newstackOrBlendmontPanel.getFiducialessParams();
  }

  String getAmplitudeContrast() {
    return ltfAmplitudeContrast.getText();
  }

  void getFilterHeaderState(PanelHeaderState state) {
    filterHeader.getState(state);
  }

  void getCtfCorrectionHeaderState(PanelHeaderState state) {
    ctfCorrectionHeader.getState(state);
  }

  void getParameters(ReconScreenState screenState) {
    newstackOrBlendmontPanel.getParameters(screenState);
    eraseGoldPanel.getParameters(screenState);
    getFilterHeaderState(screenState.getStackMtffilterHeaderState());
    getCtfCorrectionHeaderState(screenState.getStackCtfCorrectionHeaderState());
  }

  void setParameters(ReconScreenState screenState) {
    newstackOrBlendmontPanel.setParameters(screenState);
    eraseGoldPanel.setParameters(screenState);
    setFilterHeaderState(screenState.getStackMtffilterHeaderState());
    setCtfCorrectionHeaderState(screenState.getStackCtfCorrectionHeaderState());
    setUseFilterButtonState(screenState);
    setFilterButtonState(screenState);
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

  public void expand(GlobalExpandButton button) {
  }

  /**
   * Expands the appropriate panel.  Also called when advanced/basic buttons
   * are pressed in child classes.  Responsible for changing the state of the
   * big advanced/basic button at the bottom of the dialog.
   */
  public void expand(ExpandButton button) {
    if (filterHeader.equalsAdvancedBasic(button)) {
      updateAdvancedFilter(button.isExpanded());
    }
    else if (ctfCorrectionHeader.equalsAdvancedBasic(button)) {
      updateAdvancedCtfCorrection(button.isExpanded());
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

  void setParameters(ConstMetaData metaData) {
    //Parallel processing is optional in tomogram reconstruction, so only use it
    //if the user set it up.
    boolean validAutodoc = Network.isParallelProcessingEnabled(
        applicationManager, axisID, applicationManager.getPropertyUserDir());
    cbParallelProcess.setEnabled(validAutodoc);
    ConstEtomoNumber parallel = metaData
        .getFinalStackCtfCorrectionParallel(axisID);
    if (parallel == null) {
      setParallelProcess(validAutodoc && metaData.getDefaultParallel().is());
    }
    else {
      setParallelProcess(validAutodoc && parallel.is());
    }
    newstackOrBlendmontPanel.setParameters(metaData);
    eraseGoldPanel.setParameters(metaData);
    updateParallelProcess();
  }

  public void updateParallelProcess() {
    applicationManager.setParallelDialog(axisID, usingParallelProcessing());
  }

  void setParameters(BlendmontParam param) {
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      ((BlendmontDisplay) newstackOrBlendmontPanel).setParameters(param);
    }
  }

  void setEraseGoldParameters(BlendmontParam param) {
    eraseGoldPanel.setParameters(param);
  }

  void setEraseGoldParameters(NewstParam param) {
    eraseGoldPanel.setParameters(param);
  }

  void setParameters(ConstTiltParam param, boolean calculateValues)
      throws FileNotFoundException, IOException, LogFile.LockException {
    eraseGoldPanel.setParameters(param, calculateValues);
  }

  void setParameters(ConstFindBeads3dParam param, boolean initialize) {
    eraseGoldPanel.setParameters(param, initialize);
  }

  void setParameters(ConstNewstParam param) {
    if (applicationManager.getMetaData().getViewType() != ViewType.MONTAGE) {
      ((NewstackDisplay) newstackOrBlendmontPanel).setParameters(param);
    }
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    boolean advanced = isAdvanced();
    newstackOrBlendmontPanel.updateAdvanced(advanced);
    eraseGoldPanel.updateAdvanced(advanced);
    updateAdvancedFilter(advanced);
    updateAdvancedCtfCorrection(advanced);
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  /**
   * Returns true if the current tab is one that uses parallel processing and
   * parallel processing has been turned on in that tab.
   */
  public boolean usingParallelProcessing() {
    return (curTab == Tab.CTF_CORRECTION && cbParallelProcess.isEnabled() && cbParallelProcess
        .isSelected())
        || (curTab == Tab.CCD_ERASER && eraseGoldPanel
            .usingParallelProcessing());
  }

  boolean isParallelProcess() {
    return cbParallelProcess.isSelected();
  }

  boolean isUseExpectedDefocus() {
    return cbUseExpectedDefocus.isSelected();
  }

  private void layoutCcdEraser() {
    //panel
    JPanel ccdEraserRoot = new JPanel();
    tabbedPane.addTab("Erase Gold", ccdEraserRoot);
  }

  private void layoutCtfCorrectionPanel() {
    //panel
    JPanel ctfCorrectionRoot = new JPanel();
    tabbedPane.addTab("Correct CTF", ctfCorrectionRoot);
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
    ctfCorrectionBodyPanel.add(cbInvertTiltAngles);
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
  }

  /**
   * Layout the newstack panel
   */
  private void layoutNewstPanel() {
    //panels
    JPanel newstRoot = new JPanel();
    tabbedPane.addTab("Create", newstRoot);
    newstRoot.add(newstackOrBlendmontPanel.getComponent());
  }

  /**
   * Layout the MTF filter panel
   *
   */
  private void layoutFilterPanel() {
    //panels
    JPanel filterRoot = new JPanel();
    tabbedPane.addTab("2D Filter", filterRoot);
    filterPanel.setLayout(new BoxLayout(filterPanel, BoxLayout.Y_AXIS));
    filterPanel.setBorder(BorderFactory.createEtchedBorder());
    filterPanel.add(filterHeader.getContainer());
    filterPanel.add(filterBodyPanel);
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
    //configure
    btnFilter.setSize();
    btnViewFilter.setSize();
    btnUseFilter.setSize();
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
    JFileChooser chooser = new FileChooser(new File(currentMtfDirectory));
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
        "Tilt", "Findbeads3d", "CcdEraser", "Mtffilter", "3dmod" };
    String[] manPage = { alignManpage + ".html", "ctfplotter.html",
        "ctfphaseflip.html", "tilt.html", "findbeads3d.html", "ccderaser.html",
        "mtffilter.html", "3dmod.html" };
    String[] logFileLabel = { alignLogfileLabel, "Ctfplotter", "Ctfcorrection",
        alignLogfileLabel + "_3dfind", "Tilt_3dfind", "Findbeads3d",
        "Mtffilter" };
    String[] logFile = new String[7];
    logFile[0] = alignLogfile + axisID.getExtension() + ".log";
    logFile[1] = "ctfplotter" + axisID.getExtension() + ".log";
    logFile[2] = "ctfcorrection" + axisID.getExtension() + ".log";
    logFile[3] = alignLogfile + "_3dfind" + axisID.getExtension() + ".log";
    logFile[4] = "tilt_3dfind" + axisID.getExtension() + ".log";
    logFile[5] = "findbeads3d" + axisID.getExtension() + ".log";
    logFile[6] = "mtffilter" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
        "FinalAligned", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager, axisID);
  }

  public void startingAndEndingZKeyReleased(KeyEvent event) {
    expert.enableUseFilter();
  }

  void done() {
    expert.doneDialog();
    newstackOrBlendmontPanel.done();
    eraseGoldPanel.done();
    btnUseFilter.removeActionListener(finalAlignedStackListener);
    btnFilter.removeActionListener(finalAlignedStackListener);
    btnCtfCorrection.removeActionListener(finalAlignedStackListener);
    btnUseCtfCorrection.removeActionListener(finalAlignedStackListener);
    setDisplayed(false);
  }

  private void chooseConfigFile(FileTextField fileTextField) {
    JFileChooser chooser = new FileChooser(expert.getConfigDir());
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
    if (command.equals(btnFilter.getActionCommand())) {
      expert.mtffilter(btnFilter, null, deferred3dmodButton,
          run3dmodMenuOptions);
    }
    else if (command.equals(btnUseFilter.getActionCommand())) {
      expert.useMtfFilter(btnUseFilter);
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
    else if (command.equals(cbParallelProcess.getActionCommand())) {
      updateParallelProcess();
    }
  }

  private void changeTab() {
    ((Container) tabbedPane.getComponentAt(curTab.toInt())).removeAll();
    curTab = Tab.getInstance(tabbedPane.getSelectedIndex());
    Container panel = (Container) tabbedPane.getSelectedComponent();
    if (curTab == Tab.NEWST) {
      panel.add(newstackOrBlendmontPanel.getComponent());
    }
    else if (curTab == Tab.CTF_CORRECTION) {
      panel.add(ctfCorrectionMainPanel);
    }
    else if (curTab == Tab.CCD_ERASER) {
      panel.add(eraseGoldPanel.getComponent());
    }
    else if (curTab == Tab.MTF_FILTER) {
      panel.add(filterPanel);
    }
    updateParallelProcess();
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(applicationManager,
          AutodocFactory.MTF_FILTER, axisID);
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
        .setToolTipText("View the results of running mtffilter on the full "
            + "aligned stack.");
    btnUseFilter
        .setToolTipText("Use the results of running mtffilter as the new full "
            + "aligned stack.");
    try {
      autodoc = AutodocFactory.getInstance(applicationManager,
          AutodocFactory.CTF_PLOTTER, axisID);
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
    if (autodoc != null) {
      ftfConfigFile.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          "ConfigFile"));
      ltfVoltage.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.VOLTAGE_OPTION)
          + "  Also used in " + CtfPhaseFlipParam.COMMAND + ".");
      ltfSphericalAberration.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.SPHERICAL_ABERRATION_OPTION)
          + "  Also used in " + CtfPhaseFlipParam.COMMAND + ".");
      cbInvertTiltAngles.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.INVERT_TILT_ANGLES_OPTION)
          + "  Also used in " + CtfPhaseFlipParam.COMMAND + ".");
      ltfAmplitudeContrast.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPhaseFlipParam.AMPLITUDE_CONTRAST_OPTION)
          + "  Also used in " + CtfPhaseFlipParam.COMMAND + ".");
      ltfExpectedDefocus.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          CtfPlotterParam.EXPECTED_DEFOCUS_OPTION));
    }
    btnCtfPlotter.setToolTipText("Run ctfplotter");
    try {
      autodoc = AutodocFactory.getInstance(applicationManager,
          AutodocFactory.CTF_PHASE_FLIP, axisID);
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

  private static final class TabChangeListener implements ChangeListener {
    private final FinalAlignedStackDialog adaptee;

    private TabChangeListener(FinalAlignedStackDialog dialog) {
      adaptee = dialog;
    }

    public void stateChanged(final ChangeEvent event) {
      adaptee.changeTab();
    }
  }

  static final class Tab {
    private static final Tab NEWST = new Tab(0);
    private static final Tab CTF_CORRECTION = new Tab(1);
    private static final Tab CCD_ERASER = new Tab(2);
    private static final Tab MTF_FILTER = new Tab(3);

    static final Tab DEFAULT = NEWST;

    private final int index;

    private Tab(int index) {
      this.index = index;
    }

    private static Tab getInstance(int index) {
      if (index == NEWST.index) {
        return NEWST;
      }
      if (index == CTF_CORRECTION.index) {
        return CTF_CORRECTION;
      }
      if (index == CCD_ERASER.index) {
        return CCD_ERASER;
      }
      if (index == MTF_FILTER.index) {
        return MTF_FILTER;
      }
      return DEFAULT;
    }

    private int toInt() {
      return index;
    }
  }
}
