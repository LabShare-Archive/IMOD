package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;

import etomo.PeetManager;
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.storage.LogFile;
import etomo.storage.MatlabParamFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstPeetMetaData;
import etomo.type.ConstPeetScreenState;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.PeetMetaData;
import etomo.type.PeetScreenState;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
 * <p> Revision 1.18  2007/04/02 21:52:40  sueh
 * <p> bug# 964 Rearranged fields.
 * <p>
 * <p> Revision 1.17  2007/04/02 16:03:29  sueh
 * <p> bug# 964 Added Run panel.
 * <p>
 * <p> Revision 1.16  2007/03/31 03:02:40  sueh
 * <p> bug# 964 Added szVol, edgeShift, meanFill, CCMode, and debugLevel.  Setting
 * <p> defaults.  Fixed enable bug.
 * <p>
 * <p> Revision 1.15  2007/03/30 23:51:43  sueh
 * <p> bug# 964 Added fields for the reference parameter.
 * <p>
 * <p> Revision 1.14  2007/03/27 19:31:26  sueh
 * <p> bug# 964
 * <p>
 * <p> Revision 1.13  2007/03/27 00:04:39  sueh
 * <p> bug# 964 Added setTooltipText.
 * <p>
 * <p> Revision 1.12  2007/03/26 18:39:44  sueh
 * <p> bug# 964 Moved InitMOTL and tilt range options to the Run Parameters windows.
 * <p>
 * <p> Revision 1.11  2007/03/23 20:43:03  sueh
 * <p> bug# 964 Fixed getParameters(MatlabParamFile):  tiltRangeEmpty was being
 * <p> set incorrectly.
 * <p>
 * <p> Revision 1.10  2007/03/21 19:46:16  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.9  2007/03/20 23:11:00  sueh
 * <p> bug# 964 Added "Use tilt range" checkbox.
 * <p>
 * <p> Revision 1.8  2007/03/20 00:45:17  sueh
 * <p> bug# 964 Added Initial MOTL radio buttons.
 * <p>
 * <p> Revision 1.7  2007/03/15 21:48:09  sueh
 * <p> bug# 964 Added setParameters(MatlabParamFile).
 * <p>
 * <p> Revision 1.6  2007/03/01 01:41:46  sueh
 * <p> bug# 964 Added initialize() to sets metadata fields that are only set once.
 * <p>
 * <p> Revision 1.5  2007/02/22 20:38:40  sueh
 * <p> bug# 964 Added a button to the Directory field.
 * <p>
 * <p> Revision 1.4  2007/02/21 22:30:22  sueh
 * <p> bug# 964 Fixing null pointer exception which occurred when loading the .epe file.
 * <p>
 * <p> Revision 1.3  2007/02/21 04:24:32  sueh
 * <p> bug# 964 Setting Output and Directory when Save As is called.  Disabling edit
 * <p> for Output and Directory when the paramFile is set.
 * <p>
 * <p> Revision 1.2  2007/02/20 20:36:46  sueh
 * <p> bug# 964 Started the setup panel.
 * <p>
 * <p> Revision 1.1  2007/02/19 22:03:19  sueh
 * <p> bug# 964 Dialog for PEET interface.
 * <p> </p>
 */

public final class PeetDialog implements AbstractParallelDialog, Expandable {
  public static final String rcsid = "$Id$";

  static final String DIRECTORY_LABEL = "Directory";
  static final String OUTPUT_LABEL = "Root name for output";
  static final String VOLUME_NUMBER_LABEL = "Volume #: ";
  static final String REFERENCE_FILE_LABEL = "Reference file: ";

  private static final DialogType DIALOG_TYPE = DialogType.PEET;

  private final JPanel rootPanel = new JPanel();
  private final FileTextField ftfDirectory = new FileTextField(DIRECTORY_LABEL
      + ": ");
  private final LabeledTextField ltfFnOutput = new LabeledTextField(OUTPUT_LABEL
      + ": ");
  private final SpacedPanel pnlSetupBody = new SpacedPanel();
  private final CheckBox cbTiltRange = new CheckBox(
      "Use tilt range for missing wedge compensation");
  private final SpacedPanel pnlRunParametersBody = new SpacedPanel();
  private final LabeledTextField ltfParticleNumber = new LabeledTextField(
      "Particle #: ");
  private final FileTextField ftfReferenceFile = FileTextField
      .getUnlabeledInstance(REFERENCE_FILE_LABEL);
  private final LabeledTextField ltfSzVolX = new LabeledTextField(
  "Particle volume size in X: ");
  private final LabeledTextField ltfSzVolY = new LabeledTextField("Y: ");
  private final LabeledTextField ltfSzVolZ = new LabeledTextField("Z: ");
  private final LabeledTextField ltfEdgeShift = new LabeledTextField(
      "Edge shift: ");
  private final CheckBox cbMeanFill = new CheckBox("Mean fill");
  private final LabeledTextField ltfAlignedBaseName = new LabeledTextField(
      "Aligned base name: ");
  private final LabeledTextField ltfLowCutoff = new LabeledTextField(
      "Low cutoff: ");
  private final CheckBox cbRefFlagAllTom = new CheckBox(
      "Use equal numbers of particles from all tomogram for new reference");
  private final LabeledTextField ltfLstThresholdsStart = new LabeledTextField(
      "Start: ");
  private final LabeledTextField ltfLstThresholdsIncrement = new LabeledTextField(
      "Incr.: ");
  private final LabeledTextField ltfLstThresholdsEnd = new LabeledTextField(
      "End: ");
  private final LabeledTextField ltfLstThresholdsAdditional = new LabeledTextField(
      " Additional numbers: ");
  private final CheckBox cbLstFlagAllTom = new CheckBox(
      "Use equal numbers of particles from all tomograms for averages");
  private final SpacedPanel pnlRunBody = new SpacedPanel(true);
  private final MultiLineButton btnRun = new MultiLineButton("Run");
  private final JPanel pnlAdvanced = new JPanel();
  private final LabeledSpinner lsParticlePerCPU;
  private final IterationTable iterationTable;
  private final RadioButton rbInitMotlZero;
  private final RadioButton rbInitMotlZAxis;
  private final RadioButton rbInitMotlXAndZAxis;
  private final RadioButton rbInitMotlFiles;
  private final PanelHeader phSetup;
  private final PanelHeader phRunParameters;
  private final VolumeTable volumeTable;
  private final PeetManager manager;
  private final AxisID axisID;
  private final RadioButton rbVolumeReference;
  private final Spinner sVolumeNumber;
  private final RadioButton rbVolumeFile;
  private final RadioButton rbCcModeNormalized;
  private final RadioButton rbCcModeLocal;
  private final LabeledSpinner lsDebugLevel;
  private final PanelHeader phRun;

  private PeetDialog(final PeetManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    //setup construction
    phSetup = PanelHeader.getInstance("Setup", this, DIALOG_TYPE);
    volumeTable = VolumeTable.getInstance(manager, this);
    //run parameters construction
    phRunParameters = PanelHeader.getAdvancedBasicInstance("Run Parameters",
        this, DIALOG_TYPE);
    ButtonGroup group = new ButtonGroup();
    rbVolumeReference = new RadioButton(VOLUME_NUMBER_LABEL, group);
    rbVolumeFile = new RadioButton(REFERENCE_FILE_LABEL, group);
    sVolumeNumber = new Spinner(VOLUME_NUMBER_LABEL);
    group = new ButtonGroup();
    rbInitMotlZero = new RadioButton("Set all rotational values to zero",
        MatlabParamFile.InitMotlCode.ZERO.intValue(), group);
    rbInitMotlZAxis = new RadioButton("Initialize Z axis",
        MatlabParamFile.InitMotlCode.Z_AXIS.intValue(), group);
    rbInitMotlXAndZAxis = new RadioButton("Initialize X and Z axis",
        MatlabParamFile.InitMotlCode.X_AND_Z_AXIS.intValue(), group);
    rbInitMotlFiles = new RadioButton("Use files", group);
    group = new ButtonGroup();
    rbCcModeNormalized = new RadioButton(
        "Local energy normalized cross correlation",
        MatlabParamFile.CCModeCode.NORMALIZED.intValue(), group);
    rbCcModeLocal = new RadioButton("True local correlation coefficent",
        MatlabParamFile.CCModeCode.LOCAL.intValue(), group);
    lsDebugLevel = new LabeledSpinner("Debug level: ", new SpinnerNumberModel(
        MatlabParamFile.DEBUG_LEVEL_DEFAULT, MatlabParamFile.DEBUG_LEVEL_MIN,
        MatlabParamFile.DEBUG_LEVEL_MAX, 1));
    //run construction
    phRun = PanelHeader.getInstance("Run", this, DIALOG_TYPE);
    iterationTable = IterationTable.getInstance(manager);
    lsParticlePerCPU = new LabeledSpinner("Particles per CPU: ",
        new SpinnerNumberModel(MatlabParamFile.PARTICLE_PER_CPU_DEFAULT,
            MatlabParamFile.PARTICLE_PER_CPU_MIN,
            MatlabParamFile.PARTICLE_PER_CPU_MAX, 1));
    //panels
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new EtchedBorder("PEET").getBorder());
    rootPanel.add(createSetupPanel());
    rootPanel.add(createRunParametersPanel());
    rootPanel.add(createRunPanel());
    setDefaults();
    updateDisplay();
    updateAdvanceRunParameters(phRunParameters.isAdvanced());
    setTooltipText();
  }

  private void setTooltipText() {
    ftfDirectory
        .setToolTipText("The directory which will contain the .prm file, .epe file, other data files, intermediate files, and results.  "
            + "Only one .epe file per directory.");
    ltfFnOutput.setToolTipText("The root name for the .prm file.");
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory
          .getInstance(AutodocFactory.PEET_PRM);
      String tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.INIT_MOTL_KEY);
      rbInitMotlZero.setToolTipText(tooltip);
      rbInitMotlXAndZAxis.setToolTipText(tooltip);
      rbInitMotlZAxis.setToolTipText(tooltip);
      rbInitMotlFiles.setToolTipText(tooltip);
      tooltip = EtomoAutodoc
          .getTooltip(autodoc, MatlabParamFile.TILT_RANGE_KEY);
      cbTiltRange.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParamFile.REFERENCE_KEY);
      rbVolumeReference.setToolTipText(tooltip);
      rbVolumeFile.setToolTipText(tooltip);
      sVolumeNumber.setToolTipText(tooltip);
      ltfParticleNumber.setToolTipText(tooltip);
      ftfReferenceFile.setToolTipText(tooltip);
      ltfEdgeShift.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.EDGE_SHIFT_KEY));
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParamFile.SZ_VOL_KEY);
      ltfSzVolX.setToolTipText(tooltip);
      ltfSzVolY.setToolTipText(tooltip);
      ltfSzVolZ.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParamFile.CC_MODE_KEY);
      rbCcModeNormalized.setToolTipText(tooltip);
      rbCcModeLocal.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParamFile.MEAN_FILL_KEY);
      cbMeanFill.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.DEBUG_LEVEL_KEY);
      lsDebugLevel.setToolTipText(tooltip);
      tooltip = EtomoAutodoc
          .getTooltip(autodoc, MatlabParamFile.LOW_CUTOFF_KEY);
      ltfLowCutoff.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.REF_FLAG_ALL_TOM_KEY);
      cbRefFlagAllTom.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.LST_THRESHOLDS_KEY);
      ltfLstThresholdsStart.setToolTipText(tooltip);
      ltfLstThresholdsIncrement.setToolTipText(tooltip);
      ltfLstThresholdsEnd.setToolTipText(tooltip);
      ltfLstThresholdsAdditional.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.LST_FLAG_ALL_TOM_KEY);
      cbLstFlagAllTom.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.PARTICLE_PER_CPU_KEY);
      lsParticlePerCPU.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParamFile.ALIGNED_BASE_NAME_KEY);
      ltfAlignedBaseName.setToolTipText(tooltip);
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
  }

  public static PeetDialog getInstance(final PeetManager manager,
      final AxisID axisID) {
    PeetDialog instance = new PeetDialog(manager, axisID);
    instance.addListeners();
    return instance;
  }

  public void updateDisplay(final boolean paramFileSet) {
    ftfDirectory.setEditable(!paramFileSet);
    ltfFnOutput.setEditable(!paramFileSet);
  }

  public Container getContainer() {
    return rootPanel;
  }

  public DialogType getDialogType() {
    return DIALOG_TYPE;
  }

  public void getParameters(final ParallelParam param) {
    ProcesschunksParam processchunksParam = (ProcesschunksParam) param;
    processchunksParam.setRootName(ltfFnOutput.getText());
  }

  public void getParameters(final PeetScreenState screenState) {
    phSetup.getState(screenState.getPeetSetupHeaderState());
  }

  public void setParameters(final ConstPeetScreenState screenState) {
    phSetup.setState(screenState.getPeetSetupHeaderState());
  }

  public void getParameters(final PeetMetaData metaData) {
    volumeTable.getParameters(metaData);
  }

  public void setParameters(final ConstPeetMetaData metaData) {
    ltfFnOutput.setText(metaData.getName());
    volumeTable.setParameters(metaData);
  }

  public void setParameters(final MatlabParamFile matlabParamFile) {
    MatlabParamFile.InitMotlCode initMotlCode = matlabParamFile
        .getInitMotlCode();
    if (initMotlCode == null) {
      rbInitMotlFiles.setSelected(true);
    }
    else if (initMotlCode == MatlabParamFile.InitMotlCode.ZERO) {
      rbInitMotlZero.setSelected(true);
    }
    else if (initMotlCode == MatlabParamFile.InitMotlCode.Z_AXIS) {
      rbInitMotlZAxis.setSelected(true);
    }
    else if (initMotlCode == MatlabParamFile.InitMotlCode.X_AND_Z_AXIS) {
      rbInitMotlXAndZAxis.setSelected(true);
    }
    cbTiltRange.setSelected(matlabParamFile.isTiltRangeEmpty());
    volumeTable.setParameters(matlabParamFile, rbInitMotlFiles.isSelected(),
        cbTiltRange.isSelected());
    updateDisplay();
  }

  public void getParameters(final MatlabParamFile matlabParamFile) {
    if (rbInitMotlFiles.isSelected()) {
      matlabParamFile.setInitMotlCode(rbInitMotlFiles.getRadioValue());
    }
    if (rbInitMotlZero.isSelected()) {
      matlabParamFile.setInitMotlCode(rbInitMotlZero.getRadioValue());
    }
    if (rbInitMotlZAxis.isSelected()) {
      matlabParamFile.setInitMotlCode(rbInitMotlZAxis.getRadioValue());
    }
    if (rbInitMotlXAndZAxis.isSelected()) {
      matlabParamFile.setInitMotlCode(rbInitMotlXAndZAxis.getRadioValue());
    }
    matlabParamFile.setTiltRangeEmpty(!cbTiltRange.isSelected());
    volumeTable.getParameters(matlabParamFile);
  }

  public String getFnOutput() {
    return ltfFnOutput.getText();
  }

  public boolean usingParallelProcessing() {
    return true;
  }

  public void expand(final ExpandButton button) {
    if (phSetup.equalsOpenClose(button)) {
      pnlSetupBody.setVisible(button.isExpanded());
    }
    else if (phRunParameters.equalsOpenClose(button)) {
      pnlRunParametersBody.setVisible(button.isExpanded());
    }
    else if (phRunParameters.equalsAdvancedBasic(button)) {
      updateAdvanceRunParameters(button.isExpanded());
    }
    else if (phRun.equalsOpenClose(button)) {
      pnlRunBody.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public String getDirectory() {
    return ftfDirectory.getText();
  }

  public void setDirectory(final String directory) {
    ftfDirectory.setText(directory);
  }

  public void setFnOutput(final String output) {
    ltfFnOutput.setText(output);
  }

  void msgVolumeTableSizeChanged() {
    updateDisplay();
  }

  void setUsingInitMotlFile() {
    rbInitMotlFiles.setSelected(true);
  }

  private void updateAdvanceRunParameters(boolean advanced) {
    pnlAdvanced.setVisible(advanced);
  }

  private void setDefaults() {
    if (MatlabParamFile.INIT_MOTL_DEFAULT == MatlabParamFile.InitMotlCode.ZERO) {
      rbInitMotlZero.setSelected(true);
    }
    else if (MatlabParamFile.INIT_MOTL_DEFAULT == MatlabParamFile.InitMotlCode.Z_AXIS) {
      rbInitMotlZAxis.setSelected(true);
    }
    else if (MatlabParamFile.INIT_MOTL_DEFAULT == MatlabParamFile.InitMotlCode.X_AND_Z_AXIS) {
      rbInitMotlXAndZAxis.setSelected(true);
    }
    else if (MatlabParamFile.INIT_MOTL_DEFAULT == null) {
      rbInitMotlFiles.setSelected(true);
    }
    ltfEdgeShift.setText(MatlabParamFile.EDGE_SHIFT_DEFAULT);
    MatlabParamFile.CCModeCode ccModeCode = MatlabParamFile.CCModeCode
        .getDefault();
    if (ccModeCode == MatlabParamFile.CCModeCode.NORMALIZED) {
      rbCcModeNormalized.setSelected(true);
    }
    else if (ccModeCode == MatlabParamFile.CCModeCode.LOCAL) {
      rbCcModeLocal.setSelected(true);
    }
    cbMeanFill.setSelected(MatlabParamFile.MEAN_FILL_DEFAULT);
    ltfLowCutoff.setText(MatlabParamFile.LOW_CUTOFF_DEFAULT);
  }

  private Container createSetupPanel() {
    //body
    pnlSetupBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetupBody.add(ftfDirectory.getContainer());
    pnlSetupBody.add(ltfFnOutput.getContainer());
    pnlSetupBody.add(volumeTable.getContainer());
    //main panel
    SpacedPanel pnlSetup = new SpacedPanel();
    pnlSetup.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetup.setBorder(BorderFactory.createEtchedBorder());
    pnlSetup.add(phSetup.getContainer());
    pnlSetup.add(pnlSetupBody);
    return pnlSetup.getContainer();
  }

  private Container createRunParametersPanel() {
    //volume reference
    JPanel pnlVolumeReference = new JPanel();
    pnlVolumeReference.setLayout(new BoxLayout(pnlVolumeReference,
        BoxLayout.X_AXIS));
    pnlVolumeReference.add(rbVolumeReference.getComponent());
    pnlVolumeReference.add(sVolumeNumber.getComponent());
    pnlVolumeReference.add(ltfParticleNumber.getContainer());
    //volume file
    JPanel pnlVolumeFile = new JPanel();
    pnlVolumeFile.setLayout(new BoxLayout(pnlVolumeFile, BoxLayout.X_AXIS));
    pnlVolumeFile.add(rbVolumeFile.getComponent());
    pnlVolumeFile.add(ftfReferenceFile.getContainer());
    //reference
    JPanel pnlReference = new JPanel();
    pnlReference.setLayout(new BoxLayout(pnlReference, BoxLayout.Y_AXIS));
    pnlReference.setBorder(new EtchedBorder("Reference").getBorder());
    pnlReference.add(pnlVolumeReference);
    pnlReference.add(pnlVolumeFile);
    //szVol
    SpacedPanel pnlSzVol = new SpacedPanel();
    pnlSzVol.setBoxLayout(BoxLayout.X_AXIS);
    pnlSzVol.add(ltfSzVolX.getContainer());
    pnlSzVol.add(ltfSzVolY.getContainer());
    pnlSzVol.add(ltfSzVolZ.getContainer());
    //init MOTL
    JPanel pnlInitMotl = new JPanel();
    pnlInitMotl.setLayout(new BoxLayout(pnlInitMotl, BoxLayout.Y_AXIS));
    pnlInitMotl.setBorder(new EtchedBorder("Initial Motive List").getBorder());
    pnlInitMotl.add(rbInitMotlZero.getComponent());
    pnlInitMotl.add(rbInitMotlZAxis.getComponent());
    pnlInitMotl.add(rbInitMotlXAndZAxis.getComponent());
    pnlInitMotl.add(rbInitMotlFiles.getComponent());
    //tiltRange and edgeShift
    JPanel pnlTiltRange = new JPanel();
    pnlTiltRange.setLayout(new BoxLayout(pnlTiltRange,BoxLayout.X_AXIS));
    pnlTiltRange.add(cbTiltRange);
    pnlTiltRange.add(Box.createRigidArea(FixedDim.x40_y0));
    ltfEdgeShift.setTextPreferredWidth(UIParameters.INSTANCE.getIntegerWidth());
    pnlTiltRange.add(ltfEdgeShift.getContainer());
    //CCMode
    JPanel pnlCcMode = new JPanel();
    pnlCcMode.setLayout(new BoxLayout(pnlCcMode, BoxLayout.Y_AXIS));
    pnlCcMode.setBorder(new EtchedBorder("Cross correlation measure")
        .getBorder());
    pnlCcMode.add(rbCcModeNormalized.getComponent());
    pnlCcMode.add(rbCcModeLocal.getComponent());
    //advanced right panel
    JPanel pnlAdvancedRight = new JPanel();
    pnlAdvancedRight.setLayout(new BoxLayout(pnlAdvancedRight, BoxLayout.Y_AXIS));
    pnlAdvancedRight.add(cbMeanFill);
    pnlAdvancedRight.add(ltfAlignedBaseName.getContainer());
    pnlAdvancedRight.add(ltfLowCutoff.getContainer());
    pnlAdvancedRight.add(lsDebugLevel.getContainer());
    //advanced panel
    pnlAdvanced.setLayout(new BoxLayout(pnlAdvanced,BoxLayout.X_AXIS));
    pnlAdvanced.add(pnlCcMode);
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x40_y0));
    pnlAdvanced.add(pnlAdvancedRight);
    //body
    pnlRunParametersBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRunParametersBody.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRunParametersBody.add(pnlReference);
    pnlRunParametersBody.add(pnlSzVol);
    pnlRunParametersBody.add(pnlInitMotl);
    pnlRunParametersBody.add(pnlTiltRange);
    pnlRunParametersBody.add(pnlAdvanced);
    //main panel
    JPanel pnlRunParameters = new JPanel();
    pnlRunParameters
        .setLayout(new BoxLayout(pnlRunParameters, BoxLayout.Y_AXIS));
    pnlRunParameters.setBorder(BorderFactory.createEtchedBorder());
    pnlRunParameters.add(phRunParameters.getContainer());
    pnlRunParameters.add(pnlRunParametersBody.getContainer());
    return pnlRunParameters;
  }

  private Container createRunPanel() {
    //lstThresholds
    SpacedPanel pnlLstThresholds = new SpacedPanel();
    pnlLstThresholds.setBoxLayout(BoxLayout.X_AXIS);
    pnlLstThresholds.setBorder(new EtchedBorder("Number of Particles in Averages").getBorder());
    pnlLstThresholds.add(ltfLstThresholdsStart.getContainer());
    pnlLstThresholds.add(ltfLstThresholdsIncrement.getContainer());
    pnlLstThresholds.add(ltfLstThresholdsEnd.getContainer());
    pnlLstThresholds.add(ltfLstThresholdsAdditional.getContainer());
    //body
    pnlRunBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRunBody.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRunBody.add(iterationTable.getContainer());
    pnlRunBody.add(cbRefFlagAllTom);
    pnlRunBody.add(pnlLstThresholds);
    pnlRunBody.add(cbLstFlagAllTom);
    pnlRunBody.add(lsParticlePerCPU);
    btnRun.setSize();
    pnlRunBody.add(btnRun);
    //main panel
    JPanel pnlRun = new JPanel();
    pnlRun.setLayout(new BoxLayout(pnlRun, BoxLayout.Y_AXIS));
    pnlRun.setBorder(BorderFactory.createEtchedBorder());
    pnlRun.add(phRun.getContainer());
    pnlRun.add(pnlRunBody.getContainer());
    return pnlRun;
  }

  private void action(ActionEvent action) {
    String actionCommand = action.getActionCommand();
    if (actionCommand.equals(ftfDirectory.getActionCommand())) {
      chooseDirectory();
    }
    else if (actionCommand.equals(rbInitMotlZero.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(rbInitMotlZAxis.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(rbInitMotlXAndZAxis.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(rbInitMotlFiles.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(rbVolumeReference.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(rbVolumeFile.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(cbTiltRange.getActionCommand())) {
      updateDisplay();
    }
  }

  private void referenceFileAction() {
    chooseReferenceFile();
  }

  private void updateDisplay() {
    //edge shift
    ltfEdgeShift.setEnabled(cbTiltRange.isSelected());
    //reference
    int size = volumeTable.size();
    boolean enable = size > 1;
    rbVolumeReference.setEnabled(enable);
    sVolumeNumber.setEnabled(enable && rbVolumeReference.isSelected());
    sVolumeNumber.setMax(size);
    ltfParticleNumber.setEnabled(enable && rbVolumeReference.isSelected());
    ftfReferenceFile.setEnabled(rbVolumeFile.isSelected());
    volumeTable.updateDisplay(rbInitMotlFiles.isSelected(),cbTiltRange.isSelected());
  }

  private void chooseDirectory() {
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      ftfDirectory.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }

  private void chooseReferenceFile() {
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      ftfReferenceFile.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }

  private void addListeners() {
    PDActionListener actionListener = new PDActionListener(this);
    ftfDirectory.addActionListener(actionListener);
    rbInitMotlZero.addActionListener(actionListener);
    rbInitMotlZAxis.addActionListener(actionListener);
    rbInitMotlXAndZAxis.addActionListener(actionListener);
    rbInitMotlFiles.addActionListener(actionListener);
    rbVolumeReference.addActionListener(actionListener);
    rbVolumeFile.addActionListener(actionListener);
    ftfReferenceFile.addActionListener(new ReferenceFileActionListener(this));
    cbTiltRange.addActionListener(actionListener);
  }

  private class PDActionListener implements ActionListener {
    private final PeetDialog peetDialog;

    private PDActionListener(final PeetDialog peetDialog) {
      this.peetDialog = peetDialog;
    }

    public void actionPerformed(final ActionEvent event) {
      peetDialog.action(event);
    }
  }

  private class ReferenceFileActionListener implements ActionListener {
    private final PeetDialog peetDialog;

    private ReferenceFileActionListener(final PeetDialog peetDialog) {
      this.peetDialog = peetDialog;
    }

    public void actionPerformed(final ActionEvent event) {
      peetDialog.referenceFileAction();
    }
  }
}
