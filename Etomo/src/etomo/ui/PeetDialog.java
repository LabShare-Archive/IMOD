package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.PeetManager;
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.MatlabParamFileFilter;
import etomo.storage.PeetAndMatlabParamFileFilter;
import etomo.storage.PeetFileFilter;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.ConstPeetMetaData;
import etomo.type.ConstPeetScreenState;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.PeetMetaData;
import etomo.type.PeetScreenState;
import etomo.type.Run3dmodMenuOptions;

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
 * <p> Revision 1.62  2008/08/22 17:52:00  sueh
 * <p> bug# 1136 Added sNWeightGroup.
 * <p>
 * <p> Revision 1.61  2008/05/28 02:50:18  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 1.60  2008/05/03 00:52:01  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.59  2008/04/02 19:06:55  sueh
 * <p> bug# 1104 Reformat.
 * <p>
 * <p> Revision 1.58  2008/04/02 02:28:06  sueh
 * <p> bug# 1097 Added mask fields.
 * <p>
 * <p> Revision 1.57  2008/03/06 00:28:20  sueh
 * <p> bug# 1088 Added sampleSphere radio buttons and sampleInterval.
 * <p>
 * <p> Revision 1.56  2008/02/19 00:47:25  sueh
 * <p> bug# 1078 Setting field width in ftfReferenceFile.
 * <p>
 * <p> Revision 1.55  2007/12/14 21:46:20  sueh
 * <p> bug# 1060 Changed meanFill to flgMeanFill.
 * <p>
 * <p> Revision 1.54  2007/12/10 22:45:08  sueh
 * <p> bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * <p> getParameters because it is required and has been added to the
 * <p> ProcesschunksParam constructor.
 * <p>
 * <p> Revision 1.53  2007/08/02 22:38:55  sueh
 * <p> bug# 1034 Adding a right click menu to btnAvgVol and btnRef.
 * <p>
 * <p> Revision 1.52  2007/07/24 04:08:18  sueh
 * <p> bug# 1031 Reverse the value of refFlagAllTom and lstFlagAllTom.
 * <p>
 * <p> Revision 1.51  2007/07/20 19:16:17  mast
 * <p> Add s to tomograms
 * <p>
 * <p> Revision 1.50  2007/07/10 00:43:49  sueh
 * <p> bug# 1022 In validateRun, calling IterationTable.validateRun().
 * <p>
 * <p> Revision 1.49  2007/06/08 22:21:59  sueh
 * <p> bug# 1014 Added reset().
 * <p>
 * <p> Revision 1.48  2007/06/06 22:05:52  sueh
 * <p> bug# 1010 Reorganized Setup panel.
 * <p>
 * <p> Revision 1.47  2007/06/06 16:59:22  sueh
 * <p> bug# 1015 Implemented ContextMenu.  Added popUpContextMenu().
 * <p>
 * <p> Revision 1.46  2007/06/06 16:07:01  sueh
 * <p> bug# 1013 Added validateRun().
 * <p>
 * <p> Revision 1.45  2007/06/05 21:33:08  sueh
 * <p> bug# 1010 Added flgWedgeWeight.
 * <p>
 * <p> Revision 1.44  2007/06/05 17:59:32  sueh
 * <p> bug# 1007 Adding to lstThresholds tooltip.
 * <p>
 * <p> Revision 1.43  2007/06/04 23:08:34  sueh
 * <p> bug# 1005 Passing parametersOnly to getParameters.
 * <p>
 * <p> Revision 1.42  2007/05/31 22:26:59  sueh
 * <p> bug# 1004 Changed OUTPUT_LABEL to FN_OUTPUT_LABEL.
 * <p>
 * <p> Revision 1.41  2007/05/18 23:53:35  sueh
 * <p> bug# 987
 * <p>
 * <p> Revision 1.40  2007/05/17 23:49:35  sueh
 * <p> bug# 964 Rearrange fields to save space.  Check ccMode local when
 * <p> use tilt range is checked.
 * <p>
 * <p> Revision 1.39  2007/05/16 23:48:12  sueh
 * <p> bug# 964 Removed print statements.
 * <p>
 * <p> Revision 1.38  2007/05/16 22:59:57  sueh
 * <p> bug# 964 Added btnDuplicateProject.
 * <p>
 * <p> Revision 1.37  2007/05/15 21:45:38  sueh
 * <p> bug# 964 Added btnRef.
 * <p>
 * <p> Revision 1.36  2007/05/11 16:06:48  sueh
 * <p> bug# 964 Added btnAvgVol.
 * <p>
 * <p> Revision 1.35  2007/05/08 19:18:01  sueh
 * <p> bug# 964 Passing the import directory to VolumeTable.setParameters()
 * <p> when importing the .prm file, so that files which don't have an absolute
 * <p> path will have the import directory as their parent.
 * <p>
 * <p> Revision 1.34  2007/05/08 01:20:15  sueh
 * <p> bug# 964 Using enum tooltips for radio buttons.
 * <p>
 * <p> Revision 1.33  2007/05/07 17:23:48  sueh
 * <p> bug# 964 Changed MatlabParamFile to MatlabParam.
 * <p>
 * <p> Revision 1.32  2007/05/03 21:17:31  sueh
 * <p> bug# 964 Added btnImportMatlabParamFile (not implemented yet).
 * <p>
 * <p> Revision 1.31  2007/05/02 16:35:36  sueh
 * <p> bug# 964 Default reference source not being set.  YaxisContour model
 * <p> number spinner was set to null.
 * <p>
 * <p> Revision 1.30  2007/05/01 22:29:42  sueh
 * <p> bug# 964 Added yaxisType and yaxisContour.
 * <p>
 * <p> Revision 1.29  2007/05/01 00:44:21  sueh
 * <p> bug# 964 Removed the run parameter panel.  Created a tabbed panel.
 * <p> Moved fields associated with the volume table to the Setup tab.  Moved
 * <p> the other fields to the Run tab.
 * <p>
 * <p> Revision 1.28  2007/04/27 23:39:59  sueh
 * <p> bug# 964 Changed prmParser to peetParser.
 * <p>
 * <p> Revision 1.27  2007/04/26 02:49:43  sueh
 * <p> bug# 964 Added btnRun to action().
 * <p>
 * <p> Revision 1.26  2007/04/20 20:53:35  sueh
 * <p> bug# 964 Added support for refFlagAllTom, lstFlagAllTom, ParticlePerCpu.
 * <p>
 * <p> Revision 1.25  2007/04/19 22:05:13  sueh
 * <p> bug# 964 Added support for ltfThresholds.
 * <p>
 * <p> Revision 1.24  2007/04/13 21:52:33  sueh
 * <p> bug# 964 Saving/retrieving debugLevel to/from MatlabParamFile.
 * <p>
 * <p> Revision 1.23  2007/04/13 18:50:24  sueh
 * <p> bug# 964 Saving/retrieving ccMode, meanFill, and lowCutoff to/from
 * <p> MatlabParamFile.  Adding EnumerationTypes directly to associated radio buttons.
 * <p>
 * <p> Revision 1.22  2007/04/11 22:22:06  sueh
 * <p> bug# 964 Saving edgeShift to meta data and MatlabParamFile.
 * <p>
 * <p> Revision 1.21  2007/04/09 22:00:21  sueh
 * <p> bug# 964 Getting and setting szVol from MatlabParamFile.  Filling in Y and Z from
 * <p> X when they are empty.
 * <p>
 * <p> Revision 1.20  2007/04/09 21:20:23  sueh
 * <p> bug# 964 Fixed the names of the reference fields.
 * <p>
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

public final class PeetDialog implements ContextMenu, AbstractParallelDialog,
    Expandable, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  public static final String FN_OUTPUT_LABEL = "Root name for output";
  public static final String DIRECTORY_LABEL = "Directory";

  private static final String PARTICLE_LABEL = "Particle #: ";
  private static final String REFERENCE_VOLUME_LABEL = "Volume #: ";
  private static final String REFERENCE_FILE_LABEL = "Reference file: ";
  private static final String MODEL_LABEL = "Model #: ";
  private static final String MASK_TYPE_VOLUME_LABEL = "Volume";
  private static final DialogType DIALOG_TYPE = DialogType.PEET;
  private static final String LST_THRESHOLD_START_TITLE = "Start";
  private static final String LST_THRESHOLD_INCREMENT_TITLE = "Incr.";
  private static final String LST_THRESHOLD_END_TITLE = "End";
  private static final String LST_THRESHOLD_ADDITIONAL_NUMBERS_TITLE = "Additional numbers";

  private final JPanel rootPanel = new JPanel();
  private final FileTextField ftfDirectory = new FileTextField(DIRECTORY_LABEL
      + ": ");
  private final LabeledTextField ltfFnOutput = new LabeledTextField(
      FN_OUTPUT_LABEL + ": ");
  private final SpacedPanel pnlSetupBody = new SpacedPanel();
  private final CheckBox cbTiltRange = new CheckBox(
      "Use tilt range in averaging");
  private final LabeledTextField ltfReferenceParticle = new LabeledTextField(
      PARTICLE_LABEL);
  private final FileTextField ftfReferenceFile = FileTextField
      .getUnlabeledInstance(REFERENCE_FILE_LABEL);
  private final LabeledTextField ltfSzVolX = new LabeledTextField(
      "Particle volume X: ");
  private final LabeledTextField ltfSzVolY = new LabeledTextField("Y: ");
  private final LabeledTextField ltfSzVolZ = new LabeledTextField("Z: ");
  private final LabeledTextField ltfEdgeShift = new LabeledTextField(
      "Edge shift: ");
  private final CheckBox cbFlgMeanFill = new CheckBox("Mean fill");
  private final LabeledTextField ltfAlignedBaseName = new LabeledTextField(
      "Aligned base name: ");
  private final LabeledTextField ltfLowCutoff = new LabeledTextField(
      "Low frequency filter: ");
  private final CheckBox cbRefFlagAllTom = new CheckBox(
      "Use equal numbers of particles from all tomograms for new reference");
  private final LabeledTextField ltfLstThresholdsStart = new LabeledTextField(
      LST_THRESHOLD_START_TITLE + ": ");
  private final LabeledTextField ltfLstThresholdsIncrement = new LabeledTextField(
      LST_THRESHOLD_INCREMENT_TITLE + ": ");
  private final LabeledTextField ltfLstThresholdsEnd = new LabeledTextField(
      LST_THRESHOLD_END_TITLE + ": ");
  private final LabeledTextField ltfLstThresholdsAdditional = new LabeledTextField(
      " " + LST_THRESHOLD_ADDITIONAL_NUMBERS_TITLE + ": ");
  private final LabeledTextField ltfYaxisContourObjectNumber = new LabeledTextField(
      " Object #: ");
  private final LabeledTextField ltfYaxisContourContourNumber = new LabeledTextField(
      " Contour #: ");
  private final CheckBox cbLstFlagAllTom = new CheckBox(
      "Use equal numbers of particles from all tomograms for averages");
  private final SpacedPanel pnlRunBody = new SpacedPanel(true);
  private final MultiLineButton btnRun = new MultiLineButton("Run");
  private final JPanel pnlAdvanced = new JPanel();
  private final ButtonGroup bgReference = new ButtonGroup();
  private final RadioButton rbReferenceParticle = new RadioButton(
      REFERENCE_VOLUME_LABEL, bgReference);
  private final Spinner sReferenceVolume = Spinner
      .getInstance(REFERENCE_VOLUME_LABEL + ": ");
  private final Spinner sYaxisContourModelNumber = Spinner
      .getLabeledInstance(MODEL_LABEL);
  private final RadioButton rbReferenceFile = new RadioButton(
      REFERENCE_FILE_LABEL, bgReference);
  private final LabeledSpinner lsParticlePerCPU = new LabeledSpinner(
      "Particles per CPU: ",
      new SpinnerNumberModel(MatlabParam.PARTICLE_PER_CPU_DEFAULT,
          MatlabParam.PARTICLE_PER_CPU_MIN, MatlabParam.PARTICLE_PER_CPU_MAX, 1));
  private final IterationTable iterationTable;
  private final ButtonGroup bgYaxisType = new ButtonGroup();
  private final RadioButton rbYaxisTypeYAxis = new RadioButton(
      "Original Y axis", MatlabParam.YaxisType.Y_AXIS, bgYaxisType);
  private final RadioButton rbYaxisTypeParticleModel = new RadioButton(
      "Particle model points", MatlabParam.YaxisType.PARTICLE_MODEL,
      bgYaxisType);
  private final RadioButton rbYaxisTypeContour = new RadioButton(
      "End points of contour:  ", MatlabParam.YaxisType.CONTOUR, bgYaxisType);

  private final ButtonGroup bgSampleSphere = new ButtonGroup();
  private final RadioButton rbSampleSphereNone = new RadioButton("None",
      MatlabParam.SampleSphere.NONE, bgSampleSphere);
  private final RadioButton rbSampleSphereFull = new RadioButton("Full sphere",
      MatlabParam.SampleSphere.FULL, bgSampleSphere);
  private final RadioButton rbSampleSphereHalf = new RadioButton("Half sphere",
      MatlabParam.SampleSphere.HALF, bgSampleSphere);
  private final LabeledTextField ltfSampleInterval = new LabeledTextField(
      "Sample interval: ");

  private final ButtonGroup bgMaskType = new ButtonGroup();
  private final RadioButton rbMaskTypeNone = new RadioButton("None",
      MatlabParam.MaskType.NONE, bgMaskType);
  private final RadioButton rbMaskTypeVolume = new RadioButton(
      MASK_TYPE_VOLUME_LABEL, MatlabParam.MaskType.VOLUME, bgMaskType);
  private final RadioButton rbMaskTypeSphere = new RadioButton("Sphere",
      MatlabParam.MaskType.SPHERE, bgMaskType);
  private final RadioButton rbMaskTypeCylinder = new RadioButton("Cylinder",
      MatlabParam.MaskType.CYLINDER, bgMaskType);
  private final FileTextField ftfMaskTypeVolume = new FileTextField(
      MASK_TYPE_VOLUME_LABEL + ": ");
  private final Spinner sMaskModelPtsVolumeModelNumber = Spinner
      .getLabeledInstance(MODEL_LABEL);
  private final LabeledTextField ltfMaskModelPtsVolumeParticle = new LabeledTextField(
      PARTICLE_LABEL);
  private final LabeledTextField ltfInsideMaskRadius = new LabeledTextField(
      "Inner: ");
  private final LabeledTextField ltfOutsideMaskRadius = new LabeledTextField(
      "Outer: ");
  private final CheckBox cbMaskUseReferenceParticle = new CheckBox(
      "Set cylinder orientation from reference particle");

  private final ButtonGroup bgInitMotl = new ButtonGroup();
  private final RadioButton rbInitMotlZero = new RadioButton(
      "Set all rotational values to zero", MatlabParam.InitMotlCode.ZERO,
      bgInitMotl);
  private final RadioButton rbInitMotlZAxis = new RadioButton(
      "Initialize Z axis", MatlabParam.InitMotlCode.Z_AXIS, bgInitMotl);
  private final RadioButton rbInitMotlXAndZAxis = new RadioButton(
      "Initialize X and Z axis", MatlabParam.InitMotlCode.X_AND_Z_AXIS,
      bgInitMotl);
  private final RadioButton rbInitMotlFiles = new RadioButton("Use files",
      bgInitMotl);
  private final ButtonGroup bgCcMode = new ButtonGroup();
  private final RadioButton rbCcModeNormalized = new RadioButton(
      "Local energy normalized cross correlation",
      MatlabParam.CCMode.NORMALIZED, bgCcMode);
  private final RadioButton rbCcModeLocal = new RadioButton(
      "True local correlation coefficent", MatlabParam.CCMode.LOCAL, bgCcMode);
  private final LabeledSpinner lsDebugLevel = new LabeledSpinner(
      "Debug level: ", new SpinnerNumberModel(MatlabParam.DEBUG_LEVEL_DEFAULT,
          MatlabParam.DEBUG_LEVEL_MIN, MatlabParam.DEBUG_LEVEL_MAX, 1));
  private final MultiLineButton btnImportMatlabParamFile = new MultiLineButton(
      "Import a .prm File");
  private final Run3dmodButton btnAvgVol = Run3dmodButton.get3dmodInstance(
      "Open Averaged Volumes in 3dmod", this);
  private final JPanel pnlInitMotl = new JPanel();
  private final TabbedPane tabPane = new TabbedPane();
  private final SpacedPanel pnlSetup = new SpacedPanel();
  private final JPanel pnlRun = new JPanel();
  private final SpacedPanel pnlYaxisType = new SpacedPanel();
  private final JPanel pnlCcMode = new JPanel();
  private final Run3dmodButton btnRef = Run3dmodButton.get3dmodInstance(
      "Open Reference Files in 3dmod", this);
  private final MultiLineButton btnDuplicateProject = new MultiLineButton(
      "Duplicate an Existing Project");
  private final MultiLineButton btnCopyParameters = new MultiLineButton(
      "Copy Parameters");
  private final CheckBox cbFlgWedgeWeight = new CheckBox(
      "Use tilt range in alignment");
  private final Spinner sNWeightGroup = Spinner.getLabeledInstance(
      "# of weight groups for equalizing CCCs: ",
      MatlabParam.N_WEIGHT_GROUP_DEFAULT, MatlabParam.N_WEIGHT_GROUP_DEFAULT,
      20);

  private final PanelHeader phRun;
  private final PanelHeader phSetup;
  private final VolumeTable volumeTable;
  private final PeetManager manager;
  private final AxisID axisID;

  private PeetDialog(final PeetManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
    ftfReferenceFile.setFieldWidth(UIParameters.INSTANCE.getFileWidth());
    ftfMaskTypeVolume.setFieldWidth(UIParameters.INSTANCE.getFileWidth());
    phSetup = PanelHeader.getInstance("Setup", this, DIALOG_TYPE);
    phRun = PanelHeader.getAdvancedBasicInstance("Run", this, DIALOG_TYPE);
    volumeTable = VolumeTable.getInstance(manager, this);
    iterationTable = IterationTable.getInstance(manager);
    //panels
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new EtchedBorder("PEET").getBorder());
    rootPanel.add(tabPane);
    createSetupPanel();
    createRunPanel();
    tabPane.add("Setup", pnlSetup.getContainer());
    tabPane.add("Run", pnlRun);
    tabPane.addMouseListener(new GenericMouseAdapter(this));
    changeTab();
    setDefaults();
    updateDisplay();
    updateAdvanceRunParameters(phRun.isAdvanced());
    setTooltipText();
  }

  public static PeetDialog getInstance(final PeetManager manager,
      final AxisID axisID) {
    PeetDialog instance = new PeetDialog(manager, axisID);
    instance.addListeners();
    return instance;
  }

  public void updateDisplay(final boolean paramFileSet) {
    ftfDirectory.setEditable(!paramFileSet);
    btnImportMatlabParamFile.setEnabled(!paramFileSet);
    btnDuplicateProject.setEnabled(!paramFileSet);
    btnCopyParameters.setEnabled(!paramFileSet);
    ltfFnOutput.setEditable(!paramFileSet);
    btnRun.setEnabled(paramFileSet);
  }

  public Container getContainer() {
    return rootPanel;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Processchunks", "3dmod" };
    String[] manPage = { "processchunks.html", "3dmod.html" };
    String[] logFileLabel = { "Prm", "Start" };
    String[] logFile = new String[2];
    logFile[0] = ltfFnOutput.getText() + ".prm.log";
    logFile[1] = ltfFnOutput.getText() + "-start.log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
    /* "PEET", ContextPopup.TOMO_GUIDE,*/manPagelabel, manPage, logFileLabel,
        logFile, manager, axisID);
  }

  public DialogType getDialogType() {
    return DIALOG_TYPE;
  }

  public void getParameters(final ParallelParam param) {
    ProcesschunksParam processchunksParam = (ProcesschunksParam) param;
  }

  public void getParameters(final PeetScreenState screenState) {
    phSetup.getState(screenState.getPeetSetupHeaderState());
    phRun.getState(screenState.getPeetRunHeaderState());
  }

  public void setParameters(final ConstPeetScreenState screenState) {
    phSetup.setState(screenState.getPeetSetupHeaderState());
    phRun.setState(screenState.getPeetRunHeaderState());
  }

  public void getParameters(final PeetMetaData metaData) {
    volumeTable.getParameters(metaData);
    metaData.setReferenceVolume(sReferenceVolume.getValue());
    metaData.setReferenceParticle(ltfReferenceParticle.getText());
    metaData.setReferenceFile(ftfReferenceFile.getText());
    metaData.setEdgeShift(ltfEdgeShift.getText());
    metaData.setYaxisContourModelNumber(sYaxisContourModelNumber.getValue());
    metaData.setYaxisContourObjectNumber(ltfYaxisContourObjectNumber.getText());
    metaData.setYaxisContourContourNumber(ltfYaxisContourContourNumber
        .getText());
    metaData.setFlgWedgeWeight(cbFlgWedgeWeight.isSelected());
    metaData.setMaskUseReferenceParticle(cbMaskUseReferenceParticle
        .isSelected());
    metaData.setMaskModelPtsModelNumber(sMaskModelPtsVolumeModelNumber
        .getValue());
    metaData.setMaskModelPtsParticle(ltfMaskModelPtsVolumeParticle.getText());
    metaData.setMaskTypeVolume(ftfMaskTypeVolume.getText());
    metaData.setNWeightGroup(sNWeightGroup.getValue());
  }

  /**
   * Set parameters from metaData and then overwrite them with parameters from
   * MatlabParamFile.  This allows inactive data to appear on the screen but
   * allows MatlabParamFile's active data to override active metaData.  So if
   * the user changes the .prm file, the active data on the screen will be
   * correct.
   * @param metaData
   */
  public void setParameters(final ConstPeetMetaData metaData,
      boolean parametersOnly) {
    ltfFnOutput.setText(metaData.getName());
    if (!parametersOnly) {
      volumeTable.setParameters(metaData);
      ftfReferenceFile.setText(metaData.getReferenceFile());
      sReferenceVolume.setValue(metaData.getReferenceVolume());
      ltfReferenceParticle.setText(metaData.getReferenceParticle());
      sYaxisContourModelNumber.setValue(metaData.getYaxisContourModelNumber());
      ltfYaxisContourObjectNumber.setText(metaData
          .getYaxisContourObjectNumber());
      ltfYaxisContourContourNumber.setText(metaData
          .getYaxisContourContourNumber());
      cbFlgWedgeWeight.setSelected(metaData.isFlgWedgeWeight());
    }
    ltfEdgeShift.setText(metaData.getEdgeShift());
    cbMaskUseReferenceParticle.setSelected(metaData
        .isMaskUseReferenceParticle());
    sMaskModelPtsVolumeModelNumber.setValue(metaData
        .getMaskModelPtsModelNumber());
    ltfMaskModelPtsVolumeParticle.setText(metaData.getMaskModelPtsParticle());
    ftfMaskTypeVolume.setText(metaData.getMaskTypeVolume());
    sNWeightGroup.setValue(metaData.getNWeightGroup());
  }

  /**
   * Load data from MatlabParamFile.  Load only active data after the meta data
   * has been loaded.  Do not load fnOutput.  This value cannot be modified after
   * the dataset has been created.
   * Do not rely on whether fields are enabled to make decisions in this
   * function; updateDisplay may not have been run with data in the screen.
   * When looking at the settings of dialog fields, make sure that they have
   * already been loaded in THIS function.  This is sometimes the first data-
   * loading function to be run.
   * @param matlabParamFile
   * @param importDir directory of original .prm file.  May need to set the absolute path of files from .prm file
   * @param paramatersOnly 
   */
  public void setParameters(final MatlabParam matlabParam, File importDir,
      boolean parametersOnly) {
    iterationTable.setParameters(matlabParam);
    if (!parametersOnly) {
      if (matlabParam.useReferenceFile()) {
        rbReferenceFile.setSelected(true);
        ftfReferenceFile.setText(matlabParam.getReferenceFile());
      }
      else {
        rbReferenceParticle.setSelected(true);
        sReferenceVolume.setValue(matlabParam.getReferenceVolume());
        ltfReferenceParticle.setText(matlabParam.getReferenceParticle());
      }
    }
    MatlabParam.InitMotlCode initMotlCode = matlabParam.getInitMotlCode();
    if (initMotlCode == null) {
      rbInitMotlFiles.setSelected(true);
    }
    else if (initMotlCode == MatlabParam.InitMotlCode.ZERO) {
      rbInitMotlZero.setSelected(true);
    }
    else if (initMotlCode == MatlabParam.InitMotlCode.Z_AXIS) {
      rbInitMotlZAxis.setSelected(true);
    }
    else if (initMotlCode == MatlabParam.InitMotlCode.X_AND_Z_AXIS) {
      rbInitMotlXAndZAxis.setSelected(true);
    }
    cbTiltRange.setSelected(matlabParam.useTiltRange());
    if (cbTiltRange.isSelected()) {
      ltfEdgeShift.setText(matlabParam.getEdgeShift());
      cbFlgWedgeWeight.setSelected(matlabParam.isFlgWedgeWeight());
    }
    ltfSzVolX.setText(matlabParam.getSzVolX());
    ltfSzVolY.setText(matlabParam.getSzVolY());
    ltfSzVolZ.setText(matlabParam.getSzVolZ());
    MatlabParam.CCMode ccMode = matlabParam.getCcMode();
    if (ccMode == MatlabParam.CCMode.NORMALIZED) {
      rbCcModeNormalized.setSelected(true);
    }
    else if (ccMode == MatlabParam.CCMode.LOCAL) {
      rbCcModeLocal.setSelected(true);
    }
    cbFlgMeanFill.setSelected(matlabParam.isFlgMeanFill());
    ltfAlignedBaseName.setText(matlabParam.getAlignedBaseName());
    ltfLowCutoff.setText(matlabParam.getLowCutoff());
    lsDebugLevel.setValue(matlabParam.getDebugLevel());
    ltfLstThresholdsStart.setText(matlabParam.getLstThresholdsStart());
    ltfLstThresholdsIncrement.setText(matlabParam.getLstThresholdsIncrement());
    ltfLstThresholdsEnd.setText(matlabParam.getLstThresholdsEnd());
    ltfLstThresholdsAdditional
        .setText(matlabParam.getLstThresholdsAdditional());
    cbRefFlagAllTom.setSelected(!matlabParam.isRefFlagAllTom());
    cbLstFlagAllTom.setSelected(!matlabParam.isLstFlagAllTom());
    lsParticlePerCPU.setValue(matlabParam.getParticlePerCPU());
    MatlabParam.YaxisType yaxisType = matlabParam.getYaxisType();
    if (yaxisType == MatlabParam.YaxisType.Y_AXIS) {
      rbYaxisTypeYAxis.setSelected(true);
    }
    else if (yaxisType == MatlabParam.YaxisType.PARTICLE_MODEL) {
      rbYaxisTypeParticleModel.setSelected(true);
    }
    else if (yaxisType == MatlabParam.YaxisType.CONTOUR) {
      rbYaxisTypeContour.setSelected(true);
      if (!parametersOnly) {
        sYaxisContourModelNumber.setValue(matlabParam
            .getYaxisContourModelNumber());
        ltfYaxisContourObjectNumber.setText(matlabParam
            .getYaxisContourObjectNumber());
        ltfYaxisContourContourNumber.setText(matlabParam
            .getYaxisContourContourNumber());
      }
    }
    if (!parametersOnly) {
      volumeTable.setParameters(matlabParam, rbInitMotlFiles.isSelected(),
          cbTiltRange.isSelected(), importDir);
    }
    MatlabParam.SampleSphere sampleSphere = matlabParam.getSampleSphere();
    if (sampleSphere == MatlabParam.SampleSphere.NONE) {
      rbSampleSphereNone.setSelected(true);
    }
    else if (sampleSphere == MatlabParam.SampleSphere.FULL) {
      rbSampleSphereFull.setSelected(true);
    }
    else if (sampleSphere == MatlabParam.SampleSphere.HALF) {
      rbSampleSphereHalf.setSelected(true);
    }
    ltfSampleInterval.setText(matlabParam.getSampleInterval());

    String maskTypeValue = matlabParam.getMaskType();
    MatlabParam.MaskType maskType = MatlabParam.MaskType
        .getInstance(maskTypeValue);
    if (maskType == MatlabParam.MaskType.NONE) {
      rbMaskTypeNone.setSelected(true);
    }
    else if (maskType == MatlabParam.MaskType.VOLUME) {
      rbMaskTypeVolume.setSelected(true);
      ftfMaskTypeVolume.setText(maskTypeValue);
    }
    else if (maskType == MatlabParam.MaskType.SPHERE) {
      rbMaskTypeSphere.setSelected(true);
    }
    else if (maskType == MatlabParam.MaskType.CYLINDER) {
      rbMaskTypeCylinder.setSelected(true);
    }
    if (!matlabParam.isMaskModelPtsEmpty()) {
      sMaskModelPtsVolumeModelNumber.setValue(matlabParam
          .getMaskModelPtsVolume());
      ltfMaskModelPtsVolumeParticle.setText(matlabParam
          .getMaskModelPtsParticle());
    }
    ltfInsideMaskRadius.setText(matlabParam.getInsideMaskRadius());
    ltfOutsideMaskRadius.setText(matlabParam.getOutsideMaskRadius());
    if (isEnableNWeightGroup()) {
      sNWeightGroup.setValue(matlabParam.getNWeightGroup());
    }
    updateDisplay();
  }

  public void getParameters(final MatlabParam matlabParam) {
    matlabParam.clear();
    volumeTable.getParameters(matlabParam);
    iterationTable.getParameters(matlabParam);
    matlabParam.setFnOutput(ltfFnOutput.getText());
    if (rbReferenceParticle.isSelected()) {
      matlabParam.setReferenceVolume(sReferenceVolume.getValue());
      matlabParam.setReferenceParticle(ltfReferenceParticle.getText());
    }
    else if (rbReferenceFile.isSelected()) {
      matlabParam.setReferenceFile(ftfReferenceFile.getText());
    }
    matlabParam.setInitMotlCode(((RadioButton.RadioButtonModel) bgInitMotl
        .getSelection()).getEnumeratedType());
    matlabParam.setTiltRangeEmpty(!cbTiltRange.isSelected());
    if (ltfEdgeShift.isEnabled()) {
      matlabParam.setEdgeShift(ltfEdgeShift.getText());
    }
    if (cbFlgWedgeWeight.isEnabled()) {
      matlabParam.setFlgWedgeWeight(cbFlgWedgeWeight.isSelected());
    }
    matlabParam.setSzVolX(ltfSzVolX.getText());
    matlabParam.setSzVolY(ltfSzVolY.getText());
    matlabParam.setSzVolZ(ltfSzVolZ.getText());
    matlabParam.setCcMode(((RadioButton.RadioButtonModel) bgCcMode
        .getSelection()).getEnumeratedType());
    matlabParam.setFlgMeanFill(cbFlgMeanFill.isSelected());
    matlabParam.setAlignedBaseName(ltfAlignedBaseName.getText());
    matlabParam.setLowCutoff(ltfLowCutoff.getText());
    matlabParam.setDebugLevel(lsDebugLevel.getValue());
    matlabParam.setLstThresholdsStart(ltfLstThresholdsStart.getText());
    matlabParam.setLstThresholdsIncrement(ltfLstThresholdsIncrement.getText());
    matlabParam.setLstThresholdsEnd(ltfLstThresholdsEnd.getText());
    matlabParam
        .setLstThresholdsAdditional(ltfLstThresholdsAdditional.getText());
    matlabParam.setRefFlagAllTom(!cbRefFlagAllTom.isSelected());
    matlabParam.setLstFlagAllTom(!cbLstFlagAllTom.isSelected());
    matlabParam.setParticlePerCPU(lsParticlePerCPU.getValue());
    matlabParam.setYaxisType(((RadioButton.RadioButtonModel) bgYaxisType
        .getSelection()).getEnumeratedType());
    if (rbYaxisTypeContour.isSelected()) {
      matlabParam.setYaxisContourModelNumber(sYaxisContourModelNumber
          .getValue());
      matlabParam.setYaxisContourObjectNumber(ltfYaxisContourObjectNumber
          .getText());
      matlabParam.setYaxisContourContourNumber(ltfYaxisContourContourNumber
          .getText());
    }
    matlabParam.setSampleSphere(((RadioButton.RadioButtonModel) bgSampleSphere
        .getSelection()).getEnumeratedType());
    matlabParam.setSampleInterval(ltfSampleInterval.getText());

    if (rbMaskTypeVolume.isSelected()) {
      matlabParam.setMaskType(ftfMaskTypeVolume.getText());
    }
    else {
      matlabParam.setMaskType(((RadioButton.RadioButtonModel) bgMaskType
          .getSelection()).getEnumeratedType());
    }
    if (rbMaskTypeCylinder.isSelected()
        && (rbReferenceFile.isSelected() || !cbMaskUseReferenceParticle
            .isSelected())) {
      matlabParam.setMaskModelPtsVolume(sMaskModelPtsVolumeModelNumber
          .getValue());
      matlabParam.setMaskModelPtsParticle(ltfMaskModelPtsVolumeParticle
          .getText());
    }
    else {
      matlabParam.clearMaskModelPts();
    }
    matlabParam.setInsideMaskRadius(ltfInsideMaskRadius.getText());
    matlabParam.setOutsideMaskRadius(ltfOutsideMaskRadius.getText());
    if (sNWeightGroup.isEnabled()) {
      matlabParam.setUseNWeightGroup(true);
      matlabParam.setNWeightGroup(sNWeightGroup.getValue());
    }
    else {
      matlabParam.setUseNWeightGroup(false);
    }
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
    else if (phRun.equalsAdvancedBasic(button)) {
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

  /**
   * Reset values and set defaults.
   */
  public void reset() {
    cbTiltRange.setSelected(false);
    ltfReferenceParticle.clear();
    ftfReferenceFile.clear();
    ltfSzVolX.clear();
    ltfSzVolY.clear();
    ltfSzVolZ.clear();
    ltfEdgeShift.clear();
    cbFlgMeanFill.setSelected(false);
    ltfAlignedBaseName.clear();
    ltfLowCutoff.clear();
    cbRefFlagAllTom.setSelected(false);
    ltfLstThresholdsStart.clear();
    ltfLstThresholdsIncrement.clear();
    ltfLstThresholdsEnd.clear();
    ltfLstThresholdsAdditional.clear();
    ltfYaxisContourObjectNumber.clear();
    ltfYaxisContourContourNumber.clear();
    cbLstFlagAllTom.setSelected(false);
    rbReferenceParticle.setSelected(false);
    sReferenceVolume.reset();
    sYaxisContourModelNumber.reset();
    rbReferenceFile.setSelected(false);
    rbYaxisTypeYAxis.setSelected(false);
    rbYaxisTypeParticleModel.setSelected(false);
    rbYaxisTypeContour.setSelected(false);
    rbInitMotlZero.setSelected(false);
    rbInitMotlZAxis.setSelected(false);
    rbInitMotlXAndZAxis.setSelected(false);
    rbInitMotlFiles.setSelected(false);
    rbCcModeNormalized.setSelected(false);
    rbCcModeLocal.setSelected(false);
    cbFlgWedgeWeight.setSelected(false);
    volumeTable.reset();
    iterationTable.reset();
    ltfSampleInterval.clear();
    cbMaskUseReferenceParticle.setSelected(false);
    sNWeightGroup.reset();
    setDefaults();
    updateDisplay();
  }

  void msgVolumeTableSizeChanged() {
    updateDisplay();
  }

  void setUsingInitMotlFile() {
    rbInitMotlFiles.setSelected(true);
  }

  private void setTooltipText() {
    ftfDirectory
        .setToolTipText("The directory which will contain the .prm file, .epe file, other data files, intermediate files, and results.  "
            + "Only one .epe file per directory.");
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory
          .getInstance(AutodocFactory.PEET_PRM);
      pnlInitMotl.setToolTipText(TooltipFormatter.INSTANCE.format(EtomoAutodoc
          .getTooltip(autodoc, MatlabParam.INIT_MOTL_KEY)));
      ReadOnlySection section = autodoc.getSection(
          EtomoAutodoc.FIELD_SECTION_NAME, MatlabParam.INIT_MOTL_KEY);
      rbInitMotlZero.setToolTipText(section);
      rbInitMotlXAndZAxis.setToolTipText(section);
      rbInitMotlZAxis.setToolTipText(section);
      rbInitMotlFiles.setToolTipText(section);
      String tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.TILT_RANGE_KEY);
      cbTiltRange.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.REFERENCE_KEY);
      rbReferenceParticle.setToolTipText(tooltip);
      rbReferenceFile.setToolTipText(tooltip);
      sReferenceVolume.setToolTipText(tooltip);
      ltfReferenceParticle.setToolTipText(tooltip);
      ftfReferenceFile.setToolTipText(tooltip);
      ltfEdgeShift.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.EDGE_SHIFT_KEY));
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.SZ_VOL_KEY);
      ltfSzVolX.setToolTipText(tooltip);
      ltfSzVolY.setToolTipText(tooltip);
      ltfSzVolZ.setToolTipText(tooltip);
      pnlCcMode.setToolTipText(TooltipFormatter.INSTANCE.format(EtomoAutodoc
          .getTooltip(autodoc, MatlabParam.CC_MODE_KEY)));
      section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
          MatlabParam.CC_MODE_KEY);
      rbCcModeNormalized.setToolTipText(section);
      rbCcModeLocal.setToolTipText(section);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.FLG_MEAN_FILL_KEY);
      cbFlgMeanFill.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.DEBUG_LEVEL_KEY);
      lsDebugLevel.setToolTipText(tooltip);
      ltfLowCutoff.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.LOW_CUTOFF_KEY));
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.REF_FLAG_ALL_TOM_KEY);
      cbRefFlagAllTom.setToolTipText(tooltip);
      tooltip = EtomoAutodoc
          .getTooltip(autodoc, MatlabParam.LST_THRESHOLDS_KEY)
          + "  The list can be either a list descriptor ("
          + LST_THRESHOLD_START_TITLE
          + ":"
          + LST_THRESHOLD_INCREMENT_TITLE
          + ":"
          + LST_THRESHOLD_END_TITLE
          + "), a simple list ("
          + LST_THRESHOLD_ADDITIONAL_NUMBERS_TITLE
          + "), or a combination of the two.";
      ltfLstThresholdsStart.setToolTipText(tooltip);
      ltfLstThresholdsIncrement.setToolTipText(tooltip);
      ltfLstThresholdsEnd.setToolTipText(tooltip);
      ltfLstThresholdsAdditional.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.LST_FLAG_ALL_TOM_KEY);
      cbLstFlagAllTom.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.PARTICLE_PER_CPU_KEY);
      lsParticlePerCPU.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.ALIGNED_BASE_NAME_KEY);
      ltfAlignedBaseName.setToolTipText(tooltip);
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.FN_OUTPUT_KEY);
      ltfFnOutput.setToolTipText(tooltip);
      section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
          MatlabParam.YAXIS_TYPE_KEY);
      rbYaxisTypeYAxis.setToolTipText(section);
      rbYaxisTypeParticleModel.setToolTipText(section);
      tooltip = rbYaxisTypeContour.setToolTipText(section);
      sYaxisContourModelNumber.setToolTipText(tooltip);
      ltfYaxisContourObjectNumber.setToolTipText(tooltip);
      ltfYaxisContourContourNumber.setToolTipText(tooltip);
      btnImportMatlabParamFile
          .setToolTipText("Create a new PEET project from a .prm file.");
      btnDuplicateProject
          .setToolTipText("Create a new PEET project from .epe and .prm files in another directory.");
      btnCopyParameters
          .setToolTipText("Create a new PEET project and copy the parameters (everything but the volume table) from .epe and/or .prm file(s) in another directory.");
      cbFlgWedgeWeight.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.FLG_WEDGE_WEIGHT_KEY));
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.SAMPLE_SPHERE_KEY);
      rbSampleSphereNone.setToolTipText(tooltip);
      rbSampleSphereFull.setToolTipText(tooltip);
      rbSampleSphereHalf.setToolTipText(tooltip);
      ltfSampleInterval.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.SAMPLE_INTERVAL_KEY));
      tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.MASK_TYPE_KEY);
      rbMaskTypeNone.setToolTipText(tooltip);
      rbMaskTypeVolume.setToolTipText(tooltip);
      rbMaskTypeSphere.setToolTipText(tooltip);
      rbMaskTypeCylinder.setToolTipText(tooltip);
      ftfMaskTypeVolume.setToolTipText(tooltip);
      tooltip = EtomoAutodoc
          .getTooltip(autodoc, MatlabParam.MASK_MODEL_PTS_KEY);
      sMaskModelPtsVolumeModelNumber.setToolTipText(tooltip);
      ltfMaskModelPtsVolumeParticle.setToolTipText(tooltip);
      cbMaskUseReferenceParticle.setToolTipText(tooltip);
      ltfInsideMaskRadius.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.INSIDE_MASK_RADIUS_KEY));
      ltfOutsideMaskRadius.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.OUTSIDE_MASK_RADIUS_KEY));
      sNWeightGroup.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.N_WEIGHT_GROUP_KEY));
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

  private void updateAdvanceRunParameters(boolean advanced) {
    pnlAdvanced.setVisible(advanced);
  }

  private void setDefaults() {
    lsDebugLevel.setValue(MatlabParam.DEBUG_LEVEL_DEFAULT);
    ltfEdgeShift.setText(MatlabParam.EDGE_SHIFT_DEFAULT);
    cbFlgMeanFill.setSelected(MatlabParam.FLG_MEAN_FILL_DEFAULT);
    ltfLowCutoff.setText(MatlabParam.LOW_CUTOFF_DEFAULT);
    if (MatlabParam.REFERENCE_FILE_DEFAULT) {
      rbReferenceFile.setSelected(true);
    }
    else {
      rbReferenceParticle.setSelected(true);
    }
    rbSampleSphereNone.setSelected(true);
    lsParticlePerCPU.setValue(MatlabParam.PARTICLE_PER_CPU_DEFAULT);
    rbMaskTypeNone.setSelected(true);
    sNWeightGroup.setValue(MatlabParam.N_WEIGHT_GROUP_DEFAULT);
  }

  private void createSetupPanel() {
    //project
    JPanel pnlProject = new JPanel();
    pnlProject.setLayout(new BoxLayout(pnlProject, BoxLayout.X_AXIS));
    pnlProject.add(ftfDirectory.getContainer());
    pnlProject.add(ltfFnOutput.getContainer());
    //use existing project
    JPanel pnlUseExistingProject = new JPanel();
    pnlUseExistingProject.setLayout(new BoxLayout(pnlUseExistingProject,
        BoxLayout.X_AXIS));
    pnlUseExistingProject.setBorder(new EtchedBorder("Use Existing Project")
        .getBorder());
    btnImportMatlabParamFile.setSize();
    pnlUseExistingProject.add(btnImportMatlabParamFile.getComponent());
    btnDuplicateProject.setSize();
    pnlUseExistingProject.add(btnDuplicateProject.getComponent());
    btnCopyParameters.setSize();
    pnlUseExistingProject.add(btnCopyParameters.getComponent());
    //volume reference
    JPanel pnlVolumeReference = new JPanel();
    pnlVolumeReference.setLayout(new BoxLayout(pnlVolumeReference,
        BoxLayout.X_AXIS));
    pnlVolumeReference.add(rbReferenceParticle.getComponent());
    pnlVolumeReference.add(sReferenceVolume.getContainer());
    pnlVolumeReference.add(ltfReferenceParticle.getContainer());
    //volume file
    JPanel pnlVolumeFile = new JPanel();
    pnlVolumeFile.setLayout(new BoxLayout(pnlVolumeFile, BoxLayout.X_AXIS));
    pnlVolumeFile.add(rbReferenceFile.getComponent());
    pnlVolumeFile.add(ftfReferenceFile.getContainer());
    //reference
    JPanel pnlReference = new JPanel();
    pnlReference.setLayout(new BoxLayout(pnlReference, BoxLayout.Y_AXIS));
    pnlReference.setBorder(new EtchedBorder("Reference").getBorder());
    pnlReference.add(pnlVolumeReference);
    pnlReference.add(pnlVolumeFile);
    //tiltRange and edgeShift
    SpacedPanel pnlTiltRange = new SpacedPanel();
    pnlTiltRange.setBoxLayout(BoxLayout.X_AXIS);
    pnlTiltRange.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlTiltRange.add(cbTiltRange);
    pnlTiltRange.addRigidArea(FixedDim.x20_y0);
    ltfEdgeShift.setTextPreferredWidth(UIParameters.INSTANCE.getIntegerWidth());
    pnlTiltRange.add(ltfEdgeShift.getContainer());
    //flgWedgeWeight
    SpacedPanel pnlFlgWedgeWeight = new SpacedPanel();
    pnlFlgWedgeWeight.setBoxLayout(BoxLayout.X_AXIS);
    pnlFlgWedgeWeight.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlFlgWedgeWeight.add(cbFlgWedgeWeight);
    pnlFlgWedgeWeight.addHorizontalGlue();
    //missing wedge compensation
    SpacedPanel pnlMissingWedgeCompensation = new SpacedPanel();
    pnlMissingWedgeCompensation.setBoxLayout(BoxLayout.Y_AXIS);
    pnlMissingWedgeCompensation.setBorder(new EtchedBorder(
        "Missing Wedge Compensation").getBorder());
    pnlMissingWedgeCompensation
        .setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlMissingWedgeCompensation.add(pnlTiltRange);
    pnlMissingWedgeCompensation.add(pnlFlgWedgeWeight);
    pnlMissingWedgeCompensation.add(sNWeightGroup.getContainer());
    //reference and missing wedge compensation
    JPanel pnlReferenceAndMissingWedgeCompensation = new JPanel();
    pnlReferenceAndMissingWedgeCompensation.setLayout(new BoxLayout(
        pnlReferenceAndMissingWedgeCompensation, BoxLayout.X_AXIS));
    pnlReferenceAndMissingWedgeCompensation.add(pnlReference);
    pnlReferenceAndMissingWedgeCompensation.add(Box
        .createRigidArea(FixedDim.x20_y0));
    pnlReferenceAndMissingWedgeCompensation.add(pnlMissingWedgeCompensation
        .getContainer());
    //mask type
    JPanel pnlMaskType = new JPanel();
    pnlMaskType.setLayout(new BoxLayout(pnlMaskType, BoxLayout.Y_AXIS));
    pnlMaskType.add(rbMaskTypeNone.getComponent());
    pnlMaskType.add(rbMaskTypeVolume.getComponent());
    pnlMaskType.add(rbMaskTypeSphere.getComponent());
    pnlMaskType.add(rbMaskTypeCylinder.getComponent());
    JPanel pnlMaskRadii = new JPanel();
    pnlMaskRadii.setLayout(new BoxLayout(pnlMaskRadii, BoxLayout.X_AXIS));
    pnlMaskRadii.setBorder(new EtchedBorder("Radii of Sphere or Cylinder")
        .getBorder());
    pnlMaskRadii.add(ltfInsideMaskRadius.getContainer());
    pnlMaskRadii.add(ltfOutsideMaskRadius.getContainer());
    //mask type volume and sphere details
    JPanel pnlMaskVolumeRadii = new JPanel();
    pnlMaskVolumeRadii.setLayout(new BoxLayout(pnlMaskVolumeRadii,
        BoxLayout.Y_AXIS));
    pnlMaskVolumeRadii.add(ftfMaskTypeVolume.getContainer());
    pnlMaskVolumeRadii.add(pnlMaskRadii);
    //use maskModelPts
    JPanel pnlUseMaskModelPts = new JPanel();
    pnlUseMaskModelPts.setLayout(new BoxLayout(pnlUseMaskModelPts,
        BoxLayout.X_AXIS));
    pnlUseMaskModelPts.add(cbMaskUseReferenceParticle);
    //maskModelPts
    JPanel pnlMaskModelPts = new JPanel();
    pnlMaskModelPts.setLayout(new BoxLayout(pnlMaskModelPts, BoxLayout.X_AXIS));
    pnlMaskModelPts.add(sMaskModelPtsVolumeModelNumber.getContainer());
    pnlMaskModelPts.add(ltfMaskModelPtsVolumeParticle.getContainer());
    //mask cylinder details
    JPanel pnlMaskCylinder = new JPanel();
    pnlMaskCylinder.setLayout(new BoxLayout(pnlMaskCylinder, BoxLayout.Y_AXIS));
    pnlMaskCylinder.setBorder(new EtchedBorder("Cylinder Orientation")
        .getBorder());
    pnlMaskCylinder.add(pnlUseMaskModelPts);
    pnlMaskCylinder.add(pnlMaskModelPts);
    //mask
    JPanel pnlMask = new JPanel();
    pnlMask.setLayout(new BoxLayout(pnlMask, BoxLayout.X_AXIS));
    pnlMask.setBorder(new EtchedBorder("Masking").getBorder());
    pnlMask.add(pnlMaskType);
    pnlMask.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlMask.add(pnlMaskVolumeRadii);
    pnlMask.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlMask.add(pnlMaskCylinder);
    //init MOTL
    pnlInitMotl.setLayout(new BoxLayout(pnlInitMotl, BoxLayout.Y_AXIS));
    pnlInitMotl.setBorder(new EtchedBorder("Initial Motive List").getBorder());
    pnlInitMotl.add(rbInitMotlZero.getComponent());
    pnlInitMotl.add(rbInitMotlZAxis.getComponent());
    pnlInitMotl.add(rbInitMotlXAndZAxis.getComponent());
    pnlInitMotl.add(rbInitMotlFiles.getComponent());
    //YaxisContour
    JPanel pnlYaxisContour = new JPanel();
    pnlYaxisContour.setLayout(new BoxLayout(pnlYaxisContour, BoxLayout.X_AXIS));
    pnlYaxisContour.add(rbYaxisTypeContour.getComponent());
    pnlYaxisContour.add(sYaxisContourModelNumber.getContainer());
    ltfYaxisContourObjectNumber.setTextPreferredWidth(UIParameters.INSTANCE
        .getIntegerWidth());
    pnlYaxisContour.add(ltfYaxisContourObjectNumber.getContainer());
    ltfYaxisContourContourNumber.setTextPreferredWidth(UIParameters.INSTANCE
        .getIntegerWidth());
    pnlYaxisContour.add(ltfYaxisContourContourNumber.getContainer());
    //YaxisType
    pnlYaxisType.setBoxLayout(BoxLayout.Y_AXIS);
    pnlYaxisType.setBorder(new EtchedBorder("Y Axis Type").getBorder());
    pnlYaxisType.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlYaxisType.add(rbYaxisTypeYAxis);
    pnlYaxisType.add(rbYaxisTypeParticleModel);
    pnlYaxisType.add(pnlYaxisContour);
    //init MOTL and Y axis type
    JPanel pnlInitMotlAndYAxisType = new JPanel();
    pnlInitMotlAndYAxisType.setLayout(new BoxLayout(pnlInitMotlAndYAxisType,
        BoxLayout.X_AXIS));
    pnlInitMotlAndYAxisType.add(pnlInitMotl);
    pnlInitMotlAndYAxisType.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlInitMotlAndYAxisType.add(pnlYaxisType.getContainer());
    //body
    pnlSetupBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetupBody.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlSetupBody.add(pnlProject);
    pnlSetupBody.add(pnlUseExistingProject);
    pnlSetupBody.add(volumeTable.getContainer());
    pnlSetupBody.add(pnlReferenceAndMissingWedgeCompensation);
    pnlSetupBody.add(pnlMask);
    pnlSetupBody.add(pnlInitMotlAndYAxisType);
    //main panel
    pnlSetup.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetup.setBorder(BorderFactory.createEtchedBorder());
    pnlSetup.add(phSetup.getContainer());
  }

  private void createRunPanel() {
    //Spherical Sampling
    JPanel pnlSphericalSampling = new JPanel();
    pnlSphericalSampling.setLayout(new BoxLayout(pnlSphericalSampling,
        BoxLayout.X_AXIS));
    pnlSphericalSampling.setBorder(new EtchedBorder(
        "Spherical Sampling for Theta and Psi").getBorder());
    pnlSphericalSampling.add(rbSampleSphereNone.getComponent());
    pnlSphericalSampling.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSphericalSampling.add(rbSampleSphereFull.getComponent());
    pnlSphericalSampling.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSphericalSampling.add(rbSampleSphereHalf.getComponent());
    pnlSphericalSampling.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSphericalSampling.add(ltfSampleInterval.getContainer());
    pnlSphericalSampling.add(Box.createHorizontalGlue());
    //szVol
    SpacedPanel pnlSzVol = new SpacedPanel();
    pnlSzVol.setBoxLayout(BoxLayout.X_AXIS);
    pnlSzVol.add(ltfSzVolX.getContainer());
    pnlSzVol.add(ltfSzVolY.getContainer());
    pnlSzVol.add(ltfSzVolZ.getContainer());
    //lstThresholds
    SpacedPanel pnlLstThresholds = new SpacedPanel();
    pnlLstThresholds.setBoxLayout(BoxLayout.X_AXIS);
    pnlLstThresholds.setBorder(new EtchedBorder(
        "Number of Particles in Averages").getBorder());
    pnlLstThresholds.add(ltfLstThresholdsStart.getContainer());
    pnlLstThresholds.add(ltfLstThresholdsIncrement.getContainer());
    pnlLstThresholds.add(ltfLstThresholdsEnd.getContainer());
    pnlLstThresholds.add(ltfLstThresholdsAdditional.getContainer());
    //CCMode
    pnlCcMode.setLayout(new BoxLayout(pnlCcMode, BoxLayout.Y_AXIS));
    pnlCcMode.setBorder(new EtchedBorder("Cross correlation measure")
        .getBorder());
    pnlCcMode.add(rbCcModeNormalized.getComponent());
    pnlCcMode.add(rbCcModeLocal.getComponent());
    //ParticlePerCPU
    JPanel pnlParticlePerCPU = new JPanel();
    pnlParticlePerCPU.setLayout(new BoxLayout(pnlParticlePerCPU,
        BoxLayout.X_AXIS));
    pnlParticlePerCPU.add(Box.createRigidArea(FixedDim.x200_y0));
    pnlParticlePerCPU.add(lsParticlePerCPU.getContainer());
    pnlParticlePerCPU.add(Box.createRigidArea(FixedDim.x200_y0));
    //advanced right panel
    JPanel pnlAdvancedRight = new JPanel();
    pnlAdvancedRight
        .setLayout(new BoxLayout(pnlAdvancedRight, BoxLayout.Y_AXIS));
    pnlAdvancedRight.add(cbFlgMeanFill);
    pnlAdvancedRight.add(ltfAlignedBaseName.getContainer());
    pnlAdvancedRight.add(ltfLowCutoff.getContainer());
    pnlAdvancedRight.add(lsDebugLevel.getContainer());
    //advanced panel
    pnlAdvanced.setLayout(new BoxLayout(pnlAdvanced, BoxLayout.X_AXIS));
    pnlAdvanced.add(pnlCcMode);
    pnlAdvanced.add(Box.createRigidArea(FixedDim.x40_y0));
    pnlAdvanced.add(pnlAdvancedRight);
    //button panel
    JPanel pnlButton = new JPanel();
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    btnRun.setSize();
    pnlButton.add(btnRun.getComponent());
    btnAvgVol.setSize();
    pnlButton.add(btnAvgVol.getComponent());
    btnRef.setSize();
    pnlButton.add(btnRef.getComponent());
    //body
    pnlRunBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRunBody.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRunBody.add(iterationTable.getContainer());
    pnlRunBody.add(pnlSphericalSampling);
    pnlRunBody.add(pnlSzVol);
    pnlRunBody.add(cbRefFlagAllTom);
    pnlRunBody.add(pnlLstThresholds);
    pnlRunBody.add(cbLstFlagAllTom);
    pnlRunBody.add(pnlParticlePerCPU);
    pnlRunBody.add(pnlAdvanced);
    pnlRunBody.add(pnlButton);
    //main panel
    pnlRun.setLayout(new BoxLayout(pnlRun, BoxLayout.Y_AXIS));
    pnlRun.setBorder(BorderFactory.createEtchedBorder());
    pnlRun.add(phRun.getContainer());
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), run3dmodMenuOptions);
  }

  private void action(final String actionCommand,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (actionCommand.equals(ftfDirectory.getActionCommand())) {
      chooseDirectory();
    }
    else if (actionCommand.equals(btnRun.getActionCommand())) {
      if (validateRun()) {
        manager.peetParser(null, DIALOG_TYPE);
      }
    }
    else if (actionCommand.equals(btnImportMatlabParamFile.getActionCommand())) {
      importMatlabParam();
    }
    else if (actionCommand.equals(btnDuplicateProject.getActionCommand())) {
      duplicateExistingProject();
    }
    else if (actionCommand.equals(btnCopyParameters.getActionCommand())) {
      copyParameters();
    }
    else if (actionCommand.equals(rbInitMotlZero.getActionCommand())
        || actionCommand.equals(rbInitMotlZAxis.getActionCommand())
        || actionCommand.equals(rbInitMotlXAndZAxis.getActionCommand())
        || actionCommand.equals(rbInitMotlFiles.getActionCommand())
        || actionCommand.equals(rbReferenceParticle.getActionCommand())
        || actionCommand.equals(rbReferenceFile.getActionCommand())
        || actionCommand.equals(cbTiltRange.getActionCommand())
        || actionCommand.equals(rbYaxisTypeYAxis.getActionCommand())
        || actionCommand.equals(rbYaxisTypeParticleModel.getActionCommand())
        || actionCommand.equals(rbYaxisTypeContour.getActionCommand())
        || actionCommand.equals(cbFlgWedgeWeight.getActionCommand())
        || actionCommand.equals(rbSampleSphereNone.getActionCommand())
        || actionCommand.equals(rbSampleSphereFull.getActionCommand())
        || actionCommand.equals(rbSampleSphereHalf.getActionCommand())
        || actionCommand.equals(rbMaskTypeNone.getActionCommand())
        || actionCommand.equals(rbMaskTypeVolume.getActionCommand())
        || actionCommand.equals(rbMaskTypeSphere.getActionCommand())
        || actionCommand.equals(rbMaskTypeCylinder.getActionCommand())
        || actionCommand.equals(cbMaskUseReferenceParticle.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(btnAvgVol.getActionCommand())) {
      manager.imodAvgVol(run3dmodMenuOptions);
    }
    else if (actionCommand.equals(btnRef.getActionCommand())) {
      manager.imodRef(run3dmodMenuOptions);
    }
  }

  private boolean validateRun() {
    if (!volumeTable.validateRun()) {
      return false;
    }
    return iterationTable.validateRun();
  }

  /**
   * Create a project out of a matlab param file.
   */
  private void importMatlabParam() {
    String path = ftfDirectory.getText();
    if (path == null || path.matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + "field before importing a .prm file.",
          "Entry Error");
      return;
    }
    File dir = new File(ftfDirectory.getText());
    if (!dir.exists()) {
      UIHarness.INSTANCE.openMessageDialog("Please create "
          + dir.getAbsolutePath() + " before importing a .prm file.",
          "Entry Error");
      return;
    }
    File matlabParamFile = null;
    JFileChooser chooser = new JFileChooser(dir);
    chooser.setFileFilter(new MatlabParamFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    matlabParamFile = chooser.getSelectedFile();
    manager.loadMatlabParam(matlabParamFile, false);
  }

  /**
   * Create a project out of a peet file from another directory.
   */
  private void duplicateExistingProject() {
    String path = ftfDirectory.getText();
    if (path == null || path.matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + "field before importing a .prm file.",
          "Entry Error");
      return;
    }
    File dir = new File(ftfDirectory.getText());
    if (!dir.exists()) {
      UIHarness.INSTANCE.openMessageDialog("Please create "
          + dir.getAbsolutePath() + " before importing a .prm file.",
          "Entry Error");
      return;
    }
    JFileChooser chooser = new JFileChooser(dir);
    chooser.setFileFilter(new PeetFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    File peetFile = chooser.getSelectedFile();
    manager.loadParamFile(peetFile, false);
  }

  /**
   * Create a project out of a peet file or a .prm file from another directory.
   * Copy everything but the volume table
   */
  private void copyParameters() {
    String path = ftfDirectory.getText();
    if (path == null || path.matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + "field before copying parameters.",
          "Entry Error");
      return;
    }
    File dir = new File(ftfDirectory.getText());
    if (!dir.exists()) {
      UIHarness.INSTANCE.openMessageDialog("Please create "
          + dir.getAbsolutePath() + " before copy parameters.", "Entry Error");
      return;
    }
    JFileChooser chooser = new JFileChooser(dir);
    chooser.setFileFilter(new PeetAndMatlabParamFileFilter());
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal != JFileChooser.APPROVE_OPTION) {
      return;
    }
    File file = chooser.getSelectedFile();
    manager.copyParameters(file);
  }

  private void changeTab() {
    if (tabPane.getSelectedIndex() == 0) {
      pnlSetup.add(pnlSetupBody);
      pnlRun.remove(pnlRunBody.getContainer());
    }
    else {
      pnlRun.add(pnlRunBody.getContainer());
      pnlSetup.remove(pnlSetupBody);
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  private void referenceFileAction(ActionEvent action) {
    String actionCommand = action.getActionCommand();
    if (actionCommand.equals(ftfReferenceFile.getActionCommand())) {
      chooseReferenceFile(ftfReferenceFile);
    }
    else if (actionCommand.equals(ftfMaskTypeVolume.getActionCommand())) {
      chooseReferenceFile(ftfMaskTypeVolume);
    }
  }

  private void updateDisplay() {
    //tilt range
    ltfEdgeShift.setEnabled(cbTiltRange.isSelected());
    cbFlgWedgeWeight.setEnabled(cbTiltRange.isSelected());
    rbCcModeNormalized.setEnabled(!cbFlgWedgeWeight.isSelected());
    if (cbFlgWedgeWeight.isSelected()) {
      rbCcModeLocal.setSelected(true);
    }
    int size = volumeTable.size();
    //reference
    boolean volumeRows = size > 0;
    rbReferenceParticle.setEnabled(volumeRows);
    sReferenceVolume.setEnabled(volumeRows && rbReferenceParticle.isSelected());
    sReferenceVolume.setMax(size);
    ltfReferenceParticle.setEnabled(volumeRows
        && rbReferenceParticle.isSelected());
    ftfReferenceFile.setEnabled(volumeRows && rbReferenceFile.isSelected());
    //yaxisType and yaxisContour
    rbYaxisTypeContour.setEnabled(volumeRows);
    sYaxisContourModelNumber.setEnabled(volumeRows
        && rbYaxisTypeContour.isSelected());
    sYaxisContourModelNumber.setMax(size);
    ltfYaxisContourObjectNumber.setEnabled(volumeRows
        && rbYaxisTypeContour.isSelected());
    ltfYaxisContourContourNumber.setEnabled(volumeRows
        && rbYaxisTypeContour.isSelected());
    //spherical sampling
    ltfSampleInterval.setEnabled(!rbSampleSphereNone.isSelected());
    iterationTable.updateDisplay(!rbSampleSphereNone.isSelected());
    //volume table
    volumeTable.updateDisplay(rbInitMotlFiles.isSelected(), cbTiltRange
        .isSelected());
    //mask
    ftfMaskTypeVolume.setEnabled(rbMaskTypeVolume.isSelected());
    boolean sphere = rbMaskTypeSphere.isSelected();
    boolean cylinder = rbMaskTypeCylinder.isSelected();
    ltfInsideMaskRadius.setEnabled(sphere || cylinder);
    ltfOutsideMaskRadius.setEnabled(sphere || cylinder);
    cbMaskUseReferenceParticle.setEnabled(cylinder
        && rbReferenceParticle.isSelected());
    boolean useReferenceParticle = cbMaskUseReferenceParticle.isSelected()
        && cbMaskUseReferenceParticle.isEnabled();
    sMaskModelPtsVolumeModelNumber
        .setEnabled(cylinder && !useReferenceParticle);
    sMaskModelPtsVolumeModelNumber.setMax(size);
    ltfMaskModelPtsVolumeParticle.setEnabled(cylinder && !useReferenceParticle);
    sNWeightGroup.setEnabled(isEnableNWeightGroup());
  }

  private boolean isEnableNWeightGroup() {
    return volumeTable.size() > 0 && cbTiltRange.isSelected()
        && cbFlgWedgeWeight.isSelected() && rbReferenceParticle.isSelected();
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

  private void chooseReferenceFile(FileTextField fileTextField) {
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      fileTextField.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }

  private void addListeners() {
    ReferenceFileActionListener rfActionListener = new ReferenceFileActionListener(
        this);
    ftfReferenceFile.addActionListener(rfActionListener);
    ftfMaskTypeVolume.addActionListener(rfActionListener);
    PDActionListener actionListener = new PDActionListener(this);
    ftfDirectory.addActionListener(actionListener);
    rbInitMotlZero.addActionListener(actionListener);
    rbInitMotlZAxis.addActionListener(actionListener);
    rbInitMotlXAndZAxis.addActionListener(actionListener);
    rbInitMotlFiles.addActionListener(actionListener);
    rbReferenceParticle.addActionListener(actionListener);
    rbReferenceFile.addActionListener(actionListener);
    cbTiltRange.addActionListener(actionListener);
    btnRun.addActionListener(actionListener);
    tabPane.addChangeListener(new TabChangeListener(this));
    rbYaxisTypeYAxis.addActionListener(actionListener);
    rbYaxisTypeParticleModel.addActionListener(actionListener);
    rbYaxisTypeContour.addActionListener(actionListener);
    btnImportMatlabParamFile.addActionListener(actionListener);
    btnAvgVol.addActionListener(actionListener);
    btnRef.addActionListener(actionListener);
    btnDuplicateProject.addActionListener(actionListener);
    btnCopyParameters.addActionListener(actionListener);
    cbFlgWedgeWeight.addActionListener(actionListener);
    rbSampleSphereNone.addActionListener(actionListener);
    rbSampleSphereFull.addActionListener(actionListener);
    rbSampleSphereHalf.addActionListener(actionListener);
    rbMaskTypeNone.addActionListener(actionListener);
    rbMaskTypeVolume.addActionListener(actionListener);
    rbMaskTypeSphere.addActionListener(actionListener);
    rbMaskTypeCylinder.addActionListener(actionListener);
    cbMaskUseReferenceParticle.addActionListener(actionListener);
  }

  private static final class PDActionListener implements ActionListener {
    private final PeetDialog peetDialog;

    private PDActionListener(final PeetDialog peetDialog) {
      this.peetDialog = peetDialog;
    }

    public void actionPerformed(final ActionEvent event) {
      peetDialog.action(event.getActionCommand(), null);
    }
  }

  private static final class ReferenceFileActionListener implements
      ActionListener {
    private final PeetDialog peetDialog;

    private ReferenceFileActionListener(final PeetDialog peetDialog) {
      this.peetDialog = peetDialog;
    }

    public void actionPerformed(final ActionEvent event) {
      peetDialog.referenceFileAction(event);
    }
  }

  private static final class TabChangeListener implements ChangeListener {
    private final PeetDialog peetDialog;

    public TabChangeListener(final PeetDialog peetDialog) {
      this.peetDialog = peetDialog;
    }

    public void stateChanged(final ChangeEvent changeEvent) {
      peetDialog.changeTab();
    }
  }
}
