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
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.PeetManager;
import etomo.comscript.AverageAllParam;
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
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
import etomo.util.Goodframe;
import etomo.util.InvalidParameterException;
import etomo.util.Utilities;

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
 * <p> Revision 1.88  2009/12/02 04:41:49  sueh
 * <p> Factored out RadiiOfSphereOrCylinder panel.
 * <p>
 * <p> Revision 1.87  2009/12/02 00:01:23  sueh
 * <p> bug# 1290 Removed cbMaskUseReferenceParticle.
 * <p>
 * <p> Revision 1.86  2009/12/01 00:25:48  sueh
 * <p> bug# 1285 Factored MissingWedgeCompensation out of PeetDialog.
 * <p>
 * <p> Revision 1.85  2009/11/23 23:29:21  sueh
 * <p> bug# 1292 Removing model #.  Attaching object and contour to
 * <p> yaxisObjectNum and yaxisContourNum instead of yaxisContour.
 * <p>
 * <p> Revision 1.84  2009/11/20 17:30:41  sueh
 * <p> bug# 1282 Naming all the file choosers by constructing a FileChooser
 * <p> instance instead of a JFileChooser instance.  Factored out ReferencePanel.
 * <p> Added flgRemoveDuplicates.
 * <p>
 * <p> Revision 1.83  2009/11/04 20:56:54  sueh
 * <p> bug# 1242 Added optional standard menu iterm PEET user guide.  Added
 * <p> tooltips to buttons.
 * <p>
 * <p> Revision 1.82  2009/10/29 12:03:57  sueh
 * <p> bug# 1245 In setParameters only set ftfMaskTypeVolume if
 * <p> parametersOnly is off.
 * <p>
 * <p> Revision 1.81  2009/10/19 21:07:34  sueh
 * <p> bug# 1263 Calling updateParallelProcess from changeTab.  In
 * <p> usingParallelProcessing take the current tab into account.  Added
 * <p> updateParallelProcess.
 * <p>
 * <p> Revision 1.80  2009/10/16 23:56:08  sueh
 * <p> bug# 1234 In validateRun, passed cbTiltRange and cbFlgWedgeWeight
 * <p> selection state to VolumeTable.validateRun.
 * <p>
 * <p> Revision 1.79  2009/10/16 21:13:55  sueh
 * <p> Reformatted some comments.
 * <p>
 * <p> Revision 1.78  2009/10/15 23:38:53  sueh
 * <p> bug# 1274 Added validations.  Factored out UseExistingProjectPanel.
 * <p>
 * <p> Revision 1.77  2009/09/20 21:33:28  sueh
 * <p> bug# 1268 Added timestamp and dialog identification to log.
 * <p>
 * <p> Revision 1.76  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.75  2009/04/27 18:02:36  sueh
 * <p> bug# 1211 Implemented FileContainer.  Moved last location functionality
 * <p> from VolumeTable to PeetDialog.  Added checkIncorrectPaths,
 * <p> fixIncorrectPaths, and fixIncorrectPath.  Added correctPath.
 * <p>
 * <p> Revision 1.74  2009/04/20 16:39:43  sueh
 * <p> bug# 1214 In validateRun don't run goodframe if all of the szVol values
 * <p> are empty.  Allow szVol to be empty.
 * <p>
 * <p> Revision 1.73  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.72  2009/02/26 17:26:44  sueh
 * <p> bug# 1184 Validating sxVol by passing the values to goodframe.
 * <p>
 * <p> Revision 1.71  2009/02/06 03:11:57  sueh
 * <p> bug# 1168 Adding most error checking to validateRun.
 * <p>
 * <p> Revision 1.70  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.69  2009/01/20 20:19:01  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name
 * <p> themselves.
 * <p>
 * <p> Revision 1.68  2009/01/13 19:39:36  sueh
 * <p> bug# 1170 Added cbNWeightGroup so that nWeightGroup can be enabled
 * <p> independently of flgWedgeWeight.  Getting the minimum of the nWeightGroup
 * <p> spinner from MatlabParam.  Saving cbNWeightGroup in metadata, so that its
 * <p> setting is not lost when it is disabled.  CbNWeightGroup enables
 * <p> sNWeightGroup.
 * <p>
 * <p> Revision 1.67  2008/10/10 20:43:24  sueh
 * <p> bug# 1142 Clear tiltRange when tiltRange check box is unchecked.
 * <p>
 * <p> Revision 1.66  2008/10/01 22:51:52  sueh
 * <p> bug# 1113 Added getFocusComponent and GetSetupJComponent.
 * <p>
 * <p> Revision 1.65  2008/09/30 22:02:15  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 1.64  2008/09/10 21:35:31  sueh
 * <p> bug# 1135 Check for null when calling ParsedElementList.get(int).  Check
 * <p> for null when calling ParsedElement.getElement or getRawNumber.  Set
 * <p> cbTiltRange from meta data.  Then, for backwards compatibility, turn it
 * <p> on if the .prm file tiltRange is not empty.
 * <p>
 * <p> Revision 1.63  2008/09/05 20:53:16  sueh
 * <p> bug# 1136 In getParameters(MatlabParam) setting useNWeightGroup.
 * <p>
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
 * <p> bug# 1041 Passing the ProcessName to processchunks instead of setting it
 * <p> in getParameters because it is required and has been added to the
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
    Expandable, Run3dmodButtonContainer, FileContainer,
    UseExistingProjectParent, ReferenceParent, MissingWedgeCompensationParent,
    IterationParent, MaskingParent, YAxisTypeParent,
    SphericalSamplingForThetaAndPsiParent {
  public static final String rcsid = "$Id$";

  public static final String FN_OUTPUT_LABEL = "Root name for output";
  public static final String DIRECTORY_LABEL = "Directory";

  private static final DialogType DIALOG_TYPE = DialogType.PEET;
  private static final String LST_THRESHOLD_START_TITLE = "Start";
  private static final String LST_THRESHOLD_INCREMENT_TITLE = "Incr.";
  private static final String LST_THRESHOLD_END_TITLE = "End";
  private static final String LST_THRESHOLD_ADDITIONAL_NUMBERS_TITLE = "Additional numbers";
  private static final String PARTICLE_VOLUME_LABEL = "Particle volume";
  private static final String X_LABEL = "X";
  private static final String Y_LABEL = "Y";
  private static final String Z_LABEL = "Z";
  private static final String LST_THRESHOLDS_LABEL = "Number of Particles in Averages";

  private final EtomoPanel rootPanel = new EtomoPanel();
  private final FileTextField ftfDirectory = new FileTextField(DIRECTORY_LABEL
      + ": ");
  private final LabeledTextField ltfFnOutput = new LabeledTextField(
      FN_OUTPUT_LABEL + ": ");
  private final SpacedPanel pnlSetupBody = SpacedPanel.getInstance();
  private final LabeledTextField ltfSzVolX = new LabeledTextField(
      PARTICLE_VOLUME_LABEL + " " + X_LABEL + ": ");
  private final LabeledTextField ltfSzVolY = new LabeledTextField(Y_LABEL
      + ": ");
  private final LabeledTextField ltfSzVolZ = new LabeledTextField(Z_LABEL
      + ": ");
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
  private final CheckBox cbLstFlagAllTom = new CheckBox(
      "Use equal numbers of particles from all tomograms for averages");
  private final SpacedPanel pnlRunBody = SpacedPanel.getInstance(true);
  private final MultiLineButton btnRun = new MultiLineButton("Run");
  private final JPanel pnlAdvanced = new JPanel();
  private final LabeledSpinner lsParticlePerCPU = new LabeledSpinner(
      "Particles per CPU: ",
      new SpinnerNumberModel(MatlabParam.PARTICLE_PER_CPU_DEFAULT,
          MatlabParam.PARTICLE_PER_CPU_MIN, MatlabParam.PARTICLE_PER_CPU_MAX, 1),
      MatlabParam.PARTICLE_PER_CPU_DEFAULT);
  private final IterationTable iterationTable;
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
          MatlabParam.DEBUG_LEVEL_MIN, MatlabParam.DEBUG_LEVEL_MAX, 1),
      MatlabParam.DEBUG_LEVEL_DEFAULT);
  private final Run3dmodButton btnAvgVol = Run3dmodButton.get3dmodInstance(
      "Open Averaged Volumes in 3dmod", this);
  private final EtomoPanel pnlInitMotl = new EtomoPanel();
  private final TabbedPane tabPane = new TabbedPane();
  private final SpacedPanel pnlSetup = SpacedPanel.getInstance();
  private final EtomoPanel pnlRun = new EtomoPanel();
  private final EtomoPanel pnlCcMode = new EtomoPanel();
  private final Run3dmodButton btnRef = Run3dmodButton.get3dmodInstance(
      "Open Reference Files in 3dmod", this);
  private final CheckBox cbFlgRemoveDuplicates = new CheckBox(
      "Remove duplicates");
  private final MultiLineButton btnAverageAll = new MultiLineButton(
      "Remake Averaged Volumes");
  private final SphericalSamplingForThetaAndPsiPanel sphericalSamplingForThetaAndPsiPanel;
  private final YAxisTypePanel yAxisTypePanel;
  private final MaskingPanel maskingPanel;
  private final MissingWedgeCompensationPanel missingWedgeCompensationPanel;
  private final ReferencePanel referencePanel;
  private final UseExistingProjectPanel useExistingProjectPanel;
  private final PanelHeader phRun;
  private final PanelHeader phSetup;
  private final VolumeTable volumeTable;
  private final PeetManager manager;
  private final AxisID axisID;
  private final FixPathsPanel fixPathsPanel;

  private File lastLocation = null;
  private String correctPath = null;

  private PeetDialog(final PeetManager manager, final AxisID axisID) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: "
        + DialogType.PEET);
    this.manager = manager;
    this.axisID = axisID;
    useExistingProjectPanel = UseExistingProjectPanel
        .getInstance(manager, this);
    referencePanel = ReferencePanel.getInstance(this, manager);
    missingWedgeCompensationPanel = MissingWedgeCompensationPanel.getInstance(
        this, manager);
    maskingPanel = MaskingPanel.getInstance(manager, this);
    yAxisTypePanel = YAxisTypePanel.getInstance(manager, this);
    fixPathsPanel = FixPathsPanel.getInstance(this, manager, axisID,
        DIALOG_TYPE);
    sphericalSamplingForThetaAndPsiPanel = SphericalSamplingForThetaAndPsiPanel
        .getInstance(manager, this);
    phSetup = PanelHeader.getInstance("Setup", this, DIALOG_TYPE);
    phRun = PanelHeader.getAdvancedBasicInstance("Run", this, DIALOG_TYPE);
    volumeTable = VolumeTable.getInstance(manager, this);
    iterationTable = IterationTable.getInstance(manager, this);
    //panels
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("PEET").getBorder());
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

  public Component getFocusComponent() {
    return pnlSetup.getContainer();
  }

  public JComponent getSetupJComponent() {
    return pnlSetup.getJPanel();
  }

  public static PeetDialog getInstance(final PeetManager manager,
      final AxisID axisID) {
    PeetDialog instance = new PeetDialog(manager, axisID);
    instance.addListeners();
    return instance;
  }

  /**
   * Toggles between a setup-like mode where the location and root name being
   * chosen, and a regular mode.
   * @param paramFileSet
   */
  public void updateDisplay(final boolean paramFileSet) {
    ftfDirectory.setEditable(!paramFileSet);
    useExistingProjectPanel.updateDisplay(paramFileSet);
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
        manPagelabel, manPage, logFileLabel, logFile, true, manager, axisID);
  }

  public DialogType getDialogType() {
    return DIALOG_TYPE;
  }

  public void checkIncorrectPaths() {
    boolean incorrectPaths = false;
    if (volumeTable.isIncorrectPaths()) {
      incorrectPaths = true;
    }
    else if (referencePanel.isIncorrectPaths()) {
      incorrectPaths = true;
    }
    else if (maskingPanel.isIncorrectPaths()) {
      incorrectPaths = true;
    }
    fixPathsPanel.setIncorrectPaths(incorrectPaths);
  }

  /**
   * Fix all of the incorrect paths until the user cancels a file chooser.
   */
  public void fixIncorrectPaths(boolean choosePathEveryRow) {
    if (!volumeTable.fixIncorrectPaths(choosePathEveryRow)) {
      return;
    }
    if (!referencePanel.fixIncorrectPaths(choosePathEveryRow)) {
      return;
    }
    if (!maskingPanel.fixIncorrectPaths(choosePathEveryRow)) {
      return;
    }
    checkIncorrectPaths();
  }

  /**
   * Fix an incorrect path.
   * @param fileTextField
   * @param choosePathEveryRow
   * @return false if the user cancels the file selector, otherwise true
   */
  public boolean fixIncorrectPath(FileTextField fileTextField,
      boolean choosePath) {
    File newFile = null;
    while (newFile == null || !newFile.exists()) {
      //Have the user choose the location of the file if they haven't chosen
      //before or they want to choose most of the files individuallly, otherwise
      //just use the current correctPath.
      if (correctPath == null || choosePath
          || (newFile != null && !newFile.exists())) {
        JFileChooser fileChooser = getFileChooserInstance();
        fileChooser.setSelectedFile(fileTextField.getFile());
        fileChooser.setPreferredSize(UIParameters.INSTANCE
            .getFileChooserDimension());
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        int returnVal = fileChooser.showOpenDialog(rootPanel);
        if (returnVal != JFileChooser.APPROVE_OPTION) {
          return false;
        }
        newFile = fileChooser.getSelectedFile();
        if (newFile != null && newFile.exists()) {
          correctPath = newFile.getParent();
          lastLocation = new File(correctPath);
          fileTextField.setFile(newFile);
        }
      }
      else if (correctPath != null) {
        newFile = new File(correctPath, fileTextField.getFile().getName());
        if (newFile.exists()) {
          fileTextField.setFile(newFile);
        }
      }
    }
    return true;
  }

  JFileChooser getFileChooserInstance() {
    return new FileChooser(lastLocation == null ? new File(manager
        .getPropertyUserDir()) : lastLocation);
  }

  void setLastLocation(File input) {
    lastLocation = input;
  }

  boolean isCorrectPathNull() {
    return correctPath == null;
  }

  void setCorrectPath(String correctPath) {
    this.correctPath = correctPath;
    lastLocation = new File(correctPath);
  }

  String getCorrectPath() {
    return correctPath;
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
    referencePanel.getParameters(metaData);
    missingWedgeCompensationPanel.getParameters(metaData);
    maskingPanel.getParameters(metaData);
  }

  public void getParameters(final AverageAllParam param) {
    param.setIterationNumber(iterationTable.size());
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
    volumeTable.setParameters(metaData, parametersOnly);
    referencePanel.setParameters(metaData, parametersOnly);
    missingWedgeCompensationPanel.setParameters(metaData, parametersOnly);
    maskingPanel.setParameters(metaData, parametersOnly);
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
    referencePanel.setParameters(matlabParam, parametersOnly);
    missingWedgeCompensationPanel.setParameters(matlabParam);
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
    yAxisTypePanel.setParameters(matlabParam, parametersOnly);
    volumeTable.setParameters(matlabParam, parametersOnly, rbInitMotlFiles
        .isSelected(), missingWedgeCompensationPanel.isTiltRangeSelected(),
        importDir);
    sphericalSamplingForThetaAndPsiPanel.setParameters(matlabParam);
    maskingPanel.setParameters(matlabParam, parametersOnly);
    cbFlgRemoveDuplicates.setSelected(matlabParam.isFlgRemoveDuplicates());
    updateDisplay();
  }

  public void getParameters(final MatlabParam matlabParam) {
    matlabParam.clear();
    volumeTable.getParameters(matlabParam);
    iterationTable.getParameters(matlabParam);
    matlabParam.setFnOutput(ltfFnOutput.getText());
    referencePanel.getParameters(matlabParam);
    missingWedgeCompensationPanel.getParameters(matlabParam);
    matlabParam.setInitMotlCode(((RadioButton.RadioButtonModel) bgInitMotl
        .getSelection()).getEnumeratedType());
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
    yAxisTypePanel.getParameters(matlabParam);
    sphericalSamplingForThetaAndPsiPanel.getParameters(matlabParam);
    maskingPanel.getParameters(matlabParam);
    matlabParam.setFlgRemoveDuplicates(cbFlgRemoveDuplicates.isSelected());
  }

  public boolean isReferenceFileSelected() {
    return referencePanel.isReferenceFileSelected();
  }

  public boolean isVolumeTableEmpty() {
    return volumeTable.isEmpty();
  }

  public boolean isReferenceParticleSelected() {
    return referencePanel.isReferenceParticleSelected();
  }

  public String getFnOutput() {
    return ltfFnOutput.getText();
  }

  /**
   * 
   */
  public boolean usingParallelProcessing() {
    return tabPane.getSelectedIndex() == 1;
  }

  public void expand(final GlobalExpandButton button) {
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

  public FileTextField getDirectory() {
    return ftfDirectory;
  }

  public String getDirectoryString() {
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
    referencePanel.reset();
    missingWedgeCompensationPanel.reset();
    ltfSzVolX.clear();
    ltfSzVolY.clear();
    ltfSzVolZ.clear();
    cbFlgMeanFill.setSelected(false);
    ltfAlignedBaseName.clear();
    ltfLowCutoff.clear();
    cbRefFlagAllTom.setSelected(false);
    ltfLstThresholdsStart.clear();
    ltfLstThresholdsIncrement.clear();
    ltfLstThresholdsEnd.clear();
    ltfLstThresholdsAdditional.clear();
    yAxisTypePanel.reset();
    cbLstFlagAllTom.setSelected(false);
    rbInitMotlZero.setSelected(false);
    rbInitMotlZAxis.setSelected(false);
    rbInitMotlXAndZAxis.setSelected(false);
    rbInitMotlFiles.setSelected(false);
    rbCcModeNormalized.setSelected(false);
    rbCcModeLocal.setSelected(false);
    volumeTable.reset();
    iterationTable.reset();
    sphericalSamplingForThetaAndPsiPanel.reset();
    cbFlgRemoveDuplicates.setSelected(false);
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
        .setToolTipText("The directory which will contain the .prm file, .epe "
            + "file, other data files, intermediate files, and results.  Only "
            + "one .epe file per directory.");
    btnRun.setToolTipText("Run prmParser with processchunks.");
    btnAvgVol.setToolTipText("Open all of the computed averages in 3dmod.");
    btnRef.setToolTipText("Open the references in 3dmod.");
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
          AutodocFactory.PEET_PRM, manager.getManagerKey());
      pnlInitMotl.setToolTipText(TooltipFormatter.INSTANCE.format(EtomoAutodoc
          .getTooltip(autodoc, MatlabParam.INIT_MOTL_KEY)));
      ReadOnlySection section = autodoc.getSection(
          EtomoAutodoc.FIELD_SECTION_NAME, MatlabParam.INIT_MOTL_KEY);
      rbInitMotlZero.setToolTipText(section);
      rbInitMotlXAndZAxis.setToolTipText(section);
      rbInitMotlZAxis.setToolTipText(section);
      rbInitMotlFiles.setToolTipText(section);
      String tooltip = EtomoAutodoc.getTooltip(autodoc, MatlabParam.SZ_VOL_KEY);
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
      cbFlgRemoveDuplicates.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.FLG_REMOVE_DUPLICATES_KEY));
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
  }

  private void updateAdvanceRunParameters(boolean advanced) {
    pnlAdvanced.setVisible(advanced);
  }

  private void setDefaults() {
    lsDebugLevel.setValue(MatlabParam.DEBUG_LEVEL_DEFAULT);
    cbFlgMeanFill.setSelected(MatlabParam.FLG_MEAN_FILL_DEFAULT);
    ltfLowCutoff.setText(MatlabParam.LOW_CUTOFF_DEFAULT);
    referencePanel.setDefaults();
    missingWedgeCompensationPanel.setDefaults();
    sphericalSamplingForThetaAndPsiPanel.setDefaults();
    lsParticlePerCPU.setValue(MatlabParam.PARTICLE_PER_CPU_DEFAULT);
    maskingPanel.setDefaults();
  }

  private void createSetupPanel() {
    //project
    JPanel pnlProject = new JPanel();
    pnlProject.setLayout(new BoxLayout(pnlProject, BoxLayout.X_AXIS));
    pnlProject.add(ftfDirectory.getContainer());
    pnlProject.add(ltfFnOutput.getContainer());
    //reference and missing wedge compensation
    JPanel pnlReferenceAndMissingWedgeCompensation = new JPanel();
    pnlReferenceAndMissingWedgeCompensation.setLayout(new BoxLayout(
        pnlReferenceAndMissingWedgeCompensation, BoxLayout.X_AXIS));
    pnlReferenceAndMissingWedgeCompensation.add(referencePanel.getComponent());
    pnlReferenceAndMissingWedgeCompensation.add(Box
        .createRigidArea(FixedDim.x20_y0));
    pnlReferenceAndMissingWedgeCompensation.add(missingWedgeCompensationPanel
        .getComponent());
    //init MOTL
    pnlInitMotl.setLayout(new BoxLayout(pnlInitMotl, BoxLayout.Y_AXIS));
    pnlInitMotl.setBorder(new EtchedBorder("Initial Motive List").getBorder());
    pnlInitMotl.add(rbInitMotlZero.getComponent());
    pnlInitMotl.add(rbInitMotlZAxis.getComponent());
    pnlInitMotl.add(rbInitMotlXAndZAxis.getComponent());
    pnlInitMotl.add(rbInitMotlFiles.getComponent());
    //init MOTL and Y axis type
    JPanel pnlInitMotlAndYAxisType = new JPanel();
    pnlInitMotlAndYAxisType.setLayout(new BoxLayout(pnlInitMotlAndYAxisType,
        BoxLayout.X_AXIS));
    pnlInitMotlAndYAxisType.add(pnlInitMotl);
    pnlInitMotlAndYAxisType.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlInitMotlAndYAxisType.add(yAxisTypePanel.getComponent());
    //body
    pnlSetupBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetupBody.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlSetupBody.add(pnlProject);
    pnlSetupBody.add(useExistingProjectPanel.getComponent());
    pnlSetupBody.add(fixPathsPanel.getRootComponent());
    pnlSetupBody.add(volumeTable.getContainer());
    pnlSetupBody.add(pnlReferenceAndMissingWedgeCompensation);
    pnlSetupBody.add(maskingPanel.getComponent());
    pnlSetupBody.add(pnlInitMotlAndYAxisType);
    //main panel
    pnlSetup.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetup.setBorder(BorderFactory.createEtchedBorder());
    pnlSetup.add(phSetup);
  }

  private void createRunPanel() {
    //initialize
    btnAverageAll.setSize();
    //szVol
    SpacedPanel pnlSzVol = SpacedPanel.getInstance();
    pnlSzVol.setBoxLayout(BoxLayout.X_AXIS);
    pnlSzVol.add(ltfSzVolX.getContainer());
    pnlSzVol.add(ltfSzVolY.getContainer());
    pnlSzVol.add(ltfSzVolZ.getContainer());
    //lstThresholds
    SpacedPanel pnlLstThresholds = SpacedPanel.getInstance();
    pnlLstThresholds.setBoxLayout(BoxLayout.X_AXIS);
    pnlLstThresholds.setBorder(new EtchedBorder(LST_THRESHOLDS_LABEL)
        .getBorder());
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
    pnlButton.add(btnAverageAll.getComponent());
    //body
    pnlRunBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRunBody.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRunBody.add(iterationTable.getContainer());
    pnlRunBody.add(sphericalSamplingForThetaAndPsiPanel.getComponent());
    pnlRunBody.add(cbFlgRemoveDuplicates);
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
    pnlRun.add(phRun);
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
    else if (actionCommand.equals(rbInitMotlZero.getActionCommand())
        || actionCommand.equals(rbInitMotlZAxis.getActionCommand())
        || actionCommand.equals(rbInitMotlXAndZAxis.getActionCommand())
        || actionCommand.equals(rbInitMotlFiles.getActionCommand())
        || actionCommand.equals(cbFlgRemoveDuplicates.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(btnAvgVol.getActionCommand())) {
      manager.imodAvgVol(run3dmodMenuOptions);
    }
    else if (actionCommand.equals(btnRef.getActionCommand())) {
      manager.imodRef(run3dmodMenuOptions);
    }
    else if (actionCommand.equals(btnAverageAll.getActionCommand())) {
      manager.averageAll(null, DIALOG_TYPE);
    }
  }

  private boolean validateRun() {
    //Setup tab
    //Must have a directory
    if (ftfDirectory.isEmpty()) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + " field.", "Entry Error", manager
          .getManagerKey());
      return false;
    }
    //Must have an output name
    if (ltfFnOutput.isEmpty()) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.FN_OUTPUT_LABEL + " field.", "Entry Error", manager
          .getManagerKey());
      return false;
    }
    //Validate volume table
    String errorMessage = volumeTable.validateRun(missingWedgeCompensationPanel
        .isTiltRangeSelected()
        || missingWedgeCompensationPanel.isFlgWedgeWeightSelected());
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Entry Error", manager
          .getManagerKey());
      return false;
    }
    //Must either have a volume and particle or a reference file.
    errorMessage = referencePanel.validateRun();
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Entry Error", manager
          .getManagerKey());
      return false;
    }
    //Validate missing wedge compensation panel
    errorMessage = missingWedgeCompensationPanel.validateRun();
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Entry Error", manager
          .getManagerKey());
      return false;
    }
    //Validate masking
    errorMessage = maskingPanel.validateRun();
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Entry Error", manager
          .getManagerKey());
      return false;
    }
    //validate Y axis type
    errorMessage = yAxisTypePanel.validateRun();
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Entry Error", manager
          .getManagerKey());
      return false;
    }
    //Run tab
    if (!iterationTable.validateRun()) {
      return false;
    }
    //spherical sampling for theta and psi:
    if (!sphericalSamplingForThetaAndPsiPanel.validateRun()) {
      return false;
    }
    //particle volume
    if (ltfSzVolX.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog("In " + PARTICLE_VOLUME_LABEL + ", "
          + X_LABEL + " is required.", "Entry Error", manager.getManagerKey());
      return false;
    }
    if (ltfSzVolY.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog("In " + PARTICLE_VOLUME_LABEL + ", "
          + Y_LABEL + " is required.", "Entry Error", manager.getManagerKey());
      return false;
    }
    if (ltfSzVolZ.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog("In " + PARTICLE_VOLUME_LABEL + ", "
          + Z_LABEL + " is required.", "Entry Error", manager.getManagerKey());
      return false;
    }
    Goodframe goodframe = new Goodframe(manager.getPropertyUserDir(),
        AxisID.FIRST, manager.getManagerKey());
    try {
      goodframe.run(new String[] { ltfSzVolX.getText(), ltfSzVolY.getText(),
          ltfSzVolZ.getText() });
      if (!goodframe.getOutput(0).equals(ltfSzVolX.getText())) {
        UIHarness.INSTANCE.openMessageDialog("In " + PARTICLE_VOLUME_LABEL
            + ", " + X_LABEL + " is invalid.  Try " + goodframe.getOutput(0)
            + ".", "Entry Error", manager.getManagerKey());
        return false;
      }
      if (!goodframe.getOutput(1).equals(ltfSzVolY.getText())) {
        UIHarness.INSTANCE.openMessageDialog("In " + PARTICLE_VOLUME_LABEL
            + ", " + Y_LABEL + " is invalid.  Try " + goodframe.getOutput(1)
            + ".", "Entry Error", manager.getManagerKey());
        return false;
      }
      if (!goodframe.getOutput(2).equals(ltfSzVolZ.getText())) {
        UIHarness.INSTANCE.openMessageDialog("In " + PARTICLE_VOLUME_LABEL
            + ", " + Z_LABEL + " is invalid.  Try " + goodframe.getOutput(2)
            + ".", "Entry Error", manager.getManagerKey());
        return false;
      }
    }
    catch (IOException e) {
      if (!UIHarness.INSTANCE.openYesNoDialog("Unable to validate "
          + PARTICLE_VOLUME_LABEL + ".  Continue?\n\n" + e.getMessage(),
          AxisID.ONLY, manager.getManagerKey())) {
        return false;
      }
    }
    catch (InvalidParameterException e) {
      if (!UIHarness.INSTANCE.openYesNoDialog("Unable to validate "
          + PARTICLE_VOLUME_LABEL + ".  Continue?\n\n" + e.getMessage(),
          AxisID.ONLY, manager.getManagerKey())) {
        return false;
      }
    }
    catch (NumberFormatException e) {
      if (!UIHarness.INSTANCE.openYesNoDialog("Unable to validate "
          + PARTICLE_VOLUME_LABEL + ".  Continue?\n\n" + e.getMessage(),
          AxisID.ONLY, manager.getManagerKey())) {
        return false;
      }
    }
    //Number of particles
    boolean startIsEmpty = ltfLstThresholdsStart.isEmpty();
    boolean incrementIsEmpty = ltfLstThresholdsIncrement.isEmpty();
    boolean endIsEmpty = ltfLstThresholdsEnd.isEmpty();
    boolean additionalIsEmpty = ltfLstThresholdsAdditional.isEmpty();
    if (startIsEmpty && incrementIsEmpty) {
      //lst thesholds is required
      if (additionalIsEmpty) {
        UIHarness.INSTANCE.openMessageDialog(LST_THRESHOLDS_LABEL
            + " is required.", "Entry Error", manager.getManagerKey());
        return false;
      }
      //check empty list descriptor
      if (!endIsEmpty) {
        UIHarness.INSTANCE.openMessageDialog("In " + LST_THRESHOLDS_LABEL
            + ", invalid list description.", "Entry Error", manager
            .getManagerKey());
        return false;
      }
    }
    //check list descriptor
    else if (startIsEmpty || endIsEmpty) {
      UIHarness.INSTANCE.openMessageDialog("In " + LST_THRESHOLDS_LABEL
          + ", invalid list description.", "Entry Error", manager
          .getManagerKey());
      return false;
    }
    return true;
  }

  private void gotoSetupTab() {
    tabPane.setSelectedIndex(0);
    changeTab();
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
    updateParallelProcess();
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public void updateParallelProcess() {
    manager.setParallelDialog(axisID, usingParallelProcessing());
  }

  public int getVolumeTableSize() {
    return volumeTable.size();
  }

  /**
   * Enabled/disables fields.  Calls updateDisplay() in subordinate panels.
   */
  public void updateDisplay() {
    //tilt range
    rbCcModeNormalized.setEnabled(!missingWedgeCompensationPanel
        .isFlgWedgeWeightSelected());
    if (missingWedgeCompensationPanel.isFlgWedgeWeightSelected()) {
      rbCcModeLocal.setSelected(true);
    }
    boolean volumeRows = volumeTable.size() > 0;
    referencePanel.updateDisplay();
    missingWedgeCompensationPanel.updateDisplay();
    yAxisTypePanel.updateDisplay();
    sphericalSamplingForThetaAndPsiPanel.updateDisplay();
    //iteration table - spherical sampling and FlgRemoveDuplicates
    iterationTable.updateDisplay(!sphericalSamplingForThetaAndPsiPanel
        .isSampleSphereNoneSelected(), cbFlgRemoveDuplicates.isSelected());
    //volume table
    volumeTable.updateDisplay(rbInitMotlFiles.isSelected(),
        missingWedgeCompensationPanel.isTiltRangeSelected());
    maskingPanel.updateDisplay();
  }

  private void chooseDirectory() {
    JFileChooser chooser = new FileChooser(new File(manager
        .getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      ftfDirectory.setText(chooser.getSelectedFile().getAbsolutePath());
    }
  }

  private void addListeners() {
    PDActionListener actionListener = new PDActionListener(this);
    ftfDirectory.addActionListener(actionListener);
    rbInitMotlZero.addActionListener(actionListener);
    rbInitMotlZAxis.addActionListener(actionListener);
    rbInitMotlXAndZAxis.addActionListener(actionListener);
    rbInitMotlFiles.addActionListener(actionListener);
    btnRun.addActionListener(actionListener);
    tabPane.addChangeListener(new TabChangeListener(this));
    btnAvgVol.addActionListener(actionListener);
    btnRef.addActionListener(actionListener);
    cbFlgRemoveDuplicates.addActionListener(actionListener);
    btnAverageAll.addActionListener(actionListener);
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
