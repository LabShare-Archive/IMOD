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
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 1.68  2009/01/13 19:39:36  sueh
 * <p> bug# 1170 Added cbNWeightGroup so that nWeightGroup can be enabled independently of flgWedgeWeight.  Getting the minimum of the nWeightGroup spinner from MatlabParam.  Saving cbNWeightGroup in metadata, so that its setting is not lost when it is disabled.  CbNWeightGroup enables sNWeightGroup.
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
    Expandable, Run3dmodButtonContainer, FileContainer {
  public static final String rcsid = "$Id$";

  public static final String FN_OUTPUT_LABEL = "Root name for output";
  public static final String DIRECTORY_LABEL = "Directory";

  private static final String PARTICLE_LABEL = "Particle #";
  private static final String REFERENCE_VOLUME_LABEL = "Volume #";
  private static final String REFERENCE_FILE_LABEL = "Reference file: ";
  private static final String MODEL_LABEL = "Model #: ";
  private static final String MASK_TYPE_VOLUME_LABEL = "Volume";
  private static final DialogType DIALOG_TYPE = DialogType.PEET;
  private static final String LST_THRESHOLD_START_TITLE = "Start";
  private static final String LST_THRESHOLD_INCREMENT_TITLE = "Incr.";
  private static final String LST_THRESHOLD_END_TITLE = "End";
  private static final String LST_THRESHOLD_ADDITIONAL_NUMBERS_TITLE = "Additional numbers";
  private static final String N_WEIGHT_GROUP_LABEL = "# of weight groups for equalizing CCCs: ";
  private static final String REFERENCE_LABEL = "Reference";
  private static final String Y_AXIS_TYPE_LABEL = "Y Axis Type";
  private static final String Y_AXIS_CONTOUR_OBJECT_NUMBER_LABEL = "Object #";
  private static final String Y_AXIS_CONTOUR_CONTOUR_NUMBER_LABEL = "Contour #";
  private static final String Y_AXIS_CONTOUR_LABEL = "End points of contour";
  private static final String MASK_TYPE_LABEL = "Masking";
  private static final String MASK_TYPE_SPHERE_LABEL = "Sphere";
  private static final String MASK_TYPE_CYLINDER_LABEL = "Cylinder";
  private static final String INSIDE_MASK_RADIUS_LABEL = "Inner";
  private static final String OUTSIDE_MASK_RADIUS_LABEL = "Outer";
  private static final String MASK_RADII_LABEL = "Radii of Sphere or Cylinder";
  private static final String MASK_CYLINDER_LABEL = "Cylinder Orientation";
  private static final String MASK_USE_REFERENCE_PARTICLE_LABEL = "Set cylinder orientation from reference particle";
  private static final String SPHERICAL_SAMPLING_LABEL = "Spherical Sampling for Theta and Psi";
  private static final String SAMPLE_SPHERE_FULL_LABEL = "Full sphere";
  private static final String SAMPLE_SPHERE_HALF_LABEL = "Half sphere";
  private static final String SAMPLE_INTERVAL_LABEL = "Sample interval";
  private static final String PARTICLE_VOLUME_LABEL = "Particle volume";
  private static final String X_LABEL = "X";
  private static final String Y_LABEL = "Y";
  private static final String Z_LABEL = "Z";

  private final EtomoPanel rootPanel = new EtomoPanel();
  private final FileTextField ftfDirectory = new FileTextField(DIRECTORY_LABEL
      + ": ");
  private final LabeledTextField ltfFnOutput = new LabeledTextField(
      FN_OUTPUT_LABEL + ": ");
  private final SpacedPanel pnlSetupBody = SpacedPanel.getInstance();
  private final CheckBox cbTiltRange = new CheckBox(
      "Use tilt range in averaging");
  private final LabeledTextField ltfReferenceParticle = new LabeledTextField(
      PARTICLE_LABEL + ": ");
  private final FileTextField ftfReferenceFile = FileTextField
      .getUnlabeledInstance(REFERENCE_FILE_LABEL);
  private final LabeledTextField ltfSzVolX = new LabeledTextField(
      PARTICLE_VOLUME_LABEL + " " + X_LABEL + ": ");
  private final LabeledTextField ltfSzVolY = new LabeledTextField(Y_LABEL
      + ": ");
  private final LabeledTextField ltfSzVolZ = new LabeledTextField(Z_LABEL
      + ": ");
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
  private final LabeledTextField ltfYAxisContourObjectNumber = new LabeledTextField(
      " " + Y_AXIS_CONTOUR_OBJECT_NUMBER_LABEL + ": ");
  private final LabeledTextField ltfYAxisContourContourNumber = new LabeledTextField(
      " " + Y_AXIS_CONTOUR_CONTOUR_NUMBER_LABEL + ": ");
  private final CheckBox cbLstFlagAllTom = new CheckBox(
      "Use equal numbers of particles from all tomograms for averages");
  private final SpacedPanel pnlRunBody = SpacedPanel.getInstance(true);
  private final MultiLineButton btnRun = new MultiLineButton("Run");
  private final JPanel pnlAdvanced = new JPanel();
  private final ButtonGroup bgReference = new ButtonGroup();
  private final RadioButton rbReferenceParticle = new RadioButton(
      REFERENCE_VOLUME_LABEL + ": ", bgReference);
  private final Spinner sReferenceVolume = Spinner
      .getInstance(REFERENCE_VOLUME_LABEL + ": ");
  private final Spinner sYAxisContourModelNumber = Spinner
      .getLabeledInstance(MODEL_LABEL);
  private final RadioButton rbReferenceFile = new RadioButton(
      REFERENCE_FILE_LABEL, bgReference);
  private final LabeledSpinner lsParticlePerCPU = new LabeledSpinner(
      "Particles per CPU: ",
      new SpinnerNumberModel(MatlabParam.PARTICLE_PER_CPU_DEFAULT,
          MatlabParam.PARTICLE_PER_CPU_MIN, MatlabParam.PARTICLE_PER_CPU_MAX, 1),
      MatlabParam.PARTICLE_PER_CPU_DEFAULT);
  private final IterationTable iterationTable;
  private final ButtonGroup bgYAxisType = new ButtonGroup();
  private final RadioButton rbYAxisTypeYAxis = new RadioButton(
      "Original Y axis", MatlabParam.YAxisType.Y_AXIS, bgYAxisType);
  private final RadioButton rbYAxisTypeParticleModel = new RadioButton(
      "Particle model points", MatlabParam.YAxisType.PARTICLE_MODEL,
      bgYAxisType);
  private final RadioButton rbYAxisTypeContour = new RadioButton(
      Y_AXIS_CONTOUR_LABEL + ":  ", MatlabParam.YAxisType.CONTOUR, bgYAxisType);

  private final ButtonGroup bgSampleSphere = new ButtonGroup();
  private final RadioButton rbSampleSphereNone = new RadioButton("None",
      MatlabParam.SampleSphere.NONE, bgSampleSphere);
  private final RadioButton rbSampleSphereFull = new RadioButton(
      SAMPLE_SPHERE_FULL_LABEL, MatlabParam.SampleSphere.FULL, bgSampleSphere);
  private final RadioButton rbSampleSphereHalf = new RadioButton(
      SAMPLE_SPHERE_HALF_LABEL, MatlabParam.SampleSphere.HALF, bgSampleSphere);
  private final LabeledTextField ltfSampleInterval = new LabeledTextField(
      "Sample interval: ");

  private final ButtonGroup bgMaskType = new ButtonGroup();
  private final RadioButton rbMaskTypeNone = new RadioButton("None",
      MatlabParam.MaskType.NONE, bgMaskType);
  private final RadioButton rbMaskTypeVolume = new RadioButton(
      MASK_TYPE_VOLUME_LABEL, MatlabParam.MaskType.VOLUME, bgMaskType);
  private final RadioButton rbMaskTypeSphere = new RadioButton(
      MASK_TYPE_SPHERE_LABEL, MatlabParam.MaskType.SPHERE, bgMaskType);
  private final RadioButton rbMaskTypeCylinder = new RadioButton(
      MASK_TYPE_CYLINDER_LABEL, MatlabParam.MaskType.CYLINDER, bgMaskType);
  private final FileTextField ftfMaskTypeVolume = new FileTextField(
      MASK_TYPE_VOLUME_LABEL + ": ");
  private final Spinner sMaskModelPtsVolumeModelNumber = Spinner
      .getLabeledInstance(MODEL_LABEL);
  private final LabeledTextField ltfMaskModelPtsVolumeParticle = new LabeledTextField(
      PARTICLE_LABEL + ": ");
  private final LabeledTextField ltfInsideMaskRadius = new LabeledTextField(
      INSIDE_MASK_RADIUS_LABEL + ": ");
  private final LabeledTextField ltfOutsideMaskRadius = new LabeledTextField(
      OUTSIDE_MASK_RADIUS_LABEL + ": ");
  private final CheckBox cbMaskUseReferenceParticle = new CheckBox(
      MASK_USE_REFERENCE_PARTICLE_LABEL);

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
  private final MultiLineButton btnImportMatlabParamFile = new MultiLineButton(
      "Import a .prm File");
  private final Run3dmodButton btnAvgVol = Run3dmodButton.get3dmodInstance(
      "Open Averaged Volumes in 3dmod", this);
  private final EtomoPanel pnlInitMotl = new EtomoPanel();
  private final TabbedPane tabPane = new TabbedPane();
  private final SpacedPanel pnlSetup = SpacedPanel.getInstance();
  private final EtomoPanel pnlRun = new EtomoPanel();
  private final SpacedPanel pnlYaxisType = SpacedPanel.getInstance();
  private final EtomoPanel pnlCcMode = new EtomoPanel();
  private final Run3dmodButton btnRef = Run3dmodButton.get3dmodInstance(
      "Open Reference Files in 3dmod", this);
  private final MultiLineButton btnDuplicateProject = new MultiLineButton(
      "Duplicate an Existing Project");
  private final MultiLineButton btnCopyParameters = new MultiLineButton(
      "Copy Parameters");
  private final CheckBox cbFlgWedgeWeight = new CheckBox(
      "Use tilt range in alignment");
  private final CheckBox cbNWeightGroup = new CheckBox(N_WEIGHT_GROUP_LABEL);
  private final Spinner sNWeightGroup = Spinner.getInstance(
      N_WEIGHT_GROUP_LABEL, MatlabParam.N_WEIGHT_GROUP_DEFAULT,
      MatlabParam.N_WEIGHT_GROUP_MIN, 20);

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
    fixPathsPanel = FixPathsPanel.getInstance(this, manager, axisID,
        DIALOG_TYPE);
    ftfReferenceFile.setFieldWidth(UIParameters.INSTANCE.getFileWidth());
    ftfMaskTypeVolume.setFieldWidth(UIParameters.INSTANCE.getFileWidth());
    phSetup = PanelHeader.getInstance("Setup", this, DIALOG_TYPE);
    phRun = PanelHeader.getAdvancedBasicInstance("Run", this, DIALOG_TYPE);
    volumeTable = VolumeTable.getInstance(manager, this);
    iterationTable = IterationTable.getInstance(manager);
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

  public void checkIncorrectPaths() {
    boolean incorrectPaths = false;
    if (volumeTable.isIncorrectPaths()) {
      incorrectPaths = true;
    }
    else if (!ftfReferenceFile.isEmpty() && !ftfReferenceFile.exists()) {
      incorrectPaths = true;
    }
    else if (!ftfMaskTypeVolume.isEmpty() && !ftfMaskTypeVolume.exists()) {
      incorrectPaths = true;
    }
    fixPathsPanel.setIncorrectPaths(incorrectPaths);
  }

  public void fixIncorrectPaths(boolean choosePathEveryRow) {
    if (!volumeTable.fixIncorrectPaths(choosePathEveryRow)) {
      return;
    }
    if (!ftfReferenceFile.isEmpty() && !ftfReferenceFile.exists()
        && !fixIncorrectPath(ftfReferenceFile, choosePathEveryRow)) {
      return;
    }
    if (!ftfMaskTypeVolume.isEmpty() && !ftfMaskTypeVolume.exists()
        && !fixIncorrectPath(ftfMaskTypeVolume, choosePathEveryRow)) {
      return;
    }
    checkIncorrectPaths();
  }

  /**
   * Fix an incorrect path.
   * @param fileTextField
   * @param choosePathEveryRow
   * @return false if the user cancels the file selector
   */
  private boolean fixIncorrectPath(FileTextField fileTextField,
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
    return new JFileChooser(lastLocation == null ? new File(manager
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
    metaData.setReferenceVolume(sReferenceVolume.getValue());
    metaData.setReferenceParticle(ltfReferenceParticle.getText());
    metaData.setReferenceFile(ftfReferenceFile.getText());
    metaData.setEdgeShift(ltfEdgeShift.getText());
    metaData.setYaxisContourModelNumber(sYAxisContourModelNumber.getValue());
    metaData.setYaxisContourObjectNumber(ltfYAxisContourObjectNumber.getText());
    metaData.setYaxisContourContourNumber(ltfYAxisContourContourNumber
        .getText());
    metaData.setFlgWedgeWeight(cbFlgWedgeWeight.isSelected());
    metaData.setMaskUseReferenceParticle(cbMaskUseReferenceParticle
        .isSelected());
    metaData.setMaskModelPtsModelNumber(sMaskModelPtsVolumeModelNumber
        .getValue());
    metaData.setMaskModelPtsParticle(ltfMaskModelPtsVolumeParticle.getText());
    metaData.setMaskTypeVolume(ftfMaskTypeVolume.getText());
    metaData.setUseNWeightGroup(cbNWeightGroup.isSelected());
    metaData.setNWeightGroup(sNWeightGroup.getValue());
    metaData.setTiltRange(cbTiltRange.isSelected());
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
      sYAxisContourModelNumber.setValue(metaData.getYAxisContourModelNumber());
      ltfYAxisContourObjectNumber.setText(metaData
          .getYAxisContourObjectNumber());
      ltfYAxisContourContourNumber.setText(metaData
          .getYAxisContourContourNumber());
      cbFlgWedgeWeight.setSelected(metaData.isFlgWedgeWeight());
    }
    ltfEdgeShift.setText(metaData.getEdgeShift());
    cbMaskUseReferenceParticle.setSelected(metaData
        .isMaskUseReferenceParticle());
    sMaskModelPtsVolumeModelNumber.setValue(metaData
        .getMaskModelPtsModelNumber());
    ltfMaskModelPtsVolumeParticle.setText(metaData.getMaskModelPtsParticle());
    ftfMaskTypeVolume.setText(metaData.getMaskTypeVolume());
    cbNWeightGroup.setSelected(metaData.isUseNWeightGroup());
    //backwards compatibility - raised nWeightGroup minimum from 0 to 2
    int nWeightGroup = metaData.getNWeightGroup().getInt();
    if (nWeightGroup < MatlabParam.N_WEIGHT_GROUP_MIN) {
      nWeightGroup = MatlabParam.N_WEIGHT_GROUP_MIN;
    }
    sNWeightGroup.setValue(nWeightGroup);
    //Distinguish between what is set in the tilt range numbers and the check
    //box by saving them separately.  This works better then trying to tell the
    //difference between [] and {} in the .prm file.
    cbTiltRange.setSelected(metaData.isTiltRange());
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
    //Backwards compatibility:  if the tilt range has numbers in it, then check
    //cbTiltRange.  If not, rely on the new variable in MetaData.
    if (!matlabParam.isTiltRangeEmpty()) {
      cbTiltRange.setSelected(true);
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
    MatlabParam.YAxisType yaxisType = matlabParam.getYAxisType();
    if (yaxisType == MatlabParam.YAxisType.Y_AXIS) {
      rbYAxisTypeYAxis.setSelected(true);
    }
    else if (yaxisType == MatlabParam.YAxisType.PARTICLE_MODEL) {
      rbYAxisTypeParticleModel.setSelected(true);
    }
    else if (yaxisType == MatlabParam.YAxisType.CONTOUR) {
      rbYAxisTypeContour.setSelected(true);
      if (!parametersOnly) {
        sYAxisContourModelNumber.setValue(matlabParam
            .getYAxisContourModelNumber());
        ltfYAxisContourObjectNumber.setText(matlabParam
            .getYAxisContourObjectNumber());
        ltfYAxisContourContourNumber.setText(matlabParam
            .getYAxisContourContourNumber());
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
      cbNWeightGroup.setSelected(!matlabParam.isNWeightGroupEmpty());
    }
    if (isEnableNWeightGroup() && cbNWeightGroup.isSelected()) {
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
    //If cbTiltRange is off, this overrides what was set in the volumeTable.
    if (!cbTiltRange.isSelected()) {
      matlabParam.setTiltRangeEmpty();
    }
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
    matlabParam.setYaxisType(((RadioButton.RadioButtonModel) bgYAxisType
        .getSelection()).getEnumeratedType());
    if (rbYAxisTypeContour.isSelected()) {
      matlabParam.setYAxisContourModelNumber(sYAxisContourModelNumber
          .getValue());
      matlabParam.setYaxisContourObjectNumber(ltfYAxisContourObjectNumber
          .getText());
      matlabParam.setYaxisContourContourNumber(ltfYAxisContourContourNumber
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
    ltfYAxisContourObjectNumber.clear();
    ltfYAxisContourContourNumber.clear();
    cbLstFlagAllTom.setSelected(false);
    rbReferenceParticle.setSelected(false);
    sReferenceVolume.reset();
    sYAxisContourModelNumber.reset();
    rbReferenceFile.setSelected(false);
    rbYAxisTypeYAxis.setSelected(false);
    rbYAxisTypeParticleModel.setSelected(false);
    rbYAxisTypeContour.setSelected(false);
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
    cbNWeightGroup.setSelected(false);
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
      rbYAxisTypeYAxis.setToolTipText(section);
      rbYAxisTypeParticleModel.setToolTipText(section);
      tooltip = rbYAxisTypeContour.setToolTipText(section);
      sYAxisContourModelNumber.setToolTipText(tooltip);
      ltfYAxisContourObjectNumber.setToolTipText(tooltip);
      ltfYAxisContourContourNumber.setToolTipText(tooltip);
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
      tooltip = EtomoAutodoc
          .getTooltip(autodoc, MatlabParam.N_WEIGHT_GROUP_KEY);
      cbNWeightGroup.setToolTipText(tooltip);
      sNWeightGroup.setToolTipText(tooltip);
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
    EtomoPanel pnlUseExistingProject = new EtomoPanel();
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
    EtomoPanel pnlReference = new EtomoPanel();
    pnlReference.setLayout(new BoxLayout(pnlReference, BoxLayout.Y_AXIS));
    pnlReference.setBorder(new EtchedBorder(REFERENCE_LABEL).getBorder());
    pnlReference.add(pnlVolumeReference);
    pnlReference.add(pnlVolumeFile);
    //tiltRange and edgeShift
    SpacedPanel pnlTiltRange = SpacedPanel.getInstance();
    pnlTiltRange.setBoxLayout(BoxLayout.X_AXIS);
    pnlTiltRange.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlTiltRange.add(cbTiltRange);
    pnlTiltRange.addRigidArea(FixedDim.x20_y0);
    ltfEdgeShift.setTextPreferredWidth(UIParameters.INSTANCE.getIntegerWidth());
    pnlTiltRange.add(ltfEdgeShift.getContainer());
    //flgWedgeWeight
    SpacedPanel pnlFlgWedgeWeight = SpacedPanel.getInstance();
    pnlFlgWedgeWeight.setBoxLayout(BoxLayout.X_AXIS);
    pnlFlgWedgeWeight.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlFlgWedgeWeight.add(cbFlgWedgeWeight);
    pnlFlgWedgeWeight.addHorizontalGlue();
    //missing wedge compensation
    SpacedPanel pnlMissingWedgeCompensation = SpacedPanel.getInstance();
    pnlMissingWedgeCompensation.setBoxLayout(BoxLayout.Y_AXIS);
    pnlMissingWedgeCompensation.setBorder(new EtchedBorder(
        "Missing Wedge Compensation").getBorder());
    pnlMissingWedgeCompensation
        .setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlMissingWedgeCompensation.add(pnlTiltRange);
    pnlMissingWedgeCompensation.add(pnlFlgWedgeWeight);
    SpacedPanel pnlNWeightGroup = SpacedPanel.getInstance();
    pnlNWeightGroup.setBoxLayout(BoxLayout.X_AXIS);
    pnlNWeightGroup.add(cbNWeightGroup);
    pnlNWeightGroup.add(sNWeightGroup.getContainer());
    pnlMissingWedgeCompensation.add(pnlNWeightGroup);
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
    EtomoPanel pnlMaskRadii = new EtomoPanel();
    pnlMaskRadii.setLayout(new BoxLayout(pnlMaskRadii, BoxLayout.X_AXIS));
    pnlMaskRadii.setBorder(new EtchedBorder(MASK_RADII_LABEL).getBorder());
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
    EtomoPanel pnlMaskCylinder = new EtomoPanel();
    pnlMaskCylinder.setLayout(new BoxLayout(pnlMaskCylinder, BoxLayout.Y_AXIS));
    pnlMaskCylinder
        .setBorder(new EtchedBorder(MASK_CYLINDER_LABEL).getBorder());
    pnlMaskCylinder.add(pnlUseMaskModelPts);
    pnlMaskCylinder.add(pnlMaskModelPts);
    //mask
    EtomoPanel pnlMask = new EtomoPanel();
    pnlMask.setLayout(new BoxLayout(pnlMask, BoxLayout.X_AXIS));
    pnlMask.setBorder(new EtchedBorder(MASK_TYPE_LABEL).getBorder());
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
    JPanel pnlYAxisContour = new JPanel();
    pnlYAxisContour.setLayout(new BoxLayout(pnlYAxisContour, BoxLayout.X_AXIS));
    pnlYAxisContour.add(rbYAxisTypeContour.getComponent());
    pnlYAxisContour.add(sYAxisContourModelNumber.getContainer());
    ltfYAxisContourObjectNumber.setTextPreferredWidth(UIParameters.INSTANCE
        .getIntegerWidth());
    pnlYAxisContour.add(ltfYAxisContourObjectNumber.getContainer());
    ltfYAxisContourContourNumber.setTextPreferredWidth(UIParameters.INSTANCE
        .getIntegerWidth());
    pnlYAxisContour.add(ltfYAxisContourContourNumber.getContainer());
    //YaxisType
    pnlYaxisType.setBoxLayout(BoxLayout.Y_AXIS);
    pnlYaxisType.setBorder(new EtchedBorder(Y_AXIS_TYPE_LABEL).getBorder());
    pnlYaxisType.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlYaxisType.add(rbYAxisTypeYAxis);
    pnlYaxisType.add(rbYAxisTypeParticleModel);
    pnlYaxisType.add(pnlYAxisContour);
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
    pnlSetupBody.add(fixPathsPanel.getRootComponent());
    pnlSetupBody.add(volumeTable.getContainer());
    pnlSetupBody.add(pnlReferenceAndMissingWedgeCompensation);
    pnlSetupBody.add(pnlMask);
    pnlSetupBody.add(pnlInitMotlAndYAxisType);
    //main panel
    pnlSetup.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetup.setBorder(BorderFactory.createEtchedBorder());
    pnlSetup.add(phSetup);
  }

  private void createRunPanel() {
    //Spherical Sampling
    EtomoPanel pnlSphericalSampling = new EtomoPanel();
    pnlSphericalSampling.setLayout(new BoxLayout(pnlSphericalSampling,
        BoxLayout.X_AXIS));
    pnlSphericalSampling.setBorder(new EtchedBorder(SPHERICAL_SAMPLING_LABEL)
        .getBorder());
    pnlSphericalSampling.add(rbSampleSphereNone.getComponent());
    pnlSphericalSampling.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSphericalSampling.add(rbSampleSphereFull.getComponent());
    pnlSphericalSampling.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSphericalSampling.add(rbSampleSphereHalf.getComponent());
    pnlSphericalSampling.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlSphericalSampling.add(ltfSampleInterval.getContainer());
    pnlSphericalSampling.add(Box.createHorizontalGlue());
    //szVol
    SpacedPanel pnlSzVol = SpacedPanel.getInstance();
    pnlSzVol.setBoxLayout(BoxLayout.X_AXIS);
    pnlSzVol.add(ltfSzVolX.getContainer());
    pnlSzVol.add(ltfSzVolY.getContainer());
    pnlSzVol.add(ltfSzVolZ.getContainer());
    //lstThresholds
    SpacedPanel pnlLstThresholds = SpacedPanel.getInstance();
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
        || actionCommand.equals(rbYAxisTypeYAxis.getActionCommand())
        || actionCommand.equals(rbYAxisTypeParticleModel.getActionCommand())
        || actionCommand.equals(rbYAxisTypeContour.getActionCommand())
        || actionCommand.equals(cbFlgWedgeWeight.getActionCommand())
        || actionCommand.equals(cbNWeightGroup.getActionCommand())
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
    //Setup tab

    //Must have particle number if volume is selected
    if (rbReferenceParticle.isSelected() && ltfReferenceParticle.isEnabled()
        && ltfReferenceParticle.getText().matches("\\s*")) {
      changeTab(0);
      UIHarness.INSTANCE.openMessageDialog("In " + REFERENCE_LABEL + ", "
          + PARTICLE_LABEL + " is required when " + REFERENCE_VOLUME_LABEL
          + " is selected.", "Entry Error", AxisID.ONLY, manager
          .getManagerKey());
      return false;
    }
    //if sphere or cylinder is selected, require inner or outer or both.
    if ((rbMaskTypeSphere.isSelected() || rbMaskTypeCylinder.isSelected())
        && ltfInsideMaskRadius.isEnabled()
        && ltfInsideMaskRadius.getText().matches("\\s*")
        && ltfOutsideMaskRadius.isEnabled()
        && ltfOutsideMaskRadius.getText().matches("\\s*")) {
      changeTab(0);
      UIHarness.INSTANCE.openMessageDialog("In " + MASK_TYPE_LABEL + ", "
          + INSIDE_MASK_RADIUS_LABEL + " and/or " + OUTSIDE_MASK_RADIUS_LABEL
          + " " + MASK_RADII_LABEL + " are required when either "
          + MASK_TYPE_SPHERE_LABEL + " or " + MASK_TYPE_CYLINDER_LABEL
          + " is selected.", "Entry Error", manager.getManagerKey());
      return false;
    }
    //if Cylinder is selected and "set cylinder orientation from reference
    //particle" is not selected, then Particle # is required.
    if (rbMaskTypeCylinder.isSelected()
        && !cbMaskUseReferenceParticle.isSelected()
        && ltfMaskModelPtsVolumeParticle.isEnabled()
        && ltfMaskModelPtsVolumeParticle.getText().matches("\\s*")) {
      changeTab(0);
      UIHarness.INSTANCE.openMessageDialog("In " + MASK_CYLINDER_LABEL + ", "
          + PARTICLE_LABEL + " is required when " + MASK_TYPE_CYLINDER_LABEL
          + " " + MASK_TYPE_LABEL + " is selected and "
          + MASK_USE_REFERENCE_PARTICLE_LABEL + " is not checked. ",
          "Entry Error", manager.getManagerKey());
      return false;
    }
    //If end points of contour is checked, must have object # and contour#
    if (rbYAxisTypeContour.isSelected()
        && ((ltfYAxisContourObjectNumber.isEnabled() && ltfYAxisContourObjectNumber
            .getText().matches("\\s*")) || (ltfYAxisContourContourNumber
            .isEnabled() && ltfYAxisContourContourNumber.getText().matches(
            "\\s*")))) {
      changeTab(0);
      UIHarness.INSTANCE.openMessageDialog("In " + Y_AXIS_TYPE_LABEL + ", "
          + Y_AXIS_CONTOUR_OBJECT_NUMBER_LABEL + " and "
          + Y_AXIS_CONTOUR_CONTOUR_NUMBER_LABEL + " are required when "
          + Y_AXIS_CONTOUR_LABEL + " is selected.", "Entry Error", manager
          .getManagerKey());
      return false;
    }

    //Run tab

    //spherical sampling for theta and psi:
    //If full sphere or half sphere is selected, sample interval is required.
    if ((rbSampleSphereFull.isSelected() || rbSampleSphereHalf.isSelected())
        && ltfSampleInterval.isEnabled()
        && ltfSampleInterval.getText().matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog("In " + SPHERICAL_SAMPLING_LABEL
          + ", " + SAMPLE_INTERVAL_LABEL + " is required when either "
          + SAMPLE_SPHERE_FULL_LABEL + " or " + SAMPLE_SPHERE_HALF_LABEL
          + " is selected.", "Entry Error", manager.getManagerKey());
      return false;
    }
    if (!iterationTable.validateRun()) {
      return false;
    }
    Goodframe goodframe = new Goodframe(manager.getPropertyUserDir(),
        AxisID.FIRST, manager.getManagerKey());
    String szVolX = ltfSzVolX.getText();
    String szVolY = ltfSzVolY.getText();
    String szVolZ = ltfSzVolZ.getText();
    if ((szVolX != null && !szVolX.matches("\\s*"))
        || (szVolY != null && !szVolY.matches("\\s*"))
        || (szVolZ != null && !szVolZ.matches("\\s*"))) {
      try {
        goodframe.run(new String[] { szVolX, szVolY, szVolZ });
        if (szVolX != null && !szVolX.matches("\\s*")
            && !goodframe.getOutput(0).equals(szVolX)) {
          UIHarness.INSTANCE.openMessageDialog("In " + PARTICLE_VOLUME_LABEL
              + ", " + X_LABEL + " is invalid.  Try " + goodframe.getOutput(0)
              + ".", "Entry Error", manager.getManagerKey());
          return false;
        }
        if (szVolY != null && !szVolY.matches("\\s*")
            && !goodframe.getOutput(1).equals(szVolY)) {
          UIHarness.INSTANCE.openMessageDialog("In " + PARTICLE_VOLUME_LABEL
              + ", " + Y_LABEL + " is invalid.  Try " + goodframe.getOutput(1)
              + ".", "Entry Error", manager.getManagerKey());
          return false;
        }
        if (szVolZ != null && !szVolZ.matches("\\s*")
            && !goodframe.getOutput(2).equals(szVolZ)) {
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
    }
    return true;
  }

  /**
   * Create a project out of a matlab param file.
   */
  private void importMatlabParam() {
    String path = ftfDirectory.getText();
    if (path == null || path.matches("\\s*")) {
      UIHarness.INSTANCE.openMessageDialog("Please set the "
          + PeetDialog.DIRECTORY_LABEL + "field before importing a .prm file.",
          "Entry Error", manager.getManagerKey());
      return;
    }
    File dir = new File(ftfDirectory.getText());
    if (!dir.exists()) {
      UIHarness.INSTANCE.openMessageDialog("Please create "
          + dir.getAbsolutePath() + " before importing a .prm file.",
          "Entry Error", manager.getManagerKey());
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
          "Entry Error", manager.getManagerKey());
      return;
    }
    File dir = new File(ftfDirectory.getText());
    if (!dir.exists()) {
      UIHarness.INSTANCE.openMessageDialog("Please create "
          + dir.getAbsolutePath() + " before importing a .prm file.",
          "Entry Error", manager.getManagerKey());
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
          "Entry Error", manager.getManagerKey());
      return;
    }
    File dir = new File(ftfDirectory.getText());
    if (!dir.exists()) {
      UIHarness.INSTANCE.openMessageDialog("Please create "
          + dir.getAbsolutePath() + " before copy parameters.", "Entry Error",
          manager.getManagerKey());
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

  private void changeTab(int tabIndex) {
    tabPane.setSelectedIndex(tabIndex);
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
    rbYAxisTypeContour.setEnabled(volumeRows);
    sYAxisContourModelNumber.setEnabled(volumeRows
        && rbYAxisTypeContour.isSelected());
    sYAxisContourModelNumber.setMax(size);
    ltfYAxisContourObjectNumber.setEnabled(volumeRows
        && rbYAxisTypeContour.isSelected());
    ltfYAxisContourContourNumber.setEnabled(volumeRows
        && rbYAxisTypeContour.isSelected());
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
    cbNWeightGroup.setEnabled(isEnableNWeightGroup());
    sNWeightGroup.setEnabled(isEnableNWeightGroup()
        && cbNWeightGroup.isSelected());
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
    rbYAxisTypeYAxis.addActionListener(actionListener);
    rbYAxisTypeParticleModel.addActionListener(actionListener);
    rbYAxisTypeContour.addActionListener(actionListener);
    btnImportMatlabParamFile.addActionListener(actionListener);
    btnAvgVol.addActionListener(actionListener);
    btnRef.addActionListener(actionListener);
    btnDuplicateProject.addActionListener(actionListener);
    btnCopyParameters.addActionListener(actionListener);
    cbFlgWedgeWeight.addActionListener(actionListener);
    cbNWeightGroup.addActionListener(actionListener);
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
