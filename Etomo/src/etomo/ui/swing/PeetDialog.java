package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.GridLayout;
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
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.PeetManager;
import etomo.ProcessingMethodMediator;
import etomo.comscript.AverageAllParam;
import etomo.comscript.ParallelParam;
import etomo.comscript.ProcesschunksParam;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstPeetMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.PeetMetaData;
import etomo.type.ProcessingMethod;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldLabels;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.ui.UIComponent;
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
 * <p> Revision 1.11  2011/09/01 04:27:17  sueh
 * <p> Bug# 1543 in validateRun, removed particle volume validation based on goodframe.
 * <p>
 * <p> Revision 1.10  2011/07/19 20:01:14  sueh
 * <p> Bug# 1459 Wrapped checkboxes in a panel and used glue to left justify them.  Prevented spinners
 * <p> which have a value when they are first displayed from going all the way to the right.
 * <p>
 * <p> Revision 1.9  2011/05/16 23:06:22  sueh
 * <p> bug# 1487 Backing out bug# 1485 and some of bug# 1445.
 * <p>
 * <p> Revision 1.8  2011/05/15 01:55:33  sueh
 * <p> bug# 1485 Keeping initMotlCode empty when there are init MOTL files conflicts with a new dataset.  Added
 * <p> InitMotlCode.FILES.
 * <p>
 * <p> Revision 1.7  2011/05/05 23:48:00  sueh
 * <p> bug# 1446 Corrected a label.
 * <p>
 * <p> Revision 1.6  2011/04/20 05:02:57  sueh
 * <p> bug# 1445 Added a radio button for uniform random rotation code for initMotlCode.
 * <p>
 * <p> Revision 1.5  2011/04/20 04:54:37  sueh
 * <p> bug# 1445 Added originalInitMotlCode to hold onto unrecognized initMotlCode values.  Handling unrecognized
 * <p> unrecognized initMotlCode values.
 * <p>
 * <p> Revision 1.4  2011/02/23 05:10:09  sueh
 * <p> bug# 1450 In constructor call the mediator setMethod function with
 * <p> getProcessingMethod() instead of always using PP_CPU.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:19:14  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.99  2010/05/20 23:51:58  sueh
 * <p> bug# 1368 Removed cbFlgMeanFill.
 * <p>
 * <p> Revision 1.98  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.97  2010/01/13 22:04:16  sueh
 * <p> bug# 1298 Corrected hyphenation for rbCcModeNormalized.
 * <p>
 * <p> Revision 1.96  2010/01/13 21:56:31  sueh
 * <p> bug# 1298 Made run button labels public.
 * <p>
 * <p> Revision 1.95  2009/12/23 03:20:37  sueh
 * <p> bug# 1296 Improved lstThreshold tooltips.
 * <p>
 * <p> Revision 1.94  2009/12/23 03:08:27  sueh
 * <p> bug# 1296 Added missing tooltips.
 * <p>
 * <p> Revision 1.93  2009/12/23 02:54:05  sueh
 * <p> bug# 1296 Removing unknown tooltips.
 * <p>
 * <p> Revision 1.92  2009/12/23 02:45:09  sueh
 * <p> bug# 1296 Corrected tooltips for szVol.
 * <p>
 * <p> Revision 1.91  2009/12/23 02:26:12  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.
 * <p>
 * <p> Revision 1.90  2009/12/08 16:03:58  sueh
 * <p> bug# 1287 Added cbflgAlignAverages.
 * <p>
 * <p> Revision 1.89  2009/12/08 02:47:32  sueh
 * <p> bug# 1286 Factored out panels.
 * <p>
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
    Run3dmodButtonContainer, FileContainer, ReferenceParent,
    MissingWedgeCompensationParent, IterationParent, MaskingParent, YAxisTypeParent,
    SphericalSamplingForThetaAndPsiParent, ProcessInterface, UIComponent, SwingComponent {
  public static final String rcsid = "$Id$";

  public static final String FN_OUTPUT_LABEL = "Root name for output";
  public static final String DIRECTORY_LABEL = "Directory";
  public static final String RUN_LABEL = "Run";
  public static final String AVERAGE_ALL_LABEL = "Remake Averages";
  static final String SETUP_LOCATION_DESCR = "the Setup tab";
  private static final String ALIGNED_BASE_NAME = "aligned";

  private static final DialogType DIALOG_TYPE = DialogType.PEET;
  private static final String LST_THRESHOLD_START_TITLE = "Start";
  private static final String LST_THRESHOLD_INCREMENT_TITLE = "Incr.";
  private static final String LST_THRESHOLD_END_TITLE = "End";
  private static final String LST_THRESHOLD_ADDITIONAL_NUMBERS_TITLE = "Additional numbers";
  private static final String LST_THRESHOLDS_LABEL = "Number of Particles to Average";
  private static final String SETUP_TAB_LABEL = "Setup";
  private static final String RUN_TAB_LABEL = "Run";
  private static final String ALIGNED_BASE_NAME_LABEL = "Save individual aligned particles";
  private static final String PARTICLE_PER_CPU_LABEL = "Particles per CPU";

  private final EtomoPanel rootPanel = new EtomoPanel();
  private final LabeledTextField ltfDirectory = new LabeledTextField(FieldType.STRING,
      DIRECTORY_LABEL + ": ");
  private final LabeledTextField ltfFnOutput = new LabeledTextField(FieldType.STRING,
      FN_OUTPUT_LABEL + ": ");
  private final SpacedPanel pnlSetupBody = SpacedPanel.getInstance();
  private final CheckBox cbAlignedBaseName = new CheckBox(ALIGNED_BASE_NAME_LABEL);
  private final CheckBox cbFlgStrictSearchLimits = new CheckBox(
      FieldLabels.FLG_STRICT_SEARCH_LIMITS_LABEL);
  private final CheckBox cbFlgNoReferenceRefinement = new CheckBox(
      FieldLabels.FLG_NO_REFERENCE_REFINEMENT_LABEL);
  private final LabeledTextField ltfLowCutoff = new LabeledTextField(
      FieldType.FLOATING_POINT, "Low frequency cutoff:", 5);
  private final LabeledTextField ltfLowCutoffSigma = new LabeledTextField(
      FieldType.FLOATING_POINT, "Sigma: ");
  private final CheckBox cbRefFlagAllTom = new CheckBox("For new references");
  private final LabeledTextField ltfLstThresholdsStart = new LabeledTextField(
      FieldType.INTEGER, LST_THRESHOLD_START_TITLE + ": ");
  private final LabeledTextField ltfLstThresholdsIncrement = new LabeledTextField(
      FieldType.INTEGER, LST_THRESHOLD_INCREMENT_TITLE + ": ");
  private final LabeledTextField ltfLstThresholdsEnd = new LabeledTextField(
      FieldType.INTEGER, LST_THRESHOLD_END_TITLE + ": ");
  private final LabeledTextField ltfLstThresholdsAdditional = new LabeledTextField(
      FieldType.INTEGER_ARRAY, " " + LST_THRESHOLD_ADDITIONAL_NUMBERS_TITLE + ": ");
  private final CheckBox cbLstFlagAllTom = new CheckBox("For average volumes");
  private final SpacedPanel pnlRunBody = SpacedPanel.getInstance(true);
  private final MultiLineButton btnRun = new MultiLineButton(RUN_LABEL);
  private final LabeledSpinner lsParticlePerCPU = LabeledSpinner.getInstance(
      PARTICLE_PER_CPU_LABEL + ": ", MatlabParam.PARTICLE_PER_CPU_DEFAULT,
      MatlabParam.PARTICLE_PER_CPU_MIN, MatlabParam.PARTICLE_PER_CPU_MAX, 1, 28);
  private final IterationTable iterationTable;
  private final ButtonGroup bgInitMotl = new ButtonGroup();
  private final RadioButton rbInitMotlZero = new RadioButton(
      MatlabParam.InitMotlCode.ZERO, bgInitMotl);
  private final RadioButton rbInitAlignParticleYAxes = new RadioButton(
      MatlabParam.InitMotlCode.X_AND_Z_AXIS, bgInitMotl);
  private final RadioButton rbInitMotlRandomRotations = new RadioButton(
      MatlabParam.InitMotlCode.RANDOM_ROTATIONS, bgInitMotl);
  private final RadioButton rbInitMotlRandomAxialRotations = new RadioButton(
      MatlabParam.InitMotlCode.RANDOM_AXIAL_ROTATIONS, bgInitMotl);
  private final RadioButton rbInitMotlFiles = new RadioButton("User supplied csv files",
      bgInitMotl);
  private final LabeledSpinner lsDebugLevel = LabeledSpinner.getInstance(
      FieldLabels.DEBUG_LEVEL_LABEL + ": ", MatlabParam.DEBUG_LEVEL_DEFAULT,
      MatlabParam.DEBUG_LEVEL_MIN, MatlabParam.DEBUG_LEVEL_MAX, 1, 59);
  private final Run3dmodButton btnAvgVol = Run3dmodButton.get3dmodInstance(
      "Open averages in 3dmod", this);
  private final EtomoPanel pnlInitMotl = new EtomoPanel();
  private final TabbedPane tabPane = new TabbedPane();
  private final SpacedPanel pnlSetup = SpacedPanel.getInstance();
  private final EtomoPanel pnlRun = new EtomoPanel();
  private final Run3dmodButton btnRef = Run3dmodButton.get3dmodInstance(
      "Open references in 3dmod", this);
  private final MultiLineButton btnAverageAll = new MultiLineButton(AVERAGE_ALL_LABEL);
  private final CheckBox cbflgAlignAverages = new CheckBox(
      FieldLabels.FLG_ALIGN_AVERAGES_LABEL);
  private final CheckBox cbFlgAbsValue = new CheckBox(FieldLabels.FLG_ABS_VALUE_LABEL);
  private final LabeledTextField ltfSelectClassID = new LabeledTextField(
      FieldType.INTEGER, "Average only members of classes: ");

  private final SphericalSamplingForThetaAndPsiPanel sphericalSamplingForThetaAndPsiPanel;
  private final YAxisTypePanel yAxisTypePanel;
  private final MaskingPanel maskingPanel;
  private final MissingWedgeCompensationPanel missingWedgeCompensationPanel;
  private final ReferencePanel referencePanel;
  private final VolumeTable volumeTable;
  private final PeetManager manager;
  private final AxisID axisID;
  private final FixPathsPanel fixPathsPanel;
  private final ProcessingMethodMediator mediator;

  private File lastLocation = null;
  private String correctPath = null;

  private PeetDialog(final PeetManager manager, final AxisID axisID) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: " + DialogType.PEET);
    this.manager = manager;
    this.axisID = axisID;
    mediator = manager.getProcessingMethodMediator(axisID);
    referencePanel = ReferencePanel.getInstance(this, manager);
    missingWedgeCompensationPanel = MissingWedgeCompensationPanel.getInstance(this);
    maskingPanel = MaskingPanel.getInstance(manager, this);
    yAxisTypePanel = YAxisTypePanel.getInstance(manager, this);
    fixPathsPanel = FixPathsPanel.getInstance(this, manager, axisID, DIALOG_TYPE);
    sphericalSamplingForThetaAndPsiPanel = SphericalSamplingForThetaAndPsiPanel
        .getInstance(manager, this);
    volumeTable = VolumeTable.getInstance(manager, this);
    iterationTable = IterationTable.getInstance(manager, this);
    // panels
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("PEET").getBorder());
    rootPanel.add(tabPane);
    createSetupPanel();
    createRunPanel();
    tabPane.add(SETUP_TAB_LABEL, pnlSetup.getContainer());
    tabPane.add(RUN_TAB_LABEL, pnlRun);
    tabPane.addMouseListener(new GenericMouseAdapter(this));
    changeTab();
    setDefaults();
    updateDisplay();
    setTooltipText();
    mediator.register(this);
    mediator.setMethod(this, getProcessingMethod());
  }

  /**
  * Get the processing method based on the dialogs settings.  Dialogs don't
  * need to know if QUEUE is in use in the parallel panel.
  * @return
  */
  public ProcessingMethod getProcessingMethod() {
    if (tabPane.getSelectedIndex() == 1) {
      return ProcessingMethod.PP_CPU;
    }
    return ProcessingMethod.LOCAL_CPU;
  }

  public void disableGpu(final boolean disable) {
  }

  public void lockProcessingMethod(final boolean lock) {
  }

  public Component getFocusComponent() {
    return pnlSetup.getContainer();
  }

  public JComponent getSetupJComponent() {
    return pnlSetup.getJPanel();
  }

  public static PeetDialog getInstance(final PeetManager manager, final AxisID axisID) {
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
    ltfDirectory.setEditable(!paramFileSet);
    ltfFnOutput.setEditable(!paramFileSet);
    btnRun.setEnabled(paramFileSet);
  }

  public SwingComponent getUIComponent() {
    return this;
  }

  public Container getComponent() {
    return rootPanel;
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Processchunks", "3dmod" };
    String[] manPage = { "processchunks.html", "3dmod.html" };
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent, manPagelabel,
        manPage, true, manager, axisID);
  }

  public void pack() {
    volumeTable.pack();
  }

  public DialogType getDialogType() {
    return DIALOG_TYPE;
  }

  public void convertCopiedPaths(final String origDatasetDir) {
    volumeTable.convertCopiedPaths(origDatasetDir);
    referencePanel.convertCopiedPaths(origDatasetDir);
    maskingPanel.convertCopiedPaths(origDatasetDir);
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
  public boolean fixIncorrectPath(FileTextFieldInterface fileTextField, boolean choosePath) {
    File newFile = null;
    while (newFile == null || !newFile.exists()) {
      // Have the user choose the location of the file if they haven't chosen
      // before or they want to choose most of the files individuallly, otherwise
      // just use the current correctPath.
      if (correctPath == null || choosePath || (newFile != null && !newFile.exists())) {
        JFileChooser fileChooser = getFileChooserInstance();
        fileChooser.setSelectedFile(fileTextField.getFile());
        fileChooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
        fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fileChooser.setFileFilter(fileTextField.getFileFilter());
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
    return new FileChooser(lastLocation == null ? new File(manager.getPropertyUserDir())
        : lastLocation);
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
  public void setParameters(final ConstPeetMetaData metaData) {
    ltfFnOutput.setText(metaData.getName());
    volumeTable.setParameters(metaData);
    referencePanel.setParameters(metaData);
    missingWedgeCompensationPanel.setParameters(metaData);
    maskingPanel.setParameters(metaData);
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
  public void setParameters(final MatlabParam matlabParam, File importDir) {
    iterationTable.setParameters(matlabParam);
    missingWedgeCompensationPanel.setParameters(matlabParam);
    MatlabParam.InitMotlCode initMotlCode = matlabParam.getInitMotlCode();
    if (initMotlCode == null) {
      rbInitMotlFiles.setSelected(true);
    }
    else if (initMotlCode == MatlabParam.InitMotlCode.ZERO) {
      rbInitMotlZero.setSelected(true);
    }
    else if (initMotlCode == MatlabParam.InitMotlCode.X_AND_Z_AXIS) {
      rbInitAlignParticleYAxes.setSelected(true);
    }
    else if (initMotlCode == MatlabParam.InitMotlCode.RANDOM_ROTATIONS) {
      rbInitMotlRandomRotations.setSelected(true);
    }
    else if (initMotlCode == MatlabParam.InitMotlCode.RANDOM_AXIAL_ROTATIONS) {
      rbInitMotlRandomAxialRotations.setSelected(true);
    }
    if (!matlabParam.isAlignedBaseNameEmpty()) {
      cbAlignedBaseName.setSelected(true);
      String alignedBaseName = matlabParam.getAlignedBaseName();
      if (!alignedBaseName.equals(ALIGNED_BASE_NAME)) {
        UIHarness.INSTANCE.openProblemValueMessageDialog(this, "Invalid",
            MatlabParam.ALIGNED_BASE_NAME_KEY, null, ALIGNED_BASE_NAME_LABEL,
            alignedBaseName, ALIGNED_BASE_NAME, null);
      }
    }
    else {
      cbAlignedBaseName.setSelected(false);
    }
    cbFlgStrictSearchLimits.setSelected(matlabParam.isFlgStrictSearchLimits());
    cbFlgNoReferenceRefinement.setSelected(matlabParam.isFlgNoReferenceRefinement());
    ltfLowCutoff.setText(matlabParam.getLowCutoffCutoff());
    ltfLowCutoffSigma.setText(matlabParam.getLowCutoffSigma());
    lsDebugLevel.setValue(matlabParam.getDebugLevel());
    ltfLstThresholdsStart.setText(matlabParam.getLstThresholdsStart());
    ltfLstThresholdsIncrement.setText(matlabParam.getLstThresholdsIncrement());
    ltfLstThresholdsEnd.setText(matlabParam.getLstThresholdsEnd());
    ltfLstThresholdsAdditional.setText(matlabParam.getLstThresholdsAdditional());
    cbRefFlagAllTom.setSelected(!matlabParam.isRefFlagAllTom());
    cbLstFlagAllTom.setSelected(!matlabParam.isLstFlagAllTom());
    // particle per cpu
    ConstEtomoNumber number = matlabParam.getParticlePerCPU();
    if (lsParticlePerCPU.isInRange(number)) {
      lsParticlePerCPU.setValue(number);
    }
    else {
      UIHarness.INSTANCE.openProblemValueMessageDialog(this, "Unknown",
          MatlabParam.PARTICLE_PER_CPU_KEY, null, PARTICLE_PER_CPU_LABEL,
          number.toString(), String.valueOf(MatlabParam.PARTICLE_PER_CPU_DEFAULT), null);
      lsParticlePerCPU.setValue(MatlabParam.PARTICLE_PER_CPU_DEFAULT);
    }
    yAxisTypePanel.setParameters(matlabParam);
    volumeTable.setParameters(matlabParam, rbInitMotlFiles.isSelected(),
        missingWedgeCompensationPanel.isTiltRangeRequired(),
        missingWedgeCompensationPanel.isTiltRangeMultiAxes(), importDir);
    sphericalSamplingForThetaAndPsiPanel.setParameters(matlabParam);
    maskingPanel.setParameters(matlabParam);
    cbflgAlignAverages.setSelected(matlabParam.isFlgAlignAverages());
    cbFlgAbsValue.setSelected(matlabParam.isFlgAbsValue());
    ltfSelectClassID.setText(matlabParam.getSelectClassID());
    updateDisplay();
    referencePanel.setParameters(matlabParam);
    updateDisplay();
  }

  public boolean getParameters(final MatlabParam matlabParam, final boolean forRun,
      final boolean doValidation) {
    if (!matlabParam.validate(forRun)) {
      return false;
    }
    matlabParam.clear();
    volumeTable.getParameters(matlabParam);
    iterationTable.getParameters(matlabParam);
    try {
      matlabParam.setFnOutput(ltfFnOutput.getText(doValidation));
      if (!referencePanel.getParameters(matlabParam, forRun)) {
        return false;
      }
      missingWedgeCompensationPanel.getParameters(matlabParam, doValidation);
      matlabParam.setInitMotlCode(((RadioButton.RadioButtonModel) bgInitMotl
          .getSelection()).getEnumeratedType());
      if (cbAlignedBaseName.isSelected()) {
        matlabParam.setAlignedBaseName(ALIGNED_BASE_NAME);
      }
      else {
        matlabParam.resetAlignedBaseName();
      }
      matlabParam.setFlgStrictSearchLimits(cbFlgStrictSearchLimits.isSelected());
      matlabParam.setFlgNoReferenceRefinement(cbFlgNoReferenceRefinement.isSelected());
      matlabParam.setLowCutoff(ltfLowCutoff.getText(doValidation),
          ltfLowCutoffSigma.getText(doValidation));
      matlabParam.setDebugLevel(lsDebugLevel.getValue());
      matlabParam.setLstThresholdsStart(ltfLstThresholdsStart.getText(doValidation));
      matlabParam.setLstThresholdsIncrement(ltfLstThresholdsIncrement
          .getText(doValidation));
      matlabParam.setLstThresholdsEnd(ltfLstThresholdsEnd.getText(doValidation));
      matlabParam.setLstThresholdsAdditional(ltfLstThresholdsAdditional
          .getText(doValidation));
      matlabParam.setRefFlagAllTom(!cbRefFlagAllTom.isSelected());
      matlabParam.setLstFlagAllTom(!cbLstFlagAllTom.isSelected());
      matlabParam.setParticlePerCPU(lsParticlePerCPU.getValue());
      yAxisTypePanel.getParameters(matlabParam);
      if (!sphericalSamplingForThetaAndPsiPanel.getParameters(matlabParam, doValidation)) {
        return false;
      }
      if (!maskingPanel.getParameters(matlabParam, doValidation)) {
        return false;
      }
      matlabParam.setFlgAlignAverages(cbflgAlignAverages.isSelected());
      matlabParam.setFlgAbsValue(cbFlgAbsValue.isSelected());
      matlabParam.setSelectClassID(ltfSelectClassID.getText(doValidation));
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
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

  public String getFnOutput(final boolean doValidation)
      throws FieldValidationFailedException {
    return ltfFnOutput.getText(doValidation);
  }

  public void setDirectory(final String directory) {
    ltfDirectory.setText(directory);
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

  private void setTooltipText() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.PEET_PRM, axisID);
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
    pnlInitMotl.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        MatlabParam.InitMotlCode.KEY));
    ReadOnlySection section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        MatlabParam.InitMotlCode.KEY);
    rbInitMotlZero.setToolTipText(section);
    rbInitAlignParticleYAxes.setToolTipText(section);
    rbInitMotlRandomRotations.setToolTipText(section);
    rbInitMotlRandomAxialRotations.setToolTipText(section);

    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        MatlabParam.REF_FLAG_ALL_TOM_KEY);
    cbRefFlagAllTom.setToolTipText(section, "0");

    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        MatlabParam.LST_FLAG_ALL_TOM_KEY);
    cbLstFlagAllTom.setToolTipText(section, "0");

    cbflgAlignAverages.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        MatlabParam.FLG_ALIGN_AVERAGES_KEY));

    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        MatlabParam.FLG_ABS_VALUE_KEY);
    cbFlgAbsValue.setToolTipText(section, "1");
    cbFlgNoReferenceRefinement.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        MatlabParam.FLG_NO_REFERENCE_REFINEMENT_KEY));

    ltfDirectory.setToolTipText("The directory which will contain the parameter and "
        + "project files, logs, intermediate files, and results. Data files "
        + "can also be located in this directory, but are not required to be.");
    ltfFnOutput.setToolTipText("The base name of the output files for the average "
        + "volumes, the reference volumes, and the transformation parameters.");
    btnRun.setToolTipText("Perform the alignment search and create averaged volumes.");
    btnAvgVol.setToolTipText("Open the computed averages in 3dmod.");
    btnRef.setToolTipText("Open the references in 3dmod.");
    btnAverageAll.setToolTipText("Recompute the averaged volumes");
    rbInitMotlFiles
        .setToolTipText("Use the Initial MOTL file(s) specified in the Volume "
            + "Table.");
    lsParticlePerCPU.setToolTipText("The maximum number of particles distributed "
        + "simultaneously to a single CPU during parallel processing.");
    cbAlignedBaseName.setToolTipText("Save individual aligned particles to "
        + "files aligned*.mrc.");
    cbFlgStrictSearchLimits
        .setToolTipText("When checked, the overall change for any parameter will be "
            + "limited to the largest change specified at any single iteration.");
    ltfLowCutoff
        .setToolTipText("The normalized frequency below which low frequencies will be "
            + "attenuated.  Values less <= 0 disable low frequency filtering.");
    ltfLowCutoffSigma
        .setToolTipText("An optional parameter which defines the transition "
            + "width of the low frequency filter.");
    lsDebugLevel.setToolTipText("Larger numbers result in more debug information in the "
        + "log files.");
    String tooltip = "Start, Incr, and End determine the numbers of particles in an "
        + "arithmetic sequence for which averages will be created.  I.e. "
        + "averages will be created containing Start particles, Start + Incr, "
        + "and so on up to End.";
    ltfLstThresholdsStart.setToolTipText(tooltip);
    ltfLstThresholdsIncrement.setToolTipText(tooltip);
    ltfLstThresholdsEnd.setToolTipText(tooltip);
    ltfLstThresholdsAdditional
        .setToolTipText("Additional numbers of particles for which averages are "
            + "desired.  Values must be listed in increasing order and must be "
            + "larger than End.");
    ltfSelectClassID
        .setToolTipText("Restrict averaging to members of the specified classes. This is "
            + "useful only when the motive list contains class numbers (e.g. generated "
            + "by clusterPca). WARNING: if accidentally set when running a new alignment "
            + "(or at any other time when class numbers have not been assigned in the "
            + "motive list), you will get no particles in the new averages / "
            + "references.  Format: a comma or space separated list of integers and/or "
            + "descriptions (start:optional increment:end)");
  }

  private void setDefaults() {
    lsDebugLevel.setValue(MatlabParam.DEBUG_LEVEL_DEFAULT);
    ltfLowCutoff.setText(MatlabParam.LOW_CUTOFF_DEFAULT);
    referencePanel.setDefaults();
    missingWedgeCompensationPanel.setDefaults();
    sphericalSamplingForThetaAndPsiPanel.setDefaults();
    lsParticlePerCPU.setValue(MatlabParam.PARTICLE_PER_CPU_DEFAULT);
    maskingPanel.setDefaults();
  }

  private void createSetupPanel() {
    // panels
    JPanel pnlProject = new JPanel();
    JPanel pnlReferenceAndMissingWedgeCompensation = new JPanel();
    JPanel pnlInitMotlAndYAxisType = new JPanel();
    JPanel pnlInitMotlX = new JPanel();
    // tab panel
    pnlSetup.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetup.setBorder(BorderFactory.createEtchedBorder());
    // body
    pnlSetupBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlSetupBody.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlSetupBody.add(pnlProject);
    pnlSetupBody.add(fixPathsPanel.getRootComponent());
    pnlSetupBody.add(volumeTable.getContainer());
    pnlSetupBody.add(pnlReferenceAndMissingWedgeCompensation);
    pnlSetupBody.add(maskingPanel.getComponent());
    pnlSetupBody.add(pnlInitMotlAndYAxisType);
    // project
    pnlProject.setLayout(new BoxLayout(pnlProject, BoxLayout.X_AXIS));
    pnlProject.add(ltfDirectory.getContainer());
    pnlProject.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlProject.add(ltfFnOutput.getContainer());
    pnlProject.add(Box.createHorizontalStrut(20));
    // reference and missing wedge compensation
    pnlReferenceAndMissingWedgeCompensation.setLayout(new BoxLayout(
        pnlReferenceAndMissingWedgeCompensation, BoxLayout.X_AXIS));
    pnlReferenceAndMissingWedgeCompensation.add(referencePanel.getComponent());
    pnlReferenceAndMissingWedgeCompensation.add(missingWedgeCompensationPanel
        .getComponent());
    // init MOTL and Y axis type
    pnlInitMotlAndYAxisType.setLayout(new BoxLayout(pnlInitMotlAndYAxisType,
        BoxLayout.X_AXIS));
    pnlInitMotlAndYAxisType.add(yAxisTypePanel.getComponent());
    pnlInitMotlAndYAxisType.add(Box.createHorizontalGlue());
    pnlInitMotlAndYAxisType.add(pnlInitMotlX);
    // init motl x
    pnlInitMotlX.setLayout(new BoxLayout(pnlInitMotlX, BoxLayout.X_AXIS));
    pnlInitMotlX.setBorder(new EtchedBorder(FieldLabels.INIT_MOTL_LABEL).getBorder());
    pnlInitMotlX.add(pnlInitMotl);
    pnlInitMotlX.add(Box.createRigidArea(FixedDim.x167_y0));
    // init MOTL
    pnlInitMotl.setLayout(new BoxLayout(pnlInitMotl, BoxLayout.Y_AXIS));
    pnlInitMotl.add(rbInitMotlZero.getComponent());
    pnlInitMotl.add(rbInitAlignParticleYAxes.getComponent());
    pnlInitMotl.add(rbInitMotlFiles.getComponent());
    pnlInitMotl.add(rbInitMotlRandomRotations.getComponent());
    pnlInitMotl.add(rbInitMotlRandomAxialRotations.getComponent());
  }

  private void createRunPanel() {
    // init
    ltfLowCutoffSigma.setText(MatlabParam.LOW_CUTOFF_SIGMA_DEFAULT);
    // panels
    SpacedPanel pnlLstThresholds = SpacedPanel.getInstance();
    JPanel pnlEqualNumber = new JPanel();
    JPanel pnlOptional = new JPanel();
    JPanel pnlOptionalLeft = new JPanel();
    JPanel pnlOptionalRight = new JPanel();
    JPanel pnlButton = new JPanel();
    JPanel pnlRefFlagAllTom = new JPanel();
    JPanel pnlLstFlagAllTom = new JPanel();
    JPanel pnlLowCutoff = new JPanel();
    // initialize
    ltfLstThresholdsStart.setPreferredWidth(60);
    ltfLstThresholdsIncrement.setPreferredWidth(60);
    ltfLstThresholdsEnd.setPreferredWidth(60);
    lsParticlePerCPU.setPreferredWidth(60);
    lsDebugLevel.setPreferredWidth(40);
    cbFlgAbsValue.setSelected(MatlabParam.FLG_ABS_VALUE_DEFAULT);
    // panel for tab
    pnlRun.setLayout(new BoxLayout(pnlRun, BoxLayout.Y_AXIS));
    pnlRun.setBorder(BorderFactory.createEtchedBorder());
    // body
    pnlRunBody.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRunBody.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRunBody.add(iterationTable.getContainer());
    pnlRunBody.add(sphericalSamplingForThetaAndPsiPanel.getComponent());
    pnlRunBody.add(pnlLstThresholds);
    pnlRunBody.add(pnlEqualNumber);
    pnlRunBody.add(pnlOptional);
    pnlRunBody.add(Box.createRigidArea(FixedDim.x0_y20));
    pnlRunBody.add(pnlButton);
    // lstThresholds
    pnlLstThresholds.setBoxLayout(BoxLayout.X_AXIS);
    pnlLstThresholds.setBorder(new EtchedBorder(LST_THRESHOLDS_LABEL).getBorder());
    pnlLstThresholds.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlLstThresholds.add(ltfLstThresholdsStart.getContainer());
    pnlLstThresholds.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlLstThresholds.add(ltfLstThresholdsIncrement.getContainer());
    pnlLstThresholds.add(Box.createHorizontalGlue());
    pnlLstThresholds.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlLstThresholds.add(ltfLstThresholdsEnd.getContainer());
    pnlLstThresholds.add(Box.createRigidArea(FixedDim.x20_y0));
    pnlLstThresholds.add(ltfLstThresholdsAdditional.getContainer());
    pnlLstThresholds.add(Box.createHorizontalGlue());
    // equals numbers
    pnlEqualNumber.setLayout(new BoxLayout(pnlEqualNumber, BoxLayout.X_AXIS));
    pnlEqualNumber.setBorder(new EtchedBorder(
        "Use Equal Numbers of Particles from All Tomograms").getBorder());
    pnlEqualNumber.add(Box.createHorizontalGlue());
    pnlEqualNumber.add(pnlLstFlagAllTom);
    pnlEqualNumber.add(pnlRefFlagAllTom);
    // LstFlagAllTom
    pnlLstFlagAllTom.setLayout(new BoxLayout(pnlLstFlagAllTom, BoxLayout.X_AXIS));
    pnlLstFlagAllTom.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlLstFlagAllTom.add(cbLstFlagAllTom);
    pnlLstFlagAllTom.add(Box.createHorizontalGlue());
    // RefFlagAllTom
    pnlRefFlagAllTom.setLayout(new BoxLayout(pnlRefFlagAllTom, BoxLayout.X_AXIS));
    pnlRefFlagAllTom.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRefFlagAllTom.add(cbRefFlagAllTom);
    pnlRefFlagAllTom.add(Box.createHorizontalGlue());
    // optional
    pnlOptional.setLayout(new BoxLayout(pnlOptional, BoxLayout.X_AXIS));
    pnlOptional.setBorder(new EtchedBorder("Optional / Advanced Features").getBorder());
    pnlOptional.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlOptional.add(pnlOptionalLeft);
    pnlOptional.add(Box.createRigidArea(FixedDim.x30_y0));
    pnlOptional.add(pnlOptionalRight);
    pnlOptional.add(Box.createRigidArea(FixedDim.x15_y0));
    // optional left
    pnlOptionalLeft.setLayout(new BoxLayout(pnlOptionalLeft, BoxLayout.Y_AXIS));
    pnlOptionalLeft.add(cbflgAlignAverages);
    pnlOptionalLeft.add(cbFlgAbsValue);
    pnlOptionalLeft.add(cbAlignedBaseName);
    pnlOptionalLeft.add(cbFlgStrictSearchLimits);
    pnlOptionalLeft.add(cbFlgNoReferenceRefinement);
    // optional right
    pnlOptionalRight.setLayout(new GridLayout(4, 1, 0, 5));
    pnlOptionalRight.add(lsParticlePerCPU.getContainer());
    pnlOptionalRight.add(lsDebugLevel.getContainer());
    pnlOptionalRight.add(pnlLowCutoff);
    pnlOptionalRight.add(ltfSelectClassID.getContainer());
    // low cutoff
    pnlLowCutoff.setLayout(new BoxLayout(pnlLowCutoff, BoxLayout.X_AXIS));
    pnlLowCutoff.add(ltfLowCutoff.getContainer());
    pnlLowCutoff.add(Box.createRigidArea(FixedDim.x15_y0));
    pnlLowCutoff.add(ltfLowCutoffSigma.getContainer());
    // button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createRigidArea(FixedDim.x30_y0));
    pnlButton.add(btnRun.getComponent());
    pnlButton.add(Box.createRigidArea(FixedDim.x40_y0));
    pnlButton.add(btnAvgVol.getComponent());
    pnlButton.add(Box.createRigidArea(FixedDim.x30_y0));
    pnlButton.add(btnRef.getComponent());
    pnlButton.add(Box.createRigidArea(FixedDim.x30_y0));
    pnlButton.add(btnAverageAll.getComponent());
    pnlButton.add(Box.createRigidArea(FixedDim.x30_y0));
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), run3dmodMenuOptions);
  }

  private void action(final String actionCommand,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (actionCommand.equals(btnRun.getActionCommand())) {
      if (validateRun()) {
        manager.peetParser(null, DIALOG_TYPE,
            mediator.getRunMethodForProcessInterface(getProcessingMethod()));
      }
    }
    else if (actionCommand.equals(rbInitMotlZero.getActionCommand())
        || actionCommand.equals(rbInitAlignParticleYAxes.getActionCommand())
        || actionCommand.equals(rbInitMotlRandomRotations.getActionCommand())
        || actionCommand.equals(rbInitMotlRandomAxialRotations.getActionCommand())
        || actionCommand.equals(rbInitMotlFiles.getActionCommand())) {
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
    // Setup tab
    // Must have a directory
    if (ltfDirectory.isEmpty()) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(manager, "Please set the "
          + PeetDialog.DIRECTORY_LABEL + " field.", "Entry Error");
      return false;
    }
    // Must have an output name
    if (ltfFnOutput.isEmpty()) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(manager, "Please set the "
          + PeetDialog.FN_OUTPUT_LABEL + " field.", "Entry Error");
      return false;
    }
    // Validate volume table
    String errorMessage = volumeTable.validateRun(missingWedgeCompensationPanel
        .isTiltRangeRequired());
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, "Entry Error");
      return false;
    }
    // Must either have a volume and particle or a reference file.
    errorMessage = referencePanel.validateRun();
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, "Entry Error");
      return false;
    }
    // Validate missing wedge compensation panel
    errorMessage = missingWedgeCompensationPanel.validateRun();
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, "Entry Error");
      return false;
    }
    // Validate masking
    errorMessage = maskingPanel.validateRun();
    if (errorMessage != null) {
      gotoSetupTab();
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, "Entry Error");
      return false;
    }
    // Run tab
    if (!iterationTable.validateRun()) {
      return false;
    }
    // spherical sampling for theta and psi:
    if (!sphericalSamplingForThetaAndPsiPanel.validateRun()) {
      return false;
    }
    // Number of particles
    boolean startIsEmpty = ltfLstThresholdsStart.isEmpty();
    boolean incrementIsEmpty = ltfLstThresholdsIncrement.isEmpty();
    boolean endIsEmpty = ltfLstThresholdsEnd.isEmpty();
    boolean additionalIsEmpty = ltfLstThresholdsAdditional.isEmpty();
    if (startIsEmpty && incrementIsEmpty) {
      // lst thesholds is required
      if (additionalIsEmpty) {
        UIHarness.INSTANCE.openMessageDialog(manager, LST_THRESHOLDS_LABEL
            + " is required.", "Entry Error");
        return false;
      }
      // check empty list descriptor
      if (!endIsEmpty) {
        UIHarness.INSTANCE.openMessageDialog(manager, "In " + LST_THRESHOLDS_LABEL
            + ", invalid list description.", "Entry Error");
        return false;
      }
    }
    // check list descriptor
    else if (startIsEmpty || endIsEmpty) {
      UIHarness.INSTANCE.openMessageDialog(manager, "In " + LST_THRESHOLDS_LABEL
          + ", invalid list description.", "Entry Error");
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
    mediator.setMethod(this, getProcessingMethod());
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public int getVolumeTableSize() {
    return volumeTable.size();
  }

  /**
   * Enabled/disables fields.  Calls updateDisplay() in subordinate panels.
   */
  public void updateDisplay() {
    // tilt range
    boolean volumeRows = volumeTable.size() > 0;
    referencePanel.updateDisplay();
    missingWedgeCompensationPanel.updateDisplay();
    sphericalSamplingForThetaAndPsiPanel.updateDisplay();
    // iteration table - spherical sampling and FlgRemoveDuplicates
    iterationTable.updateDisplay(!sphericalSamplingForThetaAndPsiPanel
        .isSampleSphereNoneSelected());
    // volume table
    volumeTable.updateDisplay(rbInitMotlFiles.isSelected(),
        missingWedgeCompensationPanel.isTiltRangeRequired(),
        missingWedgeCompensationPanel.isTiltRangeMultiAxes());
    maskingPanel.updateDisplay();
    cbflgAlignAverages
        .setEnabled(yAxisTypePanel.getYAxisType() != MatlabParam.YAxisType.Y_AXIS);
  }

  public boolean isSampleSphere() {
    return !sphericalSamplingForThetaAndPsiPanel.isSampleSphereNoneSelected();
  }

  private void addListeners() {
    PDActionListener actionListener = new PDActionListener(this);
    rbInitMotlZero.addActionListener(actionListener);
    rbInitAlignParticleYAxes.addActionListener(actionListener);
    rbInitMotlRandomRotations.addActionListener(actionListener);
    rbInitMotlRandomAxialRotations.addActionListener(actionListener);
    rbInitMotlFiles.addActionListener(actionListener);
    btnRun.addActionListener(actionListener);
    tabPane.addChangeListener(new TabChangeListener(this));
    btnAvgVol.addActionListener(actionListener);
    btnRef.addActionListener(actionListener);
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
