package etomo.type;

import java.io.File;
import java.util.Properties;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.FortranInputString;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SqueezevolParam;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TransferfidParam;
import etomo.comscript.TrimvolParam;
import etomo.ui.FiducialModelDialog;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
 *
 * <p>Copyright: Copyright (c) 2002-2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.63  2010/05/27 16:49:48  sueh
 * <p> bug# 1378 Added isLambdaForSmoothingListEmpty.
 * <p>
 * <p> Revision 3.61  2010/03/30 00:05:24  sueh
 * <p> bug# 1331 Added posExistsA and B.
 * <p>
 * <p> Revision 3.60  2010/03/27 04:49:32  sueh
 * <p> bug# 1333 Added defaultGpuProcessing.  Added a separate tiltParallel for
 * <p> 3dfindbeads.
 * <p>
 * <p> Revision 3.59  2010/03/05 04:00:15  sueh
 * <p> bug# 1319 Added genExists, genLog, and gen scale parameters.
 * <p>
 * <p> Revision 3.58  2010/03/03 04:57:56  sueh
 * <p> bug# 1311 Added parameters for patch tracking.
 * <p>
 * <p> Revision 3.57  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.56  2010/01/11 23:58:08  sueh
 * <p> bug# 1299 Formatted.
 * <p>
 * <p> Revision 3.55  2009/12/19 01:10:53  sueh
 * <p> bug# 1294 Added lambdaForSmoothing and lambdaForSmoothingList.
 * <p>
 * <p> Revision 3.54  2009/10/23 21:24:16  sueh
 * <p> bug# 1281 Added postExists.
 * <p>
 * <p> Revision 3.53  2009/10/16 21:13:07  sueh
 * <p> bug# 1230 Added postCurTab.
 * <p>
 * <p> Revision 3.52  2009/10/13 17:39:54  sueh
 * <p> bug# 1278 Changed setExcludeProjectionsA and B to
 * <p> setExcludeProjections.  Removed whitespace so that copytomocoms won't
 * <p> failed.
 * <p>
 * <p> Revision 3.51  2009/09/20 21:30:28  sueh
 * <p> bug# 1268 In MetaData constructor set a display value for posBinningB.
 * <p>
 * <p> Revision 3.50  2009/09/01 03:06:34  sueh
 * <p> bug# 1222 Added settings for erase gold with findbeads3d.
 * <p>
 * <p> Revision 3.49  2009/06/05 02:09:20  sueh
 * <p> bug# 1219 Added CONTOURS_ON_ONE_SURFACE_KEY, FLATTEN_KEY,
 * <p> FLATTEN_WARP_KEY, INPUT_KEY, POST_KEY, postFlattenInputTrimVol,
 * <p> postFlattenWarpContoursOnOneSurface, postFlattenWarpSpacingInX,
 * <p> postFlattenWarpSpacingInY, postSqueezeVolInputTrimVol, SPACING_IN_KEY,
 * <p> SQUEEZE_VOL_KEY, TRIM_VOL_KEY, X_KEY, Y_KEY,
 * <p> getPostFlattenWarpSpacingInX, getPostFlattenWarpSpacingInY,
 * <p> isPostFlattenWarpContoursOnOneSurface, isPostFlattenWarpInputTrimVol,
 * <p> isPostSqueezeVolInputTrimVol, setPostFlattenWarpContoursOnOneSurface,
 * <p> setPostFlattenWarpInputTrimVol, setPostFlattenWarpSpacingInX, and
 * <p> setPostFlattenWarpSpacingInY, setPostSqueezeVolInputTrimVol.
 * <p>
 * <p> Revision 3.48  2009/05/02 01:25:25  sueh
 * <p> bug# 1216 Removed B axis raptor data.
 * <p>
 * <p> Revision 3.47  2009/05/02 01:11:02  sueh
 * <p> bug# 1216 Added trackRaptorDiamA and B, trackRaptorMarkA and B,
 * <p> trackRaptorUseRawStackA and B, and trackUseRaptorA and B.
 * <p>
 * <p> Revision 3.46  2009/02/05 23:44:39  sueh
 * <p> bug# 1148 Added tomoGenTrialTomogramNameListA and B.
 * <p>
 * <p> Revision 3.45  2009/02/04 23:30:30  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 3.44  2008/12/10 18:33:24  sueh
 * <p> bug# 1162 Added a manager stamp to setDatasetName.
 * <p>
 * <p> Revision 3.43  2008/12/02 21:20:57  sueh
 * <p> bug# 1157 Added finalStackFiducialDiameterA and B.  Deprecated finalStackBetterRadiusA and B.  Getting better radius for backwards compatibility.  Changed revision number to 1.10.
 * <p>
 * <p> Revision 3.42  2008/11/20 01:36:22  sueh
 * <p> bug# 1147, bug# 1149 Added finalStackBetterRadiusA and B, and
 * <p> finalStackPolynomialOrderA and B.
 * <p>
 * <p> Revision 3.41  2008/11/11 23:49:00  sueh
 * <p> bug# 1149 Changed tomoGenBinning to finalStackBinning.  Fixed the
 * <p> names of the CtfCorrectionParallel variables (should start with
 * <p> "finalStack", not "stack").  Changed latestRevionNumber to 1.9.  Added backward compatibility for 1.8.
 * <p>
 * <p> Revision 3.40  2008/10/27 19:24:58  sueh
 * <p> bug# 1141 Add stackCtfCorrectionParallelA and B.
 * <p>
 * <p> Revision 3.39  2007/12/26 22:18:13  sueh
 * <p> bug# 1052 Changed the parameter type of set functions called by SetupDialog
 * <p> so that conversions will be done in MetaData instead of the ui.
 * <p>
 * <p> Revision 3.38  2007/12/13 01:13:01  sueh
 * <p> bug# 1056 Removed the Storables inner class from TiltParam.
 * <p>
 * <p> Revision 3.37  2007/12/10 22:36:58  sueh
 * <p> bug# 1041 Made Const class an interface so inheritance can come from
 * <p> BaseMetaData.
 * <p>
 * <p> Revision 3.36  2007/08/16 16:33:57  sueh
 * <p> bug# 1035 Added sizeToOutputInXandYA and B.
 * <p>
 * <p> Revision 3.35  2007/03/21 19:44:34  sueh
 * <p> bug# 964 Metadata should always use local strings as keys, otherwise
 * <p> backwards compatibility issues might be inadventently created.
 * <p>
 * <p> Revision 3.34  2007/03/07 21:09:17  sueh
 * <p> bug# 981 Added noBeamTiltSelected, fixedBeamTiltSelected, and fixedBeamTilt.
 * <p>
 * <p> Revision 3.33  2007/03/03 01:01:02  sueh
 * <p> bug# 973 Added targetPatchSizeXandY and numberOfLocalPatchesXandY.
 * <p>
 * <p> Revision 3.32  2007/02/05 23:29:47  sueh
 * <p> bug# 962 Changed revisionNumber to an EtomoNumber, so that it is comparable.
 * <p>
 * <p> Revision 3.31  2006/09/19 22:34:02  sueh
 * <p> bug# 920 Added first and second axisPrepends for storing axis-level values.
 * <p> Added TiltParam.Storables for A and B.
 * <p>
 * <p> Revision 3.30  2006/05/16 21:32:00  sueh
 * <p> bug# 856 Removed unused function getCombineParams.
 * <p>
 * <p> Revision 3.29  2006/05/11 19:57:53  sueh
 * <p> bug# 838 Added sample thickness.
 * <p>
 * <p> Revision 3.28  2006/03/23 19:46:09  sueh
 * <p> bug# 609 Improving the error message when a dual axis image stack does
 * <p> not end in a.st.
 * <p>
 * <p> Revision 3.27  2005/12/13 02:27:49  sueh
 * <p> bug# 773 Added defaultParallel
 * <p>
 * <p> Revision 3.26  2005/10/27 00:33:10  sueh
 * <p> bug# 725 Added bStackProcessed.
 * <p>
 * <p> Revision 3.25  2005/10/12 21:27:02  sueh
 * <p> bug# 532 Changed the parallel booleans to EtomoBoolean2 so that they
 * <p> can remember whether they where set or not.  The default for the parallel
 * <p> checkboxes is based on the existance and validity of cpu.adoc.  Use the
 * <p> default when the parallel meta data variable is null.
 * <p>
 * <p> Revision 3.24  2005/09/29 18:46:24  sueh
 * <p> bug# 532 Saving the state of the parallel checkbox states.
 * <p>
 * <p> Revision 3.23  2005/09/16 17:50:45  sueh
 * <p> bug# 532 Added combineParallelProcess.
 * <p>
 * <p> Revision 3.22  2005/08/22 17:10:37  sueh
 * <p> bug# 532 Added a member variable to save the setting of the gen tomo
 * <p> parallel process checkbox.
 * <p>
 * <p> Revision 3.21  2005/07/29 00:53:36  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.20  2005/06/11 02:42:26  sueh
 * <p> bug# 583, bug# 584  Storing screen binning for Tomo Pos and Tomo Gen
 * <p> in MetaData separately (Tomo Pos default is 3).
 * <p>
 * <p> Revision 3.19  2005/05/10 03:16:51  sueh
 * <p> bug# 659 Default useLocalAlignmentsA and B to true.
 * <p>
 * <p> Revision 3.18  2005/03/02 20:25:11  sueh
 * <p> bug# 533 Added adjustedFocus.  Only used with montaging and mag
 * <p> gradient correction.
 * <p>
 * <p> Revision 3.17  2005/03/02 00:12:11  sueh
 * <p> bug# 611 Added mag gradients correction file.
 * <p>
 * <p> Revision 3.16  2005/02/19 00:09:57  sueh
 * <p> bug# 606 Removed MetaData (Setup) zfactors, fiducialess, wholetomogram,
 * <p> and localalignments.  Add them for A and B.
 * <p>
 * <p> Revision 3.15  2005/02/15 21:05:46  sueh
 * <p> bug# 603 Removed SectionType (single or serial sections).
 * <p>
 * <p> Revision 3.14  2005/01/21 23:28:50  sueh
 * <p> bug# 509 bug# 591  Removed transferfidNumberViews.  Added
 * <p> transferfidParamA and transferfidParamB to hold the user-modifiable
 * <p> transferfid values.  Removed initializeTransferfidParam() and added
 * <p> getTransferfidParamAFields() and getTransferfidParamBFields() to get/set
 * <p> storable fields in TransferfidParam.
 * <p>
 * <p> Revision 3.13  2005/01/12 00:43:43  sueh
 * <p> bug# 579 Reusing useLocalAlignments, which isn't being used for anything
 * <p> Reseting useLocalAlignments to true;
 * <p>
 * <p> Revision 3.12  2005/01/11 18:07:38  sueh
 * <p> bug# 578 Added useZFactors.
 * <p>
 * <p> Revision 3.11  2004/12/14 21:47:48  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 3.10  2004/12/07 22:47:47  sueh
 * <p> bug# 564 Added TomogramState member variable.
 * <p>
 * <p> Revision 3.9  2004/12/02 18:29:29  sueh
 * <p> bug# 557 Added a SqueezevolParam instance to be stored in the .edf file.
 * <p>
 * <p> Revision 3.8  2004/11/19 23:35:45  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.7.4.1  2004/09/29 19:30:10  sueh
 * <p> bug# 520 Moved Storable to BaseMetaData.  Moved store() to
 * <p> ConstMetaData and BaseMetaData.  Moved revision and axisType to
 * <p> baseMEtaData.
 * <p>
 * <p> Revision 3.7  2004/06/22 02:02:35  sueh
 * <p> bug# 441 Added TrimvolParam
 * <p>
 * <p> Revision 3.6  2004/06/01 18:55:27  rickg
 * <p> Bug #391 whole tomogram sampling state implementation
 * <p>
 * <p> Revision 3.5  2004/05/25 23:59:54  sueh
 * <p> bug# 355 when axis type is not available it should be set to
 * <p> "not set"
 * <p>
 * <p> Revision 3.4  2004/04/06 03:00:40  rickg
 * <p> Updated imageRotation to store axis separately
 * <p>
 * <p> Revision 3.3  2004/02/24 18:53:22  sueh
 * <p> bug# 385 added resetToDefault() - for defaults need before
 * <p> MetaData is loaded
 * <p>
 * <p> Revision 3.2  2004/02/21 00:27:44  sueh
 * <p> bug# 386 save/load distortionFile and binning
 * <p>
 * <p> Revision 3.1  2004/02/20 23:45:37  sueh
 * <p> bug# 386 added setDistortionFile() and setBinning()
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.8  2003/11/06 22:44:07  sueh
 * <p> cleaning up tasks
 * <p>
 * <p> Revision 2.7  2003/10/08 22:03:21  sueh
 * <p> Bug263
 * <p> UI Changes
 * <p> Removed data source from Setup dialog.  Removed setDataSource() from 
 * <p> MetaData.
 * <p> DataSource is always the default (CCD) in ConstMetaData
 * <p> Grayed out ViewType.
 * <p>
 * <p> Revision 2.6  2003/09/26 19:46:16  sueh
 * <p> bug223 removed task marks
 * <p>
 * <p> Revision 2.5  2003/09/26 19:43:48  sueh
 * <p> bug223 no field should be persistant.  Changed MetaData.
 * <p> Added TransferfidNumberViews.
 * <p> Changed the done fine allignment and open fine allignment functions
 * <p> to work with MetaData
 * <p>
 * <p> Revision 2.4  2003/05/07 17:53:59  rickg
 * <p> Working direcotry is no longer stored in the metadata
 * <p> System property user.dir now defines the working directory
 * <p>
 * <p> Revision 2.3  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.2  2003/03/18 23:46:52  rickg
 * <p> Added method to get CombineParams reference
 * <p>
 * <p> Revision 2.1  2003/01/27 15:25:45  rickg
 * <p> Static function fix
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.4  2002/10/22 23:25:44  rickg
 * <p> setFilesetName method now sets the working directory as well
 * <p> by parsing the string argument
 * <p>
 * <p> Revision 1.3  2002/10/07 22:28:47  rickg
 * <p> removed unused imports
 * <p>
 * <p> Revision 1.2  2002/09/30 23:49:04  rickg
 * <p> Reformatted after emacs trashed it.
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public final class MetaData extends BaseMetaData implements ConstMetaData {
  public static final String rcsid = "$Id$";

  //Strings and keys must not change without provisions for backwards
  //capatibility.

  private static final String latestRevisionNumber = "1.11";
  private static final String newTomogramTitle = "Setup Tomogram";

  private static final String TOMO_GEN_A_TILT_PARALLEL_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName()
      + AxisID.FIRST.getExtension().toUpperCase() + ".Tilt.Parallel";
  private static final String TOMO_GEN_B_TILT_PARALLEL_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName()
      + AxisID.SECOND.getExtension().toUpperCase() + ".Tilt.Parallel";
  private static final String FINAL_STACK_A_CTF_CORRECTION_PARALLEL_GROUP = DialogType.FINAL_ALIGNED_STACK
      .getStorableName()
      + AxisID.FIRST.getExtension().toUpperCase() + ".CtfCorrection.Parallel";
  private static final String FINAL_STACK_B_CTF_CORRECTION_PARALLEL_GROUP = DialogType.FINAL_ALIGNED_STACK
      .getStorableName()
      + AxisID.SECOND.getExtension().toUpperCase() + ".CtfCorrection.Parallel";
  private static final String COMBINE_VOLCOMBINE_PARALLEL_GROUP = DialogType.TOMOGRAM_COMBINATION
      .getStorableName()
      + ".Volcombine.Parallel";
  private static final String B_STACK_PROCESSED_GROUP = "BStackProcessed";
  private static final int DEFAULT_SAMPLE_THICKNESS = 200;
  private static final String FIDUCIALESS_KEY = "Fiducialess";
  private static final String THICKNESS_KEY = "THICKNESS";
  private static final String FINAL_STACK_BINNING_A_BACKWARD_COMPATABILITY_1_8 = "TomoGenBinningA";
  private static final String FINAL_STACK_BINNING_B_BACKWARD_COMPATABILITY_1_8 = "TomoGenBinningB";

  //Axis keys
  private static final String FIRST_AXIS_KEY = "A";
  private static final String SECOND_AXIS_KEY = "B";

  //Dialog keys
  private static final String TRACK_KEY = "Track";
  private static final String FINE_KEY = "Fine";
  private static final String POS_KEY = "Pos";
  private static final String STACK_KEY = "Stack";
  private static final String GEN_KEY = "Gen";
  private static final String POST_KEY = "Post";

  //Panel keys
  private static final String NEWSTACK_OR_BLENDMONT_KEY = "NewstackOrBlendmont";
  private static final String ERASE_GOLD_KEY = "EraseGold";
  private static final String FLATTEN_KEY = "Flatten";
  private static final String FLATTEN_WARP_KEY = "FlattenWarp";
  private static final String RAPTOR_KEY = "Raptor";
  private static final String TRIM_VOL_KEY = "TrimVol";
  private static final String SQUEEZE_VOL_KEY = "SqueezeVol";

  //FieldInterface keys
  private static final String BINNING_KEY = "Binning";
  private static final String CONTOURS_ON_ONE_SURFACE_KEY = "ContoursOnOneSurface";
  private static final String DIAM_KEY = "Diam";
  private static final String INPUT_KEY = "Input";
  private static final String MARK_KEY = "Mark";
  private static final String MODEL_USE_FID_KEY = "ModelUseFid";
  private static final String RAW_STACK_KEY = "RawStack";
  private static final String SIZE_TO_OUTPUT_IN_X_AND_Y_KEY = "SizeToOutputInXandY";
  private static final String SPACING_IN_KEY = "SpacingIn";
  private static final String USE_KEY = "Use";
  private static final String X_KEY = "X";
  private static final String Y_KEY = "Y";

  private static final String TILT_3D_FIND_A_TILT_PARALLEL_KEY = STACK_KEY
      + ".A.Tilt.Parallel";
  private static final String TILT_3D_FIND_B_TILT_PARALLEL_KEY = STACK_KEY
      + ".B.Tilt.Parallel";

  //Defaults
  private static final boolean ERASE_GOLD_MODEL_USE_FID_DEFAULT = true;

  private final ApplicationManager manager;

  private String datasetName = "";
  private String backupDirectory = "";
  private String distortionFile = "";
  private String magGradientFile = "";

  private DataSource dataSource = DataSource.CCD;
  private ViewType viewType = ViewType.SINGLE_VIEW;

  private double pixelSize = Double.NaN;
  private boolean useLocalAlignmentsA = true;
  private boolean useLocalAlignmentsB = true;
  private double fiducialDiameter = Double.NaN;
  private float imageRotationA = Float.NaN;
  private float imageRotationB = Float.NaN;
  private int binning = 1;

  private boolean fiducialessAlignmentA = false;
  private boolean fiducialessAlignmentB = false;
  private boolean wholeTomogramSampleA = false;
  private boolean wholeTomogramSampleB = false;

  //  Axis specific data
  private TiltAngleSpec tiltAngleSpecA = new TiltAngleSpec();
  private String excludeProjectionsA = "";

  private TiltAngleSpec tiltAngleSpecB = new TiltAngleSpec();
  private String excludeProjectionsB = "";
  private EtomoBoolean2 useZFactorsA = new EtomoBoolean2("UseZFactorsA");
  private EtomoBoolean2 useZFactorsB = new EtomoBoolean2("UseZFactorsB");
  private EtomoBoolean2 adjustedFocusA = new EtomoBoolean2("AdjustedFocusA");
  private EtomoBoolean2 adjustedFocusB = new EtomoBoolean2("AdjustedFocusB");

  private boolean comScriptsCreated = false;

  private CombineParams combineParams;
  private final TrimvolParam trimvolParam;
  private final SqueezevolParam squeezevolParam;
  private final TransferfidParam transferfidParamA;
  private final TransferfidParam transferfidParamB;
  private final EtomoBoolean2 defaultParallel = new EtomoBoolean2(
      "DefaultParallel");
  private EtomoBoolean2 tomoGenTiltParallelA = null;
  private EtomoBoolean2 tomoGenTiltParallelB = null;
  private EtomoBoolean2 tilt3dFindTiltParallelA = null;
  private EtomoBoolean2 tilt3dFindTiltParallelB = null;
  private EtomoBoolean2 finalStackCtfCorrectionParallelA = null;
  private EtomoBoolean2 finalStackCtfCorrectionParallelB = null;
  private EtomoBoolean2 combineVolcombineParallel = null;
  private EtomoBoolean2 bStackProcessed = null;
  private StringBuffer message = new StringBuffer();
  private final EtomoNumber sampleThicknessA = new EtomoNumber(AxisID.FIRST
      .toString()
      + '.' + ProcessName.SAMPLE + '.' + THICKNESS_KEY);
  private final EtomoNumber sampleThicknessB = new EtomoNumber(AxisID.SECOND
      .toString()
      + '.' + ProcessName.SAMPLE + '.' + THICKNESS_KEY);
  private String firstAxisPrepend = null;
  private String secondAxisPrepend = null;
  private final EtomoBoolean2 defaultGpuProcessing = new EtomoBoolean2(
      "DefaultGpuProcessing");
  /**
   * @version 1.8
   */
  private final EtomoBoolean2 fiducialessA = new EtomoBoolean2("A."
      + FIDUCIALESS_KEY);
  /**
   * @version 1.8
   */
  private final EtomoBoolean2 fiducialessB = new EtomoBoolean2("B."
      + FIDUCIALESS_KEY);
  private String targetPatchSizeXandY = TiltalignParam.TARGET_PATCH_SIZE_X_AND_Y_DEFAULT;//backwards compatibility
  private String numberOfLocalPatchesXandY = TiltalignParam.NUMBER_OF_LOCAL_PATCHES_X_AND_Y_DEFAULT;
  private final EtomoBoolean2 noBeamTiltSelectedA = new EtomoBoolean2(
      AxisID.FIRST.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName() + ".NoBeamTiltSelected");
  private final EtomoBoolean2 fixedBeamTiltSelectedA = new EtomoBoolean2(
      AxisID.FIRST.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName()
          + ".FixedBeamTiltSelected");
  private final EtomoNumber fixedBeamTiltA = new EtomoNumber(
      EtomoNumber.Type.FLOAT, AxisID.FIRST.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName() + ".FixedBeamTilt");
  private final EtomoBoolean2 noBeamTiltSelectedB = new EtomoBoolean2(
      AxisID.SECOND.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName() + ".NoBeamTiltSelected");
  private final EtomoBoolean2 fixedBeamTiltSelectedB = new EtomoBoolean2(
      AxisID.SECOND.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName()
          + ".FixedBeamTiltSelected");
  private final EtomoNumber fixedBeamTiltB = new EtomoNumber(
      EtomoNumber.Type.FLOAT, AxisID.SECOND.getExtension() + "."
          + DialogType.FINE_ALIGNMENT.getStorableName() + ".FixedBeamTilt");
  private final FortranInputString sizeToOutputInXandYA = new FortranInputString(
      2);
  private final FortranInputString sizeToOutputInXandYB = new FortranInputString(
      2);
  /**
   * @deprecated in MetaData 1.10
   */
  private StringProperty finalStackBetterRadiusA = new StringProperty(
      DialogType.FINAL_ALIGNED_STACK.getStorableName() + "."
          + AxisID.FIRST.getExtension() + "." + "BetterRadius");
  /**
   * @deprecated in MetaData 1.10
   */
  private StringProperty finalStackBetterRadiusB = new StringProperty(
      DialogType.FINAL_ALIGNED_STACK.getStorableName() + "."
          + AxisID.SECOND.getExtension() + "." + "BetterRadius");
  /**
   * Added in MetaData 1.10
   * fiducial diameter in pixels
   */
  private EtomoNumber finalStackFiducialDiameterA = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, DialogType.FINAL_ALIGNED_STACK.getStorableName()
          + "." + AxisID.FIRST.getExtension() + "." + "FiducialDiameter");
  /**
   * Added in MetaData 1.10
   * fiducial diameter in pixels
   */
  private EtomoNumber finalStackFiducialDiameterB = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, DialogType.FINAL_ALIGNED_STACK.getStorableName()
          + "." + AxisID.SECOND.getExtension() + "." + "FiducialDiameter");
  private EtomoNumber finalStackPolynomialOrderA = new EtomoNumber(
      DialogType.FINAL_ALIGNED_STACK.getStorableName() + "."
          + AxisID.FIRST.getExtension() + "." + "PolynomialOrder");
  private EtomoNumber finalStackPolynomialOrderB = new EtomoNumber(
      DialogType.FINAL_ALIGNED_STACK.getStorableName() + "."
          + AxisID.SECOND.getExtension() + "." + "PolynomialOrder");
  private IntKeyList tomoGenTrialTomogramNameListA = IntKeyList
      .getStringInstance(DialogType.TOMOGRAM_GENERATION.getStorableName() + "."
          + AxisID.FIRST.getExtension() + "." + "TrialTomogramName");
  private IntKeyList tomoGenTrialTomogramNameListB = IntKeyList
      .getStringInstance(DialogType.TOMOGRAM_GENERATION.getStorableName() + "."
          + AxisID.SECOND.getExtension() + "." + "TrialTomogramName");

  /**
   * @deprecated substituted trackMethod
   */
  private final EtomoBoolean2 trackUseRaptorA = new EtomoBoolean2(TRACK_KEY
      + "." + FIRST_AXIS_KEY + "." + USE_KEY + RAPTOR_KEY);
  private final EtomoBoolean2 trackRaptorUseRawStackA = new EtomoBoolean2(
      TRACK_KEY + "." + FIRST_AXIS_KEY + "." + RAPTOR_KEY + "." + USE_KEY
          + RAW_STACK_KEY);
  private final EtomoNumber trackRaptorMarkA = new EtomoNumber(TRACK_KEY + "."
      + FIRST_AXIS_KEY + "." + RAPTOR_KEY + "." + MARK_KEY);
  private final EtomoNumber trackRaptorDiamA = new EtomoNumber(
      EtomoNumber.Type.LONG, TRACK_KEY + "." + FIRST_AXIS_KEY + "."
          + RAPTOR_KEY + "." + DIAM_KEY);

  private final EtomoBoolean2 stackEraseGoldModelUseFidA = new EtomoBoolean2(
      STACK_KEY + "." + FIRST_AXIS_KEY + "." + ERASE_GOLD_KEY + "."
          + MODEL_USE_FID_KEY);
  private final EtomoBoolean2 stackEraseGoldModelUseFidB = new EtomoBoolean2(
      STACK_KEY + "." + SECOND_AXIS_KEY + "." + ERASE_GOLD_KEY + "."
          + MODEL_USE_FID_KEY);
  private final EtomoNumber posBinningA = new EtomoNumber("TomoPosBinningA");
  private final EtomoNumber posBinningB = new EtomoNumber("TomoPosBinningB");
  private final EtomoNumber stackBinningA = new EtomoNumber(
      "FinalStackBinningA");
  private final EtomoNumber stackBinningB = new EtomoNumber(
      "FinalStackBinningB");
  private final EtomoNumber stack3dFindBinningA = new EtomoNumber(STACK_KEY
      + "." + FIRST_AXIS_KEY + "." + "3dFind.Binning");
  private final EtomoNumber stack3dFindBinningB = new EtomoNumber(STACK_KEY
      + "." + SECOND_AXIS_KEY + "." + "3dFind.Binning");

  private final EtomoBoolean2 postFlattenInputTrimVol = new EtomoBoolean2(
      POST_KEY + "." + FLATTEN_KEY + "." + INPUT_KEY + TRIM_VOL_KEY);

  private final EtomoBoolean2 postFlattenWarpContoursOnOneSurface = new EtomoBoolean2(
      POST_KEY + "." + FLATTEN_WARP_KEY + "." + CONTOURS_ON_ONE_SURFACE_KEY);
  private final EtomoNumber postFlattenWarpSpacingInX = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, POST_KEY + "." + FLATTEN_WARP_KEY + "."
          + SPACING_IN_KEY + X_KEY);
  private final EtomoNumber postFlattenWarpSpacingInY = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, POST_KEY + "." + FLATTEN_WARP_KEY + "."
          + SPACING_IN_KEY + Y_KEY);

  private final EtomoBoolean2 postSqueezeVolInputTrimVol = new EtomoBoolean2(
      POST_KEY + "." + SQUEEZE_VOL_KEY + "." + INPUT_KEY + TRIM_VOL_KEY);

  private final EtomoNumber postCurTab = new EtomoNumber(POST_KEY + ".CurTab");
  /**
   * postExists is true if the post processing dialog has opened at least once.
   */
  private final EtomoBoolean2 postExists = new EtomoBoolean2(POST_KEY
      + ".Exists");
  private final EtomoNumber lambdaForSmoothing = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, POST_KEY + ".LambdaForSmoothing");
  private final StringProperty lambdaForSmoothingList = new StringProperty(
      POST_KEY + ".LambdaForSmoothingList");

  private final StringProperty trackOverlapOfPatchesXandYA = new StringProperty(
      TRACK_KEY + "." + FIRST_AXIS_KEY + ".OverlapOfPatchesXandY");
  private final StringProperty trackOverlapOfPatchesXandYB = new StringProperty(
      TRACK_KEY + "." + SECOND_AXIS_KEY + ".OverlapOfPatchesXandY");
  private final StringProperty trackNumberOfPatchesXandYA = new StringProperty(
      TRACK_KEY + "." + FIRST_AXIS_KEY + ".NumberOfPatchesXandY");
  private final StringProperty trackNumberOfPatchesXandYB = new StringProperty(
      TRACK_KEY + "." + SECOND_AXIS_KEY + ".NumberOfPatchesXandY");
  private final StringProperty trackLengthAndOverlapA = new StringProperty(
      TRACK_KEY + "." + FIRST_AXIS_KEY + ".LengthAndOverlap");
  private final StringProperty trackLengthAndOverlapB = new StringProperty(
      TRACK_KEY + "." + SECOND_AXIS_KEY + ".LengthAndOverlap");
  private final StringProperty trackMethodA = new StringProperty(TRACK_KEY
      + "." + FIRST_AXIS_KEY + ".TrackMethod");
  private final StringProperty trackMethodB = new StringProperty(TRACK_KEY
      + "." + SECOND_AXIS_KEY + ".TrackMethod");
  /**
   * fineExists is true if the fine alignment dialog has opened at least once.
   */
  private final EtomoBoolean2 fineExistsA = new EtomoBoolean2(FINE_KEY + "."
      + FIRST_AXIS_KEY + ".Exists");
  private final EtomoBoolean2 fineExistsB = new EtomoBoolean2(FINE_KEY + "."
      + SECOND_AXIS_KEY + ".Exists");

  private final EtomoNumber genLogA = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      GEN_KEY + "." + FIRST_AXIS_KEY + ".Log");
  private final EtomoNumber genLogB = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      GEN_KEY + "." + SECOND_AXIS_KEY + ".Log");
  private final EtomoNumber genScaleFactorLogA = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, GEN_KEY + "." + FIRST_AXIS_KEY
          + ".Scale.Factor.Log");
  private final EtomoNumber genScaleFactorLogB = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, GEN_KEY + "." + SECOND_AXIS_KEY
          + ".Scale.Factor.Log");
  private final EtomoNumber genScaleOffsetLogA = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, GEN_KEY + "." + FIRST_AXIS_KEY
          + ".Scale.Offset.Log");
  private final EtomoNumber genScaleOffsetLogB = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, GEN_KEY + "." + SECOND_AXIS_KEY
          + ".Scale.Offset.Log");
  private final EtomoNumber genScaleFactorLinearA = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, GEN_KEY + "." + FIRST_AXIS_KEY
          + ".Scale.Factor.Linear");
  private final EtomoNumber genScaleFactorLinearB = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, GEN_KEY + "." + SECOND_AXIS_KEY
          + ".Scale.Factor.Linear");
  private final EtomoNumber genScaleOffsetLinearA = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, GEN_KEY + "." + FIRST_AXIS_KEY
          + ".Scale.Offset.Linear");
  private final EtomoNumber genScaleOffsetLinearB = new EtomoNumber(
      EtomoNumber.Type.DOUBLE, GEN_KEY + "." + SECOND_AXIS_KEY
          + ".Scale.Offset.Linear");
  /**
   * getExists is true if the tomogram generation dialog has opened at least
   * once.
   */
  private final EtomoBoolean2 genExistsA = new EtomoBoolean2(GEN_KEY + "."
      + FIRST_AXIS_KEY + ".Exists");
  private final EtomoBoolean2 genExistsB = new EtomoBoolean2(GEN_KEY + "."
      + SECOND_AXIS_KEY + ".Exists");
  /**
   * postExists is true if the tomogram possitioning dialog has opened at least
   * once.
   */
  private final EtomoBoolean2 posExistsA = new EtomoBoolean2(POS_KEY
      + ".A.Exists");
  private final EtomoBoolean2 posExistsB = new EtomoBoolean2(POS_KEY
      + ".B.Exists");

  public MetaData(ApplicationManager manager) {
    this.manager = manager;
    squeezevolParam = new SqueezevolParam(manager);
    combineParams = new CombineParams(manager);
    trimvolParam = new TrimvolParam(manager);
    transferfidParamA = new TransferfidParam(manager, AxisID.FIRST);
    transferfidParamB = new TransferfidParam(manager, AxisID.SECOND);
    fileExtension = DatasetFiles.RECON_DATA_FILE_EXT;
    useZFactorsA.setDisplayValue(true);
    useZFactorsB.setDisplayValue(true);
    sampleThicknessA.setDisplayValue(DEFAULT_SAMPLE_THICKNESS);
    sampleThicknessB.setDisplayValue(DEFAULT_SAMPLE_THICKNESS);
    noBeamTiltSelectedA.setDisplayValue(true);//backwards compatibility
    noBeamTiltSelectedB.setDisplayValue(true);//backwards compatibility
    trackUseRaptorA.set(false);
    trackRaptorUseRawStackA.set(true);

    sizeToOutputInXandYA.setIntegerType(new boolean[] { true, true });
    sizeToOutputInXandYB.setIntegerType(new boolean[] { true, true });

    sizeToOutputInXandYA.setPropertiesKey("A.SizeToOutputInXandY");
    sizeToOutputInXandYA.setDefault();
    sizeToOutputInXandYB.setPropertiesKey("B.SizeToOutputInXandY");
    sizeToOutputInXandYB.setDefault();

    posBinningA.setDisplayValue(3);
    posBinningB.setDisplayValue(3);
    stackBinningA.setDisplayValue(1);
    stackBinningB.setDisplayValue(1);
    stack3dFindBinningA.setDisplayValue(1);
    stack3dFindBinningB.setDisplayValue(1);

    stackEraseGoldModelUseFidA
        .setDisplayValue(ERASE_GOLD_MODEL_USE_FID_DEFAULT);
    stackEraseGoldModelUseFidB
        .setDisplayValue(ERASE_GOLD_MODEL_USE_FID_DEFAULT);
  }

  /**
   * Set the dataset name, trimming any white space from the beginning and
   * end of the string
   */
  public void setDatasetName(String fileName) {
    String pathName = fileName.trim();
    File file = new File(pathName);
    String path = file.getPath();
    datasetName = file.getName();
    fixDatasetName();
    Utilities.managerStamp(null, datasetName);
  }

  /**
   * Remove the ".st", "a.st", or "b.st" as approrpiate to the
   */
  private void fixDatasetName() {
    if (axisType == AxisType.SINGLE_AXIS) {
      if (datasetName.endsWith(".st")) {
        int nChars = datasetName.length();
        datasetName = datasetName.substring(0, nChars - 3);
      }
    }
    else {
      if (datasetName.endsWith("a.st") | datasetName.endsWith("b.st")) {
        int nChars = datasetName.length();
        datasetName = datasetName.substring(0, nChars - 4);
      }
      //if a dual axis file has the wrong format, treat it like a single axis
      //file
      else if (datasetName.endsWith(".st")) {
        int nChars = datasetName.length();
        datasetName = datasetName.substring(0, nChars - 3);
        appendMessage("Dual axis image stack files must end in a.st and b.st.\n");
      }
    }
  }

  public void setLambdaForSmoothing(String input) {
    lambdaForSmoothing.set(input);
  }

  public String getLambdaForSmoothing() {
    return lambdaForSmoothing.toString();
  }

  public void setLambdaForSmoothingList(String input) {
    lambdaForSmoothingList.set(input);
  }

  public String getLambdaForSmoothingList() {
    return lambdaForSmoothingList.toString();
  }

  public boolean isLambdaForSmoothingListEmpty() {
    return lambdaForSmoothingList.isEmpty();
  }

  public void setTransferfidAFields(TransferfidParam param) {
    transferfidParamA.setStorableFields(param);
  }

  public void setTransferfidBFields(TransferfidParam param) {
    transferfidParamB.setStorableFields(param);
  }

  public void setSampleThickness(AxisID axisID, String thickness) {
    if (axisID == AxisID.SECOND) {
      sampleThicknessB.set(thickness);
    }
    else {
      sampleThicknessA.set(thickness);
    }
  }

  /**
   * Set the backup diretory, trimming any white space from the beginning and
   * end of the string
   */
  public void setBackupDirectory(String backupDir) {
    backupDirectory = backupDir.trim();
  }

  public void setDistortionFile(String distortionFile) {
    this.distortionFile = distortionFile;
  }

  public void setMagGradientFile(String magGradientFile) {
    this.magGradientFile = magGradientFile;
  }

  public void setAdjustedFocusA(boolean adjustedFocus) {
    this.adjustedFocusA.set(adjustedFocus);
  }

  public void setAdjustedFocusB(boolean adjustedFocus) {
    this.adjustedFocusB.set(adjustedFocus);
  }

  public void setAxisType(AxisType at) {
    axisType = at;
    setAxisPrepends();
  }

  public void setViewType(ViewType vt) {
    viewType = vt;
  }

  public void setPixelSize(double pixelSize) {
    this.pixelSize = pixelSize;
  }

  public void setPixelSize(String pixelSize) {
    this.pixelSize = Double.parseDouble(pixelSize);
  }

  public void setUseLocalAlignments(AxisID axisID, boolean state) {
    if (axisID == AxisID.SECOND) {
      useLocalAlignmentsB = state;
    }
    else {
      useLocalAlignmentsA = state;
    }
  }

  public final void setBStackProcessed(boolean bStackProcessed) {
    if (this.bStackProcessed == null) {
      this.bStackProcessed = new EtomoBoolean2(B_STACK_PROCESSED_GROUP);
    }
    this.bStackProcessed.set(bStackProcessed);
  }

  public final void setBStackProcessed(String bStackProcessed) {
    if (this.bStackProcessed == null) {
      this.bStackProcessed = new EtomoBoolean2(B_STACK_PROCESSED_GROUP);
    }
    this.bStackProcessed.set(bStackProcessed);
  }

  public void setSizeToOutputInXandY(AxisID axisID, String size)
      throws FortranInputSyntaxException {
    if (axisID == AxisID.SECOND) {
      sizeToOutputInXandYB.validateAndSet(size);
    }
    else {
      sizeToOutputInXandYA.validateAndSet(size);
    }
  }

  public void setPosBinning(AxisID axisID, int binning) {
    if (axisID == AxisID.SECOND) {
      posBinningB.set(binning);
    }
    else {
      posBinningA.set(binning);
    }
  }

  public void setStackBinning(AxisID axisID, int binning) {
    if (axisID == AxisID.SECOND) {
      stackBinningB.set(binning);
    }
    else {
      stackBinningA.set(binning);
    }
  }

  public void setStack3dFindBinning(AxisID axisID, int binning) {
    if (axisID == AxisID.SECOND) {
      stack3dFindBinningB.set(binning);
    }
    else {
      stack3dFindBinningA.set(binning);
    }
  }

  public void setPostCurTab(int input) {
    postCurTab.set(input);
  }

  public void setPostExists(boolean input) {
    postExists.set(input);
  }

  public void setCombineVolcombineParallel(boolean combineVolcombineParallel) {
    if (this.combineVolcombineParallel == null) {
      this.combineVolcombineParallel = new EtomoBoolean2(
          COMBINE_VOLCOMBINE_PARALLEL_GROUP);
    }
    this.combineVolcombineParallel.set(combineVolcombineParallel);
  }

  public void setCombineVolcombineParallel(String combineVolcombineParallel) {
    if (this.combineVolcombineParallel == null) {
      this.combineVolcombineParallel = new EtomoBoolean2(
          COMBINE_VOLCOMBINE_PARALLEL_GROUP);
    }
    this.combineVolcombineParallel.set(combineVolcombineParallel);
  }

  public void setTiltParallel(AxisID axisID, PanelId panelId,
      boolean tiltParallel) {
    if (panelId == PanelId.TILT) {
      if (axisID == AxisID.SECOND) {
        if (tomoGenTiltParallelB == null) {
          tomoGenTiltParallelB = new EtomoBoolean2(
              TOMO_GEN_B_TILT_PARALLEL_GROUP);
        }
        tomoGenTiltParallelB.set(tiltParallel);
      }
      else {
        if (tomoGenTiltParallelA == null) {
          tomoGenTiltParallelA = new EtomoBoolean2(
              TOMO_GEN_A_TILT_PARALLEL_GROUP);
        }
        tomoGenTiltParallelA.set(tiltParallel);
      }
    }
    else if (panelId == PanelId.TILT_3D_FIND) {
      if (axisID == AxisID.SECOND) {
        if (tilt3dFindTiltParallelB == null) {
          tilt3dFindTiltParallelB = new EtomoBoolean2(
              TILT_3D_FIND_B_TILT_PARALLEL_KEY);
        }
        tilt3dFindTiltParallelB.set(tiltParallel);
      }
      else {
        if (tilt3dFindTiltParallelA == null) {
          tilt3dFindTiltParallelA = new EtomoBoolean2(
              TILT_3D_FIND_A_TILT_PARALLEL_KEY);
        }
        tilt3dFindTiltParallelA.set(tiltParallel);
      }
    }
  }

  public void setFinalStackCtfCorrectionParallel(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      if (finalStackCtfCorrectionParallelB == null) {
        finalStackCtfCorrectionParallelB = new EtomoBoolean2(
            FINAL_STACK_B_CTF_CORRECTION_PARALLEL_GROUP);
      }
      finalStackCtfCorrectionParallelB.set(input);
    }
    else {
      if (finalStackCtfCorrectionParallelA == null) {
        finalStackCtfCorrectionParallelA = new EtomoBoolean2(
            FINAL_STACK_A_CTF_CORRECTION_PARALLEL_GROUP);
      }
      finalStackCtfCorrectionParallelA.set(input);
    }
  }

  public void setDefaultParallel(boolean defaultParallel) {
    this.defaultParallel.set(defaultParallel);
  }

  public void setDefaultGpuProcessing(boolean input) {
    defaultGpuProcessing.set(input);
  }

  private void setTomoGenTiltParallel(AxisID axisID, String tomoGenTiltParallel) {
    if (axisID == AxisID.SECOND) {
      if (tomoGenTiltParallelB == null) {
        tomoGenTiltParallelB = new EtomoBoolean2(TOMO_GEN_B_TILT_PARALLEL_GROUP);
      }
      tomoGenTiltParallelB.set(tomoGenTiltParallel);
    }
    else {
      if (tomoGenTiltParallelA == null) {
        tomoGenTiltParallelA = new EtomoBoolean2(TOMO_GEN_A_TILT_PARALLEL_GROUP);
      }
      tomoGenTiltParallelA.set(tomoGenTiltParallel);
    }
  }

  public void setFinalStackCtfCorrectionParallel(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      if (finalStackCtfCorrectionParallelB == null) {
        finalStackCtfCorrectionParallelB = new EtomoBoolean2(
            FINAL_STACK_B_CTF_CORRECTION_PARALLEL_GROUP);
      }
      finalStackCtfCorrectionParallelB.set(input);
    }
    else {
      if (finalStackCtfCorrectionParallelA == null) {
        finalStackCtfCorrectionParallelA = new EtomoBoolean2(
            FINAL_STACK_A_CTF_CORRECTION_PARALLEL_GROUP);
      }
      finalStackCtfCorrectionParallelA.set(input);
    }
  }

  public void setUseZFactors(AxisID axisID, boolean useZFactors) {
    if (axisID == AxisID.SECOND) {
      this.useZFactorsB.set(useZFactors);
    }
    else {
      this.useZFactorsA.set(useZFactors);
    }
  }

  public void setFiducialDiameter(double fiducialDiameter) {
    this.fiducialDiameter = fiducialDiameter;
  }

  public void setFiducialDiameter(String fiducialDiameter) {
    this.fiducialDiameter = Double.parseDouble(fiducialDiameter);
  }

  public void setImageRotation(float rotation, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      imageRotationB = rotation;
    }
    else {
      imageRotationA = rotation;
    }
  }

  public void setImageRotation(String rotation, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      imageRotationB = Float.parseFloat(rotation);
    }
    else {
      imageRotationA = Float.parseFloat(rotation);
    }
  }

  public void setBinning(Number binning) {
    this.binning = binning.intValue();
  }

  public void setTiltAngleSpecA(TiltAngleSpec tiltAngleSpec) {
    tiltAngleSpecA = tiltAngleSpec;
  }

  public void setExcludeProjections(String list, AxisID axisID) {
    //Strip whitespace.
    String[] array = list.trim().split("\\s+");
    if (array != null && array.length > 1) {
      StringBuffer buffer = new StringBuffer();
      for (int i = 0; i < array.length; i++) {
        buffer.append(array[i]);
      }
      if (axisID == AxisID.SECOND) {
        excludeProjectionsB = buffer.toString();
      }
      else {
        excludeProjectionsB = buffer.toString();
      }
    }
    else {
      if (axisID == AxisID.SECOND) {
        excludeProjectionsB = list.trim();
      }
      else {
        excludeProjectionsA = list.trim();
      }
    }
  }

  public void setGenExists(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      genExistsB.set(input);
    }
    else {
      genExistsA.set(input);
    }
  }
  
  public void setPosExists(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      posExistsB.set(input);
    }
    else {
      posExistsA.set(input);
    }
  }

  public void setTiltAngleSpecB(TiltAngleSpec tiltAngleSpec) {
    tiltAngleSpecB = tiltAngleSpec;
  }

  public void setComScriptCreated(boolean state) {
    comScriptsCreated = state;
  }

  public void setCombineParams(CombineParams combine) {
    combineParams = combine;
  }

  public void setFiducialessAlignment(AxisID axisID, boolean state) {
    if (axisID == AxisID.SECOND) {
      fiducialessAlignmentB = state;
    }
    else {
      fiducialessAlignmentA = state;
    }
  }

  public void setFineExists(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      fineExistsB.set(input);
    }
    else {
      fineExistsA.set(input);
    }
  }

  public void setWholeTomogramSample(AxisID axisID, boolean state) {
    if (axisID == AxisID.SECOND) {
      wholeTomogramSampleB = state;
    }
    else {
      wholeTomogramSampleA = state;
    }
  }

  /**
   *  Get the objects attributes from the properties object.
   */
  public void load(Properties props) {
    load(props, "");
  }

  /**
   * Set up axis prepends.  For dual axis, axis a prepend is "A" and axis b
   * prepend is "B".  For single axis, axis a prepend is "" and axis b prepend
   * doesn't exist.
   */
  private void setAxisPrepends() {
    //set firstAxis and secondAxis strings (based on AxisType)
    if (axisType == AxisType.DUAL_AXIS) {
      firstAxisPrepend = AxisID.FIRST.getExtension().toUpperCase();
      secondAxisPrepend = AxisID.SECOND.getExtension().toUpperCase();
    }
    else {
      firstAxisPrepend = "";
    }
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    //reset
    revisionNumber.reset();
    distortionFile = "";
    magGradientFile = "";
    binning = 1;
    useLocalAlignmentsA = true;
    useLocalAlignmentsB = true;
    useZFactorsA.reset();
    useZFactorsB.reset();
    if (tomoGenTiltParallelA != null) {
      tomoGenTiltParallelA.reset();
    }
    if (tomoGenTiltParallelB != null) {
      tomoGenTiltParallelB.reset();
    }
    if (tilt3dFindTiltParallelA != null) {
      tilt3dFindTiltParallelA.reset();
    }
    if (tilt3dFindTiltParallelB != null) {
      tilt3dFindTiltParallelB.reset();
    }
    if (finalStackCtfCorrectionParallelA != null) {
      finalStackCtfCorrectionParallelA.reset();
    }
    if (finalStackCtfCorrectionParallelB != null) {
      finalStackCtfCorrectionParallelB.reset();
    }
    if (combineVolcombineParallel != null) {
      combineVolcombineParallel.reset();
    }
    sampleThicknessA.reset();
    sampleThicknessB.reset();
    targetPatchSizeXandY = TiltalignParam.TARGET_PATCH_SIZE_X_AND_Y_DEFAULT;//backwards compatibility
    numberOfLocalPatchesXandY = TiltalignParam.NUMBER_OF_LOCAL_PATCHES_X_AND_Y_DEFAULT;
    noBeamTiltSelectedA.reset();
    fixedBeamTiltSelectedA.reset();
    fixedBeamTiltA.reset();
    noBeamTiltSelectedB.reset();
    fixedBeamTiltSelectedB.reset();
    fixedBeamTiltB.reset();
    finalStackBetterRadiusA.reset();
    finalStackBetterRadiusB.reset();
    finalStackFiducialDiameterA.reset();
    finalStackFiducialDiameterB.reset();
    finalStackPolynomialOrderA.reset();
    finalStackPolynomialOrderB.reset();
    tomoGenTrialTomogramNameListA.reset();
    tomoGenTrialTomogramNameListB.reset();
    trackUseRaptorA.reset();
    trackRaptorUseRawStackA.reset();
    trackRaptorMarkA.reset();
    trackRaptorDiamA.reset();
    stackEraseGoldModelUseFidA.reset();
    stackEraseGoldModelUseFidB.reset();
    postFlattenInputTrimVol.reset();
    postFlattenWarpContoursOnOneSurface.reset();
    postFlattenWarpSpacingInX.reset();
    postFlattenWarpSpacingInY.reset();
    postSqueezeVolInputTrimVol.reset();
    posBinningA.reset();
    posBinningB.reset();
    stackBinningA.reset();
    stackBinningB.reset();
    stack3dFindBinningA.reset();
    stack3dFindBinningB.reset();
    postCurTab.reset();
    postExists.reset();
    lambdaForSmoothing.reset();
    lambdaForSmoothingList.reset();
    trackOverlapOfPatchesXandYA.reset();
    trackOverlapOfPatchesXandYB.reset();
    trackNumberOfPatchesXandYA.reset();
    trackNumberOfPatchesXandYB.reset();
    trackLengthAndOverlapA.reset();
    trackLengthAndOverlapB.reset();
    trackMethodA.reset();
    trackMethodB.reset();
    fineExistsA.reset();
    fineExistsB.reset();
    genLogA.reset();
    genLogB.reset();
    genScaleFactorLogA.reset();
    genScaleFactorLogB.reset();
    genScaleOffsetLogA.reset();
    genScaleOffsetLogB.reset();
    genScaleFactorLinearA.reset();
    genScaleFactorLinearB.reset();
    genScaleOffsetLinearA.reset();
    genScaleOffsetLinearB.reset();
    genExistsA.reset();
    genExistsB.reset();
    posExistsA.reset();
    posExistsB.reset();
    //load
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    axisType = AxisType.fromString(props.getProperty(group + "AxisType",
        "Not Set"));
    setAxisPrepends();
    //backwards compatibility
    revisionNumber.load(props, prepend);
    if (revisionNumber.le(EtomoVersion.getDefaultInstance("1.7"))) {
      fiducialessA.loadWithAlternateKey(props, prepend,
          ".A.Param.tilt.Fiducialess");
      fiducialessB.loadWithAlternateKey(props, prepend,
          ".B.Param.tilt.Fiducialess");
    }
    else {
      fiducialessA.load(props, prepend);
      fiducialessB.load(props, prepend);
    }
    if (revisionNumber.le(EtomoVersion.getDefaultInstance("1.8"))) {
      stackBinningA.loadWithAlternateKey(props, prepend,
          FINAL_STACK_BINNING_A_BACKWARD_COMPATABILITY_1_8);
      stackBinningB.loadWithAlternateKey(props, prepend,
          FINAL_STACK_BINNING_B_BACKWARD_COMPATABILITY_1_8);
    }
    else {
      stackBinningA.load(props, prepend);
      stackBinningB.load(props, prepend);
    }
    if (revisionNumber.le(EtomoVersion.getDefaultInstance("1.9"))) {
      //better radius needs to be converted to final stack fiducial diameter.
      finalStackBetterRadiusA.load(props, prepend);
      finalStackBetterRadiusB.load(props, prepend);
    }
    if (revisionNumber.le(EtomoVersion.getDefaultInstance("1.10"))) {
      trackUseRaptorA.load(props, prepend);
      if (trackUseRaptorA.is()) {
        trackMethodA.set(FiducialModelDialog.MethodEnumeratedType.RAPTOR
            .toString());
      }
    }
    else {
      trackMethodA.load(props, prepend);
    }
    finalStackFiducialDiameterA.load(props, prepend);
    finalStackFiducialDiameterB.load(props, prepend);

    // Make this true for now until the variable is present in all of the
    // data files so as to not break existing files
    // May-03-2002
    comScriptsCreated = Boolean.valueOf(
        props.getProperty(group + "ComScriptsCreated", "true")).booleanValue();

    // Backwards compatibility with FilesetName string
    datasetName = props.getProperty(group + "FilesetName", "");
    datasetName = props.getProperty(group + "DatasetName", datasetName);
    backupDirectory = props.getProperty(group + "BackupDirectory", "");

    dataSource = DataSource.fromString(props.getProperty(group + "DataSource",
        "CCD"));
    viewType = ViewType.fromString(props.getProperty(group + "ViewType",
        "Single View"));
    pixelSize = Double.parseDouble(props
        .getProperty(group + "PixelSize", "0.0"));

    useLocalAlignmentsA = Boolean.valueOf(
        props.getProperty(group + "UseLocalAlignmentsA", "true"))
        .booleanValue();
    useLocalAlignmentsB = Boolean.valueOf(
        props.getProperty(group + "UseLocalAlignmentsB", "true"))
        .booleanValue();
    fiducialDiameter = Double.parseDouble(props.getProperty(group
        + "FiducialDiameter", "0.0"));

    // Read in the old single image rotation or the newer separate image
    // rotation for each axis
    String strOldRotation = props.getProperty(group + "ImageRotation", "0.0");
    imageRotationA = Float.parseFloat(props.getProperty(group
        + "ImageRotationA", strOldRotation));
    imageRotationB = Float.parseFloat(props.getProperty(group
        + "ImageRotationB", strOldRotation));
    excludeProjectionsA = props.getProperty(group + "AxisA.ExcludeProjections",
        "");
    tiltAngleSpecA.load(props, group + "AxisA");

    excludeProjectionsB = props.getProperty(group + "AxisB.ExcludeProjections",
        "");
    tiltAngleSpecB.load(props, group + "AxisB");
    combineParams.load(props, group);
    distortionFile = props
        .getProperty(group + "DistortionFile", distortionFile);
    magGradientFile = props.getProperty(group + "MagGradientFile",
        magGradientFile);
    binning = Integer.parseInt(props.getProperty(group + "Binning", Integer
        .toString(binning)));

    fiducialessAlignmentA = Boolean.valueOf(
        props.getProperty(group + "FiducialessAlignmentA", "false"))
        .booleanValue();
    fiducialessAlignmentB = Boolean.valueOf(
        props.getProperty(group + "FiducialessAlignmentB", "false"))
        .booleanValue();
    wholeTomogramSampleA = Boolean.valueOf(
        props.getProperty(group + "WholeTomogramSampleA", "false"))
        .booleanValue();
    wholeTomogramSampleB = Boolean.valueOf(
        props.getProperty(group + "WholeTomogramSampleB", "false"))
        .booleanValue();
    trimvolParam.load(props, group);
    squeezevolParam.load(props, prepend);
    useZFactorsA.load(props, prepend);
    useZFactorsB.load(props, prepend);
    transferfidParamA.load(props, prepend);
    transferfidParamB.load(props, prepend);
    sizeToOutputInXandYA.load(props, prepend);
    sizeToOutputInXandYB.load(props, prepend);
    String propertyValue = props.getProperty(group
        + TOMO_GEN_A_TILT_PARALLEL_GROUP);
    if (propertyValue != null) {
      setTomoGenTiltParallel(AxisID.FIRST, propertyValue);
    }
    propertyValue = props.getProperty(group + TOMO_GEN_B_TILT_PARALLEL_GROUP);
    if (propertyValue != null) {
      setTomoGenTiltParallel(AxisID.SECOND, propertyValue);
    }
    propertyValue = props.getProperty(group
        + FINAL_STACK_A_CTF_CORRECTION_PARALLEL_GROUP);
    if (propertyValue != null) {
      setFinalStackCtfCorrectionParallel(AxisID.FIRST, propertyValue);
    }
    propertyValue = props.getProperty(group
        + FINAL_STACK_B_CTF_CORRECTION_PARALLEL_GROUP);
    if (propertyValue != null) {
      setFinalStackCtfCorrectionParallel(AxisID.SECOND, propertyValue);
    }
    propertyValue = props
        .getProperty(group + COMBINE_VOLCOMBINE_PARALLEL_GROUP);
    if (propertyValue != null) {
      setCombineVolcombineParallel(propertyValue);
    }
    propertyValue = props.getProperty(group + B_STACK_PROCESSED_GROUP);
    if (propertyValue != null) {
      setBStackProcessed(propertyValue);
    }
    defaultParallel.load(props, prepend);
    sampleThicknessA.load(props, prepend);
    sampleThicknessB.load(props, prepend);

    //use default for backward compatibility, since this new parameter may not
    //be in any file yet
    targetPatchSizeXandY = props.getProperty(group + "tiltalign."
        + TiltalignParam.TARGET_PATCH_SIZE_X_AND_Y_KEY,
        TiltalignParam.TARGET_PATCH_SIZE_X_AND_Y_DEFAULT);
    numberOfLocalPatchesXandY = props.getProperty(group + "tiltalign."
        + TiltalignParam.NUMBER_OF_LOCAL_PATCHES_X_AND_Y_KEY,
        TiltalignParam.NUMBER_OF_LOCAL_PATCHES_X_AND_Y_DEFAULT);
    noBeamTiltSelectedA.load(props, prepend);
    fixedBeamTiltSelectedA.load(props, prepend);
    fixedBeamTiltA.load(props, prepend);
    noBeamTiltSelectedB.load(props, prepend);
    fixedBeamTiltSelectedB.load(props, prepend);
    fixedBeamTiltB.load(props, prepend);
    finalStackPolynomialOrderA.load(props, prepend);
    finalStackPolynomialOrderB.load(props, prepend);
    tomoGenTrialTomogramNameListA.load(props, prepend);
    tomoGenTrialTomogramNameListB.load(props, prepend);
    trackRaptorUseRawStackA.load(props, prepend);
    trackRaptorMarkA.load(props, prepend);
    trackRaptorDiamA.load(props, prepend);
    stackEraseGoldModelUseFidA.load(props, prepend);
    stackEraseGoldModelUseFidB.load(props, prepend);
    postFlattenInputTrimVol.load(props, prepend);
    postFlattenWarpContoursOnOneSurface.load(props, prepend);
    postFlattenWarpSpacingInX.load(props, prepend);
    postFlattenWarpSpacingInY.load(props, prepend);
    postSqueezeVolInputTrimVol.load(props, prepend);
    posBinningA.load(props, prepend);
    posBinningB.load(props, prepend);
    stack3dFindBinningA.load(props, prepend);
    stack3dFindBinningB.load(props, prepend);
    postCurTab.load(props, prepend);
    postExists.load(props, prepend);
    lambdaForSmoothing.load(props, prepend);
    lambdaForSmoothingList.load(props, prepend);
    trackOverlapOfPatchesXandYA.load(props, prepend);
    trackOverlapOfPatchesXandYB.load(props, prepend);
    trackNumberOfPatchesXandYA.load(props, prepend);
    trackNumberOfPatchesXandYB.load(props, prepend);
    trackLengthAndOverlapA.load(props, prepend);
    trackLengthAndOverlapB.load(props, prepend);
    //trackMethodA is loaded in backwards compatibility section
    trackMethodB.load(props, prepend);
    fineExistsA.load(props, prepend);
    fineExistsB.load(props, prepend);
    genLogA.load(props, prepend);
    genLogB.load(props, prepend);
    genScaleFactorLogA.load(props, prepend);
    genScaleFactorLogB.load(props, prepend);
    genScaleOffsetLogA.load(props, prepend);
    genScaleOffsetLogB.load(props, prepend);
    genScaleFactorLinearA.load(props, prepend);
    genScaleFactorLinearB.load(props, prepend);
    genScaleOffsetLinearA.load(props, prepend);
    genScaleOffsetLinearB.load(props, prepend);
    genExistsA.load(props, prepend);
    genExistsB.load(props, prepend);
    posExistsA.load(props, prepend);
    posExistsB.load(props, prepend);
    defaultGpuProcessing.load(props, prepend);
    tilt3dFindTiltParallelA = EtomoBoolean2.load(tilt3dFindTiltParallelA,
        TILT_3D_FIND_A_TILT_PARALLEL_KEY, props, prepend);
    tilt3dFindTiltParallelB = EtomoBoolean2.load(tilt3dFindTiltParallelB,
        TILT_3D_FIND_B_TILT_PARALLEL_KEY, props, prepend);
  }

  public void setNoBeamTiltSelected(AxisID axisID, boolean selected) {
    if (axisID == AxisID.SECOND) {
      noBeamTiltSelectedB.set(selected);
    }
    else {
      noBeamTiltSelectedA.set(selected);
    }
  }

  public void setFixedBeamTiltSelected(AxisID axisID, boolean selected) {
    if (axisID == AxisID.SECOND) {
      fixedBeamTiltSelectedB.set(selected);
    }
    else {
      fixedBeamTiltSelectedA.set(selected);
    }
  }

  public void setGenLog(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      genLogB.set(input);
    }
    else {
      genLogA.set(input);
    }
  }

  public void setGenScaleFactorLog(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      genScaleFactorLogB.set(input);
    }
    else {
      genScaleFactorLogA.set(input);
    }
  }

  public void setGenScaleOffsetLog(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      genScaleOffsetLogB.set(input);
    }
    else {
      genScaleOffsetLogA.set(input);
    }
  }

  public void setGenScaleFactorLinear(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      genScaleFactorLinearB.set(input);
    }
    else {
      genScaleFactorLinearA.set(input);
    }
  }

  public void setGenScaleOffsetLinear(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      genScaleOffsetLinearB.set(input);
    }
    else {
      genScaleOffsetLinearA.set(input);
    }
  }

  public void setFixedBeamTilt(AxisID axisID, String fixedBeamTilt) {
    if (axisID == AxisID.SECOND) {
      this.fixedBeamTiltB.set(fixedBeamTilt);
    }
    else {
      this.fixedBeamTiltA.set(fixedBeamTilt);
    }
  }

  public void setFinalStackFiducialDiameter(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      finalStackFiducialDiameterB.set(input);
    }
    else {
      finalStackFiducialDiameterA.set(input);
    }
  }

  public void setFinalStackPolynomialOrder(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      finalStackPolynomialOrderB.set(input);
    }
    else {
      finalStackPolynomialOrderA.set(input);
    }
  }

  public void setTargetPatchSizeXandY(String targetPatchSizeXandY) {
    this.targetPatchSizeXandY = targetPatchSizeXandY;
  }

  public void setNumberOfLocalPatchesXandY(String numberOfLocalPatchesXandY) {
    this.numberOfLocalPatchesXandY = numberOfLocalPatchesXandY;
  }

  public void setFiducialess(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      fiducialessB.set(input);
    }
    else {
      fiducialessA.set(input);
    }
  }

  public String getFirstAxisPrepend() {
    return firstAxisPrepend;
  }

  public String getSecondAxisPrepend() {
    return secondAxisPrepend;
  }

  String createPrepend(String prepend) {
    if (prepend.equals("")) {
      return "Setup";
    }
    return prepend + ".Setup";
  }

  /**
   *  Insert the objects attributes into the properties object.
   */
  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    props.setProperty(group + "RevisionNumber", latestRevisionNumber);
    props.setProperty(group + "ComScriptsCreated", String
        .valueOf(comScriptsCreated));
    props.setProperty(group + "DatasetName", datasetName);
    props.setProperty(group + "BackupDirectory", backupDirectory);

    props.setProperty(group + "DataSource", dataSource.toString());
    props.setProperty(group + "AxisType", axisType.toString());
    props.setProperty(group + "ViewType", viewType.toString());

    props.setProperty(group + "PixelSize", String.valueOf(pixelSize));
    props.setProperty(group + "UseLocalAlignmentsA", String
        .valueOf(useLocalAlignmentsA));
    props.setProperty(group + "UseLocalAlignmentsB", String
        .valueOf(useLocalAlignmentsB));
    props.setProperty(group + "FiducialDiameter", String
        .valueOf(fiducialDiameter));
    props.setProperty(group + "ImageRotationA", String.valueOf(imageRotationA));
    props.setProperty(group + "ImageRotationB", String.valueOf(imageRotationB));
    tiltAngleSpecA.store(props, group + "AxisA");
    props.setProperty(group + "AxisA.ExcludeProjections", String
        .valueOf(excludeProjectionsA));

    tiltAngleSpecB.store(props, group + "AxisB");
    props.setProperty(group + "AxisB.ExcludeProjections", String
        .valueOf(excludeProjectionsB));

    combineParams.store(props, group);
    props.setProperty(group + "DistortionFile", distortionFile);
    props.setProperty(group + "MagGradientFile", magGradientFile);
    props.setProperty(group + "Binning", String.valueOf(binning));
    props.setProperty(group + "FiducialessAlignmentA", String
        .valueOf(fiducialessAlignmentA));
    props.setProperty(group + "FiducialessAlignmentB", String
        .valueOf(fiducialessAlignmentB));
    props.setProperty(group + "WholeTomogramSampleA", String
        .valueOf(wholeTomogramSampleA));
    props.setProperty(group + "WholeTomogramSampleB", String
        .valueOf(wholeTomogramSampleB));
    trimvolParam.store(props, group);
    squeezevolParam.store(props, prepend);
    useZFactorsA.store(props, prepend);
    useZFactorsB.store(props, prepend);
    transferfidParamA.store(props, prepend);
    transferfidParamB.store(props, prepend);
    if (tomoGenTiltParallelA != null) {
      tomoGenTiltParallelA.store(props, prepend);
    }
    if (tomoGenTiltParallelB != null) {
      tomoGenTiltParallelB.store(props, prepend);
    }
    EtomoBoolean2.store(tilt3dFindTiltParallelA, props, prepend,
        TILT_3D_FIND_A_TILT_PARALLEL_KEY);
    EtomoBoolean2.store(tilt3dFindTiltParallelB, props, prepend,
        TILT_3D_FIND_B_TILT_PARALLEL_KEY);
    if (finalStackCtfCorrectionParallelA != null) {
      finalStackCtfCorrectionParallelA.store(props, prepend);
    }
    if (finalStackCtfCorrectionParallelB != null) {
      finalStackCtfCorrectionParallelB.store(props, prepend);
    }
    if (combineVolcombineParallel != null) {
      combineVolcombineParallel.store(props, prepend);
    }
    if (bStackProcessed != null) {
      bStackProcessed.store(props, prepend);
    }
    defaultParallel.store(props, prepend);
    sampleThicknessA.store(props, prepend);
    sampleThicknessB.store(props, prepend);
    fiducialessA.store(props, prepend);
    fiducialessB.store(props, prepend);
    props.setProperty(group + "tiltalign."
        + TiltalignParam.TARGET_PATCH_SIZE_X_AND_Y_KEY, targetPatchSizeXandY);
    props.setProperty(group + "tiltalign."
        + TiltalignParam.NUMBER_OF_LOCAL_PATCHES_X_AND_Y_KEY,
        numberOfLocalPatchesXandY);
    noBeamTiltSelectedA.store(props, prepend);
    fixedBeamTiltSelectedA.store(props, prepend);
    fixedBeamTiltA.store(props, prepend);
    noBeamTiltSelectedB.store(props, prepend);
    fixedBeamTiltSelectedB.store(props, prepend);
    fixedBeamTiltB.store(props, prepend);
    sizeToOutputInXandYA.store(props, prepend);
    sizeToOutputInXandYB.store(props, prepend);
    finalStackPolynomialOrderA.store(props, prepend);
    finalStackPolynomialOrderB.store(props, prepend);
    finalStackFiducialDiameterA.store(props, prepend);
    finalStackFiducialDiameterB.store(props, prepend);
    tomoGenTrialTomogramNameListA.store(props, prepend);
    tomoGenTrialTomogramNameListB.store(props, prepend);
    trackRaptorUseRawStackA.store(props, prepend);
    trackRaptorMarkA.store(props, prepend);
    trackRaptorDiamA.store(props, prepend);
    stackEraseGoldModelUseFidA.store(props, prepend);
    stackEraseGoldModelUseFidB.store(props, prepend);
    postFlattenInputTrimVol.store(props, prepend);
    postFlattenWarpContoursOnOneSurface.store(props, prepend);
    postFlattenWarpSpacingInX.store(props, prepend);
    postFlattenWarpSpacingInY.store(props, prepend);
    postSqueezeVolInputTrimVol.store(props, prepend);
    posBinningA.store(props, prepend);
    posBinningB.store(props, prepend);
    stackBinningA.store(props, prepend);
    stackBinningB.store(props, prepend);
    stack3dFindBinningA.store(props, prepend);
    stack3dFindBinningB.store(props, prepend);
    postCurTab.store(props, prepend);
    postExists.store(props, prepend);
    lambdaForSmoothing.store(props, prepend);
    lambdaForSmoothingList.store(props, prepend);
    trackOverlapOfPatchesXandYA.store(props, prepend);
    trackOverlapOfPatchesXandYB.store(props, prepend);
    trackNumberOfPatchesXandYA.store(props, prepend);
    trackNumberOfPatchesXandYB.store(props, prepend);
    trackLengthAndOverlapA.store(props, prepend);
    trackLengthAndOverlapB.store(props, prepend);
    trackMethodA.store(props, prepend);
    trackMethodB.store(props, prepend);
    fineExistsA.store(props, prepend);
    fineExistsB.store(props, prepend);
    genLogA.store(props, prepend);
    genLogB.store(props, prepend);
    genScaleFactorLogA.store(props, prepend);
    genScaleFactorLogB.store(props, prepend);
    genScaleOffsetLogA.store(props, prepend);
    genScaleOffsetLogB.store(props, prepend);
    genScaleFactorLinearA.store(props, prepend);
    genScaleFactorLinearB.store(props, prepend);
    genScaleOffsetLinearA.store(props, prepend);
    genScaleOffsetLinearB.store(props, prepend);
    genExistsA.store(props, prepend);
    genExistsB.store(props, prepend);
    defaultGpuProcessing.store(props, prepend);
    posExistsA.store(props, prepend);
    posExistsB.store(props, prepend);
  }

  public boolean getTrackRaptorUseRawStack() {
    return trackRaptorUseRawStackA.is();
  }

  public void setTrackRaptorUseRawStack(boolean input) {
    trackRaptorUseRawStackA.set(input);
  }

  public String getTrackRaptorMark() {
    return trackRaptorMarkA.toString();
  }

  public void setTrackRaptorMark(String input) {
    trackRaptorMarkA.set(input);
  }

  public ConstEtomoNumber getTrackRaptorDiam() {
    return trackRaptorDiamA;
  }

  public void setTrackRaptorDiam(String input) {
    trackRaptorDiamA.set(input);
  }

  public boolean getEraseGoldModelUseFid(final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return stackEraseGoldModelUseFidB.is();
    }
    return stackEraseGoldModelUseFidA.is();
  }

  public void setEraseGoldModelUseFid(final AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      stackEraseGoldModelUseFidB.set(input);
    }
    else {
      stackEraseGoldModelUseFidA.set(input);
    }
  }

  public boolean isPostFlattenWarpInputTrimVol() {
    return postFlattenWarpContoursOnOneSurface.is();
  }

  public void setPostFlattenWarpInputTrimVol(boolean input) {
    postFlattenInputTrimVol.set(input);
  }

  public boolean isPostFlattenWarpContoursOnOneSurface() {
    return postFlattenWarpContoursOnOneSurface.is();
  }

  public void setPostFlattenWarpContoursOnOneSurface(boolean input) {
    postFlattenWarpContoursOnOneSurface.set(input);
  }

  public String getPostFlattenWarpSpacingInX() {
    return postFlattenWarpSpacingInX.toString();
  }

  public void setPostFlattenWarpSpacingInX(String input) {
    postFlattenWarpSpacingInX.set(input);
  }

  public String getPostFlattenWarpSpacingInY() {
    return postFlattenWarpSpacingInY.toString();
  }

  public void setPostFlattenWarpSpacingInY(String input) {
    postFlattenWarpSpacingInY.set(input);
  }

  public boolean isPostSqueezeVolInputTrimVol() {
    return postSqueezeVolInputTrimVol.is();
  }

  public boolean isTrackLengthAndOverlapSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !trackLengthAndOverlapB.isEmpty();
    }
    return !trackLengthAndOverlapA.isEmpty();
  }

  public boolean isTrackOverlapOfPatchesXandYSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !trackOverlapOfPatchesXandYB.isEmpty();
    }
    return !trackOverlapOfPatchesXandYA.isEmpty();
  }

  public void setPostSqueezeVolInputTrimVol(boolean input) {
    postSqueezeVolInputTrimVol.set(input);
  }

  public ConstEtomoNumber getNoBeamTiltSelected(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return noBeamTiltSelectedB;
    }
    return noBeamTiltSelectedA;
  }

  public ConstEtomoNumber getFixedBeamTiltSelected(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fixedBeamTiltSelectedB;
    }
    return fixedBeamTiltSelectedA;
  }

  public ConstEtomoNumber getFixedBeamTilt(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fixedBeamTiltB;
    }
    return fixedBeamTiltA;
  }

  public String getFinalStackFiducialDiameter(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return finalStackFiducialDiameterB.toString();
    }
    return finalStackFiducialDiameterA.toString();
  }

  /**
   * @deprecated in 1.10
   */
  public String getFinalStackBetterRadius(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return finalStackBetterRadiusB.toString();
    }
    return finalStackBetterRadiusA.toString();
  }

  public int getFinalStackPolynomialOrder(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return finalStackPolynomialOrderB.getInt();
    }
    return finalStackPolynomialOrderA.getInt();
  }

  public IntKeyList getTomoGenTrialTomogramNameList(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomoGenTrialTomogramNameListB;
    }
    return tomoGenTrialTomogramNameListA;
  }

  public String getTrackLengthAndOverlap(final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return trackLengthAndOverlapB.toString();
    }
    return trackLengthAndOverlapA.toString();
  }

  public String getTrackNumberOfPatchesXandY(final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return trackNumberOfPatchesXandYB.toString();
    }
    return trackNumberOfPatchesXandYA.toString();
  }

  public String getTrackOverlapOfPatchesXandY(final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return trackOverlapOfPatchesXandYB.toString();
    }
    return trackOverlapOfPatchesXandYA.toString();
  }

  public void setTomoGenTrialTomogramNameList(AxisID axisID, IntKeyList input) {
    if (axisID == AxisID.SECOND) {
      tomoGenTrialTomogramNameListB = input;
    }
    tomoGenTrialTomogramNameListA = input;
  }

  public void setTrackLengthAndOverlap(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      trackLengthAndOverlapB.set(input);
    }
    else {
      trackLengthAndOverlapA.set(input);
    }
  }

  public void setTrackMethod(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      trackMethodB.set(input);
    }
    else {
      trackMethodA.set(input);
    }
  }

  public void setTrackNumberOfPatchesXandY(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      trackNumberOfPatchesXandYB.set(input);
    }
    else {
      trackNumberOfPatchesXandYA.set(input);
    }
  }

  public void setTrackOverlapOfPatchesXandY(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      trackOverlapOfPatchesXandYB.set(input);
    }
    else {
      trackOverlapOfPatchesXandYA.set(input);
    }
  }

  public boolean isFinalStackFiducialDiameterNull(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return finalStackFiducialDiameterB.isNull();
    }
    return finalStackFiducialDiameterA.isNull();
  }

  public boolean isFineExists(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fineExistsB.is();
    }
    return fineExistsA.is();
  }

  public String getGenLog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return genLogB.toString();
    }
    return genLogA.toString();
  }

  public String getGenScaleFactorLog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return genScaleFactorLogB.toString();
    }
    return genScaleFactorLogA.toString();
  }

  public String getGenScaleOffsetLog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return genScaleOffsetLogB.toString();
    }
    return genScaleOffsetLogA.toString();
  }

  public String getGenScaleFactorLinear(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return genScaleFactorLinearB.toString();
    }
    return genScaleFactorLinearA.toString();
  }

  public String getGenScaleOffsetLinear(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return genScaleOffsetLinearB.toString();
    }
    return genScaleOffsetLinearA.toString();
  }

  /**
   * @deprecated in 1.10
   */
  public boolean isFinalStackBetterRadiusEmpty(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return finalStackBetterRadiusB.isEmpty();
    }
    return finalStackBetterRadiusA.isEmpty();
  }

  public String getTargetPatchSizeXandY() {
    return targetPatchSizeXandY;
  }

  public String getNumberOfLocalPatchesXandY() {
    return numberOfLocalPatchesXandY;
  }

  public boolean isFiducialess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fiducialessB.is();
    }
    return fiducialessA.is();
  }

  public TrimvolParam getTrimvolParam() {
    return trimvolParam;
  }

  public SqueezevolParam getSqueezevolParam() {
    return squeezevolParam;
  }

  public void getTransferfidAFields(TransferfidParam transferfidParam) {
    this.transferfidParamA.getStorableFields(transferfidParam);
  }

  public void getTransferfidBFields(TransferfidParam transferfidParam) {
    this.transferfidParamB.getStorableFields(transferfidParam);
  }

  public String getDatasetName() {
    return datasetName;
  }

  public String getMetaDataFileName() {
    if (datasetName.equals("")) {
      return "";
    }
    return datasetName + fileExtension;
  }

  public String getTrackMethod(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return trackMethodB.toString();
    }
    return trackMethodA.toString();

  }

  public String getName() {
    if (datasetName.equals("")) {
      return newTomogramTitle;
    }
    return datasetName;
  }

  public static String getNewFileTitle() {
    return newTomogramTitle;
  }

  public String getBackupDirectory() {
    return backupDirectory;
  }

  public String getDistortionFile() {
    return distortionFile;
  }

  public FortranInputString getSizeToOutputInXandY(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sizeToOutputInXandYB;
    }
    return sizeToOutputInXandYA;
  }

  public String getMagGradientFile() {
    return magGradientFile;
  }

  public ConstEtomoNumber getAdjustedFocusA() {
    return adjustedFocusA;
  }

  public ConstEtomoNumber getAdjustedFocusB() {
    return adjustedFocusB;
  }

  public DataSource getDataSource() {
    return dataSource;
  }

  public ViewType getViewType() {
    return viewType;
  }

  public double getPixelSize() {
    return pixelSize;
  }

  public boolean getUseLocalAlignments(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return useLocalAlignmentsB;
    }
    return useLocalAlignmentsA;
  }

  public int getPosBinning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return posBinningB.getDefaultedInt();
    }
    return posBinningA.getDefaultedInt();
  }

  public int getStackBinning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return stackBinningB.getDefaultedInt();
    }
    return stackBinningA.getDefaultedInt();
  }

  public int getStack3dFindBinning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return stack3dFindBinningB.getDefaultedInt();
    }
    return stack3dFindBinningA.getDefaultedInt();
  }

  public ConstEtomoNumber getPostCurTab() {
    return postCurTab;
  }

  public boolean isPostExists() {
    return postExists.is();
  }

  public ConstEtomoNumber getCombineVolcombineParallel() {
    return combineVolcombineParallel;
  }

  public ConstEtomoNumber getTiltParallel(AxisID axisID, PanelId panelId) {
    if (panelId == PanelId.TILT) {
      if (axisID == AxisID.SECOND) {
        return tomoGenTiltParallelB;
      }
      return tomoGenTiltParallelA;
    }
    else if (panelId == PanelId.TILT_3D_FIND) {
      if (axisID == AxisID.SECOND) {
        return tilt3dFindTiltParallelB;
      }
      return tilt3dFindTiltParallelA;
    }
    return null;
  }

  public ConstEtomoNumber getFinalStackCtfCorrectionParallel(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return finalStackCtfCorrectionParallelB;
    }
    return finalStackCtfCorrectionParallelA;
  }

  public ConstEtomoNumber getDefaultParallel() {
    return defaultParallel;
  }

  public ConstEtomoNumber getDefaultGpuProcessing() {
    return defaultGpuProcessing;
  }

  public ConstEtomoNumber getUseZFactors(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return useZFactorsB;
    }
    return useZFactorsA;
  }

  public double getFiducialDiameter() {
    return fiducialDiameter;
  }

  public float getImageRotation(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return imageRotationB;
    }
    return imageRotationA;
  }

  public int getBinning() {
    return binning;
  }

  public ConstEtomoNumber getBStackProcessed() {
    return bStackProcessed;
  }

  public EtomoNumber getSampleThickness(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sampleThicknessB;
    }
    return sampleThicknessA;
  }

  public TiltAngleSpec getTiltAngleSpecA() {
    return tiltAngleSpecA;
  }

  public TiltAngleSpec getTiltAngleSpec(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tiltAngleSpecB;
    }
    return tiltAngleSpecA;
  }

  public TiltAngleSpec getTiltAngleSpecB() {
    return tiltAngleSpecB;
  }

  public String getExcludeProjectionsA() {
    return excludeProjectionsA;
  }

  public String getExcludeProjectionsB() {
    return excludeProjectionsB;
  }

  public boolean isGenExists(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return genExistsB.is();
    }
    return genExistsA.is();
  }
  
  public boolean isPosExists(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return posExistsB.is();
    }
    return posExistsA.is();
  }

  public boolean isGenScaleFactorLinearSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !genScaleFactorLinearB.isNull();
    }
    return !genScaleFactorLinearA.isNull();
  }

  public boolean isGenScaleOffsetLinearSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !genScaleOffsetLinearB.isNull();
    }
    return !genScaleOffsetLinearA.isNull();
  }

  public boolean getComScriptCreated() {
    return comScriptsCreated;
  }

  public boolean isFiducialessAlignment(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fiducialessAlignmentB;
    }
    return fiducialessAlignmentA;
  }

  public boolean isDistortionCorrection() {
    return !distortionFile.equals("") || !magGradientFile.equals("");
  }

  public boolean isWholeTomogramSample(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return wholeTomogramSampleB;
    }
    return wholeTomogramSampleA;
  }

  public ConstCombineParams getConstCombineParams() {
    return combineParams;
  }

  public CombineParams getCombineParams() {
    return combineParams;
  }

  public boolean isValid() {
    return isValid(true, null);
  }

  public boolean isValid(boolean fromScreen) {
    return isValid(fromScreen, null);
  }

  public boolean isValid(File paramFile) {
    return isValid(false, paramFile);
  }

  public boolean isValid(boolean fromScreen, File paramFile) {
    invalidReason = "";

    String helpString;
    if (!fromScreen) {
      helpString = "  Check the Etomo data file.";
    }
    else {
      helpString = "";
    }

    if (axisType == null || axisType == AxisType.NOT_SET) {
      invalidReason = "Axis type should be either Dual Axis or Single Axis."
          + helpString;
      return false;
    }

    if (!isDatasetNameValid(paramFile)) {
      invalidReason += helpString;
      return false;
    }

    // Is the pixel size greater than zero
    if (fromScreen && pixelSize <= 0.0) {
      invalidReason = "Pixel size is not greater than zero.";
      return false;
    }

    // Is the fiducial diameter greater than zero
    if (fromScreen && fiducialDiameter <= 0.0) {
      invalidReason = "Fiducial diameter is not greater than zero.";
      return false;
    }

    return true;
  }

  public boolean isDatasetNameValid() {
    return isDatasetNameValid(null);
  }

  public boolean isDatasetNameValid(File paramFile) {
    invalidReason = "";
    if (datasetName.equals("")) {
      invalidReason = "Dataset name has not been set.";
      return false;
    }
    if (paramFile == null) {
      if (getValidDatasetDirectory(manager.getPropertyUserDir()) != null) {
        return true;
      }
    }
    else {
      if (getValidDatasetDirectory(new File(paramFile.getParent())
          .getAbsolutePath()) != null) {
        return true;
      }
    }
    return false;
  }

  public File getValidDatasetDirectory(String workingDirName) {
    // Does the working directory exist
    // If is doesn't then use the backup directory.    
    File workingDir = new File(workingDirName);
    File backupDir = new File(backupDirectory);
    File currentDir;

    //find a valid directory and set directory and type
    if (isValid(workingDir, true)) {
      currentDir = workingDir;
    }
    else if (isValid(backupDir, true)) {
      currentDir = backupDir;
    }
    else {
      //can't find a valid directory, report error

      //if no directory exists then exit
      if (!workingDir.exists() && !backupDir.exists()) {
        invalidReason = "The working directory: "
            + workingDir.getAbsolutePath() + " and the backup directory: "
            + backupDir.getAbsolutePath() + " do not exist";
        return null;
      }

      //decide which directory to complain about:
      //complain about the working directory, if it exists
      if (workingDir.exists()) {
        currentDir = workingDir;
      }
      else {
        currentDir = backupDir;
      }

      if (!currentDir.canRead()) {
        invalidReason = "Can't read " + currentDir.getAbsolutePath()
            + " directory";
        return null;
      }

      if (!currentDir.canWrite()) {
        invalidReason = "Can't write " + currentDir.getAbsolutePath()
            + " directory";
        return null;
      }

      throw new IllegalStateException("Working directory ="
          + workingDir.toString() + ",backupDir=" + backupDir.toString());
    }

    // Does the appropriate image stack exist in the working directory
    if (axisType == AxisType.DUAL_AXIS) {
      currentDir = findValidFile(datasetName + "a.st", currentDir, backupDir);
    }
    else {
      currentDir = findValidFile(datasetName + ".st", currentDir, backupDir);
    }
    return currentDir;
  }

  /**
   * Checks a file's state.  Checks whether a file exists and
   * is readable.  Optionally checks whether a file is
   * writable.
   * 
   * @param file
   * @param writeable - If true, the file must be writeable
   * @return boolean
   */
  static boolean isValid(File file, boolean writeable) {
    if (file == null) {
      return false;
    }
    if (!file.exists()) {
      return false;
    }

    return file.canRead() && (!writeable || file.canWrite());
  }

  private void appendMessage(String string) {
    message.append(string);
  }

  /**
   * Finds a file in either the current directory or an
   * alternate directory.  The file must be readable.
   * 
   * The current directory state variable can point to the 
   * alternative directory state instance.
   * In this case, only the alternative directory is checked.
   * 
   * Side Effect:
   * If the function returns null, it places an error message
   * into ConstMetaData.invalidReason.
   * 
   * @param fileName - Name of file to look for
   * @param curDir - The current directory.  This directory should be valid.
   * @param altDir - The alternate directory.
   * @return Success:  directory where file found.  Failure: null.
   * @throws IllegalArgumentException if any parameter is null
   */
  File findValidFile(String fileName, File curDir, File altDir) {
    if (fileName == null || curDir == null || altDir == null
        || !isValid(curDir, true)) {
      throw new IllegalArgumentException(
          "ConstMetaData.findValidFile(String,File,File)");
    }

    // Does the appropriate image stack exist in the working or backup directory
    File file = new File(curDir, fileName);
    while (!file.exists()) {
      if (curDir == altDir || !isValid(altDir, true)) {
        message.append(fileName + " does not exist in  "
            + curDir.getAbsolutePath());
        invalidReason = message.toString();
        message = new StringBuffer();
        return null;
      }
      curDir = altDir;
      file = new File(curDir, fileName);
    }

    if (!file.canRead()) {
      invalidReason = "Can't read " + fileName;
      return null;
    }

    return curDir;
  }

  /**
   * Finds a file in the current directory.  The file must be readable.
   * 
   * Side Effect:
   * If the function returns null, it places an error message
   * into ConstMetaData.invalidReason.
   * 
   * @param fileName - Name of file to look for
   * @param curDir - The current directory.  This directory should be valid.
   * @return Success:  directory where file found.  Failure: null.
   * @throws IllegalArgumentException if any parameter is null
   */
  File findValidFile(String fileName, File curDir) {
    if (fileName == null || curDir == null || !isValid(curDir, true)) {
      throw new IllegalArgumentException(
          "ConstMetaData.findValidFile(String,File)");
    }

    // Does the appropriate image stack exist in the working or backup directory
    File file = new File(curDir, fileName);

    if (!file.exists()) {
      invalidReason = fileName + " does not exist in "
          + curDir.getAbsolutePath();
      return null;
    }

    if (!file.canRead()) {
      invalidReason = "Can't read " + fileName;
      return null;
    }

    return curDir;
  }

  public boolean equals(Object object) {
    if (!(object instanceof MetaData))
      return false;

    MetaData cmd = (MetaData) object;
    if (!datasetName.equals(cmd.datasetName))
      return false;
    if (!backupDirectory.equals(cmd.backupDirectory))
      return false;
    if (!distortionFile.equals(cmd.distortionFile))
      return false;
    if (!magGradientFile.equals(cmd.magGradientFile))
      return false;
    if (!dataSource.equals(cmd.dataSource))
      return false;
    if (axisType != cmd.axisType)
      return false;
    if (!viewType.equals(cmd.viewType))
      return false;
    if (!(pixelSize == cmd.pixelSize))
      return false;
    if (!(useLocalAlignmentsA == cmd.useLocalAlignmentsA))
      return false;
    if (!(useLocalAlignmentsB == cmd.useLocalAlignmentsB))
      return false;
    if (!(fiducialDiameter == cmd.fiducialDiameter))
      return false;
    if (!(imageRotationA == cmd.imageRotationA))
      return false;
    if (!(imageRotationB == cmd.imageRotationB))
      return false;
    if (!(binning == cmd.binning))
      return false;
    if (!(fiducialessAlignmentA == cmd.fiducialessAlignmentA))
      return false;
    if (!(fiducialessAlignmentB == cmd.fiducialessAlignmentB))
      return false;

    // TODO tilt angle spec needs to be more complete
    if (!(tiltAngleSpecA.getType() == cmd.getTiltAngleSpecA().getType()))
      return false;
    if (!excludeProjectionsA.equals(cmd.getExcludeProjectionsA()))
      return false;

    if (!(tiltAngleSpecB.getType() == cmd.getTiltAngleSpecB().getType()))
      return false;
    if (!excludeProjectionsB.equals(cmd.getExcludeProjectionsB()))
      return false;
    if (!(comScriptsCreated == cmd.getComScriptCreated()))
      return false;
    if (!combineParams.equals(cmd.getConstCombineParams())) {
      return false;
    }
    if (!trimvolParam.equals(cmd.getTrimvolParam())) {
      return false;
    }
    if (!squeezevolParam.equals(cmd.getSqueezevolParam())) {
      return false;
    }
    if (!posBinningA.equals(cmd.posBinningA)) {
      return false;
    }
    if (!posBinningB.equals(cmd.posBinningB)) {
      return false;
    }
    if (!stackBinningA.equals(cmd.stackBinningA)) {
      return false;
    }
    if (!stackBinningB.equals(cmd.stackBinningB)) {
      return false;
    }
    if (!stack3dFindBinningA.equals(cmd.stack3dFindBinningA)) {
      return false;
    }
    if (!stack3dFindBinningB.equals(cmd.stack3dFindBinningB)) {
      return false;
    }
    if (!stackEraseGoldModelUseFidA.equals(cmd.stackEraseGoldModelUseFidA)) {
      return false;
    }
    if (!stackEraseGoldModelUseFidB.equals(cmd.stackEraseGoldModelUseFidB)) {
      return false;
    }
    return true;
  }
}