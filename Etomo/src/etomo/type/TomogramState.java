package etomo.type;

import java.io.File;
import java.io.IOException;
import java.util.Properties;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TrimvolParam;
import etomo.util.MRCHeader;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
 *
 *<p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$
 * <p> Revision 1.40  2011/02/22 05:54:05  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.39  2010/03/12 04:08:17  sueh
 * <p> bug# 1325 Added use button warnings.
 * <p>
 * <p> Revision 1.38  2010/02/17 04:52:36  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 1.37  2009/09/01 03:16:20  sueh
 * <p> bug# 1222 Added state settings for erase gold find 3d.
 * <p>
 * <p> Revision 1.36  2009/03/17 00:46:15  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.35  2009/02/13 02:32:40  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.
 * <p>
 * <p> Revision 1.34  2009/02/10 22:19:37  sueh
 * <p> bug# 1143 Added postProcTrimVolInputNColumns, Rows, and Sections.
 * <p>
 * <p> Revision 1.33  2007/12/13 01:13:20  sueh
 * <p> bug# 1056 Added adjustOrigin.
 * <p>
 * <p> Revision 1.32  2007/09/07 00:25:31  sueh
 * <p> bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * <p> instead of getInstance and createInstance.
 * <p>
 * <p> Revision 1.31  2007/08/29 21:45:50  sueh
 * <p> bug# 1041 Made BaseState an abstract class.
 * <p>
 * <p> Revision 1.30  2007/08/21 21:52:14  sueh
 * <p> bug# 771 Added tomogramSizeA and B.
 * <p>
 * <p> Revision 1.29  2007/05/11 16:06:24  sueh
 * <p> bug# 964 Reduced the visibility of createPrepend(String).
 * <p>
 * <p> Revision 1.28  2007/02/05 23:31:46  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.27  2006/09/19 22:35:11  sueh
 * <p> bug# 920 Added first and secondAxisGroups for saving axis-level values.  Fixed
 * <p> sample fiducial key.
 * <p>
 * <p> Revision 1.26  2006/09/13 23:38:59  sueh
 * <p> bug# 920 Added sampleFiducialessA and B.
 * <p>
 * <p> Revision 1.25  2006/08/14 22:22:41  sueh
 * <p> bug# 891 Added resetCombineScriptsCreated().
 * <p>
 * <p> Revision 1.24  2006/07/28 17:42:50  sueh
 * <p> bug# 909 Changed TomogramState.combineScriptsCreated to an EtomoState so
 * <p> it will show when it has not been set.  Deleting old versions of combine scripts
 * <p> create and match mode in the .edf.  Getting data from the old versions of
 * <p> combine scripts create and match mode with the new ones aren't available.
 * <p>
 * <p> Revision 1.23  2006/07/19 20:11:15  sueh
 * <p> bug# 902 Added seedingDoneA and B.
 * <p>
 * <p> Revision 1.22  2006/06/09 16:57:59  sueh
 * <p> bug# 869 Added combineMatchMode and combineScrptsCreated.
 * <p>
 * <p> Revision 1.21  2006/06/07 22:25:16  sueh
 * <p> bug# 862 Added fixedFiducials, which is set to true that first time fix fiducials is
 * <p> used.
 * <p>
 * <p> Revision 1.20  2006/05/23 21:06:23  sueh
 * <p> bug# 617 Added fidFileLastModfied and seedFileLastModified.
 * <p>
 * <p> Revision 1.19  2006/05/19 19:44:53  sueh
 * <p> bug# 838 Added sampleAngleOffset and SampleAxisZShift.  Changed xAxisTil
 * <p> to sampleXAxisTilt.
 * <p>
 * <p> Revision 1.17  2006/05/11 19:59:04  sueh
 * <p> bug# 838 Added angle offset, z shift, and x axis tilt.
 * <p>
 * <p> Revision 1.16  2006/03/20 18:00:42  sueh
 * <p> bug# 835 Added getName (a convenience function) to managers.
 * <p>
 * <p> Revision 1.15  2005/08/24 22:40:18  sueh
 * <p> bug# 715 Added invalidEdgeFunctionsA and B.
 * <p>
 * <p> Revision 1.14  2005/07/29 00:53:47  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.13  2005/07/20 17:53:43  sueh
 * <p> bug# 705 Stop printing the stack trace for IOException bugs coming from
 * <p> MRCHeader, because its filling up the error log with exceptions that are
 * <p> related to real problems.
 * <p>
 * <p> Revision 1.12  2005/06/20 16:54:26  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in setBackwardCompatibleTrimvolFlipped().
 * <p>
 * <p> Revision 1.11  2005/06/11 02:43:22  sueh
 * <p> Changed initialize() to not require a parameter.
 * <p>
 * <p> Revision 1.10  2005/04/25 20:51:43  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.9  2005/02/17 19:27:05  sueh
 * <p> bug# 606 Removed makeZFactors, newstFiducialessAlignment, and
 * <p> usedLocalAlignments.  Add makeZFactors, newstFiducialessAlignment,
 * <p> and usedLocalAlignments for A and B.
 * <p>
 * <p> Revision 1.8  2005/01/12 00:44:36  sueh
 * <p> bug# 579 Adding usedLocalAlignments.
 * <p>
 * <p> Revision 1.7  2005/01/10 23:54:23  sueh
 * <p> bug# 578 Switched member variables from EtomoBoolean to EtomoState.
 * <p> Added initialize(int) to initialize EtomoState variables to
 * <p> NO_RESULTS_VALUE.  Removed backward compatibility function for
 * <p> newstFiducialAlignment.  EtomoState variables are necessary when the
 * <p> state information was not saved in the past.
 * <p>
 * <p> Revision 1.6  2005/01/08 01:54:43  sueh
 * <p> bug# 578 Removed alignSkewOption and alignXStretchOption.  Added
 * <p> madeZFactors and newstFiducialAlignment.  Added
 * <p> getBackwordCompatible functions for madeZFactors and
 * <p> newstFiducialessAlignment.
 * <p>
 * <p> Revision 1.5  2005/01/06 18:19:05  sueh
 * <p> bug# 578 added alignSkewOption and alignXStretchOption.
 * <p>
 * <p> Revision 1.4  2004/12/16 02:31:24  sueh
 * <p> bug# 564 Manage trimvol flipped state and squeezevol flipped state
 * <p> separately.  If trimvol flipped state changes and squeezevol is not rerun,
 * <p> the squeezevol parameters with still load onto the screen correctly.
 * <p>
 * <p> Revision 1.3  2004/12/14 21:49:04  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  All state variables saved after a process is run belong in
 * <p> the state object.
 * <p>
 * <p> Revision 1.2  2004/12/08 21:32:04  sueh
 * <p> bug# 564 Added access to flipped.
 * <p>
 * <p> Revision 1.1  2004/12/07 22:54:07  sueh
 * <p> bug# 564 Contains state variables to be saved in the .edf file.
 * <p> </p>
 */
public class TomogramState extends BaseState {
  public static final String rcsid = "$Id$";

  private static final String groupString = "ReconstructionState";
  private static final String LAST_MODIFIED = "LastModified";
  private static final String USE_FID_AS_SEED = "UseFidAsSeed";
  private static final String FIXED_FIDUCIALS_KEY = "FixedFiducials";
  private static final String COMBINE_MATCH_MODE_KEY = DialogType.TOMOGRAM_COMBINATION
      .getStorableName() + "." + "MatchMode";
  private static final String COMBINE_MATCH_MODE_BACK_KEY = "Setup.Combine.MatchBtoA";
  private static final String COMBINE_SCRIPTS_CREATED_BACK_KEY = "Setup.ComScriptsCreated";
  private static final String SAMPLE_FIDUCIALESS_KEY = DialogType.TOMOGRAM_POSITIONING
      .getStorableName() + '.' + ProcessName.TILT + '.' + "Fiducialess";
  private static final String X_AXIS_TILT_KEY = "XAXISTILT";
  private static final String ADJUST_ORIGIN_KEY = "AdjustOrigin";
  private static final String A_AXIS_KEY = "A";
  private static final String B_AXIS_KEY = "B";
  private static final String TOMOGRAM_SIZE_KEY = "tomogramSize";

  // Dialog keys
  private static final String PRE_KEY = "Pre";
  private static final String TRACK_KEY = "Track";
  private static final String STACK_KEY = "Stack";
  private static final String GEN_KEY = "Gen";

  EtomoState trimvolFlipped = new EtomoState("TrimvolFlipped");
  EtomoState squeezevolFlipped = new EtomoState("SqueezevolFlipped");
  EtomoState madeZFactorsA = new EtomoState("MadeZFactors" + A_AXIS_KEY);
  EtomoState madeZFactorsB = new EtomoState("MadeZFactorsB");
  EtomoState newstFiducialessAlignmentA = new EtomoState("NewstFiducialessAlignmentA");
  EtomoState newstFiducialessAlignmentB = new EtomoState("NewstFiducialessAlignmentB");
  EtomoState usedLocalAlignmentsA = new EtomoState("UsedLocalAlignments" + A_AXIS_KEY);
  EtomoState usedLocalAlignmentsB = new EtomoState("UsedLocalAlignments" + B_AXIS_KEY);
  EtomoState invalidEdgeFunctionsA = new EtomoState("InvalidEdgeFunctions" + A_AXIS_KEY);
  EtomoState invalidEdgeFunctionsB = new EtomoState("InvalidEdgeFunctions" + B_AXIS_KEY);
  private final EtomoNumber angleOffsetA = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.FIRST.getExtension() + '.' + ProcessName.ALIGN + '.'
          + ConstTiltalignParam.ANGLE_OFFSET_KEY);
  private final EtomoNumber angleOffsetB = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.SECOND.getExtension() + '.' + ProcessName.ALIGN + '.'
          + ConstTiltalignParam.ANGLE_OFFSET_KEY);
  private final EtomoNumber axisZShiftA = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.FIRST.getExtension() + '.' + ProcessName.ALIGN + '.'
          + ConstTiltalignParam.AXIS_Z_SHIFT_KEY);
  private final EtomoNumber axisZShiftB = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.SECOND.getExtension() + '.' + ProcessName.ALIGN + '.'
          + ConstTiltalignParam.AXIS_Z_SHIFT_KEY);

  private final EtomoNumber sampleAngleOffsetA = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.FIRST.getExtension() + '.' + ProcessName.SAMPLE + '.'
          + ConstTiltalignParam.ANGLE_OFFSET_KEY);
  private final EtomoNumber sampleAngleOffsetB = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.SECOND.getExtension() + '.' + ProcessName.SAMPLE + '.'
          + ConstTiltalignParam.ANGLE_OFFSET_KEY);
  private final EtomoNumber sampleAxisZShiftA = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.FIRST.getExtension() + '.' + ProcessName.SAMPLE + '.'
          + ConstTiltalignParam.AXIS_Z_SHIFT_KEY);
  private final EtomoNumber sampleAxisZShiftB = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.SECOND.getExtension() + '.' + ProcessName.SAMPLE + '.'
          + ConstTiltalignParam.AXIS_Z_SHIFT_KEY);
  private final EtomoNumber sampleXAxisTiltA = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.FIRST.getExtension() + '.' + ProcessName.SAMPLE + '.' + X_AXIS_TILT_KEY);
  private final EtomoNumber sampleXAxisTiltB = new EtomoNumber(EtomoNumber.Type.DOUBLE,
      AxisID.SECOND.getExtension() + '.' + ProcessName.SAMPLE + '.' + X_AXIS_TILT_KEY);

  private final EtomoNumber fidFileLastModifiedA = new EtomoNumber(EtomoNumber.Type.LONG,
      AxisID.FIRST.getExtension() + '.' + ProcessName.TRACK + '.' + LAST_MODIFIED);
  private final EtomoNumber fidFileLastModifiedB = new EtomoNumber(EtomoNumber.Type.LONG,
      AxisID.SECOND.getExtension() + '.' + ProcessName.TRACK + '.' + LAST_MODIFIED);
  private final EtomoNumber seedFileLastModifiedA = new EtomoNumber(
      EtomoNumber.Type.LONG, AxisID.FIRST.getExtension() + '.' + USE_FID_AS_SEED + '.'
          + LAST_MODIFIED);
  private final EtomoNumber seedFileLastModifiedB = new EtomoNumber(
      EtomoNumber.Type.LONG, AxisID.SECOND.getExtension() + '.' + USE_FID_AS_SEED + '.'
          + LAST_MODIFIED);
  private final EtomoBoolean2 fixedFiducialsA = new EtomoBoolean2(
      AxisID.FIRST.getExtension() + "." + FIXED_FIDUCIALS_KEY);
  private final EtomoBoolean2 fixedFiducialsB = new EtomoBoolean2(
      AxisID.SECOND.getExtension() + "." + FIXED_FIDUCIALS_KEY);
  private MatchMode combineMatchMode = null;
  private final EtomoState combineScriptsCreated = new EtomoState(
      DialogType.TOMOGRAM_COMBINATION.getStorableName() + "." + "ScriptsCreated");
  private final EtomoBoolean2 seedingDoneA = new EtomoBoolean2(
      AxisID.FIRST.getExtension() + '.' + "SeedingDone");
  private final EtomoBoolean2 seedingDoneB = new EtomoBoolean2(
      AxisID.SECOND.getExtension() + '.' + "SeedingDone");
  private final EtomoBoolean2 xcorrBlendmontWasRunA = new EtomoBoolean2(
      "xcorr.blendmont.a.WasRun");
  private final EtomoBoolean2 xcorrBlendmontWasRunB = new EtomoBoolean2(
      "xcorr.blendmont.b.WasRun");

  private EtomoBoolean2 sampleFiducialessA = null;
  private EtomoBoolean2 sampleFiducialessB = null;
  private final ApplicationManager manager;
  private String firstAxisGroup = null;
  private String secondAxisGroup = null;
  /**
   * @deprecated Eventually this should be completely replaced by
   * tomogramSizeColumns/Rows/SectionsA. Keeping deprecated field because there is no way
   * to do backwards compatibility with the replacements.   Deprecated field should not be
   * stored.
   */
  private EtomoNumber tomogramSizeA = new EtomoNumber(EtomoNumber.Type.LONG, A_AXIS_KEY
      + "." + TOMOGRAM_SIZE_KEY);
  /**
   * @deprecated Eventually this should be completely replaced by
   * tomogramSizeColumns/Rows/SectionsB. Keeping deprecated field because there is no way
   * to do backwards compatibility with the replacements.   Deprecated field should not be
   * stored.
   */
  private EtomoNumber tomogramSizeB = new EtomoNumber(EtomoNumber.Type.LONG, B_AXIS_KEY
      + "." + TOMOGRAM_SIZE_KEY);
  private final EtomoBoolean2 adjustOriginA = new EtomoBoolean2(A_AXIS_KEY + "."
      + ADJUST_ORIGIN_KEY);
  private final EtomoBoolean2 adjustOriginB = new EtomoBoolean2(B_AXIS_KEY + "."
      + ADJUST_ORIGIN_KEY);
  private final EtomoNumber postProcTrimVolInputNColumns = new EtomoNumber(
      DialogType.POST_PROCESSING.getStorableName() + ".TrimVol.Input.NColumns");
  private final EtomoNumber postProcTrimVolInputNRows = new EtomoNumber(
      DialogType.POST_PROCESSING.getStorableName() + ".TrimVol.Input.NRows");
  private final EtomoNumber postProcTrimVolInputNSections = new EtomoNumber(
      DialogType.POST_PROCESSING.getStorableName() + ".TrimVol.Input.NSections");
  private final EtomoBoolean2 stackUseLinearInterpolationA = new EtomoBoolean2(STACK_KEY
      + "." + A_AXIS_KEY + ".UseLinearInterpolation");
  private final EtomoBoolean2 stackUseLinearInterpolationB = new EtomoBoolean2(STACK_KEY
      + "." + B_AXIS_KEY + ".UseLinearInterpolation");
  private final StringProperty stackUserSizeToOutputInXandYA = new StringProperty(
      STACK_KEY + "." + A_AXIS_KEY + ".SizeToOutputInXandY");
  private final StringProperty stackUserSizeToOutputInXandYB = new StringProperty(
      STACK_KEY + "." + B_AXIS_KEY + ".SizeToOutputInXandY");
  private final EtomoNumber stackImageRotationA = new EtomoNumber(STACK_KEY + "."
      + A_AXIS_KEY + ".ImageRotation");
  private final EtomoNumber stackImageRotationB = new EtomoNumber(STACK_KEY + "."
      + B_AXIS_KEY + ".ImageRotation");
  private final EtomoNumber trackLightBeadsA = new EtomoNumber("Track.A.LightBeads");
  private final EtomoNumber trackLightBeadsB = new EtomoNumber("Track.B.LightBeads");
  private final EtomoBoolean2 stackUsingNewstOrBlend3dFindOutputA = new EtomoBoolean2(
      "Track.A.UsingNewstOrBlend3dFindOutput");
  private final EtomoBoolean2 stackUsingNewstOrBlend3dFindOutputB = new EtomoBoolean2(
      "Track.B.UsingNewstOrBlend3dFindOutput");

  private final EtomoBoolean2 useFixedStackWarningA = new EtomoBoolean2(PRE_KEY
      + ".A.UseFixedStack.Warning");
  private final EtomoBoolean2 useFixedStackWarningB = new EtomoBoolean2(PRE_KEY
      + ".B.UseFixedStack.Warning");
  private final EtomoBoolean2 useRaptorResultWarningA = new EtomoBoolean2(TRACK_KEY
      + ".A.UseRaptorResult.Warning");
  private final EtomoBoolean2 useCtfCorrectionWarningA = new EtomoBoolean2(STACK_KEY
      + ".A.UseCtfCorrection.Warning");
  private final EtomoBoolean2 useCtfCorrectionWarningB = new EtomoBoolean2(STACK_KEY
      + ".B.UseCtfCorrection.Warning");
  private final EtomoBoolean2 useErasedStackWarningA = new EtomoBoolean2(STACK_KEY
      + ".A.UseErasedStack.Warning");
  private final EtomoBoolean2 useErasedStackWarningB = new EtomoBoolean2(STACK_KEY
      + ".B.UseErasedStack.Warning");
  private final EtomoBoolean2 useFilteredStackWarningA = new EtomoBoolean2(STACK_KEY
      + ".A.UseFilteredStack.Warning");
  private final EtomoBoolean2 useFilteredStackWarningB = new EtomoBoolean2(STACK_KEY
      + ".B.UseFilteredStack.Warning");

  private final StringProperty genSirtsetupSubareaSizeA = new StringProperty(GEN_KEY
      + "." + A_AXIS_KEY + ".SirtsetupSubareaSize");
  private final StringProperty genSirtsetupSubareaSizeB = new StringProperty(GEN_KEY
      + "." + B_AXIS_KEY + ".SirtsetupSubareaSize");
  private final EtomoNumber genSirtsetupyOffsetOfSubareaA = new EtomoNumber(GEN_KEY + "."
      + A_AXIS_KEY + ".SirtsetupyOffsetOfSubarea");
  private final EtomoNumber genSirtsetupyOffsetOfSubareaB = new EtomoNumber(GEN_KEY + "."
      + B_AXIS_KEY + ".SirtsetupyOffsetOfSubarea");

  private EtomoNumber tomogramSizeColumnsA = new EtomoNumber(A_AXIS_KEY + "."
      + TOMOGRAM_SIZE_KEY + ".Columns");
  private EtomoNumber tomogramSizeColumnsB = new EtomoNumber(B_AXIS_KEY + "."
      + TOMOGRAM_SIZE_KEY + ".Columns");
  private EtomoNumber tomogramSizeRowsA = new EtomoNumber(A_AXIS_KEY + "."
      + TOMOGRAM_SIZE_KEY + ".Rows");
  private EtomoNumber tomogramSizeRowsB = new EtomoNumber(B_AXIS_KEY + "."
      + TOMOGRAM_SIZE_KEY + ".Rows");
  private EtomoNumber tomogramSizeSectionsA = new EtomoNumber(A_AXIS_KEY + "."
      + TOMOGRAM_SIZE_KEY + ".Sections");
  private EtomoNumber tomogramSizeSectionsB = new EtomoNumber(B_AXIS_KEY + "."
      + TOMOGRAM_SIZE_KEY + ".Sections");

  public TomogramState(ApplicationManager manager) {
    this.manager = manager;
  }

  /**
   * Get the axis keys from meta data.  If dual axis, create a first axis group
   * string that ends in ".", so .  For single axis, the first axis key is an empty string
   * and the second axis key is null.
   * @param metaData
   */
  private void setAxisPrepends(ConstMetaData metaData) {
    firstAxisGroup = metaData.getFirstAxisPrepend();
    secondAxisGroup = metaData.getSecondAxisPrepend();
    if (secondAxisGroup != null) {
      firstAxisGroup += '.';
      secondAxisGroup += '.';
    }
  }

  public void initialize() {
    trimvolFlipped.set(EtomoState.NO_RESULT_VALUE);
    squeezevolFlipped.set(EtomoState.NO_RESULT_VALUE);
    madeZFactorsA.set(EtomoState.NO_RESULT_VALUE);
    madeZFactorsB.set(EtomoState.NO_RESULT_VALUE);
    newstFiducialessAlignmentA.set(EtomoState.NO_RESULT_VALUE);
    newstFiducialessAlignmentB.set(EtomoState.NO_RESULT_VALUE);
    usedLocalAlignmentsA.set(EtomoState.NO_RESULT_VALUE);
    usedLocalAlignmentsB.set(EtomoState.NO_RESULT_VALUE);
    invalidEdgeFunctionsA.set(EtomoState.NO_RESULT_VALUE);
    invalidEdgeFunctionsB.set(EtomoState.NO_RESULT_VALUE);
    combineScriptsCreated.set(EtomoState.NO_RESULT_VALUE);
  }

  public void store(Properties props) {
    store(props, "");
  }

  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    trimvolFlipped.store(props, prepend);
    squeezevolFlipped.store(props, prepend);
    madeZFactorsA.store(props, prepend);
    madeZFactorsB.store(props, prepend);
    newstFiducialessAlignmentA.store(props, prepend);
    newstFiducialessAlignmentB.store(props, prepend);
    usedLocalAlignmentsA.store(props, prepend);
    usedLocalAlignmentsB.store(props, prepend);
    invalidEdgeFunctionsA.store(props, prepend);
    invalidEdgeFunctionsB.store(props, prepend);
    angleOffsetA.store(props, prepend);
    angleOffsetB.store(props, prepend);
    axisZShiftA.store(props, prepend);
    axisZShiftB.store(props, prepend);
    sampleAngleOffsetA.store(props, prepend);
    sampleAngleOffsetB.store(props, prepend);
    sampleAxisZShiftA.store(props, prepend);
    sampleAxisZShiftB.store(props, prepend);
    sampleXAxisTiltA.store(props, prepend);
    sampleXAxisTiltB.store(props, prepend);
    fidFileLastModifiedA.store(props, prepend);
    fidFileLastModifiedB.store(props, prepend);
    seedFileLastModifiedA.store(props, prepend);
    seedFileLastModifiedB.store(props, prepend);
    fixedFiducialsA.store(props, prepend);
    fixedFiducialsB.store(props, prepend);
    seedingDoneA.store(props, prepend);
    seedingDoneB.store(props, prepend);
    adjustOriginA.store(props, prepend);
    adjustOriginB.store(props, prepend);
    postProcTrimVolInputNColumns.store(props, prepend);
    postProcTrimVolInputNRows.store(props, prepend);
    postProcTrimVolInputNSections.store(props, prepend);
    stackUseLinearInterpolationA.store(props, prepend);
    stackUseLinearInterpolationB.store(props, prepend);
    stackUserSizeToOutputInXandYA.store(props, prepend);
    stackUserSizeToOutputInXandYB.store(props, prepend);
    stackImageRotationA.store(props, prepend);
    stackImageRotationB.store(props, prepend);
    trackLightBeadsA.store(props, prepend);
    trackLightBeadsB.store(props, prepend);
    stackUsingNewstOrBlend3dFindOutputA.store(props, prepend);
    stackUsingNewstOrBlend3dFindOutputB.store(props, prepend);
    useFixedStackWarningA.store(props, prepend);
    useFixedStackWarningB.store(props, prepend);
    useRaptorResultWarningA.store(props, prepend);
    useCtfCorrectionWarningA.store(props, prepend);
    useCtfCorrectionWarningB.store(props, prepend);
    useErasedStackWarningA.store(props, prepend);
    useErasedStackWarningB.store(props, prepend);
    useFilteredStackWarningA.store(props, prepend);
    useFilteredStackWarningB.store(props, prepend);
    genSirtsetupSubareaSizeA.store(props, prepend);
    genSirtsetupSubareaSizeB.store(props, prepend);
    genSirtsetupyOffsetOfSubareaA.store(props, prepend);
    genSirtsetupyOffsetOfSubareaB.store(props, prepend);
    tomogramSizeColumnsA.store(props, prepend);
    tomogramSizeColumnsB.store(props, prepend);
    tomogramSizeRowsA.store(props, prepend);
    tomogramSizeRowsB.store(props, prepend);
    tomogramSizeSectionsA.store(props, prepend);
    tomogramSizeSectionsB.store(props, prepend);
    xcorrBlendmontWasRunA.store(props, prepend);
    xcorrBlendmontWasRunB.store(props, prepend);
    // backwards compatibility
    props.remove(COMBINE_MATCH_MODE_BACK_KEY);
    if (combineMatchMode == null) {
      props.remove(prepend + "." + COMBINE_MATCH_MODE_KEY);
    }
    else {
      props.setProperty(prepend + "." + COMBINE_MATCH_MODE_KEY,
          combineMatchMode.toString());
    }
    // backwards compatibility
    props.remove(COMBINE_SCRIPTS_CREATED_BACK_KEY);
    combineScriptsCreated.store(props, prepend);
    EtomoBoolean2.store(sampleFiducialessA, props, prepend, firstAxisGroup
        + SAMPLE_FIDUCIALESS_KEY);
    if (secondAxisGroup != null) {
      EtomoBoolean2.store(sampleFiducialessB, props, prepend, secondAxisGroup
          + SAMPLE_FIDUCIALESS_KEY);
    }
  }

  String createPrepend(String prepend) {
    if (prepend == "") {
      return groupString;
    }
    return prepend + "." + groupString;
  }

  public void load(Properties props) {
    load(props, "");
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    // reset
    trimvolFlipped.reset();
    squeezevolFlipped.reset();
    madeZFactorsA.reset();
    madeZFactorsB.reset();
    newstFiducialessAlignmentA.reset();
    newstFiducialessAlignmentB.reset();
    usedLocalAlignmentsA.reset();
    usedLocalAlignmentsB.reset();
    invalidEdgeFunctionsA.reset();
    invalidEdgeFunctionsB.reset();
    axisZShiftA.reset();
    axisZShiftB.reset();
    angleOffsetA.reset();
    angleOffsetB.reset();
    sampleAxisZShiftA.reset();
    sampleAxisZShiftB.reset();
    sampleAngleOffsetA.reset();
    sampleAngleOffsetB.reset();
    sampleXAxisTiltA.reset();
    sampleXAxisTiltB.reset();
    fidFileLastModifiedA.reset();
    fidFileLastModifiedB.reset();
    seedFileLastModifiedA.reset();
    seedFileLastModifiedB.reset();
    fixedFiducialsA.reset();
    fixedFiducialsB.reset();
    combineMatchMode = null;
    combineScriptsCreated.reset();
    sampleFiducialessA = null;
    sampleFiducialessB = null;
    seedingDoneA.reset();
    seedingDoneB.reset();
    tomogramSizeA.reset();
    tomogramSizeB.reset();
    adjustOriginA.reset();
    adjustOriginB.reset();
    postProcTrimVolInputNColumns.reset();
    postProcTrimVolInputNRows.reset();
    postProcTrimVolInputNSections.reset();
    stackUseLinearInterpolationA.reset();
    stackUseLinearInterpolationB.reset();
    stackUserSizeToOutputInXandYA.reset();
    stackUserSizeToOutputInXandYB.reset();
    stackImageRotationA.reset();
    stackImageRotationB.reset();
    trackLightBeadsA.reset();
    trackLightBeadsB.reset();
    stackUsingNewstOrBlend3dFindOutputA.reset();
    stackUsingNewstOrBlend3dFindOutputB.reset();
    useFixedStackWarningA.reset();
    useFixedStackWarningB.reset();
    useRaptorResultWarningA.reset();
    useCtfCorrectionWarningA.reset();
    useCtfCorrectionWarningB.reset();
    useErasedStackWarningA.reset();
    useErasedStackWarningB.reset();
    useFilteredStackWarningA.reset();
    useFilteredStackWarningB.reset();
    genSirtsetupSubareaSizeA.reset();
    genSirtsetupSubareaSizeB.reset();
    genSirtsetupyOffsetOfSubareaA.reset();
    genSirtsetupyOffsetOfSubareaB.reset();
    tomogramSizeColumnsA.reset();
    tomogramSizeColumnsB.reset();
    tomogramSizeRowsA.reset();
    tomogramSizeRowsB.reset();
    tomogramSizeSectionsA.reset();
    tomogramSizeSectionsB.reset();
    xcorrBlendmontWasRunA.reset();
    xcorrBlendmontWasRunB.reset();

    // load
    prepend = createPrepend(prepend);
    String group = prepend + ".";
    setAxisPrepends(manager.getMetaData());
    trimvolFlipped.load(props, prepend);
    squeezevolFlipped.load(props, prepend);
    madeZFactorsA.load(props, prepend);
    madeZFactorsB.load(props, prepend);
    newstFiducialessAlignmentA.load(props, prepend);
    newstFiducialessAlignmentB.load(props, prepend);
    usedLocalAlignmentsA.load(props, prepend);
    usedLocalAlignmentsB.load(props, prepend);
    invalidEdgeFunctionsA.load(props, prepend);
    invalidEdgeFunctionsB.load(props, prepend);
    angleOffsetA.load(props, prepend);
    angleOffsetB.load(props, prepend);
    axisZShiftA.load(props, prepend);
    axisZShiftB.load(props, prepend);
    sampleAngleOffsetA.load(props, prepend);
    sampleAngleOffsetB.load(props, prepend);
    sampleAxisZShiftA.load(props, prepend);
    sampleAxisZShiftB.load(props, prepend);
    sampleXAxisTiltA.load(props, prepend);
    sampleXAxisTiltB.load(props, prepend);
    fidFileLastModifiedA.load(props, prepend);
    fidFileLastModifiedB.load(props, prepend);
    seedFileLastModifiedA.load(props, prepend);
    seedFileLastModifiedB.load(props, prepend);
    fixedFiducialsA.load(props, prepend);
    fixedFiducialsB.load(props, prepend);
    seedingDoneA.load(props, prepend);
    seedingDoneB.load(props, prepend);
    tomogramSizeA.load(props, prepend);
    tomogramSizeB.load(props, prepend);
    adjustOriginA.load(props, prepend);
    adjustOriginB.load(props, prepend);
    postProcTrimVolInputNColumns.load(props, prepend);
    postProcTrimVolInputNRows.load(props, prepend);
    postProcTrimVolInputNSections.load(props, prepend);
    stackUseLinearInterpolationA.load(props, prepend);
    stackUseLinearInterpolationB.load(props, prepend);
    stackUserSizeToOutputInXandYA.load(props, prepend);
    stackUserSizeToOutputInXandYB.load(props, prepend);
    stackImageRotationA.load(props, prepend);
    stackImageRotationB.load(props, prepend);
    trackLightBeadsA.load(props, prepend);
    trackLightBeadsB.load(props, prepend);
    stackUsingNewstOrBlend3dFindOutputA.load(props, prepend);
    stackUsingNewstOrBlend3dFindOutputB.load(props, prepend);
    useFixedStackWarningA.load(props, prepend);
    useFixedStackWarningB.load(props, prepend);
    useRaptorResultWarningA.load(props, prepend);
    useCtfCorrectionWarningA.load(props, prepend);
    useCtfCorrectionWarningB.load(props, prepend);
    useErasedStackWarningA.load(props, prepend);
    useErasedStackWarningB.load(props, prepend);
    useFilteredStackWarningA.load(props, prepend);
    useFilteredStackWarningB.load(props, prepend);
    genSirtsetupSubareaSizeA.load(props, prepend);
    genSirtsetupSubareaSizeB.load(props, prepend);
    genSirtsetupyOffsetOfSubareaA.load(props, prepend);
    genSirtsetupyOffsetOfSubareaB.load(props, prepend);
    tomogramSizeColumnsA.load(props, prepend);
    tomogramSizeColumnsB.load(props, prepend);
    tomogramSizeRowsA.load(props, prepend);
    tomogramSizeRowsB.load(props, prepend);
    tomogramSizeSectionsA.load(props, prepend);
    tomogramSizeSectionsB.load(props, prepend);
    xcorrBlendmontWasRunA.load(props, prepend);
    xcorrBlendmontWasRunB.load(props, prepend);
    combineMatchMode = MatchMode.getInstance(props.getProperty(prepend + "."
        + COMBINE_MATCH_MODE_KEY));
    // backwards compatibility
    if (combineMatchMode == null) {
      String backCombineMatchMode = props.getProperty(COMBINE_MATCH_MODE_BACK_KEY);
      if (backCombineMatchMode != null) {
        combineMatchMode = MatchMode.getInstance(Boolean.valueOf(backCombineMatchMode)
            .booleanValue());
      }
    }
    combineScriptsCreated.load(props, prepend);
    if (combineScriptsCreated.isNull()) {
      // backwards compatibility
      String backCombineScriptsCreated = props
          .getProperty(COMBINE_SCRIPTS_CREATED_BACK_KEY);
      if (backCombineScriptsCreated != null) {
        combineScriptsCreated.set(backCombineScriptsCreated);
      }
    }
    sampleFiducialessA = EtomoBoolean2.getInstance(sampleFiducialessA, firstAxisGroup
        + SAMPLE_FIDUCIALESS_KEY, props, prepend);
    if (secondAxisGroup != null) {
      sampleFiducialessB = EtomoBoolean2.getInstance(sampleFiducialessB, secondAxisGroup
          + SAMPLE_FIDUCIALESS_KEY, props, prepend);
    }
  }

  public void setCombineScriptsCreated(boolean combineScriptsCreated) {
    this.combineScriptsCreated.set(combineScriptsCreated);
  }

  public void resetCombineScriptsCreated() {
    this.combineScriptsCreated.reset();
  }

  public EtomoState getCombineScriptsCreated() {
    return combineScriptsCreated;
  }

  public boolean isCombineScriptsCreated() {
    if (!combineScriptsCreated.isResultSet()) {
      return false;
    }
    return combineScriptsCreated.is();
  }

  public void setCombineMatchMode(MatchMode combineMatchMode) {
    this.combineMatchMode = combineMatchMode;
  }

  public MatchMode getCombineMatchMode() {
    return combineMatchMode;
  }

  public ConstEtomoNumber setTrimvolFlipped(boolean trimvolFlipped) {
    return this.trimvolFlipped.set(trimvolFlipped);
  }

  public ConstEtomoNumber setSqueezevolFlipped(boolean squeezevolFlipped) {
    return this.squeezevolFlipped.set(squeezevolFlipped);
  }

  public void setTomogramSizeColumns(final AxisID axisID, final int input) {
    if (axisID == AxisID.SECOND) {
      tomogramSizeColumnsB.set(input);
    }
    else {
      tomogramSizeColumnsA.set(input);
    }
  }

  public void setTomogramSizeRows(final AxisID axisID, final int input) {
    if (axisID == AxisID.SECOND) {
      tomogramSizeRowsB.set(input);
    }
    else {
      tomogramSizeRowsA.set(input);
    }
  }

  public void setTomogramSizeSections(final AxisID axisID, final int input) {
    if (axisID == AxisID.SECOND) {
      tomogramSizeSectionsB.set(input);
    }
    else {
      tomogramSizeSectionsA.set(input);
    }
  }

  public boolean isAdjustOrigin(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return adjustOriginB.is();
    }
    return adjustOriginA.is();
  }

  public void setAdjustOrigin(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      adjustOriginB.set(input);
    }
    else {
      adjustOriginA.set(input);
    }
  }

  public void setPostProcTrimVolInputNColumns(int input) {
    postProcTrimVolInputNColumns.set(input);
  }

  public void setPostProcTrimVolInputNRows(int input) {
    postProcTrimVolInputNRows.set(input);
  }

  public void setPostProcTrimVolInputNSections(int input) {
    postProcTrimVolInputNSections.set(input);
  }

  public void setStackUseLinearInterpolation(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      stackUseLinearInterpolationB.set(input);
    }
    else {
      stackUseLinearInterpolationA.set(input);
    }
  }

  public void setStackUserSizeToOutputInXandY(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      stackUserSizeToOutputInXandYB.set(input);
    }
    else {
      stackUserSizeToOutputInXandYA.set(input);
    }
  }

  public void setStackImageRotation(AxisID axisID, ConstEtomoNumber input) {
    if (axisID == AxisID.SECOND) {
      stackImageRotationB.set(input);
    }
    else {
      stackImageRotationA.set(input);
    }
  }

  public void setTrackLightBeads(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      trackLightBeadsB.set(input);
    }
    else {
      trackLightBeadsA.set(input);
    }
  }

  public void setStackUsingNewstOrBlend3dFindOutput(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      stackUsingNewstOrBlend3dFindOutputB.set(input);
    }
    else {
      stackUsingNewstOrBlend3dFindOutputA.set(input);
    }
  }

  public void setUseFixedStackWarning(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      useFixedStackWarningB.set(input);
    }
    else {
      useFixedStackWarningA.set(input);
    }
  }

  public void setUseCtfCorrectionWarning(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      useCtfCorrectionWarningB.set(input);
    }
    else {
      useCtfCorrectionWarningA.set(input);
    }
  }

  public void setUseErasedStackWarning(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      useErasedStackWarningB.set(input);
    }
    else {
      useErasedStackWarningA.set(input);
    }
  }

  public void setUseFilteredStackWarning(AxisID axisID, boolean input) {
    if (axisID == AxisID.SECOND) {
      useFilteredStackWarningB.set(input);
    }
    else {
      useFilteredStackWarningA.set(input);
    }
  }

  public void setGenSirtsetupSubareaSize(AxisID axisID, String input) {
    if (axisID == AxisID.SECOND) {
      genSirtsetupSubareaSizeB.set(input);
    }
    else {
      genSirtsetupSubareaSizeA.set(input);
    }
  }

  public void setGenSirtsetupyOffsetOfSubarea(AxisID axisID, int input) {
    if (axisID == AxisID.SECOND) {
      genSirtsetupyOffsetOfSubareaB.set(input);
    }
    else {
      genSirtsetupyOffsetOfSubareaA.set(input);
    }
  }

  public void setUseRaptorResultWarning(boolean input) {
    useRaptorResultWarningA.set(input);
  }

  public boolean isPostProcTrimVolInputNColumnsNull() {
    return postProcTrimVolInputNColumns.isNull();
  }

  public boolean isPostProcTrimVolInputNRowsNull() {
    return postProcTrimVolInputNRows.isNull();
  }

  public boolean isPostProcTrimVolInputNSectionsNull() {
    return postProcTrimVolInputNSections.isNull();
  }

  public boolean isStackUseLinearInterpolation(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return stackUseLinearInterpolationB.is();
    }
    return stackUseLinearInterpolationA.is();
  }

  public String getStackUserSizeToOutputInXandY(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return stackUserSizeToOutputInXandYB.toString();
    }
    return stackUserSizeToOutputInXandYA.toString();
  }

  public ConstEtomoNumber getStackImageRotation(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return stackImageRotationB;
    }
    return stackImageRotationA;
  }

  public boolean isTrackLightBeads(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return trackLightBeadsB.is();
    }
    return trackLightBeadsA.is();
  }

  public boolean isStackUsingNewstOrBlend3dFindOutput(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return stackUsingNewstOrBlend3dFindOutputB.is();
    }
    return stackUsingNewstOrBlend3dFindOutputA.is();
  }

  public boolean isUseFixedStackWarning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return useFixedStackWarningB.is();
    }
    return useFixedStackWarningA.is();
  }

  public boolean isUseCtfCorrectionWarning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return useCtfCorrectionWarningB.is();
    }
    return useCtfCorrectionWarningA.is();
  }

  public boolean isUseErasedStackWarning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return useErasedStackWarningB.is();
    }
    return useErasedStackWarningA.is();
  }

  public boolean isUseFilteredStackWarning(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return useFilteredStackWarningB.is();
    }
    return useFilteredStackWarningA.is();
  }

  public String getGenSirtsetupSubareaSize(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return genSirtsetupSubareaSizeB.toString();
    }
    return genSirtsetupSubareaSizeA.toString();
  }

  public String getGenSirtsetupyOffsetOfSubarea(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return genSirtsetupyOffsetOfSubareaB.toString();
    }
    return genSirtsetupyOffsetOfSubareaA.toString();
  }

  public boolean isUseRaptorResultWarning() {
    return useRaptorResultWarningA.is();
  }

  public boolean isTrackLightBeadsNull(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return trackLightBeadsB.isNull();
    }
    return trackLightBeadsA.isNull();
  }

  public int getPostProcTrimVolInputNColumns() {
    return postProcTrimVolInputNColumns.getInt();
  }

  public int getPostProcTrimVolInputNRows() {
    return postProcTrimVolInputNRows.getInt();
  }

  public int getPostProcTrimVolInputNSections() {
    return postProcTrimVolInputNSections.getInt();
  }

  public ConstEtomoNumber getTomogramSize(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramSizeB;
    }
    return tomogramSizeA;
  }

  public ConstEtomoNumber getTomogramSizeColumns(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramSizeColumnsB;
    }
    return tomogramSizeColumnsA;
  }

  public ConstEtomoNumber getTomogramSizeRows(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramSizeRowsB;
    }
    return tomogramSizeRowsA;
  }

  public ConstEtomoNumber getTomogramSizeSections(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramSizeSectionsB;
    }
    return tomogramSizeSectionsA;
  }

  public ConstEtomoNumber setMadeZFactors(AxisID axisID, boolean madeZFactors) {
    if (axisID == AxisID.SECOND) {
      return this.madeZFactorsB.set(madeZFactors);
    }
    return this.madeZFactorsA.set(madeZFactors);
  }

  public void setAlignAxisZShift(AxisID axisID, double axisZShift) {
    if (axisID == AxisID.SECOND) {
      axisZShiftB.set(axisZShift);
    }
    else {
      axisZShiftA.set(axisZShift);
    }
  }

  public ConstEtomoNumber getAlignAxisZShift(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return axisZShiftB;
    }
    return axisZShiftA;
  }

  public ConstEtomoNumber getSampleAxisZShift(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sampleAxisZShiftB;
    }
    return sampleAxisZShiftA;
  }

  public EtomoBoolean2 getSampleFiducialess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sampleFiducialessB;
    }
    return sampleFiducialessA;
  }

  public void setSampleAxisZShift(AxisID axisID, ConstEtomoNumber axisZShift) {
    if (axisID == AxisID.SECOND) {
      sampleAxisZShiftB.set(axisZShift);
    }
    else {
      sampleAxisZShiftA.set(axisZShift);
    }
  }

  public void setSampleAxisZShift(AxisID axisID, double axisZShift) {
    if (axisID == AxisID.SECOND) {
      sampleAxisZShiftB.set(axisZShift);
    }
    else {
      sampleAxisZShiftA.set(axisZShift);
    }
  }

  public void setSampleFiducialess(AxisID axisID, boolean sampleFiducialess) {
    if (axisID == AxisID.SECOND) {
      sampleFiducialessB = EtomoBoolean2.getInstance(sampleFiducialessB, secondAxisGroup
          + SAMPLE_FIDUCIALESS_KEY, sampleFiducialess);
    }
    else {
      sampleFiducialessA = EtomoBoolean2.getInstance(sampleFiducialessA, firstAxisGroup
          + SAMPLE_FIDUCIALESS_KEY, sampleFiducialess);
    }
  }

  public void setFidFileLastModified(AxisID axisID, long fidFileLastModified) {
    if (axisID == AxisID.SECOND) {
      fidFileLastModifiedB.set(fidFileLastModified);
    }
    else {
      fidFileLastModifiedA.set(fidFileLastModified);
    }
  }

  public void setFixedFiducials(AxisID axisID, boolean fixedFiducials) {
    if (axisID == AxisID.SECOND) {
      fixedFiducialsB.set(fixedFiducials);
    }
    else {
      fixedFiducialsA.set(fixedFiducials);
    }
  }

  public void setSeedFileLastModified(AxisID axisID, long seedFileLastModified) {
    if (axisID == AxisID.SECOND) {
      seedFileLastModifiedB.set(seedFileLastModified);
    }
    else {
      seedFileLastModifiedA.set(seedFileLastModified);
    }
  }

  public void resetFidFileLastModified(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      fidFileLastModifiedB.reset();
    }
    else {
      fidFileLastModifiedA.reset();
    }
  }

  public void resetSeedFileLastModified(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      seedFileLastModifiedB.reset();
    }
    else {
      seedFileLastModifiedA.reset();
    }
  }

  public void setAlignAngleOffset(AxisID axisID, double angleOffset) {
    if (axisID == AxisID.SECOND) {
      angleOffsetB.set(angleOffset);
    }
    else {
      angleOffsetA.set(angleOffset);
    }
  }

  public ConstEtomoNumber getFidFileLastModified(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fidFileLastModifiedB;
    }
    return fidFileLastModifiedA;
  }

  public ConstEtomoNumber getSeedFileLastModified(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return seedFileLastModifiedB;
    }
    return seedFileLastModifiedA;
  }

  public boolean isFixedFiducials(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return fixedFiducialsB.is();
    }
    return fixedFiducialsA.is();
  }

  public boolean isSeedingDone(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return seedingDoneB.is();
    }
    return seedingDoneA.is();
  }

  public boolean isXcorrBlendmontWasRun(final AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return xcorrBlendmontWasRunB.is();
    }
    return xcorrBlendmontWasRunA.is();
  }

  public void setXcorrBlendmontWasRun(final AxisID axisID, final boolean input) {
    if (axisID == AxisID.SECOND) {
      xcorrBlendmontWasRunB.set(input);
    }
    else {
      xcorrBlendmontWasRunA.set(input);
    }
  }

  public ConstEtomoNumber getAlignAngleOffset(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return angleOffsetB;
    }
    return angleOffsetA;
  }

  public ConstEtomoNumber getSampleAngleOffset(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sampleAngleOffsetB;
    }
    return sampleAngleOffsetA;
  }

  public void setSampleAngleOffset(AxisID axisID, ConstEtomoNumber angleOffset) {
    if (axisID == AxisID.SECOND) {
      sampleAngleOffsetB.set(angleOffset);
    }
    else {
      sampleAngleOffsetA.set(angleOffset);
    }
  }

  public void setSampleAngleOffset(AxisID axisID, double angleOffset) {
    if (axisID == AxisID.SECOND) {
      sampleAngleOffsetB.set(angleOffset);
    }
    else {
      sampleAngleOffsetA.set(angleOffset);
    }
  }

  public ConstEtomoNumber getSampleXAxisTilt(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return sampleXAxisTiltB;
    }
    return sampleXAxisTiltA;
  }

  public void setSampleXAxisTilt(AxisID axisID, double xAxisTilt) {
    if (axisID == AxisID.SECOND) {
      sampleXAxisTiltB.set(xAxisTilt);
    }
    else {
      sampleXAxisTiltA.set(xAxisTilt);
    }
  }

  public void setSeedingDone(AxisID axisID, boolean seedingDone) {
    if (axisID == AxisID.SECOND) {
      seedingDoneB.set(seedingDone);
    }
    else {
      seedingDoneA.set(seedingDone);
    }
  }

  public ConstEtomoNumber setInvalidEdgeFunctions(AxisID axisID,
      boolean invalidEdgeFunctions) {
    if (axisID == AxisID.SECOND) {
      return this.invalidEdgeFunctionsB.set(invalidEdgeFunctions);
    }
    return this.invalidEdgeFunctionsA.set(invalidEdgeFunctions);
  }

  public ConstEtomoNumber setNewstFiducialessAlignment(AxisID axisID,
      boolean newstFiducialessAlignment) {
    if (axisID == AxisID.SECOND) {
      return this.newstFiducialessAlignmentB.set(newstFiducialessAlignment);
    }
    return this.newstFiducialessAlignmentA.set(newstFiducialessAlignment);
  }

  public ConstEtomoNumber setUsedLocalAlignments(AxisID axisID,
      boolean usedLocalAlignments) {
    if (axisID == AxisID.SECOND) {
      return this.usedLocalAlignmentsB.set(usedLocalAlignments);
    }
    return this.usedLocalAlignmentsA.set(usedLocalAlignments);
  }

  public ConstEtomoNumber getTrimvolFlipped() {
    return trimvolFlipped;
  }

  public ConstEtomoNumber getSqueezevolFlipped() {
    return squeezevolFlipped;
  }

  public ConstEtomoNumber getMadeZFactors(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return madeZFactorsB;
    }
    return madeZFactorsA;
  }

  public ConstEtomoNumber getInvalidEdgeFunctions(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return invalidEdgeFunctionsB;
    }
    return invalidEdgeFunctionsA;
  }

  public ConstEtomoNumber getNewstFiducialessAlignment(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return newstFiducialessAlignmentB;
    }
    return newstFiducialessAlignmentA;
  }

  public boolean isNewstFiducialessAlignment(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return newstFiducialessAlignmentB.is();
    }
    return newstFiducialessAlignmentA.is();
  }

  public ConstEtomoNumber getUsedLocalAlignments(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return usedLocalAlignmentsB;
    }
    return usedLocalAlignmentsA;
  }

  /**
   * Backward compatibility
   * function decide whether trimvol is flipped based on the header
   * @return
   */
  public boolean getBackwardCompatibleTrimvolFlipped() {
    // If trimvol has not been run, then assume that the tomogram has not been
    // flipped.
    EtomoDirector etomoDirector = EtomoDirector.INSTANCE;
    String datasetName = manager.getName();
    File trimvolFile = new File(manager.getPropertyUserDir(),
        TrimvolParam.getOutputFileName(datasetName));
    if (!trimvolFile.exists()) {
      return false;
    }
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        trimvolFile.getAbsolutePath(), AxisID.ONLY);
    try {
      if (!header.read(manager)) {
        return false;
      }
    }
    catch (IOException e) {
      return false;
    }
    catch (Exception e) {
      e.printStackTrace();
      return false;
    }
    if (header.getNRows() < header.getNSections()) {
      System.err.println("Assuming that " + trimvolFile.getName()
          + " has not been flipped\n" + "because the Y is less then Z in the header.");
      return false;
    }
    System.err.println("Assuming that " + trimvolFile.getName() + " has been flipped\n"
        + "because the Y is greater or equal to Z in the header.");
    return true;
  }

  /**
   * Backward compatibility
   * function decide whether tiltalign was run with local alignments based
   * file time.
   * @return
   */
  public boolean getBackwardCompatibleUsedLocalAlignments(AxisID axisID) {
    String userDir = manager.getPropertyUserDir();
    String datasetName = manager.getName();
    File localXfFile = new File(userDir, datasetName + axisID.getExtension() + "local.xf");
    File transformFile = new File(userDir, datasetName + axisID.getExtension() + ".tltxf");
    if (!localXfFile.exists()) {
      System.err.println("Assuming that local alignments where not used " + "\nbecause "
          + localXfFile.getName() + " does not exist.");
      return false;
    }
    if (!transformFile.exists()) {
      System.err.println("Assuming that local alignments where not used " + "\nbecause "
          + transformFile.getName() + " does not exist.");
      return false;
    }
    if (localXfFile.lastModified() < transformFile.lastModified()) {
      System.err.println("Assuming that local alignments where not used " + "\nbecause "
          + localXfFile.getName() + " was modified before " + transformFile.getName()
          + ".");
      return false;
    }
    System.err.println("Assuming that local alignments where used " + "\nbecause "
        + localXfFile.getName() + " was modified after " + transformFile.getName() + ".");
    return true;
  }

  /**
   * Backward compatibility
   * function decide whether z factors where made based on the relationship
   * between .zfac file and the .tltxf file
   * @return
   */
  public boolean getBackwardCompatibleMadeZFactors(AxisID axisID) {
    EtomoDirector etomoDirector = EtomoDirector.INSTANCE;
    String userDir = manager.getPropertyUserDir();
    String datasetName = manager.getName();
    File zFactorFile = new File(userDir, TiltalignParam.getOutputZFactorFileName(
        datasetName, axisID));
    File tltxfFile = new File(userDir, datasetName + axisID.getExtension() + ".tltxf");
    if (!zFactorFile.exists()) {
      System.err.println("Assuming that madeZFactors is false\n" + "because "
          + zFactorFile.getName() + " does not exist.");
      return false;
    }
    if (!tltxfFile.exists()) {
      System.err.println("Assuming that madeZFactors is false\n" + "because "
          + tltxfFile.getName() + " does not exist.");
      return false;
    }
    if (zFactorFile.lastModified() < tltxfFile.lastModified()) {
      System.err.println("Assuming that madeZFactors is false\n" + "because "
          + zFactorFile.getName() + " is older then " + tltxfFile.getName() + ".");

      return false;
    }
    System.err.println("Assuming that madeZFactors is true\n" + "because "
        + zFactorFile.getName() + " was modified after " + tltxfFile.getName() + ".");
    return true;
  }

}