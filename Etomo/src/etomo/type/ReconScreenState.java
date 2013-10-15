package etomo.type;

import java.util.Properties;

import etomo.comscript.Patchcrawl3DParam;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2005 - 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ReconScreenState extends BaseScreenState {
  public static final String rcsid = "$Id$";

  private static final String HEADER_GROUP = ".Header";

  /**
   * @deprecated
   */
  public static final String TOMO_GEN_NEWST_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName() + ".Newst" + HEADER_GROUP;
  /**
   * @deprecated
   */
  public static final String TOMO_GEN_MTFFILTER_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName() + ".Mtffilter" + HEADER_GROUP;
  public static final String STACK_NEWST_HEADER_GROUP = DialogType.FINAL_ALIGNED_STACK
      .getStorableName() + ".Newst" + HEADER_GROUP;
  public static final String STACK_MTFFILTER_HEADER_GROUP = DialogType.FINAL_ALIGNED_STACK
      .getStorableName() + ".Mtffilter" + HEADER_GROUP;
  public static final String STACK_CTF_CORRECTION_HEADER_GROUP = DialogType.FINAL_ALIGNED_STACK
      .getStorableName() + ".CtfCorrection" + HEADER_GROUP;
  public static final String TOMO_GEN_TILT_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName() + ".Tilt" + HEADER_GROUP;
  public static final String TOMO_GEN_TRIAL_TILT_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName() + ".TrialTilt" + HEADER_GROUP;

  private static final String SETUP_GROUP = DialogType.TOMOGRAM_COMBINATION
      .getStorableName() + ".Setup.";
  private static final String SOLVEMATCH_GROUP = "Solvematch";
  private static final String PATCHCORR_GROUP = "Patchcorr";
  private static final String VOLCOMBINE_GROUP = "Volcombine";
  private static final String PATCHCORR_KERNEL_SIGMA_KEY = DialogType.TOMOGRAM_COMBINATION
      .getStorableName()
      + '.'
      + ProcessName.PATCHCORR
      + '.'
      + Patchcrawl3DParam.KERNEL_SIGMA_KEY;

  public static final String COMBINE_SETUP_TO_SELECTOR_HEADER_GROUP = SETUP_GROUP
      + "ToSelector." + HEADER_GROUP;
  public static final String COMBINE_SETUP_SOLVEMATCH_HEADER_GROUP = SETUP_GROUP
      + SOLVEMATCH_GROUP + HEADER_GROUP;
  public static final String COMBINE_SETUP_PATCHCORR_HEADER_GROUP = SETUP_GROUP
      + PATCHCORR_GROUP + HEADER_GROUP;
  public static final String COMBINE_SETUP_VOLCOMBINE_HEADER_GROUP = SETUP_GROUP
      + VOLCOMBINE_GROUP + HEADER_GROUP;
  public static final String COMBINE_SETUP_TEMP_DIR_HEADER_GROUP = SETUP_GROUP
      + "TempDir" + HEADER_GROUP;

  public static final String COMBINE_INITIAL_SOLVEMATCH_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
      .getStorableName() + ".Initial." + SOLVEMATCH_GROUP + HEADER_GROUP;

  private static final String FINAL_GROUP = DialogType.TOMOGRAM_COMBINATION
      .getStorableName() + ".Final.";

  public static final String COMBINE_FINAL_PATCH_REGION_HEADER_GROUP = FINAL_GROUP
      + "PatchRegion" + HEADER_GROUP;
  public static final String COMBINE_FINAL_PATCHCORR_HEADER_GROUP = FINAL_GROUP
      + PATCHCORR_GROUP + HEADER_GROUP;
  public static final String COMBINE_FINAL_MATCHORWARP_HEADER_GROUP = FINAL_GROUP
      + "Matchorwarp" + HEADER_GROUP;
  public static final String COMBINE_FINAL_VOLCOMBINE_HEADER_GROUP = FINAL_GROUP
      + VOLCOMBINE_GROUP + HEADER_GROUP;

  // Dialog keys
  private static final String STACK_KEY = "Stack";

  // Panel keys
  private static final String ERASE_GOLD_KEY = "EraseGold";
  private static final String NEWSTACK_OR_BLENDMONT_KEY = "NewstackOrBlendmont";

  // Header keys
  private static final String HEADER_KEY = "Header";
  private static final String NEWST_KEY = "Newst";

  /**
   * @deprecated
   */
  private final PanelHeaderState tomoGenMtffilterHeaderState = new PanelHeaderState(
      TOMO_GEN_MTFFILTER_HEADER_GROUP);
  /**
   * @deprecated
   */
  private final PanelHeaderState tomoGenNewstHeaderState = new PanelHeaderState(
      TOMO_GEN_NEWST_HEADER_GROUP);
  private final PanelHeaderState stackNewstHeaderState = new PanelHeaderState(
      STACK_NEWST_HEADER_GROUP);
  private final PanelHeaderState stackMtffilterHeaderState = new PanelHeaderState(
      STACK_MTFFILTER_HEADER_GROUP);
  private final PanelHeaderState stackCtfCorrectionHeaderState = new PanelHeaderState(
      STACK_CTF_CORRECTION_HEADER_GROUP);
  private final PanelHeaderState tomoGenTiltHeaderState = new PanelHeaderState(
      TOMO_GEN_TILT_HEADER_GROUP);
  private final PanelHeaderState tomoGenTrialTiltHeaderState = new PanelHeaderState(
      TOMO_GEN_TRIAL_TILT_HEADER_GROUP);
  private final PanelHeaderState tomoGenSirtHeaderState = new PanelHeaderState(
      DialogType.TOMOGRAM_GENERATION.getStorableName() + ".Sirt" + HEADER_GROUP);

  private final PanelHeaderState combineSetupToSelectorHeaderState = new PanelHeaderState(
      COMBINE_SETUP_TO_SELECTOR_HEADER_GROUP);
  private final PanelHeaderState combineSetupSolvematchHeaderState = new PanelHeaderState(
      COMBINE_SETUP_SOLVEMATCH_HEADER_GROUP);
  private final PanelHeaderState combineSetupPatchcorrHeaderState = new PanelHeaderState(
      COMBINE_SETUP_PATCHCORR_HEADER_GROUP);
  private final PanelHeaderState combineSetupVolcombineHeaderState = new PanelHeaderState(
      COMBINE_SETUP_VOLCOMBINE_HEADER_GROUP);
  private final PanelHeaderState combineSetupTempDirHeaderState = new PanelHeaderState(
      COMBINE_SETUP_TEMP_DIR_HEADER_GROUP);

  private final PanelHeaderState combineInitialSolvematchHeaderState = new PanelHeaderState(
      COMBINE_INITIAL_SOLVEMATCH_HEADER_GROUP);

  private final PanelHeaderState combineFinalPatchRegionHeaderState = new PanelHeaderState(
      COMBINE_FINAL_PATCH_REGION_HEADER_GROUP);
  private final PanelHeaderState combineFinalPatchcorrHeaderState = new PanelHeaderState(
      COMBINE_FINAL_PATCHCORR_HEADER_GROUP);
  private final PanelHeaderState combineFinalMatchorwarpHeaderState = new PanelHeaderState(
      COMBINE_FINAL_MATCHORWARP_HEADER_GROUP);
  private final PanelHeaderState combineFinalVolcombineHeaderState = new PanelHeaderState(
      COMBINE_FINAL_VOLCOMBINE_HEADER_GROUP);
  private EtomoNumber patchcorrKernelSigma = null;
  private final EtomoVersion version = EtomoVersion.getDefaultInstance("1.1");

  private final PanelHeaderState stackEraseGoldNewstHeaderState = new PanelHeaderState(
      STACK_KEY + "." + ERASE_GOLD_KEY + "." + NEWSTACK_OR_BLENDMONT_KEY + "."
          + NEWST_KEY + "." + HEADER_KEY);

  private final PanelHeaderState stackFindBeads3dHeaderState = new PanelHeaderState(
      STACK_KEY + ".FindBeads3d" + HEADER_KEY);
  private final PanelHeaderState stackAlignAndTiltHeaderState = new PanelHeaderState(
      STACK_KEY + ".AlignAndTilt" + HEADER_KEY);

  public ReconScreenState(AxisID axisID, AxisType axisType) {
    super(axisID, axisType);
  }

  public void store(Properties props) {
    super.store(props);
  }

  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = getPrepend(prepend);
    stackNewstHeaderState.store(props, prepend);
    stackMtffilterHeaderState.store(props, prepend);
    stackCtfCorrectionHeaderState.store(props, prepend);
    tomoGenTiltHeaderState.store(props, prepend);
    tomoGenTrialTiltHeaderState.store(props, prepend);
    stackEraseGoldNewstHeaderState.store(props, prepend);
    stackFindBeads3dHeaderState.store(props, prepend);
    stackAlignAndTiltHeaderState.store(props, prepend);
    tomoGenSirtHeaderState.store(props, prepend);
    if (axisID == AxisID.FIRST) {
      combineSetupToSelectorHeaderState.store(props, prepend);
      combineSetupSolvematchHeaderState.store(props, prepend);
      combineSetupPatchcorrHeaderState.store(props, prepend);
      combineSetupVolcombineHeaderState.store(props, prepend);
      combineSetupTempDirHeaderState.store(props, prepend);

      combineInitialSolvematchHeaderState.store(props, prepend);

      combineFinalPatchRegionHeaderState.store(props, prepend);
      combineFinalPatchcorrHeaderState.store(props, prepend);
      combineFinalMatchorwarpHeaderState.store(props, prepend);
      combineFinalVolcombineHeaderState.store(props, prepend);
      EtomoNumber.store(patchcorrKernelSigma, PATCHCORR_KERNEL_SIGMA_KEY, props, prepend);
    }
  }

  public void load(Properties props) {
    super.load(props);
  }

  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    prepend = getPrepend(prepend);

    // backwards compatibility
    // Moved newst and mtffilter to final aligned stack dialog at version 1.1.
    if (version.lt(EtomoVersion.getDefaultInstance("1.1"))) {
      tomoGenMtffilterHeaderState.load(props, prepend);
      stackMtffilterHeaderState.set(tomoGenMtffilterHeaderState);
      tomoGenNewstHeaderState.load(props, prepend);
      stackNewstHeaderState.set(tomoGenNewstHeaderState);
    }
    else {
      stackMtffilterHeaderState.load(props, prepend);
      stackNewstHeaderState.load(props, prepend);
    }
    stackCtfCorrectionHeaderState.load(props, prepend);
    tomoGenTiltHeaderState.load(props, prepend);
    tomoGenSirtHeaderState.load(props, prepend);
    tomoGenTrialTiltHeaderState.load(props, prepend);
    stackEraseGoldNewstHeaderState.load(props, prepend);
    stackFindBeads3dHeaderState.load(props, prepend);
    stackAlignAndTiltHeaderState.load(props, prepend);
    if (axisID == AxisID.FIRST) {
      combineSetupToSelectorHeaderState.load(props, prepend);
      combineSetupSolvematchHeaderState.load(props, prepend);
      combineSetupPatchcorrHeaderState.load(props, prepend);
      combineSetupVolcombineHeaderState.load(props, prepend);
      combineSetupTempDirHeaderState.load(props, prepend);

      combineInitialSolvematchHeaderState.load(props, prepend);

      combineFinalPatchRegionHeaderState.load(props, prepend);
      combineFinalPatchcorrHeaderState.load(props, prepend);
      combineFinalMatchorwarpHeaderState.load(props, prepend);
      combineFinalVolcombineHeaderState.load(props, prepend);
      patchcorrKernelSigma = EtomoNumber.load(patchcorrKernelSigma,
          EtomoNumber.Type.DOUBLE, PATCHCORR_KERNEL_SIGMA_KEY, props, prepend);
    }
  }

  private static String getAxisExtension(AxisID axisID) {
    if (axisID == AxisID.ONLY) {
      axisID = AxisID.FIRST;
    }
    return axisID.getExtension().toUpperCase();
  }

  /**
   * Get a newst header state; either the header from the main newstack tab, or
   * the header from the erase gold newstack panel.
   * @param dialogType
   * @param parentPanelType
   * @return
   */
  public PanelHeaderState getNewstHeaderState() {
    return stackNewstHeaderState;
  }

  public PanelHeaderState getStackMtffilterHeaderState() {
    return stackMtffilterHeaderState;
  }

  public PanelHeaderState getStackCtfCorrectionHeaderState() {
    return stackCtfCorrectionHeaderState;
  }

  public PanelHeaderState getStackFindBeads3dHeaderState() {
    return stackFindBeads3dHeaderState;
  }

  public PanelHeaderState getStackAlignAndTiltHeaderState() {
    return stackAlignAndTiltHeaderState;
  }

  public PanelHeaderState getTomoGenTiltHeaderState() {
    return tomoGenTiltHeaderState;
  }

  public PanelHeaderState getTomoGenSirtHeaderState() {
    return tomoGenSirtHeaderState;
  }

  public PanelHeaderState getTomoGenTrialTiltHeaderState() {
    return tomoGenTrialTiltHeaderState;
  }

  public PanelHeaderState getCombineSetupToSelectorHeaderState() {
    return combineSetupToSelectorHeaderState;
  }

  public PanelHeaderState getCombineSetupSolvematchHeaderState() {
    return combineSetupSolvematchHeaderState;
  }

  public PanelHeaderState getCombineSetupPatchcorrHeaderState() {
    return combineSetupPatchcorrHeaderState;
  }

  public PanelHeaderState getCombineSetupTempDirHeaderState() {
    return combineSetupTempDirHeaderState;
  }

  public PanelHeaderState getCombineInitialSolvematchHeaderState() {
    return combineInitialSolvematchHeaderState;
  }

  public PanelHeaderState getCombineFinalPatchRegionHeaderState() {
    return combineFinalPatchRegionHeaderState;
  }

  public PanelHeaderState getCombineFinalPatchcorrHeaderState() {
    return combineFinalPatchcorrHeaderState;
  }

  public PanelHeaderState getCombineFinalMatchorwarpHeaderState() {
    return combineFinalMatchorwarpHeaderState;
  }

  public PanelHeaderState getCombineFinalVolcombineHeaderState() {
    return combineFinalVolcombineHeaderState;
  }

  public PanelHeaderState getCombineSetupVolcombineHeaderState() {
    return combineSetupVolcombineHeaderState;
  }

  public ConstEtomoNumber getPatchcorrKernelSigma() {
    return patchcorrKernelSigma;
  }

  public void setPatchcorrKernelSigma(String patchcorrKernelSigma) {
    if (this.patchcorrKernelSigma == null) {
      this.patchcorrKernelSigma = new EtomoNumber(EtomoNumber.Type.DOUBLE,
          PATCHCORR_KERNEL_SIGMA_KEY);
    }
    this.patchcorrKernelSigma.set(patchcorrKernelSigma);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.12  2011/02/22 05:51:50  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.11  2009/09/01 03:15:35  sueh
 * <p> bug# 1222 Added stackAlignAndTiltHeaderState,
 * <p> stackEraseGoldNewstHeaderState, and stackFindBeads3dHeaderState.
 * <p>
 * <p> Revision 1.10  2008/11/21 17:10:56  sueh
 * <p> bug# 1123 Added fineAlignBeamTiltHeaderState.
 * <p>
 * <p> Revision 1.9  2008/10/27 20:17:48  sueh
 * <p> bug# 1141 Changed finalStackMtffilterHeaderState to
 * <p> stackMtffilterHeaderState.  Added stackCtfCorrectionHeaderState.
 * <p>
 * <p> Revision 1.8  2008/10/16 21:01:57  sueh
 * <p> bug# 1141 Added finalMtffilterHeaderState and newstHeaderState.\Deprecated tomoGenMtffilterHeaderState and
 * <p> tomoGenNewstHeaderState.
 * <p>
 * <p> Revision 1.7  2007/02/05 23:30:56  sueh
 * <p> bug# 962 Moved EtomoNumber type info to inner class.
 * <p>
 * <p> Revision 1.6  2006/08/29 20:06:45  sueh
 * <p> bug# 924 Added kernelSigma.
 * <p>
 * <p> Revision 1.5  2006/05/11 19:58:23  sueh
 * <p> Making class final.
 * <p>
 * <p> Revision 1.4  2006/01/20 21:08:38  sueh
 * <p> updated copyright year
 * <p>
 * <p> Revision 1.3  2005/10/13 22:15:26  sueh
 * <p> bug# 532 Added combineSetupVolcombineHeader.
 * <p>
 * <p> Revision 1.2  2005/09/29 18:47:27  sueh
 * <p> bug# 532 Added header state variables for combine.
 * <p>
 * <p> Revision 1.1  2005/09/27 23:22:11  sueh
 * <p> bug# 532 This is a top level Storable object that should be written to the
 * <p> .edf and .ejf files.  One instance per axis is created.
 * <p> </p>
 */
