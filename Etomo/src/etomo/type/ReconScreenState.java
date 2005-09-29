package etomo.type;

import java.util.Properties;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright (c) 2005</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
*/
public class ReconScreenState extends BaseScreenState {
  public static  final String  rcsid =  "$Id$";
  
  public static final String TOMO_GEN_NEWST_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName() + ".Newst.Header";
  public static final String TOMO_GEN_MTFFILTER_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName() + ".Mtffilter.Header";
  public static final String TOMO_GEN_TILT_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName() + ".Tilt.Header";
  public static final String TOMO_GEN_TRIAL_TILT_HEADER_GROUP = DialogType.TOMOGRAM_GENERATION
      .getStorableName() + ".TrialTilt.Header";
  
  public static final String COMBINE_SETUP_TO_SELECTOR_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
      .getStorableName() + ".Setup.ToSelector.Header";
  public static final String COMBINE_SETUP_SOLVEMATCH_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
  .getStorableName() + ".Setup.Solvematch.Header";
  public static final String COMBINE_SETUP_PATCHCORR_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
  .getStorableName() + ".Setup.Patchcorr.Header";
  public static final String COMBINE_SETUP_TEMP_DIR_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
  .getStorableName() + ".Setup.TempDir.Header";
  
  public static final String COMBINE_INITIAL_SOLVEMATCH_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
  .getStorableName() + ".Initial.Solvematch.Header";
  
  public static final String COMBINE_FINAL_PATCH_REGION_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
  .getStorableName() + ".Initial.PatchRegion.Header";
  public static final String COMBINE_FINAL_PATCHCORR_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
  .getStorableName() + ".Final.Patchcorr.Header";
  public static final String COMBINE_FINAL_MATCHORWARP_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
  .getStorableName() + ".Final.Matchorwarp.Header";
  public static final String COMBINE_FINAL_VOLCOMBINE_HEADER_GROUP = DialogType.TOMOGRAM_COMBINATION
  .getStorableName() + ".Final.Volcombine.Header";
  
  private final PanelHeaderState tomoGenNewstHeaderState = new PanelHeaderState(TOMO_GEN_NEWST_HEADER_GROUP);
  private final PanelHeaderState tomoGenMtffilterHeaderState = new PanelHeaderState(TOMO_GEN_MTFFILTER_HEADER_GROUP);
  private final PanelHeaderState tomoGenTiltHeaderState = new PanelHeaderState(TOMO_GEN_TILT_HEADER_GROUP);
  private final PanelHeaderState tomoGenTrialTiltHeaderState = new PanelHeaderState(TOMO_GEN_TRIAL_TILT_HEADER_GROUP);
  
  private final PanelHeaderState combineSetupToSelectorHeaderState = new PanelHeaderState(COMBINE_SETUP_TO_SELECTOR_HEADER_GROUP);
  private final PanelHeaderState combineSetupSolvematchHeaderState = new PanelHeaderState(COMBINE_SETUP_SOLVEMATCH_HEADER_GROUP);
  private final PanelHeaderState combineSetupPatchcorrHeaderState = new PanelHeaderState(COMBINE_SETUP_PATCHCORR_HEADER_GROUP);
  private final PanelHeaderState combineSetupTempDirHeaderState = new PanelHeaderState(COMBINE_SETUP_TEMP_DIR_HEADER_GROUP);
  
  private final PanelHeaderState combineInitialSolvematchHeaderState = new PanelHeaderState(COMBINE_INITIAL_SOLVEMATCH_HEADER_GROUP);

  private final PanelHeaderState combineFinalPatchRegionHeaderState = new PanelHeaderState(COMBINE_FINAL_PATCH_REGION_HEADER_GROUP);
  private final PanelHeaderState combineFinalPatchcorrHeaderState = new PanelHeaderState(COMBINE_FINAL_PATCHCORR_HEADER_GROUP);
  private final PanelHeaderState combineFinalMatchorwarpHeaderState = new PanelHeaderState(COMBINE_FINAL_MATCHORWARP_HEADER_GROUP);
  private final PanelHeaderState combineFinalVolcombineHeaderState = new PanelHeaderState(COMBINE_FINAL_VOLCOMBINE_HEADER_GROUP);

  public ReconScreenState(AxisID axisID, AxisType axisType) {
    super(axisID, axisType);
  }
  
  public void store(Properties props) {
    super.store(props);
  }
  
  public void store(Properties props, String prepend) {
    super.store(props, prepend);
    prepend = getPrepend(prepend);
    tomoGenNewstHeaderState.store(props, prepend);
    tomoGenMtffilterHeaderState.store(props, prepend);
    tomoGenTiltHeaderState.store(props, prepend);
    tomoGenTrialTiltHeaderState.store(props, prepend);
    if (axisID == AxisID.FIRST) {
      combineSetupToSelectorHeaderState.store(props, prepend);
      combineSetupSolvematchHeaderState.store(props, prepend);
      combineSetupPatchcorrHeaderState.store(props, prepend);
      combineSetupTempDirHeaderState.store(props, prepend);
      
      combineInitialSolvematchHeaderState.store(props, prepend);
      
      combineFinalPatchRegionHeaderState.store(props, prepend);
      combineFinalPatchcorrHeaderState.store(props, prepend);
      combineFinalMatchorwarpHeaderState.store(props, prepend);
      combineFinalVolcombineHeaderState.store(props, prepend);
    }
  }
  
  public void load(Properties props) {
    super.load(props);
  }
  
  public void load(Properties props, String prepend) {
    super.load(props, prepend);
    prepend = getPrepend(prepend);
    tomoGenNewstHeaderState.load(props, prepend);
    tomoGenMtffilterHeaderState.load(props, prepend);
    tomoGenTiltHeaderState.load(props, prepend);
    tomoGenTrialTiltHeaderState.load(props, prepend);
    if (axisID == AxisID.FIRST) {
      combineSetupToSelectorHeaderState.load(props, prepend);
      combineSetupSolvematchHeaderState.load(props, prepend);
      combineSetupPatchcorrHeaderState.load(props, prepend);
      combineSetupTempDirHeaderState.load(props, prepend);
      
      combineInitialSolvematchHeaderState.load(props, prepend);
      
      combineFinalPatchRegionHeaderState.load(props, prepend);
      combineFinalPatchcorrHeaderState.load(props, prepend);
      combineFinalMatchorwarpHeaderState.load(props, prepend);
      combineFinalVolcombineHeaderState.load(props, prepend);
    }
  }
  
  private final static String getAxisExtension(AxisID axisID) {
    if (axisID == AxisID.ONLY) {
      axisID = AxisID.FIRST;
    }
    return axisID.getExtension().toUpperCase();
  }
  
  public final PanelHeaderState getTomoGenNewstHeaderState() {
    return tomoGenNewstHeaderState;
  }
  public final PanelHeaderState getTomoGenMtffilterHeaderState() {
    return tomoGenMtffilterHeaderState;
  }
  public final PanelHeaderState getTomoGenTiltHeaderState() {
    return tomoGenTiltHeaderState;
  }
  public final PanelHeaderState getTomoGenTrialTiltHeaderState() {
    return tomoGenTrialTiltHeaderState;
  }
  public final PanelHeaderState getCombineSetupToSelectorHeaderState() {
    return combineSetupToSelectorHeaderState;
  }
  public final PanelHeaderState getCombineSetupSolvematchHeaderState() {
    return combineSetupSolvematchHeaderState;
  }
  public final PanelHeaderState getCombineSetupPatchcorrHeaderState() {
    return combineSetupPatchcorrHeaderState;
  }
  public final PanelHeaderState getCombineSetupTempDirHeaderState() {
    return combineSetupTempDirHeaderState;
  }
  public final PanelHeaderState getCombineInitialSolvematchHeaderState() {
    return combineInitialSolvematchHeaderState;
  }
  public final PanelHeaderState getCombineFinalPatchRegionHeaderState() {
    return combineFinalPatchRegionHeaderState;
  }
  public final PanelHeaderState getCombineFinalPatchcorrHeaderState() {
    return combineFinalPatchcorrHeaderState;
  }
  public final PanelHeaderState getCombineFinalMatchorwarpHeaderState() {
    return combineFinalMatchorwarpHeaderState;
  }
  public final PanelHeaderState getCombineFinalVolcombineHeaderState() {
    return combineFinalVolcombineHeaderState;
  }
}
/**
* <p> $Log$
* <p> Revision 1.1  2005/09/27 23:22:11  sueh
* <p> bug# 532 This is a top level Storable object that should be written to the
* <p> .edf and .ejf files.  One instance per axis is created.
* <p> </p>
*/