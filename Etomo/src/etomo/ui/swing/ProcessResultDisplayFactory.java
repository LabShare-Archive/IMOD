package etomo.ui.swing;

import java.util.Vector;

import etomo.process.ProcessResultDisplayFactoryInterface;
import etomo.type.BaseScreenState;
import etomo.type.DialogType;
import etomo.type.ProcessResultDisplay;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ProcessResultDisplayFactory implements
    ProcessResultDisplayFactoryInterface {
  public static final String rcsid = "$Id$";

  private final BaseScreenState screenState;
  private final Vector dependentDisplayList = new Vector();

  // Recon
  // preprocessing

  private final ProcessResultDisplay findXRays = Run3dmodButton
      .getDeferredToggle3dmodInstance("Find X-rays (Trial Mode)",
          DialogType.PRE_PROCESSING);
  private final ProcessResultDisplay createFixedStack = Run3dmodButton
      .getDeferredToggle3dmodInstance(CcdEraserXRaysPanel.ERASE_LABEL,
          DialogType.PRE_PROCESSING);
  private final ProcessResultDisplay useFixedStack = MultiLineButton
      .getToggleButtonInstance(CcdEraserXRaysPanel.USE_FIXED_STACK_LABEL,
          DialogType.PRE_PROCESSING);

  // coarse alignment

  private final ProcessResultDisplay coarseTiltxcorr = MultiLineButton
      .getToggleButtonInstance("Calculate Cross- Correlation",
          DialogType.COARSE_ALIGNMENT);
  private final ProcessResultDisplay distortionCorrectedStack = MultiLineButton
      .getToggleButtonInstance("Make Distortion Corrected Stack",
          DialogType.COARSE_ALIGNMENT);
  private final ProcessResultDisplay fixEdgesMidas = MultiLineButton
      .getToggleButtonInstance("Fix Edges With Midas", DialogType.COARSE_ALIGNMENT);
  private final ProcessResultDisplay coarseAlign = Run3dmodButton
      .getDeferredToggle3dmodInstance("Generate Coarse Aligned Stack",
          DialogType.COARSE_ALIGNMENT);
  private final ProcessResultDisplay midas = MultiLineButton.getToggleButtonInstance(
      "Fix Alignment With Midas", DialogType.COARSE_ALIGNMENT);

  // fiducial model

  private final ProcessResultDisplay transferFiducials = Run3dmodButton
      .getDeferredToggle3dmodInstance("Transfer Fiducials From Other Axis",
          DialogType.FIDUCIAL_MODEL);
  private final ProcessResultDisplay raptor = Run3dmodButton
      .getDeferredToggle3dmodInstance(RaptorPanel.RUN_RAPTOR_LABEL,
          DialogType.FIDUCIAL_MODEL);
  private final ProcessResultDisplay useRaptor = MultiLineButton.getToggleButtonInstance(
      RaptorPanel.USE_RAPTOR_RESULT_LABEL, DialogType.FIDUCIAL_MODEL);
  private final ProcessResultDisplay seedFiducialModel = Run3dmodButton
      .getToggle3dmodInstance(FiducialModelDialog.SEEDING_NOT_DONE_LABEL,
          DialogType.FIDUCIAL_MODEL);
  private final ProcessResultDisplay trackFiducials = MultiLineButton
      .getToggleButtonInstance(BeadtrackPanel.TRACK_LABEL, DialogType.FIDUCIAL_MODEL);
  private final ProcessResultDisplay fixFiducialModel = Run3dmodButton
      .getToggle3dmodInstance("Fix Fiducial Model", DialogType.FIDUCIAL_MODEL);
  private final ProcessResultDisplay trackTiltxcorr = Run3dmodButton
      .getDeferredToggle3dmodInstance("Track Patches", DialogType.FIDUCIAL_MODEL);
  private final Run3dmodButton autofidseed = Run3dmodButton
      .getDeferredToggle3dmodInstance(FiducialModelDialog.AUTOFIDSEED_NEW_MODEL_LABEL,
          DialogType.FIDUCIAL_MODEL);
  private final ProcessResultDisplay imodchopconts = Run3dmodButton
      .getDeferredToggle3dmodInstance("Recut or Restore Contours",
          DialogType.FIDUCIAL_MODEL);

  // fine alignment

  private final ProcessResultDisplay computeAlignment = MultiLineButton
      .getToggleButtonInstance("Compute Alignment", DialogType.FINE_ALIGNMENT);

  // positioning

  private final ProcessResultDisplay sampleTomogram = Run3dmodButton
      .getDeferredToggle3dmodInstance(TomogramPositioningExpert.SAMPLE_TOMOGRAMS_LABEL,
          DialogType.TOMOGRAM_POSITIONING);
  private final ProcessResultDisplay computePitch = MultiLineButton
      .getToggleButtonInstance("Compute Z Shift & Pitch Angles",
          DialogType.TOMOGRAM_POSITIONING);
  private final ProcessResultDisplay finalAlignment = MultiLineButton
      .getToggleButtonInstance("Create Final Alignment", DialogType.TOMOGRAM_POSITIONING);

  // final aligned stack

  private final ProcessResultDisplay fullAlignedStack = Run3dmodButton
      .getDeferredToggle3dmodInstance(NewstackOrBlendmontPanel.RUN_BUTTON_LABEL,
          DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay ctfCorrection = Run3dmodButton
      .getDeferredToggle3dmodInstance(FinalAlignedStackDialog.CTF_CORRECTION_LABEL,
          DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay useCtfCorrection = MultiLineButton
      .getToggleButtonInstance(FinalAlignedStackDialog.USE_CTF_CORRECTION_LABEL,
          DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay xfModel = Run3dmodButton
      .getDeferredToggle3dmodInstance("Transform Fiducial Model",
          DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay stackTilt = Run3dmodButton
      .getDeferredToggle3dmodInstance(Tilt3dFindPanel.TILT_3D_FIND_LABEL,
          DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay findBeads3d = Run3dmodButton
      .getDeferredToggle3dmodInstance("Run Findbeads3d", DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay reprojectModel = Run3dmodButton
      .getDeferredToggle3dmodInstance(ReprojectModelPanel.REPROJECT_MODEL_LABEL,
          DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay ccdEraserBeads = Run3dmodButton
      .getDeferredToggle3dmodInstance(CcdEraserBeadsPanel.CCD_ERASER_LABEL,
          DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay useCcdEraserBeads = Run3dmodButton
      .getDeferredToggle3dmodInstance(CcdEraserBeadsPanel.USE_ERASED_STACK_LABEL,
          DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay filter = Run3dmodButton
      .getDeferredToggle3dmodInstance("Filter", DialogType.FINAL_ALIGNED_STACK);
  private final ProcessResultDisplay useFilteredStack = MultiLineButton
      .getToggleButtonInstance(FinalAlignedStackDialog.USE_FILTERED_STACK_LABEL,
          DialogType.FINAL_ALIGNED_STACK);

  // generation

  private final ProcessResultDisplay useTrialTomogram = MultiLineButton
      .getToggleButtonInstance("Use Current Trial Tomogram",
          DialogType.TOMOGRAM_GENERATION);
  private final ProcessResultDisplay genTilt = Run3dmodButton
      .getDeferredToggle3dmodInstance("Generate Tomogram", DialogType.TOMOGRAM_GENERATION);
  private final ProcessResultDisplay deleteAlignedStack = MultiLineButton
      .getToggleButtonInstance("Delete Intermediate Image Stacks",
          DialogType.TOMOGRAM_GENERATION);
  private final ProcessResultDisplay sirtsetup = Run3dmodButton
      .getDeferredToggle3dmodInstance("Run SIRT", DialogType.TOMOGRAM_GENERATION);
  private final ProcessResultDisplay useSirt = MultiLineButton.getToggleButtonInstance(
      "Use SIRT Output File", DialogType.TOMOGRAM_GENERATION);

  // combination

  private final ProcessResultDisplay createCombine = MultiLineButton
      .getToggleButtonInstance("Create Combine Scripts", DialogType.TOMOGRAM_COMBINATION);
  private final ProcessResultDisplay combine = Run3dmodButton
      .getDeferredToggle3dmodInstance("Start Combine", DialogType.TOMOGRAM_COMBINATION);
  private final ProcessResultDisplay restartCombine = Run3dmodButton
      .getDeferredToggle3dmodInstance("Restart Combine", DialogType.TOMOGRAM_COMBINATION);
  private final ProcessResultDisplay restartMatchvol1 = Run3dmodButton
      .getDeferredToggle3dmodInstance("Restart at Matchvol1",
          DialogType.TOMOGRAM_COMBINATION);
  private final ProcessResultDisplay restartPatchcorr = Run3dmodButton
      .getDeferredToggle3dmodInstance("Restart at Patchcorr",
          DialogType.TOMOGRAM_COMBINATION);
  private final ProcessResultDisplay restartMatchorwarp = Run3dmodButton
      .getDeferredToggle3dmodInstance("Restart at Matchorwarp",
          DialogType.TOMOGRAM_COMBINATION);
  private final ProcessResultDisplay restartVolcombine = Run3dmodButton
      .getDeferredToggle3dmodInstance("Restart at Volcombine",
          DialogType.TOMOGRAM_COMBINATION);

  // post processing
  private final ProcessResultDisplay trimVolume = Run3dmodButton
      .getDeferredToggle3dmodInstance("Trim Volume", DialogType.POST_PROCESSING);
  private final ProcessResultDisplay flatten = Run3dmodButton
      .getDeferredToggle3dmodInstance(FlattenVolumePanel.FLATTEN_LABEL,
          DialogType.POST_PROCESSING);
  private final ProcessResultDisplay flattenWarp = new MultiLineButton(
      FlattenVolumePanel.FLATTEN_WARP_LABEL);
  private final ProcessResultDisplay squeezeVolume = Run3dmodButton
      .getDeferredToggle3dmodInstance("Squeeze Volume", DialogType.POST_PROCESSING);
  private final ProcessResultDisplay smoothingAssessment = Run3dmodButton
      .getDeferredToggle3dmodInstance(SmoothingAssessmentPanel.FLATTEN_WARP_LABEL,
          DialogType.POST_PROCESSING);

  private ProcessResultDisplayFactory(BaseScreenState screenState) {
    this.screenState = screenState;
  }

  public static ProcessResultDisplayFactory getInstance(BaseScreenState screenState) {
    ProcessResultDisplayFactory instance = new ProcessResultDisplayFactory(screenState);
    instance.initialize();
    return instance;
  }

  private void initialize() {
    // initialize global dependency list
    // all displays should be added to this list
    // preprocessing
    addDependency(findXRays);
    addDependency(createFixedStack);
    addDependency(useFixedStack);
    // coarse alignment
    addDependency(coarseTiltxcorr);
    addDependency(distortionCorrectedStack);
    addDependency(fixEdgesMidas);
    addDependency(coarseAlign);
    addDependency(midas);
    // fiducial model
    addDependency(transferFiducials);
    addDependency(seedFiducialModel);
    addDependency(autofidseed);
    addDependency(trackTiltxcorr);
    addDependency(imodchopconts);
    addDependency(raptor);
    addDependency(useRaptor);
    addDependency(trackFiducials);
    addDependency(fixFiducialModel);
    // fine alignment
    addDependency(computeAlignment);
    // positioning
    addDependency(sampleTomogram);
    addDependency(computePitch);
    addDependency(finalAlignment);
    // stack
    addDependency(fullAlignedStack);
    addDependency(ctfCorrection);
    addDependency(useCtfCorrection);
    addDependency(xfModel);
    addDependency(stackTilt);
    addDependency(findBeads3d);
    addDependency(reprojectModel);
    addDependency(ccdEraserBeads);
    addDependency(useCcdEraserBeads);
    addDependency(filter);
    addDependency(useFilteredStack);
    // generation
    addDependency(useTrialTomogram);
    addDependency(genTilt);
    addDependency(deleteAlignedStack);
    addDependency(sirtsetup);
    addDependency(useSirt);
    // combination
    addDependency(createCombine);
    addDependency(combine);
    addDependency(restartCombine);
    addDependency(restartMatchvol1);
    addDependency(restartPatchcorr);
    addDependency(restartMatchorwarp);
    addDependency(restartVolcombine);
    // post processing
    addDependency(trimVolume);
    addDependency(smoothingAssessment);
    addDependency(flattenWarp);
    addDependency(flatten);
    addDependency(squeezeVolume);

    // set screeen state

    // preprocessing
    findXRays.setScreenState(screenState);
    createFixedStack.setScreenState(screenState);
    useFixedStack.setScreenState(screenState);
    // coarse alignment
    coarseTiltxcorr.setScreenState(screenState);
    distortionCorrectedStack.setScreenState(screenState);
    fixEdgesMidas.setScreenState(screenState);
    coarseAlign.setScreenState(screenState);
    midas.setScreenState(screenState);
    // fiducial model
    transferFiducials.setScreenState(screenState);
    trackTiltxcorr.setScreenState(screenState);
    imodchopconts.setScreenState(screenState);
    raptor.setScreenState(screenState);
    useRaptor.setScreenState(screenState);
    seedFiducialModel.setScreenState(screenState);
    autofidseed.setScreenState(screenState);
    trackFiducials.setScreenState(screenState);
    fixFiducialModel.setScreenState(screenState);
    // fine alignment
    computeAlignment.setScreenState(screenState);
    // positioning
    sampleTomogram.setScreenState(screenState);
    computePitch.setScreenState(screenState);
    finalAlignment.setScreenState(screenState);
    // stack
    fullAlignedStack.setScreenState(screenState);
    ctfCorrection.setScreenState(screenState);
    useCtfCorrection.setScreenState(screenState);
    xfModel.setScreenState(screenState);
    stackTilt.setScreenState(screenState);
    findBeads3d.setScreenState(screenState);
    reprojectModel.setScreenState(screenState);
    ccdEraserBeads.setScreenState(screenState);
    useCcdEraserBeads.setScreenState(screenState);
    filter.setScreenState(screenState);
    useFilteredStack.setScreenState(screenState);
    // generation
    useTrialTomogram.setScreenState(screenState);
    genTilt.setScreenState(screenState);
    deleteAlignedStack.setScreenState(screenState);
    sirtsetup.setScreenState(screenState);
    useSirt.setScreenState(screenState);
    // combination
    createCombine.setScreenState(screenState);
    combine.setScreenState(screenState);
    restartCombine.setScreenState(screenState);
    restartMatchvol1.setScreenState(screenState);
    restartPatchcorr.setScreenState(screenState);
    restartMatchorwarp.setScreenState(screenState);
    restartVolcombine.setScreenState(screenState);
    // post processing
    trimVolume.setScreenState(screenState);
    smoothingAssessment.setScreenState(screenState);
    flattenWarp.setScreenState(screenState);
    flatten.setScreenState(screenState);
    squeezeVolume.setScreenState(screenState);

    // turn off everything after button

    // preprocessing
    addDependents(findXRays);
    addDependents(createFixedStack);
    addDependents(useFixedStack);
    // coarse alignment
    addDependents(coarseTiltxcorr);
    addDependents(distortionCorrectedStack);
    addDependents(fixEdgesMidas);
    addDependents(coarseAlign);
    // fiducial model
    addDependents(trackTiltxcorr);
    addDependents(imodchopconts);
    addDependents(transferFiducials);
    addDependents(seedFiducialModel);
    addDependents(autofidseed);
    addDependents(useRaptor);
    addDependents(trackFiducials);
    addDependents(fixFiducialModel);
    // fine alignment
    addDependents(computeAlignment);
    // positioning
    addDependents(sampleTomogram);
    addDependents(computePitch);
    addDependents(finalAlignment);
    // stack
    addDependents(fullAlignedStack);
    addDependents(useCtfCorrection);
    addDependents(useCcdEraserBeads);
    addDependents(useFilteredStack);
    // generation
    addDependents(genTilt);
    addDependents(useTrialTomogram);
    addDependents(sirtsetup);
    addDependents(useSirt);
    // combination
    addDependents(createCombine);
    addDependents(combine);
    addDependents(restartCombine);
    addDependents(restartMatchvol1);
    addDependents(restartPatchcorr);
    addDependents(restartMatchorwarp);
    addDependents(restartVolcombine);
    // post processing
    addDependents(trimVolume);

    // turn off selected buttons

    // fiducial model
    raptor.addDependentDisplay(useRaptor);
    // stack
    ctfCorrection.addDependentDisplay(useCtfCorrection);
    xfModel.addDependentDisplay(ccdEraserBeads);
    xfModel.addDependentDisplay(useCcdEraserBeads);
    stackTilt.addDependentDisplay(findBeads3d);
    stackTilt.addDependentDisplay(reprojectModel);
    stackTilt.addDependentDisplay(ccdEraserBeads);
    stackTilt.addDependentDisplay(useCcdEraserBeads);
    findBeads3d.addDependentDisplay(reprojectModel);
    findBeads3d.addDependentDisplay(ccdEraserBeads);
    findBeads3d.addDependentDisplay(useCcdEraserBeads);
    reprojectModel.addDependentDisplay(ccdEraserBeads);
    reprojectModel.addDependentDisplay(useCcdEraserBeads);
    ccdEraserBeads.addDependentDisplay(useCcdEraserBeads);
    filter.addDependentDisplay(useFilteredStack);
    // gen
    genTilt.addDependentDisplay(useTrialTomogram);
    sirtsetup.addDependentDisplay(useSirt);
    deleteAlignedStack.addDependentDisplay(fullAlignedStack);
    // combination
    combine.addFailureDisplay(restartCombine);
    combine.addSuccessDisplay(restartCombine);
    restartCombine.addFailureDisplay(combine);
    restartCombine.addSuccessDisplay(combine);
    // post processing
    flattenWarp.addDependentDisplay(flatten);
  }

  private synchronized void addDependency(ProcessResultDisplay display) {
    dependentDisplayList.add(display);
    // DependencyIndex should be unique and match the index of the
    // dependentDisplayList where the display is stored.
    display.setDependencyIndex(dependentDisplayList.size() - 1);
  }

  public ProcessResultDisplay getProcessResultDisplay(int dependencyIndex) {
    if (dependencyIndex < 0 || dependencyIndex >= dependentDisplayList.size()) {
      return null;
    }
    return (ProcessResultDisplay) dependentDisplayList.get(dependencyIndex);
  }

  /**
   * Add everything after this display to its dependency list.
   * @param display
   */
  private void addDependents(ProcessResultDisplay display) {
    if (display == null) {
      return;
    }
    int index = display.getDependencyIndex();
    if (index < 0) {
      return;
    }
    for (int i = index + 1; i < dependentDisplayList.size(); i++) {
      display.addDependentDisplay((ProcessResultDisplay) dependentDisplayList.get(i));
    }
  }

  // preprocessing

  public ProcessResultDisplay getFindXRays() {
    return findXRays;
  }

  public ProcessResultDisplay getCreateFixedStack() {
    return createFixedStack;
  }

  public ProcessResultDisplay getUseFixedStack() {
    return useFixedStack;
  }

  // coarse alignment

  public ProcessResultDisplay getTiltxcorr(final DialogType dialogType) {
    if (dialogType == DialogType.COARSE_ALIGNMENT) {
      return coarseTiltxcorr;
    }
    else if (dialogType == DialogType.FIDUCIAL_MODEL) {
      return trackTiltxcorr;
    }
    return null;
  }

  public ProcessResultDisplay getImodchopconts() {
    return imodchopconts;
  }

  public ProcessResultDisplay getAutofidseed() {
    return autofidseed;
  }

  public ProcessResultDisplay getDistortionCorrectedStack() {
    return distortionCorrectedStack;
  }

  public ProcessResultDisplay getFixEdgesMidas() {
    return fixEdgesMidas;
  }

  public ProcessResultDisplay getCoarseAlign() {
    return coarseAlign;
  }

  public ProcessResultDisplay getMidas() {
    return midas;
  }

  // fiducial model

  public ProcessResultDisplay getTransferFiducials() {
    return transferFiducials;
  }

  public ProcessResultDisplay getRaptor() {
    return raptor;
  }

  public ProcessResultDisplay getUseRaptor() {
    return useRaptor;
  }

  public ProcessResultDisplay getSeedFiducialModel() {
    return seedFiducialModel;
  }

  public ProcessResultDisplay getTrackFiducials() {
    return trackFiducials;
  }

  public ProcessResultDisplay getFixFiducialModel() {
    return fixFiducialModel;
  }

  // fine alignment

  public ProcessResultDisplay getComputeAlignment() {
    return computeAlignment;
  }

  // positioning

  public ProcessResultDisplay getSampleTomogram() {
    return sampleTomogram;
  }

  public ProcessResultDisplay getComputePitch() {
    return computePitch;
  }

  public ProcessResultDisplay getFinalAlignment() {
    return finalAlignment;
  }

  // stack

  public ProcessResultDisplay getFullAlignedStack() {
    return fullAlignedStack;
  }

  public ProcessResultDisplay getCtfCorrection() {
    return ctfCorrection;
  }

  public ProcessResultDisplay getUseCtfCorrection() {
    return useCtfCorrection;
  }

  public ProcessResultDisplay getXfModel() {
    return xfModel;
  }

  public ProcessResultDisplay getFindBeads3d() {
    return findBeads3d;
  }

  public ProcessResultDisplay getTilt(DialogType dialogType) {
    if (dialogType == DialogType.FINAL_ALIGNED_STACK) {
      return stackTilt;
    }
    return genTilt;
  }

  public ProcessResultDisplay getSirtsetup() {
    return sirtsetup;
  }

  public ProcessResultDisplay getUseSirt() {
    return useSirt;
  }

  public ProcessResultDisplay getReprojectModel() {
    return reprojectModel;
  }

  public ProcessResultDisplay getCcdEraserBeads() {
    return ccdEraserBeads;
  }

  public ProcessResultDisplay getUseCcdEraserBeads() {
    return useCcdEraserBeads;
  }

  public ProcessResultDisplay getFilter() {
    return filter;
  }

  public ProcessResultDisplay getUseFilteredStack() {
    return useFilteredStack;
  }

  // generation

  public ProcessResultDisplay getUseTrialTomogram() {
    return useTrialTomogram;
  }

  public ProcessResultDisplay getDeleteAlignedStack() {
    return deleteAlignedStack;
  }

  // combination

  public ProcessResultDisplay getCreateCombine() {
    return createCombine;
  }

  public ProcessResultDisplay getCombine() {
    return combine;
  }

  public ProcessResultDisplay getRestartCombine() {
    return restartCombine;
  }

  public ProcessResultDisplay getRestartMatchvol1() {
    return restartMatchvol1;
  }

  public ProcessResultDisplay getRestartPatchcorr() {
    return restartPatchcorr;
  }

  public ProcessResultDisplay getRestartMatchorwarp() {
    return restartMatchorwarp;
  }

  public ProcessResultDisplay getRestartVolcombine() {
    return restartVolcombine;
  }

  // post processing

  public ProcessResultDisplay getTrimVolume() {
    return trimVolume;
  }

  public ProcessResultDisplay getFlatten() {
    return flatten;
  }

  public ProcessResultDisplay getFlattenWarp() {
    return flattenWarp;
  }

  public ProcessResultDisplay getSmoothingAssessment() {
    return smoothingAssessment;
  }

  public ProcessResultDisplay getSqueezeVolume() {
    return squeezeVolume;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.2  2011/02/11 23:10:57  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/12/05 05:15:34  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.
 * <p>
 * <p> Revision 1.16  2010/11/13 16:06:53  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.15  2010/03/03 04:59:07  sueh
 * <p> bug# 1311 Added patchTracking.
 * <p>
 * <p> Revision 1.14  2009/12/19 01:11:16  sueh
 * <p> bug# 1294 Added smoothingAssessment.
 * <p>
 * <p> Revision 1.13  2009/10/01 18:49:00  sueh
 * <p> bug# 1239 Changed PostProcessDialog.getFlattenWarpDisplay to
 * <p> getFlattenWarpButton.
 * <p>
 * <p> Revision 1.12  2009/09/01 03:14:14  sueh
 * <p> bug# 1222 Added findBeads3d, flatten, flattenWarp, reprojectModel, and
 * <p> tilt3dFind.  Clarified code the adds dependancies.
 * <p>
 * <p> Revision 1.11  2009/05/02 01:12:06  sueh
 * <p> bug# 1216 Added raptor and useRaptor.
 * <p>
 * <p> Revision 1.10  2008/11/20 01:38:23  sueh
 * <p> bug# 1147 added ccdEraserBeads, useCcdEraserBeads, and xfModel.
 * <p>
 * <p> Revision 1.9  2008/10/27 20:15:32  sueh
 * <p> bug# 1141 Added ctfCorrection and useCtfCorrection.
 * <p>
 * <p> Revision 1.8  2008/10/16 20:59:47  sueh
 * <p> bug# 1141 Created FinalAlignedStack dialog to run full aligned stack and mtf filter.
 * <p>
 * <p> Revision 1.7  2008/05/07 02:45:21  sueh
 * <p> bug# 847 Getting the the postioning buttons from the expert.
 * <p>
 * <p> Revision 1.6  2008/05/03 00:45:32  sueh
 * <p> bug# 847 Reformatted.
 * <p>
 * <p> Revision 1.5  2008/01/14 22:02:44  sueh
 * <p> bug# 1050 Added getProcessResultDisplay(int dependencyIndex) which retrieves
 * <p> a display based on the unique dependency index.
 * <p>
 * <p> Revision 1.4  2007/09/10 20:34:58  sueh
 * <p> bug# 925 Using getInstance to construct ProcessResultDisplayFactory.  Calling
 * <p> initialize() from getInstance.  Putting all initialization into initialize().  Simplified
 * <p> the get functions.
 * <p>
 * <p> Revision 1.3  2006/03/20 17:59:11  sueh
 * <p> reformatted
 * <p>
 * <p> Revision 1.2  2006/02/06 21:18:17  sueh
 * <p> bug 521 Added all the process dialog toggle buttons.  Added an array
 * <p> of dependent displays.  Making the displays final so they can be added as
 * <p> dependent displays before they are initialized.
 * <p>
 * <p> Revision 1.1  2006/01/31 20:52:33  sueh
 * <p> bug# 521 Class to manage toggle buttons that affect or can be affected
 * <p> by other toggle buttons.  Defines how ProcessResultDisplay's affect each
 * <p> other.  Buttons continue to exist after their dialogs are removed.
 * <p> </p>
 */
