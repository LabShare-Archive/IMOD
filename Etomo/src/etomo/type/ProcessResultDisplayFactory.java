package etomo.type;

import java.util.Vector;

import etomo.ui.AlignmentEstimationDialog;
import etomo.ui.CoarseAlignDialog;
import etomo.ui.FiducialModelDialog;
import etomo.ui.PostProcessingDialog;
import etomo.ui.PreProcessingDialog;
import etomo.ui.TomogramCombinationDialog;
import etomo.ui.TomogramGenerationDialog;
import etomo.ui.TomogramPositioningDialog;

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
public final class ProcessResultDisplayFactory {
  public static final String rcsid = "$Id$";

  private final BaseScreenState screenState;
  private final Vector dependentDisplayList = new Vector();

  //Recon
  //preprocessing

  private final ProcessResultDisplay findXRays = PreProcessingDialog
      .getFindXRaysDisplay();
  private final ProcessResultDisplay createFixedStack = PreProcessingDialog
      .getCreateFixedStackDisplay();
  private final ProcessResultDisplay useFixedStack = PreProcessingDialog
      .getUseFixedStackDisplay();

  //coarse alignment

  private final ProcessResultDisplay crossCorrelate = CoarseAlignDialog
      .getCrossCorrelateDisplay();
  private final ProcessResultDisplay distortionCorrectedStack = CoarseAlignDialog
      .getDistortionCorrectedStackDisplay();
  private final ProcessResultDisplay fixEdgesMidas = CoarseAlignDialog
      .getFixEdgesMidasDisplay();
  private final ProcessResultDisplay coarseAlign = CoarseAlignDialog
      .getCoarseAlignDisplay();
  private final ProcessResultDisplay midas = CoarseAlignDialog
      .getMidasDisplay();

  //fiducial model

  private final ProcessResultDisplay transferFiducials = FiducialModelDialog
      .getTransferFiducialsDisplay();
  private final ProcessResultDisplay seedFiducialModel = FiducialModelDialog
      .getSeedFiducialModelDisplay();
  private final ProcessResultDisplay trackFiducials = FiducialModelDialog
      .getTrackFiducialsDisplay();
  private final ProcessResultDisplay fixFiducialModel = FiducialModelDialog
      .getFixFiducialModelDisplay();

  //fine alignment

  private final ProcessResultDisplay computeAlignment = AlignmentEstimationDialog
      .getComputeAlignmentDisplay();

  //positioning

  private final ProcessResultDisplay sampleTomogram = TomogramPositioningDialog
      .getSampleTomogramDisplay();
  private final ProcessResultDisplay computePitch = TomogramPositioningDialog
      .getComputePitchDisplay();
  private final ProcessResultDisplay finalAlignment = TomogramPositioningDialog
      .getFinalAlignmentDisplay();

  //generation

  private final ProcessResultDisplay fullAlignedStack = TomogramGenerationDialog
      .getFullAlignedStackDisplay();
  private final ProcessResultDisplay filter = TomogramGenerationDialog
      .getFilterDisplay();
  private final ProcessResultDisplay useFilteredStack = TomogramGenerationDialog
      .getUseFilteredStackDisplay();
  private final ProcessResultDisplay useTrialTomogram = TomogramGenerationDialog
      .getUseTrialTomogramDisplay();
  private final ProcessResultDisplay generateTomogram = TomogramGenerationDialog
      .getGenerateTomogramDisplay();
  private final ProcessResultDisplay deleteAlignedStack = TomogramGenerationDialog
      .getDeleteAlignedStackDisplay();

  //combination

  private final ProcessResultDisplay createCombine = TomogramCombinationDialog
      .getCreateCombineDisplay();
  private final ProcessResultDisplay combine = TomogramCombinationDialog
      .getCombineDisplay();
  private final ProcessResultDisplay restartCombine = TomogramCombinationDialog
      .getRestartCombineDisplay();
  private final ProcessResultDisplay restartMatchvol1 = TomogramCombinationDialog
      .getRestartMatchvol1Display();
  private final ProcessResultDisplay restartPatchcorr = TomogramCombinationDialog
      .getRestartPatchcorrDisplay();
  private final ProcessResultDisplay restartMatchorwarp = TomogramCombinationDialog
      .getRestartMatchorwarpDisplay();
  private final ProcessResultDisplay restartVolcombine = TomogramCombinationDialog
      .getRestartVolcombineDisplay();

  //post processing
  private final ProcessResultDisplay trimVolume = PostProcessingDialog
      .getTrimVolumeDisplay();
  private final ProcessResultDisplay squeezeVolume = PostProcessingDialog
      .getSqueezeVolumeDisplay();

  public ProcessResultDisplayFactory(BaseScreenState screenState) {
    this.screenState = screenState;
    //preprocessing
    addDependency(findXRays);
    addDependency(createFixedStack);
    addDependency(useFixedStack);
    //coarse alignment
    addDependency(crossCorrelate);
    addDependency(distortionCorrectedStack);
    addDependency(fixEdgesMidas);
    addDependency(coarseAlign);
    addDependency(midas);
    //fiducial model
    addDependency(transferFiducials);
    addDependency(seedFiducialModel);
    addDependency(trackFiducials);
    addDependency(fixFiducialModel);
    //fine alignment
    addDependency(computeAlignment);
    //positioning
    addDependency(sampleTomogram);
    addDependency(computePitch);
    addDependency(finalAlignment);
    //generation
    addDependency(fullAlignedStack);
    addDependency(filter);
    addDependency(useFilteredStack);
    addDependency(useTrialTomogram);
    addDependency(generateTomogram);
    addDependency(deleteAlignedStack);
    //combination
    addDependency(createCombine);
    addDependency(combine);
    addDependency(restartCombine);
    addDependency(restartMatchvol1);
    addDependency(restartPatchcorr);
    addDependency(restartMatchorwarp);
    addDependency(restartVolcombine);
    //post processing
    addDependency(squeezeVolume);
    addDependency(trimVolume);
  }

  private synchronized void addDependency(ProcessResultDisplay display) {
    dependentDisplayList.add(display);
    display.setDependencyIndex(dependentDisplayList.size() - 1);
  }

  private void addDependents(ProcessResultDisplay display) {
    if (display == null) {
      return;
    }
    int index = display.getDependencyIndex();
    if (index < 0) {
      return;
    }
    for (int i = index + 1; i < dependentDisplayList.size(); i++) {
      display.addDependentDisplay((ProcessResultDisplay) dependentDisplayList
          .get(i));
    }
  }

  //preprocessing

  public ProcessResultDisplay getFindXRays() {
    if (!findXRays.isInitialized()) {
      findXRays.setInitialized(true);
      findXRays.setScreenState(screenState);
      addDependents(findXRays);
    }
    return findXRays;
  }

  public ProcessResultDisplay getCreateFixedStack() {
    if (!createFixedStack.isInitialized()) {
      createFixedStack.setInitialized(true);
      createFixedStack.setScreenState(screenState);
      addDependents(createFixedStack);
    }
    return createFixedStack;
  }

  public ProcessResultDisplay getUseFixedStack() {
    if (!useFixedStack.isInitialized()) {
      useFixedStack.setInitialized(true);
      useFixedStack.setScreenState(screenState);
      addDependents(useFixedStack);
    }
    return useFixedStack;
  }

  //coarse alignment

  public ProcessResultDisplay getCrossCorrelate() {
    if (!crossCorrelate.isInitialized()) {
      crossCorrelate.setInitialized(true);
      crossCorrelate.setScreenState(screenState);
      addDependents(crossCorrelate);
    }
    return crossCorrelate;
  }

  public ProcessResultDisplay getDistortionCorrectedStack() {
    if (!distortionCorrectedStack.isInitialized()) {
      distortionCorrectedStack.setInitialized(true);
      distortionCorrectedStack.setScreenState(screenState);
      addDependents(distortionCorrectedStack);
    }
    return distortionCorrectedStack;
  }

  public ProcessResultDisplay getFixEdgesMidas() {
    if (!fixEdgesMidas.isInitialized()) {
      fixEdgesMidas.setInitialized(true);
      fixEdgesMidas.setScreenState(screenState);
      addDependents(fixEdgesMidas);
    }
    return fixEdgesMidas;
  }

  public ProcessResultDisplay getCoarseAlign() {
    if (!coarseAlign.isInitialized()) {
      coarseAlign.setInitialized(true);
      coarseAlign.setScreenState(screenState);
      addDependents(coarseAlign);
    }
    return coarseAlign;
  }

  public ProcessResultDisplay getMidas() {
    if (!midas.isInitialized()) {
      midas.setInitialized(true);
      midas.setScreenState(screenState);
      addDependents(midas);
    }
    return midas;
  }

  //fiducial model

  public ProcessResultDisplay getTransferFiducials() {
    if (!transferFiducials.isInitialized()) {
      transferFiducials.setInitialized(true);
      transferFiducials.setScreenState(screenState);
      addDependents(transferFiducials);
    }
    return transferFiducials;
  }

  public ProcessResultDisplay getSeedFiducialModel() {
    if (!seedFiducialModel.isInitialized()) {
      seedFiducialModel.setInitialized(true);
      seedFiducialModel.setScreenState(screenState);
      addDependents(seedFiducialModel);
    }
    return seedFiducialModel;
  }

  public ProcessResultDisplay getTrackFiducials() {
    if (!trackFiducials.isInitialized()) {
      trackFiducials.setInitialized(true);
      trackFiducials.setScreenState(screenState);
      addDependents(trackFiducials);
    }
    return trackFiducials;
  }

  public ProcessResultDisplay getFixFiducialModel() {
    if (!fixFiducialModel.isInitialized()) {
      fixFiducialModel.setInitialized(true);
      fixFiducialModel.setScreenState(screenState);
      addDependents(fixFiducialModel);
    }
    return fixFiducialModel;
  }

  //fine alignment

  public ProcessResultDisplay getComputeAlignment() {
    if (!computeAlignment.isInitialized()) {
      computeAlignment.setInitialized(true);
      computeAlignment.setScreenState(screenState);
      addDependents(computeAlignment);
    }
    return computeAlignment;
  }

  //positioning

  public ProcessResultDisplay getSampleTomogram() {
    if (!sampleTomogram.isInitialized()) {
      sampleTomogram.setInitialized(true);
      sampleTomogram.setScreenState(screenState);
      addDependents(sampleTomogram);
    }
    return sampleTomogram;
  }

  public ProcessResultDisplay getComputePitch() {
    if (!computePitch.isInitialized()) {
      computePitch.setInitialized(true);
      computePitch.setScreenState(screenState);
      addDependents(computePitch);
    }
    return computePitch;
  }

  public ProcessResultDisplay getFinalAlignment() {
    if (!finalAlignment.isInitialized()) {
      finalAlignment.setInitialized(true);
      finalAlignment.setScreenState(screenState);
      addDependents(finalAlignment);
    }
    return finalAlignment;
  }

  //generation

  public ProcessResultDisplay getFullAlignedStack() {
    if (!fullAlignedStack.isInitialized()) {
      fullAlignedStack.setInitialized(true);
      fullAlignedStack.setScreenState(screenState);
      addDependents(fullAlignedStack);
    }
    return fullAlignedStack;
  }

  public ProcessResultDisplay getFilter() {
    if (!filter.isInitialized()) {
      filter.setInitialized(true);
      filter.setScreenState(screenState);
      //filter is optional
      filter.addDependentDisplay(getUseFilteredStack());
    }
    return filter;
  }

  public ProcessResultDisplay getUseFilteredStack() {
    if (!useFilteredStack.isInitialized()) {
      useFilteredStack.setInitialized(true);
      useFilteredStack.setScreenState(screenState);
      addDependents(useFilteredStack);
    }
    return useFilteredStack;
  }

  public ProcessResultDisplay getUseTrialTomogram() {
    if (!useTrialTomogram.isInitialized()) {
      useTrialTomogram.setInitialized(true);
      useTrialTomogram.setScreenState(screenState);
      //use trial tomogram and generate tomogram are equals in the dependency order
      addDependents(getGenerateTomogram());
    }
    return useTrialTomogram;
  }

  public ProcessResultDisplay getGenerateTomogram() {
    if (!generateTomogram.isInitialized()) {
      generateTomogram.setInitialized(true);
      generateTomogram.setScreenState(screenState);
      addDependents(generateTomogram);
    }
    return generateTomogram;
  }

  public ProcessResultDisplay getDeleteAlignedStack() {
    if (!deleteAlignedStack.isInitialized()) {
      deleteAlignedStack.setInitialized(true);
      deleteAlignedStack.setScreenState(screenState);
      addDependents(deleteAlignedStack);
    }
    return deleteAlignedStack;
  }

  //combination

  public ProcessResultDisplay getCreateCombine() {
    if (!createCombine.isInitialized()) {
      createCombine.setInitialized(true);
      createCombine.setScreenState(screenState);
      addDependents(createCombine);
    }
    return createCombine;
  }

  public ProcessResultDisplay getCombine() {
    if (!combine.isInitialized()) {
      combine.setInitialized(true);
      combine.setScreenState(screenState);
      //combine and restart combine are equals in the dependency order
      addDependents(getRestartCombine());
      //combine and restart combine run the same process (solvematch)
      restartCombine.addFailureDisplay(restartCombine);
      restartCombine.addSuccessDisplay(restartCombine);
    }
    return combine;
  }

  public ProcessResultDisplay getRestartCombine() {
    if (!restartCombine.isInitialized()) {
      restartCombine.setInitialized(true);
      restartCombine.setScreenState(screenState);
      addDependents(restartCombine);
      //combine and restart combine run the same process (solvematch)
      restartCombine.addFailureDisplay(getCombine());
      restartCombine.addSuccessDisplay(combine);
    }
    return restartCombine;
  }

  public ProcessResultDisplay getRestartMatchvol1() {
    if (!restartMatchvol1.isInitialized()) {
      restartMatchvol1.setInitialized(true);
      restartMatchvol1.setScreenState(screenState);
      addDependents(restartMatchvol1);

    }
    return restartMatchvol1;
  }

  public ProcessResultDisplay getRestartPatchcorr() {
    if (!restartPatchcorr.isInitialized()) {
      restartPatchcorr.setInitialized(true);
      restartPatchcorr.setScreenState(screenState);
      addDependents(restartPatchcorr);
    }
    return restartPatchcorr;
  }

  public ProcessResultDisplay getRestartMatchorwarp() {
    if (!restartMatchorwarp.isInitialized()) {
      restartMatchorwarp.setInitialized(true);
      restartMatchorwarp.setScreenState(screenState);
      addDependents(restartMatchorwarp);
    }
    return restartMatchorwarp;
  }

  public ProcessResultDisplay getRestartVolcombine() {
    if (!restartVolcombine.isInitialized()) {
      restartVolcombine.setInitialized(true);
      restartVolcombine.setScreenState(screenState);
      addDependents(restartVolcombine);
    }
    return restartVolcombine;
  }

  //post processing

  public ProcessResultDisplay getTrimVolume() {
    if (!trimVolume.isInitialized()) {
      trimVolume.setInitialized(true);
      trimVolume.setScreenState(screenState);
      addDependents(trimVolume);

    }
    return trimVolume;
  }

  public ProcessResultDisplay getSqueezeVolume() {
    if (!squeezeVolume.isInitialized()) {
      squeezeVolume.setInitialized(true);
      squeezeVolume.setScreenState(screenState);
    }
    return squeezeVolume;
  }
}
/**
 * <p> $Log$
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