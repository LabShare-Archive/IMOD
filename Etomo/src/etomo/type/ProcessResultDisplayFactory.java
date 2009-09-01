package etomo.type;

import java.util.Vector;

import etomo.process.ProcessResultDisplayFactoryInterface;
import etomo.ui.AlignmentEstimationDialog;
import etomo.ui.CoarseAlignDialog;
import etomo.ui.FiducialModelDialog;
import etomo.ui.FinalAlignedStackDialog;
import etomo.ui.PostProcessingDialog;
import etomo.ui.PreProcessingDialog;
import etomo.ui.TomogramCombinationDialog;
import etomo.ui.TomogramGenerationDialog;
import etomo.ui.TomogramPositioningExpert;

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
  private final ProcessResultDisplay raptor = FiducialModelDialog
      .getRaptorDisplay();
  private final ProcessResultDisplay useRaptor = FiducialModelDialog
      .getUseRaptorDisplay();
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

  private final ProcessResultDisplay sampleTomogram = TomogramPositioningExpert
      .getSampleTomogramDisplay();
  private final ProcessResultDisplay computePitch = TomogramPositioningExpert
      .getComputePitchDisplay();
  private final ProcessResultDisplay finalAlignment = TomogramPositioningExpert
      .getFinalAlignmentDisplay();

  //final aligned stack

  private final ProcessResultDisplay fullAlignedStack = FinalAlignedStackDialog
      .getFullAlignedStackDisplay();
  private final ProcessResultDisplay ctfCorrection = FinalAlignedStackDialog
      .getCtfCorrectionDisplay();
  private final ProcessResultDisplay useCtfCorrection = FinalAlignedStackDialog
      .getUseCtfCorrectionDisplay();
  private final ProcessResultDisplay xfModel = FinalAlignedStackDialog
      .getXfModelDisplay();
  private final ProcessResultDisplay tilt3dFind = FinalAlignedStackDialog
      .getTilt3dFindButton();
  private final ProcessResultDisplay findBeads3d = FinalAlignedStackDialog
      .getFindBeads3dButton();
  private final ProcessResultDisplay reprojectModel = FinalAlignedStackDialog
      .getReprojectModelDisplay();
  private final ProcessResultDisplay ccdEraserBeads = FinalAlignedStackDialog
      .getCcdEraserButton();
  private final ProcessResultDisplay useCcdEraserBeads = FinalAlignedStackDialog
      .getUseCcdEraserDisplay();
  private final ProcessResultDisplay filter = FinalAlignedStackDialog
      .getFilterDisplay();
  private final ProcessResultDisplay useFilteredStack = FinalAlignedStackDialog
      .getUseFilteredStackDisplay();

  //generation

  private final ProcessResultDisplay useTrialTomogram = TomogramGenerationDialog
      .getUseTrialTomogramResultDisplay();
  private final ProcessResultDisplay generateTomogram = TomogramGenerationDialog
      .getGenerateTomogramResultDisplay();
  private final ProcessResultDisplay deleteAlignedStack = TomogramGenerationDialog
      .getDeleteAlignedStackResultDisplay();

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
  private final ProcessResultDisplay flatten = PostProcessingDialog
      .getFlattenDisplay();
  private final ProcessResultDisplay flattenWarp = PostProcessingDialog
      .getFlattenWarpDisplay();
  private final ProcessResultDisplay squeezeVolume = PostProcessingDialog
      .getSqueezeVolumeDisplay();

  private ProcessResultDisplayFactory(BaseScreenState screenState) {
    this.screenState = screenState;
  }

  public static ProcessResultDisplayFactory getInstance(
      BaseScreenState screenState) {
    ProcessResultDisplayFactory instance = new ProcessResultDisplayFactory(
        screenState);
    instance.initialize();
    return instance;
  }

  private void initialize() {
    //initialize global dependency list
    //all displays should be added to this list
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
    addDependency(raptor);
    addDependency(useRaptor);
    addDependency(trackFiducials);
    addDependency(fixFiducialModel);
    //fine alignment
    addDependency(computeAlignment);
    //positioning
    addDependency(sampleTomogram);
    addDependency(computePitch);
    addDependency(finalAlignment);
    //stack
    addDependency(fullAlignedStack);
    addDependency(ctfCorrection);
    addDependency(useCtfCorrection);
    addDependency(xfModel);
    addDependency(tilt3dFind);
    addDependency(findBeads3d);
    addDependency(reprojectModel);
    addDependency(ccdEraserBeads);
    addDependency(useCcdEraserBeads);
    addDependency(filter);
    addDependency(useFilteredStack);
    //generation
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
    addDependency(trimVolume);
    addDependency(flattenWarp);
    addDependency(flatten);
    addDependency(squeezeVolume);

    //set screeen state

    //preprocessing
    findXRays.setScreenState(screenState);
    createFixedStack.setScreenState(screenState);
    useFixedStack.setScreenState(screenState);
    //coarse alignment
    crossCorrelate.setScreenState(screenState);
    distortionCorrectedStack.setScreenState(screenState);
    fixEdgesMidas.setScreenState(screenState);
    coarseAlign.setScreenState(screenState);
    midas.setScreenState(screenState);
    //fiducial model
    transferFiducials.setScreenState(screenState);
    raptor.setScreenState(screenState);
    useRaptor.setScreenState(screenState);
    seedFiducialModel.setScreenState(screenState);
    trackFiducials.setScreenState(screenState);
    fixFiducialModel.setScreenState(screenState);
    //fine alignment
    computeAlignment.setScreenState(screenState);
    //positioning
    sampleTomogram.setScreenState(screenState);
    computePitch.setScreenState(screenState);
    finalAlignment.setScreenState(screenState);
    //stack
    fullAlignedStack.setScreenState(screenState);
    ctfCorrection.setScreenState(screenState);
    useCtfCorrection.setScreenState(screenState);
    xfModel.setScreenState(screenState);
    tilt3dFind.setScreenState(screenState);
    findBeads3d.setScreenState(screenState);
    reprojectModel.setScreenState(screenState);
    ccdEraserBeads.setScreenState(screenState);
    useCcdEraserBeads.setScreenState(screenState);
    filter.setScreenState(screenState);
    useFilteredStack.setScreenState(screenState);
    //generation
    useTrialTomogram.setScreenState(screenState);
    generateTomogram.setScreenState(screenState);
    deleteAlignedStack.setScreenState(screenState);
    //combination
    createCombine.setScreenState(screenState);
    combine.setScreenState(screenState);
    restartCombine.setScreenState(screenState);
    restartMatchvol1.setScreenState(screenState);
    restartPatchcorr.setScreenState(screenState);
    restartMatchorwarp.setScreenState(screenState);
    restartVolcombine.setScreenState(screenState);
    //post processing
    trimVolume.setScreenState(screenState);
    flattenWarp.setScreenState(screenState);
    flatten.setScreenState(screenState);
    squeezeVolume.setScreenState(screenState);

    //turn off everything after display

    //preprocessing
    addDependents(findXRays);
    addDependents(createFixedStack);
    addDependents(useFixedStack);
    //coarse alignment
    addDependents(crossCorrelate);
    addDependents(distortionCorrectedStack);
    addDependents(fixEdgesMidas);
    addDependents(coarseAlign);
    //fiducial model
    addDependents(transferFiducials);
    addDependents(seedFiducialModel);
    addDependents(useRaptor);
    addDependents(trackFiducials);
    addDependents(fixFiducialModel);
    //fine alignment
    addDependents(computeAlignment);
    //positioning
    addDependents(sampleTomogram);
    addDependents(computePitch);
    addDependents(finalAlignment);
    //stack
    addDependents(fullAlignedStack);
    addDependents(useCtfCorrection);
    addDependents(useCcdEraserBeads);
    addDependents(useFilteredStack);
    //generation
    addDependents(generateTomogram);
    //combination
    addDependents(createCombine);
    addDependents(combine);
    addDependents(restartCombine);
    addDependents(restartMatchvol1);
    addDependents(restartPatchcorr);
    addDependents(restartMatchorwarp);
    addDependents(restartVolcombine);
    //post processing
    addDependents(trimVolume);

    //turn off selected displays

    //fiducial model
    raptor.addDependentDisplay(useRaptor);
    //stack
    ctfCorrection.addDependentDisplay(useCtfCorrection);
    xfModel.addDependentDisplay(ccdEraserBeads);
    xfModel.addDependentDisplay(useCcdEraserBeads);
    tilt3dFind.addDependentDisplay(findBeads3d);
    tilt3dFind.addDependentDisplay(reprojectModel);
    tilt3dFind.addDependentDisplay(ccdEraserBeads);
    tilt3dFind.addDependentDisplay(useCcdEraserBeads);
    findBeads3d.addDependentDisplay(reprojectModel);
    findBeads3d.addDependentDisplay(ccdEraserBeads);
    findBeads3d.addDependentDisplay(useCcdEraserBeads);
    reprojectModel.addDependentDisplay(ccdEraserBeads);
    reprojectModel.addDependentDisplay(useCcdEraserBeads);
    ccdEraserBeads.addDependentDisplay(useCcdEraserBeads);
    filter.addDependentDisplay(useFilteredStack);
    //combination
    combine.addFailureDisplay(restartCombine);
    combine.addSuccessDisplay(restartCombine);
    restartCombine.addFailureDisplay(combine);
    restartCombine.addSuccessDisplay(combine);
    //post processing
    flattenWarp.addDependentDisplay(flatten);
  }

  private synchronized void addDependency(ProcessResultDisplay display) {
    dependentDisplayList.add(display);
    //DependencyIndex should be unique and match the index of the
    //dependentDisplayList where the display is stored.
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
      display.addDependentDisplay((ProcessResultDisplay) dependentDisplayList
          .get(i));
    }
  }

  //preprocessing

  public ProcessResultDisplay getFindXRays() {
    return findXRays;
  }

  public ProcessResultDisplay getCreateFixedStack() {
    return createFixedStack;
  }

  public ProcessResultDisplay getUseFixedStack() {
    return useFixedStack;
  }

  //coarse alignment

  public ProcessResultDisplay getCrossCorrelate() {
    return crossCorrelate;
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

  //fiducial model

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

  //fine alignment

  public ProcessResultDisplay getComputeAlignment() {
    return computeAlignment;
  }

  //positioning

  public ProcessResultDisplay getSampleTomogram() {
    return sampleTomogram;
  }

  public ProcessResultDisplay getComputePitch() {
    return computePitch;
  }

  public ProcessResultDisplay getFinalAlignment() {
    return finalAlignment;
  }

  //stack

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

  public ProcessResultDisplay getTilt3dFind() {
    return tilt3dFind;
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

  //generation

  public ProcessResultDisplay getUseTrialTomogram() {
    return useTrialTomogram;
  }

  public ProcessResultDisplay getGenerateTomogram() {
    return generateTomogram;
  }

  public ProcessResultDisplay getDeleteAlignedStack() {
    return deleteAlignedStack;
  }

  //combination

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

  //post processing

  public ProcessResultDisplay getTrimVolume() {
    return trimVolume;
  }

  public ProcessResultDisplay getFlatten() {
    return flatten;
  }

  public ProcessResultDisplay getFlattenWarp() {
    return flattenWarp;
  }

  public ProcessResultDisplay getSqueezeVolume() {
    return squeezeVolume;
  }
}
/**
 * <p> $Log$
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
