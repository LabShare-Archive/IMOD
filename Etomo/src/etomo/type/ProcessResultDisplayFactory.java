package etomo.type;

import etomo.ui.PostProcessingDialog;
import etomo.ui.TomogramCombinationDialog;

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
  public static  final String  rcsid =  "$Id$";
  
  private final BaseScreenState screenState;
  
  //combination
  private ProcessResultDisplay combine = null;
  private ProcessResultDisplay restartCombine = null;
  private ProcessResultDisplay restartMatchvol1 = null;
  private ProcessResultDisplay restartPatchcorr = null;
  private ProcessResultDisplay restartMatchorwarp = null;
  private ProcessResultDisplay restartVolcombine = null;
  
  //post processing
  private ProcessResultDisplay squeezeVolume = null;
  private ProcessResultDisplay trimVolume = null;
  
  public ProcessResultDisplayFactory(BaseScreenState screenState) {
    this.screenState = screenState;
  }
  
  //combination
  
  public ProcessResultDisplay getCombine() {
    if (combine == null) {
      combine = TomogramCombinationDialog.getCombineDisplay();
      combine.setScreenState(screenState);
      combine.addFollowingDisplay(getRestartMatchvol1());
      combine.addFollowingDisplay(getRestartPatchcorr());
      combine.addFollowingDisplay(getRestartMatchorwarp());
      combine.addFollowingDisplay(getRestartVolcombine());
      combine.addSuccessDisplay(getRestartCombine());
      combine.addFailureDisplay(restartCombine);
    }
    return combine;
  }

  public ProcessResultDisplay getRestartCombine() {
    if (restartCombine == null) {
      restartCombine = TomogramCombinationDialog.getRestartCombineDisplay();
      restartCombine.setScreenState(screenState);
      restartCombine.addFollowingDisplay(getRestartMatchvol1());
      restartCombine.addFollowingDisplay(getRestartPatchcorr());
      restartCombine.addFollowingDisplay(getRestartMatchorwarp());
      restartCombine.addFollowingDisplay(getRestartVolcombine());
      restartCombine.addSuccessDisplay(getCombine());
      restartCombine.addFailureDisplay(combine);
    }
    return restartCombine;
  }
  
  public ProcessResultDisplay getRestartMatchvol1() {
    if (restartMatchvol1 == null) {
      restartMatchvol1 = TomogramCombinationDialog.getRestartMatchvol1Display();
      restartMatchvol1.setScreenState(screenState);
      restartMatchvol1.addFollowingDisplay(getRestartPatchcorr());
      restartMatchvol1.addFollowingDisplay(getRestartMatchorwarp());
      restartMatchvol1.addFollowingDisplay(getRestartVolcombine());
      restartMatchvol1.addFailureDisplay(getCombine());
      restartMatchvol1.addFailureDisplay(getRestartCombine());
    }
    return restartMatchvol1;
  }
  
  public ProcessResultDisplay getRestartPatchcorr() {
    if (restartPatchcorr == null) {
      restartPatchcorr = TomogramCombinationDialog.getRestartPatchcorrDisplay();
      restartPatchcorr.setScreenState(screenState);
      restartPatchcorr.addFollowingDisplay(getRestartMatchorwarp());
      restartPatchcorr.addFollowingDisplay(getRestartVolcombine());
      restartPatchcorr.addFailureDisplay(getCombine());
      restartPatchcorr.addFailureDisplay(getRestartCombine());
    }
    return restartPatchcorr;
  }
  
  public ProcessResultDisplay getRestartMatchorwarp() {
    if (restartMatchorwarp == null) {
      restartMatchorwarp = TomogramCombinationDialog.getRestartMatchorwarpDisplay();
      restartMatchorwarp.setScreenState(screenState);
      restartMatchorwarp.addFollowingDisplay(getRestartVolcombine());
      restartMatchorwarp.addFailureDisplay(getCombine());
      restartMatchorwarp.addFailureDisplay(getRestartCombine());

    }
    return restartMatchorwarp;
  }
  
  public ProcessResultDisplay getRestartVolcombine() {
    if (restartVolcombine == null) {
      restartVolcombine = TomogramCombinationDialog.getRestartVolcombineDisplay();
      restartVolcombine.setScreenState(screenState);
      restartVolcombine.addFollowingDisplay(getTrimVolume());
      restartVolcombine.addFollowingDisplay(getSqueezeVolume());
      restartVolcombine.addFailureDisplay(getCombine());
      restartVolcombine.addFailureDisplay(getRestartCombine());
      restartVolcombine.addSuccessDisplay(combine);
      restartVolcombine.addSuccessDisplay(restartCombine);
    }
    return restartVolcombine;
  }
  
  //post processing
  
  public ProcessResultDisplay getTrimVolume() {
    if (trimVolume == null) {
      trimVolume = PostProcessingDialog.getTrimVolumeDisplay();
      trimVolume.setScreenState(screenState);
      trimVolume.addFollowingDisplay(getSqueezeVolume());

    }
    return trimVolume;
  }
  
  public ProcessResultDisplay getSqueezeVolume() {
    if (squeezeVolume == null) {
      squeezeVolume = PostProcessingDialog.getSqueezeVolumeDisplay();
      squeezeVolume.setScreenState(screenState);

    }
    return squeezeVolume;
  }
}
/**
* <p> $Log$ </p>
*/