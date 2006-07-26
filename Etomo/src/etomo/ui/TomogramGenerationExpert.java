package etomo.ui;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.NewstParam;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.process.ImodManager;
import etomo.process.ProcessState;
import etomo.storage.autodoc.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.ProcessResult;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessTrack;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.type.ViewType;
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
 */
public final class TomogramGenerationExpert extends ReconUIExpert {
  public static final String rcsid = "$Id$";

  private final ComScriptManager comScriptMgr;
  private final TomogramState state;
  private final ReconScreenState screenState;

  private TomogramGenerationDialog dialog = null;
  private boolean advanced = false;

  public TomogramGenerationExpert(ApplicationManager manager,
      MainTomogramPanel mainPanel, ProcessTrack processTrack, AxisID axisID) {
    super(manager, mainPanel, processTrack, axisID, DialogType.TOMOGRAM_GENERATION);
    comScriptMgr = manager.getComScriptManager();
    state = manager.getState();
    screenState = manager.getScreenState(axisID);
  }

  /**
   * Open the tomogram generation dialog
   */
  public void openDialog() {
    if (!showDialog(dialog) || dialog != null) {
      return;
    }
    Utilities.timestamp("new", "TomogramGenerationDialog",
        Utilities.STARTED_STATUS);
    dialog = new TomogramGenerationDialog(manager, this, axisID);
    Utilities.timestamp("new", "TomogramGenerationDialog",
        Utilities.FINISHED_STATUS);
    // no longer managing image size

    // Read in the newst{|a|b}.com parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions
    if (metaData.getViewType() == ViewType.MONTAGE) {
      comScriptMgr.loadBlend(axisID);
      dialog.setBlendParams(comScriptMgr.getBlendParam(axisID));
    }
    else {
      comScriptMgr.loadNewst(axisID);
      dialog.setNewstParams(comScriptMgr.getNewstComNewstParam(axisID));
    }
    dialog.setParameters(metaData);
    dialog.setParameters(screenState);
    // Read in the tilt{|a|b}.com parameters and display the dialog panel
    comScriptMgr.loadTilt(axisID);
    comScriptMgr.loadMTFFilter(axisID);
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    // If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      // upgrade and save param to comscript
      UIExpertUtilities.INSTANCE.upgradeOldTiltCom(manager, axisID, tiltParam);
    }
    dialog.setTiltParams(tiltParam);
    dialog.setMTFFilterParam(comScriptMgr.getMTFFilterParam(axisID));
    //updateDialog()
    dialog.updateFilter(Utilities.fileExists(manager, ".ali", axisID));

    // Set the fidcialess state and tilt axis angle
    dialog.setFiducialessAlignment(metaData.isFiducialessAlignment(axisID));
    dialog.setImageRotation(metaData.getImageRotation(axisID));
    setEnabledTiltParameters();
    openDialog(dialog);
  }
  
  public void updateDialog() {
  dialog.updateFilter(Utilities.fileExists(manager, ".ali", axisID));}

  /**
   * Start the next process specified by the nextProcess string
   */
  public void startNextProcess(ProcessResultDisplay processResultDisplay) {
    ProcessName nextProcess = getNextProcess();
    resetNextProcess();
    if (nextProcess == ProcessName.PROCESSCHUNKS) {
      processchunks(manager, dialog, processResultDisplay);
    }
  }

  public void setEnabledTiltParameters() {
    if (dialog == null) {
      return;
    }
    boolean madeZFactors = false;
    boolean newstFiducialessAlignment = false;
    boolean usedLocalAlignments = false;
    // madeZFactors
    if (!state.getMadeZFactors(axisID).isNull()) {
      madeZFactors = state.getMadeZFactors(axisID).is();
    }
    else {
      madeZFactors = state.getBackwardCompatibleMadeZFactors(axisID);
    }
    // newstFiducialessAlignment
    if (!state.getNewstFiducialessAlignment(axisID).isNull()) {
      newstFiducialessAlignment = state.getNewstFiducialessAlignment(axisID)
          .is();
    }
    else {
      newstFiducialessAlignment = dialog.isFiducialessAlignment();
    }
    // usedLocalAlignments
    if (!state.getUsedLocalAlignments(axisID).isNull()) {
      usedLocalAlignments = state.getUsedLocalAlignments(axisID).is();
    }
    else {
      usedLocalAlignments = state
          .getBackwardCompatibleUsedLocalAlignments(axisID);
    }
    // enable parameters
    dialog.enableUseZFactors(madeZFactors && !newstFiducialessAlignment);
    dialog.enableUseLocalAlignment(usedLocalAlignments
        && !newstFiducialessAlignment);
  }

  boolean doneDialog() {
    if (dialog == null) {
      return false;
    }
    DialogExitState exitState = dialog.getExitState();
    if (exitState == DialogExitState.EXECUTE) {
      manager.closeImod(ImodManager.MTF_FILTER_KEY, axisID, "filtered stack");
      manager.closeImods(ImodManager.TRIAL_TOMOGRAM_KEY, axisID,
          "Trial tomogram");
    }
    if (exitState != DialogExitState.CANCEL) {
      if (!saveDialog()) {
        return false;
      }
    }
    // Clean up the existing dialog
    leaveDialog(exitState);
    dialog = null;
    return true;
  }

  protected boolean saveDialog() {
    advanced = dialog.isAdvanced();
    // Get the user input data from the dialog box
    dialog.getParameters(metaData);
    dialog.getParameters(screenState);
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(manager, dialog,
        axisID)) {
      return false;
    }
    if (metaData.getViewType() == ViewType.MONTAGE) {
      updateBlendCom();
    }
    else {
      if (updateNewstCom() == null) {
        return false;
      }
    }
    if (!updateTiltCom(true)) {
      return false;
    }
    if (!updateMTFFilterCom()) {
      return false;
    }
    manager.saveIntermediateParamFile(axisID);
    return true;
  }

  /**
   * Get the set the blendmont parameters and update the blend com script.
   * @param axisID
   * @return
   */
  private BlendmontParam updateBlendCom() {
    BlendmontParam blendParam = comScriptMgr.getBlendParam(axisID);
    dialog.getBlendParams(blendParam);
    blendParam.setMode(BlendmontParam.BLEND_MODE);
    blendParam.setBlendmontState();
    comScriptMgr.saveBlend(blendParam, axisID);
    return blendParam;
  }

  /**
   * Update the newst.com from the TomogramGenerationDialog reads metaData
   * 
   * @param axisID
   * @return true if successful
   */
  private ConstNewstParam updateNewstCom() {
    // Set a reference to the correct object
    if (dialog == null) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "Can not update newst?.com without an active tomogram generation dialog",
              "Program logic error", axisID);
      return null;
    }
    NewstParam newstParam = null;
    try {
      newstParam = comScriptMgr.getNewstComNewstParam(axisID);
      // Make sure the size output is removed, it was only there for a
      // copytomocoms template
      newstParam.setSizeToOutputInXandY("/");
      newstParam.setCommandMode(NewstParam.FULL_ALIGNED_STACK_MODE);
      newstParam.setFiducialessAlignment(metaData
          .isFiducialessAlignment(axisID));
      dialog.getNewstParams(newstParam);
      comScriptMgr.saveNewst(newstParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Newst Parameter Syntax Error", axisID);
      return null;
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
    }
    return newstParam;
  }

  /**
   * Update the tilt.com from the TomogramGenerationDialog
   * 
   * @param axisID
   * @param useDefaultRec If true set the reconstruction output filename to what
   *        is expected of the com scripts. If false use the trial tomogram
   *        filename specified in the TomogramGenerationDialog
   * @return true if successful
   */
  private boolean updateTiltCom(boolean useDefaultRec) {
    // Set a reference to the correct object
    if (dialog == null) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "Can not update tilt?.com without an active tomogram generation dialog",
              "Program logic error", axisID);
      return false;
    }
    TiltParam tiltParam = null;
    try {
      tiltParam = comScriptMgr.getTiltParam(axisID);
      dialog.getTiltParams(tiltParam);
      if (useDefaultRec) {
        String outputFileName;
        if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
          outputFileName = metaData.getDatasetName() + "_full.rec";
        }
        else {
          outputFileName = metaData.getDatasetName() + axisID.getExtension()
              + ".rec";
        }
        tiltParam.setOutputFile(outputFileName);
      }
      else {
        String trialTomogramName = dialog.getTrialTomogramName();
        tiltParam.setOutputFile(trialTomogramName);
      }
      if (metaData.getViewType() == ViewType.MONTAGE) {
        // binning is currently always 1 and correct size should be coming from
        // copytomocoms
        // tiltParam.setMontageFullImage(propertyUserDir,
        // tomogramGenerationDialog.getBinning());
      }
      UIExpertUtilities.INSTANCE.rollTiltComAngles(manager, axisID);
      comScriptMgr.saveTilt(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID);
      return false;
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID);
      return false;
    }
    return true;
  }

  /**
   * Update the mtffilter.com from the TomogramGenerationDialog
   * 
   * @param axisID
   * @return true if successful
   */
  private boolean updateMTFFilterCom() {
    // Set a reference to the correct object
    if (dialog == null) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "Can not update mtffilter?.com without an active tomogram generation dialog",
              "Program logic error", axisID);
      return false;
    }
    try {
      MTFFilterParam mtfFilterParam = comScriptMgr.getMTFFilterParam(axisID);
      dialog.getMTFFilterParam(mtfFilterParam);
      String inputFileName;
      String outputFileName;
      if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
        inputFileName = metaData.getDatasetName() + AxisID.ONLY.getExtension()
            + ".ali";
        outputFileName = metaData.getDatasetName() + AxisID.ONLY.getExtension()
            + "_filt.ali";
      }
      else {
        inputFileName = metaData.getDatasetName() + axisID.getExtension()
            + ".ali";
        outputFileName = metaData.getDatasetName() + axisID.getExtension()
            + "_filt.ali";
      }
      mtfFilterParam.setInputFile(inputFileName);
      mtfFilterParam.setOutputFile(outputFileName);
      comScriptMgr.saveMTFFilter(mtfFilterParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "MTF Filter Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "MTF Filter Parameter Syntax Error", axisID);
      return false;
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "MTF Filter Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "MTF Filter Parameter Syntax Error", axisID);
      return false;
    }
    return true;
  }

  protected ProcessDialog getDialog() {
    return dialog;
  }

  /**
   * 
   */
  public void newst(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    // Get the user input from the dialog
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(manager, dialog,
        axisID)) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    ConstNewstParam newstParam = null;
    BlendmontParam blendmontParam = null;
    if (metaData.getViewType() == ViewType.MONTAGE) {
      blendmontParam = updateBlendCom();
    }
    else {
      newstParam = updateNewstCom();
      if (newstParam == null) {
        sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
        return;
      }
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.newst(axisID, processResultDisplay, newstParam,
        blendmontParam), processResultDisplay);
  }

  /**
   */
  void mtffilter(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (!updateMTFFilterCom()) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.mtffilter(axisID, processResultDisplay),
        processResultDisplay);
  }

  /**
   * Start a tilt process in trial mode
   * 
   * @param axisID
   */
  void trialTilt(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (!updateTiltCom(false)) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.tiltProcess(axisID, processResultDisplay),
        processResultDisplay);
  }

  /**
   * Run the tilt command script for the specified axis
   */
  public void tilt(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (!updateTiltCom(true)) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.tiltProcess(axisID, processResultDisplay),
        processResultDisplay);
  }

  /**
   * Open 3dmod on the current test volume
   * 
   * @param axisID
   */
  public void imodTestVolume(Run3dmodMenuOptions menuOptions) {
    manager.imodTestVolume(axisID, menuOptions, dialog.getTrialTomogramName());
  }

  public void commitTestVolume(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    sendMsg(manager.commitTestVolume(axisID, processResultDisplay, dialog
        .getTrialTomogramName()), processResultDisplay);
  }
  
  public void splittilt(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    splittilt(false, processResultDisplay);
  }

  public void splittilt(boolean trialMode,
      ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (!updateTiltCom(!trialMode)) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    SplittiltParam param = updateSplittiltParam();
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    ProcessResult processResult = manager.splittilt(axisID, trialMode,
        processResultDisplay, param);
    if (processResult == null) {
      setNextProcess(ProcessName.PROCESSCHUNKS);
    }
    sendMsg(processResult, processResultDisplay);
  }

  private final SplittiltParam updateSplittiltParam() {
    if (dialog == null) {
      return null;
    }
    SplittiltParam param = new SplittiltParam(axisID);
    if (!getParallelPanel().getParameters(param)) {
      return null;
    }
    param.setSeparateChunks(CpuAdoc.INSTANCE.isSeparateChunks(axisID));
    return param;
  }
  
  /**
   * Replace the full aligned stack with the filtered full aligned stack created
   * from mtffilter
   * 
   * @param axisID
   */
  public void useMtfFilter(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (manager.isAxisBusy(axisID, processResultDisplay)) {
      return;
    }
    setProgressBar("Using filtered full aligned stack", 1, axisID);
    // Instantiate file objects for the original raw stack and the fixed
    // stack
    String fullAlignedStackFilename = manager.getPropertyUserDir() + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + ".ali";
    File fullAlignedStack = new File(fullAlignedStackFilename);
    String filteredFullAlignedStackFilename = manager.getPropertyUserDir() + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + "_filt.ali";
    File filteredFullAlignedStack = new File(filteredFullAlignedStackFilename);
    if (!filteredFullAlignedStack.exists()) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "The filtered full aligned stack doesn't exist.  Create the filtered full aligned stack first",
              "Filtered full aligned stack missing", axisID);
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    if (fullAlignedStack.exists() && filteredFullAlignedStack.exists()) {
      try {
        Utilities.renameFile(fullAlignedStack, new File(fullAlignedStack
            .getAbsolutePath()
            + "~"));
      }
      catch (IOException except) {
        UIHarness.INSTANCE.openMessageDialog("Unable to backup "
            + fullAlignedStack.getAbsolutePath() + "\n" + except.getMessage(),
            "File Rename Error", axisID);
        sendMsg(ProcessResult.FAILED, processResultDisplay);
        return;
      }
    }
    // don't have to rename full aligned stack because it is a generated
    // file
    try {
      Utilities.renameFile(filteredFullAlignedStack, fullAlignedStack);
    }
    catch (IOException except) {
      UIHarness.INSTANCE.openMessageDialog(except.getMessage(), "File Rename Error",
          axisID);
      sendMsg(ProcessResult.FAILED, processResultDisplay);
      return;
    }
    manager.closeImod(ImodManager.FINE_ALIGNED_KEY, axisID,
        "original full aligned stack");
    stopProgressBar(axisID);
    sendMsg(ProcessResult.SUCCEEDED, processResultDisplay);
  }
}
/**
 * <p> $Log$ </p>
 */
