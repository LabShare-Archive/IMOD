package etomo.ui;

import java.io.File;
import java.io.IOException;
import java.util.Vector;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstMTFFilterParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.NewstParam;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.process.ImodManager;
import etomo.process.ProcessState;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.MetaData;
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
  private boolean enableFiltering = false;
  private boolean getBinningFromNewst = true;
  private Vector trialTomogramList = null;

  public TomogramGenerationExpert(ApplicationManager manager,
      MainTomogramPanel mainPanel, ProcessTrack processTrack, AxisID axisID) {
    super(manager, mainPanel, processTrack, axisID,
        DialogType.TOMOGRAM_GENERATION);
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
    trialTomogramList = new Vector();
    Utilities.timestamp("new", "TomogramGenerationDialog",
        Utilities.FINISHED_STATUS);
    // no longer managing image size

    // Read in the newst{|a|b}.com parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions
    if (metaData.getViewType() == ViewType.MONTAGE) {
      comScriptMgr.loadBlend(axisID);
      setBlendParams(comScriptMgr.getBlendParam(axisID));
    }
    else {
      comScriptMgr.loadNewst(axisID);
      setNewstParams(comScriptMgr.getNewstComNewstParam(axisID));
    }
    setParameters(metaData);
    setParameters(screenState);
    // Read in the tilt{|a|b}.com parameters and display the dialog panel
    comScriptMgr.loadTilt(axisID);
    comScriptMgr.loadMTFFilter(axisID);
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    metaData.getTiltParam(tiltParam, axisID);
    // If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      // upgrade and save param to comscript
      UIExpertUtilities.INSTANCE.upgradeOldTiltCom(manager, axisID, tiltParam);
    }
    setTiltParams(tiltParam);
    setMTFFilterParam(comScriptMgr.getMTFFilterParam(axisID));
    //updateDialog()
    updateFilter(Utilities.fileExists(manager, ".ali", axisID));

    // Set the fidcialess state and tilt axis angle
    dialog.setFiducialessAlignment(metaData.isFiducialessAlignment(axisID));
    dialog.setImageRotation(metaData.getImageRotation(axisID));
    setEnabledTiltParameters();
    openDialog(dialog);
  }

  public void updateDialog() {
    updateFilter(Utilities.fileExists(manager, ".ali", axisID));
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  public void startNextProcess(ProcessResultDisplay processResultDisplay) {
    ProcessName nextProcess = getNextProcess();
    resetNextProcess();
    if (nextProcess == ProcessName.PROCESSCHUNKS) {
      processchunks(manager, dialog, processResultDisplay, ProcessName.TILT);
    }
  }

  public void reconnectTilt(ProcessName processName) {
    ProcessResultDisplay display = manager.getProcessResultDisplayFactory(
        axisID).getGenerateTomogram();
    sendMsgProcessStarting(display);
    manager.reconnectTilt(axisID, processName, display);
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
      newstFiducialessAlignment = dialog.isFiducialess();
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
    dialog.setUseZFactorsEnabled(madeZFactors && !newstFiducialessAlignment);
    dialog.setUseLocalAlignmentEnabled(usedLocalAlignments
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
    trialTomogramList = null;
    return true;
  }

  protected boolean saveDialog() {
    if (dialog == null) {
      return false;
    }
    advanced = dialog.isAdvanced();
    // Get the user input data from the dialog box
    try {
      getParameters(metaData);
    }
    catch (FortranInputSyntaxException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Data File Error");
    }
    getParameters(screenState);
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(manager, dialog,
        axisID)) {
      return false;
    }
    if (metaData.getViewType() == ViewType.MONTAGE) {
      try {
        updateBlendCom();
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE
            .openMessageDialog(e.getMessage(), "Update Com Error");
      }
      catch (InvalidParameterException e) {
        UIHarness.INSTANCE
            .openMessageDialog(e.getMessage(), "Update Com Error");
      }
      catch (IOException e) {
        UIHarness.INSTANCE
            .openMessageDialog(e.getMessage(), "Update Com Error");
      }
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
    manager.saveStorables(axisID);
    return true;
  }

  /**
   * Get the set the blendmont parameters and update the blend com script.
   * @param axisID
   * @return
   */
  private BlendmontParam updateBlendCom() throws FortranInputSyntaxException,
      InvalidParameterException, IOException {
    if (dialog == null) {
      return null;
    }
    BlendmontParam blendParam = comScriptMgr.getBlendParam(axisID);
    getBlendParams(blendParam);
    blendParam.setMode(BlendmontParam.Mode.BLEND);
    blendParam.setBlendmontState();
    comScriptMgr.saveBlend(blendParam, axisID);
    return blendParam;
  }

  /**
   * Update the newst.com from the TomogramGenerationDialog.  Reads metaData.
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
      newstParam.setCommandMode(NewstParam.Mode.FULL_ALIGNED_STACK);
      newstParam.setFiducialessAlignment(metaData
          .isFiducialessAlignment(axisID));
      getNewstParams(newstParam);
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
      metaData.getTiltParam(tiltParam, axisID);
      getTiltParams(tiltParam);
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
      metaData.setTiltParam(tiltParam, axisID);
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
    catch (IOException e) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = e.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Tilt Parameter",
          axisID);
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
      getMTFFilterParam(mtfFilterParam);
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
    if (dialog == null) {
      return;
    }
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
      try {
        blendmontParam = updateBlendCom();
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE
            .openMessageDialog(e.getMessage(), "Update Com Error");
      }
      catch (InvalidParameterException e) {
        UIHarness.INSTANCE
            .openMessageDialog(e.getMessage(), "Update Com Error");
      }
      catch (IOException e) {
        UIHarness.INSTANCE
            .openMessageDialog(e.getMessage(), "Update Com Error");
      }
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
    if (dialog == null) {
      return;
    }
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
    if (dialog == null) {
      return;
    }
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
    if (dialog == null) {
      return;
    }
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
  void imodTestVolume(Run3dmodMenuOptions menuOptions) {
    if (dialog == null) {
      return;
    }
    manager.imodTestVolume(axisID, menuOptions, dialog.getTrialTomogramName());
  }

  void commitTestVolume(ProcessResultDisplay processResultDisplay) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    sendMsg(manager.commitTestVolume(axisID, processResultDisplay, dialog
        .getTrialTomogramName()), processResultDisplay);
  }

  void splittilt(ProcessResultDisplay processResultDisplay) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    splittilt(false, processResultDisplay);
  }

  void splittilt(boolean trialMode, ProcessResultDisplay processResultDisplay) {
    if (dialog == null) {
      return;
    }
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
    param.setSeparateChunks(CpuAdoc.getInstance(axisID, manager)
        .isSeparateChunks());
    return param;
  }

  /**
   * Replace the full aligned stack with the filtered full aligned stack created
   * from mtffilter
   * 
   * @param axisID
   */
  void useMtfFilter(ProcessResultDisplay processResultDisplay) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    if (manager.isAxisBusy(axisID, processResultDisplay)) {
      return;
    }
    setProgressBar("Using filtered full aligned stack", 1, axisID);
    // Instantiate file objects for the original raw stack and the fixed
    // stack
    String fullAlignedStackFilename = manager.getPropertyUserDir()
        + File.separator + metaData.getDatasetName() + axisID.getExtension()
        + ".ali";
    File fullAlignedStack = new File(fullAlignedStackFilename);
    String filteredFullAlignedStackFilename = manager.getPropertyUserDir()
        + File.separator + metaData.getDatasetName() + axisID.getExtension()
        + "_filt.ali";
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
      UIHarness.INSTANCE.openMessageDialog(except.getMessage(),
          "File Rename Error", axisID);
      sendMsg(ProcessResult.FAILED, processResultDisplay);
      return;
    }
    manager.closeImod(ImodManager.FINE_ALIGNED_KEY, axisID,
        "original full aligned stack");
    stopProgressBar(axisID);
    sendMsg(ProcessResult.SUCCEEDED, processResultDisplay);
  }

  private void updateFilter(boolean enable) {
    if (dialog == null) {
      return;
    }
    enableFiltering = enable;
    dialog.setFilterButtonEnabled(enableFiltering);
    dialog.setViewFilterButtonEnabled(enableFiltering);
    enableUseFilter();
  }

  protected void enableUseFilter() {
    if (dialog == null) {
      return;
    }
    if (!enableFiltering) {
      dialog.setUseFilterEnabled(false);
      return;
    }
    String startingAndEndingZ = dialog.getStartingAndEndingZ();
    if (startingAndEndingZ.length() == 0 || startingAndEndingZ.matches("\\s+")) {
      //btnFilter.setSelected(false);
      dialog.setUseFilterEnabled(true);
    }
    else {
      dialog.setUseFilterEnabled(false);
    }
  }

  private void setNewstParams(ConstNewstParam newstParam) {
    if (dialog == null) {
      return;
    }
    dialog.setUseLinearInterpolation(newstParam.isLinearInterpolation());
    if (getBinningFromNewst) {
      dialog.setBinning(newstParam.getBinByFactor());
    }
  }

  private void setParameters(ConstMetaData metaData) {
    if (dialog == null) {
      return;
    }
    ConstEtomoNumber binning = metaData.getTomoGenBinning(axisID);
    if (!binning.isNull()) {
      getBinningFromNewst = false;
      dialog.setBinning(binning);
    }
    boolean validAutodoc = CpuAdoc.getInstance(AxisID.ONLY, manager).isValid();
    ConstEtomoNumber tomoGenTiltParallel = metaData
        .getTomoGenTiltParallel(axisID);
    dialog.setParallelProcessEnabled(validAutodoc);
    if (tomoGenTiltParallel == null) {
      dialog.setParallelProcess(validAutodoc
          && metaData.getDefaultParallel().is());
    }
    else {
      dialog.setParallelProcess(validAutodoc && tomoGenTiltParallel.is());
    }
    dialog.setSizeToOutputInXandY(metaData.getSizeToOutputInXandY(axisID)
        .toString(true));
    updateParallelProcess();
  }

  void updateParallelProcess() {
    manager.setParallelDialog(axisID, dialog);
  }

  private final void setParameters(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.setNewstHeaderState(screenState.getTomoGenNewstHeaderState());
    dialog.setFilterHeaderState(screenState.getTomoGenMtffilterHeaderState());
    dialog.setTiltHeaderState(screenState.getTomoGenTiltHeaderState());
    dialog.setTrialHeaderState(screenState.getTomoGenTrialTiltHeaderState());
    dialog.setAdvanced();
    dialog.setNewstButtonState(screenState);
    dialog.setTiltButtonState(screenState);
    dialog.setDeleteStackButtonState(screenState);
    dialog.setUseFilterButtonState(screenState);
    dialog.setUseTrialButtonState(screenState);
    dialog.setFilterButtonState(screenState);
  }

  private void getParameters(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.getNewstHeaderState(screenState.getTomoGenNewstHeaderState());
    dialog.getFilterHeaderState(screenState.getTomoGenMtffilterHeaderState());
    dialog.getTiltHeaderState(screenState.getTomoGenTiltHeaderState());
    dialog.getTrialHeaderState(screenState.getTomoGenTrialTiltHeaderState());
  }

  private void getParameters(MetaData metaData)
      throws FortranInputSyntaxException {
    if (dialog == null) {
      return;
    }
    metaData.setTomoGenBinning(axisID, dialog.getBinning());
    metaData.setTomoGenTiltParallel(axisID, dialog.isParallelProcess());
    try {
      metaData.setSizeToOutputInXandY(axisID, dialog.getSizeToOutputInXandY());
    }
    catch (FortranInputSyntaxException e) {
      e.printStackTrace();
      throw new FortranInputSyntaxException(
          TomogramGenerationDialog.SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + ":  "
              + e.getMessage());
    }
  }

  private void setBlendParams(BlendmontParam blendmontParam) {
    dialog.setUseLinearInterpolation(blendmontParam.isLinearInterpolation());
  }

  /**
   * Set the UI parameters with the specified tiltParam values
   * WARNING: be sure the setNewstParam is called first so the binning value for
   * the stack is known.  The thickness, first and last slice, width and x,y,z
   * offsets are scaled so that they are represented to the user in unbinned
   * dimensions.
   * @param tiltParam
   */
  private void setTiltParams(ConstTiltParam tiltParam) {
    if (dialog == null) {
      return;
    }
    if (tiltParam.hasWidth()) {
      dialog.setTomoWidth(tiltParam.getWidth());
    }
    if (tiltParam.hasThickness()) {
      dialog.setTomoThickness(tiltParam.getThickness());
    }
    if (tiltParam.hasXShift()) {
      dialog.setXShift(tiltParam.getXShift());
    }
    if (tiltParam.hasZShift()) {
      dialog.setZShift(tiltParam.getZShift());
    }
    if (tiltParam.hasSlice()) {
      dialog.setSliceStart(tiltParam.getIdxSliceStart());
      dialog.setSliceStop(tiltParam.getIdxSliceStop());
    }
    if (tiltParam.hasSliceIncr()) {
      dialog.setSliceIncr(tiltParam.getIncrSlice());
    }
    if (tiltParam.hasXAxisTilt()) {
      dialog.setXAxisTilt(tiltParam.getXAxisTilt());
    }
    if (tiltParam.hasTiltAngleOffset()) {
      dialog.setTiltAngleOffset(tiltParam.getTiltAngleOffset());
    }
    if (tiltParam.hasRadialWeightingFunction()) {
      dialog.setRadialMax(tiltParam.getRadialBandwidth());
      dialog.setRadialFallOff(tiltParam.getRadialFalloff());
    }
    if (tiltParam.hasScale()) {
      dialog.setDensityOffset(tiltParam.getScaleFLevel());
      dialog.setDensityScale(tiltParam.getScaleCoeff());
    }
    if (tiltParam.hasLogOffset()) {
      dialog.setLogOffset(tiltParam.getLogShift());
    }
    dialog.setUseLocalAlignment(metaData.getUseLocalAlignments(axisID));
    dialog.setUseZFactors(metaData.getUseZFactors(axisID).is());
    dialog.setExtraExcludeList(tiltParam.getExcludeList2());
  }

  //  Copy the newstack parameters from the GUI to the NewstParam object
  private void getNewstParams(NewstParam newstParam)
      throws FortranInputSyntaxException {
    if (dialog == null) {
      return;
    }
    newstParam.setLinearInterpolation(dialog.isUseLinearInterpolation());
    int binning = dialog.getBinning();
    // Only explcitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script  
    if (binning > 1) {
      newstParam.setBinByFactor(binning);
    }
    else {
      newstParam.setBinByFactor(Integer.MIN_VALUE);
    }
    newstParam.setSizeToOutputInXandY(dialog.getSizeToOutputInXandY(), dialog
        .getBinning());
  }

  private void getBlendParams(BlendmontParam blendmontParam)
      throws FortranInputSyntaxException, InvalidParameterException,
      IOException {
    if (dialog == null) {
      return;
    }
    blendmontParam.setLinearInterpolation(dialog.isUseLinearInterpolation());
    blendmontParam.setBinByFactor(dialog.getBinning());
    try {
      blendmontParam.convertToStartingAndEndingXandY(dialog
          .getSizeToOutputInXandY());
    }
    catch (FortranInputSyntaxException e) {
      e.printStackTrace();
      throw new FortranInputSyntaxException(
          TomogramGenerationDialog.SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + ":  "
              + e.getMessage());
    }
  }

  /**
   * Get the tilt parameters from the requested axis panel
   */
  private void getTiltParams(TiltParam tiltParam) throws NumberFormatException,
      InvalidParameterException, IOException {
    if (dialog == null) {
      return;
    }
    String badParameter = "";
    try {
      badParameter = "IMAGEBINNED";
      tiltParam.setImageBinned();
      //Do not manage full image size.  It is coming from copytomocoms.      
      if (dialog.isTomoWidthSet()) {
        badParameter = dialog.getTomoWidthLabel();
        tiltParam.setWidth(dialog.getTomoWidth());
      }
      else {
        tiltParam.resetWidth();
      }
      //set Z Shift
      if (dialog.isZShiftSet()) {
        badParameter = dialog.getZShiftLabel();
        tiltParam.setZShift(dialog.getZShift());
      }
      else {
        tiltParam.resetZShift();
      }

      //set X Shift
      if (dialog.isXShiftSet()) {
        badParameter = dialog.getXShiftLabel();
        tiltParam.setXShift(dialog.getXShift());
      }
      else if (dialog.isZShiftSet()) {
        tiltParam.setXShift(0);
        dialog.setXShift(0);
      }
      else {
        tiltParam.resetXShift();
      }

      boolean sliceRangeSpecified = false;
      if (dialog.isSliceStartSet() && dialog.isSliceStopSet()) {
        badParameter = dialog.getSliceStartLabel();
        tiltParam.setIdxSliceStart(dialog.getSliceStart());
        badParameter = dialog.getSliceStopLabel();
        tiltParam.setIdxSliceStop(dialog.getSliceStop());
        sliceRangeSpecified = true;
      }
      else if (dialog.isSliceStartNull() && dialog.isSliceStopNull()) {
        tiltParam.resetIdxSlice();
      }
      else {
        throw (new InvalidParameterException(
            "You must supply both the first and last slices if you want to specify either."));
      }
      if (dialog.isSliceIncrSet()) {
        if (sliceRangeSpecified) {
          badParameter = dialog.getSliceIncrLabel();
          tiltParam.setIncrSlice(dialog.getSliceIncr());
        }
        else {
          throw (new InvalidParameterException(
              "You must supply both the first and last slices to specify the slice step."));
        }
      }
      else {
        tiltParam.resetIncrSlice();
      }

      if (dialog.isTomoThicknessSet()) {
        badParameter = dialog.getTomoThicknessLabel();
        tiltParam.setThickness(dialog.getTomoThickness());
      }
      else {
        tiltParam.resetThickness();
      }

      if (dialog.isXAxisTiltSet()) {
        badParameter = dialog.getXAxisTiltLabel();
        tiltParam.setXAxisTilt(dialog.getXAxisTilt());
      }
      else {
        tiltParam.resetXAxisTilt();
      }

      if (dialog.isTiltAngleOffsetSet()) {
        badParameter = dialog.getTiltAngleOffsetLabel();
        tiltParam.setTiltAngleOffset(dialog.getTiltAngleOffset());
      }
      else {
        tiltParam.resetTiltAngleOffset();
      }

      if (dialog.isRadialMaxSet() || dialog.isRadialFallOffSet()) {
        badParameter = dialog.getRadialMaxLabel();
        tiltParam.setRadialBandwidth(dialog.getRadialMax());
        badParameter = dialog.getRadialFallOffLabel();
        tiltParam.setRadialFalloff(dialog.getRadialFallOff());
      }
      else {
        tiltParam.resetRadialFilter();
      }

      if (dialog.isDensityOffsetSet() || dialog.isDensityScaleSet()) {
        badParameter = dialog.getDensityScaleLabel();
        tiltParam.setScaleCoeff(dialog.getDensityScale());
        badParameter = dialog.getDensityOffsetLabel();
        tiltParam.setScaleFLevel(dialog.getDensityOffset());
      }
      else {
        tiltParam.resetScale();
      }

      if (dialog.isLogOffsetSet()) {
        badParameter = dialog.getLogOffsetLabel();
        tiltParam.setLogShift(dialog.getLogOffset());
      }
      else {
        tiltParam.setLogShift(Float.NaN);
      }

      if (dialog.isUseLocalAlignment() && dialog.isUseLocalAlignmentEnabled()) {
        tiltParam.setLocalAlignFile(metaData.getDatasetName()
            + axisID.getExtension() + "local.xf");
      }
      else {
        tiltParam.setLocalAlignFile("");
      }
      metaData.setUseLocalAlignments(axisID, dialog.isUseLocalAlignment());
      tiltParam.setFiducialess(dialog.isFiducialess());
      tiltParam.setUseZFactors(dialog.isUseZFactors()
          && dialog.isUseZFactorsEnabled());
      metaData.setUseZFactors(axisID, dialog.isUseZFactors());
      tiltParam.setExcludeList2(dialog.getExtraExcludeList());
      badParameter = TiltParam.SUBSETSTART_KEY;
      if (metaData.getViewType() == ViewType.MONTAGE) {
        tiltParam.setMontageSubsetStart();
      }
      else {
        tiltParam.setSubsetStart();
      }
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
    catch (IOException e) {
      e.printStackTrace();
      throw new IOException(badParameter + ":  " + e.getMessage());
    }
  }

  private void getMTFFilterParam(MTFFilterParam mtfFilterParam)
      throws FortranInputSyntaxException {
    if (dialog == null) {
      return;
    }
    mtfFilterParam.setLowPassRadiusSigma(dialog.getLowPassRadiusSigma());
    mtfFilterParam.setStartingAndEndingZ(dialog.getStartingAndEndingZ());
    mtfFilterParam.setMtfFile(dialog.getMtfFile());
    mtfFilterParam.setMaximumInverse(dialog.getMaximumInverse());
    mtfFilterParam.setInverseRolloffRadiusSigma(dialog
        .getInverseRolloffRadiusSigma());
  }

  private void setMTFFilterParam(ConstMTFFilterParam mtfFilterParam) {
    if (dialog == null) {
      return;
    }
    dialog.setMtfFile(mtfFilterParam.getMtfFile());
    dialog.setMaximumInverse(mtfFilterParam.getMaximumInverseString());
    dialog.setLowPassRadiusSigma(mtfFilterParam.getLowPassRadiusSigmaString());
    dialog.setStartingAndEndingZ(mtfFilterParam.getStartingAndEndingZString());
    dialog.setInverseRolloffRadiusSigma(mtfFilterParam
        .getInverseRolloffRadiusSigmaString());
    enableUseFilter();
  }

  void trialAction(ProcessResultDisplay trial) {
    if (dialog == null) {
      return;
    }
    String trialTomogramName = dialog.getTrialTomogramName();
    if (trialTomogramName == "") {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Missing trial tomogram filename:";
      errorMessage[1] = "A filename for the trial tomogram must be entered in the Trial"
          + " tomogram filename edit box.";
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID);
      return;
    }
    if (!trialTomogramList.contains(trialTomogramName)) {
      trialTomogramList.add(trialTomogramName);
      dialog.addToTrialTomogramName(trialTomogramName);
    }
    if (dialog.isParallelProcess()) {
      splittilt(true, trial);
    }
    else {
      trialTilt(trial);
    }
  }

  void tiltAction(ProcessResultDisplay tilt) {
    if (dialog == null) {
      return;
    }
    if (dialog.isParallelProcess()) {
      splittilt(tilt);
    }
    else {
      tilt(tilt);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.14  2007/08/16 16:36:37  sueh
 * <p> bug# 1035 Added ltfSizeToOutputInXandY updating in newst.  Converting to
 * <p> startingAndEndingX and Y in blend.  Calculating SUBSETSTART in tilt.
 * <p>
 * <p> Revision 1.13  2007/07/17 21:45:21  sueh
 * <p> bug# 1018 Getting cpu.adoc information from CpuAdoc.
 * <p>
 * <p> Revision 1.12  2007/05/18 23:54:20  sueh
 * <p> bug# 987 Made CpuAdoc thread-safe.  Added minNice.
 * <p>
 * <p> Revision 1.11  2007/03/21 19:47:27  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.10  2007/02/05 23:45:46  sueh
 * <p> bug# 962 Moved comscript mode info to inner class.
 * <p>
 * <p> Revision 1.9  2006/12/02 04:59:21  sueh
 * <p> bug# 944 Added get/setProcessName ProcesschunksParam so the process
 * <p> being run can be identified.
 * <p>
 * <p> Revision 1.8  2006/11/15 21:35:49  sueh
 * <p> bug# 872 Changed saveIntermediateParamFile to saveStorables.
 * <p>
 * <p> Revision 1.7  2006/09/19 22:38:06  sueh
 * <p> bug# 920 Refreshing meta data values in TiltParam each time tilt.com is loaded.
 * <p>
 * <p> Revision 1.6  2006/09/14 00:00:47  sueh
 * <p> bug# 920 Rename X offset and Z offset to X shift and Z shift.
 * <p>
 * <p> Revision 1.5  2006/08/03 21:36:24  sueh
 * <p> bug# Added reconnectTilt().
 * <p>
 * <p> Revision 1.4  2006/07/31 21:46:10  sueh
 * <p> Removed unnecessary print
 * <p>
 * <p> Revision 1.3  2006/07/28 21:27:42  sueh
 * <p> bug# 868 Moved complex button actions from dialog to expert
 * <p>
 * <p> Revision 1.2  2006/07/28 20:14:12  sueh
 * <p> bug# 868 Adding sets and gets to dialog, moving functionality to expert
 * <p>
 * <p> Revision 1.1  2006/07/26 16:41:52  sueh
 * <p> bug# 868 Moved functions associated with TomogramGenerationDialog from
 * <p> ApplicationManager to TomogramGenerationExpert.
 * <p> </p>
 */
