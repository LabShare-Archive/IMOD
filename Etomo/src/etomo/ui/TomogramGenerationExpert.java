package etomo.ui;

import java.io.IOException;

import etomo.ApplicationManager;
import etomo.ProcessSeries;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SplittiltParam;
import etomo.comscript.TiltParam;
import etomo.process.ImodManager;
import etomo.process.ProcessState;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.ConstProcessSeries;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.IntKeyList;
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
  private boolean getBinningFromNewst = true;
  //A way to know what items are currently in the trial tomogram combo box.
  //It is set from MetaData, which is assumed to be not null.
  private IntKeyList trialTomogramList = null;

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
    if (!canShowDialog()) {
      return;
    }
    if (showDialog(dialog)) {
      return;
    }
    //Create the dialog and show it.
    Utilities.timestamp("new", "TomogramGenerationDialog",
        Utilities.STARTED_STATUS);
    dialog = new TomogramGenerationDialog(manager, this, axisID);
    Utilities.timestamp("new", "TomogramGenerationDialog",
        Utilities.FINISHED_STATUS);
    // no longer managing image size
    setParameters(metaData);
    setParameters(screenState);
    // Read in the tilt{|a|b}.com parameters and display the dialog panel
    comScriptMgr.loadTilt(axisID);
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    tiltParam.setFiducialess(metaData.isFiducialess(axisID));
    // If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      // upgrade and save param to comscript
      UIExpertUtilities.INSTANCE.upgradeOldTiltCom(manager, axisID, tiltParam);
    }
    setTiltParams(tiltParam);
    // Set the fidcialess state and tilt axis angle
    setEnabledTiltParameters();
    openDialog(dialog);
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  public void startNextProcess(String nextProcess,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, DialogType dialogType) {
    if (nextProcess.equals(ProcessName.PROCESSCHUNKS.toString())) {
      processchunks(manager, dialog, processResultDisplay, processSeries,
          ProcessName.TILT);
    }
  }

  public boolean reconnectTilt(ProcessName processName) {
    ProcessResultDisplay display = manager.getProcessResultDisplayFactory(
        axisID).getGenerateTomogram();
    sendMsgProcessStarting(display);
    return manager.reconnectTilt(axisID, processName, display);
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
      newstFiducialessAlignment = metaData.isFiducialessAlignment(axisID);
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
    //Hold onto the finished dialog in case anything is running that needs it or
    //there are next processes that need it.
    return true;
  }

  boolean saveDialog() {
    if (dialog == null) {
      return false;
    }
    advanced = dialog.isAdvanced();
    // Get the user input data from the dialog box
    try {
      getParameters(metaData);
    }
    catch (FortranInputSyntaxException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Data File Error", manager.getManagerKey());
    }
    getParameters(screenState);
    if (updateTiltCom(true) == null) {
      return false;
    }
    manager.saveStorables(axisID);
    return true;
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
  private ConstTiltParam updateTiltCom(boolean useDefaultRec) {
    // Set a reference to the correct object
    if (dialog == null) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "Can not update tilt?.com without an active tomogram generation dialog",
              "Program logic error", axisID, manager.getManagerKey());
      return null;
    }
    TiltParam tiltParam = null;
    try {
      tiltParam = comScriptMgr.getTiltParam(axisID);
      tiltParam.setFiducialess(metaData.isFiducialess(axisID));
      if (!getTiltParams(tiltParam)) {
        return null;
      }
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
      metaData.setFiducialess(axisID, tiltParam.isFiducialess());
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, manager.getManagerKey());
      return null;
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, manager.getManagerKey());
      return null;
    }
    catch (IOException e) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = e.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Tilt Parameter",
          axisID, manager.getManagerKey());
      return null;
    }
    return tiltParam;
  }

  protected ProcessDialog getDialog() {
    return dialog;
  }

  /**
   * Start a tilt process in trial mode
   * 
   * @param axisID
   */
  void trialTilt(ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstTiltParam param = updateTiltCom(false);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.tiltProcess(axisID, processResultDisplay, processSeries,
        param), processResultDisplay);
  }

  /**
   * Run the tilt command script for the specified axis
   */
  public void tilt(ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstTiltParam param = updateTiltCom(true);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.tiltProcess(axisID, processResultDisplay, processSeries,
        param), processResultDisplay);
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

  void splittilt(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    splittilt(false, processResultDisplay, processSeries);
  }

  void splittilt(boolean trialMode, ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    if (updateTiltCom(!trialMode) == null) {
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
        processResultDisplay, processSeries, param, dialogType);
    if (processResult == null) {
      processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString());
    }
    sendMsg(processResult, processResultDisplay);
  }

  private final SplittiltParam updateSplittiltParam() {
    if (dialog == null) {
      return null;
    }
    SplittiltParam param = new SplittiltParam(axisID);
    if (!getParameters(param)) {
      return null;
    }
    param.setSeparateChunks(CpuAdoc.getInstance(axisID,
        manager.getPropertyUserDir(), manager.getManagerKey()).isSeparateChunks());
    return param;
  }

  boolean getParameters(final SplittiltParam param) {
    ConstEtomoNumber numMachines = param.setNumMachines(getParallelPanel()
        .getCPUsSelected());
    if (!numMachines.isValid()) {
      UIHarness.INSTANCE.openMessageDialog(getParallelPanel()
          .getCPUsSelectedLabel()
          + " " + numMachines.getInvalidReason(), "Unable to run splittilt",
          axisID, manager.getManagerKey());
      return false;
    }
    return true;
  }

  private void setParameters(ConstMetaData metaData) {
    if (dialog == null) {
      return;
    }
    trialTomogramList = metaData.getTomoGenTrialTomogramNameList(axisID);
    dialog.setTrialTomogramNameList(trialTomogramList);
    CpuAdoc cpuAdoc = CpuAdoc.getInstance(AxisID.ONLY, manager
        .getPropertyUserDir(), manager.getManagerKey());
    //Parallel processing is optional in tomogram reconstruction, so only use it
    //if the user set it up.
    boolean validAutodoc = cpuAdoc.isAvailable();
    dialog.setParallelProcessEnabled(validAutodoc);
    ConstEtomoNumber tomoGenTiltParallel = metaData
        .getTomoGenTiltParallel(axisID);
    if (tomoGenTiltParallel == null) {
      dialog.setParallelProcess(validAutodoc
          && metaData.getDefaultParallel().is());
    }
    else {
      dialog.setParallelProcess(validAutodoc && tomoGenTiltParallel.is());
    }
    updateParallelProcess();
  }

  void updateParallelProcess() {
    manager.setParallelDialog(axisID, dialog);
  }

  private final void setParameters(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.setTiltHeaderState(screenState.getTomoGenTiltHeaderState());
    dialog.setTrialHeaderState(screenState.getTomoGenTrialTiltHeaderState());
    dialog.setAdvanced();
    dialog.setTiltButtonState(screenState);
    dialog.setDeleteStackButtonState(screenState);
    dialog.setUseTrialButtonState(screenState);
  }

  private void getParameters(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.getTiltHeaderState(screenState.getTomoGenTiltHeaderState());
    dialog.getTrialHeaderState(screenState.getTomoGenTrialTiltHeaderState());
  }

  private void getParameters(MetaData metaData)
      throws FortranInputSyntaxException {
    if (dialog == null) {
      return;
    }
    metaData.setTomoGenTrialTomogramNameList(axisID, trialTomogramList);
    metaData.setTomoGenTiltParallel(axisID, dialog.isParallelProcess());
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

  /**
   * Get the tilt parameters from the requested axis panel
   */
  private boolean getTiltParams(TiltParam tiltParam)
      throws NumberFormatException, InvalidParameterException, IOException {
    if (dialog == null) {
      return false;
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
      //TiltParam.fiducialess is based on whether final alignment was run
      //fiducialess.
      // newstFiducialessAlignment
      boolean newstFiducialessAlignment = false;
      if (!state.getNewstFiducialessAlignment(axisID).isNull()) {
        newstFiducialessAlignment = state.getNewstFiducialessAlignment(axisID)
            .is();
      }
      else {
        newstFiducialessAlignment = metaData.isFiducialessAlignment(axisID);
      }
      tiltParam.setFiducialess(newstFiducialessAlignment);

      tiltParam.setUseZFactors(dialog.isUseZFactors()
          && dialog.isUseZFactorsEnabled());
      metaData.setUseZFactors(axisID, dialog.isUseZFactors());
      tiltParam.setExcludeList2(dialog.getExtraExcludeList());
      badParameter = TiltParam.SUBSETSTART_KEY;
      if (metaData.getViewType() == ViewType.MONTAGE) {
        tiltParam.setMontageSubsetStart();
      }
      else if (!tiltParam.setSubsetStart()) {
        return false;
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
    return true;
  }

  void trialAction(ProcessResultDisplay trial, ProcessSeries processSeries) {
    if (dialog == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
    }
    String trialTomogramName = dialog.getTrialTomogramName();
    if (trialTomogramName == "") {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Missing trial tomogram filename:";
      errorMessage[1] = "A filename for the trial tomogram must be entered in the Trial"
          + " tomogram filename edit box.";
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, manager.getManagerKey());
      return;
    }
    if (!trialTomogramList.containsValue(trialTomogramName)) {
      trialTomogramList.add(trialTomogramName);
      dialog.addToTrialTomogramName(trialTomogramName);
    }
    if (dialog.isParallelProcess()) {
      splittilt(true, trial, processSeries);
    }
    else {
      trialTilt(trial, processSeries);
    }
  }

  void tiltAction(ProcessResultDisplay tilt, ProcessSeries processSeries,
      Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    if (dialog == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    if (dialog.isParallelProcess()) {
      splittilt(tilt, processSeries);
    }
    else {
      tilt(tilt, processSeries);
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.29  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.28  2009/02/13 02:37:54  sueh
 * <p> bug# 1148 In doneDialog no longer setting trialTomogramList to null
 * <p> because this causes a null pointer exception on close.
 * <p>
 * <p> Revision 1.27  2009/02/05 23:45:52  sueh
 * <p> bug# 1148 Setting and getting trialTomogramList from metadata.
 * <p>
 * <p> Revision 1.26  2008/11/11 23:54:47  sueh
 * <p> bug# 1149 Removed getBinning(), which was used to save binning to
 * <p> MetaData.tomoGenBinning.  TomoGenBinning is now finalStackBinning
 * <p> and comes from the FinalStack dialog.  Is was always supposed to be saved from the binning in the Align box.
 * <p>
 * <p> Revision 1.25  2008/10/27 20:45:10  sueh
 * <p> bug# 1141 Added getParameters(SplittiltParam).
 * <p>
 * <p> Revision 1.24  2008/10/16 22:32:35  sueh
 * <p> bug# 1141 Created FinalAlignedStack dialog to run full aligned stack and mtf filter.
 * <p>
 * <p> Revision 1.23  2008/07/19 01:12:45  sueh
 * <p> bug# 1125 Making it easier to access CpuAdoc by not passing the
 * <p> manager to it; all it needs is the current directory.
 * <p>
 * <p> Revision 1.22  2008/07/15 17:48:06  sueh
 * <p> bug# 1124 In getTiltParams(TiltParam) corrected the setting of
 * <p> TiltParam.fiducialess.  Basing it on the last time final alignment was run.
 * <p>
 * <p> Revision 1.21  2008/05/28 02:51:46  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 1.20  2008/05/13 23:07:58  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 1.19  2008/05/03 00:57:31  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.18  2008/02/01 01:37:31  sueh
 * <p> bug# 1075 Handling header failure in setSubsetStart.
 * <p>
 * <p> Revision 1.17  2008/01/25 22:29:34  sueh
 * <p> bug# 1070 Don't use parallel processing unless the cpu.adoc or
 * <p> IMOD_PROCESSORS has been set by the user.
 * <p>
 * <p> Revision 1.16  2007/12/13 01:14:00  sueh
 * <p> bug# 1056 Removed the Storables inner class from TiltParam.
 * <p>
 * <p> Revision 1.15  2007/12/10 22:49:29  sueh
 * <p> bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * <p> getParameters because it is required and has been added to the
 * <p> ProcesschunksParam constructor.  Removed getParameters
 * <p> ProcesschunksParam) because it is empty.
 * <p>
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
