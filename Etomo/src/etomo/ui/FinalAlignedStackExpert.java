package etomo.ui;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.ProcessSeries;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstMTFFilterParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.NewstParam;
import etomo.process.ImodManager;
import etomo.process.ProcessState;
import etomo.storage.CpuAdoc;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstMetaData;
import etomo.type.ConstProcessSeries;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessResult;
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
* <p>Copyright: Copyright 2008</p>
*
* <p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$ </p>
*/
public final class FinalAlignedStackExpert extends ReconUIExpert{
  public static  final String  rcsid =  "$Id$";
  
  private final ComScriptManager comScriptMgr;
  private final TomogramState state;
  private final ReconScreenState screenState;

  private FinalAlignedStackDialog dialog = null;
  private boolean advanced = false;
  private boolean enableFiltering = false;
  private boolean getBinningFromNewst = true;

  public FinalAlignedStackExpert(ApplicationManager manager,
      MainTomogramPanel mainPanel, ProcessTrack processTrack, AxisID axisID) {
    super(manager, mainPanel, processTrack, axisID,
        DialogType.FINAL_ALIGNED_STACK);
    comScriptMgr = manager.getComScriptManager();
    state = manager.getState();
    screenState = manager.getScreenState(axisID);
  }

  /**
   * Open the final aligned stack dialog
   */
  public void openDialog() {
    if (!canShowDialog()) {
      return;
    }
    if (showDialog(dialog)) {
      return;
    }
    //Create the dialog and show it.
    Utilities.timestamp("new", "FinalAlignedStackDialog",
        Utilities.STARTED_STATUS);
    dialog =  FinalAlignedStackDialog.getInstance(manager, this, axisID);
    Utilities.timestamp("new", "FinalAlignedStackDialog",
        Utilities.FINISHED_STATUS);
    // no longer managing image size

    // Read in the newst{|a|b}.com parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions - from when full alignment and tilt where
    // on the same dialog
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
    comScriptMgr.loadMTFFilter(axisID);
    setMTFFilterParam(comScriptMgr.getMTFFilterParam(axisID));
    //updateDialog()
    updateFilter(Utilities.fileExists(manager, ".ali", axisID));

    // Set the fidcialess state and tilt axis angle
    dialog.setFiducialessAlignment(metaData.isFiducialessAlignment(axisID));
    dialog.setImageRotation(metaData.getImageRotation(axisID));
    openDialog(dialog);
  }

  public void updateDialog() {
    updateFilter(Utilities.fileExists(manager, ".ali", axisID));
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  public void startNextProcess(String nextProcess,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, DialogType dialogType) {
  }

  boolean doneDialog() {
    if (dialog == null) {
      return false;
    }
    DialogExitState exitState = dialog.getExitState();
    if (exitState == DialogExitState.EXECUTE) {
      manager.closeImod(ImodManager.MTF_FILTER_KEY, axisID, "filtered stack");
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
   * Update the newst.com from the FinalAlignedStackDialog.  Reads metaData.
   * 
   * @param axisID
   * @return true if successful
   */
  private ConstNewstParam updateNewstCom() {
    // Set a reference to the correct object
    if (dialog == null) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "Can not update newst?.com without an active final aligned stack dialog",
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
   * Update the mtffilter.com from the FinalAlignedStackDialog
   * 
   * @param axisID
   * @return true if successful
   */
  private boolean updateMTFFilterCom() {
    // Set a reference to the correct object
    if (dialog == null) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "Can not update mtffilter?.com without an active final aligned stack dialog",
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
  public void newst(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    if (dialog == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
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
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.newst(axisID, processResultDisplay, processSeries,
        newstParam, blendmontParam), processResultDisplay);
  }

  /**
   */
  void mtffilter(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
    }
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    if (!updateMTFFilterCom()) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.mtffilter(axisID, processResultDisplay, processSeries),
        processResultDisplay);
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
    CpuAdoc cpuAdoc = CpuAdoc.getInstance(AxisID.ONLY, manager.getPropertyUserDir());
    //Parallel processing is optional in tomogram reconstruction, so only use it
    //if the user set it up.
    boolean validAutodoc = cpuAdoc.isAvailable();
    dialog.setParallelProcessEnabled(validAutodoc);
      dialog.setParallelProcess(validAutodoc);
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
    dialog.setNewstHeaderState(screenState.getFinalNewstHeaderState());
    dialog.setFilterHeaderState(screenState.getFinalMtffilterHeaderState());
    dialog.setAdvanced();
    dialog.setNewstButtonState(screenState);
    dialog.setUseFilterButtonState(screenState);
    dialog.setFilterButtonState(screenState);
  }

  private void getParameters(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.getNewstHeaderState(screenState.getFinalNewstHeaderState());
    dialog.getFilterHeaderState(screenState.getFinalMtffilterHeaderState());
  }

  private void getParameters(MetaData metaData)
      throws FortranInputSyntaxException {
    if (dialog == null) {
      return;
    }
    try {
      metaData.setSizeToOutputInXandY(axisID, dialog.getSizeToOutputInXandY());
    }
    catch (FortranInputSyntaxException e) {
      e.printStackTrace();
      throw new FortranInputSyntaxException(
          FinalAlignedStackDialog.SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + ":  "
              + e.getMessage());
    }
  }

  private void setBlendParams(BlendmontParam blendmontParam) {
    dialog.setUseLinearInterpolation(blendmontParam.isLinearInterpolation());
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
          FinalAlignedStackDialog.SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + ":  "
              + e.getMessage());
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
}
