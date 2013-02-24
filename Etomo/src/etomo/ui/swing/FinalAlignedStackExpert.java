package etomo.ui.swing;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.ProcessSeries;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstCtfPhaseFlipParam;
import etomo.comscript.ConstCtfPlotterParam;
import etomo.comscript.ConstMTFFilterParam;
import etomo.comscript.ConstSplitCorrectionParam;
import etomo.comscript.CtfPhaseFlipParam;
import etomo.comscript.CtfPlotterParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.SplitCorrectionParam;
import etomo.process.ImodManager;
import etomo.process.ProcessState;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.ProcessName;
import etomo.type.ProcessResult;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessTrack;
import etomo.type.ProcessingMethod;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.type.ViewType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.DatasetFiles;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
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
 * <p> $Log$
 * <p> Revision 1.7  2011/05/05 20:24:21  sueh
 * <p> bug# 1481 In startNextProcess correcting the root name passed to processchunks.
 * <p>
 * <p> Revision 1.6  2011/04/04 17:20:33  sueh
 * <p> bug# 1416 Passing process name as a string to processchunks.
 * <p>
 * <p> Revision 1.5  2011/03/02 00:00:12  sueh
 * <p> bug# 1452 Removing image rotation conversion between float and
 * <p> double.  Using string where possible.
 * <p>
 * <p> Revision 1.4  2011/02/21 19:51:19  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.3  2011/02/03 06:22:16  sueh
 * <p> bug# 1422 Control of the processing method has been centralized in the
 * <p> processing method mediator class.  Implementing ProcessInterface.
 * <p> Supplying processes with the current processing method.
 * <p>
 * <p> Revision 1.2  2010/12/05 05:04:39  sueh
 * <p> bug# 1416 Changed setEnabledTiltParameters to setTiltState.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.25  2010/04/28 16:40:35  sueh
 * <p> bug# 1344 Removed the manager from the some of the
 * <p> openMessageDialog calls so that they won't go into the project log.
 * <p> Updated calls to closeImod.
 * <p>
 * <p> Revision 1.24  2010/03/19 02:40:31  sueh
 * <p> bug# 1325 In openDialog loading TiltalignParam and setting parameters.
 * <p>
 * <p> Revision 1.23  2010/03/12 04:15:25  sueh
 * <p> bug# 1325 In doneDialog warning the user when a use button wasn't pressed.
 * <p>
 * <p> Revision 1.22  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.21  2009/10/19 16:29:04  sueh
 * <p> bug# 1253 Added invertTiltAngles.
 * <p>
 * <p> Revision 1.20  2009/10/13 17:40:25  sueh
 * <p> bug# 1273 Formatted.
 * <p>
 * <p> Revision 1.19  2009/09/22 23:55:24  sueh
 * <p> bug# 1269 Added setEnabledTiltParameters and called it from openDialog.
 * <p>
 * <p> Revision 1.18  2009/09/02 22:45:48  sueh
 * <p> bug# 1254 Checking for a valid stack before using the stack.
 * <p>
 * <p> Revision 1.17  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.16  2009/06/16 22:54:30  sueh
 * <p> bug# 1221 Moved some newst, blendmont, and ccderaser functionality
 * <p> into process panels to avoid factoring into panels to happen.
 * <p>
 * <p> Revision 1.15  2009/06/15 20:23:25  sueh
 * <p> bug# 1221 Reformatted.
 * <p>
 * <p> Revision 1.14  2009/06/12 19:49:24  sueh
 * <p> bug# 1221 Factored running newst, making it independent of the
 * <p> final aligned dialog and expert.
 * <p>
 * <p> Revision 1.13  2009/06/10 22:17:04  sueh
 * <p> bug# 1221 Factoring Newstack and blendmont into NewstackPanel.
 * <p>
 * <p> Revision 1.12  2009/03/17 00:46:23  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.11  2009/02/13 02:32:50  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.
 * <p>
 * <p> Revision 1.10  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.9  2009/01/26 22:43:32  sueh
 * <p> bug# 1173 Saved current tab from dialog.  Added new line to simple
 * <p> defocus file.
 * <p>
 * <p> Revision 1.8  2008/12/15 23:03:30  sueh
 * <p> bug# 1161 Added parameters imageRotation and manager to
 * <p> NewstParam.setSizeToOutputInXandY.
 * <p>
 * <p> Revision 1.7  2008/12/09 21:08:06  sueh
 * <p> bug# 1154 In getParameters(CtfPhaseFlipParam) setting OutputFileName.
 * <p>
 * <p> Revision 1.6  2008/12/02 21:22:55  sueh
 * <p> bug# 1157 Changed better radius to fiducial diameter.  Getting fiducial
 * <p> diameter from better radius saved in the .edf file for backwards
 * <p> compatability.
 * <p>
 * <p> Revision 1.5  2008/11/20 01:45:29  sueh
 * <p> bug# 1147 Added functions to run xfmodel and ccderaser.  Bug# 1147
 * <p> Getting binning only from meta data, not from newst, which is overwritten
 * <p> by Tomogram Positioning - Whole Tomogram.
 * <p>
 * <p> Revision 1.4  2008/11/11 23:52:56  sueh
 * <p> bug# 1149 Always getting binning from metadata.finalStackBinning (was
 * <p> tomoGenBinning).  Saving binning to metadata.finalStackBinning.
 * <p>
 * <p> Revision 1.3  2008/10/30 20:22:50  sueh
 * <p> bug# 1141 Offer to close ctf correction on done.
 * <p>
 * <p> Revision 1.2  2008/10/27 20:39:57  sueh
 * <p> bug# 1141 Added CTF Correction.  Corrected problem where Newst was
 * <p> getting its binning from whole tomogram positioning.
 * <p>
 * <p> Revision 1.1  2008/10/16 21:24:35  sueh
 * <p> bug# 1141 Dialog for running newst (full align) and filtering
 * <p> </p>
 */
public final class FinalAlignedStackExpert extends ReconUIExpert {
  public static final String rcsid = "$Id$";

  private final ComScriptManager comScriptMgr;
  private final TomogramState state;
  private final ReconScreenState screenState;

  private FinalAlignedStackDialog dialog = null;
  private boolean advanced = false;
  private boolean enableFiltering = false;
  private FinalAlignedStackDialog.Tab curTab = FinalAlignedStackDialog.Tab.DEFAULT;

  public FinalAlignedStackExpert(ApplicationManager manager, MainTomogramPanel mainPanel,
      ProcessTrack processTrack, AxisID axisID) {
    super(manager, mainPanel, processTrack, axisID, DialogType.FINAL_ALIGNED_STACK);
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
    String actionMessage = manager.setCurrentDialogType(dialogType, axisID);
    if (showDialog(dialog, actionMessage)) {
      return;
    }
    // Create the dialog and show it.
    Utilities.timestamp("new", "FinalAlignedStackDialog", Utilities.STARTED_STATUS);
    dialog = FinalAlignedStackDialog.getInstance(manager, this, axisID, curTab);
    Utilities.timestamp("new", "FinalAlignedStackDialog", Utilities.FINISHED_STATUS);

    dialog.initialize();
    dialog.setParameters(metaData);// TEMP metadata

    // no longer managing image size

    // Read in the newst{|a|b}.com parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions - from when full alignment and tilt where
    // on the same dialog
    if (metaData.getViewType() == ViewType.MONTAGE) {
      comScriptMgr.loadBlend(axisID);
      dialog.setParameters(comScriptMgr.getBlendParam(axisID));
      // if blend_3dfind.com doesn't exist copy blend.com to blend_3dfind.com.
      File blend3dFindCom = new File(manager.getPropertyUserDir(),
          ProcessName.BLEND_3D_FIND.getComscript(axisID));
      try {
        if (!blend3dFindCom.exists()) {
          Utilities.copyFile(
              new File(manager.getPropertyUserDir(), ProcessName.BLEND
                  .getComscript(axisID)), blend3dFindCom);
        }
        comScriptMgr.loadBlend3dFind(axisID);
        dialog.setEraseGoldParameters(comScriptMgr.getBlendParamFromBlend3dFind(axisID));
      }
      catch (IOException e) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Unable to copy to blend.com to blend_3d_find.com.  Will not be "
                + "able to use findbeads3d when erasing gold.", "Etomo Error");
      }
    }
    else {
      comScriptMgr.loadNewst(axisID);
      dialog.setParameters(comScriptMgr.getNewstComNewstParam(axisID));// TEMP newstparam
      // if newst_3dfind.com doesn't exist copy newst.com to newst_3dfind.com.
      File newst3dFindCom = new File(manager.getPropertyUserDir(),
          ProcessName.NEWST_3D_FIND.getComscript(axisID));
      try {
        if (!newst3dFindCom.exists()) {
          Utilities.copyFile(
              new File(manager.getPropertyUserDir(), ProcessName.NEWST
                  .getComscript(axisID)), newst3dFindCom);
        }
        comScriptMgr.loadNewst3dFind(axisID);
        dialog.setEraseGoldParameters(comScriptMgr.getNewstParamFromNewst3dFind(axisID));
      }
      catch (IOException e) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Unable to copy to newst.com to newst_3d_find.com.  Will not be "
                + "able to use findbeads3d when erasing gold.", "Etomo Error");
      }
    }
    // if tilt_3dfind.com doesn't exist copy tilt.com to tilt_3dfind.com.
    File tilt3dFindCom = new File(manager.getPropertyUserDir(),
        ProcessName.TILT_3D_FIND.getComscript(axisID));
    boolean newTilt3dFindCom = false;
    try {
      if (!tilt3dFindCom.exists()) {
        newTilt3dFindCom = true;
        Utilities
            .copyFile(
                new File(manager.getPropertyUserDir(), ProcessName.TILT
                    .getComscript(axisID)), tilt3dFindCom);
      }
      comScriptMgr.loadTilt3dFind(axisID);
      dialog.setParameters(comScriptMgr.getTiltParamFromTilt3dFind(axisID),
          newTilt3dFindCom);
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to copy to blend.com to blend_3d_find.com.  Will not be "
              + "able to use findbeads3d when erasing gold.", "Etomo Error");
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to copy to blend.com to blend_3d_find.com.  Will not be "
              + "able to use findbeads3d when erasing gold.", "Etomo Error");
    }
    // Backwards compatibility
    // If track light beads isn't be saved to state, get light beads from
    // track.com.
    comScriptMgr.loadTrack(axisID);
    comScriptMgr.loadFindBeads3d(axisID);
    dialog.setParameters(comScriptMgr.getFindBeads3dParam(axisID), newTilt3dFindCom);
    comScriptMgr.loadTilt3dFindReproject(axisID);

    // Get the align{|a|b}.com parameters
    comScriptMgr.loadAlign(axisID);
    dialog.setParameters(comScriptMgr.getTiltalignParam(axisID), newTilt3dFindCom);

    // backward compatibility
    // Try loading ctfcorrection.com first. If it isn't there, copy it with
    // copytomocoms and load it.
    // Then try loading ctfplotter.com. If it isn't there, copy it with
    // copytomocoms, using the voltage, etc from ctfcorrection.com.
    // Ignore ctfplotter.param.
    if (!comScriptMgr.loadCtfCorrection(axisID, false)) {
      manager.setupCtfCorrectionComScript(axisID);
      comScriptMgr.loadCtfCorrection(axisID, true);
    }
    CtfPhaseFlipParam ctfPhaseFlipParam = comScriptMgr.getCtfPhaseFlipParam(axisID);
    setParameters(ctfPhaseFlipParam);
    if (!comScriptMgr.loadCtfPlotter(axisID, false)) {
      // Get the voltage, etc from ctfcorrection.com, since it has been loaded, and it
      // is somewhat more likely that it was updated
      manager.setupCtfPlotterComScript(axisID, ctfPhaseFlipParam);
      comScriptMgr.loadCtfPlotter(axisID, true);
    }
    CtfPlotterParam param = comScriptMgr.getCtfPlotterParam(axisID);
    if (metaData.isStackCtfAutoFitRangeAndStepSet(axisID)) {
      param.setAutoFitRangeAndStep(metaData.getStackCtfAutoFitRangeAndStep(axisID));
    }
    setParameters(param);
    comScriptMgr.saveCtfPlotter(param, axisID);
    dialog.setParameters(screenState);
    comScriptMgr.loadMTFFilter(axisID);
    setParameters(comScriptMgr.getMTFFilterParam(axisID));
    // updateDialog()
    updateFilter(Utilities.fileExists(manager, ".ali", axisID));

    // Set the fidcialess state and tilt axis angle
    // From updateFiducialessParams
    dialog.setFiducialessAlignment(metaData.isFiducialessAlignment(axisID));
    dialog.setImageRotation(metaData.getImageRotation(axisID).toString());
    dialog.setTiltState(state, metaData);
    dialog.setOverrideParameters(metaData);
    openDialog(dialog, actionMessage);
  }

  public void updateDialog() {
    updateFilter(Utilities.fileExists(manager, ".ali", axisID));
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  public boolean startNextProcess(ProcessSeries.Process process,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      DialogType dialogType, ProcessDisplay display) {
    if (process.equals(ProcessName.PROCESSCHUNKS.toString())) {
      processchunks(manager, dialog, processResultDisplay, processSeries, process
          .getSubprocessName().toString() + axisID.getExtension(),
          process.getOutputImageFileType(), process.getProcessingMethod(), false);
      return true;
    }
    if (process.equals(ProcessName.TILT_3D_FIND.toString())) {
      manager.tilt3dFindAction(processResultDisplay, processSeries, null, null,
          (TiltDisplay) display, axisID, dialogType, process.getProcessingMethod());
      return true;
    }
    return false;
  }

  void doneDialog() {
    if (dialog == null) {
      return;
    }
    curTab = dialog.getCurTab();
    DialogExitState exitState = dialog.getExitState();
    if (exitState != DialogExitState.CANCEL && !manager.isExiting()
        && exitState != DialogExitState.POSTPONE) {
      if (state.isUseCtfCorrectionWarning(axisID)) {
        // The use button wasn't pressed and the user is moving on to the next
        // dialog. Don't put this message in the log.
        UIHarness.INSTANCE.openMessageDialog(null,
            "To use the CTF correction go back to Final Aligned Stack and press "
                + "the \"" + FinalAlignedStackDialog.USE_CTF_CORRECTION_LABEL
                + "\" button in the " + FinalAlignedStackDialog.CTF_TAB_LABEL + " tab.",
            "Entry Warning", axisID);
        // Only warn once.
        state.setUseCtfCorrectionWarning(axisID, false);
      }
      if (state.isUseErasedStackWarning(axisID)) {
        // The use button wasn't pressed and the user is moving on to the next
        // dialog. Don't put this message in the log.
        UIHarness.INSTANCE.openMessageDialog(
            null,
            "To use the stack with the erased beads go back to Final Aligned "
                + "Stack and press the \""
                + FinalAlignedStackDialog.getUseErasedStackLabel() + "\" button in the "
                + FinalAlignedStackDialog.getErasedStackTabLabel() + " tab.",
            "Entry Warning", axisID);
        // Only warn once.
        state.setUseErasedStackWarning(axisID, false);
      }
      if (state.isUseFilteredStackWarning(axisID)) {
        // The use button wasn't pressed and the user is moving on to the next
        // dialog. Don't put this message in the log.
        UIHarness.INSTANCE.openMessageDialog(null,
            "To use the MTF filtered stack go back to Final Aligned "
                + "Stack and press the \""
                + FinalAlignedStackDialog.USE_FILTERED_STACK_LABEL + "\" button in the "
                + FinalAlignedStackDialog.MTF_FILTER_TAB_LABEL + " tab.",
            "Entry Warning", axisID);
        // Only warn once.
        state.setUseFilteredStackWarning(axisID, false);
      }
    }
    if (exitState == DialogExitState.EXECUTE) {
      manager.closeImod(ImodManager.MTF_FILTER_KEY, axisID, "MTF filtered stack", false);
      manager.closeImod(ImodManager.CTF_CORRECTION_KEY, axisID, "CTF corrected stack",
          false);
      manager.closeImod(ImodManager.ERASED_FIDUCIALS_KEY, axisID, "Erased beads stack",
          false);
      manager.closeImod(ImodManager.FINE_ALIGNED_3D_FIND_KEY, axisID,
          "Aligned stack for 3d find", false);
      manager.closeImod(ImodManager.FULL_VOLUME_3D_FIND_KEY, axisID, "tomogram 3d find",
          false);
    }
    if (exitState != DialogExitState.CANCEL) {
      saveDialog();
    }
    // Clean up the existing dialog
    leaveDialog(exitState);
    // Hold onto the finished dialog (don't set dialog to null) in case anything
    // is running that needs it or there are next processes that need it.
  }

  void saveDialog() {
    if (dialog == null) {
      return;
    }
    advanced = dialog.isAdvanced();
    // Get the user input data from the dialog box
    try {
      dialog.getParameters(metaData);
    }
    catch (FortranInputSyntaxException e) {
      UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(), "Data File Error");
    }
    dialog.getParameters(screenState);
    UIExpertUtilities.INSTANCE.updateFiducialessParams(manager,
        dialog.getFiducialessParams(), axisID, false);
    if (metaData.getViewType() == ViewType.MONTAGE) {
      try {
        manager.updateBlendCom(dialog.getBlendmontDisplay(), axisID, false, false);
        manager.updateBlend3dFindCom(dialog.getBlendmont3dFindDisplay(), axisID, false,
            false);
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(), "Update Com Error");
      }
      catch (InvalidParameterException e) {
        UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(), "Update Com Error");
      }
      catch (IOException e) {
        UIHarness.INSTANCE.openMessageDialog(manager, e.getMessage(), "Update Com Error");
      }
    }
    else {
      manager.updateNewstCom(dialog.getNewstackDisplay(), axisID, false, false);
      manager.updateNewst3dFindCom(dialog.getNewstack3dFindDisplay(), axisID, false,
          false);
    }
    updateMTFFilterCom(false);
    updateCtfPlotterCom(false);
    updateCtfCorrectionCom(false);
    manager.updateTilt3dFindCom(dialog.getTilt3dFindDisplay(), axisID, false);
    manager.updateFindBeads3dCom(dialog.getFindBeads3dDisplay(), axisID, false);
    manager.updateGoldEraserParam(dialog.getCcdEraserBeadsDisplay(), axisID, false);
    manager.saveStorables(axisID);
  }

  private ConstCtfPlotterParam updateCtfPlotterCom(final boolean doValidation) {
    CtfPlotterParam param = comScriptMgr.getCtfPlotterParam(axisID);
    if (!getParameters(param, doValidation)) {
      return null;
    }
    comScriptMgr.saveCtfPlotter(param, axisID);
    return param;
  }

  private ConstCtfPhaseFlipParam updateCtfCorrectionCom(final boolean doValidation) {
    CtfPhaseFlipParam param = comScriptMgr.getCtfPhaseFlipParam(axisID);
    if (!getParameters(param, doValidation)) {
      return null;
    }
    comScriptMgr.saveCtfPhaseFlip(param, axisID);
    return param;
  }

  /**
   * Update the mtffilter.com from the FinalAlignedStackDialog
   * 
   * @param axisID
   * @return true if successful
   */
  private ConstMTFFilterParam updateMTFFilterCom(final boolean doValidation) {
    // Set a reference to the correct object
    if (dialog == null) {
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Can not update mtffilter?.com without an active final aligned stack dialog",
          "Program logic error", axisID);
      return null;
    }
    MTFFilterParam mtfFilterParam = null;
    try {
      mtfFilterParam = comScriptMgr.getMTFFilterParam(axisID);
      if (!getParameters(mtfFilterParam, doValidation)) {
        return null;
      }
      String inputFileName;
      String outputFileName;
      if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
        inputFileName = metaData.getDatasetName() + AxisID.ONLY.getExtension() + ".ali";
        outputFileName = metaData.getDatasetName() + AxisID.ONLY.getExtension()
            + "_filt.ali";
      }
      else {
        inputFileName = metaData.getDatasetName() + axisID.getExtension() + ".ali";
        outputFileName = metaData.getDatasetName() + axisID.getExtension() + "_filt.ali";
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
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage,
          "MTF Filter Parameter Syntax Error", axisID);
      return null;
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "MTF Filter Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(manager, errorMessage,
          "MTF Filter Parameter Syntax Error", axisID);
      return null;
    }
    return mtfFilterParam;
  }

  ProcessDialog getDialog() {
    return dialog;
  }

  /**
   */
  void mtffilter(ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      Deferred3dmodButton deferred3dmodButton, Run3dmodMenuOptions run3dmodMenuOptions) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
    }
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstMTFFilterParam param;
    if ((param = updateMTFFilterCom(true)) == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.mtffilter(param, axisID, processResultDisplay, processSeries),
        processResultDisplay);
  }

  void ctfPlotter(ProcessResultDisplay processResultDisplay) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstCtfPlotterParam param = updateCtfPlotterCom(true);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.ctfPlotter(axisID, processResultDisplay), processResultDisplay);
  }

  /**
   * Create or reuse a _simple.defocus file.
   * Expected format: 1 1 0 0 expectedDefocus.  The first four numbers don't
   * matter.
   * @return
   */
  boolean createSimpleDefocusFile(final boolean doValidation) {
    try {
      boolean updateToDate = false;
      LogFile file = null;
      LogFile.ReaderId readerId = null;
      LogFile.WriterId writerId = null;
      try {
        file = LogFile.getInstance(DatasetFiles.getSimpleDefocusFile(manager, axisID));
        if (file.exists()) {
          readerId = file.openReader();
          String line = file.readLine(readerId);
          if (file.closeRead(readerId)) {
            readerId = null;
          }
          String[] array = line.split("\\s+");
          if (array[4].equals(Utilities.convertMicronsToNanometers(dialog
              .getExpectedDefocus(doValidation)))) {
            updateToDate = true;
          }
          else {
            file.backup();
          }
        }
        if (!updateToDate) {
          writerId = file.openWriter();
          file.write(
              "1 1 0 0 "
                  + Utilities.convertMicronsToNanometers(dialog
                      .getExpectedDefocus(doValidation)), writerId);
          file.newLine(writerId);
          updateToDate = true;
          if (file.closeWriter(writerId)) {
            writerId = null;
          }
        }
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      if (readerId != null && !readerId.isEmpty()) {
        file.closeRead(readerId);
      }
      if (writerId != null && !writerId.isEmpty()) {
        file.closeWriter(writerId);
      }
      if (!updateToDate) {
        if (!UIHarness.INSTANCE.openYesNoDialog(manager,
            DatasetFiles.getSimpleDefocusFileName(manager, axisID)
                + " may not be up to date.  Continue?", axisID)) {
          return false;
        }
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void ctfCorrection(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions,
      final ProcessingMethod correctionProcessingMethod) {
    if (dialog == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    if (dialog.isUseExpectedDefocus() && !createSimpleDefocusFile(true)) {
      return;
    }
    if (!correctionProcessingMethod.isLocal()) {
      splitcorrection(processResultDisplay, processSeries, correctionProcessingMethod);
    }
    else {
      ctfCorrection(processResultDisplay, processSeries);
    }
  }

  void ctfCorrection(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstCtfPhaseFlipParam param = updateCtfCorrectionCom(true);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.ctfCorrection(param, axisID, processResultDisplay, processSeries),
        processResultDisplay);
  }

  private ConstSplitCorrectionParam updateSplitCorrectionParam(final boolean doValidation) {
    if (dialog == null) {
      return null;
    }
    SplitCorrectionParam param = new SplitCorrectionParam(axisID);
    if (!getParameters(param, doValidation)) {
      return null;
    }
    return param;
  }

  boolean getParameters(final SplitCorrectionParam param, final boolean doValidation) {
    try {
      param.setCpus(getParallelPanel().getCPUsSelected(doValidation));
      MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
          DatasetFiles.getFullAlignedStackFileName(manager, axisID), axisID);
      try {
        if (header.read(manager)) {
          param.setMaxZ(header.getNSections());
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      catch (InvalidParameterException e) {
        e.printStackTrace();
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void splitcorrection(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, final ProcessingMethod correctionProcessingMethod) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    if (updateCtfCorrectionCom(true) == null) {
      return;
    }
    ConstSplitCorrectionParam param = updateSplitCorrectionParam(true);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    ProcessResult processResult = manager.splitCorrection(axisID, processResultDisplay,
        processSeries, param, dialogType, correctionProcessingMethod);
    if (processResult != null) {
      sendMsg(processResult, processResultDisplay);
    }
  }

  /**
   * Replace the full aligned stack with the ctf corrected full aligned stack
   * created by ctfcorrection.com
   */
  void useCtfCorrection(ProcessResultDisplay processResultDisplay) {
    if (manager.useFileAsFullAlignedStack(processResultDisplay,
        FileType.CTF_CORRECTED_STACK, FinalAlignedStackDialog.CTF_CORRECTION_LABEL,
        axisID, dialogType)) {
      state.setUseCtfCorrectionWarning(axisID, false);
    }
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
    startProgressBar("Using filtered full aligned stack", axisID);
    File mtfFilteredStack = FileType.MTF_FILTERED_STACK.getFile(manager, axisID);
    if (!mtfFilteredStack.exists()) {
      UIHarness.INSTANCE
          .openMessageDialog(
              manager,
              "The filtered full aligned stack doesn't exist.  Create the filtered full aligned stack first",
              "Filtered full aligned stack missing", axisID);
      stopProgressBar(axisID);
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    if (FileType.ALIGNED_STACK.getFile(manager, axisID).exists()
        && mtfFilteredStack.exists()) {
      if (!Utilities.isValidStack(mtfFilteredStack, manager, axisID)) {
        UIHarness.INSTANCE.openMessageDialog(manager, mtfFilteredStack.getName()
            + " is not a valid MRC file.", "Entry Error", axisID);
        stopProgressBar(axisID);
        sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
        return;
      }
      try {
        manager.backupImageFile(FileType.ALIGNED_STACK, axisID);
      }
      catch (IOException except) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Unable to backup " + FileType.ALIGNED_STACK.getFileName(manager, axisID)
                + "\n" + except.getMessage(), "File Rename Error", axisID);
        stopProgressBar(axisID);
        sendMsg(ProcessResult.FAILED, processResultDisplay);
        return;
      }
    }
    try {
      manager
          .renameImageFile(FileType.MTF_FILTERED_STACK, FileType.ALIGNED_STACK, axisID);
    }
    catch (IOException except) {
      UIHarness.INSTANCE.openMessageDialog(manager, except.getMessage(),
          "File Rename Error", axisID);
      stopProgressBar(axisID);
      sendMsg(ProcessResult.FAILED, processResultDisplay);
      return;
    }
    stopProgressBar(axisID);
    sendMsg(ProcessResult.SUCCEEDED, processResultDisplay);
    state.setUseFilteredStackWarning(axisID, false);
  }

  private void updateFilter(final boolean enable) {
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
    String startingAndEndingZ = null;
    startingAndEndingZ = dialog.getStartingAndEndingZ();
    if (startingAndEndingZ.length() == 0 || startingAndEndingZ.matches("\\s+")) {
      // btnFilter.setSelected(false);
      dialog.setUseFilterEnabled(true);
    }
    else {
      dialog.setUseFilterEnabled(false);
    }
  }

  File getConfigDir() {
    File calibDir = EtomoDirector.INSTANCE.getIMODCalibDirectory();
    if (calibDir.exists()) {
      File dir = new File(calibDir, "CTFnoise");
      if (dir.exists()) {
        return dir;
      }
      return calibDir;
    }
    return new File(manager.getPropertyUserDir());
  }

  private boolean getParameters(final MTFFilterParam mtfFilterParam,
      final boolean doValidation) throws FortranInputSyntaxException {
    if (dialog == null) {
      return false;
    }
    try {
      mtfFilterParam.setLowPassRadiusSigma(dialog.getLowPassRadiusSigma(doValidation));
      mtfFilterParam.setStartingAndEndingZ(dialog.getStartingAndEndingZ(doValidation));
      mtfFilterParam.setMtfFile(dialog.getMtfFile(doValidation));
      mtfFilterParam.setMaximumInverse(dialog.getMaximumInverse(doValidation));
      mtfFilterParam.setInverseRolloffRadiusSigma(dialog
          .getInverseRolloffRadiusSigma(doValidation));
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
    return true;
  }

  private void setParameters(ConstCtfPlotterParam param) {
    if (dialog == null) {
      return;
    }
    dialog.setConfigFile(param.getConfigFile());
    dialog.setExpectedDefocus(Utilities.convertNanometersToMicrons(param
        .getExpectedDefocus()));
    dialog.setOffsetToAdd(param.getOffsetToAdd());
  }

  private boolean getParameters(CtfPlotterParam param, final boolean doValidation) {
    if (dialog == null) {
      return false;
    }
    try {
      param.setVoltage(dialog.getVoltage(doValidation));
      param.setSphericalAberration(dialog.getSphericalAberration(doValidation));
      param.setInvertTiltAngles(dialog.getInvertTiltAngles());
      param.setAmplitudeContrast(dialog.getAmplitudeContrast(doValidation));
      param.setExpectedDefocus(Utilities.convertMicronsToNanometers(dialog
          .getExpectedDefocus(doValidation)));
      param.setOffsetToAdd(dialog.getOffsetToAdd(doValidation));
      param.setConfigFile(dialog.getConfigFile());
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public void setTiltState() {
    if (dialog == null) {
      return;
    }
    dialog.setTiltState(state, metaData);
  }

  private void setParameters(ConstCtfPhaseFlipParam param) {
    if (dialog == null) {
      return;
    }
    dialog.setVoltage(param.getVoltage());
    dialog.setSphericalAberration(param.getSphericalAberration());
    dialog.setInvertTiltAngles(param.getInvertTiltAngles());
    dialog.setAmplitudeContrast(param.getAmplitudeContrast());
    dialog.setUseExpectedDefocus(param.getDefocusFile().endsWith(
        DatasetFiles.SIMPLE_DEFOCUS_EXT));
    dialog.setInterpolationWidth(param.getInterpolationWidth());
    dialog.setDefocusTol(param.getDefocusTol());
    dialog.updateCtfPlotter();
  }

  private boolean getParameters(CtfPhaseFlipParam param, final boolean doValidation) {
    if (dialog == null) {
      return false;
    }
    try {
      param.setVoltage(dialog.getVoltage(doValidation));
      param.setSphericalAberration(dialog.getSphericalAberration(doValidation));
      param.setInvertTiltAngles(dialog.getInvertTiltAngles());
      param.setAmplitudeContrast(dialog.getAmplitudeContrast(doValidation));
      if (dialog.isUseExpectedDefocus()) {
        param.setDefocusFile(DatasetFiles.getSimpleDefocusFileName(manager, axisID));
      }
      else {
        param.setDefocusFile(DatasetFiles.getCtfPlotterFileName(manager, axisID));
      }
      param.setInterpolationWidth(dialog.getInterpolationWidth(doValidation));
      param.setDefocusTol(dialog.getDefocusTol(doValidation));
      param.setOutputFileName(DatasetFiles.getCtfCorrectionFileName(manager, axisID));
      param.setPixelSize(metaData.getPixelSize()
          * Utilities.getStackBinning(manager, axisID, FileType.ALIGNED_STACK));
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  private boolean setParameters(final ConstMTFFilterParam mtfFilterParam) {
    if (dialog == null) {
      return false;
    }
    dialog.setMtfFile(mtfFilterParam.getMtfFile());
    dialog.setMaximumInverse(mtfFilterParam.getMaximumInverseString());
    dialog.setLowPassRadiusSigma(mtfFilterParam.getLowPassRadiusSigmaString());
    dialog.setStartingAndEndingZ(mtfFilterParam.getStartingAndEndingZString());
    dialog.setInverseRolloffRadiusSigma(mtfFilterParam
        .getInverseRolloffRadiusSigmaString());
    enableUseFilter();
    return true;
  }
}
