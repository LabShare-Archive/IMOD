package etomo.ui;

import java.io.File;

import etomo.ApplicationManager;
import etomo.comscript.BlendmontParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.ConstTomopitchParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.NewstParam;
import etomo.comscript.ProcessDetails;
import etomo.comscript.TiltParam;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TomopitchParam;
import etomo.comscript.XfproductParam;
import etomo.process.ImodManager;
import etomo.process.ProcessState;
import etomo.storage.TomopitchLog;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoBoolean2;
import etomo.type.MetaData;
import etomo.type.DialogExitState;
import etomo.type.ProcessResult;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessTrack;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.type.ViewType;
import etomo.util.DatasetFiles;
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
public final class TomogramPositioningExpert extends ReconUIExpert {
  public static final String rcsid = "$Id$";

  private final ComScriptManager comScriptMgr;
  private final TomogramState state;

  private TomogramPositioningDialog dialog = null;

  private boolean advanced = false;

  //backward compatibility functionality - if the metadata binning is missing
  //get binning from newst
  private boolean getBinningFromNewst = true;

  public TomogramPositioningExpert(ApplicationManager manager,
      MainTomogramPanel mainPanel, ProcessTrack processTrack, AxisID axisID) {
    super(manager, mainPanel, processTrack, axisID,
        DialogType.TOMOGRAM_POSITIONING);
    comScriptMgr = manager.getComScriptManager();
    state = manager.getState();
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  public void startNextProcess(ProcessResultDisplay processResultDisplay) {
    ProcessName nextProcess = getNextProcess();
    resetNextProcess();
    //whole tomogram
    if (nextProcess == ProcessName.TILT) {
      sampleTilt(processResultDisplay);
    }
  }

  /**
   * Post processing for sample and tilt.  ProcessDetails is required in both
   * cases.
   * @param processDetails
   * @param state
   */
  public void postProcess(ProcessDetails processDetails, TomogramState state) {
    state.setSampleXAxisTilt(axisID, processDetails
        .getDoubleValue(TiltParam.Field.X_AXIS_TILT));
    boolean fiducialess = processDetails
        .getBooleanValue(TiltParam.Field.FIDUCIALESS);
    state.setSampleFiducialess(axisID, fiducialess);
    if (!fiducialess) {
      state.setSampleAxisZShift(axisID, state.getAlignAxisZShift(axisID));
      state.setSampleAngleOffset(axisID, state.getAlignAngleOffset(axisID));
    }
    else {
      //no alignment for fidless
      state.setSampleAxisZShift(axisID, processDetails
          .getDoubleValue(TiltParam.Field.Z_SHIFT));
      state.setSampleAngleOffset(axisID, processDetails
          .getDoubleValue(TiltParam.Field.TILT_ANGLE_OFFSET));
    }
    updateFiducialessDisplay();
  }

  /**
   * Open the tomogram positioning dialog
   * @param axisID
   */
  public void openDialog() {
    if (!showDialog(dialog) || dialog != null) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    Utilities.timestamp("new", "TomogramPositioningDialog",
        Utilities.STARTED_STATUS);
    dialog = new TomogramPositioningDialog(manager, this, axisID);
    Utilities.timestamp("new", "TomogramPositioningDialog",
        Utilities.FINISHED_STATUS);
    // Read in the meta data parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions
    setParameters(metaData);
    if (metaData.getViewType() != ViewType.MONTAGE) {
      comScriptMgr.loadNewst(axisID);
      setParameters(comScriptMgr.getNewstComNewstParam(axisID));
    }
    else {
      comScriptMgr.loadBlend(axisID);
      setParameters(comScriptMgr.getBlendParam(axisID));
    }

    // Get the align{|a|b}.com parameters
    comScriptMgr.loadAlign(axisID);
    TiltalignParam tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
    if (metaData.getViewType() != ViewType.MONTAGE) {
      //upgrade and save param to comscript
      UIExpertUtilities.INSTANCE.upgradeOldAlignCom(manager, axisID,
          tiltalignParam);
    }
    setAlignParam(tiltalignParam);

    // Get the tilt{|a|b}.com parameters
    comScriptMgr.loadTilt(axisID);
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    tiltParam.setFiducialess(metaData.isFiducialess(axisID));
    setTiltParam(tiltParam);
    //If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      //upgrade and save param to comscript
      UIExpertUtilities.INSTANCE.upgradeOldTiltCom(manager, axisID, tiltParam);
    }

    // Get the tomopitch{|a|b}.com parameters
    comScriptMgr.loadTomopitch(axisID);
    setTomopitchParam(comScriptMgr.getTomopitchParam(axisID));

    // Set the whole tomogram sampling state, fidcialess state, and tilt axis
    // angle
    dialog.setWholeTomogram(metaData.isWholeTomogramSample(axisID));

    setFiducialess();
    dialog.setImageRotation(metaData.getImageRotation(axisID));
    setButtonState(manager.getScreenState(axisID));
    fiducialessAction();
    openDialog(dialog);
  }

  private void setButtonState(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.setSampleButtonState(screenState);
    dialog.setTomopitchButtonState(screenState);
    dialog.setAlignButtonState(screenState);
  }

  void sampleAction(ProcessResultDisplay sample) {
    if (dialog == null) {
      return;
    }
    if (dialog.isWholeTomogram()) {
      wholeTomogram(sample);
    }
    else {
      createSample(sample);
    }
  }

  void createBoundary(Run3dmodMenuOptions menuOptions) {
    if (dialog == null) {
      return;
    }
    if (dialog.isWholeTomogram()) {
      manager.imodFullSample(axisID, menuOptions);
    }
    else {
      manager.imodSample(axisID, menuOptions);
    }
  }

  boolean doneDialog() {
    if (dialog == null) {
      return false;
    }
    DialogExitState exitState = dialog.getExitState();
    if (exitState == DialogExitState.EXECUTE) {
      ConstEtomoNumber sampleFiducialess = state.getSampleFiducialess(axisID);
      if ((sampleFiducialess == null || !sampleFiducialess.is())
          && dialog.isTomopitchButton() && dialog.isAlignButtonEnabled()
          && !dialog.isAlignButton()) {
        if (!UIHarness.INSTANCE
            .openYesNoWarningDialog(
                "Final alignment is not done or is out of date.\nReally leave Tomogram Positioning?",
                axisID)) {
          return false;
        }
      }
      manager
          .closeImod(ImodManager.SAMPLE_KEY, axisID, "sample reconstruction");
    }
    if (exitState != DialogExitState.CANCEL) {
      if (!saveDialog()) {
        return false;
      }
    }
    leaveDialog(exitState);
    dialog = null;
    return true;
  }

  void fiducialessAction() {
    if (dialog == null) {
      return;
    }
    updateFiducialessDisplay(dialog.isFiducialess());
    updateFiducialessDisplay();
    //Save tilt param.
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    tiltParam.setFiducialess(metaData.isFiducialess(axisID));
    if (dialog.isFiducialess()) {
      dialog.setTiltAngleOffset(tiltParam.getTiltAngleOffset());
      dialog.setZShift(tiltParam.getZShift());
    }
    else {
      getTiltAngleOffset(tiltParam);
      getZShift(tiltParam);
    }
    tiltParam.resetSubsetStart();
    comScriptMgr.saveTilt(tiltParam, axisID);
    metaData.setFiducialess(axisID, tiltParam.isFiducialess());
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  protected boolean saveDialog() {
    if (dialog == null) {
      return false;
    }
    advanced = dialog.isAdvanced();
    //  Get all of the parameters from the panel
    if (updateAlignCom() == null) {
      return false;
    }
    if (updateTomoPosTiltCom(false) == null) {
      return false;
    }
    if (!updateTomopitchCom()) {
      return false;
    }
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(manager, dialog,
        axisID)) {
      return false;
    }
    if (metaData.getViewType() != ViewType.MONTAGE && updateNewstCom() == null) {
      return false;
    }
    manager.saveStorables(axisID);
    return true;
  }

  /**
   * Run the sample com script
   */
  public void createSample(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    // Make sure that we have an active positioning dialog
    if (dialog == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not update sample.com without an active positioning dialog",
          "Program logic error", axisID);
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    //  Get the user input data from the dialog box
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(manager, dialog,
        axisID)) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    ConstTiltParam tiltParam = updateTomoPosTiltCom(true);
    if (tiltParam == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    comScriptMgr.loadTilt(axisID);
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.createSample(axisID, processResultDisplay, tiltParam),
        processResultDisplay);
  }

  /**
   * Create a whole tomogram for positioning the tomogram in the volume
   * 
   * @param axisID
   */
  public void wholeTomogram(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    // Make sure that we have an active positioning dialog
    if (dialog == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not save comscripts without an active positioning dialog",
          "Program logic error", axisID);
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    // Get the user input from the dialog
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(manager, dialog,
        axisID)) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    ConstNewstParam newstParam = null;
    BlendmontParam blendmontParam = null;
    if (metaData.getViewType() != ViewType.MONTAGE) {
      newstParam = updateNewstCom();
      if (newstParam == null) {
        sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
        return;
      }
    }
    else {
      blendmontParam = updateBlendCom();
      if (blendmontParam == null) {
        sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
        return;
      }
    }
    if (updateTomoPosTiltCom(false) == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    ProcessResult processResult;
    if (metaData.getViewType() != ViewType.MONTAGE) {
      processResult = manager.wholeTomogram(axisID, processResultDisplay,
          newstParam);
    }
    else {
      processResult = manager.wholeTomogram(axisID, processResultDisplay,
          blendmontParam);
    }
    if (processResult != null) {
      sendMsg(processResult, processResultDisplay);
      return;
    }
    setNextProcess(ProcessName.TILT);
  }

  public void tomopitch(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (dialog == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not save comscript without an active positioning dialog",
          "Program logic error", axisID);
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (!updateTomopitchCom()) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.tomopitch(axisID, processResultDisplay),
        processResultDisplay);
  }

  /**
   * Open the tomopitch log file
   * 
   * @param axisID
   */
  private void openTomopitchLog() {
    String logFileName = DatasetFiles.getTomopitchLogFileName(manager, axisID);
    TextPageWindow logFileWindow = new TextPageWindow();
    logFileWindow.setVisible(logFileWindow.setFile(manager.getPropertyUserDir()
        + File.separator + logFileName));
  }

  /**
   * If dialog is null, then the tomopitch output will not end up on the tomo
   * pos dialog.  But this is unlikely to happen because tomopitch runs very
   * fast.  The values will have to be save somewhere else if this is a problem.
   * @param axisID
   */
  public void setTomopitchOutput() {
    if (dialog == null) {
      return;
    }
    TomopitchLog log = new TomopitchLog(manager, axisID);
    if (!setParameters(log)) {
      openTomopitchLog();
    }
  }

  public void finalAlign(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (dialog == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not save comscript without an active positioning dialog",
          "Program logic error", axisID);
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    ConstTiltalignParam tiltalignParam = updateAlignCom();
    if (tiltalignParam == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.finalAlign(axisID, processResultDisplay, tiltalignParam),
        processResultDisplay);
  }

  /**
   * Whole tomogram
   * @param processResultDisplay
   */
  private void sampleTilt(ProcessResultDisplay processResultDisplay) {
    comScriptMgr.loadTilt(axisID);
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    tiltParam.setCommandMode(TiltParam.Mode.WHOLE);
    tiltParam.setFiducialess(metaData.isFiducialess(axisID));
    sendMsg(manager.sampleTilt(axisID, processResultDisplay, tiltParam),
        processResultDisplay);
  }

  private ConstTiltalignParam updateAlignCom() {
    TiltalignParam tiltalignParam;
    try {
      tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
      getAlignParams(tiltalignParam);
      rollAlignComAngles();
      comScriptMgr.saveAlign(tiltalignParam, axisID);
      //update xfproduct in align.com
      XfproductParam xfproductParam = comScriptMgr.getXfproductInAlign(axisID);
      xfproductParam.setScaleShifts(UIExpertUtilities.INSTANCE.getStackBinning(
          manager, axisID, ".preali"));
      comScriptMgr.saveXfproductInAlign(xfproductParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tiltalign Parameter Syntax Error", axisID);
      return null;
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xfproduct Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Xfproduct Parameter Syntax Error", axisID);
      return null;
    }
    return tiltalignParam;
  }

  /**
   * Update the tilt{|a|b}.com file with sample parameters for the specified
   * axis
   */
  private ConstTiltParam updateTomoPosTiltCom(boolean sample) {
    // Make sure that we have an active positioning dialog
    if (dialog == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not update sample.com without an active positioning dialog",
          "Program logic error", axisID);
      return null;
    }
    // Get the current tilt parameters, make any user changes and save the
    // parameters back to the tilt{|a|b}.com
    TiltParam tiltParam = null;
    try {
      tiltParam = comScriptMgr.getTiltParam(axisID);
      tiltParam.setFiducialess(metaData.isFiducialess(axisID));
      getTiltParams(tiltParam);
      if (sample) {
        getTiltParamsForSample(tiltParam);
        tiltParam.setCommandMode(TiltParam.Mode.SAMPLE);
      }
      else {
        tiltParam.setCommandMode(TiltParam.Mode.WHOLE);
      }
      getParameters(metaData, tiltParam);
      String outputFileName;
      if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
        outputFileName = metaData.getDatasetName() + "_full.rec";
      }
      else {
        outputFileName = metaData.getDatasetName() + axisID.getExtension()
            + ".rec";
      }
      tiltParam.setOutputFile(outputFileName);
      if (metaData.getViewType() == ViewType.MONTAGE) {
        //binning is currently always 1 and correct size should be coming from
        //copytomocoms
        //tiltParam.setMontageFullImage(propertyUserDir, tomogramPositioningDialog.getBinning());
      }
      rollTiltComAngles();
      comScriptMgr.saveTilt(tiltParam, axisID);
      metaData.setFiducialess(axisID, tiltParam.isFiducialess());
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID);
      return null;
    }
    return tiltParam;
  }

  /**
   * Update the tomopitch{|a|b}.com file with sample parameters for the
   * specified axis
   */
  private boolean updateTomopitchCom() {
    // Make sure that we have an active positioning dialog
    if (dialog == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not update tomopitch.com without an active positioning dialog",
          "Program logic error", axisID);
      return false;
    }
    // Get the current tilt parameters, make any user changes and save the
    // parameters back to the tilt{|a|b}.com
    try {
      TomopitchParam tomopitchParam = comScriptMgr.getTomopitchParam(axisID);
      getTomopitchParam(tomopitchParam);
      comScriptMgr.saveTomopitch(tomopitchParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tomopitch Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tomopitch Parameter Syntax Error", axisID);
      return false;
    }
    return true;
  }

  /**
   * Update the newst{|a|b}.com scripts with the parameters from the tomogram
   * positioning dialog. The positioning dialog is passed so that the
   * signature is different from the standard method.
   * 
   * @param tomogramPositioningDialog
   * @param axisID
   * @return
   */
  private ConstNewstParam updateNewstCom() {
    if (dialog == null) {
      return null;
    }
    //  Get the whole tomogram positions state
    metaData.setWholeTomogramSample(axisID, dialog.isWholeTomogram());
    NewstParam newstParam = comScriptMgr.getNewstComNewstParam(axisID);
    getNewstParam(newstParam);
    try {
      // Make sure the size output is removed, it was only there as a 
      // copytomocoms template
      newstParam.setCommandMode(NewstParam.Mode.WHOLE_TOMOGRAM_SAMPLE);
      newstParam.resetSizeToOutputInXandY();
    }
    catch (FortranInputSyntaxException e) {
      e.printStackTrace();
    }
    comScriptMgr.saveNewst(newstParam, axisID);
    return newstParam;
  }

  /**
   * Update the blend{|a|b}.com scripts with the parameters from the tomogram
   * positioning dialog. The positioning dialog is passed so that the
   * signature is different from the standard method.
   * 
   * @param tomogramPositioningDialog
   * @param axisID
   * @return
   */
  private BlendmontParam updateBlendCom() {
    if (dialog == null) {
      return null;
    }
    //  Get the whole tomogram positions state
    metaData.setWholeTomogramSample(axisID, dialog.isWholeTomogram());
    BlendmontParam blendmontParam = comScriptMgr.getBlendParam(axisID);
    getParameters(blendmontParam);
    blendmontParam.setMode(BlendmontParam.Mode.WHOLE_TOMOGRAM_SAMPLE);
    blendmontParam.setBlendmontState();
    blendmontParam.resetStartingAndEndingXandY();
    comScriptMgr.saveBlend(blendmontParam, axisID);
    return blendmontParam;
  }

  private void getAlignParams(TiltalignParam tiltalignParam) {
    if (dialog == null) {
      return;
    }
    tiltalignParam.setAngleOffset(dialog.getAngleOffsetTotal());
    tiltalignParam.setAxisZShift(dialog.getTiltAxisZShiftTotal());
    updateMetaData();
  }

  private void updateMetaData() {
    if (dialog == null) {
      return;
    }
    boolean wholeTomogram = dialog.isWholeTomogram();
    if (wholeTomogram != metaData.isWholeTomogramSample(axisID)) {
      metaData.setWholeTomogramSample(axisID, wholeTomogram);
    }
    metaData.setTomoPosBinning(axisID, dialog.getBinningValue());
  }

  void rollAlignComAngles() {
    if (dialog == null) {
      return;
    }
    dialog.updateTiltAngleOffsetDisplay(false);
    dialog.updateTiltAxisZShiftDisplay(false);
  }

  void rollTiltComAngles() {
    if (dialog == null) {
      return;
    }
    dialog.updateXAxisTiltDisplay(false);
  }

  private void getTiltParamsForSample(TiltParam tiltParam) {
    if (dialog == null) {
      return;
    }
    tiltParam.setThickness(dialog.getSampleThickness());
  }

  /**
   * If state.sampleFiducialess doesn't match the fiducialess checkbox, don't
   * allow Tomopitch to be run.  If state.sampleFiducialess is null (sample was
   * never run) enable Tomopitch.
   */
  void updateFiducialessDisplay() {
    if (dialog == null) {
      return;
    }
    EtomoBoolean2 sampleFiducialess = state.getSampleFiducialess(axisID);
    boolean enable = false;
    if (sampleFiducialess == null
        || sampleFiducialess.is() == dialog.isFiducialess()) {
      enable = true;
    }
    dialog.setTomopitchEnabled(enable);
    dialog.setAngleOffsetEnabled(enable);
    dialog.setTiltAxisZShiftEnabled(enable);
    dialog.setAlignButtonEnabled(enable);
    dialog.setTiltAngleOffsetEnabled(enable);
    dialog.setZShiftEnabled(enable);
    dialog.setXAxisTiltEnabled(enable);
    dialog.setThicknessEnabled(enable);
  }

  private void getTiltAngleOffset(TiltParam tiltParam) {
    if (dialog == null) {
      return;
    }
    tiltParam.setTiltAngleOffset(dialog.getTiltAngleOffset());
  }

  private void getZShift(TiltParam tiltParam) {
    if (dialog == null) {
      return;
    }
    tiltParam.setZShift(dialog.getZShift());
  }

  /**
   * Get the tilt.com parameters from the dialog
   * @param tiltParam
   * @throws NumberFormatException
   */
  private void getTiltParams(TiltParam tiltParam) {
    if (dialog == null) {
      return;
    }
    boolean fiducialess = dialog.isFiducialess();
    tiltParam.setFiducialess(fiducialess);
    if (fiducialess) {
      tiltParam.setTiltAngleOffset(dialog.getTiltAngleOffset());
      tiltParam.setZShift(dialog.getZShift());
    }
    tiltParam.setXAxisTilt(dialog.getXAxisTiltTotal());
    tiltParam.setImageBinned(getBinning());
    tiltParam.setThickness(dialog.getThickness());
    updateMetaData();
  }

  private void getParameters(MetaData metaData, ConstTiltParam tiltParam) {
    if (dialog == null) {
      return;
    }
    metaData.setSampleThickness(axisID, dialog.getSampleThickness());
  }

  /**
   * Get the tomopitch.com parameters from the dialog
   * @param TomopitchParam
   */
  private void getTomopitchParam(TomopitchParam tomopitchParam) {
    if (dialog == null) {
      return;
    }
    tomopitchParam.setScaleFactor(getBinning());
    tomopitchParam.setExtraThickness(dialog.getExtraThickness());
    tomopitchParam.setAngleOffsetOld(state.getSampleAngleOffset(axisID));
    tomopitchParam.setZShiftOld(state.getSampleAxisZShift(axisID));
    tomopitchParam.setXAxisTiltOld(state.getSampleXAxisTilt(axisID));
    updateMetaData();
  }

  /**
   * Get the newst.com parameters from the dialog
   * @param newstParam
   */
  private void getNewstParam(NewstParam newstParam) {
    int binning = getBinning();
    //Only whole tomogram can change binning
    // Only explcitly write out the binning if its value is something other than
    // the default of 1 to keep from cluttering up the com script  
    if (binning == 1) {
      newstParam.setBinByFactor(Integer.MIN_VALUE);
    }
    else {
      newstParam.setBinByFactor(binning);
    }
    updateMetaData();
  }

  /**
   * Set the metadata parameters in the dialog
   * @param newstParam
   */
  private void setParameters(ConstMetaData metaData) {
    if (dialog == null) {
      return;
    }
    ConstEtomoNumber binning = metaData.getTomoPosBinning(axisID);
    if (!binning.isNull()) {
      getBinningFromNewst = false;
      dialog.setBinning(binning);
    }
    dialog.setSampleThickness(metaData.getSampleThickness(axisID));
  }

  /**
   * Set the newst.com parameters in the dialog
   * @param param
   */
  private void setParameters(ConstNewstParam param) {
    if (dialog == null) {
      return;
    }
    if (getBinningFromNewst) {
      dialog.setBinning(param.getBinByFactor());
    }
  }

  /**
   * Set the blend.com parameters in the dialog
   * @param param
   */
  private void setParameters(BlendmontParam param) {
    if (dialog == null) {
      return;
    }
    if (getBinningFromNewst) {
      dialog.setBinning(param.getBinByFactor());
    }
  }

  /**
   * Set the align.com parameters in the dialog
   * @param tiltalignParam
   */
  private void setAlignParam(ConstTiltalignParam tiltalignParam) {
    if (dialog == null) {
      return;
    }
    dialog.setAngleOffset(tiltalignParam.getAngleOffset());
    dialog.setTiltAxisZShift(tiltalignParam.getAxisZShift());
  }

  private void setTiltParam(ConstTiltParam tiltParam) {
    if (dialog == null) {
      return;
    }
    dialog.setXAxisTilt(tiltParam.getXAxisTilt());
    dialog.setThickness(tiltParam.getThickness());
    dialog.setTiltAngleOffset(tiltParam.getTiltAngleOffset());
    dialog.setZShift(tiltParam.getZShift());
  }

  private void setTomopitchParam(ConstTomopitchParam tomopitchParam) {
    if (dialog == null) {
      return;
    }
    dialog.setExtraThickness(tomopitchParam.getExtraThicknessString());
  }

  /**
   * Get the newst.com parameters from the dialog
   * @param newstParam
   */
  private void getParameters(BlendmontParam blendmontParam) {
    blendmontParam.setBinByFactor(getBinning());
    updateMetaData();
  }

  private boolean setParameters(TomopitchLog log) {
    if (dialog == null) {
      return false;
    }
    boolean missingData = false;
    missingData = !dialog.setAngleOffset(log);
    missingData = !dialog.setTiltAxisZShift(log);
    missingData = !dialog.setXAxisTilt(log);
    dialog.setTiltAngleOffset(log);
    dialog.setZShift(log);

    ConstEtomoNumber thickness = log.getThickness();
    if (thickness.isNull()) {
      missingData = true;
    }
    else {
      dialog.setThickness(thickness);
    }
    UIHarness.INSTANCE.pack(axisID, manager);
    return !missingData;
  }

  private int getBinning() {
    if (dialog == null) {
      return 1;
    }
    if (!dialog.isWholeTomogram()) {
      return 1;
    }
    return dialog.getBinningValue();
  }

  private void setFiducialess() {
    if (dialog == null) {
      return;
    }
    boolean fiducialess = metaData.isFiducialessAlignment(axisID);
    dialog.setFiducialessAlignment(fiducialess);
  }

  void updateFiducialessDisplay(boolean fiducialess) {
    if (dialog == null) {
      return;
    }
    dialog.setAngleOffsetEnabled(!fiducialess);
    dialog.setTiltAxisZShiftEnabled(!fiducialess);
    dialog.setAlignButtonEnabled(!fiducialess);
    dialog.setRotationEnabled(fiducialess);
    dialog.setFinalAlignmentPanelVisible(!fiducialess);
    dialog.setTiltAngleOffsetVisible(fiducialess);
    dialog.setZShiftVisible(fiducialess);

    if (dialog.isWholeTomogram()) {
      dialog.setBinningEnabled(true);
      dialog.setSampleButton("Create Whole Tomogram");
      dialog
          .setSampleButtonToolTip("Create whole tomogram for drawing positioning model.");
    }
    else {
      dialog.setBinningEnabled(false);
      dialog.setSampleButton("Create Sample Tomograms");
      dialog
          .setSampleButtonToolTip(TomogramPositioningDialog.SAMPLE_TOMOGRAMS_TOOLTIP);
    }
  }

  protected ProcessDialog getDialog() {
    return dialog;
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.21  2008/01/28 23:45:16  sueh
 * <p> bug# 1071 Setting commandMode to WHOLE in sampleTilt, which is only used
 * <p> for whole tomogram samples.  When using wholeTomogram to create a whole
 * <p> tomogram sample, call updateTomoPosTiltCom with sample == true.  Otherwise
 * <p> it sets the command mode to SAMPLE.
 * <p>
 * <p> Revision 1.20  2007/12/13 01:14:23  sueh
 * <p> bug# 1056 Setting the Mode in TiltParam.
 * <p>
 * <p> Revision 1.19  2007/08/16 16:37:52  sueh
 * <p> bug# 1035 Resetting sizeToOutputInXandY in newst.  Resetting
 * <p> startingAndEndingX and Y in blend.  Resetting SUBSETSTART to 0 0 in tilt.
 * <p>
 * <p> Revision 1.18  2007/02/09 00:54:22  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 1.17  2007/02/05 23:46:14  sueh
 * <p> bug# 962 Moved comscript mode info to inner class.
 * <p>
 * <p> Revision 1.16  2006/11/15 21:36:00  sueh
 * <p> bug# 872 Changed saveIntermediateParamFile to saveStorables.
 * <p>
 * <p> Revision 1.15  2006/10/25 22:12:54  sueh
 * <p> bug# 952  fiducialessAction():  when unchecking fidless, update tilt.com from the
 * <p> screen version of z shift and tilt angle offset.
 * <p>
 * <p> Revision 1.14  2006/10/19 22:53:28  sueh
 * <p> bug# 941  Refit on fiducialess action
 * <p>
 * <p> Revision 1.13  2006/10/19 22:01:16  sueh
 * <p> bug# 942  updateFiducialessDisplay():  checking for dialog == null
 * <p>
 * <p> Revision 1.12  2006/09/19 22:38:35  sueh
 * <p> bug# 920 Refreshing meta data values in TiltParam each time tilt.com is loaded.
 * <p>
 * <p> Revision 1.11  2006/09/14 00:04:48  sueh
 * <p> bug# 920 Turn off tomopitch fields when the fiducialess setting with which
 * <p> sample was created does not match the fiducialess setting on the screen.
 * <p>
 * <p> Revision 1.10  2006/07/28 20:14:56  sueh
 * <p> bug# 868 checking for null dialog
 * <p>
 * <p> Revision 1.9  2006/07/26 16:43:08  sueh
 * <p> bug# 868  Created a base class for tomogram reconstruction experts called
 * <p> ReconUIExpert.  Moved responsibility for changing the process result display
 * <p> to the expert class.
 * <p>
 * <p> Revision 1.8  2006/07/18 18:02:22  sueh
 * <p> bug# 906 Don't complain about alignment not being done if the button is disabled.
 * <p>
 * <p> Revision 1.7  2006/07/04 20:42:51  sueh
 * <p> bug# 898 Return false from done and save functions if their dialog continues
 * <p> to be displayed.
 * <p>
 * <p> Revision 1.6  2006/07/04 18:04:49  sueh
 * <p> bug# 896 doneDialog():  return false if the user decides not to exit the dialog.
 * <p>
 * <p> Revision 1.5  2006/06/30 20:21:48  sueh
 * <p> bug# 884 Adding timestamps for constructing dialog objects.
 * <p>
 * <p> Revision 1.4  2006/06/30 20:04:44  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog done() functions,
 * <p> which is called by the button action functions and saveAction() in
 * <p> ProcessDialog.  Removed the button action function overides.  Set displayed to
 * <p> false after the done dialog function is called.  Added saveAction().
 * <p>
 * <p> Revision 1.3  2006/06/19 17:07:43  sueh
 * <p> bug# 851 saveDialog():  calling manager.closeImod to close the sample 3dmod
 * <p> instead of doing it in this class.
 * <p>
 * <p> Revision 1.2  2006/06/09 19:52:23  sueh
 * <p> bug# 870 Added ways for ApplicationManager to force an exit state:
 * <p> doneDialog(DialogExitState) and saveDialog(DialogExitState).
 * <p>
 * <p> Revision 1.1  2006/05/19 19:52:01  sueh
 * <p> bug# 866 Class to contain all the functionality details associated with
 * <p> tomogram positioning.
 * <p> </p>
 */
