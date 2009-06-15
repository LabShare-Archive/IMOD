package etomo.ui;

import java.io.File;
import java.io.IOException;

import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.ProcessSeries;
import etomo.comscript.BlendmontParam;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.ConstCtfPhaseFlipParam;
import etomo.comscript.ConstCtfPlotterParam;
import etomo.comscript.ConstMTFFilterParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ConstSplitCorrectionParam;
import etomo.comscript.CtfPhaseFlipParam;
import etomo.comscript.CtfPlotterParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.SplitCorrectionParam;
import etomo.comscript.XfmodelParam;
import etomo.process.ImodManager;
import etomo.process.ProcessState;
import etomo.storage.CpuAdoc;
import etomo.storage.LogFile;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.ConstProcessSeries;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.EnumeratedType;
import etomo.type.EtomoNumber;
import etomo.type.ProcessName;
import etomo.type.ProcessResult;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessTrack;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TomogramState;
import etomo.type.ViewType;
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
    dialog = FinalAlignedStackDialog.getInstance(manager, this, axisID, curTab);
    Utilities.timestamp("new", "FinalAlignedStackDialog",
        Utilities.FINISHED_STATUS);
    // no longer managing image size

    // Read in the newst{|a|b}.com parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions - from when full alignment and tilt where
    // on the same dialog
    if (metaData.getViewType() == ViewType.MONTAGE) {
      comScriptMgr.loadBlend(axisID);
      setParameters(comScriptMgr.getBlendParam(axisID));
    }
    else {
      comScriptMgr.loadNewst(axisID);
      setParameters(comScriptMgr.getNewstComNewstParam(axisID));//TEMP newstparam
    }
    setParameters(metaData);//TEMP metadata
    //backward compatibility
    //Try loading ctfcorrection.com first.  If it isn't there, copy it with
    //copytomocoms and load it.
    //Then try loading ctfplotter.com.  If it isn't there, copy it with
    //copytomocoms, using the voltage, etc from ctfcorrection.com.
    //Ignore ctfplotter.param.
    if (!comScriptMgr.loadCtfCorrection(axisID, false)) {
      manager.setupCtfCorrectionComScript(axisID);
      comScriptMgr.loadCtfCorrection(axisID, true);
    }
    CtfPhaseFlipParam ctfPhaseFlipParam = comScriptMgr
        .getCtfPhaseFlipParam(axisID);
    setParameters(ctfPhaseFlipParam);
    if (!comScriptMgr.loadCtfPlotter(axisID, false)) {
      //Get the voltage, etc from ctfcorrection.com, since it has been loaded, and it
      //is somewhat more likely that it was updated
      manager.setupCtfPlotterComScript(axisID, ctfPhaseFlipParam);
      comScriptMgr.loadCtfPlotter(axisID, true);
    }
    setParameters(comScriptMgr.getCtfPlotterParam(axisID));
    setParameters(screenState);
    comScriptMgr.loadMTFFilter(axisID);
    setParameters(comScriptMgr.getMTFFilterParam(axisID));
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
    if (nextProcess.equals(ProcessName.PROCESSCHUNKS.toString())) {
      processchunks(manager, dialog, processResultDisplay, processSeries,
          ProcessName.CTF_CORRECTION);
    }
  }

  boolean doneDialog() {
    if (dialog == null) {
      return false;
    }
    curTab = dialog.getCurTab();
    DialogExitState exitState = dialog.getExitState();
    if (exitState == DialogExitState.EXECUTE) {
      manager.closeImod(ImodManager.MTF_FILTER_KEY, axisID,
          "MTF filtered stack");
      manager.closeImod(ImodManager.CTF_CORRECTION_KEY, axisID,
          "CTF corrected stack");
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
      dialog.getParameters(metaData);
    }
    catch (FortranInputSyntaxException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Data File Error",
          manager.getManagerKey());
    }
    getParameters(screenState);
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(manager, dialog
        .getFiducialessParams(), axisID)) {
      return false;
    }
    if (metaData.getViewType() == ViewType.MONTAGE) {
      try {
        manager.updateBlendCom(dialog.getBlendmontDisplay(), axisID);
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE.openMessageDialog(e.getMessage(),
            "Update Com Error", manager.getManagerKey());
      }
      catch (InvalidParameterException e) {
        UIHarness.INSTANCE.openMessageDialog(e.getMessage(),
            "Update Com Error", manager.getManagerKey());
      }
      catch (IOException e) {
        UIHarness.INSTANCE.openMessageDialog(e.getMessage(),
            "Update Com Error", manager.getManagerKey());
      }
    }
    else {
      if (manager.updateNewstCom(dialog.getNewstackDisplay(), axisID) == null) {
        return false;
      }
    }
    if (!updateMTFFilterCom()) {
      return false;
    }
    updateCtfPlotterCom();
    updateCtfCorrectionCom();
    manager.saveStorables(axisID);
    return true;
  }

  private ConstCtfPlotterParam updateCtfPlotterCom() {
    CtfPlotterParam param = comScriptMgr.getCtfPlotterParam(axisID);
    getParameters(param);
    comScriptMgr.saveCtfPlotter(param, axisID);
    return param;
  }

  private ConstCtfPhaseFlipParam updateCtfCorrectionCom() {
    CtfPhaseFlipParam param = comScriptMgr.getCtfPhaseFlipParam(axisID);
    getParameters(param);
    comScriptMgr.saveCtfPhaseFlip(param, axisID);
    return param;
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
              "Program logic error", axisID, manager.getManagerKey());
      return false;
    }
    try {
      MTFFilterParam mtfFilterParam = comScriptMgr.getMTFFilterParam(axisID);
      getParameters(mtfFilterParam);
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
          "MTF Filter Parameter Syntax Error", axisID, manager.getManagerKey());
      return false;
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "MTF Filter Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "MTF Filter Parameter Syntax Error", axisID, manager.getManagerKey());
      return false;
    }
    return true;
  }

  ProcessDialog getDialog() {
    return dialog;
  }

  void xfmodel(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    if (dialog == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    sendMsgProcessStarting(processResultDisplay);
    XfmodelParam param = new XfmodelParam(manager, axisID);
    setDialogState(ProcessState.INPROGRESS);
    ProcessResult processResult = manager.xfmodel(axisID, processResultDisplay,
        processSeries, param, dialogType);
    sendMsg(processResult, processResultDisplay);
  }

  public void ccdEraser(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    if (dialog == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    sendMsgProcessStarting(processResultDisplay);
    CCDEraserParam param = updateCcdEraserParam();
    if (param == null) {
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    ProcessResult processResult = manager.ccdEraser(axisID,
        processResultDisplay, processSeries, param, dialogType);
    sendMsg(processResult, processResultDisplay);
  }

  void seedEraseFiducialModel(Run3dmodMenuOptions run3dmodMenuOptions,
      Run3dmodButton run3dmodButton) {
    manager.imodSeedModel(axisID, run3dmodMenuOptions, run3dmodButton,
        ImodManager.FINE_ALIGNED_KEY, DatasetFiles.getEraseFiducialsModelName(
            manager, axisID), null);
  }

  void imodErasedFiducials(Run3dmodMenuOptions run3dmodMenuOptions) {
    manager.imodOpen(axisID, ImodManager.ERASED_FIDUCIALS_KEY, DatasetFiles
        .getEraseFiducialsModelName(manager, axisID), run3dmodMenuOptions);
  }

  private CCDEraserParam updateCcdEraserParam() {
    CCDEraserParam param = new CCDEraserParam();
    param.setInputFile(DatasetFiles
        .getFullAlignedStackFileName(manager, axisID));
    param
        .setModelFile(DatasetFiles.getEraseFiducialsModelName(manager, axisID));
    param.setOutputFile(DatasetFiles
        .getErasedFiducialsFileName(manager, axisID));
    EtomoNumber fiducialDiameter = new EtomoNumber(EtomoNumber.Type.DOUBLE);
    fiducialDiameter.set(dialog.getFiducialDiameter());
    param.setBetterRadius(fiducialDiameter.getDouble() / 2.0);
    param.setPolynomialOrder(dialog.getPolynomialOrder());
    if (param.validate(manager.getManagerKey())) {
      return param;
    }
    return null;
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

  void ctfPlotter(ProcessResultDisplay processResultDisplay) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstCtfPlotterParam param = updateCtfPlotterCom();
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.ctfPlotter(axisID, processResultDisplay),
        processResultDisplay);
  }

  /**
   * Create or reuse a _simple.defocus file.
   * Expected format: 1 1 0 0 expectedDefocus.  The first four numbers don't
   * matter.
   * @return
   */
  boolean createSimpleDefocusFile() {
    boolean updateToDate = false;
    LogFile file = null;
    LogFile.ReaderId readerId = null;
    LogFile.WriterId writerId = null;
    try {
      file = LogFile.getInstance(DatasetFiles.getSimpleDefocusFile(manager,
          axisID), manager.getManagerKey());
      if (file.exists()) {
        readerId = file.openReader();
        String line = file.readLine(readerId);
        if (file.closeReader(readerId)) {
          readerId = null;
        }
        String[] array = line.split("\\s+");
        if (array[4].equals(dialog.getExpectedDefocus())) {
          updateToDate = true;
        }
        else {
          file.backup();
        }
      }
      if (!updateToDate) {
        writerId = file.openWriter();
        file.write("1 1 0 0 " + dialog.getExpectedDefocus(), writerId);
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
      file.closeReader(readerId);
    }
    if (writerId != null && !writerId.isEmpty()) {
      file.closeWriter(writerId);
    }
    if (!updateToDate) {
      if (!UIHarness.INSTANCE.openYesNoDialog(DatasetFiles
          .getSimpleDefocusFileName(manager, axisID)
          + " may not be up to date.  Continue?", axisID, manager
          .getManagerKey())) {
        return false;
      }
    }
    return true;
  }

  void ctfCorrection(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    if (dialog == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(manager, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    if (dialog.isUseExpectedDefocus() && !createSimpleDefocusFile()) {
      return;
    }
    if (dialog.isParallelProcess()) {
      splitcorrection(processResultDisplay, processSeries);
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
    ConstCtfPhaseFlipParam param = updateCtfCorrectionCom();
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    sendMsg(manager.ctfCorrection(axisID, processResultDisplay, processSeries),
        processResultDisplay);
  }

  private ConstSplitCorrectionParam updateSplitCorrectionParam() {
    if (dialog == null) {
      return null;
    }
    SplitCorrectionParam param = new SplitCorrectionParam(axisID);
    getParameters(param);
    return param;
  }

  void getParameters(final SplitCorrectionParam param) {
    param.setCpus(getParallelPanel().getCPUsSelected());
    MRCHeader header = MRCHeader.getInstance(manager.getPropertyUserDir(),
        DatasetFiles.getFullAlignedStackFileName(manager, axisID), axisID,
        manager.getManagerKey());
    try {
      if (header.read()) {
        param.setMaxZ(header.getNSections());
      }
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
    }
  }

  void splitcorrection(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    updateCtfCorrectionCom();
    ConstSplitCorrectionParam param = updateSplitCorrectionParam();
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    ProcessResult processResult = manager.splitCorrection(axisID,
        processResultDisplay, processSeries, param, dialogType);
    if (processResult == null) {
      processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString());
    }
    sendMsg(processResult, processResultDisplay);
  }

  /**
   * Replace the full aligned stack with the ctf corrected full aligned stack
   * created by ctfcorrection.com
   */
  void useCtfCorrection(ProcessResultDisplay processResultDisplay) {
    useFileAsFullAlignedStack(processResultDisplay, DatasetFiles
        .getCtfCorrectionFile(manager, axisID),
        FinalAlignedStackDialog.CTF_CORRECTION_LABEL);
  }

  void useCcdEraser(ProcessResultDisplay processResultDisplay) {
    useFileAsFullAlignedStack(processResultDisplay, DatasetFiles
        .getErasedFiducialsFile(manager, axisID),
        FinalAlignedStackDialog.CCD_ERASER_LABEL);
  }

  /**
   * Replace the full aligned stack with the ctf corrected full aligned stack
   * created by ctfcorrection.com
   */
  void useFileAsFullAlignedStack(ProcessResultDisplay processResultDisplay,
      File output, String buttonLabel) {
    if (dialog == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    if (manager.isAxisBusy(axisID, processResultDisplay)) {
      return;
    }
    setProgressBar("Using " + output.getName() + " as full aligned stack", 1,
        axisID);
    // Instantiate file objects for the original raw stack and the fixed
    // stack
    File fullAlignedStack = DatasetFiles.getFullAlignedStackFile(manager,
        axisID);
    if (!output.exists()) {
      UIHarness.INSTANCE.openMessageDialog(output.getAbsolutePath()
          + " doesn't exist.  Press " + buttonLabel + " to create this file.",
          buttonLabel + " Output Missing", axisID, manager.getManagerKey());
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    setDialogState(ProcessState.INPROGRESS);
    if (fullAlignedStack.exists() && output.exists()) {
      try {
        Utilities.renameFile(fullAlignedStack, new File(fullAlignedStack
            .getAbsolutePath()
            + "~"));
      }
      catch (IOException except) {
        UIHarness.INSTANCE.openMessageDialog("Unable to backup "
            + fullAlignedStack.getAbsolutePath() + "\n" + except.getMessage(),
            "File Rename Error", axisID, manager.getManagerKey());
        sendMsg(ProcessResult.FAILED, processResultDisplay);
        return;
      }
    }
    // don't have to rename full aligned stack because it is a generated
    // file
    try {
      Utilities.renameFile(output, fullAlignedStack);
    }
    catch (IOException except) {
      UIHarness.INSTANCE.openMessageDialog(except.getMessage(),
          "File Rename Error", axisID, manager.getManagerKey());
      sendMsg(ProcessResult.FAILED, processResultDisplay);
      return;
    }
    manager.closeImod(ImodManager.FINE_ALIGNED_KEY, axisID,
        "original full aligned stack");
    stopProgressBar(axisID);
    sendMsg(ProcessResult.SUCCEEDED, processResultDisplay);
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
              "Filtered full aligned stack missing", axisID, manager
                  .getManagerKey());
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
            "File Rename Error", axisID, manager.getManagerKey());
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
          "File Rename Error", axisID, manager.getManagerKey());
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

  //TEMP
  private void setParameters(ConstNewstParam newstParam) {
    if (dialog == null) {
      return;
    }
    dialog.setUseLinearInterpolation(newstParam.isLinearInterpolation());
  }

  private void setParameters(ConstMetaData metaData) {
    if (dialog == null) {
      return;
    }
    dialog.setBinning(metaData.getFinalStackBinning(axisID));
    CpuAdoc cpuAdoc = CpuAdoc.getInstance(AxisID.ONLY, manager
        .getPropertyUserDir(), manager.getManagerKey());
    if (!metaData.isFinalStackFiducialDiameterNull(axisID)) {
      dialog
          .setFiducialDiameter(metaData.getFinalStackFiducialDiameter(axisID));
    }
    else if (!metaData.isFinalStackBetterRadiusEmpty(axisID)) {
      //backwards compatibility - used to save better radius, convert it to
      //fiducial diameter in pixels
      EtomoNumber betterRadius = new EtomoNumber(EtomoNumber.Type.DOUBLE);
      betterRadius.set(metaData.getFinalStackBetterRadius(axisID));
      dialog.setFiducialDiameter(Math
          .round(betterRadius.getDouble() * 2 * 10.0) / 10.0);
    }
    else {
      //Currently not allowing an empty value to be saved.
      //Default fiducialDiameter to fiducialDiameter from setup / pixel size
      //(convert to pixels).  Round to 1 decimal place.
      dialog.setFiducialDiameter(Math.round(metaData.getFiducialDiameter()
          / metaData.getPixelSize() * 10.0) / 10.0);
    }
    dialog.setPolynomialOrder(PolynomialOrder.getInstance(metaData
        .getFinalStackPolynomialOrder(axisID)));
    //Parallel processing is optional in tomogram reconstruction, so only use it
    //if the user set it up.
    boolean validAutodoc = cpuAdoc.isAvailable();
    dialog.setParallelProcessEnabled(validAutodoc);
    ConstEtomoNumber parallel = metaData
        .getFinalStackCtfCorrectionParallel(axisID);
    if (parallel == null) {
      dialog.setParallelProcess(validAutodoc
          && metaData.getDefaultParallel().is());
    }
    else {
      dialog.setParallelProcess(validAutodoc && parallel.is());
    }
    updateParallelProcess();

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
    dialog.setNewstHeaderState(screenState.getStackNewstHeaderState());
    dialog.setFilterHeaderState(screenState.getStackMtffilterHeaderState());
    dialog.setCtfCorrectionHeaderState(screenState
        .getStackCtfCorrectionHeaderState());
    dialog.setAdvanced();
    dialog.setNewstButtonState(screenState);
    dialog.setUseFilterButtonState(screenState);
    dialog.setFilterButtonState(screenState);
  }

  private void getParameters(ReconScreenState screenState) {
    if (dialog == null) {
      return;
    }
    dialog.getNewstHeaderState(screenState.getStackNewstHeaderState());
    dialog.getFilterHeaderState(screenState.getStackMtffilterHeaderState());
    dialog.getCtfCorrectionHeaderState(screenState
        .getStackCtfCorrectionHeaderState());
  }

  private void setParameters(BlendmontParam blendmontParam) {
    dialog.setUseLinearInterpolation(blendmontParam.isLinearInterpolation());
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

  private void getParameters(MTFFilterParam mtfFilterParam)
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

  private void setParameters(ConstCtfPlotterParam param) {
    if (dialog == null) {
      return;
    }
    dialog.setConfigFile(param.getConfigFile());
    dialog.setExpectedDefocus(param.getExpectedDefocus());
  }

  private void getParameters(CtfPlotterParam param) {
    if (dialog == null) {
      return;
    }
    param.setVoltage(dialog.getVoltage());
    param.setSphericalAberration(dialog.getSphericalAberration());
    param.setAmplitudeContrast(dialog.getAmplitudeContrast());
    param.setExpectedDefocus(dialog.getExpectedDefocus());
    param.setConfigFile(dialog.getConfigFile());
  }

  private void setParameters(ConstCtfPhaseFlipParam param) {
    if (dialog == null) {
      return;
    }
    dialog.setVoltage(param.getVoltage());
    dialog.setSphericalAberration(param.getSphericalAberration());
    dialog.setAmplitudeContrast(param.getAmplitudeContrast());
    dialog.setUseExpectedDefocus(param.getDefocusFile().endsWith(
        DatasetFiles.SIMPLE_DEFOCUS_EXT));
    dialog.setInterpolationWidth(param.getInterpolationWidth());
    dialog.setDefocusTol(param.getDefocusTol());
    dialog.updateCtfPlotter();
  }

  private void getParameters(CtfPhaseFlipParam param) {
    if (dialog == null) {
      return;
    }
    param.setVoltage(dialog.getVoltage());
    param.setSphericalAberration(dialog.getSphericalAberration());
    param.setAmplitudeContrast(dialog.getAmplitudeContrast());
    if (dialog.isUseExpectedDefocus()) {
      param.setDefocusFile(DatasetFiles.getSimpleDefocusFileName(manager,
          axisID));
    }
    else {
      param.setDefocusFile(DatasetFiles.getCtfPlotterFileName(manager, axisID));
    }
    param.setInterpolationWidth(dialog.getInterpolationWidth());
    param.setDefocusTol(dialog.getDefocusTol());
    param.setOutputFileName(DatasetFiles.getCtfCorrectionFileName(manager,
        axisID));
  }

  private void setParameters(ConstMTFFilterParam mtfFilterParam) {
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

  static final class PolynomialOrder implements EnumeratedType {
    static final PolynomialOrder USE_MEAN = new PolynomialOrder(
        new EtomoNumber().set(0));
    static final PolynomialOrder FIT_A_PLANE = new PolynomialOrder(
        new EtomoNumber().set(1));
    private static final PolynomialOrder DEFAULT = USE_MEAN;

    private final ConstEtomoNumber value;

    private PolynomialOrder(final ConstEtomoNumber value) {
      this.value = value;
    }

    private static PolynomialOrder getInstance(int value) {
      if (USE_MEAN.value.equals(value)) {
        return USE_MEAN;
      }
      if (FIT_A_PLANE.value.equals(value)) {
        return FIT_A_PLANE;
      }
      return DEFAULT;
    }

    public boolean isDefault() {
      return this == DEFAULT;
    }

    public String toString() {
      return value.toString();
    }
  }
}
