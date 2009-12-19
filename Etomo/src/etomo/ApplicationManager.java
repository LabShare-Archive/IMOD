package etomo;

import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.Toolkit;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.channels.FileChannel;

import etomo.comscript.ArchiveorigParam;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.BlendmontParam;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.ClipParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.CombineComscriptState;
import etomo.comscript.CombineParams;
import etomo.comscript.Command;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.ConstFindBeads3dParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ConstSetParam;
import etomo.comscript.ConstSplitCorrectionParam;
import etomo.comscript.ConstSqueezevolParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.CtfPhaseFlipParam;
import etomo.comscript.ExtractmagradParam;
import etomo.comscript.ExtractpiecesParam;
import etomo.comscript.ExtracttiltsParam;
import etomo.comscript.FindBeads3dParam;
import etomo.comscript.FlattenWarpParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.GotoParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.MatchshiftsParam;
import etomo.comscript.MatchvolParam;
import etomo.comscript.MrcTaperParam;
import etomo.comscript.NewstParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.ProcessDetails;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.RunraptorParam;
import etomo.comscript.SetParam;
import etomo.comscript.SolvematchParam;
import etomo.comscript.SolvematchmodParam;
import etomo.comscript.SolvematchshiftParam;
import etomo.comscript.SplitcombineParam;
import etomo.comscript.SplittiltParam;
import etomo.comscript.SqueezevolParam;
import etomo.comscript.TiltParam;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TiltxcorrParam;
import etomo.comscript.TransferfidParam;
import etomo.comscript.TrimvolParam;
import etomo.comscript.WarpVolParam;
import etomo.comscript.XfmodelParam;
import etomo.comscript.XfproductParam;
import etomo.process.BaseProcessManager;
import etomo.process.ContinuousListenerTarget;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.process.ProcessData;
import etomo.process.ProcessManager;
import etomo.process.ProcessResultDisplayFactoryInterface;
import etomo.process.ProcessState;
import etomo.process.SystemProcessException;
import etomo.storage.CpuAdoc;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.storage.TaErrorLog;
import etomo.storage.XrayStackArchiveFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.CombineProcessType;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstMetaData;
import etomo.type.ConstProcessSeries;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.EtomoState;
import etomo.type.FiducialMatch;
import etomo.type.FileType;
import etomo.type.ImageFileType;
import etomo.type.InterfaceType;
import etomo.type.InvalidEtomoNumberException;
import etomo.type.MatchMode;
import etomo.type.MetaData;
import etomo.type.ProcessResult;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ProcessTrack;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.type.TomogramState;
import etomo.type.ViewType;
import etomo.ui.AbstractParallelDialog;
import etomo.ui.AlignmentEstimationDialog;
import etomo.ui.BeadTrackDisplay;
import etomo.ui.BeadtrackPanel;
import etomo.ui.BlendmontDisplay;
import etomo.ui.CcdEraserDisplay;
import etomo.ui.CleanUpDialog;
import etomo.ui.CoarseAlignDialog;
import etomo.ui.FiducialModelDialog;
import etomo.ui.FiducialessParams;
import etomo.ui.FinalAlignedStackDialog;
import etomo.ui.FinalAlignedStackExpert;
import etomo.ui.FindBeads3dDisplay;
import etomo.ui.FlattenWarpDisplay;
import etomo.ui.LogPanel;
import etomo.ui.MainPanel;
import etomo.ui.MainTomogramPanel;
import etomo.ui.NewstackDisplay;
import etomo.ui.ParallelPanel;
import etomo.ui.PostProcessingDialog;
import etomo.ui.PreProcessingDialog;
import etomo.ui.ProcessDialog;
import etomo.ui.Deferred3dmodButton;
import etomo.ui.ProcessDisplay;
import etomo.ui.SetupDialogExpert;
import etomo.ui.TiltDisplay;
import etomo.ui.TiltXcorrDisplay;
import etomo.ui.TomogramCombinationDialog;
import etomo.ui.TomogramGenerationExpert;
import etomo.ui.TomogramPositioningExpert;
import etomo.ui.TrialTiltDisplay;
import etomo.ui.UIExpert;
import etomo.ui.UIExpertUtilities;
import etomo.ui.UIHarness;
import etomo.ui.WarpVolDisplay;
import etomo.util.DatasetFiles;
import etomo.util.FidXyz;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

/**
 * <p>
 * Description: Handles high level message processing, management of other
 * high-level objects and signal routing for tomogram reconstructions.
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002 - 2006
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3-Dimensional Electron Microscopy of
 * Cells (BL3DEMC), University of Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class ApplicationManager extends BaseManager implements
    ContinuousListenerTarget {
  public static final String rcsid = "$Id$";

  // Process dialog references
  private SetupDialogExpert setupDialogExpert = null;
  private PreProcessingDialog preProcDialogA = null;
  private PreProcessingDialog preProcDialogB = null;
  private CoarseAlignDialog coarseAlignDialogA = null;
  private CoarseAlignDialog coarseAlignDialogB = null;
  private FiducialModelDialog fiducialModelDialogA = null;
  private FiducialModelDialog fiducialModelDialogB = null;
  private AlignmentEstimationDialog fineAlignmentDialogA = null;
  private AlignmentEstimationDialog fineAlignmentDialogB = null;
  private TomogramPositioningExpert tomogramPositioningExpertA = null;
  private TomogramPositioningExpert tomogramPositioningExpertB = null;
  private FinalAlignedStackExpert finalAlignedStackExpertA = null;
  private FinalAlignedStackExpert finalAlignedStackExpertB = null;
  private TomogramGenerationExpert tomogramGenerationExpertA = null;
  private TomogramGenerationExpert tomogramGenerationExpertB = null;
  protected TomogramCombinationDialog tomogramCombinationDialog = null;
  private PostProcessingDialog postProcessingDialog = null;
  private CleanUpDialog cleanUpDialog = null;
  private MetaData metaData = null;
  private MainTomogramPanel mainPanel;
  private ProcessTrack processTrack;
  private ProcessManager processMgr;
  private TomogramState state = null;
  private boolean[] advancedA = new boolean[DialogType.TOTAL_RECON];
  private boolean[] advancedB = new boolean[DialogType.TOTAL_RECON];
  private ReconScreenState screenStateA = null;
  private ReconScreenState screenStateB = null;
  private ProcessResultDisplayFactory processResultDisplayFactoryA = null;
  private ProcessResultDisplayFactory processResultDisplayFactoryB = null;
  //True if reconnect() has been run for the specified axis.
  private boolean reconnectRunA = false;
  private boolean reconnectRunB = false;

  /**
   * Does initialization and loads the .edf file. Opens the setup dialog if
   * there is no .edf file.
   */
  ApplicationManager(String paramFileName, AxisID axisID) {
    super();
    metaData = new MetaData(this);
    createState();
    processMgr = new ProcessManager(this);
    initializeUIParameters(paramFileName, axisID);
    initializeAdvanced();
    // Open the etomo data file if one was found on the command line
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      if (!paramFileName.equals("")) {
        imodManager.setMetaData(metaData);
        if (loadedParamFile) {
          openProcessingPanel();
          mainPanel.setStatusBarText(paramFile, metaData, logPanel);
        }
        else {
          openSetupDialog();
          return;
        }
      }
      else {
        openSetupDialog();
        return;
      }
    }
  }

  public void doAutomation() {
    if (setupDialogExpert != null) {
      setupDialogExpert.doAutomation();
    }
    super.doAutomation();
  }

  public boolean setParamFile() {
    return loadedParamFile;
  }

  public LogPanel createLogPanel() {
    return LogPanel.getInstance(getManagerKey());
  }

  /**
   * Initialize advancedA and advancedB, which remember the advanced state of
   * dialogs when the user switches from one to another. This is necessary
   * because the done function is run each time a user switches dialogs.
   * 
   */
  private void initializeAdvanced() {
    boolean isAdvanced = EtomoDirector.INSTANCE.getAdvanced();
    for (int i = 0; i < DialogType.TOTAL_RECON; i++) {
      advancedA[i] = isAdvanced;
      advancedB[i] = isAdvanced;
    }
  }

  public ProcessResultDisplayFactoryInterface getProcessResultDisplayFactoryInterface(
      AxisID axisID) {
    return getProcessResultDisplayFactory(axisID);
  }

  public ProcessResultDisplayFactory getProcessResultDisplayFactory(
      AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (processResultDisplayFactoryB == null) {
        processResultDisplayFactoryB = ProcessResultDisplayFactory
            .getInstance(getBaseScreenState(axisID));
      }
      return processResultDisplayFactoryB;
    }
    if (processResultDisplayFactoryA == null) {
      processResultDisplayFactoryA = ProcessResultDisplayFactory
          .getInstance(getBaseScreenState(axisID));
    }
    return processResultDisplayFactoryA;
  }

  /**
   * Checks the advanced state of a dialog.
   * @param dialogType
   * @param axisID
   * @return
   */
  public boolean isAdvanced(DialogType dialogType, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return advancedB[dialogType.toIndex()];
    }
    return advancedA[dialogType.toIndex()];
  }

  /**
   * Sets the advanced state of a dialog.
   * @param dialogType
   * @param axisID
   * @param advanced
   */
  public void setAdvanced(DialogType dialogType, AxisID axisID, boolean advanced) {
    if (axisID == AxisID.SECOND) {
      advancedB[dialogType.toIndex()] = advanced;
    }
    advancedA[dialogType.toIndex()] = advanced;
  }

  /**
   * Sets the advanced state of a dialog. Assumes the A axis (or single axis).
   * @param dialogType
   * @param advanced
   */
  public void setAdvanced(DialogType dialogType, boolean advanced) {
    advancedA[dialogType.toIndex()] = advanced;
  }

  /**
   * Finds out whether the manager is new, which means that it has no .edf file.
   * If setupDialog is not null, then the manager is new.
   */
  public boolean isNewManager() {
    return setupDialogExpert != null;
  }

  /**
   * Check if setup dialog has been modified by the user. Return true if there
   * is text in the dataset field.
   * @return
   */
  public boolean isSetupChanged() {
    if (setupDialogExpert == null) {
      return false;
    }
    if (setupDialogExpert.getDataset() == null
        || setupDialogExpert.getDataset().matches("\\s*")) {
      return false;
    }
    return true;
  }

  /**
   * Open the setup dialog
   */
  public void openSetupDialog() {
    // Open the dialog in the appropriate mode for the current state of
    // processing
    setCurrentDialogType(DialogType.SETUP_RECON, AxisID.ONLY);
    if (setupDialogExpert == null) {
      Utilities.timestamp("new", "SetupDialog", Utilities.STARTED_STATUS);
      //check for distortion directory
      File distortionDir = DatasetFiles.getDistortionDir(propertyUserDir,
          AxisID.ONLY, getManagerKey());
      setupDialogExpert = SetupDialogExpert.getInstance(this,
          distortionDir != null && distortionDir.exists());
      Utilities.timestamp("new", "SetupDialog", Utilities.FINISHED_STATUS);
      setupDialogExpert.initializeFields((ConstMetaData) metaData, userConfig);
    }
    mainPanel.openSetupPanel(setupDialogExpert);
    if (!GraphicsEnvironment.isHeadless()) {
      Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
      Dimension frameSize = mainPanel.getSize();
      mainPanel.setLocation((screenSize.width - frameSize.width) / 2,
          (screenSize.height - frameSize.height) / 2);
    }
  }

  /**
   * Close message from the setup dialog window
   */
  public boolean doneSetupDialog() {
    // Get the selected exit button
    DialogExitState exitState = setupDialogExpert.getExitState();
    if (exitState != DialogExitState.CANCEL) {
      if (!setupDialogExpert.isValid()) {
        return false;
      }
      // Set the current working directory for the application saving the
      // old user.dir property until the meta data is valid
      String oldUserDir = propertyUserDir;
      propertyUserDir = setupDialogExpert.getWorkingDirectory()
          .getAbsolutePath();
      metaData = setupDialogExpert.getFields();
      if (metaData == null) {
        return false;
      }
      if (metaData.isValid()) {
        if (setupDialogExpert.checkForSharedDirectory()) {
          uiHarness.openMessageDialog("This directory (" + propertyUserDir
              + ") is already being used by an .edf file.  Either open the "
              + "existing .edf file or create a new directory for the new "
              + "reconstruction.", "WARNING:  CANNOT PROCEED", AxisID.ONLY,
              getManagerKey());
          return false;
        }
        processTrack.setSetupState(ProcessState.INPROGRESS);
        // final initialization of IMOD manager
        imodManager.setMetaData(metaData);
        // set paramFile so meta data can be saved
        paramFile = new File(propertyUserDir, metaData.getMetaDataFileName());
        mainPanel.setStatusBarText(paramFile, metaData, logPanel);
        if (userConfig.getSwapYAndZ()) {
          TrimvolParam trimvolParam = metaData.getTrimvolParam();
          trimvolParam.setSwapYZ(true);
        }
        userConfig.putDataFile(paramFile.getAbsolutePath());
        loadedParamFile = true;
        state.initialize();
      }
      else {
        String[] errorMessage = new String[2];
        errorMessage[0] = "Setup Parameter Error";
        errorMessage[1] = metaData.getInvalidReason();
        uiHarness.openMessageDialog(errorMessage, "Setup Parameter Error",
            AxisID.ONLY, getManagerKey());
        propertyUserDir = oldUserDir;
        return false;
      }
      // This is really the method to use the existing com scripts
      if (exitState == DialogExitState.EXECUTE) {
        if (!processMgr.setupComScripts(AxisID.ONLY)) {
          return false;
        }
        //Create the .rawtlt file if the angle type is range.  This makes it
        //easy to display titl angles in 3dmod.
        if (metaData.getTiltAngleSpecA().getType() == TiltAngleType.RANGE) {
          try {
            AxisType axisType = metaData.getAxisType();
            makeRawtltFile(axisType == AxisType.DUAL_AXIS ? AxisID.FIRST
                : AxisID.ONLY);
            if (axisType == AxisType.DUAL_AXIS) {
              makeRawtltFile(AxisID.SECOND);
            }
          }
          catch (IOException e) {
            e.printStackTrace();
          }
          catch (InvalidParameterException e) {
            e.printStackTrace();
          }
        }
      }
      processTrack.setSetupState(ProcessState.COMPLETE);
      metaData.setComScriptCreated(true);
      EtomoDirector.INSTANCE.renameCurrentManager(metaData.getDatasetName());
      closeImods(ImodManager.PREVIEW_KEY, AxisID.FIRST, "Axis A preview stack");
      closeImods(ImodManager.PREVIEW_KEY, AxisID.SECOND, "Axis B preview stack");
    }
    // Switch the main window to the procesing panel
    openProcessingPanel();
    // Free the dialog
    setupDialogExpert = null;
    saveStorables(AxisID.ONLY);
    return true;
  }

  public void setupCtfCorrectionComScript(AxisID axisID) {
    processMgr.setupCtfCorrectionComScript(axisID);
  }

  public void setupCtfPlotterComScript(AxisID axisID,
      CtfPhaseFlipParam ctfPhaseFlipParam) {
    processMgr.setupCtfPlotterComScript(ctfPhaseFlipParam, axisID);
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.RECON;
  }

  /**
   * Open the main window in processing mode
   * MUST run reconnect for all axis
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(metaData.getAxisType());
    mainPanel.updateAllProcessingStates(processTrack);
    setPanel();
    if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
      reconnect(processMgr.getSavedProcessData(AxisID.FIRST), AxisID.FIRST);
      reconnect(processMgr.getSavedProcessData(AxisID.SECOND), AxisID.SECOND);
    }
    else {
      reconnect(processMgr.getSavedProcessData(AxisID.ONLY), AxisID.ONLY);
    }
  }

  private boolean isReconnectRun(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return reconnectRunB;
    }
    return reconnectRunA;
  }

  private void setReconnectRun(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      reconnectRunB = true;
    }
    else {
      reconnectRunA = true;
    }
  }

  /**
   * Attempts to reconnect to a currently running process.  Only run once per
   * axis.  Only attempts one reconnect.
   * Must run super.reconnect first
   * @param axisID - axis of the running process.
   * @return true if a reconnect was attempted.
   */
  public boolean reconnect(ProcessData processData, AxisID axisID) {
    if (super.reconnect(processData, axisID)) {
      return true;
    }
    if (isReconnectRun(axisID)) {
      return false;
    }
    setReconnectRun(axisID);
    if (processData == null) {
      return false;
    }
    if (processData.getProcessName() == ProcessName.TILT) {
      if (processData.isOnDifferentHost()) {
        handleDifferentHost(processData, axisID);
        return false;
      }
      if (processData.isRunning()) {
        System.err.println("\nAttempting to reconnect in Axis "
            + axisID.toString() + "\n" + processData);
        if (!((TomogramGenerationExpert) getUIExpert(
            DialogType.TOMOGRAM_GENERATION, axisID)).reconnectTilt(processData
            .getProcessName())) {
          System.err.println("\nReconnect in Axis" + axisID.toString()
              + " failed");
        }
        return true;
      }
    }
    return false;
  }

  public boolean reconnectTilt(AxisID axisID, ProcessName processName,
      ProcessResultDisplay display) {
    boolean ret = processMgr.reconnectTilt(axisID, display);
    setThreadName(processName.toString(), axisID);
    return ret;
  }

  /**
   * Open the pre-processing dialog
   */
  public void openPreProcDialog(AxisID axisID) {
    // Check to see if the com files are present otherwise pop up a dialog
    // box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(metaData, axisID,
        getManagerKey())) {
      mainPanel.showBlankProcess(axisID);
      return;
    }
    setCurrentDialogType(DialogType.PRE_PROCESSING, axisID);
    mainPanel.selectButton(axisID, "Pre-processing");
    // TODO: When a panel is overwriten by another should it be nulled and
    // closed or left and and reshown when needed?
    // Problem with stale data for align and tilt info since they are on
    // multiple panels
    // Check to see if the dialog panel is already open
    if (showIfExists(preProcDialogA, preProcDialogB, axisID)) {
      return;
    }
    Utilities.timestamp("new", "PreProcessingDialog", Utilities.STARTED_STATUS);
    PreProcessingDialog preProcDialog = new PreProcessingDialog(this, axisID);
    Utilities
        .timestamp("new", "PreProcessingDialog", Utilities.FINISHED_STATUS);
    if (axisID == AxisID.SECOND) {
      preProcDialogB = preProcDialog;
    }
    else {
      preProcDialogA = preProcDialog;
    }
    // Load the required ccderaser{|a|b}.com files
    // Fill in the parameters and set it to the appropriate state
    comScriptMgr.loadEraser(axisID);
    preProcDialog.setCCDEraserParams(comScriptMgr.getCCDEraserParam(axisID));
    preProcDialog.setParameters(getScreenState(axisID));
    mainPanel.showProcess(preProcDialog.getContainer(), axisID);
    setParallelDialog(axisID, preProcDialog.usingParallelProcessing());
  }

  /**
   * Closes preprocessing dialog and deletes dialog.
   */
  public void donePreProcDialog(AxisID axisID) {
    PreProcessingDialog preProcDialog;
    if (axisID == AxisID.SECOND) {
      preProcDialog = preProcDialogB;
    }
    else {
      preProcDialog = preProcDialogA;
    }
    savePreProcDialog(preProcDialog, axisID);
    // Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      preProcDialogB = null;
    }
    else {
      preProcDialogA = null;
    }
    preProcDialog = null;
  }

  /**
   * Updates comscripts and edf file
   * @param preProcDialog
   * @param axisID
   */
  public void savePreProcDialog(PreProcessingDialog preProcDialog, AxisID axisID) {
    setAdvanced(preProcDialog.getDialogType(), axisID, preProcDialog
        .isAdvanced());

    // Keep dialog box open until we get good info or it is cancelled
    DialogExitState exitState = preProcDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      updateEraserCom(preProcDialog.getCCDEraserDisplay(), axisID, false);
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setPreProcessingState(ProcessState.COMPLETE, axisID);
        mainPanel.setPreProcessingState(ProcessState.COMPLETE, axisID);
        // Go to the coarse align dialog by default
        openCoarseAlignDialog(axisID);
        // If there are raw stack imod processes open ask the user if they
        // should be closed.
        closeImod(ImodManager.RAW_STACK_KEY, axisID, "raw stack");
        closeImod(ImodManager.ERASED_STACK_KEY, axisID, "fixed stack");
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
        mainPanel.setPreProcessingState(ProcessState.INPROGRESS, axisID);
        // Go to the coarse align dialog by default
        mainPanel.showBlankProcess(axisID);
      }
      saveStorables(axisID);
    }
  }

  /**
   * Open 3dmod to create the manual erase model
   */
  public void imodManualErase(AxisID axisID, Run3dmodMenuOptions menuOptions,
      DialogType dialogType) {
    String eraseModelName = metaData.getDatasetName() + axisID.getExtension()
        + ".erase";
    try {
      if (metaData.getViewType() == ViewType.MONTAGE) {
        imodManager.setFrames(ImodManager.RAW_STACK_KEY, axisID, true);
      }
      imodManager.open(ImodManager.RAW_STACK_KEY, axisID, eraseModelName, true,
          menuOptions);
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
      mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on raw stack", axisID, getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Axis type problem in 3dmod erase", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Get the eraser script parameters from the CCD eraser panel and write them
   * out the eraser{|a|b}.com script
   * @param axisID The axisID to process
   * @param trialMode Set to trial mode if true
   */
  private void updateEraserCom(CcdEraserDisplay display, AxisID axisID,
      boolean trialMode) {
    // Get the user input data from the dialog box. The CCDEraserParam
    // is first initialized from the currently loaded com script to
    // provide deafault values for those not handled by the dialog box
    // get function needs some error checking
    CCDEraserParam ccdEraserParam = comScriptMgr.getCCDEraserParam(axisID);
    display.getParameters(ccdEraserParam);
    ccdEraserParam.setTrialMode(trialMode);
    comScriptMgr.saveEraser(ccdEraserParam, axisID);
  }

  /**
   * Run the eraser script for the specified axis
   * @param axisID
   */
  private void eraser(AxisID axisID, ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, DialogType dialogType,
      CcdEraserDisplay display) {
    updateEraserCom(display, axisID, false);
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    String threadName;
    try {
      threadName = processMgr.eraser(axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute eraser" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Run CCDeraser in trial mode
   * @param axisID
   */
  public void findXrays(AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions, DialogType dialogType,
      CcdEraserDisplay display) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    updateEraserCom(display, axisID, true);
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.eraser(axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute eraser" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Open 3dmod to view the xray model on the raw stack
   */
  public void imodXrayModel(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    String xRayModel = metaData.getDatasetName() + axisID.getExtension()
        + "_peak.mod";
    try {
      imodManager.setPreserveContrast(ImodManager.RAW_STACK_KEY, axisID, true);
      if (metaData.getViewType() == ViewType.MONTAGE) {
        imodManager.setFrames(ImodManager.RAW_STACK_KEY, axisID, true);
      }
      imodManager.open(ImodManager.RAW_STACK_KEY, axisID, xRayModel,
          menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening coarse stack", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod to view the erased stack
   */
  public void imodErasedStack(AxisID axisID,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    if (Utilities.getFile(this, true, axisID, "_fixed.st", "erased stack") == null) {
      return;
    }
    try {
      if (metaData.getViewType() == ViewType.MONTAGE) {
        imodManager.setPieceListFileName(ImodManager.ERASED_STACK_KEY, axisID,
            metaData.getDatasetName() + axisID.getExtension() + ".pl");
      }
      File tiltFile = DatasetFiles.getRawTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(ImodManager.ERASED_STACK_KEY, axisID, tiltFile
            .getName());
      }
      else {
        imodManager.resetTiltFile(ImodManager.ERASED_STACK_KEY, axisID);
      }
      imodManager.open(ImodManager.ERASED_STACK_KEY, axisID,
          run3dmodMenuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening erased stack", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  public void archiveOriginalStack(ProcessSeries processSeries,
      final DialogType dialogType) {
    if (processMgr.inUse(AxisID.ONLY, null)) {
      return;
    }
    archiveOriginalStack(null, processSeries, dialogType);
  }

  /**
   * Archive the orginal stacks during clean up.
   * @param axisID
   */
  private void archiveOriginalStack(AxisID currentAxisID,
      ProcessSeries processSeries, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    // figure out which original stack to archive
    AxisID stackAxisID = currentAxisID;
    if (stackAxisID == null) {
      if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
        stackAxisID = AxisID.FIRST;
      }
      else {
        stackAxisID = AxisID.ONLY;
      }
    }
    // set next process to archiveorig so that the second axis can be done
    if (stackAxisID == AxisID.FIRST) {
      processSeries.setNextProcess(ArchiveorigParam.COMMAND_NAME);
    }
    // else {
    // resetNextProcess(AxisID.ONLY);
    // }
    // check for original stack
    File originalStack = Utilities.getFile(this, false, stackAxisID,
        "_orig.st", "original stack");
    if (!originalStack.exists()) {
      if (stackAxisID == AxisID.FIRST) {
        // Nothing to do on the first axis, so move on to the second axis
        processSeries.startNextProcess(AxisID.ONLY, null);
        return;
      }
      else {
        return;
      }
    }
    // set progress bar and process state
    mainPanel.startProgressBar("Archiving " + stackAxisID + " stack",
        AxisID.ONLY, ProcessName.ARCHIVEORIG);
    processTrack.setCleanUpState(ProcessState.INPROGRESS);
    mainPanel.setCleanUpState(ProcessState.INPROGRESS);
    // create param
    ArchiveorigParam param = new ArchiveorigParam(this, stackAxisID);
    // run process
    try {
      setThreadName(processMgr.archiveOrig(param, processSeries), AxisID.ONLY);
    }
    catch (SystemProcessException e) {
      // resetNextProcess(AxisID.ONLY);
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ArchiveorigParam.COMMAND_NAME
          + " command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY, getManagerKey());
    }
  }

  public void deleteOriginalStack(Command archiveorigParam, String[] output) {
    AxisID axisID;
    ArchiveorigParam.Mode mode = (ArchiveorigParam.Mode) archiveorigParam
        .getCommandMode();
    if (mode == ArchiveorigParam.Mode.AXIS_A) {
      axisID = AxisID.FIRST;
    }
    else if (mode == ArchiveorigParam.Mode.AXIS_B) {
      axisID = AxisID.SECOND;
    }
    else if (mode == ArchiveorigParam.Mode.AXIS_ONLY) {
      axisID = AxisID.ONLY;
    }
    else {
      return;
    }
    File originalStack = Utilities.getFile(this, false, axisID, "_orig.st", "");
    // This function is only run is archiveorig succeeds so this "if" statement
    // should always fail.
    if (!originalStack.exists()
        || output == null
        || output.length != 6
        || !output[4].equals("It is now safe to delete "
            + originalStack.getName())) {
      throw new IllegalStateException(
          "Unexpected result from running archiveorig" + output.toString());
    }
    String[] message = new String[output.length - 2 + 3];
    message[0] = "Result of " + archiveorigParam.getCommandLine() + ":\n";
    int messageIndex = 1;
    for (int i = 2; i < output.length; i++) {
      message[messageIndex++] = output[i] + "\n";
    }
    message[messageIndex++] = "\n";
    message[messageIndex] = "Delete " + originalStack.getAbsolutePath() + "?";
    if (uiHarness.openDeleteDialog(message, AxisID.ONLY, getManagerKey())) {
      System.err.println("Deleting " + originalStack.getAbsolutePath());
      originalStack.delete();
      if (cleanUpDialog != null) {
        cleanUpDialog.setArchiveFields();
      }
    }
    updateArchiveDisplay();
  }

  public String getArchiveInfo(AxisID axisID) {
    File stack = Utilities.getFile(this, false, axisID, ".st", "");
    File originalStack = Utilities.getFile(this, false, axisID, "_orig.st", "");
    File xrayStack = Utilities.getFile(this, false, axisID, "_xray.st.gz", "");
    if (stack == null && originalStack == null || xrayStack == null) {
      throw new IllegalStateException("Unable to get file information");
    }
    if (stack.exists() && !originalStack.exists() && xrayStack.exists()) {
      return stack.getName();
    }
    return null;
  }

  public void updateArchiveDisplay() {
    if (cleanUpDialog == null) {
      return;
    }
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      cleanUpDialog.updateArchiveDisplay(DatasetFiles.getOriginalStack(this,
          AxisID.ONLY).exists());
    }
    else {
      cleanUpDialog.updateArchiveDisplay(DatasetFiles.getOriginalStack(this,
          AxisID.FIRST).exists()
          || DatasetFiles.getOriginalStack(this, AxisID.SECOND).exists());
    }
  }

  /**
   * Replace the raw stack with the fixed stack created from eraser
   * @param axisID
   */
  public void replaceRawStack(AxisID axisID,
      ProcessResultDisplay processResultDisplay, DialogType dialogType) {
    sendMsgProcessStarting(processResultDisplay);
    mainPanel.setProgressBar("Using fixed stack", 1, axisID);
    // Instantiate file objects for the original raw stack and the fixed stack
    String rawStackFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + ".st";
    File rawStack = new File(rawStackFilename);
    String rawStackRename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + "_orig.st";
    File rawRename = new File(rawStackRename);
    File fixedStack = Utilities.getFile(this, true, axisID, "_fixed.st",
        "erased stack");
    if (fixedStack == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!Utilities.isValidStack(fixedStack, this, axisID)) {
      uiHarness
          .openMessageDialog(
              fixedStack.getName() + " is not a valid MRC file.",
              "Entry Error", axisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);

    // Rename the fixed stack to the raw stack file name and save the orginal
    // raw stack to _orig.st if that does not already exist
    try {
      if (!rawRename.exists()) {
        Utilities.renameFile(rawStack, rawRename);
      }
      if (renameXrayStack(axisID)) {
        Utilities.renameFile(fixedStack, rawStack);
      }
      sendMsgProcessSucceeded(processResultDisplay);
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(), "File Rename Error",
          axisID, getManagerKey());
      sendMsgProcessFailed(processResultDisplay);
    }
    closeImod(ImodManager.RAW_STACK_KEY, axisID, "raw stack");
    // An _orig.st file may have been created, so refresh the Clean Up dialog's
    // archive fields.
    if (cleanUpDialog != null) {
      cleanUpDialog.setArchiveFields();
    }
    updateArchiveDisplay();
    mainPanel.stopProgressBar(axisID);
  }

  /**
   * Renames the _xray.st.gz file to {dataset}{axisID}_xray.st.gz.{number}.
   * Numbers start at 1 and go up.
   * @param axisID
   * @return false if rename fails
   */
  private boolean renameXrayStack(AxisID axisID) {
    File xrayStack = new File(propertyUserDir, metaData.getDatasetName()
        + axisID.getExtension() + "_xray.st.gz");
    if (!xrayStack.exists()) {
      return true;
    }
    File userDir = new File(propertyUserDir);
    String[] xrayStackArchives = userDir.list(new XrayStackArchiveFilter());
    int fileNumber = 0;
    if (xrayStackArchives != null) {
      EtomoNumber newFileNumber = new EtomoNumber(EtomoNumber.Type.INTEGER);
      for (int i = 0; i < xrayStackArchives.length; i++) {
        newFileNumber.set(xrayStackArchives[i].substring(xrayStackArchives[i]
            .lastIndexOf('.') + 1));
        if (newFileNumber.isValid() && !newFileNumber.isNull()) {
          fileNumber = Math.max(fileNumber, newFileNumber.getInt());
        }
      }
    }
    File xrayStackArchive = new File(propertyUserDir, metaData.getDatasetName()
        + axisID.getExtension() + "_xray.st.gz" + "."
        + Integer.toString(fileNumber + 1));
    if (xrayStackArchive.exists()) {
      throw new IllegalStateException(xrayStackArchive.getName()
          + " should not exist.");
    }
    try {
      Utilities.renameFile(xrayStack, xrayStackArchive);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "Unable to move "
          + xrayStack.getName() + " to " + xrayStackArchive.getName()
          + ".\nCannot continue.\nFirst run \"mv " + xrayStack.getName() + " "
          + xrayStackArchive.getName() + "\" from the command line.", axisID,
          getManagerKey());
      return false;
    }
    return true;
  }

  /**
   * Bring up a preview 3dmod.
   * @param axisID
   */
  public void imodPreview(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    if (setupDialogExpert == null) {
      return;
    }
    String key = ImodManager.PREVIEW_KEY;
    MetaData previewMetaData = setupDialogExpert.getMetaData();
    imodManager.setPreviewMetaData(previewMetaData);
    File previewWorkingDir = previewMetaData
        .getValidDatasetDirectory(setupDialogExpert.getWorkingDirectory()
            .getAbsolutePath());
    if (previewWorkingDir == null) {
      uiHarness.openMessageDialog(previewMetaData.getInvalidReason(),
          "Raw Image Stack", axisID, getManagerKey());
      return;
    }
    try {
      int previewNumber = imodManager.newImod(key, axisID);
      imodManager.setWorkingDirectory(key, axisID, previewNumber,
          previewWorkingDir);
      imodManager.open(key, axisID, previewNumber, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening raw stack", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open the coarse alignment dialog
   */
  public void openCoarseAlignDialog(AxisID axisID) {
    // Check to see if the com files are present otherwise pop up a dialog
    // box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(metaData, axisID,
        getManagerKey())) {
      mainPanel.showBlankProcess(axisID);
      return;
    }
    setCurrentDialogType(DialogType.COARSE_ALIGNMENT, axisID);
    mainPanel.selectButton(axisID, "Coarse Alignment");
    if (showIfExists(coarseAlignDialogA, coarseAlignDialogB, axisID)) {
      return;
    }
    Utilities.timestamp("new", "CoarseAlignDialog", Utilities.STARTED_STATUS);
    CoarseAlignDialog coarseAlignDialog = CoarseAlignDialog.getInstance(this,
        axisID);
    Utilities.timestamp("new", "CoarseAlignDialog", Utilities.FINISHED_STATUS);
    if (axisID == AxisID.SECOND) {
      coarseAlignDialogB = coarseAlignDialog;
    }
    else {
      coarseAlignDialogA = coarseAlignDialog;
    }
    // Create the dialog box
    comScriptMgr.loadXcorr(axisID);
    comScriptMgr.loadUndistort(axisID);
    coarseAlignDialog.setCrossCorrelationParams(comScriptMgr
        .getTiltxcorrParam(axisID));
    if (metaData.getViewType() == ViewType.MONTAGE) {
      comScriptMgr.loadPreblend(axisID);
    }
    else {
      comScriptMgr.loadPrenewst(axisID);
    }

    if (metaData.getViewType() == ViewType.MONTAGE) {
      BlendmontParam blendmontParam = comScriptMgr.getPreblendParam(axisID);
      coarseAlignDialog.setParams(blendmontParam);
    }
    else {
      NewstParam prenewstParam = comScriptMgr.getPrenewstParam(axisID);
      coarseAlignDialog.setPrenewstParams(prenewstParam);
    }

    coarseAlignDialog.setFiducialessAlignment(metaData
        .isFiducialessAlignment(axisID));
    coarseAlignDialog.setImageRotation(metaData.getImageRotation(axisID));
    coarseAlignDialog.setParameters(getScreenState(axisID));
    mainPanel.showProcess(coarseAlignDialog.getContainer(), axisID);
    setParallelDialog(axisID, coarseAlignDialog.usingParallelProcessing());
  }

  /**
   * Get the parameters from the coarse align process dialog box
   */
  public void doneCoarseAlignDialog(AxisID axisID) {
    // Set a reference to the correct object
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    saveCoarseAlignDialog(coarseAlignDialog, axisID);
    // Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      coarseAlignDialogB = null;
    }
    else {
      coarseAlignDialogA = null;
    }
    coarseAlignDialog = null;
  }

  /**
   * Get the parameters from the coarse align process dialog box
   */
  public void saveCoarseAlignDialog(CoarseAlignDialog coarseAlignDialog,
      AxisID axisID) {
    setAdvanced(coarseAlignDialog.getDialogType(), axisID, coarseAlignDialog
        .isAdvanced());
    DialogExitState exitState = coarseAlignDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      coarseAlignDialog.getParameters(getScreenState(axisID));
      // Get the user input data from the dialog box
      updateXcorrCom(coarseAlignDialog.getTiltXcorrDisplay(), axisID);
      updateBlendmontInXcorrCom(axisID);
      try {
        if (metaData.getViewType() != ViewType.MONTAGE) {
          updatePrenewstCom(coarseAlignDialog.getNewstackDisplay(), axisID);
        }
      }
      catch (InvalidParameterException e) {
        e.printStackTrace();
        uiHarness.openMessageDialog("Unable to update prenewst com:  "
            + e.getMessage(), "Etomo Error", axisID, getManagerKey());
      }
      catch (IOException e) {
        e.printStackTrace();
        uiHarness.openMessageDialog("Unable to update prenewst com:  "
            + e.getMessage(), "Etomo Error", axisID, getManagerKey());
      }
      UIExpertUtilities.INSTANCE.updateFiducialessParams(this,
          coarseAlignDialog, axisID);
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setCoarseAlignmentState(ProcessState.COMPLETE, axisID);
        mainPanel.setCoarseAlignState(ProcessState.COMPLETE, axisID);
        // Go to the fiducial model dialog by default
        if (metaData.isFiducialessAlignment(axisID)) {
          getUIExpert(DialogType.TOMOGRAM_POSITIONING, axisID).openDialog();
          // Check to see if the user wants to keep any coarse aligned imods
          // open
          closeImod(ImodManager.COARSE_ALIGNED_KEY, axisID,
              "coarsely aligned stack");
        }
        else {
          openFiducialModelDialog(axisID);
        }
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
        mainPanel.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
        mainPanel.showBlankProcess(axisID);
      }
      saveStorables(axisID);
    }
  }

  /**
   * Get the parameters from the display and run the cross correlation script
   */
  private void crossCorrelate(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, DialogType dialogType,
      TiltXcorrDisplay display) {
    // Get the parameters from the dialog box
    ConstTiltxcorrParam tiltxcorrParam = updateXcorrCom(display, axisID);
    if (tiltxcorrParam == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    String threadName;
    try {
      BlendmontParam blendmontParam = updateBlendmontInXcorrCom(axisID);
      if (blendmontParam == null) {
        threadName = processMgr.crossCorrelate(tiltxcorrParam, axisID,
            processResultDisplay, processSeries);
      }
      else {
        threadName = processMgr.crossCorrelate(blendmontParam, axisID,
            processResultDisplay, processSeries);
      }
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute xcorr" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * if the b stack hasn't been processed, run extracttilts before running
   * crossCorrelate().
   * @param axisID
   */
  public void preCrossCorrelate(AxisID axisID,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      final DialogType dialogType, TiltXcorrDisplay display) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType, display);
    }
    sendMsgProcessStarting(processResultDisplay);
    sendMsgProcessStarting(processResultDisplay);
    if (axisID == AxisID.SECOND && processBStack()) {
      processSeries.setLastProcess(ProcessName.XCORR.toString());
      extracttilts(axisID, processResultDisplay, processSeries);
      return;
    }
    crossCorrelate(axisID, processResultDisplay, processSeries, dialogType,
        display);
  }

  /**
   * if the b stack hasn't been processed, run extracttilts before running
   * eraser().
   * @param axisID
   */
  public void preEraser(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions,
      final DialogType dialogType, CcdEraserDisplay display) {
    sendMsgProcessStarting(processResultDisplay);
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType, display);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    if (axisID == AxisID.SECOND && processBStack()) {
      processSeries.setLastProcess(ProcessName.ERASER.toString());
      extracttilts(axisID, processResultDisplay, processSeries);
      return;
    }
    eraser(axisID, processResultDisplay, processSeries, dialogType, display);
  }

  private boolean processBStack() {
    AxisID axisID = AxisID.SECOND;
    ConstEtomoNumber bStackProcessed = metaData.getBStackProcessed();
    if (bStackProcessed == null || bStackProcessed.is()) {
      return false;
    }
    File bStack = DatasetFiles.getStack(propertyUserDir, metaData, axisID);
    if (!bStack.exists()) {
      return false;
    }
    comScriptMgr.loadTilt(axisID);
    TiltParam param = comScriptMgr.getTiltParam(axisID);
    param.setFiducialess(metaData.isFiducialess(axisID));
    if (metaData.getViewType() == ViewType.MONTAGE) {
      param.setMontageFullImage();
    }
    else {
      param.setFullImage(bStack);
    }
    UIExpertUtilities.INSTANCE.rollTiltComAngles(this, axisID);
    comScriptMgr.saveTilt(param, axisID);
    metaData.setFiducialess(axisID, param.isFiducialess());
    metaData.setBStackProcessed(true);
    saveStorables(axisID);
    return true;
  }

  private void extracttilts(AxisID axisID,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries) {
    processSeries.setNextProcess(ExtractpiecesParam.COMMAND_NAME);
    if (metaData.getTiltAngleSpecA().getType() != TiltAngleType.EXTRACT
        && processSeries != null) {
      processSeries.startNextProcess(axisID, processResultDisplay);
      return;
    }
    File rawTiltFile = DatasetFiles.getRawTilt(this, axisID);
    if (rawTiltFile.exists() && processSeries != null) {
      processSeries.startNextProcess(axisID, processResultDisplay);
      return;
    }
    String threadName;
    try {
      threadName = processMgr.extracttilts(axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ExtracttiltsParam.COMMAND_NAME;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      if (processSeries != null) {
        processSeries.startNextProcess(axisID, processResultDisplay);
      }
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + ExtracttiltsParam.COMMAND_NAME,
        axisID, ProcessName.EXTRACTTILTS);
  }

  private void extractpieces(AxisID axisID,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    processSeries.setNextProcess(ExtractmagradParam.COMMAND_NAME);
    if (metaData.getViewType() != ViewType.MONTAGE && processSeries != null) {
      processSeries.startNextProcess(axisID, processResultDisplay);
      return;
    }
    File pieceListFile = DatasetFiles.getPieceListFile(this, axisID);
    if (pieceListFile.exists() && processSeries != null) {
      processSeries.startNextProcess(axisID, processResultDisplay);
      return;
    }
    String threadName;
    try {
      threadName = processMgr.extractpieces(axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ExtractpiecesParam.COMMAND_NAME;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      if (processSeries != null) {
        processSeries.startNextProcess(axisID, processResultDisplay);
      }
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + ExtractpiecesParam.COMMAND_NAME,
        axisID, ProcessName.EXTRACTPIECES);
  }

  private void extractmagrad(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    String magGradientFileName = metaData.getMagGradientFile();
    if (magGradientFileName == null || magGradientFileName.matches("\\s*+")
        && processSeries != null) {
      processSeries.startNextProcess(axisID, processResultDisplay);
      return;
    }
    File magGradientFile = DatasetFiles.getMagGradient(this, axisID);
    if (magGradientFile.exists() && processSeries != null) {
      processSeries.startNextProcess(axisID, processResultDisplay);
      return;
    }
    ExtractmagradParam param = new ExtractmagradParam(this, axisID);
    param.setRotationAngle(metaData.getImageRotation(axisID));
    param.setGradientTable(metaData.getMagGradientFile());
    String threadName;
    try {
      threadName = processMgr.extractmagrad(param, axisID,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ExtractmagradParam.COMMAND_NAME;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      if (processSeries != null) {
        processSeries.startNextProcess(axisID, processResultDisplay);
      }
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + ExtractmagradParam.COMMAND_NAME,
        axisID, ProcessName.EXTRACTMAGRAD);
  }

  /**
   * run undistort.com
   * @param axisID
   */
  public void makeDistortionCorrectedStack(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    updateUndistortCom(axisID);
    processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
    mainPanel.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.makeDistortionCorrectedStack(axisID,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute undistort" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return;
    }
    setThreadName(threadName, axisID);
  }

  public void clipStats(AxisID axisID, FileType inputFileType,
      ConstProcessSeries processSeries) {
    ClipParam clipParam = new ClipParam(this, axisID, inputFileType.getFile(
        this, axisID), new File(getPropertyUserDir()), ClipParam.Mode.STATS);
    try {
      threadNameA = processMgr.clipStats(clipParam, processSeries);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run clip stats\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY,
          getManagerKey());
      return;
    }
    mainPanel.startProgressBar("clip stats ", AxisID.ONLY);
  }

  /**
   * Run the coarse alignment script
   */
  public void coarseAlign(AxisID axisID,
      final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions, DialogType dialogType,
      BlendmontDisplay blendmontDisplay, NewstackDisplay newstackDisplay) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    ProcessName processName;
    BlendmontParam blendmontParam = null;
    if (metaData.getViewType() == ViewType.MONTAGE) {
      try {
        blendmontParam = updatePreblendCom(blendmontDisplay, axisID);
      }
      catch (FortranInputSyntaxException e) {
        UIHarness.INSTANCE.openMessageDialog(e.getMessage(),
            "Update Com Error", getManagerKey());
      }
      catch (InvalidParameterException e) {
        UIHarness.INSTANCE.openMessageDialog(e.getMessage(),
            "Update Com Error", getManagerKey());
      }
      catch (IOException e) {
        UIHarness.INSTANCE.openMessageDialog(e.getMessage(),
            "Update Com Error", getManagerKey());
      }
      processName = BlendmontParam.getProcessName(BlendmontParam.Mode.PREBLEND);
    }
    else {
      try {
        if (!updatePrenewstCom(newstackDisplay, axisID)) {
          sendMsgProcessFailedToStart(processResultDisplay);
          return;
        }
        processName = ProcessName.PRENEWST;
      }
      catch (InvalidParameterException e) {
        e.printStackTrace();
        uiHarness.openMessageDialog("Unable to update prenewst com:  "
            + e.getMessage(), "Etomo Error", axisID, getManagerKey());
        return;
      }
      catch (IOException e) {
        e.printStackTrace();
        uiHarness.openMessageDialog("Unable to update prenewst com:  "
            + e.getMessage(), "Etomo Error", axisID, getManagerKey());
        return;
      }
    }
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    String threadName = null;
    try {
      if (metaData.getViewType() == ViewType.MONTAGE) {
        threadName = processMgr.preblend(blendmontParam, axisID,
            processResultDisplay, processSeries);
      }
      else {
        threadName = processMgr.coarseAlign(axisID, processResultDisplay,
            processSeries);
      }
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + processName + axisID.getExtension()
          + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return;
    }
    processSeries.setNextProcess("checkUpdateFiducialModel");
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    setThreadName(threadName, axisID);
  }

  /**
   * Open 3dmod to view the coarsely aligned stack
   */
  public void imodRawStack(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.setOpenLogOff(ImodManager.RAW_STACK_KEY, axisID);
      File tiltFile = DatasetFiles.getRawTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(ImodManager.RAW_STACK_KEY, axisID, tiltFile
            .getName());
      }
      else {
        imodManager.resetTiltFile(ImodManager.RAW_STACK_KEY, axisID);
      }
      imodManager.open(ImodManager.RAW_STACK_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening raw stack", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod to view the coarsely aligned stack
   */
  public void imodCoarseAlign(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.setOpenLogOff(ImodManager.COARSE_ALIGNED_KEY, axisID);
      File tiltFile = DatasetFiles.getRawTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(ImodManager.COARSE_ALIGNED_KEY, axisID,
            tiltFile.getName());
      }
      else {
        imodManager.resetTiltFile(ImodManager.COARSE_ALIGNED_KEY, axisID);
      }
      imodManager.open(ImodManager.COARSE_ALIGNED_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening coarse stack", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Run midas on the raw stack
   */
  public void midasRawStack(AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(this,
        mapCoarseAlignDialog(axisID), axisID)) {
      return;
    }
    if (metaData.getViewType() == ViewType.MONTAGE) {
      processMgr.midasBlendStack(axisID, metaData.getImageRotation(axisID));
    }
    else {
      processMgr.midasRawStack(axisID, metaData.getImageRotation(axisID));
    }
    processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
    mainPanel.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
    sendMsgProcessSucceeded(processResultDisplay);
  }

  /**
   * Run fix edges in Midas
   */
  public void midasFixEdges(AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    processMgr.midasFixEdges(axisID);
    processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
    mainPanel.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
    sendMsgProcessSucceeded(processResultDisplay);
  }

  /**
   * Get the required parameters from the dialog box and update the xcorr.com
   * script
   * @return true if successful in getting the parameters and saving the com
   *         script
   */
  private ConstTiltxcorrParam updateXcorrCom(TiltXcorrDisplay display,
      AxisID axisID) {
    TiltxcorrParam tiltXcorrParam = null;
    try {
      tiltXcorrParam = comScriptMgr.getTiltxcorrParam(axisID);
      display.getParameters(tiltXcorrParam);
      comScriptMgr.saveXcorr(tiltXcorrParam, axisID);
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      uiHarness.openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error",
          axisID, getManagerKey());
      return null;
    }
    catch (NumberFormatException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Align Parameter Syntax Error";
      errorMessage[1] = axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error",
          axisID, getManagerKey());
      return null;
    }
    return tiltXcorrParam;
  }

  /**
   * Update the blendmont command in the xcorr comscript. if blendmont does not
   * have to be run, update the goto param to skip blendmont return blendmont
   * param if blendmont has to be run.
   * @param axisID
   * @return
   */
  private BlendmontParam updateBlendmontInXcorrCom(AxisID axisID) {
    boolean runningBlendmont = false;
    BlendmontParam blendmontParam = null;
    // handle montaging
    if (metaData.getViewType() == ViewType.MONTAGE) {
      blendmontParam = comScriptMgr.getBlendmontParamFromTiltxcorr(axisID);
      GotoParam gotoParam = comScriptMgr.getGotoParamFromTiltxcorr(axisID);
      runningBlendmont = blendmontParam.setBlendmontState();
      if (runningBlendmont) {
        gotoParam.setLabel(BlendmontParam.GOTO_LABEL);
      }
      else {
        gotoParam.setLabel(TiltxcorrParam.GOTO_LABEL);
      }
      comScriptMgr.saveXcorr(gotoParam, axisID);
      comScriptMgr.saveXcorr(blendmontParam, axisID);
    }
    if (!runningBlendmont) {
      return null;
    }
    return blendmontParam;
  }

  /**
   * update undistort.com from xcorr.com
   * @param axisID
   */
  private void updateUndistortCom(AxisID axisID) {
    BlendmontParam blendmontParam = comScriptMgr
        .getBlendmontParamFromTiltxcorr(axisID);
    blendmontParam.setMode(BlendmontParam.Mode.UNDISTORT);
    blendmontParam.setBlendmontState();
    comScriptMgr.saveXcorrToUndistort(blendmontParam, axisID);
  }

  /**
   * Get the prenewst parameters from the dialog box and update the prenewst com
   * script as well as the align com script since binning of the pre-aligned
   * stack has an affect on the scaling of the fiducial model.
   * @param axisID
   * @return
   */
  private boolean updatePrenewstCom(NewstackDisplay display, AxisID axisID)
      throws InvalidParameterException, IOException {
    NewstParam prenewstParam = comScriptMgr.getPrenewstParam(axisID);
    try {
      display.getParameters(prenewstParam);
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "prenewst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Prenewst Parameter Syntax Error", axisID, getManagerKey());
      return false;
    }
    comScriptMgr.savePrenewst(prenewstParam, axisID);
    return true;
  }

  /**
   * Get the set the blendmont parameters and update the preblend com script.
   * @param axisID
   * @return
   */
  private BlendmontParam updatePreblendCom(BlendmontDisplay display,
      AxisID axisID) throws FortranInputSyntaxException,
      InvalidParameterException, IOException {
    BlendmontParam preblendParam = comScriptMgr.getPreblendParam(axisID);
    display.getParameters(preblendParam);
    preblendParam.setBlendmontState();
    comScriptMgr.savePreblend(preblendParam, axisID);
    return preblendParam;
  }

  /**
   * Return the CoarseAlignDialog associated the specified AxisID
   * 
   * @param axisID
   * @return
   */
  private CoarseAlignDialog mapCoarseAlignDialog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return coarseAlignDialogB;
    }
    return coarseAlignDialogA;
  }

  /**
   * Open the fiducial model generation dialog
   */
  public void openFiducialModelDialog(AxisID axisID) {
    // Check to see if the com files are present otherwise pop up a dialog
    // box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(metaData, axisID,
        getManagerKey())) {
      mainPanel.showBlankProcess(axisID);
      return;
    }
    setCurrentDialogType(DialogType.FIDUCIAL_MODEL, axisID);
    mainPanel.selectButton(axisID, "Fiducial Model Gen.");
    if (showIfExists(fiducialModelDialogA, fiducialModelDialogB, axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    Utilities.timestamp("new", "FiducialModelDialog", Utilities.STARTED_STATUS);
    FiducialModelDialog fiducialModelDialog = FiducialModelDialog.getInstance(
        this, axisID, metaData.getAxisType());
    Utilities
        .timestamp("new", "FiducialModelDialog", Utilities.FINISHED_STATUS);
    if (axisID == AxisID.SECOND) {
      fiducialModelDialogB = fiducialModelDialog;
    }
    else {
      fiducialModelDialogA = fiducialModelDialog;
    }
    updateDialog(fiducialModelDialog, axisID);
    // Load the required track{|a|b}.com files, fill in the dialog box
    // params
    // and set it to the appropriate state
    comScriptMgr.loadTrack(axisID);
    // Create a default transferfid object to populate the alignment dialog
    fiducialModelDialog.setTransferFidParams();
    fiducialModelDialog.setBeadtrackParams(comScriptMgr
        .getBeadtrackParam(axisID));
    fiducialModelDialog.setParameters(getScreenState(axisID));
    fiducialModelDialog.setParameters(metaData);
    mainPanel.showProcess(fiducialModelDialog.getContainer(), axisID);
    setParallelDialog(axisID, fiducialModelDialog.usingParallelProcessing());
  }

  /**
   * Save comscripts and the .edf file and delete the dialog.
   */
  public void doneFiducialModelDialog(AxisID axisID) {
    // Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (axisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }
    saveFiducialModelDialog(fiducialModelDialog, axisID);
    // Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      fiducialModelDialogB = null;
    }
    else {
      fiducialModelDialogA = null;
    }
    fiducialModelDialog = null;
  }

  /**
   * Save comscripts and the .edf file and delete the dialog.
   */
  public void saveFiducialModelDialog(FiducialModelDialog fiducialModelDialog,
      AxisID axisID) {
    setAdvanced(fiducialModelDialog.getDialogType(), axisID,
        fiducialModelDialog.isAdvanced());
    DialogExitState exitState = fiducialModelDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      fiducialModelDialog.getParameters(getScreenState(axisID));
      fiducialModelDialog.getTransferFidParams();
      fiducialModelDialog.getParameters(metaData);
      // Get the user input data from the dialog box
      updateTrackCom(fiducialModelDialog.getBeadTrackDisplay(), axisID);
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setFiducialModelState(ProcessState.COMPLETE, axisID);
        mainPanel.setFiducialModelState(ProcessState.COMPLETE, axisID);
        openFineAlignmentDialog(axisID);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
        mainPanel.setFiducialModelState(ProcessState.INPROGRESS, axisID);
        mainPanel.showBlankProcess(axisID);
      }
      saveStorables(axisID);
    }
  }

  /**
   * Open 3dmod in seed mode with model and tilt file
   */
  public void imodFindBeads3d(AxisID axisID, Run3dmodMenuOptions menuOptions,
      ProcessResultDisplay processResultDisplay, String key, String model,
      File tiltFile, DialogType dialogType) {
    sendMsgProcessStarting(processResultDisplay);
    try {
      imodManager.setPreserveContrast(key, axisID, true);
      imodManager.setOpenBeadFixer(key, axisID, true);
      imodManager.setBeadfixerMode(key, axisID, ImodProcess.SEED_MODE);
      imodManager.setOpenLogOff(key, axisID);
      imodManager.setDeleteAllSections(key, axisID, true);
      if (tiltFile != null && tiltFile.exists()) {
        imodManager.setTiltFile(key, axisID, tiltFile.getName());
      }
      else {
        imodManager.resetTiltFile(key, axisID);
      }
      imodManager.open(key, axisID, model, true, menuOptions);
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
      mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
      sendMsgProcessSucceeded(processResultDisplay);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod on "
          + key + " with model: " + model, axisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
    }
  }

  /**
   * Open 3dmod in seed mode with model and tilt file
   */
  public void imodSeedModel(AxisID axisID, Run3dmodMenuOptions menuOptions,
      ProcessResultDisplay processResultDisplay, String key, String model,
      File tiltFile, DialogType dialogType) {
    sendMsgProcessStarting(processResultDisplay);
    try {
      imodManager.setPreserveContrast(key, axisID, true);
      imodManager.setOpenBeadFixer(key, axisID, true);
      imodManager.setAutoCenter(key, axisID, true);
      imodManager.setBeadfixerMode(key, axisID, ImodProcess.SEED_MODE);
      imodManager.setOpenLogOff(key, axisID);
      if (tiltFile != null && tiltFile.exists()) {
        imodManager.setTiltFile(key, axisID, tiltFile.getName());
      }
      else {
        imodManager.resetTiltFile(key, axisID);
      }
      imodManager.open(key, axisID, model, true, menuOptions);
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
      mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
      sendMsgProcessSucceeded(processResultDisplay);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod on "
          + key + " with model: " + model, axisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
    }
  }

  private boolean okToFiducialModelTrack(AxisID axisID) {
    File fiducialFile = DatasetFiles.getFiducialModelFile(this, axisID);
    if (!fiducialFile.exists()) {
      return true;
    }
    long fiducialLastModified = fiducialFile.lastModified();
    ConstEtomoNumber fiducialLastTracked = state.getFidFileLastModified(axisID);
    long seedLastModified = 0;
    File seedFile = DatasetFiles.getSeedFile(this, axisID);
    if (seedFile.exists()) {
      seedLastModified = seedFile.lastModified();
    }
    // If the fid file is more recent and it is not the product of tracking
    // then ask before overwriting it by tracking
    if (fiducialLastModified > seedLastModified
        && !fiducialLastTracked.isNull()
        && fiducialLastModified != fiducialLastTracked.getLong()) {
      return uiHarness
          .openYesNoWarningDialog(
              "If you track fiducials now, you will overwrite the fiducial file you just modified.\nFirst press the "
                  + BeadtrackPanel.USE_MODEL_LABEL
                  + " button.\n\nWARNING:  If you press Yes, you will overwrite your fiducial file.",
              axisID, getManagerKey());
    }
    return true;
  }

  private boolean okToMakeFiducialModelSeedModel(AxisID axisID) {
    File seedFile = DatasetFiles.getSeedFile(this, axisID);
    if (!seedFile.exists()) {
      return true;
    }
    long seedLastModified = seedFile.lastModified();
    ConstEtomoNumber seedLastCopied = state.getSeedFileLastModified(axisID);
    long fiducialLastModified = 0;
    File fiducialFile = DatasetFiles.getFiducialModelFile(this, axisID);
    if (fiducialFile.exists()) {
      fiducialLastModified = fiducialFile.lastModified();
    }
    // If the seed file is more recent then the fid file
    // then ask before overwriting it by copying the fid file into the seed
    // file.
    if (seedLastModified > fiducialLastModified
        && seedLastModified != seedLastCopied.getLong()) {
      return uiHarness
          .openYesNoWarningDialog(
              "If you copy the fiducial file to the seed file now, you will overwrite the seed file you just modified.\nFirst press the "
                  + BeadtrackPanel.TRACK_LABEL
                  + " button.\n\nWARNING:  If you press Yes, you will overwrite your seed file.",
              axisID, getManagerKey());
    }
    return true;
  }

  /**
   * Get the beadtrack parameters from the fiducial model dialog and run the
   * track com script
   */
  public void fiducialModelTrack(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, DialogType dialogType,
      BeadTrackDisplay display) {
    sendMsgProcessStarting(processResultDisplay);
    BeadtrackParam beadtrackParam;
    if ((beadtrackParam = updateTrackCom(display, axisID)) == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!okToFiducialModelTrack(axisID)) {
      return;
    }
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    String threadName;
    try {
      threadName = processMgr.fiducialModelTrack(beadtrackParam, axisID,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute track" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Tracking fiducials", axisID, ProcessName.TRACK);
  }

  /**
   * Using Fiducial Model as Seed
   * @param axisID
   */
  public boolean makeFiducialModelSeedModel(AxisID axisID) {
    boolean retval = true;
    String seedModelFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + ".seed";
    File seedModel = new File(seedModelFilename);
    String fiducialModelFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + ".fid";
    File fiducialModel = new File(fiducialModelFilename);
    if (!fiducialModel.exists()) {
      uiHarness.openMessageDialog("Fiducial model does not exist.",
          BeadtrackPanel.USE_MODEL_LABEL + " failed.", axisID, getManagerKey());
      return false;
    }
    if (!okToMakeFiducialModelSeedModel(axisID)) {
      return false;
    }/*
     * if (seedModel.exists() && seedModel.lastModified() >
     * fiducialModel.lastModified()) { String[] message = new String[3];
     * message[0] = "WARNING: The seed model file is more recent the fiducial
     * model file"; message[1] = "To avoid losing your changes to the seed
     * model file,"; message[2] = "track fiducials before pressing Use
     * Fiducial Model as Seed."; uiHarness.openMessageDialog(message, "Use
     * Fiducial Model as Seed Failed", axisID); return; }
     */
    mainPanel.setProgressBar("Using Fiducial Model as Seed", 1, axisID);
    processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
    mainPanel.setFiducialModelState(ProcessState.INPROGRESS, axisID);
    String origSeedModelFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + "_orig.seed";
    File origSeedModel = new File(origSeedModelFilename);
    // backup original seed model file
    if (seedModel.exists() && origSeedModel.exists()) {
      backupFile(seedModel, axisID);
    }
    try {
      if (seedModel.exists() && !origSeedModel.exists()) {
        Utilities.renameFile(seedModel, origSeedModel);
      }
      // rename fiducial model file to seed model file
      Utilities.renameFile(fiducialModel, seedModel);
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(), "File Rename Error",
          axisID, getManagerKey());
      retval = false;
    }
    try {
      if (imodManager.isOpen(ImodManager.COARSE_ALIGNED_KEY, axisID)) {
        if (seedModel.getName().equals(
            imodManager.getModelName(ImodManager.COARSE_ALIGNED_KEY, axisID))) {
          String[] message = new String[2];
          message[0] = "The old seed model file is open in 3dmod";
          message[1] = "Should it be closed?";
          if (uiHarness.openYesNoDialog(message, axisID, getManagerKey())) {
            imodManager.quit(ImodManager.COARSE_ALIGNED_KEY, axisID);
          }
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
      System.err.println("Axis type exception in replaceRawStack");
      retval = false;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
      retval = false;
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "System Process Exception",
          axisID, getManagerKey());
      retval = false;
    }
    File seedFile = DatasetFiles.getSeedFile(this, axisID);
    if (seedFile.exists()) {
      state.setSeedFileLastModified(axisID, seedFile.lastModified());
    }
    else {
      state.resetSeedFileLastModified(axisID);
    }
    mainPanel.stopProgressBar(axisID);
    state.setSeedingDone(axisID, true);
    if (axisID == AxisID.SECOND) {
      updateDialog(fiducialModelDialogA, AxisID.FIRST);
      if (fiducialModelDialogB != null) {
        fiducialModelDialogB.updateDisplay();
      }
    }
    else {
      updateDialog(fiducialModelDialogB, AxisID.SECOND);
      if (fiducialModelDialogA != null) {
        fiducialModelDialogA.updateDisplay();
      }
    }
    return retval;
  }

  public long getBeadfixerDiameter(AxisID axisID) {
    return Math.round(metaData.getFiducialDiameter() / metaData.getPixelSize()
        / UIExpertUtilities.INSTANCE.getStackBinning(this, axisID, ".preali"));
  }

  public void logTaErrorLogMessage(AxisID axisID) {
    logMessage(TaErrorLog.getInstance(getPropertyUserDir(), axisID), axisID,
        getManagerKey());
  }

  public void getContinuousMessage(String message, AxisID axisID) {
    if (message != null
        && message.indexOf("Tiltalign ran with exit code 0") != -1) {
      processMgr.generateAlignLogs(axisID);
      logTaErrorLogMessage(axisID);
    }
  }

  /**
   * Open 3dmod with the new fidcuial model
   */
  public void imodFixFiducials(AxisID axisID, Run3dmodMenuOptions menuOptions,
      ProcessResultDisplay processResultDisplay, String beadfixerMode,
      String skipList) {
    sendMsgProcessStarting(processResultDisplay);
    // if fix fiducials has already run, don't change auto center
    boolean setAutoCenter = !state.isFixedFiducials(axisID);
    String fiducialModel = metaData.getDatasetName() + axisID.getExtension()
        + ".fid";
    try {
      imodManager
          .setOpenBeadFixer(ImodManager.COARSE_ALIGNED_KEY, axisID, true);
      if (setAutoCenter) {
        imodManager
            .setAutoCenter(ImodManager.COARSE_ALIGNED_KEY, axisID, false);
      }
      imodManager.setSkipList(ImodManager.COARSE_ALIGNED_KEY, axisID, skipList);
      imodManager.setBeadfixerMode(ImodManager.COARSE_ALIGNED_KEY, axisID,
          beadfixerMode);
      imodManager.setOpenLog(ImodManager.COARSE_ALIGNED_KEY, axisID,
          beadfixerMode == ImodProcess.RESIDUAL_MODE, DatasetFiles.getLogName(
              this, axisID, ProcessName.ALIGN));
      File tiltFile = DatasetFiles.getRawTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(ImodManager.COARSE_ALIGNED_KEY, axisID,
            tiltFile.getName());
      }
      else {
        imodManager.resetTiltFile(ImodManager.COARSE_ALIGNED_KEY, axisID);
      }
      if (beadfixerMode.equals(ImodProcess.RESIDUAL_MODE)) {
        imodManager.setContinuousListenerTarget(ImodManager.COARSE_ALIGNED_KEY,
            axisID, this);
      }
      imodManager.open(ImodManager.COARSE_ALIGNED_KEY, axisID, fiducialModel,
          true, menuOptions);
      sendMsgProcessSucceeded(processResultDisplay);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on coarse aligned stack with model: "
              + fiducialModel, axisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
    state.setFixedFiducials(axisID, true);
  }

  /**
   * Update the tilt parameters dependent on the align script - local alignments
   * file - the exclude list
   */
  private void updateTiltCom(ConstTiltalignParam tiltalignParam,
      AxisID currentAxis) {
    comScriptMgr.loadTilt(currentAxis);
    TiltParam tiltParam = comScriptMgr.getTiltParam(currentAxis);
    tiltParam.setFiducialess(metaData.isFiducialess(currentAxis));
    String alignFileExtension = currentAxis.getExtension() + "local.xf";
    if (tiltalignParam.getLocalAlignments().is()) {
      tiltParam.setLocalAlignFile(metaData.getDatasetName()
          + alignFileExtension);
    }
    else {
      tiltParam.setLocalAlignFile("");
    }
    tiltParam.setExcludeList(tiltalignParam.getExcludeList());
    UIExpertUtilities.INSTANCE.rollTiltComAngles(this, currentAxis);
    comScriptMgr.saveTilt(tiltParam, currentAxis);
    metaData.setFiducialess(currentAxis, tiltParam.isFiducialess());
  }

  /**
   * Update the specified track com script
   */
  private BeadtrackParam updateTrackCom(BeadTrackDisplay display, AxisID axisID) {
    if (display == null) {
      uiHarness.openMessageDialog(
          "Can not update track?.com without an active display",
          "Program logic error", axisID, getManagerKey());
      return null;
    }
    BeadtrackParam beadtrackParam;
    try {
      beadtrackParam = comScriptMgr.getBeadtrackParam(axisID);
      display.getParameters(beadtrackParam);
      comScriptMgr.saveTrack(beadtrackParam, axisID);
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      uiHarness.openMessageDialog(errorMessage,
          "Beadtrack Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (NumberFormatException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Beadtrack Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (InvalidEtomoNumberException e) {
      return null;
    }
    return beadtrackParam;
  }

  /**
   * Open the alignment estimation dialog
   */
  public void openFineAlignmentDialog(AxisID axisID) {
    // Check to see if the com files are present otherwise pop up a dialog
    // box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(metaData, axisID,
        getManagerKey())) {
      mainPanel.showBlankProcess(axisID);
      return;
    }
    setCurrentDialogType(DialogType.FINE_ALIGNMENT, axisID);
    mainPanel.selectButton(axisID, "Fine Alignment");
    if (showIfExists(fineAlignmentDialogA, fineAlignmentDialogB, axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    Utilities.timestamp("new", "AlignmentEstimationDialog",
        Utilities.STARTED_STATUS);
    AlignmentEstimationDialog fineAlignmentDialog = new AlignmentEstimationDialog(
        this, axisID);
    Utilities.timestamp("new", "AlignmentEstimationDialog",
        Utilities.FINISHED_STATUS);
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialogB = fineAlignmentDialog;
    }
    else {
      fineAlignmentDialogA = fineAlignmentDialog;
    }

    // Load the required align{|a|b}.com files, fill in the dialog box
    // params and set it to the appropriate state
    comScriptMgr.loadAlign(axisID);
    TiltalignParam tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
    // If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      // upgrade and save param to comscript
      UIExpertUtilities.INSTANCE.upgradeOldAlignCom(this, axisID,
          tiltalignParam);
    }
    fineAlignmentDialog.setParameters(metaData);
    fineAlignmentDialog.setTiltalignParams(tiltalignParam);
    fineAlignmentDialog.setParameters(getScreenState(axisID));
    // Create a default transferfid object to populate the alignment dialog
    mainPanel.showProcess(fineAlignmentDialog.getContainer(), axisID);
    setParallelDialog(axisID, fineAlignmentDialog.usingParallelProcessing());
  }

  /**
   * Calls saveAction for the dialog type. SaveAction() is a way to call the
   * dialog's done function without pressing an exit button.
   * @param dialogType
   * @param axisID
   */
  public void saveCurrentDialog(AxisID axisID) {
    DialogType currentDialogType = getCurrentDialogType(axisID);
    // handle dialogs with experts
    UIExpert expert = getUIExpert(currentDialogType, axisID);
    if (expert != null) {
      expert.saveAction();
      return;
    }
    // handle accessible dialogs
    ProcessDialog dialog = getDialog(currentDialogType, axisID);
    if (dialog != null) {
      dialog.saveAction();
      return;
    }
  }

  /**
   * Call BaseManager.exitProgram(). Call saveDialog. Return the value of
   * BaseManager.exitProgram(). To guarantee that etomo can always exit, catch
   * all unrecognized Exceptions and Errors and return true.
   */
  public boolean exitProgram(AxisID axisID) {
    try {
      if (super.exitProgram(axisID)) {
        endThreads();
        saveParamFile();
        return true;
      }
      return false;
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
  }

  /**
   * When SetupDialog is displayed, save the log properties to user config.
   * Always call BaseManager.saveParamFile after doing this.
   */
  public boolean saveParamFile() throws LogFile.LockException, IOException {
    if (getParameterStore() == null) {
      EtomoDirector.INSTANCE.getUserConfiguration().setLogProperties(
          logPanel.getCurrentFrameProperties());
    }
    return super.saveParamFile();
  }

  public void save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
    saveDialogs();
  }

  /**
   * Saves all dialogs that are not set to null. Called by exitProgram().
   * 
   */
  public void saveDialogs() {
    if (metaData == null) {
      return;
    }
    AxisID firstAxisID = metaData.getAxisType() == AxisType.DUAL_AXIS ? AxisID.FIRST
        : AxisID.ONLY;
    if (preProcDialogA != null) {
      savePreProcDialog(preProcDialogA, firstAxisID);
    }
    if (preProcDialogB != null) {
      savePreProcDialog(preProcDialogB, AxisID.SECOND);
    }
    if (coarseAlignDialogA != null) {
      saveCoarseAlignDialog(coarseAlignDialogA, firstAxisID);
    }
    if (coarseAlignDialogB != null) {
      saveCoarseAlignDialog(coarseAlignDialogB, AxisID.SECOND);
    }
    if (fiducialModelDialogA != null) {
      saveFiducialModelDialog(fiducialModelDialogA, firstAxisID);
    }
    if (fiducialModelDialogB != null) {
      saveFiducialModelDialog(fiducialModelDialogB, AxisID.SECOND);
    }
    if (fineAlignmentDialogA != null) {
      saveAlignmentEstimationDialog(fineAlignmentDialogA, firstAxisID);
    }
    if (fineAlignmentDialogB != null) {
      saveAlignmentEstimationDialog(fineAlignmentDialogB, AxisID.SECOND);
    }

    getUIExpert(DialogType.TOMOGRAM_POSITIONING, AxisID.FIRST).saveDialog(
        DialogExitState.SAVE);
    getUIExpert(DialogType.TOMOGRAM_POSITIONING, AxisID.SECOND).saveDialog(
        DialogExitState.SAVE);
    getUIExpert(DialogType.FINAL_ALIGNED_STACK, AxisID.FIRST).saveDialog(
        DialogExitState.SAVE);
    getUIExpert(DialogType.FINAL_ALIGNED_STACK, AxisID.SECOND).saveDialog(
        DialogExitState.SAVE);
    getUIExpert(DialogType.TOMOGRAM_GENERATION, AxisID.FIRST).saveDialog(
        DialogExitState.SAVE);
    getUIExpert(DialogType.TOMOGRAM_GENERATION, AxisID.SECOND).saveDialog(
        DialogExitState.SAVE);

    if (tomogramCombinationDialog != null) {
      saveTomogramCombinationDialog(tomogramCombinationDialog);
    }
    if (postProcessingDialog != null) {
      savePostProcessing(postProcessingDialog);
    }
    if (cleanUpDialog != null) {
      saveCleanUp(cleanUpDialog);
    }
  }

  /**
   * @param dialogType
   * @param axisID
   * @return a dialog
   */
  private ProcessDialog getDialog(DialogType dialogType, AxisID axisID) {
    if (dialogType == null) {
      return null;
    }
    if (dialogType == DialogType.PRE_PROCESSING) {
      if (axisID == AxisID.SECOND) {
        return preProcDialogB;
      }
      else {
        return preProcDialogA;
      }
    }
    else if (dialogType == DialogType.COARSE_ALIGNMENT) {
      return mapCoarseAlignDialog(axisID);
    }
    else if (dialogType == DialogType.FIDUCIAL_MODEL) {
      if (axisID == AxisID.SECOND) {
        return fiducialModelDialogB;
      }
      else {
        return fiducialModelDialogA;
      }
    }
    else if (dialogType == DialogType.FINE_ALIGNMENT) {
      if (axisID == AxisID.SECOND) {
        return fineAlignmentDialogB;
      }
      else {
        return fineAlignmentDialogA;
      }
    }
    else if (dialogType == DialogType.TOMOGRAM_COMBINATION) {
      return tomogramCombinationDialog;
    }
    else if (dialogType == DialogType.POST_PROCESSING) {
      return postProcessingDialog;
    }
    else if (dialogType == DialogType.CLEAN_UP) {
      return cleanUpDialog;
    }
    return null;
  }

  /**
   * 
   */
  public void doneAlignmentEstimationDialog(AxisID axisID) {
    // Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog;
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialog = fineAlignmentDialogB;
    }
    else {
      fineAlignmentDialog = fineAlignmentDialogA;
    }
    saveAlignmentEstimationDialog(fineAlignmentDialog, axisID);
    // Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialogB = null;
    }
    else {
      fineAlignmentDialogA = null;
    }
    fineAlignmentDialog = null;
  }

  /**
   * 
   */
  public void saveAlignmentEstimationDialog(
      AlignmentEstimationDialog fineAlignmentDialog, AxisID axisID) {
    setAdvanced(fineAlignmentDialog.getDialogType(), axisID,
        fineAlignmentDialog.isAdvanced());
    DialogExitState exitState = fineAlignmentDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      fineAlignmentDialog.getParameters(getScreenState(axisID));
      // Get the user input data from the dialog box
      updateAlignCom(axisID);
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
        mainPanel.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
        mainPanel.showBlankProcess(axisID);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setFineAlignmentState(ProcessState.COMPLETE, axisID);
        mainPanel.setFineAlignmentState(ProcessState.COMPLETE, axisID);
        // Check to see if the user wants to keep any coarse aligned imods
        // open
        closeImod(ImodManager.COARSE_ALIGNED_KEY, axisID,
            "coarsely aligned stack");
        closeImod(ImodManager.FIDUCIAL_MODEL_KEY, axisID, "fiducial model");
        getUIExpert(DialogType.TOMOGRAM_POSITIONING, axisID).openDialog();
      }
      saveStorables(axisID);
    }
  }

  public void closeImods(String key, AxisID axisID, String description) {
    // Check to see if the user wants to keep any imods open
    try {
      if (imodManager.isOpen(key, axisID)) {
        String[] message = new String[2];
        message[0] = description + "(s) are open in 3dmod";
        message[1] = "Should they be closed?";
        if (uiHarness.openYesNoDialog(message, axisID, getManagerKey())) {
          imodManager.quitAll(key, axisID);
        }
      }
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "System Process Exception",
          axisID, getManagerKey());
    }
  }

  private void closeImod(String key, String description) {
    // Check to see if the user wants to keep any imods open
    try {
      if (imodManager.isOpen(key)) {
        String[] message = new String[2];
        message[0] = "The " + description + " is open in 3dmod";
        message[1] = "Should it be closed?";
        if (uiHarness.openYesNoDialog(message, AxisID.ONLY, getManagerKey())) {
          imodManager.quit(key);
        }
      }
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", AxisID.ONLY,
          getManagerKey());
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "System Process Exception",
          AxisID.ONLY, getManagerKey());
    }
  }

  public void closeImod(String key, AxisID axisID, String description) {
    // Check to see if the user wants to keep any imods open
    try {
      if (imodManager.isOpen(key, axisID)) {
        String[] message = new String[2];
        message[0] = "The " + description + " is open in 3dmod";
        message[1] = "Should it be closed?";
        if (uiHarness.openYesNoDialog(message, axisID, getManagerKey())) {
          imodManager.quit(key, axisID);
        }
      }
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "System Process Exception",
          axisID, getManagerKey());
    }
  }

  /**
   * Execute the fine alignment script (align.com) for the appropriate axis This
   * will also reset the fiducialess alignment flag, setFiducialAlign does not
   * need to be called because the necessary copies are called at the end of the
   * align script.
   * 
   * @param the AxisID identifying the axis to align.
   */
  public void fineAlignment(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    sendMsgProcessStarting(processResultDisplay);
    // Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog = (AlignmentEstimationDialog) getDialog(
        DialogType.FINE_ALIGNMENT, axisID);
    if (fineAlignmentDialog != null && !fineAlignmentDialog.isValid()) {
      return;
    }
    ConstTiltalignParam tiltalignParam = updateAlignCom(axisID);
    if (tiltalignParam == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processTrack.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
    mainPanel.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.fineAlignment(tiltalignParam, axisID,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute align" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    metaData.setFiducialessAlignment(axisID, false);
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Aligning stack", axisID, ProcessName.ALIGN);
  }

  /**
   * Open 3dmod with the new fidcuial model
   */
  public void imodViewResiduals(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    String fiducialModel = metaData.getDatasetName() + axisID.getExtension()
        + ".resmod";
    try {
      imodManager.setPreserveContrast(ImodManager.COARSE_ALIGNED_KEY, axisID,
          true);
      imodManager.setBeadfixerMode(ImodManager.COARSE_ALIGNED_KEY, axisID,
          ImodProcess.RESIDUAL_MODE);
      imodManager.setOpenLogOff(ImodManager.COARSE_ALIGNED_KEY, axisID);
      File tiltFile = DatasetFiles.getRawTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(ImodManager.COARSE_ALIGNED_KEY, axisID,
            tiltFile.getName());
      }
      else {
        imodManager.resetTiltFile(ImodManager.COARSE_ALIGNED_KEY, axisID);
      }
      imodManager.open(ImodManager.COARSE_ALIGNED_KEY, axisID, fiducialModel,
          menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on coarse aligned stack with model: "
              + fiducialModel, axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmodv with a model
   */
  public void imodViewModel(AxisID axisID, FileType modelFileType) {
    try {
      imodManager.open(modelFileType.getImodManagerKey(), axisID, modelFileType
          .getFileName(this, axisID));
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod on "
          + modelFileType.getFileName(this, axisID), axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  public void imodFineAlign3dFind(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    //Get the correct ImodManager key.  The file to bring up is whatever file
    //was last used as input for tilt_3dfind.
    String key;
    if (state.isStackUsingNewstOrBlend3dFindOutput(axisID)) {
      key = FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getImodManagerKey();
    }
    else {
      key = FileType.NEWST_OR_BLEND_OUTPUT.getImodManagerKey();
    }
    try {
      File tiltFile = DatasetFiles.getTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(key, axisID, tiltFile.getName());
      }
      else {
        imodManager.resetTiltFile(key, axisID);
      }
      imodManager.open(key, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod on "
          + key, axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod to view the fine aligned stack or the fine aligned stack for
   * findbeads3d.
   * 
   * @param axisID the AxisID to coarse align.
   */
  public void imodFineAlign(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      File tiltFile = DatasetFiles.getTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(ImodManager.FINE_ALIGNED_KEY, axisID, tiltFile
            .getName());
      }
      else {
        imodManager.resetTiltFile(ImodManager.FINE_ALIGNED_KEY, axisID);
      }
      imodManager.open(ImodManager.FINE_ALIGNED_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod on "
          + ImodManager.FINE_ALIGNED_KEY, axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod to view the MTF filter results
   * 
   * @param axisID the AxisID to coarse align.
   */
  public void imodMTFFilter(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      File tiltFile = DatasetFiles.getTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(ImodManager.MTF_FILTER_KEY, axisID, tiltFile
            .getName());
      }
      else {
        imodManager.resetTiltFile(ImodManager.MTF_FILTER_KEY, axisID);
      }
      imodManager.open(ImodManager.MTF_FILTER_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on MTF filter results", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  public void imodCtfCorrection(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      File tiltFile = DatasetFiles.getTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(ImodManager.CTF_CORRECTION_KEY, axisID,
            tiltFile.getName());
      }
      else {
        imodManager.resetTiltFile(ImodManager.CTF_CORRECTION_KEY, axisID);
      }
      imodManager.open(ImodManager.CTF_CORRECTION_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod on "
          + ProcessName.CTF_CORRECTION.toString() + " results", axisID,
          getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  public boolean coordFileExists() {
    return DatasetFiles.getTransferFidCoordFile(this).exists();
  }

  void processSucceeded(AxisID axisID, ProcessName processName) {
    if (processName == ProcessName.TRANSFERFID) {
      FiducialModelDialog fiducialModelDialog;
      if (axisID == AxisID.SECOND) {
        fiducialModelDialog = fiducialModelDialogB;
      }
      else {
        fiducialModelDialog = fiducialModelDialogA;
      }
      if (fiducialModelDialog == null) {
        return;
      }
      fiducialModelDialog.updateDisplay();
    }
  }

  /**
   * Transfer the fiducial to the specified axis
   * 
   * @param destAxisID
   */
  public void transferfid(final AxisID destAxisID,
      final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (destAxisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }
    if (destAxisID != AxisID.ONLY
        && !Utilities.fileExists(this, "fid.xyz",
            (destAxisID == AxisID.FIRST ? AxisID.SECOND : AxisID.FIRST))) {
      uiHarness.openMessageDialog(
          "It is recommended that you run Fine Alignment on axis "
              + (destAxisID == AxisID.FIRST ? "B" : "A") + " at least once",
          "Warning", destAxisID, getManagerKey());
    }
    if (fiducialModelDialog == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    TransferfidParam transferfidParam = new TransferfidParam(this, destAxisID);
    // Setup the default parameters depending upon the axis to transfer
    // the fiducials from
    String datasetName = metaData.getDatasetName();
    transferfidParam.setDatasetName(datasetName);
    if (destAxisID == AxisID.FIRST) {
      transferfidParam.setBToA(true);
    }
    else {
      transferfidParam.setBToA(false);
    }
    // Get any user specified changes
    fiducialModelDialog.getTransferFidParams(transferfidParam);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.transferFiducials(transferfidParam,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute transferfid command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          destAxisID, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    setThreadName(threadName, destAxisID);
    mainPanel.startProgressBar("Transferring fiducials", destAxisID,
        ProcessName.TRANSFERFID);
    updateDialog(fiducialModelDialog, destAxisID);
  }

  /**
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters from
   * the alignment estimation dialog. This also updates the local alignment
   * state of the appropriate tilt files.
   */
  private ConstTiltalignParam updateAlignCom(final AxisID axisID) {
    AlignmentEstimationDialog fineAlignmentDialog = (AlignmentEstimationDialog) getDialog(
        DialogType.FINE_ALIGNMENT, axisID);
    if (fineAlignmentDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update align?.com without an active alignment dialog",
          "Program logic error", axisID, getManagerKey());
      return null;
    }
    TiltalignParam tiltalignParam;
    try {
      tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
      fineAlignmentDialog.getParameters(metaData);
      fineAlignmentDialog.getTiltalignParams(tiltalignParam);
      UIExpertUtilities.INSTANCE.rollAlignComAngles(this, axisID);
      comScriptMgr.saveAlign(tiltalignParam, axisID);
      // Update the tilt.com script with the dependent parameters
      updateTiltCom(tiltalignParam, axisID);
      // update xfproduct in align.com
      XfproductParam xfproductParam = comScriptMgr.getXfproductInAlign(axisID);
      xfproductParam.setScaleShifts(UIExpertUtilities.INSTANCE.getStackBinning(
          this, axisID, ".preali"));
      comScriptMgr.saveXfproductInAlign(xfproductParam, axisID);
      if (fineAlignmentDialog.getExitState() != DialogExitState.SAVE) {
        mainPanel.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
      }
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = except.getNewString();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Tiltalign Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Tiltalign Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    return tiltalignParam;
  }

  public UIExpert getUIExpert(DialogType dialogType, AxisID axisID) {
    if (dialogType == DialogType.TOMOGRAM_POSITIONING) {
      if (axisID == AxisID.SECOND) {
        if (tomogramPositioningExpertB == null) {
          tomogramPositioningExpertB = new TomogramPositioningExpert(this,
              mainPanel, processTrack, axisID);
        }
        return tomogramPositioningExpertB;
      }
      if (tomogramPositioningExpertA == null) {
        tomogramPositioningExpertA = new TomogramPositioningExpert(this,
            mainPanel, processTrack, axisID);
      }
      return tomogramPositioningExpertA;
    }
    else if (dialogType == DialogType.FINAL_ALIGNED_STACK) {
      if (axisID == AxisID.SECOND) {
        if (finalAlignedStackExpertB == null) {
          finalAlignedStackExpertB = new FinalAlignedStackExpert(this,
              mainPanel, processTrack, axisID);
        }
        return finalAlignedStackExpertB;
      }
      if (finalAlignedStackExpertA == null) {
        finalAlignedStackExpertA = new FinalAlignedStackExpert(this, mainPanel,
            processTrack, axisID);
      }
      return finalAlignedStackExpertA;
    }
    else if (dialogType == DialogType.TOMOGRAM_GENERATION) {
      if (axisID == AxisID.SECOND) {
        if (tomogramGenerationExpertB == null) {
          tomogramGenerationExpertB = new TomogramGenerationExpert(this,
              mainPanel, processTrack, axisID);
        }
        return tomogramGenerationExpertB;
      }
      if (tomogramGenerationExpertA == null) {
        tomogramGenerationExpertA = new TomogramGenerationExpert(this,
            mainPanel, processTrack, axisID);
      }
      return tomogramGenerationExpertA;
    }
    return null;
  }

  public SetupDialogExpert getSetupDialogExpert() {
    if (!EtomoDirector.INSTANCE.getArguments().isTest()) {
      throw new IllegalStateException("only for testing");
    }
    return setupDialogExpert;
  }

  /**
   * Run the sample com script
   */
  public ProcessResult createSample(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, ConstTiltParam tiltParam) {
    // Make sure we have a current prexg and _nonfid.xf if fiducialess is
    // selected
    if (metaData.isFiducialessAlignment(axisID)) {
      generateFiducialessTransforms(axisID);
    }
    else {
      try {
        processMgr.setupFiducialAlign(axisID);
      }
      catch (IOException except) {
        except.printStackTrace();
        String[] message = new String[2];
        message[0] = "Problem copying fiducial alignment files";
        message[1] = except.getMessage();
        uiHarness.openMessageDialog(message,
            "Unable to copy fiducial alignment files", axisID, getManagerKey());
      }
    }

    String threadName;
    try {
      threadName = processMgr.createSample(axisID, processResultDisplay,
          processSeries, tiltParam);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute sample" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    mainPanel.startProgressBar("Creating sample tomogram", axisID,
        ProcessName.SAMPLE);
    setThreadName(threadName, axisID);
    return null;
  }

  /**
   * Create a whole tomogram for positioning the tomogram in the volume
   * 
   * @param axisID
   */
  public ProcessResult wholeTomogram(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, ConstNewstParam param) {
    // Make sure we have a current prexg and _nonfid.xf if fiducialess is
    // selected
    if (metaData.isFiducialessAlignment(axisID)) {
      generateFiducialessTransforms(axisID);
    }
    else {
      try {
        processMgr.setupFiducialAlign(axisID);
      }
      catch (IOException except) {
        except.printStackTrace();
        String[] message = new String[2];
        message[0] = "Problem copying fiducial alignment files";
        message[1] = except.getMessage();
        uiHarness.openMessageDialog(message,
            "Unable to copy fiducial alignment files", axisID, getManagerKey());
      }
    }
    String threadName;
    try {
      threadName = processMgr.newst(param, axisID, processResultDisplay,
          processSeries, ProcessName.NEWST);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    return null;
  }

  /**
   * Create a whole tomogram for positioning the tomogram in the volume
   * 
   * @param axisID
   */
  public ProcessResult wholeTomogram(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, BlendmontParam param) {
    // Make sure we have a current prexg and _nonfid.xf if fiducialess is
    // selected
    if (metaData.isFiducialessAlignment(axisID)) {
      generateFiducialessTransforms(axisID);
    }
    else {
      try {
        processMgr.setupFiducialAlign(axisID);
      }
      catch (IOException except) {
        except.printStackTrace();
        String[] message = new String[2];
        message[0] = "Problem copying fiducial alignment files";
        message[1] = except.getMessage();
        uiHarness.openMessageDialog(message,
            "Unable to copy fiducial alignment files", axisID, getManagerKey());
      }
    }
    String threadName;
    try {
      threadName = processMgr.blend(param, axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    return null;
  }

  /**
   * Open 3dmod on the sample volume for the specified axis along with the
   * tomopitch model
   * 
   * @param axisID
   */
  public void imodSample(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      // It is safe to use open contours in all cases.
      imodManager.setOpenContours(ImodManager.SAMPLE_KEY, axisID, true);
      imodManager.setPointLimit(ImodManager.SAMPLE_KEY, axisID, 2);
      imodManager.open(ImodManager.SAMPLE_KEY, axisID, menuOptions);
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening sample reconstruction", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod on the full volume along with the tomopitch model
   * 
   * @param axisID
   */
  public void imodFullSample(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    String tomopitchModelName = "tomopitch" + axisID.getExtension() + ".mod";
    try {
      imodManager.setOpenContours(ImodManager.FULL_VOLUME_KEY, axisID, true);
      imodManager.setPointLimit(ImodManager.FULL_VOLUME_KEY, axisID, 2);
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID, tomopitchModelName,
          true, menuOptions);
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening sample reconstruction", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * 
   */
  public ProcessResult tomopitch(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    sendMsgProcessStarting(processResultDisplay);
    String threadName;
    try {
      threadName = processMgr.tomopitch(axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute tomopitch" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    mainPanel.startProgressBar("Finding sample position", axisID,
        ProcessName.TOMOPITCH);
    setThreadName(threadName, axisID);
    return null;
  }

  /**
   * post processing after a successful process
   * @param axisID
   * @param processName
   * @param processDetails
   */
  public void postProcess(AxisID axisID, ProcessName processName,
      ProcessDetails processDetails, ProcessResultDisplay processResultDisplay) {
    if (processName == ProcessName.ALIGN
        && processResultDisplay == getProcessResultDisplayFactory(axisID)
            .getComputeAlignment()) {
      try {
        imodManager.reopenLog(ImodManager.COARSE_ALIGNED_KEY, axisID);
      }
      catch (AxisTypeException e) {
        uiHarness.openMessageDialog("Unable to reopen log file.\n"
            + e.getMessage(), "3dmod Error", getManagerKey());
      }
      catch (IOException e) {
        uiHarness.openMessageDialog("Unable to reopen log file.\n"
            + e.getMessage(), "3dmod Error", getManagerKey());
      }
      catch (SystemProcessException e) {
        uiHarness.openMessageDialog("Unable to reopen log file.\n"
            + e.getMessage(), "3dmod Error", getManagerKey());
      }
    }
    else if (processName == ProcessName.PATCHCORR) {
      if (tomogramCombinationDialog != null) {
        //backwards compatibility - patchcorr used to create patch_vector.mod
        tomogramCombinationDialog.updatePatchVectorModelDisplay();
      }
    }
  }

  public void tomogramPositioningPostProcess(AxisID axisID,
      ProcessDetails processDetails) {
    ((TomogramPositioningExpert) getUIExpert(DialogType.TOMOGRAM_POSITIONING,
        axisID)).postProcess(processDetails, state);
  }

  /**
   * processing done after an unsuccessful process
   * @param axisID
   * @param processName
   * @param processDetails
   */
  public void errorProcess(AxisID axisID, ProcessName processName,
      ProcessDetails processDetails) {
    if (processName == ProcessName.COMBINE) {
      if (tomogramCombinationDialog != null) {
        tomogramCombinationDialog.updatePatchVectorModelDisplay();
      }
    }
  }

  public void msgPatchVectorCreated() {
    if (tomogramCombinationDialog != null) {
      tomogramCombinationDialog.updatePatchVectorModelDisplay();
    }
  }

  public void setTomopitchOutput(AxisID axisID) {
    ((TomogramPositioningExpert) getUIExpert(DialogType.TOMOGRAM_POSITIONING,
        axisID)).setTomopitchOutput();
  }

  public void setEnabledTiltParameters(AxisID axisID) {
    ((FinalAlignedStackExpert) getUIExpert(DialogType.FINAL_ALIGNED_STACK,
        axisID)).setEnabledTiltParameters();
    ((TomogramGenerationExpert) getUIExpert(DialogType.TOMOGRAM_GENERATION,
        axisID)).setEnabledTiltParameters();
  }

  /**
   * Compute the final alignment from the updated parameters in the positioning
   * dialog.
   * 
   * @param axisID
   */
  public ProcessResult finalAlign(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, ConstTiltalignParam tiltalignParam) {
    String threadName;
    try {
      threadName = processMgr.fineAlignment(tiltalignParam, axisID,
          processResultDisplay, processSeries);
      metaData.setFiducialessAlignment(axisID, false);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute align" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    mainPanel.startProgressBar("Calculating final alignment", axisID,
        ProcessName.ALIGN);
    setThreadName(threadName, axisID);
    return null;
  }

  /**
   * Generate the prexg and _nonfid.xf for the specified axis and setup the
   * transform files for fiducialless mode
   * @param axisID
   */
  private void generateFiducialessTransforms(AxisID axisID) {
    try {
      processMgr.generatePreXG(axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to generate prexg";
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message, "Unable to generate prexg", axisID,
          getManagerKey());
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to generate prexg";
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message, "Unable to generate prexg", axisID,
          getManagerKey());
    }
    try {
      processMgr.generateNonFidXF(axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to generate _nonfid.xf";
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message, "Unable to generate _nonfid.xf",
          axisID, getManagerKey());
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to generate _nonfid.xf";
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message, "Unable to generate _nonfid.xf",
          axisID, getManagerKey());
    }
    try {
      processMgr.setupNonFiducialAlign(axisID);
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to setup fiducialless align files";
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message,
          "Unable to setup fiducialless align files", axisID, getManagerKey());
    }
    catch (InvalidParameterException except) {
      String[] message = new String[2];
      message[0] = "Unable to setup fiducialless align files";
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message,
          "Unable to setup fiducialless align files", axisID, getManagerKey());
    }
  }

  public void makeRawtltFile(AxisID axisID) throws IOException,
      InvalidParameterException {
    File rawtlt = new File(propertyUserDir, metaData.getDatasetName()
        + axisID.getExtension() + ".rawtlt");
    // backing up .rawtlt, which is currently unnecessary because this function
    // is only called when .rawtlt doesn't exist
    Utilities.renameFile(rawtlt, new File(propertyUserDir, metaData
        .getDatasetName()
        + axisID.getExtension() + ".rawtlt~"));

    BufferedWriter bufferedWriter = null;
    try {
      bufferedWriter = new BufferedWriter(new FileWriter(rawtlt));
      TiltAngleSpec tiltAngleSpec = null;
      if (axisID == AxisID.SECOND) {
        tiltAngleSpec = metaData.getTiltAngleSpecB();
      }
      else {
        tiltAngleSpec = metaData.getTiltAngleSpecA();
      }
      double startingAngle = tiltAngleSpec.getRangeMin();
      double step = tiltAngleSpec.getRangeStep();
      MRCHeader rawStackHeader = MRCHeader.getInstance(this, axisID, ".st",
          getManagerKey());
      if (!rawStackHeader.read()) {
        String message = "Unable to create " + rawtlt.getAbsolutePath();
        uiHarness.openMessageDialog(message, "Unable to create raw tilt file",
            axisID, getManagerKey());
        throw new IOException(message);
      }
      int sections = rawStackHeader.getNSections();
      for (int curSection = 0; curSection < sections; curSection++) {
        bufferedWriter.write(Double.toString(startingAngle
            + (step * curSection)));
        bufferedWriter.newLine();
      }
    }
    catch (IOException except) {
      String[] message = new String[2];
      message[0] = "Unable to create " + rawtlt.getAbsolutePath();
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message, "Unable to create raw tilt file",
          axisID, getManagerKey());
      throw except;
    }
    catch (InvalidParameterException except) {
      except.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to create " + rawtlt.getAbsolutePath();
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message, "Unable to create raw tilt file",
          axisID, getManagerKey());
      throw except;
    }

    if (bufferedWriter != null) {
      bufferedWriter.close();
    }
  }

  public void setEnabledFixEdgesWithMidas(AxisID axisID) {
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    if (coarseAlignDialog == null) {
      return;
    }
    coarseAlignDialog.setEnabledFixEdgesMidasButton();
  }

  /**
   * Update the newst.com from the display.  Reads metaData.
   * 
   * @param axisID
   * @return true if successful
   */
  public ConstNewstParam updateNewstCom(NewstackDisplay display, AxisID axisID) {
    // Set a reference to the correct object
    if (display == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not update newst?.com without an active display",
          "Program logic error", axisID, getManagerKey());
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
      display.getParameters(newstParam);
      comScriptMgr.saveNewst(newstParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Newst Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Newst Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to update newst com:  "
          + e.getMessage(), "Etomo Error", axisID, getManagerKey());
      return null;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to update newst com:  "
          + e.getMessage(), "Etomo Error", axisID, getManagerKey());
      return null;
    }
    return newstParam;
  }

  /**
   * Update the newst3dfind.com from the display.  Reads metaData.
   * 
   * @param axisID
   * @return true if successful
   */
  public ConstNewstParam updateNewst3dFindCom(NewstackDisplay display,
      AxisID axisID) {
    // Set a reference to the correct object
    if (display == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not update newst3dfind?.com without an active display",
          "Program logic error", axisID, getManagerKey());
      return null;
    }
    NewstParam newstParam = null;
    try {
      newstParam = comScriptMgr.getNewstParamFromNewst3dFind(axisID);
      display.getParameters(newstParam);
      comScriptMgr.saveNewst3dFind(newstParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Newst Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Newst Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to update newst 3dfind com:  "
          + e.getMessage(), "Etomo Error", axisID, getManagerKey());
      return null;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to update newst 3dfind com:  "
          + e.getMessage(), "Etomo Error", axisID, getManagerKey());
      return null;
    }
    //Update mrctaper
    MrcTaperParam mrcTaperParam = comScriptMgr
        .getMrcTaperParamFromNewst3dFind(axisID);
    mrcTaperParam.setInputFile(FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT
        .getFileName(this, axisID));
    comScriptMgr.saveNewst3dFind(mrcTaperParam, axisID);
    return newstParam;
  }

  /**
   * Get the set the blendmont parameters and update the blend.com script.
   * @param axisID
   * @return
   */
  public BlendmontParam updateBlendCom(BlendmontDisplay display, AxisID axisID)
      throws FortranInputSyntaxException, InvalidParameterException,
      IOException {
    BlendmontParam blendParam;
    blendParam = comScriptMgr.getBlendParam(axisID);
    display.getParameters(blendParam);
    blendParam.setMode(BlendmontParam.Mode.BLEND);
    blendParam.setBlendmontState();
    comScriptMgr.saveBlend(blendParam, axisID);
    return blendParam;
  }

  /**
   * Get the set the blendmont parameters and update the blend3dfin.com script.
   * @param axisID
   * @return
   */
  public BlendmontParam updateBlend3dFindCom(BlendmontDisplay display,
      AxisID axisID) throws FortranInputSyntaxException,
      InvalidParameterException, IOException {
    //Update blendmont
    BlendmontParam blendParam = comScriptMgr
        .getBlendParamFromBlend3dFind(axisID);
    display.getParameters(blendParam);
    blendParam.setMode(BlendmontParam.Mode.BLEND_3DFIND);
    blendParam.setBlendmontState();
    blendParam.setImageOutputFile(FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT
        .getFileName(this, axisID));
    comScriptMgr.saveBlend3dFind(blendParam, axisID);
    //Update mrctaper
    MrcTaperParam mrcTaperParam = comScriptMgr
        .getMrcTaperParamFromBlend3dFind(axisID);
    mrcTaperParam.setInputFile(FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT
        .getFileName(this, axisID));
    comScriptMgr.saveBlend3dFind(mrcTaperParam, axisID);
    return blendParam;
  }

  /**
   * Run blend_3dfind.com
   * @param processResultDisplay
   * @param processSeries
   * @param deferred3dmodButton
   * @param axisID
   * @param run3dmodMenuOptions
   * @param dialogType
   * @param fiducialessParams
   * @param blendmontDisplay
   */
  public void blend3dFind(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      AxisID axisID, Run3dmodMenuOptions run3dmodMenuOptions,
      DialogType dialogType, BlendmontDisplay blendmontDisplay) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // Get the user input from the dialog
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(this, state
        .getStackImageRotation(axisID).getFloat(), state
        .isNewstFiducialessAlignment(axisID), axisID)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    BlendmontParam param = null;
    try {
      param = updateBlend3dFindCom(blendmontDisplay, axisID);
    }
    catch (FortranInputSyntaxException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Update Com Error",
          getManagerKey());
    }
    catch (InvalidParameterException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Update Com Error",
          getManagerKey());
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Update Com Error",
          getManagerKey());
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    // Make sure we have a current prexg and _nonfid.xf if fiducialess is
    // selected
    if (metaData.isFiducialessAlignment(axisID)) {
      generateFiducialessTransforms(axisID);
    }
    else {
      try {
        processMgr.setupFiducialAlign(axisID);
      }
      catch (IOException except) {
        except.printStackTrace();
        String[] message = new String[2];
        message[0] = "Problem copying fiducial alignment files";
        message[1] = except.getMessage();
        uiHarness.openMessageDialog(message,
            "Unable to copy fiducial alignment files", axisID, getManagerKey());
      }
    }
    String threadName = null;
    try {
      threadName = processMgr.blend(param, axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute blend_3dfind";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Run blend.com
   * @param processResultDisplay
   * @param processSeries
   * @param deferred3dmodButton
   * @param axisID
   * @param run3dmodMenuOptions
   * @param dialogType
   * @param fiducialessParams
   * @param blendmontDisplay
   */
  public void blend(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      AxisID axisID, Run3dmodMenuOptions run3dmodMenuOptions,
      DialogType dialogType, FiducialessParams fiducialessParams,
      BlendmontDisplay blendmontDisplay) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // Get the user input from the dialog
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(this,
        fiducialessParams, axisID)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    BlendmontParam param = null;
    try {
      param = updateBlendCom(blendmontDisplay, axisID);
    }
    catch (FortranInputSyntaxException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Update Com Error",
          getManagerKey());
    }
    catch (InvalidParameterException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Update Com Error",
          getManagerKey());
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(e.getMessage(), "Update Com Error",
          getManagerKey());
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    // Make sure we have a current prexg and _nonfid.xf if fiducialess is
    // selected
    if (metaData.isFiducialessAlignment(axisID)) {
      generateFiducialessTransforms(axisID);
    }
    else {
      try {
        processMgr.setupFiducialAlign(axisID);
      }
      catch (IOException except) {
        except.printStackTrace();
        String[] message = new String[2];
        message[0] = "Problem copying fiducial alignment files";
        message[1] = except.getMessage();
        uiHarness.openMessageDialog(message,
            "Unable to copy fiducial alignment files", axisID, getManagerKey());
      }
    }
    String threadName = null;
    try {
      threadName = processMgr.blend(param, axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute blend";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
    }
    setThreadName(threadName, axisID);
  }

  public boolean equalsBinning(AxisID axisID, int binning, FileType fileType) {
    long fileBinning = UIExpertUtilities.INSTANCE.getStackBinning(this, axisID,
        fileType);
    return binning == fileBinning;
  }

  public double calcUnbinnedBeadDiameterPixels() {
    return (double) (Math.round(metaData.getFiducialDiameter()
        / metaData.getPixelSize() * 100)) / 100.0;
  }

  /**
   * Run newst.com.
   * @param processResultDisplay
   * @param processSeries
   * @param deferred3dmodButton
   * @param axisID
   * @param run3dmodMenuOptions
   * @param dialogType
   * @param fiducialessParams
   * @param newstackDisplay
   */
  public void newst(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      AxisID axisID, Run3dmodMenuOptions run3dmodMenuOptions,
      DialogType dialogType, FiducialessParams fiducialessParams,
      NewstackDisplay newstackDisplay, ProcessName processName) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // Get the user input from the dialog
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(this,
        fiducialessParams, axisID)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    ConstNewstParam param = null;
    param = updateNewstCom(newstackDisplay, axisID);
    if (param == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    // Make sure we have a current prexg and _nonfid.xf if fiducialess is
    // selected
    if (metaData.isFiducialessAlignment(axisID)) {
      generateFiducialessTransforms(axisID);
    }
    else {
      try {
        processMgr.setupFiducialAlign(axisID);
      }
      catch (IOException except) {
        except.printStackTrace();
        String[] message = new String[2];
        message[0] = "Problem copying fiducial alignment files";
        message[1] = except.getMessage();
        uiHarness.openMessageDialog(message,
            "Unable to copy fiducial alignment files", axisID, getManagerKey());
      }
    }
    String threadName = null;
    try {
      threadName = processMgr.newst(param, axisID, processResultDisplay,
          processSeries, processName);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Run newst_3dfind.com.
   * @param processResultDisplay
   * @param processSeries
   * @param deferred3dmodButton
   * @param axisID
   * @param run3dmodMenuOptions
   * @param dialogType
   * @param fiducialessParams
   * @param newstackDisplay
   */
  public void newst3dFind(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      AxisID axisID, Run3dmodMenuOptions run3dmodMenuOptions,
      DialogType dialogType, NewstackDisplay newstackDisplay) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // Get the user input from the dialog
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(this, state
        .getStackImageRotation(axisID).getFloat(), state
        .isNewstFiducialessAlignment(axisID), axisID)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    ConstNewstParam param = null;
    param = updateNewst3dFindCom(newstackDisplay, axisID);
    if (param == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    // Make sure we have a current prexg and _nonfid.xf if fiducialess is
    // selected
    if (metaData.isFiducialessAlignment(axisID)) {
      generateFiducialessTransforms(axisID);
    }
    else {
      try {
        processMgr.setupFiducialAlign(axisID);
      }
      catch (IOException except) {
        except.printStackTrace();
        String[] message = new String[2];
        message[0] = "Problem copying fiducial alignment files";
        message[1] = except.getMessage();
        uiHarness.openMessageDialog(message,
            "Unable to copy fiducial alignment files", axisID, getManagerKey());
      }
    }
    String threadName = null;
    try {
      threadName = processMgr.newst(param, axisID, processResultDisplay,
          processSeries, ProcessName.NEWST_3D_FIND);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Run newst_3dfind.com.
   * @param processResultDisplay
   * @param processSeries
   * @param deferred3dmodButton
   * @param axisID
   * @param run3dmodMenuOptions
   * @param dialogType
   * @param fiducialessParams
   * @param newstackDisplay
   */
  public void findBeads3d(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      AxisID axisID, Run3dmodMenuOptions run3dmodMenuOptions,
      DialogType dialogType, FindBeads3dDisplay display) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // Get the user input from the dialog
    if (!UIExpertUtilities.INSTANCE.updateFiducialessParams(this, state
        .getStackImageRotation(axisID).getFloat(), state
        .isNewstFiducialessAlignment(axisID), axisID)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    ConstFindBeads3dParam param = null;
    param = updateFindBeads3dCom(display, axisID);
    if (param == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    String threadName = null;
    try {
      threadName = processMgr.findBeads3d(param, axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar(ProcessName.FIND_BEADS_3D.toString(), axisID,
        ProcessName.FIND_BEADS_3D);
  }

  public FindBeads3dParam updateFindBeads3dCom(FindBeads3dDisplay display,
      AxisID axisID) {
    FindBeads3dParam param = comScriptMgr.getFindBeads3dParam(axisID);
    if (!state.isTrackLightBeadsNull(axisID)) {
      param.setLightBeads(state.isTrackLightBeads(axisID));
    }
    else {
      //backwards compatibility
      //Get light beads from track.com.
      BeadtrackParam beadtrackParam = comScriptMgr.getBeadtrackParam(axisID);
    }
    display.getParameters(param);
    comScriptMgr.saveFindBeads3d(param, axisID);
    return param;
  }

  private void sendMsg(ProcessResult displayState,
      ProcessResultDisplay processResultDisplay) {
    if (displayState == null || processResultDisplay == null) {
      return;
    }
    processResultDisplay.msg(displayState);
  }

  public boolean isAxisBusy(AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    return processMgr.inUse(axisID, processResultDisplay);
  }

  /**
   */
  public ProcessResult mtffilter(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    String threadName;
    try {
      threadName = processMgr.mtffilter(axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute mtffilter" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    return null;
  }

  public ProcessResult ctfPlotter(AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    try {
      processMgr.ctfPlotter(axisID, processResultDisplay);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute "
          + ProcessName.CTF_PLOTTER.getComscript(axisID);
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    processTrack.setFinalAlignedStackState(ProcessState.INPROGRESS, axisID);
    mainPanel.setFinalAlignedStackState(ProcessState.INPROGRESS, axisID);
    //No way to know when this is done, so I'm assuming that it succeeded.
    return null;
  }

  public ProcessResult ctfCorrection(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    String threadName;
    try {
      threadName = processMgr.ctfCorrection(axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute "
          + ProcessName.CTF_CORRECTION.getComscript(axisID);
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    processTrack.setFinalAlignedStackState(ProcessState.INPROGRESS, axisID);
    mainPanel.setFinalAlignedStackState(ProcessState.INPROGRESS, axisID);
    return null;
  }

  public ProcessResult sampleTilt(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, TiltParam tiltParam) {
    String threadName;
    try {
      threadName = processMgr.tilt(axisID, processResultDisplay, processSeries,
          tiltParam, null);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute tilt" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    return null;
  }

  public void reprojectModelAction(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, TiltDisplay display,
      AxisID axisID, DialogType dialogType) {
    if (display == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    reprojectModel(processResultDisplay, processSeries, display, axisID,
        dialogType);
  }

  public void tilt3dFindAction(ProcessResultDisplay tilt,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, TiltDisplay display,
      AxisID axisID, DialogType dialogType) {
    if (display == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    if (display.isParallelProcess()) {
      splittilt3dFind(tilt, processSeries, display, axisID, dialogType);
    }
    else {
      tilt3dFind(tilt, processSeries, display, axisID, dialogType);
    }
  }

  public void tiltAction(ProcessResultDisplay tilt,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, TiltDisplay display,
      AxisID axisID, DialogType dialogType) {
    if (display == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    if (display.isParallelProcess()) {
      splittilt(tilt, processSeries, display, axisID, dialogType);
    }
    else {
      tilt(tilt, processSeries, display, axisID, dialogType);
    }
  }

  /**
   * Open 3dmod on the current test volume
   * 
   * @param axisID
   */
  public void imodTestVolume(Run3dmodMenuOptions menuOptions, AxisID axisID,
      TrialTiltDisplay display) {
    if (display == null) {
      return;
    }
    imodTestVolume(axisID, menuOptions, display.getTrialTomogramName());
  }

  public void commitTestVolume(ProcessResultDisplay processResultDisplay,
      AxisID axisID, TrialTiltDisplay display) {
    if (display == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    sendMsg(commitTestVolume(axisID, processResultDisplay, display
        .getTrialTomogramName()), processResultDisplay);
  }

  public void trialAction(ProcessResultDisplay trial,
      ProcessSeries processSeries, TrialTiltDisplay display, AxisID axisID,
      DialogType dialogType) {
    if (display == null) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    String trialTomogramName = display.getTrialTomogramName();
    if (trialTomogramName == "") {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Missing trial tomogram filename:";
      errorMessage[1] = "A filename for the trial tomogram must be entered in the Trial"
          + " tomogram filename edit box.";
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return;
    }
    if (!display.containsTrialTomogramName(trialTomogramName)) {
      display.addTrialTomogramName(trialTomogramName);
    }
    if (display.isParallelProcess()) {
      splitTrialTilt(trial, processSeries, display, axisID, dialogType);
    }
    else {
      trialTilt(trial, processSeries, display, axisID, dialogType);
    }
  }

  private void splittilt3dFind(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, TiltDisplay display, AxisID axisID,
      DialogType dialogType) {
    if (display == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstTiltParam tiltParam = updateTilt3dFindCom(display, axisID);
    if (tiltParam == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    SplittiltParam splittiltParam = updateSplittiltParam(display, axisID);
    if (splittiltParam == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    ProcessResult processResult = splittilt(axisID, processResultDisplay,
        processSeries, splittiltParam, dialogType);
    if (processResult == null) {
      processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString(),
          ProcessName.TILT_3D_FIND);
    }
    sendMsg(processResult, processResultDisplay);
  }

  private void splittilt(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, TiltDisplay display, AxisID axisID,
      DialogType dialogType) {
    if (display == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstTiltParam tiltParam = updateTiltCom(display, axisID);
    if (tiltParam == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    SplittiltParam splittiltParam = updateSplittiltParam(display, axisID);
    if (splittiltParam == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    ProcessResult processResult = splittilt(axisID, processResultDisplay,
        processSeries, splittiltParam, dialogType);
    if (processResult == null) {
      processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString(),
          ProcessName.TILT);
    }
    sendMsg(processResult, processResultDisplay);
  }

  private void splitTrialTilt(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, TrialTiltDisplay display, AxisID axisID,
      DialogType dialogType) {
    if (display == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstTiltParam tiltParam = updateTrialTiltCom(display, axisID);
    if (tiltParam == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    SplittiltParam splittiltParam = updateSplittiltParam(display, axisID);
    if (splittiltParam == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    ProcessResult processResult = splittilt(axisID, processResultDisplay,
        processSeries, splittiltParam, dialogType);
    if (processResult == null) {
      processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString(),
          ProcessName.TILT);
    }
    sendMsg(processResult, processResultDisplay);
  }

  /**
   * Run the tilt_3dfind command script for the specified axis
   */
  private void reprojectModel(ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, TiltDisplay display, AxisID axisID,
      DialogType dialogType) {
    if (display == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    File tilt3dFindReprojectCom = new File(getPropertyUserDir(),
        ProcessName.TILT_3D_FIND_REPROJECT.getComscript(axisID));
    if (!tilt3dFindReprojectCom.exists()) {
      UIHarness.INSTANCE.openMessageDialog(tilt3dFindReprojectCom.getName()
          + " does not exist.  Run "
          + FinalAlignedStackDialog.getTilt3dFindButtonLabel()
          + " before running "
          + FinalAlignedStackDialog.getReprojectModelButtonLabel(),
          "Entry Error", axisID, getManagerKey());
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    ConstTiltParam param = updateTilt3dFindReprojectCom(display, axisID);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    sendMsg(tilt3dFindReprojectProcess(axisID, processResultDisplay,
        processSeries, param, "Reprojecting Model",
        ProcessName.TILT_3D_FIND_REPROJECT), processResultDisplay);
  }

  /**
   * Run the tilt_3dfind command script for the specified axis
   */
  private void tilt3dFind(ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, TiltDisplay display, AxisID axisID,
      DialogType dialogType) {
    if (display == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstTiltParam param = updateTilt3dFindCom(display, axisID);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    sendMsg(tilt3dFindProcess(axisID, processResultDisplay, processSeries,
        param, null, ProcessName.TILT_3D_FIND), processResultDisplay);
  }

  /**
   * Run the tilt command script for the specified axis
   */
  private void tilt(ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, TiltDisplay display, AxisID axisID,
      DialogType dialogType) {
    if (display == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstTiltParam param = updateTiltCom(display, axisID);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    sendMsg(tiltProcess(axisID, processResultDisplay, processSeries, param,
        null), processResultDisplay);
  }

  /**
   * Start a tilt process in trial mode
   * 
   * @param axisID
   */
  private void trialTilt(ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, TrialTiltDisplay display,
      AxisID axisID, DialogType dialogType) {
    if (display == null) {
      return;
    }
    sendMsgProcessStarting(processResultDisplay);
    ConstTiltParam param = updateTrialTiltCom(display, axisID);
    if (param == null) {
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    sendMsg(tiltProcess(axisID, processResultDisplay, processSeries, param,
        null), processResultDisplay);
  }

  /**
   * Tilt process initiator. Since tilt can be started from multiple points in
   * the process chain we need separate the execution from the parameter
   * collection and state updating
   * 
   * @param axisID
   */
  private ProcessResult tilt3dFindReprojectProcess(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, ConstTiltParam param,
      String processTitle, ProcessName processName) {
    String threadName;
    try {
      threadName = processMgr.tilt3dFindReproject(axisID, processResultDisplay,
          processSeries, param, processTitle, processName);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute tilt_3dfind" + axisID.getExtension()
          + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar(ProcessName.TILT_3D_FIND_REPROJECT.toString(),
        axisID, ProcessName.TILT_3D_FIND_REPROJECT);
    return null;
  }

  /**
   * Tilt_3dfind process initiator. Since tilt 3dfind can be started from multiple points in
   * the process chain we need separate the execution from the parameter
   * collection and state updating
   * 
   * @param axisID
   */
  private ProcessResult tilt3dFindProcess(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, ConstTiltParam param,
      String processTitle, ProcessName processName) {
    String threadName;
    try {
      threadName = processMgr.tilt3dFind(axisID, processResultDisplay,
          processSeries, param, processTitle, processName);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute tilt_3dfind" + axisID.getExtension()
          + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    return null;
  }

  /**
   * Tilt process initiator. Since tilt can be started from multiple points in
   * the process chain we need separate the execution from the parameter
   * collection and state updating
   * 
   * @param axisID
   */
  private ProcessResult tiltProcess(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, ConstTiltParam param,
      String processTitle) {
    String threadName;
    try {
      threadName = processMgr.tilt(axisID, processResultDisplay, processSeries,
          param, processTitle);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute tilt" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID, getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    return null;
  }

  /**
   * Update the non-dialog dependent parts of the tilt param in
   * tilt_3dfind_reproject.com.
   * @param param
   * @param axisID
   */
  private void getParametersForTilt3dFindReproject(TiltParam param,
      AxisID axisID) {
    param.setProjectModel(FileType.FIND_BEADS_3D_OUTPUT_MODEL);
    param.setOutputFile(FileType.CCD_ERASER_BEADS_INPUT_MODEL.getFileName(this,
        axisID));
    param.setProcessName(ProcessName.TILT_3D_FIND_REPROJECT);
  }

  /**
   * Backup tilt_3dfind_reproject.com.
   * Copy tilt_3dfind_reproject.com from tilt_3dfind.com.  Modify it so that it
   * is correct for reprojecting and save it.
   * @param axisID
   */
  public void copyTilt3dFindReprojectCom(AxisID axisID) {
    File tilt3dFindCom = new File(getPropertyUserDir(),
        ProcessName.TILT_3D_FIND.getComscript(axisID));
    File tilt3dFindReprojectCom = new File(getPropertyUserDir(),
        ProcessName.TILT_3D_FIND_REPROJECT.getComscript(axisID));
    if (!tilt3dFindCom.exists()) {
      System.err.println("ERROR:  " + tilt3dFindCom.getName()
          + " does not exist.  Unable to create "
          + tilt3dFindReprojectCom.getName());
      return;
    }
    if (tilt3dFindReprojectCom.exists()) {
      //Backup tilt_3dfind_reproject.com
      try {
        LogFile logFile = LogFile.getInstance(tilt3dFindReprojectCom,
            getManagerKey());
        logFile.backup();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
    }
    //Copy file
    try {
      Utilities.copyFile(new File(getPropertyUserDir(),
          ProcessName.TILT_3D_FIND.getComscript(axisID)),
          tilt3dFindReprojectCom);
      comScriptMgr.loadTilt3dFindReproject(axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      System.err.println("ERROR:  Unable to create "
          + tilt3dFindReprojectCom.getName());
    }
    //Update the tilt command in the com file.
    TiltParam tiltParam = null;
    try {
      tiltParam = comScriptMgr.getTiltParamFromTilt3dFindReproject(axisID);
      getParametersForTilt3dFindReproject(tiltParam, axisID);
      comScriptMgr.saveTilt3dFindReproject(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      except.printStackTrace();
    }
  }

  /**
   * Update the tilt_3dfind_reproject.com.
   * 
   * @param axisID
   * @return true if successful
   */
  public ConstTiltParam updateTilt3dFindReprojectCom(TiltDisplay display,
      AxisID axisID) {
    TiltParam tiltParam = null;
    try {
      tiltParam = comScriptMgr.getTiltParamFromTilt3dFindReproject(axisID);
      getParametersForTilt3dFindReproject(tiltParam, axisID);
      if (display != null) {
        display.getParameters(tiltParam);
      }
      comScriptMgr.saveTilt3dFindReproject(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (IOException e) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = e.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Tilt Parameter",
          axisID, getManagerKey());
      return null;
    }
    return tiltParam;
  }

  /**
   * Update the tilt_3dfind.com from the TomogramGenerationDialog
   * 
   * @param axisID
   * @return true if successful
   */
  public ConstTiltParam updateTilt3dFindCom(TiltDisplay display, AxisID axisID) {
    // Set a reference to the correct object
    if (display == null) {
      UIHarness.INSTANCE.openMessageDialog(
          "Can not update tilt_3dfind?.com without an active display",
          "Program logic error", axisID, getManagerKey());
      return null;
    }
    TiltParam tiltParam = null;
    try {
      tiltParam = comScriptMgr.getTiltParamFromTilt3dFind(axisID);
      tiltParam.setFiducialess(metaData.isFiducialess(axisID));
      if (!display.getParameters(tiltParam)) {
        return null;
      }
      comScriptMgr.saveTilt3dFind(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (IOException e) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = e.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Tilt Parameter",
          axisID, getManagerKey());
      return null;
    }
    return tiltParam;
  }

  /**
   * Update the tilt.com from the TomogramGenerationDialog
   * 
   * @param axisID
   * @return true if successful
   */
  public ConstTiltParam updateTiltCom(TiltDisplay display, AxisID axisID) {
    // Set a reference to the correct object
    if (display == null) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "Can not update tilt?.com without an active tomogram generation dialog",
              "Program logic error", axisID, getManagerKey());
      return null;
    }
    TiltParam tiltParam = null;
    try {
      tiltParam = comScriptMgr.getTiltParam(axisID);
      tiltParam.setFiducialess(metaData.isFiducialess(axisID));
      if (!display.getParameters(tiltParam)) {
        return null;
      }
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
        // binning is currently always 1 and correct size should be coming from
        // copytomocoms
        // tiltParam.setMontageFullImage(propertyUserDir,
        // tomogramGenerationDialog.getBinning());
      }
      UIExpertUtilities.INSTANCE.rollTiltComAngles(this, axisID);
      comScriptMgr.saveTilt(tiltParam, axisID);
      metaData.setFiducialess(axisID, tiltParam.isFiducialess());
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (IOException e) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = e.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Tilt Parameter",
          axisID, getManagerKey());
      return null;
    }
    return tiltParam;
  }

  /**
   * Update the tilt.com from the TomogramGenerationDialog
   * Use the trial tomogram filename specified in the TomogramGenerationDialog
   * @return a ConstTiltParam instance if successful
   */
  public ConstTiltParam updateTrialTiltCom(TrialTiltDisplay display,
      AxisID axisID) {
    // Set a reference to the correct object
    if (display == null) {
      UIHarness.INSTANCE
          .openMessageDialog(
              "Can not update tilt?.com without an active tomogram generation dialog",
              "Program logic error", axisID, getManagerKey());
      return null;
    }
    TiltParam tiltParam = null;
    try {
      tiltParam = comScriptMgr.getTiltParam(axisID);
      tiltParam.setFiducialess(metaData.isFiducialess(axisID));
      if (!display.getParameters(tiltParam)) {
        return null;
      }
      String trialTomogramName = display.getTrialTomogramName();
      tiltParam.setOutputFile(trialTomogramName);

      if (metaData.getViewType() == ViewType.MONTAGE) {
        // binning is currently always 1 and correct size should be coming from
        // copytomocoms
        // tiltParam.setMontageFullImage(propertyUserDir,
        // tomogramGenerationDialog.getBinning());
      }
      UIExpertUtilities.INSTANCE.rollTiltComAngles(this, axisID);
      comScriptMgr.saveTilt(tiltParam, axisID);
      metaData.setFiducialess(axisID, tiltParam.isFiducialess());
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Tilt Parameter Syntax Error", axisID, getManagerKey());
      return null;
    }
    catch (IOException e) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = e.getMessage();
      UIHarness.INSTANCE.openMessageDialog(errorMessage, "Tilt Parameter",
          axisID, getManagerKey());
      return null;
    }
    return tiltParam;
  }

  private SplittiltParam updateSplittiltParam(TiltDisplay tiltDisplay,
      AxisID axisID) {
    if (tiltDisplay == null) {
      return null;
    }
    SplittiltParam param = new SplittiltParam(axisID);
    if (!tiltDisplay.getParameters(param)) {
      return null;
    }
    param.setSeparateChunks(CpuAdoc.getInstance(axisID, getPropertyUserDir(),
        getManagerKey()).isSeparateChunks());
    return param;
  }

  /**
   * Open 3dmod to view the tomogram
   * 
   * @param axisID the AxisID of the tomogram to open.
   */
  public void imodFullVolume(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod with the tomogram", axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod to view a file
   * 
   * @param axisID the AxisID of the file to open.
   */
  public void imod(FileType fileType, AxisID axisID,
      Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(fileType.getImodManagerKey(), axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod to view a file with a model
   * 
   * @param axisID the AxisID of the file to open.
   */
  public void imod(FileType fileType, FileType modelFileType, AxisID axisID,
      Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(fileType.getImodManagerKey(), axisID, modelFileType
          .getFileName(this, axisID), menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod to view a file with a model
   * 
   * @param axisID the AxisID of the file to open.
   */
  public void imodReprojectModel(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    FileType fileType;
    if (state.isStackUsingNewstOrBlend3dFindOutput(axisID)) {
      fileType = FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT;
    }
    else {
      fileType = FileType.NEWST_OR_BLEND_OUTPUT;
    }
    try {
      imodManager.open(fileType.getImodManagerKey(), axisID,
          FileType.CCD_ERASER_BEADS_INPUT_MODEL.getFileName(this, axisID),
          menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open 3dmod on the current test volume
   * 
   * @param axisID
   */
  public void imodTestVolume(AxisID axisID, Run3dmodMenuOptions menuOptions,
      String trialTomogramName) {
    try {
      imodManager.newImod(ImodManager.TRIAL_TOMOGRAM_KEY, axisID,
          trialTomogramName);
      imodManager.open(ImodManager.TRIAL_TOMOGRAM_KEY, axisID, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      String message[] = new String[2];
      message[0] = "Unable to open specified tomogram:" + trialTomogramName;
      message[1] = "Does it exist in the working directory?";
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod with the tomogram", axisID, getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  public ProcessResult commitTestVolume(AxisID axisID,
      ProcessResultDisplay processResultDisplay, String trialTomogramName) {
    // Check to see if the trial tomogram exist
    File trialTomogramFile = new File(propertyUserDir, trialTomogramName);
    if (!trialTomogramFile.exists()) {
      String message[] = new String[2];
      message[0] = "The specified tomogram does not exist:" + trialTomogramName;
      message[1] = "It must be calculated before commiting";
      uiHarness.openMessageDialog(message, "Can't rename tomogram", axisID,
          getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    // rename the trial tomogram to the output filename of appropriate
    // tilt.com
    File outputFile;
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      outputFile = new File(propertyUserDir, metaData.getDatasetName()
          + "_full.rec");
    }
    else {
      outputFile = new File(propertyUserDir, metaData.getDatasetName()
          + axisID.getExtension() + ".rec");
    }
    mainPanel.setProgressBar("Using trial tomogram: " + trialTomogramName, 1,
        axisID);
    if (outputFile.exists() && trialTomogramFile.exists()) {
      try {
        Utilities.renameFile(outputFile, new File(outputFile.getAbsolutePath()
            + "~"));
      }
      catch (IOException except) {
        uiHarness.openMessageDialog("Unable to backup "
            + outputFile.getAbsolutePath() + "\n" + except.getMessage(),
            "File Rename Error", axisID, getManagerKey());
        mainPanel.stopProgressBar(axisID);
        return ProcessResult.FAILED;
      }
    }
    try {
      Utilities.renameFile(trialTomogramFile, outputFile);
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(), "File Rename Error",
          axisID, getManagerKey());
      mainPanel.stopProgressBar(axisID);
      return ProcessResult.FAILED;
    }
    mainPanel.stopProgressBar(axisID);
    return ProcessResult.SUCCEEDED;
  }

  /**
   * Delete the pre-aligned and aligned stack for the specified axis
   * 
   * @param axisID
   */
  public void deleteAlignedStacks(AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (isAxisBusy(axisID, processResultDisplay)) {
      return;
    }
    mainPanel.setProgressBar("Deleting aligned image stack", 1, axisID);
    //
    // Don't do preali now because users may do upto generation before
    // transfering
    // the fiducials
    //
    // File preali =
    // new File(
    // System.getProperty("user.dir"),
    // metaData.getDatasetName() + axisID.getExtension() + ".preali");
    // if (preali.exists()) {
    // if (!preali.delete()) {
    // mainFrame.openMessageDialog(
    // "Unable to delete pre-aligned stack: " + preali.getAbsolutePath(),
    // "Can not delete file");
    // }
    // }
    File aligned = new File(propertyUserDir, metaData.getDatasetName()
        + axisID.getExtension() + ".ali");
    if (aligned.exists()) {
      if (!aligned.delete()) {
        StringBuffer message = new StringBuffer(
            "Unable to delete aligned stack: " + aligned.getAbsolutePath());
        if (Utilities.isWindowsOS()) {
          message.append("\nIf this file is open in 3dmod, close 3dmod.");
        }
        uiHarness.openMessageDialog(message.toString(), "Can not delete file",
            axisID, getManagerKey());
      }
    }
    mainPanel.stopProgressBar(axisID);
    sendMsgProcessSucceeded(processResultDisplay);
  }

  public long getTomogramSize(AxisID axisID) {
    long size = 0;
    FileInputStream stream;
    try {
      stream = new FileInputStream(DatasetFiles.getTomogram(this, axisID));
      FileChannel fileChannel = stream.getChannel();
      size = fileChannel.size();
    }
    catch (FileNotFoundException e) {
    }
    catch (IOException e) {
    }
    return size;
  }

  /**
   * Open the tomogram combination dialog
   */
  public void openTomogramCombinationDialog() {
    // Check to see if the com files are present otherwise pop up a dialog
    // box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(metaData, AxisID.ONLY,
        getManagerKey())) {
      mainPanel.showBlankProcess(AxisID.ONLY);
      return;
    }
    // Verify that this process is applicable
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      uiHarness.openMessageDialog(
          "This step is valid only for a dual axis tomogram",
          "Invalid tomogram combination selection", AxisID.ONLY,
          getManagerKey());
      mainPanel.showBlankProcess(AxisID.ONLY);
      return;
    }
    setCurrentDialogType(DialogType.TOMOGRAM_COMBINATION, AxisID.FIRST);
    mainPanel.selectButton(AxisID.FIRST, "Tomogram Combination");
    if (tomogramCombinationDialog == null) {
      Utilities.timestamp("new", "TomogramCombinationDialog",
          Utilities.STARTED_STATUS);
      tomogramCombinationDialog = new TomogramCombinationDialog(this);
      Utilities.timestamp("new", "TomogramCombinationDialog",
          Utilities.FINISHED_STATUS);
      // Get the setupcombine parameters and set the default patch
      // boundaries if
      // they have not already been set
      CombineParams combineParams = new CombineParams(metaData
          .getCombineParams());
      if (!combineParams.isPatchBoundarySet()) {
        //The first time combine is opened for this dataset, set tomogram size
        state.setTomogramSize(AxisID.FIRST, getTomogramSize(AxisID.FIRST));
        state.setTomogramSize(AxisID.SECOND, getTomogramSize(AxisID.SECOND));
        String recFileName;
        MatchMode matchMode = combineParams.getMatchMode();
        if (matchMode == null || matchMode == MatchMode.B_TO_A) {
          recFileName = metaData.getDatasetName() + "a.rec";
        }
        else {
          recFileName = metaData.getDatasetName() + "b.rec";
        }
        try {
          combineParams.setDefaultPatchBoundaries(recFileName);
        }
        catch (InvalidParameterException except) {
          String[] detailedMessage = new String[4];
          detailedMessage[0] = "Unable to set default patch boundaries";
          detailedMessage[1] = "Are both tomograms computed and available?";
          detailedMessage[2] = "";
          detailedMessage[3] = except.getMessage();
          uiHarness.openMessageDialog(detailedMessage, "Invalid parameter: "
              + recFileName, AxisID.ONLY, getManagerKey());
          // Delete the dialog
          tomogramCombinationDialog = null;
          mainPanel.showBlankProcess(AxisID.ONLY);
          return;
        }
        catch (IOException except) {
          uiHarness.openMessageDialog(except.getMessage(), "IO Error: "
              + recFileName, AxisID.ONLY, getManagerKey());
          // Delete the dialog
          tomogramCombinationDialog = null;
          mainPanel.showBlankProcess(AxisID.ONLY);
          return;
        }
      }
      // Fill in the dialog box params and set it to the appropriate state
      tomogramCombinationDialog.setCombineParams(combineParams);
      backwardsCompatibilityCombineScriptsExist();
      // If setupcombine has been run load the com scripts, otherwise disable
      // the
      // apropriate panels in the tomogram combination dialog
      // tomogramCombinationDialog.enableCombineTabs(combineScriptsExist());
      if (state.isCombineScriptsCreated()) {
        // Check to see if a solvematch.com file exists and load it if so
        // otherwise load the correct old solvematch* file
        File solvematch = new File(propertyUserDir, "solvematch.com");
        if (solvematch.exists()) {
          loadSolvematch();
        }
        else {
          // For backward compatibility using fiducialMatch instead of
          // modelBased. ModelBased was not updated when fiducialMatch was
          // changed and ficucialMatch wasn't update when modelBased was
          // changed.
          // But it looks like modelBased was never changed. In version 3.2.6
          // CombineParam.setModelBased() was never called. So fiducialMatch is
          // probably correct in earlier versions.
          boolean modelBased = false;
          ConstCombineParams metaDataCombineParams = metaData
              .getCombineParams();
          if (metaDataCombineParams.getFiducialMatch() == FiducialMatch.USE_MODEL
              || metaDataCombineParams.getFiducialMatch() == FiducialMatch.USE_MODEL_ONLY) {
            modelBased = true;
          }
          loadSolvematch(modelBased);
        }
        loadMatchvol1();
        loadPatchcorr();
        loadMatchorwarp();
        loadVolcombine();
        loadCombineComscript();
        tomogramCombinationDialog.synchronize(
            TomogramCombinationDialog.lblSetup, true/* false */);
      }
      else {
        //force the user to set Z values on a new combine
        //make sure not destroying user entries by checking for patchcorr.com
        if (!DatasetFiles.getAxisOnlyComFile(this, ProcessName.PATCHCORR)
            .exists()) {
          tomogramCombinationDialog.setZMin("");
          tomogramCombinationDialog.setZMax("");
        }
      }
    }
    tomogramCombinationDialog.show();
    tomogramCombinationDialog.setParameters(metaData);
    tomogramCombinationDialog.setParameters(getScreenState(AxisID.ONLY));
    // Show the process panel
    mainPanel.showProcess(tomogramCombinationDialog.getContainer(),
        AxisID.FIRST);
    setParallelDialog(AxisID.FIRST, tomogramCombinationDialog
        .usingParallelProcessing());
  }

  /**
   * Open the matching models in the 3dmod reconstruction instances
   */
  public void imodMatchingModel(boolean binBy2, Run3dmodMenuOptions menuOptions) {
    if (tomogramCombinationDialog == null) {
      return;
    }
    // FIXME do we want to be updating here break model (maybe it is saving
    // the bin by 2 state?
    updateCombineParams();

    try {
      if (binBy2) {
        imodManager.setBinning(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST, 2);
        imodManager.setBinning(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND, 2);
      }
      imodManager.setOpenContours(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST,
          true);
      imodManager.setOpenContours(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND,
          true);
      imodManager.open(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST, metaData
          .getDatasetName()
          + AxisID.FIRST.getExtension() + ".matmod", true, menuOptions);
      imodManager.open(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND, metaData
          .getDatasetName()
          + AxisID.SECOND.getExtension() + ".matmod", true, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on tomograms for matching models", AxisID.ONLY,
          getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", AxisID.ONLY,
          getManagerKey());
    }
  }

  /**
   * Open the matchcheck results in 3dmod
   */
  public void imodMatchCheck(Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.MATCH_CHECK_KEY, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on matchcheck.mat or matchcheck.rec", AxisID.ONLY,
          getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", AxisID.ONLY,
          getManagerKey());
    }
  }

  /**
   * Open the patch region models in tomogram being matched to
   */
  public void imodPatchRegionModel(Run3dmodMenuOptions menuOptions) {
    // FIXME do we want to be updating here break model
    // Get the latest combine parameters from the dialog
    updateCombineParams();
    AxisID axisID;
    MatchMode matchMode = metaData.getCombineParams().getMatchMode();
    // TEMP
    System.err.println("matchMode=" + matchMode);
    if (matchMode == null || matchMode == MatchMode.B_TO_A) {
      axisID = AxisID.FIRST;
    }
    else {
      axisID = AxisID.SECOND;
    }
    System.err.println("axisID=" + axisID);
    try {
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID, "patch_region.mod",
          true, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on tomogram for patch region models", AxisID.ONLY,
          getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open the patch vector models in 3dmod
   */
  public void imodPatchVectorModel(String key) {
    try {
      imodManager.open(key);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on tomogram for patch vector model", AxisID.ONLY,
          getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", AxisID.ONLY,
          getManagerKey());
    }
  }

  /**
   * Open the tomogram being matched to
   */
  public void imodMatchedToTomogram(Run3dmodMenuOptions menuOptions) {
    AxisID axisID;
    MatchMode matchMode = metaData.getCombineParams().getMatchMode();
    if (matchMode == null || matchMode == MatchMode.B_TO_A) {
      axisID = AxisID.FIRST;
    }
    else {
      axisID = AxisID.SECOND;
    }
    try {
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on tomogram being matched to", AxisID.ONLY,
          getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Tomogram combination done method, move on to the post processing window
   */
  public void doneTomogramCombinationDialog() {
    saveTomogramCombinationDialog(tomogramCombinationDialog);
  }

  /**
   * Tomogram combination done method, move on to the post processing window
   */
  public void saveTomogramCombinationDialog(
      TomogramCombinationDialog tomogramCombinationDialog) {
    // if the dialog isn't displayed, it wouldn't have been changed since it was
    // last saved
    if (tomogramCombinationDialog == null
        || !tomogramCombinationDialog.isDisplayed()) {
      return;
    }
    setAdvanced(tomogramCombinationDialog.getDialogType(),
        tomogramCombinationDialog.isAdvanced());
    DialogExitState exitState = tomogramCombinationDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(AxisID.ONLY);
    }
    else {
      tomogramCombinationDialog.synchronizeFromCurrentTab();
      // Update the com script and metadata info from the tomogram
      // combination dialog box. Since there are multiple pages and scripts
      // associated with the postpone button get the ones that are appropriate
      updateCombineParams();
      tomogramCombinationDialog.getParameters(metaData);
      tomogramCombinationDialog.getParameters(getScreenState(AxisID.ONLY));
      if (!tomogramCombinationDialog.isChanged(state)) {
        updateSolvematchCom();
        updateMatchvol1Com();
        updatePatchcorrCom();
        updateMatchorwarpCom(false);
        updateVolcombineCom();
      }
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
        mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
        mainPanel.showBlankProcess(AxisID.ONLY);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setTomogramCombinationState(ProcessState.COMPLETE);
        mainPanel.setTomogramCombinationState(ProcessState.COMPLETE);
        closeImod(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST,
            "axis A full volume");
        closeImod(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND,
            "axis B full volume");
        closeImod(ImodManager.MATCH_CHECK_KEY, AxisID.SECOND,
            "match check volume");
        closeImod(ImodManager.PATCH_VECTOR_MODEL_KEY, "patch vector model");
        openPostProcessingDialog();
      }
      saveStorables(AxisID.ONLY);
    }
  }

  /**
   * Check to see if the combine scripts exist
   * 
   * @return true if the combine scripts exist
   */
  public void backwardsCompatibilityCombineScriptsExist() {
    int combineScriptsCreatedValue = state.getCombineScriptsCreated().getInt();
    if (combineScriptsCreatedValue == EtomoState.FALSE_VALUE) {
      return;
    }
    //Check if combineScriptsCreated was not set.  Also check if it is true, to
    //make sure that it is still true.
    File solvematchshift = new File(propertyUserDir, "solvematchshift.com");
    File solvematchmod = new File(propertyUserDir, "solvematchmod.com");
    File solvematch = new File(propertyUserDir, "solvematch.com");
    File matchvol1 = new File(propertyUserDir, "matchvol1.com");
    File matchorwarp = new File(propertyUserDir, "matchorwarp.com");
    File patchcorr = new File(propertyUserDir, "patchcorr.com");
    File volcombine = new File(propertyUserDir, "volcombine.com");
    File warpvol = new File(propertyUserDir, "warpvol.com");
    if ((solvematch.exists() || (solvematchshift.exists() && solvematchmod
        .exists()))
        && matchvol1.exists()
        && matchorwarp.exists()
        && patchcorr.exists()
        && volcombine.exists() && warpvol.exists()) {
      state.setCombineScriptsCreated(true);
    }
    else {
      state.resetCombineScriptsCreated();
    }
  }

  /**
   * Run the setupcombine script with the current combine parameters stored in
   * metaData object. updateCombineCom is called first to get the currect
   * parameters from the dialog.
   * 
   * @param tomogramCombinationDialog the calling dialog.
   */
  public boolean createCombineScripts(ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    mainPanel.startProgressBar("Creating combine scripts", AxisID.ONLY,
        ProcessName.SOLVEMATCH);
    updateCombineParams();
    try {
      if (!processMgr.setupCombineScripts(metaData, processResultDisplay)) {
        mainPanel.stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
        return false;
      }
      processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
      mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    }
    catch (IOException except) {
      mainPanel.stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run setupcombine\n"
          + except.getMessage(), "Setupcombine IOException", AxisID.ONLY,
          getManagerKey());
      return false;
    }
    // Reload the initial and final match paramaters from the newly created
    // scripts

    // Reload all of the parameters into the ComScriptManager
    loadSolvematch();
    loadMatchvol1();
    loadPatchcorr();
    loadMatchorwarp();
    loadVolcombine();
    loadCombineComscript();
    mainPanel.stopProgressBar(AxisID.ONLY);
    state.setTomogramSize(AxisID.FIRST, getTomogramSize(AxisID.FIRST));
    state.setTomogramSize(AxisID.SECOND, getTomogramSize(AxisID.SECOND));
    return true;
  }

  /**
   * Update the combine parameters from the a specified tab of the calling
   * dialog assumes that the dialog is synchronized
   * 
   * @param tomogramCombinationDialog the calling dialog.
   */
  private void updateCombineParams() {
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update combine.com without an active tomogram combination dialog",
              "Program logic error", AxisID.ONLY, getManagerKey());
      return;
    }
    // start with existing combine params and update it from the screen
    CombineParams combineParams = metaData.getCombineParams();
    try {
      tomogramCombinationDialog.getCombineParams(combineParams);
      String recFileName;
      MatchMode matchMode = combineParams.getMatchMode();
      if (matchMode == null || matchMode == MatchMode.B_TO_A) {
        recFileName = metaData.getDatasetName() + "a.rec";
      }
      else {
        recFileName = metaData.getDatasetName() + "b.rec";
      }
      try {
        combineParams.setMaxPatchZMax(recFileName);
      }
      catch (InvalidParameterException except) {
        String[] detailedMessage = new String[4];
        detailedMessage[0] = "Unable to get max patch Z boundary";
        detailedMessage[1] = "Are both tomograms computed and available?";
        detailedMessage[2] = "";
        detailedMessage[3] = except.getMessage();
        uiHarness.openMessageDialog(detailedMessage, "Invalid parameter: "
            + recFileName, AxisID.ONLY, getManagerKey());
        // Delete the dialog
        tomogramCombinationDialog = null;
        return;
      }
      catch (IOException except) {
        uiHarness.openMessageDialog(except.getMessage(), "IO Error: "
            + recFileName, AxisID.ONLY, getManagerKey());
        // Delete the dialog
        tomogramCombinationDialog = null;
        return;
      }
      if (state.isCombineScriptsCreated() && !combineParams.isValid(true)) {
        uiHarness.openMessageDialog(combineParams.getInvalidReasons(),
            "Invalid combine parameters", AxisID.ONLY, getManagerKey());
        return;
      }
    }
    catch (NumberFormatException except) {
      uiHarness.openMessageDialog(except.getMessage(), "Number format error",
          AxisID.ONLY, getManagerKey());
      return;
    }
    CombineParams originalCombineParams = metaData.getCombineParams();
    if (!originalCombineParams.equals(combineParams)) {
      metaData.setCombineParams(combineParams);
      saveStorables(AxisID.ONLY);
    }
    return;
  }

  /**
   * Load the solvematch com script into the tomogram combination dialog
   */
  public void loadSolvematch() {
    comScriptMgr.loadSolvematch();
    tomogramCombinationDialog.setSolvematchParams(comScriptMgr.getSolvematch());
  }

  /**
   * Merge solvematchshift and solvematchmod com scripts in solvematch and load
   * solvematchinto the tomogram combination dialog
   */
  public void loadSolvematch(boolean modelBased) {
    // get solvematchshift param
    comScriptMgr.loadSolvematchshift();
    SolvematchshiftParam solvematchshiftParam = comScriptMgr
        .getSolvematchshift();
    // get solvematchmod param
    comScriptMgr.loadSolvematchmod();
    SolvematchmodParam solvematchmodParam = comScriptMgr.getSolvematchmod();
    // merge shift and mod into solvematch param
    SolvematchParam solvematchParam = new SolvematchParam(this);
    solvematchParam.mergeSolvematchshift(solvematchshiftParam, modelBased);
    solvematchParam.mergeSolvematchmod(solvematchmodParam, modelBased);
    // update the dialog from the same source
    tomogramCombinationDialog.setSolvematchParams(solvematchParam);
    // create the new solvematch com script
    comScriptMgr.loadSolvematch();
    comScriptMgr.saveSolvematch(solvematchParam);
    // add matchshifts to the solvematch com script
    MatchshiftsParam matchshiftsParam = comScriptMgr
        .getMatchshiftsFromSolvematchshifts();
    comScriptMgr.saveSolvematch(matchshiftsParam);
  }

  /**
   * Update the solvematch com file from the tomogramCombinationDialog
   * @return true if successful, false if not
   */
  public boolean updateSolvematchCom() {
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update solvematch.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY, getManagerKey());
      return false;
    }
    try {
      comScriptMgr.loadSolvematch();
      SolvematchParam solvematchParam = comScriptMgr.getSolvematch();
      tomogramCombinationDialog.getSolvematchParams(solvematchParam);
      comScriptMgr.saveSolvematch(solvematchParam);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Solvematch Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Solvematch Parameter Syntax Error", AxisID.ONLY, getManagerKey());
      return false;
    }
    return true;
  }

  /**
   * Update the matchvol1 com file from the tomogramCombinationDialog
   * @return true if successful, false if not
   */
  public boolean updateMatchvol1Com() {
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update matchvol1.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY, getManagerKey());
      return false;
    }
    try {
      comScriptMgr.loadMatchvol1();
      MatchvolParam param = comScriptMgr.getMatchvolParam();
      tomogramCombinationDialog.getParameters(param);
      comScriptMgr.saveMatchvol(param);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Matchvol1 Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Matchvol1 Parameter Syntax Error", AxisID.ONLY, getManagerKey());
      return false;
    }
    return true;
  }

  private void createNewSolvematch(SolvematchParam solvematchParam) {
    try {
      comScriptMgr.useTemplate("solvematch", metaData.getDatasetName(),
          AxisType.DUAL_AXIS, AxisID.ONLY);
    }
    catch (Exception e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to create solvematch com script";
      message[1] = "Check file and directory permissions";
      uiHarness.openMessageDialog(message, "Can't create solvematch.com",
          AxisID.ONLY, getManagerKey());
      return;
    }
    // Write it out the converted object disk
    comScriptMgr.loadSolvematch();
    comScriptMgr.saveSolvematch(solvematchParam);
    tomogramCombinationDialog.setSolvematchParams(solvematchParam);
  }

  /**
   * load the matchvol1 com script into the tomogram combination dialog
   *
   */
  private void loadMatchvol1() {
    comScriptMgr.loadMatchvol1();
    tomogramCombinationDialog.setParameters(comScriptMgr.getMatchvolParam());
  }

  /**
   * Load the patchcorr com script into the tomogram combination dialog
   */
  private void loadPatchcorr() {
    comScriptMgr.loadPatchcorr();
    tomogramCombinationDialog.setPatchcrawl3DParams(comScriptMgr
        .getPatchcrawl3D());
  }

  private void loadVolcombine() {
    comScriptMgr.loadVolcombine();
    //try to load reduction factor
    ConstSetParam setParam = comScriptMgr.getSetParamFromVolcombine(
        SetParam.COMBINEFFT_REDUCTION_FACTOR_NAME, EtomoNumber.Type.FLOAT);
    tomogramCombinationDialog.setReductionFactorParams(setParam);
    tomogramCombinationDialog.enableReductionFactor(setParam != null
        && setParam.isValid());
    //try to load low from both radius
    setParam = comScriptMgr.getSetParamFromVolcombine(
        SetParam.COMBINEFFT_LOW_FROM_BOTH_RADIUS_NAME,
        SetParam.COMBINEFFT_LOW_FROM_BOTH_RADIUS_TYPE, SetParam.COMMAND_NAME);
    tomogramCombinationDialog.setLowFromBothRadiusParams(setParam);
    tomogramCombinationDialog.enableLowFromBothRadius(setParam != null
        && setParam.isValid());
  }

  /**
   * Update the patchcorr.com script from the information in the tomogram
   * combination dialog box
   * 
   * @return boolean
   */
  private boolean updatePatchcorrCom() {
    // Set a reference to the correct object
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update patchcorr.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY, getManagerKey());
      return false;
    }
    try {
      Patchcrawl3DParam patchcrawl3DParam = comScriptMgr.getPatchcrawl3D();
      tomogramCombinationDialog.getPatchcrawl3DParams(patchcrawl3DParam);
      comScriptMgr.savePatchcorr(patchcrawl3DParam);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Patchcorr Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Patchcorr Parameter Syntax Error", AxisID.ONLY, getManagerKey());
      return false;
    }
    return true;
  }

  /**
   * Update the volcombine.com script from the information in the tomogram
   * combination dialog box
   * 
   * @return boolean
   */
  private boolean updateVolcombineCom() {
    // Set a reference to the correct object
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update volcombine.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY, getManagerKey());
      return false;
    }
    try {
      //Make sure the reduction factor set command is available in volcombine.com
      SetParam setParam = comScriptMgr.getSetParamFromVolcombine(
          SetParam.COMBINEFFT_REDUCTION_FACTOR_NAME, EtomoNumber.Type.FLOAT);
      boolean setParamIsValid = setParam != null && setParam.isValid();
      tomogramCombinationDialog.enableReductionFactor(setParamIsValid);
      tomogramCombinationDialog.getReductionFactorParam(setParam);
      if (setParamIsValid) {
        comScriptMgr.saveVolcombine(setParam);
      }
      //Make sure the low from both radius set command is available in volcombine.com
      setParam = comScriptMgr.getSetParamFromVolcombine(
          SetParam.COMBINEFFT_LOW_FROM_BOTH_RADIUS_NAME,
          SetParam.COMBINEFFT_LOW_FROM_BOTH_RADIUS_TYPE, SetParam.COMMAND_NAME);
      setParamIsValid = setParam != null && setParam.isValid();
      tomogramCombinationDialog.enableLowFromBothRadius(setParamIsValid);
      tomogramCombinationDialog.getLowFromBothRadiusParam(setParam);
      if (setParamIsValid) {
        comScriptMgr.saveVolcombine(setParam, SetParam.COMMAND_NAME);
      }
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Volcombine Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Volcombine Parameter Syntax Error", AxisID.ONLY, getManagerKey());
      return false;
    }
    return true;
  }

  /**
   * Load the matchorwarp com script into the tomogram combination dialog
   */
  private void loadMatchorwarp() {
    comScriptMgr.loadMatchorwarp();
    tomogramCombinationDialog.setMatchorwarpParams(comScriptMgr
        .getMatchorwarParam());
  }

  private CombineComscriptState getCombineComscript() {
    CombineComscriptState combineComscriptState = comScriptMgr
        .getCombineComscript();
    if (combineComscriptState == null) {
      try {
        comScriptMgr.useTemplate(CombineComscriptState.COMSCRIPT_NAME,
            AxisType.DUAL_AXIS, AxisID.ONLY, true);
      }
      catch (Exception e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Unable to copy combine com script";
        message[1] = "Check file and directory permissions";
        uiHarness.openMessageDialog(message, "Can't create a new combine.com",
            AxisID.ONLY, getManagerKey());
        return null;
      }
      comScriptMgr.loadCombine();
      combineComscriptState = comScriptMgr.getCombineComscript();
    }
    return combineComscriptState;
  }

  /**
   * Load the combine com script
   */
  private void loadCombineComscript() {
    comScriptMgr.loadCombine();
    CombineComscriptState combineComscriptState = getCombineComscript();
    if (combineComscriptState == null) {
      return;
    }
    tomogramCombinationDialog.setRunVolcombine(combineComscriptState
        .isRunVolcombine());
    tomogramCombinationDialog.updateDisplay();
  }

  /**
   * 
   * @return boolean
   */
  private CombineComscriptState updateCombineComscriptState(
      CombineProcessType startCommand) {
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update combine.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY, getManagerKey());
      return null;
    }
    // set goto a the start of combine
    CombineComscriptState combineComscriptState = getCombineComscript();
    if (combineComscriptState == null) {
      return null;
    }
    // set first command to run
    combineComscriptState
        .setStartCommand(startCommand.getIndex(), comScriptMgr);
    // set last command to run:
    // Volcombine is the last command to run in the combine script if parallel
    // processing is not used and either the the "stop before running
    // volcombine"
    // checkbox is off or "Restart at volcombine" was pressed.
    if (!tomogramCombinationDialog.usingParallelProcessing()
        && (startCommand == CombineProcessType.VOLCOMBINE || tomogramCombinationDialog
            .isRunVolcombine())) {
      combineComscriptState.setEndCommand(CombineProcessType.VOLCOMBINE
          .getIndex(), comScriptMgr);
    }
    else {
      combineComscriptState.setEndCommand(CombineProcessType.getInstance(
          CombineProcessType.VOLCOMBINE_INDEX - 1).getIndex(), comScriptMgr);
    }
    return combineComscriptState;
  }

  /**
   * Update the matchorwarp.com script from the information in the tomogram
   * combination dialog box
   * 
   * @return boolean
   */
  private boolean updateMatchorwarpCom(boolean trialMode) {
    // Set a reference to the correct object
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update matchorwarp.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY, getManagerKey());
      return false;
    }
    try {
      MatchorwarpParam matchorwarpParam = comScriptMgr.getMatchorwarParam();
      tomogramCombinationDialog.getMatchorwarpParams(matchorwarpParam);
      matchorwarpParam.setTrial(trialMode);
      comScriptMgr.saveMatchorwarp(matchorwarpParam);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Matchorwarp Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Matchorwarp Parameter Syntax Error", AxisID.ONLY, getManagerKey());
      return false;
    }
    return true;
  }

  /**
   * Initiate the combine process from the beginning
   */
  public void combine(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    sendMsgProcessStarting(processResultDisplay);
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    // FIXME: what are the necessary updates
    // Update the scripts from the dialog panel
    updateCombineParams();
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineProcessType.SOLVEMATCH);
    if (combineComscriptState == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateSolvematchCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateMatchvol1Com()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updatePatchcorrCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateVolcombineCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }

    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY, getManagerKey());
      return;
    }
    // Set the next process to execute when this is finished
    if (tomogramCombinationDialog.usingParallelProcessing()
        && tomogramCombinationDialog.isRunVolcombine()) {
      processSeries.setNextProcess(SplitcombineParam.COMMAND_NAME);
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  public void showPane(String comscript, CombineProcessType combineProcessType) {
    if (comscript.equals(CombineComscriptState.COMSCRIPT_NAME)) {
      tomogramCombinationDialog.showPane(combineProcessType);
    }
  }

  /**
   * Execute the matchvol1 com script and put patchcorr in the execution queue
   */
  public void matchvol1Combine(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // FIXME: what are the necessary updates
    // Update the scripts from the dialog panel
    updateCombineParams();
    if (!updateMatchvol1Com()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updatePatchcorrCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateVolcombineCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    updateCombineParams();
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineProcessType.MATCHVOL1);
    if (combineComscriptState == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);
    // Check to see if solve.xf exists first
    File solveXf = new File(propertyUserDir, "solve.xf");
    if (!solveXf.exists()) {
      // nextProcess = "";
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = "solve.xf must exist in the working";
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY, getManagerKey());
      return;
    }
    // Set the next process to execute when this is finished
    if (tomogramCombinationDialog.usingParallelProcessing()
        && tomogramCombinationDialog.isRunVolcombine()) {
      processSeries.setNextProcess(SplitcombineParam.COMMAND_NAME);
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  /**
   * Initiate the combine process from patchcorr step
   */
  public void patchcorrCombine(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    updateCombineParams();
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineProcessType.PATCHCORR);
    if (combineComscriptState == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updatePatchcorrCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateVolcombineCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }

    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);

    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute patchcorr.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY, getManagerKey());
      return;
    }
    // Set the next process to execute when this is finished
    if (tomogramCombinationDialog.usingParallelProcessing()
        && tomogramCombinationDialog.isRunVolcombine()) {
      processSeries.setNextProcess(SplitcombineParam.COMMAND_NAME);
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  private void warnStaleFile(String key, boolean future) {
    warnStaleFile(key, null, future);
  }

  private void warnStaleFile(String key, AxisID axisID) {
    warnStaleFile(key, axisID, false);
  }

  private void warnStaleFile(String key, AxisID axisID, boolean future) {
    try {
      if (imodManager.warnStaleFile(key, axisID)) {
        String[] message = new String[4];
        if (future) {
          message[0] = "3dmod is open to the existing " + key + ".";
          message[1] = "A new " + key + " will be created on disk.";
        }
        else {
          message[0] = "3dmod is open to the old " + key + ".";
          message[1] = "A new " + key + " has been created on disk.";
        }
        message[2] = "You will not be able to see the new version of " + key
            + " until you close this 3dmod.";
        message[3] = "Do you wish to quit this 3dmod now?";
        if (uiHarness.openYesNoDialog(message, axisID, getManagerKey())) {
          imodManager.quit(key, axisID);
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "System Process Exception",
          axisID, getManagerKey());
    }
  }

  /**
   * Initiate the combine process from matchorwarp step
   */
  public void matchorwarpCombine(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineProcessType.MATCHORWARP);
    if (combineComscriptState == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (!updateVolcombineCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY, getManagerKey());
      return;
    }
    // Set the next process to execute when this is finished
    if (tomogramCombinationDialog.usingParallelProcessing()
        && tomogramCombinationDialog.isRunVolcombine()) {
      processSeries.setNextProcess(SplitcombineParam.COMMAND_NAME);
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  /**
   * Initiate the combine process from matchorwarp step
   */
  public void matchorwarpTrial(ConstProcessSeries processSeries) {
    if (updateMatchorwarpCom(true)) {
      processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
      mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
      // Set the next process to execute when this is finished
      // nextProcess = next;
      String threadName = null;
      try {
        threadName = processMgr.matchorwarp(processSeries);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute matchorwarp.com";
        message[1] = e.getMessage();
        uiHarness.openMessageDialog(message, "Unable to execute com script",
            AxisID.ONLY, getManagerKey());
        return;
      }
      setThreadName(threadName, AxisID.FIRST);
      // FIXME why show the final pane when the button that calls this function
      // on the final pane?
      tomogramCombinationDialog.showPane(CombineProcessType.MATCHORWARP);
      mainPanel.startProgressBar("matchorwarp", AxisID.FIRST,
          ProcessName.MATCHORWARP);
    }
  }

  /**
   * Execute the combine script starting at volcombine
   */
  public void volcombine(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineProcessType.VOLCOMBINE);
    updateCombineParams();
    if (!updateVolcombineCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName = null;
    try {
      threadName = processMgr.combine(combineComscriptState,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY, getManagerKey());
      return;
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  /**
   * Convert the patch.mod to patch.out
   * 
   */
  public void modelToPatch() {
    try {
      processMgr.modelToPatch(AxisID.ONLY);
    }
    catch (SystemProcessException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to convert patch_vector.mod to patch.out";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Patch vector model error",
          AxisID.ONLY, getManagerKey());
      return;
    }
    catch (LogFile.LockException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to convert patch_vector.mod to patch.out";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Patch vector model error",
          AxisID.ONLY, getManagerKey());
      return;
    }
  }

  /**
   * Open the post processing dialog
   */
  public void openPostProcessingDialog() {
    // Check to see if the com files are present otherwise pop up a dialog
    // box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(metaData, AxisID.ONLY,
        getManagerKey())) {
      mainPanel.showBlankProcess(AxisID.ONLY);
      return;
    }
    // Open the dialog in the appropriate mode for the current state of
    // processing
    setCurrentDialogType(DialogType.POST_PROCESSING, AxisID.ONLY);
    mainPanel.selectButton(AxisID.ONLY, DialogType.POST_PROCESSING.toString());
    if (postProcessingDialog == null) {
      Utilities.timestamp("new", "PostProcessingDialog",
          Utilities.STARTED_STATUS);
      postProcessingDialog = PostProcessingDialog.getInstance(this);
      Utilities.timestamp("new", "PostProcessingDialog",
          Utilities.FINISHED_STATUS);
      // Set the appropriate input and output files
      TrimvolParam trimvolParam = metaData.getTrimvolParam();
      try {
        trimvolParam.setDefaultRange(state, getManagerKey(), metaData
            .isPostExists());
      }
      catch (InvalidParameterException except) {
        String[] detailedMessage = new String[4];
        detailedMessage[0] = "Unable to set trimvol range";
        detailedMessage[1] = "Does the reconstruction file exist yet?";
        detailedMessage[2] = "";
        detailedMessage[3] = except.getMessage();
        uiHarness.openMessageDialog(detailedMessage, "Invalid parameter: "
            + trimvolParam.getInputFileName(), AxisID.ONLY, getManagerKey());
        // Delete the dialog
        postProcessingDialog = null;
        mainPanel.showBlankProcess(AxisID.ONLY);
        return;
      }
      catch (IOException except) {
        uiHarness.openMessageDialog(except.getMessage(), "IO Error: "
            + trimvolParam.getInputFileName(), AxisID.ONLY, getManagerKey());
        // Delete the dialog
        postProcessingDialog = null;
        mainPanel.showBlankProcess(AxisID.ONLY);
        return;
      }
      metaData.setPostExists(true);
      saveStorables(AxisID.ONLY);
      postProcessingDialog.setTrimvolParams(trimvolParam);
      postProcessingDialog.setParameters(metaData.getSqueezevolParam());
    }
    postProcessingDialog.setParameters(getScreenState(AxisID.ONLY));
    postProcessingDialog.setParameters(metaData);
    comScriptMgr.loadFlatten(AxisID.ONLY);
    //Set from flatten.com after meta data.  Flatten.com is not created by
    //copytomocoms and may be empty.
    if (comScriptMgr.isWarpVolParamInFlatten(AxisID.ONLY)) {
      postProcessingDialog.setParameters(comScriptMgr
          .getWarpVolParamFromFlatten(AxisID.ONLY));
    }
    mainPanel.showProcess(postProcessingDialog.getContainer(), AxisID.ONLY);
    setParallelDialog(AxisID.ONLY, postProcessingDialog
        .usingParallelProcessing());
  }

  /**
   * Open the clean up dialog
   */
  public void openCleanUpDialog() {
    // Check to see if the com files are present otherwise pop up a dialog
    // box informing the user to run the setup process
    if (!UIExpertUtilities.INSTANCE.areScriptsCreated(metaData, AxisID.ONLY,
        getManagerKey())) {
      mainPanel.showBlankProcess(AxisID.ONLY);
      return;
    }
    // Open the dialog in the appropriate mode for the current state of
    // processing
    setCurrentDialogType(DialogType.CLEAN_UP, AxisID.ONLY);
    mainPanel.selectButton(AxisID.ONLY, "Clean Up");
    if (cleanUpDialog == null) {
      Utilities.timestamp("new", "CleanUpDialog", Utilities.STARTED_STATUS);
      cleanUpDialog = new CleanUpDialog(this);
      Utilities.timestamp("new", "CleanUpDialog", Utilities.FINISHED_STATUS);
    }
    updateArchiveDisplay();
    mainPanel.showProcess(cleanUpDialog.getContainer(), AxisID.ONLY);
    setParallelDialog(AxisID.ONLY, cleanUpDialog.usingParallelProcessing());
  }

  /**
   * Close the post processing dialog panel
   */
  public void donePostProcessing() {
    savePostProcessing(postProcessingDialog);
    postProcessingDialog = null;
  }

  /**
   * Close the post processing dialog panel
   */
  public void savePostProcessing(PostProcessingDialog postProcessingDialog) {
    setAdvanced(postProcessingDialog.getDialogType(), postProcessingDialog
        .isAdvanced());
    DialogExitState exitState = postProcessingDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(AxisID.ONLY);
    }
    else {
      updateTrimvolParam();
      updateFlattenWarpParam(postProcessingDialog.getFlattenWarpDisplay(),
          AxisID.ONLY);
      updateSqueezevolParam();
      postProcessingDialog.getParameters(metaData);
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setPostProcessingState(ProcessState.INPROGRESS);
        mainPanel.setPostProcessingState(ProcessState.INPROGRESS);
        mainPanel.showBlankProcess(AxisID.ONLY);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setPostProcessingState(ProcessState.COMPLETE);
        mainPanel.setPostProcessingState(ProcessState.COMPLETE);
        closeImod(ImodManager.COMBINED_TOMOGRAM_KEY, "full tomogram");
        closeImod(ImodManager.TRIMMED_VOLUME_KEY, "trimmed volume");
        closeImod(ImodManager.FLAT_VOLUME_KEY, "flattened volume");
        closeImod(ImodManager.SQUEEZED_VOLUME_KEY, "squeezed volume");
        openCleanUpDialog();
      }
      saveStorables(AxisID.ONLY);
    }
  }

  /**
   * Close the clean up dialog panel
   */
  public void doneCleanUp() {
    saveCleanUp(cleanUpDialog);
    cleanUpDialog = null;
    mainPanel.showBlankProcess(AxisID.ONLY);
  }

  /**
   * Close the clean up dialog panel
   */
  public void saveCleanUp(CleanUpDialog cleanUpDialog) {
    setAdvanced(cleanUpDialog.getDialogType(), cleanUpDialog.isAdvanced());
    DialogExitState exitState = cleanUpDialog.getExitState();
    if (exitState != DialogExitState.CANCEL) {
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setCleanUpState(ProcessState.INPROGRESS);
        mainPanel.setCleanUpState(ProcessState.INPROGRESS);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setCleanUpState(ProcessState.COMPLETE);
        mainPanel.setCleanUpState(ProcessState.COMPLETE);
      }
      saveStorables(AxisID.ONLY);
    }
  }

  /**
   * Open the combined (or full) volume in 3dmod
   */
  public void imodCombinedTomogram(Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.COMBINED_TOMOGRAM_KEY, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on the trimmed tomogram", AxisID.ONLY,
          getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", AxisID.ONLY,
          getManagerKey());
    }
  }

  /**
   * calls ProcessManager.generateAlignLogs() when the ta files are out of date
   * @param commandName
   * @param axisID
   * @return true if any log files where changed
   */
  public boolean updateLog(String commandName, AxisID axisID) {
    LogFile alignLogFile;
    try {
      alignLogFile = LogFile.getInstance(propertyUserDir, axisID,
          ProcessName.ALIGN, getManagerKey());
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      return false;
    }
    //File alignLog = new File(propertyUserDir, alignLogName);
    String taErrorLogName = "taError" + axisID.getExtension() + ".log";
    File taErrorLog = new File(propertyUserDir, taErrorLogName);
    if (!alignLogFile.exists()) {
      return false;
    }
    if (!taErrorLog.exists()
        || taErrorLog.lastModified() < alignLogFile.lastModified()) {
      processMgr.generateAlignLogs(axisID);
      return true;
    }
    return false;
  }

  /**
   * Open the trimmed volume in 3dmod
   */
  public void imodTrimmedVolume(Run3dmodMenuOptions menuOptions, AxisID axisID) {
    TrimvolParam trimvolParam = new TrimvolParam(this);
    if (!postProcessingDialog.getTrimvolParams(trimvolParam)) {
      return;
    }
    try {
      imodManager.setSwapYZ(ImodManager.TRIMMED_VOLUME_KEY, axisID,
          !trimvolParam.isSwapYZ() && !trimvolParam.isRotateX());
      imodManager.setStartNewContoursAtNewZ(ImodManager.TRIMMED_VOLUME_KEY,
          axisID, false);
      imodManager.open(ImodManager.TRIMMED_VOLUME_KEY, axisID, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on the trimmed tomogram", axisID, getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open flatten.com output
   * @param menuOptions
   * @param axisID
   */
  public void imodFlatten(Run3dmodMenuOptions menuOptions, AxisID axisID) {
    String key = ImodManager.FLAT_VOLUME_KEY;
    try {
      imodManager.open(key, axisID, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on the " + key, axisID, getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  public boolean isFlipped(ImageFileType imageFileType) {
    if (imageFileType == null) {
      return false;
    }
    if (imageFileType == ImageFileType.TRIM_VOL_OUTPUT) {
      return isTrimvolFlipped();
    }
    else if (imageFileType == ImageFileType.SQUEEZE_VOL_OUTPUT) {
      return isSqueezevolFlipped();
    }
    throw new IllegalStateException("Unknown file type " + imageFileType);
  }

  /**
   * return true if the result of squeezevol is flipped.
   * If squeezevol hasn't been done, return true if the result of trimvol is
   * flipped.
   * @return
   */
  public boolean isSqueezevolFlipped() {
    if (!state.getSqueezevolFlipped().isNull()) {
      return state.getSqueezevolFlipped().is();
    }
    return isTrimvolFlipped();
  }

  /**
   * return true if the result of squeezevol is flipped.
   * If squeezevol hasn't been done, return true if the result of trimvol is
   * flipped.
   * @return
   */
  public boolean isTrimvolFlipped() {
    if (state.getTrimvolFlipped().isNull()) {
      return state.getBackwardCompatibleTrimvolFlipped();
    }
    return state.getTrimvolFlipped().is();
  }

  public void imodMakeSurfaceModel(Run3dmodMenuOptions menuOptions,
      AxisID axisID, int binning, ImageFileType imageFileType) {
    //Pick ImodManager key
    //Need to look at tomogram edge on.  Use -Y, unless using squeezevol and it
    //is not flipped.
    String key = imageFileType.getImodManagerKey();
    boolean useSwapYZ = isFlipped(imageFileType);
    try {
      imodManager.setSwapYZ(key, axisID, useSwapYZ);
      imodManager.setOpenContours(key, axisID, true);
      imodManager.setStartNewContoursAtNewZ(key, axisID, true);
      imodManager.setBinningXY(key, binning);
      imodManager.open(key, axisID, DatasetFiles.getFlattenWarpInputName(this),
          true, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on the " + key, axisID, getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Open the squeezed volume in 3dmod
   */
  public void imodSqueezedVolume(Run3dmodMenuOptions menuOptions, AxisID axisID) {
    // Make sure that the post processing panel is open
    if (postProcessingDialog == null) {
      uiHarness.openMessageDialog("Post processing dialog not open",
          "Program logic error", axisID, getManagerKey());
      return;
    }
    try {
      imodManager.setSwapYZ(ImodManager.SQUEEZED_VOLUME_KEY, axisID,
          !isSqueezevolFlipped());
      imodManager.setStartNewContoursAtNewZ(ImodManager.SQUEEZED_VOLUME_KEY,
          axisID, false);
      imodManager.open(ImodManager.SQUEEZED_VOLUME_KEY, axisID, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on the squeezed tomogram", axisID, getManagerKey());
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
  }

  /**
   * Execute trimvol
   */
  public void trimVolume(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // Make sure that the post processing panel is open
    if (postProcessingDialog == null) {
      uiHarness.openMessageDialog("Post processing dialog not open",
          "Program logic error", AxisID.ONLY, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    TrimvolParam trimvolParam = updateTrimvolParam();
    if (trimvolParam == null) {
      return;
    }
    // Start the trimvol process
    processTrack.setPostProcessingState(ProcessState.INPROGRESS);
    mainPanel.setPostProcessingState(ProcessState.INPROGRESS);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.trimVolume(trimvolParam, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute trimvol command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY, getManagerKey());
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar("Trimming volume", AxisID.ONLY,
        ProcessName.TRIMVOL);
  }

  WarpVolParam updateWarpVolParam(final WarpVolDisplay display,
      final AxisID axisID) {
    WarpVolParam param = comScriptMgr.getWarpVolParamFromFlatten(axisID);
    if (display == null) {
      uiHarness.openMessageDialog(
          "Unable to get information from the display.", "Etomo Error", axisID,
          getManagerKey());
      return null;
    }
    if (!display.getParameters(param)) {
      return null;
    }
    comScriptMgr.saveFlatten(param, axisID);
    return param;
  }

  FlattenWarpParam updateFlattenWarpParam(FlattenWarpDisplay display,
      final AxisID axisID) {
    FlattenWarpParam param = new FlattenWarpParam(this);
    if (display == null) {
      uiHarness.openMessageDialog(
          "Unable to get information from the display.", "Etomo Error", axisID,
          getManagerKey());
      return null;
    }
    if (!display.getParameters(param)) {
      return null;
    }
    return param;
  }

  /**
   * Execute flatten.com
   */
  public void flatten(final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions,
      final DialogType dialogType, final AxisID axisID, WarpVolDisplay display) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    WarpVolParam param = updateWarpVolParam(display, axisID);
    if (param == null) {
      return;
    }
    //Run process
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.flatten(axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.FLATTEN;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Execute flattenwarp
   */
  public void flattenWarp(final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions,
      final DialogType dialogType, final AxisID axisID,
      FlattenWarpDisplay display) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    FlattenWarpParam param = updateFlattenWarpParam(display, axisID);
    if (param == null) {
      return;
    }
    //Run process
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.flattenWarp(param, processResultDisplay,
          processSeries, axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + param.getProcessName();
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + param.getProcessName(), axisID,
        param.getProcessName());
  }

  RunraptorParam updateRunraptorParam(AxisID axisID) {
    RunraptorParam param = new RunraptorParam(this, axisID);
    FiducialModelDialog dialog = (FiducialModelDialog) getDialog(
        DialogType.FIDUCIAL_MODEL, axisID);
    if (dialog == null) {
      uiHarness.openMessageDialog(
          "Unable to get information from the fiducial model dialog.",
          "Etomo Error", axisID, getManagerKey());
      return null;
    }
    if (!dialog.getParameters(param)) {
      return null;
    }
    return param;
  }

  /**
   * Execute runraptor
   */
  public void runraptor(final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions,
      final DialogType dialogType, final AxisID axisID) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    RunraptorParam param = updateRunraptorParam(axisID);
    if (param == null) {
      return;
    }
    //Attempt to backup the output file
    try {
      LogFile outputFile = LogFile.getInstance(DatasetFiles
          .getRaptorFiducialModel(this, axisID), getManagerKey());
      outputFile.backup();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    //Run process
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.runraptor(param, processResultDisplay,
          processSeries, axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute runraptor";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running runraptor", axisID,
        ProcessName.RUNRAPTOR);
  }

  /**
   * Open 3dmod with the runraptor result
   */
  public void imodRunraptorResult(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    String imodManagerKey;
    imodManagerKey = ImodManager.COARSE_ALIGNED_KEY;
    String model = DatasetFiles.getRaptorFiducialModelName(this, axisID);
    try {
      File tiltFile = DatasetFiles.getRawTiltFile(this, axisID);
      if (tiltFile.exists()) {
        imodManager.setTiltFile(imodManagerKey, axisID, tiltFile.getName());
      }
      else {
        imodManager.resetTiltFile(imodManagerKey, axisID);
      }
      imodManager.open(imodManagerKey, axisID, model, true, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID, getManagerKey());
      return;
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open 3dmod on "
          + imodManagerKey + " with model: " + model, axisID, getManagerKey());
      return;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID,
          getManagerKey());
    }
    state.setFixedFiducials(axisID, true);
  }

  /**
   * Replace .fid with _raptor.fid
   * 
   * @param axisID
   */
  public void useRunraptorResult(ProcessResultDisplay processResultDisplay,
      AxisID axisID, DialogType dialogType) {
    sendMsgProcessStarting(processResultDisplay);
    if (isAxisBusy(axisID, processResultDisplay)) {
      return;
    }
    mainPanel
        .setProgressBar("Using RAPTOR result as fiducial model", 1, axisID);
    processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    //Back up .fid file
    LogFile fidFile = null;
    try {
      fidFile = LogFile.getInstance(DatasetFiles.getFiducialModelFile(this,
          axisID), getManagerKey());
      fidFile.backup();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to backup "
          + DatasetFiles.getFiducialModelName(this, axisID), "File Error",
          getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    //Rename _raptor.fid file
    LogFile raptorFile = null;
    try {
      raptorFile = LogFile.getInstance(DatasetFiles.getRaptorFiducialModel(
          this, axisID), getManagerKey());
      raptorFile.move(fidFile);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to rename "
          + DatasetFiles.getRaptorFiducialModelName(this, axisID),
          "File Error", getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    mainPanel.stopProgressBar(axisID);
    sendMsgProcessSucceeded(processResultDisplay);
  }

  /**
   * Execute squeezevol
   */
  public void squeezevol(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    sendMsgProcessStarting(processResultDisplay);
    // Make sure that the post processing panel is open
    if (postProcessingDialog == null) {
      uiHarness.openMessageDialog("Post processing dialog not open",
          "Program logic error", AxisID.ONLY, getManagerKey());
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    ConstSqueezevolParam squeezevolParam = updateSqueezevolParam();
    // Start the trimvol process
    processTrack.setPostProcessingState(ProcessState.INPROGRESS);
    mainPanel.setPostProcessingState(ProcessState.INPROGRESS);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.squeezeVolume(squeezevolParam,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute squeezevol command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY, getManagerKey());
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar("Squeezing volume", AxisID.ONLY,
        ProcessName.SQUEEZEVOL);
  }

  /**
   * updates ConstMetaData.trimvolParam with data from postProcessingDialog.
   * Sets isDataParamDirty if necessary
   * @return the updated TrimvolParam
   * 
   */
  private TrimvolParam updateTrimvolParam() {
    // Get trimvol param data from dialog.
    TrimvolParam dialogTrimvolParam = new TrimvolParam(this);
    if (!postProcessingDialog.getTrimvolParams(dialogTrimvolParam)) {
      return null;
    }
    // Get the metadata trimvol param.
    TrimvolParam trimvolParam = metaData.getTrimvolParam();
    postProcessingDialog.getTrimvolParams(trimvolParam);
    // Add input and output files.
    trimvolParam.setInputFileName(metaData.getAxisType(), metaData
        .getDatasetName());
    trimvolParam.setOutputFileName(metaData.getDatasetName() + ".rec");
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS
        && !state.isAdjustOrigin(AxisID.ONLY)) {
      trimvolParam.setKeepSameOrigin(true);
    }
    return trimvolParam;
  }

  /**
   * updates ConstMetaData.squeezevolParam with data from postProcessingDialog.
   * Sets isDataParamDirty if necessary
   * @return the updated SqueezevolParam
   * 
   */
  private ConstSqueezevolParam updateSqueezevolParam() {
    // Get the metadata trimvol param.
    SqueezevolParam squeezevolParam = metaData.getSqueezevolParam();
    postProcessingDialog.getParameters(squeezevolParam);
    return squeezevolParam;
  }

  //
  // Utility functions
  //
  private boolean showIfExists(ProcessDialog panelA, ProcessDialog panelB,
      AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (panelB == null) {
        return false;
      }
      else {
        mainPanel.showProcess(panelB.getContainer(), axisID);
        return true;
      }
    }
    else {
      if (panelA == null) {
        return false;
      }
      else {
        mainPanel.showProcess(panelA.getContainer(), axisID);
        return true;
      }
    }
  }

  /**
   * Start generic progress bar
   * @param value
   * @param axisID
   */
  public void startProgressBar(String label, AxisID axisID,
      ProcessName processName) {
    mainPanel.startProgressBar(label, axisID, processName);
  }

  /**
   * Set the progress bar to the specified value
   * 
   * @param value
   * @param axisID
   */
  public void setProgressBarValue(int value, AxisID axisID) {
    mainPanel.setProgressBarValue(value, axisID);
  }

  /**
   * @param axisID
   */
  public void progressBarDone(AxisID axisID, ProcessEndState processEndState) {
    mainPanel.stopProgressBar(axisID, processEndState);
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  void startNextProcess(AxisID axisID, String nextProcess,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      DialogType dialogType, ProcessDisplay display, ProcessName subprocessName) {
    UIExpert uiExpert = getUIExpert(dialogType, axisID);
    if (uiExpert != null) {
      uiExpert.startNextProcess(nextProcess, processResultDisplay,
          processSeries, dialogType, display, subprocessName);
    }
    else if (nextProcess.equals("checkUpdateFiducialModel")) {
      checkUpdateFiducialModel(axisID, processResultDisplay, processSeries);
    }
    else if (nextProcess.equals(ArchiveorigParam.COMMAND_NAME)) {
      archiveOriginalStack(AxisID.SECOND, processSeries, dialogType);
    }
    else if (nextProcess.equals(ProcessName.PROCESSCHUNKS.toString())
        && subprocessName == ProcessName.VOLCOMBINE) {
      processchunksVolcombine(processResultDisplay, processSeries);
    }
    else if (nextProcess.equals(SplitcombineParam.COMMAND_NAME)) {
      splitcombine(processSeries, null, null, dialogType);
    }
    else if (nextProcess.equals(ExtractpiecesParam.COMMAND_NAME)) {
      extractpieces(axisID, processResultDisplay, processSeries, dialogType);
    }
    else if (nextProcess.equals(ExtractmagradParam.COMMAND_NAME)) {
      extractmagrad(axisID, processResultDisplay, processSeries);
    }
    else if (nextProcess.equals(ProcessName.XCORR.toString())) {
      crossCorrelate(axisID, processResultDisplay, processSeries, dialogType,
          (TiltXcorrDisplay) display);
    }
    else if (nextProcess.equals(ProcessName.ERASER.toString())) {
      eraser(axisID, processResultDisplay, processSeries, dialogType,
          (CcdEraserDisplay) display);
    }
  }

  void updateDialog(ProcessName processName, AxisID axisID) {
    if (axisID != AxisID.ONLY
        && (processName == ProcessName.PRENEWST || processName == ProcessName.TRACK)) {
      updateDialog(fiducialModelDialogB, AxisID.SECOND);
      updateDialog(fiducialModelDialogA, AxisID.FIRST);
    }
    if (processName == ProcessName.NEWST || processName == ProcessName.BLEND) {
      ((FinalAlignedStackExpert) getUIExpert(DialogType.FINAL_ALIGNED_STACK,
          axisID)).updateDialog();
    }
  }

  private void updateDialog(FiducialModelDialog dialog, AxisID axisID) {
    if (dialog == null || axisID == AxisID.ONLY) {
      return;
    }
    boolean prealisExist = Utilities.fileExists(this, ".preali", AxisID.FIRST)
        && Utilities.fileExists(this, ".preali", AxisID.SECOND);
    boolean fidExists = false;
    if (axisID == AxisID.FIRST) {
      fidExists = Utilities.fileExists(this, ".fid", AxisID.SECOND);
    }
    else {
      fidExists = Utilities.fileExists(this, ".fid", AxisID.FIRST);
    }
    dialog.setTransferfidEnabled(prealisExist && fidExists);
    dialog.updateEnabled();
  }

  private void setBackgroundThreadName(String name, AxisID axisID,
      String processName) {
    setThreadName(name, axisID);
    if (axisID == AxisID.SECOND) {
      throw new IllegalStateException("No Axis B background processes exist.");
    }
    else {
      backgroundProcessA = true;
      backgroundProcessNameA = processName;
    }
  }

  // Test helper functions
  /**
   * Return the currently executing thread name for the specified axis
   * @param axisID
   * @return
   */
  String getThreadName(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return threadNameB;
    }
    return threadNameA;
  }

  void checkUpdateFiducialModel(AxisID axisID,
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    FidXyz fidXyz = UIExpertUtilities.INSTANCE.getFidXyz(this, axisID);
    MRCHeader prealiHeader = MRCHeader.getInstance(this, axisID, ".preali",
        getManagerKey());
    MRCHeader rawstackHeader = MRCHeader.getInstance(this, axisID, ".st",
        getManagerKey());
    try {
      fidXyz.read();
      if (!prealiHeader.read() || !rawstackHeader.read()) {
        processDone(axisID, processResultDisplay, processSeries);
        return;
      }
    }
    catch (IOException except) {
      processDone(axisID, processResultDisplay, processSeries);
      return;
    }
    catch (InvalidParameterException except) {
      except.printStackTrace();
      processDone(axisID, processResultDisplay, processSeries);
      return;
    }
    if (!fidXyz.exists()) {
      processDone(axisID, processResultDisplay, processSeries);
      return;
    }
    // if fidXyz.getPixelSize() is 1, then the binning used in align.com must
    // have been 1, if the preali binning is also 1, then no error message
    // should
    // be sent. preali binning is preali pixel spacing / .st pixel spacing
    boolean fidXyzPixelSizeSet = fidXyz.isPixelSizeSet();
    if (!fidXyzPixelSizeSet) {
      if (Math.round(prealiHeader.getXPixelSpacing()
          / rawstackHeader.getXPixelSpacing()) == 1) {
        processDone(axisID, processResultDisplay, null);
        return;
      }
    }
    if (!fidXyzPixelSizeSet
        || fidXyz.getPixelSize() != prealiHeader.getXPixelSpacing()) {
      // if (getStackBinning(axisID, ".preali") !=
      // getBackwardCompatibleAlignBinning(axisID)) {
      String title = "Prealigned image stack binning has changed";
      String[] message = new String[] {
          "The prealigned image stack binning has changed.  You " + "must:",
          "    1. Go  to Fiducial Model Gen. and Press Fix Fiducial "
              + "Model to open the fiducial model.",
          "    2. Save the fiducial model " + "by pressing \"s\".",
          "    3. Go to Fine Alignment and press Compute "
              + "Alignment to rerun align" + axisID.getExtension() + ".com." };
      uiHarness.openMessageDialog(message, title, axisID, getManagerKey());
    }
    processDone(axisID, processResultDisplay, processSeries);
  }

  void createComScriptManager() {
    comScriptMgr = new ComScriptManager(this);
  }

  void createMainPanel() {
    mainPanel = new MainTomogramPanel(this);
  }

  /**
   * Set the data set parameter file. This also updates the mainframe data
   * parameters.
   * @param paramFile a File object specifying the data set parameter file.
   */
  public void setParamFile(File paramFile) {
    this.paramFile = paramFile;
    // Update main window information and status bar
    mainPanel.setStatusBarText(paramFile, metaData, logPanel);
  }

  void createProcessTrack() {
    processTrack = new ProcessTrack();
  }

  private void createState() {
    state = new TomogramState(this);
  }

  public TomogramState getState() {
    return state;
  }

  public BaseState getBaseState() {
    return state;
  }

  public MetaData getMetaData() {
    return metaData;
  }

  public ConstMetaData getConstMetaData() {
    return (ConstMetaData) metaData;
  }

  public void xfmodel(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, AxisID axisID,
      DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    sendMsgProcessStarting(processResultDisplay);
    XfmodelParam param = new XfmodelParam(this, axisID);
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    ProcessResult processResult = xfmodel(axisID, processResultDisplay,
        processSeries, param, dialogType);
    sendMsg(processResult, processResultDisplay);
  }

  public void seedEraseFiducialModel(Run3dmodMenuOptions run3dmodMenuOptions,
      AxisID axisID, DialogType dialogType) {
    imodSeedModel(axisID, run3dmodMenuOptions, null,
        ImodManager.FINE_ALIGNED_KEY, FileType.CCD_ERASER_BEADS_INPUT_MODEL
            .getFileName(this, axisID), null, dialogType);
  }

  private ProcessResult xfmodel(AxisID axisID,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      final XfmodelParam param, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    String threadName;
    try {
      threadName = processMgr.xfmodel(param, axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.XFMODEL;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + ProcessName.XFMODEL, axisID,
        ProcessName.XFMODEL);
    return null;
  }

  /**
   * Replace the full aligned stack with the ctf corrected full aligned stack
   * created by ctfcorrection.com
   */
  public void useFileAsFullAlignedStack(
      ProcessResultDisplay processResultDisplay, File output,
      String buttonLabel, AxisID axisID, DialogType dialogType) {
    sendMsgProcessStarting(processResultDisplay);
    if (isAxisBusy(axisID, processResultDisplay)) {
      return;
    }
    mainPanel.setProgressBar("Using " + output.getName()
        + " as full aligned stack", 1, axisID);
    // Instantiate file objects for the original raw stack and the fixed
    // stack
    File fullAlignedStack = DatasetFiles.getFullAlignedStackFile(this, axisID);
    if (!output.exists()) {
      UIHarness.INSTANCE.openMessageDialog(output.getAbsolutePath()
          + " doesn't exist.  Press " + buttonLabel + " to create this file.",
          buttonLabel + " Output Missing", axisID, getManagerKey());
      sendMsg(ProcessResult.FAILED_TO_START, processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    if (fullAlignedStack.exists() && output.exists()) {
      if (!Utilities.isValidStack(output, this, axisID)) {
        uiHarness.openMessageDialog(output.getName()
            + " is not a valid MRC file.", "Entry Error", axisID,
            getManagerKey());
        sendMsgProcessFailedToStart(processResultDisplay);
        return;
      }
      try {
        Utilities.renameFile(fullAlignedStack, new File(fullAlignedStack
            .getAbsolutePath()
            + "~"));
      }
      catch (IOException except) {
        UIHarness.INSTANCE.openMessageDialog("Unable to backup "
            + fullAlignedStack.getAbsolutePath() + "\n" + except.getMessage(),
            "File Rename Error", axisID, getManagerKey());
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
          "File Rename Error", axisID, getManagerKey());
      sendMsg(ProcessResult.FAILED, processResultDisplay);
      return;
    }
    closeImod(ImodManager.FINE_ALIGNED_KEY, axisID,
        "original full aligned stack");
    mainPanel.stopProgressBar(axisID);
    sendMsg(ProcessResult.SUCCEEDED, processResultDisplay);
  }

  public void useCcdEraser(ProcessResultDisplay processResultDisplay,
      AxisID axisID, DialogType dialogType, String ccdEraserLabel) {
    useFileAsFullAlignedStack(processResultDisplay, DatasetFiles
        .getErasedFiducialsFile(this, axisID), ccdEraserLabel, axisID,
        dialogType);
  }

  public void imodErasedFiducials(Run3dmodMenuOptions run3dmodMenuOptions,
      AxisID axisID) {
    imodOpen(axisID, ImodManager.ERASED_FIDUCIALS_KEY,
        FileType.CCD_ERASER_BEADS_INPUT_MODEL.getFileName(this, axisID),
        run3dmodMenuOptions, false);
  }

  public CCDEraserParam updateCcdEraserParam(CcdEraserDisplay display) {
    CCDEraserParam param = new CCDEraserParam();
    if (display.getParameters(param)) {
      return param;
    }
    return null;
  }

  public void ccdEraser(ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, AxisID axisID,
      DialogType dialogType, CcdEraserDisplay display) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    sendMsgProcessStarting(processResultDisplay);
    CCDEraserParam param = updateCcdEraserParam(display);
    if (param == null) {
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    ProcessResult processResult = ccdEraser(axisID, processResultDisplay,
        processSeries, param, dialogType);
    sendMsg(processResult, processResultDisplay);
  }

  private ProcessResult ccdEraser(AxisID axisID,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      final CCDEraserParam param, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    String threadName;
    try {
      threadName = processMgr.ccdEraser(param, axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.CCD_ERASER;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + ProcessName.CCD_ERASER, axisID,
        ProcessName.CCD_ERASER);
    return null;
  }

  public BaseMetaData getBaseMetaData() {
    return (BaseMetaData) metaData;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  public void openNextDialog(AxisID axisID, DialogType dialogType) {
    if (dialogType == DialogType.TOMOGRAM_POSITIONING) {
      getUIExpert(DialogType.FINAL_ALIGNED_STACK, axisID).openDialog();
    }
    else if (dialogType == DialogType.FINAL_ALIGNED_STACK) {
      getUIExpert(DialogType.TOMOGRAM_GENERATION, axisID).openDialog();
    }
    else if (dialogType == DialogType.TOMOGRAM_GENERATION) {
      if (isDualAxis()) {
        mainPanel.showBlankProcess(axisID);
      }
      else {
        openPostProcessingDialog();
      }
    }
  }

  void getProcessTrack(Storable[] storable, int index) {
    if (storable == null) {
      return;
    }
    storable[index] = processTrack;
  }

  BaseProcessTrack getProcessTrack() {
    return processTrack;
  }

  /**
   * Interrupt the currently running thread for this axis
   * 
   * @param axisID
   */
  public void kill(AxisID axisID) {
    processMgr.kill(axisID);
  }

  /**
   * Interrupt the currently running thread for this axis
   * 
   * @param axisID
   */
  public void pause(AxisID axisID) {
    processMgr.pause(axisID);
  }

  private ProcessResult splittilt(AxisID axisID,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      SplittiltParam param, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    String threadName;
    try {
      threadName = processMgr.splittilt(param, axisID, processResultDisplay,
          processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + SplittiltParam.COMMAND_NAME;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString(),
        ProcessName.TILT);
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + SplittiltParam.COMMAND_NAME,
        axisID, ProcessName.SPLITTILT);
    return null;
  }

  public ProcessResult splitCorrection(AxisID axisID,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      ConstSplitCorrectionParam param, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    String threadName;
    try {
      threadName = processMgr.splitCorrection(param, axisID,
          processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.SPLIT_CORRECTION;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID,
          getManagerKey());
      return ProcessResult.FAILED_TO_START;
    }
    processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString(),
        ProcessName.CTF_CORRECTION);
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Running " + ProcessName.SPLIT_CORRECTION,
        axisID, ProcessName.SPLIT_CORRECTION);
    return null;
  }

  public void splitcombine(ProcessSeries processSeries,
      Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    ProcessResultDisplay processResultDisplay = getProcessResultDisplayFactory(
        AxisID.ONLY).getRestartVolcombine();
    sendMsgProcessStarting(processResultDisplay);
    if (!updateVolcombineCom()) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    String threadName;
    try {
      threadName = processMgr.splitcombine(processResultDisplay, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + SplitcombineParam.COMMAND_NAME;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY, getManagerKey());
      return;
    }
    processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString(),
        ProcessName.VOLCOMBINE);
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar("Running " + SplitcombineParam.COMMAND_NAME,
        AxisID.ONLY, ProcessName.SPLITCOMBINE);
  }

  private void processchunksVolcombine(
      ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    processchunks(AxisID.ONLY, DialogType.TOMOGRAM_COMBINATION,
        tomogramCombinationDialog, processResultDisplay, processSeries,
        ProcessName.VOLCOMBINE);
  }

  /**
   * Run processchunks.
   * @param axisID
   */
  private void processchunks(AxisID axisID, DialogType dialogType,
      AbstractParallelDialog dialog, ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries, ProcessName processName) {
    sendMsgProcessStarting(processResultDisplay);
    if (dialog == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, axisID, processName);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    dialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (processTrack != null) {
      processTrack.setState(ProcessState.INPROGRESS, axisID, dialogType);
    }
    mainPanel.setState(ProcessState.INPROGRESS, axisID, dialogType);
    //param should never be set to resume
    parallelPanel.getParallelProgressDisplay().resetResults();
    processchunks(axisID, param, processResultDisplay, processSeries);
  }

  public BaseProcessManager getProcessManager() {
    return processMgr;
  }

  Storable[] getStorables(int offset) {
    int arraySize = 5 + offset;
    boolean dualAxis = true;
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      dualAxis = false;
      arraySize--;
    }
    Storable[] storables = new Storable[arraySize];
    int index = offset;
    storables[index++] = metaData;
    storables[index++] = state;
    storables[index++] = processTrack;
    storables[index++] = getScreenState(AxisID.FIRST);
    if (dualAxis) {
      storables[index] = getScreenState(AxisID.SECOND);
    }
    return storables;
  }

  public ReconScreenState getScreenState(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (screenStateB == null) {
        screenStateB = new ReconScreenState(axisID, metaData.getAxisType());
      }
      return screenStateB;
    }
    if (screenStateA == null) {
      screenStateA = new ReconScreenState(axisID, metaData.getAxisType());
    }
    return screenStateA;
  }

  public BaseScreenState getBaseScreenState(AxisID axisID) {
    return (BaseScreenState) getScreenState(axisID);
  }

  public boolean canChangeParamFileName() {
    // if the param file hasn't been loaded, any param name that is added while
    // be overwritten when Setup Tomogram is complete, so don't allow the user
    // to
    // do a Save As.
    return this.loadedParamFile;
  }

  public boolean canSnapshot() {
    return !isNewManager();
  }

  public String getName() {
    if (metaData == null) {
      return MetaData.getNewFileTitle();
    }
    return metaData.getName();
  }
}
/**
 * <p> $Log$
 * <p> Revision 3.343  2009/12/11 17:23:07  sueh
 * <p> bug# 1291 Added clipstats.
 * <p>
 * <p> Revision 3.342  2009/10/27 19:55:51  sueh
 * <p> bug# 1275 Added createLogPanel - moves the resposibility for creating the
 * <p> log panel to the child classes.  That way the Front Page manager doesn't
 * <p> have to have a log panel.
 * <p>
 * <p> Revision 3.341  2009/10/23 22:12:07  sueh
 * <p> bug# 1275 Made touch() a start function in BaseProcessManager.
 * <p>
 * <p> Revision 3.340  2009/10/23 21:22:33  sueh
 * <p> bug# 1281 In openPostProcessingDialog, checking, setting, and saving
 * <p> metaData.postExists, which keeps track of whether the post processing
 * <p> dialog has been brought up before.
 * <p>
 * <p> Revision 3.339  2009/10/01 18:44:34  sueh
 * <p> bug# 1233 In save... functions always try to save everything and don't
 * <p> pass back a boolean.
 * <p>
 * <p> Revision 3.338  2009/09/28 19:05:05  sueh
 * <p> bug# 1223 In imodFixFiducials removing call to turn off auto new contour.
 * <p>
 * <p> Revision 3.337  2009/09/22 23:41:46  sueh
 * <p> bug# 1269 Added a call to FinalAlignedStackExpert.setEnabledTiltParameters.
 * <p>
 * <p> Revision 3.336  2009/09/21 17:38:40  sueh
 * <p> bug# 1267 Newst process monitor now takes newst and newst_3dfind.  Tilt process monitor has a child class which handles tilt_3dfind.
 * <p>
 * <p> Revision 3.335  2009/09/17 19:11:11  sueh
 * <p> bug# 1257 In NewstParam.setSizeToOutputInXandY forgot to read the
 * <p> header.  Adding read call and throwing InvalidParameterException and
 * <p> IOException.
 * <p>
 * <p> Revision 3.334  2009/09/02 22:41:45  sueh
 * <p> bug# 1254 Checking for a valid stack before using the stack.  Turning on 2d filtering for blendmont.
 * <p>
 * <p> Revision 3.333  2009/09/01 03:17:35  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.332  2009/08/24 20:20:28  sueh
 * <p> bug# 1254 Don't use Toolkit unless the GraphicsEnvironment is not
 * <p> headless.
 * <p>
 * <p> Revision 3.331  2009/06/16 22:43:36  sueh
 * <p> bug# 1221 Factor out gold eraser panel.
 * <p>
 * <p> Revision 3.330  2009/06/15 20:13:00  sueh
 * <p> bug# 1221 Made beadtrack functionality dialog independant.
 * <p>
 * <p> Revision 3.329  2009/06/12 19:45:37  sueh
 * <p> bug# 1221 Factored out running newst, making it dependent only on
 * <p> NewstackPanel.
 * <p>
 * <p> Revision 3.328  2009/06/11 16:32:19  sueh
 * <p> bug# 1221 Sending the process panel to the process function in the
 * <p> manager wrapped in a ProcessDisplay interface.  Changing eraser,
 * <p> findxrays, flatten, flattenWarp, imodFlatten, imodMakeSurface,
 * <p> imodManualErase, isFlipped, preEraser, replaceRawStack,
 * <p> savePreProcDialog, startNextProcess, updateEraserCom,
 * <p> updateFlattenWarpParam, updateWarpVolParam.
 * <p>
 * <p> Revision 3.327  2009/06/05 01:44:04  sueh
 * <p> bug# 1219 Added flatten, flattenWarp, imodFlatten,
 * <p> imodMakeSurfaceModel, isFlipped, isSqueezed, isTrimvolFlipped,
 * <p> updateFlattenWarpParam, and updateWarpVolParam.
 * <p>
 * <p> Revision 3.326  2009/05/04 16:44:38  sueh
 * <p> bug# 1216 In openFiducialModelDialog using AxisType to prevent
 * <p> displaying the RAPTOR interface unless the dataset is dual axis.
 * <p>
 * <p> Revision 3.325  2009/05/02 01:05:52  sueh
 * <p> bug# 1216 Added imodRawStack, imodRunraptorResult, runraptor,
 * <p> updateRunraptorParam, and useRunraptorResult.
 * <p>
 * <p> Revision 3.324  2009/04/15 16:51:27  sueh
 * <p> bug# 1190 Logging reconnect attempts.  Returning false and logging failure for major reconnection failures.  Preventing deleteAlignedStacks from running on a busy axis.
 * <p>
 * <p> Revision 3.323  2009/04/14 23:00:33  sueh
 * <p> bug# 1190 Logging reconnect.  Handling some situations where process data is not
 * <p> running in reconnect.
 * <p>
 * <p> Revision 3.322  2009/04/13 22:18:49  sueh
 * <p> bug# 1207 Moved the call the ProcessData.isRunning to
 * <p> processMgr.getRunningProcess so it can be called fewer times.  Implemented
 * <p> doAutomation in BaseManager.
 * <p>
 * <p> Revision 3.321  2009/03/23 16:53:54  sueh
 * <p> bug# 1187 Changed sendContinuousMessage, which is the wrong verb, to getContinuousMessage.
 * <p>
 * <p> Revision 3.320  2009/03/23 16:45:57  sueh
 * <p> bug# 1187 Moved taError.log logging to ApplicationManager.  Implement ContinuousMessageTarget:  added sendContinuousMessage which generates the ta align logs and calls logTaErrorLogMessage.
 * <p>
 * <p> Revision 3.319  2009/03/17 00:23:25  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.318  2009/03/05 23:22:03  sueh
 * <p> bug$ 1194 Added saveParamFile to logPanel.frameProperties to UserConfiguration.
 * <p>
 * <p> Revision 3.317  2009/03/02 18:52:42  sueh
 * <p> bug# 1193 Do all reconnects in openProcessingPanel.
 * <p>
 * <p> Revision 3.316  2009/02/13 02:11:46  sueh
 * <p> bug# 1176 Checking return value of MRCHeader.read.
 * <p>
 * <p> Revision 3.315  2009/02/10 21:36:49  sueh
 * <p> bug# 1143 Passing state to TrimvolParam.setDefaultRange.
 * <p>
 * <p> Revision 3.314  2009/02/04 22:31:15  sueh
 * <p> bug# 1158 passing logPanel to mainPanel.setStatusBarText so its title can
 * <p> be updated.
 * <p>
 * <p> Revision 3.313  2008/12/09 21:25:41  sueh
 * <p> bug# 1160 Changed getFiducialDiameterPerPixel; divided the diameter by coarse aligned stack binning.
 * <p>
 * <p> Revision 3.312  2008/12/05 00:49:29  sueh
 * <p> bug# 1156 Added a skipList parameter to imodFixFiducials.
 * <p>
 * <p> Revision 3.311  2008/11/21 17:10:22  sueh
 * <p> bug# 1123 In saveAlignmentEstimationDialog calling
 * <p> FinalAlignmentDialog.getParameters(ReconScreenState).
 * <p>
 * <p> Revision 3.310  2008/11/20 01:23:55  sueh
 * <p> bug# 1147 Added ccdEraser(), xfmodel().  Changed imodSeedFiducials
 * <p> to imodSeedModel and made it more generic.
 * <p>
 * <p> Revision 3.309  2008/10/27 17:43:20  sueh
 * <p> bug# 1141 Added ctfCorrection, ctfPlotter, imodCtfCorrection,
 * <p> splitCorrection, and FinalAlignedStack dialogs.
 * <p>
 * <p> Revision 3.308  2008/10/16 20:53:50  sueh
 * <p> bug# 1141 Created FinalAlignedStack dialog to run full aligned stack and mtf filter.
 * <p>
 * <p> Revision 3.307  2008/09/30 19:45:49  sueh
 * <p> bug# 1113 Reformatting
 * <p>
 * <p> Revision 3.306  2008/07/24 18:04:17  sueh
 * <p> bug# 1128 In imodSample calling ImodManager.setPointLimit.
 * <p>
 * <p> Revision 3.305  2008/07/24 17:57:13  sueh
 * <p> bug# 1128 In imodFullSample called ImodManager.setPointLimit.
 * <p>
 * <p> Revision 3.304  2008/06/20 20:53:17  sueh
 * <p> bug# 1112 Use .tlt when opening FINE_ALIGNED and MTF_FILTER 3dmods.
 * <p>
 * <p> Revision 3.303  2008/06/19 23:20:01  sueh
 * <p> bug# 1112 Added tilt angle for the 3dmods that display tilt series
 * <p>
 * <p> Revision 3.302  2008/05/28 02:33:04  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  Pass it to manager.startNextProcess so the UIExpert can be
 * <p> retrieved.  DialogType must be passed to any function that constructs a
 * <p> ProcessSeries instance.
 * <p>
 * <p> Revision 3.301  2008/05/14 19:38:35  sueh
 * <p> bug# 1094 Fixed strings in savePostProcessing.
 * <p>
 * <p> Revision 3.300  2008/05/13 20:40:55  sueh
 * <p> bug# 847 Added Deferred3dmodButton and Run3dmodMenuOptions to
 * <p> functions where 3dmod may run after the process.
 * <p>
 * <p> Revision 3.299  2008/05/06 23:53:31  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.
 * <p>
 * <p> Revision 3.298  2008/05/03 00:28:29  sueh
 * <p> bug# 847 Passing ProcessSeries to the process manager, startNextProcess, and to all process functions.  To avoid having to decide which processes are next processes, pass it everywhere, even to processes that don't use ProcessResultDisplay.  The UI should not create any ProcessSeries and should pass them as null (since they don't know about processes).  Before adding to a process series, create it if it doesn't exist.  Before checking a process series, make sure it exists.  Since a series is only created when it doesn't exist and is needed, I don't have to keep track of which process comes first in a series.
 * <p>
 * <p> Revision 3.297  2008/01/31 20:13:29  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 3.296  2008/01/23 21:02:03  sueh
 * <p> bug# 1064  Added reconnectRunA and B.  Can't use the reconnectRun
 * <p> functionality in BaseManager because BaseManager.reconnect is run before
 * <p> ApplicationManager.reconnect.
 * <p>
 * <p> Revision 3.295  2008/01/14 20:12:44  sueh
 * <p> bug# 1050 In openProcessingPanel do not call reconnect for Axis B.  Processchunks reconnect must be done while Axis B is displayed.  In reconnect call super.reconnect to do the processchunks reconnect.
 * <p>
 * <p> Revision 3.294  2007/12/26 21:53:34  sueh
 * <p> bug# 1052 Created SetupDialogExpert.  Made SetupDialog package protected.
 * <p>
 * <p> Revision 3.293  2007/12/13 01:01:45  sueh
 * <p> bug# 1056 Moved post processing decision making back to ProcessManager.
 * <p>
 * <p> Revision 3.292  2007/12/10 21:46:18  sueh
 * <p> bug# 1041 Formatted
 * <p>
 * <p> Revision 3.291  2007/11/06 18:56:06  sueh
 * <p> bug# 1047 Generalize TrimvolParam.
 * <p>
 * <p> Revision 3.290  2007/09/27 19:18:33  sueh
 * <p> bug# 1044 Made ProcessorTable the ParallelProgress display instead of
 * <p> ParallelPanel.
 * <p>
 * <p> Revision 3.289  2007/09/07 00:14:57  sueh
 * <p> bug# 989 Using a public INSTANCE for EtomoDirector instead of getInstance
 * <p> and createInstance.
 * <p>
 * <p> Revision 3.288  2007/08/29 21:20:51  sueh
 * <p> bug# 1041 Made getBaseState public.
 * <p>
 * <p> Revision 3.287  2007/08/22 14:58:35  sueh
 * <p> bug# 1036 Showing a blank process when opening a dialog fails.
 * <p>
 * <p> Revision 3.286  2007/08/21 21:48:35  sueh
 * <p> bug# 771 Added getTomogramSize.  Setting state.tomogramSize in
 * <p> createCombineScripts and openTomogramCombinationDialog.
 * <p>
 * <p> Revision 3.285  2007/08/08 14:44:46  sueh
 * <p> bug# 834 In doneSetupDialog, setting MetaData.TrimvolParam.swapYZ if it is set
 * <p> in userConfig.
 * <p>
 * <p> Revision 3.284  2007/07/30 18:30:56  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 3.283  2007/07/27 16:50:05  sueh
 * <p> bug# 979 In openFiducialModelDialog returning success/failure boolean.  Using
 * <p> getInstance to construct FiducialModelDialog because it uses action listeners,
 * <p> which shouldn't be created during construction.
 * <p>
 * <p> Revision 3.282  2007/07/24 19:24:48  sueh
 * <p> bug# 1032 Don't set meta data in imod manager unless paramFileName is set.
 * <p>
 * <p> Revision 3.281  2007/06/08 21:50:18  sueh
 * <p> bug# 1014 Removed setMetaData(ImodManager) and placing the call to ImodManager.setMetaData after the call to initializeUIParameters.
 * <p>
 * <p> Revision 3.280  2007/05/21 22:27:29  sueh
 * <p> bug# 964 Added get getInterfaceType().
 * <p>
 * <p> Revision 3.279  2007/04/09 19:10:45  sueh
 * <p> bug# 964 Added setParamFile(), which just returns loadedParamFile
 * <p>
 * <p> Revision 3.278  2007/03/07 20:52:03  sueh
 * <p> bug# Validate beam tilt fields in the fine alignment dialog in fineAlignment().
 * <p>
 * <p> Revision 3.277  2007/03/03 00:30:53  sueh
 * <p> bug# 973 Getting/setting metadata in Fine Align dialog.
 * <p>
 * <p> Revision 3.276  2007/02/21 22:28:42  sueh
 * <p> 964 Removing incorrect comment.
 * <p>
 * <p> Revision 3.275  2007/02/05 21:24:23  sueh
 * <p> bug# 962 Creating process manager later.
 * <p>
 * <p> Revision 3.274  2006/11/28 22:45:23  sueh
 * <p> bug# 934 Changed BaseManager.stop() to endThreads().
 * <p>
 * <p> Revision 3.273  2006/11/15 18:14:38  sueh
 * <p> bug# 872 Changed getParamFileStorableArray to getStorables.  Letting the base
 * <p> save param file function call save().  getStorables always gets all the storables
 * <p> (including meta data) each time, to make it simpler.
 * <p>
 * <p> Revision 3.272  2006/10/24 21:13:17  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 3.271  2006/10/16 22:33:13  sueh
 * <p> bug# 919  Changed touch(File) to touch(String absolutePath).  bug# 933  Moved
 * <p> the code in msgAlignPostProcess to postProcess.  Only doing align post process
 * <p> (reopening log) when the Computer Alignment button was used to run Fine
 * <p> Alignment.
 * <p>
 * <p> Revision 3.270  2006/10/13 22:18:24  sueh
 * <p> bug# 927 In volcombine, managing combinefft LowFromBothRadius.
 * <p>
 * <p> Revision 3.269  2006/10/11 10:05:08  sueh
 * <p> bug# 931 Added delete functionality to LogFile - changed BackupException to
 * <p> FileException.
 * <p>
 * <p> Revision 3.268  2006/10/10 05:00:40  sueh
 * <p> bug# 931 Managing log files with LogFile
 * <p>
 * <p> Revision 3.267  2006/09/20 20:41:25  sueh
 * <p> bug# 928 Added errorProcess().
 * <p>
 * <p> Revision 3.266  2006/09/19 21:53:35  sueh
 * <p> bug# 920 Values where not being saved correctly by TomogramState.  Add
 * <p> tilt.fiducialess to metaData.  Bug# 928 Add post processing for patchcorr.
 * <p> Change imodPatchVectorModel so that it can open either patch_vector.mod or
 * <p> patch_vector_ccc.mod.
 * <p>
 * <p> Revision 3.265  2006/09/13 23:05:52  sueh
 * <p> bug# 920 Moving some postProcessing to TomogramPositioningExpert.
 * <p>
 * <p> Revision 3.264  2006/09/05 17:33:07  sueh
 * <p> bug# 917 Added loadMatchvol1 and updateMatchvol1Com
 * <p>
 * <p> Revision 3.263  2006/08/30 16:48:52  sueh
 * <p> bug# 922 Fix null pointer error in updateDialog()
 * <p>
 * <p> Revision 3.262  2006/08/18 00:12:58  sueh
 * <p> bug# 914 open combine dialog:  make sure patchcorr.com doesn't exist before
 * <p> removing Zs
 * <p>
 * <p> Revision 3.261  2006/08/14 22:21:43  sueh
 * <p> bug# 891 backwardCompatibilityCombineScriptsExist():  double checking true
 * <p> values.
 * <p>
 * <p> Revision 3.260  2006/08/14 18:32:08  sueh
 * <p> bug# 890 Returning a success/failure boolean from getTrimvolParams.
 * <p>
 * <p> Revision 3.259  2006/08/11 23:47:08  sueh
 * <p> bug# 816 Added msgAlignPostProcess().
 * <p>
 * <p> Revision 3.258  2006/08/11 21:43:54  sueh
 * <p> bug# 816 Setting open log when opening the coarse aligned stack.
 * <p>
 * <p> Revision 3.257  2006/08/03 21:17:51  sueh
 * <p> bug# 769 Added reconnectTilt()
 * <p>
 * <p> Revision 3.256  2006/08/02 22:01:11  sueh
 * <p> bug# 769 Added reconnect()
 * <p>
 * <p> Revision 3.255  2006/08/01 20:05:47  sueh
 * <p> bug# 769 Making sure that the axis for a single axis dialog is ONLY.
 * <p>
 * <p> Revision 3.254  2006/07/28 22:22:09  sueh
 * <p> bug# 910 SaveTomogramCombinationDialog():  added call to
 * <p> TomogramCombinationDialog.synchronizeFromCurrentTab(), so that anything the
 * <p> user changes on the current tab will be remembered on exit.
 * <p>
 * <p> Revision 3.253  2006/07/28 19:43:16  sueh
 * <p> bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p> usingParallelProcessing.
 * <p>
 * <p> Revision 3.252  2006/07/28 17:41:04  sueh
 * <p> bug# 909 Changed TomogramState.combineScriptsCreated to an EtomoState so
 * <p> it will show when it has not been set.
 * <p>
 * <p> Revision 3.251  2006/07/26 16:28:46  sueh
 * <p> bug# 868 Moved functions associated with TomogramGenerationDialog to
 * <p> TomogramGenerationExpert.
 * <p>
 * <p> Revision 3.250  2006/07/21 22:23:42  sueh
 * <p> bug# 901 Just check for the distortion directory to set calibrationAvailable.
 * <p>
 * <p> Revision 3.249  2006/07/21 22:10:57  sueh
 * <p> bug# 901 Getting the calibration directory environment variable name from
 * <p> EnvironmentVariable.
 * <p>
 * <p> Revision 3.248  2006/07/19 20:04:30  sueh
 * <p> bug# 902 MakeFiducialModelSeedModel:  Set seeding done and update the
 * <p> fiducial model display.  Added processSucceeded.
 * <p>
 * <p> Revision 3.247  2006/07/19 15:11:54  sueh
 * <p> bug# 903  UpdateCombineParams:  don't validate until after the scripts are
 * <p> created so that the Z limits can be blank.
 * <p>
 * <p> Revision 3.246  2006/07/18 20:54:28  sueh
 * <p> bug# 904 OpenTomogramCombinationDialog:  clear the Z min and max values if
 * <p> the scripts have not been created.
 * <p>
 * <p> Revision 3.245  2006/07/17 21:15:29  sueh
 * <p> bug# 900 Added imodSendEvent functionality back.  Uses the
 * <p> SystemProcessException.
 * <p>
 * <p> Revision 3.244  2006/07/05 23:23:14  sueh
 * <p> Added extra message for Windows when delete file fails.  Get fine alignment
 * <p> fix fiducials to set the right mode.
 * <p>
 * <p> Revision 3.243  2006/07/04 20:36:41  sueh
 * <p> bug# 898 Return false from done and save functions if their dialog continues
 * <p> to be displayed.
 * <p>
 * <p> Revision 3.242  2006/07/03 21:27:56  sueh
 * <p> bug# 895 Stop 3dmod request handler worker string on exit.
 * <p>
 * <p> Revision 3.241  2006/06/30 20:21:27  sueh
 * <p> bug# 884 Adding timestamps for constructing dialog objects.
 * <p>
 * <p> Revision 3.240  2006/06/30 19:58:27  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog.done() function,
 * <p> which is called from the button action functions and saveAction() in
 * <p> ProcessDialog.
 * <p>
 * <p> Revision 3.239  2006/06/27 17:45:42  sueh
 * <p> bug# 879 imodTrimmedVolume():  set swap yz in 3dmod if both swap yz and
 * <p> rotate x are not used.
 * <p>
 * <p> Revision 3.238  2006/06/22 20:54:08  sueh
 * <p> bug# 797 Catching io exception when opening 3dmods.
 * <p>
 * <p> Revision 3.237  2006/06/19 17:05:39  sueh
 * <p> bug# 851 Added closeImod() and closeImods().  Closing 3dmods when they are
 * <p> not needed.
 * <p>
 * <p> Revision 3.236  2006/06/16 15:23:48  sueh
 * <p> bug# 734 Moved track and use buttons from fiducial model dialog to beadtracker
 * <p> dialog.
 * <p>
 * <p> Revision 3.235  2006/06/14 00:05:32  sueh
 * <p> bug# 852 Moved classes that know a autodoc language.
 * <p>
 * <p> Revision 3.234  2006/06/09 19:48:50  sueh
 * <p> bug# 870 For Tomo Pos forcing the exit state of the dialog to be SAVE when
 * <p> running doneDialog and saveDialog.  Application manager only calls these
 * <p> functions when exiting or switching dialogs without (not using the Cancel,
 * <p> Postpone, or Done buttons).
 * <p>
 * <p> Revision 3.233  2006/06/09 16:28:04  sueh
 * <p> bug# 869 openTomogramCombinationDialog():  Saving combine script creation
 * <p> states in TomogramState.  For backward compatibility using combineScriptsExist
 * <p> to check for combine scripts if the state isn * <p>
 * <p> Revision 3.232  2006/06/08 19:03:41  sueh
 * <p> bug# 867 updateSplittiltParam:  Setting separate chunks.
 * <p>
 * <p> Revision 3.231  2006/06/07 22:23:05  sueh
 * <p> bug# 766 imodFixFiducials():  turning off auto center when fix fiducials is first run.
 * <p>
 * <p> Revision 3.230  2006/06/05 15:59:46  sueh
 * <p> bug# 766 getParamFileStorableArray():  Add the option have elements in the storable array that aer set by the base manager.
 * <p>
 * <p> Revision 3.229  2006/05/23 21:00:13  sueh
 * <p> bug# 617 Added okToFiducialModelTrack() and
 * <p> okToMakeFiducialModelSeedModel().
 * <p>
 * <p> Revision 3.228  2006/05/22 22:34:14  sueh
 * <p> bug# 577 Removed unused function backgroundProcess(String, AxisID).
 * <p>
 * <p> Revision 3.227  2006/05/19 19:24:28  sueh
 * <p> bug# 866 Moved tomo pos functions to TomogramPositioningExpert and
 * <p> UIExpertUtilities.
 * <p>
 * <p> Revision 3.225  2006/05/16 21:17:47  sueh
 * <p> bug# 856 Fixing checkin comment
 * <p>
 * <p> Revision 3.224  2006/05/16 21:16:17  sueh
 * <p> bug# 856 Fixing checkin comment
 * <p>
 * <p> Revision 3.223  2006/05/16 21:13:52  sueh
 * <p> bug# 856 Removed dialogMatchMode from CombineParam.  Letting the
 * <p> screen save the script state, since it already is.  In
 * <p> loadCombineComscript(), added a call to
 * <p> TomogramCombinationDialog.updateDisplay() so that the tabs are correct.
 * <p> Passing the BaseManager to SolvematchParam so that it can get the fiducial
 * <p> model.  Changed TomogramCombinationDialog.setScriptMatchMode() to
 * <p> setCombineState() and making it handle the use corresponding list checkbox.
 * <p> Changed TomogramCombinationDialog.isUpToDate() to isChanged().
 * <p> IsChanged() looks at match direction and the use corresponding list checkbox
 * <p> and also looks at whether the scripts exist.  Synchronizing setup to initial
 * <p> on load.  The other doesn't work for match direction anymore.
 * <p>
 * <p> Revision 3.222  2006/05/12 17:12:44  sueh
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 3.221  2006/05/11 19:27:06  sueh
 * <p> bug 838 Getting tomopitch results from the log file placing them in
 * <p> Tomo Pos.  Dividing results into original, added, and total so that the user
 * <p> can see what happed.  Align.com and tilt.com are saved with the total,
 * <p> so roll the original and addded numbers into total (actually just hide them).
 * <p>
 * <p> Revision 3.220  2006/04/25 18:50:39  sueh
 * <p> bug# 787 Changed DialogType.SETUP to SETUP_RECON.
 * <p>
 * <p> Revision 3.219  2006/04/11 13:36:56  sueh
 * <p> bug# 809 Manage auto center and seed mode separately from
 * <p> openBeadFixer so that seed mode doesn't always have to be managed.
 * <p>
 * <p> Revision 3.218  2006/03/30 21:14:51  sueh
 * <p> bug# 809 getFiducialDiameterPerPixel:  diameter per pixel is the same for
 * <p> axis A and B.
 * <p>
 * <p> Revision 3.217  2006/03/30 21:08:33  sueh
 * <p> bug# 809 Seed fiducial model opens bead fixer and sets auto center and
 * <p> seed mode.  Fix fiducial model sets auto center.
 * <p>
 * <p> Revision 3.216  2006/03/27 19:15:09  sueh
 * <p> bug# 437 In saveCoarseAlignDialog, getting screen state parameters.
 * <p>
 * <p> Revision 3.215  2006/03/22 23:32:23  sueh
 * <p> bug# 498 In commitTestVolume(), don't backup the tomogram unless the
 * <p> trial tomogram exists.  In useMtfFilter(), don't backup the .ali file unless
 * <p> the _filt.ali file exists.
 * <p>
 * <p> Revision 3.214  2006/03/22 23:20:45  sueh
 * <p> bug# 498 When generating a tomogram, back up the .ali file when using
 * <p> the mtf filter file.  Also backup the tomogram when using a trial tomogram.
 * <p>
 * <p> Revision 3.213  2006/03/22 00:33:29  sueh
 * <p> bug# 836 Preventing useMtfFilter() from running when the axis is busy
 * <p>
 * <p> Revision 3.212  2006/03/20 17:47:03  sueh
 * <p> bug# 895 Changed DialogType to work with multiple managers, not just
 * <p> ApplicationManager.
 * <p>
 * <p> Revision 3.211  2006/03/16 01:46:23  sueh
 * <p> bug# 727, bug# 830, bug# 813, bug# 828
 * <p>
 * <p> Revision 3.210  2006/02/20 22:08:44  sueh
 * <p> bug# 828 Don't save values from Combine Setup tab to the .edf on exit
 * <p> because they might contradict values that where saved by Create Combine
 * <p> Scripts.  So Combine Setup does not hold the screen values, unlike other
 * <p> dialogs.  This is because the script values are set by Create Combine
 * <p> Scripts and not written directly from the screen.
 * <p>
 * <p> Revision 3.209  2006/02/07 00:08:28  sueh
 * <p> bug# 521 Getting the splitcombine process result display from
 * <p> ProcessResultDisplayFactory so that it is always the Restart at
 * <p> Volcombine button.
 * <p>
 * <p> Revision 3.208  2006/02/06 20:57:22  sueh
 * <p> bug# 521 Removed unnecessary getParameters(ReconScreenState)
 * <p> functions from process dialogs; toggle buttons that are managed by
 * <p> ProcessResultDisplayFactory can save their own state.
 * <p>
 * <p> Revision 3.207  2006/01/31 20:36:04  sueh
 * <p> bug# 521 Calling BaseManager.doneProcessDialogin all done functions.
 * <p>
 * <p> Revision 3.206  2006/01/26 21:37:48  sueh
 * <p> bug# 401 Added PRocessResultDisplay parameters to the functions called
 * <p> by toggle buttons.
 * <p>
 * <p> Revision 3.205  2006/01/20 20:42:49  sueh
 * <p> bug# 401 Added ProcessResultDisplay functionality for tilt, splittilt,
 * <p> processchunks - tilt, newst, and blend.
 * <p>
 * <p> Revision 3.204  2006/01/12 17:00:04  sueh
 * <p> bug# 800 In doneSetupDialog(), check whether metaData exists before
 * <p> using it.
 * <p>
 * <p> Revision 3.203  2005/12/23 01:55:23  sueh
 * <p> bug# 675 Split the test option functionality.  Control headlessness with
 * <p> --headless.  This allow both JUnit and JfcUnit to use the special test
 * <p> functions.
 * <p>
 * <p> Revision 3.202  2005/12/12 21:48:32  sueh
 * <p> bug# 779 Made BaseManager.resetNextProcess() private.
 * <p>
 * <p> Revision for 20:20:21  sueh
 * <p> bug# 776 Added canSnapshot.
 't found. Changed
 * <p>
 * <p> Revision 3.200  2005/11/29 22:17:29  sueh
 * <p> Deleted aligned image stack:  The process bar said "stacks", which was
 * <p> misleading.
 * <p>
 * <p> Revision 3.199  2005/11/21 21:50:34  sueh
 * <p> bug# 761 updateSplittiltParam() should fail if
 * <p> ParallelPanel.getParameters(SplittiltParam) fails.
 * <p>
 * <p> Revision 3.198  2005/11/19 01:40:07  sueh
 * <p> bug# 744 Moved functions only used by process manager post
 * <p> processing and error processing from Commands to ProcessDetails.
 * <p> This allows ProcesschunksParam to be passed to DetackedProcess
 * <p> without having to add unnecessary functions to it.
 * <p>
 * bug# 867 3.197  2005/11/10 17:49:52  sueh
 * <p> Constructor should not be public
 * <p>
 * <p> Revision 3.196  2005/11/04 00:52:14  sueh
 * <p> fixed copyright
 * <p>
 * <p> Revision 3.195  2005/11/03 21:20:38  sueh
 * <p> bug# 755 Stop opening combine when tomo gen is done.
 * <p>
 * <p> Revision 3.194  2005/11/02 21:31:18  sueh
 * <p> bug# 731 In createCombineScripts():  Setting process end state to failed
 * <p> if combine setup fails.
 * <p>
 * <p> Revision 3.193  2005/10/31 17:51:34  sueh
 * <p> bug# 730 Added saveDialog which gets the data from the dialogs and
 * <p> saves it to the .edf file.  Calling saveDialog() from exitProgram().  Split
 * <p> the done functions into done and save functions.  The save functions
 * <p> save the dialog data and the done functions call the save functions and
 * <p> then quit the dialog.  Added canChangeParamFileName() - enabling Save
 * <p> As based on whether the param file has been loaded.
 * <p>
 * <p> Revision 3.192  2005/10/29 00:01:29  sueh
 * <p> bug# 725 reseting next processing when running crossCorrelate and
 * <p> eraser to prevent an infinite loop.
 * <p>
 * <p> Revision 3.191  2005/10/28 21:46:15  sueh
 * <p> bug# 725 setComScripts() do not fail on error messages if exitValue is 0.
 * <p> This means that copytomocoms succeeded but a process it called printed
 * <p> an error message.
 * <p>
 * <p> Revision 3.190  2005/10/28 18:44:56  sueh
 * <p> bug# 746, bug# 725 deleted updateSplitcombineParam().
 * <p>
 * <p> Revision 3.189  2005/10/27 00:05:27  sueh
 * <p> bug# 725 Processing the b stack when it is added late.  Added functions:
 * <p> extractmagrad, extractpieces, extracttilts, preCrossCorrelate, preEraser,
 * <p> and processBStack.
 * <p>
 * <p> Revision 3.188  2005/10/19 00:17:43  sueh
 * <p> bug# 673 Added updateArchiveDisplay() to update the display of the
 * <p> archive button on the clean up panel.
 * <p>
 * <p> Revision 3.187  2005/10/18 22:09:54  sueh
 * <p> bug# 737 Setting nextProcess after running process, because the axis
 * <p> busy test is run when running process.  bug# 727 Can't reproduce this
 * <p> bug so added some prints to the error log to document it, if it appears
 * <p> again.
 * <p>
 * <p> Revision 3.186  2005/10/15 00:28:13  sueh
 * <p> bug# 532 Called BaseManager.setParallelDialog() when opening each
 * <p> dialog.
 * <p>
 * <p> Revision 3.185  2005/10/14 21:04:37  sueh
 * <p> bug# 730 Changed loadedTestParamFile to loadedParamFile.
 * <p>
 * <p> Revision 3.184  2005/10/13 22:08:48  sueh
 * <p> Bug# 532 In synchronized(), always copying all fields
 * <p>
 * <p> Revision 3.183  2005/09/29 18:38:21  sueh
 * <p> bug# 532 Preventing Etomo from saving to the .edf or .ejf file over and
 * <p> over during exit.  Added BaseManager.exiting and
 * <p> saveIntermediateParamFile(), which will not save when exiting it true.
 * <p> Setting exiting to true in BaseManager.exitProgram().  Moved call to
 * <p> saveParamFile() to the child exitProgram functions so that the param file
 * <p> is saved after all the done functions are run.
 * <p>
 * <p> Revision 3.182  2005/09/27 21:08:25  sueh
 * <p> bug# 532 Added ReconScreenState screenStateA and B to hold screen
 * <p> state information that is not saved in the comscripts and not used to run
 * <p> processes.  Calling mainPanel.done() during exit to save the state of the
 * <p> parallel processing dialog.  Added getParamFileStorableArray(), which
 * <p> creates, fills, and returns storable array for the .edf file.  Removed
 * <p> getNumStorables().
 * <p>
 * <p> Revision 3.181  2005/09/22 20:40:56  sueh
 * <p> bug# 532 changed packDialogs to packPanel and move them to
 * <p> BaseManager.  The packs done in packDialog where for the processor
 * <p> table and where sent through dialogs.  The processor table is no longer
 * <p> associated with the dialogs.  So packing the processor table is done
 * <p> through the main panel.
 * <p>
 * <p> Revision 3.180  2005/09/21 16:03:47  sueh
 * <p> bug# 532 Moved processchunks() and setThreadName() to BaseManager.
 * <p>
 * <p> Revision 3.179  2005/09/19 16:33:22  sueh
 * <p> bug# 532 removed code to find an intermittent bug from
 * <p> doneTomgramGeneration() and updateTiltCom().
 * <p>
 * <p> Revision 3.178  2005/09/16 21:20:26  sueh
 * <p> bug# 532 Changed ParallelDialog.resetParallelPanel() to
 * <p> resetParallelProgressDisplay() because ParallelDialog is generic.
 * <p>
 * <p> Revision 3.177  2005/09/16 20:52:14  sueh
 * <p> bug# 532 When parallel processing checkbox is on, using nextProcess to
 * <p> run processchunks after combine finishes everything but volcombine.
 * <p>
 * <p> Revision 3.176  2005/09/16 17:14:07  sueh
 * <p> bug# 532 Added processchunksTilt() and processchunksVolcombine(), so
 * <p> startNextProcess() can distinguish between the two processchunks calls.
 * <p> Stopped passing the dialog to processchunks; getting it in
 * <p> processchunksTilt() and processchunkVolcombine().  Added functions
 * <p> updateSplitcombine and splitcombine.
 * <p>
 * <p> Revision 3.175  2005/09/02 18:51:05  sueh
 * <p> bug# 720 Pass the manager to TrimvolParam instead of propertyUserDir
 * <p> because TrimvolParam is constructed by MetaData before
 * <p> propertyUserDir is set.
 * <p>
 * <p> Revision 3.174  2005/08/25 01:43:45  sueh
 * <p> bug# 688 calling updateBlendmontInXcorr from doneCoarseAlignment to
 * <p> make sure that xcorr is properly updated
 * <p>
 * <p> Revision 3.173  2005/08/24 22:28:24  sueh
 * <p> bug# 715 Passing the param to crossCorrelate() so it can be used in
 * <p> postProcess() and errorProcess().  Use TiltxcorrParam when blendmont
 * <p> will not run.  Use BlendmontParam when blendmont will run.
 * <p>
 * <p> Revision 3.172  2005/08/22 15:51:15  sueh
 * <p> bug# 532 In doneTomgramGenartionDialog, call
 * <p> TomogramGenerationDialog.stopParallelPanel() to stop parallel panel
 * <p> from sampling the load average when the panal is not displayed.  Added
 * <p> pause().
 * <p>
 * <p> Revision 3.171  2005/08/15 17:46:40  sueh
 * <p> reformatting
 * <p>
 * <p> Revision 3.170  2005/08/11 23:20:47  sueh
 * <p> bug# 711  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Move setting about whether a type of 3dmod run can be
 * <p> binned in Z to ImodManager.
 * <p>
 * <p> Revision 3.169  2005/08/09 19:52:07  sueh
 * <p> bug# 711 Pass Run3dmodMenuOption to all imod functions, except the
 * <p> ones that use modv.
 * <p>
 * <p> Revision 3.168  2005/08/04 19:04:39  sueh
 * <p> bug# 532 added packDialogs() to request sizing functionality that is not
 * <p> performed by pack().
 * <p>
 * <p> Revision 3.167  2005/08/01 17:56:08  sueh
 * <p> bug# 532 Added processchunks().  Added processchunks to
 * <p> startNextProcess().
 * <p>
 * <p> Revision 3.166  2005/07/29 19:43:43  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 3.165  2005/07/29 00:31:20  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.164  2005/07/26 16:57:09  sueh
 * <p> bug# 701 Pass ProcessEndState to the progress bar when stopping it.
 * <p>
 * <p> Revision 3.163  2005/07/21 21:25:16  sueh
 * <p> bug# 532 added splittilt() and updateSplittiltParam().  Added
 * <p> setPauseButton to let the main panel manage the pause button the same
 * <p> way as it manages the kill button.
 * <p>
 * <p> Revision 3.162  2005/07/20 16:56:55  sueh
 * <p> bug# 705 Stop printing the stack trace for IOException bugs coming from
 * <p> MRCHeader, because its filling up the error log with exceptions that are
 * <p> related to real problems.
 * <p>
 * <p> Revision 3.161  2005/07/18 22:01:20  sueh
 * <p> bug# 532 Added parallelProcessTilt().
 * <p>
 * <p> Revision 3.160  2005/07/14 21:55:23  sueh
 * <p> bug# 626 Added montaged views to wholeTomogram().  Added
 * <p> updateBlendCom(TomogramPostioningDialog, AxisID).  Passing back
 * <p> BlendmontParam from both updateBlendCom()'s because they are being
 * <p> updated from the screen..
 * <p>
 * <p> Revision 3.159  2005/07/11 22:42:37  sueh
 * <p> bug# 619 Placed the parallel processing panel on the tomogram
 * <p> generation screen.  Aded functions:  parallelProcessTiltDemo,
 * <p> signalTiltCompleted, signalTiltError, signalTiltKilled, and
 * <p> splitParallelProcessTilt.  Removed functions:
 * <p> dummySplitParallelProcess, dummySplitParallelProcess,
 * <p> getParallelDialog, and parallelProcess.
 * <p>
 * <p> Revision 3.158  2005/07/01 21:07:01  sueh
 * <p> bug 619 Added demo functions to open a parallel processing JDialog.
 * <p>
 * <p> Revision 3.157  2005/06/22 23:34:58  sueh
 * <p> bug# 583 getStackBinning() was not returning default binning if the result
 * <p> of the binning calculation was < 1.
 * <p>
 * <p> Revision 3.156  2005/06/21 00:02:58  sueh
 * <p> bug# 522 Added pass-through function call to
 * <p> BaseProcessManager.touch() for MRCHeaderTest.
 * <p>
 * <p> Revision 3.155  2005/06/20 16:39:35  sueh
 * <p> bug# 522 Made MRCHeader an n'ton.  Getting instance instead of
 * <p> constructing in getMrcHeader().
 * <p>
 * <p> Revision 3.154  2005/06/13 23:34:47  sueh
 * <p> bug# 583 Preventing tilt.com from being overwritten with a default
 * <p> imageBinned after the .ali file is deleted.  DoneTomogramGeneration()
 * <p> needs update and save tilt.com, but the result from getStackBinning will
 * <p> be wrong if the .ali file has been deleted.  Move the responsibility for
 * <p> getting the right imageBinned to TiltParam.  Modify getStackBinning() to
 * <p> have an option to return a null value when it fails to calculate the stack
 * <p> binning.  If TiltParam.setImageBinned() gets a null value and
 * <p> imageBinned is not null, it won't override the current imageBinned value.
 * <p>
 * <p> Revision 3.153  2005/06/10 22:43:43  sueh
 * <p> bug# 583, bug# 682, bug# 584, bug# 679  Moved binning calculation to
 * <p> ApplicationManager.  Storing screen binning for Tomo Pos and Tomo
 * <p> Gen in MetaData separately (Tomo Pos default is 3).  Upgraded
 * <p> align.com and tilt.com to have all unbinned parameters and a binning
 * <p> value.  No longer managing full image size in tilt.com, except to upgrade
 * <p> the file.  Setting xfproduct scale shifts in align.com the same way binning
 * <p> is set.  Added functions:  getBackwardCompatibleAlignBinning,
 * <p> getBackwardCompatibleTiltBinning, getStackBinning,
 * <p> upgradeOldAlignCom, updateOldTiltCom.
 * <p>
 * <p> Revision 3.152  2005/06/03 19:50:32  sueh
 * <p> bug# 671 getMrcHeader(): use the full path in the stack file name.
 * <p> SaveDialog(): check axisType and pass the correct axisID.
 * <p>
 * <p> Revision 3.151  2005/06/01 21:18:41  sueh
 * <p> bug# 667 Remove the Controller classes.  Trying make meta data and
 * <p> app manager equals didn't work very well.  Meta data is created by and
 * <p> managed by app mgr.
 * <p>
 * <p> Revision 3.150  2005/05/19 20:46:10  sueh
 * <p> bug# 662 Added renameXrayStack() to rename the _xray.st.gz file to
 * <p> _xray.st.gz.#.
 * <p>
 * <p> Revision 3.149  2005/05/18 22:26:26  sueh
 * <p> bug# 662 Added functions to archive original stack:
 * <p> archiveOriginalStack, deleteOriginalStack, and getArchiveInfo.  Also
 * <p> modified replaceRawStack() to refresh the archive display in the Clean Up
 * <p> dialog.  Added an archiveorig option to startNextProcess() to get the
 * <p> second axis.
 * <p>
 * <p> Revision 3.148  2005/05/17 19:02:38  sueh
 * <p> bug# 663 Changed updateDataParameter() to setStatusBarText().
 * <p>
 * <p> Revision 3.147  2005/05/09 21:32:36  sueh
 * <p> bug# 658 In updateTrackCom() printing stack trace when there is an
 * <p> exception.
 * <p>
 * <p> Revision 3.146  2005/04/26 17:31:51  sueh
 * <p> bug# 615 Change the name of the UIHarness member variable to
 * <p> uiHarness.
 * <p>
 * <p> Revision 3.145  2005/04/25 20:28:21  sueh
 * <p> bug# 615 Passing the axis where the command originated to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p>
 * <p> Revision 3.144  2005/04/07 21:47:09  sueh
 * <p> bug# 626 Added makeDistortionCorrectionStack() to run undistort.com.
 * <p> Added setEnabledFixEdgedWithMidas() to enable the Fix Edges With Midas
 * <p> button.
 * <p>
 * <p> Revision 3.143  2005/03/29 23:48:53  sueh
 * <p> bug# 618 Fixed problem with switching dialogs.  PostProcessing and
 * <p> CleanUp where not running open and done functions when the user
 * <p> changed dialogs using the left-hand buttons.  Nulled out dialog variables.
 * <p> Also added cleanup dialog to getDialog().
 * <p>
 * <p> Revision 3.142  2005/03/29 19:47:44  sueh
 * <p> bug# 623 When montaging, setting full image size when updating tilt.com
 * <p> from the .ali file.  When the .ali file is not available set the full image size
 * <p> from running goodframe on the X and Y sizes in the .st file.
 * <p>
 * <p> Revision 3.141  2005/03/24 20:18:35  sueh
 * <p> bug# 621 Fixed bug where post processing button didn't get highlighted.
 * <p>
 * <p> Revision 3.140  2005/03/24 17:48:15  sueh
 * <p> bug# 621 Added Clean Up dialog.
 * <p>
 * <p> Revision 3.139  2005/03/19 01:09:36  sueh
 * <p> adding comments
 * <p>
 * <p> Revision 3.138  2005/03/11 01:57:22  sueh
 * <p> bug# 612 Change nextProcess to support axis A and B.
 * <p>
 * <p> Revision 3.137  2005/03/11 01:31:31  sueh
 * <p> bug# 533 Removed newst updates that where happening when Montage
 * <p> was set.  Sending TomogramGeneration.linearInterpolation to blend.com.
 * <p> Update blend.com in doneTomogramGeneration.
 * <p>
 * <p> Revision 3.136  2005/03/09 22:45:34  sueh
 * <p> bug# 533 In newst() running blend instead of newst when doing a montage.
 * <p>
 * <p> Revision 3.135  2005/03/09 17:58:16  sueh
 * <p> bug# 533 In done functions, only update newst when the view type is not
 * <p> montage.  ProcessManager.crossCorrelate needs to know whether
 * <p> blendmont will actually be run.
 * <p>
 * <p> Revision 3.134  2005/03/08 18:29:54  sueh
 * <p> bug# 533 In midasRawStack() call midasBlendStack() instead of
 * <p> midasRawStack() for montaging.
 * <p>
 * <p> Revision 3.133  2005/03/08 00:39:09  sueh
 * <p> bug# 533 CoarseAlign(): get the preblend command file name from
 * <p> BlendmontParam.
 * <p>
 * <p> Revision 3.132  2005/03/07 23:57:01  sueh
 * <p> bug# 533 Added midasEdges to call midas for fixing edge with a montage
 * <p> view.  Substituting preblend for prenewst in coarse align with a montage
 * <p> view.
 * <p>
 * <p> Revision 3.131  2005/03/04 00:06:31  sueh
 * <p> bug# 533 Changes for montaging only.  imodErasedStack(): add piece list
 * <p> file to 3dmod call.  ImodManualErase():  add frames to 3dmod call.
 * <p> UpdateXcorrCom():  Update blendmont param.  Update goto param based
 * <p> on whether blendmont needs to be called.
 * <p>
 * <p> Revision 3.130  2005/03/02 23:10:57  sueh
 * <p> bug# 533 imodXrayModel():  set frames to true if processing a montage.
 * <p>
 * <p> Revision 3.129  2005/03/01 22:07:03  sueh
 * <p> bug# 610 getDialog():  test for dialogType is null and return null.
 * <p>
 * <p> Revision 3.128  2005/03/01 20:49:42  sueh
 * <p> bug# 607 Catching Throwable in exitProgram and returning true to make
 * <p> sure that Etomo can always exit.  Bug# 610 Keeping track of current
 * <p> dialog type in ApplicationManager by setting it in each open function.
 * <p> Changing saveDialog to saveCurrentDialog and use currentDialogType to
 * <p> pick the dialog to save.
 * <p>
 * <p> Revision 3.127  2005/02/18 23:59:08  sueh
 * <p> bug# 606 Removed MetaData (Setup) zfactors, fiducialess, wholetomogram,
 * <p> and localalignments.  Add them for A and B.
 * <p>
 * <p> Revision 3.126  2005/02/17 19:25:44  sueh
 * <p> bug# 606 Pass AxisID when setting and getting makeZFactors,
 * <p> newstFiducialessAlignment, and usedLocalAlignments.
 * <p>
 * <p> Revision 3.125  2005/02/17 02:38:36  sueh
 * <p> Removing print statements.
 * <p>
 * <p> Revision 3.124  2005/02/16 22:31:23  sueh
 * <p> bug# 604 Added checkForSharedDirectory() to check if there is an .edf
 * <p> file using different stacks in the directory.
 * <p>
 * <p> Revision 3.123  2005/02/07 21:48:17  sueh
 * <p> bug# 594 Added isSetupChanged() to check if user has enter data into
 * <p> Setup dialog.
 * <p>
 * <p> Revision 3.122  2005/01/26 04:23:18  sueh
 * <p> bug# 83 mtfFilter():  Removed called to startProgressBar().
 * <p>
 * <p> Revision 3.121  2005/01/21 22:05:55  sueh
 * <p> bug# 509 bug# 591  Moved the management of MetaData to the Controller
 * <p> class.  Moved the set/get of tranferfid metadata fields to TransferfidPanel.
 * <p> get/setParameters.  Clarifying EtomoNumber: using isNull() in stead of
 * <p> isSet().
 * <p>
 * <p> Revision 3.120  2005/01/14 02:57:30  sueh
 * <p> bug# 511 Added saveDialog() and getDialog().  In done functions, added a
 * <p> check for exit state ==  save to avoid changing the process state or asking
 * <p> to close 3dmods when the user only switched dialogs.
 * <p>
 * <p> Revision 3.119  2005/01/12 00:40:32  sueh
 * <p> bug# 579 Renaming enableZFactors() to enableTiltParameters().  If
 * <p> newstFiducialessAlignment isn't set, use the checkbox value on the
 * <p> screen.
 * <p>
 * <p> Revision 3.118  2005/01/10 23:18:33  sueh
 * <p> bug# 578 Initializing TomogramState.  Changing enableZFactor() to not
 * <p> need backward compatibility information on newstFiducialessAlignment.
 * <p>
 * <p> Revision 3.117  2005/01/08 00:28:05  sueh
 * <p> bug# 578 Added enableZFactors() to enable useZFactors checkbox in
 * <p> tomogram generation.  Set commandMode in NewstParam so
 * <p> ProcessManager.postProcess() can tell whether Full Align Stack was run.
 * <p> Set fiducialessAlignment in NewstParam when Full Align Stack is run.
 * <p> Pass newstParam to ProcessManager.newst() so it can be queried in
 * <p> postProcess().
 * <p>
 * <p> Revision 3.116  2005/01/05 18:53:12  sueh
 * <p> bug# 578 Pass tiltalign to processManager.fineAlignment() by passing it
 * <p> back from updateAlignCom().
 * <p>
 * <p> Revision 3.115  2004/12/28 23:40:35  sueh
 * <p> bug# 567 In updateTiltCom(), adapt to new TiltalignParam names and types.
 * <p>
 * <p> Revision 3.114  2004/12/16 02:51:52  sueh
 * <p> bug# 559 Updating status bar with param file when ending setup dialog.
 * <p>
 * <p> Revision 3.113  2004/12/16 01:29:04  sueh
 * <p> bug# check whether squeezvol output file is flipped before displaying it in
 * <p> 3dmod.
 * <p>
 * <p> Revision 3.112  2004/12/14 21:21:26  sueh
 * <p> bug# 565: Fixed bug:  Losing process track when backing up .edf file and
 * <p> only saving metadata.  bug# 572:  Removing state object from meta data
 * <p> and managing it with a manager object.
 * <p>
 * <p> Revision 3.111  2004/12/13 19:08:19  sueh
 * <p> bug# 565 Saving process track to edf file as well as meta data in
 * <p> doneSetupDialog.
 * <p>
 * <p> Revision 3.110  2004/12/09 04:46:29  sueh
 * <p> bug# 565 Removed isParamFileDirty.  Saved meta data and process track
 * <p> in done function.
 * <p>
 * <p> Revision 3.109  2004/12/08 21:16:14  sueh
 * <p> bug# 564 Changed get and set Input and Output File functions to get and set
 * <p> Input and Output FileName to avoid confusion with new getOutputFile()
 * <p> function.
 * <p>
 * <p> Revision 3.108  2004/12/04 01:25:59  sueh
 * <p> bug# 557 Added imodSqueezedVolume().
 * <p>
 * <p> Revision 3.107  2004/12/03 20:18:50  sueh
 * <p> bug# 556 Do not load SetupParam into combine dialog or update it in
 * <p> volcombine if SetParam not valid (has the wrong set name) or is null.
 * <p> Disable ReductionFactor in combine dialog if SetParam is missing or
 * <p> invalid.
 * <p>
 * <p> Revision 3.106  2004/12/03 02:24:49  sueh
 * <p> bug# 568 Added getTomogramMetaData() to get a writeable meta data.
 * <p>
 * <p> Revision 3.105  2004/12/02 18:23:09  sueh
 * <p> bug# 557 Added squeezevol() and loaded sqeezevol parameters into
 * <p> post processing dialog.
 * <p>
 * <p> Revision 3.104  2004/11/30 00:32:45  sueh
 * <p> bug# 556 Adding functions to parse volcombine.
 * <p>
 * <p> Revision 3.103  2004/11/19 22:31:32  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.101.2.14  2004/11/12 22:42:54  sueh
 * <p> bug# 520 Moved imodGetRubberbandCoordinates to base class.
 * <p>
 * <p> Revision 3.101.2.13  2004/10/29 01:15:02  sueh
 * <p> bug# 520 Removing unecessary functions that provided services to
 * <p> BaseManager.  BaseManager can use get... functions to get the
 * <p> mainPanel, metaData, and processTrack.
 * <p>
 * <p> Revision 3.101.2.12  2004/10/21 17:48:01  sueh
 * <p> bug# 520 Fixed status bar by called updateDataParameters when opening
 * <p> the processing panel.
 * <p>
 * <p> Revision 3.101.2.11  2004/10/11 01:55:28  sueh
 * <p> bug# 520 moved responsibility for mainPanel, metaData, processTrack,
 * <p> and progressManager to child classes.  Used abstract functions to use
 * <p> these variables in the base classes.  This is more reliable and doesn't
 * <p> require casting.
 * <p>
 * <p> Revision 3.101.2.10  2004/10/08 21:11:33  sueh
 * <p> bug# 520 Backed out conversion from properties to user.dir.
 * <p>
 * <p> Revision 3.101.2.9  2004/10/08 15:36:53  sueh
 * <p> bug# 520 Setting workingDirName instead of system property for manager
 * <p> level working directory.
 * <p>
 * <p> Revision 3.101.2.8  2004/10/01 20:56:06  sueh
 * <p> bug# 520 Moving getMetaDAta() from base class to this class.
 * <p>
 * <p> Revision 3.101.2.7  2004/09/29 17:27:48  sueh
 * <p> bug# 520 Removed MainPanel pass-through functions.  Casting mainPanel
 * <p> and other members from BaseManager to private local variables in the
 * <p> create functions.
 * <p>
 * <p> Revision 3.101.2.6  2004/09/15 22:32:14  sueh
 * <p> bug# 520 call openMessageDialog in mainPanel instead of mainFrame
 * <p>
 * <p> Revision 3.101.2.5  2004/09/13 16:24:54  sueh
 * <p> bug# 520 Added isNewManager(); true if setup dialog came up.  Telling
 * <p> EtomoDirectory the dataset name when completes successfully.
 * <p>
 * <p> Revision 3.101.2.4  2004/09/09 21:46:40  sueh
 * <p> bug# 552 loading combine.com while creating combine scripts
 * <p>
 * <p> Revision 3.101.2.3  2004/09/08 19:20:38  sueh
 * <p> bug# 520 Casting mainPanel to MainTomogramPanel where necessary.
 * <p> Calling MainFrame.show in EtomoDirector.  Moving kill() to super class.
 * <p>
 * <p> Revision 3.101.2.2  2004/09/07 17:49:36  sueh
 * <p> bug# 520 getting mainFrame and userConfig from EtomoDirector, moved
 * <p> settings dialog to BaseManager,  moved backupFiles() to BaseManager,
 * <p> moved exitProgram() and processing variables to BaseManager, split
 * <p> MainPanel off from MainFrame
 * <p>
 * <p> Revision 3.101.2.1  2004/09/03 20:32:54  sueh
 * <p> bug# 520 beginning to remove functions and variables that can go in
 * <p> BaseManager - removed functions associated with the constructor, moved
 * <p> constructor code to EtomoDirector.  Added create functions to override
 * <p> abstract create functions in BaseManager
 * <p>
 * <p> Revision 3.101  2004/09/02 19:51:18  sueh
 * <p> bug# 527 adding ImodMAnager.setOpenContour calls to
 * <p> imodMAatchingModel()
 * <p> bug# 541 remove unnecessary setBinning call in imodMatchingModel.
 * <p> Don't need to set binning to its default
 * <p>
 * <p> Revision 3.100  2004/08/31 16:52:53  sueh
 * <p> bug# 508 removing JUnit tests that require an X server
 * <p>
 * <p> Revision 3.99  2004/08/26 01:09:51  sueh
 * <p> bug# 508 handling exiting while a background process is
 * <p> running: adding setBackgroundThreadName() and variables
 * <p> backgroundProcessA and backgroundProcessNameA.  Using
 * <p> setBackgroundThreadName() in combine, matchvol1, patchcorr,
 * <p> matchorwarp, and volcombine.  Reseting background thread
 * <p> variables in processDone().  Adding more information to the
 * <p> processes running dialog in exitProgram().
 * <p>
 * <p> Revision 3.98  2004/08/20 22:57:12  sueh
 * <p> bug# 515 improving error handling for Setup dialog
 * <p> Changed:
 * <p> doneSetupDialog
 * <p>
 * <p> Revision 3.97  2004/08/20 21:49:27  sueh
 * <p> bug# 508 made some private items protected so they can by
 * <p> inherited classes.
 * <p> Added:
 * <p> getTest
 * <p> Changed:
 * <p> tomogram CombinationDialog
 * <p> updateComdeSc
 * <p>
 * <p> Revision 3.96  2004/08/19 03:09:24  sueh
 * <p> bug# 508 Added a --selftest option to tell objects to perform extra tests.
 * <p> Used "selftest" because "test" was already taken.  Created load and
 * <p> update functions for the combine comscript.  The update function uses
 * <p> CombineComscriptState to set the start and end command that
 * <p> combine.com will run. Changed the combine functions matchvol1,
 * <p> matchorwarp, etc so that they run combine.com.  Removed combine
 * <p> functions from startNextProcess().  Pulled together functions that had
 * <p> been split up so they could run from either nextProcess or restart; now
 * <p> there is no next process for combine.  Add functions to control the
 * <p> progressBar and the TomogramCombination tab panes.
 * <p> Added:
 * <p> static boolean selfTest
 * <p> getCombineComscript()
 * <p> boolean isSelfTest()
 * <p> loadCombineComscript()
 * <p> matchvol1Combine()
 * <p> showPane(String comscript, String pane)
 * <p> startProgressBar(String label, AxisID axisID)
 * <p> updateCombineComscriptState(int startCommand)
 * <p> Changed:
 * <p> combine()
 * <p> matchorwarpCombine()
 * <p> matchorwarpTrial()
 * <p> openTomogramCombinationDialog()
 * <p> parseCommandLine(String[] args)
 * <p> patchcorrCombine()
 * <p> startNextProcess(AxisID axisID)
 * <p> volcombine()
 * <p> Deleted:
 * <p> matchorwarp(String next)
 * <p> matchvol1()
 * <p> patchcorr()
 * <p> restartAtMatchvol1()
 * <p>
 * <p> Revision 3.95  2004/08/06 23:12:19  sueh
 * <p> bug# 508 added commented out processMgr.combine() call
 * <p> to combine()
 * <p>
 * <p> Revision 3.94  2004/08/03 18:53:45  sueh
 * <p> bug# 519 get the correct tiltAngleSpec for axis B
 * <p>
 * <p> Revision 3.93  2004/08/02 23:51:31  sueh
 * <p> bug# 519 improving error handling in
 * <p> makeRawtltFile()
 * <p>
 * <p> Revision 3.92  2004/08/02 23:03:54  sueh
 * <p> bug# 519 added makeRawtltFile(): create a new .rawtlt file from
 * <p> starting angle, step angle, and # sections
 * <p>
 * <p> Revision 3.91  2004/07/24 01:56:03  sueh
 * <p> bug# 513 change packMainWindow() to call
 * <p> MainFrame.fitWindow() when the Basic/Advanced
 * <p> button is pressed.  packMainWindow() is also called when
 * <p> the Setup dialog is opened.  Calling fitWindow() doesn't cause
 * <p> any proglems for Setup.
 * <p>
 * <p> Revision 3.90  2004/07/23 00:09:30  sueh
 * <p> bug# 513 add function to get UserConfiguration
 * <p>
 * <p> Revision 3.89  2004/07/21 00:22:29  sueh
 * <p> bug# 507 In startNextProcess(), checking
 * <p> tomogramCombinationDialog before running volcombine.
 * <p>
 * <p> Revision 3.88  2004/07/12 17:41:07  sueh
 * <p> bug# 492 in imodPreview: getting metadata from
 * <p> SetupDialog.getDataset().  Also changed the metadata variable
 * <p> name to previewMetaData to avoid confusing it with the
 * <p> member variable metaData
 * <p>
 * <p> Revision 3.87  2004/07/02 00:43:43  sueh
 * <p> bug# 487 adding public functions to get FidXyz and MRCHeader
 * <p>
 * <p> Revision 3.86  2004/06/30 17:27:36  rickg
 * <p> Bug #488 Rotation.xf not being updated correctly, now done anytime
 * <p> the fiducialless parameters are updated.
 * <p>
 * <p> Revision 3.85  2004/06/30 00:16:25  sueh
 * <p> bug# 487 adding checkUpdateFiducialModel(), which compares
 * <p> the current and previous binning in Coarse Align.  This function
 * <p> is run when prenewst.com finishes.
 * <p>
 * <p> Revision 3.84  2004/06/28 22:10:29  rickg
 * <p> Bug #470 Moved the fiducial mode file copying to the same sections
 * <p> where the fiducialless is handled.
 * <p>
 * <p> Revision 3.83  2004/06/28 04:36:39  rickg
 * <p> Bug #470 Added method to update prexf and _nonfid.xf
 * <p>
 * <p> Revision 3.82  2004/06/25 23:26:51  sueh
 * <p> bug# 485 In openTomogramCombinationDialog, after loading
 * <p> the comscripts, synchronize from initial and final tabs to setup
 * <p> and update combineParams
 * <p>
 * <p> Revision 3.81  2004/06/25 21:18:31  sueh
 * <p> bug# 486 corrected set state to inprogress calls.
 * <p>
 * <p> Revision 3.80  2004/06/24 21:41:39  sueh
 * <p> bug# 482 making the call to
 * <p> loadSolvematch(boolean modelBased) compatible with
 * <p> previous versions of the .edf file.
 * <p>
 * <p> Revision 3.79  2004/06/24 20:22:05  sueh
 * <p> bug# 482 removed loadSolvematchshift and mod functions
 * <p>
 * <p> Revision 3.78  2004/06/24 18:43:00  sueh
 * <p> bug# 482 add loadSolvematch(boolean modelBased) to merge
 * <p> solvematchshift and mod into solvematch and add
 * <p> matchshifts
 * <p>
 * <p> Revision 3.77  2004/06/22 23:00:12  sueh
 * <p> bug# 455 Added open contours to sample() and fullSample().
 * <p> Substituted open() calls for model() calls.  Removed extra
 * <p> model() calls.  Setting preserveContrast separately.
 * <p>
 * <p> Revision 3.76  2004/06/22 02:04:28  sueh
 * <p> bug# 441 Created updateTrimvolParam() and added it to
 * <p> trimVolume() and donePostProcessing().  Moved the logic
 * <p> used to create input and output file names to TrimvolParam.
 * <p>
 * <p> Revision 3.75  2004/06/21 17:22:37  rickg
 * <p> Bug #461 z shift is scaled by the prealigned binning
 * <p>
 * <p> Revision 3.74  2004/06/21 00:03:53  sueh
 * <p> bug# 436 adding restartAtMatchvol1(), which updates later comscripts
 * <p> and calls matchvol1().  This is necessary because
 * <p> startNextProcess() does not need to update comscripts.
 * <p>
 * <p> Revision 3.73  2004/06/18 00:52:53  sueh
 * <p> bug# 476 put the logic to check if the fixed stack exists in a
 * <p> separate function and call it in replaceRawStack() and
 * <p> imodErasedStack
 * <p>
 * <p> Revision 3.72  2004/06/17 16:23:34  sueh
 * <p> bug# 466 in imodFullSample() turning on model mode.
 * <p>
 * <p> Revision 3.71  2004/06/15 20:08:49  rickg
 * <p> Bug #383 Run solvematch instead of solvematch{shift|mod}
 * <p>
 * <p> Revision 3.70  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.69  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.68  2004/06/10 18:27:12  sueh
 * <p> bug# 463 changing ImodManager.create() to newImod, using
 * <p> ImodManager.setOpenBeadFixer() instead of openBeadFixer
 * <p>
 * <p> Revision 3.67  2004/06/10 18:19:13  rickg
 * <p> Removed redudant dialog parameter fetching from mtffilter and
 * <p> imodPatchRegionModel
 * <p>
 * <p> Revision 3.66  2004/06/10 17:27:53  sueh
 * <p> bug# 462 remove ImodManager.reset() calls
 * <p>
 * <p> Revision 3.65  2004/06/05 00:59:36  sueh
 * <p> bug# 433 add updateLog to call ProcessManager.generateAlignLogs()
 * <p> when the ta logs are out of date
 * <p>
 * <p> Revision 3.64  2004/06/02 23:49:59  rickg
 * <p> Bug #391 only update the rotation.xf if the mode is fiducialess
 * <p>
 * <p> Revision 3.62  2004/06/01 18:53:48  rickg
 * <p> Bug #391 whole tomogram sampling state implementation
 * <p>
 * <p> Revision 3.61  2004/05/27 22:49:54  rickg
 * <p> Bug #391 offer to close preali window for fidless case
 * <p> standardized parameter gathering for tomogram positioning
 * <p> logic for calling updateFiducialAlign
 * <p>
 * <p> Revision 3.60  2004/05/26 04:57:14  rickg
 * <p> Bug #391 Fiducialess handling for the gneration dialog
 * <p>
 * <p> Revision 3.59  2004/05/26 00:00:32  sueh
 * <p> bug# 355 validate metaData when retrieve parameters from an
 * <p> .edf file
 * <p>
 * <p> Revision 3.58  2004/05/25 23:19:34  rickg
 * <p> Bug #391 Fiducialess implementation
 * <p>
 * <p> Revision 3.57  2004/05/24 20:16:45  sueh
 * <p> bug# 409 corrected .ali file names for mtffilter for single axis
 * <p>
 * <p> Revision 3.56  2004/05/21 21:55:13  sueh
 * <p> bug# 443 setting the output file name in tilta.com when
 * <p> running sample
 * <p>
 * <p> Revision 3.55  2004/05/21 02:21:38  sueh
 * <p> bug# 83 removing generic progress bar
 * <p>
 * <p> Revision 3.54  2004/05/15 01:43:55  sueh
 * <p> bug# 415 if saveTestParamIfNecessary() returns false, then the user
 * <p> pressed cancel our there was a problem saving, so don't exit.
 * <p>
 * <p> Revision 3.53  2004/05/15 00:41:00  sueh
 * <p> bug# 302 changing function name updateCombineParams()
 * <p>
 * <p> Revision 3.52  2004/05/13 20:15:10  sueh
 * <p> bug# 33  imodGetRubberbandCoordinates() checks for rubberband data
 * <p>
 * <p> Revision 3.51  2004/05/11 21:11:21  sueh
 * <p> bug# 302 Standardizing synchronization.
 * <p> Syncing with PatchRegionModel button push.
 * <p> UpdateCombineCom(), which updates metadata, is not necessary to
 * <p> run scripts so it doesn't need a success return value.
 * <p> Put syncing first in most cases.
 * <p> Follow syncing by updateCombineCom().
 * <p> In createCombineScripts() run either loadSolvematchShift() or
 * <p> loadSolvematchMod(), instead of only loadSolvematchShift.
 * <p> If MatchingModels is selected after Create scripts is done, call
 * <p> modelCombine() from combine() and visa versa.
 * <p>
 * <p> Revision 3.50  2004/05/07 19:54:45  sueh
 * <p> bug# 33 adding error processing to
 * <p> imodGetRubberbandCoordinates()
 * <p>
 * <p> Revision 3.49  2004/05/06 20:22:33  sueh
 * <p> bug# 33 added getRubberbandCoordinates()
 * <p>
 * <p> Revision 3.48  2004/05/05 21:24:53  sueh
 * <p> bug #430  If changes to .seed happened more recently then .fid, do not
 * <p> mv .fid .seed.  Otherwise backup .seed to .seed~ if not backing up
 * <p> .seed to _orig.seed.  Ok to use fid as seed when .seed does not exist.
 * <p> Orginal bug# 276.
 * <p>
 * <p> Revision 3.47  2004/05/03 22:29:21  sueh
 * <p> bug# 416 Move Bin by 2 settings between tabs in
 * <p> TomogramCombinationDialog.  Set binning in ImodManager.
 * <p>
 * <p> Revision 3.46  2004/05/03 18:04:41  sueh
 * <p> bug# 418 adding printStackTrace for more information
 * <p>
 * <p> Revision 3.45  2004/04/28 22:16:57  sueh
 * <p> bug# 320 user interaction goes in app manager
 * <p>
 * <p> Revision 3.44  2004/04/28 20:13:10  rickg
 * <p> bug #429 all file renames are now handled by the utilities static
 * <p> function to deal with the windows bug
 * <p>
 * <p> Revision 3.43  2004/04/28 00:47:52  sueh
 * <p> trying delete  full aligned stack so that rename works in Windows
 * <p>
 * <p> Revision 3.42  2004/04/28 00:40:29  sueh
 * <p> adding error message if rename filter file doesn't work
 * <p>
 * <p> Revision 3.41  2004/04/27 23:20:13  sueh
 * <p> bug# 320 warn the user about a stale patch vector model after
 * <p> any button press that will lead to creating a new patch vector
 * <p> model
 * <p>
 * <p> Revision 3.40  2004/04/27 22:02:27  sueh
 * <p> bug# 320 try to close the 3dmod with patch vector model before
 * <p> running patchcorr
 * <p>
 * <p> Revision 3.39  2004/04/27 01:01:34  sueh
 * <p> bug# 427 using tomopitch param when running tomopitch
 * <p>
 * <p> Revision 3.38  2004/04/26 21:20:32  sueh
 * <p> bug# 427 added code for tomopitch comscript (not finished)
 * <p>
 * <p> Revision 3.37  2004/04/26 18:36:52  rickg
 * <p> bug #426 Added full image code, fixed order of com script
 * <p> loading for tomogram positioning
 * <p>
 * <p> Revision 3.36  2004/04/26 17:15:54  sueh
 * <p> bug# 83 removing generic progress bar from patchcorr
 * <p>
 * <p> Revision 3.35  2004/04/26 00:24:59  rickg
 * <p> bug #426 Implemented full tomogram sampling
 * <p>
 * <p> Revision 3.34  2004/04/24 08:05:40  rickg
 * <p> bug #391 restructuring
 * <p>
 * <p> Revision 3.32  2004/04/22 23:31:33  rickg
 * <p> bug #391 Added processing for non fid aligne
 * <p> Added getIMODBinPath and getMetaData
 * <p>
 * <p> Revision 3.31  2004/04/19 19:25:04  sueh
 * <p> bug# 409 removing prints
 * <p>
 * <p> Revision 3.30  2004/04/16 02:20:39  sueh
 * <p> removing print statements
 * <p>
 * <p> Revision 3.29  2004/04/16 02:06:30  sueh
 * <p> bug# 409 No longer backing up .ali during useMtfFilter.
 * <p> Changed updateTransferfidEnabled() to updateDialog(FiducialModelDialog) - it
 * <p> does all updates on the FiducialModelDialog - and clarified the code.
 * <p> Create updateDialog(ProcessName) to call the specific updateDialog functions.
 * <p> Calling updateDialog(ProcessName in processDone()
 * <p> Added updateDialog() calls where needed.
 * <p>
 * <p> Revision 3.28  2004/04/06 19:04:06  rickg
 * <p> Print out java system info at start of session
 * <p>
 * <p> Revision 3.27  2004/04/06 17:51:03  rickg
 * <p> bug #391 basic single stage fiducialess alignment
 * <p>
 * <p> Revision 3.26  2004/03/29 21:00:48  sueh
 * <p> bug# 409 Added run mtffilter, view mtffilter result, use mtffilter
 * <p> result as the full
 * <p> aligned stack
 * <p>
 * <p> Revision 3.25  2004/03/24 03:08:17  rickg
 * <p> Bug# 395 Implemented ability to create binned tomogram
 * <p>
 * <p> Revision 3.24  2004/03/22 23:51:23  sueh
 * <p> bug# 83 starting the progress bar as soon as possible
 * <p>
 * <p> Revision 3.23  2004/03/13 00:35:05  rickg
 * <p> Bug# 390 Add prenewst and xfproduct management
 * <p>
 * <p> Revision 3.22  2004/03/11 23:58:14  rickg
 * <p> Bug #410 Newstack PIP transition
 * <p> Formatted code
 * <p>
 * <p> Revision 3.21  2004/03/10 00:44:32  sueh
 * <p> bug# 408 added getIMODCalibDirectory()
 * <p>
 * <p> Revision 3.20  2004/03/09 22:06:56  sueh
 * <p> bug# 407 adding IMOD_CALIB_DIR optional environment variable
 * <p>
 * <p> Revision 3.19  2004/03/06 00:22:39  sueh
 * <p> bug# 250 changed updateCombineCom() - remove duplicate code - call
 * <p> updateCombineCom(int) with NO_TAB
 * <p>
 * <p> Revision 3.18  2004/03/05 18:26:55  sueh
 * <p> bug# 250 changed patchcorrCombine() - updating CombineParams
 * <p> changed updateCombineCom(int) - change the parameter name because
 * <p> its only necessary when a copy to Setup is required
 * <p>
 * <p> Revision 3.17  2004/03/02 00:09:04  sueh
 * <p> bug #250 added updateCombineCom(int fromTab) - update CombineParams
 * <p> from a tab
 * <p> changed combine - default call to combine(int copyFromTab) with NO_TAB
 * <p> added combine(int copyFromTab) copies fields from copyFromTab to setup
 * <p> tab.
 * <p> modelCombine() - same as combine
 * <p> matchvol1() - same as combine
 * <p>
 * <p> Revision 3.16  2004/02/27 20:10:49  sueh
 * <p> bug# 250 changed createCombineScripts() - copied the
 * <p> Setup Use Matching Models value into the Initial tab
 * <p>
 * <p> Revision 3.15  2004/02/25 22:42:23  sueh
 * <p> bug# 403 removed unnecessary code
 * <p>
 * <p> Revision 3.14  2004/02/25 22:17:52  sueh
 * <p> bug# 403 resetState() is no longer setting imodManager to
 * <p> null
 * <p>
 * <p> Revision 3.13  2004/02/16 18:54:38  sueh
 * <p> bug# 276 Added makeFiducialModelSeedModel() to copy the
 * <p>  .seed file to the .fid file.
 * <p>
 * <p> Revision 3.12  2004/02/07 03:12:02  sueh
 * <p> bug# 169 Create ImodManager just once, set metadata
 * <p> separately, reformatted, changed imodRawStack() to
 * <p> ImodPreview()
 * <p>
 * <p> Revision 3.11  2004/02/05 18:05:32  sueh
 * <p> bug# 306 get the trimvol params from the screen and set
 * <p> swapYZ for 3dmod
 * <p>
 * <p> Revision 3.10  2004/02/05 00:19:07  sueh
 * <p> bug# 292 preserving contrast in view x-ray model, changed
 * <p> imodXrayModel()
 * <p>
 * <p> Revision 3.9  2004/02/04 18:12:46  sueh
 * <p> bug# 171 ask to automatically quit all running 3dmod
 * <p> programs.
 * <p>
 * <p> Revision 3.8  2004/01/22 21:09:39  rickg
 * <p> Get screen size in openSetupDialog instead of app init
 * <p>
 * <p> Revision 3.7  2004/01/17 00:14:17  rickg
 * <p> Added a --test argument that prevents the main window from
 * <p> opening up.
 * <p>
 * <p> Revision 3.6  2003/12/08 22:34:32  sueh
 * <p> bug# 169 adding new function imodRawStack
 * <p>
 * <p> Revision 3.5  2003/12/05 01:25:01  sueh
 * <p> bug242 moved getEnvironmentVariable() to Utilities
 * <p>
 * <p> Revision 3.4  2003/12/04 22:09:03  sueh
 * <p> bug242 Converting to new interface.
 * <p>
 * <p> Revision 3.3  2003/11/26 23:36:27  rickg
 * <p> Debug flag and getter changed to static.
 * <p>
 * <p> Revision 3.2  2003/11/11 00:24:52  sueh
 * <p> Bug349 imodFixFiducials(AxisID): call
 * <p> imodManager.openBeadFixer()
 * <p>
 * <p> Revision 3.1  2003/11/10 07:28:54  rickg
 * <p> ContextPopup initialization no longer needed
 * <p> Some more stderr printing on exceptions
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.93  2003/11/07 19:49:38  rickg
 * <p> Don't delete preali in delete aligned stacks code.
 * <p>
 * <p> Revision 2.92  2003/11/07 00:52:56  rickg
 * <p> Added test helper methods
 * <p> Changed transferfid parameter name to indicate that it is called with
 * <p> the destination axis
 * <p>
 * <p> Revision 2.91  2003/11/06 22:45:47  sueh
 * <p> cleaning up task tags and prints
 * <p>
 * <p> Revision 2.90  2003/11/06 21:41:27  sueh
 * <p> bug348 imodFineAlign(AsixID): removed calls to
 * <p> setFineAlignmentState() for processTrack and mainFrame.
 * <p>
 * <p> Revision 2.89  2003/11/05 20:31:48  rickg
 * <p> Bug #292 Added preserve contrast seed model and residual model
 * <p> opens
 * <p>
 * <p> Revision 2.88  2003/11/05 20:04:05  rickg
 * <p> Bug# 347 Message written to process monitor area
 * <p>
 * <p> Revision 2.87  2003/11/05 19:39:17  rickg
 * <p> Bug# 295 Query the combination dialog instead of the metaData
 * <p> object as to the state of the match direction for opening the patch
 * <p> region model.
 * <p>
 * <p> Revision 2.86  2003/11/05 19:20:21  rickg
 * <p> Bug# 290 Save tomo gen data when done is pressed
 * <p>
 * <p> Revision 2.85  2003/11/05 18:05:50  sueh
 * <p> bug278 created backupFile(File) to backup the .edf file
 * <p> called backupFile(File) from saveTestParamFile()
 * <p>
 * <p> Revision 2.84  2003/11/04 20:55:42  rickg
 * <p> Bug #345 IMOD Diriectory supplied by a static function from 
 * <p> ApplicationManager
 * <p>
 * <p> Revision 2.83  2003/10/27 23:55:41  rickg
 * <p> Bug# 283 Added method to open the tomopitch log file
 * <p>
 * <p> Revision 2.82  2003/10/24 21:45:09  rickg
 * <p> Spelling fix
 * <p>
 * <p> Revision 2.81  2003/10/23 23:06:13  sueh
 * <p> bug271 called isValid() in SetupDialog
 * <p>
 * <p> Revision 2.80  2003/10/22 21:32:02  rickg
 * <p> Bug# 287 Default value handling for SLICE OFFSET and SHIFT
 * <p>
 * <p> Revision 2.79  2003/10/21 23:45:05  rickg
 * <p> Added function to delete the aligned stacks
 * <p>
 * <p> Revision 2.78  2003/10/21 02:34:20  sueh
 * <p> Bug325 Pulled out generic default UI retrieval functionality and placed
 * <p> it in ButtonHelper.
 * <p>
 * <p> Revision 2.77  2003/10/20 22:02:13  rickg
 * <p> Bug# 228 Check to see if solve.xf exists before running matchvol1
 * <p>
 * <p> Revision 2.76  2003/10/20 17:32:09  rickg
 * <p> Use existence of combine com scripts
 * <p> ConstCombineParams.scriptsCreated flag
 * <p>
 * <p> Revision 2.75  2003/10/17 02:00:07  sueh
 * <p> Bug317 added new function - to retrieve default UI resources
 * <p>
 * <p> Revision 2.73  2003/10/10 23:17:01  sueh
 * <p> bug251 removing marks
 * <p>
 * <p> Revision 2.72  2003/10/07 22:40:40  sueh
 * <p> bug251 moved transferfid from fine alignment dialog
 * <p> to fiducial model dialog
 * <p>
 * <p> Revision 2.71  2003/10/05 23:59:35  rickg
 * <p> Bug# 252
 * <p> Adde complete message to progresss region for shor processes
 * <p>
 * <p> Revision 2.70  2003/10/05 21:36:05  rickg
 * <p> Bug# 256
 * <p> Catch SystemProcessException for attempted multiple
 * <p> processes in a axis
 * <p>
 * <p> Revision 2.69  2003/10/03 22:11:15  rickg
 * <p> Bug# 255
 * <p> added returns to catch sections for doneSetupDialog
 * <p> Don't want to continue to main window if copytomocoms did not
 * <p> succeed
 * <p>
 * <p> Revision 2.68  2003/10/02 18:57:46  sueh
 * <p> bug236 added testing:
 * <p> NewstParamTest
 * <p> ComScriptTest
 * <p>
 * <p> Removed marks
 * <p>
 * <p> Revision 2.67  2003/09/30 03:18:43  rickg
 * <p> Bug# 248
 * <p> changed openTestParamFile to loadTestParamFile
 * <p> split out resetState method
 * <p> added logic to openExistingData use a File object or
 * <p> open the File Dialog and drop into the setup page if it fails.
 * <p>
 * <p> Revision 2.66  2003/09/30 02:18:57  rickg
 * <p> Bug 249
 * <p> Proper New dialog behavior when not saving the EDF
 * <p> Also moved message dialogs to the mainFrame
 * <p>
 * <p> Revision 2.65  2003/09/29 23:34:57  sueh
 * <p> bug236 Added UseLinearInterpolation to
 * <p> TomogramGenerationDialog.
 * <p>
 * <p> UseLinearInterpolation:
 * <p> check box
 * <p> Advanced
 * <p> newst -linear
 * <p>
 * <p> Files:
 * <p> ComScriptManager.java
 * <p> ConstNewstParam.java
 * <p> NewstParam.java
 * <p> TomogramGenerationDialog.java
 * <p> ApplicationManager.java
 * <p>
 * <p> Revision 2.64  2003/09/26 19:46:16  sueh
 * <p> bug223 removed task marks
 * <p>
 * <p> Revision 2.63  2003/09/26 19:43:48  sueh
 * <p> bug223 no field should be persistant.  Changed MetaData.
 * <p> Added TransferfidNumberViews.
 * <p> Changed the done fine allignment and open fine allignment functions
 * <p> to work with MetaData
 * <p>
 * <p> Revision 2.62  2003/09/09 17:20:29  rickg
 * <p> Check to see if the _orig.st stack exists, do not replace if it does.
 * <p>
 * <p> Revision 2.61  2003/09/08 22:18:50  rickg
 * <p> Catch exception thrown buy ProcessManager.startComScript
 * <p>
 * <p> Revision 2.60  2003/09/08 05:44:47  rickg
 * <p> Added trial tilt
 * <p> Output for a single axis tomogram is changed to
 * <p> dataset_full.rec
 * <p>
 * <p> Revision 2.59  2003/08/20 21:57:09  rickg
 * <p> Only close imods in specified directory
 * <p>
 * <p> Revision 2.58  2003/08/05 21:35:22  rickg
 * <p> Retry commit, eclipse broken?
 * <p>
 * <p> Revision 2.57  2003/07/28 22:53:09  rickg
 * <p> Fixed postpone logic for combine panel.  Combine scripts
 * <p> created flag is now reset only when the CombineParams are
 * <p> modified.
 * <p>
 * <p> Combine postpone will now save combine sub script parameters
 * <p>
 * <p> Revision 2.56  2003/07/25 22:51:11  rickg
 * <p> Imod model mode management changes
 * <p> Save original stack as _orig.st
 * <p>
 * <p> Revision 2.55  2003/07/22 22:16:15  rickg
 * <p> Erased stack methods and trial mode
 * <p>
 * <p> Revision 2.54  2003/07/01 19:24:30  rickg
 * <p> Fixed progress bars for prenewst, newst and tomogram generation
 * <p>
 * <p> Revision 2.53  2003/06/27 20:33:28  rickg
 * <p> Changed below method to public
 * <p>
 * <p> Revision 2.52  2003/06/27 20:23:32  rickg
 * <p> Adde getter method for the com script manager
 * <p>
 * <p> Revision 2.51  2003/06/10 05:29:30  rickg
 * <p> Data persistence behavior of the combination and post
 * <p> processing panels now match the others.
 * <p>
 * <p> Revision 2.50  2003/06/10 05:15:23  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.49  2003/06/09 04:28:21  rickg
 * <p> Set state to in progress if any thing is exected for a given
 * <p> process panel
 * <p>
 * <p> Revision 2.48  2003/06/05 21:19:13  rickg
 * <p> Explicit transferfid B to A false setting
 * <p>
 * <p> Revision 2.47  2003/05/27 08:42:04  rickg
 * <p> Progress bar determinant delegate methods
 * <p>
 * <p> Revision 2.46  2003/05/23 22:49:41  rickg
 * <p> Spelling correction
 * <p>
 * <p> Revision 2.45  2003/05/23 14:29:11  rickg
 * <p> Progress bar determinant delegate methods
 * <p>
 * <p> Revision 2.44  2003/05/21 22:56:54  rickg
 * <p> Initial kill implementation
 * <p>
 * <p> Revision 2.43  2003/05/19 22:05:31  rickg
 * <p> Added openNewDataset method
 * <p> unset isDataParamDirty in daving method
 * <p>
 * <p> Revision 2.42  2003/05/15 22:24:24  rickg
 * <p> Reordered method sequence in opening processing panel to
 * <p> prevent slider from taking up all of the window.
 * <p>
 * <p> Revision 2.41  2003/05/15 20:13:05  rickg
 * <p> Fixed PLAF for windows
 * <p>
 * <p> Revision 2.40  2003/05/15 19:39:44  rickg
 * <p> Look and feel handling
 * <p>
 * <p> Revision 2.39  2003/05/14 23:22:51  rickg
 * <p> Exit if no IMOD_DIR is defined.  We can't run any of the non com scripts
 * <p>
 * <p> Revision 2.38  2003/05/14 21:45:27  rickg
 * <p> New trimvol constructor for windows
 * <p>
 * <p> Revision 2.37  2003/05/14 14:36:08  rickg
 * <p> Temporary change to volcombine
 * <p>
 * <p> Revision 2.36  2003/05/13 19:58:22  rickg
 * <p> TransferfidParams constructed with IMODDirectory File
 * <p>
 * <p> Revision 2.35  2003/05/10 19:12:56  rickg
 * <p> OS independent path implementation
 * <p>
 * <p> Revision 2.34  2003/05/10 18:01:56  rickg
 * <p> Fixes to get IMOD_DIR home and current working directory
 * <p> in a OS agnostic manner
 * <p>
 * <p> Revision 2.33  2003/05/09 23:25:36  rickg
 * <p> Working change to get env vars from all OSs
 * <p>
 * <p> Revision 2.32  2003/05/09 17:52:59  rickg
 * <p> include this in ImodManager constructor, needed for fiducial model calls
 * <p>
 * <p> Revision 2.31  2003/05/08 23:18:45  rickg
 * <p> Added --debug option, off by default
 * <p>
 * <p> Revision 2.30  2003/05/08 20:14:30  rickg
 * <p> Don't set main window location to (0,0) confuses SGI
 * <p>
 * <p> Revision 2.29  2003/05/08 19:58:53  rickg
 * <p> Work around for bug in File.getParent
 * <p>
 * <p> Revision 2.28  2003/05/07 23:04:29  rickg
 * <p> System property user.dir now defines the working directory
 * <p> Home is now read from the System properties
 * <p>
 * <p> Revision 2.27  2003/04/30 18:48:51  rickg
 * <p> Changed matchcheck* to a single imod instance
 * <p>
 * <p> Revision 2.26  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.25  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.24  2003/04/17 23:11:26  rickg
 * <p> Added cancel handling from post processing dialog
 * <p>
 * <p> Revision 2.23  2003/04/16 22:49:49  rickg
 * <p> Trimvol implmentation
 * <p>
 * <p> Revision 2.22  2003/04/16 00:13:54  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 2.21  2003/04/14 23:57:18  rickg
 * <p> Trimvol management changes
 * <p>
 * <p> Revision 2.20  2003/04/10 23:40:03  rickg
 * <p> Initial exit function handling of imod and other processes
 * <p> Initial openPostProcessingDialog
 * <p>
 * <p> Revision 2.19  2003/03/26 00:52:25  rickg
 * <p> Added button to convert patch_vector.mod to patch.out
 * <p>
 * <p> Revision 2.18  2003/03/22 00:40:35  rickg
 * <p> slovematchmod label change
 * <p>
 * <p> Revision 2.17  2003/03/20 21:18:55  rickg
 * <p> Added matchshift results button/access
 * <p>
 * <p> Revision 2.16  2003/03/20 16:58:42  rickg
 * <p> Added methods: imodMatchedToTomgram, matchorwarpTrial
 * <p> Added trial mode handling to matchorwarp
 * <p>
 * <p> Revision 2.15  2003/03/19 00:23:04  rickg
 * <p> Added imod patch vector model pass through
 * <p>
 * <p> Revision 2.14  2003/03/18 23:56:54  rickg
 * <p> ComScript method name changes
 * <p> Apropriate loading of combine scripts
 * <p> Added pass through method to open matching models
 * <p> Done not longer executes combine
 * <p> Updated combine related methods to match new
 * <p> combination dialog
 * <p>
 * <p> Revision 2.13  2003/03/18 17:03:15  rickg
 * <p> Combine development in progress
 * <p>
 * <p> Revision 2.12  2003/03/18 15:01:31  rickg
 * <p> Combine development in progress
 * <p>
 * <p> Revision 2.11  2003/03/18 00:32:32  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.10  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.9  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.8  2003/03/06 01:19:17  rickg
 * <p> Combine changes in progress
 * <p>
 * <p> Revision 2.7  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.6  2003/02/24 23:27:21  rickg
 * <p> Added process interrupt method
 * <p>
 * <p> Revision 2.5  2003/01/30 00:43:32  rickg
 * <p> Blank second axis panel when done with tomogram generation
 * <p>
 * <p> Revision 2.4  2003/01/29 15:22:58  rickg
 * <p> Updated logic for combine step
 * <p>
 * <p> Revision 2.3  2003/01/28 20:42:53  rickg
 * <p> Bug fix: save current dialog state when running align.com
 * <p>
 * <p> Revision 2.2  2003/01/28 00:15:29  rickg
 * <p> Main window now remembers its size
 * <p>
 * <p> Revision 2.1  2003/01/27 18:12:41  rickg
 * <p> Fixed bug from single window transition in positioning dialog
 * <p> opening function
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.36.2.1  2003/01/24 18:27:46  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.36  2003/01/10 20:46:34  rickg
 * <p> Added ability to view 3D fiducial models
 * <p>
 * <p> Revision 1.35  2003/01/10 18:39:58  rickg
 * <p> Using existing com scripts now gives the correct
 * <p> process state
 * <p>
 * <p> Revision 1.34  2003/01/10 18:33:16  rickg
 * <p> Added test parameter filename to command line args
 * <p>
 * <p> Revision 1.33  2003/01/08 21:04:38  rickg
 * <p> More descriptive error dialog when the are not
 * <p> available for combining.
 * <p>
 * <p> Revision 1.32  2003/01/07 00:30:16  rickg
 * <p> Added imodViewResidual method
 * <p>
 * <p> Revision 1.31  2003/01/06 04:53:16  rickg
 * <p> Set default parameters for transferfid panel and handle
 * <p> new backwards flag for b to a
 * <p>
 * <p> Revision 1.30  2003/01/04 00:41:00  rickg
 * <p> Implemented transferfid method
 * <p>
 * <p> Revision 1.29  2002/12/19 00:35:20  rickg
 * <p> Implemented persitent advanced state handling
 * <p>
 * <p> Revision 1.27  2002/12/11 21:26:31  rickg
 * <p> Added font setting into user prefs setting
 * <p>
 * <p> Revision 1.26  2002/12/11 05:39:00  rickg
 * <p> Added basic font change method
 * <p>
 * <p> Revision 1.25  2002/12/11 00:39:48  rickg
 * <p> Basic handling of settings dialog
 * <p> added setUserPreferences method
 * <p>
 * <p> Revision 1.24  2002/12/09 04:18:50  rickg
 * <p> Better handling of current working directory, user.dir and
 * <p> metaData always agree now.
 * <p>
 * <p> Revision 1.23  2002/12/05 01:21:02  rickg
 * <p> Added isAdvanced stub
 * <p>
 * <p> Revision 1.22  2002/11/21 19:24:38  rickg
 * <p> Set user.dir to current working directory
 * <p>
 * <p> Revision 1.21  2002/10/29 18:22:04  rickg
 * <p> Simplified rawstack open checking
 * <p>
 * <p> Revision 1.20  2002/10/25 19:30:43  rickg
 * <p> Modifies several catches to explicilty specify exception
 * <p>
 * <p> Revision 1.19  2002/10/24 19:52:55  rickg
 * <p> Added command line --demo argument
 * <p>
 * <p> Revision 1.18  2002/10/22 21:38:24  rickg
 * <p> ApplicationManager now controls both demo and debug
 * <p> modes
 * <p>
 * <p> Revision 1.17  2002/10/17 22:47:35  rickg
 * <p> process dialogs are now managed attributes
 * <p> setVisible calls changed to show
 * <p> unused variable dialogFinshed removed
 * <p>
 * <p> Revision 1.16  2002/10/17 16:23:04  rickg
 * <p> Added private method to update the dependent tilt parameters when the
 * <p> align.com parameters are changed
 * <p>
 * <p> Revision 1.15  2002/10/16 17:37:18  rickg
 * <p> Construct a imodManager when a new data set is opened
 * <p>
 * <p> Revision 1.14  2002/10/14 22:44:27  rickg
 * <p> Added combine to execute section of doneCombine
 * <p>
 * <p> Revision 1.13  2002/10/14 19:04:18  rickg
 * <p> openMessageDialog made public
 * <p>
 * <p> Revision 1.12  2002/10/10 23:40:33  rickg
 * <p> refactored createCombineScripts to setupCombineScripts
 * <p>
 * <p> Revision 1.11  2002/10/10 19:16:19  rickg
 * <p> Get HOME and IMOD_DIR environement variables during
 * <p> initialization instead of each time they requested.  Also
 * <p> exit if they are not available.
 * <p>
 * <p> Revision 1.10  2002/10/09 04:29:17  rickg
 * <p> Implemented calls to updateCombineCom
 * <p>
 * <p> Revision 1.9  2002/10/09 00:04:37  rickg
 * <p> Added default patch boundary logic
 * <p> still needs work on getting combine parameters at the correct times
 * <p>
 * <p> Revision 1.8  2002/10/07 22:20:21  rickg
 * <p> removed unused imports
 * <p>
 * <p> Revision 1.7  2002/09/30 23:44:43  rickg
 * <p> Started implementing updateCombineCom
 * <p>
 * <p> Revision 1.6  2002/09/30 22:01:29  rickg
 * <p> Added check to verify dual axis for combination
 * <p>
 * <p> Revision 1.5  2002/09/20 18:56:09  rickg
 * <p> Added private message and yes/no dialog methods
 * <p> Check to see if the raw stack and coarsely aligned stacks should be
 * <p> closed by the user
 * <p>
 * <p> Revision 1.4  2002/09/19 22:57:56  rickg
 * <p> Imod mangement is now handled through the ImodManager
 * <p>
 * <p> Revision 1.3  2002/09/17 23:44:56  rickg
 * <p> Adding ImodManager, in progress
 * <p>
 * <p> Revision 1.2  2002/09/13 21:29:57  rickg
 * <p> Started updating for ImodManager
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

