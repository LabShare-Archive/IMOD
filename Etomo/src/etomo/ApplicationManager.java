package etomo;

import java.awt.Dimension;
import java.awt.Toolkit;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

import etomo.comscript.ArchiveorigParam;
import etomo.comscript.BadComScriptException;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.BlendmontParam;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.CombineComscriptState;
import etomo.comscript.CombineParams;
import etomo.comscript.Command;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ConstSetParam;
import etomo.comscript.ConstSqueezevolParam;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.ConstTiltxcorrParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.GotoParam;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.MatchshiftsParam;
import etomo.comscript.NewstParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.ProcesschunksParam;
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
import etomo.comscript.TomopitchParam;
import etomo.comscript.TransferfidParam;
import etomo.comscript.TrimvolParam;
import etomo.comscript.XfproductParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.ProcessManager;
import etomo.process.ProcessState;
import etomo.process.SystemProcessException;
import etomo.storage.EtomoFileFilter;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.storage.XrayStackArchiveFilter;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseState;
import etomo.type.ConstMetaData;
import etomo.type.DialogExitState;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.FiducialMatch;
import etomo.type.InvalidEtomoNumberException;
import etomo.type.MetaData;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessTrack;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.TiltAngleSpec;
import etomo.type.TomogramState;
import etomo.type.ViewType;
import etomo.ui.AlignmentEstimationDialog;
import etomo.ui.CleanUpDialog;
import etomo.ui.CoarseAlignDialog;
import etomo.ui.FiducialModelDialog;
import etomo.ui.FiducialessParams;
import etomo.ui.MainPanel;
import etomo.ui.MainTomogramPanel;
import etomo.ui.PostProcessingDialog;
import etomo.ui.PreProcessingDialog;
import etomo.ui.ProcessDialog;
import etomo.ui.SetupDialog;
import etomo.ui.TextPageWindow;
import etomo.ui.TomogramCombinationDialog;
import etomo.ui.TomogramGenerationDialog;
import etomo.ui.TomogramPositioningDialog;
import etomo.util.FidXyz;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

/**
 * <p>Description: Handles high level message 
 *  processing, management of other high-level objects and signal routing for
 *  tomogram reconstructions.</p>
 *
 * <p>Copyright: Copyright (c) 2002-2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 */
public class ApplicationManager extends BaseManager {
  public static final String rcsid = "$Id$";

  //  Process dialog references
  private SetupDialog setupDialog = null;

  private PreProcessingDialog preProcDialogA = null;

  private PreProcessingDialog preProcDialogB = null;

  private CoarseAlignDialog coarseAlignDialogA = null;

  private CoarseAlignDialog coarseAlignDialogB = null;

  private FiducialModelDialog fiducialModelDialogA = null;

  private FiducialModelDialog fiducialModelDialogB = null;

  private AlignmentEstimationDialog fineAlignmentDialogA = null;

  private AlignmentEstimationDialog fineAlignmentDialogB = null;

  private TomogramPositioningDialog tomogramPositioningDialogA = null;

  private TomogramPositioningDialog tomogramPositioningDialogB = null;

  private TomogramGenerationDialog tomogramGenerationDialogA = null;

  private TomogramGenerationDialog tomogramGenerationDialogB = null;

  protected TomogramCombinationDialog tomogramCombinationDialog = null;

  private PostProcessingDialog postProcessingDialog = null;

  private CleanUpDialog cleanUpDialog = null;

  private MetaData metaData = null;
  private MainTomogramPanel mainPanel;
  private ProcessTrack processTrack;
  private ProcessManager processMgr;
  private TomogramState state;
  private boolean[] advancedA = new boolean[DialogType.TOTAL];
  private boolean[] advancedB = new boolean[DialogType.TOTAL];
  private DialogType currentDialogTypeA = null;
  private DialogType currentDialogTypeB = null;

  /**
   * Does initialization and loads the .edf file.  Opens the setup dialog
   * if there is no .edf file.
   */
  public ApplicationManager(String paramFileName, AxisID axisID) {
    super();
    this.metaData = new MetaData(this);
    initializeUIParameters(paramFileName, axisID);
    initializeAdvanced();
    // Open the etomo data file if one was found on the command line
    if (!test) {
      if (!paramFileName.equals("")) {
        if (loadedTestParamFile) {
          openProcessingPanel();
          mainPanel.setStatusBarText(paramFile, metaData);
        }
        else {
          openSetupDialog();
        }
      }
      else {
        openSetupDialog();
      }
    }
  }

  /**
   * Initialize advancedA and advancedB, which remember the advanced state of
   * dialogs when the user switches from one to another.  This is necessary
   * because the done function is run each time a user switches dialogs.
   *
   */
  private void initializeAdvanced() {
    boolean isAdvanced = EtomoDirector.getInstance().getAdvanced();
    for (int i = 0; i < DialogType.TOTAL; i++) {
      advancedA[i] = isAdvanced;
      advancedB[i] = isAdvanced;
    }
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
   * Sets the advanced state of a dialog.  Assumes the A axis (or single axis).
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
    return setupDialog != null;
  }

  /**
   * Check if setup dialog has been modified by the user.
   * Return true if there is text in the dataset field.
   * @return
   */
  public boolean isSetupChanged() {
    if (setupDialog == null) {
      return false;
    }
    if (setupDialog.getDatasetString() == null
        || setupDialog.getDatasetString().matches("\\s*")) {
      return false;
    }
    return true;
  }

  /**
   * Open the setup dialog
   */
  public void openSetupDialog() {
    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    setCurrentDialogType(DialogType.SETUP, AxisID.ONLY);
    if (setupDialog == null) {
      setupDialog = new SetupDialog(this);
      setupDialog.initializeFields((ConstMetaData) metaData);
    }
    mainPanel.openSetupPanel(setupDialog);
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    Dimension frameSize = mainPanel.getSize();
    mainPanel.setLocation((screenSize.width - frameSize.width) / 2,
        (screenSize.height - frameSize.height) / 2);
  }

  /**
   * Checks for an existing reconstruction on a different stack in the current
   * directory.  Assumes that the new .edf file for this instance has not been
   * created yet.
   * Since .com file names are not stack specific, it is necessary
   * to prevent interference by doing only one reconstruction per directory.  A
   * secondary goal is to have only one tilt series per directory.  Multiple
   * .edf files accessing the same stacks are allowed so that the user can back
   * up their .edf file or start a fresh .edf file.  The user may also have one
   * single and one dual reconstruction in a directory, as long as they have a
   * stack in common.
   * @return True if there is already an .edf file in the propertyUserDir and
   * it is referencing a stack other then the one(s) specified in the setup
   * dialog.  True if the new .edf file and the existing .edf file are both
   * single axis, even if one file is accessing the A stack and the other is
   * accessing the B stack.  False if no existing .edf file is found.  False if
   * the new .edf file and the existing .edf file are single and dual axis, as
   * long as they have a stack in common.  False if there is a conflict, but the
   * stacks that one of .edf files references don't exist.
   */
  private boolean checkForSharedDirectory() {
    if (setupDialog == null) {
      throw new IllegalStateException(
          "Function can only be run during setup phase,\nbefore the .edf file has been created.");
    }
    File directory = new File(propertyUserDir);
    //Get all the edf files in propertyUserDir.
    File[] edfFiles = directory.listFiles(new EtomoFileFilter());
    if (edfFiles == null) {
      return false;
    }
    String datasetName = metaData.getDatasetName();
    AxisType axisType = metaData.getAxisType();
    String extension = metaData.getFileExtension();
    File firstStack = null;
    File secondStack = null;
    String firstStackName = null;
    String secondStackName = null;
    //Create File instances based on the stacks specified in the setup dialog
    if (axisType == AxisType.DUAL_AXIS) {
      firstStack = new File(propertyUserDir, datasetName
          + AxisID.FIRST.getExtension() + ".st");
      firstStackName = firstStack.getName();
      secondStack = new File(propertyUserDir, datasetName
          + AxisID.SECOND.getExtension() + ".st");
      secondStackName = secondStack.getName();
    }
    else if (axisType == AxisType.SINGLE_AXIS) {
      firstStack = new File(propertyUserDir, datasetName
          + AxisID.ONLY.getExtension() + ".st");
      firstStackName = firstStack.getName();
    }
    //open any .edf files in propertyUserDir - assuming the .edf file for this
    //instance hasn't been created yet.
    //If there is at least one .edf file that references existing stacks that
    //are not the stacks the will be used in this instance, then the directory
    //is already in use.
    //Doing a dual and single axis on the same stack is not sharing a directory.
    //However doing two single axis reconstructions on the same tilt series,
    //where one is done on A and the other is done on B, would be considered
    //sharing a directory.
    for (int i = 0; i < edfFiles.length; i++) {
      MetaData savedMetaData = new MetaData(this);
      ParameterStore paramStore = new ParameterStore(edfFiles[i]);
      Storable[] storable = new Storable[1];
      storable[0] = savedMetaData;
      try {
        paramStore.load(storable);
      }
      catch (IOException e) {
        e.printStackTrace();
        continue;
      }
      //Create File instances based on the stacks specified in the edf file
      //found in propertyUserDir.
      AxisType savedAxisType = savedMetaData.getAxisType();
      String savedDatasetName = savedMetaData.getDatasetName();
      File savedFirstStack;
      File savedSecondStack;
      String savedFirstStackName;
      String savedSecondStackName;
      if (savedAxisType == AxisType.DUAL_AXIS) {
        savedFirstStack = new File(propertyUserDir, savedDatasetName
            + AxisID.FIRST.getExtension() + ".st");
        savedFirstStackName = savedFirstStack.getName();
        savedSecondStack = new File(propertyUserDir, savedDatasetName
            + AxisID.SECOND.getExtension() + ".st");
        savedSecondStackName = savedSecondStack.getName();
        if (axisType == AxisType.DUAL_AXIS) {
          //compare dual axis A against saved dual axis A
          if (savedFirstStack.exists()
              && !firstStackName.equals(savedFirstStackName)) {
            return true;
          }
          //compare dual axis B against saved dual axis B
          if (savedSecondStack.exists()
              && !secondStackName.equals(savedSecondStackName)) {
            return true;
          }
        }
        else if (axisType == AxisType.SINGLE_AXIS) {
          //compare single axis against saved dual axis A
          //compare single axis against saved dual axis B
          if (savedFirstStack.exists()
              && !firstStackName.equals(savedFirstStackName)
              && (!savedSecondStack.exists() || (savedSecondStack.exists() && !firstStackName
                  .equals(savedSecondStackName)))) {
            return true;
          }
        }
      }
      else if (savedAxisType == AxisType.SINGLE_AXIS) {
        savedFirstStack = new File(propertyUserDir, savedDatasetName
            + AxisID.ONLY.getExtension() + ".st");
        savedFirstStackName = savedFirstStack.getName();
        if (axisType == AxisType.DUAL_AXIS) {
          //compare dual axis A against saved single axis
          //compare dual axis B against saved single axis
          if (savedFirstStack.exists()
              && !firstStackName.equals(savedFirstStackName)
              && !secondStackName.equals(savedFirstStackName)) {
            return true;
          }
        }
        else if (axisType == AxisType.SINGLE_AXIS) {
          //compare single axis against saved single axis
          if (savedFirstStack.exists()
              && !firstStackName.equals(savedFirstStackName)) {
            return true;
          }
        }
      }
    }
    return false;
  }

  /**
   * Close message from the setup dialog window
   */
  public void doneSetupDialog() {
    if (setupDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update metadata parameters without an active setup dialog",
          "Program logic error", AxisID.ONLY);
      return;
    }
    //  Get the selected exit button
    DialogExitState exitState = setupDialog.getExitState();
    if (exitState != DialogExitState.CANCEL) {
      if (!setupDialog.isValid()) {
        return;
      }
      // Set the current working directory for the application saving the
      // old user.dir property until the meta data is valid
      String oldUserDir = propertyUserDir;
      propertyUserDir = setupDialog.getWorkingDirectory().getAbsolutePath();
      metaData = setupDialog.getFields();
      metaData.initialize();
      if (metaData == null) {
        return;
      }
      if (metaData.isValid()) {
        if (checkForSharedDirectory()) {
          uiHarness.openMessageDialog("This directory (" + propertyUserDir
              + ") is already being used by an .edf file.  Either open the "
              + "existing .edf file or create a new directory for the new "
              + "reconstruction.", "WARNING:  CANNOT PROCEED", AxisID.ONLY);
          return;
        }
        processTrack.setSetupState(ProcessState.INPROGRESS);
        //final initialization of IMOD manager
        imodManager.setMetaData(metaData);
        //set paramFile so meta data can be saved
        paramFile = new File(propertyUserDir, metaData.getMetaDataFileName());
        mainPanel.setStatusBarText(paramFile, metaData);
        userConfig.putDataFile(paramFile.getAbsolutePath());
        loadedTestParamFile = true;
        state.initialize();
      }
      else {
        String[] errorMessage = new String[2];
        errorMessage[0] = "Setup Parameter Error";
        errorMessage[1] = metaData.getInvalidReason();
        uiHarness.openMessageDialog(errorMessage, "Setup Parameter Error",
            AxisID.ONLY);
        propertyUserDir = oldUserDir;
        return;
      }
      // This is really the method to use the existing com scripts
      if (exitState == DialogExitState.EXECUTE) {
        try {
          processMgr.setupComScripts(metaData, AxisID.ONLY);
        }
        catch (BadComScriptException except) {
          except.printStackTrace();
          uiHarness.openMessageDialog(except.getMessage(),
              "Can't run copytomocoms", AxisID.ONLY);
          return;
        }
        catch (IOException except) {
          uiHarness.openMessageDialog("Can't run copytomocoms\n"
              + except.getMessage(), "Copytomocoms IOException", AxisID.ONLY);
          return;
        }
      }
      processTrack.setSetupState(ProcessState.COMPLETE);
      metaData.setComScriptCreated(true);
      EtomoDirector.getInstance().renameCurrentManager(
          metaData.getDatasetName());
    }
    //  Switch the main window to the procesing panel
    openProcessingPanel();
    //  Free the dialog
    setupDialog = null;
    saveTestParamFile(AxisID.ONLY);
  }

  /**
   * Open the main window in processing mode
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(metaData.getAxisType());
    mainPanel.updateAllProcessingStates(processTrack);
    setPanel();
  }

  /**
   * Open the pre-processing dialog
   */
  public void openPreProcDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(axisID);
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
    PreProcessingDialog preProcDialog = new PreProcessingDialog(this, axisID);
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
    mainPanel.showProcess(preProcDialog.getContainer(), axisID);
  }

  /**
   * Closes preprocessing dialog.  Updates comscripts and edf file and deletes
   * dialog.
   */
  public void donePreProcDialog(AxisID axisID) {
    PreProcessingDialog preProcDialog;
    if (axisID == AxisID.SECOND) {
      preProcDialog = preProcDialogB;
    }
    else {
      preProcDialog = preProcDialogA;
    }
    if (preProcDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update preprocessing parameters without an active "
              + "preprocessing dialog", "Program logic error", axisID);
      return;
    }
    setAdvanced(preProcDialog.getDialogType(), axisID, preProcDialog
        .isAdvanced());

    //  Keep dialog box open until we get good info or it is cancelled
    DialogExitState exitState = preProcDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      updateEraserCom(axisID, false);
      // If there are raw stack imod processes open ask the user if they
      // should be closed.
      try {
        if (exitState != DialogExitState.SAVE
            && imodManager.isOpen(ImodManager.RAW_STACK_KEY, axisID)) {
          String[] message = new String[2];
          message[0] = "The raw stack is open in 3dmod";
          message[1] = "Should it be closed?";
          if (uiHarness.openYesNoDialog(message, axisID)) {
            imodManager.quit(ImodManager.RAW_STACK_KEY, axisID);
          }
        }
      }
      catch (AxisTypeException except) {
        except.printStackTrace();
        uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
            axisID);
      }
      catch (SystemProcessException except) {
        except.printStackTrace();
        uiHarness.openMessageDialog(except.getMessage(),
            "Problem closing raw stack", axisID);
      }
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setPreProcessingState(ProcessState.COMPLETE, axisID);
        mainPanel.setPreProcessingState(ProcessState.COMPLETE, axisID);
        //  Go to the coarse align dialog by default
        openCoarseAlignDialog(axisID);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
        mainPanel.setPreProcessingState(ProcessState.INPROGRESS, axisID);
        //  Go to the coarse align dialog by default
        mainPanel.showBlankProcess(axisID);
      }
      saveTestParamFile(axisID);
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      preProcDialogB = null;
    }
    else {
      preProcDialogA = null;
    }
    preProcDialog = null;
  }

  /**
   * Open 3dmod to create the manual erase model
   */
  public void imodManualErase(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    String eraseModelName = metaData.getDatasetName() + axisID.getExtension()
        + ".erase";
    try {
      if (metaData.getViewType() == ViewType.MONTAGE) {
        imodManager.setFrames(ImodManager.RAW_STACK_KEY, axisID, true);
      }
      imodManager.open(ImodManager.RAW_STACK_KEY, axisID, eraseModelName, true,
          menuOptions);
      processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
      mainPanel.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on raw stack", axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Axis type problem in 3dmod erase", axisID);
    }
  }

  /**
   * Get the eraser script parameters from the CCD eraser panel and
   * write them out the eraser{|a|b}.com script
   * @param axisID    The axisID to process
   * @param trialMode Set to trial mode if true
   */
  private void updateEraserCom(AxisID axisID, boolean trialMode) {
    PreProcessingDialog preProcDialog;
    if (axisID == AxisID.SECOND) {
      preProcDialog = preProcDialogB;
    }
    else {
      preProcDialog = preProcDialogA;
    }
    //  Get the user input data from the dialog box. The CCDEraserParam
    //  is first initialized from the currently loaded com script to
    //  provide deafault values for those not handled by the dialog box
    //  get function needs some error checking
    CCDEraserParam ccdEraserParam = comScriptMgr.getCCDEraserParam(axisID);
    preProcDialog.getCCDEraserParams(ccdEraserParam);
    ccdEraserParam.setTrialMode(trialMode);
    comScriptMgr.saveEraser(ccdEraserParam, axisID);
  }

  /**
   * Run the eraser script for the specified axis
   * @param axisID
   */
  public void eraser(AxisID axisID) {
    updateEraserCom(axisID, false);
    processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    mainPanel.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.eraser(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute eraser" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Run CCDeraser in trial mode
   * @param axisID
   */
  public void findXrays(AxisID axisID) {
    updateEraserCom(axisID, true);
    processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    mainPanel.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.eraser(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute eraser" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
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
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening coarse stack", axisID);
    }
  }

  /**
   * Open 3dmod to view the erased stack
   */
  public void imodErasedStack(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    if (Utilities.getFile(this, true, axisID, "_fixed.st", "erased stack") == null) {
      return;
    }
    try {
      if (metaData.getViewType() == ViewType.MONTAGE) {
        imodManager.setPieceListFileName(ImodManager.ERASED_STACK_KEY, axisID,
            metaData.getDatasetName() + axisID.getExtension() + ".pl");
      }
      imodManager.open(ImodManager.ERASED_STACK_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening erased stack", axisID);
    }
  }

  public void archiveOriginalStack() {
    archiveOriginalStack(null);
  }

  /**
   * Archive the orginal stacks during clean up.
   * @param axisID
   */
  private void archiveOriginalStack(AxisID currentAxisID) {
    //figure out which original stack to archive
    AxisID stackAxisID = currentAxisID;
    if (stackAxisID == null) {
      if (metaData.getAxisType() == AxisType.DUAL_AXIS) {
        stackAxisID = AxisID.FIRST;
      }
      else {
        stackAxisID = AxisID.ONLY;
      }
    }
    //set next process to archiveorig so that the second axis can be done
    if (stackAxisID == AxisID.FIRST) {
      setNextProcess(AxisID.ONLY, ArchiveorigParam.COMMAND_NAME);
    }
    else {
      resetNextProcess(AxisID.ONLY);
    }
    //check for original stack
    File originalStack = Utilities.getFile(this, false, stackAxisID,
        "_orig.st", "original stack");
    if (!originalStack.exists()) {
      if (stackAxisID == AxisID.FIRST) {
        //Nothing to do on the first axis, so move on to the second axis
        startNextProcess(AxisID.ONLY);
        return;
      }
      else {
        return;
      }
    }
    //set progress bar and process state
    mainPanel.startProgressBar("Archiving " + stackAxisID + " stack",
        AxisID.ONLY);
    processTrack.setCleanUpState(ProcessState.INPROGRESS);
    mainPanel.setCleanUpState(ProcessState.INPROGRESS);
    //create param
    ArchiveorigParam param = new ArchiveorigParam(this, stackAxisID);
    //run process
    try {
      setThreadName(processMgr.archiveOrig(param), AxisID.ONLY);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ArchiveorigParam.COMMAND_NAME
          + " command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY);
    }
  }

  public void deleteOriginalStack(Command archiveorigParam, String[] output) {
    AxisID axisID;
    int mode = archiveorigParam.getCommandMode();
    if (mode == ArchiveorigParam.AXIS_A_MODE) {
      axisID = AxisID.FIRST;
    }
    else if (mode == ArchiveorigParam.AXIS_B_MODE) {
      axisID = AxisID.SECOND;
    }
    else if (mode == ArchiveorigParam.AXIS_ONLY_MODE) {
      axisID = AxisID.ONLY;
    }
    else {
      return;
    }
    File originalStack = Utilities.getFile(this, false, axisID, "_orig.st", "");
    //This function is only run is archiveorig succeeds so this "if" statement
    //should always fail.
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
    if (uiHarness.openDeleteDialog(message, AxisID.ONLY)) {
      System.err.println("Deleting " + originalStack.getAbsolutePath());
      originalStack.delete();
      if (cleanUpDialog != null) {
        cleanUpDialog.setArchiveFields();
      }
    }
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

  /**
   * Replace the raw stack with the fixed stack created from eraser
   * @param axisID
   */
  public void replaceRawStack(AxisID axisID) {
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
      return;
    }
    processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    mainPanel.setPreProcessingState(ProcessState.INPROGRESS, axisID);

    // Rename the fixed stack to the raw stack file name and save the orginal
    // raw stack to _orig.st if that does not already exist
    try {
      if (!rawRename.exists()) {
        Utilities.renameFile(rawStack, rawRename);
      }
      if (renameXrayStack(axisID)) {
        Utilities.renameFile(fixedStack, rawStack);
      }
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(), "File Rename Error",
          axisID);
    }
    try {
      if (imodManager.isOpen(ImodManager.RAW_STACK_KEY, axisID)) {
        String[] message = new String[2];
        message[0] = "The replaced raw stack is open in 3dmod";
        message[1] = "Should it be closed?";
        if (uiHarness.openYesNoDialog(message, axisID)) {
          imodManager.quit(ImodManager.RAW_STACK_KEY, axisID);
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
      System.err.println("Axis type exception in replaceRawStack");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      System.err.println("System process exception in replaceRawStack");
    }
    //An _orig.st file may have been created, so refresh the Clean Up dialog's
    //archive fields.
    if (cleanUpDialog != null) {
      cleanUpDialog.setArchiveFields();
    }
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
      EtomoNumber newFileNumber = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
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
          + xrayStackArchive.getName() + "\" from the command line.", axisID);
      return false;
    }
    return true;
  }

  /**
   * Bring up a preview 3dmod.
   * @param axisID
   */
  public void imodPreview(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    if (setupDialog == null) {
      return;
    }
    String key = ImodManager.PREVIEW_KEY;
    MetaData previewMetaData = setupDialog.getDataset();
    imodManager.setPreviewMetaData(previewMetaData);
    File previewWorkingDir = previewMetaData
        .getValidDatasetDirectory(setupDialog.getWorkingDirectory()
            .getAbsolutePath());
    if (previewWorkingDir == null) {
      uiHarness.openMessageDialog(previewMetaData.getInvalidReason(),
          "Raw Image Stack", axisID);
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
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening raw stack", axisID);
    }
  }

  /**
   * Open the coarse alignment dialog
   */
  public void openCoarseAlignDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(axisID);
      return;
    }
    setCurrentDialogType(DialogType.COARSE_ALIGNMENT, axisID);
    mainPanel.selectButton(axisID, "Coarse Alignment");
    if (showIfExists(coarseAlignDialogA, coarseAlignDialogB, axisID)) {
      return;
    }
    CoarseAlignDialog coarseAlignDialog = new CoarseAlignDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      coarseAlignDialogB = coarseAlignDialog;
    }
    else {
      coarseAlignDialogA = coarseAlignDialog;
    }
    //  Create the dialog box
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
    coarseAlignDialog.setTiltAxisAngle(metaData.getImageRotation(axisID));
    mainPanel.showProcess(coarseAlignDialog.getContainer(), axisID);
  }

  /**
   * Get the parameters from the coarse align process dialog box
   */
  public void doneCoarseAlignDialog(AxisID axisID) {
    //  Set a reference to the correct object
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    if (coarseAlignDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update coarse align without an active coarse align dialog",
          "Program logic error", axisID);
      return;
    }
    setAdvanced(coarseAlignDialog.getDialogType(), axisID, coarseAlignDialog
        .isAdvanced());
    DialogExitState exitState = coarseAlignDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (updateXcorrCom(axisID) == null) {
        return;
      }
      updateBlendmontInXcorrCom(axisID);
      if (metaData.getViewType() != ViewType.MONTAGE
          && !updatePrenewstCom(axisID)) {
        return;
      }
      if (!updateFiducialessParams(coarseAlignDialog, axisID)) {
        return;
      }
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setCoarseAlignmentState(ProcessState.COMPLETE, axisID);
        mainPanel.setCoarseAlignState(ProcessState.COMPLETE, axisID);
        //  Go to the fiducial model dialog by default
        if (metaData.isFiducialessAlignment(axisID)) {
          openTomogramPositioningDialog(axisID);
          // Check to see if the user wants to keep any coarse aligned imods
          // open
          try {
            if (imodManager.isOpen(ImodManager.COARSE_ALIGNED_KEY, axisID)) {
              String[] message = new String[2];
              message[0] = "The coarsely aligned stack is open in 3dmod";
              message[1] = "Should it be closed?";
              if (uiHarness.openYesNoDialog(message, axisID)) {
                imodManager.quit(ImodManager.COARSE_ALIGNED_KEY, axisID);
              }
            }
          }
          catch (AxisTypeException except) {
            except.printStackTrace();
            uiHarness.openMessageDialog(except.getMessage(),
                "AxisType problem", axisID);
          }
          catch (SystemProcessException except) {
            except.printStackTrace();
            uiHarness.openMessageDialog(except.getMessage(),
                "Problem closing coarse stack", axisID);
          }
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
      saveTestParamFile(axisID);
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      coarseAlignDialogB = null;
    }
    else {
      coarseAlignDialogA = null;
    }
    coarseAlignDialog = null;
  }

  /**
   * Get the parameters from dialog box and run the cross correlation script
   */
  public void crossCorrelate(AxisID axisID) {
    // Get the parameters from the dialog box
    ConstTiltxcorrParam tiltxcorrParam = updateXcorrCom(axisID);
    if (tiltxcorrParam != null) {
      processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
      mainPanel.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        BlendmontParam blendmontParam = updateBlendmontInXcorrCom(axisID);
        if (blendmontParam == null) {
          threadName = processMgr.crossCorrelate(tiltxcorrParam, axisID);
        }
        else {
          threadName = processMgr.crossCorrelate(blendmontParam, axisID);
        }
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute xcorr" + axisID.getExtension() + ".com";
        message[1] = e.getMessage();
        uiHarness.openMessageDialog(message, "Unable to execute com script",
            axisID);
        return;
      }
      setThreadName(threadName, axisID);
    }
  }

  /**
   * run undistort.com
   * @param axisID
   */
  public void makeDistortionCorrectedStack(AxisID axisID) {
    updateUndistortCom(axisID);
    processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
    mainPanel.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.makeDistortionCorrectedStack(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute undistort" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Run the coarse alignment script
   */
  public void coarseAlign(AxisID axisID) {
    ProcessName processName;
    BlendmontParam blendmontParam = null;
    if (metaData.getViewType() == ViewType.MONTAGE) {
      blendmontParam = updatePreblendCom(axisID);
      processName = BlendmontParam.getProcessName(BlendmontParam.PREBLEND_MODE);
    }
    else {
      if (!updatePrenewstCom(axisID)) {
        return;
      }
      processName = ProcessName.PRENEWST;
    }
    processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
    mainPanel.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
    setNextProcess(axisID, "checkUpdateFiducialModel");
    String threadName;
    try {
      if (metaData.getViewType() == ViewType.MONTAGE) {
        threadName = processMgr.preblend(blendmontParam, axisID);
      }
      else {
        threadName = processMgr.coarseAlign(axisID);
      }
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + processName + axisID.getExtension()
          + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Open 3dmod to view the coarsely aligned stack
   */
  public void imodCoarseAlign(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.COARSE_ALIGNED_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening coarse stack", axisID);
    }
  }

  /**
   * Run midas on the raw stack
   */
  public void midasRawStack(AxisID axisID) {
    if (!updateFiducialessParams(mapCoarseAlignDialog(axisID), axisID)) {
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
  }

  /**
   * Run fix  edges in Midas
   */
  public void midasFixEdges(AxisID axisID) {
    processMgr.midasFixEdges(axisID);
    processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
    mainPanel.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
  }

  /**
   * Get the required parameters from the dialog box and update the xcorr.com
   * script
   * @return true if successful in getting the parameters and saving the com
   *         script
   */
  private ConstTiltxcorrParam updateXcorrCom(AxisID axisID) {
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    TiltxcorrParam tiltXcorrParam = null;
    try {
      tiltXcorrParam = comScriptMgr.getTiltxcorrParam(axisID);
      coarseAlignDialog.getCrossCorrelationParams(tiltXcorrParam);
      comScriptMgr.saveXcorr(tiltXcorrParam, axisID);
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      uiHarness.openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error",
          axisID);
      return null;
    }
    catch (NumberFormatException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Align Parameter Syntax Error";
      errorMessage[1] = axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error",
          axisID);
      return null;
    }
    return tiltXcorrParam;
  }

  /**
   * Update the blendmont command in the xcorr comscript.  if blendmont does not
   * have to be run, update the goto param to skip blendmont
   * return blendmont param if blendmont has to be run.
   * @param axisID
   * @return
   */
  private BlendmontParam updateBlendmontInXcorrCom(AxisID axisID) {
    boolean runningBlendmont = false;
    BlendmontParam blendmontParam = null;
    //handle montaging
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
    blendmontParam.setMode(BlendmontParam.UNDISTORT_MODE);
    blendmontParam.setBlendmontState();
    comScriptMgr.saveXcorrToUndistort(blendmontParam, axisID);
  }

  /**
   * Get the prenewst parameters from the dialog box and update the prenewst
   * com script as well as the align com script since binning of the pre-aligned
   * stack has an affect on the scaling of the fiducial model.
   * @param axisID
   * @return
   */
  private boolean updatePrenewstCom(AxisID axisID) {
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    NewstParam prenewstParam = comScriptMgr.getPrenewstParam(axisID);
    coarseAlignDialog.getPrenewstParams(prenewstParam);
    comScriptMgr.savePrenewst(prenewstParam, axisID);
    return true;
  }

  /**
   * Get the set the blendmont parameters and update the preblend com script.
   * @param axisID
   * @return
   */
  private BlendmontParam updatePreblendCom(AxisID axisID) {
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    BlendmontParam preblendParam = comScriptMgr.getPreblendParam(axisID);
    coarseAlignDialog.getParams(preblendParam);
    preblendParam.setBlendmontState();
    comScriptMgr.savePreblend(preblendParam, axisID);
    return preblendParam;
  }

  /**
   * Get the set the blendmont parameters and update the blend com script.
   * @param axisID
   * @return
   */
  private BlendmontParam updateBlendCom(AxisID axisID) {
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    BlendmontParam blendParam = comScriptMgr.getBlendParam(axisID);
    tomogramGenerationDialog.getBlendParams(blendParam);
    blendParam.setMode(BlendmontParam.BLEND_MODE);
    blendParam.setBlendmontState();
    comScriptMgr.saveBlend(blendParam, axisID);
    return blendParam;
  }

  /**
   * Get the fiducialess parameters from the specified dialog and set the
   * metaData and rotationXF script
   * 
   * @param axisID
   * @param dialog
   * @return
   */
  private boolean updateFiducialessParams(FiducialessParams dialog,
      AxisID axisID) {
    float tiltAxisAngle;
    try {
      tiltAxisAngle = dialog.getTiltAxisAngle();
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Tilt axis rotation format error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Tilt axis rotation syntax error", axisID);
      return false;
    }
    metaData.setFiducialessAlignment(axisID, dialog.isFiducialessAlignment());
    metaData.setImageRotation(tiltAxisAngle, axisID);
    updateRotationXF(tiltAxisAngle, axisID);
    return true;
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
   * Write out the rotation transform for the specified axis
   * 
   * @param axisID
   */
  private void updateRotationXF(float angle, AxisID axisID) {
    //  Open the appropriate rotation file
    String fnRotationXF = propertyUserDir + File.separator + "rotation"
        + axisID.getExtension() + ".xf";
    File rotationXF = new File(fnRotationXF);
    try {
      BufferedWriter out = new BufferedWriter(new FileWriter(rotationXF));
      //  Write out the transform to perform the rotation
      double rads = -1 * angle * Math.PI / 180;
      out.write(String.valueOf(Math.cos(rads)) + "   "
          + String.valueOf(Math.sin(-rads)) + "   "
          + String.valueOf(Math.sin(rads)) + "   "
          + String.valueOf(Math.cos(rads)) + "   0   0");
      out.newLine();
      //  Close the file
      out.close();
    }
    catch (IOException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Rotation Transform IO Exception";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = fnRotationXF;
      uiHarness.openMessageDialog(errorMessage,
          "Rotation Transform IO Exception", axisID);
    }
  }

  /**
   * Open the fiducial model generation dialog
   */
  public void openFiducialModelDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(axisID);
      return;
    }
    setCurrentDialogType(DialogType.FIDUCIAL_MODEL, axisID);
    mainPanel.selectButton(axisID, "Fiducial Model Gen.");
    if (showIfExists(fiducialModelDialogA, fiducialModelDialogB, axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    FiducialModelDialog fiducialModelDialog = new FiducialModelDialog(this,
        axisID);
    if (axisID == AxisID.SECOND) {
      fiducialModelDialogB = fiducialModelDialog;
    }
    else {
      fiducialModelDialogA = fiducialModelDialog;
    }
    updateDialog(fiducialModelDialog, axisID);
    //  Load the required track{|a|b}.com files, fill in the dialog box
    // params
    //  and set it to the appropriate state
    comScriptMgr.loadTrack(axisID);
    //  Create a default transferfid object to populate the alignment dialog
    fiducialModelDialog.setTransferFidParams();
    fiducialModelDialog.setBeadtrackParams(comScriptMgr
        .getBeadtrackParam(axisID));
    mainPanel.showProcess(fiducialModelDialog.getContainer(), axisID);
  }

  /**
   * Save comscripts and the .edf file and delete the dialog.
   */
  public void doneFiducialModelDialog(AxisID axisID) {
    //  Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (axisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }
    if (fiducialModelDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update fiducial model without an active fiducial model dialog",
              "Program logic error", axisID);
      return;
    }
    setAdvanced(fiducialModelDialog.getDialogType(), axisID,
        fiducialModelDialog.isAdvanced());
    DialogExitState exitState = fiducialModelDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      fiducialModelDialog.getTransferFidParams();
      //  Get the user input data from the dialog box
      if (!updateTrackCom(axisID)) {
        return;
      }
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
      saveTestParamFile(axisID);
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      fiducialModelDialogB = null;
    }
    else {
      fiducialModelDialogA = null;
    }
    fiducialModelDialog = null;
  }

  /**
   * Open 3dmod with the seed model
   */
  public void imodSeedFiducials(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    String seedModel = metaData.getDatasetName() + axisID.getExtension()
        + ".seed";
    try {
      imodManager.setPreserveContrast(ImodManager.COARSE_ALIGNED_KEY, axisID,
          true);
      imodManager.open(ImodManager.COARSE_ALIGNED_KEY, axisID, seedModel, true,
          menuOptions);
      processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
      mainPanel.setFiducialModelState(ProcessState.INPROGRESS, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on coarse aligned stack with model: " + seedModel,
          axisID);
    }
  }

  /**
   * Get the beadtrack parameters from the fiducial model dialog and run the
   * track com script
   */
  public void fiducialModelTrack(AxisID axisID) {
    if (updateTrackCom(axisID)) {
      processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
      mainPanel.setFiducialModelState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.fiducialModelTrack(axisID);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute track" + axisID.getExtension() + ".com";
        message[1] = e.getMessage();
        uiHarness.openMessageDialog(message, "Unable to execute com script",
            axisID);
        return;
      }
      setThreadName(threadName, axisID);
      mainPanel.startProgressBar("Tracking fiducials", axisID);
    }
  }

  /**
   * Using Fiducial Model as Seed
   * @param axisID
   */
  public void makeFiducialModelSeedModel(AxisID axisID) {
    String seedModelFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + ".seed";
    File seedModel = new File(seedModelFilename);
    String fiducialModelFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + ".fid";
    File fiducialModel = new File(fiducialModelFilename);
    if (seedModel.exists()
        && seedModel.lastModified() > fiducialModel.lastModified()) {
      String[] message = new String[3];
      message[0] = "WARNING: The seed model file is more recent the fiducial model file";
      message[1] = "To avoid losing your changes to the seed model file,";
      message[2] = "track fiducials before pressing Use Fiducial Model as Seed.";
      uiHarness.openMessageDialog(message, "Use Fiducial Model as Seed Failed",
          axisID);
      return;
    }
    mainPanel.setProgressBar("Using Fiducial Model as Seed", 1, axisID);
    processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
    mainPanel.setFiducialModelState(ProcessState.INPROGRESS, axisID);
    String origSeedModelFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + "_orig.seed";
    File origSeedModel = new File(origSeedModelFilename);
    //backup original seed model file
    if (seedModel.exists() && origSeedModel.exists()) {
      backupFile(seedModel, axisID);
    }
    try {
      if (seedModel.exists() && !origSeedModel.exists()) {
        Utilities.renameFile(seedModel, origSeedModel);
      }
      //rename fiducial model file to seed model file
      Utilities.renameFile(fiducialModel, seedModel);
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(), "File Rename Error",
          axisID);
    }
    try {
      if (imodManager.isOpen(ImodManager.COARSE_ALIGNED_KEY, axisID)) {
        if (seedModel.getName().equals(
            imodManager.getModelName(ImodManager.COARSE_ALIGNED_KEY, axisID))) {
          String[] message = new String[2];
          message[0] = "The old seed model file is open in 3dmod";
          message[1] = "Should it be closed?";
          if (uiHarness.openYesNoDialog(message, axisID)) {
            imodManager.quit(ImodManager.COARSE_ALIGNED_KEY, axisID);
          }
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
      System.err.println("Axis type exception in replaceRawStack");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      System.err.println("System process exception in replaceRawStack");
    }
    mainPanel.stopProgressBar(axisID);
    if (axisID == AxisID.SECOND) {
      updateDialog(fiducialModelDialogA, AxisID.FIRST);
    }
    else {
      updateDialog(fiducialModelDialogB, AxisID.SECOND);
    }
  }

  /**
   * Open 3dmod with the new fidcuial model
   */
  public void imodFixFiducials(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    String fiducialModel = metaData.getDatasetName() + axisID.getExtension()
        + ".fid";
    try {
      imodManager
          .setOpenBeadFixer(ImodManager.COARSE_ALIGNED_KEY, axisID, true);
      imodManager.open(ImodManager.COARSE_ALIGNED_KEY, axisID, fiducialModel,
          true, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on coarse aligned stack with model: "
              + fiducialModel, axisID);
    }
  }

  /**
   * Update the specified track com script
   */
  private boolean updateTrackCom(AxisID axisID) {
    //  Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (axisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }
    if (fiducialModelDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update track?.com without an active fiducial model dialog",
          "Program logic error", axisID);
      return false;
    }
    try {
      BeadtrackParam beadtrackParam = comScriptMgr.getBeadtrackParam(axisID);
      fiducialModelDialog.getBeadtrackParams(beadtrackParam);
      comScriptMgr.saveTrack(beadtrackParam, axisID);
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      uiHarness.openMessageDialog(errorMessage,
          "Beadtrack Parameter Syntax Error", axisID);
      return false;
    }
    catch (NumberFormatException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Beadtrack Parameter Syntax Error", axisID);
      return false;
    }
    catch (InvalidEtomoNumberException e) {
      return false;
    }
    return true;
  }

  /**
   * Open the alignment estimation dialog
   */
  public void openFineAlignmentDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(axisID);
      return;
    }
    setCurrentDialogType(DialogType.FINE_ALIGNMENT, axisID);
    mainPanel.selectButton(axisID, "Fine Alignment");
    if (showIfExists(fineAlignmentDialogA, fineAlignmentDialogB, axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    AlignmentEstimationDialog fineAlignmentDialog = new AlignmentEstimationDialog(
        this, axisID);
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
    //If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      //upgrade and save param to comscript
      upgradeOldAlignCom(axisID, tiltalignParam);
    }
    fineAlignmentDialog.setTiltalignParams(tiltalignParam);

    //  Create a default transferfid object to populate the alignment dialog
    mainPanel.showProcess(fineAlignmentDialog.getContainer(), axisID);
  }

  /**
   * Calls done function for the dialog type, if the dialog exit state is SAVE.
   * This is the default and means that no exit button was pressed.
   * @param dialogType
   * @param axisID
   */
  public void saveCurrentDialog(AxisID axisID) {
    DialogType currentDialogType = getCurrentDialogType(axisID);
    ProcessDialog dialog = getDialog(currentDialogType, axisID);
    if (dialog == null || dialog.getExitState() != DialogExitState.SAVE) {
      return;
    }
    if (currentDialogType == DialogType.PRE_PROCESSING) {
      donePreProcDialog(axisID);
    }
    else if (currentDialogType == DialogType.COARSE_ALIGNMENT) {
      doneCoarseAlignDialog(axisID);
    }
    else if (currentDialogType == DialogType.FIDUCIAL_MODEL) {
      doneFiducialModelDialog(axisID);
    }
    else if (currentDialogType == DialogType.FINE_ALIGNMENT) {
      doneAlignmentEstimationDialog(axisID);
    }
    else if (currentDialogType == DialogType.TOMOGRAM_POSITIONING) {
      doneTomogramPositioningDialog(axisID);
    }
    else if (currentDialogType == DialogType.TOMOGRAM_GENERATION) {
      doneTomogramGenerationDialog(axisID);
    }
    else if (currentDialogType == DialogType.TOMOGRAM_COMBINATION) {
      doneTomogramCombinationDialog();
    }
    else if (currentDialogType == DialogType.POST_PROCESSING) {
      donePostProcessing();
    }
    else if (currentDialogType == DialogType.CLEAN_UP) {
      doneCleanUp();
    }
  }

  /**
   * Set the current dialog type.  This function is called from open functions and
   * from showBlankPRocess().  It allows Etomo to call the done function when
   * the user switches to another dialog.
   * @param dialogType
   * @param axisID
   */
  public void setCurrentDialogType(DialogType dialogType, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      currentDialogTypeB = dialogType;
    }
    else {
      currentDialogTypeA = dialogType;
    }
  }

  /**
   * Gets the current dialog type.
   * @param axisID
   * @return
   */
  private DialogType getCurrentDialogType(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return currentDialogTypeB;
    }
    return currentDialogTypeA;
  }

  /**
   * Call BaseManager.exitProgram().  Call saveDialog.   Return the value of
   * BaseManager.exitProgram().  To guarantee that etomo can always exit, catch
   * all unrecognized Exceptions and Errors and return true.  This function
   * should be called exitManager, since individual managers are closed
   * inpendently of the program.
   */
  public boolean exitProgram(AxisID axisID) {
    try {
      if (super.exitProgram(axisID)) {
        saveDialog();
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
   * Saves all dialogs that are not set to null.  Called by exitProgram().
   *
   */
  public void saveDialog() {
    AxisID firstAxisID = metaData.getAxisType() == AxisType.DUAL_AXIS ? AxisID.FIRST
        : AxisID.ONLY;
    if (preProcDialogA != null) {
      donePreProcDialog(firstAxisID);
    }
    if (preProcDialogB != null) {
      donePreProcDialog(AxisID.SECOND);
    }
    if (coarseAlignDialogA != null) {
      doneCoarseAlignDialog(firstAxisID);
    }
    if (coarseAlignDialogB != null) {
      doneCoarseAlignDialog(AxisID.SECOND);
    }
    if (fiducialModelDialogA != null) {
      doneFiducialModelDialog(firstAxisID);
    }
    if (fiducialModelDialogB != null) {
      doneFiducialModelDialog(AxisID.SECOND);
    }
    if (fineAlignmentDialogA != null) {
      doneAlignmentEstimationDialog(firstAxisID);
    }
    if (fineAlignmentDialogB != null) {
      doneAlignmentEstimationDialog(AxisID.SECOND);
    }
    if (tomogramPositioningDialogA != null) {
      doneTomogramPositioningDialog(firstAxisID);
    }
    if (tomogramPositioningDialogB != null) {
      doneTomogramPositioningDialog(AxisID.SECOND);
    }
    if (tomogramGenerationDialogA != null) {
      doneTomogramGenerationDialog(firstAxisID);
    }
    if (tomogramGenerationDialogB != null) {
      doneTomogramGenerationDialog(AxisID.SECOND);
    }
    if (tomogramCombinationDialog != null) {
      doneTomogramCombinationDialog();
    }
    if (postProcessingDialog != null) {
      donePostProcessing();
    }
    if (cleanUpDialog != null) {
      doneCleanUp();
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
    else if (dialogType == DialogType.TOMOGRAM_POSITIONING) {
      return mapPositioningDialog(axisID);
    }
    else if (dialogType == DialogType.TOMOGRAM_GENERATION) {
      return mapGenerationDialog(axisID);
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
    //  Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog;
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialog = fineAlignmentDialogB;
    }
    else {
      fineAlignmentDialog = fineAlignmentDialogA;
    }
    if (fineAlignmentDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update align?.com without an active alignment dialog",
          "Program logic error", axisID);
      return;
    }
    setAdvanced(fineAlignmentDialog.getDialogType(), axisID,
        fineAlignmentDialog.isAdvanced());
    DialogExitState exitState = fineAlignmentDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (updateAlignCom(axisID) == null) {
        return;
      }
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
        mainPanel.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
        mainPanel.showBlankProcess(axisID);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setFineAlignmentState(ProcessState.COMPLETE, axisID);
        mainPanel.setFineAlignmentState(ProcessState.COMPLETE, axisID);
        openTomogramPositioningDialog(axisID);

        // Check to see if the user wants to keep any coarse aligned imods
        // open
        try {
          if (imodManager.isOpen(ImodManager.COARSE_ALIGNED_KEY, axisID)) {
            String[] message = new String[2];
            message[0] = "The coarsely aligned stack is open in 3dmod";
            message[1] = "Should it be closed?";
            if (uiHarness.openYesNoDialog(message, axisID)) {
              imodManager.quit(ImodManager.COARSE_ALIGNED_KEY, axisID);
            }
          }
        }
        catch (AxisTypeException except) {
          except.printStackTrace();
          uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
              axisID);
        }
        catch (SystemProcessException except) {
          except.printStackTrace();
          uiHarness.openMessageDialog(except.getMessage(),
              "Problem closing coarse stack", axisID);
        }
      }
      saveTestParamFile(axisID);
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialogB = null;
    }
    else {
      fineAlignmentDialogA = null;
    }
    fineAlignmentDialog = null;
  }

  /**
   * Execute the fine alignment script (align.com) for the appropriate axis
   * This will also reset the fiducialess alignment flag, setFiducialAlign
   * does not need to be called because the necessary copies are called at the
   * end of the align script.
   * 
   * @param the
   *            AxisID identifying the axis to align.
   */
  public void fineAlignment(AxisID axisID) {
    ConstTiltalignParam tiltalignParam = updateAlignCom(axisID);
    if (tiltalignParam == null) {
      return;
    }
    processTrack.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
    mainPanel.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.fineAlignment(tiltalignParam, axisID);
      metaData.setFiducialessAlignment(axisID, false);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute align" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Aligning stack", axisID);
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
      imodManager.open(ImodManager.COARSE_ALIGNED_KEY, axisID, fiducialModel,
          menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on coarse aligned stack with model: "
              + fiducialModel, axisID);
    }
  }

  /**
   * Open 3dmodv with the new fidcuial model
   */
  public void imodView3DModel(AxisID axisID) {
    String fiducialModel = metaData.getDatasetName() + axisID.getExtension()
        + ".3dmod";
    try {
      imodManager.open(ImodManager.FIDUCIAL_MODEL_KEY, axisID, fiducialModel);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on fine aligned stack", axisID);
    }
  }

  /**
   * Open 3dmod to view the coarsely aligned stack
   * 
   * @param axisID
   *            the AxisID to coarse align.
   */
  public void imodFineAlign(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.FINE_ALIGNED_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on fine aligned stack", axisID);
    }
  }

  /**
   * Open 3dmod to view the MTF filter results
   * 
   * @param axisID
   *            the AxisID to coarse align.
   */
  public void imodMTFFilter(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.MTF_FILTER_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on MTF filter results", axisID);
    }
  }

  /**
   * Transfer the fiducial to the specified axis
   * 
   * @param destAxisID
   */
  public void transferfid(AxisID destAxisID) {
    //  Set a reference to the correct object
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
          "Warning", destAxisID);
    }
    if (fiducialModelDialog != null) {
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
      String threadName;
      try {
        threadName = processMgr.transferFiducials(transferfidParam);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute transferfid command";
        message[1] = e.getMessage();
        uiHarness.openMessageDialog(message, "Unable to execute command",
            destAxisID);
        return;
      }
      setThreadName(threadName, destAxisID);
      mainPanel.startProgressBar("Transferring fiducials", destAxisID);
      updateDialog(fiducialModelDialog, destAxisID);
    }
  }

  /**
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters
   * from the alignment estimation dialog. This also updates the local
   * alignment state of the appropriate tilt files.
   */
  private ConstTiltalignParam updateAlignCom(AxisID axisID) {
    //  Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog;
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialog = fineAlignmentDialogB;
    }
    else {
      fineAlignmentDialog = fineAlignmentDialogA;
    }
    if (fineAlignmentDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update align?.com without an active alignment dialog",
          "Program logic error", axisID);
      return null;
    }
    TiltalignParam tiltalignParam;
    try {
      tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
      fineAlignmentDialog.getTiltalignParams(tiltalignParam);
      comScriptMgr.saveAlign(tiltalignParam, axisID);
      //  Update the tilt.com script with the dependent parameters
      updateTiltCom(tiltalignParam, axisID);
      //update xfproduct in align.com
      XfproductParam xfproductParam = comScriptMgr.getXfproductInAlign(axisID);
      xfproductParam.setScaleShifts(getStackBinning(axisID, ".preali"));
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
          "Tiltalign Parameter Syntax Error", axisID);
      return null;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Tiltalign Parameter Syntax Error", axisID);
      return null;
    }
    return tiltalignParam;
  }

  /**
   * Update the tilt parameters dependent on the align script - local
   * alignments file - the exclude list
   */
  private void updateTiltCom(ConstTiltalignParam tiltalignParam,
      AxisID currentAxis) {
    comScriptMgr.loadTilt(currentAxis);
    TiltParam tiltParam = comScriptMgr.getTiltParam(currentAxis);
    String alignFileExtension = currentAxis.getExtension() + "local.xf";
    if (tiltalignParam.getLocalAlignments().is()) {
      tiltParam.setLocalAlignFile(metaData.getDatasetName()
          + alignFileExtension);
    }
    else {
      tiltParam.setLocalAlignFile("");
    }
    tiltParam.setExcludeList(tiltalignParam.getExcludeList());
    comScriptMgr.saveTilt(tiltParam, currentAxis);
  }

  /**
   * Open the tomogram positioning dialog for the specified axis
   * @param axisID
   */
  public void openTomogramPositioningDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(axisID);
      return;
    }
    setCurrentDialogType(DialogType.TOMOGRAM_POSITIONING, axisID);
    mainPanel.selectButton(axisID, "Tomogram Positioning");
    if (showIfExists(tomogramPositioningDialogA, tomogramPositioningDialogB,
        axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    TomogramPositioningDialog tomogramPositioningDialog = new TomogramPositioningDialog(
        this, axisID);
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialogB = tomogramPositioningDialog;
    }
    else {
      tomogramPositioningDialogA = tomogramPositioningDialog;
    }
    // Read in the meta data parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions
    tomogramPositioningDialog.setParameters(metaData);
    if (metaData.getViewType() != ViewType.MONTAGE) {
      comScriptMgr.loadNewst(axisID);
      tomogramPositioningDialog.setParameters(comScriptMgr
          .getNewstComNewstParam(axisID));
    }
    else {
      comScriptMgr.loadBlend(axisID);
      tomogramPositioningDialog.setParameters(comScriptMgr
          .getBlendParam(axisID));
    }

    // Get the align{|a|b}.com parameters
    comScriptMgr.loadAlign(axisID);
    TiltalignParam tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
    if (metaData.getViewType() != ViewType.MONTAGE) {
      //upgrade and save param to comscript
      upgradeOldAlignCom(axisID, tiltalignParam);
    }
    tomogramPositioningDialog.setAlignParams(tiltalignParam);

    // Get the tilt{|a|b}.com parameters
    comScriptMgr.loadTilt(axisID);
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    //If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      //upgrade and save param to comscript
      upgradeOldTiltCom(axisID, tiltParam);
    }
    tomogramPositioningDialog.setTiltParams(tiltParam);

    // Get the tomopitch{|a|b}.com parameters
    comScriptMgr.loadTomopitch(axisID);
    tomogramPositioningDialog.setTomopitchParams(comScriptMgr
        .getTomopitchParam(axisID));

    // Set the whole tomogram sampling state, fidcialess state, and tilt axis
    // angle
    tomogramPositioningDialog.setWholeTomogramSampling(metaData
        .isWholeTomogramSample(axisID));

    tomogramPositioningDialog.setFiducialessAlignment(metaData
        .isFiducialessAlignment(axisID));
    tomogramPositioningDialog.setTiltAxisAngle(metaData
        .getImageRotation(axisID));

    // Open the dialog panel
    mainPanel.showProcess(tomogramPositioningDialog.getContainer(), axisID);
  }

  /**
   *  
   */
  public void doneTomogramPositioningDialog(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    if (tomogramPositioningDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update sample.com without an active positioning dialog",
          "Program logic error", axisID);
      return;
    }
    setAdvanced(tomogramPositioningDialog.getDialogType(), axisID,
        tomogramPositioningDialog.isAdvanced());
    DialogExitState exitState = tomogramPositioningDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      //  Get all of the parameters from the panel
      if (updateAlignCom(tomogramPositioningDialog, axisID) == null) {
        return;
      }
      if (!updateSampleTiltCom(axisID)) {
        return;
      }
      if (!updateTomopitchCom(axisID)) {
        return;
      }
      if (!updateFiducialessParams(tomogramPositioningDialog, axisID)) {
        return;
      }
      if (metaData.getViewType() != ViewType.MONTAGE
          && updateNewstCom(tomogramPositioningDialog, axisID) == null) {
        return;
      }

      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setTomogramPositioningState(ProcessState.INPROGRESS,
            axisID);
        mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
        mainPanel.showBlankProcess(axisID);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setTomogramPositioningState(ProcessState.COMPLETE, axisID);
        mainPanel.setTomogramPositioningState(ProcessState.COMPLETE, axisID);
        openTomogramGenerationDialog(axisID);
        try {
          if (imodManager.isOpen(ImodManager.SAMPLE_KEY, axisID)) {
            String[] message = new String[2];
            message[0] = "The sample reconstruction is open in 3dmod";
            message[1] = "Should it be closed?";
            if (uiHarness.openYesNoDialog(message, axisID)) {
              imodManager.quit(ImodManager.SAMPLE_KEY, axisID);
            }
          }
        }
        catch (AxisTypeException except) {
          except.printStackTrace();
          uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
              axisID);
        }
        catch (SystemProcessException except) {
          except.printStackTrace();
          uiHarness.openMessageDialog(except.getMessage(),
              "Problem closing sample reconstruction", axisID);
        }
      }
      saveTestParamFile(axisID);
    }

    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialogB = null;
    }
    else {
      tomogramPositioningDialogA = null;
    }
    tomogramPositioningDialog = null;
  }

  /**
   * Run the sample com script
   */
  public void createSample(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    // Make sure that we have an active positioning dialog
    if (tomogramPositioningDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update sample.com without an active positioning dialog",
          "Program logic error", axisID);
      return;
    }
    //  Get the user input data from the dialog box
    if (!updateFiducialessParams(tomogramPositioningDialog, axisID)) {
      return;
    }
    if (!updateSampleTiltCom(axisID)) {
      return;
    }
    processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);

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
            "Unable to copy fiducial alignment files", axisID);
      }
    }

    String threadName;
    try {
      threadName = processMgr.createSample(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute sample" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar("Creating sample tomogram", axisID);
  }

  /**
   * Create a whole tomogram for positioning the tomogram in the volume
   * 
   * @param axisID
   */
  public void wholeTomogram(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    // Get the user input from the dialog
    if (!updateFiducialessParams(tomogramPositioningDialog, axisID)) {
      return;
    }
    ConstNewstParam newstParam = null;
    BlendmontParam blendmontParam = null;
    if (metaData.getViewType() != ViewType.MONTAGE) {
      newstParam = updateNewstCom(tomogramPositioningDialog, axisID);
      if (newstParam == null) {
        return;
      }
    }
    else {
      blendmontParam = updateBlendCom(tomogramPositioningDialog, axisID);
      if (blendmontParam == null) {
        return;
      }
    }
    if (!updateSampleTiltCom(axisID)) {
      return;
    }
    processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);

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
            "Unable to copy fiducial alignment files", axisID);
      }
    }

    setNextProcess(axisID, "tilt");
    String threadName;
    try {
      if (metaData.getViewType() != ViewType.MONTAGE) {
        threadName = processMgr.newst(newstParam, axisID);
      }
      else {
        threadName = processMgr.blend(blendmontParam, axisID);
      }
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Open 3dmod on the sample volume for the specified axis along with the
   * tomopitch model
   * 
   * @param axisID
   */
  public void imodSample(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      //It is safe to use open contours in all cases.
      imodManager.setOpenContours(ImodManager.SAMPLE_KEY, axisID, true);
      imodManager.open(ImodManager.SAMPLE_KEY, axisID, menuOptions);
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening sample reconstruction", axisID);
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
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID, tomopitchModelName,
          true, menuOptions);
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Problem opening sample reconstruction", axisID);
    }
  }

  /**
   *  
   */
  public void tomopitch(AxisID axisID) {
    if (updateTomopitchCom(axisID)) {
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.tomopitch(axisID);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute tomopitch" + axisID.getExtension()
            + ".com";
        message[1] = e.getMessage();
        uiHarness.openMessageDialog(message, "Unable to execute com script",
            axisID);
        return;
      }
      setThreadName(threadName, axisID);
      mainPanel.startProgressBar("Finding sample position", axisID);
    }
  }

  /**
   * Open the tomopitch log file
   * 
   * @param axisID
   */
  public void openTomopitchLog(AxisID axisID) {
    String logFileName = "tomopitch" + axisID.getExtension() + ".log";
    TextPageWindow logFileWindow = new TextPageWindow();
    logFileWindow.setVisible(logFileWindow.setFile(propertyUserDir
        + File.separator + logFileName));
  }

  /**
   * Compute the final alignment from the updated parameters in the
   * positioning dialog.
   * 
   * @param axisID
   */
  public void finalAlign(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    ConstTiltalignParam tiltalignParam = updateAlignCom(
        tomogramPositioningDialog, axisID);
    if (tiltalignParam != null) {
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.fineAlignment(tiltalignParam, axisID);
        metaData.setFiducialessAlignment(axisID, false);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute align" + axisID.getExtension() + ".com";
        message[1] = e.getMessage();
        uiHarness.openMessageDialog(message, "Unable to execute com script",
            axisID);
        return;
      }
      setThreadName(threadName, axisID);
      mainPanel.startProgressBar("Calculating final alignment", axisID);
    }
  }

  /**
   * Update the tilt{|a|b}.com file with sample parameters for the specified
   * axis
   */
  private boolean updateSampleTiltCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    // Make sure that we have an active positioning dialog
    if (tomogramPositioningDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update sample.com without an active positioning dialog",
          "Program logic error", axisID);
      return false;
    }
    // Get the current tilt parameters, make any user changes and save the
    // parameters back to the tilt{|a|b}.com
    try {
      TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
      tomogramPositioningDialog.getTiltParams(tiltParam);
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
      comScriptMgr.saveTilt(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Tilt Parameter Syntax Error",
          axisID);
      return false;
    }
    return true;
  }

  /**
   * Update the tomopitch{|a|b}.com file with sample parameters for the
   * specified axis
   */
  private boolean updateTomopitchCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    // Make sure that we have an active positioning dialog
    if (tomogramPositioningDialog == null) {
      uiHarness.openMessageDialog(
          "Can not update tomopitch.com without an active positioning dialog",
          "Program logic error", axisID);
      return false;
    }
    // Get the current tilt parameters, make any user changes and save the
    // parameters back to the tilt{|a|b}.com
    try {
      TomopitchParam tomopitchParam = comScriptMgr.getTomopitchParam(axisID);
      tomogramPositioningDialog.getTomopitchParams(tomopitchParam);
      comScriptMgr.saveTomopitch(tomopitchParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tomopitch Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Tomopitch Parameter Syntax Error", axisID);
      return false;
    }
    return true;
  }

  /**
   * Return the TomogramPositioningDialog associated the specified AxisID
   * 
   * @param axisID
   * @return
   */
  private TomogramPositioningDialog mapPositioningDialog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramPositioningDialogB;
    }
    return tomogramPositioningDialogA;
  }

  /**
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters
   * from the tomogram positioning dialog. The positioning dialog is passed so
   * that the signature is different from the standard method.
   */
  private ConstTiltalignParam updateAlignCom(
      TomogramPositioningDialog tomogramPositioningDialog, AxisID axisID) {
    TiltalignParam tiltalignParam;
    try {
      tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
      tomogramPositioningDialog.getAlignParams(tiltalignParam);
      comScriptMgr.saveAlign(tiltalignParam, axisID);
      //update xfproduct in align.com
      XfproductParam xfproductParam = comScriptMgr.getXfproductInAlign(axisID);
      xfproductParam.setScaleShifts(getStackBinning(axisID, ".preali"));
      comScriptMgr.saveXfproductInAlign(xfproductParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Tiltalign Parameter Syntax Error", axisID);
      return null;
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xfproduct Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Xfproduct Parameter Syntax Error", axisID);
      return null;
    }
    return tiltalignParam;
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
  private ConstNewstParam updateNewstCom(
      TomogramPositioningDialog tomogramPositioningDialog, AxisID axisID) {

    //  Get the whole tomogram positions state
    metaData.setWholeTomogramSample(axisID, tomogramPositioningDialog
        .isWholeTomogramSampling());

    NewstParam newstParam = comScriptMgr.getNewstComNewstParam(axisID);
    tomogramPositioningDialog.getNewstParamst(newstParam);
    try {
      // Make sure the size output is removed, it was only there as a 
      // copytomocoms template
      newstParam.setCommandMode(NewstParam.WHOLE_TOMOGRAM_SAMPLE_MODE);
      newstParam.setSizeToOutputInXandY("/");
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
  private BlendmontParam updateBlendCom(
      TomogramPositioningDialog tomogramPositioningDialog, AxisID axisID) {
    //  Get the whole tomogram positions state
    metaData.setWholeTomogramSample(axisID, tomogramPositioningDialog
        .isWholeTomogramSampling());
    BlendmontParam blendmontParam = comScriptMgr.getBlendParam(axisID);
    tomogramPositioningDialog.getParams(blendmontParam);
    blendmontParam.setMode(BlendmontParam.WHOLE_TOMOGRAM_SAMPLE_MODE);
    blendmontParam.setBlendmontState();
    comScriptMgr.saveBlend(blendmontParam, axisID);
    return blendmontParam;
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
      uiHarness.openMessageDialog(message, "Unable to generate prexg", axisID);
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
          axisID);
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
          "Unable to setup fiducialless align files", axisID);
    }
    catch (InvalidParameterException except) {
      String[] message = new String[2];
      message[0] = "Unable to setup fiducialless align files";
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message,
          "Unable to setup fiducialless align files", axisID);
    }
  }

  public void makeRawtltFile(AxisID axisID) throws IOException,
      InvalidParameterException {
    File rawtlt = new File(propertyUserDir, metaData.getDatasetName()
        + axisID.getExtension() + ".rawtlt");
    //backing up .rawtlt, which is currently unnecessary because this function
    //is only called when .rawtlt doesn't exist
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
      MRCHeader rawStackHeader = getMrcHeader(axisID, ".st");
      rawStackHeader.read();
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
          axisID);
      throw except;
    }
    catch (InvalidParameterException except) {
      except.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to create " + rawtlt.getAbsolutePath();
      message[1] = except.getMessage();
      uiHarness.openMessageDialog(message, "Unable to create raw tilt file",
          axisID);
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

  public void setEnabledTiltParameters(AxisID axisID) {
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      return;
    }
    boolean madeZFactors = false;
    boolean newstFiducialessAlignment = false;
    boolean usedLocalAlignments = false;
    //madeZFactors
    if (!state.getMadeZFactors(axisID).isNull()) {
      madeZFactors = state.getMadeZFactors(axisID).is();
    }
    else {
      madeZFactors = state.getBackwardCompatibleMadeZFactors(axisID);
    }
    //newstFiducialessAlignment
    if (!state.getNewstFiducialessAlignment(axisID).isNull()) {
      newstFiducialessAlignment = state.getNewstFiducialessAlignment(axisID)
          .is();
    }
    else {
      newstFiducialessAlignment = tomogramGenerationDialog
          .isFiducialessAlignment();
    }
    //usedLocalAlignments
    if (!state.getUsedLocalAlignments(axisID).isNull()) {
      usedLocalAlignments = state.getUsedLocalAlignments(axisID).is();
    }
    else {
      usedLocalAlignments = state
          .getBackwardCompatibleUsedLocalAlignments(axisID);
    }
    //enable parameters
    tomogramGenerationDialog.enableUseZFactors(madeZFactors
        && !newstFiducialessAlignment);
    tomogramGenerationDialog.enableUseLocalAlignment(usedLocalAlignments
        && !newstFiducialessAlignment);
  }

  /**
   * Open the tomogram generation dialog
   */
  public void openTomogramGenerationDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(axisID);
      return;
    }
    //
    setCurrentDialogType(DialogType.TOMOGRAM_GENERATION, axisID);
    mainPanel.selectButton(axisID, "Tomogram Generation");
    if (showIfExists(tomogramGenerationDialogA, tomogramGenerationDialogB,
        axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    TomogramGenerationDialog tomogramGenerationDialog = new TomogramGenerationDialog(
        this, axisID);
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialogB = tomogramGenerationDialog;
    }
    else {
      tomogramGenerationDialogA = tomogramGenerationDialog;
    }
    //no longer managing image size

    // Read in the newst{|a|b}.com parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions
    if (metaData.getViewType() == ViewType.MONTAGE) {
      comScriptMgr.loadBlend(axisID);
      tomogramGenerationDialog.setBlendParams(comScriptMgr
          .getBlendParam(axisID));
    }
    else {
      comScriptMgr.loadNewst(axisID);
      tomogramGenerationDialog.setNewstParams(comScriptMgr
          .getNewstComNewstParam(axisID));
    }
    tomogramGenerationDialog.setParameters(metaData);
    // Read in the tilt{|a|b}.com parameters and display the dialog panel
    comScriptMgr.loadTilt(axisID);
    comScriptMgr.loadMTFFilter(axisID);
    TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
    //If this is a montage, then binning can only be 1, so no need to upgrade
    if (metaData.getViewType() != ViewType.MONTAGE) {
      //upgrade and save param to comscript
      upgradeOldTiltCom(axisID, tiltParam);
    }
    tomogramGenerationDialog.setTiltParams(tiltParam);
    tomogramGenerationDialog.setMTFFilterParam(comScriptMgr
        .getMTFFilterParam(axisID));
    updateDialog(tomogramGenerationDialog, axisID);

    //  Set the fidcialess state and tilt axis angle
    tomogramGenerationDialog.setFiducialessAlignment(metaData
        .isFiducialessAlignment(axisID));
    tomogramGenerationDialog
        .setTiltAxisAngle(metaData.getImageRotation(axisID));
    setEnabledTiltParameters(axisID);

    mainPanel.showProcess(tomogramGenerationDialog.getContainer(), axisID);
  }

  /**
   *  
   */
  public void doneTomogramGenerationDialog(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update tilt?.com without an active tomogram generation dialog",
              "Program logic error", axisID);
      return;
    }
    setAdvanced(tomogramGenerationDialog.getDialogType(), axisID,
        tomogramGenerationDialog.isAdvanced());
    DialogExitState exitState = tomogramGenerationDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      tomogramGenerationDialog.getParameters(metaData);
      if (!updateFiducialessParams(tomogramGenerationDialog, axisID)) {
        return;
      }
      if (metaData.getViewType() == ViewType.MONTAGE) {
        updateBlendCom(axisID);
      }
      else {
        if (updateNewstCom(axisID) == null) {
          return;
        }
      }
      if (!updateTiltCom(axisID, true)) {
        return;
      }
      if (!updateMTFFilterCom(axisID)) {
        return;
      }

      if (exitState == DialogExitState.POSTPONE) {
        processTrack
            .setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
        mainPanel.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
        mainPanel.showBlankProcess(axisID);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setTomogramGenerationState(ProcessState.COMPLETE, axisID);
        mainPanel.setTomogramGenerationState(ProcessState.COMPLETE, axisID);
        if (isDualAxis()) {
          openTomogramCombinationDialog();
          if (axisID == AxisID.SECOND) {
            mainPanel.showBlankProcess(axisID);
          }
        }
        else {
          openPostProcessingDialog();
        }
      }
      saveTestParamFile(axisID);
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialogB = null;
    }
    else {
      tomogramGenerationDialogA = null;
    }
    tomogramGenerationDialog = null;
  }

  /**
   * Update the tilt.com from the TomogramGenerationDialog
   * 
   * @param axisID
   * @param useDefaultRec
   *            If true set the reconstruction output filename to what is
   *            expected of the com scripts. If false use the trial tomogram
   *            filename specified in the TomogramGenerationDialog
   * @return true if successful
   */
  private boolean updateTiltCom(AxisID axisID, boolean useDefaultRec) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update tilt?.com without an active tomogram generation dialog",
              "Program logic error", axisID);
      return false;
    }
    try {
      TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
      tomogramGenerationDialog.getTiltParams(tiltParam);
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
        String trialTomogramName = tomogramGenerationDialog
            .getTrialTomogramName();
        tiltParam.setOutputFile(trialTomogramName);
      }
      if (metaData.getViewType() == ViewType.MONTAGE) {
        //binning is currently always 1 and correct size should be coming from
        //copytomocoms
        //tiltParam.setMontageFullImage(propertyUserDir, tomogramGenerationDialog.getBinning());
      }
      comScriptMgr.saveTilt(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Tilt Parameter Syntax Error",
          axisID);
      return false;
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Tilt Parameter Syntax Error",
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
  private boolean updateMTFFilterCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update mtffilter?.com without an active tomogram generation dialog",
              "Program logic error", axisID);
      return false;
    }
    try {
      MTFFilterParam mtfFilterParam = comScriptMgr.getMTFFilterParam(axisID);
      tomogramGenerationDialog.getMTFFilterParam(mtfFilterParam);
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
      uiHarness.openMessageDialog(errorMessage,
          "MTF Filter Parameter Syntax Error", axisID);
      return false;
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "MTF Filter Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "MTF Filter Parameter Syntax Error", axisID);
      return false;
    }
    return true;
  }

  /**
   * Update the newst.com from the TomogramGenerationDialog
   * reads metaData
   * 
   * @param axisID
   * @return true if successful
   */
  private ConstNewstParam updateNewstCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      uiHarness
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
      tomogramGenerationDialog.getNewstParams(newstParam);
      comScriptMgr.saveNewst(newstParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Newst Parameter Syntax Error",
          axisID);
      return null;
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
    }
    return newstParam;
  }

  /**
   * Return the TomogramGenerationDialog associated with the specified AxisID
   * 
   * @param axisID
   * @return
   */
  private TomogramGenerationDialog mapGenerationDialog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramGenerationDialogB;
    }
    return tomogramGenerationDialogA;
  }

  /**
   *  
   */
  public void newst(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);

    // Get the user input from the dialog
    if (!updateFiducialessParams(tomogramGenerationDialog, axisID)) {
      return;
    }
    ConstNewstParam newstParam = null;
    BlendmontParam blendmontParam = null;
    if (metaData.getViewType() == ViewType.MONTAGE) {
      blendmontParam = updateBlendCom(axisID);
    }
    else {
      newstParam = updateNewstCom(axisID);
      if (newstParam == null) {
        return;
      }
    }

    processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
    mainPanel.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);

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
            "Unable to copy fiducial alignment files", axisID);
      }
    }

    String threadName;
    try {
      if (metaData.getViewType() == ViewType.MONTAGE) {
        threadName = processMgr.blend(blendmontParam, axisID);
      }
      else {
        threadName = processMgr.newst(newstParam, axisID);
      }
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Replace the full aligned stack with the filtered full aligned stack
   * created from mtffilter
   * 
   * @param axisID
   */
  public void useMtfFilter(AxisID axisID) {
    mainPanel.setProgressBar("Using filtered full aligned stack", 1, axisID);
    // Instantiate file objects for the original raw stack and the fixed
    // stack
    String fullAlignedStackFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + ".ali";
    File fullAlignedStack = new File(fullAlignedStackFilename);
    String filteredFullAlignedStackFilename = propertyUserDir + File.separator
        + metaData.getDatasetName() + axisID.getExtension() + "_filt.ali";
    File filteredFullAlignedStack = new File(filteredFullAlignedStackFilename);
    if (!filteredFullAlignedStack.exists()) {
      uiHarness
          .openMessageDialog(
              "The filtered full aligned stack doesn't exist.  Create the filtered full aligned stack first",
              "Filtered full aligned stack missing", axisID);
      return;
    }
    processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
    mainPanel.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
    //don't have to rename full aligned stack because it is a generated
    // file
    try {
      Utilities.renameFile(filteredFullAlignedStack, fullAlignedStack);
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(), "File Rename Error",
          axisID);
    }
    try {
      if (imodManager.isOpen(ImodManager.FINE_ALIGNED_KEY, axisID)) {
        String[] message = new String[2];
        message[0] = "The original full aligned stack is open in 3dmod";
        message[1] = "Should it be closed?";
        if (uiHarness.openYesNoDialog(message, axisID)) {
          imodManager.quit(ImodManager.FINE_ALIGNED_KEY, axisID);
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
      System.err.println("Axis type exception in useMtfFilter");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      System.err.println("System process exception in useMtfFilter");
    }
    mainPanel.stopProgressBar(axisID);
  }

  /**
   */
  public void mtffilter(AxisID axisID) {
    if (updateMTFFilterCom(axisID)) {
      processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.mtffilter(axisID);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute mtffilter" + axisID.getExtension()
            + ".com";
        message[1] = e.getMessage();
        uiHarness.openMessageDialog(message, "Unable to execute com script",
            axisID);
        return;
      }
      setThreadName(threadName, axisID);
    }
  }

  /**
   * Start a tilt process in trial mode
   * 
   * @param axisID
   */
  public void trialTilt(AxisID axisID) {
    if (updateTiltCom(axisID, false)) {
      processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      tiltProcess(axisID);
    }
  }

  /**
   * Run the tilt command script for the specified axis
   */
  public void tilt(AxisID axisID) {
    if (updateTiltCom(axisID, true)) {
      processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      mainPanel.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      tiltProcess(axisID);
    }
  }

  /**
   * Tilt process initiator. Since tilt can be started from multiple points in
   * the process chain we need separate the execution from the parameter
   * collection and state updating
   * 
   * @param axisID
   */
  private void tiltProcess(AxisID axisID) {
    resetNextProcess(axisID);
    String threadName;
    try {
      threadName = processMgr.tilt(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute tilt" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          axisID);
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Open 3dmod to view the tomogram
   * 
   * @param axisID
   *            the AxisID of the tomogram to open.
   */
  public void imodFullVolume(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod with the tomogram", axisID);
    }
  }

  /**
   * Open 3dmod on the current test volume
   * 
   * @param axisID
   */
  public void imodTestVolume(AxisID axisID, Run3dmodMenuOptions menuOptions) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog;
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialog = tomogramGenerationDialogB;
    }
    else {
      tomogramGenerationDialog = tomogramGenerationDialogA;
    }
    String trialTomogramName = tomogramGenerationDialog.getTrialTomogramName();
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
          "Can't open 3dmod with the tomogram", axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
  }

  public void commitTestVolume(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog;
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialog = tomogramGenerationDialogB;
    }
    else {
      tomogramGenerationDialog = tomogramGenerationDialogA;
    }
    String trialTomogramName = tomogramGenerationDialog.getTrialTomogramName();
    //  Check to see if the trial tomogram exist
    File trialTomogramFile = new File(propertyUserDir, trialTomogramName);
    if (!trialTomogramFile.exists()) {
      String message[] = new String[2];
      message[0] = "The specified tomogram does not exist:" + trialTomogramName;
      message[1] = "It must be calculated before commiting";
      uiHarness.openMessageDialog(message, "Can't rename tomogram", axisID);
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
    try {
      Utilities.renameFile(trialTomogramFile, outputFile);
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(), "File Rename Error",
          axisID);
    }
    mainPanel.stopProgressBar(axisID);
  }

  /**
   * Delete the pre-aligned and aligned stack for the specified axis
   * 
   * @param axisID
   */
  public void deleteAlignedStacks(AxisID axisID) {
    mainPanel.setProgressBar("Deleting aligned image stacks", 1, axisID);
    //
    // Don't do preali now because users may do upto generation before
    // transfering
    // the fiducials
    //
    //     File preali =
    //      new File(
    //        System.getProperty("user.dir"),
    //        metaData.getDatasetName() + axisID.getExtension() + ".preali");
    //    if (preali.exists()) {
    //      if (!preali.delete()) {
    //        mainFrame.openMessageDialog(
    //          "Unable to delete pre-aligned stack: " + preali.getAbsolutePath(),
    //          "Can not delete file");
    //      }
    //    }
    File aligned = new File(propertyUserDir, metaData.getDatasetName()
        + axisID.getExtension() + ".ali");
    if (aligned.exists()) {
      if (!aligned.delete()) {
        uiHarness.openMessageDialog("Unable to delete aligned stack: "
            + aligned.getAbsolutePath(), "Can not delete file", axisID);
      }
    }
    mainPanel.stopProgressBar(axisID);
  }

  /**
   * Open the tomogram combination dialog
   */
  public void openTomogramCombinationDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(AxisID.ONLY);
      return;
    }
    // Verify that this process is applicable
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      uiHarness.openMessageDialog(
          "This step is valid only for a dual axis tomogram",
          "Invalid tomogram combination selection", AxisID.ONLY);
      return;
    }
    setCurrentDialogType(DialogType.TOMOGRAM_COMBINATION, AxisID.FIRST);
    mainPanel.selectButton(AxisID.FIRST, "Tomogram Combination");
    if (tomogramCombinationDialog == null) {
      tomogramCombinationDialog = new TomogramCombinationDialog(this);
      // Get the setupcombine parameters and set the default patch
      // boundaries if
      // they have not already been set
      CombineParams combineParams = new CombineParams(metaData
          .getCombineParams());
      if (!combineParams.isPatchBoundarySet()) {
        String recFileName;
        if (combineParams.getMatchBtoA()) {
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
              + recFileName, AxisID.ONLY);
          // Delete the dialog
          tomogramCombinationDialog = null;
          return;
        }
        catch (IOException except) {
          uiHarness.openMessageDialog(except.getMessage(), "IO Error: "
              + recFileName, AxisID.ONLY);
          //Delete the dialog
          tomogramCombinationDialog = null;
          return;
        }
      }
      // Fill in the dialog box params and set it to the appropriate state
      tomogramCombinationDialog.setCombineParams(combineParams);
      
      // If setupcombine has been run load the com scripts, otherwise disable the
      // apropriate panels in the tomogram combination dialog
      tomogramCombinationDialog.enableCombineTabs(combineScriptsExist());
      if (combineScriptsExist()) {
        // Check to see if a solvematch.com file exists and load it if so
        //otherwise load the correct old solvematch* file
        File solvematch = new File(propertyUserDir, "solvematch.com");
        if (solvematch.exists()) {
          loadSolvematch();
        }
        else {
          //For backward compatibility using fiducialMatch instead of
          //modelBased.  ModelBased was not updated when fiducialMatch was
          //changed and ficucialMatch wasn't update when modelBased was changed.
          //But it looks like modelBased was never changed.  In version 3.2.6
          //CombineParam.setModelBased() was never called.  So fiducialMatch is
          //probably correct in earlier versions.
          boolean modelBased = false;
          ConstCombineParams metaDataCombineParams = metaData
              .getCombineParams();
          if (metaDataCombineParams.getFiducialMatch() == FiducialMatch.USE_MODEL
              || metaDataCombineParams.getFiducialMatch() == FiducialMatch.USE_MODEL_ONLY) {
            modelBased = true;
          }
          loadSolvematch(modelBased);
        }
        loadPatchcorr();
        loadMatchorwarp();
        loadVolcombine();
        loadCombineComscript();
        tomogramCombinationDialog.synchronize(
            TomogramCombinationDialog.lblSetup, false,
            TomogramCombinationDialog.ALL_FIELDS);
        updateCombineParams();
      }
    }
    tomogramCombinationDialog.setParameters(metaData);
    //  Show the process panel
    mainPanel.showProcess(tomogramCombinationDialog.getContainer(),
        AxisID.FIRST);
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
          "Can't open 3dmod on tomograms for matching models", AxisID.ONLY);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
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
          "Can't open 3dmod on matchcheck.mat or matchcheck.rec", AxisID.ONLY);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
  }

  /**
   * Open the patch region models in tomogram being matched to
   */
  public void imodPatchRegionModel(Run3dmodMenuOptions menuOptions) {
    // FIXME do we want to be updating here break model
    // Get the latest combine parameters from the dialog
    updateCombineParams();

    // FIXME axis should be a parameter to the call
    AxisID axisID;
    if (metaData.getCombineParams().getMatchBtoA()) {
      axisID = AxisID.FIRST;
    }
    else {
      axisID = AxisID.SECOND;
    }
    try {
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID, "patch_region.mod",
          true, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on tomogram for patch region models", AxisID.ONLY);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
  }

  /**
   * Open the patch vector models in 3dmod
   */
  public void imodPatchVectorModel() {
    try {
      imodManager.open(ImodManager.PATCH_VECTOR_MODEL_KEY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on tomogram for patch vector model", AxisID.ONLY);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
  }

  /**
   * Open the tomogram being matched to
   */
  public void imodMatchedToTomogram(Run3dmodMenuOptions menuOptions) {
    AxisID axisID;
    if (metaData.getCombineParams().getMatchBtoA()) {
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
          "Can't open 3dmod on tomogram being matched to", AxisID.ONLY);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
  }

  /**
   * Tomogram combination done method, move on to the post processing window
   */
  public void doneTomogramCombinationDialog() {
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update combine.com without an active tomogram combination dialog",
              "Program logic error", AxisID.ONLY);
      return;
    }
    setAdvanced(tomogramCombinationDialog.getDialogType(),
        tomogramCombinationDialog.isAdvanced());
    DialogExitState exitState = tomogramCombinationDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(AxisID.ONLY);
    }
    else {
      // Update the com script and metadata info from the tomogram
      // combination dialog box. Since there are multiple pages and scripts
      // associated with the postpone button get the ones that are appropriate
      updateCombineParams();
      tomogramCombinationDialog.getParameters(metaData);
      if (tomogramCombinationDialog.isCombinePanelEnabled()) {
        if (!updateSolvematchCom()) {
          return;
        }
        if (!updatePatchcorrCom()) {
          return;
        }
        if (!updateMatchorwarpCom(false)) {
          return;
        }
        if (!updateVolcombineCom()) {
          return;
        }
      }
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
        mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
        mainPanel.showBlankProcess(AxisID.ONLY);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setTomogramCombinationState(ProcessState.COMPLETE);
        mainPanel.setTomogramCombinationState(ProcessState.COMPLETE);
        openPostProcessingDialog();
      }
      saveTestParamFile(AxisID.ONLY);
    }
  }

  /**
   * Check to see if the combine scripts exist
   * 
   * @return true if the combine scripts exist
   */
  public boolean combineScriptsExist() {
    File solvematchshift = new File(propertyUserDir, "solvematchshift.com");
    File solvematchmod = new File(propertyUserDir, "solvematchmod.com");
    File solvematch = new File(propertyUserDir, "solvematch.com");
    File matchvol1 = new File(propertyUserDir, "matchvol1.com");
    File matchorwarp = new File(propertyUserDir, "matchorwarp.com");
    File patchcorr = new File(propertyUserDir, "patchcorr.com");
    File volcombine = new File(propertyUserDir, "volcombine.com");
    File warpvol = new File(propertyUserDir, "warpvol.com");
    return (solvematch.exists() || (solvematchshift.exists() && solvematchmod
        .exists()))
        && matchvol1.exists()
        && matchorwarp.exists()
        && patchcorr.exists()
        && volcombine.exists() && warpvol.exists();
  }

  /**
   * Run the setupcombine script with the current combine parameters stored in
   * metaData object. updateCombineCom is called first to get the currect
   * parameters from the dialog.
   * 
   * @param tomogramCombinationDialog
   *            the calling dialog.
   */
  public void createCombineScripts() {
    mainPanel.setProgressBar("Creating combine scripts", 1, AxisID.ONLY);
    updateCombineParams();
    try {
      processMgr.setupCombineScripts(metaData);
      processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
      mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't run setupcombine", AxisID.ONLY);
      return;
    }
    catch (IOException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run setupcombine\n"
          + except.getMessage(), "Setupcombine IOException", AxisID.ONLY);
      return;
    }

    // Reload the initial and final match paramaters from the newly created
    // scripts

    //  Reload all of the parameters into the ComScriptManager
    loadSolvematch();
    loadPatchcorr();
    loadMatchorwarp();
    loadVolcombine();
    loadCombineComscript();

    tomogramCombinationDialog.enableCombineTabs(true);
    mainPanel.stopProgressBar(AxisID.ONLY);
  }

  /**
   * Update the combine parameters from the a specified tab of the calling
   * dialog assumes that the dialog is synchronized
   * 
   * @param tomogramCombinationDialog
   *            the calling dialog.
   */
  private void updateCombineParams() {
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update combine.com without an active tomogram combination dialog",
              "Program logic error", AxisID.ONLY);
      return;
    }
    CombineParams combineParams = new CombineParams(this);
    try {
      tomogramCombinationDialog.getCombineParams(combineParams);
      if (!combineParams.isValid(true)) {
        uiHarness.openMessageDialog(combineParams.getInvalidReasons(),
            "Invalid combine parameters", AxisID.ONLY);
        return;
      }
    }
    catch (NumberFormatException except) {
      uiHarness.openMessageDialog(except.getMessage(), "Number format error",
          AxisID.ONLY);
      return;
    }
    CombineParams originalCombineParams = metaData.getCombineParams();
    if (!originalCombineParams.equals(combineParams)) {
      metaData.setCombineParams(combineParams);
      saveTestParamFile(AxisID.ONLY);
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
   * Merge solvematchshift and solvematchmod com scripts in solvematch and
   * load solvematchinto the tomogram
   * combination dialog
   */
  public void loadSolvematch(boolean modelBased) {
    //get solvematchshift param
    comScriptMgr.loadSolvematchshift();
    SolvematchshiftParam solvematchshiftParam = comScriptMgr
        .getSolvematchshift();
    //get solvematchmod param
    comScriptMgr.loadSolvematchmod();
    SolvematchmodParam solvematchmodParam = comScriptMgr.getSolvematchmod();
    //merge shift and mod into solvematch param
    SolvematchParam solvematchParam = new SolvematchParam();
    solvematchParam.mergeSolvematchshift(solvematchshiftParam, modelBased);
    solvematchParam.mergeSolvematchmod(solvematchmodParam, modelBased);
    //update the dialog from the same source
    tomogramCombinationDialog.setSolvematchParams(solvematchParam);
    //create the new solvematch com script
    comScriptMgr.loadSolvematch();
    comScriptMgr.saveSolvematch(solvematchParam);
    //add matchshifts to the solvematch com script
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
              "Program logic error", AxisID.ONLY);
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
          "Solvematch Parameter Syntax Error", AxisID.ONLY);
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
          AxisID.ONLY);
      return;
    }
    //  Write it out the converted object disk
    comScriptMgr.loadSolvematch();
    comScriptMgr.saveSolvematch(solvematchParam);
    tomogramCombinationDialog.setSolvematchParams(solvematchParam);
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
    ConstSetParam setParam = comScriptMgr.getSetParamFromVolcombine();
    tomogramCombinationDialog.setVolcombineParams(setParam);

    tomogramCombinationDialog.enableReductionFactor(setParam != null
        && setParam.isValid());
  }

  /**
   * Update the patchcorr.com script from the information in the tomogram
   * combination dialog box
   * 
   * @return boolean
   */
  private boolean updatePatchcorrCom() {
    //  Set a reference to the correct object
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update patchcorr.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY);
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
          "Patchcorr Parameter Syntax Error", AxisID.ONLY);
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
    //  Set a reference to the correct object
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update volcombine.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY);
      return false;
    }
    try {
      SetParam setParam = comScriptMgr.getSetParamFromVolcombine();
      boolean setParamIsValid = setParam != null && setParam.isValid();
      tomogramCombinationDialog.enableReductionFactor(setParamIsValid);
      tomogramCombinationDialog.getVolcombineParams(setParam);
      if (setParamIsValid) {
        comScriptMgr.saveVolcombine(setParam);
      }
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Volcombine Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Volcombine Parameter Syntax Error", AxisID.ONLY);
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
            AxisID.ONLY);
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
  }

  /**
   * 
   * @return boolean
   */
  protected CombineComscriptState updateCombineComscriptState(int startCommand) {
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update combine.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY);
      return null;
    }
    //set goto a the start of combine
    CombineComscriptState combineComscriptState = getCombineComscript();
    if (combineComscriptState == null) {
      return null;
    }
    //set first command to run
    combineComscriptState.setStartCommand(startCommand, comScriptMgr);
    //set last command to run:
    //Volcombine is the last command to run in the combine script if parallel
    //processing is not used and either the the "stop before running volcombine"
    //checkbox is off or "Restart at volcombine" was pressed.
    if (!tomogramCombinationDialog.isParallelProcessSelected()
        && (startCommand == CombineComscriptState.VOLCOMBINE_INDEX || tomogramCombinationDialog
            .isRunVolcombine())) {
      combineComscriptState.setEndCommand(
          CombineComscriptState.VOLCOMBINE_INDEX, comScriptMgr);
    }
    else {
      combineComscriptState.setEndCommand(
          CombineComscriptState.VOLCOMBINE_INDEX - 1, comScriptMgr);
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
    //  Set a reference to the correct object
    if (tomogramCombinationDialog == null) {
      uiHarness
          .openMessageDialog(
              "Can not update matchorwarp.com without an active tomogram generation dialog",
              "Program logic error", AxisID.ONLY);
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
          "Matchorwarp Parameter Syntax Error", AxisID.ONLY);
      return false;
    }
    return true;
  }

  /**
   * Initiate the combine process from the beginning
   */
  public void combine() {
    resetNextProcess(AxisID.ONLY);
    //  FIXME: what are the necessary updates
    //  Update the scripts from the dialog panel
    updateCombineParams();
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineComscriptState.SOLVEMATCH_INDEX);
    if (combineComscriptState == null) {
      return;
    }
    if (!updateSolvematchCom()) {
      return;
    }
    if (!updatePatchcorrCom()) {
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      return;
    }
    if (!updateVolcombineCom()) {
      return;
    }

    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);
    //  Set the next process to execute when this is finished
    if (tomogramCombinationDialog.isParallelProcessSelected()
        && tomogramCombinationDialog.isRunVolcombine()) {
      setNextProcess(AxisID.ONLY, SplitcombineParam.COMMAND_NAME);
    }
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState);
    }
    catch (SystemProcessException e) {
      resetNextProcess(AxisID.ONLY);
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY);
      return;
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  public void showPane(String comscript, String pane) {
    if (comscript.equals(CombineComscriptState.COMSCRIPT_NAME)) {
      tomogramCombinationDialog.showPane(pane);
    }
  }

  /**
   * Execute the matchvol1 com script and put patchcorr in the execution queue
   */
  public void matchvol1Combine() {
    resetNextProcess(AxisID.ONLY);
    //  FIXME: what are the necessary updates
    //  Update the scripts from the dialog panel
    updateCombineParams();
    if (!updatePatchcorrCom()) {
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      return;
    }
    if (!updateVolcombineCom()) {
      return;
    }
    updateCombineParams();
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineComscriptState.MATCHVOL1_INDEX);
    if (combineComscriptState == null) {
      return;
    }
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);
    //  Check to see if solve.xf exists first
    File solveXf = new File(propertyUserDir, "solve.xf");
    if (!solveXf.exists()) {
      //nextProcess = "";
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = "solve.xf must exist in the working";
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY);
      return;
    }
    //  Set the next process to execute when this is finished
    if (tomogramCombinationDialog.isParallelProcessSelected()
        && tomogramCombinationDialog.isRunVolcombine()) {
      setNextProcess(AxisID.ONLY, SplitcombineParam.COMMAND_NAME);
    }
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState);
    }
    catch (SystemProcessException e) {
      resetNextProcess(AxisID.ONLY);
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY);
      return;
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  /**
   * Initiate the combine process from patchcorr step
   */
  public void patchcorrCombine() {
    resetNextProcess(AxisID.ONLY);
    updateCombineParams();
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineComscriptState.PATCHCORR_INDEX);
    if (combineComscriptState == null) {
      return;
    }
    if (!updatePatchcorrCom()) {
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      return;
    }
    if (!updateVolcombineCom()) {
      return;
    }

    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);
    //  Set the next process to execute when this is finished
    if (tomogramCombinationDialog.isParallelProcessSelected()
        && tomogramCombinationDialog.isRunVolcombine()) {
      setNextProcess(AxisID.ONLY, SplitcombineParam.COMMAND_NAME);
    }
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState);
    }
    catch (SystemProcessException e) {
      resetNextProcess(AxisID.ONLY);
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute patchcorr.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY);
      return;
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  protected void warnStaleFile(String key, boolean future) {
    warnStaleFile(key, null, future);
  }

  protected void warnStaleFile(String key, AxisID axisID) {
    warnStaleFile(key, axisID, false);
  }

  protected void warnStaleFile(String key, AxisID axisID, boolean future) {
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
        if (uiHarness.openYesNoDialog(message, axisID)) {
          imodManager.quit(key, axisID);
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
    }
  }

  /**
   * Initiate the combine process from matchorwarp step
   */
  public void matchorwarpCombine() {
    resetNextProcess(AxisID.ONLY);
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineComscriptState.MATCHORWARP_INDEX);
    if (combineComscriptState == null) {
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      return;
    }
    if (!updateVolcombineCom()) {
      return;
    }
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    //  Set the next process to execute when this is finished
    if (tomogramCombinationDialog.isParallelProcessSelected()
        && tomogramCombinationDialog.isRunVolcombine()) {
      setNextProcess(AxisID.ONLY, SplitcombineParam.COMMAND_NAME);
    }
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState);
    }
    catch (SystemProcessException e) {
      resetNextProcess(AxisID.ONLY);
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY);
      return;
    }
    setBackgroundThreadName(threadName, AxisID.FIRST,
        CombineComscriptState.COMSCRIPT_NAME);
  }

  /**
   * Initiate the combine process from matchorwarp step
   */
  public void matchorwarpTrial() {
    if (updateMatchorwarpCom(true)) {
      processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
      mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
      //  Set the next process to execute when this is finished
      //nextProcess = next;
      String threadName;
      try {
        threadName = processMgr.matchorwarp();
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute matchorwarp.com";
        message[1] = e.getMessage();
        uiHarness.openMessageDialog(message, "Unable to execute com script",
            AxisID.ONLY);
        return;
      }
      setThreadName(threadName, AxisID.FIRST);
      tomogramCombinationDialog.showPane(TomogramCombinationDialog.lblFinal);
      mainPanel.startProgressBar("matchorwarp", AxisID.FIRST);
    }
  }

  /**
   * Execute the combine script starting at volcombine
   */
  public void volcombine() {
    CombineComscriptState combineComscriptState = updateCombineComscriptState(CombineComscriptState.VOLCOMBINE_INDEX);
    updateCombineParams();
    if (!updateVolcombineCom()) {
      return;
    }
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    //  Set the next process to execute when this is finished
    //nextProcess = "";
    String threadName;
    try {
      threadName = processMgr.combine(combineComscriptState);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute combine.com";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute com script",
          AxisID.ONLY);
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
          AxisID.ONLY);
      return;
    }
  }

  /**
   * Open the post processing dialog
   */
  public void openPostProcessingDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(AxisID.ONLY);
      return;
    }
    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    setCurrentDialogType(DialogType.POST_PROCESSING, AxisID.ONLY);
    mainPanel.selectButton(AxisID.ONLY, DialogType.POST_PROCESSING.toString());
    if (postProcessingDialog == null) {
      postProcessingDialog = new PostProcessingDialog(this);
      //  Set the appropriate input and output files
      TrimvolParam trimvolParam = metaData.getTrimvolParam();
      try {
        trimvolParam.setDefaultRange(TrimvolParam.getInputFileName(metaData
            .getAxisType(), metaData.getDatasetName()));
      }
      catch (InvalidParameterException except) {
        String[] detailedMessage = new String[4];
        detailedMessage[0] = "Unable to set trimvol range";
        detailedMessage[1] = "Does the reconstruction file exist yet?";
        detailedMessage[2] = "";
        detailedMessage[3] = except.getMessage();
        uiHarness.openMessageDialog(detailedMessage, "Invalid parameter: "
            + trimvolParam.getInputFileName(), AxisID.ONLY);
        //    Delete the dialog
        postProcessingDialog = null;
        return;
      }
      catch (IOException except) {
        uiHarness.openMessageDialog(except.getMessage(), "IO Error: "
            + trimvolParam.getInputFileName(), AxisID.ONLY);
        //      Delete the dialog
        postProcessingDialog = null;
        return;
      }
      postProcessingDialog.setTrimvolParams(trimvolParam);
      postProcessingDialog.setParameters(metaData.getSqueezevolParam());
    }
    mainPanel.showProcess(postProcessingDialog.getContainer(), AxisID.ONLY);
  }

  /**
   * Open the clean up dialog
   */
  public void openCleanUpDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog(AxisID.ONLY);
      return;
    }
    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    setCurrentDialogType(DialogType.CLEAN_UP, AxisID.ONLY);
    mainPanel.selectButton(AxisID.ONLY, "Clean Up");
    if (cleanUpDialog == null) {
      cleanUpDialog = new CleanUpDialog(this);
    }
    mainPanel.showProcess(cleanUpDialog.getContainer(), AxisID.ONLY);
  }

  /**
   * Close the post processing dialog panel
   */
  public void donePostProcessing() {
    if (postProcessingDialog == null) {
      uiHarness.openMessageDialog("Post processing dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    setAdvanced(postProcessingDialog.getDialogType(), postProcessingDialog
        .isAdvanced());
    DialogExitState exitState = postProcessingDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainPanel.showBlankProcess(AxisID.ONLY);
    }
    else {
      updateTrimvolParam();
      updateSqueezevolParam();
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setPostProcessingState(ProcessState.INPROGRESS);
        mainPanel.setPostProcessingState(ProcessState.INPROGRESS);
        mainPanel.showBlankProcess(AxisID.ONLY);
      }
      else if (exitState != DialogExitState.SAVE) {
        processTrack.setPostProcessingState(ProcessState.COMPLETE);
        mainPanel.setPostProcessingState(ProcessState.COMPLETE);
        openCleanUpDialog();
      }
      saveTestParamFile(AxisID.ONLY);
    }
    postProcessingDialog = null;
  }

  /**
   * Close the clean up dialog panel
   */
  public void doneCleanUp() {
    if (cleanUpDialog == null) {
      uiHarness.openMessageDialog("Clean Up dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
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
      saveTestParamFile(AxisID.ONLY);
    }
    cleanUpDialog = null;
    mainPanel.showBlankProcess(AxisID.ONLY);
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
          "Can't open 3dmod on the trimmed tomogram", AxisID.ONLY);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
  }

  /**
   * calls ProcessManager.generateAlignLogs() when the ta files are out of date
   * @param commandName
   * @param axisID
   * @return true if any log files where changed
   */
  public boolean updateLog(String commandName, AxisID axisID) {
    String alignLogName = "align" + axisID.getExtension() + ".log";
    File alignLog = new File(propertyUserDir, alignLogName);
    String taErrorLogName = "taError" + axisID.getExtension() + ".log";
    File taErrorLog = new File(propertyUserDir, taErrorLogName);
    if (!alignLog.exists()) {
      return false;
    }
    if (!taErrorLog.exists()
        || taErrorLog.lastModified() < alignLog.lastModified()) {
      processMgr.generateAlignLogs(axisID);
      return true;
    }
    return false;
  }

  /**
   * Open the trimmed volume in 3dmod
   */
  public void imodTrimmedVolume(Run3dmodMenuOptions menuOptions) {
    TrimvolParam trimvolParam = new TrimvolParam(this);
    postProcessingDialog.getTrimvolParams(trimvolParam);
    try {
      imodManager.setSwapYZ(ImodManager.TRIMMED_VOLUME_KEY, !trimvolParam
          .isSwapYZ());
      imodManager.open(ImodManager.TRIMMED_VOLUME_KEY, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on the trimmed tomogram", AxisID.ONLY);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
  }

  /**
   * Open the squeezed volume in 3dmod
   */
  public void imodSqueezedVolume(Run3dmodMenuOptions menuOptions) {
    // Make sure that the post processing panel is open
    if (postProcessingDialog == null) {
      uiHarness.openMessageDialog("Post processing dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    try {
      imodManager.setSwapYZ(ImodManager.SQUEEZED_VOLUME_KEY,
          !postProcessingDialog.isSqueezevolFlipped());
      imodManager.open(ImodManager.SQUEEZED_VOLUME_KEY, menuOptions);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Can't open 3dmod on the squeezed tomogram", AxisID.ONLY);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
  }

  /**
   * Execute trimvol
   */
  public void trimVolume() {
    // Make sure that the post processing panel is open
    if (postProcessingDialog == null) {
      uiHarness.openMessageDialog("Post processing dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    TrimvolParam trimvolParam = updateTrimvolParam();
    // Start the trimvol process
    processTrack.setPostProcessingState(ProcessState.INPROGRESS);
    mainPanel.setPostProcessingState(ProcessState.INPROGRESS);
    String threadName;
    try {
      threadName = processMgr.trimVolume(trimvolParam);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute trimvol command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar("Trimming volume", AxisID.ONLY);
  }

  /**
   * Execute squeezevol
   */
  public void squeezevol() {
    // Make sure that the post processing panel is open
    if (postProcessingDialog == null) {
      uiHarness.openMessageDialog("Post processing dialog not open",
          "Program logic error", AxisID.ONLY);
      return;
    }
    ConstSqueezevolParam squeezevolParam = updateSqueezevolParam();
    // Start the trimvol process
    processTrack.setPostProcessingState(ProcessState.INPROGRESS);
    mainPanel.setPostProcessingState(ProcessState.INPROGRESS);
    String threadName;
    try {
      threadName = processMgr.squeezeVolume(squeezevolParam);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute squeezevol command";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command",
          AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel.startProgressBar("Squeezing volume", AxisID.ONLY);
  }

  /**
   * updates ConstMetaData.trimvolParam with data from postProcessingDialog.
   * Sets isDataParamDirty if necessary
   * @return the updated TrimvolParam
   *
   */
  protected TrimvolParam updateTrimvolParam() {
    //Get trimvol param data from dialog.
    TrimvolParam dialogTrimvolParam = new TrimvolParam(this);
    postProcessingDialog.getTrimvolParams(dialogTrimvolParam);
    //Get the metadata trimvol param.
    TrimvolParam trimvolParam = metaData.getTrimvolParam();
    postProcessingDialog.getTrimvolParams(trimvolParam);
    //Add input and output files.
    trimvolParam.setInputFileName(metaData.getAxisType(), metaData
        .getDatasetName());
    trimvolParam.setOutputFileName(metaData.getDatasetName());
    return trimvolParam;
  }

  /**
   * updates ConstMetaData.squeezevolParam with data from postProcessingDialog.
   * Sets isDataParamDirty if necessary
   * @return the updated SqueezevolParam
   *
   */
  protected ConstSqueezevolParam updateSqueezevolParam() {
    //Get the metadata trimvol param.
    SqueezevolParam squeezevolParam = metaData.getSqueezevolParam();
    postProcessingDialog.getParameters(squeezevolParam);
    return squeezevolParam;
  }

  //
  //  Utility functions
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
   *  
   */
  private void setupRequestDialog(AxisID axisID) {
    String[] message = new String[2];
    message[0] = "The setup process has not been completed";
    message[1] = "Complete the Setup process before opening other process dialogs";
    uiHarness.openMessageDialog(message, "Program Operation Error", axisID);
    return;
  }

  /**
   * Run the specified command as a background process with a indeterminate
   * progress bar.
   */
  public void backgroundProcess(String commandLine, AxisID axisID) {
    processMgr.test(commandLine, axisID);
  }

  /**
   * Start generic progress bar
   * @param value
   * @param axisID
   */
  public void startProgressBar(String label, AxisID axisID) {
    mainPanel.startProgressBar(label, axisID);
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
  protected void startNextProcess(AxisID axisID) {
    String nextProcess = getNextProcess(axisID);
    if (nextProcess.equals("tilt")) {
      tiltProcess(axisID);
    }
    else if (nextProcess.equals("checkUpdateFiducialModel")) {
      checkUpdateFiducialModel(axisID);
      return;
    }
    else if (nextProcess.equals(ArchiveorigParam.COMMAND_NAME)) {
      archiveOriginalStack(AxisID.SECOND);
    }
    else if (nextProcess
        .equals(getNextProcessProcesschunksString(ProcessName.TILT))) {
      processchunksTilt(axisID);
    }
    else if (nextProcess
        .equals(getNextProcessProcesschunksString(ProcessName.VOLCOMBINE))) {
      processchunksVolcombine();
    }
    else if (nextProcess.equals(SplitcombineParam.COMMAND_NAME)) {
      splitcombine();
    }
  }

  protected void updateDialog(ProcessName processName, AxisID axisID) {
    if (axisID != AxisID.ONLY
        && (processName == ProcessName.PRENEWST || processName == ProcessName.TRACK)) {
      updateDialog(fiducialModelDialogB, AxisID.SECOND);
      updateDialog(fiducialModelDialogA, AxisID.FIRST);
    }
    if (processName == ProcessName.NEWST) {
      if (axisID == AxisID.SECOND) {
        updateDialog(tomogramGenerationDialogB, axisID);
      }
      else {
        updateDialog(tomogramGenerationDialogA, axisID);
      }
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

  private void updateDialog(TomogramGenerationDialog dialog, AxisID axisID) {
    if (dialog == null) {
      return;
    }
    dialog.updateFilter(Utilities.fileExists(this, ".ali", axisID));
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

  //  Test helper functions
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

  protected void checkUpdateFiducialModel(AxisID axisID) {
    resetNextProcess(axisID);
    FidXyz fidXyz = getFidXyz(axisID);
    MRCHeader prealiHeader = getMrcHeader(axisID, ".preali");
    MRCHeader rawstackHeader = getMrcHeader(axisID, ".st");
    try {
      fidXyz.read();
      prealiHeader.read();
      rawstackHeader.read();
    }
    catch (IOException except) {
      return;
    }
    catch (InvalidParameterException except) {
      except.printStackTrace();
      return;
    }
    if (!fidXyz.exists()) {
      return;
    }
    //if fidXyz.getPixelSize() is 1, then the binning used in align.com must
    //have been 1, if the preali binning is also 1, then no error message should
    //be sent.  preali binning is preali pixel spacing / .st pixel spacing
    boolean fidXyzPixelSizeSet = fidXyz.isPixelSizeSet();
    if (!fidXyzPixelSizeSet) {
      if (Math.round(prealiHeader.getXPixelSpacing()
          / rawstackHeader.getXPixelSpacing()) == 1) {
        return;
      }
    }
    if (!fidXyzPixelSizeSet
        || fidXyz.getPixelSize() != prealiHeader.getXPixelSpacing()) {
      //if (getStackBinning(axisID, ".preali") != getBackwardCompatibleAlignBinning(axisID)) {
      uiHarness
          .openMessageDialog(
              "The prealigned image stack binning has changed.  You must:\n    1. Go "
                  + "to Fiducial Model Gen. and Press Fix Fiducial Model to open the "
                  + "fiducial model.\n    2. Save the fiducial model by pressing "
                  + "\"s\".\n    3. Go to Fine Alignment and press Compute Alignment to"
                  + " rerun align" + axisID.getExtension() + ".com.",
              "Prealigned image stack binning has changed", axisID);
      return;
    }
  }

  public FidXyz getFidXyz(AxisID axisID) {
    return new FidXyz(propertyUserDir, metaData.getDatasetName()
        + axisID.getExtension() + "fid.xyz");
  }

  public MRCHeader getMrcHeader(AxisID axisID, String filename) {
    File file = new File(propertyUserDir, metaData.getDatasetName()
        + axisID.getExtension() + filename);
    return MRCHeader.getInstance(propertyUserDir, file.getAbsolutePath(),
        axisID);
  }

  protected void createComScriptManager() {
    comScriptMgr = new ComScriptManager(this);
  }

  protected void createProcessManager() {
    processMgr = new ProcessManager(this);
  }

  protected void createMainPanel() {
    mainPanel = new MainTomogramPanel(this);
    mainPanel = (MainTomogramPanel) mainPanel;
  }

  /**
   * Set the data set parameter file. This also updates the mainframe data
   * parameters.
   * @param paramFile a File object specifying the data set parameter file.
   */
  public void setTestParamFile(File paramFile) {
    this.paramFile = paramFile;
    //  Update main window information and status bar
    mainPanel.setStatusBarText(paramFile, metaData);
  }

  protected void createProcessTrack() {
    processTrack = new ProcessTrack();
  }

  protected int getNumStorables() {
    return 3;
  }

  protected void createState() {
    state = new TomogramState(this);
  }

  public TomogramState getState() {
    return state;
  }

  protected BaseState getBaseState() {
    return state;
  }

  public MetaData getMetaData() {
    return metaData;
  }

  public ConstMetaData getConstMetaData() {
    return (ConstMetaData) metaData;
  }

  public BaseMetaData getBaseMetaData() {
    return (BaseMetaData) metaData;
  }

  protected void setMetaData(ImodManager imodManager) {
    imodManager.setMetaData(metaData);
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  protected void getProcessTrack(Storable[] storable, int index) {
    if (storable == null) {
      return;
    }
    storable[index] = processTrack;
  }

  protected BaseProcessTrack getProcessTrack() {
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

  /**
   * Gets the binning that can be used to repair a older tilt.com file.  Older
   * tilt.com file contain parameters, including full image size, which have
   * been binned.  And they do not contain the binning.  Function calculates the
   * binning from the raw stack full image size and the full image size in 
   * tilt.com.
   * @param axisID
   * @param tiltParam
   * @return
   */
  private int getBackwardCompatibleTiltBinning(AxisID axisID,
      ConstTiltParam tiltParam) {
    MRCHeader rawstackHeader = getMrcHeader(axisID, ".st");
    try {
      rawstackHeader.read();
    }

    catch (InvalidParameterException e) {
      e.printStackTrace();
      return 1;
    }
    catch (IOException e) {
      return 1;
    }
    int binning = 1;
    int tiltFullImageX = tiltParam.getFullImageX();
    //defaults to Integer.MIN_VALUE in ConstTiltParam
    if (tiltFullImageX > 0) {
      binning = Math.round(rawstackHeader.getNColumns() / tiltFullImageX);
    }
    if (binning < 1) {
      return 1;
    }
    return binning;
  }

  /**
   * Gets the binning that can be used to run tilt against a stack (.preali or
   * .ali).  Function calculates the binning from the stack's pixel spacing and
   * the raw stack's pixel spacing.
   * @param axisID
   * @param stackExtension
   * @return
   */
  public long getStackBinning(AxisID axisID, String stackExtension) {
    return getStackBinning(axisID, stackExtension, false);
  }

  /**
   * Gets the binning that can be used to run tilt against a stack (.preali or
   * .ali).  Function calculates the binning from the stack's pixel spacing and
   * the raw stack's pixel spacing.
   * @param axisID
   * @param stackExtension .ali or .preali
   * @param nullIfFailed when true returns null on failure, otherwise returns 1
   * on failure.
   * @return
   */
  public long getStackBinning(AxisID axisID, String stackExtension,
      boolean nullIfFailed) {
    MRCHeader rawstackHeader = getMrcHeader(axisID, ".st");
    MRCHeader stackHeader = getMrcHeader(axisID, stackExtension);
    long defaultValue = nullIfFailed ? EtomoNumber.LONG_NULL_VALUE : 1;
    try {
      rawstackHeader.read();
      stackHeader.read();
    }
    catch (InvalidParameterException e) {
      //missing file
      e.printStackTrace();
      return defaultValue;
    }
    catch (IOException e) {
      return defaultValue;
    }
    long binning = defaultValue;
    double rawstackXPixelSpacing = rawstackHeader.getXPixelSpacing();
    if (rawstackXPixelSpacing > 0) {
      binning = Math.round(stackHeader.getXPixelSpacing()
          / rawstackXPixelSpacing);
    }
    if (binning != defaultValue && binning < 1) {
      return 1;
    }
    return binning;
  }

  /**
   * Gets the binning that can be used to repair a older align.com file.  Older
   * align.com file contain a binned zshift parameter.  And they do not contain
   * the binning.  Function calculates the binning from the raw stack pixel
   * spacing and the fid.xyz pixel spacing (if it exists) or the .preali pixel
   * spacing.  The fid.xyz pixel spacing is more accurate because the file is
   * created by align when align succeeds.  If align fails, then the .preali
   * pixel spacing will be inaccurate if the binning has changed since an
   * the last time the .preali was built.
   * @param axisID
   * @param tiltParam
   * @return
   */
  private long getBackwardCompatibleAlignBinning(AxisID axisID) {
    MRCHeader rawstackHeader = getMrcHeader(axisID, ".st");
    try {
      rawstackHeader.read();
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      return 1;
    }
    catch (IOException e) {
      return 1;
    }
    FidXyz fidXyz = getFidXyz(axisID);
    boolean fidXyzFailed = false;
    try {
      fidXyz.read();
    }
    catch (IOException e) {
      e.printStackTrace();
      fidXyzFailed = true;
    }
    if (!fidXyzFailed) {
      //Another layer of backward compatibility.  Handle the time before fid.xyz
      //files where created.  This also handles the situation where align.com has
      //not been run and has the original values from copytomocoms.
      if (!fidXyz.exists()) {
        return 1;
      }
      //Align.com must have failed.  The fallback is to use pixel spacing from
      //.preali.
      if (fidXyz.isEmpty()) {
        fidXyzFailed = true;
      }
      //Another layer of backward compatibility.  There was a small period of
      //time when binning existed but the pixel spacing was not added to fid.xyz
      //(3.2.7 (3/16/04) - 3.2.20 (6/19/04).  If this fid.xyz was created before
      //binning we could return 1, but we don't know that and we can't trust
      //change times on old files that could have been copied, so we need to use
      //the fallback in this case.
      else if (!fidXyz.isPixelSizeSet()) {
        fidXyzFailed = true;
      }
    }
    double rawstackXPixelSpacing = rawstackHeader.getXPixelSpacing();
    //Unable to calculate binning
    if (rawstackXPixelSpacing <= 0) {
      return 1;
    }
    //calculate binning from fid.xyz
    if (!fidXyzFailed) {
      long binning = Math.round(fidXyz.getPixelSize() / rawstackXPixelSpacing);
      if (binning < 1) {
        return 1;
      }
      return binning;
    }
    //fallback to .preali
    MRCHeader stackHeader = getMrcHeader(axisID, ".preali");
    try {
      stackHeader.read();
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      return 1;
    }
    catch (IOException e) {
      e.printStackTrace();
      return 1;
    }
    long binning = Math.round(stackHeader.getXPixelSpacing()
        / rawstackXPixelSpacing);
    if (binning < 1) {
      return 1;
    }
    return binning;
  }

  public void touch(File file) {
    processMgr.touch(file);
  }

  /**
   * If tiltParam is an old version, upgrade it to the new version and save it
   * to tilt.com.
   * @param axisID
   * @param tiltParam
   */
  private void upgradeOldTiltCom(AxisID axisID, TiltParam tiltParam) {
    if (!tiltParam.isOldVersion()) {
      return;
    }
    int correctionBinning = getBackwardCompatibleTiltBinning(axisID, tiltParam);
    long currentBinning = getStackBinning(axisID, ".ali");
    if (tiltParam.upgradeOldVersion(correctionBinning, currentBinning)) {
      comScriptMgr.saveTilt(tiltParam, axisID);
    }
  }

  /**
   * If tiltalignParam is an old version, upgrade it to the new version and save
   * it to align.com.
   * @param axisID
   * @param tiltalignParam
   */
  private void upgradeOldAlignCom(AxisID axisID, TiltalignParam tiltalignParam) {
    if (!tiltalignParam.isOldVersion()) {
      return;
    }
    long correctionBinning = getBackwardCompatibleAlignBinning(axisID);
    long currentBinning = getStackBinning(axisID, ".preali");
    if (tiltalignParam.upgradeOldVersion(correctionBinning, currentBinning)) {
      comScriptMgr.saveAlign(tiltalignParam, axisID);
    }
  }

  private final SplittiltParam updateSplittiltParam(AxisID axisID) {
    TomogramGenerationDialog dialog = (TomogramGenerationDialog) getDialog(
        DialogType.TOMOGRAM_GENERATION, axisID);
    if (dialog == null) {
      return null;
    }
    SplittiltParam param = new SplittiltParam(axisID);
    getParallelPanel(axisID).getParameters(param);
    return param;
  }

  public void splittilt(AxisID axisID) {
    splittilt(axisID, false);
  }

  public void splittilt(AxisID axisID, boolean trialMode) {
    if (!updateTiltCom(axisID, !trialMode)) {
      return;
    }
    SplittiltParam param = updateSplittiltParam(axisID);
    if (param == null) {
      return;
    }
    processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
    mainPanel.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
    setNextProcess(axisID, getNextProcessProcesschunksString(ProcessName.TILT));
    String threadName;
    try {
      threadName = processMgr.splittilt(param, axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + SplittiltParam.COMMAND_NAME;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID);
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel
        .startProgressBar("Running " + SplittiltParam.COMMAND_NAME, axisID);
    return;
  }
  
  private final SplitcombineParam updateSplitcombineParam() {
    if (tomogramCombinationDialog == null) {
      return null;
    }
    SplitcombineParam param = new SplitcombineParam();
    tomogramCombinationDialog.getParameters(param);
    return param;
  }
  
  public void splitcombine() {
    if (!updateVolcombineCom()) {
      return;
    }
    SplitcombineParam param = updateSplitcombineParam();
    if (param == null) {
      return;
    }
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainPanel.setTomogramCombinationState(ProcessState.INPROGRESS);
    setNextProcess(AxisID.ONLY, getNextProcessProcesschunksString(ProcessName.VOLCOMBINE));
    String threadName;
    try {
      threadName = processMgr.splitcombine(param);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + SplitcombineParam.COMMAND_NAME;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", AxisID.ONLY);
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainPanel
        .startProgressBar("Running " + SplitcombineParam.COMMAND_NAME, AxisID.ONLY);
    return;
  }
  
  private final String getNextProcessProcesschunksString(ProcessName processName) {
    return ProcesschunksParam.COMMAND_NAME + " " + processName;
  }
  
  public final void processchunksTilt(AxisID axisID) {
    processchunks(axisID, mapGenerationDialog(axisID));
  }
  
  public final void processchunksVolcombine() {
    processchunks(AxisID.ONLY, tomogramCombinationDialog);
  }
  
  protected BaseProcessManager getProcessManager() {
    return processMgr;
  }
}
/**
 * <p> $Log$
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

