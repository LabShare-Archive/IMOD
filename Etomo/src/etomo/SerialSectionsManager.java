package etomo;

import java.io.File;
import java.io.IOException;

import etomo.comscript.BlendmontParam;
import etomo.comscript.ConstNewstParam;
import etomo.comscript.ExtractpiecesParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.MidasParam;
import etomo.comscript.NewstParam;
import etomo.comscript.SerialSectionsComScriptManager;
import etomo.comscript.TomodataplotsParam;
import etomo.comscript.MidasParam.Mode;
import etomo.comscript.XfalignParam;
import etomo.comscript.XftoxgParam;
import etomo.logic.DatasetTool;
import etomo.logic.SerialSectionsStartupData;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.SerialSectionsProcessManager;
import etomo.process.SystemProcessException;
import etomo.storage.BlendmontLog;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AutoAlignmentMetaData;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.ConstProcessSeries;
import etomo.type.ConstSerialSectionsMetaData;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.InterfaceType;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.SerialSectionsMetaData;
import etomo.type.SerialSectionsState;
import etomo.type.ViewType;
import etomo.ui.UIComponent;
import etomo.ui.swing.Deferred3dmodButton;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.MainSerialSectionsPanel;
import etomo.ui.swing.ProcessDisplay;
import etomo.ui.swing.SerialSectionsDialog;
import etomo.ui.swing.SerialSectionsStartupDialog;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.InvalidParameterException;
import etomo.util.Utilities;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2012</p>
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
public final class SerialSectionsManager extends BaseManager {
  public static final String rcsid = "$Id:$";

  private static final AxisID AXIS_ID = AxisID.ONLY;

  private final SerialSectionsState state = new SerialSectionsState();

  private final SerialSectionsMetaData metaData;
  private final SerialSectionsProcessManager processMgr;

  private SerialSectionsStartupDialog startupDialog = null;
  private SerialSectionsDialog dialog = null;
  private AutoAlignmentController autoAlignmentController = null;
  /**
   * valid is for handling failure before the manager key is set in EtomoDirector.
   */
  private boolean valid = true;
  private String origUserDir = null;

  // Initialized during parent constructor
  private MainSerialSectionsPanel mainPanel;
  private SerialSectionsComScriptManager comScriptMgr;

  private SerialSectionsManager() {
    this("");
  }

  SerialSectionsManager(final String paramFileName) {
    super();
    metaData = new SerialSectionsMetaData(getLogProperties());
    processMgr = new SerialSectionsProcessManager(this);
    initializeUIParameters(paramFileName, AXIS_ID);
  }

  static SerialSectionsManager getInstance() {
    SerialSectionsManager instance = new SerialSectionsManager();
    instance.openDialog();
    return instance;
  }

  static SerialSectionsManager getInstance(final String paramFileName) {
    SerialSectionsManager instance = new SerialSectionsManager(paramFileName);
    instance.openDialog();
    return instance;
  }

  void initializeUIParameters(final String paramFileName, final AxisID axisID) {
    super.initializeUIParameters(paramFileName, axisID);
    if (loadedParamFile) {
      System.setProperty("user.dir", propertyUserDir);
    }
  }

  private void openDialog() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData, logWindow);
      if (loadedParamFile) {
        openSerialSectionsDialog(null);
      }
      else {
        openSerialSectionsStartupDialog();
      }
    }
  }

  public boolean isStartupPopupOpen() {
    return !loadedParamFile;
  }

  void display() {
    if (startupDialog != null) {
      startupDialog.display();
    }
  }

  /**
   * MUST run reconnect for all axes
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
    reconnect(axisProcessData.getSavedProcessData(AXIS_ID), AXIS_ID, true);
  }

  /**
   * Create (if necessary) and show the serial sections startup dialog.
   */
  private void openSerialSectionsStartupDialog() {
    if (startupDialog == null) {
      String actionMessage = Utilities.prepareDialogActionMessage(
          DialogType.SERIAL_SECTIONS_STARTUP, AxisID.ONLY, null);
      mainPanel.setStaticProgressBar("Starting Serial Sections interface", AXIS_ID);
      startupDialog = SerialSectionsStartupDialog.getInstance(this, AXIS_ID);
      if (actionMessage != null) {
        System.err.println(actionMessage);
      }
    }
  }

  public void setStartupData(final SerialSectionsStartupData startupData) {
    startupDialog = null;
    setParamFile(startupData);
    openSerialSectionsDialog(startupData);
    if (!loadedParamFile) {
      mainPanel.stopProgressBar(AXIS_ID, ProcessEndState.FAILED);
      UIHarness.INSTANCE.openMessageDialog(this,
          "Failed to load or create parameter file, unable to continue.", "Failed",
          AxisID.ONLY);
      valid = false;
      return;
    }
    mainPanel.stopProgressBar(AXIS_ID, ProcessEndState.DONE);
  }

  /**
   * Tries to set paramFile.  Returns true if able to set paramFile.
   * If paramFile is already set, returns true.  Returns false if unable
   * to set paramFile.  Updates the serial sections dialog display if paramFile
   * was set successfully.
   * @return
   */
  public boolean setParamFile(final SerialSectionsStartupData startupData) {
    if (loadedParamFile) {
      return true;
    }
    if (startupData == null) {
      return false;
    }
    String name = startupData.getRootName();
    File paramFile = startupData.getParamFile();
    if (!paramFile.exists()) {
      processMgr.createNewFile(paramFile.getAbsolutePath());
    }
    initializeUIParameters(paramFile, AXIS_ID, false);
    if (!loadedParamFile) {
      return false;
    }
    metaData.setStartupData(startupData);
    if (!metaData.isValid()) {
      uiHarness.openMessageDialog(this,
          "Invalid data, unable to proceed.  Please exit and restart Etomo",
          "Fatal Error");
      return false;
    }
    imodManager.setMetaData(metaData);
    mainPanel.setStatusBarText(paramFile, metaData, logWindow);
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getName());
    return true;
  }

  public void cancelStartup() {
    mainPanel.stopProgressBar(AXIS_ID, ProcessEndState.KILLED);
    EtomoDirector.INSTANCE.closeCurrentManager(AxisID.ONLY, false);
  }

  /**
   * Create (if necessary) and show the serial sections dialog.  Update data if the param
   * file has been set.
   */
  private void openSerialSectionsDialog(final SerialSectionsStartupData startupData) {
    if (!loadedParamFile && startupData == null) {
      UIHarness.INSTANCE
          .openMessageDialog(this,
              "Failed to load the parameter file, unable to continue.", "Failed",
              AxisID.ONLY);
      valid = false;
      return;
    }
    if (dialog == null) {
      dialog = SerialSectionsDialog.getInstance(this, AXIS_ID);
    }
    autoAlignmentController = new AutoAlignmentController(this, dialog, imodManager,
        metaData.getStack());
    dialog.setAutoAlignmentController(autoAlignmentController);
    if (loadedParamFile) {
      autoAlignmentController.createEmptyXfFile();
    }
    setSerialSectionsDialogParameters();
    mainPanel.showProcess(dialog.getRootContainer(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(
        DialogType.SERIAL_SECTIONS, AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  boolean startNextProcess(final UIComponent uiComponent, final AxisID axisID,
      final ProcessSeries.Process process,
      final ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      final DialogType dialogType, final ProcessDisplay display) {
    if (super.startNextProcess(uiComponent, axisID, process, processResultDisplay,
        processSeries, dialogType, display)) {
      return true;
    }
    if (process.equals(Task.CHANGE_DIRECTORY)) {
      changeDirectory(axisID, processSeries);
      return true;
    }
    if (process.equals(Task.EXTRACT_PIECES)) {
      extractpieces(uiComponent, axisID, processSeries);
      return true;
    }
    if (process.equals(Task.CREATE_COMSCRIPTS)) {
      createComscripts(uiComponent, axisID, processSeries);
      return true;
    }
    if (process.equals(Task.COPY_DISTORTION_FIELD_FILE)) {
      copyDistortionFieldFile(processSeries, uiComponent, axisID);
      return true;
    }
    if (process.equals(Task.DONE_STARTUP_DIALOG)) {
      doneStartupDialog(processSeries, axisID);
      return true;
    }
    if (process.equals(Task.RESET_STARTUP_STATE)) {
      resetStartupState(processSeries, axisID);
      return true;
    }
    if (process.equals(Task.XFTOXG)) {
      xftoxg(processSeries, axisID);
      return true;
    }
    if (process.equals(Task.ALIGN)) {
      align(processSeries, axisID);
      return true;
    }
    return false;
  }

  public void completeStartup(final UIComponent uiComponent, final AxisID axisID) {
    ProcessSeries processSeries = new ProcessSeries(this,
        DialogType.SERIAL_SECTIONS_STARTUP);
    processSeries.setNextProcess(Task.CHANGE_DIRECTORY);
    processSeries.addProcess(Task.EXTRACT_PIECES);
    processSeries.addProcess(Task.CREATE_COMSCRIPTS);
    processSeries.addProcess(Task.COPY_DISTORTION_FIELD_FILE, true);
    processSeries.setLastProcess(Task.DONE_STARTUP_DIALOG);
    processSeries.setFailProcess(Task.RESET_STARTUP_STATE);
    processSeries.startNextProcess(axisID);
  }

  public void preblend(ProcessSeries processSeries,
      final Deferred3dmodButton deferred3dmodButton, final AxisID axisID,
      final Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (getViewType() != ViewType.MONTAGE || dialog == null) {
      processSeries.startFailProcess(axisID);
      return;
    }
    BlendmontParam param = updatePreblendComscript(axisID, true);
    if (param == null) {
      processSeries.startFailProcess(axisID);
      return;
    }
    String threadName = null;
    try {
      threadName = processMgr.blend(param, axisID, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to run preblend.\n" + e.getMessage(),
          "Process Failed", axisID);
      processSeries.startFailProcess(axisID);
      return;
    }
    File stack = getStack();
    if (stack != null
        && !DatasetTool.isOneBy(getPropertyUserDir(), stack.getName(), this, axisID)) {
      processSeries.addProcess(TomodataplotsParam.Task.SERIAL_SECTIONS_MEAN_MAX);
    }
    setThreadName(threadName, axisID);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
  }

  /**
   * Run fix edges in Midas
   */
  public void midasFixEdges(final AxisID axisID, final ConstProcessSeries processSeries) {
    MidasParam param = new MidasParam(this, axisID, Mode.SERIAL_SECTIONS_FIX_EDGES);
    getParameters(param);
    dialog.getParameters(param);
    try {
      processMgr.midas(param);
    }
    catch (SystemProcessException e) {
      UIHarness.INSTANCE.openMessageDialog(this,
          "Unable open midas on " + metaData.getStack() + ".  ", "Unable to Run Process",
          axisID);
      if (processSeries != null) {
        processSeries.startFailProcess(axisID);
      }
      return;
    }
    if (processSeries != null) {
      processSeries.startNextProcess(axisID);
    }
  }

  public void align(final AxisID axisID, final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    ProcessSeries processSeries = new ProcessSeries(this,
        DialogType.SERIAL_SECTIONS_STARTUP);
    processSeries.setNextProcess(Task.XFTOXG);
    processSeries.setLastProcess(Task.ALIGN);
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    processSeries.startNextProcess(axisID);
  }

  private void changeDirectory(final AxisID axisID, final ConstProcessSeries processSeries) {
    File stack = getStack();
    if (stack == null) {
      if (processSeries != null) {
        processSeries.startFailProcess(axisID);
      }
      return;
    }
    propertyUserDir = stack.getParent();
    origUserDir = System.setProperty("user.dir", propertyUserDir);
    if (processSeries != null) {
      processSeries.startNextProcess(axisID);
    }
  }

  /**
   * Runs extractpieces.  Starts a failure process if it fails.  Starts the next process
   * if it didn't spawn a process thread.
   * @param axisID
   */
  private void extractpieces(final UIComponent uiComponent, final AxisID axisID,
      final ProcessSeries processSeries) {
    ViewType viewType = getViewType();
    if (viewType == null) {
      if (processSeries != null) {
        processSeries.startFailProcess(axisID);
      }
      return;
    }
    if (viewType != ViewType.MONTAGE) {
      if (processSeries != null) {
        processSeries.startNextProcess(axisID);
      }
      return;
    }
    File pieceListFile = DatasetFiles.getPieceListFile(this, axisID);
    if (!pieceListFile.exists()) {
      ExtractpiecesParam param = new ExtractpiecesParam(getStack().getName(), getName(),
          AxisType.SINGLE_AXIS, this, axisID);
      String threadName;
      try {
        threadName = processMgr.extractpieces(param, axisID, processSeries);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        uiHarness.openMessageDialog(this, uiComponent, "Can not execute "
            + ExtractpiecesParam.COMMAND_NAME + "\n" + e.getMessage(),
            "Unable to execute command", axisID);
        if (processSeries != null) {
          processSeries.startFailProcess(axisID);
        }
        return;
      }
      setThreadName(threadName, axisID);
      getMainPanel().startProgressBar("Running " + ExtractpiecesParam.COMMAND_NAME,
          axisID, ProcessName.EXTRACTPIECES);
    }
  }

  /**
   * Creates blend or newst comscripts.
   * @param axisID
   * @param processSeries
   */
  private void createComscripts(final UIComponent uiComponent, final AxisID axisID,
      final ConstProcessSeries processSeries) {
    SerialSectionsStartupData startupData = null;
    if (startupDialog == null || (startupData = startupDialog.getStartupData()) == null) {
      if (processSeries != null) {
        processSeries.startFailProcess(axisID);
      }
      return;
    }
    if (getViewType() == ViewType.MONTAGE) {
      // preblend
      if (!FileType.PREBLEND_COMSCRIPT.getFile(this, axisID).exists()) {
        try {
          Utilities.copyFile(FileType.SLOPPY_BLEND_COMSCRIPT,
              FileType.PREBLEND_COMSCRIPT, this, axisID);
        }
        catch (IOException e) {
          e.printStackTrace();
          uiHarness.openMessageDialog(this, uiComponent, "Unable to copy "
              + FileType.SLOPPY_BLEND_COMSCRIPT.getFile(this, axisID).getAbsolutePath()
              + " to " + FileType.PREBLEND_COMSCRIPT.getFileName(this, axisID),
              "Unable to Create Comscripts", axisID);
          if (processSeries != null) {
            processSeries.startFailProcess(axisID);
          }
          return;
        }
      }
      comScriptMgr.loadPreblend(axisID);
      BlendmontParam param = comScriptMgr
          .getBlendmontParamFromPreblend(axisID, getName());
      startupData.getPreblendParameters(param, this);
      comScriptMgr.savePreblend(param, axisID);
      // blend
      if (!FileType.BLEND_COMSCRIPT.getFile(this, axisID).exists()) {
        BaseProcessManager.touch(FileType.BLEND_COMSCRIPT.getFile(this, axisID)
            .getAbsolutePath(), this);
      }
      comScriptMgr.loadBlend(axisID);
      param = comScriptMgr.getBlendmontParamFromBlend(axisID, getName());
      startupData.getBlendParameters(param, this);
      comScriptMgr.saveBlend(param, axisID);
    }
    else {
      // newst
      if (!FileType.NEWST_COMSCRIPT.getFile(this, axisID).exists()) {
        BaseProcessManager.touch(FileType.NEWST_COMSCRIPT.getFile(this, axisID)
            .getAbsolutePath(), this);
      }
      comScriptMgr.loadNewst(axisID);
      NewstParam param = comScriptMgr.getNewstackParam(axisID, getName());
      startupData.getParameters(param, this);
      comScriptMgr.saveNewst(param, axisID);
    }
    if (processSeries != null) {
      processSeries.startNextProcess(axisID);
    }
  }

  private void xftoxg(final ProcessSeries processSeries, final AxisID axisID) {
    if (dialog == null) {
      if (processSeries != null) {
        processSeries.startFailProcess(axisID);
      }
      return;
    }
    autoAlignmentController.copyMostRecentXfFile("Align tab");
    XftoxgParam param = new XftoxgParam(this);
    param.setXfFileName(FileType.LOCAL_TRANSFORMATION_LIST.getFileName(this, axisID));
    param.setXgFileName(FileType.GLOBAL_TRANSFORMATION_LIST.getFileName(this, axisID));
    dialog.getParameters(param);
    String threadName = null;
    try {
      threadName = processMgr.xftoxg(param, axisID, processSeries);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute xftoxg.";
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(this, message, "Unable to execute process", axisID);
      if (processSeries != null) {
        processSeries.startFailProcess(axisID);
      }
      return;
    }
    setThreadName(threadName, axisID);
    mainPanel.startProgressBar(XftoxgParam.COMMAND_NAME, AxisID.ONLY, ProcessName.XFTOXG);
  }

  private void align(final ProcessSeries processSeries, final AxisID axisID) {
    if (dialog == null) {
      if (processSeries != null) {
        processSeries.startFailProcess(axisID);
      }
      return;
    }
    String threadName = null;
    if (metaData.getViewType() == ViewType.MONTAGE) {
      BlendmontParam param = updateBlendComscript(axisID, true);
      if (param == null) {
        if (processSeries != null) {
          processSeries.startFailProcess(axisID);
        }
        return;
      }
      try {
        threadName = processMgr.blend(param, axisID, processSeries);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
        message[1] = e.getMessage();
        uiHarness
            .openMessageDialog(this, message, "Unable to execute com script", axisID);
        if (processSeries != null) {
          processSeries.startFailProcess(axisID);
        }
        return;
      }
    }
    else {
      ConstNewstParam param = updateNewstCom(axisID, true);
      if (param == null) {
        if (processSeries != null) {
          processSeries.startFailProcess(axisID);
        }
        return;
      }
      try {
        threadName = processMgr.newst(param, axisID, processSeries);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
        message[1] = e.getMessage();
        uiHarness
            .openMessageDialog(this, message, "Unable to execute com script", axisID);
        if (processSeries != null) {
          processSeries.startFailProcess(axisID);
        }
        return;
      }
    }
    setThreadName(threadName, axisID);
  }

  private ConstNewstParam updateNewstCom(final AxisID axisID, final boolean doValidation) {
    if (dialog == null) {
      return null;
    }
    comScriptMgr.loadNewst(axisID);
    NewstParam param = null;
    try {
      param = comScriptMgr.getNewstackParam(axisID, getName());
      param.setCommandMode(NewstParam.Mode.FULL_ALIGNED_STACK);
      param.setTransformFile(FileType.GLOBAL_TRANSFORMATION_LIST
          .getFileName(this, axisID));
      if (!dialog.getParameters(param, doValidation)) {
        return null;
      }
      comScriptMgr.saveNewst(param, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(this, errorMessage,
          "Newst Parameter Syntax Error", axisID);
      return null;
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      UIHarness.INSTANCE.openMessageDialog(this, errorMessage,
          "Newst Parameter Syntax Error", axisID);
      return null;
    }
    catch (InvalidParameterException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to update newst com:  " + e.getMessage(),
          "Etomo Error", axisID);
      return null;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to update newst com:  " + e.getMessage(),
          "Etomo Error", axisID);
      return null;
    }
    return param;
  }

  private BlendmontParam updatePreblendComscript(final AxisID axisID,
      final boolean doValidation) {
    comScriptMgr.loadPreblend(axisID);
    BlendmontParam param = comScriptMgr.getBlendmontParamFromPreblend(axisID, getName());
    param.setMode(BlendmontParam.Mode.SERIAL_SECTION_PREBLEND);
    if (!dialog.getPreblendParameters(param, doValidation)) {
      return null;
    }
    param.setBlendmontState(state.getInvalidEdgeFunctions());
    comScriptMgr.savePreblend(param, axisID);
    return param;
  }

  public SerialSectionsState getState() {
    return state;
  }

  private BlendmontParam updateBlendComscript(final AxisID axisID,
      final boolean doValidation) {
    comScriptMgr.loadBlend(axisID);
    BlendmontParam param = comScriptMgr.getBlendmontParamFromBlend(axisID, getName());
    param.setMode(BlendmontParam.Mode.SERIAL_SECTION_BLEND);
    param.setImageInputFile(metaData.getStack());
    param.setTransformFile(FileType.GLOBAL_TRANSFORMATION_LIST.getFileName(this, axisID));
    BlendmontLog log = new BlendmontLog();
    if (log.findUnalignedStartingXandY(FileType.PREBLEND_LOG.getFile(this, axisID))) {
      param.setUnalignedStartingXandY(log.getUnalignedStartingXandY());
    }
    if (!dialog.getBlendParameters(param, doValidation)) {
      return null;
    }
    param.setBlendmontState(state.getInvalidEdgeFunctions());
    comScriptMgr.saveBlend(param, axisID);
    return param;
  }

  /**
   * Copies the distortion field file to the directory containing the stack.  Always tries
   * to start the next process.
   * @param processSeries
   * @param axisID
   * @param dialogType
   */

  private void copyDistortionFieldFile(final ConstProcessSeries processSeries,
      final UIComponent uiComponent, final AxisID axisID) {
    File distortionField = getDistortionField();
    if (distortionField != null) {
      File stack = getStack();
      if (stack != null) {
        try {
          Utilities.copyFile(distortionField, new File(stack.getParentFile(),
              distortionField.getName()));
        }
        catch (IOException e) {
          UIHarness.INSTANCE.openMessageDialog(this, uiComponent, "Unable to copy "
              + distortionField.getAbsolutePath() + ".  Please copy this file by hand.",
              "Unable to Copy File", axisID);
          if (processSeries != null) {
            processSeries.startFailProcess(axisID);
          }
          return;
        }
      }
    }
    if (processSeries != null) {
      processSeries.startNextProcess(axisID);
    }
  }

  /**
   * Attempts to close the startup dialog.
   */
  private void doneStartupDialog(final ConstProcessSeries processSeries,
      final AxisID axisID) {
    startupDialog.done();
    logWindow.show();
    if (processSeries != null) {
      processSeries.startNextProcess(axisID);
    }
  }

  /**
   * Attempts to reset the saved state of dialogType dialog.
   */
  private void resetStartupState(final ConstProcessSeries processSeries,
      final AxisID axisID) {
    if (origUserDir != null) {
      propertyUserDir = origUserDir;
      origUserDir = null;
      System.setProperty("user.dir", propertyUserDir);
    }
    startupDialog.resetSavedState();
    if (processSeries != null) {
      processSeries.startNextProcess(axisID);
    }
  }

  public void imodRaw(final AxisID axisID, final Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.RAW_STACK_KEY, axisID, new File(propertyUserDir,
          metaData.getStack()), menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(),
          "Can't open 3dmod with the tomogram", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
  }

  public void imodPreblend(final AxisID axisID, final Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.PREBLEND_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(),
          "Can't open 3dmod with the tomogram", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
  }

  public void imodPrealign(final AxisID axisID, final Run3dmodMenuOptions menuOptions) {
    if (metaData.getViewType() == ViewType.MONTAGE) {
      imodPreblend(axisID, menuOptions);
    }
    else {
      imodRaw(axisID, menuOptions);
    }
  }

  public void imodAlign(final AxisID axisID, final Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(ImodManager.ALIGNED_STACK_KEY, axisID, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(),
          "Can't open 3dmod with the tomogram", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
  }

  public boolean exitProgram(final AxisID axisID) {
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

  public boolean save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
    saveSerialSectionsDialog(false);
    return true;
  }

  private boolean saveSerialSectionsDialog(final boolean forRun) {
    if (dialog == null) {
      return false;
    }
    if (paramFile == null) {
      if (!setParamFile()) {
        return false;
      }
    }
    dialog.getParameters(metaData, false);
    if (getViewType() == ViewType.MONTAGE) {
      updatePreblendComscript(AXIS_ID, false);
      updateBlendComscript(AXIS_ID, false);
    }
    else {
      updateNewstCom(AXIS_ID, false);
    }
    saveStorables(AXIS_ID);
    return true;
  }

  public String getName() {
    if (loadedParamFile) {
      return metaData.getName();
    }
    String name = null;
    if (startupDialog != null) {
      name = startupDialog.getRootName();
    }
    if (name != null) {
      return name;
    }
    return metaData.getName();
  }

  /**
   * Attempts to get the distortion field file.  Returns null if unable to get the file.
   * @param dialogType
   * @return
   */
  private File getDistortionField() {
    if (loadedParamFile) {
      return new File(metaData.getDistortionField());
    }
    if (startupDialog != null) {
      return startupDialog.getDistortionField();
    }
    return null;
  }

  /**
   * Attempts to get the stack file.  Returns null if unable to get the file.
   * @param dialogType
   * @return
   */
  public File getStack() {
    if (loadedParamFile) {
      return new File(propertyUserDir, metaData.getStack());
    }
    if (startupDialog != null) {
      return startupDialog.getStack();
    }
    return null;
  }

  /**
   * Attempts to get the view type.  Returns null if unable to get it.
   * @param dialogType
   * @return
   */
  public ViewType getViewType() {
    if (loadedParamFile) {
      return metaData.getViewType();
    }
    if (startupDialog != null) {
      return startupDialog.getViewType();
    }
    return null;
  }

  public ConstSerialSectionsMetaData getMetaData() {
    return metaData;
  }

  private void setSerialSectionsDialogParameters() {
    if (loadedParamFile && paramFile != null && metaData.isValid()) {
      dialog.setParameters(metaData);
      if (getViewType() == ViewType.MONTAGE) {
        comScriptMgr.loadPreblend(AXIS_ID);
        BlendmontParam param = comScriptMgr.getBlendmontParamFromPreblend(AXIS_ID,
            getName());
        dialog.setPreblendParameters(param);
        comScriptMgr.loadBlend(AXIS_ID);
        param = comScriptMgr.getBlendmontParamFromBlend(AXIS_ID, getName());
        dialog.setBlendParameters(param);
      }
      else {
        comScriptMgr.loadNewst(AXIS_ID);
        ConstNewstParam param = comScriptMgr.getNewstackParam(AXIS_ID, getName());
        dialog.setParameters(param);
      }
    }
  }

  public void getParameters(final MidasParam param) {
    param.setInputFileName(metaData.getStack());
  }

  public void getAutoAlignmentParameters(final MidasParam param, final AxisID axisID) {
    if (getViewType() == ViewType.MONTAGE) {
      param.setInputFileName(FileType.PREBLEND_OUTPUT_MRC.getFileName(this, axisID));
    }
    else {
      param.setInputFileName(metaData.getStack());
    }
  }

  public void getAutoAlignmentParameters(final XfalignParam param, final AxisID axisID) {
    if (getViewType() == ViewType.MONTAGE) {
      param.setInputFileName(FileType.PREBLEND_OUTPUT_MRC.getFileName(this, axisID));
    }
    else {
      param.setInputFileName(metaData.getStack());
    }
  }

  void createComScriptManager() {
    comScriptMgr = new SerialSectionsComScriptManager(this);
  }

  void createMainPanel() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      mainPanel = new MainSerialSectionsPanel(this);
    }
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.SERIAL_SECTIONS;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  public BaseProcessManager getProcessManager() {
    return processMgr;
  }

  AutoAlignmentMetaData getAutoAlignmentMetaData() {
    return metaData.getAutoAlignmentMetaData();
  }

  public boolean updateMetaData(final DialogType dialogType, final AxisID axisID,
      final boolean doValidation) {
    if (!dialog.getParameters(metaData, doValidation)) {
      return false;
    }
    return true;
  }

  Storable[] getStorables(final int offset) {
    Storable[] storables = new Storable[2 + offset];
    int index = offset;
    storables[index++] = metaData;
    storables[index++] = state;
    return storables;
  }

  public static final class Task implements TaskInterface {
    private static final Task CHANGE_DIRECTORY = new Task();
    private static final Task EXTRACT_PIECES = new Task();
    private static final Task CREATE_COMSCRIPTS = new Task();
    private static final Task COPY_DISTORTION_FIELD_FILE = new Task();
    private static final Task DONE_STARTUP_DIALOG = new Task(true);
    private static final Task RESET_STARTUP_STATE = new Task(true);
    private static final Task XFTOXG = new Task();
    private static final Task ALIGN = new Task();

    private final boolean droppable;

    private Task() {
      this(false);
    }

    private Task(final boolean droppable) {
      this.droppable = droppable;
    }

    public boolean okToDrop() {
      return droppable;
    }
  }
}
