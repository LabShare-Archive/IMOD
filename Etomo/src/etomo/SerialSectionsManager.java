package etomo;

import java.io.File;
import java.io.IOException;

import etomo.comscript.BlendmontParam;
import etomo.comscript.ExtractpiecesParam;
import etomo.comscript.NewstParam;
import etomo.comscript.SerialSectionsComScriptManager;
import etomo.logic.SerialSectionsStartupData;
import etomo.process.BaseProcessManager;
import etomo.process.SerialSectionsProcessManager;
import etomo.process.SystemProcessException;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AutoAlignmentMetaData;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.ConstProcessSeries;
import etomo.type.ConstSerialSectionsMetaData;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.InterfaceType;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.SerialSectionsMetaData;
import etomo.type.ViewType;
import etomo.ui.swing.LogInterface;
import etomo.ui.swing.LogPanel;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.MainSerialSectionsPanel;
import etomo.ui.swing.ProcessDisplay;
import etomo.ui.swing.SerialSectionsDialog;
import etomo.ui.swing.SerialSectionsStartupDialog;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;
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

  private final LogPanel logPanel = LogPanel.getInstance(this);

  private final SerialSectionsMetaData metaData;
  private final SerialSectionsProcessManager processMgr;

  private SerialSectionsStartupDialog startupDialog = null;
  private SerialSectionsDialog dialog = null;
  private AutoAlignmentController autoAlignmentController = null;
  /**
   * valid is for handling failure before the manager key is set in EtomoDirector.
   */
  private boolean valid = true;

  //Initialized during parent constructor
  private MainSerialSectionsPanel mainPanel;
  private SerialSectionsComScriptManager comScriptMgr;

  private SerialSectionsManager() {
    this("");
  }

  SerialSectionsManager(final String paramFileName) {
    super();
    metaData = new SerialSectionsMetaData();
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
      mainPanel.setStatusBarText(paramFile, metaData, logPanel);
      if (loadedParamFile) {
        openSerialSectionsDialog(null);
      }
      else {
        openSerialSectionsStartupDialog();
      }
    }
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
    mainPanel.setStatusBarText(paramFile, metaData, logPanel);
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
    autoAlignmentController = new AutoAlignmentController(this, dialog);
    dialog.setAutoAlignmentController(autoAlignmentController);
    setSerialSectionsDialogParameters();
    mainPanel.showProcess(dialog.getRootContainer(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(
        DialogType.SERIAL_SECTIONS, AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  public void startStartupProcessSeries(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay) {
    ProcessSeries processSeries = new ProcessSeries(this,
        DialogType.SERIAL_SECTIONS_STARTUP);
    processSeries.setNextProcess(Task.EXTRACT_PIECES);
    processSeries.addProcess(Task.CREATE_COMSCRIPTS);
    processSeries.addProcess(Task.COPY_DISTORTION_FIELD_FILE, true);
    processSeries.setLastProcess(Task.CLOSE_DIALOG);
    processSeries.setFailProcess(Task.RESET_SAVED_STATE_DIALOG);
    processSeries.startNextProcess(axisID, processResultDisplay);
  }

  void startNextProcess(final AxisID axisID, final ProcessSeries.Process process,
      final ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      final DialogType dialogType, final ProcessDisplay display) {
    if (process.equals(Task.EXTRACT_PIECES)) {
      extractpieces(axisID, processResultDisplay, processSeries, dialogType);
    }
    else if (process.equals(Task.CREATE_COMSCRIPTS)) {
      createComscripts(axisID, processSeries, processResultDisplay);
    }
    else if (process.equals(Task.COPY_DISTORTION_FIELD_FILE)) {
      copyDistortionFieldFile(processSeries, axisID, processResultDisplay);
    }
    else if (process.equals(Task.CLOSE_DIALOG)) {
      closeDialog(dialogType, processSeries, axisID, processResultDisplay);
    }
    if (process.equals(Task.RESET_SAVED_STATE_DIALOG)) {
      resetSavedStateDialog(dialogType, processSeries, axisID, processResultDisplay);
    }
  }

  /**
   * Runs extractpieces.  Starts a failure process if it fails.  Starts the next process
   * if it didn't spawn a process thread.
   * @param axisID
   * @param processResultDisplay
   * @param dialogType
   * @param startupData
   */
  private void extractpieces(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      final DialogType dialogType) {
    ViewType viewType = getViewType();
    if (viewType == null) {
      if (processSeries != null) {
        processSeries.startFailProcess(axisID, processResultDisplay);
      }
      return;
    }
    if (viewType == ViewType.MONTAGE) {
      File pieceListFile = DatasetFiles.getPieceListFile(this, axisID);
      if (!pieceListFile.exists()) {
        ExtractpiecesParam param = new ExtractpiecesParam(getStack().getName(),
            getName(), AxisType.SINGLE_AXIS, this, axisID);
        String threadName;
        try {
          threadName = processMgr.extractpieces(param, axisID, processResultDisplay,
              processSeries);
        }
        catch (SystemProcessException e) {
          e.printStackTrace();
          String[] message = new String[2];
          message[0] = "Can not execute " + ExtractpiecesParam.COMMAND_NAME;
          message[1] = e.getMessage();
          uiHarness.openMessageDialog(this, message, "Unable to execute command", axisID);
          if (processSeries != null) {
            processSeries.startFailProcess(axisID, processResultDisplay);
          }
          return;
        }
        setThreadName(threadName, axisID);
        getMainPanel().startProgressBar("Running " + ExtractpiecesParam.COMMAND_NAME,
            axisID, ProcessName.EXTRACTPIECES);
      }
    }
    else if (processSeries != null) {
      processSeries.startNextProcess(axisID, processResultDisplay);
    }
  }

  /**
   * Creates blend or newst comscripts.
   * @param axisID
   * @param processSeries
   * @param processResultDisplay
   */
  private void createComscripts(final AxisID axisID,
      final ConstProcessSeries processSeries,
      final ProcessResultDisplay processResultDisplay) {
    SerialSectionsStartupData startupData = null;
    if (startupDialog == null || (startupData = startupDialog.getStartupData()) == null) {
      if (processSeries != null) {
        processSeries.startFailProcess(axisID, processResultDisplay);
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
          uiHarness.openMessageDialog(this, "Unable to copy "
              + FileType.SLOPPY_BLEND_COMSCRIPT.getFile(this, axisID).getAbsolutePath()
              + " to " + FileType.PREBLEND_COMSCRIPT.getFileName(this, axisID),
              "Unable to Create Comscripts", axisID);
          if (processSeries != null) {
            processSeries.startFailProcess(axisID, processResultDisplay);
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
      processSeries.startNextProcess(axisID, processResultDisplay);
    }
  }

  /**
   * Copies the distortion field file to the directory containing the stack.  Always tries
   * to start the next process.
   * @param processSeries
   * @param axisID
   * @param dialogType
   */
  private void copyDistortionFieldFile(final ProcessSeries processSeries,
      final AxisID axisID, final ProcessResultDisplay processResultDisplay) {
    File distortionField = getDistortionField();
    if (distortionField != null) {
      File stack = getStack();
      if (stack != null) {
        try {
          Utilities.copyFile(distortionField, new File(stack.getParentFile(),
              distortionField.getName()));
        }
        catch (IOException e) {
          UIHarness.INSTANCE.openMessageDialog(this,
              "Unable to copy " + distortionField.getAbsolutePath()
                  + ".  Please copy this file by hand.", "Unable to Copy File", axisID);
          if (processSeries != null) {
            processSeries.startFailProcess(axisID, processResultDisplay);
          }
          return;
        }
      }
    }
    if (processSeries != null) {
      processSeries.startNextProcess(axisID, processResultDisplay);
    }
  }

  /**
   * Attempts to close the dialogType dialog.  Currently only closes the startup dialog.
   * Tries to start the next process
   * @param dialogType
   */
  private void closeDialog(final DialogType dialogType,
      final ProcessSeries processSeries, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay) {
    if (dialogType == DialogType.SERIAL_SECTIONS_STARTUP) {
      startupDialog.close();
      if (processSeries != null) {
        processSeries.startNextProcess(axisID, processResultDisplay);
      }
    }
    else if (processSeries != null) {
      processSeries.startFailProcess(axisID, processResultDisplay);
    }
  }

  /**
   * Attempts to reset the saved state of dialogType dialog.  Currently only does this for
   * startup dialog.
   * @param dialogType
   */
  private void resetSavedStateDialog(final DialogType dialogType,
      final ProcessSeries processSeries, final AxisID axisID,
      final ProcessResultDisplay processResultDisplay) {
    if (dialogType == DialogType.SERIAL_SECTIONS_STARTUP) {
      startupDialog.resetSavedState();
      if (processSeries != null) {
        processSeries.startNextProcess(axisID, processResultDisplay);
      }
    }
    else if (processSeries != null) {
      processSeries.startFailProcess(axisID, processResultDisplay);
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

  public void save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
    saveSerialSectionsDialog(false);
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
    dialog.getParameters(metaData);
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
  private File getStack() {
    if (loadedParamFile) {
      return new File(metaData.getStack());
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
  private ViewType getViewType() {
    if (loadedParamFile) {
      return ViewType.fromString(metaData.getViewType());
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
    }
  }

  void createComScriptManager() {
    comScriptMgr = new SerialSectionsComScriptManager(this);
  }

  void createMainPanel() {
    mainPanel = new MainSerialSectionsPanel(this);
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.SERIAL_SECTIONS;
  }

  public LogInterface getLogInterface() {
    return logPanel;
  }

  public LogPanel getLogPanel() {
    return logPanel;
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

  public boolean updateMetaData(final DialogType dialogType, final AxisID axisID) {
    return dialog.getParameters(metaData);
  }

  Storable[] getStorables(final int offset) {
    Storable[] storables = new Storable[1 + offset];
    int index = offset;
    storables[index++] = metaData;
    return storables;
  }

  public static final class Task implements TaskInterface {
    private static final Task EXTRACT_PIECES = new Task(false);
    private static final Task CREATE_COMSCRIPTS = new Task(false);
    private static final Task COPY_DISTORTION_FIELD_FILE = new Task(false);
    private static final Task CLOSE_DIALOG = new Task(true);
    private static final Task RESET_SAVED_STATE_DIALOG = new Task(true);

    private final boolean droppable;

    private Task(final boolean droppable) {
      this.droppable = droppable;
    }

    public boolean okToDrop() {
      return droppable;
    }
  }
}
