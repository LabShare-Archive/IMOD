package etomo;

import java.io.File;
import java.io.IOException;

import etomo.comscript.BaseComScriptManager;
import etomo.comscript.ExtractpiecesParam;
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
import etomo.type.ConstSerialSectionsMetaData;
import etomo.type.DialogType;
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

  private BaseComScriptManager comScriptMgr = null;
  private SerialSectionsStartupDialog startupDialog = null;
  private SerialSectionsDialog dialog = null;
  private AutoAlignmentController autoAlignmentController = null;
  /**
   * valid is for handling failure before the manager key is set in EtomoDirector.
   */
  private boolean valid = true;

  private MainSerialSectionsPanel mainPanel;

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

  void startNextProcess(final AxisID axisID, final ProcessSeries.Process process,
      final ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      final DialogType dialogType, final ProcessDisplay display) {
    if (process.equals(Task.COPY_DISTORTION_FIELD_FILE)) {
      copyDistortionFieldFile(processSeries, axisID, dialogType);
    }
    else if (process.equals(Task.CLOSE_DIALOG)) {
      closeDialog(dialogType);
    }
    else if (process.equals(Task.RESET_SAVED_STATE_DIALOG)) {
      resetSavedStateDialog(dialogType);
    }
  }

  /**
   * Sets the next and last process and starts the extractpiecelist process.  If the
   * extractpiecelist did not have to be run,
   * runs the next process.
   * @param axisID
   * @param processResultDisplay
   * @param dialogType
   * @param startupData
   */
  public boolean extractpieces(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay, final DialogType dialogType,
      final SerialSectionsStartupData startupData) {
    ProcessSeries processSeries = new ProcessSeries(this, dialogType);
    processSeries.setNextProcess(Task.COPY_DISTORTION_FIELD_FILE);
    processSeries.setFailProcess(Task.RESET_SAVED_STATE_DIALOG);
    processSeries.setLastProcess(Task.CLOSE_DIALOG);
    if (startupData.getViewType() == ViewType.MONTAGE) {
      File pieceListFile = DatasetFiles.getPieceListFile(this, axisID);
      if (!pieceListFile.exists()) {
        ExtractpiecesParam param = new ExtractpiecesParam(startupData.getStack()
            .getName(), startupData.getRootName(), AxisType.SINGLE_AXIS, this, axisID);
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
          return false;
        }
        setThreadName(threadName, axisID);
        getMainPanel().startProgressBar("Running " + ExtractpiecesParam.COMMAND_NAME,
            axisID, ProcessName.EXTRACTPIECES);
        return true;
      }
    }
    // extractpiecelist did not run and there was no failure.
    processSeries.startNextProcess(axisID, processResultDisplay);
    return true;
  }

  /**
   * Copies the distortion field file to the directory containing the stack.  Always runs 
   * the next process.
   * @param processSeries
   * @param axisID
   * @param dialogType
   */
  public void copyDistortionFieldFile(final ProcessSeries processSeries,
      final AxisID axisID, final DialogType dialogType) {
    File distortionField = getDistortionField(dialogType);
    if (distortionField != null) {
      File stack = getStack(dialogType);
      if (stack != null) {
        try {
          Utilities.copyFile(distortionField, new File(stack.getParentFile(),
              distortionField.getName()));
        }
        catch (IOException e) {
          UIHarness.INSTANCE.openMessageDialog(this,
              "Unable to copy " + distortionField.getAbsolutePath()
                  + ".  Please copy this file by hand.", "Unable to Copy File", axisID);
        }
      }
    }
    if (processSeries != null) {
      processSeries.startNextProcess(axisID, null);
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

  /**
   * Attempts to get the distortion field file from the dialogType dialog.  Returns null
   * if unable to get the file.
   * @param dialogType
   * @return
   */
  private File getDistortionField(final DialogType dialogType) {
    if (dialogType != DialogType.SERIAL_SECTIONS_STARTUP || startupDialog == null) {
      return null;
    }
    return startupDialog.getDistortionField();
  }

  /**
   * Attempts to get the stack file from the dialogType dialog.  Returns null if unable to
   * get the file.  Currently only gets the stack from the startup dialog.
   * @param dialogType
   * @return
   */
  private File getStack(final DialogType dialogType) {
    if (dialogType != DialogType.SERIAL_SECTIONS_STARTUP || startupDialog == null) {
      return null;
    }
    return startupDialog.getStack();
  }

  /**
   * Attempts to close the dialogType dialog.  Currently only closes the startup dialog.
   * @param dialogType
   */
  private void closeDialog(final DialogType dialogType) {
    if (dialogType == DialogType.SERIAL_SECTIONS_STARTUP) {
      startupDialog.close();
    }
  }
  
  public ConstSerialSectionsMetaData getMetaData() {
    return metaData;
  }

  /**
   * Attempts to reset the saved state of dialogType dialog.  Currently only does this for
   * startup dialog.
   * @param dialogType
   */
  private void resetSavedStateDialog(final DialogType dialogType) {
    if (dialogType == DialogType.SERIAL_SECTIONS_STARTUP) {
      startupDialog.resetSavedState();
    }
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

  public String getName() {
    return metaData.getName();
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
    private static final Task COPY_DISTORTION_FIELD_FILE = new Task();
    public static final Task CLOSE_DIALOG = new Task();
    private static final Task RESET_SAVED_STATE_DIALOG = new Task();

    private Task() {
    }
  }
}
