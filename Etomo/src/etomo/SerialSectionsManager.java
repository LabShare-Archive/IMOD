package etomo;

import java.io.File;

import etomo.comscript.BaseComScriptManager;
import etomo.comscript.ExtractpiecesParam;
import etomo.comscript.SerialSectionsComScriptManager;
import etomo.logic.SerialSectionsStartupData;
import etomo.process.BaseProcessManager;
import etomo.process.SerialSectionsProcessManager;
import etomo.process.SystemProcessException;
import etomo.storage.Storable;
import etomo.type.AutoAlignmentMetaData;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
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
  private SerialSectionsStartupDialog serialSectionsStartupDialog = null;
  private SerialSectionsDialog serialSectionsDialog = null;
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
    if (serialSectionsStartupDialog != null) {
      serialSectionsStartupDialog.display();
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
    if (serialSectionsStartupDialog == null) {
      String actionMessage = Utilities.prepareDialogActionMessage(
          DialogType.SERIAL_SECTIONS_STARTUP, AxisID.ONLY, null);
      mainPanel.setStaticProgressBar("Starting Serial Sections interface", AXIS_ID);
      serialSectionsStartupDialog = SerialSectionsStartupDialog
          .getInstance(this, AXIS_ID);
      if (actionMessage != null) {
        System.err.println(actionMessage);
      }
    }
  }

  public void setStartupData(final SerialSectionsStartupData startupData) {
    serialSectionsStartupDialog = null;
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
    if (serialSectionsDialog == null) {
      serialSectionsDialog = SerialSectionsDialog.getInstance(this, AXIS_ID, startupData);
    }
    autoAlignmentController = new AutoAlignmentController(this, serialSectionsDialog);
    serialSectionsDialog.setAutoAlignmentController(autoAlignmentController);
    setSerialSectionsDialogParameters();
    mainPanel.showProcess(serialSectionsDialog.getRootContainer(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(
        DialogType.SERIAL_SECTIONS, AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  public void extractpieces(final AxisID axisID,
      final ProcessResultDisplay processResultDisplay, final DialogType dialogType,
      final SerialSectionsStartupData startupData) {
    if (startupData.getViewType() != ViewType.MONTAGE) {
      return;
    }
    File pieceListFile = DatasetFiles.getPieceListFile(this, axisID);
    if (pieceListFile.exists()) {
      return;
    }
    ExtractpiecesParam param = new ExtractpiecesParam(startupData.getStack().getName(),
        startupData.getRootName(), AxisType.SINGLE_AXIS, this, axisID);
    String threadName;
    try {
      threadName = processMgr.extractpieces(param, axisID, processResultDisplay);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ExtractpiecesParam.COMMAND_NAME;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(this, message, "Unable to execute command", axisID);
      return;
    }
    setThreadName(threadName, axisID);
    getMainPanel().startProgressBar("Running " + ExtractpiecesParam.COMMAND_NAME, axisID,
        ProcessName.EXTRACTPIECES);
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
    return serialSectionsDialog.getMetaData(metaData);
  }

  Storable[] getStorables(final int offset) {
    Storable[] storables = new Storable[1 + offset];
    int index = offset;
    storables[index++] = metaData;
    return storables;
  }
}
