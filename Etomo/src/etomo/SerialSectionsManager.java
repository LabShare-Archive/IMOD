package etomo;

import etomo.comscript.BaseComScriptManager;
import etomo.comscript.SerialSectionsComScriptManager;
import etomo.logic.SerialSectionsStartupData;
import etomo.process.BaseProcessManager;
import etomo.process.SerialSectionsProcessManager;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.DialogType;
import etomo.type.InterfaceType;
import etomo.type.SerialSectionsMetaData;
import etomo.ui.swing.LogInterface;
import etomo.ui.swing.LogPanel;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.MainSerialSectionsPanel;
import etomo.ui.swing.SerialSectionsDialog;
import etomo.ui.swing.SerialSectionsStartupDialog;
import etomo.ui.swing.UIHarness;
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
    reconnect(processMgr.getSavedProcessData(AXIS_ID), AXIS_ID, true);
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
      serialSectionsDialog = SerialSectionsDialog.getInstance(this, AXIS_ID);
    }
    setSerialSectionsDialogParameters();
    mainPanel.showProcess(serialSectionsDialog.getContainer(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(
        DialogType.SERIAL_SECTIONS, AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }
  
  private void setSerialSectionsDialogParameters() {
    if (loadedParamFile&&paramFile != null && metaData.isValid()) {
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

  Storable[] getStorables(final int offset) {
    Storable[] storables = new Storable[3 + offset];
    int index = offset;
    storables[index++] = metaData;
    return storables;
  }
}
