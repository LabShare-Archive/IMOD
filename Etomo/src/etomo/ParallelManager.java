package etomo;

import java.io.File;

import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.ParallelProcessManager;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.ParallelMetaData;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.AbstractParallelDialog;
import etomo.ui.MainPanel;
import etomo.ui.MainParallelPanel;
import etomo.ui.ParallelDialog;
import etomo.ui.UIHarness;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright (c) 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */

public final class ParallelManager extends BaseManager {
  public static final String rcsid = "$Id$";

  private static final AxisID AXIS_ID = AxisID.ONLY;

  private final BaseScreenState screenState = new BaseScreenState(AXIS_ID,
      AxisType.SINGLE_AXIS);

  private final ParallelMetaData metaData;

  private ParallelDialog parallelDialog = null;

  private MainParallelPanel mainPanel;
  private ParallelProcessManager processMgr;

  public ParallelManager() {
    this("");
  }

  public ParallelManager(String paramFileName) {
    super();
    this.metaData = new ParallelMetaData();
    initializeUIParameters(paramFileName, AXIS_ID);
    if (!EtomoDirector.getInstance().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData);
      openParallelDialog();
    }
  }

  public boolean canChangeParamFileName() {
    return false;
  }

  public boolean canSnapshot() {
    return false;
  }

  protected void createComScriptManager() {
  }

  protected void createMainPanel() {
    mainPanel = new MainParallelPanel(this);
  }

  protected void createProcessManager() {
    processMgr = new ParallelProcessManager(this);
  }

  protected void createProcessTrack() {
  }

  protected void createState() {
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public final BaseScreenState getBaseScreenState(AxisID axisID) {
    return screenState;
  }

  protected BaseState getBaseState() {
    return null;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  protected final Storable[] getParamFileStorableArray(boolean includeMetaData) {
    int arraySize = 2;
    if (!includeMetaData) {
      arraySize--;
    }
    Storable[] storable = new Storable[arraySize];
    int index = 0;
    if (includeMetaData) {
      storable[index++] = metaData;
    }
    storable[index++] = screenState;
    return storable;
  }

  protected BaseProcessManager getProcessManager() {
    return processMgr;
  }

  protected BaseProcessTrack getProcessTrack() {
    return null;
  }

  protected void getProcessTrack(Storable[] storable, int index) {
  }

  public boolean isNewManager() {
    return true;
  }

  public void kill(AxisID axisID) {
    processMgr.kill(axisID);
  }

  public void pause(AxisID axisID) {
    processMgr.pause(axisID);
  }

  public boolean save(AxisID axisID) {
    mainPanel.done();
    saveDialog();
    return saveParamFile(axisID);
  }
  
  public boolean exitProgram(AxisID axisID) {
    try {
      if (super.exitProgram(axisID)) {
        save(axisID);
        return true;
      }
      return false;
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
  }

  protected void setMetaData(ImodManager imodManager) {
  }

  public void setParamFile(File paramFile) {
    this.paramFile = paramFile;
  }

  protected void startNextProcess(AxisID axisID, String nextProcess,
      ProcessResultDisplay processResultDisplay) {
  }

  public void touch(File file) {
    processMgr.touch(file);
  }

  public String getName() {
    return metaData.getName();
  }

  protected void updateDialog(ProcessName processName, AxisID axisID) {
  }

  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
  }

  private void openParallelDialog() {
    if (parallelDialog == null) {
      parallelDialog = new ParallelDialog(this, AXIS_ID);
    }
    mainPanel.setParallelDialog(AXIS_ID, parallelDialog.isParallel());
    parallelDialog.setParameters(screenState);
    if (paramFile != null && metaData.isValid()) {
      parallelDialog.setParameters(metaData);
      parallelDialog.updateDisplay(false);
    }
    mainPanel.showProcess(parallelDialog.getContainer(), AXIS_ID);
  }

  private void saveParallelDialog() {
    if (parallelDialog == null) {
      return;
    }
    parallelDialog.getParameters(screenState);
  }

  /**
   * set the paramFile and change the state, if necessary
   * run BaseManager.processchunks
   */
  public final void processchunks(AxisID axisID, AbstractParallelDialog dialog,
      ProcessResultDisplay processResultDisplay) {
    if (parallelDialog == null) {
      return;
    }
    if (paramFile == null) {
      if (!setParamFile()) {
        return;
      }
      parallelDialog.updateDisplay(false);
    }
    super.processchunks(axisID, dialog, processResultDisplay);
  }

  private boolean setParamFile() {
    //set paramFile and propertyUserDir
    propertyUserDir = parallelDialog.getWorkingDir().getAbsolutePath();
    System.err.println("propertyUserDir: " + propertyUserDir);
    parallelDialog.getParameters(metaData);
    String errorMessage = metaData.validate();
    if (errorMessage != null) {
      UIHarness.INSTANCE.openMessageDialog(errorMessage,
          "Parallel Process error", AXIS_ID);
      return false;
    }
    setParamFile(new File(propertyUserDir, metaData.getMetaDataFileName()));
    System.err.println("paramFile: " + paramFile);
    EtomoDirector.getInstance().renameCurrentManager(metaData.getRootName());
    mainPanel.setStatusBarText(paramFile, metaData);
    return true;
  }

  private void saveDialog() {
    saveParallelDialog();
  }
}
/**
 * <p> $Log$ </p>
 */