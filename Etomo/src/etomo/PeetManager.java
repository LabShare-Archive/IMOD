package etomo;

import java.io.File;

import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.PeetProcessManager;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.PeetMetaData;
import etomo.type.PeetScreenState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.MainPanel;
import etomo.ui.MainPeetPanel;
import etomo.ui.PeetDialog;
import etomo.util.DatasetFiles;

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
 * 
 * <p> $Log$
 * <p> Revision 1.4  2007/02/21 22:29:04  sueh
 * <p> bug# 964 Getting save on exit to work.
 * <p>
 * <p> Revision 1.3  2007/02/21 04:16:53  sueh
 * <p> bug# 964 Initializing parameters when the param file is chosen.
 * <p>
 * <p> Revision 1.2  2007/02/20 20:34:50  sueh
 * <p> bug# 964 Added setName to set propertyUserDir and update the display.
 * <p>
 * <p> Revision 1.1  2007/02/19 21:50:49  sueh
 * <p> bug# 964 Manager for the PEET interface.
 * <p> </p>
 */
public class PeetManager extends BaseManager {
  public static final String rcsid = "$Id$";

  private static final AxisID AXIS_ID = AxisID.ONLY;
  private final PeetScreenState screenState = new PeetScreenState(AXIS_ID,
      AxisType.SINGLE_AXIS);
  private final PeetMetaData metaData;
  private final PeetProcessManager processMgr;
  private PeetDialog peetDialog = null;
  private MainPeetPanel mainPanel;

  public boolean canChangeParamFileName() {
    return !loadedParamFile;
  }

  public boolean canSnapshot() {
    return false;
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public final BaseScreenState getBaseScreenState(AxisID axisID) {
    return screenState;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  public String getName() {
    return metaData.getName();
  }

  public void kill(AxisID axisID) {
    processMgr.kill(axisID);
  }

  public void pause(AxisID axisID) {
    processMgr.pause(axisID);
  }

  public void setParamFile(File paramFile) {
    if (!paramFile.exists()) {
      touch(paramFile.getAbsolutePath());
    }
    initializeUIParameters(paramFile, AXIS_ID);
    if (loadedParamFile) {
      String rootName = DatasetFiles.getRootName(paramFile);
      metaData.setName(rootName);
      if (peetDialog != null) {
        peetDialog.setDirectory(paramFile.getParent());
        peetDialog.setOutput(rootName);
        peetDialog.updateDisplay(true);
      }
    }
  }

  public void touch(String absolutePath) {
    processMgr.touch(absolutePath);
    try {
      Thread.sleep(15);
    }
    catch (InterruptedException e) {
    }
  }

  /**
   * Tries to set paramFile.  Returns true if able to set paramFile.
   * If paramFile is already set, returns true.  Returns false if unable
   * to set paramFile.  Updates the peet dialog display if paramFile
   * was set successfully.
   * @return
   */
  public boolean setParamFile() {
    if (loadedParamFile) {
      return true;
    }
    if (peetDialog == null) {
      return false;
    }
    peetDialog.getParameters(metaData);
    if (!metaData.isValid()) {
      return false;
    }
    File paramFile = new File(peetDialog.getDirectory(), metaData.getName()
        + DatasetFiles.PEET_DATA_FILE_EXT);
    if (!paramFile.exists()) {
      touch(paramFile.getAbsolutePath());
    }
    initializeUIParameters(paramFile, AXIS_ID);
    if (!loadedParamFile) {
      return false;
    }
    peetDialog.updateDisplay(true);
    return true;
  }

  /**
   * Call BaseManager.exitProgram(). Call savePeetDialog. Return the value of
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

  public void save() throws LogFile.FileException, LogFile.WriteException {
    super.save();
    mainPanel.done();
    savePeetDialog();
  }

  protected void updateDialog(ProcessName processName, AxisID axisID) {
  }

  protected void startNextProcess(AxisID axisID, String nextProcess,
      ProcessResultDisplay processResultDisplay) {
  }

  protected void setMetaData(ImodManager imodManager) {
  }

  protected void processSucceeded(AxisID axisID, ProcessName processName) {
  }

  protected BaseState getBaseState() {
    return null;
  }

  protected void createProcessTrack() {
  }

  protected void createMainPanel() {
    mainPanel = new MainPeetPanel(this);
  }

  protected void createComScriptManager() {
  }

  protected BaseProcessManager getProcessManager() {
    return processMgr;
  }

  protected BaseProcessTrack getProcessTrack() {
    return null;
  }

  protected void getProcessTrack(Storable[] storable, int index) {
  }

  protected final Storable[] getStorables(int offset) {
    Storable[] storables = new Storable[2 + offset];
    int index = offset;
    storables[index++] = metaData;
    storables[index++] = screenState;
    return storables;
  }

  PeetManager() {
    this("");
  }

  PeetManager(String paramFileName) {
    super();
    this.metaData = new PeetMetaData();
    createState();
    processMgr = new PeetProcessManager(this);
    initializeUIParameters(paramFileName, AXIS_ID);
    if (!EtomoDirector.getInstance().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData);
      openPeetDialog();
    }
  }

  private void createState() {
  }

  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
  }

  /**
   * Create (if necessary) and show the peet dialog.  Update data if the param
   * file has been set.
   */
  private void openPeetDialog() {
    if (peetDialog == null) {
      peetDialog = new PeetDialog(this, AXIS_ID);
    }
    mainPanel.setParallelDialog(AXIS_ID, peetDialog.usingParallelProcessing());
    if (paramFile != null && metaData.isValid()) {
      peetDialog.setParameters(metaData);
      peetDialog.setParameters(screenState);
      peetDialog.setDirectory(propertyUserDir);
      peetDialog.updateDisplay(loadedParamFile);
    }
    mainPanel.showProcess(peetDialog.getContainer(), AXIS_ID);
  }

  private void savePeetDialog() {
    if (peetDialog == null) {
      return;
    }
    if (paramFile == null) {
      if (!setParamFile()) {
        return;
      }
    }
    peetDialog.getParameters(metaData);
    peetDialog.getParameters(screenState);
    saveStorables(AXIS_ID);
  }
}
