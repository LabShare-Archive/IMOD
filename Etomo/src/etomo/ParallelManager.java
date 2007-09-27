package etomo;

import java.io.File;
import java.io.IOException;

import etomo.comscript.ProcesschunksParam;
import etomo.process.BaseProcessManager;
import etomo.process.ParallelProcessManager;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.InterfaceType;
import etomo.type.ParallelMetaData;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.AbstractParallelDialog;
import etomo.ui.MainPanel;
import etomo.ui.MainParallelPanel;
import etomo.ui.ParallelDialog;
import etomo.ui.ParallelPanel;
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
    createState();
    processMgr = new ParallelProcessManager(this);
    initializeUIParameters(paramFileName, AXIS_ID);
    if (!EtomoDirector.INSTANCE.isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData);
      openParallelDialog();
    }
  }
  
  public boolean setParamFile() {
    return loadedParamFile;
  }
  
  public InterfaceType getInterfaceType() {
    return InterfaceType.PP;
  }

  public boolean canChangeParamFileName() {
    return false;
  }

  public boolean canSnapshot() {
    return false;
  }

  protected void createComScriptManager() {
  }

  protected void processSucceeded(AxisID axisID, ProcessName processName) {
  }

  protected void createMainPanel() {
    mainPanel = new MainParallelPanel(this);
  }

  protected void createProcessTrack() {
  }

  private void createState() {
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public final BaseScreenState getBaseScreenState(AxisID axisID) {
    return screenState;
  }

  public BaseState getBaseState() {
    return null;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  protected final Storable[] getStorables(int offset) {
    Storable[] storables = new Storable[2 + offset];
    int index = offset;
    storables[index++] = metaData;
    storables[index++] = screenState;
    return storables;
  }

  protected BaseProcessManager getProcessManager() {
    return processMgr;
  }

  protected BaseProcessTrack getProcessTrack() {
    return null;
  }

  protected void getProcessTrack(Storable[] storable, int index) {
  }

  public void kill(AxisID axisID) {
    processMgr.kill(axisID);
  }

  public void pause(AxisID axisID) {
    processMgr.pause(axisID);
  }

  public void save(AxisID axisID) throws IOException {
    mainPanel.done();
    saveDialog();
  }

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

  public void setParamFile(File paramFile) {
    this.paramFile = paramFile;
  }

  protected void startNextProcess(AxisID axisID, String nextProcess,
      ProcessResultDisplay processResultDisplay) {
  }

  public void touch(String absolutePath) {
    processMgr.touch(absolutePath);
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
    mainPanel.setParallelDialog(AXIS_ID, parallelDialog
        .usingParallelProcessing());
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
      if (!setNewParamFile()) {
        return;
      }
      parallelDialog.updateDisplay(false);
    }
    sendMsgProcessStarting(processResultDisplay);
    if (dialog == null) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, axisID);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    dialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    parallelPanel.getParallelProgressDisplay().resetResults();
    processchunks(axisID, param, processResultDisplay);
  }

  private boolean setNewParamFile() {
    if (loadedParamFile) {
      return true;
    }
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
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getRootName());
    mainPanel.setStatusBarText(paramFile, metaData);
    return true;
  }

  private void saveDialog() {
    saveParallelDialog();
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.16  2007/09/07 00:16:30  sueh
 * <p> bug# 989 Using a public INSTANCE for EtomoDirector instead of getInstance
 * <p> and createInstance.
 * <p>
 * <p> Revision 1.15  2007/08/29 21:33:21  sueh
 * <p> bug# 1041 Made getBaseState public.
 * <p>
 * <p> Revision 1.14  2007/06/08 21:50:42  sueh
 * <p> bug# 1014 Removed setMetaData(ImodManager).
 * <p>
 * <p> Revision 1.13  2007/05/21 22:28:23  sueh
 * <p> bug# 964 Added getInterfaceType().
 * <p>
 * <p> Revision 1.12  2007/04/09 19:30:19  sueh
 * <p> bug# 964 Added setParamFile(), which just returns loadedParamFile
 * <p>
 * <p> Revision 1.11  2007/02/19 21:50:10  sueh
 * <p> bug# 964 Removed isNewManager() because it is only used by Application
 * <p> Manager.
 * <p>
 * <p> Revision 1.10  2007/02/05 21:27:59  sueh
 * <p> bug# 962 Creating process manager later.
 * <p>
 * <p> Revision 1.9  2006/11/28 22:48:48  sueh
 * <p> bug# 934 Changed BaseManager.stop() to endThreads().
 * <p>
 * <p> Revision 1.8  2006/11/15 18:49:00  sueh
 * <p> bug# 872 Changed getParamFileStorableArray to getStorables.  Letting the base
 * <p> save param file function call save().  getStorables always gets all the storables
 * <p> (including meta data) each time, to make it simpler.
 * <p>
 * <p> Revision 1.7  2006/10/16 22:36:32  sueh
 * <p> bug# 919  Changed touch(File) to touch(String absolutePath).
 * <p>
 * <p> Revision 1.6  2006/09/13 23:07:23  sueh
 * <p> bug# 920 Moving BaseManager.createState() call to child classes, so it can be
 * <p> done after meta data is created.
 * <p>
 * <p> Revision 1.5  2006/07/28 19:43:45  sueh
 * <p> bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p> usingParallelProcessing.
 * <p>
 * <p> Revision 1.4  2006/07/26 16:33:55  sueh
 * <p> bug# 868 Temporarily moving part of processchunks to the specific manager.
 * <p> Eventually the process side of processchunks will be in UIExpert.
 * <p>
 * <p> Revision 1.3  2006/07/19 20:05:39  sueh
 * <p> bug# 902 Added processSucceeded().
 * <p>
 * <p> Revision 1.2  2006/06/05 16:01:30  sueh
 * <p> bug# 766 getParamFileStorableArray():  Add the option have elements in the storable array that aer set by the base manager.
 * <p>
 * <p> Revision 1.1  2006/03/20 17:50:19  sueh
 * <p> bug# 835 Manager (at the same level as ApplicationManager and
 * <p> JoinManager) to manage generic parallel processes.
 * <p> </p>
 */
