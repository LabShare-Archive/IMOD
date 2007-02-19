package etomo;

import java.io.File;

import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.PeetProcessManager;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.PeetMetaData;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.MainPanel;
import etomo.ui.MainPeetPanel;
import etomo.ui.PeetDialog;

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
 * <p> $Log$ </p>
 */
public class PeetManager extends BaseManager {
  public static final String rcsid = "$Id$";

  private static final AxisID AXIS_ID = AxisID.ONLY;
  private final BaseScreenState screenState = new BaseScreenState(AXIS_ID,
      AxisType.SINGLE_AXIS);
  private final PeetMetaData metaData;
  private final PeetProcessManager processMgr;
  private MainPeetPanel mainPanel;
  private PeetDialog peetDialog = null;

  public boolean canChangeParamFileName() {
    return false;
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
    this.paramFile = paramFile;
  }

  public void touch(String absolutePath) {
    processMgr.touch(absolutePath);
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

  private void openPeetDialog() {
    if (peetDialog == null) {
      peetDialog = new PeetDialog(this, AXIS_ID);
    }
    mainPanel.setParallelDialog(AXIS_ID, peetDialog.usingParallelProcessing());
    if (paramFile != null && metaData.isValid()) {
      peetDialog.setParameters(metaData);
      peetDialog.updateDisplay();
    }
    mainPanel.showProcess(peetDialog.getContainer(), AXIS_ID);
  }
  
  private void savePeetlDialog() {
    if (peetDialog == null) {
      return;
    }
  }
}
