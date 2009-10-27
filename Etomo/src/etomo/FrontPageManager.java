package etomo;

import java.io.File;
import java.io.IOException;

import etomo.process.BaseProcessManager;
import etomo.process.ProcessResultDisplayFactoryInterface;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.DialogType;
import etomo.type.FrontPageMetaData;
import etomo.type.InterfaceType;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.FrontPageDialog;
import etomo.ui.LogPanel;
import etomo.ui.MainFrontPagePanel;
import etomo.ui.MainPanel;
import etomo.ui.ProcessDisplay;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
public final class FrontPageManager extends BaseManager {
  public static final String rcsid = "$Id$";

  private static final AxisID AXIS_ID = AxisID.ONLY;
  private FrontPageDialog frontPageDialog = null;
  private MainFrontPagePanel mainPanel;
  private final FrontPageMetaData metaData;

  public FrontPageManager() {
    super();
    this.metaData = new FrontPageMetaData();
    createState();
    initializeUIParameters("", AXIS_ID);
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData, logPanel);
      openFrontPageDialog();
    }
  }

  public ProcessResultDisplayFactoryInterface getProcessResultDisplayFactoryInterface(
      AxisID axisID) {
    return null;
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
  
  LogPanel createLogPanel() {
    return null;
  }

  public boolean canSnapshot() {
    return false;
  }

  protected void createComScriptManager() {
  }

  protected void processSucceeded(final AxisID axisID,
      final ProcessName processName) {
  }

  protected void createMainPanel() {
    mainPanel = new MainFrontPagePanel(this);
  }

  protected void createProcessTrack() {
  }

  private void createState() {
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public BaseScreenState getBaseScreenState(final AxisID axisID) {
    return null;
  }

  public BaseState getBaseState() {
    return null;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  Storable[] getStorables(final int offset) {
    Storable[] storables = new Storable[3 + offset];
    int index = offset;
    storables[index++] = metaData;
    return storables;
  }

  public BaseProcessManager getProcessManager() {
    return null;
  }

  BaseProcessTrack getProcessTrack() {
    return null;
  }

  void getProcessTrack(final Storable[] storable, final int index) {
  }

  public void kill(final AxisID axisID) {
  }

  public void pause(final AxisID axisID) {
  }

  public void save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
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

  public void setParamFile(final File paramFile) {
    this.paramFile = paramFile;
  }

  void startNextProcess(final AxisID axisID, final String nextProcess,
      final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, DialogType dialogType,
      ProcessDisplay display, ProcessName subProcessName) {
  }

  public String getName() {
    return metaData.getName();
  }

  void updateDialog(final ProcessName processName, final AxisID axisID) {
  }

  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
  }

  private void openFrontPageDialog() {
    if (frontPageDialog == null) {
      frontPageDialog = FrontPageDialog.getInstance();
    }
    mainPanel.setParallelDialog(AXIS_ID, frontPageDialog
        .usingParallelProcessing());
    mainPanel.showProcess(frontPageDialog.getContainer(), AXIS_ID);
  }
}
