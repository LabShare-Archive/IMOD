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
import etomo.ui.swing.FrontPageDialog;
import etomo.ui.swing.LogInterface;
import etomo.ui.swing.LogPanel;
import etomo.ui.swing.MainFrontPagePanel;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.ProcessDisplay;

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
 * <p> $Log$
 * <p> Revision 1.6  2011/02/03 05:52:00  sueh
 * <p> bug# 1422 Using ProcessingMethod to keep track of which type of
 * <p> processing method is in use.  The decisions about when to display the
 * <p> parallel processing table have been centralized in
 * <p> ProcessingMethodMediator.
 * <p>
 * <p> Revision 1.5  2010/11/13 16:02:54  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.4  2010/09/08 19:16:53  sueh
 * <p> bug# 1401 Added doAutomation.
 * <p>
 * <p> Revision 1.3  2010/04/28 15:33:39  sueh
 * <p> bug# 1344 Added getFileSubdirectoryName and getMetaData.  Using
 * <p> ProcessSeries.Process to hold process information.
 * <p>
 * <p> Revision 1.2  2010/02/17 04:40:39  sueh
 * <p> bug# 1301 Moved comScriptMgr and logPanel from BaseManager to child
 * <p> class.
 * <p>
 * <p> Revision 1.1  2009/10/27 20:38:42  sueh
 * <p> bug# 1275 A manager for the default dialog.
 * <p> </p>
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
      mainPanel.setStatusBarText(paramFile, metaData, null);
      openFrontPageDialog();
      uiHarness.toFront(this);
    }
  }

  public void doAutomation() {
    if (EtomoDirector.INSTANCE.getArguments().isReconAutomation()) {
      frontPageDialog.reconActionForAutomation();
    }

    super.doAutomation();
  }

  public ProcessResultDisplayFactoryInterface getProcessResultDisplayFactoryInterface(
      AxisID axisID) {
    return null;
  }

  public FrontPageMetaData getMetaData() {
    return metaData;
  }

  public boolean setParamFile() {
    return loadedParamFile;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.PP;
  }

  public LogInterface getLogInterface() {
    return null;
  }

  public LogPanel getLogPanel() {
    return null;
  }

  public boolean canChangeParamFileName() {
    return false;
  }

  public boolean canSnapshot() {
    return false;
  }

  protected void createComScriptManager() {
  }

  protected void processSucceeded(final AxisID axisID, final ProcessName processName) {
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

  public String getFileSubdirectoryName() {
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

  public boolean isInManagerFrame() {
    return false;
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

  void startNextProcess(final AxisID axisID, final ProcessSeries.Process process,
      final ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      DialogType dialogType, ProcessDisplay display) {
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
    mainPanel.showProcess(frontPageDialog.getContainer(), AXIS_ID);
  }
}
