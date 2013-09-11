package etomo;

import java.io.IOException;

import etomo.process.BaseProcessManager;
import etomo.storage.LogFile;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.FrontPageMetaData;
import etomo.type.InterfaceType;
import etomo.type.ProcessName;
import etomo.ui.FrontPageUIHarness;
import etomo.ui.swing.LogInterface;
import etomo.ui.swing.LogWindow;
import etomo.ui.swing.MainFrontPagePanel;
import etomo.ui.swing.MainPanel;

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
  private FrontPageUIHarness dialogExpert = new FrontPageUIHarness(this, AXIS_ID);
  private MainFrontPagePanel mainPanel;
  private final FrontPageMetaData metaData;

  public FrontPageManager() {
    super();
    this.metaData = new FrontPageMetaData(getLogProperties());
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
      dialogExpert.reconActionForAutomation();
    }

    super.doAutomation();
  }

  public FrontPageMetaData getMetaData() {
    return metaData;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.FRONT_PAGE;
  }

  public LogInterface getLogInterface() {
    return null;
  }

  void processSucceeded(final AxisID axisID, final ProcessName processName) {
  }

  void createMainPanel() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      mainPanel = new MainFrontPagePanel(this);
    }
  }

  private void createState() {
  }
  
  LogWindow createLogWindow(){
    return null;
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
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

  public void kill(final AxisID axisID) {
  }

  public void pause(final AxisID axisID) {
  }

  public boolean save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
    return true;
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

  public String getName() {
    return metaData.getName();
  }

  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
  }

  private void openFrontPageDialog() {
    dialogExpert.openDialog();
  }
}
