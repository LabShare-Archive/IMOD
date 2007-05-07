package etomo;

import java.io.File;

import etomo.comscript.PeetParserParam;
import etomo.comscript.ProcesschunksParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.PeetProcessManager;
import etomo.process.SystemProcessException;
import etomo.storage.ComFileFilter;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.PeetMetaData;
import etomo.type.PeetScreenState;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.ui.MainPanel;
import etomo.ui.MainPeetPanel;
import etomo.ui.ParallelPanel;
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
 * <p> Revision 1.16  2007/05/03 00:45:38  sueh
 * <p> bug# 964 Added getParallelProcessingDefaultNice() and
 * <p> removeComFiles().
 * <p>
 * <p> Revision 1.15  2007/04/27 23:37:54  sueh
 * <p> bug# 964 Added processchunks() and startNextProcess.  Changed
 * <p> prmParser() to peetParser.
 * <p>
 * <p> Revision 1.14  2007/04/26 02:42:56  sueh
 * <p> bug# 964 Added prmParser.
 * <p>
 * <p> Revision 1.13  2007/04/20 20:47:04  sueh
 * <p> bug# 964 Fixed bug setParamFile():  don't run initializeUIParameters() if name is
 * <p> empty.
 * <p>
 * <p> Revision 1.12  2007/04/02 21:41:04  sueh
 * <p> bug# 964 Change PeetDialog.ltfOutput to lftFnOutput.
 * <p>
 * <p> Revision 1.11  2007/03/26 23:30:39  sueh
 * <p> bug# 964 Setting metadata in ImodManager.
 * <p>
 * <p> Revision 1.10  2007/03/26 18:33:32  sueh
 * <p> bug# 964 Prevent MatlabParamFile from loading a .prm file unless the user asks
 * <p> for the file to be read.
 * <p>
 * <p> Revision 1.9  2007/03/21 18:07:34  sueh
 * <p> bug# 964 In savePeetDialog, writing to the .prm file.
 * <p>
 * <p> Revision 1.8  2007/03/20 23:00:19  sueh
 * <p> bug# 964 Fixed bug in setParamFile() where metadata.name was being reset
 * <p> after it was retrieved from the screen.
 * <p>
 * <p> Revision 1.7  2007/03/15 21:41:32  sueh
 * <p> bug# 964 Added a MatlabParamFile member variable.
 * <p>
 * <p> Revision 1.6  2007/03/01 01:11:18  sueh
 * <p> bug# 964 Removed protected modifiers.  Interpackage inheritance doesn't require
 * <p> it.
 * <p>
 * <p> Revision 1.5  2007/02/22 20:33import etomo.storage.autodoc.Autodoc;:46  sueh
 * <p> bug# 964 In setParamFile(), creating the param file if it doesn't exist.
 * <p>
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
public final class PeetManager extends BaseManager {
  public static final String rcsid = "$Id$";

  private static final AxisID AXIS_ID = AxisID.ONLY;

  private final PeetScreenState screenState = new PeetScreenState(AXIS_ID,
      AxisType.SINGLE_AXIS);
  private final PeetMetaData metaData;
  private final PeetProcessManager processMgr;

  private PeetDialog peetDialog = null;
  private MatlabParam matlabParam = null;
  private MainPeetPanel mainPanel;

  PeetManager() {
    this("");
  }

  PeetManager(String paramFileName) {
    super();
    this.metaData = new PeetMetaData();
    createState();
    processMgr = new PeetProcessManager(this);
    initializeUIParameters(paramFileName, AXIS_ID);
    if (loadedParamFile) {
      loadMatlabAutodoc(false);
    }
    if (!EtomoDirector.getInstance().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData);
      openPeetDialog();
    }
  }

  public boolean canChangeParamFileName() {
    return !loadedParamFile;
  }

  public boolean canSnapshot() {
    return false;
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public BaseScreenState getBaseScreenState(AxisID axisID) {
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
      imodManager.setMetaData(metaData);
      loadMatlabAutodoc(false);
      if (peetDialog != null) {
        peetDialog.setDirectory(paramFile.getParent());
        peetDialog.setFnOutput(rootName);
        peetDialog.updateDisplay(true);
      }
    }
  }

  public void touch(String absolutePath) {
    processMgr.touch(absolutePath);
    try {
      Thread.sleep(20);
    }
    catch (InterruptedException e) {
    }
  }

  /**
   * Creates matlabParam from matlabParamFile and sets the paramFile and
   * propertyUserDir.  Changes the location of the file in matlabParam to 
   * propertyUserDir and loads peetDialog.  This creates a new project out of an
   * existing .prm file.
   * @param matlabParamFile
   */
  public void setMatlabParam(File matlabParamFile) {
    matlabParam = new MatlabParam(matlabParamFile, false);
    matlabParam.read();
    peetDialog.setFnOutput(matlabParam.getFnOutput());
    setParamFile();
    matlabParam.setFileDir(propertyUserDir);
    loadPeetDialog();
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
    String name = peetDialog.getFnOutput();
    if (name == null || name.matches("\\s*")) {
      return false;
    }
    File paramFile = new File(peetDialog.getDirectory(), name
        + DatasetFiles.PEET_DATA_FILE_EXT);
    if (!paramFile.exists()) {
      touch(paramFile.getAbsolutePath());
    }
    initializeUIParameters(paramFile, AXIS_ID);
    if (!loadedParamFile) {
      return false;
    }
    metaData.setName(name);
    if (!metaData.isValid()) {
      uiHarness.openMessageDialog(
          "Invalid data, unable to proceed.  Please exit and restart Etomo",
          "Fatal Error");
      return false;
    }
    imodManager.setMetaData(metaData);
    if (matlabParam == null) {
      loadMatlabAutodoc(true);
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

  public int getParallelProcessingDefaultNice() {
    return 18;
  }

  public void peetParser() {
    savePeetDialog();
    PeetParserParam param = new PeetParserParam(this, matlabParam.getFile());
    try {
      LogFile log = LogFile.getInstance(param.getLogFile());
      try {
        log.backup();
      }
      catch (LogFile.FileException e) {
        e.printStackTrace();
      }
      removeComFiles();
      String threadName = processMgr.peetParser(param);
      setNextProcess(AxisID.ONLY, ProcessName.PROCESSCHUNKS.toString());
      setThreadName(threadName, AxisID.ONLY);
      mainPanel.startProgressBar("Running " + ProcessName.PEET_PARSER,
          AxisID.ONLY, ProcessName.PEET_PARSER);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to run " + ProcessName.PEET_PARSER
          + ", SystemProcessException.\n" + e.getMessage(), "Process Error");
    }
  }

  private void removeComFiles() {
    File dir = new File(getPropertyUserDir());
    File[] comFileArray = dir.listFiles(new ComFileFilter(metaData.getName()));
    if (comFileArray == null) {
      return;
    }
    for (int i = 0; i < comFileArray.length; i++) {
      if (comFileArray[i].isFile()) {
        comFileArray[i].delete();
      }
    }
  }

  void updateDialog(ProcessName processName, AxisID axisID) {
  }

  void setMetaData(ImodManager imodManager) {
  }

  void processSucceeded(AxisID axisID, ProcessName processName) {
  }

  BaseState getBaseState() {
    return null;
  }

  void createProcessTrack() {
  }

  void createMainPanel() {
    mainPanel = new MainPeetPanel(this);
  }

  void createComScriptManager() {
  }

  BaseProcessManager getProcessManager() {
    return processMgr;
  }

  BaseProcessTrack getProcessTrack() {
    return null;
  }

  void getProcessTrack(Storable[] storable, int index) {
  }

  Storable[] getStorables(int offset) {
    Storable[] storables = new Storable[2 + offset];
    int index = offset;
    storables[index++] = metaData;
    storables[index++] = screenState;
    return storables;
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  void startNextProcess(AxisID axisID, String nextProcess,
      ProcessResultDisplay processResultDisplay) {
    if (nextProcess.equals(ProcessName.PROCESSCHUNKS.toString())) {
      processchunks();
    }
  }

  /**
   * Initialize metalabParamFile.  Dependent on metaData.
   */
  private void loadMatlabAutodoc(boolean newFile) {
    if (!loadedParamFile || matlabParam != null || paramFile == null) {
      return;
    }
    matlabParam = new MatlabParam(DatasetFiles.getMatlabParamFile(this),
        newFile);
    matlabParam.read();
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
      peetDialog = PeetDialog.getInstance(this, AXIS_ID);
    }
    mainPanel.setParallelDialog(AXIS_ID, peetDialog.usingParallelProcessing());
    loadPeetDialog();
    mainPanel.showProcess(peetDialog.getContainer(), AXIS_ID);
  }
  
  private void loadPeetDialog() {
    if (paramFile != null && metaData.isValid()) {
      peetDialog.setParameters(metaData);
      peetDialog.setParameters(screenState);
      if (matlabParam != null) {
        peetDialog.setParameters(matlabParam);
      }
      peetDialog.setDirectory(propertyUserDir);
      peetDialog.updateDisplay(loadedParamFile);
    }
  }

  private boolean savePeetDialog() {
    if (peetDialog == null) {
      return false;
    }
    if (paramFile == null) {
      if (!setParamFile()) {
        return false;
      }
    }
    if (matlabParam == null) {
      return false;
    }
    peetDialog.getParameters(metaData);
    peetDialog.getParameters(screenState);
    saveStorables(AXIS_ID);
    peetDialog.getParameters(matlabParam);
    matlabParam.write();
    return true;
  }

  /**
   * Run processchunks.
   * @param axisID
   */
  private void processchunks() {
    ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
    if (peetDialog == null) {
      return;
    }
    peetDialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      return;
    }
    //param should never be set to resume
    parallelPanel.resetResults();
    processchunks(AxisID.ONLY, param, null);
  }
}