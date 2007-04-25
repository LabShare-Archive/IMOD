package etomo;

import java.awt.Dimension;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Vector;

import etomo.comscript.ComScriptManager;
import etomo.comscript.LoadAverageParam;
import etomo.comscript.ProcesschunksParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.ImodqtassistProcess;
import etomo.process.SystemProcessException;
import etomo.process.SystemProcessInterface;
import etomo.process.ProcessData;
import etomo.storage.LogFile;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.DialogType;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.UserConfiguration;
import etomo.ui.LoadAverageDisplay;
import etomo.ui.MainPanel;
import etomo.ui.AbstractParallelDialog;
import etomo.ui.ParallelPanel;
import etomo.ui.UIHarness;
import etomo.util.Utilities;

/**
 * <p>Description: Base class for ApplicationManager and JoinManager</p>
 * 
 * <p>Copyright: Copyright (c) 2002 - 2005</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public abstract class BaseManager {
  public static final String rcsid = "$Id$";

  //protected static variables
  private static boolean headless = false;
  //protected MainFrame mainFrame = null;
  UIHarness uiHarness = UIHarness.INSTANCE;
  static UserConfiguration userConfig = EtomoDirector.getInstance()
      .getUserConfiguration();

  //protected variables
  boolean loadedParamFile = false;
  // imodManager manages the opening and closing closing of imod(s), message
  // passing for loading model
  final ImodManager imodManager;
  //  This object controls the reading and writing of David's com scripts
  ComScriptManager comScriptMgr = null;
  File paramFile = null;
  //FIXME homeDirectory may not have to be visible
  String homeDirectory;
  // Control variable for process execution
  private String nextProcessA = "";
  private String nextProcessB = "";
  private String lastProcessA = "";
  private String lastProcessB = "";
  String threadNameA = "none";

  String threadNameB = "none";

  boolean backgroundProcessA = false;
  String backgroundProcessNameA = null;
  String propertyUserDir = null;//working directory for this manager

  //private static variables
  private boolean debug = false;
  private boolean exiting = false;
  private boolean initialized = false;
  private ProcessResultDisplayFactory processResultDisplayFactoryA = null;
  private ProcessResultDisplayFactory processResultDisplayFactoryB = null;
  private DialogType processDialogTypeA = null;
  private DialogType processDialogTypeB = null;
  private DialogType currentDialogTypeA = null;
  private DialogType currentDialogTypeB = null;
  private ParameterStore parameterStore = null;

  abstract void createComScriptManager();

  abstract void createMainPanel();

  abstract void createProcessTrack();

  abstract void updateDialog(ProcessName processName, AxisID axisID);

  abstract void setMetaData(ImodManager imodManager);

  public abstract BaseMetaData getBaseMetaData();

  public abstract MainPanel getMainPanel();

  abstract void getProcessTrack(Storable[] storable, int index);

  abstract BaseProcessTrack getProcessTrack();

  abstract BaseState getBaseState();

  public abstract void kill(AxisID axisID);

  public abstract void pause(AxisID axisID);

  public abstract void touch(String absolutePath);

  abstract BaseProcessManager getProcessManager();

  public abstract BaseScreenState getBaseScreenState(AxisID axisID);

  public abstract boolean canChangeParamFileName();

  public abstract void setParamFile(File paramFile);

  public abstract boolean canSnapshot();
  
  public abstract boolean setParamFile();

  abstract void processSucceeded(AxisID axisID, ProcessName processName);

  abstract void startNextProcess(AxisID axisID, String nextProcess,
      ProcessResultDisplay processResultDisplay);

  abstract Storable[] getStorables(int offset);

  public abstract String getName();

  public BaseManager() {
    propertyUserDir = System.getProperty("user.dir");
    createProcessTrack();
    createComScriptManager();
    //  Initialize the program settings
    debug = EtomoDirector.getInstance().isDebug();
    headless = EtomoDirector.getInstance().isHeadless();
    if (!headless) {
      createMainPanel();
    }
    imodManager = new ImodManager(this);
    initProgram();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  String paramString() {
    return "headless=" + headless + ",uiHarness=" + uiHarness + ",userConfig="
        + userConfig + ",\nloadedParamFile=" + loadedParamFile
        + ",imodManager=" + imodManager + ",\ncomScriptMgr=" + comScriptMgr
        + ",paramFile=" + paramFile + ",\nhomeDirectory=" + homeDirectory
        + ",nextProcessA=" + nextProcessA + ",\nnextProcessB=" + nextProcessB
        + ",lastProcessA=" + lastProcessA + ",\nlastProcessB=" + lastProcessB
        + ",threadNameA=" + threadNameA + ",\nthreadNameB=" + threadNameB
        + ",backgroundProcessA=" + backgroundProcessA
        + ",\nbackgroundProcessNameA=" + backgroundProcessNameA
        + ",\npropertyUserDir=" + propertyUserDir + ",\ndebug=" + debug
        + ",exiting" + exiting + "," + super.toString();
  }

  private void initProgram() {
    System.err.println("propertyUserDir:  " + propertyUserDir);
  }

  public String getPropertyUserDir() {
    return propertyUserDir;
  }

  public ProcessResultDisplayFactory getProcessResultDisplayFactory(
      AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (processResultDisplayFactoryB == null) {
        processResultDisplayFactoryB = new ProcessResultDisplayFactory(
            getBaseScreenState(axisID));
      }
      return processResultDisplayFactoryB;
    }
    if (processResultDisplayFactoryA == null) {
      processResultDisplayFactoryA = new ProcessResultDisplayFactory(
          getBaseScreenState(axisID));
    }
    return processResultDisplayFactoryA;
  }

  public String setPropertyUserDir(String propertyUserDir) {
    //avoid empty strings
    if (propertyUserDir.matches("\\s*")) {
      propertyUserDir = null;
    }
    String oldPropertyUserDir = this.propertyUserDir;
    this.propertyUserDir = propertyUserDir;
    return oldPropertyUserDir;
  }

  void initializeUIParameters(File dataFile, AxisID axisID) {
    if (!headless) {
      if (dataFile != null) {
        loadedParamFile = loadParamFile(dataFile, axisID);
      }
    }
    initialized = true;
  }

  void initializeUIParameters(String paramFileName, AxisID axisID) {
    if (paramFileName == null || paramFileName.equals("")) {
      initializeUIParameters((File) null, axisID);
    }
    else {
      initializeUIParameters(new File(paramFileName), axisID);
    }
  }

  /**
   * Save etomo to parametersState by asking the child manager for a list of
   * storable objects.  This is used when storable objects may have been
   * changes and nothing has been saved (update comscript functions).  This will
   * often do unnecessary saves but it guarentees that everything will be saved.
   * @throws IOException
   */
  public void saveStorables(AxisID axisID) {
    getParameterStore();
    if (parameterStore == null) {
      return;
    }
    parameterStore.setAutoStore(false);
    Storable[] storables = getStorables();
    if (storables == null) {
      return;
    }
    try {
      for (int i = 0; i < storables.length; i++) {
        parameterStore.save(storables[i]);
      }
    }
    catch (LogFile.FileException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to save to properties.  "
          + e.getMessage(), "Etomo Error", axisID);
    }
    catch (LogFile.WriteException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to write to properties.  "
          + e.getMessage(), "Etomo Error", axisID);
    }
    parameterStore.setAutoStore(true);
    try {
      parameterStore.storeProperties();
    }
    catch (LogFile.FileException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to save to "
          + paramFile.getAbsolutePath() + ".  " + e.getMessage(),
          "Etomo Error", axisID);
    }
    catch (LogFile.WriteException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to write to "
          + paramFile.getAbsolutePath() + ".  " + e.getMessage(),
          "Etomo Error", axisID);
    }
  }

  /**
   * Get the storable objects from the child and base manager.
   * @return
   */
  private Storable[] getStorables() {
    Storable[] storables = getStorables(2);
    storables[0] = getProcessManager().getProcessData(AxisID.FIRST);
    storables[1] = getProcessManager().getProcessData(AxisID.SECOND);
    return storables;
  }

  /**
   * Save etomo to parameterStore by asking the child manager to save its state.
   * This is used when using the done functionality of the dialogs (file save
   * and exit).
   * @throws IOException
   */
  void save() throws LogFile.FileException, LogFile.WriteException {
    if (parameterStore == null) {
      return;
    }
    parameterStore.setAutoStore(false);
    parameterStore.save(getProcessManager().getProcessData(AxisID.FIRST));
    parameterStore.save(getProcessManager().getProcessData(AxisID.SECOND));
  }

  /**
   * A message asking the ApplicationManager to save the parameter
   * information to a file.
   */
  public final boolean saveParamFile() throws LogFile.FileException,
      LogFile.WriteException {
    setParamFile();
    if (getParameterStore() == null) {
      return false;
    }
    if (!EtomoDirector.getInstance().isMemoryAvailable()) {
      return true;
    }
    save();
    parameterStore.setAutoStore(true);
    parameterStore.storeProperties();
    //save(getStorableArray(true), axisID);
    //  Update the MRU test data filename list
    userConfig.putDataFile(paramFile.getAbsolutePath());
    uiHarness.setMRUFileLabels(userConfig.getMRUFileList());
    // Reset the process track flag, if it exists
    BaseProcessTrack processTrack = getProcessTrack();
    if (processTrack != null) {
      processTrack.resetModified();
    }
    return true;
  }

  /**
   * Creates parameterStore if it doesn't already exist.  Return null if paramFile is
   * null.
   * @return
   */
  public ParameterStore getParameterStore() {
    if (paramFile == null) {
      return null;
    }
    synchronized (paramFile) {
      if (parameterStore != null) {
        return parameterStore;
      }
      if (parameterStore != null) {
        return parameterStore;
      }
      parameterStore = new ParameterStore(paramFile);
      return parameterStore;
    }
  }

  void endThreads() {
    imodManager.stopRequestHandler();
    getMainPanel().endThreads();
    if (parameterStore != null) {
      parameterStore.setAutoStore(false);
    }
  }

  /**
   * Exit the program.  To guarantee that etomo can always exit, catch all
   * unrecognized Exceptions and Errors and return true.
   */
  boolean exitProgram(AxisID axisID) {
    exiting = true;
    try {
      //Check for processes that will die if etomo exits
      SystemProcessInterface processA = getProcessManager().getThread(
          AxisID.FIRST);
      SystemProcessInterface processB = getProcessManager().getThread(
          AxisID.SECOND);
      boolean nohupA = processA == null || processA.isNohup();
      boolean nohupB = processB == null || processB.isNohup();
      if (!nohupA || !nohupB) {
        if (!uiHarness
            .openYesNoWarningDialog(
                "WARNING!!\nThere is process running which will stop if eTomo exits.\nDo you still wish to exit the program?"
                    .toString(), axisID)) {
          exiting = false;
          return false;
        }
      }
      //  Check to see if next processes have to be done
      ArrayList messageArray = new ArrayList();
      String nextProcessA = null;
      String nextProcessB = null;
      if (isNextProcessSet(AxisID.FIRST)) {
        nextProcessA = getNextProcess(AxisID.FIRST);
      }
      if (isNextProcessSet(AxisID.SECOND)) {
        nextProcessB = getNextProcess(AxisID.SECOND);
      }
      if (nextProcessA != null || nextProcessB != null) {
        boolean twoProcesses = nextProcessA != null && nextProcessB != null;
        StringBuffer message = new StringBuffer(
            "WARNING!!!\nIf you exit now then the current process");
        if (twoProcesses) {
          message.append("es");
        }
        message.append(" will not finish.  The subprocess");
        if (twoProcesses) {
          message.append("es");
        }
        if (nextProcessA != null) {
          message.append(" " + nextProcessA + " on Axis A");
        }
        if (twoProcesses) {
          message.append(" and");
        }
        if (nextProcessB != null) {
          message.append(" " + nextProcessB + " on Axis B");
        }
        message
            .append(" should run next.\nDo you still wish to exit the program?");

        if (!uiHarness.openYesNoWarningDialog(message.toString(), axisID)) {
          exiting = false;
          return false;
        }
      }
      if (!checkUnidentifiedProcess(AxisID.FIRST)
          || !checkUnidentifiedProcess(AxisID.SECOND)) {
        exiting = false;
        return false;
      }
      //  Should we close the 3dmod windows
      try {
        if (imodManager.isOpen()) {
          String[] message = new String[3];
          message[0] = "There are still 3dmod programs running.";
          message[1] = "Do you wish to end these programs?";
          if (uiHarness.openYesNoDialog(message, axisID)) {
            imodManager.quit();
          }
        }
      }
      catch (AxisTypeException except) {
        except.printStackTrace();
        uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
            axisID);
      }
      ImodqtassistProcess.INSTANCE.quit();
      imodManager.disconnect();
      return true;
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
  }

  private boolean checkUnidentifiedProcess(AxisID axisID) {
    SystemProcessInterface thread = getProcessManager().getThread(axisID);
    if (thread == null) {
      return true;
    }
    ProcessData processData = thread.getProcessData();
    if (processData != null && processData.isEmpty()) {
      if (!uiHarness
          .openYesNoWarningDialog(
              "There currently is an unidentified process running.\nPlease wait a few seconds while it is identified.\n\nExit without waiting?",
              axisID)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Check if the current data set is a dual axis data set
   * @return true if the data set is a dual axis data set
   */
  public boolean isDualAxis() {
    if (getBaseMetaData().getAxisType() == AxisType.SINGLE_AXIS) {
      return false;
    }
    else {
      return true;
    }
  }

  public Vector imodGetRubberbandCoordinates(String imodKey, AxisID axisID) {
    Vector results = null;
    try {
      results = imodManager.getRubberbandCoordinates(imodKey);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "System Process Exception",
          AxisID.ONLY);
    }
    return results;
  }

  void setPanel() {
    uiHarness.pack(this);
    //  Resize to the users preferrred window dimensions
    getMainPanel().setSize(
        new Dimension(userConfig.getMainWindowWidth(), userConfig
            .getMainWindowHeight()));
    uiHarness.doLayout();
    uiHarness.validate();
    if (isDualAxis()) {
      getMainPanel().setDividerLocation(0.51);
    }
  }

  //get functions

  /**
   * Return the absolute IMOD bin path
   * @return
   */
  public static String getIMODBinPath() {
    return EtomoDirector.getInstance().getIMODDirectory().getAbsolutePath()
        + File.separator + "bin" + File.separator;
  }

  /**
   * Return a reference to THE com script manager
   * @return
   */
  public ComScriptManager getComScriptManager() {
    return comScriptMgr;
  }

  /**
   * Open or raise a specific 3dmod to view a file with binning.
   * Or open a new 3dmod.
   * Return the index of the 3dmod opened or raised.
   */
  public int imodOpen(String imodKey, int imodIndex, File file, int binning,
      Run3dmodMenuOptions menuOptions) {
    try {
      if (imodIndex == -1) {
        imodIndex = imodManager.newImod(imodKey, file);
      }
      imodManager.setBinningXY(imodKey, imodIndex, binning);
      imodManager.open(imodKey, imodIndex, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open " + imodKey
          + " 3dmod with imodIndex=" + imodIndex, AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", AxisID.ONLY);
    }
    return imodIndex;
  }

  /**
   * Open or raise a specific 3dmod to view a file with a model.
   * Or open a new 3dmod.
   * Return the index of the 3dmod opened or raised.
   */
  public int imodOpen(String imodKey, int imodIndex, String absoluteFilePath,
      String absoluteModelPath, Run3dmodMenuOptions menuOptions) {
    File file = new File(absoluteFilePath);
    try {
      if (imodIndex == -1) {
        imodIndex = imodManager.newImod(imodKey, file);
      }
      imodManager.open(imodKey, imodIndex, absoluteModelPath, true,menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open " + imodKey
          + " 3dmod with imodIndex=" + imodIndex, AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", AxisID.ONLY);
    }
    return imodIndex;
  }

  /**
   * Open 3dmod
   */
  public void imodOpen(String imodKey, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.open(imodKey, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "Can't open " + imodKey
          + " in 3dmod ", AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "IO Exception", AxisID.ONLY);
    }
  }

  /**
   * Return the parameter file as a File object
   * @return a File object specifying the data set parameter file.
   */
  public File getParamFile() {
    return paramFile;
  }

  /**
   * A message asking the ApplicationManager to load in the information from the
   * parameter file.
   * @param paramFile the File object specifiying the data parameter file.
   */
  boolean loadParamFile(File paramFile, AxisID axisID) {
    FileInputStream processDataStream;
    // Set the current working directory for the application, this is the
    // path to the EDF or EJF file.  The working directory is defined by the current
    // user.dir system property.
    // Uggh, stupid JAVA bug, getParent() only returns the parent if the File
    // was created with the full path
    paramFile = new File(paramFile.getAbsolutePath());
    propertyUserDir = paramFile.getParent();
    StringBuffer invalidReason = new StringBuffer();
    if (!Utilities.isValidFile(paramFile, "Parameter file", invalidReason,
        true, true, true, false)) {
      uiHarness.openMessageDialog(invalidReason.toString(), "File Error",
          axisID);
      return false;
    }
    this.paramFile = paramFile;
    // Read in the test parameter data file
    getParameterStore();
    //must load meta data before other storables can be constructed
    try {
      parameterStore.load(getBaseMetaData());
      Storable[] storables = getStorables();
      if (storables != null) {
        for (int i = 0; i < storables.length; i++) {
          parameterStore.load(storables[i]);
        }
      }
    }
    catch (LogFile.WriteException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not find the test parameter data file:";
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "Etomo Error", axisID);
      return false;
    }
    // Update the MRU test data filename list
    userConfig.putDataFile(paramFile.getAbsolutePath());
    //  Initialize a new IMOD manager
    setMetaData(imodManager);
    return true;
  }

  void backupFile(File file, AxisID axisID) {
    if (file != null && file.exists()) {
      File backupFile = new File(file.getAbsolutePath() + "~");
      try {
        Utilities.renameFile(file, backupFile);
      }
      catch (IOException except) {
        System.err.println("Unable to backup file: " + file.getAbsolutePath()
            + " to " + backupFile.getAbsolutePath());
        uiHarness.openMessageDialog(except.getMessage(), "File Rename Error",
            axisID);
      }
    }
  }

  /**
   * Stop progress bar and start next process.
   * @param threadName
   * @param exitValue
   * @param processName
   * @param axisID
   */
  public final void processDone(String threadName, int exitValue,
      ProcessName processName, AxisID axisID, ProcessEndState endState,
      boolean failed, ProcessResultDisplay processResultDisplay) {
    processDone(threadName, exitValue, processName, axisID, false, endState,
        null, failed, processResultDisplay);
  }

  public final void processDone(String threadName, int exitValue,
      ProcessName processName, AxisID axisID, boolean forceNextProcess,
      ProcessEndState endState, boolean failed,
      ProcessResultDisplay processResultDisplay) {
    processDone(threadName, exitValue, processName, axisID, forceNextProcess,
        endState, null, failed, processResultDisplay);
  }

  /**
   * Notification message that a background process is done.
   * 
   * @param threadName
   *            The name of the thread that has finished
   */
  public final void processDone(String threadName, int exitValue,
      ProcessName processName, AxisID axisID, boolean forceNextProcess,
      ProcessEndState endState, String statusString, boolean failed,
      ProcessResultDisplay processResultDisplay) {
    if (threadName.equals(threadNameA)) {
      getMainPanel().stopProgressBar(AxisID.FIRST, endState, statusString);
      threadNameA = "none";
      backgroundProcessA = false;
      backgroundProcessNameA = null;
      //axisID = AxisID.FIRST;
    }
    else if (threadName.equals(threadNameB)) {
      getMainPanel().stopProgressBar(AxisID.SECOND, endState, statusString);
      threadNameB = "none";
      //axisID = AxisID.SECOND;
    }
    else {
      uiHarness.openMessageDialog("Unknown thread finished!!!", "Thread name: "
          + threadName, axisID);
    }
    if (processName != null) {
      updateDialog(processName, axisID);
    }
    //Try to start the next process if the process succeeded, or if
    //forceNextProcess is true (unless the user killed the process, it makes the
    //nextProcess execute even when the current process failed).
    if (endState != ProcessEndState.KILLED
        && (exitValue == 0 || forceNextProcess)) {
      if (!startNextProcess(axisID, processResultDisplay)) {
        sendMsgProcessSucceeded(processResultDisplay);
        processSucceeded(axisID, processName);
      }
    }
    else {
      //If the process failed or was killed, get rid of the nextProcess and the
      //lastProcess, so they can't be executed by mistake.
      resetNextProcess(axisID);
      resetLastProcess(axisID);
      if (failed) {
        sendMsgProcessFailed(processResultDisplay);
      }
    }
  }

  /**
   * Run processchunks.
   * @param axisID
   */
  public void processchunks(AxisID axisID, ProcesschunksParam param,
      ProcessResultDisplay processResultDisplay) {
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    String threadName;
    try {
      threadName = getProcessManager().processchunks(axisID, param,
          parallelPanel, processResultDisplay);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.PROCESSCHUNKS;
      message[1] = e.getMessage();
      UIHarness.INSTANCE.openMessageDialog(message,
          "Unable to execute command", axisID);
      processResultDisplay.msgProcessFailedToStart();
      return;
    }
    //set param in parallel panel so it can do a resume
    parallelPanel.setProcessInfo(param, processResultDisplay);
    setThreadName(threadName, axisID);
  }

  /**
   * This is a process done function for processes which are completed while the
   * original manager function waits and do not use the process manager.
   * It is necessary to call this function if this type of processes is the last
   * in a sequence which contains comscript or background processes, since the
   * processResultDisplay will not be run.
   * This processDone function always assumes success because the secondary
   * process won't run if the proceeding process failed.  The exception to this
   * is when forceNextProcess is on.  Pass forceNextProcess and an exit value
   * from the process where forceNextProcess was true.  The last process
   * will have to take responsibility for success/failure when forceNextProcess
   * is on.
   * @param axisID
   * @param processResultDisplay
   */
  final void processDone(AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    if (!startNextProcess(axisID, processResultDisplay)) {
      sendMsgProcessSucceeded(processResultDisplay);
    }
  }

  void sendMsgProcessStarting(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgProcessStarting();
  }

  void sendMsgProcessFailedToStart(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgProcessFailedToStart();
  }

  void sendMsgProcessSucceeded(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgProcessSucceeded();
  }

  void sendMsgProcessFailed(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgProcessFailed();
  }

  void sendMsgSecondaryProcess(ProcessResultDisplay processResultDisplay) {
    if (processResultDisplay == null) {
      return;
    }
    processResultDisplay.msgSecondaryProcess();
  }

  /**
   * Set the current dialog type. This function is called from open functions
   * and from showBlankPRocess(). It allows Etomo to call the done function when
   * the user switches to another dialog.
   * @param dialogType
   * @param axisID
   */
  public void setCurrentDialogType(DialogType dialogType, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      currentDialogTypeB = dialogType;
    }
    else {
      currentDialogTypeA = dialogType;
    }
  }

  /**
   * Gets the current dialog type.
   * @param axisID
   * @return
   */
  DialogType getCurrentDialogType(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return currentDialogTypeB;
    }
    return currentDialogTypeA;
  }

  /**
   * Keep final.  If nextProcess is not set, but lastProcess is set, then it
   * sets nextProcess to lastProcess and resets lastProcess.  Then, if
   * nextProcess is set, it sends a message to processResultDisplay and starts
   * the next process.  This function resets nextProcess.
   * @param axisID
   * @param processResultDisplay
   * @return true if a next process is run.
   */
  final boolean startNextProcess(AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    if (debug) {
      System.err.println("startNextProcess:nextProcess="
          + getNextProcess(axisID) + ",lastProcess=" + getLastProcess(axisID));
    }
    if (!isNextProcessSet(axisID) && isLastProcessSet(axisID)) {
      setNextProcess(axisID, getLastProcess(axisID));
      resetLastProcess(axisID);
    }
    if (isNextProcessSet(axisID)) {
      sendMsgSecondaryProcess(processResultDisplay);
      String nextProcess = getNextProcess(axisID);
      resetNextProcess(axisID);
      startNextProcess(axisID, nextProcess, processResultDisplay);
      return true;
    }
    return false;
  }

  /**
   * Keep final.
   * @param axisID
   * @param nextProcess
   */
  final void setNextProcess(AxisID axisID, String nextProcess) {
    if (axisID == AxisID.SECOND) {
      nextProcessB = nextProcess;

    }
    else {
      nextProcessA = nextProcess;
    }
  }

  /**
   * Keep private final.
   * @param axisID
   */
  private final void resetNextProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      nextProcessB = "";
    }
    else {
      nextProcessA = "";
    }
  }

  private final String getNextProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return nextProcessB;
    }
    return nextProcessA;
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  private final boolean isNextProcessSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (!nextProcessB.equals("") || processDialogTypeB != null) {
        return true;
      }
    }
    else if (!nextProcessA.equals("") || processDialogTypeA != null) {
      return true;
    }
    return false;
  }

  public void setProcessDialogType(AxisID axisID, DialogType dialogType) {
    if (axisID == AxisID.SECOND) {
      processDialogTypeB = dialogType;
    }
    else {
      processDialogTypeA = dialogType;
    }
  }

  public void resetProcessDialogType(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      processDialogTypeB = null;
    }
    else {
      processDialogTypeA = null;
    }
  }

  public DialogType getProcessDialogType(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return processDialogTypeB;
    }
    return processDialogTypeA;
  }

  /**
   * Keep final.
   * @param axisID
   * @param lastProcess
   */
  final void setLastProcess(AxisID axisID, String lastProcess) {
    if (debug) {
      System.err.println("setLastProcess:lastProcess=" + lastProcess);
    }
    if (axisID == AxisID.SECOND) {
      lastProcessB = lastProcess;
    }
    else {
      lastProcessA = lastProcess;
    }
  }

  /**
   * Keep final.
   * @param axisID
   */
  private final void resetLastProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      lastProcessB = "";
    }
    else {
      lastProcessA = "";
    }
  }

  private final String getLastProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return lastProcessB;
    }
    return lastProcessA;
  }

  private final boolean isLastProcessSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !lastProcessB.equals("");
    }
    return !lastProcessA.equals("");
  }

  public final void startGetLoadAverage(LoadAverageDisplay display,
      String computer) {
    LoadAverageParam param = LoadAverageParam.getInstance(computer, this);
    getProcessManager().startGetLoadAverage(param,
        display.getLoadAverageMonitor());
  }

  public final void endGetLoadAverage(LoadAverageDisplay display,
      String computer) {
    LoadAverageParam param = LoadAverageParam.getInstance(computer, this);
    getProcessManager().endGetLoadAverage(param,
        display.getLoadAverageMonitor());
  }

  public final void stopGetLoadAverage(LoadAverageDisplay display,
      String computer) {
    LoadAverageParam param = LoadAverageParam.getInstance(computer, this);
    getProcessManager().stopGetLoadAverage(param,
        display.getLoadAverageMonitor());
  }

  public final void makeCurrent() {
    if (propertyUserDir == null) {
      EtomoDirector.getInstance().makeCurrent();
    }
    else {
      System.setProperty("user.dir", propertyUserDir);
    }
  }

  public final void savePreferences(AxisID axisID, Storable storable) {
    MainPanel mainPanel = getMainPanel();
    try {
      if (getProcessManager().inUse(axisID, null)) {
        return;
      }
      mainPanel.setProgressBar("Saving defaults", 1, axisID);
      EtomoDirector.getInstance().getParameterStore().save(storable);
    }
    catch (LogFile.FileException e) {
      uiHarness.openMessageDialog("Unable to save preferences.\n"
          + e.getMessage(), "Etomo Error", axisID);
    }
    catch (LogFile.WriteException e) {
      uiHarness.openMessageDialog("Unable to write preferences.\n"
          + e.getMessage(), "Etomo Error", axisID);
    }
    mainPanel.stopProgressBar(axisID);
  }

  /**
   * Map the thread name to the correct axis
   * 
   * @param name
   *            The name of the thread to assign to the axis
   * @param axisID
   *            The axis of the thread to be mapped
   */
  void setThreadName(String name, AxisID axisID) {
    if (name == null) {
      name = "none";
    }
    if (axisID == AxisID.SECOND) {
      threadNameB = name;
    }
    else {
      threadNameA = name;
    }
  }

  public final void resume(AxisID axisID, ProcesschunksParam param,
      ProcessResultDisplay processResultDisplay) {
    sendMsgProcessStarting(processResultDisplay);
    if (param == null) {
      uiHarness.openMessageDialog("No command to resume", "Resume");
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    parallelPanel.getResumeParameters(param);
    String threadName;
    try {
      threadName = getProcessManager().processchunks(axisID, param,
          parallelPanel, processResultDisplay);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.PROCESSCHUNKS;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID);
      return;
    }
    setThreadName(threadName, axisID);

  }

  /**
   * 
   * @param axisID
   * @param dialog
   */
  public final void setParallelDialog(AxisID axisID,
      AbstractParallelDialog dialog) {
    getMainPanel().setParallelDialog(axisID, dialog.usingParallelProcessing());
  }

  public final void packPanel(AxisID axisID) {
    getMainPanel().pack(axisID);
  }

  public final void packPanel() {
    packPanel(AxisID.FIRST);
    packPanel(AxisID.SECOND);
  }

  public final void tomosnapshot(AxisID axisID) {
    String threadName;
    try {
      threadName = getProcessManager().tomosnapshot(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.TOMOSNAPSHOT;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute "
          + ProcessName.TOMOSNAPSHOT, axisID);
      return;
    }
    setThreadName(threadName, axisID);
    getMainPanel().startProgressBar("Running " + ProcessName.TOMOSNAPSHOT,
        axisID, ProcessName.TOMOSNAPSHOT);
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.78  2007/03/26 23:30:04  sueh
 * <p> bug# 964 Moved some of the imodOpen functions to the parent class to be shared.
 * <p>
 * <p> Revision 1.77  2007/03/01 01:09:27  sueh
 * <p> bug# 964 Removed protected modifiers.  Interpackage inheritance doesn't require
 * <p> it.
 * <p>
 * <p> Revision 1.76  2007/02/22 20:31:30  sueh
 * <p> bug# 966 Getting the shell environment variable in LoadAverageParam.
 * <p>
 * <p> Revision 1.75  2007/02/21 04:16:39  sueh
 * <p> bug# 964 Initializing parameters when the param file is chosen.
 * <p>
 * <p> Revision 1.74  2007/02/19 21:48:49  sueh
 * <p> bug# 964 Removed isNewManager() because it is only used by Application
 * <p> Manager.
 * <p>
 * <p> Revision 1.73  2007/02/05 21:25:59  sueh
 * <p> bug# 692  In processDone, run startNextProcess even if the process is empty,
 * <p> so that last process can be automatically placed in process.
 * <p>
 * <p> Revision 1.72  2006/11/28 22:47:02  sueh
 * <p> bug# 934 Changed BaseManager.stop() to endThreads().  Added a command to
 * <p> end main panel threads, which are load averages.  These threads are shared
 * <p> between managers, so they will only end if any other manager is using them.
 * <p>
 * <p> Revision 1.71  2006/11/16 23:13:42  sueh
 * <p> bug# 872 Removed print statement
 * <p>
 * <p> Revision 1.70  2006/11/15 18:18:21  sueh
 * <p> bug# 872 There are two ways to save:  saveStorables and saveParamFile.
 * <p> SaveParamFile is used with the done... functions when exiting etomo.
 * <p> SaveStorables is for saving before or after processes, when more then one
 * <p> storable may require saving.
 * <p>
 * <p> Revision 1.69  2006/10/24 21:13:43  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p> Revision 1.68  2006/10/16 22:33:29  sueh
 * <p> bug# 919  Changed touch(File) to touch(String absolutePath).
 * <p>
 * <p> Revision 1.67  2006/09/13 23:06:47  sueh
 * <p> bug# 920 Moving createState() call to child classes, so it can be done after meta
 * <p> data is created.
 * <p>
 * <p> Revision 1.66  2006/08/03 21:25:41  sueh
 * <p> bug# 769 ProcessDone():  Changed boolean error to boolean failed because is
 * <p> also covers killing a process.
 * <p>
 * <p> Revision 1.65  2006/07/28 19:43:31  sueh
 * <p> bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p> usingParallelProcessing.
 * <p>
 * <p> Revision 1.64  2006/07/26 16:31:46  sueh
 * <p> bug# 868 Added processchunks to ReconUIExpert.  Changed
 * <p> nextProcessDialogType to processDialogType.  Moved currentDialogType from
 * <p> ApplicationManager to BaseManager.
 * <p>
 * <p> Revision 1.63  2006/07/19 20:05:10  sueh
 * <p> bug# 902 ProcessDone:  calling processSucceeded.
 * <p>
 * <p> Revision 1.62  2006/07/17 21:15:57  sueh
 * <p> bug# 900 Added imodSendEvent functionality back.  Uses the
 * <p> SystemProcessException.
 * <p>
 * <p> Revision 1.61  2006/06/30 19:58:59  sueh
 * <p> bug# 877 Calling all the done dialog functions from the dialog.done() function,
 * <p> which is called from the button action functions and saveAction() in
 * <p> ProcessDialog.  Removing doneProcessDialog().
 * <p>
 * <p> Revision 1.60  2006/06/22 20:58:26  sueh
 * <p> bug# 797 imodGetRubberbandCoordinates() checking for errors before getting to
 * <p> this function
 * <p>
 * <p> Revision 1.59  2006/06/21 15:46:19  sueh
 * <p> bug# 581 Quitting Imodqtassist process when etomo exits.
 * <p>
 * <p> Revision 1.58  2006/06/15 16:12:42  sueh
 * <p> bug# 871 exitProgram():  Added a test for running processes that can't continue
 * <p> if etomo exits.
 * <p>
 * <p> Revision 1.57  2006/06/05 16:01:01  sueh
 * <p> bug# 766 getParamFileStorableArray():  Add the option have elements in the storable array that aer set by the base manager.  Add  initialized so that the
 * <p> manager can tell when an .edf is first loaded.
 * <p>
 * <p> Revision 1.56  2006/05/19 19:26:03  sueh
 * <p> bug# 866 Added nextProcessDialogType, to let the manager call
 * <p> UIExpert.nextProcess()
 * <p>
 * <p> Revision 1.55  2006/03/22 00:34:59  sueh
 * <p> bug# 836 Preventing savePreferences() from running when the axis is
 * <p> busy
 * <p>
 * <p> Revision 1.54  2006/03/20 17:48:14  sueh
 * <p> bug# 835 ParallelManager is overriding processChunks
 * <p>
 * <p> Revision 1.53  2006/01/31 20:35:17  sueh
 * <p> bug# 521 Added doneProcessDialog, a function which should be called
 * <p> by all of the ApplicationManager done functions.  It calls
 * <p> ProcessDialog.done().  Creating the ProcessResultDisplayFactory's with
 * <p> screen states, so that the displays can save their states.
 * <p>
 * <p> Revision 1.52  2006/01/26 21:46:30  sueh
 * <p> bug# 401 Removing processResultDisplayA and B before this method will
 * <p> fail when the user presses the same button twice.  Passing the
 * <p> processResultDisplay to the process manager to get it back when
 * <p> processDone is called.  Added a simple processDone for secondary
 * <p> processes that don't use the existing processDone functions.
 * <p> Added processResultDisplay parameters to all the toggle button functions.
 * <p>
 * <p> Revision 1.51  2006/01/20 20:43:34  sueh
 * <p> bug# 401 Added ProcessResultDisplay functionality to processDone and
 * <p> startNextProcess.  Added setProcessResultDisplay.
 * <p>
 * <p> Revision 1.50  2005/12/23 01:55:43  sueh
 * <p> bug# 675 Split the test option functionality.  Control headlessness with
 * <p> --headless.  This allow both JUnit and JfcUnit to use the special test
 * <p> functions.
 * <p>
 * <p> Revision 1.49  2005/12/14 01:26:31  sueh
 * <p> bug# 782 Updated the toString() function.
 * <p>
 * <p> Revision 1.48  2005/12/12 21:57:56  sueh
 * <p> bug# 779 Made BaseManager.resetNextProcess() private.  Added
 * <p> startNextProcess(), which calls resetNextProcess and then calls a child
 * <p> startNextProcess.  ResetNextProcess is only called from
 * <p> startNextProcess and processDone.
 * <p>
 * <p> Revision 1.47  2005/12/09 20:21:29  sueh
 * <p> bug# 776 Added tomosnapshot
 * <p>
 * <p> Revision 1.46  2005/12/08 00:53:29  sueh
 * <p> bug# 504 Changed exitProgram() to only warn the user when exiting would
 * <p> prevent the process from completing.
 * <p>
 * <p> Revision 1.45  2005/11/21 21:59:15  sueh
 * <p> bug# 761 In processchunks(), fail if
 * <p> ParallelPanel.getParameters(ProcesschunksParam) fails.
 * <p>
 * <p> Revision 1.44  2005/11/19 01:43:15  sueh
 * <p> bug# 744 Added processDone(String, int, ProcessName, AxisID,
 * <p> boolean, ProcessEndState).
 * <p>
 * <p> Revision 1.43  2005/11/10 17:52:23  sueh
 * <p> bug# 733 Added manager parameters to ProcesschunksParam constructor.
 * <p>
 * <p> Revision 1.42  2005/11/04 00:52:26  sueh
 * <p> fixed copyright
 * <p>
 * <p> Revision 1.41  2005/11/02 21:33:39  sueh
 * <p> bug# 754 Getting error and warning tags from ProcessMessages.
 * <p>
 * <p> Revision 1.40  2005/10/31 17:52:40  sueh
 * <p> bug# 730 Renamed getTestParamFile to getParamFile.  Made
 * <p> canChangeParamFileName() abstract.
 * <p>
 * <p> Revision 1.39  2005/10/27 00:07:26  sueh
 * <p> bug# 725 Allowing multiple paths when running nextProcess.  Added
 * <p> lastProcessA and B.  Added functions:  getLastProcess,
 * <p> isLastProcessSet, resetLastProcess, and setLastProcess.
 * <p>
 * <p> Revision 1.38  2005/10/15 00:27:42  sueh
 * <p> bug# 532 Changed showParallelStatus() to setParallelDialog().  Removed
 * <p> currentParallelDialogsA and B.  Simplified setParallelDialog() to just call
 * <p> MainPanel.setParallelDialog() with  dialog.isParallel().  isParallel() is true
 * <p> if any parallel processing check boxes are true on the dialog.
 * <p>
 * <p> Revision 1.37  2005/10/14 21:04:57  sueh
 * <p> bug# 730 Changed loadedTestParamFile to loadedParamFile.
 * <p>
 * <p> Revision 1.36  2005/09/29 18:38:48  sueh
 * <p> bug# 532 Preventing Etomo from saving to the .edf or .ejf file over and
 * <p> over during exit.  Added BaseManager.exiting and
 * <p> saveIntermediateParamFile(), which will not save when exiting it true.
 * <p> Setting exiting to true in BaseManager.exitProgram().  Moved call to
 * <p> saveParamFile() to the child exitProgram functions so that the param file
 * <p> is saved after all the done functions are run.
 * <p>
 * <p> Revision 1.35  2005/09/27 21:12:38  sueh
 * <p> bug# 532 Moved the creation of the the .edf file Storable array to the child
 * <p> classes because the classes the go into the array vary.  Moved the
 * <p> parallel panels to the axis level panels.
 * <p>
 * <p> Revision 1.34  2005/09/22 20:43:38  sueh
 * <p> bug# 532 Added showParallelStatus(), which is called when a parallel
 * <p> processing checkbox is checked.
 * <p>
 * <p> Revision 1.33  2005/09/21 16:04:34  sueh
 * <p> bug# 532 Moved processchunks() and setThreadName() to BaseManager.
 * <p>
 * <p> Revision 1.32  2005/09/19 16:38:29  sueh
 * <p> bug# 532 In getParallelPanel(), calling
 * <p> ParallelPanel.setContainer(ParallelDialog).
 * <p>
 * <p> Revision 1.31  2005/09/13 00:27:25  sueh
 * <p> bug# 532 Removed unecessary import.
 * <p>
 * <p> Revision 1.30  2005/09/13 00:13:46  sueh
 * <p> bug# 532 Modified savePreferences() to check whether the axis is busy.
 * <p>
 * <p> Revision 1.29  2005/09/12 23:56:50  sueh
 * <p> bug# 532 Added savePreferences() to save a Storable class to the .etomo
 * <p> file without overwriting preference entries from other Storable classes.
 * <p>
 * <p> Revision 1.28  2005/09/09 21:20:24  sueh
 * <p> bug# 532 Made LoadAverageParam an n'ton (one for each computer) so
 * <p> that there aren't IntermittentSystemPrograms then computers.  This allows
 * <p> IntermittentSystemProgram to be used for other things and conforms to
 * <p> it definition of having one instance per IntermittentCommand, instead of
 * <p> one instance per computer.
 * <p>
 * <p> Revision 1.27  2005/09/01 18:34:42  sueh
 * <p> bug# 532 Made the parallel panels manager level.  Added parallelPanelA
 * <p> and B.  Added getParallPanel(), which constructs the panel if necessary
 * <p> and returns it.
 * <p>
 * <p> Revision 1.26  2005/08/22 22:04:35  sueh
 * <p> bug# 714 Added makeCurrent() to set the user.dir property when the
 * <p> manager is switched.
 * <p>
 * <p> Revision 1.25  2005/08/22 15:57:13  sueh
 * <p> bug# 532 Added start and stop GetLoadAverage() to send load
 * <p> information to a LoadAverageDisplay.
 * <p>
 * <p> Revision 1.24  2005/08/10 20:38:29  sueh
 * <p> bug# 711 Made UIParameters constructor private.  Can't force it no be
 * <p> called since this is mostly static functions.
 * <p>
 * <p> Revision 1.23  2005/08/04 19:05:42  sueh
 * <p> bug# 532  Sending the manager to UIHarness.pack() so that
 * <p> packDialogs() can be called.
 * <p>
 * <p> Revision 1.22  2005/08/01 17:57:59  sueh
 * <p> Removed unnecessary FIXME
 * <p>
 * <p> Revision 1.21  2005/07/29 00:38:52  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.20  2005/07/26 17:03:16  sueh
 * <p> bug# 701 Pass ProcessEndState to the progress bar when stopping it.
 * <p>
 * <p> Revision 1.19  2005/06/21 00:03:37  sueh
 * <p> bug# 522 Added pass-through function call to
 * <p> BaseProcessManager.touch() for MRCHeaderTest.  Added toString()
 * <p> overide.
 * <p>
 * <p> Revision 1.18  2005/06/03 20:09:18  sueh
 * <p> bug# 671 processDone():  Removed code changing the axisID because it
 * <p> doesn't seem be necessary and it causes single axis file names to have
 * <p> an incorrect "a" added to them.
 * <p>
 * <p> Revision 1.17  2005/05/20 21:11:01  sueh
 * <p> bug# 664 saveTestParamFile(): do not attempt to save to a file if the
 * <p> memory is very low.  If the save fails, the file may be truncated.
 * <p>
 * <p> Revision 1.16  2005/05/18 22:30:37  sueh
 * <p> bug# 622 Made nextProcessA and B private because they are set using
 * <p> reset and setProcess functions.  Added a new parameter to
 * <p> processDone():  boolean forceNextProcess (default is false).
 * <p> ForceNextProcess is for when the next process is independent of the
 * <p> outcome of the previous process and makes startNextProcess() run
 * <p> regardless of value of exitValue.  It is used for archiveorig, to get the
 * <p> second axis.
 * <p>
 * <p> Revision 1.15  2005/05/17 19:08:40  sueh
 * <p> bug# 520 Fixed some recently introduced bugs in join.  We are saving to
 * <p> .ejf file more often.  When the first section is needs to be flipped, the
 * <p> join manager isn't ready to flip, because the paramFile isn't set.  So don't
 * <p> both to save in this situation.
 * <p>
 * <p> Revision 1.14  2005/04/26 17:34:35  sueh
 * <p> bug# 615 Made MainFrame a package-level class.  All MainFrame
 * <p> functionality is handled through UIHarness to make Etomo more
 * <p> compatible with JUnit.  Removing the mainFrame member variable.
 * <p>
 * <p> Revision 1.13  2005/04/25 20:32:08  sueh
 * <p> bug# 615 Passing the axis where the command originated to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.
 * <p>
 * <p> Revision 1.12  2005/04/21 20:28:13  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.
 * <p>
 * <p> Revision 1.11  2005/03/19 01:09:52  sueh
 * <p> adding comments
 * <p>
 * <p> Revision 1.10  2005/03/11 01:57:43  sueh
 * <p> bug# 612 Change nextProcess to support axis A and B.
 * <p>
 * <p> Revision 1.9  2005/03/01 20:50:37  sueh
 * <p> bug# 607 Catching Throwable in exitProgram and returning true to make
 * <p> sure that Etomo can always exit.
 * <p>
 * <p> Revision 1.8  2005/02/09 18:39:41  sueh
 * <p> bug# 595 There is no way to stop the user from running combine when
 * <p> another combine is still running from a previous Etomo session in the same
 * <p> dataset.  So warn the user that they won't receive a warning if they do
 * <p> this.
 * <p>
 * <p> Revision 1.7  2005/01/21 22:07:29  sueh
 * <p> bug# 509 bug# 591  Moved the management of MetaData to the Controller
 * <p> class.
 * <p>
 * <p> Revision 1.6  2004/12/14 21:23:39  sueh
 * <p> bug# 565: Fixed bug:  Losing process track when backing up .edf file and
 * <p> only saving metadata.  Removed unnecessary class JoinProcessTrack.
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.
 * <p> Saving all objects to the .edf/ejf file each time a save is done.
 * <p>
 * <p> Revision 1.5  2004/12/09 04:49:17  sueh
 * <p> bug# 565 Removed isDataParamDirty.  Synchronized storage of param
 * <p> file with store(Storable[]).  Automatically saving to param file on exit.
 * <p> Changed saveTestParamIfNecessary() to saveTestParamOnExit().
 * <p>
 * <p> Revision 1.4  2004/12/03 02:30:44  sueh
 * <p> bug# 568 Added setDataParamDirty() so that meta data can be changed
 * <p> in other objects.
 * <p>
 * <p> Revision 1.3  2004/11/23 00:14:11  sueh
 * <p> bug# 520 Allowed propertyUserDir to be set.  Prevented the construction
 * <p> of mainPanel when test is true.
 * <p>
 * <p> Revision 1.2  2004/11/19 22:33:55  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.19  2004/11/18 23:57:20  sueh
 * <p> bug# 520 Added saveMetaData to save only meta data.  Added
 * <p> boolean canChangePAramFileName to tell MainFrame whether Save As
 * <p> should be enabled.
 * <p>
 * <p> Revision 1.1.2.18  2004/11/17 02:18:27  sueh
 * <p> bug# 520 Fixed a comment.
 * <p>
 * <p> Revision 1.1.2.17  2004/11/15 22:04:42  sueh
 * <p> bug# 520 Removed the function isFileValid() because it is only called once.
 * <p> Placed the code from isFileValid() into loadTestParamFile().
 * <p>
 * <p> Revision 1.1.2.16  2004/11/12 22:43:34  sueh
 * <p> bug# 520 Moved imodGetRubberbandCoordinates from ApplicationManager.
 * <p>
 * <p> Revision 1.1.2.15  2004/10/29 01:15:23  sueh
 * <p> bug# 520 Removing unecessary functions that provided services to
 * <p> BaseManager.  BaseManager can use get... functions to get the
 * <p> mainPanel, metaData, and processTrack.
 * <p>
 * <p> Revision 1.1.2.14  2004/10/22 03:18:05  sueh
 * <p> bug# 520 Removed a FIXME comment.
 * <p>
 * <p> Revision 1.1.2.13  2004/10/21 17:49:56  sueh
 * <p> bug# 520 In loadTestParamFile() converted paramFile to a file created with
 * <p> an absolute path, so metaData validation would not fail.
 * <p>
 * <p> Revision 1.1.2.12  2004/10/15 00:00:02  sueh
 * <p> bug# 520 Moving getTestParamFilename() to mainPanel.  It is used for
 * <p> saving existing data files, so knows which type (.edf or .ejf) it is saving.
 * <p>
 * <p> Revision 1.1.2.11  2004/10/11 01:55:43  sueh
 * <p> bug# 520 moved responsibility for mainPanel, metaData, processTrack,
 * <p> and progressManager to child classes.  Used abstract functions to use
 * <p> these variables in the base classes.  This is more reliable and doesn't
 * <p> require casting.
 * <p>
 * <p> Revision 1.1.2.10  2004/10/08 21:12:27  sueh
 * <p> bug# 520 Backed out conversion from properties user.dir to workingDir
 * <p>
 * <p> Revision 1.1.2.9  2004/10/08 15:40:48  sueh
 * <p> bug# 520 Set workingDirName instead of system property for manager-
 * <p> level working directory.  Moved SettingsDialog to EtomoDirector.  Since
 * <p> EtomoDirector is a singleton, made all functions and member variables
 * <p> non-static.  The singleton code controls how many EtomoDirector
 * <p> instances can exist.  Moved application-level code in initProgram and
 * <p> exitProgram to EtomoDirector.
 * <p>
 * <p> Revision 1.1.2.8  2004/10/01 20:58:02  sueh
 * <p> bug# 520 Changed getMetaDAta() to getBaseMetaData() so it can return
 * <p> the abstract base class for objects that don't know which type of manager
 * <p> they are using.
 * <p>
 * <p> Revision 1.1.2.7  2004/09/29 17:37:09  sueh
 * <p> bug# 520 Using BaseMetaData, BaseProcessTrack, and
 * <p> BaseProcessManager.  Moved processDone() from app mgr to base mgr.
 * <p> Created abstract startNextProcess() and
 * <p> updateDialog(ProcessName, AxisID).  Removed resetState(),
 * <p> openNewDataset() and openExistingDataset().  Managers will not be
 * <p> reset and this functionality will be handled by EtomoDirector.
 * <p>
 * <p> Revision 1.1.2.6  2004/09/15 22:33:39  sueh
 * <p> bug# 520 call openMessageDialog in mainPanel instead of mainFrame.
 * <p> Move packMainWindow and setPanel from ApplicationMAnager to
 * <p> BaseManager.
 * <p>
 * <p> Revision 1.1.2.5  2004/09/13 16:26:46  sueh
 * <p> bug# 520 Adding abstract isNewManager.  Each manager would have a
 * <p> different way to tell whether they had a file open.
 * <p>
 * <p> Revision 1.1.2.4  2004/09/09 17:28:38  sueh
 * <p> bug# 520 MRU file labels already being set from EtomoDirector
 * <p>
 * <p> Revision 1.1.2.3  2004/09/08 19:27:19  sueh
 * <p> bug# 520 putting initialize UI parameters into a separate function
 * <p>
 * <p> Revision 1.1.2.2  2004/09/07 17:51:00  sueh
 * <p> bug# 520 getting mainFrame and userConfig from EtomoDirector, moved
 * <p> settings dialog to BaseManager,  moved backupFiles() to BaseManager,
 * <p> moved exitProgram() and processing variables to BaseManager, split
 * <p> MainPanel off from MainFrame
 * <p>
 * <p> Revision 1.1.2.1  2004/09/03 20:37:24  sueh
 * <p> bug# 520 Base class for ApplicationManager and JoinManager.  Transfering
 * <p> constructor functionality from AppMgr
 * <p> </p>
 */
