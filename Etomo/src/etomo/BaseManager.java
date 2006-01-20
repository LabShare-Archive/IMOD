package etomo;

import java.awt.Dimension;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;

import etomo.comscript.ComScriptManager;
import etomo.comscript.LoadAverageParam;
import etomo.comscript.ProcesschunksParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.process.ProcessMessages;
import etomo.process.ProcessState;
import etomo.process.SystemProcessException;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.UserConfiguration;
import etomo.ui.LoadAverageDisplay;
import etomo.ui.MainPanel;
import etomo.ui.ParallelDialog;
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
  protected UIHarness uiHarness = UIHarness.INSTANCE;
  protected static UserConfiguration userConfig = EtomoDirector.getInstance()
      .getUserConfiguration();

  //protected variables
  protected boolean loadedParamFile = false;
  // imodManager manages the opening and closing closing of imod(s), message
  // passing for loading model
  protected ImodManager imodManager = null;
  //  This object controls the reading and writing of David's com scripts
  protected ComScriptManager comScriptMgr = null;
  //FIXME paramFile may not have to be visible
  protected File paramFile = null;
  //FIXME homeDirectory may not have to be visible
  protected String homeDirectory;
  // Control variable for process execution
  private String nextProcessA = "";
  private String nextProcessB = "";
  private String lastProcessA = "";
  private String lastProcessB = "";
  protected String threadNameA = "none";

  protected String threadNameB = "none";

  protected boolean backgroundProcessA = false;
  protected String backgroundProcessNameA = null;
  protected String propertyUserDir = null;//working directory for this manager

  //private static variables
  private static boolean debug = false;
  private boolean exiting = false;
  private ProcessResultDisplay processResultDisplayA = null;
  private ProcessResultDisplay processResultDisplayB = null;

  protected abstract void createComScriptManager();

  protected abstract void createProcessManager();

  protected abstract void createMainPanel();

  protected abstract void createProcessTrack();

  protected abstract void createState();

  protected abstract void updateDialog(ProcessName processName, AxisID axisID);

  protected abstract void setMetaData(ImodManager imodManager);

  public abstract BaseMetaData getBaseMetaData();

  public abstract MainPanel getMainPanel();

  protected abstract void getProcessTrack(Storable[] storable, int index);

  protected abstract BaseProcessTrack getProcessTrack();

  protected abstract BaseState getBaseState();

  public abstract void kill(AxisID axisID);

  public abstract void pause(AxisID axisID);

  protected abstract Storable[] getParamFileStorableArray(
      boolean includeMetaData);

  public abstract void touch(File file);

  protected abstract BaseProcessManager getProcessManager();

  public abstract BaseScreenState getBaseScreenState(AxisID axisID);

  public abstract boolean save(AxisID axisID);

  public abstract boolean canChangeParamFileName();

  //FIXME needs to be public?
  public abstract boolean isNewManager();

  public abstract void setTestParamFile(File paramFile);

  public abstract boolean canSnapshot();

  protected abstract void startNextProcess(AxisID axisID, String nextProcess,
      ProcessResultDisplay processResultDisplay);

  public BaseManager() {
    propertyUserDir = System.getProperty("user.dir");
    createProcessTrack();
    createState();
    createProcessManager();
    createComScriptManager();
    //  Initialize the program settings
    debug = EtomoDirector.getInstance().isDebug();
    headless = EtomoDirector.getInstance().isHeadless();
    if (!headless) {
      createMainPanel();
      //mainFrame = EtomoDirector.getInstance().getMainFrame();
    }
    createImodManager();
    initProgram();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
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

  public String setPropertyUserDir(String propertyUserDir) {
    String oldPropertyUserDir = this.propertyUserDir;
    this.propertyUserDir = propertyUserDir;
    return oldPropertyUserDir;
  }

  protected void initializeUIParameters(String paramFileName, AxisID axisID) {
    if (!headless) {
      // Open the etomo data file if one was found on the command line
      if (!paramFileName.equals("")) {
        File etomoDataFile = new File(paramFileName);
        loadedParamFile = loadTestParamFile(etomoDataFile, axisID);
      }
    }
  }

  public final void saveIntermediateParamFile(AxisID axisID) {
    if (exiting) {
      return;
    }
    saveParamFile(axisID);
  }

  /**
   * A message asking the ApplicationManager to save the test parameter
   * information to a file.
   */
  protected final boolean saveParamFile(AxisID axisID) {
    if (paramFile == null) {
      return false;
    }
    if (!EtomoDirector.getInstance().isMemoryAvailable()) {
      return true;
    }
    save(getParamFileStorableArray(true), axisID);
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
   * Saves Storables in a synchronized function.  Only this function should use
   * the ParameterStore.save() call.  This prevents problems when multiple
   * threads try to save to the paramFile.
   * Backs up paramFile before saving.
   * paramFile must not be null.
   * @param storable
   */
  private synchronized void save(Storable[] storable, AxisID axisID) {
    if (storable == null || paramFile == null) {
      return;
    }
    backupFile(paramFile, axisID);
    ParameterStore paramStore = new ParameterStore(paramFile);
    try {
      paramStore.save(storable);
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file save error";
      errorMessage[1] = "Could not save test parameter data to file:";
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Test parameter file save error", axisID);
    }
  }

  /**
   * Exit the program.  To guarantee that etomo can always exit, catch all
   * unrecognized Exceptions and Errors and return true.
   */
  public boolean exitProgram(AxisID axisID) {
    exiting = true;
    try {
      //  Check to see if any processes are still running
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
      /*//handle background processes
       if (!threadNameA.equals("none") && backgroundProcessA) {
       messageArray.add("The " + backgroundProcessNameA
       + " process will continue to run after Etomo ends.");
       String osName = System.getProperty("os.name").toLowerCase();
       if (osName.indexOf("linux") == -1 && osName.indexOf("mac os") == -1) {
       messageArray
       .add("Etomo will not be able to warn you if you interfere with this process by running another at the same time.");
       }
       messageArray
       .add("Check " + backgroundProcessNameA + ".log for status.");
       messageArray.add(" ");
       }
       //handle regular processes
       if ((!threadNameA.equals("none") && !backgroundProcessA)
       || !threadNameB.equals("none")) {
       messageArray.add("There are still processes running.");
       messageArray.add("Exiting Etomo now may terminate those processes.");
       }
       if (messageArray.size() > 0) {
       messageArray.add("Do you still wish to exit the program?");
       if (!uiHarness.openYesNoDialog((String[]) messageArray
       .toArray(new String[messageArray.size()]), axisID)) {
       exiting = false;
       return false;
       }
       }*/
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
      catch (SystemProcessException except) {
        except.printStackTrace();
        uiHarness.openMessageDialog(except.getMessage(),
            "Problem closing 3dmod", axisID);
      }
      return true;
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
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
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
          "Unable to retrieve rubberband coordinates from " + imodKey + ".",
          axisID);
    }
    Vector messageArray = new Vector();
    if (results == null) {
      messageArray.add("Unable to retrieve rubberband coordinates from "
          + imodKey + ".");
      return null;
    }
    else {
      boolean success = false;
      String result = null;
      Iterator i = results.iterator();
      while (i.hasNext()) {
        result = (String) i.next();
        if (result.indexOf(ImodProcess.IMOD_SEND_EVENT_STRING) != -1
            || result.indexOf(ProcessMessages.ERROR_TAG) != -1
            || result.indexOf(ProcessMessages.WARNING_TAG) != -1) {
          messageArray.add(result);
          i.remove();
        }
        if (result.indexOf(ImodProcess.RUBBERBAND_RESULTS_STRING) != -1) {
          success = true;
        }
      }
      if (!success) {
        messageArray.add("Unable to retrieve rubberband coordinates from "
            + imodKey + ".");
      }
    }
    if (messageArray.size() > 0) {
      String[] messages = (String[]) messageArray
          .toArray(new String[messageArray.size()]);
      uiHarness.openMessageDialog(messages, "Rubberband Coordinates", axisID);
    }
    return results;
  }

  protected void setPanel() {
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
   * Return the parameter file as a File object
   * @return a File object specifying the data set parameter file.
   */
  public File getParamFile() {
    return paramFile;
  }

  /**
   * A message asking the ApplicationManager to load in the information from the
   * test parameter file.
   * @param paramFile the File object specifiying the data parameter file.
   */
  protected boolean loadTestParamFile(File paramFile, AxisID axisID) {
    FileInputStream processDataStream;
    try {
      // Read in the test parameter data file
      ParameterStore paramStore = new ParameterStore(paramFile);
      paramStore.load(getBaseMetaData());
      Storable[] storable = getParamFileStorableArray(false);
      paramStore.load(storable);

      // Set the current working directory for the application, this is the
      // path to the EDF or EJF file.  The working directory is defined by the current
      // user.dir system property.
      // Uggh, stupid JAVA bug, getParent() only returns the parent if the File
      // was created with the full path
      paramFile = new File(paramFile.getAbsolutePath());
      propertyUserDir = paramFile.getParent();
      // Update the MRU test data filename list
      userConfig.putDataFile(paramFile.getAbsolutePath());
      //  Initialize a new IMOD manager
      setMetaData(imodManager);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not find the test parameter data file:";
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage, "File not found error", axisID);
      return false;
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not read the test parameter data from file:";
      errorMessage[2] = except.getMessage();
      uiHarness.openMessageDialog(errorMessage,
          "Test parameter file read error", axisID);
      return false;
    }
    StringBuffer invalidReason = new StringBuffer();
    if (!Utilities.isValidFile(paramFile, "Parameter file", invalidReason,
        true, true, true, false)) {
      uiHarness.openMessageDialog(invalidReason.toString(), "File Error",
          axisID);
      return false;
    }
    this.paramFile = paramFile;
    return true;
  }

  protected void backupFile(File file, AxisID axisID) {
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

  private void createImodManager() {
    imodManager = new ImodManager(this);
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
      boolean error) {
    processDone(threadName, exitValue, processName, axisID, false, endState,
        null, error);
  }

  public final void processDone(String threadName, int exitValue,
      ProcessName processName, AxisID axisID, boolean forceNextProcess,
      ProcessEndState endState, boolean error) {
    processDone(threadName, exitValue, processName, axisID, forceNextProcess,
        endState, null, error);
  }

  /**
   * Notification message that a background process is done.
   * 
   * @param threadName
   *            The name of the thread that has finished
   */
  public final void processDone(String threadName, int exitValue,
      ProcessName processName, AxisID axisID, boolean forceNextProcess,
      ProcessEndState endState, String statusString, boolean error) {
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
    //  Start the next process if one exists and the exit value was equal zero
    if (isNextProcessSet(axisID) && endState != ProcessEndState.KILLED
        && (exitValue == 0 || forceNextProcess)) {
      startNextProcess(axisID);
    }
    else {
      ProcessResultDisplay processResultDisplay = getProcessResultDisplay(axisID);
      if (error) {
        processResultDisplay.msgProcessFailed();
      }
      else {
        processResultDisplay.msgProcessSucceeded();
      }
      resetNextProcess(axisID);
    }
  }

  /**
   * Keep final.
   * @param axisID
   */
  protected final void startNextProcess(AxisID axisID) {
    String nextProcess = getNextProcess(axisID);
    ProcessResultDisplay processResultDisplay = getProcessResultDisplay(axisID);
    resetNextProcess(axisID);
    processResultDisplay.msgSecondaryProcess();
    startNextProcess(axisID, nextProcess, processResultDisplay);
  }

  /**
   * Keep final.
   * @param axisID
   * @param nextProcess
   */
  protected final void setNextProcess(AxisID axisID, String nextProcess) {
    if (debug) {
      System.err.println("setNextProcess:axisID=" + axisID + ",nextProcess="
          + nextProcess);
    }
    if (axisID == AxisID.SECOND) {
      nextProcessB = nextProcess;
    }
    else {
      nextProcessA = nextProcess;
    }
  }

  /**
   * Keep final.
   * @param axisID
   * @param nextProcess
   */
  protected final void setProcessResultDisplay(AxisID axisID,
      ProcessResultDisplay processResultDisplay) {
    if (debug) {
      System.err.println("setProcessResultDisplay:axisID=" + axisID
          + ",processResultDisplay=" + processResultDisplay);
    }
    if (axisID == AxisID.SECOND) {
      processResultDisplayB = processResultDisplay;
    }
    else {
      processResultDisplayA = processResultDisplay;
    }
  }

  /**
   * Keep private final.
   * @param axisID
   */
  private final void resetNextProcess(AxisID axisID) {
    if (debug) {
      System.err.println("resetNextProcess:axisID=" + axisID);
    }
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

  private final ProcessResultDisplay getProcessResultDisplay(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return processResultDisplayB;
    }
    return processResultDisplayA;
  }

  private final boolean isNextProcessSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !nextProcessB.equals("");
    }
    return !nextProcessA.equals("");
  }

  /**
   * Keep final.
   * @param axisID
   * @param lastProcess
   */
  protected final void setLastProcess(AxisID axisID, String lastProcess) {
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
  protected final void resetLastProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      lastProcessB = "";
    }
    else {
      lastProcessA = "";
    }
  }

  protected final String getLastProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return lastProcessB;
    }
    return lastProcessA;
  }

  protected final boolean isLastProcessSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !lastProcessB.equals("");
    }
    return !lastProcessA.equals("");
  }

  public final void startGetLoadAverage(LoadAverageDisplay display,
      String computer) {
    LoadAverageParam param = LoadAverageParam.getInstance(computer);
    getProcessManager().startGetLoadAverage(param,
        display.getLoadAverageMonitor());
  }

  public final void stopGetLoadAverage(LoadAverageDisplay display,
      String computer) {
    LoadAverageParam param = LoadAverageParam.getInstance(computer);
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
    mainPanel.setProgressBar("Saving defaults", 1, axisID);
    if (!EtomoDirector.getInstance().savePreferences(storable, axisID)) {
      mainPanel.stopProgressBar(axisID, ProcessEndState.FAILED);
    }
    else {
      mainPanel.stopProgressBar(axisID);
    }
  }

  /**
   * Map the thread name to the correct axis
   * 
   * @param name
   *            The name of the thread to assign to the axis
   * @param axisID
   *            The axis of the thread to be mapped
   */
  protected void setThreadName(String name, AxisID axisID) {
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

  public final void resume(AxisID axisID, ProcesschunksParam param) {
    if (param == null) {
      uiHarness.openMessageDialog("No command to resume", "Resume");
      return;
    }
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    parallelPanel.getResumeParameters(param);
    String threadName;
    try {
      threadName = getProcessManager().processchunks(axisID, param,
          parallelPanel);
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
   * Run processchunks.
   * @param axisID
   */
  protected final void processchunks(AxisID axisID, ParallelDialog dialog,
      ProcessResultDisplay processResultDisplay) {
    if (dialog == null) {
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, axisID);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    dialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      processResultDisplay.msgProcessFailedToStart();
      return;
    }
    getProcessTrack().setState(ProcessState.INPROGRESS, axisID, dialog);
    getMainPanel().setState(ProcessState.INPROGRESS, axisID, dialog);
    //param should never be set to resume
    parallelPanel.resetResults();
    String threadName;
    try {
      threadName = getProcessManager().processchunks(axisID, param,
          parallelPanel);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.PROCESSCHUNKS;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(message, "Unable to execute command", axisID);
      processResultDisplay.msgProcessFailedToStart();
      return;
    }
    //set param in parallel panel so it can do a resume
    parallelPanel.setProcesschunksParam(param);
    setThreadName(threadName, axisID);
  }

  /**
   * 
   * @param axisID
   * @param dialog
   */
  public final void setParallelDialog(AxisID axisID, ParallelDialog dialog) {
    getMainPanel().setParallelDialog(axisID, dialog.isParallel());
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
        axisID);
  }
}
/**
 * <p> $Log$
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
