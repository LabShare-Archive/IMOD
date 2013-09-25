package etomo;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import javax.swing.JFileChooser;

import etomo.comscript.CommandDetails;
import etomo.comscript.IntermittentCommand;
import etomo.comscript.ProcesschunksParam;
import etomo.comscript.TomodataplotsParam;
import etomo.process.AxisProcessData;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.ImodqtassistProcess;
import etomo.process.LoadMonitor;
import etomo.process.ProcessResultDisplayFactoryInterface;
import etomo.process.SystemProcessException;
import etomo.process.SystemProcessInterface;
import etomo.process.ProcessData;
import etomo.storage.ChunkComscriptFileFilter;
import etomo.storage.DirectiveMap;
import etomo.storage.LogFile;
import etomo.storage.Loggable;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AutoAlignmentMetaData;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.ConstProcessSeries;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.InterfaceType;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.UserConfiguration;
import etomo.type.ViewType;
import etomo.ui.LogProperties;
import etomo.ui.UIComponent;
import etomo.ui.swing.FileChooser;
import etomo.ui.swing.FixedDim;
import etomo.ui.swing.LogInterface;
import etomo.ui.swing.LogWindow;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.ParallelPanel;
import etomo.ui.swing.ProcessDisplay;
import etomo.ui.swing.UIHarness;
import etomo.util.UniqueKey;
import etomo.util.Utilities;

/**
 * <p>
 * Description: Base class for ApplicationManager and JoinManager
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002 - 2005
 * </p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3-Dimensional Electron Microscopy of
 * Cells (BL3DEM), University of Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public abstract class BaseManager {
  public static final String rcsid = "$Id$";

  // static variables
  private static boolean headless = false;
  // proected MainFrame mainFrame = null;
  UIHarness uiHarness = UIHarness.INSTANCE;
  static UserConfiguration userConfig = EtomoDirector.INSTANCE.getUserConfiguration();
  boolean loadedParamFile = false;
  // imodManager manages the opening and closing closing of imod(s), message
  // passing for loading model
  final ImodManager imodManager;
  File paramFile = null;
  // FIXME homeDirectory may not have to be visible
  String homeDirectory;
  String threadNameA = "none";
  String threadNameB = "none";
  boolean backgroundProcessA = false;
  String backgroundProcessNameA = null;
  String propertyUserDir = null;// working directory for this manager
  // private static variables
  private boolean debug = false;
  private boolean exiting = false;
  private boolean initialized = false;
  private DialogType currentDialogTypeA = null;
  private DialogType currentDialogTypeB = null;
  private ParameterStore parameterStore = null;
  // True if reconnect() has been run for the specified axis.
  private boolean reconnectRunA = false;
  private boolean reconnectRunB = false;
  private final ProcessingMethodMediator processingMethodMediatorA = new ProcessingMethodMediator();
  private final ProcessingMethodMediator processingMethodMediatorB = new ProcessingMethodMediator();
  private final ManagerKey managerKey = new ManagerKey();
  final AxisProcessData axisProcessData = new AxisProcessData(this);
  private final ResumeData resumeDataA = new ResumeData();
  private final ResumeData resumeDataB = new ResumeData();
  final LogWindow logWindow = createLogWindow();

  public void dumpState() {
    System.err.println("[headless:" + headless + ",loadedParamFile:" + loadedParamFile
        + ",paramFile:");
    if (paramFile != null) {
      System.err.println(paramFile.getAbsolutePath() + ",");
    }
    System.err.println("homeDirectory:" + homeDirectory + ",threadNameA:" + threadNameA
        + ",\nthreadNameB:" + threadNameB + ",backgroundProcessA:" + backgroundProcessA
        + ",\nbackgroundProcessNameA:" + backgroundProcessNameA + ",propertyUserDir:"
        + propertyUserDir + ",\ndebug:" + debug + ",exiting:" + exiting + ",initialized:"
        + initialized + ",\ncurrentDialogTypeA:" + currentDialogTypeA
        + ",\ncurrentDialogTypeB:" + currentDialogTypeB + ",reconnectRunA:"
        + reconnectRunA + ",\nreconnectRunA:" + reconnectRunA + ",reconnectRunB:"
        + reconnectRunB + ",\nmanagerKey:" + managerKey + "]");
  }

  abstract public InterfaceType getInterfaceType();

  abstract void createMainPanel();

  public abstract BaseMetaData getBaseMetaData();

  public abstract MainPanel getMainPanel();

  public abstract BaseProcessManager getProcessManager();

  abstract Storable[] getStorables(int offset);

  public abstract String getName();

  /**
   * Return the subdirectory of the dataset location where some of the files are
   * stored. This is necessary the NAD manager. Return null if there is no
   * subdirectory.
   * 
   * @return
   */
  public String getFileSubdirectoryName() {
    return null;
  }

  public BaseManager() {
    propertyUserDir = System.getProperty("user.dir");
    createProcessTrack();
    createComScriptManager();
    // Initialize the program settings
    debug = EtomoDirector.INSTANCE.getArguments().isDebug();
    debug = true;
    headless = EtomoDirector.INSTANCE.getArguments().isHeadless();
    createMainPanel();
    imodManager = new ImodManager(this);
    initProgram();
  }

  public int getParallelProcessingDefaultNice() {
    return 15;
  }

  public String toString() {
    return "[" + paramString() + "]";
  }

  String paramString() {
    return getName();
  }

  public AxisProcessData getAxisProcessData() {
    return axisProcessData;
  }

  LogWindow createLogWindow() {
    return LogWindow.getInstance();
  }

  public void showHideLog() {
    if (logWindow != null) {
      logWindow.showHide();
    }
  }

  public LogInterface getLogInterface() {
    return logWindow;
  }

  public LogProperties getLogProperties() {
    return logWindow;
  }

  private void initProgram() {
    System.err.println("propertyUserDir:  " + propertyUserDir);
  }

  public String getPropertyUserDir() {
    return propertyUserDir;
  }

  public boolean canChangeParamFileName() {
    return false;
  }

  public boolean canSaveDirectives() {
    return false;
  }

  void createComScriptManager() {
  }

  void createProcessTrack() {
  }

  public ViewType getViewType() {
    return ViewType.DEFAULT;
  }

  public BaseScreenState getBaseScreenState(final AxisID axisID) {
    return null;
  }

  public BaseState getBaseState() {
    return null;
  }

  public ProcessResultDisplayFactoryInterface getProcessResultDisplayFactoryInterface(
      final AxisID axisID) {
    return null;
  }

  BaseProcessTrack getProcessTrack() {
    return null;
  }

  void getProcessTrack(final Storable[] storable, final int index) {
  }

  public boolean isInManagerFrame() {
    return false;
  }

  AutoAlignmentMetaData getAutoAlignmentMetaData() {
    return null;
  }

  public String getStatus() {
    return getMainPanel().getStatus();
  }

  public boolean updateMetaData(final DialogType dialogType, final AxisID axisID,
      final boolean doValidation) {
    return false;
  }

  /**
   * Interrupt the currently running thread for this axis
   * 
   * @param axisID
   */
  public void kill(final AxisID axisID) {
    getProcessManager().kill(axisID);
  }

  public void pause(final AxisID axisID) {
    getProcessManager().pause(axisID);
  }

  void processSucceeded(final AxisID axisID, final ProcessName processName) {
  }

  /**
   * In most managers the param file should already be set.
   * @return
   */
  public boolean setParamFile() {
    return loadedParamFile;
  }

  public boolean isSetupDone() {
    return loadedParamFile;
  }

  public boolean setParamFile(final File paramFile) {
    this.paramFile = paramFile;
    return true;
  }

  /**
   * Returns true if a process was started.
   * @param uiComponent
   * @param axisID
   * @param process
   * @param processResultDisplay
   * @param processSeries
   * @param dialogType
   * @param display
   * @return
   */
  boolean startNextProcess(final UIComponent uiComponent, final AxisID axisID,
      final ProcessSeries.Process process,
      final ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      final DialogType dialogType, final ProcessDisplay display) {
    ResumeData resumeData = axisID == AxisID.SECOND ? resumeDataB : resumeDataA;
    if (process.equals(Task.RESUME)) {
      resume(axisID);
      return true;
    }
    TaskInterface task = process.getTask();
    if (task instanceof TomodataplotsParam.Task) {
      tomodataplots(task, axisID, processSeries);
      return true;
    }
    return false;
  }

  void updateDialog(final ProcessName processName, final AxisID axisID) {
  }

  public void logMessage(final File file) {
    if (file == null || !file.exists() || file.isDirectory() || !file.canRead()) {
      return;
    }
    LogInterface logInterface = getLogInterface();
    if (logInterface != null) {
      logInterface.logMessage(file);
    }
    else {
      System.err.println("Logging from file: " + file.getAbsolutePath());
      try {
        LogFile logFile = LogFile.getInstance(file);
        LogFile.ReaderId id = logFile.openReader();
        String line = null;
        while ((line = logFile.readLine(id)) != null) {
          System.err.println(line);
        }
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
        System.err.println("Unable to log from file.  " + e.getMessage());
      }
      catch (FileNotFoundException e) {
        e.printStackTrace();
        System.err.println("Unable to log from file.  " + e.getMessage());
      }
      catch (IOException e) {
        e.printStackTrace();
        System.err.println("Unable to log from file.  " + e.getMessage());
      }
    }
  }

  public void logMessage(Loggable loggable, AxisID axisID) {
    LogInterface logInterface = getLogInterface();
    if (logInterface != null) {
      logInterface.logMessage(loggable, axisID);
    }
    else {
      try {
        List messageList = loggable.getLogMessage();
        System.err.println(Utilities.getDateTimeStamp());
        for (int i = 0; i < messageList.size(); i++) {
          System.err.println(messageList.get(i));
        }
      }
      catch (IOException e) {
        e.printStackTrace();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
    }
  }

  public ProcessingMethodMediator getProcessingMethodMediator(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return processingMethodMediatorB;
    }
    return processingMethodMediatorA;
  }

  public void logMessage(String[] message, String title, AxisID axisID) {
    LogInterface logInterface = getLogInterface();
    if (logInterface != null) {
      logInterface.logMessage(title, axisID, message);
    }
    else {
      System.err.println(Utilities.getDateTimeStamp() + "\n" + title + " - " + axisID
          + " axis:");
      for (int i = 0; i < message.length; i++) {
        System.err.println(message[i]);
      }
    }
  }

  public void logMessageWithKeyword(final String[] message, final String keyword,
      final String title, final AxisID axisID) {
    LogInterface logInterface = getLogInterface();
    if (message == null) {
      return;
    }
    boolean loggedTitle = false;
    for (int i = 0; i < message.length; i++) {
      if (message[i].indexOf(keyword) != -1) {
        if (!loggedTitle) {
          loggedTitle = true;
          if (logInterface != null) {
            logInterface.logMessage(title, axisID);
          }
          else {
            System.err.println(Utilities.getDateTimeStamp() + "\n" + title + " - "
                + axisID + " axis:");
          }
        }
        if (logInterface != null) {
          logInterface.logMessage(message[i]);
        }
        else {
          System.err.println(message[i]);
        }
      }
    }
  }

  public void logMessageWithKeyword(final FileType fileType, final String keyword,
      final String title, final AxisID axisID) {
    LogInterface logInterface = getLogInterface();
    if (fileType == null) {
      return;
    }
    try {
      LogFile logFile = LogFile.getInstance(propertyUserDir,
          fileType.getFileName(this, axisID));
      LogFile.ReaderId id = logFile.openReader();
      if (id == null || id.isEmpty()) {
        return;
      }
      boolean loggedTitle = false;
      String line = logFile.readLine(id);
      while (line != null) {
        if (line.indexOf(keyword) != -1) {
          if (!loggedTitle) {
            loggedTitle = true;
            if (logInterface != null) {
              logInterface.logMessage(title, axisID);
            }
            else {
              System.err.println(Utilities.getDateTimeStamp() + "\n" + title + " - "
                  + axisID + " axis:");
            }
          }
          if (logInterface != null) {
            logInterface.logMessage(line);
          }
          else {
            System.err.println(line);
          }
        }
        line = logFile.readLine(id);
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    catch (FileNotFoundException e) {
    }
    catch (IOException e) {
    }
  }

  public void updateDirectiveMap(final DirectiveMap directiveMap,
      final StringBuffer errmsg) {
  }

  public void logMessage(List<String> message, String title, AxisID axisID) {
    LogInterface logInterface = getLogInterface();
    if (logInterface != null) {
      logInterface.logMessage(title, axisID, message);
    }
    else {
      System.err.println(Utilities.getDateTimeStamp() + "\n" + title + " - " + axisID
          + " axis:");
      Iterator<String> i = message.iterator();
      while (i.hasNext()) {
        System.err.println(i.next());
      }
    }
  }

  public void saveLog() {
    LogInterface logInterface = getLogInterface();
    if (logInterface != null) {
      logInterface.save();
    }
  }

  void setManagerKey(UniqueKey uniqueKey) {
    managerKey.setKey(uniqueKey);
  }

  ManagerKey getManagerKey() {
    return managerKey;
  }

  public Component getFocusComponent() {
    return null;
  }

  public String setPropertyUserDir(String propertyUserDir) {
    // avoid empty strings
    if (propertyUserDir.matches("\\s*")) {
      propertyUserDir = null;
    }
    Utilities.managerStamp(propertyUserDir, null);
    String oldPropertyUserDir = this.propertyUserDir;
    this.propertyUserDir = propertyUserDir;
    return oldPropertyUserDir;
  }

  public void pack() {
  }

  void initializeUIParameters(File dataFile, AxisID axisID,
      boolean loadedFromADifferentFile) {
    if (!headless) {
      if (dataFile != null) {
        loadedParamFile = loadParamFile(dataFile, axisID, loadedFromADifferentFile);
      }
    }
    initialized = true;
  }

  void initializeUIParameters(String paramFileName, AxisID axisID) {
    if (paramFileName == null || paramFileName.equals("")) {
      initializeUIParameters((File) null, axisID, false);
    }
    else {
      initializeUIParameters(new File(paramFileName), axisID, false);
    }
  }

  /**
   * Save storable to the data file.
   * 
   * @param axisID
   * @param processData
   */
  public void saveStorable(AxisID axisID, Storable storable) {
    try {
      getParameterStore();
      if (parameterStore == null) {
        return;
      }
      parameterStore.setAutoStore(true);
      parameterStore.save(storable);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Unable to save or write to properties.  " + e.getMessage(), "Etomo Error",
          axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Unable to save or write to properties.  " + e.getMessage(), "Etomo Error",
          axisID);
    }
  }

  boolean saveMetaDataToParameterStore() {
    try {
      ParameterStore parameterStore = getParameterStore();
      if (parameterStore == null) {
        return false;
      }
      parameterStore.save(getBaseMetaData());
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Cannot save or write to metaData.\n" + e.getMessage(), "Etomo Error");
    }
    catch (IOException e) {
      uiHarness.openMessageDialog(this,
          "Cannot save or write to metaData.\n" + e.getMessage(), "Etomo Error");
    }
    return true;
  }

  /**
   * Save etomo to parametersState by asking the child manager for a list of
   * storable objects. This is used when storable objects may have been changes
   * and nothing has been saved (update comscript functions). This will often do
   * unnecessary saves but it guarentees that everything will be saved.
   * 
   * @throws IOException
   */
  public void saveStorables(AxisID axisID) {
    try {
      getParameterStore();
      if (parameterStore == null) {
        return;
      }
      parameterStore.setAutoStore(false);
      Storable[] storables = getStorables();
      if (storables == null) {
        return;
      }
      for (int i = 0; i < storables.length; i++) {
        parameterStore.save(storables[i]);
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Unable to save or write to properties.  " + e.getMessage(), "Etomo Error",
          axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Unable to save or write to properties.  " + e.getMessage(), "Etomo Error",
          axisID);
    }
    parameterStore.setAutoStore(true);
    try {
      parameterStore.storeProperties();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(
          this,
          "Unable to save or write to " + paramFile.getAbsolutePath() + ".  "
              + e.getMessage(), "Etomo Error", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(
          this,
          "Unable to save or write to " + paramFile.getAbsolutePath() + ".  "
              + e.getMessage(), "Etomo Error", axisID);
    }
  }

  /**
   * Get the storable objects from the child and base manager.
   * 
   * @return
   */
  private Storable[] getStorables() {
    Storable[] storables = getStorables(2);
    if (storables == null) {
      // Manager does not have a data file.
      return null;
    }
    storables[0] = getProcessManager().getProcessData(AxisID.FIRST);
    storables[1] = getProcessManager().getProcessData(AxisID.SECOND);
    return storables;
  }

  /**
   * Save etomo to parameterStore by asking the child manager to save its state.
   * This is used when using the done functionality of the dialogs (file save
   * and exit).
   * 
   * @throws IOException
   */
  boolean save() throws LogFile.LockException, LogFile.LockException, IOException {
    if (parameterStore == null) {
      return false;
    }
    parameterStore.setAutoStore(false);
    parameterStore.save(getProcessManager().getProcessData(AxisID.FIRST));
    parameterStore.save(getProcessManager().getProcessData(AxisID.SECOND));
    return true;
  }

  public boolean saveToFile() {
    return false;
  }

  public boolean saveAsToFile() {
    return false;
  }

  public boolean closeFrame() {
    return false;
  }

  /**
   * A message asking the ApplicationManager to save the parameter information
   * to a file.
   */
  public boolean saveParamFile() throws LogFile.LockException, IOException {
    if (!isSetupDone()) {
      return false;
    }
    setParamFile();
    if (getParameterStore() == null) {
      return false;
    }
    if (!EtomoDirector.INSTANCE.isMemoryAvailable()) {
      return true;
    }
    save();
    parameterStore.setAutoStore(true);
    parameterStore.storeProperties();
    saveStorables(AxisID.ONLY);
    // Update the MRU test data filename list
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
   * Creates parameterStore if it doesn't already exist. Return null if
   * paramFile is null.
   * 
   * @return
   */
  public ParameterStore getParameterStore() throws LogFile.LockException {
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
      parameterStore = ParameterStore.getInstance(paramFile);
      return parameterStore;
    }
  }

  void endThreads() {
    imodManager.stopRequestHandler();
    ProcessingMethodMediator mediator = getProcessingMethodMediator(AxisID.FIRST);
    if (mediator != null) {
      mediator.msgExiting();
    }
    mediator = getProcessingMethodMediator(AxisID.SECOND);
    if (mediator != null) {
      mediator.msgExiting();
    }
    if (parameterStore != null) {
      parameterStore.setAutoStore(false);
    }
  }

  /**
   * @param axisID
   */
  public void progressBarDone(AxisID axisID, ProcessEndState processEndState) {
    getMainPanel().stopProgressBar(axisID, processEndState);
  }

  private boolean checkNextProcess(AxisID axisID) {
    BaseProcessManager processManager = getProcessManager();
    if (processManager != null) {
      SystemProcessInterface processA = axisProcessData.getThread(AxisID.FIRST);
      SystemProcessInterface processB = axisProcessData.getThread(AxisID.SECOND);
      // Check to see if next processes have to be done
      ArrayList messageArray = new ArrayList();
      ConstProcessSeries processSeriesA = null;
      ConstProcessSeries processSeriesB = null;
      boolean droppedProcessA = false;
      boolean droppedProcessB = false;
      if (processA != null && (processSeriesA = processA.getProcessSeries()) != null) {
        droppedProcessA = processSeriesA.willProcessBeDropped(processManager
            .getProcessData(AxisID.FIRST));
      }
      if (processB != null && (processSeriesB = processB.getProcessSeries()) != null) {
        droppedProcessB = processSeriesB.willProcessBeDropped(processManager
            .getProcessData(AxisID.SECOND));
      }
      if (droppedProcessA || droppedProcessB) {
        StringBuffer message = new StringBuffer(
            "WARNING!!!\nIf you exit now then not all processes will complete.");
        if (exiting) {
          message.append("Do you still wish to exit the program?");
        }
        else {
          message.append("Do you still wish to close this interface?");
        }
        if (!uiHarness.openYesNoWarningDialog(this, message.toString(), axisID)) {
          exiting = false;
          return false;
        }
      }
      if (!checkUnidentifiedProcess(AxisID.FIRST)
          || !checkUnidentifiedProcess(AxisID.SECOND)) {
        exiting = false;
        return false;
      }
    }
    return true;
  }

  /**
   * Renames an image file. Pops up an error message and returns if the from
   * file doesn't exist. If either the from file type or the to file type have
   * imod manager key(s), calls closeImod to tell the user to close the file(s)
   * before renaming them.
   * 
   * @param fromFileType
   * @param toFileType
   * @param axisID
   */
  public void renameImageFile(FileType fromFileType, FileType toFileType, AxisID axisID)
      throws IOException {
    if (fromFileType == null || toFileType == null) {
      return;
    }
    if (!fromFileType.getFile(this, axisID).exists()) {
      uiHarness.openMessageDialog(this, "Unable to rename file.  "
          + fromFileType.getFile(this, axisID).getName() + " doesn't exist.",
          "Entry Error", axisID);
      return;
    }
    closeImod(fromFileType, axisID, true);
    closeImod(toFileType, axisID, true);
    Utilities.renameFile(fromFileType.getFile(this, axisID),
        toFileType.getFile(this, axisID));
  }

  /**
   * Renames an image file where the from file type doesn't contain the file
   * name. Pops up an error message and returns if the from file doesn't exist.
   * If either the from file type or the to file type have imod manager key(s),
   * calls closeImod to tell the user to close the file(s) before renaming them.
   * 
   * @param fromFileType
   * @param toFileType
   * @param axisID
   */
  void renameImageFile(final FileType fromFileType, final File fromFile,
      final FileType toFileType, final AxisID axisID, final boolean useFileNameInClose)
      throws IOException {
    if (fromFileType == null || toFileType == null) {
      return;
    }
    if (!fromFile.exists()) {
      uiHarness.openMessageDialog(this, "Unable to rename file.  " + fromFile.getName()
          + " doesn't exist.", "Entry Error", axisID);
      return;
    }
    if (useFileNameInClose) {
      closeImod(fromFileType, fromFile, axisID, true);
    }
    else {
      closeImod(fromFileType, axisID, true);
    }
    closeImod(toFileType, axisID, true);
    Utilities.renameFile(fromFile, toFileType.getFile(this, axisID));
  }

  /**
   * Renames an image file to image_file_name~. Does nothing if the file doesn't
   * exist. If the file type has imod manager key(s), calls closeImod to tell
   * the user to close the file before renaming it.
   * 
   * @param fromFileType
   * @param toFileType
   * @param axisID
   */
  public void backupImageFile(FileType fileType, AxisID axisID) throws IOException {
    if (fileType == null || !fileType.getFile(this, axisID).exists()) {
      return;
    }
    closeImod(fileType, axisID, true);
    Utilities.backupFile(fileType.getFile(this, axisID));
  }

  /**
   * Asks to close a stale file
   * 
   * @param fileType
   * @param axisID
   */
  public void closeStaleFile(FileType fileType, AxisID axisID) {
    closeImod(fileType, axisID, true);
  }

  /**
   * Ask to close all 3dmods associated with this file type. A file type may
   * have multiple 3dmod keys.
   * 
   * @param fileType
   * @param axisID
   * @param warnOnce -
   *          when true, only one warning is give for the open 3dmod instance
   */
  public void closeImod(FileType fileType, AxisID axisID, boolean warnOnce) {
    if (fileType == null) {
      return;
    }
    closeImod(fileType.getImodManagerKey(this), axisID,
        fileType.getFileName(this, axisID), warnOnce);
    closeImod(fileType.getImodManagerKey2(this), axisID,
        fileType.getFileName(this, axisID), warnOnce);
  }

  /**
   * Ask to close all 3dmods associated with this file type and file. A file
   * type may have multiple 3dmod keys.
   * 
   * @param fileType
   * @param axisID
   * @param stale -
   *          when true, only one warning is give for the open 3dmod instance
   */
  private void closeImod(FileType fileType, File file, AxisID axisID, boolean warnOnce) {
    if (fileType == null) {
      return;
    }
    closeImod(fileType.getImodManagerKey(this), file.getName(), axisID,
        fileType.getDescription(this), warnOnce);
    closeImod(fileType.getImodManagerKey2(this), file.getName(), axisID,
        fileType.getImodManagerKey2(this), warnOnce);
  }

  /**
   * Ask to close all 3dmods associated with this file type and file. A file
   * type may have multiple 3dmod keys.
   * 
   * @param fileType
   * @param axisID
   * @param stale -
   *          when true, only one warning is give for the open 3dmod instance
   */
  void closeImod(FileType fileType, String fileName, AxisID axisID, boolean warnOnce) {
    if (fileType == null) {
      return;
    }
    closeImod(fileType.getImodManagerKey(this), fileName, axisID,
        fileType.getDescription(this), warnOnce);
    closeImod(fileType.getImodManagerKey2(this), fileName, axisID,
        fileType.getImodManagerKey2(this), warnOnce);
  }

  public boolean closeImod(String key, AxisID axisID, String description, boolean warnOnce) {
    return closeImod(key, axisID, description, null, warnOnce);
  }

  /**
   * Returns a group of up to three files.
   * 
   * @param key1
   * @param key2
   * @param key3
   * @param axisID
   * @param fullMessage
   * @return true if files where closed or files did not need to be closed
   */
  public boolean closeImods(String key1, String key2, String key3, AxisID axisID,
      String fullMessage) {
    if (EtomoDirector.INSTANCE.getArguments().isTest()) {
      System.err.println("closeImods:key1:" + key1 + "key2:" + key2 + "key3:" + key3
          + ",axisID:" + axisID + ",fullMessage:" + fullMessage);
    }
    if (key1 == null && key2 == null && key3 == null) {
      return true;
    }
    try {
      if ((key1 != null && imodManager.isOpen(key1, axisID))
          || (key2 != null && imodManager.isOpen(key2, axisID))
          || (key3 != null && imodManager.isOpen(key3, axisID))) {
        if (!EtomoDirector.INSTANCE.getArguments().isAutoClose3dmod()) {
          if (uiHarness.openYesNoDialog(null, fullMessage, axisID)) {
            if (key1 != null) {
              imodManager.quit(key1, axisID);
            }
            if (key2 != null) {
              imodManager.quit(key2, axisID);
            }
            if (key3 != null) {
              imodManager.quit(key3, axisID);
            }
            releaseFile();
            return true;
          }
        }
        else {
          if (key1 != null) {
            imodManager.quit(key1, axisID);
          }
          if (key2 != null) {
            imodManager.quit(key2, axisID);
          }
          if (key3 != null) {
            imodManager.quit(key3, axisID);
          }
          releaseFile();
          return true;
        }
      }
      else {
        return true;
      }
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "System Process Exception",
          axisID);
    }
    return false;
  }

  /**
   * Close the 3dmod instance denoted by key and axisID if either the
   * --autoclose3dmod param was passed to etomo, or the user wants the 3dmod to
   * be closed.
   * 
   * If stale is true then the popup can only be display once for this instance
   * of 3dmod. Also the pop message will include a warning that the file
   * displayed in 3dmod will be out of date.
   * 
   * @param key
   * @param axisID
   * @param description
   * @param stale
   * @return true if file was closed or file did not need to be closed
   */
  public boolean closeImod(String key, AxisID axisID, String description, String message,
      boolean warnOnce) {
    if (EtomoDirector.INSTANCE.getArguments().isTest()) {
      System.err.println("closeImod:key:" + key + ",axisID:" + axisID + ",description:"
          + description + ",message:" + message);
    }
    if (key == null) {
      return true;
    }
    try {
      if (imodManager.isOpen(key, axisID)) {
        if (!EtomoDirector.INSTANCE.getArguments().isAutoClose3dmod()) {
          if (!warnOnce || (warnOnce && imodManager.warnStaleFile(key, axisID))) {
            if (uiHarness
                .openYesNoDialog(
                    null,
                    description
                        + " is open in 3dmod."
                        + (warnOnce ? "  This 3dmod instance will display an out of date version of this file.  "
                            : "") + (message == null ? "" : "  " + message + "  ")
                        + "Should file be closed?", axisID)) {
              imodManager.quit(key, axisID);
              releaseFile();
              return true;
            }
          }
        }
        else {
          imodManager.quit(key, axisID);
          releaseFile();
          return true;
        }
      }
      else {
        return true;
      }
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "System Process Exception",
          axisID);
    }
    return false;
  }

  /**
   * Close the 3dmod instance denoted by key and axisID if either the
   * --autoclose3dmod param was passed to etomo, or the user wants the 3dmod to
   * be closed.
   * 
   * If stale is true then the popup can only be display once for this instance
   * of 3dmod. Also the pop message will include a warning that the file
   * displayed in 3dmod will be out of date.
   * 
   * @param key
   * @param axisID
   * @param description
   * @param stale
   */
  private void closeImod(String key, String fileName, AxisID axisID, String description,
      boolean stale) {
    if (EtomoDirector.INSTANCE.getArguments().isTest()) {
      System.err.println("closeImod:key:" + key + ",fileName:" + fileName + ",axisID:"
          + axisID + ",description:" + description + ",stale:" + stale);
    }
    if (key == null) {
      return;
    }
    try {
      if (imodManager.isOpen(key, axisID, fileName)) {
        if (!EtomoDirector.INSTANCE.getArguments().isAutoClose3dmod()) {
          if (!stale || (stale && imodManager.warnStaleFile(key, axisID))) {
            String[] message = new String[2];
            message[0] = description
                + " "
                + fileName
                + " is open in 3dmod."
                + (stale ? "  This 3dmod instance will display an out of date version of this file.  "
                    : "");
            message[1] = "Should it be closed?";
            if (uiHarness.openYesNoDialog(null, message, axisID)) {
              imodManager.quit(key, axisID, fileName);
              releaseFile();
            }
          }
        }
        else {
          imodManager.quit(key, axisID, fileName);
          releaseFile();
        }
      }
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "System Process Exception",
          axisID);
    }
  }

  void releaseFile() {
    if (!Utilities.isWindowsOS()) {
      // Nothing to do
      return;
    }
    // Give Windows a chance to release control of the file.
    try {
      if (debug) {
        System.err.println("Waiting for Windows file lock to be released.");
      }
      Thread.sleep(3000);
    }
    catch (InterruptedException e) {
    }
  }

  private void close3dmods(AxisID axisID) {
    if (EtomoDirector.INSTANCE.getArguments().isTest()) {
      System.err.println("close3dmods:axisID:" + axisID);
    }
    // Should we close the 3dmod windows
    try {
      if (imodManager.isOpen()) {
        if (!EtomoDirector.INSTANCE.getArguments().isAutoClose3dmod()) {
          String[] message = new String[3];
          message[0] = "There are still 3dmod programs running.";
          message[1] = "Do you wish to end these programs?";
          if (uiHarness.openYesNoDialog(null, message, axisID)) {
            imodManager.quit();
            releaseFile();
          }
        }
        else {
          imodManager.quit();
          releaseFile();
        }
      }
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
    }
  }

  private void disconnect3dmods() {
    try {
      imodManager.disconnect();
    }
    catch (Throwable e) {
      e.printStackTrace();
    }
  }

  /**
   * 
   * @param axisID
   * @return
   */
  boolean close(AxisID axisID) {
    if (!checkNextProcess(axisID)) {
      return false;
    }
    close3dmods(axisID);
    disconnect3dmods();
    return true;
  }

  /**
   * Exit the program. To guarantee that etomo can always exit, catch all
   * unrecognized Exceptions and Errors and return true.
   */
  boolean exitProgram(AxisID axisID) {
    exiting = true;
    try {
      // Check for processes that will die if etomo exits
      BaseProcessManager processManager = getProcessManager();
      if (processManager != null) {
        SystemProcessInterface processA = axisProcessData.getThread(AxisID.FIRST);
        SystemProcessInterface processB = axisProcessData.getThread(AxisID.SECOND);
        boolean nohupA = processA == null || processA.isNohup();
        boolean nohupB = processB == null || processB.isNohup();
        if (!nohupA || !nohupB) {
          if (!uiHarness
              .openYesNoWarningDialog(
                  this,
                  "WARNING!!\nThere is process running which will stop if eTomo exits.\nDo you still wish to exit the program?"
                      .toString(), axisID)) {
            exiting = false;
            return false;
          }
        }
      }
      if (!checkNextProcess(axisID)) {
        return false;
      }
      close3dmods(axisID);
      ImodqtassistProcess.INSTANCE.quit();
    }
    catch (Throwable e) {
      e.printStackTrace();
    }
    // Do this even if everything else fails
    disconnect3dmods();
    return true;
  }

  private boolean checkUnidentifiedProcess(AxisID axisID) {
    SystemProcessInterface thread = axisProcessData.getThread(axisID);
    if (thread == null) {
      return true;
    }
    ProcessData processData = thread.getProcessData();
    if (processData != null && processData.isEmpty()) {
      if (!uiHarness
          .openYesNoWarningDialog(
              this,
              "There currently is an unidentified process running.\nPlease wait a few seconds while it is identified.\n\nExit without waiting?",
              axisID)) {
        return false;
      }
    }
    return true;
  }

  /**
   * Check if the current data set is a dual axis data set
   * 
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

  public boolean isExiting() {
    return exiting;
  }

  public Vector imodGetRubberbandCoordinates(String imodKey, AxisID axisID) {
    Vector results = null;
    try {
      results = imodManager.getRubberbandCoordinates(imodKey);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "System Process Exception",
          AxisID.ONLY);
    }
    return results;
  }

  void setPanel() {
    uiHarness.pack(this);
    // Resize to the users preferrred window dimensions
    getMainPanel().setSize(
        new Dimension(userConfig.getMainWindowWidth(), userConfig.getMainWindowHeight()));
    uiHarness.doLayout(this);
    uiHarness.validate(this);
    if (isDualAxis()) {
      getMainPanel().setDividerLocation(0.51);
    }
  }

  // get functions

  /**
   * Return the absolute IMOD bin path
   * 
   * @return
   */
  public static String getIMODBinPath() {
    return EtomoDirector.INSTANCE.getIMODDirectory().getAbsolutePath() + File.separator
        + "bin" + File.separator;
  }

  /**
   * Open or raise a specific 3dmod to view a file with binning. Or open a new
   * 3dmod. Return the index of the 3dmod opened or raised.
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
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "Can't open " + imodKey
          + " 3dmod with imodIndex=" + imodIndex, AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", AxisID.ONLY);
    }
    return imodIndex;
  }

  /**
   * Open or raise a specific 3dmod to view a file with a model. Or open a new
   * 3dmod. Return the index of the 3dmod opened or raised.
   */
  public int imodOpen(String imodKey, int imodIndex, String absoluteFilePath,
      String absoluteModelPath, Run3dmodMenuOptions menuOptions) {
    File file = new File(absoluteFilePath);
    try {
      if (imodIndex == -1) {
        imodIndex = imodManager.newImod(imodKey, file);
      }
      imodManager.open(imodKey, imodIndex, absoluteModelPath, true, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "Can't open " + imodKey
          + " 3dmod with imodIndex=" + imodIndex, AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", AxisID.ONLY);
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
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "Can't open " + imodKey
          + " in 3dmod ", AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", AxisID.ONLY);
    }
  }

  public void imodOpen(AxisID axisID, String imodKey, String model,
      Run3dmodMenuOptions menuOptions, boolean modelMode) {
    try {
      imodManager.open(imodKey, axisID, model, modelMode, menuOptions);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem", axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "Can't open " + imodKey
          + " in 3dmod ", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", axisID);
    }
  }

  /**
   * Return the parameter file as a File object
   * 
   * @return a File object specifying the data set parameter file.
   */
  public File getParamFile() {
    return paramFile;
  }

  /**
   * Loads storables, sets the param file, and sets up the ImodManager. Loads
   * the meta data object first, and then loads all the storables. Set
   * loadedFromADIfferentFile to true if duplicating or extracting data from
   * another param file to create this project. In this case storables will not
   * be loaded.
   * 
   * @param paramFile
   *          the File object specifiying the data parameter file.
   * @param loadedFromADifferentFile
   *          True if duplicating or extracting data from another param file to
   *          create this project.
   */
  boolean loadParamFile(File paramFile, AxisID axisID, boolean loadedFromADifferentFile) {
    FileInputStream processDataStream;
    // Set the current working directory for the application, this is the
    // path to the EDF or EJF file. The working directory is defined by the
    // current
    // user.dir system property.
    // Uggh, stupid JAVA bug, getParent() only returns the parent if the File
    // was created with the full path
    paramFile = new File(paramFile.getAbsolutePath());
    String paramFileParent = paramFile.getParent();
    if (paramFileParent.endsWith(" ")) {
      uiHarness.openMessageDialog(this, "The directory, " + paramFileParent
          + ", cannot be used because it ends with a space.", "Unusable Directory Name",
          AxisID.ONLY);
      return false;
    }
    propertyUserDir = paramFileParent;
    StringBuffer invalidReason = new StringBuffer();
    if (!Utilities.isValidFile(paramFile, "Parameter file", invalidReason, true, true,
        true, false)) {
      uiHarness.openMessageDialog(this, invalidReason.toString(), "File Error", axisID);
      return false;
    }
    this.paramFile = paramFile;
    if (!loadedFromADifferentFile) {
      // Read in the test parameter data file
      try {
        getParameterStore();
        if (parameterStore == null) {
          return false;
        }
        // must load meta data before other storables can be constructed
        parameterStore.load(getBaseMetaData());
        Storable[] storables = getStorables();
        if (storables != null) {
          for (int i = 0; i < storables.length; i++) {
            parameterStore.load(storables[i]);
          }
        }
      }
      catch (LogFile.LockException except) {
        except.printStackTrace();
        String[] errorMessage = new String[3];
        errorMessage[0] = "Test parameter file read error";
        errorMessage[1] = "Could not find the test parameter data file:";
        errorMessage[2] = except.getMessage();
        uiHarness.openMessageDialog(this, errorMessage, "Etomo Error", axisID);
        return false;
      }
    }
    // Update the MRU test data filename list
    userConfig.putDataFile(paramFile.getAbsolutePath());
    return true;
  }

  boolean backupFile(File file, AxisID axisID) {
    if (file != null && file.exists()) {
      File backupFile = new File(file.getAbsolutePath() + "~");
      try {
        Utilities.renameFile(file, backupFile);
      }
      catch (IOException except) {
        System.err.println("Unable to backup file: " + file.getAbsolutePath() + " to "
            + backupFile.getAbsolutePath());
        uiHarness.openMessageDialog(this, except.getMessage(), "File Rename Error",
            axisID);
        return false;
      }
    }
    return true;
  }

  /**
   * Stop progress bar and start next process.
   * 
   * @param threadName
   * @param exitValue
   * @param processName
   * @param axisID
   */
  public final void processDone(String threadName, int exitValue,
      ProcessName processName, AxisID axisID, ProcessEndState endState, boolean failed,
      ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      boolean nonBlocking) {
    processDone(threadName, exitValue, processName, axisID, false, endState, null,
        failed, processResultDisplay, processSeries, nonBlocking);
  }

  public final void processDone(String threadName, int exitValue,
      ProcessName processName, AxisID axisID, boolean forceNextProcess,
      ProcessEndState endState, boolean failed,
      ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      boolean nonBlocking) {
    processDone(threadName, exitValue, processName, axisID, forceNextProcess, endState,
        null, failed, processResultDisplay, processSeries, nonBlocking);
  }

  /**
   * Notification message that a background process is done.
   * 
   * @param threadName
   *          The name of the thread that has finished
   */
  public final void processDone(String threadName, int exitValue,
      ProcessName processName, AxisID axisID, boolean forceNextProcess,
      ProcessEndState endState, String statusString, boolean failed,
      ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      boolean nonBlocking) {
    if (threadName.equals(threadNameA)) {
      getMainPanel().stopProgressBar(AxisID.FIRST, endState, statusString);
      threadNameA = "none";
      backgroundProcessA = false;
      backgroundProcessNameA = null;
      // axisID = AxisID.FIRST;
    }
    else if (threadName.equals(threadNameB)) {
      getMainPanel().stopProgressBar(AxisID.SECOND, endState, statusString);
      threadNameB = "none";
      // axisID = AxisID.SECOND;
    }
    else if (!nonBlocking) {
      uiHarness.openMessageDialog(this, "Unknown thread finished!!!" + "\nThread name: "
          + threadName, "Unknown Thread", axisID);
    }
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    if (parallelPanel != null) {
      parallelPanel.msgProcessDone();
    }
    if (processName != null) {
      updateDialog(processName, axisID);
    }
    setPauseProcess(axisID, endState, processSeries);
    // Try to start the next process if the process succeeded, or if
    // forceNextProcess is true (unless the user killed the process, it makes
    // the
    // nextProcess execute even when the current process failed).
    // If the process is
    if (endState != ProcessEndState.KILLED && (exitValue == 0 || forceNextProcess)) {
      if (processSeries == null
          || !processSeries.startNextProcess(axisID, processResultDisplay)) {
        sendMsgProcessSucceeded(processResultDisplay);
        processSucceeded(axisID, processName);
      }
    }
    else if (endState == ProcessEndState.PAUSED) {
      if (processSeries == null
          || !processSeries.startPauseProcess(axisID, processResultDisplay)) {
        sendMsgProcessSucceeded(processResultDisplay);
        processSucceeded(axisID, processName);
      }
    }
    else {
      // ProcessSeries gets thrown away after it fails or the processes are used
      // up, so the processes don't have to be cleared as they did when next
      // processes where managed by BaseManager.
      if (failed) {
        sendMsgProcessFailed(processResultDisplay);
        if (processSeries != null) {
          processSeries.startFailProcess(axisID, processResultDisplay);
        }
      }
    }
  }

  /**
   * Should remain private.
   * 
   * @param axisID
   * @return
   */
  private boolean isReconnectRun(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return reconnectRunB;
    }
    return reconnectRunA;
  }

  public boolean isValid() {
    return true;
  }

  /**
   * Should remail private
   * 
   * @param axisID
   */
  private void setReconnectRun(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      reconnectRunB = true;
    }
    else {
      reconnectRunA = true;
    }
  }

  /**
   * Save param file and open dialogs
   * @param errmsg - should not be null
   * @return timestamp or null if this functionality is not implemented
   */
  public String saveAll(final StringBuffer errmsg) {
    return null;
  }

  public void doAutomation() {
    if (EtomoDirector.INSTANCE.getArguments().isExit()) {
      uiHarness.exit(AxisID.ONLY, 0);
    }
  }

  /**
   * @param processData
   * @param axisID
   */
  void handleDifferentHost(ProcessData processData, AxisID axisID) {
    String hostName = processData.getHostName();
    if (processData.isRunning()) {
      // Handles the case where ssh hostname ps finds the pid of this process.
      if (uiHarness.openYesNoDialog(this,
          "WARNING:  Cannot connect to " + processData.getProcessName()
              + ".  The process is running on " + hostName
              + ".  Please exit Etomo, run xhost " + hostName + ", ssh to " + hostName
              + ", and run etomo in order to connect to this process.  Exit "
              + "Etomo Y/N?", axisID)) {
        // Exit from etomo.
        uiHarness.exit(AxisID.ONLY, 0);
      }
    }
    else if (processData.isSshFailed()) {
      // Handles the case where the ssh fails.
      uiHarness.openMessageDialog(this,
          "WARNING:  Cannot connect to " + processData.getProcessName()
              + ".  This process may be running on " + hostName
              + ".  Unable to connect to " + hostName + " to find out.  If "
              + processData.getProcessName() + " is still running on " + hostName
              + ", please exit Etomo, run xhost " + hostName + ", ssh to " + hostName
              + ", and run etomo in order to connect to this process.",
          "Reconnect Warning", axisID);
    }
  }

  /**
   * IMPORTANT: Must turn off the blocking in BaseProcessManager when it is
   * first run. If it doesn't then no process that blocks an axis can run.
   * Attempts to reconnect to a currently running process. Only run once per
   * axis. Only attempts one reconnect.
   * 
   * @param axisID -
   *          axis of the running process.
   * @return true if a reconnect was attempted.
   * @throws RuntimeException
   *           if any Throwable is caught
   */
  public boolean reconnect(final ProcessData processData, final AxisID axisID,
      final boolean multiLineMessages) {
    try {
      if (isReconnectRun(axisID)) {
        // Just in case
        getProcessManager().unblockAxis(axisID);
        return false;
      }
      setReconnectRun(axisID);
      if (processData == null) {
        getProcessManager().unblockAxis(axisID);
        return false;
      }
      if (processData.getProcessName() == ProcessName.PROCESSCHUNKS) {
        if (processData.isOnDifferentHost()) {
          handleDifferentHost(processData, axisID);
          getProcessManager().unblockAxis(axisID);
          return false;
        }
        if (processData.isRunning()) {
          System.err.println("\nAttempting to reconnect in Axis " + axisID.toString()
              + "\n" + processData);
          if (!reconnectProcesschunks(processData, axisID, multiLineMessages)) {
            System.err.println("\nReconnect in Axis" + axisID.toString() + " failed");
          }
          getProcessManager().unblockAxis(axisID);
          return true;
        }
      }
    }
    catch (Throwable t) {
      t.printStackTrace();
      getProcessManager().unblockAxis(axisID);
      throw new RuntimeException(t);
    }
    BaseProcessManager processManager = getProcessManager();
    if (processManager != null) {
      processManager.unblockAxis(axisID);
    }
    return false;
  }

  public boolean reconnectProcesschunks(final ProcessData processData,
      final AxisID axisID, final boolean multiLineMessages) {
    ProcessResultDisplay display = null;
    ProcessResultDisplayFactoryInterface factory = getProcessResultDisplayFactoryInterface(axisID);
    if (factory != null) {
      display = factory.getProcessResultDisplay(processData.getDisplayKey().getInt());
    }
    if (display != null) {
      sendMsgProcessStarting(display);
    }
    MainPanel mainPanel = getMainPanel();
    // FIXME - null pointer getPArallelPanel
    String lastProcess = processData.getLastProcess();
    ProcessSeries processSeries = new ProcessSeries(this, processData.getDialogType());
    if (lastProcess != null) {
      processSeries.setLastProcess(lastProcess);
    }
    boolean ret = getProcessManager().reconnectProcesschunks(axisID, processData,
        display, processSeries, multiLineMessages, isPopupChunkWarnings());
    setThreadName(processData.getProcessName().toString(), axisID);
    return ret;
  }

  boolean isPopupChunkWarnings() {
    return true;
  }

  public final void tomodataplots(final TaskInterface task, final AxisID axisID,
      final ConstProcessSeries processSeries) {
    if (canRunTomodataplots(task, axisID)) {
      TomodataplotsParam param = new TomodataplotsParam();
      param.setTask(task);
      BaseProcessManager processManager = getProcessManager();
      if (processManager != null) {
        processManager.tomodataplots(param, axisID);
      }
    }
    if (processSeries != null) {
      processSeries.startNextProcess(AxisID.ONLY, null);
    }
  }

  public boolean canRunTomodataplots(final TaskInterface task, final AxisID axisID) {
    return true;
  }

  /**
   * Run processchunks.
   * 
   * @param axisID
   */
  public void processchunks(final AxisID axisID, final ProcesschunksParam param,
      final ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      final boolean popupChunkWarnings, final ProcessingMethod processingMethod,
      final boolean multiLineMessages, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    BaseMetaData metaData = getBaseMetaData();
    metaData.setCurrentProcesschunksRootName(axisID, param.getRootName().toString());
    metaData.setCurrentProcesschunksSubdirName(axisID, param.getSubdirName());
    saveStorable(axisID, metaData);
    String threadName;
    try {
      threadName = getProcessManager().processchunks(axisID, param,
          parallelPanel.getParallelProgressDisplay(), processResultDisplay,
          processSeries, popupChunkWarnings, processingMethod, multiLineMessages);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.PROCESSCHUNKS;
      message[1] = e.getMessage();
      UIHarness.INSTANCE.openMessageDialog(this, message, "Unable to execute command",
          axisID);
      if (processResultDisplay != null) {
        processResultDisplay.msgProcessFailedToStart();
      }
      return;
    }
    // set param in parallel panel so it can do a resume
    parallelPanel.setProcessInfo(param, processResultDisplay);
    setThreadName(threadName, axisID);
  }

  /**
   * This is a process done function for processes which are completed while the
   * original manager function waits and do not use the process manager. It is
   * necessary to call this function if this type of processes is the last in a
   * sequence which contains comscript or background processes, since the
   * processResultDisplay will not be run. This processDone function always
   * assumes success because the secondary process won't run if the proceeding
   * process failed. The exception to this is when forceNextProcess is on. Pass
   * forceNextProcess and an exit value from the process where forceNextProcess
   * was true. The last process will have to take responsibility for
   * success/failure when forceNextProcess is on.
   * 
   * @param axisID
   * @param processResultDisplay
   */
  final void processDone(AxisID axisID, ProcessResultDisplay processResultDisplay,
      ConstProcessSeries processSeries) {
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    if (parallelPanel != null) {
      parallelPanel.msgProcessDone();
    }
    if (processSeries == null
        || !processSeries.startNextProcess(axisID, processResultDisplay)) {
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

  /**
   * Set the current dialog type. This function is called from open functions
   * and from showBlankPRocess(). It allows Etomo to call the done function when
   * the user switches to another dialog.
   * 
   * @param dialogType
   * @param axisID
   * @return the action message
   */
  public String setCurrentDialogType(DialogType dialogType, AxisID axisID) {
    String actionMessage = null;
    if (axisID == AxisID.SECOND) {
      actionMessage = Utilities.prepareDialogActionMessage(dialogType, axisID,
          currentDialogTypeB);
      currentDialogTypeB = dialogType;
    }
    else {
      actionMessage = Utilities.prepareDialogActionMessage(dialogType, axisID,
          currentDialogTypeA);
      currentDialogTypeA = dialogType;
    }
    return actionMessage;
  }

  /**
   * Gets the current dialog type.
   * 
   * @param axisID
   * @return
   */
  DialogType getCurrentDialogType(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return currentDialogTypeB;
    }
    return currentDialogTypeA;
  }

  public void setDebug(boolean debug) {
    this.debug = debug;
  }

  public final void startLoad(IntermittentCommand param, LoadMonitor monitor) {
    getProcessManager().startLoad(param, monitor);
  }

  public final void endLoad(IntermittentCommand param, LoadMonitor monitor) {
    getProcessManager().endLoad(param, monitor);
  }

  public final void stopLoad(IntermittentCommand param, LoadMonitor monitor) {
    getProcessManager().stopLoad(param, monitor);
  }

  /**
   * Called when the manager's interface is either displayed or hidden
   * @param current - true when interface is displayed
   */
  public final void msgCurrentManagerChanged(final boolean current) {
    if (current) {
      makePropertyUserDirLocal();
    }
    if (logWindow != null) {
      logWindow.msgCurrentManagerChanged(current, isStartupPopupOpen());
    }
  }

  /**
   * Startup popup dialogs are used to set the dataset name and location in some
   * interfaces.
   * @return
   */
  public boolean isStartupPopupOpen() {
    // Most interfaces don't use a startup popup.
    return false;
  }

  public final void makePropertyUserDirLocal() {
    // make the manager's directory the local directory
    if (propertyUserDir == null) {
      EtomoDirector.INSTANCE.makeOriginalDirLocal();
    }
    else {
      System.setProperty("user.dir", propertyUserDir);
    }
  }

  public final void savePreferences(AxisID axisID, Storable storable) {
    MainPanel mainPanel = getMainPanel();
    try {
      ParameterStore localParameterStore = EtomoDirector.INSTANCE.getParameterStore();
      localParameterStore.save(storable);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to save preferences.\n" + e.getMessage(),
          "Etomo Error", axisID);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to save preferences.\n" + e.getMessage(),
          "Etomo Error", axisID);
    }
  }

  /**
   * Map the thread name to the correct axis
   * 
   * @param name
   *          The name of the thread to assign to the axis
   * @param axisID
   *          The axis of the thread to be mapped
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

  public static File chunkComscriptAction(Container root) {
    // Open up the file chooser in the working directory
    JFileChooser chooser = new FileChooser(new File(
        EtomoDirector.INSTANCE.getOriginalUserDir()));
    ChunkComscriptFileFilter filter = new ChunkComscriptFileFilter();
    chooser.setFileFilter(filter);
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(root);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      return chooser.getSelectedFile();
    }
    return null;
  }

  public final void resetCurrentProcesschunks(AxisID axisID) {
    BaseMetaData metaData = getBaseMetaData();
    metaData.resetCurrentProcesschunksRootName(axisID);
    metaData.resetCurrentProcesschunksSubdirName(axisID);
  }

  private void setPauseProcess(final AxisID axisID, final ProcessEndState endState,
      final ProcessSeries processSeries) {
    if (processSeries != null && endState == ProcessEndState.PAUSED
        && !(axisID == AxisID.SECOND ? resumeDataB : resumeDataA).isNull()) {
      processSeries.setPauseProcess(Task.RESUME);
    }
  }

  private void saveResume(final AxisID axisID, final ProcesschunksParam param,
      final ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      final boolean popupChunkWarnings, final ProcessingMethod processingMethod,
      final boolean multiLineMessages) {
    if (axisProcessData.isPausing(axisID)) {
      ResumeData resumeData = axisID == AxisID.SECOND ? resumeDataB : resumeDataA;
      resumeData.set(param, processResultDisplay, processSeries, popupChunkWarnings,
          processingMethod, multiLineMessages);
      axisProcessData.setWillResume(axisID);
    }
  }

  private void resume(final AxisID axisID) {
    ResumeData resumeData = axisID == AxisID.SECOND ? resumeDataB : resumeDataA;
    if (!resumeData.isNull()) {
      try {
        Thread.sleep(3000);
      }
      catch (InterruptedException e) {
      }
      resume(axisID, resumeData.getProcesschunksParam(),
          resumeData.getProcessResultDisplay(), resumeData.getProcessSeries(),
          resumeData.isPopupChunkWarnings(), resumeData.getProcessingMethod(),
          resumeData.isMultiLineMessages());
    }
    resumeData.reset();
  }

  /**
   * Get the current processchunks root name from meta data. If it exists,
   * attempt to resume processchunks.
   * 
   * @param axisID
   * @param param
   * @param processResultDisplay
   * @param processSeries
   * @param subcommandDetails
   */
  public void resume(final AxisID axisID, ProcesschunksParam param,
      ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      final CommandDetails subcommandDetails, final boolean popupChunkWarnings,
      final ProcessingMethod processingMethod, final boolean multiLineMessages,
      final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    boolean axisInUse = getProcessManager().inUse(axisID, processResultDisplay, false);
    ProcessResultDisplay origProcessResultDisplay = null;
    if (axisInUse) {
      // When the axis is in use, store the resume information. The result display should
      // not change.
      origProcessResultDisplay = processResultDisplay;
      processResultDisplay = null;
    }
    sendMsgProcessStarting(processResultDisplay);
    BaseMetaData metaData = getBaseMetaData();
    if (param == null) {
      String rootName = metaData.getCurrentProcesschunksRootName(axisID);
      if (rootName != null && !rootName.matches("\\s*")) {
        param = new ProcesschunksParam(this, axisID, rootName, null);
        param.setSubcommandDetails(subcommandDetails);
        if (metaData.isCurrentProcesschunksSubdirNameSet(axisID)) {
          param.setSubdirName(metaData.getCurrentProcesschunksSubdirName(axisID));
        }
      }
      else {
        uiHarness.openMessageDialog(this, "No command to resume", "Resume Failed");
        sendMsgProcessFailedToStart(processResultDisplay);
        return;
      }
    }
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    if (!parallelPanel.getResumeParameters(param, true)) {
      sendMsgProcessFailedToStart(processResultDisplay);
      return;
    }
    if (axisInUse) {
      saveResume(axisID, param, origProcessResultDisplay, processSeries,
          popupChunkWarnings, processingMethod, multiLineMessages);
      return;
    }
    resume(axisID, param, processResultDisplay, processSeries, popupChunkWarnings,
        processingMethod, multiLineMessages);
  }

  private void resume(final AxisID axisID, final ProcesschunksParam param,
      final ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      final boolean popupChunkWarnings, final ProcessingMethod processingMethod,
      final boolean multiLineMessages) {
    BaseMetaData metaData = getBaseMetaData();
    metaData.setCurrentProcesschunksRootName(axisID, param.getRootName());
    metaData.setCurrentProcesschunksSubdirName(axisID, param.getSubdirName());
    saveStorable(axisID, metaData);
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(axisID);
    String threadName;
    try {
      threadName = getProcessManager().processchunks(axisID, param,
          parallelPanel.getParallelProgressDisplay(), processResultDisplay,
          processSeries, popupChunkWarnings, processingMethod, multiLineMessages);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute " + ProcessName.PROCESSCHUNKS;
      message[1] = e.getMessage();
      uiHarness.openMessageDialog(this, message, "Unable to execute command", axisID);
      return;
    }
    setThreadName(threadName, axisID);
  }

  public final void tomosnapshot(AxisID axisID) {
    BaseProcessManager processManager = getProcessManager();
    if (processManager != null) {
      processManager.tomosnapshot(axisID, isTomosnapshotThumbnail());
    }
    else {
      uiHarness.openMessageDialog(this, "No processes can be run in this interface.",
          "Unable to run tomosnapshot", axisID);
    }
  }

  boolean isTomosnapshotThumbnail() {
    return false;
  }

  public static final class Task implements TaskInterface {
    private static final Task RESUME = new Task();

    private Task() {
    }

    public boolean okToDrop() {
      return false;
    }
  }

  private static final class ResumeData {
    private ProcesschunksParam param = null;
    private ProcessResultDisplay processResultDisplay = null;
    private ProcessSeries processSeries = null;
    private boolean popupChunkWarnings = false;
    private ProcessingMethod processingMethod = null;
    private boolean multiLineMessages = false;

    private synchronized void set(final ProcesschunksParam param,
        final ProcessResultDisplay processResultDisplay,
        final ProcessSeries processSeries, final boolean popupChunkWarnings,
        final ProcessingMethod processingMethod, final boolean multiLineMessages) {
      this.param = param;
      this.processResultDisplay = processResultDisplay;
      this.processSeries = processSeries;
      this.popupChunkWarnings = popupChunkWarnings;
      this.processingMethod = processingMethod;
      this.multiLineMessages = multiLineMessages;
    }

    private synchronized boolean isNull() {
      return param == null;
    }

    private synchronized void reset() {
      param = null;
      processResultDisplay = null;
      processSeries = null;
      popupChunkWarnings = false;
      processingMethod = null;
      multiLineMessages = false;
    }

    private ProcesschunksParam getProcesschunksParam() {
      return param;
    }

    private ProcessResultDisplay getProcessResultDisplay() {
      return processResultDisplay;
    }

    private ProcessSeries getProcessSeries() {
      return processSeries;
    }

    private ProcessingMethod getProcessingMethod() {
      return processingMethod;
    }

    private boolean isMultiLineMessages() {
      return multiLineMessages;
    }

    private boolean isPopupChunkWarnings() {
      return popupChunkWarnings;
    }
  }
}
/**
 * <p>
 * $Log$
 * Revision 1.146  2011/06/30 00:18:17  sueh
 * Bug# 1502 Added useFileNameInClose parameter to renameImageFile.  This allows the rename function to use the from file name to rename, but not use it to close 3dmod.  This is useful for 3dmod types that open a group of files.
 *
 * Revision 1.145  2011/06/28 20:01:55  sueh
 * Added test prints to closeImod functions.
 *
 * Revision 1.144  2011/06/28 03:02:23  sueh
 * Bug# 1501 In releaseFile, increase the wait time for Windows to release the file.
 *
 * Revision 1.143  2011/05/19 16:25:06  sueh
 * bug# 1473 Removed unused function clearUIParameters.
 *
 * Revision 1.142  2011/05/05 01:26:55  sueh
 * bug# 1396  Popping up an error message an failing when the dataset directory ends in a space.
 *
 * Revision 1.141  2011/04/09 06:20:52  sueh
 * bug# 1416 Need to pass the manager to most FileType functions so that TILT_OUTPUT can distinguish
 * between single and dual axis type.  Replaced FileType.toString and toString2 with getDescription and
 * getImodManagerKey2.  ToString wasn't working well with the composite file types.
 *
 * Revision 1.140  2011/04/04 16:44:52  sueh
 * bug# 1416 Modified backFile, checkNextProcess, reconnectProcesschunks, resume.
 *
 * Revision 1.139  2011/02/26 04:19:27  sueh
 * bug# 1453 Added logMessage(List, String, AxisID).
 *
 * Revision 1.138  2011/02/08 21:40:29  sueh
 * bug# 1437
 *
 * Revision 1.137  2011/02/03 05:51:42  sueh
 * bug# 1422 Using ProcessingMethod to keep track of which type of
 * processing method is in use.  The decisions about when to display the
 * parallel processing table have been centralized in
 * ProcessingMethodMediator.
 *
 * Revision 1.136  2010/11/13 16:02:54  sueh
 * bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 *
 * Revision 1.135  2010/07/02 03:13:43  sueh
 * bug# 1388 Added popupChunkWarnings to processchunks and resume.
 *
 * Revision 1.134  2010/05/15 17:40:18  sueh
 * bug# 1358 For Windows XP giving more time to release file (releaseFile()).
 *
 * Revision 1.133  2010/05/11 21:27:17  sueh
 * bug#1358
 *
 * Revision 1.132  2010/05/06 01:40:35  sueh
 * bug# 1344 Increase the sleep time in releaseFile to make sure that it
 * always works.
 *
 * Revision 1.131  2010/05/05 20:44:08  sueh
 * bug# 1344 Added releaseFile.
 *
 * <p>
 * Revision 1.130 2010/05/03 21:46:13 sueh
 * <p>
 * bug# 1344 Added a sleep to closeImod functions to give Windows time to
 * <p>
 * release control of the image files.
 * <p>
 * <p>
 * Revision 1.129 2010/04/28 15:32:11 sueh
 * <p>
 * bug# 1344 Added closeImod, closeImods, backImageFile,
 * <p>
 * renameImageFile, closeStaleFile functions. Added abstract function
 * <p>
 * getFileSubdirectoryName. Using ProcessSeries.Process to hold process
 * <p>
 * information.
 * <p>
 * <p>
 * Revision 1.128 2010/04/10 00:29:38 sueh
 * <p>
 * bug# 1349 Changed close3dmods.
 * <p>
 * <p>
 * Revision 1.127 2010/03/18 22:40:45 sueh
 * <p>
 * bug# 1323 processchunks and resume was saving meta data to .etomo
 * <p>
 * instead of .edf.
 * <p>
 * <p>
 * Revision 1.126 2010/03/12 03:56:52 sueh
 * <p>
 * bug# 1325 Added isExiting.
 * <p>
 * <p>
 * Revision 1.125 2010/02/26 20:37:31 sueh
 * <p>
 * Changing the complex popup titles are making it hard to complete the
 * <p>
 * uitests.
 * <p>
 * <p>
 * Revision 1.124 2010/02/17 04:38:12 sueh
 * <p>
 * bug# 1301 Moved comScriptMgr and logPanel to child class.
 * <p>
 * <p>
 * Revision 1.123 2009/11/20 16:49:49 sueh
 * <p>
 * bug# 1282 Naming all the file chooser by constructing a FileChooser
 * <p>
 * instance instead of a JFileChooser instance.
 * <p>
 * <p>
 * Revision 1.122 2009/10/27 19:56:43 sueh
 * <p>
 * bug# 1275 Moving the resposibility for creating thelog panel to the child
 * <p>
 * classes. That way the Front Page manager doesn't have to have a log
 * <p>
 * panel. Handling a null process manager.
 * <p>
 * <p>
 * Revision 1.121 2009/10/23 22:20:59 sueh
 * <p>
 * bug# 1275 Made touch() a start function in BaseProcessManager.
 * <p>
 * <p>
 * Revision 1.120 2009/09/01 03:17:35 sueh
 * <p>
 * bug# 1222
 * <p>
 * <p>
 * Revision 1.119 2009/08/25 15:50:31 sueh
 * <p>
 * bug# 1218 Creating main panel even when headless.
 * <p>
 * <p>
 * Revision 1.118 2009/06/11 16:40:03 sueh
 * <p>
 * bug# 1221 Sending the process panel to the process function in the
 * <p>
 * manager wrapped in a ProcessDisplay interface. Changed
 * <p>
 * startNextProcess.
 * <p>
 * <p>
 * Revision 1.117 2009/06/01 18:45:48 sueh
 * <p>
 * Added comment.
 * <p>
 * <p>
 * Revision 1.116 2009/05/06 20:49:46 sueh
 * <p>
 * bug# 1207 In handleDifferentHost, exiting by calling UIHarness.exit.
 * <p>
 * <p>
 * Revision 1.115 2009/04/15 16:51:42 sueh
 * <p>
 * bug# 1190 Logging reconnect attempts. Returning false and logging failure for
 * major reconnection failures.
 * <p>
 * <p>
 * Revision 1.114 2009/04/14 23:00:43 sueh
 * <p>
 * bug# 1190 Logging reconnect. Handling some situations where process data is
 * not
 * <p>
 * running in reconnect.
 * <p>
 * <p>
 * Revision 1.113 2009/04/13 22:20:25 sueh
 * <p>
 * bug# 1207 Implemented doAutomation in BaseManager. Added handleDifferentHost.
 * Passed processData to reconnect.
 * <p>
 * <p>
 * Revision 1.112 2009/04/06 22:37:32 sueh
 * <p>
 * bug# 1206 BaseProcessManager.tomosnapshot is not returning an exception.
 * <p>
 * <p>
 * Revision 1.111 2009/04/02 19:05:10 sueh
 * <p>
 * bug# 1206 Simplify tomosnapshot so that it uses as little of Etomo as
 * possible, since it
 * <p>
 * is supposed to be called when there is an error.
 * <p>
 * <p>
 * Revision 1.110 2009/03/16 23:44:28 sueh
 * <p>
 * bug# 1186 Added member variable managerKey. Add this managerKey to LogPanel.
 * <p>
 * In logMessage pass the managerKey received as a param to LogPanel.logMessage.
 * <p>
 * <p>
 * Revision 1.109 2009/03/05 23:22:48 sueh
 * <p>
 * bug# 1194 In getStorables() and save() save logPanel.
 * <p>
 * <p>
 * Revision 1.108 2009/03/02 18:53:51 sueh
 * <p>
 * bug# 1193 In reconnect(AxisID) unblock the axis.
 * <p>
 * <p>
 * Revision 1.107 2009/02/04 22:38:17 sueh
 * <p>
 * bug# 1158 Added logPanel.
 * <p>
 * <p>
 * Revision 1.106 2009/01/26 22:40:57 sueh
 * <p>
 * bug# 1173 Added boolean nonBlocking to processDone functions, so it
 * <p>
 * knows not to pop up an error message that thread name is not set.
 * <p>
 * <p>
 * Revision 1.105 2008/12/10 18:30:49 sueh
 * <p>
 * bug# 1162 Added a manager stamp to setPropertyUserDir.
 * <p>
 * <p>
 * Revision 1.104 2008/11/20 01:24:32 sueh
 * <p>
 * bug# 1147 Added imodOpen for opening with a model.
 * <p>
 * <p>
 * Revision 1.103 2008/10/06 22:36:42 sueh
 * <p>
 * bug# 1113 Removed packPanel, which is unecessary since scrolling was
 * <p>
 * removed.
 * <p>
 * <p>
 * Revision 1.102 2008/09/30 19:46:31 sueh
 * <p>
 * bug# 1113 Added getFocusComponent.
 * <p>
 * <p>
 * Revision 1.101 2008/05/28 02:46:37 sueh
 * <p>
 * bug# 1111 Removed processDialogTypeA and B. The dialogType for
 * <p>
 * processes should be handled by ProcessSeries. Passing a DialogType
 * <p>
 * parameter to startNextProcess.
 * <p>
 * <p>
 * Revision 1.100 2008/05/13 20:53:36 sueh
 * <p>
 * bug# 847 In exitProgram, factored out functionality which also needs to
 * <p>
 * be done when just closing a manager.
 * <p>
 * <p>
 * Revision 1.99 2008/05/06 23:53:49 sueh
 * <p>
 * bug#847 Running deferred 3dmods by using the button that usually calls
 * <p>
 * them. This avoids having to duplicate the calls and having a
 * <p>
 * startNextProcess function just for 3dmods.
 * <p>
 * <p>
 * Revision 1.98 2008/05/03 00:30:02 sueh
 * <p>
 * bug# 847 Removed lastProcess and nextProcess member variables and associated
 * functions. Placed all of next process functionality in ProcessSeries.
 * <p>
 * <p>
 * Revision 1.97 2008/02/14 21:27:15 sueh
 * <p>
 * bug# 1077 Make sure that ImodManager.disconnect is called, even if
 * <p>
 * there is an exception.
 * <p>
 * <p>
 * Revision 1.96 2008/01/31 20:13:40 sueh
 * <p>
 * bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p>
 * Revision 1.95 2008/01/23 21:06:55 sueh
 * <p>
 * bug# 1064 Added reconnectRunA and B to ApplicationManager. Can't use the
 * <p>
 * reconnectRun functionality in BaseManager because BaseManager.reconnect is
 * <p>
 * run before ApplicationManager.reconnect.
 * <p>
 * <p>
 * Revision 1.94 2008/01/14 20:20:46 sueh
 * <p>
 * bug# 1050 Added reconnect to reconnect to processchunks. Also added
 * <p>
 * reconnectRunA and B to prevent multiple reconnect attempts per axis.
 * <p>
 * <p>
 * Revision 1.93 2007/12/26 21:55:54 sueh
 * <p>
 * bug# 1052 Added doAutomation() to EtomoDirector, BaseManager and the
 * <p>
 * manager classes. Function should run any user-specified automatic
 * functionality
 * <p>
 * before giving control to the user.
 * <p>
 * <p>
 * Revision 1.92 2007/12/10 21:47:09 sueh
 * <p>
 * bug# 1041 Added saveStorable() to save a single Storable to the datafile.
 * Added
 * <p>
 * processFailed() to reset next process and last process.
 * <p>
 * <p>
 * Revision 1.91 2007/11/06 18:57:10 sueh
 * <p>
 * bug# 1047 Added the ability to run in a subdirectory to ProcesschunksParam.
 * <p>
 * <p>
 * Revision 1.90 2007/09/27 19:20:56 sueh
 * <p>
 * bug# 1044 Made ProcessorTable the ParallelProgress display instead of
 * <p>
 * ParallelPanel. Generalized load functionality.
 * <p>
 * <p>
 * Revision 1.89 2007/09/10 20:24:14 sueh
 * <p>
 * bug# 925 Using getInstance to construct ProcessResultDisplayFactory.
 * <p>
 * <p>
 * Revision 1.88 2007/09/07 00:15:13 sueh
 * <p>
 * bug# 989 Using a public INSTANCE for EtomoDirector instead of getInstance
 * <p>
 * and createInstance.
 * <p>
 * <p>
 * Revision 1.87 2007/08/29 21:22:17 sueh
 * <p>
 * bug# 1041 Added chunkComscriptAction. In resume, if param is null, create it.
 * <p>
 * <p>
 * Revision 1.86 2007/07/30 22:36:41 sueh
 * <p>
 * bug# 963 Need to overide saveParamFile in JoinManager - removing "final"
 * <p>
 * keyword.
 * <p>
 * <p>
 * Revision 1.85 2007/07/30 18:31:12 sueh
 * <p>
 * bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p>
 * Revision 1.84 2007/06/08 21:49:21 sueh
 * <p>
 * bug# 1014 Added clearUIParameters, to back out the changes made in
 * <p>
 * initializeUIParameters. Removing call to setMetaData in loadParamFile
 * <p>
 * because it shouldn't be backed out.
 * <p>
 * <p>
 * Revision 1.83 2007/06/04 22:50:58 sueh
 * <p>
 * bug# 1005 Added boolean loadedFromADifferentFile to
 * <p>
 * initializeUIParameters and loadParamFile.
 * <p>
 * <p>
 * Revision 1.82 2007/05/21 22:27:50 sueh
 * <p>
 * bug# 964 Added abstract function getInterfaceType().
 * <p>
 * <p>
 * Revision 1.81 2007/05/03 00:45:17 sueh
 * <p>
 * bug# 964 Added getParallelProcessingDefaultNice().
 * <p>
 * <p>
 * Revision 1.80 2007/04/27 23:36:55 sueh
 * <p>
 * bug# 964 In processchunks(), handling a null processResultDisplay.
 * <p>
 * <p>
 * Revision 1.79 2007/04/09 19:26:14 sueh
 * <p>
 * bug# 964 In saveParamFile, call setParamFile so that PeetManager.paramFile is
 * <p>
 * set.
 * <p>
 * <p>
 * Revision 1.78 2007/03/26 23:30:04 sueh
 * <p>
 * bug# 964 Moved some of the imodOpen functions to the parent class to be
 * shared.
 * <p>
 * <p>
 * Revision 1.77 2007/03/01 01:09:27 sueh
 * <p>
 * bug# 964 Removed protected modifiers. Interpackage inheritance doesn't
 * require
 * <p>
 * it.
 * <p>
 * <p>
 * Revision 1.76 2007/02/22 20:31:30 sueh
 * <p>
 * bug# 966 Getting the shell environment variable in LoadAverageParam.
 * <p>
 * <p>
 * Revision 1.75 2007/02/21 04:16:39 sueh
 * <p>
 * bug# 964 Initializing parameters when the param file is chosen.
 * <p>
 * <p>
 * Revision 1.74 2007/02/19 21:48:49 sueh
 * <p>
 * bug# 964 Removed isNewManager() because it is only used by Application
 * <p>
 * Manager.
 * <p>
 * <p>
 * Revision 1.73 2007/02/05 21:25:59 sueh
 * <p>
 * bug# 692 In processDone, run startNextProcess even if the process is empty,
 * <p>
 * so that last process can be automatically placed in process.
 * <p>
 * <p>
 * Revision 1.72 2006/11/28 22:47:02 sueh
 * <p>
 * bug# 934 Changed BaseManager.stop() to endThreads(). Added a command to
 * <p>
 * end main panel threads, which are load averages. These threads are shared
 * <p>
 * between managers, so they will only end if any other manager is using them.
 * <p>
 * <p>
 * Revision 1.71 2006/11/16 23:13:42 sueh
 * <p>
 * bug# 872 Removed print statement
 * <p>
 * <p>
 * Revision 1.70 2006/11/15 18:18:21 sueh
 * <p>
 * bug# 872 There are two ways to save: saveStorables and saveParamFile.
 * <p>
 * SaveParamFile is used with the done... functions when exiting etomo.
 * <p>
 * SaveStorables is for saving before or after processes, when more then one
 * <p>
 * storable may require saving.
 * <p>
 * <p>
 * Revision 1.69 2006/10/24 21:13:43 sueh
 * <p>
 * bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
 * <p>
 * Revision 1.68 2006/10/16 22:33:29 sueh
 * <p>
 * bug# 919 Changed touch(File) to touch(String absolutePath).
 * <p>
 * <p>
 * Revision 1.67 2006/09/13 23:06:47 sueh
 * <p>
 * bug# 920 Moving createState() call to child classes, so it can be done after
 * meta
 * <p>
 * data is created.
 * <p>
 * <p>
 * Revision 1.66 2006/08/03 21:25:41 sueh
 * <p>
 * bug# 769 ProcessDone(): Changed boolean error to boolean failed because is
 * <p>
 * also covers killing a process.
 * <p>
 * <p>
 * Revision 1.65 2006/07/28 19:43:31 sueh
 * <p>
 * bug# 868 Changed AbstractParallelDialog.isParallel to
 * <p>
 * usingParallelProcessing.
 * <p>
 * <p>
 * Revision 1.64 2006/07/26 16:31:46 sueh
 * <p>
 * bug# 868 Added processchunks to ReconUIExpert. Changed
 * <p>
 * nextProcessDialogType to processDialogType. Moved currentDialogType from
 * <p>
 * ApplicationManager to BaseManager.
 * <p>
 * <p>
 * Revision 1.63 2006/07/19 20:05:10 sueh
 * <p>
 * bug# 902 ProcessDone: calling processSucceeded.
 * <p>
 * <p>
 * Revision 1.62 2006/07/17 21:15:57 sueh
 * <p>
 * bug# 900 Added imodSendEvent functionality back. Uses the
 * <p>
 * SystemProcessException.
 * <p>
 * <p>
 * Revision 1.61 2006/06/30 19:58:59 sueh
 * <p>
 * bug# 877 Calling all the done dialog functions from the dialog.done()
 * function,
 * <p>
 * which is called from the button action functions and saveAction() in
 * <p>
 * ProcessDialog. Removing doneProcessDialog().
 * <p>
 * <p>
 * Revision 1.60 2006/06/22 20:58:26 sueh
 * <p>
 * bug# 797 imodGetRubberbandCoordinates() checking for errors before getting to
 * <p>
 * this function
 * <p>
 * <p>
 * Revision 1.59 2006/06/21 15:46:19 sueh
 * <p>
 * bug# 581 Quitting Imodqtassist process when etomo exits.
 * <p>
 * <p>
 * Revision 1.58 2006/06/15 16:12:42 sueh
 * <p>
 * bug# 871 exitProgram(): Added a test for running processes that can't
 * continue
 * <p>
 * if etomo exits.
 * <p>
 * <p>
 * Revision 1.57 2006/06/05 16:01:01 sueh
 * <p>
 * bug# 766 getParamFileStorableArray(): Add the option have elements in the
 * storable array that aer set by the base manager. Add initialized so that the
 * <p>
 * manager can tell when an .edf is first loaded.
 * <p>
 * <p>
 * Revision 1.56 2006/05/19 19:26:03 sueh
 * <p>
 * bug# 866 Added nextProcessDialogType, to let the manager call
 * <p>
 * UIExpert.nextProcess()
 * <p>
 * <p>
 * Revision 1.55 2006/03/22 00:34:59 sueh
 * <p>
 * bug# 836 Preventing savePreferences() from running when the axis is
 * <p>
 * busy
 * <p>
 * <p>
 * Revision 1.54 2006/03/20 17:48:14 sueh
 * <p>
 * bug# 835 ParallelManager is overriding processChunks
 * <p>
 * <p>
 * Revision 1.53 2006/01/31 20:35:17 sueh
 * <p>
 * bug# 521 Added doneProcessDialog, a function which should be called
 * <p>
 * by all of the ApplicationManager done functions. It calls
 * <p>
 * ProcessDialog.done(). Creating the ProcessResultDisplayFactory's with
 * <p>
 * screen states, so that the displays can save their states.
 * <p>
 * <p>
 * Revision 1.52 2006/01/26 21:46:30 sueh
 * <p>
 * bug# 401 Removing processResultDisplayA and B before this method will
 * <p>
 * fail when the user presses the same button twice. Passing the
 * <p>
 * processResultDisplay to the process manager to get it back when
 * <p>
 * processDone is called. Added a simple processDone for secondary
 * <p>
 * processes that don't use the existing processDone functions.
 * <p>
 * Added processResultDisplay parameters to all the toggle button functions.
 * <p>
 * <p>
 * Revision 1.51 2006/01/20 20:43:34 sueh
 * <p>
 * bug# 401 Added ProcessResultDisplay functionality to processDone and
 * <p>
 * startNextProcess. Added setProcessResultDisplay.
 * <p>
 * <p>
 * Revision 1.50 2005/12/23 01:55:43 sueh
 * <p>
 * bug# 675 Split the test option functionality. Control headlessness with
 * <p>
 * --headless. This allow both JUnit and JfcUnit to use the special test
 * <p>
 * functions.
 * <p>
 * <p>
 * Revision 1.49 2005/12/14 01:26:31 sueh
 * <p>
 * bug# 782 Updated the toString() function.
 * <p>
 * <p>
 * Revision 1.48 2005/12/12 21:57:56 sueh
 * <p>
 * bug# 779 Made BaseManager.resetNextProcess() private. Added
 * <p>
 * startNextProcess(), which calls resetNextProcess and then calls a child
 * <p>
 * startNextProcess. ResetNextProcess is only called from
 * <p>
 * startNextProcess and processDone.
 * <p>
 * <p>
 * Revision 1.47 2005/12/09 20:21:29 sueh
 * <p>
 * bug# 776 Added tomosnapshot
 * <p>
 * <p>
 * Revision 1.46 2005/12/08 00:53:29 sueh
 * <p>
 * bug# 504 Changed exitProgram() to only warn the user when exiting would
 * <p>
 * prevent the process from completing.
 * <p>
 * <p>
 * Revision 1.45 2005/11/21 21:59:15 sueh
 * <p>
 * bug# 761 In processchunks(), fail if
 * <p>
 * ParallelPanel.getParameters(ProcesschunksParam) fails.
 * <p>
 * <p>
 * Revision 1.44 2005/11/19 01:43:15 sueh
 * <p>
 * bug# 744 Added processDone(String, int, ProcessName, AxisID,
 * <p>
 * boolean, ProcessEndState).
 * <p>
 * <p>
 * Revision 1.43 2005/11/10 17:52:23 sueh
 * <p>
 * bug# 733 Added manager parameters to ProcesschunksParam constructor.
 * <p>
 * <p>
 * Revision 1.42 2005/11/04 00:52:26 sueh
 * <p>
 * fixed copyright
 * <p>
 * <p>
 * Revision 1.41 2005/11/02 21:33:39 sueh
 * <p>
 * bug# 754 Getting error and warning tags from ProcessMessages.
 * <p>
 * <p>
 * Revision 1.40 2005/10/31 17:52:40 sueh
 * <p>
 * bug# 730 Renamed getTestParamFile to getParamFile. Made
 * <p>
 * canChangeParamFileName() abstract.
 * <p>
 * <p>
 * Revision 1.39 2005/10/27 00:07:26 sueh
 * <p>
 * bug# 725 Allowing multiple paths when running nextProcess. Added
 * <p>
 * lastProcessA and B. Added functions: getLastProcess,
 * <p>
 * isLastProcessSet, resetLastProcess, and setLastProcess.
 * <p>
 * <p>
 * Revision 1.38 2005/10/15 00:27:42 sueh
 * <p>
 * bug# 532 Changed showParallelStatus() to setParallelDialog(). Removed
 * <p>
 * currentParallelDialogsA and B. Simplified setParallelDialog() to just call
 * <p>
 * MainPanel.setParallelDialog() with dialog.isParallel(). isParallel() is true
 * <p>
 * if any parallel processing check boxes are true on the dialog.
 * <p>
 * <p>
 * Revision 1.37 2005/10/14 21:04:57 sueh
 * <p>
 * bug# 730 Changed loadedTestParamFile to loadedParamFile.
 * <p>
 * <p>
 * Revision 1.36 2005/09/29 18:38:48 sueh
 * <p>
 * bug# 532 Preventing Etomo from saving to the .edf or .ejf file over and
 * <p>
 * over during exit. Added BaseManager.exiting and
 * <p>
 * saveIntermediateParamFile(), which will not save when exiting it true.
 * <p>
 * Setting exiting to true in BaseManager.exitProgram(). Moved call to
 * <p>
 * saveParamFile() to the child exitProgram functions so that the param file
 * <p>
 * is saved after all the done functions are run.
 * <p>
 * <p>
 * Revision 1.35 2005/09/27 21:12:38 sueh
 * <p>
 * bug# 532 Moved the creation of the the .edf file Storable array to the child
 * <p>
 * classes because the classes the go into the array vary. Moved the
 * <p>
 * parallel panels to the axis level panels.
 * <p>
 * <p>
 * Revision 1.34 2005/09/22 20:43:38 sueh
 * <p>
 * bug# 532 Added showParallelStatus(), which is called when a parallel
 * <p>
 * processing checkbox is checked.
 * <p>
 * <p>
 * Revision 1.33 2005/09/21 16:04:34 sueh
 * <p>
 * bug# 532 Moved processchunks() and setThreadName() to BaseManager.
 * <p>
 * <p>
 * Revision 1.32 2005/09/19 16:38:29 sueh
 * <p>
 * bug# 532 In getParallelPanel(), calling
 * <p>
 * ParallelPanel.setContainer(ParallelDialog).
 * <p>
 * <p>
 * Revision 1.31 2005/09/13 00:27:25 sueh
 * <p>
 * bug# 532 Removed unecessary import.
 * <p>
 * <p>
 * Revision 1.30 2005/09/13 00:13:46 sueh
 * <p>
 * bug# 532 Modified savePreferences() to check whether the axis is busy.
 * <p>
 * <p>
 * Revision 1.29 2005/09/12 23:56:50 sueh
 * <p>
 * bug# 532 Added savePreferences() to save a Storable class to the .etomo
 * <p>
 * file without overwriting preference entries from other Storable classes.
 * <p>
 * <p>
 * Revision 1.28 2005/09/09 21:20:24 sueh
 * <p>
 * bug# 532 Made LoadAverageParam an n'ton (one for each computer) so
 * <p>
 * that there aren't IntermittentSystemPrograms then computers. This allows
 * <p>
 * IntermittentSystemProgram to be used for other things and conforms to
 * <p>
 * it definition of having one instance per IntermittentCommand, instead of
 * <p>
 * one instance per computer.
 * <p>
 * <p>
 * Revision 1.27 2005/09/01 18:34:42 sueh
 * <p>
 * bug# 532 Made the parallel panels manager level. Added parallelPanelA
 * <p>
 * and B. Added getParallPanel(), which constructs the panel if necessary
 * <p>
 * and returns it.
 * <p>
 * <p>
 * Revision 1.26 2005/08/22 22:04:35 sueh
 * <p>
 * bug# 714 Added makeCurrent() to set the user.dir property when the
 * <p>
 * manager is switched.
 * <p>
 * <p>
 * Revision 1.25 2005/08/22 15:57:13 sueh
 * <p>
 * bug# 532 Added start and stop GetLoadAverage() to send load
 * <p>
 * information to a LoadAverageDisplay.
 * <p>
 * <p>
 * Revision 1.24 2005/08/10 20:38:29 sueh
 * <p>
 * bug# 711 Made UIParameters constructor private. Can't force it no be
 * <p>
 * called since this is mostly static functions.
 * <p>
 * <p>
 * Revision 1.23 2005/08/04 19:05:42 sueh
 * <p>
 * bug# 532 Sending the manager to UIHarness.pack() so that
 * <p>
 * packDialogs() can be called.
 * <p>
 * <p>
 * Revision 1.22 2005/08/01 17:57:59 sueh
 * <p>
 * Removed unnecessary FIXME
 * <p>
 * <p>
 * Revision 1.21 2005/07/29 00:38:52 sueh
 * <p>
 * bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p>
 * because the current manager changes when the user changes the tab.
 * <p>
 * Passing the manager where its needed.
 * <p>
 * <p>
 * Revision 1.20 2005/07/26 17:03:16 sueh
 * <p>
 * bug# 701 Pass ProcessEndState to the progress bar when stopping it.
 * <p>
 * <p>
 * Revision 1.19 2005/06/21 00:03:37 sueh
 * <p>
 * bug# 522 Added pass-through function call to
 * <p>
 * BaseProcessManager.touch() for MRCHeaderTest. Added toString()
 * <p>
 * overide.
 * <p>
 * <p>
 * Revision 1.18 2005/06/03 20:09:18 sueh
 * <p>
 * bug# 671 processDone(): Removed code changing the axisID because it
 * <p>
 * doesn't seem be necessary and it causes single axis file names to have
 * <p>
 * an incorrect "a" added to them.
 * <p>
 * <p>
 * Revision 1.17 2005/05/20 21:11:01 sueh
 * <p>
 * bug# 664 saveTestParamFile(): do not attempt to save to a file if the
 * <p>
 * memory is very low. If the save fails, the file may be truncated.
 * <p>
 * <p>
 * Revision 1.16 2005/05/18 22:30:37 sueh
 * <p>
 * bug# 622 Made nextProcessA and B private because they are set using
 * <p>
 * reset and setProcess functions. Added a new parameter to
 * <p>
 * processDone(): boolean forceNextProcess (default is false).
 * <p>
 * ForceNextProcess is for when the next process is independent of the
 * <p>
 * outcome of the previous process and makes startNextProcess() run
 * <p>
 * regardless of value of exitValue. It is used for archiveorig, to get the
 * <p>
 * second axis.
 * <p>
 * <p>
 * Revision 1.15 2005/05/17 19:08:40 sueh
 * <p>
 * bug# 520 Fixed some recently introduced bugs in join. We are saving to
 * <p>
 * .ejf file more often. When the first section is needs to be flipped, the
 * <p>
 * join manager isn't ready to flip, because the paramFile isn't set. So don't
 * <p>
 * both to save in this situation.
 * <p>
 * <p>
 * Revision 1.14 2005/04/26 17:34:35 sueh
 * <p>
 * bug# 615 Made MainFrame a package-level class. All MainFrame
 * <p>
 * functionality is handled through UIHarness to make Etomo more
 * <p>
 * compatible with JUnit. Removing the mainFrame member variable.
 * <p>
 * <p>
 * Revision 1.13 2005/04/25 20:32:08 sueh
 * <p>
 * bug# 615 Passing the axis where the command originated to the message
 * <p>
 * functions so that the message will be popped up in the correct window.
 * <p>
 * This requires adding AxisID to many objects. Move the interface for
 * <p>
 * popping up message dialogs to UIHarness. It prevents headless
 * <p>
 * exceptions during a test execution. It also allows logging of dialog
 * <p>
 * messages during a test. It also centralizes the dialog interface and
 * <p>
 * allows the dialog functions to be synchronized to prevent dialogs popping
 * <p>
 * up in both windows at once.
 * <p>
 * <p>
 * Revision 1.12 2005/04/21 20:28:13 sueh
 * <p>
 * bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p>
 * that requires it.
 * <p>
 * <p>
 * Revision 1.11 2005/03/19 01:09:52 sueh
 * <p>
 * adding comments
 * <p>
 * <p>
 * Revision 1.10 2005/03/11 01:57:43 sueh
 * <p>
 * bug# 612 Change nextProcess to support axis A and B.
 * <p>
 * <p>
 * Revision 1.9 2005/03/01 20:50:37 sueh
 * <p>
 * bug# 607 Catching Throwable in exitProgram and returning true to make
 * <p>
 * sure that Etomo can always exit.
 * <p>
 * <p>
 * Revision 1.8 2005/02/09 18:39:41 sueh
 * <p>
 * bug# 595 There is no way to stop the user from running combine when
 * <p>
 * another combine is still running from a previous Etomo session in the same
 * <p>
 * dataset. So warn the user that they won't receive a warning if they do
 * <p>
 * this.
 * <p>
 * <p>
 * Revision 1.7 2005/01/21 22:07:29 sueh
 * <p>
 * bug# 509 bug# 591 Moved the management of MetaData to the Controller
 * <p>
 * class.
 * <p>
 * <p>
 * Revision 1.6 2004/12/14 21:23:39 sueh
 * <p>
 * bug# 565: Fixed bug: Losing process track when backing up .edf file and
 * <p>
 * only saving metadata. Removed unnecessary class JoinProcessTrack.
 * <p>
 * bug# 572: Removing state object from meta data and managing it with a
 * <p>
 * manager class.
 * <p>
 * Saving all objects to the .edf/ejf file each time a save is done.
 * <p>
 * <p>
 * Revision 1.5 2004/12/09 04:49:17 sueh
 * <p>
 * bug# 565 Removed isDataParamDirty. Synchronized storage of param
 * <p>
 * file with store(Storable[]). Automatically saving to param file on exit.
 * <p>
 * Changed saveTestParamIfNecessary() to saveTestParamOnExit().
 * <p>
 * <p>
 * Revision 1.4 2004/12/03 02:30:44 sueh
 * <p>
 * bug# 568 Added setDataParamDirty() so that meta data can be changed
 * <p>
 * in other objects.
 * <p>
 * <p>
 * Revision 1.3 2004/11/23 00:14:11 sueh
 * <p>
 * bug# 520 Allowed propertyUserDir to be set. Prevented the construction
 * <p>
 * of mainPanel when test is true.
 * <p>
 * <p>
 * Revision 1.2 2004/11/19 22:33:55 sueh
 * <p>
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p>
 * Revision 1.1.2.19 2004/11/18 23:57:20 sueh
 * <p>
 * bug# 520 Added saveMetaData to save only meta data. Added
 * <p>
 * boolean canChangePAramFileName to tell MainFrame whether Save As
 * <p>
 * should be enabled.
 * <p>
 * <p>
 * Revision 1.1.2.18 2004/11/17 02:18:27 sueh
 * <p>
 * bug# 520 Fixed a comment.
 * <p>
 * <p>
 * Revision 1.1.2.17 2004/11/15 22:04:42 sueh
 * <p>
 * bug# 520 Removed the function isFileValid() because it is only called once.
 * <p>
 * Placed the code from isFileValid() into loadTestParamFile().
 * <p>
 * <p>
 * Revision 1.1.2.16 2004/11/12 22:43:34 sueh
 * <p>
 * bug# 520 Moved imodGetRubberbandCoordinates from ApplicationManager.
 * <p>
 * <p>
 * Revision 1.1.2.15 2004/10/29 01:15:23 sueh
 * <p>
 * bug# 520 Removing unecessary functions that provided services to
 * <p>
 * BaseManager. BaseManager can use get... functions to get the
 * <p>
 * mainPanel, metaData, and processTrack.
 * <p>
 * <p>
 * Revision 1.1.2.14 2004/10/22 03:18:05 sueh
 * <p>
 * bug# 520 Removed a FIXME comment.
 * <p>
 * <p>
 * Revision 1.1.2.13 2004/10/21 17:49:56 sueh
 * <p>
 * bug# 520 In loadTestParamFile() converted paramFile to a file created with
 * <p>
 * an absolute path, so metaData validation would not fail.
 * <p>
 * <p>
 * Revision 1.1.2.12 2004/10/15 00:00:02 sueh
 * <p>
 * bug# 520 Moving getTestParamFilename() to mainPanel. It is used for
 * <p>
 * saving existing data files, so knows which type (.edf or .ejf) it is saving.
 * <p>
 * <p>
 * Revision 1.1.2.11 2004/10/11 01:55:43 sueh
 * <p>
 * bug# 520 moved responsibility for mainPanel, metaData, processTrack,
 * <p>
 * and progressManager to child classes. Used abstract functions to use
 * <p>
 * these variables in the base classes. This is more reliable and doesn't
 * <p>
 * require casting.
 * <p>
 * <p>
 * Revision 1.1.2.10 2004/10/08 21:12:27 sueh
 * <p>
 * bug# 520 Backed out conversion from properties user.dir to workingDir
 * <p>
 * <p>
 * Revision 1.1.2.9 2004/10/08 15:40:48 sueh
 * <p>
 * bug# 520 Set workingDirName instead of system property for manager-
 * <p>
 * level working directory. Moved SettingsDialog to EtomoDirector. Since
 * <p>
 * EtomoDirector is a singleton, made all functions and member variables
 * <p>
 * non-static. The singleton code controls how many EtomoDirector
 * <p>
 * instances can exist. Moved application-level code in initProgram and
 * <p>
 * exitProgram to EtomoDirector.
 * <p>
 * <p>
 * Revision 1.1.2.8 2004/10/01 20:58:02 sueh
 * <p>
 * bug# 520 Changed getMetaDAta() to getBaseMetaData() so it can return
 * <p>
 * the abstract base class for objects that don't know which type of manager
 * <p>
 * they are using.
 * <p>
 * <p>
 * Revision 1.1.2.7 2004/09/29 17:37:09 sueh
 * <p>
 * bug# 520 Using BaseMetaData, BaseProcessTrack, and
 * <p>
 * BaseProcessManager. Moved processDone() from app mgr to base mgr.
 * <p>
 * Created abstract startNextProcess() and
 * <p>
 * updateDialog(ProcessName, AxisID). Removed resetState(),
 * <p>
 * openNewDataset() and openExistingDataset(). Managers will not be
 * <p>
 * reset and this functionality will be handled by EtomoDirector.
 * <p>
 * <p>
 * Revision 1.1.2.6 2004/09/15 22:33:39 sueh
 * <p>
 * bug# 520 call openMessageDialog in mainPanel instead of mainFrame.
 * <p>
 * Move packMainWindow and setPanel from ApplicationMAnager to
 * <p>
 * BaseManager.
 * <p>
 * <p>
 * Revision 1.1.2.5 2004/09/13 16:26:46 sueh
 * <p>
 * bug# 520 Adding abstract isNewManager. Each manager would have a
 * <p>
 * different way to tell whether they had a file open.
 * <p>
 * <p>
 * Revision 1.1.2.4 2004/09/09 17:28:38 sueh
 * <p>
 * bug# 520 MRU file labels already being set from EtomoDirector
 * <p>
 * <p>
 * Revision 1.1.2.3 2004/09/08 19:27:19 sueh
 * <p>
 * bug# 520 putting initialize UI parameters into a separate function
 * <p>
 * <p>
 * Revision 1.1.2.2 2004/09/07 17:51:00 sueh
 * <p>
 * bug# 520 getting mainFrame and userConfig from EtomoDirector, moved
 * <p>
 * settings dialog to BaseManager, moved backupFiles() to BaseManager,
 * <p>
 * moved exitProgram() and processing variables to BaseManager, split
 * <p>
 * MainPanel off from MainFrame
 * <p>
 * <p>
 * Revision 1.1.2.1 2004/09/03 20:37:24 sueh
 * <p>
 * bug# 520 Base class for ApplicationManager and JoinManager. Transfering
 * <p>
 * constructor functionality from AppMgr
 * <p>
 * </p>
 */
