package etomo;

import java.awt.Dimension;
import java.awt.GraphicsEnvironment;
import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.util.Date;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIDefaults;
import javax.swing.UIManager;
import javax.swing.plaf.FontUIResource;

import etomo.logic.VersionControl;
import etomo.process.IntermittentBackgroundProcess;
import etomo.process.ProcessRestarter;
import etomo.storage.EtomoFileFilter;
import etomo.storage.JoinFileFilter;
import etomo.storage.LogFile;
import etomo.storage.ParallelFileFilter;
import etomo.storage.ParameterStore;
import etomo.storage.PeetFileFilter;
import etomo.storage.SerialSectionsFileFilter;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.DirectiveFileType;
import etomo.type.EtomoNumber;
import etomo.type.ImodVersion;
import etomo.type.JoinMetaData;
import etomo.type.MetaData;
import etomo.type.ParallelMetaData;
import etomo.type.PeetMetaData;
import etomo.type.SerialSectionsMetaData;
import etomo.type.ToolType;
import etomo.type.UserConfiguration;
import etomo.ui.swing.MainFrame;
import etomo.ui.swing.SettingsDialog;
import etomo.ui.swing.UIHarness;
import etomo.ui.swing.UIParameters;
import etomo.util.EnvironmentVariable;
import etomo.util.UniqueHashedArray;
import etomo.util.UniqueKey;
import etomo.util.Utilities;

/**
 * <p>Description: Directs ApplicationManager and JoinManager through
 * BaseManager.</p>
 * 
 * <p>Copyright: Copyright (c) 2004 - 2005</p>
 * 
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */

public class EtomoDirector {
  public static final String rcsid = "$Id$";

  public static final String USER_CONFIG_FILE_EXT = ".etomo";

  private static final int TO_BYTES = 1024;
  public static final double MIN_AVAILABLE_MEMORY_REQUIRED = 2 * TO_BYTES * TO_BYTES;
  public static final int NUMBER_STORABLES = 2;
  private static final String JAVA_MEMORY_LIMIT_ENV_VAR = "ETOMO_MEM_LIM";

  public static final EtomoDirector INSTANCE = new EtomoDirector();

  private SettingsDialog settingsDialog = null;
  private boolean outOfMemoryMessage = false;
  private String originalUserDir = null;
  private boolean testFailed = false;
  private final EtomoNumber javaMemoryLimit = new EtomoNumber(EtomoNumber.Type.LONG);
  private boolean imodBriefHeader = false;
  private EtomoNumber numberOfProcessorsWindows = null;

  // state
  private ManagerKey currentManagerKey = null;
  private boolean defaultWindow = false;
  // advanced dialog state for this instance, this gets set upon startup from
  // the user configuration and can be modified for this instance by either
  // the option or advanced menu items
  private boolean isAdvanced = false;

  // Initialized in initialize() or in function called by initialize().
  private String homeDirectory;
  private UserConfiguration userConfig;
  private ParameterStore parameterStore;
  private UniqueHashedArray managerList;
  private File IMODDirectory;
  private File IMODCalibDirectory;
  private UtilityThread utilityThread;

  private final Arguments arguments = new Arguments();

  private EtomoDirector() {
  }

  public static void main(String[] args) {
    EtomoDirector.INSTANCE.arguments.parse(args);
    if (EtomoDirector.INSTANCE.arguments.isHeadless()) {
      setup(args);
    }
    else {
      SwingUtilities.invokeLater(new Setup(args));
    }
  }

  private static void setup(final String[] args) {
    try {
      if (!EtomoDirector.INSTANCE.arguments.isHelp()) {
        // Print out java properties
        Enumeration enumeration = System.getProperties().propertyNames();
        while (enumeration.hasMoreElements()) {
          Object key = enumeration.nextElement();
          System.err.println(key + ":  " + System.getProperty((String) key));
        }
      }
      if (EtomoDirector.INSTANCE.arguments.isDebug()) {
        // print more environment info
        System.err.println();
        Map env = System.getenv();
        Set keys = env.keySet();
        Iterator iterator = keys.iterator();
        while (iterator.hasNext()) {
          Object key = iterator.next();
          System.err.println(key + ":  " + env.get(key));
        }
        if (EtomoDirector.INSTANCE.arguments.getDebugLevel().isExtra()) {
          System.err.println();
          System.err.println("Java lib:");
          File libDir = new File(new File(System.getProperty("java.home")), "lib");
          String[] libDirList = libDir.list();
          if (libDirList != null) {
            for (int i = 0; i < libDirList.length; i++) {
              System.err.println(libDirList[i]);
            }
          }
        }
      }
      if (!EtomoDirector.INSTANCE.arguments.isHelp()) {
        Utilities.dateTimeStamp();
        Utilities.setStartTime();
      }
      INSTANCE.initialize();
      // automation must be done last in main, otherwise initialization may not
      // complete normally.
      EtomoDirector.INSTANCE.doAutomation();
    }
    catch (OutOfMemoryError e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog((BaseManager) null,
          "WARNING:  Ran out of memory."
              + "\nPlease close open log file windows or exit Etomo.", "Out of Memory");
      if (EtomoDirector.INSTANCE.getArguments().isHeadless()) {
        UIHarness.INSTANCE.exit(AxisID.ONLY, 1);
      }
      throw e;
    }
    catch (Exception e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog((BaseManager) null, e.getMessage(),
          "Exception");
      if (EtomoDirector.INSTANCE.getArguments().isHeadless()) {
        UIHarness.INSTANCE.exit(AxisID.ONLY, 1);
      }
    }
    catch (Error e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog((BaseManager) null, e.getMessage(), "Error");
      if (EtomoDirector.INSTANCE.getArguments().isHeadless()) {
        UIHarness.INSTANCE.exit(AxisID.ONLY, 1);
      }
    }
  }

  private void setAdvanced(boolean state) {
    isAdvanced = state;
  }

  public void doAutomation() {
    if (managerList == null) {
      return;
    }
    BaseManager manager = null;
    if (currentManagerKey != null) {
      manager = (BaseManager) managerList.get(currentManagerKey.getKey());
    }
    if (manager != null) {
      manager.doAutomation();
    }
  }

  /**
   * Do automation in a non-default manager.
   * @param managerKey
   */
  private void doAutomation(final ManagerKey managerKey) {
    if (managerList == null) {
      UIHarness.INSTANCE.openMessageDialog((BaseManager) null,
          "Unable to open interface.", "Interface Failed");
      if (arguments.isHeadless()) {
        UIHarness.INSTANCE.exit(AxisID.ONLY, 1);
      }
    }
    BaseManager manager = null;
    if (managerKey != null) {
      manager = (BaseManager) managerList.get(managerKey.getKey());
    }
    if (manager != null) {
      manager.doAutomation();
    }
  }

  private void initialize() {
    // Get the HOME directory environment variable to find the program
    // configuration file
    homeDirectory = System.getProperty("user.home");
    if (homeDirectory.equals("")) {
      String[] message = new String[2];
      message[0] = "Can not find home directory! Unable to load user preferences";
      message[1] = "Set HOME environment variable and restart program to fix this problem";
      UIHarness.INSTANCE.openMessageDialog(getCurrentManager(), message,
          "Program Initialization Error", AxisID.ONLY);
      System.exit(1);
    }
    imodBriefHeader = EnvironmentVariable.INSTANCE.exists(null, homeDirectory,
        "IMOD_BRIEF_HEADER", AxisID.ONLY);
    if (Utilities.isWindowsOS()
        && EnvironmentVariable.INSTANCE.exists(null, homeDirectory,
            "NUMBER_OF_PROCESSORS", AxisID.ONLY)) {
      numberOfProcessorsWindows = new EtomoNumber();
      numberOfProcessorsWindows.set(EnvironmentVariable.INSTANCE.getValue(null,
          homeDirectory, "NUMBER_OF_PROCESSORS", AxisID.ONLY));
      System.err.println("NUMBER_OF_PROCESSORS:" + numberOfProcessorsWindows);
    }
    // Set the user preferences
    userConfig = new UserConfiguration();
    // Create a File object specifying the user configuration file
    // create the user config file
    File userConfigFile = null;
    try {
      userConfigFile = new File(homeDirectory, USER_CONFIG_FILE_EXT);
    }
    catch (Exception except) {
      System.err.println("Could not create .etomo:");
      System.err.println(except.getMessage());
      System.exit(1);
    }
    // create config file it if it doesn't exist
    try {
      userConfigFile.createNewFile();
    }
    catch (IOException except) {
      System.err.println("Could not create file:" + userConfigFile.getAbsolutePath());
      System.err.println(except.getMessage());
      System.exit(1);
    }
    try {
      if (!arguments.isIgnoreSettings()) {
        parameterStore = ParameterStore.getInstance(userConfigFile);
      }
      else {
        parameterStore = ParameterStore.getFilelessInstance();
      }
      parameterStore.load(userConfig);
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(getCurrentManager(),
          "Can't load user configuration.\n" + except.getMessage(), "Etomo Error");
    }
    setUserPreferences();
    List<String> paramFileNameList = arguments.getParamFileNameList();
    if (arguments.isHelp()) {
      printUsageMessage();
      return;
    }
    UIHarness.INSTANCE.createMainFrame();
    if (!arguments.validate(UIHarness.INSTANCE.getMainFrame())) {
      UIHarness.INSTANCE.exit(AxisID.ONLY, 1);
      return;
    }
    initIMODDirectory();
    int paramFileNameListSize = paramFileNameList.size();
    String paramFileName = null;
    managerList = new UniqueHashedArray();
    // if no param file is found bring up Parallel manager
    if (paramFileNameListSize == 0) {
      defaultWindow = true;
      openFrontPage(true, AxisID.ONLY);
    }
    else {
      ManagerKey saveKey = null;
      ManagerKey managerKey = null;
      for (int i = 0; i < paramFileNameListSize; i++) {
        paramFileName = paramFileNameList.get(i);
        managerKey = null;
        if (paramFileName.endsWith(DataFileType.RECON.extension)) {
          managerKey = openTomogram(paramFileName, false, AxisID.ONLY);
        }
        else if (paramFileName.endsWith(DataFileType.JOIN.extension)) {
          managerKey = openJoin(paramFileName, false, AxisID.ONLY);
        }
        else if (paramFileName.endsWith(DataFileType.PARALLEL.extension)) {
          managerKey = openParallel(paramFileName, false, AxisID.ONLY);
        }
        else if (paramFileName.endsWith(DataFileType.PEET.extension)) {
          managerKey = openPeet(paramFileName, false, AxisID.ONLY);
        }
        else if (paramFileName.endsWith(DataFileType.SERIAL_SECTIONS.extension)) {
          managerKey = openSerialSections(paramFileName, false, AxisID.ONLY);
        }
        if (i == 0) {
          saveKey = managerKey;
        }
      }
      if (saveKey == null) {
        managerKey = openFrontPage(true, AxisID.ONLY);
        saveKey = managerKey;
      }
      currentManagerKey = saveKey;
    }
    initProgram();
    BaseManager manager = null;
    if (currentManagerKey != null) {
      manager = (BaseManager) managerList.get(currentManagerKey.getKey());
    }
    if (manager != null) {
      UIHarness.INSTANCE.setCurrentManager(manager, currentManagerKey.getKey(), true);
    }
    if (currentManagerKey != null) {
      UIHarness.INSTANCE.selectWindowMenuItem(currentManagerKey.getKey());
    }
    setCurrentManager(currentManagerKey, false);
    UIHarness.INSTANCE.setMRUFileLabels(userConfig.getMRUFileList());
    UIHarness.INSTANCE.pack(manager);
    if (manager == null) {
      UIHarness.INSTANCE.openMessageDialog((BaseManager) null, "Invalid dataset file",
          "Unable to Open Dataset");
    }
    UIHarness.INSTANCE.setVisible(manager, true);
    System.err.println("imod:  " + getIMODDirectory());
    if (manager == null) {
      UIHarness.INSTANCE.setTitle(null, MainFrame.ETOMO_TITLE);
    }
  }

  /**
   *  
   */
  private void initProgram() {
    originalUserDir = System.getProperty("user.dir");
    System.err.println("GraphicsEnvironment.isHeadless()="
        + GraphicsEnvironment.isHeadless());
    // print versions
    System.err.println("\neTomo version:  " + ImodVersion.CURRENT_VERSION + " "
        + VersionControl.TIME_STAMP);
    List<String> imodInfo = VersionControl.getImodInfo(null);
    if (imodInfo != null && imodInfo.size() > 0) {
      System.err.println("IMOD Version: " + imodInfo.get(0));
    }
    String version = VersionControl.getPeetVersion();
    if (version != null) {
      System.err.println("PEET Version: " + VersionControl.getPeetVersion());
    }
    System.err.println();
    // Get the IMOD calibration directory so we know where to find documentation
    // Check to see if is defined on the command line first with -D
    // Otherwise check to see if we can get it from the environment
    String imodCalibDirectoryName = System.getProperty(EnvironmentVariable.CALIB_DIR);
    if (imodCalibDirectoryName == null) {
      imodCalibDirectoryName = EnvironmentVariable.INSTANCE.getValue(null, null,
          EnvironmentVariable.CALIB_DIR, AxisID.ONLY);
      if (!imodCalibDirectoryName.equals("")) {
        if (arguments.isDebug()) {
          System.err.println(EnvironmentVariable.CALIB_DIR + " (env): "
              + imodCalibDirectoryName);
        }
      }
      else {
        System.err.println("WARNING:\nThe environment variable "
            + EnvironmentVariable.CALIB_DIR + "is not set.\n"
            + "Several eTomo functions will not be available:\n"
            + "Image distortion field files, " + "Mag gradient correction, "
            + "and parallel processing.\n");
      }
    }
    else {
      if (arguments.isDebug()) {
        System.err.println(EnvironmentVariable.CALIB_DIR + " (-D): "
            + imodCalibDirectoryName);
      }
    }
    IMODCalibDirectory = new File(imodCalibDirectoryName);
    utilityThread = new UtilityThread();
    new Thread(utilityThread).start();
    // get the java memory limit
    // check it before complaining about having too little memory available
    // SGI seems to go very low on the available memory, but its fine as long
    // as long as it does't get near the java memory limit.
    String sJavaMemoryLimit = EnvironmentVariable.INSTANCE.getValue(null,
        originalUserDir, JAVA_MEMORY_LIMIT_ENV_VAR, AxisID.ONLY);
    if (sJavaMemoryLimit != null) {
      int conversionNumber = 1;
      if (sJavaMemoryLimit.endsWith("k") || sJavaMemoryLimit.endsWith("K")) {
        conversionNumber = TO_BYTES;
        sJavaMemoryLimit = sJavaMemoryLimit.substring(0, sJavaMemoryLimit.length() - 1);
      }
      else if (sJavaMemoryLimit.endsWith("m") || sJavaMemoryLimit.endsWith("M")) {
        conversionNumber = TO_BYTES * TO_BYTES;
        sJavaMemoryLimit = sJavaMemoryLimit.substring(0, sJavaMemoryLimit.length() - 1);
      }
      javaMemoryLimit.set(sJavaMemoryLimit);
      javaMemoryLimit.set(javaMemoryLimit.getLong() * conversionNumber);
      System.err.println(JAVA_MEMORY_LIMIT_ENV_VAR + "=" + javaMemoryLimit);
      System.err
          .println("MIN_AVAILABLE_MEMORY_REQUIRED=" + MIN_AVAILABLE_MEMORY_REQUIRED);
    }
  }

  private void printProperties(String type) {
    System.err.println("\nprintProperties:type=" + type);
    if (type == null) {
      return;
    }
    java.util.Enumeration keys = UIManager.getDefaults().keys();
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      String lowerCaseKey = key.toString().toLowerCase();
      if (lowerCaseKey.indexOf(type.toLowerCase()) != -1) {
        System.err.println(key + "=" + UIManager.get(key));
      }
    }
    System.err.println();
  }

  private final void initIMODDirectory() {
    // Get the IMOD directory so we know where to find documentation
    // Check to see if is defined on the command line first with -D
    // Otherwise check to see if we can get it from the environment
    String imodDirectoryName = System.getProperty("IMOD_DIR");
    if (imodDirectoryName == null) {
      imodDirectoryName = EnvironmentVariable.INSTANCE.getValue(null, null, "IMOD_DIR",
          AxisID.ONLY);
      if (imodDirectoryName.equals("")) {
        String[] message = new String[3];
        message[0] = "Can not find IMOD directory!";
        message[1] = "Set IMOD_DIR environment variable and restart program to fix this problem";
        UIHarness.INSTANCE.openMessageDialog(getCurrentManager(), message,
            "Program Initialization Error", AxisID.ONLY);
        System.exit(1);
      }
      else {
        if (arguments.isDebug()) {
          System.err.println("IMOD_DIR (env): " + imodDirectoryName);
        }
      }
    }
    else {
      if (arguments.isDebug()) {
        System.err.println("IMOD_DIR (-D): " + imodDirectoryName);
      }
    }
    IMODDirectory = new File(imodDirectoryName);
  }

  /**
   * Gets the current manager.  It is important that
   * this function remain package level because the current manager changes when
   * the user switches tabs, which would change the functionality of code
   * executed after a process is finished.
   * @return the current manager
   */
  private BaseManager getCurrentManager() {
    if (currentManagerKey == null) {
      return null;
    }
    return (BaseManager) managerList.get(currentManagerKey.getKey());
  }

  public BaseManager getCurrentManagerForTest() {
    if (!arguments.isTest()) {
      throw new IllegalStateException("Illegal use of getCurrentManagerForTest");
    }
    if (currentManagerKey == null) {
      return null;
    }
    return (BaseManager) managerList.get(currentManagerKey.getKey());
  }

  /**
   * for testing.
   * @param propertyUserDir
   * @return the old property user dir
   */
  public String setCurrentPropertyUserDir(String propertyUserDir) {
    if (!arguments.isTest()) {
      throw new IllegalStateException("test-only function");
    }
    if (currentManagerKey == null) {
      return System.setProperty("user.dir", propertyUserDir);
    }
    return ((BaseManager) managerList.get(currentManagerKey.getKey()))
        .setPropertyUserDir(propertyUserDir);
  }

  /**
   * Called when the propertyUserDir is empty, or there is no manager.
   */
  public final void makeOriginalDirLocal() {
    System.setProperty("user.dir", originalUserDir);
    Utilities.managerStamp(originalUserDir, null);
  }

  public final String getOriginalUserDir() {
    return originalUserDir;
  }

  public void setCurrentManager(ManagerKey managerKey) {
    setCurrentManager(managerKey, false);
  }

  public void setCurrentManager(UniqueKey key) {
    setCurrentManager(key, false);
  }

  /**
   * Gets a manager from managerList with managerKey.
   * @param managerKey
   * @param newWindow
   */
  public synchronized void setCurrentManager(ManagerKey managerKey, boolean newWindow) {
    if (managerKey == null || managerKey == null || managerKey.getKey() == null) {
      return;
    }
    setCurrentManager((BaseManager) managerList.get(managerKey.getKey()), managerKey,
        newWindow);
  }

  /**
   *  Gets a manager from managerList with key.
   * @param key
   * @param newWindow
   */
  public synchronized void setCurrentManager(UniqueKey key, boolean newWindow) {
    if (key == null) {
      return;
    }
    BaseManager newCurrentManager = (BaseManager) managerList.get(key);
    setCurrentManager(newCurrentManager, newCurrentManager.getManagerKey(), newWindow);
  }

  /**
   * Checks newCurrentManager, sets currentManagerKey, and sets the current
   * manager in UIHarness.
   * @param managerKey
   * @param newWindow
   */
  private void setCurrentManager(BaseManager newCurrentManager, ManagerKey managerKey,
      boolean newWindow) {
    if (newCurrentManager == null) {
      throw new NullPointerException("managerKey=" + managerKey);
    }
    currentManagerKey = managerKey;
    UIHarness.INSTANCE.setCurrentManager(newCurrentManager, currentManagerKey.getKey(),
        newWindow);
  }

  /**
   * Creates ApplicationManager and adds it to ManagerList.
   * @param etomoDataFileName
   * @param makeCurrent
   * @param axisID
   * @return
   */
  private ManagerKey openTomogram(String etomoDataFileName, boolean makeCurrent,
      AxisID axisID) {
    ApplicationManager manager;
    if (etomoDataFileName == null || etomoDataFileName.equals(MetaData.getNewFileTitle())) {
      manager = new ApplicationManager("", axisID);
      UIHarness.INSTANCE.setEnabledNewTomogramMenuItem(false);
    }
    else {
      manager = new ApplicationManager(etomoDataFileName, axisID);
    }
    return setManager(manager, makeCurrent);
  }

  public ManagerKey openJoin(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    return openJoin(JoinMetaData.getNewFileTitle(), makeCurrent, axisID);
  }

  public ManagerKey openParallel(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    return openParallel((String) null, makeCurrent, axisID);
  }

  public ManagerKey openGenericParallel(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    return openParallel(ParallelMetaData.NEW_GENERIC_PARALLEL_PROCESS_TITLE, makeCurrent,
        axisID);
  }

  public ManagerKey openAnisotropicDiffusion(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    return openParallel(ParallelMetaData.NEW_ANISOTROPIC_DIFFUSION_TITLE, makeCurrent,
        axisID);
  }

  /**
   * Build, save, and display a ToolsManager instance.
   * @param dialogType may not be null
   */
  public void openTools(ToolType toolType) {
    ToolsManager manager = new ToolsManager(toolType);
    UIHarness.INSTANCE.addFrame(manager, false);
    manager.initialize();
    Utilities.managerStamp(manager.getPropertyUserDir(), manager.getName());
  }

  /**
   * Build, save, and display a ToolsManager instance.
   * @param axisID
   * @param dialogType may not be null
   */
  public void openDirectiveEditor(final DirectiveFileType directiveFileType,
      final BaseManager dataSource, final String timestamp, final StringBuffer errmsg) {
    DirectiveEditorManager manager = new DirectiveEditorManager(directiveFileType,
        dataSource, timestamp, errmsg);
    UIHarness.INSTANCE.addFrame(manager, true);
    manager.initialize();
    Utilities.managerStamp(manager.getPropertyUserDir(), manager.getName());
  }

  public ManagerKey openPeet(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    return openPeet(PeetMetaData.NEW_TITLE, makeCurrent, axisID);
  }

  public ManagerKey openSerialSections(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    return openSerialSections(SerialSectionsMetaData.NEW_TITLE, makeCurrent, axisID);
  }

  private ManagerKey openJoin(File etomoJoinFile, boolean makeCurrent, AxisID axisID) {
    if (etomoJoinFile == null) {
      return openJoin(makeCurrent, axisID);
    }
    return openJoin(etomoJoinFile.getAbsolutePath(), makeCurrent, axisID);
  }

  public ManagerKey openFrontPage(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    FrontPageManager manager = new FrontPageManager();
    return setManager(manager, makeCurrent);
  }

  private ManagerKey openParallel(File etomoParallelFile, boolean makeCurrent,
      AxisID axisID) {
    if (etomoParallelFile == null) {
      return openParallel(makeCurrent, axisID);
    }
    return openParallel(etomoParallelFile.getAbsolutePath(), makeCurrent, axisID);
  }

  private ManagerKey openPeet(File etomoPeetFile, boolean makeCurrent, AxisID axisID) {
    if (etomoPeetFile == null) {
      return openPeet(makeCurrent, axisID);
    }
    return openPeet(etomoPeetFile.getAbsolutePath(), makeCurrent, axisID);
  }

  private ManagerKey openSerialSections(File etomoSerialSectionsFile,
      boolean makeCurrent, AxisID axisID) {
    if (etomoSerialSectionsFile == null) {
      return openSerialSections(makeCurrent, axisID);
    }
    return openSerialSections(etomoSerialSectionsFile.getAbsolutePath(), makeCurrent,
        axisID);
  }

  private ManagerKey openJoin(String etomoJoinFileName, boolean makeCurrent, AxisID axisID) {
    JoinManager manager;
    if (etomoJoinFileName == null
        || etomoJoinFileName.equals(JoinMetaData.getNewFileTitle())) {
      manager = new JoinManager("", axisID);
      UIHarness.INSTANCE.setEnabledNewJoinMenuItem(false);
    }
    else {
      manager = new JoinManager(etomoJoinFileName, axisID);
    }
    return setManager(manager, makeCurrent);
  }

  private ManagerKey openParallel(String parallelFileName, boolean makeCurrent,
      AxisID axisID) {
    ParallelManager manager;
    if (parallelFileName == null) {
      manager = new ParallelManager();
    }
    else if (parallelFileName.equals(ParallelMetaData.NEW_GENERIC_PARALLEL_PROCESS_TITLE)) {
      manager = new ParallelManager(DialogType.PARALLEL);
      UIHarness.INSTANCE.setEnabledNewGenericParallelMenuItem(false);
    }
    else if (parallelFileName.equals(ParallelMetaData.NEW_ANISOTROPIC_DIFFUSION_TITLE)) {
      manager = new ParallelManager(DialogType.ANISOTROPIC_DIFFUSION);
      UIHarness.INSTANCE.setEnabledNewAnisotropicDiffusionMenuItem(false);
    }
    else {
      manager = new ParallelManager(parallelFileName);
    }
    return setManager(manager, makeCurrent);
  }

  private ManagerKey openPeet(String peetFileName, boolean makeCurrent, AxisID axisID) {
    PeetManager manager;
    if (peetFileName == null || peetFileName.equals(PeetMetaData.NEW_TITLE)) {
      manager = PeetManager.getInstance();
      UIHarness.INSTANCE.setEnabledNewPeetMenuItem(false);
    }
    else {
      manager = PeetManager.getInstance(peetFileName);
    }
    ManagerKey key = setManager(manager, makeCurrent);
    manager.display();
    if (!manager.isValid()) {
      closeCurrentManager(AxisID.ONLY, false);
      return null;
    }
    return key;
  }

  private ManagerKey openSerialSections(String serialSectionsFileName,
      boolean makeCurrent, AxisID axisID) {
    SerialSectionsManager manager;
    if (serialSectionsFileName == null
        || serialSectionsFileName.equals(SerialSectionsMetaData.NEW_TITLE)) {
      manager = SerialSectionsManager.getInstance();
      UIHarness.INSTANCE.setEnabledNewSerialSectionsMenuItem(false);
    }
    else {
      manager = SerialSectionsManager.getInstance(serialSectionsFileName);
    }
    ManagerKey key = setManager(manager, makeCurrent);
    manager.display();
    if (!manager.isValid()) {
      closeCurrentManager(AxisID.ONLY, false);
      return null;
    }
    return key;
  }

  private ManagerKey setManager(BaseManager manager, boolean makeCurrent) {
    UniqueKey uniqueKey;
    uniqueKey = managerList.add(manager.getName(), manager);
    manager.setManagerKey(uniqueKey);
    ManagerKey managerKey = manager.getManagerKey();
    UIHarness.INSTANCE.addWindow(manager, managerKey.getKey());
    if (makeCurrent) {
      UIHarness.INSTANCE.selectWindowMenuItem(managerKey.getKey(), true);
      setCurrentManager(managerKey, true);
    }
    return managerKey;
  }

  public void openTomogram(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    openTomogram(MetaData.getNewFileTitle(), makeCurrent, axisID);
  }

  public void openTomogramAndDoAutomation(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    doAutomation(openTomogram(MetaData.getNewFileTitle(), makeCurrent, axisID));
  }

  /**
   * When etomo is run with no data file, it automatically opens a Setup
   * Tomogram window.  This window should be closed if the user opens another
   * window without adding data to the Setup Tomogram fields.  This is only true
   * if the Setup Tomogram was opened as the default window.
   */
  private void closeDefaultWindow(AxisID axisID) {
    if (defaultWindow && managerList.size() == 1) {
      BaseManager manager = ((BaseManager) managerList.get(currentManagerKey.getKey()));
      if (manager instanceof FrontPageManager) {
        defaultWindow = false;
        closeCurrentManager(axisID, false);
      }
    }
  }

  private void saveLogs() {
    if (managerList != null) {
      for (int i = 0; i < managerList.size(); i++) {
        ((BaseManager) managerList.get(i)).saveLog();
      }
    }
  }

  public void openManager(File dataFile, boolean makeCurrent, AxisID axisID) {
    if (dataFile == null) {
      throw new IllegalStateException("null dataFile");
    }
    closeDefaultWindow(axisID);
    EtomoFileFilter etomoFileFilter = new EtomoFileFilter();
    if (etomoFileFilter.accept(dataFile)) {
      openTomogram(dataFile, makeCurrent, axisID);
      return;
    }
    JoinFileFilter joinFileFilter = new JoinFileFilter();
    if (joinFileFilter.accept(dataFile)) {
      openJoin(dataFile, makeCurrent, axisID);
      return;
    }
    ParallelFileFilter parallelFileFilter = new ParallelFileFilter();
    if (parallelFileFilter.accept(dataFile)) {
      openParallel(dataFile, makeCurrent, axisID);
      return;
    }
    PeetFileFilter peetFileFilter = new PeetFileFilter();
    if (peetFileFilter.accept(dataFile)) {
      openPeet(dataFile, makeCurrent, axisID);
      return;
    }
    SerialSectionsFileFilter serialSectionsFileFilter = new SerialSectionsFileFilter();
    if (serialSectionsFileFilter.accept(dataFile)) {
      openSerialSections(dataFile, makeCurrent, axisID);
      return;
    }
    UIHarness.INSTANCE.openMessageDialog(getCurrentManager(), "Unknown file type "
        + dataFile.getName() + ".", "Unknown File Type", axisID);
    throw new IllegalStateException("unknown dataFile");
  }

  public void openTomogram(File etomoDataFile, boolean makeCurrent, AxisID axisID) {
    if (etomoDataFile == null) {
      openTomogram(makeCurrent, axisID);
    }
    else {
      openTomogram(etomoDataFile.getAbsolutePath(), makeCurrent, axisID);
    }
  }

  public boolean closeCurrentManager(AxisID axisID, boolean exiting) {
    BaseManager currentManager = getCurrentManager();
    if (currentManager == null) {
      return true;
    }
    if (exiting) {
      if (!currentManager.exitProgram(axisID)) {
        return false;
      }
    }
    else {
      if (!currentManager.close(axisID)) {
        return false;
      }
    }
    managerList.remove(currentManagerKey.getKey());
    enableOpenManagerMenuItem();
    UIHarness.INSTANCE.removeWindow(currentManagerKey.getKey());
    currentManagerKey = null;
    if (managerList.size() == 0) {
      UIHarness.INSTANCE.removeWindow(null);
      // mainFrame.setWindowMenuLabels(controllerList);
      UIHarness.INSTANCE.setCurrentManager(null, null);
      UIHarness.INSTANCE.selectWindowMenuItem((UniqueKey) null);
      setCurrentManager((ManagerKey) null, false);
      return true;
    }
    setCurrentManager(managerList.getKey(0));
    UIHarness.INSTANCE.selectWindowMenuItem(managerList.getKey(0));
    return true;
  }

  private void enableOpenManagerMenuItem() {
    UniqueKey key = currentManagerKey.getKey();
    if (key.getName().equals(MetaData.getNewFileTitle())) {
      UIHarness.INSTANCE.setEnabledNewTomogramMenuItem(true);
    }
    else if (key.getName().equals(JoinMetaData.getNewFileTitle())) {
      UIHarness.INSTANCE.setEnabledNewJoinMenuItem(true);
    }
    else if (key.getName().equals(ParallelMetaData.NEW_GENERIC_PARALLEL_PROCESS_TITLE)) {
      UIHarness.INSTANCE.setEnabledNewGenericParallelMenuItem(true);
    }
    else if (key.getName().equals(ParallelMetaData.NEW_ANISOTROPIC_DIFFUSION_TITLE)) {
      UIHarness.INSTANCE.setEnabledNewAnisotropicDiffusionMenuItem(true);
    }
    else if (key.getName().equals(PeetMetaData.NEW_TITLE)) {
      UIHarness.INSTANCE.setEnabledNewPeetMenuItem(true);
    }
    else if (key.getName().equals(SerialSectionsMetaData.NEW_TITLE)) {
      UIHarness.INSTANCE.setEnabledNewSerialSectionsMenuItem(true);
    }
  }

  /**
   * Set failure while doing UI testing to prevent popups as the test tries to
   * end.
   * @param failed
   */
  public void setTestFailed(boolean input) {
    if (!arguments.isTest()) {
      throw new IllegalStateException("test=" + arguments.isTest());
    }
    testFailed = input;
  }

  /**
   * @return true if this is a UI test and it has failed
   */
  public boolean isTestFailed() {
    return testFailed;
  }

  /**
   * Close all managers, unless the user prevents it.  If all managers are
   * closed, stop threads, save data and return true.  To guarantee that etomo
   * can exit during a failure, catch all uncaught Exceptions and Errors and
   * return true.
   * @return true if etomo can exit, false if the user prevented exit
   */
  public boolean exitProgram(AxisID axisID) {
    try {
      saveLogs();
      if (managerList != null) {
        while (managerList.size() != 0) {
          if (!closeCurrentManager(axisID, true)) {
            return false;
          }
        }
      }
      if (utilityThread != null) {
        utilityThread.stop();
      }
      ProcessRestarter.stop();
      IntermittentBackgroundProcess.stop();
      if (isMemoryAvailable()) {
        // Should we close the 3dmod windows
        // Save the current window size to the user config
        Dimension size = UIHarness.INSTANCE.getSize(null);
        userConfig.setMainWindowWidth(size.width);
        userConfig.setMainWindowHeight(size.height);
        // Write out the user configuration data
        parameterStore.save(userConfig);
        return true;
      }
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
    return true;
  }

  public ParameterStore getParameterStore() {
    return parameterStore;
  }

  public void renameCurrentManager(String managerName) {
    enableOpenManagerMenuItem();
    UniqueKey oldKey = currentManagerKey.getKey();
    currentManagerKey.setKey(managerList.rekey(currentManagerKey.getKey(), managerName));
    UIHarness.INSTANCE.renameWindow(oldKey, currentManagerKey.getKey());
  }

  public UserConfiguration getUserConfiguration() {
    if (userConfig == null) {
      throw new NullPointerException();
    }
    return userConfig;
  }

  private final void printUsageMessage() {
    System.out.println("Usage: etomo [options] [data files]\n");
    Arguments.printHelpMessage();
  }

  /**
   * Set the user preferences
   */
  private void setUserPreferences() {
    if (arguments.isHeadless()) {
      return;
    }
    ToolTipManager.sharedInstance().setInitialDelay(userConfig.getToolTipsInitialDelay());
    ToolTipManager.sharedInstance().setDismissDelay(userConfig.getToolTipsDismissDelay());
    setUIFont(userConfig.getFontFamily(), userConfig.getFontSize());
    setLookAndFeel(userConfig.getNativeLookAndFeel());
    isAdvanced = userConfig.getAdvancedDialogs();
    UIParameters.INSTANCE.setFontSize(userConfig.getFontSize());
    // CpuAdoc.INSTANCE.setUserConfig(userConfig.getParallelProcessing(), userConfig
    // .getCpus());
  }

  /**
   * Sets the look and feel for the program.
   * 
   * @param nativeLookAndFeel
   *          set to true to use the host os look and feel, false will use the
   *          Metal look and feel.
   */
  private void setLookAndFeel(boolean nativeLookAndFeel) {
    String lookAndFeelClassName;

    // UIManager.LookAndFeelInfo plaf[] = UIManager.getInstalledLookAndFeels();
    // for(int i = 0; i < plaf.length; i++) {
    // System.err.println(plaf[i].getClassName());
    // }
    System.err.println();
    String osName = System.getProperty("os.name");
    if (arguments.isDebug()) {
      System.err.println("os.name: " + osName);
    }
    if (nativeLookAndFeel) {
      if (osName.startsWith("Mac OS X")) {
        lookAndFeelClassName = "apple.laf.AquaLookAndFeel";
        if (arguments.isDebug()) {
          System.err.println("Setting AquaLookAndFeel");
        }
      }
      else if (osName.startsWith("Windows")) {
        lookAndFeelClassName = "com.sun.java.swing.plaf.windows.WindowsLookAndFeel";
        if (arguments.isDebug()) {
          System.err.println("Setting WindowsLookAndFeel");
        }
      }
      else {
        lookAndFeelClassName = "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
        if (arguments.isDebug()) {
          System.err.println("Setting MotifLookAndFeel");
        }
      }
    }
    else {
      lookAndFeelClassName = UIManager.getCrossPlatformLookAndFeelClassName();
      if (arguments.isDebug()) {
        System.err.println("Setting MetalLookAndFeel");
      }
    }
    try {
      UIManager.setLookAndFeel(lookAndFeelClassName);
    }
    catch (Exception excep) {
      System.err.println("Could not set " + lookAndFeelClassName + " look and feel");
    }
    if (arguments.getDebugLevel().isExtra()) {
      // print look and feel info
      System.err.println("\nLook and feel defaults:");
      UIDefaults uiDefaults = UIManager.getLookAndFeelDefaults();
      Enumeration enumeration = uiDefaults.keys();
      while (enumeration.hasMoreElements()) {
        Object key = enumeration.nextElement();
        System.err.println(key + ":  " + uiDefaults.getString(key));
      }
    }
  }

  /**
   *  
   */
  private void setUIFont(String fontFamily, int fontSize) {
    // sets the default font for all Swing components.
    // ex.
    // setUIFont (new javax.swing.plaf.FontUIResource("Serif",Font.ITALIC,12));
    // Taken from: http://www.rgagnon.com/javadetails/java-0335.html
    java.util.Enumeration keys = UIManager.getDefaults().keys();
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      Object value = UIManager.get(key);
      if (value instanceof FontUIResource) {
        FontUIResource currentFont = (FontUIResource) value;
        FontUIResource newFont = new FontUIResource(fontFamily, currentFont.getStyle(),
            fontSize);
        UIManager.put(key, newFont);
      }
    }
  }

  public Arguments getArguments() {
    return arguments;
  }

  /**
   * Return the IMOD directory
   */
  public File getIMODDirectory() {
    // Return a copy of the IMODDirectory object
    return new File(IMODDirectory.getAbsolutePath());
  }

  public ConstEtomoNumber getNumberOfProcessorsWindows() {
    return numberOfProcessorsWindows;
  }

  /**
   * Return the IMOD calibration directory
   */
  public File getIMODCalibDirectory() {
    // Return a copy of the IMODDirectory object
    return new File(IMODCalibDirectory.getAbsolutePath());
  }

  /**
   * Get the current advanced state
   */
  public boolean getAdvanced() {
    return isAdvanced;
  }

  /**
   *  
   */
  public void getSettingsParameters() {
    if (settingsDialog != null) {
      if (settingsDialog.isAppearanceSettingChanged(userConfig)) {
        UIHarness.INSTANCE
            .openInfoMessageDialog(
                getCurrentManager(),
                "You must exit from eTomo and re-run it for this change to fully take effect.",
                "Settings", AxisID.FIRST);
      }
      settingsDialog.getParameters(userConfig);
      setUserPreferences();
      UIHarness.INSTANCE.repaintWindow(null);
    }
  }

  private String getPropertyUserDir() {
    return currentManagerKey == null ? originalUserDir : getCurrentManager()
        .getPropertyUserDir();
  }

  /**
   * Open up the settings dialog box
   */
  public void openSettingsDialog() {
    // Open the dialog in the appropriate mode for the current state of
    // processing
    if (settingsDialog == null) {
      settingsDialog = SettingsDialog.getInstance(getCurrentManager(),
          getPropertyUserDir());
      settingsDialog.setParameters(userConfig);
      Dimension frmSize = UIHarness.INSTANCE.getSize(getCurrentManager());
      Point loc = UIHarness.INSTANCE.getLocation(getCurrentManager());
      settingsDialog.setLocation(loc.x, loc.y + 20);
      settingsDialog.setModal(false);
    }
    settingsDialog.setVisible(true);
  }

  public void saveSettingsDialog() {
    try {
      parameterStore.save(userConfig);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(getCurrentManager(),
          "Unable to save or write preferences to " + parameterStore.getAbsolutePath()
              + ".\n" + e.getMessage(), "Etomo Error");
    }
    catch (IOException e) {
      UIHarness.INSTANCE.openMessageDialog(getCurrentManager(),
          "Unable to save or write preferences to " + parameterStore.getAbsolutePath()
              + ".\n" + e.getMessage(), "Etomo Error");
    }
  }

  /**
   *  
   */
  public void closeSettingsDialog() {
    if (settingsDialog != null) {
      settingsDialog.dispose();
    }
  }

  public long getAvailableMemory() {
    // System.err.println("max=  " + Runtime.getRuntime().maxMemory());
    // System.err.println("total=" + Runtime.getRuntime().totalMemory());
    // System.err.println("free= " + Runtime.getRuntime().freeMemory());
    return Runtime.getRuntime().maxMemory() - Runtime.getRuntime().totalMemory()
        + Runtime.getRuntime().freeMemory();
  }

  public boolean isImodBriefHeader() {
    return imodBriefHeader;
  }

  public boolean isMemoryAvailable() {
    long availableMemory = getAvailableMemory();
    long usedMemory = Runtime.getRuntime().totalMemory()
        - Runtime.getRuntime().freeMemory();
    // System.out.println();
    // System.out.println("Available memory = " + availableMemory);
    // System.out.println("Memory in use    = " + usedMemory);
    // System.out.println();
    if (arguments.isDebug() || arguments.isDisplayMemory()) {
      System.err.println("Available memory = " + availableMemory);
      System.err.println("Memory in use    = " + usedMemory);
    }
    // Check to see if the memory has been made available up to the memory limit.
    // SGI doesn't make all the memory available up to the memory limit until it
    // needs to.
    // Memory limit is adjusted down because availableMemory never matches
    // javaMemoryLimit.
    // Old code for SGI
    /* if (javaMemoryLimit.isNull() || availableMemory + usedMemory >=
     * javaMemoryLimit.getLong() - (MIN_AVAILABLE_MEMORY_REQUIRED * 3)) { //Check
     * available memory if (availableMemory < MIN_AVAILABLE_MEMORY_REQUIRED) { //send
     * message once per memory problem if (!outOfMemoryMessage) {
     * UIHarness.INSTANCE.openMessageDialog(
     * "WARNING:  Ran out of memory.  Changes to the .edf file and/or" +
     * " comscript files may not be saved." +
     * "\nPlease close open windows or exit Etomo.", "Out of Memory"); }
     * outOfMemoryMessage = true; return false; } } */
    if (availableMemory <= MIN_AVAILABLE_MEMORY_REQUIRED) {
      if (!outOfMemoryMessage) {
        UIHarness.INSTANCE.openMessageDialog(getCurrentManager(),
            "WARNING:  Ran out of memory.  Changes to the .edf file and/or"
                + " comscript files may not be saved."
                + "\nPlease close open windows or exit Etomo.", "Out of Memory");
        outOfMemoryMessage = true;
      }
      return false;
    }
    // memory problem is gone - reset message
    outOfMemoryMessage = false;
    return true;
  }

  /**
   * Return the users home directory environment variable HOME or an empty
   * string if it doesn't exist.
   */
  public String getHomeDirectory() {
    return homeDirectory;
  }

  private static final class Setup implements Runnable {
    private final String[] args;

    private Setup(final String[] args) {
      this.args = args;
    }

    public void run() {
      setup(args);
    }
  }

  private final class UtilityThread implements Runnable {
    private final int autoSaveLogInterval = 1;// in minutes
    private final int autoSaveLogEvery;
    private final boolean displayMemory;
    private final int displayMemoryInterval;
    private final int displayMemoryEvery;
    private final int sleep;// in minutes

    private boolean stop = false;
    private Thread utilityThread = null;

    private UtilityThread() {
      displayMemoryInterval = arguments.getDisplayMemoryInterval();
      displayMemory = arguments.isDisplayMemory() && displayMemoryInterval >= 1;
      if (!displayMemory || displayMemoryInterval == autoSaveLogInterval) {
        // sleep for five minutes
        sleep = autoSaveLogInterval;
        // run autosave after every sleep
        autoSaveLogEvery = 1;
        displayMemoryEvery = 1;// ignored if !displayMemory
      }
      else {
        // This could be more complicated to allow longer sleeps, but I don't
        // think its worth it.
        sleep = 1;
        // run autosave after 5 one-minute sleeps
        autoSaveLogEvery = autoSaveLogInterval;
        displayMemoryEvery = displayMemoryInterval;
      }
    }

    public final void run() {
      utilityThread = Thread.currentThread();
      if (displayMemory) {
        System.err.println(new Date());
        INSTANCE.isMemoryAvailable();
      }
      int autoSaveLogCount = 0;
      int displayMemoryCount = 0;
      while (!stop) {
        try {
          Thread.sleep(1000 * 60 * sleep);
        }
        catch (InterruptedException e) {
          e.printStackTrace();
        }
        if (displayMemory) {
          if (++displayMemoryCount == displayMemoryEvery) {
            displayMemoryCount = 0;
            System.err.println(new Date());
            INSTANCE.isMemoryAvailable();
          }
        }
        if (++autoSaveLogCount == autoSaveLogEvery) {
          autoSaveLogCount = 0;
          INSTANCE.saveLogs();
        }
      }
      utilityThread = null;
    }

    private final void stop() {
      stop = true;
      if (utilityThread != null) {
        utilityThread.interrupt();
      }
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.109  2011/09/10 03:58:16  sueh
 * <p> Bug# 1441 Added ACTION_TAG.
 * <p>
 * <p> Revision 1.108  2011/07/23 03:23:04  sueh
 * <p> Bug# 1517 In initialize printing the value of NUMBER_OF_PROCESSORS (Windows).
 * <p>
 * <p> Revision 1.107  2011/07/23 02:59:39  sueh
 * <p> Bug# 1517 Set numberOfProcessorsWindows to null unless the OS is Windows.
 * <p>
 * <p> Revision 1.106  2011/07/23 02:26:32  sueh
 * <p> Bug# 1517 Added numberOfProcessorsWindows.
 * <p>
 * <p> Revision 1.105  2011/07/14 00:29:12  sueh
 * <p> Bug# 1411 In openSettingsDialog open the Settings dialog a small, fixed amount below the main frame
 * <p> location, rather then below the entire frame.
 * <p>
 * <p> Revision 1.104  2011/02/21 21:05:27  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.103  2010/11/13 16:02:54  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.102  2010/10/13 20:17:58  sueh
 * <p> bug# 1392 Handling the ignoresettings parameter by loading userconfig
 * <p> with a file.
 * <p>
 * <p> Revision 1.101  2010/09/24 00:53:35  sueh
 * <p> bug# 1404 Correcting comment.
 * <p>
 * <p> Revision 1.100  2010/09/08 19:16:09  sueh
 * <p> bug# 1401 Added doAutomation(ManagerKey).  Removed unnecessary
 * <p> return value in openTomogram.
 * <p>
 * <p> Revision 1.99  2010/07/08 01:16:55  sueh
 * <p> bug# 1387 Getting rid of the !%!@#% includes for resource printing
 * <p> because it didn't compile on Windows.
 * <p>
 * <p> Revision 1.98  2010/07/08 01:00:27  sueh
 * <p> bug# 1387 Removed resource print because it didn't compile.
 * <p>
 * <p> Revision 1.97  2010/07/07 21:49:36  sueh
 * <p> bug# 1387 printing information about the java lib directory.
 * <p>
 * <p> Revision 1.96  2010/07/07 21:21:56  sueh
 * <p> bug# 1387 printing more information.
 * <p>
 * <p> Revision 1.95  2010/07/02 20:17:19  sueh
 * <p> bug# 1387 Moving system information to the top, so it will be there during
 * <p> any failure.
 * <p>
 * <p> Revision 1.94  2010/07/02 20:00:33  sueh
 * <p> bug# 1387 Putting information print earlier.
 * <p>
 * <p> Revision 1.93  2010/06/02 21:43:45  sueh
 * <p> bug# 1380 Improved the comment for exitProgram and changed testDone
 * <p> to testFailed for clarity.
 * <p>
 * <p> Revision 1.92  2010/02/17 04:40:16  sueh
 * <p> bug# 1301 Getting rid of managerkey and using manager to pop up
 * <p> messages.  Need to know what kind of manager we have to pick the right
 * <p> frame.
 * <p>
 * <p> Revision 1.91  2010/01/13 21:47:06  sueh
 * <p> bug# 1298 Do not open any file that doesn't have a dataset extension.
 * <p>
 * <p> Revision 1.90  2010/01/11 23:47:40  sueh
 * <p> bug# 1299 Removed responsibility anything other then cpu.adoc from
 * <p> CpuAdoc.
 * <p>
 * <p> Revision 1.89  2009/10/28 17:46:42  sueh
 * <p> bug# 1275 Changed closeDefaultWindow so that it closes
 * <p> FrontPageManager.
 * <p>
 * <p> Revision 1.88  2009/10/27 20:38:19  sueh
 * <p> bug# 1275 Making FrontPageManager the default manager.  Added
 * <p> openFrontPage.
 * <p>
 * <p> Revision 1.87  2009/10/23 19:42:06  sueh
 * <p> bug# 1275 No longer using a default manager.  Added FrontPagePanel
 * <p> instead.  Separate options for generic parallel process and NAD.
 * <p>
 * <p> Revision 1.86  2009/08/24 20:22:20  sueh
 * <p> bug# 1254 Printing headless state.
 * <p>
 * <p> Revision 1.85  2009/06/22 15:32:43  sueh
 * <p> bug# 1224 Added ImodVersion to the log.
 * <p>
 * <p> Revision 1.84  2009/06/10 17:21:30  sueh
 * <p> bug# 1202 Added imodBriefHeader.
 * <p>
 * <p> Revision 1.83  2009/04/13 22:21:03  sueh
 * <p> bug# 1207 In exitProgram avoided null pointer exception.
 * <p>
 * <p> Revision 1.82  2009/04/02 19:05:38  sueh
 * <p> bug# 1206 Handle managerKey is null.
 * <p>
 * <p> Revision 1.81  2009/03/17 00:22:42  sueh
 * <p> bug# 1186 Changed currentManagerKey to a ManagerKey, which holds a UniqueKey.  Set the unique key in managerKey when it changes.
 * <p>
 * <p> Revision 1.80  2009/03/09 17:22:28  sueh
 * <p> bug# 1172 For now just adding --fg option.
 * <p>
 * <p> Revision 1.79  2009/02/04 22:39:39  sueh
 * <p> bug# 1158 Changed MemoryThread to UtilityThread.  Made it a thread for
 * <p> running multiple periodic tasks.  Always runs now.  Added saving the log
 * <p> panel to it.
 * <p>
 * <p> Revision 1.78  2008/12/15 22:56:43  sueh
 * <p> bug# 1161 Made EtomoDirector.getCurrentManager private.  Added a
 * <p> public test version for public access.
 * <p>
 * <p> Revision 1.77  2008/12/10 18:31:49  sueh
 * <p> bug# 1162 Added a date/time stamp to main.  Added a manager stamp
 * <p> to makeCurrent().
 * <p>
 * <p> Revision 1.76  2008/07/19 00:22:28  sueh
 * <p> bug# 1125 In setUserPreferences set CpuAdoc from userConfig.
 * <p>
 * <p> Revision 1.75  2008/05/28 17:24:11  sueh
 * <p> bug# 1110 In doAutomation check for null manager.
 * <p>
 * <p> Revision 1.74  2008/05/13 20:57:33  sueh
 * <p> bug# 847 In closeCurrentManager, call BaseManager.close when closing
 * <p> a manager but not exiting the program.  This let the manager check for
 * <p> next processes and close 3dmods.
 * <p>
 * <p> Revision 1.73  2008/05/03 00:32:47  sueh
 * <p> bug# 847 Found an apparent bug where change managers is causing
 * <p> exitProgram to be called.  Added a boolean exiting parameter to
 * <p> closeCurrentManager to only call exitProgram when actually exiting.
 * <p>
 * <p> Revision 1.72  2008/04/17 20:40:36  sueh
 * <p> bug# 1106 Fixed null pointer exception in doAutomation.
 * <p>
 * <p> Revision 1.71  2008/03/21 23:59:49  sueh
 * <p> bug# 1099 Fixed isMemoryAvailable.  Removed the SGI tests since it is
 * <p> not supported in this version.  Testing against availableMemory, which
 * <p> seems to be accurate in Mac, Windows, and Linux.  Also increased
 * <p> MIN_AVAILABLE_MEMORY_REQUIRED.
 * <p>
 * <p> Revision 1.70  2008/03/19 01:00:29  sueh
 * <p> bug# 1099 Added getAvailableMemory
 * <p>
 * <p> Revision 1.69  2008/01/31 20:16:16  sueh
 * <p> bug# 1055 Stop using lazy instanciation to create parameterStore.
 * <p>
 * <p> Revision 1.68  2007/12/26 21:56:56  sueh
 * <p> bug# 1052 Added doAutomation() to EtomoDirector, BaseManager and the
 * <p> manager classes.  Function should run any user-specified automatic functionality
 * <p> before giving control to the user.  Moved argument handling functionality to
 * <p> Arguments class.
 * <p>
 * <p> Revision 1.67  2007/12/10 21:49:06  sueh
 * <p> bug# 1041 Made the Const meta data objects into interfaces and moved their
 * <p> functionality to the main meta data objects.  This makes is easier to inherit
 * <p> functionality from BaseMetaData.
 * <p>
 * <p> Revision 1.66  2007/09/07 00:14:13  sueh
 * <p> bug# 989 Making the processing of the argument list independent of the
 * <p> instanciation of EtomoDirector:  creating Arguments, adding an instance to
 * <p> EtomoDirector and instanciating it in main().  Getting rid of
 * <p> EtomoDirector.getInstance and createInstance.  Calling EtomoDirector.initialize
 * <p> in main().  Main must be called as early as possible.  Return default argument
 * <p> values when arguments is null, except for test and selftest - JfcUnit cannot call
 * <p> main early enough so assume that its argument list is in use when arguments is
 * <p> null.
 * <p>
 * <p> Revision 1.65  2007/08/06 19:53:30  sueh
 * <p> bug# 916 Warning when making a settings change can effect appearance.
 * <p>
 * <p> Revision 1.64  2007/07/30 22:37:04  sueh
 * <p> bug# 963 Updated usage message.
 * <p>
 * <p> Revision 1.63  2007/07/30 18:31:23  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.62  2007/05/29 19:16:00  sueh
 * <p> bug# 994 In exitProgram, stop the ProcessRestarter and all the
 * <p> IntermittentBackgroundProcesses.
 * <p>
 * <p> Revision 1.61  2007/05/02 21:05:19  sueh
 * <p> bug# 964 Removed Import PRM and Duplicate PEET menu items.
 * <p>
 * <p> Revision 1.60  2007/05/02 16:34:05  sueh
 * <p> bug# 964 Moved newstuff into mainstream.
 * <p>
 * <p> Revision 1.59  2007/03/31 02:49:34  sueh
 * <p> bug# 964 Enabling/disabling Duplicate Peet menu item.
 * <p>
 * <p> Revision 1.58  2007/02/22 20:32:54  sueh
 * <p> bug# 964 In initialize, putting .epe file recognition under newstuff.
 * <p>
 * <p> Revision 1.57  2007/02/19 21:49:18  sueh
 * <p> bug# 964 Added PEET manager.
 * <p>
 * <p> Revision 1.56  2007/02/05 21:26:38  sueh
 * <p> bug# 962  Put EtomoNumber type info into an inner class.
 * <p>
 * <p> Revision 1.55  2006/11/28 22:48:21  sueh
 * <p> bug# 934 In exitProgram() calling
 * <p> IntermittentBackgroundProcess.endRestartThread().
 * <p>
 * <p> Revision 1.54  2006/11/16 23:15:36  sueh
 * <p> bug# 882 isMemoryAvailable:  checking available memory plus used memory against JAVA_MEM_LIM before checking for enough available memory.
 * <p>
 * <p> Revision 1.53  2006/11/15 18:46:37  sueh
 * <p> bug# 872 Using ParameterStore to save to the .etomo file.
 * <p>
 * <p> Revision 1.52  2006/07/26 21:50:54  sueh
 * <p> bug# 907 setting headless to GraphicsEnvironment.isHeadless().
 * <p>
 * <p> Revision 1.51  2006/07/21 22:11:10  sueh
 * <p> bug# 901 Getting the calibration directory environment variable name from
 * <p> EnvironmentVariable.
 * <p>
 * <p> Revision 1.50  2006/07/20 17:18:58  sueh
 * <p> bug# 848 SetUserPreferences():  Set the font size in UIParameters.
 * <p>
 * <p> Revision 1.49  2006/07/19 15:12:22  sueh
 * <p> Removed unnecessary imports.
 * <p>
 * <p> Revision 1.48  2006/07/18 19:07:01  sueh
 * <p> bug# 905 Removing unnecessary properties printing
 * <p>
 * <p> Revision 1.47  2006/07/18 17:44:26  sueh
 * <p> bug# 905 Setting the user configuration before opening the first manager.
 * <p>
 * <p> Revision 1.46  2006/06/30 17:01:48  sueh
 * <p> Added warning about IMOD_CALIB_DIR.
 * <p>
 * <p> Revision 1.45  2006/06/30 16:29:37  sueh
 * <p> bug# 883 Added EnvironmentVariable, a class to get and store environment
 * <p> variables.
 * <p>
 * <p> Revision 1.44  2006/06/07 23:33:53  sueh
 * <p> bug# 766 Getting home directory earlier because parallel processing needs it
 * <p> immediately to load the parallel processing table.  Adding parellel processing
 * <p> managers to the manager list.
 * <p>
 * <p> Revision 1.43  2006/04/25 18:52:45  sueh
 * <p> bug# 787 Added testDone to EtomoDirector so that Etomo can exit
 * <p> without popups when the UITest package fails or asks Etomo to exit.
 * <p>
 * <p> Revision 1.42  2006/04/10 19:02:33  sueh
 * <p> buG# 835 enabling "New Parallel Process" when processchunks is run.
 * <p>
 * <p> Revision 1.41  2006/03/20 17:48:39  sueh
 * <p> bug# 835 Added ParallelManager.
 * <p>
 * <p> Revision 1.40  2006/02/09 22:43:28  sueh
 * <p> Added the imod directory to the stderr prints
 * <p>
 * <p> Revision 1.39  2006/01/04 20:16:39  sueh
 * <p> bug# 675 Removed kill Etomo functionality.  Running each test separately
 * <p> from a script so that each instance of Etomo will have a separate virtual
 * <p> machine.
 * <p>
 * <p> Revision 1.38  2006/01/03 23:18:32  sueh
 * <p> bug# 675 Made Utilities more independent from EtomoDirector.  Added
 * <p> removeInstance_test to destroy the EtomoDirector istance.
 * <p>
 * <p> Revision 1.37  2005/12/23 02:02:14  sueh
 * <p> bug# 675 Stop initializing etomo from getInstance().  Throw an exception in
 * <p> getInstance() if etomo hasn't been initialized.  This allows better control of
 * <p> when and with which options etomo runs.  This is necessary for creating
 * <p> classes to be usable without running etomo:  they will throw an exception
 * <p> if they try to use EtomoDirector when etomo isn't running.  Split the test
 * <p> option functionality into test and headless.  Headless controls whether a
 * <p> window is created and test gives access to test-only functions.  Also added
 * <p> the printNames option, which sends the name given to screen elements
 * <p> to the standard out.
 * <p> bug# 786 Added the help option and a help message.
 * <p>
 * <p> Revision 1.36  2005/12/09 20:22:08  sueh
 * <p> fixed file comment
 * <p>
 * Revision 1.35  2005/11/29 22:18:36  sueh
 * bug# 757 Set IMODDirectory as early as possible to avoid failures.
 *
 * Revision 1.34  2005/11/14 21:18:15  sueh
 * bug# 762 Added functions MemoryThread.setStop,
 * getDisplayMemoryInterval, and isDisplayMemory.
 *
 * Revision 1.33  2005/11/10 17:53:55  sueh
 * bug# 733 Added getCurrentTestManager for getting a manager for testing.
 * Removed --debug option from test because it clutters up the error log.
 *
 * Revision 1.32  2005/09/14 20:15:30  sueh
 * bug# 532 Added thread to print the memory usage at intervals.
 *
 * Revision 1.31  2005/09/13 00:27:50  sueh
 * bug# 532 Added timestamp option.
 *
 * Revision 1.30  2005/09/13 00:14:15  sueh
 * bug# 532 Added --memory option to display memory usage.
 *
 * Revision 1.29  2005/09/12 23:57:32  sueh
 * bug# 532 Added savePreferences() to save a Storable class to the .etomo
 * file without overwriting preference entries from other Storable classes.
 * Added loadPreferences() to load a single storable class from the .etomo
 * file.
 *
 * Revision 1.28  2005/08/22 22:05:45  sueh
 * bug# 714 Added makeCurrent() to set the user.dir property from
 * originalUserDir
 *
 * Revision 1.27  2005/08/22 16:01:56  sueh
 * bug# 532 Moved HashedArray to UniqueHashedArray and created a new
 * version of HashedArray which is simpler and doesn't use UniqueKey.
 *
 * Revision 1.26  2005/08/04 19:07:30  sueh
 * bug# 532  Sending the manager to UIHarness.pack() so that
 * packDialogs() can be called.
 *
 * Revision 1.25  2005/07/29 00:39:11  sueh
 * bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * because the current manager changes when the user changes the tab.
 * Passing the manager where its needed.
 *
 * Revision 1.24  2005/07/14 21:55:57  sueh
 * Fixed bug in parseCommandLine().  Newstuff number parse was wrong.
 *
 * Revision 1.23  2005/06/21 20:32:26  sueh
 * bug# 671 Setting currentManagerKey after all .edf files on the command
 * line are processed.  Since the first one is set to current and the axis type
 * retrieved from EtomoDirector comes from the current manager, an
 * incorrect axis type would be retrieved if the current manager was set
 * before the processing of incoming .edf files was finished.
 *
 * Revision 1.22  2005/06/21 00:40:44  sueh
 * bug# 522 In order to get a current manager when --test is set, moved call
 * to EtomoDirector.setCurrentManager() out of WindowSwitch.setWindow().
 * Now the call follows all the calls to UIHarness.selectWindowMenuItem().
 *
 * Revision 1.21  2005/06/17 17:48:22  sueh
 * bug# 685 Setting relative timestamp start.
 *
 * Revision 1.20  2005/06/01 21:24:28  sueh
 * bug# 667 Removing the Controller classes.  Trying make meta data and
 * app manager equals didn't work very well.  Meta data is created by and
 * managed by app mgr and the class structure should reflect that.
 * Changing controllerList to managerList.  Changing currentControllerKey to
 * current ManagerKey.
 *
 * Revision 1.19  2005/05/31 23:11:28  sueh
 * bug# 667 First step to removing the Controller classes.  Change
 * getCurrentMetaData() to getCurrentName().  getCurrentMetaData() was
 * only used to get the dataset name for utility functions that don't know
 * whether the name comes from a join and a reconstruction.
 *
 * Revision 1.18  2005/05/20 21:51:31  sueh
 * bug# 644 isMemoryAvailable():  improved out of memory message.
 *
 * Revision 1.17  2005/05/20 21:13:38  sueh
 * bug# 664 exitProgram(): do not attempt to save to a file if the
 * memory is very low.  If the save fails, the file may be truncated.  Added
 * isMemoryAvailable() to check available memory: if available memory is
 * too low, display a message (if a message hasn't been displayed for this
 * condition before) and return false.
 *
 * Revision 1.16  2005/04/27 02:11:24  sueh
 * bug# 615 CreateMenus() is being called from EtomoFrame.  Delete
 * createMenus() call to avoid creating an extra menu for MainFrame.
 *
 * Revision 1.15  2005/04/26 18:34:10  sueh
 * bug# 615 Change the name of the UIHarness member variable to
 * uiHarness.  Fixed a null pointer exception the happened when testing with
 * JUnit in initialize().
 *
 * Revision 1.14  2005/04/26 17:35:48  sueh
 * bug# 615 Made MainFrame a package-level class.  All MainFrame
 * functionality is handled through UIHarness to make Etomo more
 * compatible with JUnit.  Removing the mainFrame member variable.
 *
 * Revision 1.13  2005/04/25 20:33:20  sueh
 * bug# 615 Passing the axis where the command originated to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.  Move the interface for
 * popping up message dialogs to UIHarness.  It prevents headless
 * exceptions during a test execution.  It also allows logging of dialog
 * messages during a test.  It also centralizes the dialog interface and
 * allows the dialog functions to be synchronized to prevent dialogs popping
 * up in both windows at once.  All Frame functions will use UIHarness as a
 * public interface.
 *
 * Revision 1.12  2005/04/21 20:29:22  sueh
 * bug# 615 Removing deprecated function show() and replaced is with
 * setVisible(true).
 *
 * Revision 1.11  2005/04/20 01:36:31  sueh
 * bug# 615 Making the singleton instance pointer final to prevent in from
 * being changed by another object.
 *
 * Revision 1.10  2005/04/16 01:50:17  sueh
 * bug# 615 Adding newstuffNum to look at more then one group of new code.
 *
 * Revision 1.9  2005/03/02 20:24:17  sueh
 * bug# 533 Added --newstuff command line option.  This option can used to
 * to expose transitional work.  This way alpha testing can be done without
 * exposing transitional work.
 *
 * Revision 1.8  2005/03/01 20:58:56  sueh
 * bug# 607 Catching Throwable in exitProgram and returning true to make
 * sure that Etomo can always exit.
 *
 * Revision 1.7  2005/02/09 22:17:48  sueh
 * bug# 594 Calling MainFrame.setCurrentManager with newWindow = true
 * when the window is first opened.  This prevents Setup Tomogram from
 * coming up blank.  It also prevents the use of
 * MainPanel.fitWindow() functionality, which doesn't work when opening
 * Etomo.
 *
 * Revision 1.6  2005/02/08 18:17:01  sueh
 * bug# 596 Added closeDefaultWindow() to close the window that comes
 * by default.  Closing default window when it is not in use and another
 * window is opened.
 *
 * Revision 1.5  2005/02/07 22:07:38  sueh
 * bug# 594 Using WindowSwitch through MainFrame to manage switching
 * between .edf and .ejf files.  Removed
 * MainFrame.setWindowMenuLabels(HashedArray).  Insteading adding,
 * renaming, and removing individually.
 *
 * Revision 1.4  2005/01/21 22:12:38  sueh
 * bug# 509 bug# 591  Managing a list of Controllers instead of a list of
 * managers.  This give better access to controller classes such as
 * MetaData.  Changed managerList to ControllerList and currentManagerKey
 * to currentControllerKey.
 *
 * Revision 1.3  2004/11/23 00:24:15  sueh
 * bug# 520 Prevent mainFrame from being accessed if test is set.  Create
 * instance of EtomoDirector when create hasn't been run.   GetInstance
 * calls createInstance with the test flag set so that unit tests don't have to
 * call createInstance.  Added setCurrentPropertyUserDir.
 *
 * Revision 1.2  2004/11/19 22:34:24  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 1.1.2.11  2004/10/15 00:01:37  sueh
 * bug# 520 Added a public openManager() function to work with the Open..
 * menu item.  It will open either an AppManager or a JoinManager
 * depending on the type of file name it receives.
 *
 * Revision 1.1.2.10  2004/10/11 01:58:57  sueh
 * bug# 520 Moved initProgram to before creating mainFrame menus to that
 * userConfiguration could be initialized.  Using a variable called
 * propertyUserDir instead of the "user.dir" property.  This property would
 * need a different value for each manager.
 *
 * Revision 1.1.2.9  2004/10/08 15:42:50  sueh
 * bug# 520 Moved SettingsDialog to EtomoDirector.  Since EtomoDirector
 * is a singleton, made all functions and member variables non-static.  The
 * singleton code controls how many EtomoDirector instances can exist.
 * Moved application-level code in initProgram and exitProgram to
 * EtomoDirector.
 *
 * Revision 1.1.2.8  2004/10/07 16:32:23  sueh
 * bug# 520 Simplified EtomoDirector() by doing the initialize in a separate
 * function.  Some of the initializations performed use the EtomoDirector this
 * pointer.  It is more reliable to complete construction before using the
 * instance in another class.  Fixed bug:  was testing the test member
 * variable before it had been set.
 *
 * Revision 1.1.2.7  2004/10/06 01:24:03  sueh
 * bug# 520 Prevented having more then one Setup screen or more then one
 * New Join screen.
 *
 * Revision 1.1.2.6  2004/10/01 21:00:21  sueh
 * bug# 520 moving newJoinName and newTomogramName to the meta
 * data classes.  Adding openManager() to do some open manager functionality
 * generically.
 *
 * Revision 1.1.2.5  2004/09/13 20:23:22  sueh
 * bug# 520 fix exitProgram() so it calls exitProgram for all the managers in
 * managerList.
 *
 * Revision 1.1.2.4  2004/09/13 16:40:41  sueh
 * bug# 520 Finding manager by key because there can be duplicate names.
 * Using a etomo.util.HashedArray to store managers because they may
 * have duplicate names and they need to because accessed by index and
 * \key.  Making a set of openTomogram and OpenJoin functions to creating
 * new ApplicationManagers and JoinManagers.  Adding calls to functions
 * that create the Window menu items and check the current menu item to
 * EtomoDirector.  Add public functions to access the manager list.  Add
 * public functions to set the current manager, close the current manager,
 * rename the current manager, and exit the program.
 *
 * Revision 1.1.2.3  2004/09/09 17:32:42  sueh
 * bug# 520 Allow retrieval of manager by .edf file name or by order by
 * adding an ArrayList of .edf file names.  Call MainFrame.createMenus after
 * manager list is created (remove call from MainFrame()).  Add access
 * and create functions for the manager list.
 *
 * Revision 1.1.2.2  2004/09/07 17:52:52  sueh
 * bug# 520 moved MainFrame and UserConfiguration to EtomoDirector
 *
 * Revision 1.1.2.1  2004/09/03 20:59:07  sueh
 * bug# 520 transfering constructor code from ApplicationManager.  Allowing
 * multiple ApplicationManagers and JoinManagers
 * <p> </p>
 */
