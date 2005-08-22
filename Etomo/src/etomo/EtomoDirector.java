package etomo;

import java.awt.Dimension;
import java.awt.Point;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.plaf.FontUIResource;

import etomo.storage.EtomoFileFilter;
import etomo.storage.JoinFileFilter;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstMetaData;
import etomo.type.UserConfiguration;
import etomo.ui.SettingsDialog;
import etomo.ui.UIHarness;
import etomo.util.UniqueHashedArray;
import etomo.util.UniqueKey;
import etomo.util.Utilities;

/**
 * <p>
 * Description: Directs ApplicationManager and JoinManager through BaseManager.
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2004
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
 * 
 * <p>
 * $Log$
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
 *
 * </p>
 */

public class EtomoDirector {
  public static final String rcsid = "$Id$";
  
  public static final double MIN_AVAILABLE_MEMORY = 0.75;
  private static final EtomoDirector theEtomoDirector = new EtomoDirector();
  private File IMODDirectory;
  private File IMODCalibDirectory;
  //private MainFrame mainFrame = null;
  private UserConfiguration userConfig = null;
  private boolean debug = false;
  private boolean demo = false;
  private boolean test = false;
  private boolean selfTest = false;
  private boolean newstuff = false;
  private int newstuffNum = 0;
  private UniqueHashedArray managerList = null;
  private UniqueKey currentManagerKey = null;
  private String homeDirectory;
  private boolean defaultWindow = false;
  private static boolean initialized = false;
  private UIHarness uiHarness = UIHarness.INSTANCE;
  // advanced dialog state for this instance, this gets set upon startup from
  // the user configuration and can be modified for this instance by either
  // the option or advanced menu items
  private boolean isAdvanced = false;
  private SettingsDialog settingsDialog = null;
  private boolean outOfMemoryMessage = false;

  public static void main(String[] args) {
    createInstance(args);
  }

  public synchronized static EtomoDirector createInstance(String[] args) {
    if (!initialized) {
      Utilities.setStartTime();
      initialized = true;
      theEtomoDirector.initialize(args);
    }
    return theEtomoDirector;
  }

  /**
   * Get the singleton instance of EtomoDirector
   * This function can be used to create the instance of EtomoDirector without
   * a command line.  This functionality is used by test objects.
   * @return
   */
  public static EtomoDirector getInstance() {
    if (!initialized) {
      String[] args = {"--test","--selftest","--debug"};
      return createInstance(args);
    }
    return theEtomoDirector;
  }

  private EtomoDirector() {
  }
  
  private void initialize(String[] args) {
    createUserConfiguration();
    ArrayList paramFileNameList = parseCommandLine(args);
    uiHarness.createMainFrame();
    int paramFileNameListSize = paramFileNameList.size();
    String paramFileName = null;
    managerList = new UniqueHashedArray();
    //if no param file is found bring up AppMgr.SetupDialog
    if (paramFileNameListSize == 0) {
      defaultWindow = true;
      openTomogram(true, AxisID.ONLY);
    }
    else {
      UniqueKey saveKey = null;
      for (int i = 0; i < paramFileNameListSize; i++) {
        paramFileName = (String) paramFileNameList.get(i);
        UniqueKey managerKey = null;
        if (paramFileName.endsWith(".edf")) {
          managerKey = openTomogram(paramFileName, false, AxisID.ONLY);
        }
        else if (paramFileName.endsWith(".ejf")) {
          managerKey = openJoin(paramFileName, false, AxisID.ONLY);
        }
        if (i == 0) {
          saveKey = managerKey;
        }
      }
      currentManagerKey = saveKey;
    }
    initProgram();
    BaseManager manager = (BaseManager) managerList.get(currentManagerKey);
    if (manager != null) {
      uiHarness.setCurrentManager(manager, currentManagerKey, true);
    }
    uiHarness.selectWindowMenuItem(currentManagerKey);
    setCurrentManager(currentManagerKey, false);
    uiHarness.setMRUFileLabels(userConfig.getMRUFileList());
    uiHarness.pack(manager);
    uiHarness.setVisible(true);
  }
  
  /**
   *  
   */
  private void initProgram() {
    System.err.println("java.version:  " + System.getProperty("java.version"));
    System.err.println("java.vendor:  " + System.getProperty("java.vendor"));
    System.err.println("java.home:  " + System.getProperty("java.home"));
    System.err.println("java.vm.version:  "
      + System.getProperty("java.vm.version"));
    System.err.println("java.vm.vendor:  "
      + System.getProperty("java.vm.vendor"));
    System.err.println("java.vm.home:  " + System.getProperty("java.vm.home"));
    System.err.println("java.class.version:  "
      + System.getProperty("java.class.version"));
    System.err.println("java.class.path:  "
      + System.getProperty("java.class.path"));
    System.err.println("java.library.path:  "
      + System.getProperty("java.library.path"));
    System.err.println("java.io.tmpdir:  "
      + System.getProperty("java.io.tmpdir"));
    System.err.println("java.compiler:  " + System.getProperty("java.compiler"));
    System.err.println("java.ext.dirs:  " + System.getProperty("java.ext.dirs"));
    System.err.println("os.name:  " + System.getProperty("os.name"));
    System.err.println("os.arch:  " + System.getProperty("os.arch"));
    System.err.println("os.version:  " + System.getProperty("os.version"));
    System.err.println("user.name:  " + System.getProperty("user.name"));
    System.err.println("user.home:  " + System.getProperty("user.home"));
    System.err.println("user.dir:  " + System.getProperty("user.dir"));
    // Get the HOME directory environment variable to find the program
    // configuration file
    homeDirectory = System.getProperty("user.home");
    if (homeDirectory.equals("")) {
      String[] message = new String[2];
      message[0] = "Can not find home directory! Unable to load user preferences";
      message[1] = "Set HOME environment variable and restart program to fix this problem";
      uiHarness.openMessageDialog(message, "Program Initialization Error", AxisID.ONLY);
      System.exit(1);
    }
    // Get the IMOD directory so we know where to find documentation
    // Check to see if is defined on the command line first with -D
    // Otherwise check to see if we can get it from the environment
    String imodDirectoryName = System.getProperty("IMOD_DIR");
    if (imodDirectoryName == null) {
      imodDirectoryName = Utilities.getEnvironmentVariable(null, "IMOD_DIR",
          AxisID.ONLY);
      if (imodDirectoryName.equals("")) {
        String[] message = new String[3];
        message[0] = "Can not find IMOD directory!";
        message[1] = "Set IMOD_DIR environment variable and restart program to fix this problem";
        uiHarness.openMessageDialog(message, "Program Initialization Error", AxisID.ONLY);
        System.exit(1);
      }
      else {
        if (debug) {
          System.err.println("IMOD_DIR (env): " + imodDirectoryName);
        }
      }
    }
    else {
      if (debug) {
        System.err.println("IMOD_DIR (-D): " + imodDirectoryName);
      }
    }
    IMODDirectory = new File(imodDirectoryName);
    // Get the IMOD calibration directory so we know where to find documentation
    // Check to see if is defined on the command line first with -D
    // Otherwise check to see if we can get it from the environment
    String imodCalibDirectoryName = System.getProperty("IMOD_CALIB_DIR");
    if (imodCalibDirectoryName == null) {
      imodCalibDirectoryName = Utilities.getEnvironmentVariable(null,
          "IMOD_CALIB_DIR", AxisID.ONLY);
      if (!imodCalibDirectoryName.equals("")) {
        if (debug) {
          System.err.println("IMOD_CALIB_DIR (env): " + imodCalibDirectoryName);
        }
      }
    }
    else {
      if (debug) {
        System.err.println("IMOD_CALIB_DIR (-D): " + imodCalibDirectoryName);
      }
    }
    IMODCalibDirectory = new File(imodCalibDirectoryName);
    //  Create a File object specifying the user configuration file
    File userConfigFile = new File(homeDirectory, ".etomo");
    //  Make sure the config file exists, create it if it doesn't
    try {
      userConfigFile.createNewFile();
    }
    catch (IOException except) {
      System.err.println("Could not create file:"
        + userConfigFile.getAbsolutePath());
      System.err.println(except.getMessage());
    }
    // Load in the user configuration
    ParameterStore userParams = new ParameterStore(userConfigFile);
    Storable storable[] = new Storable[1];
    storable[0] = userConfig;
    try {
      userParams.load(storable);
    }
    catch (IOException except) {
      uiHarness.openMessageDialog(except.getMessage(),
        "IO Exception: Can't load user configuration"
          + userConfigFile.getAbsolutePath(), AxisID.ONLY);
    }
    //  Set the user preferences
    setUserPreferences();
  }

  /**
   * Gets the current manager.  It is important that
   * this function remain package level because the current manager changes when
   * the user switches tabs, which would change the functionality of code
   * executed after a process is finished.
   * @return the current manager
   */
  BaseManager getCurrentManager() {
    if (currentManagerKey == null) {
      throw new IllegalStateException("No current manager");
    }
    return (BaseManager) managerList.get(currentManagerKey);
  }

  /**
   * Sets the property user dir for the current manager.  It is important that
   * this function remain package level because the current manager changes when
   * the user switches tabs, which would change the functionality of code
   * executed after a process is finished.  Used only by test.
   * @param propertyUserDir
   * @return the old property user dir
   */
  String setCurrentPropertyUserDir(String propertyUserDir) {
    if (currentManagerKey == null) {
      return System.setProperty("user.dir", propertyUserDir);
    }
    return ((BaseManager) managerList.get(currentManagerKey)).setPropertyUserDir(propertyUserDir);
  }
  
  public UniqueKey getManagerKey(int index) {
    return managerList.getKey(index);
  }
  
  public void setCurrentManager(UniqueKey managerKey) {
    setCurrentManager(managerKey, false);
  }
  
  public synchronized void setCurrentManager(UniqueKey managerKey, boolean newWindow) {
    if (managerKey == null) {
      return;
    }
    BaseManager newCurrentManager = (BaseManager) managerList.get(managerKey);
    if (newCurrentManager == null) {
      throw new NullPointerException("managerKey=" + managerKey); 
    }
    currentManagerKey = managerKey;
    uiHarness.setCurrentManager(newCurrentManager, currentManagerKey, newWindow);
  }
  
  private UniqueKey openTomogram(String etomoDataFileName, boolean makeCurrent, AxisID axisID) {
    ApplicationManager manager;
    if (etomoDataFileName == null
        || etomoDataFileName.equals(ConstMetaData.getNewFileTitle())) {
      manager = new ApplicationManager("", axisID);
      uiHarness.setEnabledNewTomogramMenuItem(false);
    }
    else {
      manager = new ApplicationManager(etomoDataFileName, axisID);
    }
    return setManager(manager, makeCurrent);
  }
  
  public UniqueKey openJoin(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    return openJoin(ConstJoinMetaData.getNewFileTitle(), makeCurrent, axisID);
  }
  
  private UniqueKey openJoin(File etomoJoinFile, boolean makeCurrent, AxisID axisID) {
    if (etomoJoinFile == null) {
      return openJoin(makeCurrent, axisID);
    }
    return openJoin(etomoJoinFile.getAbsolutePath(), makeCurrent, axisID);
  }
  
  private UniqueKey openJoin(String etomoJoinFileName, boolean makeCurrent, AxisID axisID) {
    JoinManager manager;
    if (etomoJoinFileName == null
        || etomoJoinFileName.equals(ConstJoinMetaData.getNewFileTitle())) {
      manager = new JoinManager("", axisID);
      uiHarness.setEnabledNewJoinMenuItem(false);
    }
    else {
      manager = new JoinManager(etomoJoinFileName, axisID);
    }
    return setManager(manager, makeCurrent);
  }
  
  private UniqueKey setManager(BaseManager manager, boolean makeCurrent) {
    UniqueKey managerKey;
    managerKey = managerList.add(manager.getBaseMetaData().getName(), manager);
    uiHarness.addWindow(manager, managerKey);
    if (makeCurrent) {
      uiHarness.selectWindowMenuItem(managerKey, true);
      setCurrentManager(managerKey, true);
    }
    return managerKey;
  }
  
  public UniqueKey openTomogram(boolean makeCurrent, AxisID axisID) {
    closeDefaultWindow(axisID);
    return openTomogram(ConstMetaData.getNewFileTitle(), makeCurrent, axisID);
  }
  
  /**
   * When etomo is run with no data file, it automatically opens a Setup
   * Tomogram window.  This window should be closed if the user opens another
   * window without adding data to the Setup Tomogram fields.  This is only true
   * if the Setup Tomogram was opened as the default window.
   */
  private void closeDefaultWindow(AxisID axisID) {
    if (defaultWindow && managerList.size() == 1) {
      BaseManager manager = ((BaseManager) managerList.get(currentManagerKey));
      if (manager instanceof ApplicationManager) {
        ApplicationManager appManager = (ApplicationManager) manager;
        if (appManager.isNewManager() && !appManager.isSetupChanged()) {
          defaultWindow = false;
          closeCurrentManager(axisID);
        }
      }
    }

  }
  
  public UniqueKey openManager(File dataFile, boolean makeCurrent, AxisID axisID) {
    if (dataFile == null) {
      throw new IllegalStateException("null dataFile");
    }
    closeDefaultWindow(axisID);
    EtomoFileFilter etomoFileFilter = new EtomoFileFilter();
    if (etomoFileFilter.accept(dataFile)) {
      return openTomogram(dataFile, makeCurrent, axisID);
    }
    JoinFileFilter joinFileFilter = new JoinFileFilter();
    if (joinFileFilter.accept(dataFile)) {
      return openJoin(dataFile, makeCurrent, axisID);
    }
    String[] message = { "Unknown file type " + dataFile.getName() + ".",
        "Open this file as an " + etomoFileFilter.getDescription() + "?" };
    if (uiHarness.openYesNoDialog(message, axisID)) {
      return openTomogram(dataFile, makeCurrent, axisID);
    }
    else {
      return openJoin(dataFile, makeCurrent, axisID);
    }
  }
  
  public UniqueKey openTomogram(File etomoDataFile, boolean makeCurrent, AxisID axisID) {
    if (etomoDataFile == null) {
      return openTomogram(makeCurrent, axisID);
    }
    return openTomogram(etomoDataFile.getAbsolutePath(), makeCurrent, axisID);
  }

  public boolean closeCurrentManager(AxisID axisID) {
    BaseManager currentManager = getCurrentManager();
    if (!currentManager.exitProgram(axisID)) {
      return false;
    }
    managerList.remove(currentManagerKey);
    enableOpenManagerMenuItem();
    uiHarness.removeWindow(currentManagerKey);
    currentManagerKey = null;
    if (managerList.size() == 0) {
      uiHarness.removeWindow(currentManagerKey);
      //mainFrame.setWindowMenuLabels(controllerList);
      uiHarness.setCurrentManager(null, null);
      uiHarness.selectWindowMenuItem(null);
      setCurrentManager(null, false);
      return true;
    }
    setCurrentManager(managerList.getKey(0));
    uiHarness.selectWindowMenuItem(managerList.getKey(0));
    return true;
  }
  
  private void enableOpenManagerMenuItem() {
    if (currentManagerKey.getName().equals(ConstMetaData.getNewFileTitle())) {
      uiHarness.setEnabledNewTomogramMenuItem(true);
    }
    if (currentManagerKey.getName().equals(ConstJoinMetaData.getNewFileTitle())) {
      uiHarness.setEnabledNewJoinMenuItem(true);
    }
  }
  
  /**
   * To guarantee that etomo can always exit, catch all unrecognized Exceptions
   * and Errors and return true.
   * @return
   */
  public boolean exitProgram(AxisID axisID) {
    try {
      while (managerList.size() != 0) {
        if (!closeCurrentManager(axisID)) {
          return false;
        }
      }
      if (isMemoryAvailable()) {
        //  Should we close the 3dmod windows
        //  Save the current window size to the user config
        Dimension size = uiHarness.getSize();
        userConfig.setMainWindowWidth(size.width);
        userConfig.setMainWindowHeight(size.height);
        //  Write out the user configuration data
        File userConfigFile = new File(homeDirectory, ".etomo");
        //  Make sure the config file exists, create it if it doesn't
        try {
          userConfigFile.createNewFile();
        }
        catch (IOException except) {
          System.err.println("IOException: Could not create file:"
              + userConfigFile.getAbsolutePath() + "\n" + except.getMessage());
          System.err.println(except.getMessage());
          return true;
        }
        ParameterStore userParams = new ParameterStore(userConfigFile);
        Storable storable[] = new Storable[1];
        storable[0] = userConfig;
        if (!userConfigFile.canWrite()) {
          uiHarness.openMessageDialog(
              "Change permissions of $HOME/.etomo to allow writing",
              "Unable to save user configuration file", axisID);
        }
        if (userConfigFile.canWrite()) {
          try {
            userParams.save(storable);
          }
          catch (IOException excep) {
            excep.printStackTrace();
            uiHarness.openMessageDialog(
                "IOException: unable to save user parameters\n"
                    + excep.getMessage(), "Unable to save user parameters",
                axisID);
          }
        }
        return true;
      }
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
    return true;
  }
  
  public void renameCurrentManager(String managerName) {
    enableOpenManagerMenuItem();
    UniqueKey oldKey = currentManagerKey;
    currentManagerKey = managerList.rekey(currentManagerKey, managerName);
    uiHarness.renameWindow(oldKey, currentManagerKey);
  }
  
  public UserConfiguration getUserConfiguration() {
    if (userConfig == null) {
      throw new NullPointerException();
    }
    return userConfig;
  }
  
  private void  createUserConfiguration() {
    userConfig = new UserConfiguration();
  }
  
  /**
   * Parse the command line. This method will return a non-empty string if there
   * is a etomo data .
   * 
   * @param The
   *          command line arguments
   * @return A string that will be set to the etomo data filename if one is
   *         found on the command line otherwise it is "".
   */
  private ArrayList parseCommandLine(String[] args) {
    ArrayList paramFileNameList = new ArrayList();
    //  Parse the command line arguments
    int i = 0;
    while (i < args.length ) {
      // Filename argument should be the only one not beginning with at least
      // one dash
      if (!args[i].startsWith("-")) {
        paramFileNameList.add(args[i]);
      }
      if (args[i].equals("--debug")) {
        debug = true;
      }
      if (args[i].equals("--demo")) {
        demo = true;
      }
      if (args[i].equals("--test")) {
        test = true;
      }
      if (args[i].equals("--selftest")) {
        selfTest = true;
      }
      if (args[i].equals("--newstuff")) {
        newstuff = true;
        //--newstuff can be used alone, or followed by a 1 or 0 (default).
        if (i+1 < args.length && !args[i+1].startsWith("--")) {
          try {
            newstuffNum = Integer.parseInt(args[++i]);
          }
          catch (NumberFormatException e) {
            newstuffNum = 0;
            i--;
          }
        }
      }
      i++;
    }
    return paramFileNameList;
  }
  
  /**
   * Set the user preferences
   */
  private void setUserPreferences() {
    //FIXME this function may not have to be visible
    ToolTipManager.sharedInstance().setInitialDelay(
      userConfig.getToolTipsInitialDelay());
    ToolTipManager.sharedInstance().setDismissDelay(
      userConfig.getToolTipsDismissDelay());
    setUIFont(userConfig.getFontFamily(), userConfig.getFontSize());
    setLookAndFeel(userConfig.getNativeLookAndFeel());
    isAdvanced = userConfig.getAdvancedDialogs();
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

    //UIManager.LookAndFeelInfo plaf[] = UIManager.getInstalledLookAndFeels();
    //for(int i = 0; i < plaf.length; i++) {
    //  System.err.println(plaf[i].getClassName());
    //}
    String osName = System.getProperty("os.name");
    if (debug) {
      System.err.println("os.name: " + osName);
    }
    if (nativeLookAndFeel) {
      if (osName.startsWith("Mac OS X")) {
        lookAndFeelClassName = "apple.laf.AquaLookAndFeel";
        if (debug) {
          System.err.println("Setting AquaLookAndFeel");
        }
      }
      else if (osName.startsWith("Windows")) {
        lookAndFeelClassName = "com.sun.java.swing.plaf.windows.WindowsLookAndFeel";
        if (debug) {
          System.err.println("Setting WindowsLookAndFeel");
        }
      }
      else {
        lookAndFeelClassName = "com.sun.java.swing.plaf.motif.MotifLookAndFeel";
        if (debug) {
          System.err.println("Setting MotifLookAndFeel");
        }
      }
    }
    else {
      lookAndFeelClassName = UIManager.getCrossPlatformLookAndFeelClassName();
      if (debug) {
        System.err.println("Setting MetalLookAndFeel");
      }
    }
    try {
      UIManager.setLookAndFeel(lookAndFeelClassName);
    }
    catch (Exception excep) {
      System.err.println("Could not set " + lookAndFeelClassName
        + " look and feel");
    }
  }
  
  /**
   *  
   */
  private void setUIFont(String fontFamily, int fontSize) {
    // sets the default font for all Swing components.
    // ex.
    //  setUIFont (new javax.swing.plaf.FontUIResource("Serif",Font.ITALIC,12));
    // Taken from: http://www.rgagnon.com/javadetails/java-0335.html
    java.util.Enumeration keys = UIManager.getDefaults().keys();
    while (keys.hasMoreElements()) {
      Object key = keys.nextElement();
      Object value = UIManager.get(key);
      if (value instanceof FontUIResource) {
        FontUIResource currentFont = (FontUIResource) value;
        FontUIResource newFont = new FontUIResource(fontFamily,
          currentFont.getStyle(), fontSize);
        UIManager.put(key, newFont);
      }
    }
  }

  public boolean isDebug() {
    return debug;
  }

  public boolean isSelfTest() {
    return selfTest;
  }

  public boolean isDemo() {
    return demo;
  }

  public boolean isTest() {
    return test;
  }
  
  public boolean isNewstuff() {
    return newstuff;
  }
  
  public int getManagerListSize() {
    return managerList.size();
  }
  
  /**
   * Return the IMOD directory
   */
  public File getIMODDirectory() {
    //  Return a copy of the IMODDirectory object
    return new File(IMODDirectory.getAbsolutePath());
  }
  
  /**
   * Return the IMOD calibration directory
   */
  public File getIMODCalibDirectory() {
    //  Return a copy of the IMODDirectory object
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
      settingsDialog.getParameters(userConfig);
      setUserPreferences();
      uiHarness.repaintWindow();
    }
  }
  

  /**
   * Open up the settings dialog box
   */
  public void openSettingsDialog() {
    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    if (settingsDialog == null) {
      settingsDialog = new SettingsDialog();
      settingsDialog.setParameters(userConfig);
      Dimension frmSize = uiHarness.getSize();
      Point loc = uiHarness.getLocation();
      settingsDialog.setLocation(loc.x, loc.y + frmSize.height);
      settingsDialog.setModal(false);
    }
    settingsDialog.setVisible(true);
    //settingsDialog.show();
  }
  
  /**
   *  
   */
  public void closeSettingsDialog() {
    if (settingsDialog != null) {
      settingsDialog.dispose();
    }
  }
  
  public boolean isMemoryAvailable() {
    long availableMemory = Runtime.getRuntime().maxMemory()
        - Runtime.getRuntime().totalMemory()
        + Runtime.getRuntime().freeMemory();
    long usedMemory = Runtime.getRuntime().totalMemory()
        - Runtime.getRuntime().freeMemory();
    //System.out.println();
    //System.out.println("Available memory = " + availableMemory);
    //System.out.println("Memory in use    = " + usedMemory);
    //System.out.println();
    if (debug) {
      System.err.println("Available memory = " + availableMemory);
      System.err.println("Memory in use    = " + usedMemory);
    }
    //Check available memory
    if (availableMemory < EtomoDirector.MIN_AVAILABLE_MEMORY * 1024.0 * 1024.0) {
      //send message once per memory problem
      if (!outOfMemoryMessage) {
        UIHarness.INSTANCE
            .openMessageDialog(
                "WARNING:  Ran out of memory.  Changes to the .edf file and/or"
                    + "comscript files may not be saved."
                    + "\nPlease close open windows or exit Etomo.",
                "Out of Memory");
      }
      outOfMemoryMessage = true;
      return false;
    }
    //memory problem is gone - reset message
    outOfMemoryMessage = false;
    return true;
  }
  
  /**
   * Return the users home directory environment variable HOME or an empty
   * string if it doesn't exist.
   */
  private String getHomeDirectory() {
    return homeDirectory;
  }

  private void setAdvanced(boolean state) {
    isAdvanced = state;
  }

}