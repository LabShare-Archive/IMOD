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
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstMetaData;
import etomo.type.UserConfiguration;
import etomo.ui.MainFrame;
import etomo.ui.SettingsDialog;
import etomo.util.HashedArray;
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
  
  private static EtomoDirector theEtomoDirector = null;
  private File IMODDirectory;
  private File IMODCalibDirectory;
  private MainFrame mainFrame = null;
  private UserConfiguration userConfig = null;
  private boolean debug = false;
  private boolean demo = false;
  private boolean test = false;
  private boolean selfTest = false;
  private HashedArray managerList = null;
  private UniqueKey currentManagerKey = null;
  private String homeDirectory;
  // advanced dialog state for this instance, this gets set upon startup from
  // the user configuration and can be modified for this instance by either
  // the option or advanced menu items
  private boolean isAdvanced = false;
  private SettingsDialog settingsDialog = null;

  public static void main(String[] args) {
    createInstance(args);
  }

  public synchronized static EtomoDirector createInstance(String[] args) {
    if (theEtomoDirector == null) {
      theEtomoDirector = new EtomoDirector();
      theEtomoDirector.initialize(args);
    }
    return theEtomoDirector;
  }

  public static EtomoDirector getInstance() {
    if (theEtomoDirector == null) {
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
    if (!test) {
      createMainFrame();
    }
    int paramFileNameListSize = paramFileNameList.size();
    String paramFileName = null;
    managerList = new HashedArray();
    ApplicationManager appMgr = null;
    //if no param file is found bring up AppMgr.SetupDialog
    if (paramFileNameListSize == 0) {
      openTomogram(true);
    }
    else {
      boolean makeCurrent;
      for (int i = 0; i < paramFileNameListSize; i++) {
        makeCurrent = false;
        paramFileName = (String) paramFileNameList.get(i);
        UniqueKey managerKey = null;
        if (i == 0) {
          makeCurrent = true;
        }
        if (paramFileName.endsWith(".edf")) {
          managerKey = openTomogram(paramFileName, makeCurrent);
        }
        else if (paramFileName.endsWith(".ejf")) {
          managerKey = openJoin(paramFileName, makeCurrent);
        }
      }
    }
    initProgram();
    if (!test) {
      mainFrame.createMenus();
      mainFrame.setWindowMenuLabels(managerList);
      mainFrame.setCurrentManager((BaseManager) managerList
          .get(currentManagerKey));
      mainFrame.selectWindowMenuItem(currentManagerKey);
      mainFrame.setMRUFileLabels(userConfig.getMRUFileList());
      mainFrame.pack();
      mainFrame.show();
    }
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
      mainFrame.openMessageDialog(message, "Program Initialization Error");
      System.exit(1);
    }
    // Get the IMOD directory so we know where to find documentation
    // Check to see if is defined on the command line first with -D
    // Otherwise check to see if we can get it from the environment
    String imodDirectoryName = System.getProperty("IMOD_DIR");
    if (imodDirectoryName == null) {
      imodDirectoryName = Utilities.getEnvironmentVariable("IMOD_DIR");
      if (imodDirectoryName.equals("")) {
        String[] message = new String[3];
        message[0] = "Can not find IMOD directory!";
        message[1] = "Set IMOD_DIR environment variable and restart program to fix this problem";
        mainFrame.openMessageDialog(message, "Program Initialization Error");
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
      imodCalibDirectoryName = Utilities.getEnvironmentVariable("IMOD_CALIB_DIR");
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
      mainFrame.openMessageDialog(except.getMessage(),
        "IO Exception: Can't load user configuration"
          + userConfigFile.getAbsolutePath());
    }
    //  Set the user preferences
    setUserPreferences();
  }

  public BaseManager getCurrentManager() {
    return (BaseManager) managerList.get(currentManagerKey);
  }
  
  public String getCurrentPropertyUserDir() {
    if (currentManagerKey == null) {
      return System.getProperty("user.dir");
    }
    return ((BaseManager) managerList.get(currentManagerKey)).getPropertyUserDir();
  }
  
  public String setCurrentPropertyUserDir(String propertyUserDir) {
    if (currentManagerKey == null) {
      return System.setProperty("user.dir", propertyUserDir);
    }
    return ((BaseManager) managerList.get(currentManagerKey)).setPropertyUserDir(propertyUserDir);
  }
  
  public UniqueKey getManagerKey(int index) {
    return managerList.getKey(index);
  }
  
  public synchronized void setCurrentManager(UniqueKey managerKey) {
    BaseManager newCurrentManager = (BaseManager) managerList.get(managerKey);
    if (newCurrentManager == null) {
      throw new NullPointerException("managerKey=" + managerKey); 
    }
    currentManagerKey = managerKey;
    if (!test) {
      mainFrame.setWindowMenuLabels(managerList);
      mainFrame.setCurrentManager(newCurrentManager);
      mainFrame.selectWindowMenuItem(currentManagerKey);
    }
  }
  
  private UniqueKey openTomogram(String etomoDataFileName, boolean makeCurrent) {
    ApplicationManager manager;
    if (etomoDataFileName == null
        || etomoDataFileName.equals(ConstMetaData.getNewFileTitle())) {
      manager = new ApplicationManager("");
      if (!test) {
        mainFrame.setEnabledNewTomogramMenuItem(false);
      }
    }
    else {
      manager = new ApplicationManager(etomoDataFileName);
    }
    UniqueKey managerKey;
    return openManager(manager, makeCurrent);
  }
  
  public UniqueKey openJoin(boolean makeCurrent) {
    return openJoin(ConstJoinMetaData.getNewFileTitle(), makeCurrent);
  }
  
  private UniqueKey openJoin(File etomoJoinFile, boolean makeCurrent) {
    if (etomoJoinFile == null) {
      return openJoin(makeCurrent);
    }
    return openJoin(etomoJoinFile.getAbsolutePath(), makeCurrent);
  }
  
  private UniqueKey openJoin(String etomoJoinFileName, boolean makeCurrent) {
    JoinManager manager;
    if (etomoJoinFileName == null
        || etomoJoinFileName.equals(ConstJoinMetaData.getNewFileTitle())) {
      manager = new JoinManager("");
      mainFrame.setEnabledNewJoinMenuItem(false);
    }
    else {
      manager = new JoinManager(etomoJoinFileName);
    }
    return openManager(manager, makeCurrent);
  }
  
  private UniqueKey openManager(BaseManager manager, boolean makeCurrent) {
    UniqueKey managerKey;
    managerKey = managerList.add(manager.getBaseMetaData().getName(), manager);
    if (makeCurrent) {
      setCurrentManager(managerKey);
    }
    return managerKey;
  }
  
  public UniqueKey openTomogram(boolean makeCurrent) {
    return openTomogram(ConstMetaData.getNewFileTitle(), makeCurrent);
  }
  
  public UniqueKey openManager(File dataFile, boolean makeCurrent) {
    if (dataFile == null) {
      throw new IllegalStateException("null dataFile");
    }
    EtomoFileFilter etomoFileFilter = new EtomoFileFilter();
    if (etomoFileFilter.accept(dataFile)) {
      return openTomogram(dataFile, makeCurrent);
    }
    JoinFileFilter joinFileFilter = new JoinFileFilter();
    if (joinFileFilter.accept(dataFile)) {
      return openJoin(dataFile, makeCurrent);
    }
    String[] message = { "Unknown file type " + dataFile.getName() + ".",
        "Open this file as an " + etomoFileFilter.getDescription() + "?" };
    if (mainFrame.openYesNoDialog(message)) {
      return openTomogram(dataFile, makeCurrent);
    }
    else {
      return openJoin(dataFile, makeCurrent);
    }
  }
  
  public UniqueKey openTomogram(File etomoDataFile, boolean makeCurrent) {
    if (etomoDataFile == null) {
      return openTomogram(makeCurrent);
    }
    return openTomogram(etomoDataFile.getAbsolutePath(), makeCurrent);
  }

  public boolean closeCurrentManager() {
    BaseManager currentManager = getCurrentManager();
    if (!currentManager.exitProgram()) {
      return false;
    }
    managerList.remove(currentManagerKey);
    enableOpenManagerMenuItem();
    currentManagerKey = null;
    if (managerList.size() == 0) {
      if (!test) {
        mainFrame.setWindowMenuLabels(managerList);
        mainFrame.setCurrentManager(null);
      }
      return true;
    }
    setCurrentManager(managerList.getKey(0));
    return true;
  }
  
  private void enableOpenManagerMenuItem() {
    if (currentManagerKey.getName().equals(ConstMetaData.getNewFileTitle())) {
      mainFrame.setEnabledNewTomogramMenuItem(true);
    }
    if (currentManagerKey.getName().equals(ConstJoinMetaData.getNewFileTitle())) {
      mainFrame.setEnabledNewJoinMenuItem(true);
    }
  }
  
  public boolean exitProgram() {
    while (managerList.size() != 0) {
      if (!closeCurrentManager()) {
        return false;
      }
    }
    //  Should we close the 3dmod windows
    //  Save the current window size to the user config
    Dimension size = mainFrame.getSize();
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
      mainFrame.openMessageDialog(
          "Change permissions of $HOME/.etomo to allow writing",
          "Unable to save user configuration file");
    }
    if (userConfigFile.canWrite()) {
      try {
        userParams.save(storable);
      }
      catch (IOException excep) {
        excep.printStackTrace();
        mainFrame.openMessageDialog(
            "IOException: unable to save user parameters\n"
                + excep.getMessage(), "Unable to save user parameters");
      }
    }
    return true;
  }
  
  public void renameCurrentManager(String managerName) {
    enableOpenManagerMenuItem();
    currentManagerKey = managerList.rekey(currentManagerKey, managerName);
    if (!test) {
      mainFrame.setWindowMenuLabels(managerList);
      mainFrame.selectWindowMenuItem(currentManagerKey);
    }
  }
  
  private void createMainFrame() {
    mainFrame = new MainFrame();
  }
  
  public MainFrame getMainFrame() {
    if (mainFrame == null) {
      throw new NullPointerException();
    }
    return mainFrame;
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
    for (int i = 0; i < args.length; i++) {
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
      mainFrame.repaintWindow();
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
      Dimension frmSize = mainFrame.getSize();
      Point loc = mainFrame.getLocation();
      settingsDialog.setLocation(loc.x, loc.y + frmSize.height);
      settingsDialog.setModal(false);
    }
    settingsDialog.show();
  }
  
  /**
   *  
   */
  public void closeSettingsDialog() {
    if (settingsDialog != null) {
      settingsDialog.dispose();
    }
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