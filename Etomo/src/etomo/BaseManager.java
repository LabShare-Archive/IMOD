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
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.process.SystemProcessException;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseState;
import etomo.type.ProcessName;
import etomo.type.UserConfiguration;
import etomo.ui.MainPanel;
import etomo.ui.UIHarness;
import etomo.ui.UIParameters;
import etomo.util.Utilities;

/**
* <p>Description: Base class for ApplicationManager and JoinManager</p>
* 
* <p>Copyright: Copyright (c) 2002, 2003, 2004</p>
*
*<p>Organization:
* Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEM),
* University of Colorado</p>
* 
* @author $Author$
* 
* @version $Revision$
* 
* <p> $Log$
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
public abstract class BaseManager {
  public static  final String  rcsid =  "$Id$";
  
  //protected static variables
  protected static boolean test = false;
  //protected MainFrame mainFrame = null;
  protected UIHarness uiHarness = UIHarness.INSTANCE;
  protected static UserConfiguration userConfig = EtomoDirector.getInstance()
      .getUserConfiguration();
  
  //protected variables
  protected boolean loadedTestParamFile = false;
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
  // FIXME: this going to need to expand to handle both axis
  protected String nextProcessA = "";
  protected String nextProcessB = "";

  protected String threadNameA = "none";

  protected String threadNameB = "none";
  
  protected boolean backgroundProcessA = false;
  protected String backgroundProcessNameA = null;
  protected String propertyUserDir = null;//working directory for this manager

  
  //private static variables
  private static boolean debug = false;

  protected abstract void createComScriptManager();
  protected abstract void createProcessManager();
  protected abstract void createMainPanel();
  protected abstract void createProcessTrack();
  protected abstract void createState();
  protected abstract void updateDialog(ProcessName processName, AxisID axisID);
  protected abstract void startNextProcess(AxisID axisID);
  protected abstract void setMetaData(ImodManager imodManager);
  public abstract BaseMetaData getBaseMetaData();
  public abstract MainPanel getMainPanel();
  protected abstract void getProcessTrack(Storable[] storable, int index);
  protected abstract BaseProcessTrack getProcessTrack();
  protected abstract BaseState getBaseState();
  public abstract void kill(AxisID axisID);
  protected abstract int getNumStorables();

  //FIXME needs to be public?
  public abstract boolean isNewManager();
  public abstract void setTestParamFile(File paramFile);
  
  public BaseManager() {
    propertyUserDir = System.getProperty("user.dir");
    createProcessTrack();
    createState();
    createProcessManager();
    createComScriptManager();
    //  Initialize the program settings
    debug = EtomoDirector.getInstance().isDebug();
    test = EtomoDirector.getInstance().isTest();
    if (!test) {
      createMainPanel();
      //mainFrame = EtomoDirector.getInstance().getMainFrame();
    }
    //imodManager should be created only once.
    createImodManager();
    initProgram();
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
    if (!test) {
      //  Initialize the static UIParameter object
      UIParameters uiparameters = new UIParameters();
      // Open the etomo data file if one was found on the command line
      if (!paramFileName.equals("")) {
        File etomoDataFile = new File(paramFileName);
        loadedTestParamFile = loadTestParamFile(etomoDataFile, axisID);
      }
    }
  }
  
  public boolean canChangeParamFileName() {
    return true;
  }
  
  /**
   * A message asking the ApplicationManager to save the test parameter
   * information to a file.
   */
  public void saveTestParamFile(AxisID axisID) {
    Storable[] storable = new Storable[getNumStorables()];
    storable[0] = getBaseMetaData();
    storable[1] = getBaseState();
    getProcessTrack(storable, 2);
    save(storable, axisID);
    //  Update the MRU test data filename list
    userConfig.putDataFile(paramFile.getAbsolutePath());
    uiHarness.setMRUFileLabels(userConfig.getMRUFileList());
    // Reset the process track flag, if it exists
    BaseProcessTrack processTrack = getProcessTrack();
    if (processTrack != null) {
      processTrack.resetModified();
    }
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
    if (storable == null) {
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
      uiHarness.openMessageDialog(errorMessage, "Test parameter file save error", axisID);
    }
  }
 
  /**
   * Exit the program.  To guarantee that etomo can always exit, catch all
   * unrecognized Exceptions and Errors and return true.
   */
  public boolean exitProgram(AxisID axisID) {
    try {
      //  Check to see if any processes are still running
      ArrayList messageArray = new ArrayList();
      //handle background processes
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
          return false;
        }
      }
      saveTestParamOnExit(axisID);
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
        uiHarness.openMessageDialog(except.getMessage(),
            "AxisType problem", axisID);
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
  
  protected void saveTestParamOnExit(AxisID axisID) {
    if (paramFile != null) {
      saveTestParamFile(axisID);
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
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem", axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(),
        "Unable to retrieve rubberband coordinates from " + imodKey + ".", axisID);
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
          || result.indexOf(ImodProcess.ERROR_STRING) != -1
          || result.indexOf(ImodProcess.WARNING_STRING) != -1) {
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
      String[] messages = (String[]) messageArray.toArray(new String[messageArray.size()]);
      uiHarness.openMessageDialog(messages, "Rubberband Coordinates", axisID);
    }
    return results;
  }


  protected void setPanel() {
    uiHarness.pack();
    //  Resize to the users preferrred window dimensions
    getMainPanel().setSize(new Dimension(userConfig.getMainWindowWidth(),
        userConfig.getMainWindowHeight()));
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
   * Return the test parameter file as a File object
   * @return a File object specifying the data set parameter file.
   */
  //FIXME this may not have to be visible
  public File getTestParamFile() {
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
      Storable[] storable = new Storable[this.getNumStorables()];
      storable[0] = getBaseMetaData();
      storable[1] = getBaseState();
      getProcessTrack(storable, 2);
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
    if (!Utilities.isValidFile(paramFile, "Parameter file", invalidReason, true, true, true, false)) {
      uiHarness.openMessageDialog(invalidReason.toString(), "File Error", axisID);
      return false;
    }
    this.paramFile = paramFile;
    return true;
  }
  
  protected void backupFile(File file, AxisID axisID) {
    if (file.exists()) {
      File backupFile = new File(file.getAbsolutePath() + "~");
      try {
        Utilities.renameFile(file, backupFile);
      }
      catch (IOException except) {
        System.err.println("Unable to backup file: " + file.getAbsolutePath()
          + " to " + backupFile.getAbsolutePath());
        uiHarness.openMessageDialog(except.getMessage(), "File Rename Error", axisID);
      }
    }
  }
  
  private static boolean getTest() {
    return test;
  }
  
  //create functions
  
  private void createImodManager() {
    imodManager = new ImodManager();
  }
  
  /**
   * Notification message that a background process is done.
   * 
   * @param threadName
   *            The name of the thread that has finished
   */
  public void processDone(String threadName, int exitValue,
    ProcessName processName, AxisID axisID) {
    if (threadName.equals(threadNameA)) {
      getMainPanel().stopProgressBar(AxisID.FIRST);
      threadNameA = "none";
      backgroundProcessA = false;
      backgroundProcessNameA = null;
    }
    else if (threadName.equals(threadNameB)) {
      getMainPanel().stopProgressBar(AxisID.SECOND);
      threadNameB = "none";
    }
    else {
      uiHarness.openMessageDialog("Unknown thread finished!!!", "Thread name: "
        + threadName, axisID);
    }
    if (processName != null) {
      updateDialog(processName, axisID);
    }
    //  Start the next process if one exists and the exit value was equal zero
    if (isNextProcessSet(axisID)) {
      if (exitValue == 0) {
        startNextProcess(axisID);
      }
      else {
        resetNextProcess(axisID);
      }
    }
  }
  
  protected void setNextProcess(AxisID axisID, String nextProcess) {
    if (axisID == AxisID.SECOND) {
      nextProcessB = nextProcess;
    }
    else {
      nextProcessA = nextProcess;
    }
  }
  
  protected void resetNextProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      nextProcessB = "";
    }
    else {
      nextProcessA = "";
    }
  }
  
  protected String getNextProcess(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return nextProcessB;
    }
    return nextProcessA;
  }
  
  protected boolean isNextProcessSet(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return !nextProcessB.equals("");
    }
    return !nextProcessA.equals("");
  }
  
  public void packMainWindow(AxisID axisID) {
    uiHarness.repaint(axisID);
    uiHarness.fitWindow(axisID);
  }
}
