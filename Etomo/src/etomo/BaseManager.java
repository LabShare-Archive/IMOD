package etomo;

import java.awt.Dimension;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JOptionPane;

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
import etomo.type.ProcessName;
import etomo.type.UserConfiguration;
import etomo.ui.MainFrame;
import etomo.ui.MainPanel;
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
  protected static MainFrame mainFrame = EtomoDirector.getInstance()
      .getMainFrame();
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
  protected boolean isDataParamDirty = false;
  // Control variable for process execution
  // FIXME: this going to need to expand to handle both axis
  protected String nextProcess = "";

  protected String threadNameA = "none";

  protected String threadNameB = "none";
  
  protected boolean backgroundProcessA = false;
  protected String backgroundProcessNameA = null;
  protected String propertyUserDir = null;

  
  //private static variables
  private static boolean debug = false;

  protected abstract void createComScriptManager();
  protected abstract void createProcessManager();
  protected abstract void createMainPanel();
  protected abstract void createMetaData();
  protected abstract void createProcessTrack();
  protected abstract void updateDialog(ProcessName processName, AxisID axisID);
  protected abstract void startNextProcess(AxisID axisID);
  protected abstract void setMetaData(ImodManager imodManager);
  public abstract BaseMetaData getBaseMetaData();
  public abstract MainPanel getMainPanel();
  protected abstract BaseProcessTrack getProcessTrack();
  public abstract void kill(AxisID axisID);

  //FIXME needs to be public?
  public abstract boolean isNewManager();
  public abstract void setTestParamFile(File paramFile);
  
  public BaseManager() {
    propertyUserDir = System.getProperty("user.dir");
    createMetaData();
    createProcessTrack();
    createProcessManager();
    createComScriptManager();
    createMainPanel();
    //  Initialize the program settings
    debug = EtomoDirector.getInstance().isDebug();
    test = EtomoDirector.getInstance().isTest();
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
  protected void initializeUIParameters(String paramFileName) {
    if (!test) {
      //  Initialize the static UIParameter object
      UIParameters uiparameters = new UIParameters();
      // Open the etomo data file if one was found on the command line
      if (!paramFileName.equals("")) {
        File etomoDataFile = new File(paramFileName);
        loadedTestParamFile = loadTestParamFile(etomoDataFile);
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
  public void saveTestParamFile() {
    try {
      backupFile(paramFile);
      ParameterStore paramStore = new ParameterStore(paramFile);
      Storable[] storable = new Storable[2];
      storable[0] = getBaseMetaData();
      storable[1] = getProcessTrack();
      paramStore.save(storable);
      //  Update the MRU test data filename list
      userConfig.putDataFile(paramFile.getAbsolutePath());
      mainFrame.setMRUFileLabels(userConfig.getMRUFileList());
      // Reset the process track flag
      getProcessTrack().resetModified();
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file save error";
      errorMessage[1] = "Could not save test parameter data to file:";
      errorMessage[2] = except.getMessage();
      getMainPanel().openMessageDialog(errorMessage, "Test parameter file save error");
    }
    isDataParamDirty = false;
  }
  
  /**
   * save the meta data parameter
   * information to a file.
   */
  public void saveMetaData() {
    try {
      backupFile(paramFile);
      ParameterStore paramStore = new ParameterStore(paramFile);
      Storable[] storable = new Storable[1];
      storable[0] = getBaseMetaData();
      paramStore.save(storable);
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file save error";
      errorMessage[1] = "Could not save meta data to file:";
      errorMessage[2] = except.getMessage();
      getMainPanel().openMessageDialog(errorMessage, "Test parameter file save error");
    }
    isDataParamDirty = false;
  }
  
  /**
   * Exit the program
   */
  public boolean exitProgram() {
    //  Check to see if any processes are still running
    ArrayList messageArray = new ArrayList();
    //handle background processes
    if (!threadNameA.equals("none") && backgroundProcessA) {
      messageArray.add("The " + backgroundProcessNameA
        + " process will continue to run after Etomo ends.");
      messageArray.add("Check " + backgroundProcessNameA
        + ".log for status.");
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
      if (!mainFrame.openYesNoDialog(
        (String[]) messageArray.toArray(new String[messageArray.size()]))) {
        return false;
      }
    }
    if (saveTestParamIfNecessary()) {
      //  Should we close the 3dmod windows
      try {
        if (imodManager.isOpen()) {
          String[] message = new String[3];
          message[0] = "There are still 3dmod programs running.";
          message[1] = "Do you wish to end these programs?";
          if (mainFrame.openYesNoDialog(message)) {
            imodManager.quit();
          }
        }
      }
      catch (AxisTypeException except) {
        except.printStackTrace();
        getMainPanel().openMessageDialog(except.getMessage(), "AxisType problem");
      }
      catch (SystemProcessException except) {
        except.printStackTrace();
        getMainPanel().openMessageDialog(except.getMessage(),
          "Problem closing 3dmod");
      }
      return true;
    }
    return false;
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
  
  public Vector imodGetRubberbandCoordinates(String imodKey) {
    Vector results = null;
    try {
      results = imodManager.getRubberbandCoordinates(imodKey);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      getMainPanel().openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      getMainPanel().openMessageDialog(except.getMessage(),
        "Unable to retrieve rubberband coordinates from " + imodKey + ".");
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
      getMainPanel().openMessageDialog(messages, "Rubberband Coordinates");
    }
    return results;
  }


  protected void setPanel() {
    mainFrame.pack();
    //  Resize to the users preferrred window dimensions
    getMainPanel().setSize(new Dimension(userConfig.getMainWindowWidth(),
        userConfig.getMainWindowHeight()));
    mainFrame.doLayout();
    mainFrame.validate();
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
  protected boolean loadTestParamFile(File paramFile) {
    FileInputStream processDataStream;
    try {
      // Read in the test parameter data file
      ParameterStore paramStore = new ParameterStore(paramFile);
      Storable[] storable = new Storable[2];
      storable[0] = getBaseMetaData();
      storable[1] = getProcessTrack();
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
      getMainPanel().openMessageDialog(errorMessage, "File not found error");
      return false;
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not read the test parameter data from file:";
      errorMessage[2] = except.getMessage();
      getMainPanel().openMessageDialog(errorMessage,
        "Test parameter file read error");
      return false;
    }
    StringBuffer invalidReason = new StringBuffer();
    if (!Utilities.isValidFile(paramFile, "Parameter file", invalidReason, true, true, true, false)) {
      getMainPanel().openMessageDialog(invalidReason.toString(), "File Error");
      return false;
    }
    this.paramFile = paramFile;
    return true;
  }
  
  
  protected void backupFile(File file) {
    if (file.exists()) {
      File backupFile = new File(file.getAbsolutePath() + "~");
      try {
        Utilities.renameFile(file, backupFile);
      }
      catch (IOException except) {
        System.err.println("Unable to backup file: " + file.getAbsolutePath()
          + " to " + backupFile.getAbsolutePath());
        getMainPanel().openMessageDialog(except.getMessage(), "File Rename Error");
      }
    }
  }
  
  /**
   * If the current state needs to be saved the users is queried with a dialog
   * box.
   * @return True if either: the current state does not need to be saved, the
   * state is successfully saved, or the user chooses not to save the current
   * state by selecting no.  False is returned if the state can not be
   * successfully saved, or the user chooses cancel.
   */
  protected boolean saveTestParamIfNecessary() {
    // Check to see if the current dataset needs to be saved
    if (isDataParamDirty || getProcessTrack().isModified()) {
      String[] message = {"Save the current data file ?"};
      int returnValue = mainFrame.openYesNoCancelDialog(message);
      if (returnValue == JOptionPane.CANCEL_OPTION) {
        return false;
      }
      if (returnValue == JOptionPane.NO_OPTION) {
        return true;
      }
      // If the selects Yes then try to save the current EDF file
      if (paramFile == null) {
        if (!getMainPanel().getTestParamFilename()) {
          return false;
        }
      }
      // Be sure the file saving was successful
      saveTestParamFile();
      if (isDataParamDirty) {
        return false;
      }
    }
    return true;
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
      getMainPanel().openMessageDialog("Unknown thread finished!!!", "Thread name: "
        + threadName);
    }
    if (processName != null) {
      updateDialog(processName, axisID);
    }
    //  Start the next process if one exists and the exit value was equal zero
    if (!nextProcess.equals("")) {
      if (exitValue == 0) {
        startNextProcess(axisID);
      }
      else {
        nextProcess = "";
      }
    }
  }
  
  public void packMainWindow() {
    mainFrame.repaint();
    getMainPanel().fitWindow();
  }

}
