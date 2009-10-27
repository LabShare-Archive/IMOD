package etomo;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.comscript.PeetParserParam;
import etomo.comscript.ProcesschunksParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.PeetProcessManager;
import etomo.process.ProcessResultDisplayFactoryBlank;
import etomo.process.ProcessResultDisplayFactoryInterface;
import etomo.process.SystemProcessException;
import etomo.storage.ComFileFilter;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.ParameterStore;
import etomo.storage.PeetFileFilter;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.ConstProcessSeries;
import etomo.type.DialogType;
import etomo.type.IntKeyList;
import etomo.type.InterfaceType;
import etomo.type.PeetMetaData;
import etomo.type.PeetScreenState;
import etomo.type.PeetState;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.LogPanel;
import etomo.ui.MainPanel;
import etomo.ui.MainPeetPanel;
import etomo.ui.ParallelPanel;
import etomo.ui.PeetDialog;
import etomo.ui.ProcessDisplay;
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
 * <p> Revision 1.56  2009/10/26 21:28:28  sueh
 * <p> bug# 1175 Save as is not a good idea when the param file is not loaded,
 * <p> so don't allow it at all.
 * <p>
 * <p> Revision 1.55  2009/10/23 22:22:34  sueh
 * <p> bug# 1275 Made touch() a start function in BaseProcessManager.
 * <p>
 * <p> Revision 1.54  2009/10/15 23:26:59  sueh
 * <p> bug# 1274 Changed String PeetDialog.getDirectory() to getDirectoryString.
 * <p>
 * <p> Revision 1.53  2009/09/16 16:31:21  sueh
 * <p> bug# 1227 In setParamFile(), setting the tab after setting the status bar.
 * <p>
 * <p> Revision 1.52  2009/09/01 03:17:35  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.51  2009/06/11 16:46:26  sueh
 * <p> bug# 1221 Sending the process panel to the process function in the
 * <p> manager wrapped in a ProcessDisplay interface.  Changed
 * <p> startNextProcess.
 * <p>
 * <p> Revision 1.50  2009/04/27 17:57:09  sueh
 * <p> bug# 1213 Moved call to set status bar from loadMatlabParam to
 * <p> setParamFile.
 * <p>
 * <p> Revision 1.49  2009/04/14 23:01:38  sueh
 * <p> bug# 1207  In reconnect:  handling some situations where process data is not running.
 * <p>
 * <p> Revision 1.48  2009/04/13 22:22:22  sueh
 * <p> bug# 1207 Implemented doAutomation in BaseManager.
 * <p>
 * <p> Revision 1.47  2009/03/17 00:30:14  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.46  2009/03/02 18:56:13  sueh
 * <p> bug# 1193 Commented openProcessingPanel().
 * <p>
 * <p> Revision 1.45  2009/02/04 23:04:24  sueh
 * <p> bug# 1158 passing logPanel to mainPanel.setStatusBarText so its title can
 * <p> be updated.
 * <p>
 * <p> Revision 1.44  2008/10/01 22:50:01  sueh
 * <p> bug# 1113 Added getFocusComponent.
 * <p>
 * <p> Revision 1.43  2008/05/28 02:48:03  sueh
 * <p> bug# 1111 Removed processDialogTypeA and B from BaseManager.
 * <p> The dialogType for processes should be handled by ProcessSeries.
 * <p> Passing a DialogType parameter to startNextProcess.
 * <p>
 * <p> Revision 1.42  2008/05/06 23:55:23  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.
 * <p>
 * <p> Revision 1.41  2008/05/03 00:34:03  sueh
 * <p> bug# 847 Passing ProcessSeries to the process manager,
 * <p> startNextProcess, and to all process functions.  To avoid having to decide
 * <p> which processes are next processes, pass it everywhere, even to
 * <p> processes that don't use ProcessResultDisplay.  The UI should not create
 * <p> any ProcessSeries and should pass them as null (since they don't know
 * <p> about processes).  Before adding to a process series, create it if it doesn't
 * <p> exist.  Before checking a process series, make sure it exists.  Since a
 * <p> series is only created when it doesn't exist and is needed, I don't have to
 * <p> keep track of which process comes first in a series.
 * <p>
 * <p> Revision 1.40  2008/05/01 22:53:12  sueh
 * <p> bug# 1107 Removed imod(String, List, Run3dmodMenuOptions) and add
 * <p> buildFileNameArray(List) to make it easier to customize imodAvgVol() and
 * <p> imodRef().  Didn't need to customize them after all.
 * <p>
 * <p> Revision 1.39  2008/04/02 01:37:23  sueh
 * <p> bug# 1095 Added comment to setPeetDialogParameters.
 * <p>
 * <p> Revision 1.38  2008/03/06 00:24:13  sueh
 * <p> bug# 1088 In peetParser added error message when matlabParam is null.
 * <p>
 * <p> Revision 1.37  2008/01/31 20:16:55  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.36  2008/01/14 20:22:59  sueh
 * <p> bug# 1050 Added an empty getProcessResultDisplayFactoryInterface.  Calling
 * <p> BaseManager.reconnect from openProcessingPanel to reconnect to parallel
 * <p> processes.
 * <p>
 * <p> Revision 1.35  2007/12/26 21:57:45  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.34  2007/12/10 21:53:25  sueh
 * <p> bug# 1041 Changed getBaseState to return state.
 * <p>
 * <p> Revision 1.33  2007/09/27 19:21:23  sueh
 * <p> bug# 1044 Made ProcessorTable the ParallelProgress display instead of
 * <p> ParallelPanel.
 * <p>
 * <p> Revision 1.32  2007/09/07 00:16:41  sueh
 * <p> bug# 989 Using a public INSTANCE for EtomoDirector instead of getInstance
 * <p> and createInstance.
 * <p>
 * <p> Revision 1.31  2007/08/29 21:40:11  sueh
 * <p> bug# 1041 Made getBaseState public.
 * <p>
 * <p> Revision 1.30  2007/08/02 22:37:39  sueh
 * <p> bug# 1034 Adding a right click menu to imodAvgVol and imodRef.
 * <p>
 * <p> Revision 1.29  2007/07/30 18:33:36  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.28  2007/06/08 22:13:17  sueh
 * <p> bug# 1014 Removed setMetaData(ImodManager).  When importing
 * <p> a .prm, duplicating a project, or copying parameters fail if reading
 * <p> the .prm file fails.  If initializeUIParameters has been called, back out the
 * <p> changes by calling clearUIParameters.
 * <p>
 * <p> Revision 1.27  2007/06/07 23:02:30  sueh
 * <p> bug# 1009 In loadMatlabParam setting matlabParam.fnOutput from PeetDialog, in case the user set it.
 * <p>
 * <p> Revision 1.26  2007/06/05 17:41:39  sueh
 * <p> bug# 1006 Added calls to mainPanel.setStatusBarText and
 * <p> EtomoDirectory.renameCurrentManager in loadMatlabParam and
 * <p> loadParamFile.
 * <p>
 * <p> Revision 1.25  2007/06/04 23:03:59  sueh
 * <p> bug# 1005 Added boolean parametersOnly to setPeetDialogParameters,
 * <p> loadMatlabParam, and loadParamFile.  Added boolean metaDataLoaded to
 * <p> setPeetDialogParameters.
 * <p>
 * <p> Revision 1.24  2007/05/31 22:23:38  sueh
 * <p> bug# 1004 Using createNewFile() instead of touch() to create the .epe file.
 * <p>
 * <p> Revision 1.23  2007/05/21 22:28:36  sueh
 * <p> bug# 964 Added getInterfaceType().
 * <p>
 * <p> Revision 1.22  2007/05/16 23:47:08  sueh
 * <p> bug# 964 Added isUserDirValid() to check for directories that already
 * <p> contains an .epe file.
 * <p>
 * <p> Revision 1.21  2007/05/16 22:58:24  sueh
 * <p> bug# 964 Added loadParamFile(File) to load the peet dialog with data from
 * <p> an .epe file and a .prm file from a different database.
 * <p>
 * <p> Revision 1.20  2007/05/15 21:44:33  sueh
 * <p> bug# 964 Added imodRef().
 * <p>
 * <p> Revision 1.19  2007/05/11 14:34:33  sueh
 * <p> bug# 964 Added imodAvgVol().
 * <p>
 * <p> Revision 1.18  2007/05/08 19:16:48  sueh
 * <p> bug# 964 Passing the import directory to loadPeetDialog when importing
 * <p> the .prm file, so that files which don't have an absolute path will have the
 * <p> import directory as their parent.  Setting the user.dir property when
 * <p> initializeUIParameters completes successfully.
 * <p>
 * <p> Revision 1.17  2007/05/07 17:19:58  sueh
 * <p> bug# 964 Added setMatlabParam(File).
 * <p>
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
  private final ProcessResultDisplayFactoryBlank processResultDisplayFactory = new ProcessResultDisplayFactoryBlank();

  private final PeetMetaData metaData;
  private final PeetProcessManager processMgr;
  private final PeetState state;

  private PeetDialog peetDialog = null;
  private MatlabParam matlabParam = null;
  private MainPeetPanel mainPanel;

  PeetManager() {
    this("");
  }

  PeetManager(String paramFileName) {
    super();
    metaData = new PeetMetaData();
    state = new PeetState();
    createState();
    processMgr = new PeetProcessManager(this);
    initializeUIParameters(paramFileName, AXIS_ID);
    if (loadedParamFile) {
      setMatlabParam(false);
    }
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData, logPanel);
      openPeetDialog();
    }
  }

  void initializeUIParameters(String paramFileName, AxisID axisID) {
    super.initializeUIParameters(paramFileName, axisID);
    if (loadedParamFile) {
      System.setProperty("user.dir", propertyUserDir);
    }
  }

  public boolean canChangeParamFileName() {
    return false;
  }

  public ProcessResultDisplayFactoryInterface getProcessResultDisplayFactoryInterface(
      AxisID axisID) {
    return processResultDisplayFactory;
  }

  public boolean canSnapshot() {
    return false;
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.PEET;
  }
  
  public LogPanel createLogPanel() {
    return LogPanel.getInstance(getManagerKey());
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

  public Component getFocusComponent() {
    if (peetDialog == null) {
      return null;
    }
    return peetDialog.getFocusComponent();
  }

  public String getName() {
    return metaData.getName();
  }

  public void kill(AxisID axisID) {
    processMgr.kill(axisID);
  }

  public PeetState getState() {
    return state;
  }

  public void pause(AxisID axisID) {
    processMgr.pause(axisID);
  }

  /**
   * Only one .epe file per directory
   * @param userDir
   * @param name
   * @return
   */
  private boolean isUserDirValid(File userDir, String name) {
    File[] paramFiles = userDir.listFiles(new PeetFileFilter(false));
    if (paramFiles == null || paramFiles.length == 0) {
      return true;
    }
    //OK to use directory if it contains an .epe file of the same name
    if (paramFiles.length == 1
        && DatasetFiles.getPeetRootName(paramFiles[0].getName()).equals(name)) {
      return true;
    }
    uiHarness.openMessageDialog(
        "The directory " + userDir.getAbsolutePath() + " can contain only one "
            + DatasetFiles.PEET_DATA_FILE_EXT + " file.", "Entry Error",
        getManagerKey());
    return false;
  }

  public void copyParameters(final File file) {
    if (new PeetFileFilter().accept(file)) {
      loadParamFile(file, true);
    }
    else {
      loadMatlabParam(file, true);
    }
  }

  /**
   * Creates matlabParam from matlabParamFile and sets the paramFile and
   * propertyUserDir.  Changes the location of the file in matlabParam to 
   * propertyUserDir and loads peetDialog.  This creates a new project out of an
   * existing .prm file.
   * @param matlabParamFile
   * @param parametersOnly - allows the user to reuse a set of parameters, means that fnOutput is required
   */
  public void loadMatlabParam(final File matlabParamFile, boolean parametersOnly) {
    matlabParam = new MatlabParam(matlabParamFile, false, getManagerKey());
    if (!matlabParam.read()) {
      return;
    }
    String fnOutput = peetDialog.getFnOutput();
    if (fnOutput == null || fnOutput.matches("\\s*")) {
      if (parametersOnly) {
        uiHarness.openMessageDialog(PeetDialog.FN_OUTPUT_LABEL
            + " is required when copying parameters.", "Entry Error",
            getManagerKey());
        return;
      }
      peetDialog.setFnOutput(matlabParam.getFnOutput());
    }
    if (!setParamFile()) {
      return;
    }
    matlabParam.setFnOutput(peetDialog.getFnOutput());
    matlabParam.setFile(propertyUserDir);
    setPeetDialogParameters(matlabParamFile.getParentFile(), false,
        parametersOnly);
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getName());
  }

  /**
   * Load a param file and a matlab param file from another directory.
   * @param origParamFile
   * @param parametersOnly - allows the user to reuse a set of parameters, means
   * that fnOutput is required
   */
  public void loadParamFile(final File origParamFile, boolean parametersOnly) {
    String newName = peetDialog.getFnOutput();
    boolean emptyNewName = newName == null || newName.matches("\\s*");
    if (parametersOnly && emptyNewName) {
      uiHarness.openMessageDialog(PeetDialog.FN_OUTPUT_LABEL
          + " is required when copying parameters.", "Entry Error",
          getManagerKey());
      return;
    }
    String newDirName = peetDialog.getDirectoryString();
    File newDir = new File(newDirName);
    if (origParamFile.getParentFile().equals(newDir)) {
      uiHarness.openMessageDialog(
          "Cannot duplicate a project in the same directory.", "Entry Error",
          getManagerKey());
      return;
    }
    if (!isUserDirValid(newDir, newName)) {
      return;
    }
    //load the original param file
    PeetMetaData origMetaData = new PeetMetaData();
    try {
      ParameterStore origParameterStore = ParameterStore.getInstance(
          origParamFile, getManagerKey());
      if (origParameterStore != null) {
        origParameterStore.load(origMetaData);
        Storable[] storables = getStorables(0);
        if (storables != null) {
          for (int i = 0; i < storables.length; i++) {
            origParameterStore.load(storables[i]);
          }
        }
      }
      //If the user didn't specify a name, use the one from the origParamFile
      if (emptyNewName) {
        newName = origMetaData.getName();
      }
      //set the param file
      File paramFile = DatasetFiles.getPeetDataFile(newDirName, newName);
      if (!paramFile.exists()) {
        processMgr.createNewFile(paramFile.getAbsolutePath());
      }
      initializeUIParameters(paramFile, AXIS_ID, true);
      if (loadedParamFile) {
        //copy data from the original param file
        metaData.copy(origMetaData);
        metaData.setName(newName);
        //read the matlab param file associated with the original param file
        matlabParam = new MatlabParam(DatasetFiles.getMatlabParamFile(
            origParamFile.getParent(), origMetaData.getName()), false,
            getManagerKey());
        //the .prm file is not readable but paramFile has already been set, so
        //all the initiazation changes have to be backed out.
        if (!matlabParam.read()) {
          clearUIParameters();
          peetDialog.reset();
          return;
        }
        //change the matlab file location to the user-specified directory and
        //change the name of the file to user-specified name
        matlabParam.setFnOutput(newName);
        matlabParam.setFile(propertyUserDir);
        //load the param file and matlab file into the PEET dialog
        peetDialog.setFnOutput(newName);
        peetDialog.updateDisplay(true);
        setPeetDialogParameters(null, true, parametersOnly);
        mainPanel.setStatusBarText(paramFile, metaData, logPanel);
        EtomoDirector.INSTANCE.renameCurrentManager(metaData.getName());
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
  }

  public void setParamFile(File paramFile) {
    if (!paramFile.exists()) {
      processMgr.createNewFile(paramFile.getAbsolutePath());
    }
    initializeUIParameters(paramFile, AXIS_ID, false);
    if (loadedParamFile) {
      String rootName = DatasetFiles.getRootName(paramFile);
      metaData.setName(rootName);
      imodManager.setMetaData(metaData);
      setMatlabParam(false);
      if (peetDialog != null) {
        peetDialog.setDirectory(paramFile.getParent());
        peetDialog.setFnOutput(rootName);
        peetDialog.updateDisplay(true);
      }
    }
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
    String dirName = peetDialog.getDirectoryString();
    if (!isUserDirValid(new File(dirName), name)) {
      return false;
    }
    File paramFile = new File(dirName, name + DatasetFiles.PEET_DATA_FILE_EXT);
    if (!paramFile.exists()) {
      processMgr.createNewFile(paramFile.getAbsolutePath());
    }
    initializeUIParameters(paramFile, AXIS_ID, false);
    if (!loadedParamFile) {
      return false;
    }
    metaData.setName(name);
    if (!metaData.isValid()) {
      uiHarness.openMessageDialog(
          "Invalid data, unable to proceed.  Please exit and restart Etomo",
          "Fatal Error", getManagerKey());
      return false;
    }
    imodManager.setMetaData(metaData);
    mainPanel.setStatusBarText(paramFile, metaData, logPanel);
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getName());
    if (matlabParam == null) {
      setMatlabParam(true);
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

  public void save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
    savePeetDialog();
  }

  public int getParallelProcessingDefaultNice() {
    return 18;
  }

  /**
   * Open the *AvgVol*.mrc files in 3dmod
   */
  public void imodAvgVol(Run3dmodMenuOptions menuOptions) {
    //build the list of files - they should be in order
    IntKeyList.Walker lstThresholds = state.getLstThresholds();
    final StringBuffer name = new StringBuffer(metaData.getName());
    name.append("_AvgVol_").append(state.getIterationListSize()).append('P');
    StringBuffer fileName;
    List fileNameList = new ArrayList();
    while (lstThresholds.hasNext()) {
      fileName = new StringBuffer();
      fileName.append(name).append(lstThresholds.nextString()).append(".mrc");
      fileNameList.add(fileName.toString());
    }
    try {
      imodManager.open(ImodManager.AVG_VOL_KEY,
          buildFileNameArray(fileNameList), menuOptions);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
    }
  }

  /**
   * Open the *Ref*.mrc files in 3dmod
   */
  public void imodRef(Run3dmodMenuOptions menuOptions) {
    //build the list of files - they should be in order
    int iterationListSize = state.getIterationListSize();
    final StringBuffer name = new StringBuffer(metaData.getName());
    name.append("_Ref");
    StringBuffer fileName;
    List fileNameList = new ArrayList();
    for (int i = 1; i <= iterationListSize + 1; i++) {
      fileName = new StringBuffer();
      fileName.append(name).append(i).append(".mrc");
      fileNameList.add(fileName.toString());
    }
    try {
      imodManager.open(ImodManager.REF_KEY, buildFileNameArray(fileNameList),
          menuOptions);
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
    }
  }

  private String[] buildFileNameArray(List fileNameList) {
    String[] fileNameArray;
    if (fileNameList.size() == 0) {
      fileNameArray = new String[0];
    }
    else if (fileNameList.size() == 1) {
      fileNameArray = new String[1];
      fileNameArray[0] = (String) fileNameList.get(0);
    }
    else {
      fileNameArray = (String[]) fileNameList.toArray(new String[fileNameList
          .size()]);
    }
    return fileNameArray;
  }

  public void peetParser(ProcessSeries processSeries, DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    savePeetDialog();
    if (matlabParam == null) {
      uiHarness.openMessageDialog("Must set " + PeetDialog.DIRECTORY_LABEL
          + " and " + PeetDialog.FN_OUTPUT_LABEL, "Entry Error",
          getManagerKey());
      return;
    }
    PeetParserParam param = new PeetParserParam(this, matlabParam.getFile());
    param.getParameters(matlabParam);
    try {
      try {
        LogFile log = LogFile.getInstance(param.getLogFile(), getManagerKey());
        log.backup();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
      removeComFiles();
      String threadName = processMgr.peetParser(param, processSeries);
      processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString(),
          ProcessName.PEET_PARSER);
      setThreadName(threadName, AxisID.ONLY);
      mainPanel.startProgressBar("Running " + ProcessName.PEET_PARSER,
          AxisID.ONLY, ProcessName.PEET_PARSER);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog("Unable to run " + ProcessName.PEET_PARSER
          + ", SystemProcessException.\n" + e.getMessage(), "Process Error",
          getManagerKey());
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

  void processSucceeded(AxisID axisID, ProcessName processName) {
  }

  public BaseState getBaseState() {
    return state;
  }

  void createProcessTrack() {
  }

  void createMainPanel() {
    mainPanel = new MainPeetPanel(this);
  }

  void createComScriptManager() {
  }

 public BaseProcessManager getProcessManager() {
    return processMgr;
  }

  BaseProcessTrack getProcessTrack() {
    return null;
  }

  void getProcessTrack(Storable[] storable, int index) {
  }

  Storable[] getStorables(int offset) {
    Storable[] storables = new Storable[3 + offset];
    int index = offset;
    storables[index++] = metaData;
    storables[index++] = screenState;
    storables[index++] = state;
    return storables;
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  void startNextProcess(final AxisID axisID, final String nextProcess,
      final ProcessResultDisplay processResultDisplay,
      ProcessSeries processSeries, DialogType dialogType,
      ProcessDisplay display, ProcessName subProcessName) {
    if (nextProcess.equals(ProcessName.PROCESSCHUNKS.toString())) {
      processchunks(processSeries);
    }
  }

  /**
   * Initialize metalabParamFile.  Dependent on metaData.
   */
  private void setMatlabParam(boolean newFile) {
    if (!loadedParamFile || matlabParam != null || paramFile == null) {
      return;
    }
    matlabParam = new MatlabParam(DatasetFiles.getMatlabParamFile(this),
        newFile, getManagerKey());
    matlabParam.read();
  }

  private void createState() {
  }

  /**
   * MUST run reconnect for all axis
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
    reconnect(processMgr.getSavedProcessData(AxisID.ONLY), AxisID.ONLY);
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
    setPeetDialogParameters(null, true, false);
    mainPanel.showProcess(peetDialog.getContainer(), AXIS_ID);
  }

  private void setPeetDialogParameters(File importDir, boolean metaDataLoaded,
      boolean parametersOnly) {
    if (paramFile != null && metaData.isValid()) {
      if (metaDataLoaded) {
        peetDialog.setParameters(metaData, parametersOnly);
        peetDialog.setParameters(screenState);
      }
      //Always load from matlabParam after loading the data file because some
      //values are in both places (maskModelPts) and the .prm values take
      //precedence over the .epe values.
      if (matlabParam != null) {
        peetDialog.setParameters(matlabParam, importDir, parametersOnly);
      }
      peetDialog.setDirectory(propertyUserDir);
      peetDialog.checkIncorrectPaths();
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
  private void processchunks(ConstProcessSeries processSeries) {
    if (peetDialog == null) {
      return;
    }
    ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY,
        peetDialog.getFnOutput());
    ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
    peetDialog.getParameters(param);
    if (!parallelPanel.getParameters(param)) {
      getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
      return;
    }
    //param should never be set to resume
    parallelPanel.getParallelProgressDisplay().resetResults();
    processchunks(AxisID.ONLY, param, null, processSeries);
  }
}