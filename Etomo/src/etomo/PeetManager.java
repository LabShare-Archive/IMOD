package etomo;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import etomo.comscript.AverageAllParam;
import etomo.comscript.PeetParserParam;
import etomo.comscript.ProcesschunksParam;
import etomo.logic.PeetStartupData;
import etomo.logic.VersionControl;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.PeetProcessManager;
import etomo.process.ProcessData;
import etomo.process.SystemProcessException;
import etomo.storage.AveragedFileNames;
import etomo.storage.ComFileFilter;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.MatlabParamFileFilter;
import etomo.storage.ParameterStore;
import etomo.storage.PeetFileFilter;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.DialogType;
import etomo.type.FileType;
import etomo.type.InterfaceType;
import etomo.type.PeetMetaData;
import etomo.type.PeetScreenState;
import etomo.type.PeetState;
import etomo.type.ProcessEndState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessingMethod;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldValidationFailedException;
import etomo.ui.UIComponent;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.MainPeetPanel;
import etomo.ui.swing.ParallelPanel;
import etomo.ui.swing.PeetDialog;
import etomo.ui.swing.PeetStartupDialog;
import etomo.ui.swing.ProcessDisplay;
import etomo.ui.swing.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.EnvironmentVariable;
import etomo.util.Utilities;

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
 * <p> Revision 1.68  2011/02/21 21:07:24  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.67  2011/02/03 05:53:05  sueh
 * <p> bug# 1422 Using ProcessingMethod to keep track of which type of
 * <p> processing method is in use.  The decisions about when to display the
 * <p> parallel processing table have been centralized in
 * <p> ProcessingMethodMediator.
 * <p>
 * <p> Revision 1.66  2010/11/13 16:02:54  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.65  2010/07/02 03:14:14  sueh
 * <p> bug# 1388 Calling processchunks with popupChunkWarnings equals to false.
 * <p>
 * <p> Revision 1.64  2010/04/28 15:37:21  sueh
 * <p> bug# 1344 Added getFileSubdirectoryName.  Passing params to process
 * <p> manager functions, standardizing "3dmod is open" messages to always
 * <p> use closeImod.  Using ProcessSeries.Process to hold process information.
 * <p>
 * <p> Revision 1.63  2010/02/17 04:42:47  sueh
 * <p> bug# 1301 Moved comScriptMgr and logPanel from BaseManager to child
 * <p> class.
 * <p>
 * <p> Revision 1.62  2010/01/13 21:52:09  sueh
 * <p> bug# 1292 In imodAvgVol getting the list of files from averageFilenames.txt.
 * <p>
 * <p> Revision 1.61  2009/12/08 02:30:06  sueh
 * <p> bug# 1286 Added averageAll.  Removed parserLstThresholds from PeetState; saving lstThresholds after prmParser is run.
 * <p>
 * <p> Revision 1.60  2009/12/01 00:20:34  sueh
 * <p> bug# 1285 In imodAvgVol if lstThresholds is empty use parserLstThresholds.
 * <p>
 * <p> Revision 1.59  2009/11/24 00:42:59  sueh
 * <p> bug# 1289 Added isInterfaceAvailable to check for PARTICLE_DIR and pop
 * <p> up message if it is not available.
 * <p>
 * <p> Revision 1.58  2009/10/29 12:01:52  sueh
 * <p> bug# 1245 Changed copyParameters, which called either loadParamFile or loadMatlabParam with parameteresOnly set to true, to loadParam(File, boolean parametersOnly).  It is being called by both buttons in UseExistingProjectPanel.
 * <p>
 * <p> Revision 1.57  2009/10/27 20:39:14  sueh
 * <p> bug# 1275 Moving the resposibility for creating the log panel to the child
 * <p> classes.  That way the Front Page manager doesn't have to have a log
 * <p> panel.  Handling a null process manager.
 * <p>
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

  private final PeetMetaData metaData;
  private final PeetProcessManager processMgr;
  private final PeetState state;

  private PeetDialog peetDialog = null;
  private MatlabParam matlabParam = null;
  private MainPeetPanel mainPanel;
  private PeetStartupDialog peetStartupDialog = null;
  private LoadState loadState = null;
  /**
   * valid is for handling failure before the manager key is set in EtomoDirector.
   */
  private boolean valid = true;

  private PeetManager() {
    this("");
  }

  private PeetManager(final String paramFileName) {
    super();
    metaData = new PeetMetaData(getLogProperties());
    state = new PeetState();
    createState();
    processMgr = new PeetProcessManager(this);
    initializeUIParameters(paramFileName, AXIS_ID);
    if (loadedParamFile) {
      setMatlabParam(false);
    }
  }

  static PeetManager getInstance() {
    PeetManager instance = new PeetManager();
    instance.openDialog();
    return instance;
  }

  static PeetManager getInstance(final String paramFileName) {
    PeetManager instance = new PeetManager(paramFileName);
    if (instance.loadState != LoadState.EXIT) {
      instance.openDialog();
    }
    return instance;
  }

  private void openDialog() {
    if (!VersionControl.isCompatiblePeet(AxisID.ONLY)) {
      valid = false;
      return;
    }
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openProcessingPanel();
      mainPanel.setStatusBarText(paramFile, metaData, logWindow);
      if (loadedParamFile) {
        openPeetDialog(null);
      }
      else {
        openPeetStartupDialog();
      }
    }
  }
  
  public boolean isStartupPopupOpen() {
    return !loadedParamFile;
  }

  void display() {
    if (peetStartupDialog != null) {
      peetStartupDialog.display();
    }
  }

  final boolean isTomosnapshotThumbnail() {
    return true;
  }

  public boolean isValid() {
    return valid && loadState != LoadState.EXIT;
  }

  void initializeUIParameters(final String paramFileName, final AxisID axisID) {
    super.initializeUIParameters(paramFileName, axisID);
    if (loadedParamFile) {
      System.setProperty("user.dir", propertyUserDir);
    }
  }

  static public boolean isInterfaceAvailable() {
    if (!EnvironmentVariable.INSTANCE.exists(null,
        EtomoDirector.INSTANCE.getOriginalUserDir(), EnvironmentVariable.PARTICLE_DIR,
        AxisID.ONLY)) {
      UIHarness.INSTANCE.openMessageDialog((BaseManager) null,
          "PEET is an optional package for particle averaging, which has "
              + "not been installed and correctly configured.  See the "
              + "PEET link under Other Programs at " + "http://bio3d.colorado.edu/.",
          "Interface Unavailable");
      return false;
    }
    return true;
  }

  public void pack() {
    if (peetDialog != null) {
      peetDialog.pack();
    }
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.PEET;
  }

  public BaseMetaData getBaseMetaData() {
    return metaData;
  }

  public BaseScreenState getBaseScreenState(final AxisID axisID) {
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

  public PeetState getState() {
    return state;
  }

  /**
   * Copy the .prm file, and the .epe file if requested.  Modify them, and load the
   * dataset into the manager and the dialog.
   * @param file .epe or .prm file from an existing dataset
   * @param parametersOnly
   */
  public void copyDataset(final PeetStartupData startupData) {
    if (startupData == null) {
      return;
    }
    File file = new File(startupData.getCopyFrom());
    // Create and correct the .epe file the new dataset directory.
    // Get the root name for this dataset
    String fnOutput = startupData.getBaseName();
    File destDir = new File(startupData.getDirectory());
    // Create the new file
    File destPeetFile = startupData.getParamFile();
    boolean peetFileCopied = false;
    if (new PeetFileFilter().accept(file)) {
      // Copy the .epe file
      try {
        Utilities.copyFile(file, destPeetFile);
        peetFileCopied = true;
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
    // If .epe file not copied, then create an empty .epe
    if (!peetFileCopied) {
      BaseProcessManager.touch(destPeetFile.getAbsolutePath(), this);
    }
    // Completely load the properties structure from the copied file
    ParameterStore destParameterStore;
    try {
      destParameterStore = ParameterStore.getInstance(destPeetFile);
      PeetMetaData destMetaData = new PeetMetaData(getLogProperties());
      destParameterStore.load(destMetaData);
      PeetScreenState screenState = new PeetScreenState(AxisID.ONLY, AxisType.SINGLE_AXIS);
      destParameterStore.load(screenState);
      PeetState state = new PeetState();
      destParameterStore.load(state);
      // Modify the properties to work with the new dataset
      destMetaData.setRootName(fnOutput);
      // Wipe out process data in case there is a process running in the source dataset.
      ProcessData processData = processMgr.getProcessData(AxisID.ONLY);
      processData.reset();
      // Save the properties back to the copied file
      destParameterStore.setAutoStore(true);
      destParameterStore.save(processData);
      destParameterStore.save(destMetaData);
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to load " + destPeetFile.getName()
          + ".  Close this file if it is open.  " + e.getMessage(), "Unable to Continue");
      return;
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to load " + destPeetFile.getName()
          + ".  " + e.getMessage(), "Unable to Continue");
      return;
    }

    // Create and correct the .prm file in the new dataset directory
    // Create the new file
    File destMatlabFile = new File(destDir, fnOutput + DatasetFiles.MATLAB_PARAM_FILE_EXT);
    File sourceMatlabFile;
    if (new MatlabParamFileFilter().accept(file)) {
      sourceMatlabFile = file;
    }
    else {
      String sourcePeetFileAbsolutePath = file.getAbsolutePath();
      sourceMatlabFile = new File(sourcePeetFileAbsolutePath.substring(0,
          sourcePeetFileAbsolutePath.lastIndexOf('.'))
          + DatasetFiles.MATLAB_PARAM_FILE_EXT);
    }
    // Attempt to copy the source .prm file to the directory of this dataset.
    if (!sourceMatlabFile.exists()) {
      uiHarness.openMessageDialog(this,
          "Unable to copy .prm file. " + sourceMatlabFile.getAbsolutePath()
              + " does not exist.", "Entry Error");
      return;
    }
    try {
      Utilities.copyFile(sourceMatlabFile, destMatlabFile);
    }
    catch (IOException e) {
      BaseProcessManager.touch(destMatlabFile.getAbsolutePath(), this);
    }
    // Load the .prm file
    MatlabParam param = loadMatlabParam(destMatlabFile, false);
    if (loadState == LoadState.SUCCESS) {
      param.setFnOutput(fnOutput);
      param.setFile(destDir.getAbsolutePath());
      param.write(this);
    }
    else if (loadState == LoadState.EXIT) {
      return;
    }

    // load the dateset into the manager and dialog
    initializeUIParameters(destPeetFile.getAbsolutePath(), AXIS_ID);
    if (!loadedParamFile) {
      uiHarness.openMessageDialog(this,
          "Unable to load " + destPeetFile.getAbsolutePath() + ".", "Entry Error");
      return;
    }
    if (peetDialog != null) {
      peetDialog.setFnOutput(fnOutput);
      peetDialog.setDirectory(startupData.getDirectory());

      if (metaData.isValid()) {
        peetDialog.setParameters(metaData);
      }
    }
    matlabParam = param;
    // Always load from matlabParam after loading the data file because some
    // values are in both places (maskModelPts) and the .prm values take
    // precedence over the .epe values.
    if (peetDialog != null) {
      peetDialog.setParameters(matlabParam,
          peetFileCopied ? null : sourceMatlabFile.getParentFile());
      peetDialog.updateDisplay(true);
    }
    mainPanel.setStatusBarText(paramFile, metaData, logWindow);
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getName());
    if (peetDialog != null) {
      peetDialog.convertCopiedPaths(file.getParentFile().getAbsolutePath());
      peetDialog.checkIncorrectPaths();
    }
  }

  public boolean setParamFile(final File paramFile) {
    if (!paramFile.exists()) {
      processMgr.createNewFile(paramFile.getAbsolutePath());
    }
    initializeUIParameters(paramFile, AXIS_ID, false);
    if (loadedParamFile) {
      String rootName = DatasetFiles.getRootName(paramFile);
      metaData.setName(rootName);
      imodManager.setMetaData(metaData);
      setMatlabParam(false);
      if (loadState == LoadState.EXIT) {
        return false;
      }
      if (peetDialog != null) {
        peetDialog.setDirectory(paramFile.getParent());
        peetDialog.setFnOutput(rootName);
        peetDialog.updateDisplay(true);
      }
    }
    return true;
  }

  /**
   * Tries to set paramFile.  Returns true if able to set paramFile.
   * If paramFile is already set, returns true.  Returns false if unable
   * to set paramFile.  Updates the peet dialog display if paramFile
   * was set successfully.
   * @return
   */
  public boolean setParamFile(final PeetStartupData startupData) {
    if (loadedParamFile) {
      return true;
    }
    if (startupData == null) {
      return false;
    }
    String name = startupData.getBaseName();
    if (peetDialog != null) {
      peetDialog.setFnOutput(name);
    }
    String dirName = startupData.getDirectory();
    if (peetDialog != null) {
      peetDialog.setDirectory(dirName);
    }
    File paramFile = startupData.getParamFile();
    if (!paramFile.exists()) {
      processMgr.createNewFile(paramFile.getAbsolutePath());
    }
    initializeUIParameters(paramFile, AXIS_ID, false);
    if (!loadedParamFile) {
      return false;
    }
    metaData.setName(name);
    if (!metaData.isValid()) {
      uiHarness.openMessageDialog(this,
          "Invalid data, unable to proceed.  Please exit and restart Etomo",
          "Fatal Error");
      return false;
    }
    imodManager.setMetaData(metaData);
    mainPanel.setStatusBarText(paramFile, metaData, logWindow);
    EtomoDirector.INSTANCE.renameCurrentManager(metaData.getName());
    if (matlabParam == null) {
      setMatlabParam(true);
      if (loadState == LoadState.EXIT) {
        return false;
      }
    }
    if (peetDialog != null) {
      peetDialog.updateDisplay(true);
    }
    return true;
  }

  /**
   * Call BaseManager.exitProgram(). Call savePeetDialog. Return the value of
   * BaseManager.exitProgram(). To guarantee that etomo can always exit, catch
   * all unrecognized Exceptions and Errors and return true.
   */
  public boolean exitProgram(final AxisID axisID) {
    try {
      if (super.exitProgram(axisID)) {
        endThreads();
        if (loadState != LoadState.EXIT) {
          saveParamFile();
        }
        return true;
      }
      return false;
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
  }

  public boolean save() throws LogFile.LockException, IOException {
    super.save();
    mainPanel.done();
    savePeetDialog(false, false);
    return true;
  }

  public int getParallelProcessingDefaultNice() {
    return 18;
  }

  /**
   * Open the *AvgVol*.mrc files in 3dmod
   */
  public void imodAvgVol(final Run3dmodMenuOptions menuOptions) {
    AveragedFileNames averagedFileNames = new AveragedFileNames();
    List avgVolList = new AveragedFileNames().getList(this, AxisID.ONLY,
        "Must press either " + PeetDialog.RUN_LABEL + " or "
            + PeetDialog.AVERAGE_ALL_LABEL + ".", "Process Not Run");
    try {
      imodManager.open(ImodManager.AVG_VOL_KEY, buildFileNameArray(avgVolList),
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

  /**
   * Open the *Ref*.mrc files in 3dmod
   */
  public void imodRef(final Run3dmodMenuOptions menuOptions) {
    // build the list of files - they should be in order
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
      imodManager
          .open(ImodManager.REF_KEY, buildFileNameArray(fileNameList), menuOptions);
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

  private String[] buildFileNameArray(final List fileNameList) {
    String[] fileNameArray;
    if (fileNameList.size() == 0) {
      fileNameArray = new String[0];
    }
    else if (fileNameList.size() == 1) {
      fileNameArray = new String[1];
      fileNameArray[0] = (String) fileNameList.get(0);
    }
    else {
      fileNameArray = (String[]) fileNameList.toArray(new String[fileNameList.size()]);
    }
    return fileNameArray;
  }

  public void peetParser(ProcessSeries processSeries, final DialogType dialogType,
      final ProcessingMethod peetProcessingMethod) {
    if (processMgr.inUse(AxisID.ONLY, null, true)) {
      return;
    }
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (!savePeetDialog(true, true)) {
      return;
    }
    if (matlabParam == null) {
      uiHarness.openMessageDialog(this, "Must set " + PeetDialog.DIRECTORY_LABEL
          + " and " + PeetDialog.FN_OUTPUT_LABEL, "Entry Error");
      return;
    }
    PeetParserParam param = new PeetParserParam(this, matlabParam.getFile());
    param.setParameters(matlabParam);
    try {
      try {
        LogFile log = LogFile.getInstance(param.getLogFile());
        log.backup();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
      removeComFiles();
      String threadName = processMgr.peetParser(param, processSeries);
      processSeries.setNextProcess(ProcessName.PROCESSCHUNKS.toString(),
          ProcessName.PEET_PARSER, FileType.AVERAGED_VOLUMES, FileType.REFERENCE_VOLUMES,
          peetProcessingMethod);
      setThreadName(threadName, AxisID.ONLY);
      mainPanel.startProgressBar("Running " + ProcessName.PEET_PARSER, AxisID.ONLY,
          ProcessName.PEET_PARSER);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to run " + ProcessName.PEET_PARSER
          + ", SystemProcessException.\n" + e.getMessage(), "Process Error");
    }
  }

  boolean isPopupChunkWarnings() {
    return false;
  }

  public void averageAll(ProcessSeries processSeries, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (!savePeetDialog(true, true)) {
      return;
    }
    if (matlabParam == null) {
      uiHarness.openMessageDialog(this, "Must set " + PeetDialog.DIRECTORY_LABEL
          + " and " + PeetDialog.FN_OUTPUT_LABEL, "Entry Error");
      return;
    }
    AverageAllParam param = new AverageAllParam(this, matlabParam.getFile());
    peetDialog.getParameters(param);
    param.setParameters(matlabParam);
    try {
      try {
        LogFile log = LogFile.getInstance(AverageAllParam.getLogFile());
        log.backup();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
      String threadName = processMgr.averageAll(param, processSeries);
      setThreadName(threadName, AxisID.ONLY);
      mainPanel.startProgressBar("Running " + ProcessName.AVERAGE_ALL, AxisID.ONLY,
          ProcessName.AVERAGE_ALL);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to run " + ProcessName.AVERAGE_ALL
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

  public BaseState getBaseState() {
    return state;
  }

  void createMainPanel() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      mainPanel = new MainPeetPanel(this);
    }
  }

  public BaseProcessManager getProcessManager() {
    return processMgr;
  }

  Storable[] getStorables(final int offset) {
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
  boolean startNextProcess(final UIComponent uiComponent, final AxisID axisID,
      final ProcessSeries.Process process,
      final ProcessResultDisplay processResultDisplay, final ProcessSeries processSeries,
      final DialogType dialogType, final ProcessDisplay display) {
    if (super.startNextProcess(uiComponent, axisID, process, processResultDisplay,
        processSeries, dialogType, display)) {
      return true;
    }
    if (process.equals(ProcessName.PROCESSCHUNKS.toString())) {
      processchunks(processSeries, process.getOutputImageFileType(),
          process.getProcessingMethod(), dialogType);
      return true;
    }
    return false;
  }

  /**
   * Initialize metalabParamFile.  Dependent on metaData.
   */
  private void setMatlabParam(final boolean newFile) {
    if (!loadedParamFile || matlabParam != null || paramFile == null) {
      return;
    }
    matlabParam = loadMatlabParam(DatasetFiles.getMatlabParamFile(this), newFile);
  }

  public MatlabParam loadMatlabParam(final File matlabFile, final boolean newFile) {
    MatlabParam param = null;
    do {
      // Load the .prm file
      param = new MatlabParam(this, AXIS_ID, matlabFile, newFile);
      List<String> errorList = new ArrayList<String>();
      if (param.read(this, errorList, peetDialog)) {
        loadState = LoadState.SUCCESS;
      }
      else if (!errorList.isEmpty()) {
        // Handle error in the .prm file
        List<String> message = new ArrayList();
        message.add("Unable to successfully parse " + matlabFile.getAbsolutePath() + ".");
        message.add("");
        message
            .add("Fix the .prm file and press Yes to reload.  Or press No to exit from this dataset.");
        message.add("");
        message.add("Error(s):");
        errorList.addAll(0, message);
        if (UIHarness.INSTANCE.openYesNoDialog(this,
            errorList.toArray(new String[errorList.size()]), AxisID.ONLY)) {
          // The .prm file has been fixed - must reload.
          loadState = LoadState.RELOAD;
        }
        else {
          // Must exit from etomo without saving to the .prm file, if the .prm can't be
          // fixed.
          loadState = LoadState.EXIT;
          EtomoDirector.INSTANCE.closeCurrentManager(AxisID.ONLY, false);
        }
      }
    } while (loadState == LoadState.RELOAD);
    return param;
  }

  private void createState() {
  }

  /**
   * MUST run reconnect for all axis
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
    reconnect(axisProcessData.getSavedProcessData(AxisID.ONLY), AxisID.ONLY, true);
  }

  /**
   * Create (if necessary) and show the peet dialog.  Update data if the param
   * file has been set.
   */
  private void openPeetDialog(final PeetStartupData startupData) {
    if (!loadedParamFile && startupData == null) {
      UIHarness.INSTANCE
          .openMessageDialog(this,
              "Failed to load the parameter file, unable to continue.", "Failed",
              AxisID.ONLY);
      valid = false;
      return;
    }
    if (peetDialog == null) {
      peetDialog = PeetDialog.getInstance(this, AXIS_ID);
    }
    setPeetDialogParameters(null, true);
    mainPanel.showProcess(peetDialog.getComponent(), AXIS_ID);
    String actionMessage = Utilities.prepareDialogActionMessage(DialogType.PEET,
        AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  /**
   * Create (if necessary) and show the peet dialog.  Update data if the param
   * file has been set.
   */
  private void openPeetStartupDialog() {
    if (peetStartupDialog == null) {
      String actionMessage = Utilities.prepareDialogActionMessage(
          DialogType.PEET_STARTUP, AxisID.ONLY, null);
      mainPanel.setStaticProgressBar("Starting PEET interface", AXIS_ID);
      peetStartupDialog = PeetStartupDialog.getInstance(this, AXIS_ID);
      if (actionMessage != null) {
        System.err.println(actionMessage);
      }
    }
  }

  public void cancelStartup() {
    mainPanel.stopProgressBar(AXIS_ID, ProcessEndState.KILLED);
    EtomoDirector.INSTANCE.closeCurrentManager(AxisID.ONLY, false);
  }

  public void setStartupData(final PeetStartupData startupData) {
    peetStartupDialog = null;
    openPeetDialog(startupData);
    if (startupData.isCopyFrom()) {
      copyDataset(startupData);
      if (loadState == LoadState.EXIT) {
        return;
      }
    }
    else {
      setParamFile(startupData);
      if (loadState == LoadState.EXIT) {
        return;
      }
    }
    if (!loadedParamFile) {
      mainPanel.stopProgressBar(AXIS_ID, ProcessEndState.FAILED);
      UIHarness.INSTANCE.openMessageDialog(this,
          "Failed to load or create parameter file, unable to continue.", "Failed",
          AxisID.ONLY);
      valid = false;
      return;
    }
    mainPanel.stopProgressBar(AXIS_ID, ProcessEndState.DONE);
  }

  private void setPeetDialogParameters(final File importDir, final boolean metaDataLoaded) {
    if (paramFile != null && metaData.isValid()) {
      if (metaDataLoaded) {
        peetDialog.setParameters(metaData);
      }
      // Always load from matlabParam after loading the data file because some
      // values are in both places (maskModelPts) and the .prm values take
      // precedence over the .epe values.
      if (matlabParam != null) {
        peetDialog.setParameters(matlabParam, importDir);
      }
      peetDialog.setDirectory(propertyUserDir);
      peetDialog.checkIncorrectPaths();
      peetDialog.updateDisplay(loadedParamFile);
    }
  }

  private boolean savePeetDialog(final boolean forRun, final boolean doValidation) {
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
    saveStorables(AXIS_ID);
    if (!peetDialog.getParameters(matlabParam, forRun, doValidation))
      return false;
    if (loadState != LoadState.EXIT) {
      matlabParam.write(this);
    }
    return true;
  }

  /**
   * Run processchunks.
   * @param axisID
   */
  private void processchunks(final ProcessSeries processSeries,
      final FileType outputImageFileType, final ProcessingMethod processingMethod,
      final DialogType dialogType) {
    if (peetDialog == null) {
      return;
    }
    try {
      ProcesschunksParam param = new ProcesschunksParam(this, AxisID.ONLY,
          peetDialog.getFnOutput(true), outputImageFileType);
      ParallelPanel parallelPanel = getMainPanel().getParallelPanel(AxisID.ONLY);
      peetDialog.getParameters(param);
      if (!parallelPanel.getParameters(param, true)) {
        getMainPanel().stopProgressBar(AxisID.ONLY, ProcessEndState.FAILED);
        return;
      }
      // param should never be set to resume
      parallelPanel.getParallelProgressDisplay().resetResults();
      processchunks(AxisID.ONLY, param, null, processSeries, false, processingMethod,
          true, dialogType);
    }
    catch (FieldValidationFailedException e) {
      return;
    }
  }

  private static final class LoadState {
    private static final LoadState SUCCESS = new LoadState();
    private static final LoadState RELOAD = new LoadState();
    private static final LoadState EXIT = new LoadState();

    private LoadState() {
    }
  }
}