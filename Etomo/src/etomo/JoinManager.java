package etomo;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Vector;

import etomo.comscript.FinishjoinParam;
import etomo.comscript.FlipyzParam;
import etomo.comscript.MakejoincomParam;
import etomo.comscript.MidasParam;
import etomo.comscript.ProcessDetails;
import etomo.comscript.StartJoinParam;
import etomo.comscript.XfalignParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.process.JoinProcessManager;
import etomo.process.ProcessMessages;
import etomo.process.SystemProcessException;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseProcessTrack;
import etomo.type.BaseScreenState;
import etomo.type.BaseState;
import etomo.type.ConstJoinMetaData;
import etomo.type.EtomoNumber;
import etomo.type.JoinMetaData;
import etomo.type.JoinState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.SlicerAngles;
import etomo.ui.JoinDialog;
import etomo.ui.MainJoinPanel;
import etomo.ui.MainPanel;
import etomo.util.Utilities;

/**
 * <p>Description: </p>
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
 * 
 * <p> $Log$
 * <p> Revision 1.45  2006/10/16 22:35:47  sueh
 * <p> bug# 919 Doing makejoincom post processing in JoinManager.postProcess().
 * <p> call JoinDialog.setInverted().
 * <p>
 * <p> Revision 1.44  2006/09/13 23:07:14  sueh
 * <p> bug# 920 Moving BaseManager.createState() call to child classes, so it can be
 * <p> done after meta data is created.
 * <p>
 * <p> Revision 1.43  2006/07/19 20:05:29  sueh
 * <p> bug# 902 Added processSucceeded().
 * <p>
 * <p> Revision 1.42  2006/07/17 21:16:28  sueh
 * <p> bug# 900 Added imodSendEvent functionality back.  Uses the
 * <p> SystemProcessException.
 * <p>
 * <p> Revision 1.41  2006/06/22 20:58:46  sueh
 * <p> bug# 797 Catching io exception when opening 3dmods.
 * <p>
 * <p> Revision 1.40  2006/06/05 16:01:17  sueh
 * <p> bug# 766 getParamFileStorableArray():  Add the option have elements in the storable array that aer set by the base manager.
 * <p>
 * <p> Revision 1.39  2006/04/06 18:47:32  sueh
 * <p> bug# 808 Added newStartJoinParam and to manage StartJoinParam so
 * <p> that it can be modified by another process.
 * <p>
 * <p> Revision 1.38  2006/03/20 17:49:16  sueh
 * <p> bug# 835 Changed setTestParamFile to setParamFile
 * <p>
 * <p> Revision 1.37  2006/01/27 18:38:02  sueh
 * <p> bug# 801 Added validation for makejoin and finishjoin
 * <p>
 * <p> Revision 1.36  2006/01/20 20:44:41  sueh
 * <p> bug# 401 Added ProcessResultDisplay functionality to
 * <p> startNextProcess.
 * <p>
 * <p> Revision 1.35  2005/12/23 02:03:28  sueh
 * <p> bug# 675 Split the test option functionality.  Control headlessness with
 * <p> --headless.  This allow both JUnit and JfcUnit to use the special test
 * <p> functions.
 * <p>
 * <p> Revision 1.34  2005/12/16 18:25:31  sueh
 * <p> bug# 785 Setting JoinState.doneMode in doneJoinDialog.  In
 * <p> setMode(String) getting the doneMode and then clearing it.  If the
 * <p> doneMode is set to Changing Sample, set the mode to Sample Not
 * <p> Produced.  The Sample Not Produced mode cannot use the revert button
 * <p> and can't get into the Align and Join tabs.
 * <p>
 * <p> Revision 1.33  2005/12/14 01:27:01  sueh
 * <p> bug# 782 Added toString().
 * <p>
 * <p> Revision 1.32  2005/12/12 21:58:13  sueh
 * <p> bug# 779 Made BaseManager.resetNextProcess() private.
 * <p>
 * <p> Revision 1.31  2005/12/09 20:22:21  sueh
 * <p> bug# 776 Added canSnapshot.
 * <p>
 * <p> Revision 1.30  2005/11/29 22:19:54  sueh
 * <p> bug# 757 Added the manager to JoinMetaData.  Deleted
 * <p> saveTestParamOnExit(), which wasn't being used.
 * <p>
 * <p> Revision 1.29  2005/11/10 17:50:05  sueh
 * <p> Constructor should not be public
 * <p>
 * <p> Revision 1.28  2005/11/02 23:56:28  sueh
 * <p> Removed temporary print statement.
 * <p>
 * <p> Revision 1.27  2005/11/02 21:34:12  sueh
 * <p> bug# 754 Getting error and warning tags from ProcessMessages.
 * <p>
 * <p> Revision 1.26  2005/10/31 17:53:05  sueh
 * <p> bug# 730 Added getParamFile() and save().
 * <p>
 * <p> Revision 1.25  2005/10/18 22:10:15  sueh
 * <p> bug# 737 Setting nextProcess after running process, because the axis
 * <p> busy test is run when running process.
 * <p>
 * <p> Revision 1.24  2005/10/14 21:05:48  sueh
 * <p> bug# 730 Changed loadedTestParamFile to loadedParamFile.  Added
 * <p> doneJoinDialog(); calling it from exitProgram().
 * <p>
 * <p> Revision 1.23  2005/09/29 18:39:00  sueh
 * <p> bug# 532 Preventing Etomo from saving to the .edf or .ejf file over and
 * <p> over during exit.  Added BaseManager.exiting and
 * <p> saveIntermediateParamFile(), which will not save when exiting it true.
 * <p> Setting exiting to true in BaseManager.exitProgram().  Moved call to
 * <p> saveParamFile() to the child exitProgram functions so that the param file
 * <p> is saved after all the done functions are run.
 * <p>
 * <p> Revision 1.22  2005/09/27 21:14:55  sueh
 * <p> Added exitProgram to call mainPanel.done() and save the state of the
 * <p> parallel processing dialog.  Added getParamFileStorableArray(), which
 * <p> creates, fills, and returns storable array for the .edf file.  Removed
 * <p> getNumStorables().
 * <p>
 * <p> Revision 1.21  2005/08/22 16:02:23  sueh
 * <p> bug# 532 Added pause().
 * <p>
 * <p> Revision 1.20  2005/08/11 23:23:19  sueh
 * <p> bug# 711  Change join 3dmod buttons to Run3dmodButton.  These
 * <p> 3dmod configurations should not be binned in Z.
 * <p>
 * <p> Revision 1.19  2005/08/04 19:07:52  sueh
 * <p> bug# 532 added packDialogs() to request sizing functionality that is not
 * <p> performed by pack().
 * <p>
 * <p> Revision 1.18  2005/07/29 00:42:27  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.17  2005/06/21 00:41:13  sueh
 * <p> bug# 522 Added pass-through function call to
 * <p> BaseProcessManager.touch() for MRCHeaderTest.
 * <p>
 * <p> Revision 1.16  2005/06/01 21:25:16  sueh
 * <p> bug# 667 Removing the Controller classes.  Trying make meta data and
 * <p> app manager equals didn't work very well.  Meta data is created by and
 * <p> managed by app mgr and the class structure should reflect that.
 * <p>
 * <p> Revision 1.15  2005/05/17 19:09:22  sueh
 * <p> bug# 520 Setting the status bar when join is opened with an .ejf file.
 * <p>
 * <p> Revision 1.14  2005/04/26 17:36:01  sueh
 * <p> bug# 615 Change the name of the UIHarness member variable to
 * <p> uiHarness.
 * <p>
 * <p> Revision 1.13  2005/04/25 20:34:21  sueh
 * <p> bug# 615 Passing the axis where the command originated to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.  All Frame functions will use UIHarness as a
 * <p> public interface.
 * <p>
 * <p> Revision 1.12  2005/03/11 01:57:59  sueh
 * <p> bug# 612 Change nextProcess to support axis A and B.
 * <p>
 * <p> Revision 1.11  2005/01/25 21:21:03  sueh
 * <p> Changing setShift(ConstEtomoNumber, ConstEtomoNumber) to
 * <p> setShift(int, int).  This allows the removal of
 * <p> ConstEtomoNumber.getNegation(), which is too specialized a function.
 * <p>
 * <p> Revision 1.10  2005/01/21 22:17:10  sueh
 * <p> bug# 509 bug# 591  Moved the management of MetaData to the Controller
 * <p> class.
 * <p>
 * <p> Revision 1.9  2004/12/14 21:23:54  sueh
 * <p> bug# 565: Fixed bug:  Losing process track when backing up .edf file and
 * <p> only saving metadata.  bug# 572:  Removing state object from meta data
 * <p> and managing it with a manager object.
 * <p>
 * <p> Revision 1.8  2004/12/09 04:49:42  sueh
 * <p> bug# 565 Removed isDataParamDirty.  Automatically saving to param file on exit.
 * <p> Changed saveTestParamIfNecessary() to saveTestParamOnExit().
 * <p>
 * <p> Revision 1.7  2004/12/04 00:32:13  sueh
 * <p> bug# 570 Setting paramFile by calling endSetupMode() when running
 * <p> makejoincom.
 * <p>
 * <p> Revision 1.6  2004/11/24 18:10:12  sueh
 * <p> bug# 520 Added binning in XY.
 * <p>
 * <p> Revision 1.5  2004/11/24 00:58:08  sueh
 * <p> bug# 520 makejoincom: set nextProcess to "" when running process
 * <p> makejoincom() throws anexception.
 * <p>
 * <p> Revision 1.4  2004/11/23 22:29:03  sueh
 * <p> bug# 520 Fixing makejoincom - shouldn't do a set mode until the process
 * <p> is finished.  Putting startProgressBar() call last is process run functions
 * <p> and removing call to stopProgressBar() to prevent incorrect starting and
 * <p> stopping
 * <p>
 * <p> Revision 1.3  2004/11/20 01:55:44  sueh
 * <p> bug# 520 Added abortAddSection() to turn on Add Section button after
 * <p> killing flipyz process.
 * <p>
 * <p> Revision 1.2  2004/11/19 22:34:55  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.31  2004/11/19 00:00:01  sueh
 * <p> bug# 520 Overrode saveTestParamIfNecessary to ask to save the screen
 * <p> if fields have changed.  Added updateMetaDataFromJoinDialog to call
 * <p> JoinDialog.getMetaData(), change for a valid meta data and paramFile,
 * <p> save the meta data to the paramFile.
 * <p>
 * <p> Revision 1.1.2.30  2004/11/17 02:20:47  sueh
 * <p> bug# 520 Added endSetupMode() to do things that need to be done once,
 * <p> such as set paramFile, place the param file name on the status bar, and
 * <p> set the meta data in imod manager.  If paramFile is already set, it will only
 * <p> call setMode().
 * <p>
 * <p> Revision 1.1.2.29  2004/11/16 23:26:24  sueh
 * <p> bug# 520 JoinDialog mode names should end with _MODE.
 * <p>
 * <p> Revision 1.1.2.28  2004/11/16 02:20:03  sueh
 * <p> bug# 520 Replacing EtomoInteger, EtomoDouble, EtomoFloat, and
 * <p> EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.27  2004/11/15 22:06:55  sueh
 * <p> bug# 520  Change endSetupMode() to setMode() and change it to handle
 * <p> JoinDialog.SAMPLE_PRODUCED and JoinDialog.SAMPLE_PRODUCED.
 * <p> Remove doneJoinDialog() because it is not being used.
 * <p>
 * <p> Revision 1.1.2.26  2004/11/12 22:44:33  sueh
 * <p> bug# 520 Consolidated the imodOpen functions by passing an
 * <p> ImodManager key.
 * <p>
 * <p> Revision 1.1.2.25  2004/11/11 01:34:23  sueh
 * <p> bug# 520 Adding binning to open 3dmod functions.
 * <p>
 * <p> Revision 1.1.2.24  2004/11/08 22:08:04  sueh
 * <p> bug# 520 consolidate functionality that calls finishjoin into runFinishjoin().
 * <p> Add functions to set shift and size.
 * <p>
 * <p> Revision 1.1.2.23  2004/10/29 22:07:25  sueh
 * <p> bug# 520 Don't run createEmptyXfFile when the .ejf file isn't loaded.  When
 * <p> makejoincom is run, run createEmptyXfFile.
 * <p>
 * <p> Revision 1.1.2.22  2004/10/29 01:16:24  sueh
 * <p> bug# 520 Removing unecessary functions that provided services to
 * <p> BaseManager.  BaseManager can use get... functions to get the
 * <p> mainPanel, metaData, and processTrack.  Setting workingDirectory in
 * <p> JoinDialog from propertyUserDir, when paramFile is loaded.
 * <p>
 * <p> Revision 1.1.2.21  2004/10/28 16:51:46  sueh
 * <p> bug# 520 Copying most recent .xf file to rootName.xf before running
 * <p> finishjoin, midasSample, and refine xfalign.  Added copyXfFile,
 * <p> createEmptyXfFile, and copyMostRecentXfFile.
 * <p>
 * <p> Revision 1.1.2.20  2004/10/21 17:50:48  sueh
 * <p> bug# 520 In endSetupMode() set paramFile and status bar.
 * <p>
 * <p> Revision 1.1.2.19  2004/10/21 02:31:51  sueh
 * <p> bug# 520 Added enableMidas, finishJoin, revertXfalign, xfalignInitial,
 * <p> xfalignRefine.
 * <p>
 * <p> Revision 1.1.2.18  2004/10/18 19:05:09  sueh
 * <p> bug# 520 Added midasSample().  Added validation to startjoin.
 * <p>
 * <p> Revision 1.1.2.17  2004/10/18 17:28:59  sueh
 * <p> bug# 520 Added setSetupMode(), which tests for a valid meta data object
 * <p> and then enables the other tabs and disables rootName.  This is done
 * <p> when Make Sample is pressed and when a new join manager is created.
 * <p> Added xfalign.
 * <p>
 * <p> Revision 1.1.2.16  2004/10/15 00:14:00  sueh
 * <p> bug# 520 Initializing ui parameters on JoinManager construction.  Setting
 * <p> metaData in JoinDialog.
 * <p>
 * <p> Revision 1.1.2.15  2004/10/14 17:09:30  sueh
 * <p> bug# 520 Added imodOpenJoinSampleAverages.
 * <p>
 * <p> Revision 1.1.2.14  2004/10/14 03:24:33  sueh
 * <p> bug# 520 Added open join samples in Imod.  Using the Make Samples as
 * <p> a signal somewhat like exiting Setup successfully.  In this case I only
 * <p> need to call imodManager.setMetaData and enable the other join tabs.
 * <p>
 * <p> Revision 1.1.2.13  2004/10/14 02:26:38  sueh
 * <p> bug# 520 Added setWorkingDir() to set the propertyUserDir.
 * <p>
 * <p> Revision 1.1.2.12  2004/10/11 01:59:23  sueh
 * <p> bug# 520 moved responsibility for mainPanel, metaData, processTrack,
 * <p> and progressManager to child classes.  Used abstract functions to use
 * <p> these variables in the base classes.  This is more reliable and doesn't
 * <p> require casting.
 * <p>
 * <p> Revision 1.1.2.11  2004/10/08 15:44:52  sueh
 * <p> bug# 520 Fixed Make Samples functionality.  Used startNextProcess to
 * <p> call startjoin.com.  Used BackgroundProcess to call makejoincom
 * <p>
 * <p> Revision 1.1.2.10  2004/10/06 01:26:11  sueh
 * <p> bug# 520 Changed Make Join button to Make Samples.  Added flip().
 * <p>
 * <p> Revision 1.1.2.9  2004/10/01 21:00:44  sueh
 * <p> bug# 520 Added getMetaData()
 * <p>
 * <p> Revision 1.1.2.8  2004/09/29 17:42:26  sueh
 * <p> bug# 520 Casting mainPanel and other members from BaseManager to
 * <p> private local variables in the create functions.  Removed
 * <p> openNewDataset() and openExistingDataset().  This functionality is
 * <p> handled in EtomoDirector.  Added startJoin().  Added setTestParamFile()
 * <p> implementation.
 * <p>
 * <p> Revision 1.1.2.7  2004/09/22 22:03:53  sueh
 * <p> bug# 520 Made the imod functions more general by passing in the
 * <p> ImodManager key.  Added imodGetSlicerAngles.
 * <p>
 * <p> Revision 1.1.2.6  2004/09/21 17:43:41  sueh
 * <p> bug# 520 add imodTomogram and imodRemoveTomogram
 * <p>
 * <p> Revision 1.1.2.5  2004/09/15 22:34:34  sueh
 * <p> bug# 520 casting  base manager when necessary.  Added JoinDialog
 * <p>
 * <p> Revision 1.1.2.4  2004/09/13 16:41:18  sueh
 * <p> bug# 520 added isNewManager stub function
 * <p>
 * <p> Revision 1.1.2.3  2004/09/08 19:28:25  sueh
 * <p> bug# 520 update call to BaseMAnager()
 * <p>
 * <p> Revision 1.1.2.2  2004/09/07 17:55:15  sueh
 * <p> bug# 520 moved mainFrame show responsibility to EtomoDirector
 * <p>
 * <p> Revision 1.1.2.1  2004/09/03 21:03:27  sueh
 * <p> bug# 520 adding place holders for create functions for now
 * <p> </p>
 */
public final class JoinManager extends BaseManager {
  public static final String rcsid = "$Id$";

  //  Process dialog references
  private JoinDialog joinDialog = null;

  //variables cast from base class variables
  //initialized in create function
  private MainJoinPanel mainPanel;
  private final JoinMetaData metaData;
  private JoinProcessManager processMgr;
  private JoinState state;
  private StartJoinParam startJoinParam = null;

  JoinManager(String paramFileName, AxisID axisID) {
    super();
    this.metaData = new JoinMetaData(this);
    createState();
    initializeUIParameters(paramFileName, axisID);
    if (!paramFileName.equals("") && loadedParamFile) {
      mainPanel.setStatusBarText(paramFile, metaData);
    }
    if (!EtomoDirector.getInstance().isHeadless()) {
      openJoinDialog();
      setMode();
    }
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "joinDialog=" + joinDialog + ",metaData=" + metaData
        + ",\nprocessMgr=" + processMgr + ",state=" + state + ",\nsuper["
        + super.paramString() + "]";
  }

  public boolean isNewManager() {
    return true;
  }

  protected void createComScriptManager() {
  }

  protected void createProcessManager() {
    processMgr = new JoinProcessManager(this);
  }

  protected void createProcessTrack() {
  }
  
  protected void processSucceeded(AxisID axisID, ProcessName processName) {
  }

  /**
   * Open the join dialog
   */
  public void openJoinDialog() {
    openProcessingPanel();
    if (joinDialog == null) {
      if (loadedParamFile) {
        joinDialog = new JoinDialog(this, propertyUserDir, metaData);
      }
      else {
        joinDialog = new JoinDialog(this, metaData);
      }
    }
    if (loadedParamFile) {
      createEmptyXfFile(metaData.getRootName());
    }
    mainPanel.showProcess(joinDialog.getContainer(), AxisID.ONLY);
  }

  /**
   * Return the test parameter file as a File object
   * @return a File object specifying the data set parameter file.
   */
  public File getParamFile() {
    if (paramFile == null) {
      doneJoinDialog();
    }
    return paramFile;
  }

  private void doneJoinDialog() {
    if (joinDialog == null) {
      return;
    }
    String workingDir = joinDialog.getWorkingDirName();
    if (!loadedParamFile && workingDir != null && !workingDir.matches("\\s*+")) {
      propertyUserDir = workingDir;
    }
    String rootName = joinDialog.getRootName();
    if (!loadedParamFile && rootName != null && !rootName.matches("\\s*+")) {
      paramFile = new File(propertyUserDir, rootName
          + metaData.getFileExtension());
      loadedParamFile = true;
    }
    joinDialog.getMetaData(metaData);
    state.setDoneMode(joinDialog.getMode());
  }

  protected void createMainPanel() {
    mainPanel = new MainJoinPanel(this);
  }

  /**
   * Open 3dmod with binning 
   */
  public void imodOpen(String imodKey, int binning,
      Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.setBinningXY(imodKey, binning);
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
      uiHarness.openMessageDialog(e.getMessage(),
          "IO Exception", AxisID.ONLY);
    }
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
      uiHarness.openMessageDialog(e.getMessage(),
          "IO Exception", AxisID.ONLY);
    }
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
      imodManager.open(imodKey, imodIndex, file, menuOptions);
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
      uiHarness.openMessageDialog(e.getMessage(),
          "IO Exception", AxisID.ONLY);
    }
    return imodIndex;
  }

  /**
   * Remove a specific 3dmod
   * @param imodKey
   * @param imodIndex
   */
  public void imodRemove(String imodKey, int imodIndex) {
    if (imodIndex == -1) {
      return;
    }
    try {
      imodManager.delete(imodKey, imodIndex);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(),
          "IO Exception", AxisID.ONLY);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "System Process Exception", AxisID.ONLY);
    }
  }

  public SlicerAngles imodGetSlicerAngles(String imodKey, int imodIndex) {
    Vector results = null;
    try {
      if (imodIndex == -1) {
        uiHarness.openMessageDialog("The is no open " + imodKey
            + " 3dmod for the highlighted row.", "No 3dmod", AxisID.ONLY);
      }
      results = imodManager.getSlicerAngles(imodKey, imodIndex);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(),
          "IO Exception", AxisID.ONLY);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(e.getMessage(), "System Process Exception", AxisID.ONLY);
    }
    Vector messageArray = new Vector();
    SlicerAngles slicerAngles = null;
    if (results == null) {
      messageArray.add("Unable to retrieve slicer angles.");
      messageArray.add("The " + imodKey + " may not be open in 3dmod.");
    }
    else {
      slicerAngles = new SlicerAngles();
      boolean foundResultLine1 = false;
      boolean foundResult = false;
      String result = null;
      for (int i = 0; i < results.size(); i++) {
        result = (String) results.get(i);
        if (result.indexOf(ProcessMessages.ERROR_TAG) != -1
            || result.indexOf(ProcessMessages.WARNING_TAG) != -1) {
          messageArray.add(result);
        }
        else if (!foundResultLine1 && !foundResult
            && result.equals(ImodProcess.SLICER_ANGLES_RESULTS_STRING1)) {
          foundResultLine1 = true;
        }
        else if (foundResultLine1 && !foundResult
            && result.equals(ImodProcess.SLICER_ANGLES_RESULTS_STRING2)) {
          foundResult = true;
        }
        else if (foundResult && !slicerAngles.isComplete()) {
          try {
            slicerAngles.add(result);
          }
          catch (NumberFormatException e) {
            messageArray.add(result);
          }
        }
        else {
          messageArray.add(result);
        }
      }
      if (!slicerAngles.isComplete()) {
        messageArray.add("Unable to retrieve slicer angles from " + imodKey
            + " 3dmod.");
        if (!slicerAngles.isEmpty()) {
          messageArray.add("slicerAngles=" + slicerAngles);
        }
      }
    }
    if (messageArray.size() > 0) {
      String[] messages = (String[]) messageArray
          .toArray(new String[messageArray.size()]);
      uiHarness.openMessageDialog(messages, "Slicer Angles", AxisID.ONLY);
    }
    return slicerAngles;
  }

  public void makejoincom() {
    if (!joinDialog.getMetaData(metaData)) {
      return;
    }
    if (!metaData.isValid(joinDialog.getWorkingDirName())) {
      uiHarness.openMessageDialog(metaData.getInvalidReason(), "Invalid Data",
          AxisID.ONLY);
      return;
    }
    if (!joinDialog.validateMakejoincom()) {
      return;
    }
    String rootName = metaData.getRootName();
    EtomoDirector.getInstance().renameCurrentManager(rootName);
    createEmptyXfFile(rootName);
    MakejoincomParam makejoincomParam = new MakejoincomParam(metaData, state, this);
    if (paramFile == null) {
      endSetupMode();
    }
    try {
      threadNameA = processMgr.makejoincom(makejoincomParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run makejoincom\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
    setNextProcess(AxisID.ONLY, "startjoin");
    mainPanel.startProgressBar("Makejoincom", AxisID.ONLY,ProcessName.MAKEJOINCOM);
  }
  
  /**
   * post processing after a successful process
   * @param axisID
   * @param processName
   * @param processDetails
   */
  public void postProcess(String commandName,
      ProcessDetails processDetails) {
    if (processDetails != null) {
    if (processDetails.getBooleanValue(MakejoincomParam.Fields.ROTATE)) {
      StartJoinParam param = newStartJoinParam();
      param.setRotate(true);
      param.setTotalRows(processDetails
          .getIntValue(MakejoincomParam.Fields.TOTAL_ROWS));
      param.setRotationAnglesList(processDetails
          .getHashtable(MakejoincomParam.Fields.ROTATION_ANGLES_LIST));
    }
    }
    joinDialog.setInverted();
  }

  /**
   * if paramFile is not set, attempts to end setup mode and set the param file name.
   * @return
   */
  public boolean endSetupMode() {
    if (paramFile != null) {
      return setMode();
    }
    String workingDirName = joinDialog.getWorkingDirName();
    if (!setMode(workingDirName)) {
      return false;
    }
    propertyUserDir = workingDirName;
    imodManager.setMetaData(metaData);
    paramFile = new File(propertyUserDir, metaData.getRootName()
        + metaData.getFileExtension());
    loadedParamFile = true;
    mainPanel.setStatusBarText(paramFile, metaData);
    return true;
  }

  public boolean canChangeParamFileName() {
    return false;
  }

  /**
   * Run midas on the sample
   */
  public void midasSample() {
    if (!updateMetaDataFromJoinDialog(AxisID.ONLY)) {
      return;
    }
    MidasParam midasParam = new MidasParam(this, AxisID.ONLY);
    if (!copyMostRecentXfFile(JoinDialog.MIDAS_TEXT)) {
      return;
    }
    try {
      processMgr.midasSample(midasParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run" + JoinDialog.MIDAS_TEXT + "\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
  }

  public void xfalignInitial() {
    if (!updateMetaDataFromJoinDialog(AxisID.ONLY)) {
      return;
    }
    XfalignParam xfalignParam = new XfalignParam(this,
        XfalignParam.INITIAL_MODE);
    try {
      threadNameA = processMgr.xfalign(xfalignParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run initial xfalign\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      joinDialog.enableMidas();
      return;
    }
    mainPanel.startProgressBar("Initial xfalign", AxisID.ONLY,ProcessName.XFALIGN);
  }

  public void xfalignRefine() {
    if (!updateMetaDataFromJoinDialog(AxisID.ONLY)) {
      return;
    }
    XfalignParam xfalignParam = new XfalignParam(this, XfalignParam.REFINE_MODE);
    if (!copyMostRecentXfFile(JoinDialog.REFINE_AUTO_ALIGNMENT_TEXT)) {
      return;
    }
    try {
      threadNameA = processMgr.xfalign(xfalignParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run "
          + JoinDialog.REFINE_AUTO_ALIGNMENT_TEXT + "\n" + except.getMessage(),
          "SystemProcessException", AxisID.ONLY);
      joinDialog.enableMidas();
      return;
    }
    mainPanel.startProgressBar("Refine xfalign", AxisID.ONLY,ProcessName.XFALIGN);
  }

  private boolean copyMostRecentXfFile(String commandDescription) {
    String rootName = metaData.getRootName();
    String xfFileName = rootName + ".xf";
    File newXfFile = Utilities.mostRecentFile(propertyUserDir, xfFileName,
        rootName + MidasParam.getOutputFileExtension(), rootName
            + XfalignParam.getOutputFileExtension(), rootName + "_empty.xf");
    //If the most recent .xf file is not root.xf, copy it to root.xf
    if (!newXfFile.getName().equals(xfFileName)) {
      File xfFile = new File(propertyUserDir, xfFileName);
      try {
        Utilities.copyFile(newXfFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
        String[] message = {
            "Unable to copy " + newXfFile.getAbsolutePath() + " to "
                + xfFileName + ".",
            "Copy " + newXfFile.getName() + " to " + xfFileName,
            " and then rerun " + commandDescription + "." };
        uiHarness.openMessageDialog(message,
            "Cannot run " + commandDescription, AxisID.ONLY);
        return false;
      }
    }
    return true;
  }

  public void copyXfFile(File xfOutputFile) {
    File xfFile = new File(propertyUserDir, metaData.getRootName() + ".xf");
    if (xfOutputFile != null && xfOutputFile.exists()) {
      try {
        Utilities.copyFile(xfOutputFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
        String[] message = {
            "Unable to copy " + xfOutputFile.getAbsolutePath() + " to "
                + xfFile.getName() + ".",
            "Copy " + xfOutputFile.getName() + " to " + xfFile.getName() + "." };
        uiHarness.openMessageDialog(message, "Cannot Copy File", AxisID.ONLY);
      }
    }
  }

  public void createEmptyXfFile(String rootName) {
    File emptyXfFile = new File(propertyUserDir, rootName + "_empty.xf");
    if (!emptyXfFile.exists()) {
      String emptyLine = "   1.0000000   0.0000000   0.0000000   1.0000000       0.000       0.000";
      try {
        BufferedWriter bufferedWriter = new BufferedWriter(new FileWriter(
            emptyXfFile));
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.write(emptyLine);
        bufferedWriter.newLine();
        bufferedWriter.close();
      }
      catch (IOException e) {
        e.printStackTrace();
        return;
      }
    }
    File xfFile = new File(propertyUserDir, rootName + ".xf");
    if (!xfFile.exists()) {
      try {
        Utilities.copyFile(emptyXfFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
      }
    }
  }

  /**
   * sets the mode in joinDialog based on whether the working directory and
   * root name are entered and whether a sample is saved
   * @param workingDirName
   * @return
   */
  public boolean setMode(String workingDirName) {
    //get a non-shared copy of doneMode
    int doneMode = state.getDoneMode();
    //only check done mode when we first set the mode
    state.clearDoneMode();
    if (!metaData.isValid(workingDirName)) {
      joinDialog.setMode(JoinDialog.SETUP_MODE);
      return false;
    }
    if (!state.isSampleProduced()
        || doneMode == JoinDialog.CHANGING_SAMPLE_MODE) {
      //either the sample was not produced, or the user had been changing the
      //sample when they exited the join dialog.  If the done mode is
      //CHANGING_SAMPLE_MODE, then the sample values are not valid and the
      //original sample values have been lost.
      joinDialog.setMode(JoinDialog.SAMPLE_NOT_PRODUCED_MODE);
    }
    else {
      joinDialog.setMode(JoinDialog.SAMPLE_PRODUCED_MODE);
    }
    return true;
  }

  public boolean setMode() {
    return setMode(propertyUserDir);
  }

  public void startjoin() {
    if (!metaData.isValid(joinDialog.getWorkingDir())) {
      uiHarness.openMessageDialog(metaData.getInvalidReason(), "Invalid Data",
          AxisID.ONLY);
      return;
    }
    try {
      if (startJoinParam == null) {
        startJoinParam = new StartJoinParam(AxisID.ONLY);
      }
      threadNameA = processMgr.startjoin(startJoinParam);
      startJoinParam = null;
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run startjoin.com\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar("Startjoin", AxisID.ONLY,ProcessName.STARTJOIN);
  }

  public void revertXfFileToMidas() {
    File midasOutputFile = new File(propertyUserDir, metaData.getRootName()
        + MidasParam.getOutputFileExtension());
    touch(midasOutputFile.getAbsolutePath());
    copyXfFile(midasOutputFile);
  }

  public void touch(String absolutePath) {
    processMgr.touch(absolutePath);
  }

  public void revertXfFileToEmpty() {
    File emptyFile = new File(propertyUserDir, metaData.getRootName()
        + "_empty.xf");
    touch(emptyFile.getAbsolutePath());
    copyXfFile(emptyFile);
  }

  public void enableMidas() {
    joinDialog.enableMidas();
  }

  public void runFinishjoin(int mode, String buttonText) {
    if (!updateMetaDataFromJoinDialog(AxisID.ONLY)) {
      return;
    }
    FinishjoinParam finishjoinParam = new FinishjoinParam(this, mode);
    if (!copyMostRecentXfFile(buttonText)) {
      return;
    }
    if (!joinDialog.validateFinishjoin()) {
      return;
    }
    try {
      threadNameA = processMgr.finishjoin(finishjoinParam);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run " + buttonText + "\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar("Finishjoin: " + buttonText, AxisID.ONLY,ProcessName.FINISHJOIN);
  }

  private boolean updateMetaDataFromJoinDialog(AxisID axisID) {
    if (!joinDialog.getMetaData(metaData)) {
      return false;
    }
    if (!metaData.isValid(propertyUserDir)) {
      uiHarness.openMessageDialog(metaData.getInvalidReason(), "Invalid Data",
          axisID);
      return false;
    }
    saveIntermediateParamFile(axisID);
    return true;
  }

  public void setSize(String sizeInXString, String sizeInYString) {
    EtomoNumber sizeInX = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    EtomoNumber sizeInY = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    sizeInX.set(sizeInXString);
    joinDialog.setSizeInX(sizeInX);
    sizeInY.set(sizeInYString);
    joinDialog.setSizeInY(sizeInY);
  }

  public void setShift(int shiftInX, int shiftInY) {
    joinDialog.setShiftInX(shiftInX);
    joinDialog.setShiftInY(shiftInY);
  }

  public void flip(File tomogram, File workingDir) {
    FlipyzParam flipyzParam = new FlipyzParam(tomogram, workingDir);
    try {
      threadNameA = processMgr.flipyz(flipyzParam);
    }
    catch (SystemProcessException except) {
      joinDialog.abortAddSection();
      except.printStackTrace();
      uiHarness.openMessageDialog("Can't run clip flipyz\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar("Flipping " + tomogram.getName(), AxisID.ONLY);
  }

  public void abortAddSection() {
    joinDialog.abortAddSection();
  }

  public void addSection(File tomogram) {
    joinDialog.addSection(tomogram);
  }

  /**
   * Open the main window in processing mode
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
  }

  /**
   * Set the data set parameter file. This also updates the mainframe data
   * parameters.
   * @param paramFile a File object specifying the data set parameter file.
   */
  public void setParamFile(File paramFile) {
    this.paramFile = paramFile;
    //  Update main window information and status bar
    mainPanel.setStatusBarText(paramFile, metaData);
  }

  protected void updateDialog(ProcessName processName, AxisID axisID) {
  }

  public ConstJoinMetaData getConstMetaData() {
    return (ConstJoinMetaData) metaData;
  }

  public JoinMetaData getJoinMetaData() {
    return metaData;
  }

  /**
   * Start the next process specified by the nextProcess string
   */
  protected void startNextProcess(AxisID axisID, String nextProcess,
      ProcessResultDisplay processResultDisplay) {
    if (nextProcess.equals("startjoin")) {
      startjoin();
      return;
    }
  }

  public BaseMetaData getBaseMetaData() {
    return (BaseMetaData) metaData;
  }

  protected void setMetaData(ImodManager imodManager) {
    imodManager.setMetaData(metaData);
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  protected BaseProcessTrack getProcessTrack() {
    return null;
  }

  protected void getProcessTrack(Storable[] storable, int index) {
  }

  protected void createState() {
    state = new JoinState(this);
  }

  public JoinState getState() {
    return state;
  }
  
  public StartJoinParam newStartJoinParam() {
    startJoinParam = new StartJoinParam(AxisID.ONLY);
    return startJoinParam;
  }

  protected BaseState getBaseState() {
    return state;
  }

  public final BaseScreenState getBaseScreenState(AxisID axisID) {
    return null;
  }

  /**
   * Interrupt the currently running thread for this axis
   * 
   * @param axisID
   */
  public void kill(AxisID axisID) {
    processMgr.kill(axisID);
  }

  /**
   * Interrupt the currently running thread for this axis
   * 
   * @param axisID
   */
  public void pause(AxisID axisID) {
    throw new IllegalStateException("pause is not available in join");
  }

  public final void packDialogs(AxisID axisID) {
  }

  public final void packDialogs() {
  }

  protected BaseProcessManager getProcessManager() {
    return processMgr;
  }

  protected final Storable[] getParamFileStorableArray(boolean includeMetaData, int baseElements) {
    int arraySize = 2 + baseElements;
    if (!includeMetaData) {
      arraySize--;
    }
    Storable[] storable = new Storable[arraySize];
    int index = baseElements;
    if (includeMetaData) {
      storable[index++] = metaData;
    }
    storable[index] = state;
    return storable;
  }

  public boolean exitProgram(AxisID axisID) {
    try {
      if (super.exitProgram(axisID)) {
        doneJoinDialog();
        mainPanel.done();
        saveParamFile(axisID);
        return true;
      }
      return false;
    }
    catch (Throwable e) {
      e.printStackTrace();
      return true;
    }
  }

  public boolean save(AxisID axisID) {
    doneJoinDialog();
    mainPanel.done();
    return saveParamFile(axisID);
  }

  public final boolean canSnapshot() {
    return false;
  }
  
  public String getName() {
    return metaData.getName();
  }
}