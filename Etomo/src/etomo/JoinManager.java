package etomo;

import java.awt.Component;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import etomo.comscript.ClipParam;
import etomo.comscript.FinishjoinParam;
import etomo.comscript.MakejoincomParam;
import etomo.comscript.MidasParam;
import etomo.comscript.ProcessDetails;
import etomo.comscript.RemapmodelParam;
import etomo.comscript.StartJoinParam;
import etomo.comscript.XfalignParam;
import etomo.comscript.XfjointomoParam;
import etomo.comscript.XfmodelParam;
import etomo.comscript.XftoxgParam;
import etomo.process.BaseProcessManager;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.process.JoinProcessManager;
import etomo.process.ProcessMessages;
import etomo.process.SystemProcessException;
import etomo.storage.LogFile;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AutoAlignmentMetaData;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.BaseState;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstJoinMetaData;
import etomo.type.ConstJoinState;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.InterfaceType;
import etomo.type.JoinMetaData;
import etomo.type.JoinScreenState;
import etomo.type.JoinState;
import etomo.type.ProcessName;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.SlicerAngles;
import etomo.ui.UIComponent;
import etomo.ui.swing.Deferred3dmodButton;
import etomo.ui.swing.JoinDialog;
import etomo.ui.swing.MainJoinPanel;
import etomo.ui.swing.MainPanel;
import etomo.ui.swing.ProcessDisplay;
import etomo.util.DatasetFiles;
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
 * <p> Revision 1.90  2011/02/21 21:06:14  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.89  2011/02/03 05:52:30  sueh
 * <p> bug# 1422 Using ProcessingMethod to keep track of which type of
 * <p> processing method is in use.  The decisions about when to display the
 * <p> parallel processing table have been centralized in
 * <p> ProcessingMethodMediator.
 * <p>
 * <p> Revision 1.88  2010/11/13 16:02:54  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.87  2010/09/09 17:06:15  sueh
 * <p> bug# 1402 Reformatted.
 * <p>
 * <p> Revision 1.86  2010/04/28 15:36:33  sueh
 * <p> bug# 1344 Added getFileSubdirectoryName.  Passing params to process
 * <p> manager functions, standardizing "3dmod is open" messages to always
 * <p> use closeImod.  Using ProcessSeries.Process to hold process information.
 * <p>
 * <p> Revision 1.85  2010/02/26 20:37:31  sueh
 * <p> Changing the complex popup titles are making it hard to complete the
 * <p> uitests.
 * <p>
 * <p> Revision 1.84  2010/02/17 04:41:12  sueh
 * <p> bug# 1301 Moved comScriptMgr and logPanel to child class.
 * <p>
 * <p> Revision 1.83  2009/12/11 17:24:10  sueh
 * <p> bug# 1291 Added the manager, the axis, and the mode to the ClipParam
 * <p> constructor.
 * <p>
 * <p> Revision 1.82  2009/10/27 20:38:56  sueh
 * <p> bug# 1275 Moving the resposibility for creating the log panel to the child
 * <p> classes.  That way the Front Page manager doesn't have to have a log
 * <p> panel.  Handling a null process manager.
 * <p>
 * <p> Revision 1.81  2009/10/23 22:22:15  sueh
 * <p> bug# 1275 Made touch() a start function in BaseProcessManager.
 * <p>
 * <p> Revision 1.80  2009/09/01 03:17:35  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.79  2009/06/11 16:45:53  sueh
 * <p> bug# 1221 Sending the process panel to the process function in the
 * <p> manager wrapped in a ProcessDisplay interface.  Changed
 * <p> startNextProcess.
 * <p>
 * <p> Revision 1.78  2009/04/14 23:01:18  sueh
 * <p> bug# 1207  In reconnect:  handling some situations where process data is not running.
 * <p>
 * <p> Revision 1.77  2009/04/13 22:21:53  sueh
 * <p> bug# 1207 Implemented doAutomation in BaseManager.
 * <p>
 * <p> Revision 1.76  2009/04/01 19:55:07  sueh
 * <p> bug# 1208 Replaced flip with rotx.
 * <p>
 * <p> Revision 1.75  2009/03/17 00:23:34  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.74  2009/03/02 18:54:22  sueh
 * <p> bug# 1193 Must do a reconnect in openProcessingPanel.
 * <p>
 * <p> Revision 1.73  2009/02/04 22:47:09  sueh
 * <p> bug# 1158 passing logPanel to mainPanel.setStatusBarText so its title can
 * <p> be updated.  Fixed endSetupMode, which was invalidating metadata by
 * <p> using initializeUIParameters to load a non-existant .ejf file.
 * <p>
 * <p> Revision 1.72  2008/11/20 01:25:19  sueh
 * <p> bug# 1147 Moved imodOpen(String,String,Run3dmodMenuOptions) to
 * <p> BaseManager.
 * <p>
 * <p> Revision 1.71  2008/09/30 20:42:50  sueh
 * <p> bug# 1113 Added getFocusComponent.
 * <p>
 * <p> Revision 1.70  2008/05/28 02:47:44  sueh
 * <p> bug# 1111 Removed processDialogTypeA and B from BaseManager.
 * <p> The dialogType for processes should be handled by ProcessSeries.
 * <p> Passing a DialogType parameter to startNextProcess.
 * <p>
 * <p> Revision 1.69  2008/05/13 20:58:53  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 1.68  2008/05/06 23:54:55  sueh
 * <p> bug#847 Running deferred 3dmods by using the button that usually calls
 * <p> them.  This avoids having to duplicate the calls and having a
 * <p> startNextProcess function just for 3dmods.  This requires that the 3dmod
 * <p> button be passed to the function that starts the process.
 * <p>
 * <p> Revision 1.67  2008/05/03 00:33:32  sueh
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
 * <p> Revision 1.66  2008/01/31 20:16:34  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.65  2008/01/14 20:21:49  sueh
 * <p> bug# 1050 Added an empty getProcessResultDisplayFactoryInterface.
 * <p>
 * <p> Revision 1.64  2007/12/26 21:57:12  sueh
 * <p> bug# 1052 Moved argument handling from EtomoDirector to a separate class.
 * <p>
 * <p> Revision 1.63  2007/12/10 21:49:45  sueh
 * <p> bug# 1041 Standardized JoinMetaData.getRootName to getDatasetName.
 * <p>
 * <p> Revision 1.62  2007/10/09 19:17:27  sueh
 * <p> bug# 1046
 * <p>
 * <p> Revision 1.61  2007/09/07 00:15:52  sueh
 * <p> bug# 989 Using a public INSTANCE for EtomoDirector instead of getInstance
 * <p> and createInstance.
 * <p>
 * <p> Revision 1.60  2007/08/29 21:22:38  sueh
 * <p> bug# 1041 Made getBaseState public.
 * <p>
 * <p> Revision 1.59  2007/07/30 22:38:40  sueh
 * <p> bug# 963 Using initializeUIParameters to to set param file.  In endSetupMode,
 * <p> loading meta data twice because its being reset by calling
 * <p> initializeUIParameters.  Added saveParamFile().
 * <p>
 * <p> Revision 1.58  2007/07/30 18:33:25  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.57  2007/06/08 21:50:27  sueh
 * <p> bug# 1014 Removed setMetaData(ImodManager) and placing the call to ImodManager.setMetaData after the call to initializeUIParameters.
 * <p>
 * <p> Revision 1.56  2007/05/21 22:28:10  sueh
 * <p> bug# 964 Added getInterfaceType().
 * <p>
 * <p> Revision 1.55  2007/05/11 14:31:36  sueh
 * <p> bug# 964 Changed ImodManager.TRANSFORMED_MODEL to
 * <p> TRANSFORMED_MODEL_KEY.
 * <p>
 * <p> Revision 1.54  2007/04/26 02:42:08  sueh
 * <p> bug# 964 Formatted
 * <p>
 * <p> Revision 1.53  2007/04/09 19:30:06  sueh
 * <p> bug# 964 Added setParamFile(), which just returns loadedParamFile
 * <p>
 * <p> Revision 1.52  2007/03/26 23:30:18  sueh
 * <p> bug# 964 Moved some of the imodOpen functions to the parent class to be shared.
 * <p>
 * <p> Revision 1.51  2007/02/19 21:49:38  sueh
 * <p> bug# 964 Removed isNewManager() because it is only used by Application
 * <p> Manager.
 * <p>
 * <p> Revision 1.50  2007/02/08 02:23:40  sueh
 * <p> bug# 962 Preventing xfmodel input file and output file from being the same.
 * <p>
 * <p> Revision 1.49  2007/02/05 21:27:36  sueh
 * <p> bug# 962 Add remapmodel, xfjointomo, xfmodel, and xftoxg.
 * <p>
 * <p> Revision 1.48  2006/11/28 22:48:38  sueh
 * <p> bug# 934 Changed BaseManager.stop() to endThreads().
 * <p>
 * <p> Revision 1.47  2006/11/15 18:47:01  sueh
 * <p> bug# 872 Changed getParamFileStorableArray to getStorables.  Letting the base
 * <p> save param file function call save().  getStorables always gets all the storables
 * <p> (including meta data) each time, to make it simpler.
 * <p>
 * <p> Revision 1.46  2006/10/24 21:14:06  sueh
 * <p> bug# 947 Passing the ProcessName to AxisProcessPanel.
 * <p>
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

  // Process dialog references
  private JoinDialog joinDialog = null;
  private AutoAlignmentController autoAlignmentController = null;

  // variables cast from base class variables
  // initialized in create function
  private MainJoinPanel mainPanel;
  private final JoinMetaData metaData;
  private JoinProcessManager processMgr;
  private JoinState state;
  private StartJoinParam startJoinParam = null;
  private final JoinScreenState screenState = new JoinScreenState(AxisID.ONLY,
      AxisType.SINGLE_AXIS);
  private boolean debug = false;

  JoinManager(String paramFileName, AxisID axisID) {
    super();
    this.metaData = new JoinMetaData(this,getLogProperties());
    createState();
    processMgr = new JoinProcessManager(this, state);
    initializeUIParameters(paramFileName, axisID);
    if (!paramFileName.equals("") && loadedParamFile) {
      imodManager.setMetaData(metaData);
      mainPanel.setStatusBarText(paramFile, metaData,logWindow);
    }
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      openJoinDialog();
      setMode();
    }
  }

  public InterfaceType getInterfaceType() {
    return InterfaceType.JOIN;
  }

  public boolean saveParamFile() throws LogFile.LockException, IOException {
    boolean retval;
    if ((retval = super.saveParamFile())) {
      endSetupMode();
    }
    return retval;
  }

  public boolean setParamFile() {
    if (!loadedParamFile && joinDialog != null) {
      String dir = joinDialog.getWorkingDirName();
      String root = joinDialog.getRootName();
      if (dir != null && !dir.matches("\\s*") && root != null && !root.matches("\\s*")) {
        File file = new File(dir, root + DataFileType.JOIN.extension);
        if (!file.exists()) {
          processMgr.createNewFile(file.getAbsolutePath());
        }
        initializeUIParameters(file, AxisID.ONLY, false);
        if (loadedParamFile) {
          imodManager.setMetaData(metaData);
          mainPanel.setStatusBarText(paramFile, metaData, logWindow);
        }
      }
    }
    return loadedParamFile;
  }

  public Component getFocusComponent() {
    if (joinDialog == null) {
      return null;
    }
    return joinDialog.getFocusComponent();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  String paramString() {
    return "joinDialog=" + joinDialog + ",metaData=" + metaData + ",\nprocessMgr="
        + processMgr + ",state=" + state + ",\nsuper[" + super.paramString() + "]";
  }

  /**
   * Open the join dialog
   */
  public void openJoinDialog() {
    openProcessingPanel();
    if (joinDialog == null) {
      if (loadedParamFile) {
        joinDialog = JoinDialog.getInstance(this, propertyUserDir, metaData, state);
      }
      else {
        joinDialog = JoinDialog.getInstance(this, metaData, state);
      }
      autoAlignmentController = new AutoAlignmentController(this, joinDialog,
          imodManager, null);
      joinDialog.setAutoAlignmentController(autoAlignmentController);
    }
    if (loadedParamFile) {
      autoAlignmentController.createEmptyXfFile();
    }
    mainPanel.showProcess(joinDialog.getContainer(), AxisID.ONLY);
    String actionMessage = Utilities.prepareDialogActionMessage(DialogType.JOIN,
        AxisID.ONLY, null);
    if (actionMessage != null) {
      System.err.println(actionMessage);
    }
  }

  /**
   * Return the test parameter file as a File object
   * @return a File object specifying the data set parameter file.
   */
  public File getParamFile() {
    if (paramFile == null) {
      if (!doneJoinDialog()) {
        return null;
      }
    }
    return paramFile;
  }

  public void getParameters(final MidasParam param, final AxisID axisID) {
    param.setInputFileName(FileType.JOIN_SAMPLE.getFileName(this, axisID));
    param.setSectionTableRowData(metaData.getSectionTableData());
  }

  public void getParameters(final XfalignParam param, final AxisID axisID) {
    param.setInputFileName(FileType.JOIN_SAMPLE_AVERAGES.getFileName(this, axisID));
  }

  private boolean doneJoinDialog() {
    if (joinDialog == null) {
      return false;
    }
    String workingDir = joinDialog.getWorkingDirName();
    if (!loadedParamFile && workingDir != null && !workingDir.matches("\\s*+")) {
      if (workingDir.endsWith(" ")) {
        uiHarness.openMessageDialog(this, "The directory, " + workingDir
            + ", cannot be used because it ends with a space.",
            "Unusable Directory Name", AxisID.ONLY);
        return false;
      }
      propertyUserDir = workingDir;
    }
    String rootName = joinDialog.getRootName();
    if (rootName == null) {
      return false;
    }
    if (!loadedParamFile && rootName != null && !rootName.matches("\\s*+")) {
      paramFile = new File(propertyUserDir, rootName + metaData.getFileExtension());
      if (!paramFile.exists()) {
        processMgr.createNewFile(paramFile.getAbsolutePath());
      }
      initializeUIParameters(paramFile, AxisID.ONLY, false);
      if (loadedParamFile) {
        imodManager.setMetaData(metaData);
        mainPanel.setStatusBarText(paramFile, metaData, logWindow);
      }
    }
    joinDialog.getMetaData(metaData, false);
    joinDialog.getScreenState(screenState);
    state.setDoneMode(joinDialog.getMode());
    saveStorables(AxisID.ONLY);
    return true;
  }

  public JoinScreenState getScreenState() {
    return screenState;
  }

  void createMainPanel() {
    if (!EtomoDirector.INSTANCE.getArguments().isHeadless()) {
      mainPanel = new MainJoinPanel(this);
    }
  }

  /**
   * Open 3dmod with binning 
   */
  public void imodOpen(String imodKey, int binning, Run3dmodMenuOptions menuOptions) {
    try {
      imodManager.setBinningXY(imodKey, binning);
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

  public void setDebug(boolean debug) {
    super.setDebug(debug);
    this.debug = debug;
  }

  public void imodOpen(String imodKey, int binning, String modelName,
      Run3dmodMenuOptions menuOptions) {
    if (debug) {
      System.err.println("imodOpen:modelName=" + modelName);
    }
    try {
      imodManager.setBinningXY(imodKey, binning);
      imodManager.open(imodKey, modelName, menuOptions);
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

  public void imodOpen(String imodKey) {
    try {
      imodManager.open(imodKey, AxisID.ONLY);
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

  public boolean isImodOpen(String imodKey) {
    try {
      return imodManager.isOpen(imodKey);
    }
    catch (AxisTypeException e) {
      uiHarness.openMessageDialog(this, e.getMessage(), "AxisType problem", AxisID.ONLY);
      return false;
    }
  }

  final boolean isTomosnapshotThumbnail() {
    return true;
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
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", AxisID.ONLY);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "System Process Exception",
          AxisID.ONLY);
    }
  }

  public SlicerAngles imodGetSlicerAngles(String imodKey, int imodIndex) {
    Vector results = null;
    try {
      if (imodIndex == -1) {
        uiHarness.openMessageDialog(this, "The is no open " + imodKey
            + " 3dmod for the highlighted row.", "No 3dmod", AxisID.ONLY);
      }
      results = imodManager.getSlicerAngles(imodKey, imodIndex);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, except.getMessage(), "AxisType problem",
          AxisID.ONLY);
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "IO Exception", AxisID.ONLY);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, e.getMessage(), "System Process Exception",
          AxisID.ONLY);
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
        if (ProcessMessages.getErrorIndex(result) != -1
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
        messageArray.add("Unable to retrieve slicer angles from " + imodKey + " 3dmod.");
        if (!slicerAngles.isEmpty()) {
          messageArray.add("slicerAngles=" + slicerAngles);
        }
      }
    }
    if (messageArray.size() > 0) {
      String[] messages = (String[]) messageArray
          .toArray(new String[messageArray.size()]);
      uiHarness.openMessageDialog(this, messages, "Slicer Angles", AxisID.ONLY);
    }
    return slicerAngles;
  }

  public void makejoincom(ProcessSeries processSeries,
      Deferred3dmodButton deferred3dmodButton, Run3dmodMenuOptions run3dmodMenuOptions,
      DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (!joinDialog.getMetaData(metaData, true)) {
      return;
    }
    if (!metaData.isValid(joinDialog.getWorkingDirName())) {
      uiHarness.openMessageDialog(this, metaData.getInvalidReason(), "Invalid Data",
          AxisID.ONLY);
      return;
    }
    if (!joinDialog.validateMakejoincom()) {
      return;
    }
    String rootName = metaData.getDatasetName();
    EtomoDirector.INSTANCE.renameCurrentManager(rootName);
    autoAlignmentController.createEmptyXfFile();
    MakejoincomParam makejoincomParam = new MakejoincomParam(metaData, state, this);
    if (paramFile == null) {
      endSetupMode();
    }
    if (!joinDialog.getMetaData(metaData, true)) {
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    try {
      threadNameA = processMgr.makejoincom(makejoincomParam, processSeries);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, "Can't run makejoincom\n" + except.getMessage(),
          "SystemProcessException", AxisID.ONLY);
      return;
    }
    processSeries.setNextProcess("startjoin", null);
    mainPanel.startProgressBar("Makejoincom", AxisID.ONLY, ProcessName.MAKEJOINCOM);
  }

  /**
   * post processing after a successful process
   * @param axisID
   * @param processName
   * @param processDetails
   */
  public void postProcess(String commandName, ProcessDetails processDetails) {
    try {
      if (commandName.equals(ProcessName.MAKEJOINCOM.toString())) {
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
      else if (commandName.equals(ProcessName.XFJOINTOMO.toString())) {
        joinDialog.setXfjointomoResult();
      }
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to read " + DatasetFiles.XFJOINTOMO_LOG
          + ".\n" + e.getMessage(), "Read Error");
    }
    catch (IOException e) {
      e.printStackTrace();
      uiHarness.openMessageDialog(this, "Unable to read " + DatasetFiles.XFJOINTOMO_LOG
          + ".\n" + e.getMessage(), "Read Error");
    }
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
    if (workingDirName.endsWith(" ")) {
      uiHarness.openMessageDialog(this, "The directory, " + workingDirName
          + ", cannot be used because it ends with a space.", "Unusable Directory Name",
          AxisID.ONLY);
      return false;
    }
    propertyUserDir = workingDirName;
    imodManager.setMetaData(metaData);
    paramFile = new File(propertyUserDir, metaData.getDatasetName()
        + metaData.getFileExtension());
    if (!paramFile.exists()) {
      processMgr.createNewFile(paramFile.getAbsolutePath());
    }
    loadedParamFile = true;
    // initializeUIParameters(paramFile, AxisID.ONLY, false);
    // if (loadedParamFile) {
    // imodManager.setMetaData(metaData);
    // mainPanel.setStatusBarText(paramFile, metaData, logPanel);
    // }
    mainPanel.setStatusBarText(paramFile, metaData, logWindow);
    return true;
  }

  private boolean copyMostRecentXfFile(String commandDescription) {
    String rootName = metaData.getDatasetName();
    String xfFileName = rootName + ".xf";
    File newXfFile = Utilities.mostRecentFile(propertyUserDir, xfFileName, rootName
        + MidasParam.getOutputFileExtension(),
        rootName + XfalignParam.getOutputFileExtension(), rootName + "_empty.xf");
    // If the most recent .xf file is not root.xf, copy it to root.xf
    if (!newXfFile.getName().equals(xfFileName)) {
      File xfFile = new File(propertyUserDir, xfFileName);
      try {
        Utilities.copyFile(newXfFile, xfFile);
      }
      catch (IOException e) {
        e.printStackTrace();
        String[] message = {
            "Unable to copy " + newXfFile.getAbsolutePath() + " to " + xfFileName + ".",
            "Copy " + newXfFile.getName() + " to " + xfFileName,
            " and then rerun " + commandDescription + "." };
        uiHarness.openMessageDialog(this, message, "Cannot Run Command", AxisID.ONLY);
        return false;
      }
    }
    return true;
  }

  /**
   * sets the mode in joinDialog based on whether the working directory and
   * root name are entered and whether a sample is saved
   * @param workingDirName
   * @return
   */
  public boolean setMode(String workingDirName) {
    // get a non-shared copy of doneMode
    int doneMode = state.getDoneMode();
    // only check done mode when we first set the mode
    state.clearDoneMode();
    if (!metaData.isValid(workingDirName)) {
      joinDialog.setMode(JoinDialog.SETUP_MODE);
      return false;
    }
    if (!state.isSampleProduced() || doneMode == JoinDialog.CHANGING_SAMPLE_MODE) {
      // either the sample was not produced, or the user had been changing the
      // sample when they exited the join dialog. If the done mode is
      // CHANGING_SAMPLE_MODE, then the sample values are not valid and the
      // original sample values have been lost.
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

  public void startjoin(final ProcessSeries processSeries) {
    if (!metaData.isValid(joinDialog.getWorkingDir())) {
      uiHarness.openMessageDialog(this, metaData.getInvalidReason(), "Invalid Data",
          AxisID.ONLY);
      return;
    }
    try {
      if (startJoinParam == null) {
        startJoinParam = new StartJoinParam(AxisID.ONLY);
      }
      threadNameA = processMgr.startjoin(startJoinParam, processSeries);
      startJoinParam = null;
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this,
          "Can't run startjoin.com\n" + except.getMessage(), "SystemProcessException",
          AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar("Startjoin", AxisID.ONLY, ProcessName.STARTJOIN);
  }

  public void xfjointomo(final ProcessSeries processSeries) {
    XfjointomoParam xfjointomoParam = new XfjointomoParam(this, state.getRefineTrial()
        .is());
    if (!joinDialog.getParameters(xfjointomoParam, true)) {
      return;
    }
    try {
      threadNameA = processMgr.xfjointomo(xfjointomoParam, processSeries);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, "Can't run " + ProcessName.XFJOINTOMO.toString()
          + "\n" + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar(ProcessName.XFJOINTOMO.toString(), AxisID.ONLY,
        ProcessName.XFJOINTOMO);
  }

  private void remapmodel(final ProcessSeries processSeries) {
    RemapmodelParam param = new RemapmodelParam(this);
    try {
      threadNameA = processMgr.remapmodel(param, processSeries);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, "Can't run " + RemapmodelParam.COMMAND_NAME
          + "\n" + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar(RemapmodelParam.COMMAND_NAME, AxisID.ONLY,
        ProcessName.REMAPMODEL);
  }

  public void xfmodel(String inputFile, String outputFile, ProcessSeries processSeries,
      Deferred3dmodButton deferred3dmodButton, Run3dmodMenuOptions run3dmodMenuOptions,
      final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    XfmodelParam param = new XfmodelParam(this);
    param.setInputFile(inputFile);
    param.setOutputFile(outputFile);
    if (param.isValid()) {
      processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
      xfmodel(param, processSeries, dialogType);
    }
  }

  private void xfmodel(ProcessSeries processSeries, DialogType dialogType) {
    xfmodel(new XfmodelParam(this), processSeries, dialogType);
  }

  private void xfmodel(XfmodelParam param, ProcessSeries processSeries,
      final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (debug) {
      System.err.println("xfmodel:gapExist=" + state.isGapsExist());
    }
    if (state.isGapsExist()) {
      processSeries.setNextProcess(ProcessName.REMAPMODEL.toString(), null);
    }
    try {
      threadNameA = processMgr.xfmodel(param, AxisID.ONLY, null, processSeries);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, "Can't run " + XfmodelParam.COMMAND_NAME + "\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar(XfmodelParam.COMMAND_NAME, AxisID.ONLY,
        ProcessName.XFMODEL);
  }

  private void xftoxg(ProcessSeries processSeries, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    processSeries.setNextProcess(ProcessName.XFMODEL.toString(), null);
    XftoxgParam param = new XftoxgParam(this);
    ConstEtomoNumber refSection = state.getJoinAlignmentRefSection(state.getRefineTrial()
        .is());
    if (!refSection.isNull()) {
      param.setReferenceSection(refSection);
    }
    param.setNumberToFit(0);
    param.setXfFileName(DatasetFiles.getRefineXfFileName(this));
    param.setXgFileName(DatasetFiles.getRefineXgFileName(this));
    try {
      threadNameA = processMgr.xftoxg(param, processSeries);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      uiHarness.openMessageDialog(this, "Can't run " + XftoxgParam.COMMAND_NAME + "\n"
          + except.getMessage(), "SystemProcessException", AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar(XftoxgParam.COMMAND_NAME, AxisID.ONLY, ProcessName.XFTOXG);
  }

  public void updateJoinDialogDisplay() {
    joinDialog.updateDisplay();
  }

  /**
   * Runs finishjoin in the mode specified.  If the mode is SUPPRESS_EXECUTION
   * then finishjoin is not executed, and only used for placing data into
   * JoinState.
   * @param mode
   * @param buttonText
   */
  public void finishjoin(FinishjoinParam.Mode mode, String buttonText,
      ProcessSeries processSeries, Deferred3dmodButton deferred3dmodButton,
      Run3dmodMenuOptions run3dmodMenuOptions, final DialogType dialogType) {
    if (processSeries == null) {
      processSeries = new ProcessSeries(this, dialogType);
    }
    if (!updateMetaDataFromJoinDialog(AxisID.ONLY, true)) {
      return;
    }
    FinishjoinParam param = new FinishjoinParam(this, mode);
    if (!copyMostRecentXfFile(buttonText)) {
      return;
    }
    if (!joinDialog.validateFinishjoin()) {
      return;
    }
    processSeries.setRun3dmodDeferred(deferred3dmodButton, run3dmodMenuOptions);
    if (mode == FinishjoinParam.Mode.REJOIN
        || mode == FinishjoinParam.Mode.SUPPRESS_EXECUTION) {
      processSeries.setNextProcess(ProcessName.XFTOXG.toString(), null);
    }
    if (mode == FinishjoinParam.Mode.SUPPRESS_EXECUTION) {
      processSeries.setLastProcess(ImodManager.TRANSFORMED_MODEL_KEY);
      processMgr.saveFinishjoinState(param, processSeries);
      processSeries.startNextProcess(AxisID.ONLY, null);
    }
    else {
      try {
        threadNameA = processMgr.finishjoin(param, processSeries);
      }
      catch (SystemProcessException except) {
        except.printStackTrace();
        uiHarness.openMessageDialog(this,
            "Can't run " + buttonText + "\n" + except.getMessage(),
            "SystemProcessException", AxisID.ONLY);
        return;
      }
      mainPanel.startProgressBar("Finishjoin: " + buttonText, AxisID.ONLY,
          ProcessName.FINISHJOIN);
    }
  }

  private boolean updateMetaDataFromJoinDialog(AxisID axisID, final boolean doValidation) {
    if (!joinDialog.getMetaData(metaData, doValidation)) {
      return false;
    }
    if (!metaData.isValid(propertyUserDir)) {
      uiHarness.openMessageDialog(this, metaData.getInvalidReason(), "Invalid Data",
          axisID);
      return false;
    }
    try {
      ParameterStore parameterStore = getParameterStore();
      if (parameterStore == null) {
        return false;
      }
      parameterStore.save(metaData);
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

  public void setSize(String sizeInXString, String sizeInYString) {
    EtomoNumber sizeInX = new EtomoNumber(EtomoNumber.Type.INTEGER);
    EtomoNumber sizeInY = new EtomoNumber(EtomoNumber.Type.INTEGER);
    sizeInX.set(sizeInXString);
    joinDialog.setSizeInX(sizeInX);
    sizeInY.set(sizeInYString);
    joinDialog.setSizeInY(sizeInY);
  }

  public void setShift(int shiftInX, int shiftInY) {
    joinDialog.setShiftInX(shiftInX);
    joinDialog.setShiftInY(shiftInY);
  }

  public void rotx(File tomogram, File workingDir, final ProcessSeries processSeries) {
    ClipParam clipParam = ClipParam.getRotxInstance(this, AxisID.ONLY, tomogram,
        workingDir);
    try {
      threadNameA = processMgr.rotx(clipParam, processSeries);
    }
    catch (SystemProcessException except) {
      joinDialog.abortAddSection();
      except.printStackTrace();
      uiHarness.openMessageDialog(this, "Can't run clip rotx\n" + except.getMessage(),
          "SystemProcessException", AxisID.ONLY);
      return;
    }
    mainPanel.startProgressBar("rotating " + tomogram.getName(), AxisID.ONLY);
  }

  public void abortAddSection() {
    joinDialog.abortAddSection();
  }

  public void addSection(File tomogram) {
    joinDialog.addSection(tomogram);
  }

  /**
   * Open the main window in processing mode
   * MUST run reconnect for all axis
   */
  private void openProcessingPanel() {
    mainPanel.showProcessingPanel(AxisType.SINGLE_AXIS);
    setPanel();
    reconnect(axisProcessData.getSavedProcessData(AxisID.ONLY), AxisID.ONLY, false);
  }

  /**
   * Set the data set parameter file. This also updates the mainframe data
   * parameters.
   * @param paramFile a File object specifying the data set parameter file.
   */
  public boolean setParamFile(File paramFile) {
    if (!super.setParamFile(paramFile)) {
      return false;
    }
    // Update main window information and status bar
    mainPanel.setStatusBarText(paramFile, metaData, logWindow);
    return true;
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
  boolean startNextProcess(final UIComponent uiComponent, final AxisID axisID,
      final ProcessSeries.Process process,
      final ProcessResultDisplay processResultDisplay, ProcessSeries processSeries,
      DialogType dialogType, ProcessDisplay display) {
    if (super.startNextProcess(uiComponent, axisID, process, processResultDisplay,
        processSeries, dialogType, display)) {
      return true;
    }
    if (debug) {
      System.err.println("startNextProcess:axisID=" + axisID + ",nextProcess=" + process);
    }
    if (process.equals("startjoin")) {
      startjoin(processSeries);
      return true;
    }
    if (process.equals(ProcessName.XFTOXG.toString())) {
      xftoxg(processSeries, dialogType);
      return true;
    }
    if (process.equals(ProcessName.XFMODEL.toString())) {
      xfmodel(processSeries, dialogType);
      return true;
    }
    if (process.equals(ProcessName.REMAPMODEL.toString())) {
      remapmodel(processSeries);
      return true;
    }
    if (process.equals(ImodManager.TRANSFORMED_MODEL_KEY)) {
      imodOpen(ImodManager.TRANSFORMED_MODEL_KEY);
      return true;
    }
    return false;
  }

  public BaseMetaData getBaseMetaData() {
    return (BaseMetaData) metaData;
  }

  public MainPanel getMainPanel() {
    return mainPanel;
  }

  void createState() {
    state = new JoinState(this);
  }

  public ConstJoinState getState() {
    return state;
  }

  public StartJoinParam newStartJoinParam() {
    startJoinParam = new StartJoinParam(AxisID.ONLY);
    return startJoinParam;
  }

  public BaseState getBaseState() {
    return state;
  }

  AutoAlignmentMetaData getAutoAlignmentMetaData() {
    return metaData.getAutoAlignmentMetaData();
  }

  public boolean updateMetaData(final DialogType dialogType, final AxisID axisID,
      final boolean doValidation) {
    return joinDialog.getMetaData(metaData, doValidation);
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

  public BaseProcessManager getProcessManager() {
    return processMgr;
  }

  final Storable[] getStorables(int offset) {
    Storable[] storable = new Storable[3 + offset];
    int index = offset;
    storable[index++] = metaData;
    storable[index++] = state;
    storable[index] = screenState;
    return storable;
  }

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

  public boolean save() throws LogFile.LockException, IOException {
    super.save();
    doneJoinDialog();
    mainPanel.done();
    return true;
  }

  public String getName() {
    return metaData.getName();
  }
}