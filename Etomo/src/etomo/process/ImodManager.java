package etomo.process;

import java.util.HashMap;
import java.util.Vector;
import java.util.Set;
import java.util.Iterator;
import java.io.File;
import java.io.IOException;

import etomo.BaseManager;
import etomo.EtomoDirector;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.BaseMetaData;
import etomo.type.ConstMetaData;
import etomo.type.ConstPeetMetaData;
import etomo.type.FileType;
import etomo.type.ImageFileType;
import etomo.type.JoinMetaData;
import etomo.type.ParallelMetaData;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.UIHarness;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: This class manages the opening, closing and sending of 
 * messages to the appropriate imod processes. This class is state based in the
 * sense that is initialized with MetaData information and uses that information
 * to know which data sets to work with.</p>
 *
 * <p>Copyright: Copyright (c) 2002</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.78  2010/03/11 06:00:42  sueh
 * <p> bug# 1311 Added setOpenModelView.
 * <p>
 * <p> Revision 3.77  2010/02/17 04:49:20  sueh
 * <p> bug# 1301 Using the manager instead of the manager key do pop up
 * <p> messages.
 * <p>
 * <p> Revision 3.76  2009/12/19 01:08:52  sueh
 * <p> bug# 1294 Added SMOOTHING_ASSESSMENT_KEY.
 * <p>
 * <p> Revision 3.75  2009/12/17 16:45:14  sueh
 * <p> bug# 1295 Fixed axis correction in getVector(String,AxisID).
 * <p>
 * <p> Revision 3.74  2009/09/01 03:17:56  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.73  2009/06/05 01:52:24  sueh
 * <p> bug# 1219 Added FLAT_VOLUME_KEY, flatVolumeKey, newFlatVolume, and
 * <p> setStartNewContoursAtNewZ.  Added the axisID to setSwapYZ.
 * <p>
 * <p> Revision 3.72  2009/03/24 20:16:28  sueh
 * <p> bug# 1187 Wasn't getting imodState after calling newImod in many of the
 * <p> public functions, so the functions where returning without calling the
 * <p> corresponding imodState function.
 * <p>
 * <p> Revision 3.71  2009/03/23 16:56:24  sueh
 * <p> bug# 1187 Added setContinuousListenerTarget.  Only use requestHandler if the OS is
 * <p> Windows and the --listen parameter was used.
 * <p>
 * <p> Revision 3.70  2009/03/17 00:36:03  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.69  2009/03/11 01:11:32  sueh
 * <p> bug# 1195 In getRubberbandCoordinates and getSlicerAngles pop up an
 * <p> error message if imodState is null.
 * <p>
 * <p> Revision 3.68  2008/12/09 21:30:44  sueh
 * <p> bug# 1160 Removed the management of beadfixerDiameter.  It is now
 * <p> handled by ImodState.
 * <p>
 * <p> Revision 3.67  2008/12/05 00:50:16  sueh
 * <p> bug# 1156 Added setSkipList.
 * <p>
 * <p> Revision 3.66  2008/11/20 01:32:37  sueh
 * <p> bug# 1147 Added ERASED_FIDUCIALS_KEY and newErasedFiducials.
 * <p>
 * <p> Revision 3.65  2008/10/27 17:53:44  sueh
 * <p> bug# 1141 Added ctfCorrection.
 * <p>
 * <p> Revision 3.64  2008/07/24 17:57:47  sueh
 * <p> bug# 1128 Added setPointLimit.
 * <p>
 * <p> Revision 3.63  2008/07/02 18:45:22  sueh
 * <p> bug# 1121 opening objects windows in patch vector model
 * <p>
 * <p> Revision 3.62  2008/06/19 23:31:52  sueh
 * <p> bug# 1112 Added setTiltFile and resetTiltFile.
 * <p>
 * <p> Revision 3.61  2008/05/03 00:39:17  sueh
 * <p> bug# 847 Modified open(String) to pass a null instead of an empty
 * <p> Run3dmodMenuOptions; ImodProcess handles null
 * <p> Run3dmodMenuOptions.
 * <p>
 * <p> Revision 3.60  2008/05/01 22:53:56  sueh
 * <p> bug# 1107 In newAvgVol() added -V -Z -E U to 3dmod command.
 * <p>
 * <p> Revision 3.59  2008/02/14 21:29:02  sueh
 * <p> bug# 1077 Make sure that disconnect attempts to disconnect each
 * <p> ImodState, even if there is an exception.
 * <p>
 * <p> Revision 3.58  2007/12/10 22:14:25  sueh
 * <p> bug# 1041 Making the preview meta data with BaseMetaData instead of ConstMetaData.
 * <p>
 * <p> Revision 3.57  2007/11/12 14:55:00  sueh
 * <p> bug# 1047 Added swapYZ to open(String,String[],Run3dmodMenuOptions,String).
 * <p>
 * <p> Revision 3.56  2007/11/06 19:21:59  sueh
 * <p> bug# 1047 Added keys for anisotropic diffusion.
 * <p>
 * <p> Revision 3.55  2007/08/02 22:38:24  sueh
 * <p> bug# 1034 Passing the right click menu to open(String,String[]).
 * <p>
 * <p> Revision 3.54  2007/05/15 21:44:57  sueh
 * <p> bug# 964 Added ref ImodState.
 * <p>
 * <p> Revision 3.53  2007/05/11 19:28:30  sueh
 * <p> bug# 964 In open(String,String[]), create a new ImodState if the
 * <p> fileNameArray has changed.
 * <p>
 * <p> Revision 3.52  2007/05/11 15:40:22  sueh
 * <p> bug# 964 Added avgVol.
 * <p>
 * <p> Revision 3.51  2007/03/26 23:31:58  sueh
 * <p> bug# 964 Added loadPeetMap().  Added
 * <p> open(String,int,String,boolean,Run3dmodMenuOptions) to open a file with a model.
 * <p>
 * <p> Revision 3.50  2007/02/05 22:54:44  sueh
 * <p> bug# 962 Added modeleled join and transformed model.
 * <p>
 * <p> Revision 3.49  2006/10/25 21:23:23  sueh
 * <p> bug# 951  newPatchVectorCCCModel:  opening in 3dmodv.
 * <p>
 * <p> Revision 3.48  2006/10/16 22:37:58  sueh
 * <p> bug# 933 Don't reopen log unless the 3dmod is open.
 * <p>
 * <p> Revision 3.47  2006/10/10 05:09:14  sueh
 * <p> bug# 931 Getting the patch vector model file names from DatasetFiles.
 * <p>
 * <p> Revision 3.46  2006/09/19 22:19:53  sueh
 * <p> bug# 928 Added patchVectorCCCModel.
 * <p>
 * <p> Revision 3.45  2006/08/11 23:47:26  sueh
 * <p> bug# 816 Added reopenLog().
 * <p>
 * <p> Revision 3.44  2006/08/11 21:45:52  sueh
 * <p> bug# 816 Added setOpenLog() and setOpenLogOff()
 * <p>
 * <p> Revision 3.43  2006/07/17 21:17:08  sueh
 * <p> bug# 900 Added imodSendEvent functionality back.  Uses the
 * <p> SystemProcessException.
 * <p>
 * <p> Revision 3.42  2006/07/04 20:39:03  sueh
 * <p> bug# 894 Changed seedMode to newContours.  Added setBeadfixerMode().
 * <p>
 * <p> Revision 3.41  2006/07/03 21:38:58  sueh
 * <p> bug# 895 Added ImodRequestHandler to handle requests from 3dmod in
 * <p> Windows.  Added processRequest(), which is called by ImodRequestHandler.
 * <p> Added stopRequestHandler().
 * <p>
 * <p> Revision 3.40  2006/06/22 20:59:47  sueh
 * <p> bug# 797 Catching io exception when sending messages to 3dmods.
 * <p>
 * <p> Revision 3.39  2006/06/19 17:06:17  sueh
 * <p> bug# 851 Added quiteAll() to quit all 3dmods in a vector.
 * <p>
 * <p> Revision 3.38  2006/04/11 13:47:10  sueh
 * <p> bug# 809 Manage auto center and seed mode separately from
 * <p> openBeadFixer so that seed mode doesn't always have to be managed.
 * <p>
 * <p> Revision 3.37  2006/03/30 21:22:19  sueh
 * <p> bug# 809 Setting beadfixer diameter (fiducial diameter in pixels) when the
 * <p> reconstruction meta data is set.  Passing auto center and seed mode
 * <p> settings to ImodProcess.
 * <p>
 * <p> Revision 3.36  2005/12/14 01:27:50  sueh
 * <p> bug# 782 Printing an exception when a situation related to this bug is found.
 * <p>
 * <p> Revision 3.35  2005/10/18 22:10:39  sueh
 * <p> bug# 727 Can't reproduce this bug so added some prints to the error log
 * <p> to document it, if it appears again.
 * <p>
 * <p> Revision 3.34  2005/08/11 23:27:30  sueh
 * <p> bug# 711  Add menu options for 3dmod startup window and bin by 2.
 * <p> Update the menus in both frames when one is changed.  Change enum
 * <p> Run3dmodMenuOption to Run3dmodMenuOptions, which can turn on
 * <p> multiple options at once.  This allows ImodState to combine input from
 * <p> the context menu and the pulldown menu.  Move setting about whether a
 * <p> type of 3dmod run can be binned in Z to ImodManager.  In ImodManager:
 * <p> to be sure that the 3dmod -view configuration won't use -O, -B, or -b;
 * <p> prevent Run3dmodMenuOptions from being turned on by setting
 * <p> noOptions.  Pass Run3dmodMenuOptions to ImodManager.open(),
 * <p> ImodState.open(), and ImodProcess.open().  It does not have to be
 * <p> saved.  In ImodState.open() add the menu options from the pulldown
 * <p> menu to the existing menu options.
 * <p>
 * <p> Revision 3.33  2005/08/09 19:56:31  sueh
 * <p> bug# 711 Added setRun3dmodMenuOption functions.
 * <p> bug# 712 In newMatchCheck(), passed matchcheck.mat and
 * <p> matchcheck.rec separatedly.
 * <p>
 * <p> Revision 3.32  2005/07/29 00:51:36  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 3.31  2005/04/25 20:45:39  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.  Move the interface for
 * <p> popping up message dialogs to UIHarness.  It prevents headless
 * <p> exceptions during a test execution.  It also allows logging of dialog
 * <p> messages during a test.  It also centralizes the dialog interface and
 * <p> allows the dialog functions to be synchronized to prevent dialogs popping
 * <p> up in both windows at once.  All Frame functions will use UIHarness as a
 * <p> public interface.
 * <p>
 * <p> Revision 3.30  2005/03/04 00:13:03  sueh
 * <p> bug# 533 Added setPieceListFileName() to set the -p command line
 * <p> option in the 3dmod call.
 * <p>
 * <p> Revision 3.29  2005/03/02 23:13:59  sueh
 * <p> bug# 533 Adding -fr (frames) to ignore montaging information and
 * <p> display the stack frame by frame.
 * <p>
 * <p> Revision 3.28  2004/12/04 01:26:34  sueh
 * <p> bug# 557 Added SQUEEZED_VOLUME_KEY.
 * <p>
 * <p> Revision 3.27  2004/11/24 18:10:26  sueh
 * <p> bug# 520 Added binning in XY.
 * <p>
 * <p> Revision 3.26  2004/11/19 23:21:15  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 3.25.4.10  2004/11/12 22:53:32  sueh
 * <p> bug# 520 Added trial join key.
 * <p>
 * <p> Revision 3.25.4.9  2004/11/11 01:36:41  sueh
 * <p> bug# 520 Adding more setBinning functions.
 * <p>
 * <p> Revision 3.25.4.8  2004/10/29 22:09:57  sueh
 * <p> bug# 520 Added rotTomogram to keep track of 3dmods displaying the
 * <p> rotated version of tomograms.
 * <p>
 * <p> Revision 3.25.4.7  2004/10/21 02:40:14  sueh
 * <p> bug# 520 Added Join 3dmod.
 * <p>
 * <p> Revision 3.25.4.6  2004/10/14 17:22:47  sueh
 * <p> bug# 520 fixed bug in joinSampleAverages
 * <p>
 * <p> Revision 3.25.4.5  2004/10/14 17:13:10  sueh
 * <p> bug# 520 Added join sample averages key.
 * <p>
 * <p> Revision 3.25.4.4  2004/10/14 03:28:01  sueh
 * <p> bug# 520 Added an imod for join samples (root.sample).  Added a
 * <p> setMetaData for ConstJoinMetaData.  Added loadJoinMap() which
 * <p> creates join imods like join samples.
 * <p>
 * <p> Revision 3.25.4.3  2004/09/22 22:06:36  sueh
 * <p> bug# 520 Made isOpen() check all imodStates of each type, instead of
 * <p> only the most recent.  Added getSlicerAngles().  Removed
 * <p> get(String, boolean) because it is not used in isOpen() anymore.
 * <p>
 * <p> Revision 3.25.4.2  2004/09/21 17:55:18  sueh
 * <p> bug# 520 Added a new type of 3dmod called a tomogram.  A tomogram
 * <p> 3dmod opens a file regardless of dataset and axisID.  Multiple tomogram
 * <p> 3dmods are managed at the same time, so they always have to be found
 * <p> using a vector index.  They should be deleted when the
 * <p> SectionTableRow they are associated with is deleted.  Added
 * <p> delete(key, vectorIndex), and open(key, File, int).  Added
 * <p> get(key, vectorIndex).  Added newImod(key, File) and
 * <p> newVector(key, File).  Added new tomogram functionality to
 * <p> newImodState.  Added a File parameter to newImodState.  Added
 * <p> deleteImodState(key, vectorIndex).
 * <p>
 * <p> Revision 3.25.4.1  2004/09/03 21:09:49  sueh
 * <p> bug# 520 removing ApplicationManager from ImodManager constructor,
 * <p> since its not being used
 * <p>
 * <p> Revision 3.25  2004/06/22 22:50:15  sueh
 * <p> bug# 455 Moved openWithModel logic to ImodProcess.
 * <p> Moved useMode logic to ImodState.  Removed model() functions.
 * <p> Created set functions for openContours and preserveContrast.
 * <p>
 * <p> Revision 3.24  2004/06/10 18:21:49  sueh
 * <p> bug# 463 changed create() to newImod(), changed
 * <p> openBeadFixer() to setOpenBeadFixer() and made it a state
 * <p> change function rather then a message sending function
 * <p>
 * <p> Revision 3.23  2004/06/10 17:24:16  sueh
 * <p> bug# 462 remove reset() function, only set initial and unchanging
 * <p> settings when constructing and initializing ImodState
 * <p>
 * <p> Revision 3.22  2004/06/07 00:16:44  sueh
 * <p> bug# 452 call open() in place of model().  If was calling open() with
 * <p> a model name, set openWithModel to true.
 * <p>
 * <p> Revision 3.21  2004/05/06 20:20:50  sueh
 * <p> bug# 33 added getRubberbandCoordinates()
 * <p>
 * <p> Revision 3.20  2004/05/03 22:21:49  sueh
 * <p> bug# 416 added setBinning()
 * <p> fixing bug in setSwapYZ(), should be ok to call any set function before
 * <p> the ImodState is created, unless you are passing vectorIndex (this
 * <p> specified a particular ImodState instance)
 * <p>
 * <p> Revision 3.19  2004/04/28 22:16:39  sueh
 * <p> bug# 320 user interaction goes in app manager
 * <p>
 * <p> Revision 3.18  2004/04/28 00:39:43  sueh
 * <p> bug# 320 changed warnStaleFile() message
 * <p>
 * <p> Revision 3.17  2004/04/27 23:17:03  sueh
 * <p> bug# 320 added warnStaleFile() to tell user and a file that has
 * <p> changed on disk and ask to close it
 * <p>
 * <p> Revision 3.16  2004/03/29 20:54:09  sueh
 * <p> bug# 409 add MTF Filter
 * <p>
 * <p> Revision 3.15  2004/03/07 22:35:04  sueh
 * <p> bug# 399 removed deprecated code
 * <p>
 * <p> Revision 3.14  2004/02/25 22:44:42  sueh
 * <p> bug# 403 comments - clarified setMetaData
 * <p>
 * <p> Revision 3.13  2004/02/16 18:49:13  sueh
 * <p> bug# 276 added getModelName()
 * <p>
 * <p> Revision 3.12  2004/02/07 02:58:29  sueh
 * <p> bug# 169 Added preview key, deprecated out-of-date
 * <p> functions, changed the metadata load, created an open()
 * <p> function which uses the vector index, fixed a problem in
 * <p> create(String,AxisID,String).
 * <p>
 * <p> Revision 3.11  2004/02/05 18:03:19  sueh
 * <p> bug# 306 added setSwapYZ - used to set swapYZ before
 * <p> opening 3dmod
 * <p>
 * <p> Revision 3.10  2004/02/04 18:09:44  sueh
 * <p> bug# 171 added isOpen() to find out if any 3dmod is open,
 * <p> added quit() to quit all 3dmods
 * <p>
 * <p> Revision 3.9  2003/12/04 22:01:27  sueh
 * <p> bug242 Added create() functions, to create an ImodState
 * <p> without opening it.  Deprecating old interface.  fixing
 * <p> openFullVolume() - should come up in movie mode.
 * <p>
 * <p> Revision 3.8  2003/12/03 16:39:06  sueh
 * <p> bug242 put the ImodStates into Vectors.  Allow multiple
 * <p> ImodStates on each key
 * <p>
 * <p> Revision 3.7  2003/12/02 23:16:40  sueh
 * <p> bug242 changed from ImodAssistant to ImodState, added
 * <p> reset() function to handle FullVolume.
 * <p>
 * <p> Revision 3.6  2003/12/01 17:09:54  sueh
 * <p> bug242 Allowing ImodAssistants to be created on the fly.
 * <p>
 * <p> Revision 3.5  2003/11/25 22:53:46  sueh
 * <p> bug242 removed the last dependency on non-map
 * <p> ImodAssistants, moved constant 3dmod settings to the
 * <p> constructor
 * <p>
 * <p> Revision 3.4  2003/11/22 00:08:13  sueh
 * <p> bug242 quitFinelyAligned ignoring axisID and only quitting B
 * <p> axis - fixed
 * <p>
 * <p> Revision 3.3  2003/11/21 23:53:11  sueh
 * <p> bug242 ImodManager -  incorporated new ImodAssistant
 * <p> interface changes, created generic functions, created the
 * <p> map of ImodAssistants, allow existing functions to use the
 * <p> map
 * <p>
 * <p> Revision 3.2  2003/11/15 01:42:05  sueh
 * <p> bug242 switched from ImodProcess to ImodAssistant
 * <p> without generalizing the code
 * <p>
 * <p> Revision 3.1  2003/11/11 00:20:43  sueh
 * <p> Bug349 fiducialModelA and B are now ImodProcesses.
 * <p> Initialize them. OpenFiducialModel(): changed it to work with
 * <p> ImodProcess.
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.25  2003/11/05 20:29:52  rickg
 * <p> Bug #292 Added preserve contrast functionality to coarse align open
 * <p>
 * <p> Revision 2.24  2003/11/05 18:02:45  sueh
 * <p> bug298
 * <p>
 * <p> Revision 2.23  2003/11/04 20:56:11  rickg
 * <p> Bug #345 IMOD Directory supplied by a static function from ApplicationManager
 * <p>
 * <p> Revision 2.22  2003/11/04 17:53:31  rickg
 * <p> Bug #345 Explicitly set path to 3dmodusing IMOD_DIR
 * <p>
 * <p> Revision 2.21  2003/10/30 23:36:02  rickg
 * <p> Bug# 343 Open patch vector model in model mode
 * <p>
 * <p> Revision 2.20  2003/10/14 23:44:04  rickg
 * <p> Bug# 285 Fixed select mapping for fine aligned tomogram
 * <p>
 * <p> Revision 2.19  2003/09/08 05:45:39  rickg
 * <p> Rename single axis full volume dataset
 * <p>
 * <p> Revision 2.18  2003/09/02 21:58:08  rickg
 * <p> Changed naming structure to match trial tomogram structure
 * <p>
 * <p> Revision 2.17  2003/08/25 22:18:50  rickg
 * <p> Removed errant model opening for the tomogram where a matching
 * <p> or patch region model had been previously opened
 * <p>
 * <p> Revision 2.16  2003/08/05 21:20:17  rickg
 * <p> Implemented model and movie modes where appropriate
 * <p>
 * <p> Revision 2.15  2003/07/25 22:58:23  rickg
 * <p> Model mode management changes
 * <p>
 * <p> Revision 2.14  2003/07/22 22:15:19  rickg
 * <p> Add erased stack management
 * <p>
 * <p> Revision 2.13  2003/06/05 21:15:51  rickg
 * <p> Open sample in model mode
 * <p>
 * <p> Revision 2.12  2003/05/12 23:25:34  rickg
 * <p> imodv -> 3dmod -view
 * <p>
 * <p> Revision 2.11  2003/05/09 17:50:58  rickg
 * <p> Set appmgr on construction
 * <p>
 * <p> Revision 2.10  2003/05/08 23:19:03  rickg
 * <p> Standardized debug setting
 * <p>
 * <p> Revision 2.9  2003/05/07 22:29:14  rickg
 * <p> set fill cache for matchCheck
 * <p>
 * <p> Revision 2.8  2003/04/30 18:48:34  rickg
 * <p> Changed matchcheck* to a single imod instance
 * <p>
 * <p> Revision 2.7  2003/04/28 23:25:26  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.6  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.5  2003/04/16 22:18:59  rickg
 * <p> Added imod of full and trimmed volume
 * <p>
 * <p> Revision 2.4  2003/03/20 21:18:40  rickg
 * <p> Added matchshift results button/access
 * <p>
 * <p> Revision 2.3  2003/03/19 00:23:43  rickg
 * <p> Added patch vector model management
 * <p>
 * <p> Revision 2.2  2003/03/18 00:32:33  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.1  2003/03/07 07:22:50  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.9.2.1  2003/01/24 18:36:17  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.9  2003/01/10 20:46:20  rickg
 * <p> Added ability to view 3D fiducial models
 * <p>
 * <p> Revision 1.8  2003/01/07 00:30:46  rickg
 * <p> Fixed javadoc text
 * <p>
 * <p> Revision 1.7  2002/10/29 18:21:00  rickg
 * <p> Check to see if the ImodProcess is non-null before calling open isRunning
 * <P> in is*Open() functions.  Return false if the object is null so that they
 * <p> can be called without exceptions.
 * <p>
 * <p> Revision 1.6  2002/10/07 22:25:03  rickg
 * <p> removed unused imports
 * <p> reformat after emacs messed it up
 * <p>
 * <p> Revision 1.5  2002/09/20 18:33:04  rickg
 * <p> Added rest of quit methods
 * <p>
 * <p> Revision 1.4  2002/09/20 17:16:04  rickg
 * <p> Added typed exceptions
 * <p> Added methods to check if a particular process is open
 * <p> Added quit methods for processes
 * <p>
 * <p> Revision 1.3  2002/09/19 23:11:26  rickg
 * <p> Completed initial vesion to work with ImodProcess
 * <p>
 * <p> Revision 1.2  2002/09/17 23:39:38  rickg
 * <p> ImodProcess based, in progress
 * <p>
 * <p> Revision 1.1  2002/09/13 21:28:31  rickg
 * <p> initial entry
 * <p>
 * <p> </p>
 **/
public class ImodManager {
  public static final String rcsid = "$Id$";

  public static final int DEFAULT_BEADFIXER_DIAMETER = 3;
  private AxisType axisType = AxisType.SINGLE_AXIS;
  private String datasetName = "";
  private HashMap imodMap;

  protected ImodState rawStackA;
  protected ImodState rawStackB;
  protected ImodState erasedStackA;
  protected ImodState erasedStackB;

  protected ImodState coarseAlignedA;
  protected ImodState coarseAlignedB;
  protected ImodState fineAlignedA;
  protected ImodState fineAlignedB;
  protected ImodState sampleA;
  protected ImodState sampleB;
  protected ImodState fullVolumeA;
  protected ImodState fullVolumeB;
  protected ImodState combinedTomogram;
  protected ImodState patchVectorModel;
  protected ImodState matchCheck;
  protected ImodState trimmedVolume;
  protected ImodState fiducialModelA;
  protected ImodState fiducialModelB;
  protected ImodState patchVectorCCCModel;
  protected ImodState avgVol;
  protected ImodState ref;

  private boolean metaDataSet = false;
  private boolean debug = false;

  //public keys

  public static final String RAW_STACK_KEY = new String("raw stack");
  public static final String ERASED_STACK_KEY = new String("erased stack");
  public static final String COARSE_ALIGNED_KEY = new String("coarse aligned");
  public static final String FINE_ALIGNED_KEY = new String("fine aligned");
  public static final String SAMPLE_KEY = new String("sample");
  public static final String FULL_VOLUME_KEY = new String("full volume");
  public static final String COMBINED_TOMOGRAM_KEY = new String(
      "combined tomogram");
  public static final String FIDUCIAL_MODEL_KEY = new String("fiducial model");
  public static final String TRIMMED_VOLUME_KEY = new String("trimmed volume");
  public static final String PATCH_VECTOR_MODEL_KEY = new String(
      "patch vector model");
  public static final String MATCH_CHECK_KEY = new String("match check");
  public static final String TRIAL_TOMOGRAM_KEY = new String("trial tomogram");
  public static final String MTF_FILTER_KEY = new String("mtf filter");
  public static final String PREVIEW_KEY = new String("preview");
  public static final String TOMOGRAM_KEY = new String("tomogram");
  public static final String JOIN_SAMPLES_KEY = new String("joinSamples");
  public static final String JOIN_SAMPLE_AVERAGES_KEY = new String(
      "joinSampleAverages");
  public static final String JOIN_KEY = new String("join");
  public static final String ROT_TOMOGRAM_KEY = new String("rotTomogram");
  public static final String TRIAL_JOIN_KEY = new String("TrialJoinKey");
  public static final String SQUEEZED_VOLUME_KEY = new String("SqueezedVolume");
  public static final String PATCH_VECTOR_CCC_MODEL_KEY = new String(
      "patch vector ccc model");
  public static final String MODELED_JOIN_KEY = new String("modeled join");
  public static final String TRANSFORMED_MODEL_KEY = new String(
      "transformed model");
  public static final String AVG_VOL_KEY = new String("AvgVol");
  public static final String REF_KEY = new String("Ref");
  public static final String VOLUME_KEY = new String("Volume");
  public static final String TEST_VOLUME_KEY = new String("TestVolume");
  public static final String VARYING_K_TEST_KEY = new String("VaryingKTest");
  public static final String VARYING_ITERATION_TEST_KEY = new String(
      "VaryingIterationTest");
  public static final String ANISOTROPIC_DIFFUSION_VOLUME_KEY = new String(
      "AnisotropicDiffusionVolume");
  public static final String CTF_CORRECTION_KEY = new String("CtfCorrection");
  public static final String ERASED_FIDUCIALS_KEY = new String(
      "erased fiducials");
  public static final String FLAT_VOLUME_KEY = new String("flattened volume");
  public static final String FINE_ALIGNED_3D_FIND_KEY = new String(
      "fine aligned for findbeads3d");
  public static final String FULL_VOLUME_3D_FIND_KEY = new String(
      "full volume for findbeads3d");
  public static final String SMOOTHING_ASSESSMENT_KEY = new String(
      "Smoothing assessment flattenwarp output");
  public static final String FLATTEN_INPUT_KEY = new String(
      "Flatten input file");
  public static final String FLATTEN_TOOL_OUTPUT_KEY = new String(
      "Flatten tool output file");

  //private keys - used with imodMap
  private static final String rawStackKey = RAW_STACK_KEY;
  private static final String erasedStackKey = ERASED_STACK_KEY;
  private static final String coarseAlignedKey = COARSE_ALIGNED_KEY;
  private static final String fineAlignedKey = FINE_ALIGNED_KEY;
  private static final String sampleKey = SAMPLE_KEY;
  private static final String fullVolumeKey = FULL_VOLUME_KEY;
  private String combinedTomogramKey;
  private static final String fiducialModelKey = FIDUCIAL_MODEL_KEY;
  private static final String trimmedVolumeKey = TRIMMED_VOLUME_KEY;
  private static final String patchVectorModelKey = PATCH_VECTOR_MODEL_KEY;
  private static final String matchCheckKey = MATCH_CHECK_KEY;
  private static final String trialTomogramKey = TRIAL_TOMOGRAM_KEY;
  private static final String mtfFilterKey = MTF_FILTER_KEY;
  private static final String previewKey = PREVIEW_KEY;
  private static final String tomogramKey = TOMOGRAM_KEY;
  private static final String joinSamplesKey = JOIN_SAMPLES_KEY;
  private static final String joinSampleAveragesKey = JOIN_SAMPLE_AVERAGES_KEY;
  private static final String joinKey = JOIN_KEY;
  private static final String rotTomogramKey = ROT_TOMOGRAM_KEY;
  private static final String trialJoinKey = TRIAL_JOIN_KEY;
  private static final String squeezedVolumeKey = SQUEEZED_VOLUME_KEY;
  private static final String patchVectorCCCModelKey = PATCH_VECTOR_CCC_MODEL_KEY;
  private static final String modeledJoinKey = MODELED_JOIN_KEY;
  private static final String transformedModelKey = TRANSFORMED_MODEL_KEY;
  private static final String avgVolKey = AVG_VOL_KEY;
  private static final String refKey = REF_KEY;
  private static final String volumeKey = VOLUME_KEY;
  private static final String testVolumeKey = VOLUME_KEY;
  private static final String varyingKTestKey = VARYING_K_TEST_KEY;
  private static final String varyingIterationTestKey = VARYING_ITERATION_TEST_KEY;
  private static final String anisotropicDiffusionVolumeKey = ANISOTROPIC_DIFFUSION_VOLUME_KEY;
  private static final String ctfCorrectionKey = CTF_CORRECTION_KEY;
  private static final String erasedFiducialsKey = ERASED_FIDUCIALS_KEY;
  private static final String flatVolumeKey = FLAT_VOLUME_KEY;
  private static final String fineAligned3dFindKey = FINE_ALIGNED_3D_FIND_KEY;
  private static final String fullVolume3dFindKey = FULL_VOLUME_3D_FIND_KEY;
  private static final String smoothingAssessmentKey = SMOOTHING_ASSESSMENT_KEY;
  private static final String flattenInputKey = FLATTEN_INPUT_KEY;
  private static final String flattenToolOutputKey = FLATTEN_TOOL_OUTPUT_KEY;

  private boolean useMap = true;
  private final BaseManager manager;
  private ImodRequestHandler requestHandler = null;

  //constructors

  /**
   */
  public ImodManager(BaseManager manager) {
    this.manager = manager;
    imodMap = new HashMap();
    //Only run the request handler when necesary.  In Windows when 3dmod is
    //listening to stdin and it wants to exit, it sends a request to stderr to
    //ask that the stdin receive a stop listening command.  This is because
    //3dmod in Windows can't exit when it is listening to stdin.
    if (Utilities.isWindowsOS()
        && EtomoDirector.INSTANCE.getArguments().isListen()) {
      requestHandler = ImodRequestHandler.getInstance(this);
    }
  }

  //Interface

  /**
   * for running 3dmod from the SetupDialog
   */
  public void setPreviewMetaData(BaseMetaData metaData) {
    if (metaDataSet) {
      return;
    }
    axisType = metaData.getAxisType();
    datasetName = metaData.getDatasetName();
    AxisID axisID;
    createPrivateKeys();
  }

  public void setMetaData(ConstMetaData metaData) {
    //if metaDataSet is true and the axisType is changing from dual to single,
    //combinedTomograms will not be retrievable.  However the global isOpen()
    //and quit() functions will work on them.
    metaDataSet = true;
    axisType = metaData.getAxisType();
    datasetName = metaData.getDatasetName();
    createPrivateKeys();
    if (axisType == AxisType.SINGLE_AXIS) {
      loadSingleAxisMap();
    }
    else {
      loadDualAxisMap();
    }
  }

  public void setMetaData(JoinMetaData metaData) {
    metaDataSet = true;
    axisType = metaData.getAxisType();
    datasetName = metaData.getName();
    if (datasetName.equals("")) {
      new IllegalStateException("DatasetName is empty.").printStackTrace();
    }
    createPrivateKeys();
    loadJoinMap();
  }

  public void setMetaData(ConstPeetMetaData metaData) {
    metaDataSet = true;
    axisType = metaData.getAxisType();
    datasetName = metaData.getName();
    if (datasetName.equals("")) {
      new IllegalStateException("DatasetName is empty.").printStackTrace();
    }
    createPrivateKeys();
    loadPeetMap();
  }

  public void setMetaData(ParallelMetaData metaData) {
    metaDataSet = true;
    axisType = metaData.getAxisType();
    datasetName = metaData.getName();
    if (datasetName.equals("")) {
      new IllegalStateException("DatasetName is empty.").printStackTrace();
    }
    createPrivateKeys();
    loadParallelMap();
  }

  private int newImod(String key) throws AxisTypeException {
    return newImod(key, (AxisID) null, null);
  }

  public int newImod(String key, AxisID axisID) throws AxisTypeException {
    return newImod(key, axisID, null);
  }

  public int newImod(String key, AxisID axisID, String datasetName)
      throws AxisTypeException {
    Vector vector;
    ImodState imodState;
    key = getPrivateKey(key);
    vector = getVector(key, axisID);
    if (vector == null) {
      vector = newVector(key, axisID, datasetName);
      if (axisID == null) {
        imodMap.put(key, vector);
      }
      else {
        imodMap.put(key + axisID.getExtension(), vector);
      }
      return 0;
    }
    imodState = newImodState(key, axisID, datasetName);
    vector.add(imodState);
    return vector.lastIndexOf(imodState);
  }

  public int newImod(String key, File file) throws AxisTypeException {
    Vector vector;
    ImodState imodState;
    key = getPrivateKey(key);
    vector = getVector(key);
    if (vector == null) {
      vector = newVector(key, file);
      imodMap.put(key, vector);
      return 0;
    }
    imodState = newImodState(key, file);
    vector.add(imodState);
    return vector.lastIndexOf(imodState);
  }

  public int newImod(String key, String[] fileNameArray)
      throws AxisTypeException {
    Vector vector;
    ImodState imodState;
    key = getPrivateKey(key);
    vector = getVector(key);
    if (vector == null) {
      vector = newVector(key, fileNameArray);
      imodMap.put(key, vector);
      return 0;
    }
    imodState = newImodState(key, fileNameArray);
    vector.add(imodState);
    return vector.lastIndexOf(imodState);
  }

  public int newImod(String key, String[] fileNameArray, String subdirName)
      throws AxisTypeException {
    Vector vector;
    ImodState imodState;
    key = getPrivateKey(key);
    vector = getVector(key);
    if (vector == null) {
      vector = newVector(key, fileNameArray, subdirName);
      imodMap.put(key, vector);
      return 0;
    }
    imodState = newImodState(key, fileNameArray, subdirName);
    vector.add(imodState);
    if (debug) {
      System.out.println("vector=" + vector);
    }
    return vector.lastIndexOf(imodState);
  }

  public void open(String key) throws AxisTypeException,
      SystemProcessException, IOException {
    open(key, null, null, null);
  }

  public void open(String key, Run3dmodMenuOptions menuOptions)
      throws AxisTypeException, SystemProcessException, IOException {
    open(key, null, null, menuOptions);
    //used for:
    //openCombinedTomogram
  }

  public void open(String key, String model, Run3dmodMenuOptions menuOptions)
      throws AxisTypeException, SystemProcessException, IOException {
    open(key, null, model, menuOptions);
    //used for:
    //openCombinedTomogram
  }

  public void open(String key, AxisID axisID, Run3dmodMenuOptions menuOptions)
      throws AxisTypeException, SystemProcessException, IOException {
    open(key, axisID, null, menuOptions);
  }

  public void open(String key, AxisID axisID, String model)
      throws AxisTypeException, SystemProcessException, IOException {
    open(key, axisID, model, new Run3dmodMenuOptions());
  }

  public void open(String key, AxisID axisID) throws AxisTypeException,
      SystemProcessException, IOException {
    open(key, axisID, null, new Run3dmodMenuOptions());
  }

  public void open(String key, File file, Run3dmodMenuOptions menuOptions)
      throws AxisTypeException, SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key);
    if (imodState == null) {
      newImod(key, file);
      imodState = get(key);
    }
    if (imodState != null) {
      imodState.open(menuOptions);
    }
  }

  public void open(String key, File file, Run3dmodMenuOptions menuOptions,
      boolean swapYZ) throws AxisTypeException, SystemProcessException,
      IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key);
    if (imodState == null) {
      newImod(key, file);
      imodState = get(key);
    }
    if (imodState != null) {
      imodState.setSwapYZ(swapYZ);
      imodState.open(menuOptions);
    }
  }

  public void open(String key, AxisID axisID, String model,
      Run3dmodMenuOptions menuOptions) throws AxisTypeException,
      SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      if (model == null) {
        imodState.open(menuOptions);
      }
      else {
        imodState.open(model, menuOptions);
      }
    }
  }

  public void setOpenModelView(String key, AxisID axisID)
      throws AxisTypeException, IOException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setOpenModelView();
    }
  }

  public void open(String key, String[] fileNameArray,
      Run3dmodMenuOptions menuOptions) throws AxisTypeException,
      SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, AxisID.ONLY);
    if (imodState == null || !imodState.equalsFileNameArray(fileNameArray)) {
      newImod(key, fileNameArray);
      imodState = get(key, AxisID.ONLY);
    }
    if (imodState != null) {
      imodState.open(menuOptions);
    }
  }

  public void open(String key, String[] fileNameArray,
      Run3dmodMenuOptions menuOptions, String subdirName, boolean swapYZ)
      throws AxisTypeException, SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, AxisID.ONLY);
    if (imodState == null || !imodState.equalsSubdirName(subdirName)
        || !imodState.equalsFileNameArray(fileNameArray)) {
      newImod(key, fileNameArray, subdirName);
      imodState = get(key, AxisID.ONLY);
    }
    if (imodState != null) {
      imodState.setSwapYZ(swapYZ);
      imodState.open(menuOptions);
    }
  }

  /**
   * 
   * @param key
   * @param axisID
   * @param model
   * @param modelMode
   * @throws AxisTypeException
   * @throws SystemProcessException
   */
  public void open(String key, AxisID axisID, String model, boolean modelMode,
      Run3dmodMenuOptions menuOptions) throws AxisTypeException,
      SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    //TEMP
    System.err.println("key=" + key + ",axis=" + imodState.getAxisID());
    if (imodState != null) {
      imodState.open(model, modelMode, menuOptions);
    }
    //    rawStack.model(modelName, modelMode);
  }

  /**
   * 
   * @param key
   * @param axisID
   * @param model
   * @param modelMode
   * @throws AxisTypeException
   * @throws SystemProcessException
   */
  public void open(String key, File file, String model, boolean modelMode,
      Run3dmodMenuOptions menuOptions) throws AxisTypeException,
      SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key);
    if (imodState == null) {
      newImod(key, file);
      imodState = get(key);
    }
    //TEMP
    System.err.println("key=" + key + ",axis=" + imodState.getAxisID());
    if (imodState != null) {
      imodState.open(model, modelMode, menuOptions);
    }
  }

  /**
   * 
   * @param key
   * @param axisID
   * @param vectorIndex
   * @throws AxisTypeException
   * @throws SystemProcessException
   */
  public void open(String key, AxisID axisID, int vectorIndex,
      Run3dmodMenuOptions menuOptions) throws AxisTypeException,
      SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID, vectorIndex);
    if (imodState == null) {
      throw new IllegalArgumentException(key + " was not created in "
          + axisType.toString() + " with axisID=" + axisID.getExtension()
          + " at index " + vectorIndex);
    }
    imodState.open(menuOptions);
  }

  public void open(String key, int vectorIndex, Run3dmodMenuOptions menuOptions)
      throws AxisTypeException, SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, vectorIndex);
    if (imodState == null) {
      throw new IllegalArgumentException(key + " was not created in "
          + axisType.toString() + " at index " + vectorIndex);
    }
    imodState.open(menuOptions);
  }

  public void open(String key, int vectorIndex, String model,
      boolean modelMode, Run3dmodMenuOptions menuOptions)
      throws AxisTypeException, SystemProcessException, IOException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, vectorIndex);
    if (imodState == null) {
      throw new IllegalArgumentException(key + " was not created in "
          + axisType.toString() + " at index " + vectorIndex);
    }
    imodState.open(model, modelMode, menuOptions);
  }

  public void delete(String key, int vectorIndex) throws AxisTypeException,
      IOException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, vectorIndex);
    if (imodState == null) {
      throw new IllegalArgumentException(key + " was not created in "
          + axisType.toString() + " at index " + vectorIndex);
    }
    imodState.quit();
    deleteImodState(key, vectorIndex);
  }

  public boolean isOpen(String key) throws AxisTypeException {
    return isOpen(key, null);
  }

  public boolean isOpen(String key, AxisID axisID) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return false;
    }
    return imodState.isOpen();
  }

  public boolean isOpen(String key, AxisID axisID, String datasetName)
      throws AxisTypeException {
    if (key==null) {
      return false;
    }
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID, datasetName);
    if (imodState == null) {
      return false;
    }
    return imodState.isOpen();
  }

  public boolean isOpen() throws AxisTypeException {
    if (imodMap.size() == 0) {
      return false;
    }
    Set set = imodMap.keySet();
    Iterator iterator = set.iterator();
    while (iterator.hasNext()) {
      Vector vector = getVector((String) iterator.next(), true);
      Iterator vectorInterator = vector.iterator();
      while (vectorInterator.hasNext()) {
        ImodState imodState = (ImodState) vectorInterator.next();
        if (imodState != null && imodState.isOpen()) {
          return true;
        }
      }
    }
    return false;
  }

  public String getModelName(String key, AxisID axisID)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return "";
    }
    return imodState.getModelName();
  }

  public Vector getRubberbandCoordinates(String key) throws AxisTypeException,
      IOException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key);
    if (imodState == null) {
      UIHarness.INSTANCE.openMessageDialog(manager, "3dmod is not running.",
          "3dmod Warning", AxisID.ONLY);
      return null;
    }
    return imodState.getRubberbandCoordinates();
  }

  public Vector getSlicerAngles(String key, int vectorIndex)
      throws AxisTypeException, IOException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, vectorIndex);
    if (imodState == null || !imodState.isOpen()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "3dmod is not running.",
          "3dmod Warning", AxisID.ONLY);
      return null;
    }
    return imodState.getSlicerAngles();
  }

  public void quit(String key) throws AxisTypeException, IOException,
      SystemProcessException {
    quit(key, null);
  }

  public void quit(String key, AxisID axisID) throws AxisTypeException,
      IOException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState != null) {
      imodState.quit();
    }
  }

  public void quit(String key, AxisID axisID, String datasetName)
      throws AxisTypeException, IOException, SystemProcessException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID, datasetName);
    if (imodState != null) {
      imodState.quit();
    }
  }

  public void quitAll(String key, AxisID axisID) throws AxisTypeException,
      IOException, SystemProcessException {
    Vector imodStateVector = getVector(getPrivateKey(key), axisID);
    if (imodStateVector == null || imodStateVector.size() == 0) {
      return;
    }
    for (int i = 0; i < imodStateVector.size(); i++) {
      ImodState imodState = (ImodState) imodStateVector.get(i);
      if (imodState != null && imodState.isOpen()) {
        imodState.quit();
        try {
          Thread.sleep(500);
        }
        catch (InterruptedException e) {
        }
      }
    }
  }

  public void quit() throws AxisTypeException, IOException,
      SystemProcessException {
    if (imodMap.size() == 0) {
      return;
    }
    Set set = imodMap.keySet();
    Iterator iterator = set.iterator();
    while (iterator.hasNext()) {
      Vector vector = getVector((String) iterator.next(), true);
      int size = vector.size();
      for (int i = 0; i < size; i++) {
        ImodState imodState = (ImodState) vector.get(i);
        if (imodState != null) {
          imodState.quit();
        }
      }
    }
  }

  void processRequest() throws AxisTypeException {
    if (imodMap.size() == 0) {
      return;
    }
    Set set = imodMap.keySet();
    Iterator iterator = set.iterator();
    while (iterator.hasNext()) {
      Vector vector = getVector((String) iterator.next(), true);
      int size = vector.size();
      for (int i = 0; i < size; i++) {
        ImodState imodState = (ImodState) vector.get(i);
        if (imodState != null) {
          imodState.processRequest();
        }
      }
    }
  }

  public void disconnect() {
    if (imodMap.size() == 0) {
      return;
    }
    Set set = imodMap.keySet();
    Iterator iterator = set.iterator();
    while (iterator.hasNext()) {
      try {
        Vector vector = getVector((String) iterator.next(), true);
        int size = vector.size();
        for (int i = 0; i < size; i++) {
          try {
            ImodState imodState = (ImodState) vector.get(i);
            if (imodState != null && imodState.isOpen()) {
              imodState.disconnect();
            }
          }
          catch (Throwable e) {
            e.printStackTrace();
          }
        }
      }
      catch (Throwable e) {
        e.printStackTrace();
      }
    }
  }

  public void setSwapYZ(String key, AxisID axisID, boolean swapYZ)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    imodState.setSwapYZ(swapYZ);
  }

  public void setSwapYZ(String key, File file, boolean swapYZ)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key);
    if (imodState == null) {
      newImod(key, file);
      imodState = get(key);
    }
    imodState.setSwapYZ(swapYZ);
  }

  public void setOpenBeadFixer(String key, AxisID axisID, boolean openBeadFixer)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return;
    }
    if (imodState.isUseModv()) {
      throw new UnsupportedOperationException(
          "The Bead Fixer cannot be opened in 3dmodv");
    }
    imodState.setOpenBeadFixer(openBeadFixer);
  }

  public void setAutoCenter(String key, AxisID axisID, boolean autoCenter)
      throws AxisTypeException {
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return;
    }
    imodState.setAutoCenter(autoCenter);
  }

  public void setSkipList(String key, AxisID axisID, String skipList)
      throws AxisTypeException {
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return;
    }
    imodState.setSkipList(skipList);
  }

  public void setDeleteAllSections(String key, AxisID axisID, boolean on)
      throws AxisTypeException {
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return;
    }
    imodState.setDeleteAllSections(on);
  }

  public void setBeadfixerMode(String key, AxisID axisID,
      ImodProcess.BeadFixerMode mode) throws AxisTypeException {
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return;
    }
    imodState.setBeadfixerMode(mode);
  }

  public void setOpenLog(String key, AxisID axisID, boolean openLog,
      String logName) throws AxisTypeException {
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return;
    }
    imodState.setOpenLog(openLog, logName);
  }

  public void reopenLog(String key, AxisID axisID) throws AxisTypeException,
      SystemProcessException, IOException {
    ImodState imodState = get(key, axisID);
    if (imodState == null || !imodState.isOpen()) {
      return;
    }
    imodState.reopenLog();
  }

  public void setOpenLogOff(String key, AxisID axisID) throws AxisTypeException {
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return;
    }
    imodState.setOpenLogOff();
  }

  public void setNewContours(String key, AxisID axisID, boolean newContours)
      throws AxisTypeException {
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      return;
    }
    imodState.setNewContours(newContours);
  }

  public void setBinning(String key, AxisID axisID, int binning)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setBinning(binning);
    }
  }

  public void setTiltFile(String key, AxisID axisID, String tiltFile)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setTiltFile(tiltFile);
    }
  }

  public void resetTiltFile(String key, AxisID axisID) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.resetTiltFile();
    }
  }

  public void setBinning(String key, int binning) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key);
    if (imodState == null) {
      newImod(key);
      imodState = get(key);
    }
    if (imodState != null) {
      imodState.setBinning(binning);
    }
  }

  public void setBinning(String key, int vectorIndex, int binning)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, vectorIndex);
    if (imodState == null) {
      throw new IllegalArgumentException(key + " was not created in "
          + axisType.toString() + " at index " + vectorIndex);
    }
    imodState.setBinning(binning);
  }

  public void setBinningXY(String key, int binning) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key);
    if (imodState == null) {
      newImod(key);
      imodState = get(key);
    }
    if (imodState != null) {
      imodState.setBinningXY(binning);
    }
  }

  public void setContinuousListenerTarget(String key, AxisID axisID,
      ContinuousListenerTarget continuousListenerTarget)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    imodState.setContinuousListenerTarget(continuousListenerTarget);
  }

  public void setBinningXY(String key, int vectorIndex, int binning)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, vectorIndex);
    if (imodState == null) {
      throw new IllegalArgumentException(key + " was not created in "
          + axisType.toString() + " at index " + vectorIndex);
    }
    imodState.setBinningXY(binning);
  }

  public void setOpenContours(String key, AxisID axisID, boolean openContours)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setOpenContours(openContours);
    }
  }

  public void setStartNewContoursAtNewZ(String key, AxisID axisID,
      boolean startNewContoursAtNewZ) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setStartNewContoursAtNewZ(startNewContoursAtNewZ);
    }
  }

  public void setPointLimit(String key, AxisID axisID, int pointLimit)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setPointLimit(pointLimit);
    }
  }

  public void setPreserveContrast(String key, AxisID axisID,
      boolean preserveContrast) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setPreserveContrast(preserveContrast);
    }
  }

  public void setFrames(String key, AxisID axisID, boolean frames)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setFrames(frames);
    }
  }

  public void setPieceListFileName(String key, AxisID axisID,
      String pieceListFileName) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState == null) {
      newImod(key, axisID);
      imodState = get(key, axisID);
    }
    if (imodState != null) {
      imodState.setPieceListFileName(pieceListFileName);
    }
  }

  public void setWorkingDirectory(String key, AxisID axisID, int vectorIndex,
      File workingDirectory) throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID, vectorIndex);
    if (imodState != null) {
      imodState.setWorkingDirectory(workingDirectory);
    }
  }

  public void stopRequestHandler() {
    if (requestHandler != null) {
      requestHandler.stop();
    }
  }

  /**
   * Used to prevent warnings about a stale file from popping up over and over.
   * Returns true if ImodState.isWarnedStaleFile returns false.  Also turns on
   * ImodState.warnedStaleFile if it is off.
   * @param key
   * @param axisID
   * @return
   * @throws AxisTypeException
   */
  public boolean warnStaleFile(String key, AxisID axisID)
      throws AxisTypeException {
    key = getPrivateKey(key);
    ImodState imodState = get(key, axisID);
    if (imodState != null && !imodState.isWarnedStaleFile()
        && imodState.isOpen()) {
      imodState.setWarnedStaleFile(true);
      return true;
    }
    return false;
  }

  //protected methods

  protected Vector newVector(ImodState imodState) {
    Vector vector = new Vector(1);
    vector.add(imodState);
    return vector;
  }

  protected Vector newVector(String key) {
    return newVector(newImodState(key));
  }

  protected Vector newVector(String key, AxisID axisID) {
    return newVector(newImodState(key, axisID));
  }

  protected Vector newVector(String key, AxisID axisID, String datasetName) {
    return newVector(newImodState(key, axisID, datasetName));
  }

  protected Vector newVector(String key, File file) {
    return newVector(newImodState(key, file));
  }

  protected Vector newVector(String key, String[] fileNameArray) {
    return newVector(newImodState(key, fileNameArray));
  }

  protected Vector newVector(String key, String[] fileNameArray,
      String subdirName) {
    return newVector(newImodState(key, fileNameArray, subdirName));
  }

  protected ImodState newImodState(String key) {
    return newImodState(key, null, null, null, null, null);
  }

  ImodState newImodState(String key, AxisID axisID) {
    return newImodState(key, axisID, null, null, null, null);
  }

  ImodState newImodState(String key, AxisID axisID, String datasetName) {
    return newImodState(key, axisID, datasetName, null, null, null);
  }

  ImodState newImodState(String key, File file) {
    return newImodState(key, null, null, file, null, null);
  }

  ImodState newImodState(String key, String[] fileNameArray) {
    return newImodState(key, null, null, null, fileNameArray, null);
  }

  ImodState newImodState(String key, String[] fileNameArray, String subdirName) {
    return newImodState(key, null, null, null, fileNameArray, subdirName);
  }

  ImodState newImodState(String key, AxisID axisID, String datasetName,
      File file, String[] fileNameArray, String subdirName) {
    if (key.equals(RAW_STACK_KEY) && axisID != null) {
      return newRawStack(axisID);
    }
    if (key.equals(ERASED_STACK_KEY) && axisID != null) {
      return newErasedStack(axisID);
    }
    if (key.equals(COARSE_ALIGNED_KEY) && axisID != null) {
      return newCoarseAligned(axisID);
    }
    if (key.equals(FINE_ALIGNED_KEY) && axisID != null) {
      return newFineAligned(axisID);
    }
    if (key.equals(SAMPLE_KEY) && axisID != null) {
      return newSample(axisID);
    }
    if (key.equals(FULL_VOLUME_KEY) && axisID != null) {
      return newFullVolume(axisID);
    }
    if (key.equals(COMBINED_TOMOGRAM_KEY) && axisType == AxisType.DUAL_AXIS) {
      return newCombinedTomogram();
    }
    if (key.equals(PATCH_VECTOR_MODEL_KEY) && axisType == AxisType.DUAL_AXIS) {
      return newPatchVectorModel();
    }
    if (key.equals(MATCH_CHECK_KEY) && axisType == AxisType.DUAL_AXIS) {
      return newMatchCheck();
    }
    if (key.equals(FIDUCIAL_MODEL_KEY) && axisID != null) {
      return newFiducialModel(axisID);
    }
    if (key.equals(TRIMMED_VOLUME_KEY)) {
      return newTrimmedVolume();
    }
    if (key.equals(TRIAL_TOMOGRAM_KEY) && axisID != null && datasetName != null) {
      return newTrialTomogram(axisID, datasetName);
    }
    if (key.equals(MTF_FILTER_KEY) && axisID != null) {
      return newMtfFilter(axisID);
    }
    if (key.equals(PREVIEW_KEY) && axisID != null) {
      return newPreview(axisID);
    }
    if (key.equals(TOMOGRAM_KEY)) {
      return newTomogram(file, axisID);
    }
    if (key.equals(JOIN_SAMPLES_KEY)) {
      return newJoinSamples();
    }
    if (key.equals(JOIN_SAMPLE_AVERAGES_KEY)) {
      return newJoinSampleAverages();
    }
    if (key.equals(JOIN_KEY)) {
      return newJoin();
    }
    if (key.equals(ROT_TOMOGRAM_KEY)) {
      return newRotTomogram(file);
    }
    if (key.equals(TRIAL_JOIN_KEY)) {
      return newTrialJoin();
    }
    if (key.equals(MODELED_JOIN_KEY)) {
      return newModeledJoin();
    }
    if (key.equals(SQUEEZED_VOLUME_KEY)) {
      return newSqueezedVolume();
    }
    if (key.equals(PATCH_VECTOR_CCC_MODEL_KEY)
        && axisType == AxisType.DUAL_AXIS) {
      return newPatchVectorCCCModel();
    }
    if (key.equals(TRANSFORMED_MODEL_KEY)) {
      return newTransformedModel();
    }
    if (key.equals(AVG_VOL_KEY)) {
      return newAvgVol(fileNameArray);
    }
    if (key.equals(REF_KEY)) {
      return newRef(fileNameArray);
    }
    if (key.equals(VOLUME_KEY)) {
      return newVolume(file);
    }
    if (key.equals(TEST_VOLUME_KEY)) {
      return newTestVolume(file);
    }
    if (key.equals(VARYING_K_TEST_KEY)) {
      return newVaryingKTest(fileNameArray, subdirName);
    }
    if (key.equals(VARYING_ITERATION_TEST_KEY)) {
      return newVaryingIterationTest(fileNameArray, subdirName);
    }
    if (key.equals(ANISOTROPIC_DIFFUSION_VOLUME_KEY)) {
      return newAnisotropicDiffusionVolume(file);
    }
    if (key.equals(CTF_CORRECTION_KEY) && axisID != null) {
      return newCtfCorrection(axisID);
    }
    if (key.equals(ERASED_FIDUCIALS_KEY) && axisID != null) {
      return newErasedFiducials(axisID);
    }
    if (key.equals(FLAT_VOLUME_KEY) && axisID != null) {
      return newFlatVolume(axisID);
    }
    if (key.equals(FINE_ALIGNED_3D_FIND_KEY) && axisID != null) {
      return newFineAligned3dFind(axisID);
    }
    if (key.equals(FULL_VOLUME_3D_FIND_KEY) && axisID != null) {
      return newFullVolume3dFind(axisID);
    }
    if (key.equals(SMOOTHING_ASSESSMENT_KEY) && axisID != null) {
      return newSmoothingAssessment(axisID);
    }
    if (key.equals(FLATTEN_INPUT_KEY)) {
      return newFlattenInput(file);
    }
    if (key.equals(FLATTEN_TOOL_OUTPUT_KEY) && axisID != null) {
      return newFlattenToolOutput(axisID);
    }
    System.out.println("key=" + key);
    throw new IllegalArgumentException(key + " cannot be created in "
        + axisType.toString() + " with axisID=" + axisID.getExtension());
  }

  protected void createPrivateKeys() {
    if (axisType == AxisType.SINGLE_AXIS) {
      combinedTomogramKey = FULL_VOLUME_KEY;
    }
    else {
      combinedTomogramKey = COMBINED_TOMOGRAM_KEY;
    }
  }

  protected String getPrivateKey(String publicKey) {
    if (publicKey.equals(COMBINED_TOMOGRAM_KEY)) {
      return combinedTomogramKey;
    }
    else
      return publicKey;
  }

  protected void loadSingleAxisMap() {
    imodMap.put(rawStackKey, newVector(newRawStack(AxisID.ONLY)));
    imodMap.put(erasedStackKey, newVector(newErasedStack(AxisID.ONLY)));
    imodMap.put(coarseAlignedKey, newVector(newCoarseAligned(AxisID.ONLY)));
    imodMap.put(fineAlignedKey, newVector(newFineAligned(AxisID.ONLY)));
    imodMap.put(sampleKey, newVector(newSample(AxisID.ONLY)));
    imodMap.put(fullVolumeKey, newVector(newFullVolume(AxisID.ONLY)));
    imodMap.put(fiducialModelKey, newVector(newFiducialModel(AxisID.ONLY)));
    imodMap.put(trimmedVolumeKey, newVector(newTrimmedVolume()));
    imodMap.put(mtfFilterKey, newVector(newMtfFilter(AxisID.ONLY)));
    imodMap.put(squeezedVolumeKey, newVector(newSqueezedVolume()));
  }

  protected void loadJoinMap() {
    imodMap.put(joinSamplesKey, newVector(newJoinSamples()));
    imodMap.put(joinSampleAveragesKey, newVector(newJoinSampleAverages()));
    imodMap.put(joinKey, newVector(newJoin()));
    imodMap.put(trialJoinKey, newVector(newTrialJoin()));
  }

  protected void loadPeetMap() {
  }

  protected void loadParallelMap() {
  }

  protected void loadDualAxisMap() {
    imodMap.put(rawStackKey + AxisID.FIRST.getExtension(),
        newVector(newRawStack(AxisID.FIRST)));
    imodMap.put(rawStackKey + AxisID.SECOND.getExtension(),
        newVector(newRawStack(AxisID.SECOND)));
    imodMap.put(erasedStackKey + AxisID.FIRST.getExtension(),
        newVector(newErasedStack(AxisID.FIRST)));
    imodMap.put(erasedStackKey + AxisID.SECOND.getExtension(),
        newVector(newErasedStack(AxisID.SECOND)));
    imodMap.put(coarseAlignedKey + AxisID.FIRST.getExtension(),
        newVector(newCoarseAligned(AxisID.FIRST)));
    imodMap.put(coarseAlignedKey + AxisID.SECOND.getExtension(),
        newVector(newCoarseAligned(AxisID.SECOND)));
    imodMap.put(fineAlignedKey + AxisID.FIRST.getExtension(),
        newVector(newFineAligned(AxisID.FIRST)));
    imodMap.put(fineAlignedKey + AxisID.SECOND.getExtension(),
        newVector(newFineAligned(AxisID.SECOND)));
    imodMap.put(sampleKey + AxisID.FIRST.getExtension(),
        newVector(newSample(AxisID.FIRST)));
    imodMap.put(sampleKey + AxisID.SECOND.getExtension(),
        newVector(newSample(AxisID.SECOND)));
    imodMap.put(fullVolumeKey + AxisID.FIRST.getExtension(),
        newVector(newFullVolume(AxisID.FIRST)));
    imodMap.put(fullVolumeKey + AxisID.SECOND.getExtension(),
        newVector(newFullVolume(AxisID.SECOND)));
    imodMap.put(combinedTomogramKey, newVector(newCombinedTomogram()));
    imodMap.put(patchVectorModelKey, newVector(newPatchVectorModel()));
    imodMap.put(matchCheckKey, newVector(newMatchCheck()));
    imodMap.put(fiducialModelKey + AxisID.FIRST.getExtension(),
        newVector(newFiducialModel(AxisID.FIRST)));
    imodMap.put(fiducialModelKey + AxisID.SECOND.getExtension(),
        newVector(newFiducialModel(AxisID.SECOND)));
    imodMap.put(trimmedVolumeKey, newVector(newTrimmedVolume()));
    imodMap.put(mtfFilterKey + AxisID.FIRST.getExtension(),
        newVector(newMtfFilter(AxisID.FIRST)));
    imodMap.put(mtfFilterKey + AxisID.SECOND.getExtension(),
        newVector(newMtfFilter(AxisID.SECOND)));
    imodMap.put(squeezedVolumeKey, newVector(newSqueezedVolume()));
    imodMap.put(patchVectorCCCModelKey, newVector(newPatchVectorCCCModel()));
  }

  private ImodState newRawStack(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, datasetName, ".st");
    return imodState;
  }

  private ImodState newErasedStack(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, datasetName,
        "_fixed.st");
    return imodState;
  }

  private ImodState newCoarseAligned(AxisID axisID) {
    ImodState imodState;
    imodState = new ImodState(manager, axisID, datasetName, ".preali");
    return imodState;
  }

  private ImodState newFineAligned(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, datasetName, ".ali");
    return imodState;
  }

  private ImodState newSample(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, "top", "mid", "bot",
        ".rec", "tomopitch", ".mod");
    imodState.setInitialMode(ImodState.MODEL_MODE);
    return imodState;
  }

  private ImodState newFullVolume(AxisID axisID) {
    ImodState imodState;
    if (axisType == AxisType.SINGLE_AXIS) {
      imodState = new ImodState(manager, datasetName + "_full.rec", axisID);
    }
    else {
      imodState = new ImodState(manager, axisID, datasetName, ".rec");
    }
    imodState.setAllowMenuBinningInZ(true);
    imodState.setInitialSwapYZ(true);
    return imodState;
  }

  private ImodState newCombinedTomogram() {
    ImodState imodState = new ImodState(manager, "sum.rec", AxisID.ONLY);
    imodState.setAllowMenuBinningInZ(true);
    imodState.setInitialSwapYZ(true);
    return imodState;
  }

  private ImodState newPatchVectorModel() {
    ImodState imodState = new ImodState(manager,
        DatasetFiles.PATCH_VECTOR_MODEL, ImodState.MODEL_VIEW, AxisID.ONLY,
        ImodProcess.WindowOpenOption.IMODV_OBJECTS);
    imodState.setInitialMode(ImodState.MODEL_MODE);
    imodState.setNoMenuOptions(true);
    return imodState;
  }

  private ImodState newPatchVectorCCCModel() {
    ImodState imodState = new ImodState(manager,
        DatasetFiles.PATCH_VECTOR_CCC_MODEL, ImodState.MODV, AxisID.ONLY,
        ImodProcess.WindowOpenOption.IMODV_OBJECTS);
    imodState.setNoMenuOptions(true);
    return imodState;
  }

  private ImodState newTransformedModel() {
    ImodState imodState = new ImodState(manager, DatasetFiles
        .getRefineAlignedModelFileName(manager), ImodState.MODV, AxisID.ONLY);
    imodState.setNoMenuOptions(true);
    return imodState;
  }

  private ImodState newMatchCheck() {
    ImodState imodState = new ImodState(manager, "matchcheck.mat",
        "matchcheck.rec", AxisID.ONLY);
    imodState.setAllowMenuBinningInZ(true);
    imodState.setInitialSwapYZ(true);
    return imodState;
  }

  private ImodState newFiducialModel(AxisID axisID) {
    ImodState imodState = new ImodState(manager, ImodState.MODV, axisID);
    imodState.setNoMenuOptions(true);
    return imodState;
  }

  private ImodState newTrimmedVolume() {
    ImodState imodState = new ImodState(manager, datasetName + ".rec",
        AxisID.ONLY);
    imodState.setAllowMenuBinningInZ(true);
    return imodState;
  }

  private ImodState newTrialTomogram(AxisID axisID, String datasetName) {
    ImodState imodState = new ImodState(manager, datasetName, axisID);
    imodState.setAllowMenuBinningInZ(true);
    imodState.setInitialSwapYZ(true);
    return imodState;
  }

  private ImodState newMtfFilter(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, datasetName,
        "_filt.ali");
    return imodState;
  }

  private ImodState newCtfCorrection(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, datasetName,
        DatasetFiles.CTF_CORRECTION_EXT);
    return imodState;
  }

  private ImodState newErasedFiducials(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, datasetName,
        DatasetFiles.getErasedFiducialsFileExtension());
    return imodState;
  }

  private ImodState newFlatVolume(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, datasetName,
        ImageFileType.FLATTEN_OUTPUT.getExtension());
    return imodState;
  }

  private ImodState newFlattenToolOutput(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID,
        FileType.FLATTEN_TOOL_OUTPUT.getFileName(manager, axisID));
    return imodState;
  }

  private ImodState newPreview(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID, datasetName, ".st");
    return imodState;
  }

  private ImodState newTomogram(File file, AxisID axisID) {
    ImodState imodState = new ImodState(manager, file, axisID);
    return imodState;
  }

  private ImodState newJoinSamples() {
    if (datasetName.equals("")) {
      new IllegalStateException("DatasetName is empty.").printStackTrace();
      System.err.println("manager=" + manager);
    }
    ImodState imodState = new ImodState(manager, datasetName + ".sample",
        AxisID.ONLY);
    return imodState;
  }

  private ImodState newJoinSampleAverages() {
    if (datasetName.equals("")) {
      new IllegalStateException("DatasetName is empty.").printStackTrace();
      System.err.println("manager=" + manager);
    }
    ImodState imodState = new ImodState(manager, datasetName + ".sampavg",
        AxisID.ONLY);
    return imodState;
  }

  private ImodState newJoin() {
    ImodState imodState = new ImodState(manager, datasetName + ".join",
        AxisID.ONLY);
    return imodState;
  }

  private ImodState newRotTomogram(File file) {
    ImodState imodState = new ImodState(manager, file, AxisID.ONLY);
    return imodState;
  }

  private ImodState newAvgVol(String[] fileNameArray) {
    ImodState imodState = new ImodState(manager, fileNameArray, AxisID.ONLY);
    imodState.setModelViewType(ImodState.MODEL_VIEW);
    imodState.setOpenZap();
    imodState.addWindowOpenOption(ImodProcess.WindowOpenOption.ISOSURFACE);
    return imodState;
  }

  private ImodState newRef(String[] fileNameArray) {
    ImodState imodState = new ImodState(manager, fileNameArray, AxisID.ONLY);
    return imodState;
  }

  private ImodState newVolume(File file) {
    ImodState imodState = new ImodState(manager, file, AxisID.ONLY);
    return imodState;
  }

  private ImodState newTestVolume(File file) {
    ImodState imodState = new ImodState(manager, file, AxisID.ONLY);
    return imodState;
  }

  private ImodState newVaryingKTest(String[] fileNameArray, String subdirName) {
    ImodState imodState = new ImodState(manager, fileNameArray, AxisID.ONLY,
        subdirName);
    return imodState;
  }

  private ImodState newVaryingIterationTest(String[] fileNameArray,
      String subdirName) {
    ImodState imodState = new ImodState(manager, fileNameArray, AxisID.ONLY,
        subdirName);
    return imodState;
  }

  private ImodState newAnisotropicDiffusionVolume(File file) {
    ImodState imodState = new ImodState(manager, file, AxisID.ONLY);
    return imodState;
  }

  private ImodState newModeledJoin() {
    ImodState imodState = new ImodState(manager, datasetName + "_modeled.join",
        AxisID.ONLY);
    imodState.setInitialMode(ImodState.MODEL_MODE);
    imodState.setOpenContours(true);
    return imodState;
  }

  private ImodState newTrialJoin() {
    ImodState imodState = new ImodState(manager, datasetName + "_trial.join",
        AxisID.ONLY);
    return imodState;
  }

  private ImodState newSqueezedVolume() {
    ImodState imodState = new ImodState(manager, datasetName + ".sqz",
        AxisID.ONLY);
    imodState.setAllowMenuBinningInZ(true);
    return imodState;
  }

  private ImodState newFineAligned3dFind(AxisID axisID) {
    //FileType.NEWST_3D_FIND_OUTPUT is the same as FileType.BLEND_3D_FIND_OUTPUT.
    ImodState imodState = new ImodState(manager, axisID,
        FileType.NEWST_OR_BLEND_3D_FIND_OUTPUT.getFileName(manager, axisID));
    return imodState;
  }

  private ImodState newFullVolume3dFind(AxisID axisID) {
    ImodState imodState = new ImodState(manager, axisID,
        FileType.TILT_3D_FIND_OUTPUT.getFileName(manager, axisID));
    imodState.setAllowMenuBinningInZ(true);
    imodState.setInitialSwapYZ(true);
    return imodState;
  }

  private ImodState newSmoothingAssessment(AxisID axisID) {
    ImodState imodState = new ImodState(manager, ImodState.MODV, axisID);
    imodState.setNoMenuOptions(true);
    imodState.addWindowOpenOption(ImodProcess.WindowOpenOption.OBJECT_LIST);
    return imodState;
  }

  private ImodState newFlattenInput(File file) {
    return new ImodState(manager, file, AxisID.ONLY);
  }

  private boolean isPerAxis(String key) {
    if (key.equals(COMBINED_TOMOGRAM_KEY) || key.equals(PATCH_VECTOR_MODEL_KEY)
        || key.equals(MATCH_CHECK_KEY) || key.equals(TRIMMED_VOLUME_KEY)) {
      return false;
    }
    return true;
  }

  private boolean isDualAxisOnly(String key) {
    if (key.equals(COMBINED_TOMOGRAM_KEY) || key.equals(PATCH_VECTOR_MODEL_KEY)
        || key.equals(MATCH_CHECK_KEY)) {
      return true;
    }
    return false;
  }

  protected ImodState get(String key) throws AxisTypeException {
    Vector vector = getVector(key);
    if (vector == null) {
      return null;
    }
    return (ImodState) vector.lastElement();
  }

  protected ImodState get(String key, AxisID axisID) throws AxisTypeException {
    Vector vector = getVector(key, axisID);
    if (vector == null) {
      return null;
    }
    return (ImodState) vector.lastElement();
  }

  protected ImodState get(String key, AxisID axisID, String datasetName)
      throws AxisTypeException {
    Vector vector = getVector(key, axisID);
    if (vector == null) {
      return null;
    }
    Iterator iterator = vector.iterator();
    while (iterator.hasNext()) {
      ImodState imodState = (ImodState) iterator.next();
      if (imodState.getDatasetName().equals(datasetName)) {
        return imodState;
      }
    }
    return null;
  }

  protected ImodState get(String key, AxisID axisID, int vectorIndex)
      throws AxisTypeException {
    Vector vector = getVector(key, axisID);
    if (vector == null) {
      return null;
    }
    return (ImodState) vector.get(vectorIndex);
  }

  protected ImodState get(String key, int vectorIndex) throws AxisTypeException {
    Vector vector = getVector(key);
    if (vector == null) {
      return null;
    }
    return (ImodState) vector.get(vectorIndex);
  }

  protected void deleteImodState(String key, int vectorIndex)
      throws AxisTypeException {
    Vector vector = getVector(key);
    if (vector == null) {
      return;
    }
    vector.remove(vectorIndex);
  }

  protected Vector getVector(String key) throws AxisTypeException {
    Vector vector;
    if (!useMap) {
      throw new UnsupportedOperationException(
          "This operation is not supported when useMap is false");
    }
    if (axisType == AxisType.SINGLE_AXIS && isDualAxisOnly(key)) {
      throw new AxisTypeException(key + " cannot be found in "
          + axisType.toString());
    }
    if (isDualAxisOnly(key) && isPerAxis(key)) {
      throw new UnsupportedOperationException(key
          + " cannot be found without axisID information");
    }
    if (isPerAxis(key)) {
      vector = (Vector) imodMap.get(key + AxisID.ONLY.getExtension());
    }
    else {
      vector = (Vector) imodMap.get(key);
    }
    if (vector == null) {
      return null;
    }
    return (Vector) vector;
  }

  protected Vector getVector(String key, boolean axisIdInKey)
      throws AxisTypeException {
    if (!axisIdInKey) {
      return getVector(key);
    }
    return (Vector) imodMap.get(key);
  }

  protected Vector getVector(String key, AxisID axisID)
      throws AxisTypeException {
    Vector vector;
    if (axisID == null) {
      return getVector(key);
    }
    if (!useMap) {
      throw new UnsupportedOperationException(
          "This operation is not supported when useMap is false");
    }
    if (axisType == AxisType.SINGLE_AXIS) {
      if (isDualAxisOnly(key)) {
        throw new AxisTypeException(key + " cannot be found in "
            + axisType.toString());
      }
      //Correct axis
      if (axisID == AxisID.FIRST) {
        axisID = AxisID.ONLY;
      }
    }
    if (!isPerAxis(key)) {
      vector = (Vector) imodMap.get(key);
    }
    else {
      vector = (Vector) imodMap.get(key + axisID.getExtension());
    }
    if (vector == null) {
      return null;
    }
    return (Vector) vector;
  }
}