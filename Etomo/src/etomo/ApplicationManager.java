package etomo;

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Toolkit;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Iterator;
import java.util.Vector;
import javax.swing.JOptionPane;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.plaf.FontUIResource;
import etomo.comscript.BadComScriptException;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.CCDEraserParam;
import etomo.comscript.ComScriptManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.MTFFilterParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.NewstParam;
import etomo.comscript.Patchcrawl3DParam;
import etomo.comscript.SolvematchParam;
import etomo.comscript.TiltParam;
import etomo.comscript.TiltalignParam;
import etomo.comscript.TiltxcorrParam;
import etomo.comscript.TomopitchParam;
import etomo.comscript.TransferfidParam;
import etomo.comscript.TrimvolParam;
import etomo.comscript.XfproductParam;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.process.ProcessManager;
import etomo.process.ProcessState;
import etomo.process.SystemProcessException;
import etomo.storage.ParameterStore;
import etomo.storage.Storable;
import etomo.type.AxisID;
import etomo.type.AxisType;
import etomo.type.AxisTypeException;
import etomo.type.ConstMetaData;
import etomo.type.DialogExitState;
import etomo.type.MetaData;
import etomo.type.ProcessName;
import etomo.type.ProcessTrack;
import etomo.type.UserConfiguration;
import etomo.ui.AlignmentEstimationDialog;
import etomo.ui.CoarseAlignDialog;
import etomo.ui.FiducialModelDialog;
import etomo.ui.FiducialessParams;
import etomo.ui.MainFrame;
import etomo.ui.PostProcessingDialog;
import etomo.ui.PreProcessingDialog;
import etomo.ui.ProcessDialog;
import etomo.ui.SettingsDialog;
import etomo.ui.SetupDialog;
import etomo.ui.TextPageWindow;
import etomo.ui.TomogramCombinationDialog;
import etomo.ui.TomogramGenerationDialog;
import etomo.ui.TomogramPositioningDialog;
import etomo.ui.UIParameters;
import etomo.util.InvalidParameterException;
import etomo.util.MRCHeader;
import etomo.util.Utilities;

/**
 * <p>Description: Provides the main entry point, handles high level message 
 *  processing, management of other high-level objects and signal routing.</p>
 *
 * <p>Copyright: Copyright (c) 2002-2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 3.73  2004/06/18 00:52:53  sueh
 * <p> bug# 476 put the logic to check if the fixed stack exists in a
 * <p> separate function and call it in replaceRawStack() and
 * <p> imodErasedStack
 * <p>
 * <p> Revision 3.72  2004/06/17 16:23:34  sueh
 * <p> bug# 466 in imodFullSample() turning on model mode.
 * <p>
 * <p> Revision 3.71  2004/06/15 20:08:49  rickg
 * <p> Bug #383 Run solvematch instead of solvematch{shift|mod}
 * <p>
 * <p> Revision 3.70  2004/06/14 23:39:53  rickg
 * <p> Bug #383 Transitioned to using solvematch
 * <p>
 * <p> Revision 3.69  2004/06/13 17:03:23  rickg
 * <p> Solvematch mid change
 * <p>
 * <p> Revision 3.68  2004/06/10 18:27:12  sueh
 * <p> bug# 463 changing ImodManager.create() to newImod, using
 * <p> ImodManager.setOpenBeadFixer() instead of openBeadFixer
 * <p>
 * <p> Revision 3.67  2004/06/10 18:19:13  rickg
 * <p> Removed redudant dialog parameter fetching from mtffilter and
 * <p> imodPatchRegionModel
 * <p>
 * <p> Revision 3.66  2004/06/10 17:27:53  sueh
 * <p> bug# 462 remove ImodManager.reset() calls
 * <p>
 * <p> Revision 3.65  2004/06/05 00:59:36  sueh
 * <p> bug# 433 add updateLog to call ProcessManager.generateAlignLogs()
 * <p> when the ta logs are out of date
 * <p>
 * <p> Revision 3.64  2004/06/02 23:49:59  rickg
 * <p> Bug #391 only update the rotation.xf if the mode is fiducialess
 * <p>
 * <p> Revision 3.62  2004/06/01 18:53:48  rickg
 * <p> Bug #391 whole tomogram sampling state implementation
 * <p>
 * <p> Revision 3.61  2004/05/27 22:49:54  rickg
 * <p> Bug #391 offer to close preali window for fidless case
 * <p> standardized parameter gathering for tomogram positioning
 * <p> logic for calling updateFiducialAlign
 * <p>
 * <p> Revision 3.60  2004/05/26 04:57:14  rickg
 * <p> Bug #391 Fiducialess handling for the gneration dialog
 * <p>
 * <p> Revision 3.59  2004/05/26 00:00:32  sueh
 * <p> bug# 355 validate metaData when retrieve parameters from an
 * <p> .edf file
 * <p>
 * <p> Revision 3.58  2004/05/25 23:19:34  rickg
 * <p> Bug #391 Fiducialess implementation
 * <p>
 * <p> Revision 3.57  2004/05/24 20:16:45  sueh
 * <p> bug# 409 corrected .ali file names for mtffilter for single axis
 * <p>
 * <p> Revision 3.56  2004/05/21 21:55:13  sueh
 * <p> bug# 443 setting the output file name in tilta.com when
 * <p> running sample
 * <p>
 * <p> Revision 3.55  2004/05/21 02:21:38  sueh
 * <p> bug# 83 removing generic progress bar
 * <p>
 * <p> Revision 3.54  2004/05/15 01:43:55  sueh
 * <p> bug# 415 if saveTestParamIfNecessary() returns false, then the user
 * <p> pressed cancel our there was a problem saving, so don't exit.
 * <p>
 * <p> Revision 3.53  2004/05/15 00:41:00  sueh
 * <p> bug# 302 changing function name updateCombineParams()
 * <p>
 * <p> Revision 3.52  2004/05/13 20:15:10  sueh
 * <p> bug# 33  imodGetRubberbandCoordinates() checks for rubberband data
 * <p>
 * <p> Revision 3.51  2004/05/11 21:11:21  sueh
 * <p> bug# 302 Standardizing synchronization.
 * <p> Syncing with PatchRegionModel button push.
 * <p> UpdateCombineCom(), which updates metadata, is not necessary to
 * <p> run scripts so it doesn't need a success return value.
 * <p> Put syncing first in most cases.
 * <p> Follow syncing by updateCombineCom().
 * <p> In createCombineScripts() run either loadSolvematchShift() or
 * <p> loadSolvematchMod(), instead of only loadSolvematchShift.
 * <p> If MatchingModels is selected after Create scripts is done, call
 * <p> modelCombine() from combine() and visa versa.
 * <p>
 * <p> Revision 3.50  2004/05/07 19:54:45  sueh
 * <p> bug# 33 adding error processing to
 * <p> imodGetRubberbandCoordinates()
 * <p>
 * <p> Revision 3.49  2004/05/06 20:22:33  sueh
 * <p> bug# 33 added getRubberbandCoordinates()
 * <p>
 * <p> Revision 3.48  2004/05/05 21:24:53  sueh
 * <p> bug #430  If changes to .seed happened more recently then .fid, do not
 * <p> mv .fid .seed.  Otherwise backup .seed to .seed~ if not backing up
 * <p> .seed to _orig.seed.  Ok to use fid as seed when .seed does not exist.
 * <p> Orginal bug# 276.
 * <p>
 * <p> Revision 3.47  2004/05/03 22:29:21  sueh
 * <p> bug# 416 Move Bin by 2 settings between tabs in
 * <p> TomogramCombinationDialog.  Set binning in ImodManager.
 * <p>
 * <p> Revision 3.46  2004/05/03 18:04:41  sueh
 * <p> bug# 418 adding printStackTrace for more information
 * <p>
 * <p> Revision 3.45  2004/04/28 22:16:57  sueh
 * <p> bug# 320 user interaction goes in app manager
 * <p>
 * <p> Revision 3.44  2004/04/28 20:13:10  rickg
 * <p> bug #429 all file renames are now handled by the utilities static
 * <p> function to deal with the windows bug
 * <p>
 * <p> Revision 3.43  2004/04/28 00:47:52  sueh
 * <p> trying delete  full aligned stack so that rename works in Windows
 * <p>
 * <p> Revision 3.42  2004/04/28 00:40:29  sueh
 * <p> adding error message if rename filter file doesn't work
 * <p>
 * <p> Revision 3.41  2004/04/27 23:20:13  sueh
 * <p> bug# 320 warn the user about a stale patch vector model after
 * <p> any button press that will lead to creating a new patch vector
 * <p> model
 * <p>
 * <p> Revision 3.40  2004/04/27 22:02:27  sueh
 * <p> bug# 320 try to close the 3dmod with patch vector model before
 * <p> running patchcorr
 * <p>
 * <p> Revision 3.39  2004/04/27 01:01:34  sueh
 * <p> bug# 427 using tomopitch param when running tomopitch
 * <p>
 * <p> Revision 3.38  2004/04/26 21:20:32  sueh
 * <p> bug# 427 added code for tomopitch comscript (not finished)
 * <p>
 * <p> Revision 3.37  2004/04/26 18:36:52  rickg
 * <p> bug #426 Added full image code, fixed order of com script
 * <p> loading for tomogram positioning
 * <p>
 * <p> Revision 3.36  2004/04/26 17:15:54  sueh
 * <p> bug# 83 removing generic progress bar from patchcorr
 * <p>
 * <p> Revision 3.35  2004/04/26 00:24:59  rickg
 * <p> bug #426 Implemented full tomogram sampling
 * <p>
 * <p> Revision 3.34  2004/04/24 08:05:40  rickg
 * <p> bug #391 restructuring
 * <p>
 * <p> Revision 3.32  2004/04/22 23:31:33  rickg
 * <p> bug #391 Added processing for non fid aligne
 * <p> Added getIMODBinPath and getMetaData
 * <p>
 * <p> Revision 3.31  2004/04/19 19:25:04  sueh
 * <p> bug# 409 removing prints
 * <p>
 * <p> Revision 3.30  2004/04/16 02:20:39  sueh
 * <p> removing print statements
 * <p>
 * <p> Revision 3.29  2004/04/16 02:06:30  sueh
 * <p> bug# 409 No longer backing up .ali during useMtfFilter.
 * <p> Changed updateTransferfidEnabled() to updateDialog(FiducialModelDialog) - it
 * <p> does all updates on the FiducialModelDialog - and clarified the code.
 * <p> Create updateDialog(ProcessName) to call the specific updateDialog functions.
 * <p> Calling updateDialog(ProcessName in processDone()
 * <p> Added updateDialog() calls where needed.
 * <p>
 * <p> Revision 3.28  2004/04/06 19:04:06  rickg
 * <p> Print out java system info at start of session
 * <p>
 * <p> Revision 3.27  2004/04/06 17:51:03  rickg
 * <p> bug #391 basic single stage fiducialess alignment
 * <p>
 * <p> Revision 3.26  2004/03/29 21:00:48  sueh
 * <p> bug# 409 Added run mtffilter, view mtffilter result, use mtffilter
 * <p> result as the full
 * <p> aligned stack
 * <p>
 * <p> Revision 3.25  2004/03/24 03:08:17  rickg
 * <p> Bug# 395 Implemented ability to create binned tomogram
 * <p>
 * <p> Revision 3.24  2004/03/22 23:51:23  sueh
 * <p> bug# 83 starting the progress bar as soon as possible
 * <p>
 * <p> Revision 3.23  2004/03/13 00:35:05  rickg
 * <p> Bug# 390 Add prenewst and xfproduct management
 * <p>
 * <p> Revision 3.22  2004/03/11 23:58:14  rickg
 * <p> Bug #410 Newstack PIP transition
 * <p> Formatted code
 * <p>
 * <p> Revision 3.21  2004/03/10 00:44:32  sueh
 * <p> bug# 408 added getIMODCalibDirectory()
 * <p>
 * <p> Revision 3.20  2004/03/09 22:06:56  sueh
 * <p> bug# 407 adding IMOD_CALIB_DIR optional environment variable
 * <p>
 * <p> Revision 3.19  2004/03/06 00:22:39  sueh
 * <p> bug# 250 changed updateCombineCom() - remove duplicate code - call
 * <p> updateCombineCom(int) with NO_TAB
 * <p>
 * <p> Revision 3.18  2004/03/05 18:26:55  sueh
 * <p> bug# 250 changed patchcorrCombine() - updating CombineParams
 * <p> changed updateCombineCom(int) - change the parameter name because
 * <p> its only necessary when a copy to Setup is required
 * <p>
 * <p> Revision 3.17  2004/03/02 00:09:04  sueh
 * <p> bug #250 added updateCombineCom(int fromTab) - update CombineParams
 * <p> from a tab
 * <p> changed combine - default call to combine(int copyFromTab) with NO_TAB
 * <p> added combine(int copyFromTab) copies fields from copyFromTab to setup
 * <p> tab.
 * <p> modelCombine() - same as combine
 * <p> matchvol1() - same as combine
 * <p>
 * <p> Revision 3.16  2004/02/27 20:10:49  sueh
 * <p> bug# 250 changed createCombineScripts() - copied the
 * <p> Setup Use Matching Models value into the Initial tab
 * <p>
 * <p> Revision 3.15  2004/02/25 22:42:23  sueh
 * <p> bug# 403 removed unnecessary code
 * <p>
 * <p> Revision 3.14  2004/02/25 22:17:52  sueh
 * <p> bug# 403 resetState() is no longer setting imodManager to
 * <p> null
 * <p>
 * <p> Revision 3.13  2004/02/16 18:54:38  sueh
 * <p> bug# 276 Added makeFiducialModelSeedModel() to copy the
 * <p>  .seed file to the .fid file.
 * <p>
 * <p> Revision 3.12  2004/02/07 03:12:02  sueh
 * <p> bug# 169 Create ImodManager just once, set metadata
 * <p> separately, reformatted, changed imodRawStack() to
 * <p> ImodPreview()
 * <p>
 * <p> Revision 3.11  2004/02/05 18:05:32  sueh
 * <p> bug# 306 get the trimvol params from the screen and set
 * <p> swapYZ for 3dmod
 * <p>
 * <p> Revision 3.10  2004/02/05 00:19:07  sueh
 * <p> bug# 292 preserving contrast in view x-ray model, changed
 * <p> imodXrayModel()
 * <p>
 * <p> Revision 3.9  2004/02/04 18:12:46  sueh
 * <p> bug# 171 ask to automatically quit all running 3dmod
 * <p> programs.
 * <p>
 * <p> Revision 3.8  2004/01/22 21:09:39  rickg
 * <p> Get screen size in openSetupDialog instead of app init
 * <p>
 * <p> Revision 3.7  2004/01/17 00:14:17  rickg
 * <p> Added a --test argument that prevents the main window from
 * <p> opening up.
 * <p>
 * <p> Revision 3.6  2003/12/08 22:34:32  sueh
 * <p> bug# 169 adding new function imodRawStack
 * <p>
 * <p> Revision 3.5  2003/12/05 01:25:01  sueh
 * <p> bug242 moved getEnvironmentVariable() to Utilities
 * <p>
 * <p> Revision 3.4  2003/12/04 22:09:03  sueh
 * <p> bug242 Converting to new interface.
 * <p>
 * <p> Revision 3.3  2003/11/26 23:36:27  rickg
 * <p> Debug flag and getter changed to static.
 * <p>
 * <p> Revision 3.2  2003/11/11 00:24:52  sueh
 * <p> Bug349 imodFixFiducials(AxisID): call
 * <p> imodManager.openBeadFixer()
 * <p>
 * <p> Revision 3.1  2003/11/10 07:28:54  rickg
 * <p> ContextPopup initialization no longer needed
 * <p> Some more stderr printing on exceptions
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:00  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.93  2003/11/07 19:49:38  rickg
 * <p> Don't delete preali in delete aligned stacks code.
 * <p>
 * <p> Revision 2.92  2003/11/07 00:52:56  rickg
 * <p> Added test helper methods
 * <p> Changed transferfid parameter name to indicate that it is called with
 * <p> the destination axis
 * <p>
 * <p> Revision 2.91  2003/11/06 22:45:47  sueh
 * <p> cleaning up task tags and prints
 * <p>
 * <p> Revision 2.90  2003/11/06 21:41:27  sueh
 * <p> bug348 imodFineAlign(AsixID): removed calls to
 * <p> setFineAlignmentState() for processTrack and mainFrame.
 * <p>
 * <p> Revision 2.89  2003/11/05 20:31:48  rickg
 * <p> Bug #292 Added preserve contrast seed model and residual model
 * <p> opens
 * <p>
 * <p> Revision 2.88  2003/11/05 20:04:05  rickg
 * <p> Bug# 347 Message written to process monitor area
 * <p>
 * <p> Revision 2.87  2003/11/05 19:39:17  rickg
 * <p> Bug# 295 Query the combination dialog instead of the metaData
 * <p> object as to the state of the match direction for opening the patch
 * <p> region model.
 * <p>
 * <p> Revision 2.86  2003/11/05 19:20:21  rickg
 * <p> Bug# 290 Save tomo gen data when done is pressed
 * <p>
 * <p> Revision 2.85  2003/11/05 18:05:50  sueh
 * <p> bug278 created backupFile(File) to backup the .edf file
 * <p> called backupFile(File) from saveTestParamFile()
 * <p>
 * <p> Revision 2.84  2003/11/04 20:55:42  rickg
 * <p> Bug #345 IMOD Diriectory supplied by a static function from 
 * <p> ApplicationManager
 * <p>
 * <p> Revision 2.83  2003/10/27 23:55:41  rickg
 * <p> Bug# 283 Added method to open the tomopitch log file
 * <p>
 * <p> Revision 2.82  2003/10/24 21:45:09  rickg
 * <p> Spelling fix
 * <p>
 * <p> Revision 2.81  2003/10/23 23:06:13  sueh
 * <p> bug271 called isValid() in SetupDialog
 * <p>
 * <p> Revision 2.80  2003/10/22 21:32:02  rickg
 * <p> Bug# 287 Default value handling for SLICE OFFSET and SHIFT
 * <p>
 * <p> Revision 2.79  2003/10/21 23:45:05  rickg
 * <p> Added function to delete the aligned stacks
 * <p>
 * <p> Revision 2.78  2003/10/21 02:34:20  sueh
 * <p> Bug325 Pulled out generic default UI retrieval functionality and placed
 * <p> it in ButtonHelper.
 * <p>
 * <p> Revision 2.77  2003/10/20 22:02:13  rickg
 * <p> Bug# 228 Check to see if solve.xf exists before running matchvol1
 * <p>
 * <p> Revision 2.76  2003/10/20 17:32:09  rickg
 * <p> Use existence of combine com scripts
 * <p> ConstCombineParams.scriptsCreated flag
 * <p>
 * <p> Revision 2.75  2003/10/17 02:00:07  sueh
 * <p> Bug317 added new function - to retrieve default UI resources
 * <p>
 * <p> Revision 2.73  2003/10/10 23:17:01  sueh
 * <p> bug251 removing marks
 * <p>
 * <p> Revision 2.72  2003/10/07 22:40:40  sueh
 * <p> bug251 moved transferfid from fine alignment dialog
 * <p> to fiducial model dialog
 * <p>
 * <p> Revision 2.71  2003/10/05 23:59:35  rickg
 * <p> Bug# 252
 * <p> Adde complete message to progresss region for shor processes
 * <p>
 * <p> Revision 2.70  2003/10/05 21:36:05  rickg
 * <p> Bug# 256
 * <p> Catch SystemProcessException for attempted multiple
 * <p> processes in a axis
 * <p>
 * <p> Revision 2.69  2003/10/03 22:11:15  rickg
 * <p> Bug# 255
 * <p> added returns to catch sections for doneSetupDialog
 * <p> Don't want to continue to main window if copytomocoms did not
 * <p> succeed
 * <p>
 * <p> Revision 2.68  2003/10/02 18:57:46  sueh
 * <p> bug236 added testing:
 * <p> NewstParamTest
 * <p> ComScriptTest
 * <p>
 * <p> Removed marks
 * <p>
 * <p> Revision 2.67  2003/09/30 03:18:43  rickg
 * <p> Bug# 248
 * <p> changed openTestParamFile to loadTestParamFile
 * <p> split out resetState method
 * <p> added logic to openExistingData use a File object or
 * <p> open the File Dialog and drop into the setup page if it fails.
 * <p>
 * <p> Revision 2.66  2003/09/30 02:18:57  rickg
 * <p> Bug 249
 * <p> Proper New dialog behavior when not saving the EDF
 * <p> Also moved message dialogs to the mainFrame
 * <p>
 * <p> Revision 2.65  2003/09/29 23:34:57  sueh
 * <p> bug236 Added UseLinearInterpolation to
 * <p> TomogramGenerationDialog.
 * <p>
 * <p> UseLinearInterpolation:
 * <p> check box
 * <p> Advanced
 * <p> newst -linear
 * <p>
 * <p> Files:
 * <p> ComScriptManager.java
 * <p> ConstNewstParam.java
 * <p> NewstParam.java
 * <p> TomogramGenerationDialog.java
 * <p> ApplicationManager.java
 * <p>
 * <p> Revision 2.64  2003/09/26 19:46:16  sueh
 * <p> bug223 removed task marks
 * <p>
 * <p> Revision 2.63  2003/09/26 19:43:48  sueh
 * <p> bug223 no field should be persistant.  Changed MetaData.
 * <p> Added TransferfidNumberViews.
 * <p> Changed the done fine allignment and open fine allignment functions
 * <p> to work with MetaData
 * <p>
 * <p> Revision 2.62  2003/09/09 17:20:29  rickg
 * <p> Check to see if the _orig.st stack exists, do not replace if it does.
 * <p>
 * <p> Revision 2.61  2003/09/08 22:18:50  rickg
 * <p> Catch exception thrown buy ProcessManager.startComScript
 * <p>
 * <p> Revision 2.60  2003/09/08 05:44:47  rickg
 * <p> Added trial tilt
 * <p> Output for a single axis tomogram is changed to
 * <p> dataset_full.rec
 * <p>
 * <p> Revision 2.59  2003/08/20 21:57:09  rickg
 * <p> Only close imods in specified directory
 * <p>
 * <p> Revision 2.58  2003/08/05 21:35:22  rickg
 * <p> Retry commit, eclipse broken?
 * <p>
 * <p> Revision 2.57  2003/07/28 22:53:09  rickg
 * <p> Fixed postpone logic for combine panel.  Combine scripts
 * <p> created flag is now reset only when the CombineParams are
 * <p> modified.
 * <p>
 * <p> Combine postpone will now save combine sub script parameters
 * <p>
 * <p> Revision 2.56  2003/07/25 22:51:11  rickg
 * <p> Imod model mode management changes
 * <p> Save original stack as _orig.st
 * <p>
 * <p> Revision 2.55  2003/07/22 22:16:15  rickg
 * <p> Erased stack methods and trial mode
 * <p>
 * <p> Revision 2.54  2003/07/01 19:24:30  rickg
 * <p> Fixed progress bars for prenewst, newst and tomogram generation
 * <p>
 * <p> Revision 2.53  2003/06/27 20:33:28  rickg
 * <p> Changed below method to public
 * <p>
 * <p> Revision 2.52  2003/06/27 20:23:32  rickg
 * <p> Adde getter method for the com script manager
 * <p>
 * <p> Revision 2.51  2003/06/10 05:29:30  rickg
 * <p> Data persistence behavior of the combination and post
 * <p> processing panels now match the others.
 * <p>
 * <p> Revision 2.50  2003/06/10 05:15:23  rickg
 * <p> *** empty log message ***
 * <p>
 * <p> Revision 2.49  2003/06/09 04:28:21  rickg
 * <p> Set state to in progress if any thing is exected for a given
 * <p> process panel
 * <p>
 * <p> Revision 2.48  2003/06/05 21:19:13  rickg
 * <p> Explicit transferfid B to A false setting
 * <p>
 * <p> Revision 2.47  2003/05/27 08:42:04  rickg
 * <p> Progress bar determinant delegate methods
 * <p>
 * <p> Revision 2.46  2003/05/23 22:49:41  rickg
 * <p> Spelling correction
 * <p>
 * <p> Revision 2.45  2003/05/23 14:29:11  rickg
 * <p> Progress bar determinant delegate methods
 * <p>
 * <p> Revision 2.44  2003/05/21 22:56:54  rickg
 * <p> Initial kill implementation
 * <p>
 * <p> Revision 2.43  2003/05/19 22:05:31  rickg
 * <p> Added openNewDataset method
 * <p> unset isDataParamDirty in daving method
 * <p>
 * <p> Revision 2.42  2003/05/15 22:24:24  rickg
 * <p> Reordered method sequence in opening processing panel to
 * <p> prevent slider from taking up all of the window.
 * <p>
 * <p> Revision 2.41  2003/05/15 20:13:05  rickg
 * <p> Fixed PLAF for windows
 * <p>
 * <p> Revision 2.40  2003/05/15 19:39:44  rickg
 * <p> Look and feel handling
 * <p>
 * <p> Revision 2.39  2003/05/14 23:22:51  rickg
 * <p> Exit if no IMOD_DIR is defined.  We can't run any of the non com scripts
 * <p>
 * <p> Revision 2.38  2003/05/14 21:45:27  rickg
 * <p> New trimvol constructor for windows
 * <p>
 * <p> Revision 2.37  2003/05/14 14:36:08  rickg
 * <p> Temporary change to volcombine
 * <p>
 * <p> Revision 2.36  2003/05/13 19:58:22  rickg
 * <p> TransferfidParams constructed with IMODDirectory File
 * <p>
 * <p> Revision 2.35  2003/05/10 19:12:56  rickg
 * <p> OS independent path implementation
 * <p>
 * <p> Revision 2.34  2003/05/10 18:01:56  rickg
 * <p> Fixes to get IMOD_DIR home and current working directory
 * <p> in a OS agnostic manner
 * <p>
 * <p> Revision 2.33  2003/05/09 23:25:36  rickg
 * <p> Working change to get env vars from all OSs
 * <p>
 * <p> Revision 2.32  2003/05/09 17:52:59  rickg
 * <p> include this in ImodManager constructor, needed for fiducial model calls
 * <p>
 * <p> Revision 2.31  2003/05/08 23:18:45  rickg
 * <p> Added --debug option, off by default
 * <p>
 * <p> Revision 2.30  2003/05/08 20:14:30  rickg
 * <p> Don't set main window location to (0,0) confuses SGI
 * <p>
 * <p> Revision 2.29  2003/05/08 19:58:53  rickg
 * <p> Work around for bug in File.getParent
 * <p>
 * <p> Revision 2.28  2003/05/07 23:04:29  rickg
 * <p> System property user.dir now defines the working directory
 * <p> Home is now read from the System properties
 * <p>
 * <p> Revision 2.27  2003/04/30 18:48:51  rickg
 * <p> Changed matchcheck* to a single imod instance
 * <p>
 * <p> Revision 2.26  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.25  2003/04/24 17:46:54  rickg
 * <p> Changed fileset name to dataset name
 * <p>
 * <p> Revision 2.24  2003/04/17 23:11:26  rickg
 * <p> Added cancel handling from post processing dialog
 * <p>
 * <p> Revision 2.23  2003/04/16 22:49:49  rickg
 * <p> Trimvol implmentation
 * <p>
 * <p> Revision 2.22  2003/04/16 00:13:54  rickg
 * <p> Trimvol in progress
 * <p>
 * <p> Revision 2.21  2003/04/14 23:57:18  rickg
 * <p> Trimvol management changes
 * <p>
 * <p> Revision 2.20  2003/04/10 23:40:03  rickg
 * <p> Initial exit function handling of imod and other processes
 * <p> Initial openPostProcessingDialog
 * <p>
 * <p> Revision 2.19  2003/03/26 00:52:25  rickg
 * <p> Added button to convert patch_vector.mod to patch.out
 * <p>
 * <p> Revision 2.18  2003/03/22 00:40:35  rickg
 * <p> slovematchmod label change
 * <p>
 * <p> Revision 2.17  2003/03/20 21:18:55  rickg
 * <p> Added matchshift results button/access
 * <p>
 * <p> Revision 2.16  2003/03/20 16:58:42  rickg
 * <p> Added methods: imodMatchedToTomgram, matchorwarpTrial
 * <p> Added trial mode handling to matchorwarp
 * <p>
 * <p> Revision 2.15  2003/03/19 00:23:04  rickg
 * <p> Added imod patch vector model pass through
 * <p>
 * <p> Revision 2.14  2003/03/18 23:56:54  rickg
 * <p> ComScript method name changes
 * <p> Apropriate loading of combine scripts
 * <p> Added pass through method to open matching models
 * <p> Done not longer executes combine
 * <p> Updated combine related methods to match new
 * <p> combination dialog
 * <p>
 * <p> Revision 2.13  2003/03/18 17:03:15  rickg
 * <p> Combine development in progress
 * <p>
 * <p> Revision 2.12  2003/03/18 15:01:31  rickg
 * <p> Combine development in progress
 * <p>
 * <p> Revision 2.11  2003/03/18 00:32:32  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.10  2003/03/07 07:22:49  rickg
 * <p> combine layout in progress
 * <p>
 * <p> Revision 2.9  2003/03/06 05:53:28  rickg
 * <p> Combine interface in progress
 * <p>
 * <p> Revision 2.8  2003/03/06 01:19:17  rickg
 * <p> Combine changes in progress
 * <p>
 * <p> Revision 2.7  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.6  2003/02/24 23:27:21  rickg
 * <p> Added process interrupt method
 * <p>
 * <p> Revision 2.5  2003/01/30 00:43:32  rickg
 * <p> Blank second axis panel when done with tomogram generation
 * <p>
 * <p> Revision 2.4  2003/01/29 15:22:58  rickg
 * <p> Updated logic for combine step
 * <p>
 * <p> Revision 2.3  2003/01/28 20:42:53  rickg
 * <p> Bug fix: save current dialog state when running align.com
 * <p>
 * <p> Revision 2.2  2003/01/28 00:15:29  rickg
 * <p> Main window now remembers its size
 * <p>
 * <p> Revision 2.1  2003/01/27 18:12:41  rickg
 * <p> Fixed bug from single window transition in positioning dialog
 * <p> opening function
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.36.2.1  2003/01/24 18:27:46  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.36  2003/01/10 20:46:34  rickg
 * <p> Added ability to view 3D fiducial models
 * <p>
 * <p> Revision 1.35  2003/01/10 18:39:58  rickg
 * <p> Using existing com scripts now gives the correct
 * <p> process state
 * <p>
 * <p> Revision 1.34  2003/01/10 18:33:16  rickg
 * <p> Added test parameter filename to command line args
 * <p>
 * <p> Revision 1.33  2003/01/08 21:04:38  rickg
 * <p> More descriptive error dialog when the are not
 * <p> available for combining.
 * <p>
 * <p> Revision 1.32  2003/01/07 00:30:16  rickg
 * <p> Added imodViewResidual method
 * <p>
 * <p> Revision 1.31  2003/01/06 04:53:16  rickg
 * <p> Set default parameters for transferfid panel and handle
 * <p> new backwards flag for b to a
 * <p>
 * <p> Revision 1.30  2003/01/04 00:41:00  rickg
 * <p> Implemented transferfid method
 * <p>
 * <p> Revision 1.29  2002/12/19 00:35:20  rickg
 * <p> Implemented persitent advanced state handling
 * <p>
 * <p> Revision 1.27  2002/12/11 21:26:31  rickg
 * <p> Added font setting into user prefs setting
 * <p>
 * <p> Revision 1.26  2002/12/11 05:39:00  rickg
 * <p> Added basic font change method
 * <p>
 * <p> Revision 1.25  2002/12/11 00:39:48  rickg
 * <p> Basic handling of settings dialog
 * <p> added setUserPreferences method
 * <p>
 * <p> Revision 1.24  2002/12/09 04:18:50  rickg
 * <p> Better handling of current working directory, user.dir and
 * <p> metaData always agree now.
 * <p>
 * <p> Revision 1.23  2002/12/05 01:21:02  rickg
 * <p> Added isAdvanced stub
 * <p>
 * <p> Revision 1.22  2002/11/21 19:24:38  rickg
 * <p> Set user.dir to current working directory
 * <p>
 * <p> Revision 1.21  2002/10/29 18:22:04  rickg
 * <p> Simplified rawstack open checking
 * <p>
 * <p> Revision 1.20  2002/10/25 19:30:43  rickg
 * <p> Modifies several catches to explicilty specify exception
 * <p>
 * <p> Revision 1.19  2002/10/24 19:52:55  rickg
 * <p> Added command line --demo argument
 * <p>
 * <p> Revision 1.18  2002/10/22 21:38:24  rickg
 * <p> ApplicationManager now controls both demo and debug
 * <p> modes
 * <p>
 * <p> Revision 1.17  2002/10/17 22:47:35  rickg
 * <p> process dialogs are now managed attributes
 * <p> setVisible calls changed to show
 * <p> unused variable dialogFinshed removed
 * <p>
 * <p> Revision 1.16  2002/10/17 16:23:04  rickg
 * <p> Added private method to update the dependent tilt parameters when the
 * <p> align.com parameters are changed
 * <p>
 * <p> Revision 1.15  2002/10/16 17:37:18  rickg
 * <p> Construct a imodManager when a new data set is opened
 * <p>
 * <p> Revision 1.14  2002/10/14 22:44:27  rickg
 * <p> Added combine to execute section of doneCombine
 * <p>
 * <p> Revision 1.13  2002/10/14 19:04:18  rickg
 * <p> openMessageDialog made public
 * <p>
 * <p> Revision 1.12  2002/10/10 23:40:33  rickg
 * <p> refactored createCombineScripts to setupCombineScripts
 * <p>
 * <p> Revision 1.11  2002/10/10 19:16:19  rickg
 * <p> Get HOME and IMOD_DIR environement variables during
 * <p> initialization instead of each time they requested.  Also
 * <p> exit if they are not available.
 * <p>
 * <p> Revision 1.10  2002/10/09 04:29:17  rickg
 * <p> Implemented calls to updateCombineCom
 * <p>
 * <p> Revision 1.9  2002/10/09 00:04:37  rickg
 * <p> Added default patch boundary logic
 * <p> still needs work on getting combine parameters at the correct times
 * <p>
 * <p> Revision 1.8  2002/10/07 22:20:21  rickg
 * <p> removed unused imports
 * <p>
 * <p> Revision 1.7  2002/09/30 23:44:43  rickg
 * <p> Started implementing updateCombineCom
 * <p>
 * <p> Revision 1.6  2002/09/30 22:01:29  rickg
 * <p> Added check to verify dual axis for combination
 * <p>
 * <p> Revision 1.5  2002/09/20 18:56:09  rickg
 * <p> Added private message and yes/no dialog methods
 * <p> Check to see if the raw stack and coarsely aligned stacks should be
 * <p> closed by the user
 * <p>
 * <p> Revision 1.4  2002/09/19 22:57:56  rickg
 * <p> Imod mangement is now handled through the ImodManager
 * <p>
 * <p> Revision 1.3  2002/09/17 23:44:56  rickg
 * <p> Adding ImodManager, in progress
 * <p>
 * <p> Revision 1.2  2002/09/13 21:29:57  rickg
 * <p> Started updating for ImodManager
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */

public class ApplicationManager {
  public static final String rcsid = "$Id$";

  private static boolean debug = false;

  private boolean demo = false;

  private boolean test = false;

  private boolean isDataParamDirty = false;

  private String homeDirectory;

  private static File IMODDirectory;

  private static File IMODCalibDirectory;

  private UserConfiguration userConfig = new UserConfiguration();

  private MetaData metaData = new MetaData();

  private File paramFile = null;

  // advanced dialog state for this instance, this gets set upon startup from
  // the user configuration and can be modified for this instance by either
  // the option or advanced menu items
  private boolean isAdvanced = false;

  //  Process dialog references
  private SetupDialog setupDialog = null;

  private PreProcessingDialog preProcDialogA = null;

  private PreProcessingDialog preProcDialogB = null;

  private CoarseAlignDialog coarseAlignDialogA = null;

  private CoarseAlignDialog coarseAlignDialogB = null;

  private FiducialModelDialog fiducialModelDialogA = null;

  private FiducialModelDialog fiducialModelDialogB = null;

  private AlignmentEstimationDialog fineAlignmentDialogA = null;

  private AlignmentEstimationDialog fineAlignmentDialogB = null;

  private TomogramPositioningDialog tomogramPositioningDialogA = null;

  private TomogramPositioningDialog tomogramPositioningDialogB = null;

  private TomogramGenerationDialog tomogramGenerationDialogA = null;

  private TomogramGenerationDialog tomogramGenerationDialogB = null;

  private TomogramCombinationDialog tomogramCombinationDialog = null;

  private PostProcessingDialog postProcessingDialog = null;

  private SettingsDialog settingsDialog = null;

  //  This object controls the reading and writing of David's com scripts
  private ComScriptManager comScriptMgr = new ComScriptManager(this);

  //  The ProcessManager manages the execution of com scripts
  private ProcessManager processMgr = new ProcessManager(this);

  private ProcessTrack processTrack = new ProcessTrack();

  // Control variable for process execution
  // FIXME: this going to need to expand to handle both axis
  private String nextProcess = "";

  private String threadNameA = "none";

  private String threadNameB = "none";

  // imodManager manages the opening and closing closing of imod(s), message
  // passing for loading model
  private ImodManager imodManager;

  private MainFrame mainFrame;

  /**
   *  
   */
  public ApplicationManager(String[] args) {
    //  Initialize the program settings
    String testParamFilename = initProgram(args);
    //imodManager should be created only once.
    imodManager = new ImodManager(this);
    //  Create a new main window and wait for an event from the user
    if (!test) {
      mainFrame = new MainFrame(this);
      mainFrame.setMRUFileLabels(userConfig.getMRUFileList());
      //  Initialize the static UIParameter object
      UIParameters uiparameters = new UIParameters();
      // Open the etomo data file if one was found on the command line
      if (!testParamFilename.equals("")) {
        File etomoDataFile = new File(testParamFilename);
        if (loadTestParamFile(etomoDataFile)) {
          openProcessingPanel();
        }
        else {
          openSetupDialog();
        }
      }
      else {
        openSetupDialog();
      }
      mainFrame.pack();
      mainFrame.show();
    }
  }

  /**
   *  
   */
  public static void main(String[] args) {
    new ApplicationManager(args);
  }

  /**
   * Open the setup dialog
   */
  public void openSetupDialog() {
    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    if (setupDialog == null) {
      setupDialog = new SetupDialog(this);
      setupDialog.initializeFields(metaData);
    }
    mainFrame.openSetupPanel(setupDialog);
    Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
    Dimension frameSize = mainFrame.getSize();
    mainFrame.setLocation((screenSize.width - frameSize.width) / 2,
      (screenSize.height - frameSize.height) / 2);
  }

  /**
   * Close message from the setup dialog window
   */
  public void doneSetupDialog() {
    if (setupDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update metadata parameters without an active setup dialog",
        "Program logic error");
    }
    //  Get the selected exit button
    DialogExitState exitState = setupDialog.getExitState();
    if (exitState != DialogExitState.CANCEL) {
      if (!setupDialog.isValid()) {
        return;
      }
      // Set the current working directory for the application saving the
      // old user.dir property until the meta data is valid
      String oldUserDir = System.getProperty("user.dir");
      System.setProperty("user.dir",
        setupDialog.getWorkingDirectory().getAbsolutePath());
      metaData = setupDialog.getFields();
      if (metaData.isValid()) {
        mainFrame.updateDataParameters(null, metaData);
        processTrack.setSetupState(ProcessState.INPROGRESS);
        isDataParamDirty = true;
        //final initialization of IMOD manager
        imodManager.setMetaData(metaData);
      }
      else {
        String[] errorMessage = new String[2];
        errorMessage[0] = "Setup Parameter Error";
        errorMessage[1] = metaData.getInvalidReason();
        mainFrame.openMessageDialog(errorMessage, "Setup Parameter Error");
        System.setProperty("user.dir", oldUserDir);
        return;
      }
      // This is really the method to use the existing com scripts
      if (exitState == DialogExitState.EXECUTE) {
        try {
          processMgr.setupComScripts(metaData);
        }
        catch (BadComScriptException except) {
          except.printStackTrace();
          mainFrame.openMessageDialog(except.getMessage(),
            "Can't run copytomocoms");
          return;
        }
        catch (IOException except) {
          mainFrame.openMessageDialog("Can't run copytomocoms\n"
            + except.getMessage(), "Copytomocoms IOException");
          return;
        }
      }
      processTrack.setSetupState(ProcessState.COMPLETE);
      metaData.setComScriptCreated(true);
    }
    //  Switch the main window to the procesing panel
    openProcessingPanel();
    //  Free the dialog
    setupDialog = null;
  }

  /**
   * Open the main window in processing mode
   */
  public void openProcessingPanel() {
    mainFrame.showProcessingPanel(metaData.getAxisType());
    mainFrame.updateAllProcessingStates(processTrack);
    mainFrame.pack();
    //  Resize to the users preferrred window dimensions
    mainFrame.setSize(new Dimension(userConfig.getMainWindowWidth(),
      userConfig.getMainWindowHeight()));
    mainFrame.doLayout();
    mainFrame.validate();
    if (isDualAxis()) {
      mainFrame.setDividerLocation(0.51);
    }
  }

  /**
   * Open the pre-processing dialog
   */
  public void openPreProcDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    mainFrame.selectButton(axisID, "Pre-processing");
    // TODO: When a panel is overwriten by another should it be nulled and
    // closed or left and and reshown when needed?
    // Problem with stale data for align and tilt info since they are on
    // multiple panels
    // Check to see if the dialog panel is already open
    if (showIfExists(preProcDialogA, preProcDialogB, axisID)) {
      return;
    }
    PreProcessingDialog preProcDialog = new PreProcessingDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      preProcDialogB = preProcDialog;
    }
    else {
      preProcDialogA = preProcDialog;
    }
    // Load the required ccderaser{|a|b}.com files
    // Fill in the parameters and set it to the appropriate state
    comScriptMgr.loadEraser(axisID);
    preProcDialog.setCCDEraserParams(comScriptMgr.getCCDEraserParam(axisID));
    mainFrame.showProcess(preProcDialog.getContainer(), axisID);
  }

  /**
   *  
   */
  public void donePreProcDialog(AxisID axisID) {
    PreProcessingDialog preProcDialog;
    if (axisID == AxisID.SECOND) {
      preProcDialog = preProcDialogB;
    }
    else {
      preProcDialog = preProcDialogA;
    }
    if (preProcDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update preprocessing parameters without an active "
          + "preprocessing dialog", "Program logic error");
      return;
    }
    //  Keep dialog box open until we get good info or it is cancelled
    DialogExitState exitState = preProcDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      updateEraserCom(axisID, false);
      // If there are raw stack imod processes open ask the user if they
      // should be closed.
      try {
        if (imodManager.isOpen(ImodManager.RAW_STACK_KEY, axisID)) {
          String[] message = new String[2];
          message[0] = "The raw stack is open in 3dmod";
          message[1] = "Should it be closed?";
          if (mainFrame.openYesNoDialog(message)) {
            imodManager.quit(ImodManager.RAW_STACK_KEY, axisID);
          }
        }
      }
      catch (AxisTypeException except) {
        except.printStackTrace();
        mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
      }
      catch (SystemProcessException except) {
        except.printStackTrace();
        mainFrame.openMessageDialog(except.getMessage(),
          "Problem closing raw stack");
      }
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setPreProcessingState(ProcessState.COMPLETE, axisID);
        mainFrame.setPreProcessingState(ProcessState.COMPLETE, axisID);
        //  Go to the coarse align dialog by default
        openCoarseAlignDialog(axisID);
      }
      else {
        processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
        mainFrame.setPreProcessingState(ProcessState.INPROGRESS, axisID);
        //  Go to the coarse align dialog by default
        mainFrame.showBlankProcess(axisID);
      }
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      preProcDialogB = null;
    }
    else {
      preProcDialogA = null;
    }
    preProcDialog = null;
  }

  /**
   * Open 3dmod to create the manual erase model
   */
  public void imodManualErase(AxisID axisID) {
    String eraseModelName = metaData.getDatasetName() + axisID.getExtension()
      + ".erase";
    try {
      imodManager.model(ImodManager.RAW_STACK_KEY, axisID, eraseModelName, true);
      processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
      mainFrame.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on raw stack");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Axis type problem in 3dmod erase");
    }
  }

  /**
   * Get the eraser script parameters from the CCD eraser panel and
   * write them out the eraser{|a|b}.com script
   * @param axisID    The axisID to process
   * @param trialMode Set to trial mode if true
   */
  private void updateEraserCom(AxisID axisID, boolean trialMode) {
    PreProcessingDialog preProcDialog;
    if (axisID == AxisID.SECOND) {
      preProcDialog = preProcDialogB;
    }
    else {
      preProcDialog = preProcDialogA;
    }
    //  Get the user input data from the dialog box. The CCDEraserParam
    //  is first initialized from the currently loaded com script to
    //  provide deafault values for those not handled by the dialog box
    //  get function needs some error checking
    CCDEraserParam ccdEraserParam = comScriptMgr.getCCDEraserParam(axisID);
    preProcDialog.getCCDEraserParams(ccdEraserParam);
    ccdEraserParam.setTrialMode(trialMode);
    comScriptMgr.saveEraser(ccdEraserParam, axisID);
  }

  /**
   * Run the eraser script for the specified axis
   * @param axisID
   */
  public void eraser(AxisID axisID) {
    updateEraserCom(axisID, false);
    processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    mainFrame.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.eraser(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute eraser" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Run CCDeraser in trial mode
   * @param axisID
   */
  public void findXrays(AxisID axisID) {
    updateEraserCom(axisID, true);
    processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    mainFrame.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.eraser(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute eraser" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Open 3dmod to view the xray model on the raw stack
   */
  public void imodXrayModel(AxisID axisID) {
    String xRayModel = metaData.getDatasetName() + axisID.getExtension()
      + "_peak.mod";
    try {
      imodManager.model(ImodManager.RAW_STACK_KEY, axisID, xRayModel, false,
        true);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Problem opening coarse stack");
    }
  }

  /**
   * Open 3dmod to view the erased stack
   */
  public void imodErasedStack(AxisID axisID) {
    if (getFile(true, axisID, "_fixed.st", "erased stack") == null) {
      return;
    }
    try {
      imodManager.open(ImodManager.ERASED_STACK_KEY, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Problem opening erased stack");
    }
  }

  /**
   * Creates a file name and a file.  If the file doesn't exist and mustExist is
   * true, it complains and returns null, otherwise it returns the file.
   * @param mustExist
   * @param axisID
   * @param extension
   * @param fileType A string used in the error dialog
   * @return
   */
  protected File getFile(
    boolean mustExist,
    AxisID axisID,
    String extension,
    String fileDescription) {
    String filename = System.getProperty("user.dir") + File.separator
      + metaData.getDatasetName() + axisID.getExtension() + extension;
    File file = new File(filename);
    if (!file.exists() && mustExist) {
      mainFrame.openMessageDialog(
        "The " + fileDescription + " doesn't exist.  Create the "
         + fileDescription + " first",
      fileDescription + " missing");
      return null;
    }
    return file;
  }
  
  /**
   * Replace the raw stack with the fixed stack created from eraser
   * @param axisID
   */
  public void replaceRawStack(AxisID axisID) {
    mainFrame.setProgressBar("Using fixed stack", 1, axisID);
    // Instantiate file objects for the original raw stack and the fixed stack
    String rawStackFilename = System.getProperty("user.dir") + File.separator
      + metaData.getDatasetName() + axisID.getExtension() + ".st";
    File rawStack = new File(rawStackFilename);
    String rawStackRename = System.getProperty("user.dir") + File.separator
      + metaData.getDatasetName() + axisID.getExtension() + "_orig.st";
    File rawRename = new File(rawStackRename);
    File fixedStack = getFile(true, axisID, "_fixed.st", "erased stack");
    if (fixedStack == null) {
      return;
    }
    processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    mainFrame.setPreProcessingState(ProcessState.INPROGRESS, axisID);

    // Rename the fixed stack to the raw stack file name and save the orginal
    // raw stack to _orig.st if that does not already exist
    try {
      if (!rawRename.exists()) {
        Utilities.renameFile(rawStack, rawRename);
      }
      Utilities.renameFile(fixedStack, rawStack);
    }
    catch (IOException except) {
      mainFrame.openMessageDialog(except.getMessage(), "File Rename Error");
    }
    try {
      if (imodManager.isOpen(ImodManager.RAW_STACK_KEY, axisID)) {
        String[] message = new String[2];
        message[0] = "The replaced raw stack is open in 3dmod";
        message[1] = "Should it be closed?";
        if (mainFrame.openYesNoDialog(message)) {
          imodManager.quit(ImodManager.RAW_STACK_KEY, axisID);
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
      System.err.println("Axis type exception in replaceRawStack");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      System.err.println("System process exception in replaceRawStack");
    }
    mainFrame.stopProgressBar(axisID);
  }

  public void imodPreview(AxisID axisID) {
    if (setupDialog == null) {
      return;
    }
    String key = ImodManager.PREVIEW_KEY;
    MetaData metaData = setupDialog.getFields();
    imodManager.setPreviewMetaData(metaData);
    File previewWorkingDir = metaData.getValidDatasetDirectory(setupDialog.getWorkingDirectory().getAbsolutePath());
    if (previewWorkingDir == null) {
      mainFrame.openMessageDialog(metaData.getInvalidReason(),
        "Raw Image Stack");
      return;
    }
    try {
      int previewNumber = imodManager.newImod(key, axisID);
      imodManager.setWorkingDirectory(key, axisID, previewNumber,
        previewWorkingDir);
      imodManager.open(key, axisID, previewNumber);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Problem opening raw stack");
    }
  }

  /**
   * Open the coarse alignment dialog
   */
  public void openCoarseAlignDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    mainFrame.selectButton(axisID, "Coarse Alignment");
    if (showIfExists(coarseAlignDialogA, coarseAlignDialogB, axisID)) {
      return;
    }
    CoarseAlignDialog coarseAlignDialog = new CoarseAlignDialog(this, axisID);
    if (axisID == AxisID.SECOND) {
      coarseAlignDialogB = coarseAlignDialog;
    }
    else {
      coarseAlignDialogA = coarseAlignDialog;
    }
    //  Create the dialog box
    comScriptMgr.loadXcorr(axisID);
    coarseAlignDialog.setCrossCorrelationParams(comScriptMgr.getTiltxcorrParam(axisID));
    comScriptMgr.loadPrenewst(axisID);
    comScriptMgr.loadAlign(axisID);

    NewstParam prenewstParam = comScriptMgr.getPrenewstParam(axisID);
    coarseAlignDialog.setPrenewstParams(prenewstParam);

    coarseAlignDialog.setFiducialessAlignment(metaData.isFiducialessAlignment());
    coarseAlignDialog.setTiltAxisAngle(metaData.getImageRotation(axisID));
    mainFrame.showProcess(coarseAlignDialog.getContainer(), axisID);
  }

  /**
   * Get the parameters from the coarse align process dialog box
   */
  public void doneCoarseAlignDialog(AxisID axisID) {
    //  Set a reference to the correct object
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    if (coarseAlignDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update coarse align without an active coarse align dialog",
        "Program logic error");
      return;
    }
    DialogExitState exitState = coarseAlignDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (!updateXcorrCom(axisID)) {
        return;
      }
      if (!updatePrenewstCom(axisID)) {
        return;
      }
      if (!updateFiducialessParams(coarseAlignDialog, axisID)) {
        return;
      }
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setCoarseAlignmentState(ProcessState.COMPLETE, axisID);
        mainFrame.setCoarseAlignState(ProcessState.COMPLETE, axisID);
        //  Go to the fiducial model dialog by default
        if (metaData.isFiducialessAlignment()) {
          openTomogramPositioningDialog(axisID);
          // Check to see if the user wants to keep any coarse aligned imods
          // open
          try {
            if (imodManager.isOpen(ImodManager.COARSE_ALIGNED_KEY, axisID)) {
              String[] message = new String[2];
              message[0] = "The coarsely aligned stack is open in 3dmod";
              message[1] = "Should it be closed?";
              if (mainFrame.openYesNoDialog(message)) {
                imodManager.quit(ImodManager.COARSE_ALIGNED_KEY, axisID);
              }
            }
          }
          catch (AxisTypeException except) {
            except.printStackTrace();
            mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
          }
          catch (SystemProcessException except) {
            except.printStackTrace();
            mainFrame.openMessageDialog(except.getMessage(),
              "Problem closing coarse stack");
          }
        }
        else {
          openFiducialModelDialog(axisID);
        }
      }
      else {
        processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
        mainFrame.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      coarseAlignDialogB = null;
    }
    else {
      coarseAlignDialogA = null;
    }
    coarseAlignDialog = null;
  }

  /**
   * Get the parameters from dialog box and run the cross correlation script
   */
  public void crossCorrelate(AxisID axisID) {
    // Get the parameters from the dialog box
    if (updateXcorrCom(axisID)) {
      processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
      mainFrame.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.crossCorrelate(axisID);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute xcorr" + axisID.getExtension() + ".com";
        message[1] = e.getMessage();
        mainFrame.openMessageDialog(message, "Unable to execute com script");
        return;
      }
      setThreadName(threadName, axisID);
    }
  }

  /**
   * Run the coarse alignment script
   */
  public void coarseAlign(AxisID axisID) {
    if (updatePrenewstCom(axisID)) {
      processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
      mainFrame.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        if (metaData.isFiducialessAlignment()) {
          nextProcess = "generateNonFidXF";
        }
        threadName = processMgr.coarseAlign(axisID);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute prenewst" + axisID.getExtension()
          + ".com";
        message[1] = e.getMessage();
        mainFrame.openMessageDialog(message, "Unable to execute com script");
        return;
      }
      setThreadName(threadName, axisID);
    }
  }

  /**
   * Open 3dmod to view the coarsely aligned stack
   */
  public void imodCoarseAlign(AxisID axisID) {
    try {
      imodManager.open(ImodManager.COARSE_ALIGNED_KEY, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Problem opening coarse stack");
    }
  }

  /**
   * Run midas on the raw stack
   */
  public void midasRawStack(AxisID axisID) {
    if (!updateFiducialessParams(mapCoarseAlignDialog(axisID), axisID)) {
      return;
    }
    processMgr.midasRawStack(axisID, metaData.getImageRotation(axisID));
    processTrack.setCoarseAlignmentState(ProcessState.INPROGRESS, axisID);
    mainFrame.setCoarseAlignState(ProcessState.INPROGRESS, axisID);
  }

  /**
   * Get the required parameters from the dialog box and update the xcorr.com
   * script
   * @return true if successful in getting the parameters and saving the com
   *         script
   */
  private boolean updateXcorrCom(AxisID axisID) {
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    try {
      TiltxcorrParam tiltXcorrParam = comScriptMgr.getTiltxcorrParam(axisID);
      coarseAlignDialog.getCrossCorrelationParams(tiltXcorrParam);
      comScriptMgr.saveXcorr(tiltXcorrParam, axisID);
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      mainFrame.openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Xcorr Align Parameter Syntax Error";
      errorMessage[1] = axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "Xcorr Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Get the prenewst parameters from the dialog box and update the prenewst
   * com script as well as the align com script since binning of the pre-aligned
   * stack has an affect on the scaling of the fiducial model.
   * @param axisID
   * @return
   */
  private boolean updatePrenewstCom(AxisID axisID) {
    CoarseAlignDialog coarseAlignDialog = mapCoarseAlignDialog(axisID);
    NewstParam prenewstParam = comScriptMgr.getPrenewstParam(axisID);
    coarseAlignDialog.getPrenewstParams(prenewstParam);
    comScriptMgr.savePrenewst(prenewstParam, axisID);
    XfproductParam xfproductParam = comScriptMgr.getXfproductInAlign(axisID);
    int binning = prenewstParam.getBinByFactor();
    try {
      if (binning > 1) {
        xfproductParam.setScaleShifts("1," + String.valueOf(binning));
      }
      else {
        xfproductParam.setScaleShifts("/");
      }
      comScriptMgr.saveXfproductInAlign(xfproductParam, axisID);
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Align Parameter Syntax Error (xfproduct)";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      mainFrame.openMessageDialog(errorMessage, "Align Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Get the fiducialess parameters from the specified dialog and set the
   * metaData and rotationXF script
   * 
   * @param axisID
   * @param dialog
   * @return
   */
  private boolean updateFiducialessParams(FiducialessParams dialog,
    AxisID axisID) {
    float tiltAxisAngle;
    try {
      tiltAxisAngle = dialog.getTiltAxisAngle();
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Tilt axis rotation format error";
      errorMessage[1] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Tilt axis rotation syntax error");
      return false;
    }
    metaData.setFiducialessAlignment(dialog.isFiducialessAlignment());
    metaData.setImageRotation(tiltAxisAngle, axisID);

    try {
      if (metaData.isFiducialessAlignment()) {
        updateRotationXF(tiltAxisAngle, axisID);
        processMgr.setupNonFiducialAlign(axisID);
      }
      else {
        processMgr.setupFiducialAlign(axisID);
      }
    }
    catch (IOException except) {
      mainFrame.openMessageDialog(except.getMessage(), "IOException");
      return false;
    }
    return true;
  }

  /**
   * Return the CoarseAlignDialog associated the specified AxisID
   * 
   * @param axisID
   * @return
   */
  private CoarseAlignDialog mapCoarseAlignDialog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return coarseAlignDialogB;
    }
    return coarseAlignDialogA;
  }

  /**
   * Write out the rotation transform for the specified axis
   * 
   * @param axisID
   */
  private void updateRotationXF(float angle, AxisID axisID) {
    //  Open the appropriate rotation file
    String fnRotationXF = System.getProperty("user.dir") + File.separator
      + "rotation" + axisID.getExtension() + ".xf";
    File rotationXF = new File(fnRotationXF);
    try {
      BufferedWriter out = new BufferedWriter(new FileWriter(rotationXF));
      //  Write out the transform to perform the rotation
      double rads = -1 * angle * Math.PI / 180;
      out.write(String.valueOf(Math.cos(rads)) + "   "
        + String.valueOf(Math.sin(-rads)) + "   "
        + String.valueOf(Math.sin(rads)) + "   "
        + String.valueOf(Math.cos(rads)) + "   0   0");
      out.newLine();
      //  Close the file
      out.close();
    }
    catch (IOException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Rotation Transform IO Exception";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = fnRotationXF;
      mainFrame.openMessageDialog(errorMessage,
        "Rotation Transform IO Exception");
    }
    //  Create the new _nonfid.xf from the updated rotation.xf
    generateNonFidXF(axisID);
  }

  /**
   * Create the non fiducial .xf files and setup the non fiducial file
   * stucture.
   **/
  private void generateNonFidXF(AxisID axisID) {
    nextProcess = "";
    try {
      processMgr.generateNonFidXF(axisID);
    }
    catch (SystemProcessException except) {
      mainFrame.openMessageDialog(except.getMessage(), "SystemProcessException");
    }
  }

  /**
   * Open the fiducial model generation dialog
   */
  public void openFiducialModelDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    mainFrame.selectButton(axisID, "Fiducial Model Gen.");
    if (showIfExists(fiducialModelDialogA, fiducialModelDialogB, axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    FiducialModelDialog fiducialModelDialog = new FiducialModelDialog(this,
      axisID);
    if (axisID == AxisID.SECOND) {
      fiducialModelDialogB = fiducialModelDialog;
    }
    else {
      fiducialModelDialogA = fiducialModelDialog;
    }
    updateDialog(fiducialModelDialog, axisID);
    //  Load the required track{|a|b}.com files, fill in the dialog box
    // params
    //  and set it to the appropriate state
    comScriptMgr.loadTrack(axisID);
    //  Create a default transferfid object to populate the alignment dialog
    fiducialModelDialog.setTransferFidParams(getTransferfidParam());
    fiducialModelDialog.setBeadtrackParams(comScriptMgr.getBeadtrackParam(axisID));
    mainFrame.showProcess(fiducialModelDialog.getContainer(), axisID);
  }

  /**
   *  
   */
  public void doneFiducialModelDialog(AxisID axisID) {
    //  Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (axisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }
    if (fiducialModelDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update fiducial model without an active fiducial model dialog",
        "Program logic error");
      return;
    }
    TransferfidParam transferfidParam = new TransferfidParam();
    fiducialModelDialog.getTransferFidParams(transferfidParam);
    metaData.saveTransferfid(transferfidParam);
    isDataParamDirty = true;
    DialogExitState exitState = fiducialModelDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (!updateTrackCom(axisID)) {
        return;
      }
      if (exitState == DialogExitState.EXECUTE) {
        processTrack.setFiducialModelState(ProcessState.COMPLETE, axisID);
        mainFrame.setFiducialModelState(ProcessState.COMPLETE, axisID);
        openFineAlignmentDialog(axisID);
      }
      else {
        processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
        mainFrame.setFiducialModelState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      fiducialModelDialogB = null;
    }
    else {
      fiducialModelDialogA = null;
    }
    fiducialModelDialog = null;
  }

  /**
   * Open 3dmod with the seed model
   */
  public void imodSeedFiducials(AxisID axisID) {
    String seedModel = metaData.getDatasetName() + axisID.getExtension()
      + ".seed";
    try {
      imodManager.model(ImodManager.COARSE_ALIGNED_KEY, axisID, seedModel,
        true, true);
      processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
      mainFrame.setFiducialModelState(ProcessState.INPROGRESS, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on coarse aligned stack with model: " + seedModel);
    }
  }

  /**
   * Get the beadtrack parameters from the fiducial model dialog and run the
   * track com script
   */
  public void fiducialModelTrack(AxisID axisID) {
    if (updateTrackCom(axisID)) {
      processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
      mainFrame.setFiducialModelState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.fiducialModelTrack(axisID);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute track" + axisID.getExtension() + ".com";
        message[1] = e.getMessage();
        mainFrame.openMessageDialog(message, "Unable to execute com script");
        return;
      }
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Tracking fiducials", axisID);
    }
  }

  /**
   * Replace the raw stack with the fixed stack created from eraser
   * @param axisID
   */
  public void makeFiducialModelSeedModel(AxisID axisID) {
    String seedModelFilename = System.getProperty("user.dir") + File.separator
      + metaData.getDatasetName() + axisID.getExtension() + ".seed";
    File seedModel = new File(seedModelFilename);
    String fiducialModelFilename = System.getProperty("user.dir")
      + File.separator + metaData.getDatasetName() + axisID.getExtension()
      + ".fid";
    File fiducialModel = new File(fiducialModelFilename);
    if (seedModel.exists()
      && seedModel.lastModified() > fiducialModel.lastModified()) {
      String[] message = new String[3];
      message[0] = "WARNING: The seed model file is more recent the fiducial model file";
      message[1] = "To avoid losing your changes to the seed model file,";
      message[2] = "track fiducials before pressing Use Fiducial Model as Seed.";
      mainFrame.openMessageDialog(message, "Use Fiducial Model as Seed Failed");
      return;
    }
    mainFrame.setProgressBar("Using Fiducial Model as Seed", 1, axisID);
    processTrack.setFiducialModelState(ProcessState.INPROGRESS, axisID);
    mainFrame.setFiducialModelState(ProcessState.INPROGRESS, axisID);
    String origSeedModelFilename = System.getProperty("user.dir")
      + File.separator + metaData.getDatasetName() + axisID.getExtension()
      + "_orig.seed";
    File origSeedModel = new File(origSeedModelFilename);
    //backup original seed model file
    if (seedModel.exists() && origSeedModel.exists()) {
      backupFile(seedModel);
    }
    try {
      if (seedModel.exists() && !origSeedModel.exists()) {
        Utilities.renameFile(seedModel, origSeedModel);
      }
      //rename fiducial model file to seed model file
      Utilities.renameFile(fiducialModel, seedModel);
    }
    catch (IOException except) {
      mainFrame.openMessageDialog(except.getMessage(), "File Rename Error");
    }
    try {
      if (imodManager.isOpen(ImodManager.COARSE_ALIGNED_KEY, axisID)) {
        if (seedModel.getName().equals(
          imodManager.getModelName(ImodManager.COARSE_ALIGNED_KEY, axisID))) {
          String[] message = new String[2];
          message[0] = "The old seed model file is open in 3dmod";
          message[1] = "Should it be closed?";
          if (mainFrame.openYesNoDialog(message)) {
            imodManager.quit(ImodManager.COARSE_ALIGNED_KEY, axisID);
          }
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
      System.err.println("Axis type exception in replaceRawStack");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      System.err.println("System process exception in replaceRawStack");
    }
    mainFrame.stopProgressBar(axisID);
    if (axisID == AxisID.SECOND) {
      updateDialog(fiducialModelDialogA, AxisID.FIRST);
    }
    else {
      updateDialog(fiducialModelDialogB, AxisID.SECOND);
    }
  }

  /**
   * Open 3dmod with the new fidcuial model
   */
  public void imodFixFiducials(AxisID axisID) {
    String fiducialModel = metaData.getDatasetName() + axisID.getExtension()
      + ".fid";
    try {
      imodManager.setOpenBeadFixer(ImodManager.COARSE_ALIGNED_KEY, axisID, true);
      imodManager.model(ImodManager.COARSE_ALIGNED_KEY, axisID, fiducialModel,
        true, false);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on coarse aligned stack with model: " + fiducialModel);
    }
  }

  /**
   * Update the specified track com script
   */
  private boolean updateTrackCom(AxisID axisID) {
    //  Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (axisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }
    if (fiducialModelDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update track?.com without an active fiducial model dialog",
        "Program logic error");
      return false;
    }
    try {
      BeadtrackParam beadtrackParam = comScriptMgr.getBeadtrackParam(axisID);
      fiducialModelDialog.getBeadtrackParams(beadtrackParam);
      comScriptMgr.saveTrack(beadtrackParam, axisID);
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      errorMessage[2] = "New value: " + except.getNewString();
      mainFrame.openMessageDialog(errorMessage,
        "Beadtrack Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Beadtrack Parameter Syntax Error";
      errorMessage[1] = axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Beadtrack Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Open the alignment estimation dialog
   */
  public void openFineAlignmentDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    mainFrame.selectButton(axisID, "Fine Alignment");
    if (showIfExists(fineAlignmentDialogA, fineAlignmentDialogB, axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    AlignmentEstimationDialog fineAlignmentDialog = new AlignmentEstimationDialog(
      this, axisID);
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialogB = fineAlignmentDialog;
    }
    else {
      fineAlignmentDialogA = fineAlignmentDialog;
    }
    //  Load the required align{|a|b}.com files, fill in the dialog box
    // params
    //  and set it to the appropriate state
    comScriptMgr.loadAlign(axisID);
    fineAlignmentDialog.setTiltalignParams(comScriptMgr.getTiltalignParam(axisID));
    //  Create a default transferfid object to populate the alignment dialog
    mainFrame.showProcess(fineAlignmentDialog.getContainer(), axisID);
  }

  /**
   *  
   */
  public void doneAlignmentEstimationDialog(AxisID axisID) {
    //  Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog;
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialog = fineAlignmentDialogB;
    }
    else {
      fineAlignmentDialog = fineAlignmentDialogA;
    }
    if (fineAlignmentDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update align?.com without an active alignment dialog",
        "Program logic error");
      return;
    }
    DialogExitState exitState = fineAlignmentDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (!updateAlignCom(axisID)) {
        return;
      }
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
        mainFrame.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
      else {
        processTrack.setFineAlignmentState(ProcessState.COMPLETE, axisID);
        mainFrame.setFineAlignmentState(ProcessState.COMPLETE, axisID);
        openTomogramPositioningDialog(axisID);

        // Check to see if the user wants to keep any coarse aligned imods
        // open
        try {
          if (imodManager.isOpen(ImodManager.COARSE_ALIGNED_KEY, axisID)) {
            String[] message = new String[2];
            message[0] = "The coarsely aligned stack is open in 3dmod";
            message[1] = "Should it be closed?";
            if (mainFrame.openYesNoDialog(message)) {
              imodManager.quit(ImodManager.COARSE_ALIGNED_KEY, axisID);
            }
          }
        }
        catch (AxisTypeException except) {
          except.printStackTrace();
          mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
        }
        catch (SystemProcessException except) {
          except.printStackTrace();
          mainFrame.openMessageDialog(except.getMessage(),
            "Problem closing coarse stack");
        }
      }
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialogB = null;
    }
    else {
      fineAlignmentDialogA = null;
    }
    fineAlignmentDialog = null;
  }

  /**
   * Execute the fine alignment script (align.com) for the appropriate axis
   * This will also reset the fiducialess alignment flag, setFiducialAlign
   * does not need to be called because the necessary copies are called at the
   * end of the align script.
   * 
   * @param the
   *            AxisID identifying the axis to align.
   */
  public void fineAlignment(AxisID axisID) {
    if (!updateAlignCom(axisID)) {
      return;
    }
    processTrack.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
    mainFrame.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.fineAlignment(axisID);
      metaData.setFiducialessAlignment(false);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute align" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, axisID);
    mainFrame.startProgressBar("Aligning stack", axisID);
  }

  /**
   * Open 3dmod with the new fidcuial model
   */
  public void imodViewResiduals(AxisID axisID) {
    String fiducialModel = metaData.getDatasetName() + axisID.getExtension()
      + ".resmod";
    try {
      imodManager.model(ImodManager.COARSE_ALIGNED_KEY, axisID, fiducialModel,
        false, true);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on coarse aligned stack with model: " + fiducialModel);
    }
  }

  /**
   * Open 3dmodv with the new fidcuial model
   */
  public void imodView3DModel(AxisID axisID) {
    String fiducialModel = metaData.getDatasetName() + axisID.getExtension()
      + ".3dmod";
    try {
      imodManager.open(ImodManager.FIDUCIAL_MODEL_KEY, axisID, fiducialModel);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on fine aligned stack");
    }
  }

  /**
   * Open 3dmod to view the coarsely aligned stack
   * 
   * @param axisID
   *            the AxisID to coarse align.
   */
  public void imodFineAlign(AxisID axisID) {
    try {
      imodManager.open(ImodManager.FINE_ALIGNED_KEY, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on fine aligned stack");
    }
  }

  /**
   * Open 3dmod to view the MTF filter results
   * 
   * @param axisID
   *            the AxisID to coarse align.
   */
  public void imodMTFFilter(AxisID axisID) {
    try {
      imodManager.open(ImodManager.MTF_FILTER_KEY, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on MTF filter results");
    }
  }

  /**
   * Transfer the fiducial to the specified axis
   * 
   * @param destAxisID
   */
  public void transferfid(AxisID destAxisID) {
    //  Set a reference to the correct object
    FiducialModelDialog fiducialModelDialog;
    if (destAxisID == AxisID.SECOND) {
      fiducialModelDialog = fiducialModelDialogB;
    }
    else {
      fiducialModelDialog = fiducialModelDialogA;
    }
    if (destAxisID != AxisID.ONLY
      && !Utilities.fileExists(metaData, "fid.xyz", (destAxisID == AxisID.FIRST
        ? AxisID.SECOND
        : AxisID.FIRST))) {
      mainFrame.openMessageDialog(
        "It is recommended that you run Fine Alignment on axis "
          + (destAxisID == AxisID.FIRST ? "B" : "A") + " at least once",
        "Warning");
    }
    if (fiducialModelDialog != null) {
      TransferfidParam transferfidParam = new TransferfidParam();
      // Setup the default parameters depending upon the axis to transfer
      // the
      // fiducials from
      String datasetName = metaData.getDatasetName();
      transferfidParam.setDatasetName(datasetName);
      if (destAxisID == AxisID.FIRST) {
        transferfidParam.setBToA(true);
      }
      else {
        transferfidParam.setBToA(false);
      }
      // Get any user specified changes
      fiducialModelDialog.getTransferFidParams(transferfidParam);
      String threadName;
      try {
        threadName = processMgr.transferFiducials(transferfidParam);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute transferfid command";
        message[1] = e.getMessage();
        mainFrame.openMessageDialog(message, "Unable to execute command");
        return;
      }
      setThreadName(threadName, destAxisID);
      mainFrame.startProgressBar("Transferring fiducials", destAxisID);
      updateDialog(fiducialModelDialog, destAxisID);
    }
  }

  private TransferfidParam getTransferfidParam() {
    TransferfidParam param = new TransferfidParam();
    metaData.initializeTransferfid(param);
    return param;
  }

  /**
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters
   * from the alignment estimation dialog. This also updates the local
   * alignment state of the appropriate tilt files.
   */
  private boolean updateAlignCom(AxisID axisID) {
    //  Set a reference to the correct object
    AlignmentEstimationDialog fineAlignmentDialog;
    if (axisID == AxisID.SECOND) {
      fineAlignmentDialog = fineAlignmentDialogB;
    }
    else {
      fineAlignmentDialog = fineAlignmentDialogA;
    }
    if (fineAlignmentDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update align?.com without an active alignment dialog",
        "Program logic error");
      return false;
    }
    try {
      TiltalignParam tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
      fineAlignmentDialog.getTiltalignParams(tiltalignParam);
      comScriptMgr.saveAlign(tiltalignParam, axisID);
      //  Update the tilt.com script with the dependent parameters
      updateTiltCom(tiltalignParam, axisID);
      mainFrame.setFineAlignmentState(ProcessState.INPROGRESS, axisID);
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = except.getNewString();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Tiltalign Parameter Syntax Error");
      return false;
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Tiltalign Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Update the tilt parameters dependent on the align script - local
   * alignments file - the exclude list
   */
  private void updateTiltCom(ConstTiltalignParam tiltalignParam,
    AxisID currentAxis) {
    comScriptMgr.loadTilt(currentAxis);
    TiltParam tiltParam = comScriptMgr.getTiltParam(currentAxis);
    String alignFileExtension = currentAxis.getExtension() + "local.xf";
    if (tiltalignParam.getLocalAlignments()) {
      tiltParam.setLocalAlignFile(getDatasetName() + alignFileExtension);
    }
    else {
      tiltParam.setLocalAlignFile("");
    }
    tiltParam.setExcludeList(tiltalignParam.getIncludeExcludeList());
    comScriptMgr.saveTilt(tiltParam, currentAxis);
  }

  /**
   * Open the tomogram positioning dialog for the specified axis
   * @param axisID
   */
  public void openTomogramPositioningDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    mainFrame.selectButton(axisID, "Tomogram Positioning");
    if (showIfExists(tomogramPositioningDialogA, tomogramPositioningDialogB,
      axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    TomogramPositioningDialog tomogramPositioningDialog = new TomogramPositioningDialog(
      this, axisID);
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialogB = tomogramPositioningDialog;
    }
    else {
      tomogramPositioningDialogA = tomogramPositioningDialog;
    }
    // Get the original image size and pass it to the
    // TomogramGenerationDialog.
    // It is needed to set the full image size correctly
    // TODO: get the right size for montaging using montagesize
    // TODO: this functionality is the same as the openTomo...Gen.. method
    MRCHeader stackHeader = new MRCHeader(metaData.getDatasetName()
      + axisID.getExtension() + ".st");
    try {
      stackHeader.read();
      tomogramPositioningDialog.setFullImageSize(stackHeader.getNColumns(),
        stackHeader.getNRows());
    }
    catch (IOException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Unable to read full image size from projection image stack";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "MRCHeader IO Error");
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Unable to read full image size from projection image stack";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "MRCHeader Invalid Parameter Error");
    }

    // Read in the newst{|a|b}.com parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions
    comScriptMgr.loadNewst(axisID);
    tomogramPositioningDialog.setNewstParams(comScriptMgr.getNewstComNewstParam(axisID));

    // Get the align{|a|b}.com parameters
    comScriptMgr.loadAlign(axisID);
    tomogramPositioningDialog.setAlignParams(comScriptMgr.getTiltalignParam(axisID));

    // Get the tilt{|a|b}.com parameters
    comScriptMgr.loadTilt(axisID);
    tomogramPositioningDialog.setTiltParams(comScriptMgr.getTiltParam(axisID));

    // Get the tomopitch{|a|b}.com parameters
    comScriptMgr.loadTomopitch(axisID);
    tomogramPositioningDialog.setTomopitchParams(comScriptMgr.getTomopitchParam(axisID));

    // Set the whole tomogram sampling state, fidcialess state, and tilt axis
    // angle
    tomogramPositioningDialog.setWholeTomogramSampling(metaData.isWholeTomogramSample());

    tomogramPositioningDialog.setFiducialessAlignment(metaData.isFiducialessAlignment());
    tomogramPositioningDialog.setTiltAxisAngle(metaData.getImageRotation(axisID));

    // Open the dialog panel
    mainFrame.showProcess(tomogramPositioningDialog.getContainer(), axisID);
  }

  /**
   *  
   */
  public void doneTomogramPositioningDialog(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    if (tomogramPositioningDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update sample.com without an active positioning dialog",
        "Program logic error");
      return;
    }

    DialogExitState exitState = tomogramPositioningDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get all of the parameters from the panel
      if (!updateAlignCom(tomogramPositioningDialog, axisID)) {
        return;
      }
      if (!updateSampleTiltCom(axisID)) {
        return;
      }
      if (!updateTomopitchCom(axisID)) {
        return;
      }
      if (!updateFiducialessParams(tomogramPositioningDialog, axisID)) {
        return;
      }
      if (!updateNewstCom(tomogramPositioningDialog, axisID)) {
        return;
      }

      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setTomogramPositioningState(ProcessState.INPROGRESS,
          axisID);
        mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
      else {
        processTrack.setTomogramPositioningState(ProcessState.COMPLETE, axisID);
        mainFrame.setTomogramPositioningState(ProcessState.COMPLETE, axisID);
        openTomogramGenerationDialog(axisID);
        try {
          if (imodManager.isOpen(ImodManager.SAMPLE_KEY, axisID)) {
            String[] message = new String[2];
            message[0] = "The sample reconstruction is open in 3dmod";
            message[1] = "Should it be closed?";
            if (mainFrame.openYesNoDialog(message)) {
              imodManager.quit(ImodManager.SAMPLE_KEY, axisID);
            }
          }
        }
        catch (AxisTypeException except) {
          except.printStackTrace();
          mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
        }
        catch (SystemProcessException except) {
          except.printStackTrace();
          mainFrame.openMessageDialog(except.getMessage(),
            "Problem closing sample reconstruction");
        }
      }
    }

    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      tomogramPositioningDialogB = null;
    }
    else {
      tomogramPositioningDialogA = null;
    }
    tomogramPositioningDialog = null;
  }

  /**
   * Run the sample com script
   */
  public void createSample(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    // Make sure that we have an active positioning dialog
    if (tomogramPositioningDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update sample.com without an active positioning dialog",
        "Program logic error");
      return;
    }
    //  Get the user input data from the dialog box
    if (!updateFiducialessParams(tomogramPositioningDialog, axisID)) {
      return;
    }
    if (!updateSampleTiltCom(axisID)) {
      return;
    }
    processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.createSample(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute sample" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, axisID);
    mainFrame.startProgressBar("Creating sample tomogram", axisID);
  }

  /**
   * Create a whole tomogram for positioning the tomogram in the volume
   * 
   * @param axisID
   */
  public void wholeTomogram(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    // Get the user input from the dialog
    if (!updateFiducialessParams(tomogramPositioningDialog, axisID)) {
      return;
    }
    if (!updateNewstCom(tomogramPositioningDialog, axisID)) {
      return;
    }
    if (!updateSampleTiltCom(axisID)) {
      return;
    }
    processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    nextProcess = "tilt";
    String threadName;
    try {
      threadName = processMgr.newst(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Open 3dmod on the sample volume for the specified axis along with the
   * tomopitch model
   * 
   * @param axisID
   */
  public void imodSample(AxisID axisID) {
    try {
      imodManager.open(ImodManager.SAMPLE_KEY, axisID);
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Problem opening sample reconstruction");
    }
  }

  /**
   * Open 3dmod on the full volume along with the tomopitch model
   * 
   * @param axisID
   */
  public void imodFullSample(AxisID axisID) {
    String tomopitchModelName = "tomopitch" + axisID.getExtension() + ".mod";
    try {
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID);
      imodManager.model(ImodManager.FULL_VOLUME_KEY, axisID, tomopitchModelName, true);
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Problem opening sample reconstruction");
    }
  }

  /**
   *  
   */
  public void tomopitch(AxisID axisID) {
    if (updateTomopitchCom(axisID)) {
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.tomopitch(axisID);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute tomopitch" + axisID.getExtension()
          + ".com";
        message[1] = e.getMessage();
        mainFrame.openMessageDialog(message, "Unable to execute com script");
        return;
      }
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Finding sample position", axisID);
    }
  }

  /**
   * Open the tomopitch log file
   * 
   * @param axisID
   */
  public void openTomopitchLog(AxisID axisID) {
    String logFileName = "tomopitch" + axisID.getExtension() + ".log";
    TextPageWindow logFileWindow = new TextPageWindow();
    logFileWindow.setVisible(logFileWindow.setFile(System.getProperty("user.dir")
      + File.separator + logFileName));
  }

  /**
   * Compute the final alignment from the updated parameters in the
   * positioning dialog.
   * 
   * @param axisID
   */
  public void finalAlign(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    if (updateAlignCom(tomogramPositioningDialog, axisID)) {
      processTrack.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      mainFrame.setTomogramPositioningState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.fineAlignment(axisID);
        metaData.setFiducialessAlignment(false);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute align" + axisID.getExtension() + ".com";
        message[1] = e.getMessage();
        mainFrame.openMessageDialog(message, "Unable to execute com script");
        return;
      }
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("Calculating final alignment", axisID);
    }
  }

  /**
   * Update the tilt{|a|b}.com file with sample parameters for the specified
   * axis
   */
  private boolean updateSampleTiltCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    // Make sure that we have an active positioning dialog
    if (tomogramPositioningDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update sample.com without an active positioning dialog",
        "Program logic error");
      return false;
    }
    // Get the current tilt parameters, make any user changes and save the
    // parameters back to the tilt{|a|b}.com
    try {
      TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
      tomogramPositioningDialog.getTiltParams(tiltParam);
      String outputFileName;
      if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
        outputFileName = metaData.getDatasetName() + "_full.rec";
      }
      else {
        outputFileName = metaData.getDatasetName() + axisID.getExtension()
          + ".rec";
      }
      tiltParam.setOutputFile(outputFileName);
      comScriptMgr.saveTilt(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "Tilt Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Update the tomopitch{|a|b}.com file with sample parameters for the
   * specified axis
   */
  private boolean updateTomopitchCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramPositioningDialog tomogramPositioningDialog = mapPositioningDialog(axisID);
    // Make sure that we have an active positioning dialog
    if (tomogramPositioningDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update tomopitch.com without an active positioning dialog",
        "Program logic error");
      return false;
    }
    // Get the current tilt parameters, make any user changes and save the
    // parameters back to the tilt{|a|b}.com
    try {
      TomopitchParam tomopitchParam = comScriptMgr.getTomopitchParam(axisID);
      tomogramPositioningDialog.getTomopitchParams(tomopitchParam);
      comScriptMgr.saveTomopitch(tomopitchParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tomopitch Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Tomopitch Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Return the TomogramPositioningDialog associated the specified AxisID
   * 
   * @param axisID
   * @return
   */
  private TomogramPositioningDialog mapPositioningDialog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramPositioningDialogB;
    }
    return tomogramPositioningDialogA;
  }

  /**
   * updateAlignCom updates the align{|a|b}.com scripts with the parameters
   * from the tomogram positioning dialog. The positioning dialog is passed so
   * that the signature is different from the standard method.
   */
  private boolean updateAlignCom(
    TomogramPositioningDialog tomogramPositioningDialog, AxisID axisID) {
    try {
      TiltalignParam tiltalignParam = comScriptMgr.getTiltalignParam(axisID);
      tomogramPositioningDialog.getAlignParams(tiltalignParam);
      comScriptMgr.saveAlign(tiltalignParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tiltalign Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Tiltalign Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Update the newst{|a|b}.com scripts with the parameters from the tomogram
   * positioning dialog. The positioning dialog is passed so that the
   * signature is different from the standard method.
   * 
   * @param tomogramPositioningDialog
   * @param axisID
   * @return
   */
  private boolean updateNewstCom(
    TomogramPositioningDialog tomogramPositioningDialog, AxisID axisID) {

    //  Get the whole tomogram positions state
    metaData.setWholeTomogramSample(tomogramPositioningDialog.isWholeTomogramSampling());
    isDataParamDirty = true;

    NewstParam newstParam = comScriptMgr.getNewstComNewstParam(axisID);
    tomogramPositioningDialog.getNewstParamst(newstParam);
    try {
      // Make sure the size output is removed, it was only there as a 
      // copytomocoms template
      newstParam.setSizeToOutputInXandY("/");
    }
    catch (FortranInputSyntaxException e) {
      e.printStackTrace();
    }
    comScriptMgr.saveNewst(newstParam, axisID);
    return true;
  }

  /**
   * Open the tomogram generation dialog
   */
  public void openTomogramGenerationDialog(AxisID axisID) {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    // 
    mainFrame.selectButton(axisID, "Tomogram Generation");
    if (showIfExists(tomogramGenerationDialogA, tomogramGenerationDialogB,
      axisID)) {
      return;
    }
    // Create a new dialog panel and map it the generic reference
    TomogramGenerationDialog tomogramGenerationDialog = new TomogramGenerationDialog(
      this, axisID);
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialogB = tomogramGenerationDialog;
    }
    else {
      tomogramGenerationDialogA = tomogramGenerationDialog;
    }
    // Get the original image size and pass it to the
    // TomogramGenerationDialog.
    // It is needed to set the full image size correctly
    // TODO: get the right size for montaging using montagesize
    MRCHeader stackHeader = new MRCHeader(metaData.getDatasetName()
      + axisID.getExtension() + ".st");
    try {
      stackHeader.read();
      tomogramGenerationDialog.setFullImageSize(stackHeader.getNColumns(),
        stackHeader.getNRows());
    }
    catch (IOException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Unable to read full image size from projection image stack";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "MRCHeader IO Error");
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Unable to read full image size from projection image stack";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "MRCHeader Invalid Parameter Error");
    }
    // Read in the newst{|a|b}.com parameters. WARNING this needs to be done
    // before reading the tilt paramers below so that the GUI knows how to
    // correctly scale the dimensions
    comScriptMgr.loadNewst(axisID);
    tomogramGenerationDialog.setNewstParams(comScriptMgr.getNewstComNewstParam(axisID));
    // Read in the tilt{|a|b}.com parameters and display the dialog panel
    comScriptMgr.loadTilt(axisID);
    comScriptMgr.loadMTFFilter(axisID);
    tomogramGenerationDialog.setTiltParams(comScriptMgr.getTiltParam(axisID));
    tomogramGenerationDialog.setMTFFilterParam(comScriptMgr.getMTFFilterParam(axisID));
    updateDialog(tomogramGenerationDialog, axisID);

    //  Set the fidcialess state and tilt axis angle
    tomogramGenerationDialog.setFiducialessAlignment(metaData.isFiducialessAlignment());
    tomogramGenerationDialog.setTiltAxisAngle(metaData.getImageRotation(axisID));

    mainFrame.showProcess(tomogramGenerationDialog.getContainer(), axisID);
  }

  /**
   *  
   */
  public void doneTomogramGenerationDialog(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update tilt?.com without an active tomogram generation dialog",
        "Program logic error");
      return;
    }
    DialogExitState exitState = tomogramGenerationDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(axisID);
    }
    else {
      //  Get the user input data from the dialog box
      if (!updateNewstCom(axisID)) {
        return;
      }
      if (!updateTiltCom(axisID, true)) {
        return;
      }
      if (!updateMTFFilterCom(axisID)) {
        return;
      }
      if (!updateFiducialessParams(tomogramGenerationDialog, axisID)) {
        return;
      }

      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
        mainFrame.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
        mainFrame.showBlankProcess(axisID);
      }
      else {
        processTrack.setTomogramGenerationState(ProcessState.COMPLETE, axisID);
        mainFrame.setTomogramGenerationState(ProcessState.COMPLETE, axisID);
        if (isDualAxis()) {
          openTomogramCombinationDialog();
          if (axisID == AxisID.SECOND) {
            mainFrame.showBlankProcess(axisID);
          }
        }
        else {
          openPostProcessingDialog();
        }
      }
    }
    //  Clean up the existing dialog
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialogB = null;
    }
    else {
      tomogramGenerationDialogA = null;
    }
    tomogramGenerationDialog = null;
  }

  /**
   * Update the tilt.com from the TomogramGenerationDialog
   * 
   * @param axisID
   * @param useDefaultRec
   *            If true set the reconstruction output filename to what is
   *            expected of the com scripts. If false use the trial tomogram
   *            filename specified in the TomogramGenerationDialog
   * @return true if successful
   */
  private boolean updateTiltCom(AxisID axisID, boolean useDefaultRec) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update tilt?.com without an active tomogram generation dialog",
        "Program logic error");
      return false;
    }
    try {
      TiltParam tiltParam = comScriptMgr.getTiltParam(axisID);
      tomogramGenerationDialog.getTiltParams(tiltParam);
      if (useDefaultRec) {
        String outputFileName;
        if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
          outputFileName = metaData.getDatasetName() + "_full.rec";
        }
        else {
          outputFileName = metaData.getDatasetName() + axisID.getExtension()
            + ".rec";
        }
        tiltParam.setOutputFile(outputFileName);
      }
      else {
        String trialTomogramName = tomogramGenerationDialog.getTrialTomogramName();
        tiltParam.setOutputFile(trialTomogramName);
      }
      comScriptMgr.saveTilt(tiltParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "Tilt Parameter Syntax Error");
      return false;
    }
    catch (InvalidParameterException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "Tilt Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "Tilt Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Update the mtffilter.com from the TomogramGenerationDialog
   * 
   * @param axisID
   * @return true if successful
   */
  private boolean updateMTFFilterCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update mtffilter?.com without an active tomogram generation dialog",
        "Program logic error");
      return false;
    }
    try {
      MTFFilterParam mtfFilterParam = comScriptMgr.getMTFFilterParam(axisID);
      tomogramGenerationDialog.getMTFFilterParam(mtfFilterParam);
      String inputFileName;
      String outputFileName;
      if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
        inputFileName = metaData.getDatasetName() + AxisID.ONLY.getExtension()
          + ".ali";
        outputFileName = metaData.getDatasetName() + AxisID.ONLY.getExtension()
          + "_filt.ali";
      }
      else {
        inputFileName = metaData.getDatasetName() + axisID.getExtension()
          + ".ali";
        outputFileName = metaData.getDatasetName() + axisID.getExtension()
          + "_filt.ali";
      }
      mtfFilterParam.setInputFile(inputFileName);
      mtfFilterParam.setOutputFile(outputFileName);
      comScriptMgr.saveMTFFilter(mtfFilterParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "MTF Filter Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "MTF Filter Parameter Syntax Error");
      return false;
    }
    catch (FortranInputSyntaxException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "MTF Filter Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "MTF Filter Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Update the newst.com from the TomogramGenerationDialog
   * 
   * @param axisID
   * @return true if successful
   */
  private boolean updateNewstCom(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);
    if (tomogramGenerationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update newst?.com without an active tomogram generation dialog",
        "Program logic error");
      return false;
    }
    try {
      NewstParam newstParam = comScriptMgr.getNewstComNewstParam(axisID);
      // Make sure the size output is removed, it was only there for a
      // copytomocoms template
      newstParam.setSizeToOutputInXandY("/");
      tomogramGenerationDialog.getNewstParams(newstParam);
      comScriptMgr.saveNewst(newstParam, axisID);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[3];
      errorMessage[0] = "newst Parameter Syntax Error";
      errorMessage[1] = "Axis: " + axisID.getExtension();
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "Newst Parameter Syntax Error");
      return false;
    }
    catch (FortranInputSyntaxException except) {
      except.printStackTrace();
    }
    return true;
  }

  /**
   * Return the TomogramGenerationDialog associated with the specified AxisID
   * 
   * @param axisID
   * @return
   */
  private TomogramGenerationDialog mapGenerationDialog(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return tomogramGenerationDialogB;
    }
    return tomogramGenerationDialogA;
  }

  /**
   *  
   */
  public void newst(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog = mapGenerationDialog(axisID);

    // Get the user input from the dialog
    if (!updateFiducialessParams(tomogramGenerationDialog, axisID)) {
      return;
    }
    if (!updateNewstCom(axisID)) {
      return;
    }

    processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
    mainFrame.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
    String threadName;
    try {
      threadName = processMgr.newst(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute newst" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, axisID);

  }

  /**
   * Replace the full aligned stack with the filtered full aligned stack
   * created from mtffilter
   * 
   * @param axisID
   */
  public void useMtfFilter(AxisID axisID) {
    mainFrame.setProgressBar("Using filtered full aligned stack", 1, axisID);
    // Instantiate file objects for the original raw stack and the fixed
    // stack
    String fullAlignedStackFilename = System.getProperty("user.dir")
      + File.separator + metaData.getDatasetName() + axisID.getExtension()
      + ".ali";
    File fullAlignedStack = new File(fullAlignedStackFilename);
    String filteredFullAlignedStackFilename = System.getProperty("user.dir")
      + File.separator + metaData.getDatasetName() + axisID.getExtension()
      + "_filt.ali";
    File filteredFullAlignedStack = new File(filteredFullAlignedStackFilename);
    if (!filteredFullAlignedStack.exists()) {
      mainFrame.openMessageDialog(
        "The filtered full aligned stack doesn't exist.  Create the filtered full aligned stack first",
        "Filtered full aligned stack missing");
      return;
    }
    processTrack.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    mainFrame.setPreProcessingState(ProcessState.INPROGRESS, axisID);
    //don't have to rename full aligned stack because it is a generated
    // file
    try {
      Utilities.renameFile(filteredFullAlignedStack, fullAlignedStack);
    }
    catch (IOException except) {
      mainFrame.openMessageDialog(except.getMessage(), "File Rename Error");
    }
    try {
      if (imodManager.isOpen(ImodManager.FINE_ALIGNED_KEY, axisID)) {
        String[] message = new String[2];
        message[0] = "The original full aligned stack is open in 3dmod";
        message[1] = "Should it be closed?";
        if (mainFrame.openYesNoDialog(message)) {
          imodManager.quit(ImodManager.FINE_ALIGNED_KEY, axisID);
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
      System.err.println("Axis type exception in useMtfFilter");
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      System.err.println("System process exception in useMtfFilter");
    }
    mainFrame.stopProgressBar(axisID);
  }

  /**
   */
  public void mtffilter(AxisID axisID) {
    if (updateMTFFilterCom(axisID)) {
      processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      mainFrame.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      String threadName;
      try {
        threadName = processMgr.mtffilter(axisID);
      }
      catch (SystemProcessException e) {
        e.printStackTrace();
        String[] message = new String[2];
        message[0] = "Can not execute mtffilter" + axisID.getExtension()
          + ".com";
        message[1] = e.getMessage();
        mainFrame.openMessageDialog(message, "Unable to execute com script");
        return;
      }
      setThreadName(threadName, axisID);
      mainFrame.startProgressBar("MTF Filter", axisID);
    }
  }

  /**
   * Start a tilt process in trial mode
   * 
   * @param axisID
   */
  public void trialTilt(AxisID axisID) {
    if (updateTiltCom(axisID, false)) {
      processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      mainFrame.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      tiltProcess(axisID);
    }
  }

  /**
   * Run the tilt command script for the specified axis
   */
  public void tilt(AxisID axisID) {
    if (updateTiltCom(axisID, true)) {
      processTrack.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      mainFrame.setTomogramGenerationState(ProcessState.INPROGRESS, axisID);
      tiltProcess(axisID);
    }
  }

  /**
   * Tilt process initiator. Since tilt can be started from multiple points in
   * the process chain we need separate the execution from the parameter
   * collection and state updating
   * 
   * @param axisID
   */
  private void tiltProcess(AxisID axisID) {
    nextProcess = "";
    String threadName;
    try {
      threadName = processMgr.tilt(axisID);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute tilt" + axisID.getExtension() + ".com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, axisID);
  }

  /**
   * Open 3dmod to view the tomogram
   * 
   * @param axisID
   *            the AxisID of the tomogram to open.
   */
  public void imodFullVolume(AxisID axisID) {
    try {
      imodManager.open(ImodManager.FULL_VOLUME_KEY, axisID);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod with the tomogram");
    }
  }

  /**
   * Open 3dmod on the current test volume
   * 
   * @param axisID
   */
  public void imodTestVolume(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog;
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialog = tomogramGenerationDialogB;
    }
    else {
      tomogramGenerationDialog = tomogramGenerationDialogA;
    }
    String trialTomogramName = tomogramGenerationDialog.getTrialTomogramName();
    try {
      imodManager.newImod(ImodManager.TRIAL_TOMOGRAM_KEY, axisID,
        trialTomogramName);
      imodManager.open(ImodManager.TRIAL_TOMOGRAM_KEY, axisID);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      String message[] = new String[2];
      message[0] = "Unable to open specified tomogram:" + trialTomogramName;
      message[1] = "Does it exist in the working directory?";
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod with the tomogram");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
  }

  public void commitTestVolume(AxisID axisID) {
    //  Set a reference to the correct object
    TomogramGenerationDialog tomogramGenerationDialog;
    if (axisID == AxisID.SECOND) {
      tomogramGenerationDialog = tomogramGenerationDialogB;
    }
    else {
      tomogramGenerationDialog = tomogramGenerationDialogA;
    }
    String trialTomogramName = tomogramGenerationDialog.getTrialTomogramName();
    //  Check to see if the trial tomogram exist
    File trialTomogramFile = new File(System.getProperty("user.dir"),
      trialTomogramName);
    if (!trialTomogramFile.exists()) {
      String message[] = new String[2];
      message[0] = "The specified tomogram does not exist:" + trialTomogramName;
      message[1] = "It must be calculated before commiting";
      mainFrame.openMessageDialog(message, "Can't rename tomogram");
    }
    // rename the trial tomogram to the output filename of appropriate
    // tilt.com
    File outputFile;
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      outputFile = new File(System.getProperty("user.dir"),
        metaData.getDatasetName() + "_full.rec");
    }
    else {
      outputFile = new File(System.getProperty("user.dir"),
        metaData.getDatasetName() + axisID.getExtension() + ".rec");
    }
    mainFrame.setProgressBar("Using trial tomogram: " + trialTomogramName, 1,
      axisID);
    try {
      Utilities.renameFile(trialTomogramFile, outputFile);
    }
    catch (IOException except) {
      mainFrame.openMessageDialog(except.getMessage(), "File Rename Error");
    }
    mainFrame.stopProgressBar(axisID);
  }

  /**
   * Delete the pre-aligned and aligned stack for the specified axis
   * 
   * @param axisID
   */
  public void deleteAlignedStacks(AxisID axisID) {
    mainFrame.setProgressBar("Deleting aligned image stacks", 1, axisID);
    //
    // Don't do preali now because users may do upto generation before
    // transfering
    // the fiducials
    //
    //     File preali =
    //      new File(
    //        System.getProperty("user.dir"),
    //        metaData.getDatasetName() + axisID.getExtension() + ".preali");
    //    if (preali.exists()) {
    //      if (!preali.delete()) {
    //        mainFrame.openMessageDialog(
    //          "Unable to delete pre-aligned stack: " + preali.getAbsolutePath(),
    //          "Can not delete file");
    //      }
    //    }
    File aligned = new File(System.getProperty("user.dir"),
      metaData.getDatasetName() + axisID.getExtension() + ".ali");
    if (aligned.exists()) {
      if (!aligned.delete()) {
        mainFrame.openMessageDialog("Unable to delete aligned stack: "
          + aligned.getAbsolutePath(), "Can not delete file");
      }
    }
    mainFrame.stopProgressBar(axisID);
  }

  /**
   * Open the tomogram combination dialog
   */
  public void openTomogramCombinationDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    // Verify that this process is applicable
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      mainFrame.openMessageDialog(
        "This step is valid only for a dual axis tomogram",
        "Invalid tomogram combination selection");
      return;
    }
    mainFrame.selectButton(AxisID.FIRST, "Tomogram Combination");
    if (tomogramCombinationDialog == null) {
      tomogramCombinationDialog = new TomogramCombinationDialog(this);
      // Get the setupcombine parameters and set the default patch
      // boundaries if
      // they have not already been set
      CombineParams combineParams = new CombineParams(
        metaData.getCombineParams());
      if (!combineParams.isPatchBoundarySet()) {
        String recFileName;
        if (combineParams.getMatchBtoA()) {
          recFileName = metaData.getDatasetName() + "a.rec";
        }
        else {
          recFileName = metaData.getDatasetName() + "b.rec";
        }
        try {
          combineParams.setDefaultPatchBoundaries(recFileName);
        }
        catch (InvalidParameterException except) {
          String[] detailedMessage = new String[4];
          detailedMessage[0] = "Unable to set default patch boundaries";
          detailedMessage[1] = "Are both tomograms computed and available?";
          detailedMessage[2] = "";
          detailedMessage[3] = except.getMessage();
          mainFrame.openMessageDialog(detailedMessage, "Invalid parameter: "
            + recFileName);
          // Delete the dialog
          tomogramCombinationDialog = null;
          return;
        }
        catch (IOException except) {
          except.printStackTrace();
          mainFrame.openMessageDialog(except.getMessage(), "IO Error: "
            + recFileName);
          //Delete the dialog
          tomogramCombinationDialog = null;
          return;
        }
      }
      // Fill in the dialog box params and set it to the appropriate state
      tomogramCombinationDialog.setCombineParams(combineParams);

      // If setupcombine has been run load the com scripts, otherwise disable the
      // apropriate panels in the tomogram combination dialog
      tomogramCombinationDialog.enableCombineTabs(combineScriptsExist());
      if (combineScriptsExist()) {
        // Check to see if a solvematch.com file exists and load it if so
        //otherwise load the correct old solvematch* file
        File solvematch = new File(System.getProperty("user.dir"),
          "solvematch.com");
        if (solvematch.exists()) {
          loadSolvematch();
        }
        else {
          if (metaData.getCombineParams().isModelBased()) {
            loadSolvematchMod();
          }
          else {
            loadSolvematchShift();
          }
        }
        loadPatchcorr();
        loadMatchorwarp();
      }
    }
    //  Show the process panel
    mainFrame.showProcess(tomogramCombinationDialog.getContainer(),
      AxisID.FIRST);
  }

  /**
   * Open the matching models in the 3dmod reconstruction instances
   */
  public void imodMatchingModel(boolean binBy2) {
    if (tomogramCombinationDialog == null) {
      return;
    }
    // FIXME do we want to be updating here break model (maybe it is saving
    // the bin by 2 state?
    updateCombineParams();

    try {
      if (binBy2) {
        imodManager.setBinning(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST, 2);
        imodManager.setBinning(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND, 2);
      }
      else {
        imodManager.setBinning(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST, 1);
        imodManager.setBinning(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND, 1);
      }
      imodManager.model(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST,
        metaData.getDatasetName() + AxisID.FIRST.getExtension() + ".matmod",
        true);
      imodManager.model(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND,
        metaData.getDatasetName() + AxisID.SECOND.getExtension() + ".matmod",
        true);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on tomograms for matching models");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
  }

  /**
   * Open the matchcheck results in 3dmod
   */
  public void imodMatchCheck() {
    try {
      imodManager.open(ImodManager.MATCH_CHECK_KEY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on matchcheck.mat or matchcheck.rec");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
  }

  /**
   * Open the patch region models in tomogram being matched to
   */
  public void imodPatchRegionModel() {
    try {
      // FIXME do we want to be updating here break model
      // Get the latest combine parameters from the dialog
      updateCombineParams();

      // FIXME axis should be a parameter to the call
      if (metaData.getCombineParams().getMatchBtoA()) {
        imodManager.model(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST,
          "patch_region.mod", true);
      }
      else {
        imodManager.model(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND,
          "patch_region.mod", true);
      }
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on tomogram for patch region models");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
  }

  /**
   * Open the patch vector models in 3dmod
   */
  public void imodPatchVectorModel() {
    try {
      imodManager.open(ImodManager.PATCH_VECTOR_MODEL_KEY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on tomogram for patch vector model");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
  }

  /**
   * Open the tomogram being matched to
   */
  public void imodMatchedToTomogram() {
    try {
      if (metaData.getCombineParams().getMatchBtoA()) {
        imodManager.open(ImodManager.FULL_VOLUME_KEY, AxisID.FIRST);
      }
      else {
        imodManager.open(ImodManager.FULL_VOLUME_KEY, AxisID.SECOND);
      }
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on tomogram being matched to");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
  }

  /**
   * Tomogram combination done method, move on to the post processing window
   */
  public void doneTomogramCombinationDialog() {
    if (tomogramCombinationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update combine.com without an active tomogram combination dialog",
        "Program logic error");
      return;
    }
    DialogExitState exitState = tomogramCombinationDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      mainFrame.showBlankProcess(AxisID.ONLY);
    }
    else {
      // Update the com script and metadata info from the tomogram
      // combination dialog box. Since there are multiple pages and scripts
      // associated with the postpone button get the ones that are appropriate
      updateCombineParams();
      if (tomogramCombinationDialog.isCombinePanelEnabled()) {
        if (!updateSolvematchCom()) {
          return;
        }
        if (!updatePatchcorrCom()) {
          return;
        }
        if (!updateMatchorwarpCom(false)) {
          return;
        }
      }
      if (exitState == DialogExitState.POSTPONE) {
        processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
        mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
        mainFrame.showBlankProcess(AxisID.ONLY);
      }
      else {
        processTrack.setTomogramCombinationState(ProcessState.COMPLETE);
        mainFrame.setTomogramCombinationState(ProcessState.COMPLETE);
        openPostProcessingDialog();
      }
    }
  }

  /**
   * Check to see if the combine scripts exist
   * 
   * @return true if the combine scripts exist
   */
  public boolean combineScriptsExist() {
    File matchvol1 = new File(System.getProperty("user.dir"), "matchvol1.com");
    File matchorwarp = new File(System.getProperty("user.dir"),
      "matchorwarp.com");
    File patchcorr = new File(System.getProperty("user.dir"), "patchcorr.com");
    File volcombine = new File(System.getProperty("user.dir"), "volcombine.com");
    File warpvol = new File(System.getProperty("user.dir"), "warpvol.com");
    return  matchvol1.exists() && matchorwarp.exists() && patchcorr.exists()
      && volcombine.exists() && warpvol.exists();
  }

  /**
   * Run the setupcombine script with the current combine parameters stored in
   * metaData object. updateCombineCom is called first to get the currect
   * parameters from the dialog.
   * 
   * @param tomogramCombinationDialog
   *            the calling dialog.
   */
  public void createCombineScripts() {
    mainFrame.setProgressBar("Creating combine scripts", 1, AxisID.ONLY);
    updateCombineParams();
    try {
      processMgr.setupCombineScripts(metaData);
      processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
      mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
    }
    catch (BadComScriptException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "Can't run setupcombine");
      return;
    }
    catch (IOException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog("Can't run setupcombine\n"
        + except.getMessage(), "Setupcombine IOException");
      return;
    }

    // Reload the initial and final match paramaters from the newly created
    // scripts
    isDataParamDirty = true;

    //  Reload all of the parameters into the ComScriptManager
    loadSolvematch();
    loadPatchcorr();
    loadMatchorwarp();
    
    tomogramCombinationDialog.enableCombineTabs(true);
    mainFrame.stopProgressBar(AxisID.ONLY);
  }

  /**
   * Update the combine parameters from the a specified tab of the calling
   * dialog assumes that the dialog is synchronized
   * 
   * @param tomogramCombinationDialog
   *            the calling dialog.
   */
  private void updateCombineParams() {
    if (tomogramCombinationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update combine.com without an active tomogram combination dialog",
        "Program logic error");
      return;
    }
    CombineParams combineParams = new CombineParams();
    try {
      tomogramCombinationDialog.getCombineParams(combineParams);
      if (!combineParams.isValid()) {
        mainFrame.openMessageDialog(combineParams.getInvalidReasons(),
          "Invalid combine parameters");
        return;
      }
    }
    catch (NumberFormatException except) {
      mainFrame.openMessageDialog(except.getMessage(), "Number format error");
      return;
    }
    CombineParams originalCombineParams = metaData.getCombineParams();
    if (!originalCombineParams.equals(combineParams)) {
      metaData.setCombineParams(combineParams);
      isDataParamDirty = true;
    }
    return;
  }

  /**
   * Load the solvematch com script into the tomogram combination dialog
   */
  public void loadSolvematch() {
    comScriptMgr.loadSolvematch();
    tomogramCombinationDialog.setSolvematchParams(comScriptMgr.getSolvematch());
  }

  /**
   * Update the solvematch com file from the tomogramCombinationDialog
   * @return true if successful, false if not
   */
  public boolean updateSolvematchCom() {
    if (tomogramCombinationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update solvematch.com without an active tomogram generation dialog",
        "Program logic error");
      return false;
    }
    try {
      comScriptMgr.loadSolvematch();
      SolvematchParam solvematchParam = comScriptMgr.getSolvematch();
      tomogramCombinationDialog.getSolvematchParams(solvematchParam);
      comScriptMgr.saveSolvematch(solvematchParam);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Solvematch Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Solvematch Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Load the solvematchshift com script into the tomogram combination dialog
   * converting it first to a solvematch param object
   */
  public void loadSolvematchShift() {
    comScriptMgr.loadSolvematchshift();
    SolvematchParam solvematchParam = new SolvematchParam();
    solvematchParam.parseSolvematchshift(comScriptMgr.getSolvematchshift(),
      metaData.getDatasetName());
    createNewSolvematch(solvematchParam);
  }

  /**
   * Load the solvematchmod com script into the tomogram combination dialog
   * converting it first to a solvematch param object
   */
  public void loadSolvematchMod() {
    comScriptMgr.loadSolvematchmod();
    SolvematchParam solvematchParam = new SolvematchParam();
    solvematchParam.parseSolvematchmod(comScriptMgr.getSolvematchmod());
    createNewSolvematch(solvematchParam);
  }

  private void createNewSolvematch(SolvematchParam solvematchParam) {
    try {
      comScriptMgr.useTemplate("solvematch", metaData.getDatasetName(), AxisType.DUAL_AXIS, AxisID.ONLY);
    }
    catch (Exception e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Unable to create solvematch com script";
      message[1] = "Check file and directory permissions";
      openMessageDialog(message, "Can't create solvematch.com");
      return;
    }
    //  Write it out the converted object disk
    comScriptMgr.loadSolvematch();
    comScriptMgr.saveSolvematch(solvematchParam);
    tomogramCombinationDialog.setSolvematchParams(solvematchParam);
  }

  /**
   * Load the patchcorr com script into the tomogram combination dialog
   */
  private void loadPatchcorr() {
    comScriptMgr.loadPatchcorr();
    tomogramCombinationDialog.setPatchcrawl3DParams(comScriptMgr.getPatchcrawl3D());
  }

  /**
   * Update the patchcorr.com script from the information in the tomogram
   * combination dialog box
   * 
   * @return boolean
   */
  private boolean updatePatchcorrCom() {
    //  Set a reference to the correct object
    if (tomogramCombinationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update patchcorr.com without an active tomogram generation dialog",
        "Program logic error");
      return false;
    }
    try {
      Patchcrawl3DParam patchcrawl3DParam = comScriptMgr.getPatchcrawl3D();
      tomogramCombinationDialog.getPatchcrawl3DParams(patchcrawl3DParam);
      comScriptMgr.savePatchcorr(patchcrawl3DParam);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Patchcorr Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Patchcorr Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Load the matchorwarp com script into the tomogram combination dialog
   */
  private void loadMatchorwarp() {
    comScriptMgr.loadMatchorwarp();
    tomogramCombinationDialog.setMatchorwarpParams(comScriptMgr.getMatchorwarParam());
  }

  /**
   * Update the matchorwarp.com script from the information in the tomogram
   * combination dialog box
   * 
   * @return boolean
   */
  private boolean updateMatchorwarpCom(boolean trialMode) {
    //  Set a reference to the correct object
    if (tomogramCombinationDialog == null) {
      mainFrame.openMessageDialog(
        "Can not update matchorwarp.com without an active tomogram generation dialog",
        "Program logic error");
      return false;
    }
    try {
      MatchorwarpParam matchorwarpParam = comScriptMgr.getMatchorwarParam();
      tomogramCombinationDialog.getMatchorwarpParams(matchorwarpParam);
      matchorwarpParam.setTrial(trialMode);
      comScriptMgr.saveMatchorwarp(matchorwarpParam);
    }
    catch (NumberFormatException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Matchorwarp Parameter Syntax Error";
      errorMessage[1] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Matchorwarp Parameter Syntax Error");
      return false;
    }
    return true;
  }

  /**
   * Initiate the combine process from the beginning
   */
  public void combine() {
    //  FIXME: what are the necessary updates
    //  Update the scripts from the dialog panel
    updateCombineParams();

    if (!updateSolvematchCom()) {
      return;
    }
    if (!updatePatchcorrCom()) {
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      return;
    }

    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);
    //  Set the next process to execute when this is finished
    nextProcess = "matchvol1";
    String threadName;
    try {
      threadName = processMgr.solvematch();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute solvematch.com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, AxisID.FIRST);
    tomogramCombinationDialog.showPane("Initial Match");
    mainFrame.startProgressBar("Combine: solvematch", AxisID.FIRST);
  }

  public void restartAtMatchvol1() {
    //  FIXME: what are the necessary updates
    //  Update the scripts from the dialog panel
    updateCombineParams();
    if (!updatePatchcorrCom()) {
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      return;
    }

    matchvol1();
  }
  
  /**
   * Execute the matchvol1 com script and put patchcorr in the execution queue
   */
  public void matchvol1() {
    updateCombineParams();
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);
    //  Check to see if solve.xf exists first
    File solveXf = new File(System.getProperty("user.dir"), "solve.xf");
    if (!solveXf.exists()) {
      nextProcess = "";
      String[] message = new String[2];
      message[0] = "Can not execute matchvol1.com";
      message[1] = "solve.xf must exist in the working";
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    //  Set the next process to execute when this is finished
    nextProcess = "patchcorr";
    String threadName;
    try {
      threadName = processMgr.matchvol1();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute matchvol1.com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, AxisID.FIRST);
    tomogramCombinationDialog.showPane("Initial Match");
    mainFrame.startProgressBar("Combine: matchvol1", AxisID.FIRST);
  }

  /**
   * Initiate the combine process from patchcorr step
   */
  public void patchcorrCombine() {
    updateCombineParams();
    if (!updatePatchcorrCom()) {
      return;
    }
    if (!updateMatchorwarpCom(false)) {
      return;
    }

    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
    warnStaleFile(ImodManager.PATCH_VECTOR_MODEL_KEY, true);
    patchcorr();

  }

  protected void warnStaleFile(String key, boolean future) {
    warnStaleFile(key, null, future);
  }

  protected void warnStaleFile(String key, AxisID axisID) {
    warnStaleFile(key, axisID, false);
  }

  protected void warnStaleFile(String key, AxisID axisID, boolean future) {
    try {
      if (imodManager.warnStaleFile(key, axisID)) {
        String[] message = new String[4];
        if (future) {
          message[0] = "3dmod is open to the existing " + key + ".";
          message[1] = "A new " + key + " will be created on disk.";
        }
        else {
          message[0] = "3dmod is open to the old " + key + ".";
          message[1] = "A new " + key + " has been created on disk.";
        }
        message[2] = "You will not be able to see the new version of " + key
          + " until you close this 3dmod.";
        message[3] = "Do you wish to quit this 3dmod now?";
        if (mainFrame.openYesNoDialog(message)) {
          imodManager.quit(key, axisID);
        }
      }
    }
    catch (AxisTypeException e) {
      e.printStackTrace();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
    }
  }

  /**
   * Exececute the patchcorr com script and put matchorwarp in the execution
   * queue
   */
  private void patchcorr() {
    //  Set the next process to execute when this is finished
    nextProcess = "matchorwarp";
    String threadName;
    try {
      threadName = processMgr.patchcorr();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute patchcorr.com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, AxisID.FIRST);
    tomogramCombinationDialog.showPane("Final Match");
  }

  /**
   * Initiate the combine process from matchorwarp step
   */
  public void matchorwarpCombine() {
    if (updateMatchorwarpCom(false)) {
      processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
      mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
      matchorwarp("volcombine");
    }
  }

  /**
   * Initiate the combine process from matchorwarp step
   */
  public void matchorwarpTrial() {
    if (updateMatchorwarpCom(true)) {
      processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
      mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
      matchorwarp("");
    }
  }

  /**
   * Exececute the matchorwarp com script and put the process identefied by
   * next in the execution queue
   */
  private void matchorwarp(String next) {
    //  Set the next process to execute when this is finished
    nextProcess = next;
    String threadName;
    try {
      threadName = processMgr.matchorwarp();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute matchorwarp.com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, AxisID.FIRST);
    tomogramCombinationDialog.showPane("Final Match");
    mainFrame.startProgressBar("Combine: matchorwarp", AxisID.FIRST);
  }

  /**
   * Execute the volcombine com script and clear the execution queue
   */
  public void volcombine() {
    processTrack.setTomogramCombinationState(ProcessState.INPROGRESS);
    mainFrame.setTomogramCombinationState(ProcessState.INPROGRESS);
    //  Set the next process to execute when this is finished
    nextProcess = "";
    String threadName;
    try {
      threadName = processMgr.volcombine();
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute volcombine.com";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute com script");
      return;
    }
    setThreadName(threadName, AxisID.FIRST);
    tomogramCombinationDialog.showPane("Final Match");
  }

  /**
   * Convert the patch.mod to patch.out
   *  
   */
  public void modelToPatch() {
    try {
      processMgr.modelToPatch();
    }
    catch (SystemProcessException except) {
      String[] errorMessage = new String[2];
      errorMessage[0] = "Unable to convert patch_vector.mod to patch.out";
      errorMessage[1] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "Patch vector model error");
      return;
    }
  }

  /**
   * Open the post processing dialog
   */
  public void openPostProcessingDialog() {
    //  Check to see if the com files are present otherwise pop up a dialog
    //  box informing the user to run the setup process
    if (!metaData.getComScriptCreated()) {
      setupRequestDialog();
      return;
    }
    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    mainFrame.selectButton(AxisID.ONLY, "Post Processing");
    if (postProcessingDialog == null) {
      postProcessingDialog = new PostProcessingDialog(this);
      TrimvolParam trimvolParam = new TrimvolParam();
      String inputFile = "";
      if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
        inputFile = metaData.getDatasetName() + "_full.rec";
      }
      else {
        inputFile = "sum.rec";
      }
      try {
        trimvolParam.setDefaultRange(inputFile);
      }
      catch (InvalidParameterException except) {
        String[] detailedMessage = new String[4];
        detailedMessage[0] = "Unable to set trimvol range";
        detailedMessage[1] = "Does the reconstruction file exist yet?";
        detailedMessage[2] = "";
        detailedMessage[3] = except.getMessage();
        mainFrame.openMessageDialog(detailedMessage, "Invalid parameter: "
          + inputFile);
        //    Delete the dialog
        postProcessingDialog = null;
        return;
      }
      catch (IOException except) {
        except.printStackTrace();
        mainFrame.openMessageDialog(except.getMessage(), "IO Error: "
          + inputFile);
        //      Delete the dialog
        postProcessingDialog = null;
        return;
      }
      postProcessingDialog.setTrimvolParams(trimvolParam);
    }
    mainFrame.showProcess(postProcessingDialog.getContainer(), AxisID.ONLY);
  }

  /**
   * Close the post processing dialog panel
   */
  public void donePostProcessing() {
    if (postProcessingDialog == null) {
      mainFrame.openMessageDialog("Post processing dialog not open",
        "Program logic error");
      return;
    }
    DialogExitState exitState = postProcessingDialog.getExitState();
    if (exitState == DialogExitState.CANCEL) {
      postProcessingDialog = null;
    }
    else if (exitState == DialogExitState.POSTPONE) {
      processTrack.setPostProcessingState(ProcessState.INPROGRESS);
      mainFrame.setPostProcessingState(ProcessState.INPROGRESS);
    }
    else {
      processTrack.setPostProcessingState(ProcessState.COMPLETE);
      mainFrame.setPostProcessingState(ProcessState.COMPLETE);
      postProcessingDialog = null;
    }
    mainFrame.showBlankProcess(AxisID.ONLY);
  }

  /**
   * Open the combined (or full) volume in 3dmod
   */
  public void imodCombinedTomogram() {
    try {
      imodManager.open(ImodManager.COMBINED_TOMOGRAM_KEY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on the trimmed tomogram");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
  }

  public Vector imodGetRubberbandCoordinates(String imodKey) {
    Vector results = null;
    try {
      results = imodManager.getRubberbandCoordinates(imodKey);
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
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
      mainFrame.openMessageDialog(messages, "Rubberband Coordinates");
    }
    return results;
  }

  /**
   * calls ProcessManager.generateAlignLogs() when the ta files are out of date
   * @param commandName
   * @param axisID
   * @return true if any log files where changed
   */
  public boolean updateLog(String commandName, AxisID axisID) {
    String alignLogName = "align" + axisID.getExtension() + ".log";
    File alignLog = new File(System.getProperty("user.dir"), alignLogName);
    String taErrorLogName = "taError" + axisID.getExtension() + ".log";
    File taErrorLog = new File(System.getProperty("user.dir"), taErrorLogName);
    if (!alignLog.exists()) {
      return false;
    }
    if (!taErrorLog.exists()
      || taErrorLog.lastModified() < alignLog.lastModified()) {
      processMgr.generateAlignLogs(axisID);
      return true;
    }
    return false;
  }

  /**
   * Open the trimmed volume in 3dmod
   */
  public void imodTrimmedVolume() {
    TrimvolParam trimvolParam = new TrimvolParam();
    postProcessingDialog.getTrimvolParams(trimvolParam);
    try {
      imodManager.setSwapYZ(ImodManager.TRIMMED_VOLUME_KEY,
        !trimvolParam.isSwapYZ());
      imodManager.open(ImodManager.TRIMMED_VOLUME_KEY);
    }
    catch (SystemProcessException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(),
        "Can't open 3dmod on the trimmed tomogram");
    }
    catch (AxisTypeException except) {
      except.printStackTrace();
      mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
    }
  }

  /**
   * Execute trimvol
   */
  public void trimVolume() {
    // Make sure that the post processing panel is open
    if (postProcessingDialog == null) {
      mainFrame.openMessageDialog("Post processing dialog not open",
        "Program logic error");
      return;
    }
    // Get the trimvol parameters from the panel
    TrimvolParam trimvolParam = new TrimvolParam();
    postProcessingDialog.getTrimvolParams(trimvolParam);
    //  Set the appropriate input and output files
    String inputFile = "";
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      inputFile = metaData.getDatasetName() + "_full.rec";
    }
    else {
      inputFile = "sum.rec";
    }
    trimvolParam.setInputFile(inputFile);
    trimvolParam.setOutputFile(metaData.getDatasetName() + ".rec");
    // Start the trimvol process
    processTrack.setPostProcessingState(ProcessState.INPROGRESS);
    mainFrame.setPostProcessingState(ProcessState.INPROGRESS);
    String threadName;
    try {
      threadName = processMgr.trimVolume(trimvolParam);
    }
    catch (SystemProcessException e) {
      e.printStackTrace();
      String[] message = new String[2];
      message[0] = "Can not execute trimvol command";
      message[1] = e.getMessage();
      mainFrame.openMessageDialog(message, "Unable to execute command");
      return;
    }
    setThreadName(threadName, AxisID.ONLY);
    mainFrame.startProgressBar("Trimming volume", AxisID.ONLY);
  }

  //
  //  Utility functions
  //
  private boolean showIfExists(ProcessDialog panelA, ProcessDialog panelB,
    AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      if (panelB == null) {
        return false;
      }
      else {
        mainFrame.showProcess(panelB.getContainer(), axisID);
        return true;
      }
    }
    else {
      if (panelA == null) {
        return false;
      }
      else {
        mainFrame.showProcess(panelA.getContainer(), axisID);
        return true;
      }
    }
  }

  /**
   * Return a reference to THE com script manager
   * @return
   */
  public ComScriptManager getComScriptManager() {
    return comScriptMgr;
  }

  /**
   * Check if the current data set is a dual axis data set
   * @return true if the data set is a dual axis data set
   */
  public boolean isDualAxis() {
    if (metaData.getAxisType() == AxisType.SINGLE_AXIS) {
      return false;
    }
    else {
      return true;
    }
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
  public void setAdvanced(boolean state) {
    isAdvanced = state;
  }

  /**
   *  
   */
  public void packMainWindow() {
    mainFrame.repaint();
  }

  /**
   * Return the dataset name. This is the basename of the raw image stack and
   * the name used for the base of all intermediate and final data files.
   * @return a string containing the dataset name.
   */
  public String getDatasetName() {
    return metaData.getDatasetName();
  }

  /**
   * Open a new dataset starting wiht the setup dialog
   *  
   */
  public void openNewDataset() {
    if (saveTestParamIfNecessary()) {
      resetState();
      // Open the setup dialog
      openSetupDialog();
    }
  }

  /**
   * Open an existing EDF file
   */
  public void openExistingDataset(File paramFile) {
    if (saveTestParamIfNecessary()) {
      //  Ask the user for a EDF file if one is not supplied
      if (paramFile == null) {
        paramFile = mainFrame.openEtomoDataFileDialog();
      }
      if (paramFile == null) {
        return;
      }
      resetState();
      if (loadTestParamFile(paramFile)) {
        openProcessingPanel();
      }
      else {
        resetState();
        openSetupDialog();
      }
    }
  }

  /**
   * Reset the state of the application to the startup condition
   */
  private void resetState() {
    // Delete the objects associated with the current dataset
    metaData = new MetaData();
    paramFile = null;
    setupDialog = null;
    preProcDialogA = null;
    preProcDialogB = null;
    coarseAlignDialogA = null;
    coarseAlignDialogB = null;
    fiducialModelDialogA = null;
    fiducialModelDialogB = null;
    fineAlignmentDialogA = null;
    fineAlignmentDialogB = null;
    tomogramPositioningDialogA = null;
    tomogramPositioningDialogB = null;
    tomogramGenerationDialogA = null;
    tomogramGenerationDialogB = null;
    tomogramCombinationDialog = null;
    postProcessingDialog = null;
    settingsDialog = null;
    comScriptMgr = new ComScriptManager(this);
    processMgr = new ProcessManager(this);
    processTrack = new ProcessTrack();
  }

  /**
   * If the current state needs to be saved the users is queried with a dialog
   * box.
   * @return True if either: the current state does not need to be saved, the
   * state is successfully saved, or the user chooses not to save the current
   * state by selecting no.  False is returned if the state can not be
   * successfully saved, or the user chooses cancel.
   */
  private boolean saveTestParamIfNecessary() {
    // Check to see if the current dataset needs to be saved
    if (isDataParamDirty || processTrack.isModified()) {
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
        if (!mainFrame.getTestParamFilename()) {
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

  /**
   * A message asking the ApplicationManager to load in the information from the
   * test parameter file.
   * @param paramFile the File object specifiying the data parameter file.
   */
  public boolean loadTestParamFile(File paramFile) {
    FileInputStream processDataStream;
    try {
      // Read in the test parameter data file
      ParameterStore paramStore = new ParameterStore(paramFile);
      Storable[] storable = new Storable[2];
      storable[0] = metaData;
      storable[1] = processTrack;
      paramStore.load(storable);

      // Set the current working directory for the application, this is the
      // path to the EDF file.  The working directory is defined by the current
      // user.dir system property.
      // Uggh, stupid JAVA bug, getParent() only returns the parent if the File
      // was created with the full path
      File newParamFile = new File(paramFile.getAbsolutePath());
      System.setProperty("user.dir", newParamFile.getParent());
      setTestParamFile(newParamFile);
      // Update the MRU test data filename list
      userConfig.putDataFile(newParamFile.getAbsolutePath());
      mainFrame.setMRUFileLabels(userConfig.getMRUFileList());
      //  Initialize a new IMOD manager
      imodManager.setMetaData(metaData);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not find the test parameter data file:";
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage, "File not found error");
      return false;
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file read error";
      errorMessage[1] = "Could not read the test parameter data from file:";
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Test parameter file read error");
      return false;
    }
    if (!metaData.isValid(false)) {
      mainFrame.openMessageDialog(metaData.getInvalidReason(),
        ".edf file error");
      return false;
    }
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
      storable[0] = metaData;
      storable[1] = processTrack;
      paramStore.save(storable);
      //  Update the MRU test data filename list
      userConfig.putDataFile(paramFile.getAbsolutePath());
      mainFrame.setMRUFileLabels(userConfig.getMRUFileList());
      // Reset the process track flag
      processTrack.resetModified();
    }
    catch (IOException except) {
      except.printStackTrace();
      String[] errorMessage = new String[3];
      errorMessage[0] = "Test parameter file save error";
      errorMessage[1] = "Could not save test parameter data to file:";
      errorMessage[2] = except.getMessage();
      mainFrame.openMessageDialog(errorMessage,
        "Test parameter file save error");
    }
    isDataParamDirty = false;
  }

  /**
   * Return the test parameter file as a File object
   * @return a File object specifying the data set parameter file.
   */
  public File getTestParamFile() {
    return paramFile;
  }

  /**
   * Set the data set parameter file. This also updates the mainframe data
   * parameters.
   * @param paramFile a File object specifying the data set parameter file.
   */
  public void setTestParamFile(File paramFile) {
    this.paramFile = paramFile;
    //  Update main window information and status bar
    mainFrame.updateDataParameters(paramFile, metaData);
  }

  /**
   * Open up the settings dialog box
   */
  public void openSettingsDialog() {
    //  Open the dialog in the appropriate mode for the current state of
    //  processing
    if (settingsDialog == null) {
      settingsDialog = new SettingsDialog(this);
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
  public void getSettingsParameters() {
    if (settingsDialog != null) {
      settingsDialog.getParameters(userConfig);
      setUserPreferences();
      mainFrame.repaintWindow();
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

  /**
   *  
   */
  private void setupRequestDialog() {
    String[] message = new String[2];
    message[0] = "The setup process has not been completed";
    message[1] = "Complete the Setup process before opening other process dialogs";
    mainFrame.openMessageDialog(message, "Program Operation Error");
    return;
  }

  /**
   *  
   */
  private String initProgram(String[] args) {
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
    // Parse the command line
    String testParamFilename = parseCommandLine(args);
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
      return "";
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
    return testParamFilename;
  }

  /**
   * Set the user preferences
   */
  private void setUserPreferences() {
    ToolTipManager.sharedInstance().setInitialDelay(
      userConfig.getToolTipsInitialDelay());
    ToolTipManager.sharedInstance().setDismissDelay(
      userConfig.getToolTipsDismissDelay());
    setUIFont(userConfig.getFontFamily(), userConfig.getFontSize());
    setLookAndFeel(userConfig.getNativeLookAndFeel());
    isAdvanced = userConfig.getAdvancedDialogs();
  }

  /**
   * Parse the command line. This method will return a non-empty string if
   * there is a etomo data .
   * 
   * @param The
   *            command line arguments
   * @return A string that will be set to the etomo data filename if one is
   *         found on the command line otherwise it is "".
   */
  private String parseCommandLine(String[] args) {
    String testParamFilename = "";
    //  Parse the command line arguments
    for (int i = 0; i < args.length; i++) {

      // Filename argument should be the only one not beginning with at least
      // one dash
      if (!args[i].startsWith("-")) {
        testParamFilename = args[i];
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
    }
    return testParamFilename;
  }

  /**
   * Exit the program
   */
  public boolean exitProgram() {
    //  Check to see if any processes are still running
    if (!threadNameA.equals("none") || !threadNameB.equals("none")) {
      String[] message = new String[3];
      message[0] = "There are still processes running.";
      message[1] = "Exiting Etomo now may terminate those processes.";
      message[2] = "Do you still wish to exit the program?";
      if (!mainFrame.openYesNoDialog(message)) {
        return false;
      }
    }
    if (saveTestParamIfNecessary()) {
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
        mainFrame.openMessageDialog(except.getMessage(), "AxisType problem");
      }
      catch (SystemProcessException except) {
        except.printStackTrace();
        mainFrame.openMessageDialog(except.getMessage(),
          "Problem closing 3dmod");
      }
      return true;
    }
    return false;
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
  public static void setUIFont(String fontFamily, int fontSize) {
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

  /**
   * Return the IMOD directory
   */
  static public File getIMODDirectory() {
    //  Return a copy of the IMODDirectory object
    return new File(IMODDirectory.getAbsolutePath());
  }

  /**
   * Return the IMOD calibration directory
   */
  static public File getIMODCalibDirectory() {
    //  Return a copy of the IMODDirectory object
    return new File(IMODCalibDirectory.getAbsolutePath());
  }

  /**
   * Return the absolute IMOD bin path
   * @return
   */
  public static String getIMODBinPath() {
    return ApplicationManager.getIMODDirectory().getAbsolutePath()
      + File.separator + "bin" + File.separator;
  }

  /**
   * Return the users home directory environment variable HOME or an empty
   * string if it doesn't exist.
   */
  private String getHomeDirectory() {
    return homeDirectory;
  }

  /**
   * Returns the debug state.
   * 
   * @return boolean
   */
  static public boolean isDebug() {
    return debug;
  }

  /**
   * Returns the demo state.
   * 
   * @return boolean
   */
  public boolean isDemo() {
    return demo;
  }

  /**
   * Run the specified command as a background process with a indeterminate
   * progress bar.
   */
  public void backgroundProcess(String commandLine) {
    processMgr.test(commandLine);
  }

  /**
   * Open a messsage dialog with the given message and title
   * 
   * @param message
   * @param title
   */
  public void openMessageDialog(Object message, String title) {
    mainFrame.openMessageDialog(message, title);
  }

  /**
   * Set the progress bar to the beginning of determinant sequence
   * 
   * @param label
   * @param nSteps
   */
  public void setProgressBar(String label, int nSteps, AxisID axisID) {
    mainFrame.setProgressBar(label, nSteps, axisID);
  }

  /**
   * Set the progress bar to the specified value
   * 
   * @param value
   * @param axisID
   */
  public void setProgressBarValue(int value, AxisID axisID) {
    mainFrame.setProgressBarValue(value, axisID);
  }

  /**
   * Set the progress bar to the specified value and update the string
   * 
   * @param value
   * @param string
   * @param axisID
   */
  public void setProgressBarValue(int value, String string, AxisID axisID) {
    mainFrame.setProgressBarValue(value, string, axisID);
  }

  /**
   * @param axisID
   */
  public void progressBarDone(AxisID axisID) {
    mainFrame.stopProgressBar(axisID);
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
      mainFrame.stopProgressBar(AxisID.FIRST);
      threadNameA = "none";
    }
    else if (threadName.equals(threadNameB)) {
      mainFrame.stopProgressBar(AxisID.SECOND);
      threadNameB = "none";
    }
    else {
      mainFrame.openMessageDialog("Unknown thread finished!!!", "Thread name: "
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

  /**
   * Start the next process specified by the nextProcess string
   */
  private void startNextProcess(AxisID axisID) {
    if (nextProcess.equals("generateNonFidXF")) {
      generateNonFidXF(axisID);
      return;
    }
    if (nextProcess.equals("tilt")) {
      tiltProcess(axisID);
    }
    if (nextProcess.equals("matchvol1")) {
      matchvol1();
      return;
    }
    if (nextProcess.equals("patchcorr")) {
      patchcorr();
      return;
    }
    if (nextProcess.equals("matchorwarp")) {
      matchorwarp("volcombine");
      return;
    }
    if (nextProcess.equals("volcombine")) {
      volcombine();
      return;
    }
  }

  protected void updateDialog(ProcessName processName, AxisID axisID) {
    if (axisID != AxisID.ONLY
      && (processName == ProcessName.PRENEWST || processName == ProcessName.TRACK)) {
      updateDialog(fiducialModelDialogB, AxisID.SECOND);
      updateDialog(fiducialModelDialogA, AxisID.FIRST);
    }
    if (processName == ProcessName.NEWST) {
      if (axisID == AxisID.SECOND) {
        updateDialog(tomogramGenerationDialogB, axisID);
      }
      else {
        updateDialog(tomogramGenerationDialogA, axisID);
      }
    }
  }

  private void updateDialog(FiducialModelDialog dialog, AxisID axisID) {
    if (dialog == null || axisID == AxisID.ONLY) {
      return;
    }
    boolean prealisExist = Utilities.fileExists(metaData, ".preali",
      AxisID.FIRST)
      && Utilities.fileExists(metaData, ".preali", AxisID.SECOND);
    boolean fidExists = false;
    if (axisID == AxisID.FIRST) {
      fidExists = Utilities.fileExists(metaData, ".fid", AxisID.SECOND);
    }
    else {
      fidExists = Utilities.fileExists(metaData, ".fid", AxisID.FIRST);
    }
    dialog.setTransferfidEnabled(prealisExist && fidExists);
    dialog.updateEnabled();
  }

  private void updateDialog(TomogramGenerationDialog dialog, AxisID axisID) {
    if (dialog == null) {
      return;
    }
    dialog.updateFilter(Utilities.fileExists(metaData, ".ali", axisID));
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
   * Map the thread name to the correct axis
   * 
   * @param name
   *            The name of the thread to assign to the axis
   * @param axisID
   *            The axis of the thread to be mapped
   */
  private void setThreadName(String name, AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      threadNameB = name;
    }
    else {
      threadNameA = name;
    }
  }

  private void backupFile(File file) {
    if (file.exists()) {
      File backupFile = new File(file.getAbsolutePath() + "~");
      try {
        Utilities.renameFile(file, backupFile);
      }
      catch (IOException except) {
        System.err.println("Unable to backup file: " + file.getAbsolutePath()
          + " to " + backupFile.getAbsolutePath());
        mainFrame.openMessageDialog(except.getMessage(), "File Rename Error");
      }
    }
  }

  // FIXME: this is a temporary patch until we can transition the MetaData
  // object to a static object or singleton
  public ConstMetaData getMetaData() {
    return metaData;
  }

  //  Test helper functions
  /**
   * Return the currently executing thread name for the specified axis
   * @param axisID
   * @return
   */
  String getThreadName(AxisID axisID) {
    if (axisID == AxisID.SECOND) {
      return threadNameB;
    }
    return threadNameA;
  }

  MainFrame getMainFrame() {
    return mainFrame;
  }
}