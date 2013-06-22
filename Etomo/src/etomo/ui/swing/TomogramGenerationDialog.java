package etomo.ui.swing;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.SirtsetupParam;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.MetaData;
import etomo.type.ProcessingMethod;
import etomo.type.ReconScreenState;
import etomo.type.TomogramState;

/**
 * <p>
 * Description: Tomogram generation user interface
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002 - 2010</p>
 * 
 * <p>
 * Organization: Boulder Laboratory for 3D Fine Structure, University of 
 * Colorado
 * </p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p>
 * $Log$
 * Revision 1.6  2011/04/26 00:08:25  sueh
 * bug# 1416 Removed the observer/observable code because there aren't enough observers and the response isn't standard enough.
 *
 * Revision 1.5  2011/04/04 17:40:44  sueh
 * bug# 1416 Added bgMethod; backProject and sirt radio buttons, action, allowTiltComSave,
 * getProcessingMethod, getSirtsetupDIsplay.  Removed curTab, pnlTilt, tabbedPane, changeTab,
 * getSirtTiltDisplay, initTab.  Modified constructor, addListeners, createPanel, done, getInstance, getParameters.
 *
 * Revision 1.3  2011/02/03 06:22:16  sueh
 * bug# 1422 Control of the processing method has been centralized in the
 * processing method mediator class.  Implementing ProcessInterface.
 * Supplying processes with the current processing method.
 *
 * Revision 1.2  2010/12/05 05:22:36  sueh
 * bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.  Under --newstuff,
 * added tabs and SirtPanel.
 *
 * Revision 1.1  2010/11/13 16:07:34  sueh
 * bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 *
 * Revision 3.125  2010/03/05 04:05:48  sueh
 * bug# 1319 Added boolean initialize to setParameters(ConstTiltParam).
 *
 * Revision 3.124  2009/09/22 23:55:55  sueh
 * bug# 1269 Added setEnabledTiltParameters.  Got rid of unused functions.
 *
 * Revision 3.123  2009/09/01 03:18:25  sueh
 * bug# 1222
 *
 * Revision 3.122  2009/03/17 00:46:24  sueh
 * bug# 1186 Pass managerKey to everything that pops up a dialog.
 *
 * Revision 3.121  2009/02/05 23:45:18  sueh
 * bug# 1148 Added setTrialTomogramNameList.
 *
 * Revision 3.120  2009/02/04 23:36:48  sueh
 * bug# 1158 Changed id and exception classes in LogFile.
 *
 * Revision 3.119  2009/01/20 20:31:45  sueh
 * bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 *
 * Revision 3.118  2008/10/27 20:43:44  sueh
 * bug# 1141 Removed implementation statement for
 * AbstractParallelDialog because it is already implemented in the ancestor
 * class.
 *
 * Revision 3.117  2008/10/16 22:32:18  sueh
 * bug# 1141 Created FinalAlignedStack dialog to run full aligned stack and mtf filter.
 *
 * Revision 3.116  2008/09/30 22:45:50  sueh
 * bug# 1113 Using a private constructor in SpacedPanel.
 *
 * Revision 3.115  2008/07/19 01:12:37  sueh
 * bug# 1125 Making it easier to access CpuAdoc by not passing the
 * manager to it; all it needs is the current directory.
 *
 * Revision 3.114  2008/05/13 23:07:49  sueh
 * bug# 847 Adding a right click menu for deferred 3dmods to some
 * process buttons.
 *
 * Revision 3.113  2008/05/03 00:57:22  sueh
 * bug# 847 Passing null for ProcessSeries to process funtions.
 *
 * Revision 3.112  2007/12/26 22:36:58  sueh
 * bug# 1052 Return true when done() completes successfully.
 *
 * Revision 3.111  2007/12/10 22:48:55  sueh
 * bug# 1041 Passing the ProcessName to processchunks instead of setting it in
 * getParameters because it is required and has been added to the
 * ProcesschunksParam constructor.  Removed getParameters
 * ProcesschunksParam) because it is empty.
 *
 * Revision 3.110  2007/09/07 00:29:26  sueh
 * bug# 989 Using a public INSTANCE to refer to the EtomoDirector singleton
 * instead of getInstance and createInstance.
 *
 * Revision 3.109  2007/08/16 16:34:43  sueh
 * bug# 1035 Added ltfSizeToOutputInXandY to advanced newstPanel.
 *
 * Revision 3.108  2007/08/08 15:04:58  sueh
 * bug# 834 Fixed the parallel processing check box label.
 *
 * Revision 3.107  2007/07/17 21:45:10  sueh
 * bug# 1018 Getting cpu.adoc information from CpuAdoc.
 *
 * Revision 3.106  2007/05/26 00:33:32  sueh
 * bug# 994 Not automatically setting button size in SpacedPanel anymore.
 * Setting button size in UI.
 *
 * Revision 3.105  2007/05/01 22:30:26  sueh
 * bug# 964 In LabeledSpinner, saving SpinnerNumberModel so that the
 * maximum can be changed.
 *
 * Revision 3.104  2007/03/31 03:02:53  sueh
 * bug# 964 Changed PanelHeader.isAdvanceBasicExpanded to isAdvanced.
 *
 * Revision 3.103  2007/03/21 19:47:15  sueh
 * bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * Added AutodocFactory to create Autodoc instances.
 *
 * Revision 3.102  2007/03/01 01:45:32  sueh
 * bug# 964 Added LogFile to Autodoc.
 *
 * Revision 3.101  2007/02/09 00:54:03  sueh
 * bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * classes.
 *
 * Revision 3.100  2006/11/17 00:49:47  sueh
 * bug# 954 Added tooltip for extra exclude list.
 *
 * Revision 3.99  2006/11/07 23:10:44  sueh
 * bug# 954 Added tooltips
 *
 * Revision 3.98  2006/10/26 23:57:53  sueh
 * bug# 953 updated tooltips
 *
 * Revision 3.97  2006/09/14 00:00:38  sueh
 * bug# 920 Rename X offset and Z offset to X shift and Z shift.
 *
 * Revision 3.96  2006/07/28 21:27:20  sueh
 * bug# 868 Moved complex button actions to expert
 *
 * Revision 3.95  2006/07/28 20:13:51  sueh
 * bug# 868 Adding sets and gets to dialog, moving functionality to expert
 *
 * Revision 3.94  2006/07/26 16:41:40  sueh
 * bug# 868 Moved functions associated with TomogramGenerationDialog from
 * ApplicationManager to TomogramGenerationExpert.
 *
 * Revision 3.93  2006/07/05 23:26:38  sueh
 * Added tooltips to Tomo Pos.
 *
 * Revision 3.92  2006/07/04 20:42:29  sueh
 * bug# 898 Don't remove action listeners unless the done dialog function
 * succeeds.
 *
 * Revision 3.91  2006/06/30 20:04:12  sueh
 * bug# 877 Calling all the done dialog functions from the dialog done() functions,
 * which is called by the button action functions and saveAction() in
 * ProcessDialog.  Removed the button action function overides.  Set displayed to
 * false after the done dialog function is called.
 *
 * Revision 3.90  2006/06/21 15:55:14  sueh
 * bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 *
 * Revision 3.89  2006/05/19 19:48:19  sueh
 * bug# 866 Added setText(ConstEtomoNumber)
 *
 * Revision 3.88  2006/05/11 19:28:13  sueh
 * bug# 838 Making x axis tilt a double everywhere.
 *
 * Revision 3.87  2006/04/28 21:05:19  sueh
 * bug# 787 PanelHeader:  Removed the member variable title, which was
 * not used.
 *
 * Revision 3.86  2006/03/27 21:08:12  sueh
 * bug# 836 Added DialogType to PanelHeader get instances functions so
 * that the buttons in PanelHeader could save themselves.
 *
 * Revision 3.85  2006/03/22 00:36:53  sueh
 * bug# 836 buttonAction:  removed unnecessary calls to MultiLineButton.
 * msgProcessStarting.
 *
 * Revision 3.84  2006/03/20 18:07:43  sueh
 * bug# 835 Changed the interface ParallelDialog to AbstractParallelDialog.
 *
 * Revision 3.83  2006/02/06 21:22:32  sueh
 * bug# 521 Getting toggle buttons through ProcessResultDisplayFactory.
 *
 * Revision 3.82  2006/01/26 22:08:52  sueh
 * bug# 401 For MultiLineButton toggle buttons:  save the state and keep
 * the buttons turned on each they are run, unless the process fails or is
 * killed.
 *
 * Revision 3.81  2006/01/20 21:13:09  sueh
 * bug# 401 Saving the state of btnNewst and btnTilt.  Also using btnNewst
 * and btnTilt as ProcessResultDisplay's to turn on or off depending on the
 * result of the process that was run.
 *
 * Revision 3.80  2006/01/12 17:18:53  sueh
 * bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 *
 * Revision 3.79  2006/01/04 00:00:59  sueh
 * bug# 675 Converted JCheckBox's to CheckBox.
 *
 * Revision 3.78  2005/12/14 20:59:06  sueh
 * bug# 784 Added tool tips.
 *
 * Revision 3.77  2005/12/13 02:30:33  sueh
 * bug# 773 Getting default parallel processing checkbox setting from
 * metadata.defaultParallel.
 *
 * Revision 3.76  2005/11/21 20:47:17  sueh
 * bug# 772 Disabling the parallel process checkbox when the cpu.adoc is
 * missing.
 *
 * Revision 3.75  2005/11/14 22:22:05  sueh
 * bug# 762 Made btnMtfFileAction() and stateChanged() protected.
 *
 * Revision 3.74  2005/10/15 00:37:10  sueh
 * bug# 532 Changed BaseManager.showParallelStatus() to
 * setParallelDialog().
 *
 * Revision 3.73  2005/10/12 22:46:31  sueh
 * bug# 532 If parallel is not set in meta data, then the default for the parallel
 * checkboxes is based on the existance and validity of cpu.adoc.
 *
 * Revision 3.72  2005/09/29 19:13:05  sueh
 * bug# 532 Add panel headers to all of the sections in Combine.  Hide the
 * sections in the tabs that are not visible so that the visible tab can become
 * small.  Added an expand() function to each tab to handle the
 * expand/contract requests of the panel header buttons.  Added set and get
 * parameters for ReconScreenState to set and get the state of the panel
 * headers.  Added functionality to set the advanced/basic button to match
 * the panel advanced/basic buttons, when all of the panel buttons have the
 * same state.
 *
 * Revision 3.71  2005/09/27 23:49:03  sueh
 * bug# 532 When creating headers, set the group name for storing.  Set the
 * state in each header.
 *
 * Revision 3.70  2005/09/22 21:33:23  sueh
 * bug# 532 Moved the parallel process panel to AxisProcessPanel.
 *
 * Revision 3.69  2005/09/21 17:07:19  sueh
 * bug# 532 Removed all resume functionality from the dialogs.  Removed
 * resume().  Removed getParallelProgressDisplay() because the parallel
 * panel can be gotten from the manager.  Removed runTilt() because it was
 * necessary only because tilt was being run from two functions.
 *
 * Revision 3.68  2005/09/20 19:14:26  sueh
 * bug# 532 Simplify expand().  Remove PanelHeader.setOpen() calls, since
 * the panel and the button both start open.
 *
 * Revision 3.67  2005/09/16 21:21:02  sueh
 * bug# 532 Changed ParallelDialog.resetParallelPanel() to
 * resetParallelProgressDisplay() because ParallelDialog is generic.
 *
 * Revision 3.66  2005/09/16 20:57:51  sueh
 * bug# 532 Moved call to resetParallelPanel() to
 * ApplicationManager.processchunks().  Added resetParallelPanel() to
 * ParallelDialog.
 *
 * Revision 3.65  2005/09/16 18:20:47  sueh
 * bug# 532 Implemented changes in ParallelDialog:  changed
 * getParameters(ProcesschunksParam) to getParameters(ParallelParam).
 *
 * Revision 3.64  2005/09/01 18:35:42  sueh
 * bug# 532 Getting the parallel panel from the manager.
 *
 * Revision 3.63  2005/08/31 19:16:28  sueh
 * bug# 532 need to do an install and parallel processing is not ready
 *
 * Revision 3.62  2005/08/30 19:22:41  sueh
 * bug# 532 Remove the newstuff limit from the parallel processing checkbox.
 *
 * Revision 3.61  2005/08/27 22:42:44  sueh
 * bug# 532 Changed Autodoc.get() to getInstance().
 *
 * Revision 3.60  2005/08/24 00:25:00  sueh
 * bug# 532 Added ashtray.  Made tubule a 2 cpu system
 *
 * Revision 3.59  2005/08/22 18:20:32  sueh
 * bug# 532  Handling opening and closing command from the header in the
 * dialog instead of the header.
 *
 * Revision 3.58  2005/08/12 00:01:13  sueh
 * bug# 711  Change enum Run3dmodMenuOption to
 * Run3dmodMenuOptions, which can turn on multiple options at once.
 * This allows ImodState to combine input from the context menu and the
 * pulldown menu.  Prevent context menu from popping up when button is
 * disabled.  Get rid of duplicate code by running the 3dmods from a private
 * function called run3dmod(String, Run3dmodMenuOptions).  It can be
 * called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and the
 * action function.
 *
 * Revision 3.57  2005/08/10 20:48:01  sueh
 * bug# 711 Removed MultiLineToggleButton.  Making toggling an attribute
 * of MultiLineButton.
 *
 * Revision 3.56  2005/08/09 21:10:34  sueh
 * bug# 711  Implemented Run3dmodButtonContainer:  added run3dmod().
 * Changed 3dmod buttons to Run3dmodButton.  No longer inheriting
 * MultiLineButton from JButton.
 *
 * Revision 3.55  2005/08/04 20:21:06  sueh
 * bug# 532  Added runTilt() to figure out whether trial or regular tilt is being
 * run and run it.  Calling runTilt() from resume and button action.
 *
 * Revision 3.54  2005/08/01 18:17:54  sueh
 * bug# 532 Added getParameters(ProcesschunksParam).  Add resume
 * boolean (a parameter in ProcesschunksParam).
 *
 * Revision 3.53  2005/07/29 00:54:46  sueh
 * bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * because the current manager changes when the user changes the tab.
 * Passing the manager where its needed.
 *
 * Revision 3.52  2005/07/21 22:23:43  sueh
 * bug# 532 Calling ApplicationManager.splittilt().  Moved parallel process
 * checkbox to the top of the tilt panel.
 *
 * Revision 3.51  2005/07/19 22:38:39  sueh
 * bug# 532 Removed save and split buttons
 *
 * Revision 3.50  2005/07/14 22:16:26  sueh
 * bug# 626 Enabling binning for montage view.  Setting binning in
 * set and getParameters(BlendmontParam).
 *
 * Revision 3.49  2005/07/11 23:31:34  sueh
 * bug# 619 Added a "save & split" button for parallel processing tilt and trial
 *  tilt.  Receiving signal about how parallel tilt completed so resume can be
 * turned on and   off.  Switch tilt gen and trial tilt gen titles based on the
 * parallel processing check box.  Added functions: resume, signalTiltCompleted,
 * signTiltError, signalTiltKilled, updateParallelProcess.
 *
 * Revision 3.48  2005/07/07 00:00:30  sueh
 * bug# 437 In PanelHeader: Removed unnecessary Constructor parameter
 * useAdvancedBasic.  If the container is not passed then the
 * advanced/basic button with not be used.
 *
 * Revision 3.47  2005/07/06 23:51:57  sueh
 * bug# 437 Rewrote the layout functions to include panel headers in a
 * standard way.  Rewrote setAdvanced().  Added individual setAdvanced
 * functions for each panel.  Added exapand().
 *
 * Revision 3.46  2005/07/01 21:26:01  sueh
 * bug# 619 Added check box cbParallelProcess to override normal
 * tomogram generation functionality.  Only displays when --newstuff is set.
 *
 * Revision 3.45  2005/06/13 23:38:35  sueh
 * bug# 583 Preventing tilt.com from being overwritten with a default
 * imageBinned after the .ali file is deleted.  DoneTomogramGeneration()
 * needs update and save tilt.com, but the result from getStackBinning will
 * be wrong if the .ali file has been deleted.  Move the responsibility for
 * getting the right imageBinned to TiltParam.  Modify getStackBinning() to
 * have an option to return a null value when it fails to calculate the stack
 * binning.  If TiltParam.setImageBinned() gets a null value and
 * imageBinned is not null, it won't override the current imageBinned value.
 *
 * Revision 3.44  2005/06/11 02:53:48  sueh
 * bug# 583, bug# 682, bug# 584  Storing screen binning for Tomo Pos and
 * Tomo Gen in MetaData separately (Tomo Pos default is 3).  Upgraded
 * tilt.com to have all unbinned parameters and a binning value.  No longer
 * managing full image size in tilt.com, except to upgrade the file.
 *
 * Revision 3.43  2005/06/01 21:27:51  sueh
 * bug# 667 Standardizing getMetaData function names.
 *
 * Revision 3.42  2005/04/25 21:41:37  sueh
 * bug# 615 Passing the axis where a command originates to the message
 * functions so that the message will be popped up in the correct window.
 * This requires adding AxisID to many objects.  Move the interface for
 * popping up message dialogs to UIHarness.  It prevents headless
 * exceptions during a test execution.  It also allows logging of dialog
 * messages during a test.  It also centralizes the dialog interface and
 * allows the dialog functions to be synchronized to prevent dialogs popping
 * up in both windows at once.  All Frame functions will use UIHarness as a
 * public interface.
 *
 * Revision 3.41  2005/04/21 20:55:14  sueh
 * bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * that requires it.
 *
 * Revision 3.40  2005/04/16 02:05:14  sueh
 * bug# 615 Moved the adding of exit buttons to the base class.
 *
 * Revision 3.39  2005/03/29 19:54:50  sueh
 * bug# 623 Added getBinning().
 *
 * Revision 3.38  2005/03/11 01:38:28  sueh
 * bug# 533 Added get and setBlendParams to get and set
 * linearInterpolation in BlendmontParam.
 *
 * Revision 3.37  2005/03/09 22:33:23  sueh
 * bug# 533 Modify the context sensitive help so that it displays blendmont
 * man pages and log files instead of newst when the view type is montage.
 * Change "Newstack Parameters" to blendmont.
 *
 * Revision 3.36  2005/03/09 18:12:29  sueh
 * bug# 533 In the final alignment box disable linear interpolation and binning
 * when the view type is montage.
 *
 * Revision 3.35  2005/03/02 00:13:49  sueh
 * Corrected label.
 *
 * Revision 3.34  2005/02/19 00:31:21  sueh
 * bug# 606 Removed MetaData (Setup) zfactors, fiducialess, wholetomogram,
 * and localalignments.  Add them for A and B.
 *
 * Revision 3.33  2005/02/11 16:46:27  sueh
 * bug# 600 Getting tooltips using EtomoAutodoc instead of TooltipFormatter.
 *
 * Revision 3.32  2005/01/14 03:11:32  sueh
 * bug# 511 Added DialogType to super constructor.
 *
 * Revision 3.31  2005/01/12 18:35:32  sueh
 * bug# 505 Added ltfExtraExcludeList.
 *
 * Revision 3.30  2005/01/12 02:39:20  sueh
 * bug# 579 setting use local alignment correctly
 *
 * Revision 3.29  2005/01/12 00:48:10  sueh
 * bug# 579 Stop enabling/disabling Use local alignments checkbox when
 * fiducialess checkbox is selected/unselected.  This needs to initiated from
 * ProcessManager.postProcess().  Setting/getting Use local alignments
 * checkbox from metaData.
 *
 * Revision 3.28  2005/01/11 18:08:21  sueh
 * bug# 578 Sending useZFactors state to metaData to preserve it when it is
 * disabled.
 *
 * Revision 3.27  2005/01/08 01:56:11  sueh
 * bug# 578 Added z factors checkbox and a public function to
 * enable/disable it.
 *
 * Revision 3.26  2004/12/02 20:42:40  sueh
 * bug# 566 ContextPopup can specify an anchor in both the tomo guide and
 * the join guide.  Need to specify the guide to anchor.
 *
 * Revision 3.25  2004/11/20 00:06:42  sueh
 * bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 *
 * Revision 3.24.2.4  2004/10/11 02:19:11  sueh
 * bug# 520 Passed the manager to the ContextPopup object in order to get
 * the propertyUserDir.
 *
 * Revision 3.24.2.3  2004/10/08 16:41:41  sueh
 * bug# 520 Since EtomoDirector is a singleton, made all functions and
 * member variables non-static.
 *
 * Revision 3.24.2.2  2004/09/15 22:48:16  sueh
 * bug# 520 call openMessageDialog in mainPanel instead of mainFrame.
 *
 * Revision 3.24.2.1  2004/09/07 18:02:16  sueh
 * bug# 520 getting dataset name from metadata
 *
 * Revision 3.24  2004/07/20 23:28:34  sueh
 * bug# 514
 *
 * Revision 3.23  2004/07/20 23:08:33  sueh
 * bug# 502 setting fiducialess in tilt (not getting fiducialess
 * from tilt).  Use local alignment is disabled when fiducialess is
 * true
 *
 * Revision 3.22  2004/07/15 20:34:13  sueh
 * bug# 500 moving linear interpolation to Basic mode
 *
 * Revision 3.21  2004/07/15 20:17:35  sueh
 * bug# 499 added "optional" to 2d filtering title
 *
 * Revision 3.20  2004/07/02 20:42:07  sueh
 * bug# 489 added a tilt button panel to put the buttons at the bottom
 * side by side, changed the updateAdvanced() function
 *
 * Revision 3.19  2004/06/17 22:30:15  sueh
 * bug# 475 set X offset to 0 if X offset is empty and Z offset is set
 *
 * Revision 3.18  2004/06/17 21:25:47  sueh
 * bug# 473
 *
 * Revision 3.17  2004/06/17 20:18:22  sueh
 * bug# 472
 *
 * Revision 3.16  2004/06/17 18:49:55  sueh
 * bug# 472
 *
 * Revision 3.15  2004/06/01 19:02:33  rickg
 * Bug #391 moved fiducialess parameters in with newstack
 * parameters, fixed trash javadoc header
 *
 * Revision 3.14  2004/05/26 04:54:08  rickg
 * Bug #391 added fiducialess parameter interface and UI objects
 *
 * Revision 3.13  2004/04/28 16:15:18  sueh
 * bug# 409 changing border name (mast)
 *
 * Revision 3.12  2004/04/24 21:28:42  rickg
 * bug #424 Fixed and organized UI layouts
 *
 * Revision 3.11  2004/04/16 02:20:19  sueh
 * removing print statements
 *
 * Revision 3.10  2004/04/16 02:13:14  sueh
 * bug# 409 Added startingAndEndingZ
 * Added code to enable and disable filter buttons, based on a call from
 * ApplicationManager (enableFilter) and the user placing a value in
 * startingAndEndingZ
 *
 * Revision 3.9  2004/04/13 17:23:36  sueh
 * bug# 409 add file choose for mtf filter.  Automatically goes to
 * $IMOD_CALIB_DIR/Camera, if it exists.  File filter:  .mtf
 *
 * Revision 3.8  2004/04/12 17:16:47  sueh
 * bug# 409  Change HighFrequencyRadiusSigma to LowPassRadiusSigma.
 *
 * Revision 3.7  2004/03/30 17:43:16  sueh
 * bug# 409 adding tooltips to new fields
 *
 * Revision 3.6  2004/03/29 20:57:09  sueh
 * bug# 409 added actions for MTF Filter buttons, shortened screen by putting some
 * numeric fields side-by-side, stopped recreating the panel each time Basic/Advanced
 * is pressed to prevent the panels from moving
 *
 * Revision 3.5  2004/03/24 18:17:12  sueh
 * bug# 409 added some MTF filter fields without placing anything on the screen,
 * reformatted
 *
 * Revision 3.4  2004/03/24 03:03:26  rickg
 * Bug# 395 Implemented ability to create binned tomogram
 *
 * Revision 3.3  2004/03/15 20:33:55  rickg
 * button variable name changes to btn...
 *
 * Revision 3.2  2004/02/13 00:09:26  rickg
 * Updated for PIP based newstack
 *
 * Revision 3.1  2004/01/30 22:45:23  sueh
 * bug# 356 Changing buttons with html labels to
 * MultiLineButton and MultiLineToggleButton
 *
 * Revision 3.0  2003/11/07 23:19:01  rickg
 * Version 1.0.0
 *
 * Revision 2.19  2003/11/07 19:54:02  rickg
 * Don't delete preali in delete aligned stacks code.
 *
 * Revision 2.18  2003/10/30 01:43:44  rickg
 * Bug# 338 Remapped context menu entries
 *
 * Revision 2.17  2003/10/28 23:35:48  rickg
 * Bug# 336 Context menu label capitalization
 *
 * Revision 2.16  2003/10/22 21:31:02  rickg
 * Bug# 287 Default value handling for SLICE OFFSET and SHIFT
 *
 * Revision 2.15  2003/10/21 23:41:28  rickg
 * Bug# 288 Tooltips
 * Bug# 296 Added button to delete .preal and .ali
 *
 * Revision 2.14 2003/10/14 23:45:01 rickg
 * Bug# 285 Added view aligned stack button
 *
 * Revision 2.13 2003/10/14 22:53:55 rickg
 * Bug #286 Label changes
 * 
 * Revision 2.12 2003/10/02 18:57:47 sueh
 * bug236 added testing:
 * NewstParamTest
 * ComScriptTest
 * Removed marks
 *
 * Revision 2.11 2003/09/29 23:34:57 sueh
 * bug236 Added UseLinearInterpolation to
 * TomogramGenerationDialog.
 * UseLinearInterpolation:
 * check box
 * Advanced
 * newst -linear
 *
 * Files:
 * ComScriptManager.java
 * ConstNewstParam.java
 * NewstParam.java
 * TomogramGenerationDialog.java
 * ApplicationManager.java
 *
 * Revision 2.10 2003/09/08 22:51:25 rickg
 * Added commit test volume action
 *
 * Revision 2.9 2003/09/08 05:47:09 rickg
 * Added trial tilt
 * Output for a single axis tomogram is changed to
 * dataset_full.rec
 *
 * Revision 2.8 2003/06/25 22:14:57 rickg
 * Constructed a panel for the tilt parameters
 *
 * Revision 2.7 2003/06/23 23:26:59 rickg
 * Added advanced options/parameters
 *
 * Revision 2.6 2003/05/23 22:14:11 rickg
 * Removed any extensions from log file labels in context menu
 *
 * Revision 2.5 2003/05/23 21:26:55 rickg
 * *** empty log message ***
 *
 * Revision 2.4 2003/04/28 23:25:25 rickg
 * Changed visible imod references to 3dmod
 *
 * Revision 2.3 2003/04/24 17:46:54 rickg
 * Changed fileset name to dataset name
 *
 * Revision 2.2 2003/03/02 23:30:41 rickg
 * Combine layout in progress
 *
 * Revision 2.1 2003/01/24 21:04:18 rickg
 * AxisID bug fix from single buttonAction function
 *
 * Revision 2.0 2003/01/24 20:30:31 rickg
 * Single window merge to main branch
 *
 * Revision 1.6.2.1 2003/01/24 18:43:37 rickg
 * Single window GUI layout initial revision
 *
 * Revision 1.6 2002/12/19 17:45:22 rickg
 * Implemented advanced dialog state processing
 * including:
 * default advanced state set on start up
 * advanced button management now handled by
 * super class
 *
 * Revision 1.5 2002/12/19 00:30:26 rickg
 * app manager and root pane moved to super class
 *
 * Revision 1.4 2002/11/14 21:18:37 rickg
 * Added anchors into the tomoguide
 *
 * Revision 1.3 2002/10/17 22:40:22 rickg
 * Added fileset name to window title
 * this reference removed applicationManager messages
 *
 * Revision 1.2 2002/10/07 22:31:18 rickg
 * removed unused imports
 * reformat after emacs trashed it
 *
 * Revision 1.1 2002/09/09 22:57:02 rickg
 * Initial CVS entry, basic functionality not including combining
 *
 * </p>
 */

public class TomogramGenerationDialog extends ProcessDialog implements ContextMenu {
  public static final String rcsid = "$Id$";

  public static final String X_AXIS_TILT_TOOLTIP = "This line allows one to rotate the reconstruction around the X axis, so "
      + "that a section that appears to be tilted around the X axis can be "
      + "made flat to fit into a smaller volume.";

  private final ButtonGroup bgMethod = new ButtonGroup();
  private final RadioButton rbBackProjection = new RadioButton("Back Projection",
      bgMethod);
  private final RadioButton rbSirt = new RadioButton("SIRT", bgMethod);

  private final TiltPanel tiltPanel;
  private final TomogramGenerationExpert expert;
  private final SirtPanel sirtPanel;

  private TomogramGenerationDialog(ApplicationManager appMgr,
      TomogramGenerationExpert expert, AxisID axisID) {
    super(appMgr, axisID, DialogType.TOMOGRAM_GENERATION);
    this.expert = expert;
    tiltPanel = TiltPanel.getInstance(appMgr, axisID, dialogType, btnAdvanced, this);
    sirtPanel = SirtPanel.getInstance(appMgr, axisID, dialogType, btnAdvanced, this);
    tiltPanel.addFieldObserver(sirtPanel);
    sirtPanel.addResumeObserver(tiltPanel);
  }

  static TomogramGenerationDialog getInstance(ApplicationManager appMgr,
      TomogramGenerationExpert expert, AxisID axisID) {
    TomogramGenerationDialog instance = new TomogramGenerationDialog(appMgr, expert,
        axisID);
    instance.createPanel();
    instance.updateAdvanced();
    instance.addListeners();
    return instance;
  }

  private void createPanel() {
    // init
    JPanel pnlMethod = new JPanel();
    rbBackProjection.setSelected(true);
    // root panel
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    rootPanel.setBorder(new BeveledBorder("Tomogram Generation").getBorder());
    rootPanel.add(pnlMethod);
    rootPanel.add(tiltPanel.getRoot());
    rootPanel.add(sirtPanel.getRoot());
    // method panel
    pnlMethod.setLayout(new BoxLayout(pnlMethod, BoxLayout.X_AXIS));
    pnlMethod.add(Box.createHorizontalGlue());
    pnlMethod.add(rbBackProjection.getComponent());
    pnlMethod.add(Box.createHorizontalGlue());
    pnlMethod.add(rbSirt.getComponent());
    pnlMethod.add(Box.createHorizontalGlue());
    // buttons
    btnExecute.setText("Done");
    addExitButtons();
  }

  private void addListeners() {
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);
    ActionListener listener = new TomogramGenerationActionListener(this);
    rbBackProjection.addActionListener(listener);
    rbSirt.addActionListener(listener);
  }

  public void msgSirtSucceeded() {
    sirtPanel.msgSirtSucceeded();
  }

  void checkpoint(final ConstTiltParam param, final TomogramState state) {
    tiltPanel.checkpoint(param);
    sirtPanel.checkpoint(state);
  }

  public boolean allowTiltComSave() {
    return rbBackProjection.isSelected() || !sirtPanel.isResume();
  }

  void getParameters(MetaData metaData) throws FortranInputSyntaxException {
    metaData.setGenBackProjection(axisID, rbBackProjection.isSelected());
    tiltPanel.getParameters(metaData);
    sirtPanel.getParameters(metaData);
  }

  void getParameters(ReconScreenState screenState) {
    tiltPanel.getParameters(screenState);
    sirtPanel.getParameters(screenState);
  }

  void setParameters(ConstMetaData metaData) {
    if (metaData.isGenBackProjection(axisID)) {
      rbBackProjection.setSelected(true);
    }
    else {
      rbSirt.setSelected(true);
    }
    tiltPanel.setParameters(metaData);
    sirtPanel.setParameters(metaData);
    methodChanged();
  }

  /**
   * Set the UI parameters with the specified tiltParam values
   * WARNING: be sure the setNewstParam is called first so the binning value for
   * the stack is known.  The thickness, first and last slice, width and x,y,z
   * offsets are scaled so that they are represented to the user in unbinned
   * dimensions.
   * @param tiltParam
   */
  void setParameters(ConstTiltParam tiltParam, boolean initialize) {
    tiltPanel.setParameters(tiltParam, initialize);
  }

  void setParameters(final SirtsetupParam param) {
    sirtPanel.setParameters(param);
  }

  final void setParameters(ReconScreenState screenState) {
    tiltPanel.setParameters(screenState);
    sirtPanel.setParameters(screenState);
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    tiltPanel.updateAdvanced(isAdvanced());
    sirtPanel.updateAdvanced(isAdvanced());
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  /**
   * Right mouse button context menu.  Sensitive to the method radio buttons.
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    boolean sirtInUse = rbSirt.isSelected();
    int numManpages = 2;
    int numLogs = 1;
    if (sirtInUse) {
      numManpages = 3;
      numLogs = 1;
    }
    String[] manPageLabel = new String[numManpages];
    String[] manPage = new String[numManpages];
    String[] logFileLabel = new String[numLogs];
    String[] logFile = new String[numLogs];
    int i = 0;
    manPageLabel[i] = "Tilt";
    manPage[i] = "tilt.html";
    if (!sirtInUse) {
      logFileLabel[i] = "Tilt";
      logFile[i++] = "tilt" + axisID.getExtension() + ".log";
    }
    else {
      logFileLabel[i] = "Final SIRT";
      logFile[i++] = "tilt" + axisID.getExtension() + "_sirt-finish" + ".log";
      manPageLabel[i] = "Sirtsetup";
      manPage[i++] = "sirtsetup.html";
    }
    manPageLabel[i] = "3dmod";
    manPage[i] = "3dmod.html";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
        "TOMOGRAM GENERATION", ContextPopup.TOMO_GUIDE, manPageLabel, manPage,
        logFileLabel, logFile, applicationManager, axisID);
  }

  public void setTiltState(TomogramState state, ConstMetaData metaData) {
    tiltPanel.setState(state, metaData);
  }

  void done() {
    expert.doneDialog();
    tiltPanel.done();
    sirtPanel.done();
    setDisplayed(false);
  }

  TiltDisplay getTiltDisplay() {
    return tiltPanel;
  }

  SirtsetupDisplay getSirtsetupDisplay() {
    return sirtPanel;
  }

  boolean isBackProjection() {
    return rbBackProjection.isSelected();
  }

  boolean isSirt() {
    return rbSirt.isSelected();
  }

  private void methodChanged() {
    tiltPanel.msgMethodChanged();
    sirtPanel.msgMethodChanged();
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public ProcessingMethod getProcessingMethod() {
    return tiltPanel.getProcessingMethod();
  }

  private void action(final ActionEvent event) {
    String actionCommand = event.getActionCommand();
    if (actionCommand.equals(rbBackProjection.getActionCommand())
        || actionCommand.equals(rbSirt.getActionCommand())) {
      methodChanged();
    }
  }

  private static final class TomogramGenerationActionListener implements ActionListener {
    private final TomogramGenerationDialog adaptee;

    private TomogramGenerationActionListener(final TomogramGenerationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event);
    }
  }
}