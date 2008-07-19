package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;
import etomo.ApplicationManager;
import etomo.EtomoDirector;
import etomo.comscript.NewstParam;
import etomo.storage.CpuAdoc;
import etomo.storage.LogFile;
import etomo.storage.MtfFileFilter;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.PanelHeaderState;
import etomo.type.ProcessResultDisplay;
import etomo.type.ProcessResultDisplayFactory;
import etomo.type.ReconScreenState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.ViewType;

/**
 * <p>
 * Description: Tomogram generation user interface
 * </p>
 * 
 * <p>
 * Copyright: Copyright (c) 2002 - 2006</p>
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

public class TomogramGenerationDialog extends ProcessDialog implements
    ContextMenu, FiducialessParams, Expandable, AbstractParallelDialog,
    Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  public static final String X_AXIS_TILT_TOOLTIP = "This line allows one to rotate the reconstruction around the X axis, so "
      + "that a section that appears to be tilted around the X axis can be "
      + "made flat to fit into a smaller volume.";
  static final String SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL = "Size to output";

  private JPanel pnlTilt = new JPanel();

  // Fiducialess parameters
  private CheckBox cbFiducialess = new CheckBox("Fiducialless alignment");
  private LabeledTextField ltfRotation = new LabeledTextField(
      "Tilt axis rotation: ");

  // Newst/Newstack objects
  private CheckBox cbUseLinearInterpolation = new CheckBox(
      "Use linear interpolation");
  private LabeledSpinner spinBinning;

  //  Aligned stack buttons
  private final Run3dmodButton btnNewst;
  private Run3dmodButton btn3dmodFull = Run3dmodButton.get3dmodInstance(
      "View Full Aligned Stack", this);

  //  Tilt objects
  private SpacedTextField ltfXShift = new SpacedTextField("X shift:");
  private SpacedTextField ltfZShift = new SpacedTextField("Z shift: ");
  private SpacedTextField ltfSliceStart = new SpacedTextField("First slice: ");
  private SpacedTextField ltfSliceStop = new SpacedTextField("Last slice: ");
  private SpacedLabel lblInY = new SpacedLabel(" in Y");
  private SpacedTextField ltfSliceIncr = new SpacedTextField(
      "Slice step in Y: ");
  private SpacedTextField ltfTomoWidth = new SpacedTextField(
      "Tomogram width in X: ");
  private SpacedTextField ltfTomoThickness = new SpacedTextField(
      "Tomogram thickness in Z: ");
  private SpacedTextField ltfXAxisTilt = new SpacedTextField("X axis tilt: ");
  private SpacedTextField ltfTiltAngleOffset = new SpacedTextField(
      "Tilt angle offset: ");
  private SpacedTextField ltfRadialMax = new SpacedTextField(
      "Radial filter cutoff: ");
  private SpacedTextField ltfRadialFallOff = new SpacedTextField("Falloff: ");
  private SpacedTextField ltfDensityOffset = new SpacedTextField("Offset: ");
  private SpacedTextField ltfDensityScale = new SpacedTextField(
      "Output density scaling factor: ");
  private SpacedTextField ltfLogOffset = new SpacedTextField("Log offset: ");
  private CheckBox cbUseLocalAlignment = new CheckBox("Use local alignments");

  //  Trial tomogram objects

  private JLabel lblTrialTomogramName = new JLabel("Trial tomogram filename: ");
  private JComboBox cmboTrialTomogramName = new JComboBox();
  private MultiLineButton btnTrial = new MultiLineButton(
      "Generate Trial Tomogram");
  private Run3dmodButton btn3dmodTrial = Run3dmodButton.get3dmodInstance(
      "View Trial in 3dmod", this);
  private final MultiLineButton btnUseTrial;

  // MTF Filter objects
  private LabeledTextField ltfLowPassRadiusSigma = new LabeledTextField(
      "Low pass (cutoff,sigma): ");
  private ImageIcon iconFolder = new ImageIcon(ClassLoader
      .getSystemResource("images/openFile.gif"));
  private LabeledTextField ltfMtfFile = new LabeledTextField("MTF file: ");
  private JButton btnMtfFile = new JButton(iconFolder);
  private LabeledTextField ltfMaximumInverse = new LabeledTextField(
      "Maximum Inverse: ");
  private LabeledTextField ltfInverseRolloffRadiusSigma = new LabeledTextField(
      "Rolloff (radius,sigma): ");
  private final Run3dmodButton btnFilter;
  private Run3dmodButton btnViewFilter = Run3dmodButton.get3dmodInstance(
      "View Filtered Stack", this);
  private final MultiLineButton btnUseFilter;
  private SpacedTextField ltfStartingAndEndingZ = new SpacedTextField(
      "Starting and ending views: ");

  //  Tomogram generation buttons
  private final Run3dmodButton btnTilt;
  private Run3dmodButton btn3dmodTomogram = Run3dmodButton.get3dmodInstance(
      "View Tomogram In 3dmod", this);
  private final MultiLineButton btnDeleteStack;
  private CheckBox cbUseZFactors = new CheckBox("Use Z factors");
  private SpacedTextField ltfExtraExcludeList = new SpacedTextField(
      "Extra views to exclude: ");
  private CheckBox cbParallelProcess;
  //headers should not go into garbage collection
  private PanelHeader newstHeader = null;
  private PanelHeader tiltHeader = null;
  private PanelHeader trialHeader = null;
  private PanelHeader filterHeader = null;
  //panels that are changed in setAdvanced()
  private SpacedPanel inverseParamsPanel;
  private JPanel trialPanel;
  private JPanel tiltBodyPanel;
  private JPanel filterBodyPanel;
  private SpacedPanel newstBodyPanel;
  private SpacedPanel trialBodyPanel;
  private LabeledTextField ltfSizeToOutputInXandY = new LabeledTextField(
      SIZE_TO_OUTPUT_IN_X_AND_Y_LABEL + " (X,Y - unbinned): ");

  //backward compatibility functionality - if the metadata binning is missing
  //get binning from newst
  private boolean trialTilt = false;
  private final ReconScreenState screenState;
  private final ButtonListener tomogramGenerationListener;
  private final TomogramGenerationExpert expert;

  public TomogramGenerationDialog(ApplicationManager appMgr,
      TomogramGenerationExpert expert, AxisID axisID) {
    super(appMgr, axisID, DialogType.TOMOGRAM_GENERATION);
    this.expert = expert;
    screenState = appMgr.getScreenState(axisID);
    ProcessResultDisplayFactory displayFactory = appMgr
        .getProcessResultDisplayFactory(axisID);
    btnNewst = (Run3dmodButton) displayFactory.getFullAlignedStack();
    btnNewst.setContainer(this);
    btnNewst.setDeferred3dmodButton(btn3dmodFull);
    btnFilter = (Run3dmodButton) displayFactory.getFilter();
    btnFilter.setContainer(this);
    btnFilter.setDeferred3dmodButton(btnViewFilter);
    btnUseFilter = (MultiLineButton) displayFactory.getUseFilteredStack();
    btnUseTrial = (MultiLineButton) displayFactory.getUseTrialTomogram();
    btnTilt = (Run3dmodButton) displayFactory.getGenerateTomogram();
    btnTilt.setContainer(this);
    btnTilt.setDeferred3dmodButton(btn3dmodTomogram);
    btnDeleteStack = (MultiLineButton) displayFactory.getDeleteAlignedStack();
    fixRootPanel(rootSize);
    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    btnExecute.setText("Done");
    // Layout the main panel (and sub panels) and add it to the root panel
    pnlTilt.setBorder(new BeveledBorder("Tomogram Generation").getBorder());
    pnlTilt.setLayout(new BoxLayout(pnlTilt, BoxLayout.Y_AXIS));
    UIUtilities.addWithYSpace(pnlTilt, layoutNewstPanel());
    UIUtilities.addWithYSpace(pnlTilt, layoutFilterPanel());
    UIUtilities.addWithYSpace(pnlTilt, layoutTiltPanel());
    UIUtilities.alignComponentsX(pnlTilt, Component.CENTER_ALIGNMENT);

    rootPanel.add(pnlTilt);
    addExitButtons();

    // Bind the buttons to the action listener
    tomogramGenerationListener = new ButtonListener(this);
    btnNewst.addActionListener(tomogramGenerationListener);
    btn3dmodFull.addActionListener(tomogramGenerationListener);
    btnFilter.addActionListener(tomogramGenerationListener);
    btnViewFilter.addActionListener(tomogramGenerationListener);
    btnUseFilter.addActionListener(tomogramGenerationListener);
    btnTrial.addActionListener(tomogramGenerationListener);
    btn3dmodTrial.addActionListener(tomogramGenerationListener);
    btnUseTrial.addActionListener(tomogramGenerationListener);
    btnTilt.addActionListener(tomogramGenerationListener);
    btn3dmodTomogram.addActionListener(tomogramGenerationListener);
    btnDeleteStack.addActionListener(tomogramGenerationListener);
    btnMtfFile.addActionListener(new MtfFileActionListener(this));
    ltfStartingAndEndingZ
        .addKeyListener(new StartingAndEndingZKeyListener(this));
    cbFiducialess.addActionListener(tomogramGenerationListener);
    cbParallelProcess.addActionListener(tomogramGenerationListener);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    rootPanel.addMouseListener(mouseAdapter);

    updateFiducialess();
    // Set the default advanced dialog state
    updateAdvanced();
    setToolTipText();
  }

  public static ProcessResultDisplay getFullAlignedStackDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance(
        "Create Full Aligned Stack", DialogType.TOMOGRAM_GENERATION);
  }

  public static ProcessResultDisplay getFilterDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Filter",
        DialogType.TOMOGRAM_GENERATION);
  }

  public static ProcessResultDisplay getUseFilteredStackDisplay() {
    return MultiLineButton.getToggleButtonInstance("Use Filtered Stack",
        DialogType.TOMOGRAM_GENERATION);
  }

  public static ProcessResultDisplay getUseTrialTomogramDisplay() {
    return MultiLineButton.getToggleButtonInstance(
        "Use Current Trial Tomogram", DialogType.TOMOGRAM_GENERATION);
  }

  public static ProcessResultDisplay getGenerateTomogramDisplay() {
    return Run3dmodButton.getDeferredToggle3dmodInstance("Generate Tomogram",
        DialogType.TOMOGRAM_GENERATION);
  }

  void addToTrialTomogramName(String trialTomogramName) {
    cmboTrialTomogramName.addItem(trialTomogramName);
  }

  public static ProcessResultDisplay getDeleteAlignedStackDisplay() {
    return MultiLineButton.getToggleButtonInstance(
        "Delete Aligned Image Stack", DialogType.TOMOGRAM_GENERATION);
  }

  void setFilterButtonEnabled(boolean enable) {
    btnFilter.setEnabled(enable);
  }

  void setFilterButtonState(ReconScreenState screenState) {
    btnFilter.setButtonState(screenState.getButtonState(btnFilter
        .getButtonStateKey()));
  }

  void setViewFilterButtonEnabled(boolean enable) {
    btnViewFilter.setEnabled(enable);
  }

  void setXShift(float xShift) {
    ltfXShift.setText(xShift);
  }

  void setZShift(ConstEtomoNumber zShift) {
    ltfZShift.setText(zShift);
  }

  void setUseFilterEnabled(boolean enable) {
    btnUseFilter.setEnabled(enable);
  }

  void setUseTrialButtonState(ReconScreenState screenState) {
    btnUseTrial.setButtonState(screenState.getButtonState(btnUseTrial
        .getButtonStateKey()));
  }

  public void setFiducialessAlignment(boolean state) {
    cbFiducialess.setSelected(state);
    updateFiducialess();
  }

  public boolean isDensityOffsetSet() {
    return ltfDensityOffset.getText().matches("\\S+");
  }

  public boolean isDensityScaleSet() {
    return ltfDensityScale.getText().matches("\\S+");
  }

  public boolean isFiducialess() {
    return cbFiducialess.isSelected();
  }

  boolean isLogOffsetSet() {
    return ltfLogOffset.getText().matches("\\S+");
  }

  public void setImageRotation(float tiltAxisAngle) {
    ltfRotation.setText(tiltAxisAngle);
  }

  void setInverseRolloffRadiusSigma(String inverseRolloffRadiusSigma) {
    ltfInverseRolloffRadiusSigma.setText(inverseRolloffRadiusSigma);
  }

  void setLowPassRadiusSigma(String lowPassRadiusSigma) {
    ltfLowPassRadiusSigma.setText(lowPassRadiusSigma);
  }

  void setNewstHeaderState(PanelHeaderState state) {
    newstHeader.setState(state);
  }

  void setFilterHeaderState(PanelHeaderState state) {
    filterHeader.setState(state);
  }

  public float getImageRotation() throws NumberFormatException {
    return Float.parseFloat(ltfRotation.getText());
  }

  String getInverseRolloffRadiusSigma() {
    return ltfInverseRolloffRadiusSigma.getText();
  }

  float getLogOffset() {
    return Float.parseFloat(ltfLogOffset.getText());
  }

  String getLogOffsetLabel() {
    return ltfLogOffset.getLabel();
  }

  String getMaximumInverse() {
    return ltfMaximumInverse.getText();
  }

  String getLowPassRadiusSigma() {
    return ltfLowPassRadiusSigma.getText();
  }

  String getMtfFile() {
    return ltfMtfFile.getText();
  }

  String getStartingAndEndingZ() {
    return ltfStartingAndEndingZ.getText();
  }

  float getTiltAngleOffset() {
    return Float.parseFloat(ltfTiltAngleOffset.getText());
  }

  String getTiltAngleOffsetLabel() {
    return ltfTiltAngleOffset.getLabel();
  }

  void getTiltHeaderState(PanelHeaderState state) {
    tiltHeader.getState(state);
  }

  String getTomoThickness() {
    return ltfTomoThickness.getText();
  }

  String getTomoThicknessLabel() {
    return ltfTomoThickness.getLabel();
  }

  int getTomoWidth() {
    return Integer.parseInt(ltfTomoWidth.getText());
  }

  String getTomoWidthLabel() {
    return ltfTomoWidth.getLabel();
  }

  void setTrialHeaderState(PanelHeaderState state) {
    trialHeader.setState(state);
  }

  void setUseFilterButtonState(ReconScreenState screenState) {
    btnUseFilter.setButtonState(screenState.getButtonState(btnUseFilter
        .getButtonStateKey()));
  }

  int getBinning() {
    return ((Integer) spinBinning.getValue()).intValue();
  }

  float getDensityOffset() {
    return Float.parseFloat(ltfDensityOffset.getText());
  }

  String getDensityOffsetLabel() {
    return ltfDensityOffset.getLabel();
  }

  float getDensityScale() {
    return Float.parseFloat(ltfDensityScale.getText());
  }

  String getExtraExcludeList() {
    return ltfExtraExcludeList.getText();
  }

  String getDensityScaleLabel() {
    return ltfDensityScale.getLabel();
  }

  void getFilterHeaderState(PanelHeaderState state) {
    filterHeader.getState(state);
  }

  void getTrialHeaderState(PanelHeaderState state) {
    trialHeader.getState(state);
  }

  void getNewstHeaderState(PanelHeaderState state) {
    newstHeader.getState(state);
  }

  void setNewstButtonState(ReconScreenState screenState) {
    btnNewst.setButtonState(screenState.getButtonState(btnNewst
        .getButtonStateKey()));
  }

  void setParallelProcessEnabled(boolean enable) {
    cbParallelProcess.setEnabled(enable);
  }

  void setSliceStart(int sliceStart) {
    ltfSliceStart.setText(sliceStart);
  }

  void setSliceStop(int sliceStop) {
    ltfSliceStop.setText(sliceStop);
  }

  void setStartingAndEndingZ(String startingAndEndingZ) {
    ltfStartingAndEndingZ.setText(startingAndEndingZ);
  }

  void setSliceIncr(int sliceIncr) {
    ltfSliceIncr.setText(sliceIncr);
  }

  void setXAxisTilt(double xAxisTilt) {
    ltfXAxisTilt.setText(xAxisTilt);
  }

  void setTiltAngleOffset(ConstEtomoNumber tiltAngleOffset) {
    ltfTiltAngleOffset.setText(tiltAngleOffset);
  }

  void setRadialMax(float radialMax) {
    ltfRadialMax.setText(radialMax);
  }

  void setRadialFallOff(float radialFallOff) {
    ltfRadialFallOff.setText(radialFallOff);
  }

  void setDensityOffset(float densityOffset) {
    ltfDensityOffset.setText(densityOffset);
  }

  void setDensityScale(float densityScale) {
    ltfDensityScale.setText(densityScale);
  }

  void setExtraExcludeList(String extraExcludeList) {
    ltfExtraExcludeList.setText(extraExcludeList);
  }

  void setMaximumInverse(String maximumInverse) {
    ltfMaximumInverse.setText(maximumInverse);
  }

  void setMtfFile(String mtfFile) {
    ltfMtfFile.setText(mtfFile);
  }

  void setLogOffset(float logOffset) {
    ltfLogOffset.setText(logOffset);
  }

  void setParallelProcess(boolean select) {
    cbParallelProcess.setSelected(select);
  }

  void setTiltButtonState(ReconScreenState screenState) {
    btnTilt.setButtonState(screenState.getButtonState(btnTilt
        .getButtonStateKey()));
  }

  void setTiltHeaderState(PanelHeaderState state) {
    tiltHeader.setState(state);
  }

  void setTomoThickness(int tomoThickness) {
    ltfTomoThickness.setText(tomoThickness);
  }

  void setTomoWidth(int tomoWidth) {
    ltfTomoWidth.setText(tomoWidth);
  }

  /**
   * Return the selected trial tomogram name
   * 
   * @return
   */
  public String getTrialTomogramName() {
    String trialTomogramName = (String) cmboTrialTomogramName.getSelectedItem();
    if (trialTomogramName == null) {
      trialTomogramName = "";
    }
    return trialTomogramName;
  }

  String getXAxisTilt() {
    return ltfXAxisTilt.getText();
  }

  String getXAxisTiltLabel() {
    return ltfXAxisTilt.getLabel();
  }

  float getXShift() {
    return Float.parseFloat(ltfXShift.getText());
  }

  String getXShiftLabel() {
    return ltfXShift.getLabel();
  }

  float getZShift() {
    return Float.parseFloat(ltfZShift.getText());
  }

  String getZShiftLabel() {
    return ltfZShift.getLabel();
  }

  float getRadialFallOff() {
    return Float.parseFloat(ltfRadialFallOff.getText());
  }

  String getRadialFallOffLabel() {
    return ltfRadialFallOff.getLabel();
  }

  float getRadialMax() {
    return Float.parseFloat(ltfRadialMax.getText());
  }

  String getRadialMaxLabel() {
    return ltfRadialMax.getLabel();
  }

  int getSliceIncr() {
    return Integer.parseInt(ltfSliceIncr.getText());
  }

  String getSliceIncrLabel() {
    return ltfSliceIncr.getLabel();
  }

  int getSliceStart() {
    return Integer.parseInt(ltfSliceStart.getText());
  }

  String getSliceStartLabel() {
    return ltfSliceStart.getLabel();
  }

  int getSliceStop() {
    return Integer.parseInt(ltfSliceStop.getText());
  }

  String getSliceStopLabel() {
    return ltfSliceStop.getLabel();
  }

  public void expand(ExpandButton button) {
    if (tiltHeader != null) {
      if (tiltHeader.equalsOpenClose(button)) {
        tiltBodyPanel.setVisible(button.isExpanded());
      }
      else if (tiltHeader.equalsAdvancedBasic(button)) {
        updateAdvancedTilt(button.isExpanded());
      }
    }
    if (filterHeader != null) {
      if (filterHeader.equalsOpenClose(button)) {
        filterBodyPanel.setVisible(button.isExpanded());
      }
      else if (filterHeader.equalsAdvancedBasic(button)) {
        updateAdvancedFilter(button.isExpanded());
      }
    }
    if (newstHeader != null) {
      if (newstHeader.equalsOpenClose(button)) {
        newstBodyPanel.setVisible(button.isExpanded());
      }
      else if (newstHeader.equalsAdvancedBasic(button)) {
        updateAdvancedNewst(button.isExpanded());
      }
    }
    if (trialHeader != null && trialHeader.equalsOpenClose(button)) {
      trialBodyPanel.setVisible(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  private void updateAdvancedFilter(boolean advanced) {
    ltfStartingAndEndingZ.setVisible(advanced);
    //ltfLowPassRadiusSigma
    inverseParamsPanel.setVisible(advanced);
    //btnFilter
    //btnViewFilter
    //btnUseFilter
  }

  private void updateAdvancedNewst(boolean advanced) {
    ltfSizeToOutputInXandY.setVisible(advanced);
  }

  private void updateAdvancedTilt(boolean advanced) {
    ltfLogOffset.setVisible(advanced);
    ltfDensityOffset.setVisible(advanced);
    ltfDensityScale.setVisible(advanced);
    ltfTomoWidth.setVisible(advanced);
    //ltfTomoThickness
    ltfSliceStart.setVisible(advanced);
    ltfSliceStop.setVisible(advanced);
    lblInY.setVisible(advanced);
    ltfSliceIncr.setVisible(advanced);
    ltfXShift.setVisible(advanced);
    ltfZShift.setVisible(advanced);
    //ltfXAxisTilt
    ltfTiltAngleOffset.setVisible(advanced);
    //ltfRadialMax
    //ltfRadialFallOff
    ltfExtraExcludeList.setVisible(advanced);
    //cbUseLocalAlignment
    //cbUseZFactors
    //cbParallelProcess
    trialPanel.setVisible(advanced);
    //btnTilt
    //btn3dmodTomogram
    //btnDeleteStacks
  }

  void setAdvanced() {
    boolean headerAdvanced = filterHeader.isAdvanced();
    if (headerAdvanced != tiltHeader.isAdvanced()) {
      return;
    }
    if (headerAdvanced != newstHeader.isAdvanced()) {
      return;
    }
    if (headerAdvanced != isAdvanced) {
      super.setAdvanced(headerAdvanced);
    }
  }

  void setBinning(int binning) {
    spinBinning.setValue(binning);
  }

  void setBinning(ConstEtomoNumber binning) {
    spinBinning.setValue(binning);
  }

  void setUseLinearInterpolation(boolean select) {
    cbUseLinearInterpolation.setSelected(select);
  }

  void setDeleteStackButtonState(ReconScreenState screenState) {
    btnDeleteStack.setButtonState(screenState.getButtonState(btnDeleteStack
        .getButtonStateKey()));
  }

  /**
   * Update the dialog with the current advanced state
   */
  private void updateAdvanced() {
    filterHeader.setAdvanced(isAdvanced);
    tiltHeader.setAdvanced(isAdvanced);
    newstHeader.setAdvanced(isAdvanced);

    UIHarness.INSTANCE.pack(axisID, applicationManager);
  }

  public boolean usingParallelProcessing() {
    return cbParallelProcess.isEnabled() && cbParallelProcess.isSelected();
  }

  boolean isParallelProcess() {
    return cbParallelProcess.isSelected();
  }

  String getSizeToOutputInXandY() {
    return ltfSizeToOutputInXandY.getText();
  }

  void setSizeToOutputInXandY(String input) {
    ltfSizeToOutputInXandY.setText(input);
  }

  boolean isRadialFallOffSet() {
    return ltfRadialFallOff.getText().matches("\\S+");
  }

  boolean isRadialMaxSet() {
    return ltfRadialMax.getText().matches("\\S+");
  }

  boolean isSliceIncrSet() {
    return ltfSliceIncr.getText().matches("\\S+");
  }

  boolean isSliceStartSet() {
    return ltfSliceStart.getText().matches("\\S+");
  }

  boolean isSliceStartNull() {
    return ltfSliceStart.getText().matches("^\\s*$");
  }

  boolean isSliceStopNull() {
    return ltfSliceStop.getText().matches("^\\s*$");
  }

  boolean isSliceStopSet() {
    return ltfSliceStop.getText().matches("\\S+");
  }

  boolean isTiltAngleOffsetSet() {
    return ltfTiltAngleOffset.getText().matches("\\S+");
  }

  boolean isTomoThicknessSet() {
    return ltfTomoThickness.getText().matches("\\S+");
  }

  boolean isTomoWidthSet() {
    return ltfTomoWidth.getText().matches("\\S+");
  }

  boolean isUseLinearInterpolation() {
    return cbUseLinearInterpolation.isSelected();
  }

  boolean isUseLocalAlignment() {
    return cbUseLocalAlignment.isSelected();
  }

  boolean isUseLocalAlignmentEnabled() {
    return cbUseLocalAlignment.isEnabled();
  }

  boolean isUseZFactors() {
    return cbUseZFactors.isSelected();
  }

  boolean isUseZFactorsEnabled() {
    return cbUseZFactors.isEnabled();
  }

  boolean isXAxisTiltSet() {
    return ltfXAxisTilt.getText().matches("\\S+");
  }

  boolean isXShiftSet() {
    return ltfXShift.getText().matches("\\S+");
  }

  boolean isZShiftSet() {
    return ltfZShift.getText().matches("\\S+");
  }

  /**
   * Layout the newstack panel
   */
  private JPanel layoutNewstPanel() {
    //panels
    JPanel newstPanel = new JPanel();
    newstPanel.setLayout(new BoxLayout(newstPanel, BoxLayout.Y_AXIS));
    newstPanel.setBorder(BorderFactory.createEtchedBorder());
    newstBodyPanel = new SpacedPanel();
    newstBodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
    //header
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      newstHeader = PanelHeader.getAdvancedBasicInstance("Blendmont", this,
          dialogType);
    }
    else {
      newstHeader = PanelHeader.getAdvancedBasicInstance("Newstack", this,
          dialogType);
    }
    //initialization
    SpinnerNumberModel integerModel = new SpinnerNumberModel(1, 1, 8, 1);
    spinBinning = new LabeledSpinner("Aligned image stack binning ",
        integerModel);
    //buttonPanel
    buttonPanel.add(Box.createHorizontalStrut(50));
    buttonPanel.add(btnNewst.getComponent());
    buttonPanel.add(Box.createHorizontalGlue());
    buttonPanel.add(btn3dmodFull.getComponent());
    buttonPanel.add(Box.createHorizontalStrut(50));
    //newstBodyPanel
    newstBodyPanel.add(cbUseLinearInterpolation);
    newstBodyPanel.add(spinBinning);
    newstBodyPanel.add(cbFiducialess);
    newstBodyPanel.add(ltfRotation);
    newstBodyPanel.add(ltfSizeToOutputInXandY);
    newstBodyPanel.add(buttonPanel);
    newstBodyPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //newstPanel
    newstPanel.add(newstHeader.getContainer());
    newstPanel.add(newstBodyPanel.getContainer());
    UIUtilities.alignComponentsX(newstPanel, Component.LEFT_ALIGNMENT);
    //configure
    //newstHeader.setOpen(true);
    btnNewst.setSize();
    btn3dmodFull.setSize();
    return newstPanel;
  }

  /**
   * Layout the MTF filter panel
   *
   */
  private JPanel layoutFilterPanel() {
    //panels
    JPanel filterPanel = new JPanel();
    filterPanel.setLayout(new BoxLayout(filterPanel, BoxLayout.Y_AXIS));
    filterPanel.setBorder(BorderFactory.createEtchedBorder());
    filterBodyPanel = new JPanel();
    filterBodyPanel.setLayout(new BoxLayout(filterBodyPanel, BoxLayout.Y_AXIS));
    inverseParamsPanel = new SpacedPanel(true);
    inverseParamsPanel.setBoxLayout(BoxLayout.Y_AXIS);
    inverseParamsPanel.setBorder(new EtchedBorder(
        "Inverse Filtering Parameters: ").getBorder());
    SpacedPanel mtfFilePanel = new SpacedPanel();
    mtfFilePanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel inversePanel = new SpacedPanel();
    inversePanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel buttonPanel = new SpacedPanel(true);
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    filterHeader = PanelHeader.getAdvancedBasicInstance(
        "2D Filtering (optional)", this, dialogType);
    //buttonPanel
    btnFilter.setSize();
    buttonPanel.add(btnFilter);
    btnViewFilter.setSize();
    buttonPanel.add(btnViewFilter);
    btnUseFilter.setSize();
    buttonPanel.add(btnUseFilter);
    //inversePanel
    inversePanel.add(ltfMaximumInverse);
    inversePanel.add(ltfInverseRolloffRadiusSigma);
    //mtfFilePanel
    mtfFilePanel.add(ltfMtfFile);
    mtfFilePanel.add(btnMtfFile);
    //inverseParamsPanel
    inverseParamsPanel.add(mtfFilePanel);
    inverseParamsPanel.add(inversePanel);
    //filterBodyPanel
    filterBodyPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    filterBodyPanel.add(ltfStartingAndEndingZ.getContainer());
    filterBodyPanel.add(ltfLowPassRadiusSigma.getContainer());
    filterBodyPanel.add(inverseParamsPanel.getContainer());
    filterBodyPanel.add(buttonPanel.getContainer());
    //filterPanel
    filterPanel.add(filterHeader.getContainer());
    filterPanel.add(filterBodyPanel);
    //configure
    btnFilter.setSize();
    btnViewFilter.setSize();
    btnUseFilter.setSize();
    return filterPanel;
  }

  /**
   * Layout the tilt panel
   */
  private Container layoutTiltPanel() {
    //fields
    ConstEtomoNumber maxCPUs = CpuAdoc.getInstance(axisID, applicationManager.getPropertyUserDir())
        .getMaxTilt();
    if (maxCPUs != null && !maxCPUs.isNull()) {
      cbParallelProcess = new CheckBox(ParallelPanel.FIELD_LABEL
          + ParallelPanel.MAX_CPUS_STRING + maxCPUs.toString());
    }
    else {
      cbParallelProcess = new CheckBox(ParallelPanel.FIELD_LABEL);
    }
    //panels
    JPanel tiltPanel = new JPanel();
    tiltPanel.setLayout(new BoxLayout(tiltPanel, BoxLayout.Y_AXIS));
    tiltPanel.setBorder(BorderFactory.createEtchedBorder());
    tiltBodyPanel = new JPanel();
    tiltBodyPanel.setLayout(new BoxLayout(tiltBodyPanel, BoxLayout.Y_AXIS));
    JPanel densityPanel = new JPanel();
    densityPanel.setLayout(new BoxLayout(densityPanel, BoxLayout.X_AXIS));
    JPanel slicesInYPanel = new JPanel();
    slicesInYPanel.setLayout(new BoxLayout(slicesInYPanel, BoxLayout.X_AXIS));
    JPanel offsetPanel = new JPanel();
    offsetPanel.setLayout(new BoxLayout(offsetPanel, BoxLayout.X_AXIS));
    JPanel radialPanel = new JPanel();
    radialPanel.setLayout(new BoxLayout(radialPanel, BoxLayout.X_AXIS));
    JPanel checkBoxPanel = new JPanel();
    checkBoxPanel.setLayout(new BoxLayout(checkBoxPanel, BoxLayout.Y_AXIS));
    SpacedPanel trialPanel = new SpacedPanel();
    trialPanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel buttonPanel = new SpacedPanel(true);
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    tiltHeader = PanelHeader.getAdvancedBasicInstance("Tilt", this, dialogType);
    //buttonPanel
    btnTilt.setSize();
    buttonPanel.add(btnTilt);
    btn3dmodTomogram.setSize();
    buttonPanel.add(btn3dmodTomogram);
    btnDeleteStack.setSize();
    buttonPanel.add(btnDeleteStack);
    buttonPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //trialPanel
    trialPanel.add(layoutTrialPanel());
    trialPanel.alignComponentsX(Component.LEFT_ALIGNMENT);
    //checkBoxPanel
    checkBoxPanel.add(cbUseLocalAlignment);
    checkBoxPanel.add(cbUseZFactors);
    UIUtilities.alignComponentsX(checkBoxPanel, Component.LEFT_ALIGNMENT);
    //radialPanel
    radialPanel.add(ltfRadialMax.getContainer());
    radialPanel.add(ltfRadialFallOff.getContainer());
    UIUtilities.alignComponentsX(radialPanel, Component.LEFT_ALIGNMENT);
    //offsetPanel
    offsetPanel.add(ltfXShift.getContainer());
    offsetPanel.add(ltfZShift.getContainer());
    UIUtilities.alignComponentsX(offsetPanel, Component.LEFT_ALIGNMENT);
    //slicesInYPanel
    slicesInYPanel.add(ltfSliceStart.getContainer());
    slicesInYPanel.add(ltfSliceStop.getContainer());
    slicesInYPanel.add(lblInY.getContainer());
    UIUtilities.alignComponentsX(slicesInYPanel, Component.LEFT_ALIGNMENT);
    //densityPanel
    densityPanel.add(ltfDensityScale.getContainer());
    densityPanel.add(ltfDensityOffset.getContainer());
    UIUtilities.alignComponentsX(densityPanel, Component.LEFT_ALIGNMENT);
    //tiltBodyPanel
    tiltBodyPanel.add(Box.createRigidArea(FixedDim.x0_y5));
    tiltBodyPanel.add(cbParallelProcess);
    tiltBodyPanel.add(ltfLogOffset.getContainer());
    tiltBodyPanel.add(densityPanel);
    tiltBodyPanel.add(ltfTomoWidth.getContainer());
    tiltBodyPanel.add(ltfTomoThickness.getContainer());
    tiltBodyPanel.add(slicesInYPanel);
    tiltBodyPanel.add(ltfSliceIncr.getContainer());
    tiltBodyPanel.add(offsetPanel);
    tiltBodyPanel.add(ltfXAxisTilt.getContainer());
    tiltBodyPanel.add(ltfTiltAngleOffset.getContainer());
    tiltBodyPanel.add(radialPanel);
    tiltBodyPanel.add(ltfExtraExcludeList.getContainer());
    tiltBodyPanel.add(checkBoxPanel);
    tiltBodyPanel.add(trialPanel.getContainer());
    tiltBodyPanel.add(buttonPanel.getContainer());
    UIUtilities.alignComponentsX(tiltBodyPanel, Component.LEFT_ALIGNMENT);
    //tiltPanel
    tiltPanel.add(tiltHeader.getContainer());
    tiltPanel.add(tiltBodyPanel);
    UIUtilities.alignComponentsX(tiltPanel, Component.LEFT_ALIGNMENT);
    //configure
    btnTilt.setSize();
    btn3dmodTomogram.setSize();
    btnDeleteStack.setSize();
    return tiltPanel;
  }

  /**
   * Layout the trial tomogram panel
   */
  private JPanel layoutTrialPanel() {
    //panels
    trialPanel = new JPanel();
    trialPanel.setLayout(new BoxLayout(trialPanel, BoxLayout.Y_AXIS));
    trialPanel.setBorder(BorderFactory.createEtchedBorder());
    trialBodyPanel = new SpacedPanel();
    trialBodyPanel.setBoxLayout(BoxLayout.Y_AXIS);
    SpacedPanel northPanel = new SpacedPanel();
    northPanel.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel buttonPanel = new SpacedPanel();
    buttonPanel.setBoxLayout(BoxLayout.X_AXIS);
    //header
    trialHeader = PanelHeader.getInstance("Trial Tilt", this, dialogType);
    //buttonPanel
    btnTrial.setSize();
    buttonPanel.add(btnTrial);
    btn3dmodTrial.setSize();
    buttonPanel.add(btn3dmodTrial);
    btnUseTrial.setSize();
    buttonPanel.add(btnUseTrial);
    //northPanel
    northPanel.add(lblTrialTomogramName);
    northPanel.add(cmboTrialTomogramName);
    //trialBodyPanel
    trialBodyPanel.addRigidArea();
    trialBodyPanel.add(northPanel);
    trialBodyPanel.add(buttonPanel);
    //trialPanel
    trialPanel.add(trialHeader.getContainer());
    trialPanel.add(trialBodyPanel.getContainer());
    //configure
    //trialHeader.setOpen(true);
    cmboTrialTomogramName.setEditable(true);
    return trialPanel;
  }

  protected void btnMtfFileAction(ActionEvent event) {
    //Open up the file chooser in the $IMOD_CALIB_DIR/Camera, if available,
    //otherwise open in the working directory
    String currentMtfDirectory = ltfMtfFile.getText();
    if (currentMtfDirectory.equals("")) {
      File calibrationDir = EtomoDirector.INSTANCE.getIMODCalibDirectory();
      File cameraDir = new File(calibrationDir.getAbsolutePath(), "Camera");
      if (cameraDir.exists()) {
        currentMtfDirectory = cameraDir.getAbsolutePath();
      }
      else {
        currentMtfDirectory = applicationManager.getPropertyUserDir();
      }
    }
    JFileChooser chooser = new JFileChooser(new File(currentMtfDirectory));
    MtfFileFilter mtfFileFilter = new MtfFileFilter();
    chooser.setFileFilter(mtfFileFilter);
    chooser.setPreferredSize(new Dimension(400, 400));
    chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File mtfFile = chooser.getSelectedFile();
      try {
        ltfMtfFile.setText(mtfFile.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String alignManpageLabel;
    String alignManpage;
    String alignLogfileLabel;
    String alignLogfile;
    if (applicationManager.getMetaData().getViewType() == ViewType.MONTAGE) {
      alignManpageLabel = "Blendmont";
      alignManpage = "blendmont";
      alignLogfileLabel = "Blend";
      alignLogfile = "blend";
    }
    else {
      alignManpageLabel = "Newstack";
      alignManpage = "newstack";
      alignLogfileLabel = "Newst";
      alignLogfile = "newst";
    }
    String[] manPagelabel = { alignManpageLabel, "Tilt", "3dmod" };
    String[] manPage = { alignManpage + ".html", "tilt.html", "3dmod.html" };
    String[] logFileLabel = { alignLogfileLabel, "Tilt" };
    String[] logFile = new String[2];
    logFile[0] = alignLogfile + axisID.getExtension() + ".log";
    logFile[1] = "tilt" + axisID.getExtension() + ".log";
    ContextPopup contextPopup = new ContextPopup(rootPanel, mouseEvent,
        "TOMOGRAM GENERATION", ContextPopup.TOMO_GUIDE, manPagelabel, manPage,
        logFileLabel, logFile, applicationManager, axisID);
  }

  public void startingAndEndingZKeyReleased(KeyEvent event) {
    expert.enableUseFilter();
  }

  public void setUseZFactors(boolean select) {
    cbUseZFactors.setSelected(select);
  }

  public void setUseZFactorsEnabled(boolean enable) {
    cbUseZFactors.setEnabled(enable);
  }

  public void setUseLocalAlignment(boolean select) {
    cbUseLocalAlignment.setSelected(select);
  }

  public void setUseLocalAlignmentEnabled(boolean enable) {
    cbUseLocalAlignment.setEnabled(enable);
  }

  protected void updateFiducialess() {
    ltfRotation.setEnabled(cbFiducialess.isSelected());
  }

  boolean done() {
    if (expert.doneDialog()) {
      btnNewst.removeActionListener(tomogramGenerationListener);
      btnTilt.removeActionListener(tomogramGenerationListener);
      btnDeleteStack.removeActionListener(tomogramGenerationListener);
      btnUseFilter.removeActionListener(tomogramGenerationListener);
      btnUseTrial.removeActionListener(tomogramGenerationListener);
      btnFilter.removeActionListener(tomogramGenerationListener);
      setDisplayed(false);
      return true;
    }
    return false;
  }

  public void buttonAdvancedAction(ActionEvent event) {
    super.buttonAdvancedAction(event);
    updateAdvanced();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  /**
   * Executes the action associated with command.  Deferred3dmodButton is null
   * if it comes from the dialog's ActionListener.  Otherwise is comes from a
   * Run3dmodButton which called action(Run3dmodButton, Run3dmoMenuOptions).  In
   * that case it will be null unless it was set in the Run3dmodButton.
   * @param command
   * @param deferred3dmodButton
   * @param run3dmodMenuOptions
   */
  void buttonAction(final String command,
      final Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnNewst.getActionCommand())) {
      expert.newst(btnNewst, null, deferred3dmodButton, run3dmodMenuOptions);
    }
    else if (command.equals(btnFilter.getActionCommand())) {
      expert.mtffilter(btnFilter, null, deferred3dmodButton,
          run3dmodMenuOptions);
    }
    else if (command.equals(btnUseFilter.getActionCommand())) {
      expert.useMtfFilter(btnUseFilter);
    }
    else if (command.equals(btnTrial.getActionCommand())) {
      expert.trialAction(btnTrial, null);
    }
    else if (command.equals(btnUseTrial.getActionCommand())) {
      expert.commitTestVolume(btnUseTrial);
    }
    else if (command.equals(btnTilt.getActionCommand())) {
      expert
          .tiltAction(btnTilt, null, deferred3dmodButton, run3dmodMenuOptions);
    }
    else if (command.equals(btnDeleteStack.getActionCommand())) {
      applicationManager.deleteAlignedStacks(axisID, btnDeleteStack);
    }
    else if (command.equals(cbFiducialess.getActionCommand())) {
      updateFiducialess();
    }
    else if (command.equals(cbParallelProcess.getActionCommand())) {
      expert.updateParallelProcess();
    }
    else if (command.equals(btn3dmodFull.getActionCommand())) {
      applicationManager.imodFineAlign(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btn3dmodTrial.getActionCommand())) {
      expert.imodTestVolume(run3dmodMenuOptions);
    }
    else if (command.equals(btn3dmodTomogram.getActionCommand())) {
      applicationManager.imodFullVolume(axisID, run3dmodMenuOptions);
    }
    else if (command.equals(btnViewFilter.getActionCommand())) {
      applicationManager.imodMTFFilter(axisID, run3dmodMenuOptions);
    }
  }

  private class ButtonListener implements ActionListener {
    private final TomogramGenerationDialog adaptee;

    private ButtonListener(final TomogramGenerationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(),null, null);
    }
  }

  private class MtfFileActionListener implements ActionListener {
    TomogramGenerationDialog adaptee;

    MtfFileActionListener(TomogramGenerationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.btnMtfFileAction(event);
    }
  }

  class StartingAndEndingZKeyListener implements KeyListener {
    TomogramGenerationDialog adaptee;

    public StartingAndEndingZKeyListener(TomogramGenerationDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void keyReleased(KeyEvent event) {
      adaptee.startingAndEndingZKeyReleased(event);
    }

    public void keyPressed(KeyEvent event) {
    }

    public void keyTyped(KeyEvent event) {
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    ReadOnlyAutodoc autodoc = null;

    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.MTF_FILTER, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    cbParallelProcess
        .setToolTipText("Check to distribute the tilt process across multiple computers.");
    cbUseLinearInterpolation
        .setToolTipText("Make aligned stack with linear instead of cubic interpolation to "
            + "reduce noise.");
    btnNewst
        .setToolTipText("Generate the complete aligned stack for input into the tilt process."
            + "  This runs the newst.com script.");
    btn3dmodFull.setToolTipText("Open the complete aligned stack in 3dmod");
    if (autodoc != null) {
      ltfStartingAndEndingZ.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          "StartingAndEndingZ"));
      ltfLowPassRadiusSigma.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          "LowPassRadiusSigma"));
      String text = EtomoAutodoc.getTooltip(autodoc, "MtfFile");
      if (text != null) {
        ltfMtfFile.setToolTipText(text);
        btnMtfFile.setToolTipText(text);
      }
      ltfMaximumInverse.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          "MaximumInverse"));
      ltfInverseRolloffRadiusSigma.setToolTipText(EtomoAutodoc.getTooltip(
          autodoc, "InverseRolloffRadiusSigma"));
    }
    btnFilter.setToolTipText("Run mtffilter on the full aligned stack.");
    btnViewFilter
        .setToolTipText("View the results of running mtffilter on the full aligned stack.");
    btnUseFilter
        .setToolTipText("Use the results of running mtffilter as the new full aligned stack.");
    ltfTomoThickness
        .setToolTipText("Thickness, in pixels, along the z-axis of the reconstructed volume.");
    ltfSliceStart
        .setToolTipText("The first slice in the Y dimension to include in the reconstructed "
            + " volume.  Slices are numbered from 0, a last slice must also "
            + "be specified.");
    ltfSliceStop
        .setToolTipText("The last slice in the Y dimension to include in the reconstructed "
            + " volume.  Slices are numbered from 0, a first slice must also "
            + "be specified.");
    ltfSliceIncr
        .setToolTipText("Step between slices in the Y dimension.  A first and last slice must "
            + "also be entered. Default is 1.");
    ltfTomoWidth
        .setToolTipText("This entry specifies the width of the output image; the default is the "
            + "width of the input image.");
    ltfXShift
        .setToolTipText("Amount to shift the reconstructed slices in X before output.  A "
            + "positive value will shift the slice to the right, and the "
            + "output will contain the left part of the whole potentially "
            + "reconstructable area.");
    ltfZShift
        .setToolTipText("Amount to shift the reconstructed slices in Z before output.  A "
            + "positive value will shift the slice upward.");
    ltfXAxisTilt.setToolTipText(X_AXIS_TILT_TOOLTIP);
    ltfTiltAngleOffset
        .setToolTipText("Offset in degrees to apply to the tilt angles; a positive offset will "
            + "rotate the reconstructed slices counterclockwise.");
    ltfRadialMax
        .setToolTipText("The spatial frequency at which to switch from the R-weighted radial "
            + "filter to a Gaussian falloff.  Frequency is in cycles/pixel and "
            + "ranges from 0-0.5.  Both a cutoff and a falloff must be entered.");
    ltfRadialFallOff
        .setToolTipText("The sigma value of a Gaussian which determines how fast the radial "
            + "filter falls off at spatial frequencies above the cutoff frequency."
            + "  Frequency is in cycles/pixel and ranges from 0-0.5.  Both a "
            + "cutoff and a falloff must be entered ");
    ltfDensityOffset
        .setToolTipText("Amount to add to reconstructed density values before multiplying by"
            + " the scale factor and outputting the values.");
    ltfDensityScale
        .setToolTipText("Amount to multiply reconstructed density values by, after adding the "
            + "offset value.");
    ltfLogOffset
        .setToolTipText("This parameter allows one to generate a reconstruction using the "
            + "logarithm of the densities in the input file, with the value "
            + "specified added before taking the logarithm.  If no parameter is "
            + "specified the logarithm of the input data is not taken.");
    cbUseLocalAlignment
        .setToolTipText("Select this checkbox to use local alignments.  You must have "
            + "created the local alignments in the Fine Alignment step");
    btnTilt
        .setToolTipText("Compute the tomogram from the full aligned stack.  This runs "
            + "the tilt.com script.");
    btn3dmodTomogram.setToolTipText("View the reconstructed volume in 3dmod.");
    String text = "Current name of trial tomogram, which will be generated, viewed, or"
        + " used by the buttons below.";
    lblTrialTomogramName.setToolTipText(text);
    cmboTrialTomogramName
        .setToolTipText(TooltipFormatter.INSTANCE.format(text));
    btnTrial
        .setToolTipText("Compute a trial tomogram with the current parameters, using the "
            + "filename in the \" Trial tomogram filename \" box.");
    btn3dmodTrial
        .setToolTipText("View the trial tomogram whose name is shown in \"Trial "
            + "tomogram filename\" box.");
    btnUseTrial
        .setToolTipText("Rename the trial tomogram whose name is shown in the \"Trial "
            + "tomogram filename\" box to be the final tomogram.");
    btnDeleteStack
        .setToolTipText("Delete the aligned stack for this axis.  Once the "
            + "tomogram is calculated this intermediate file is not used and can be "
            + "" + "deleted to free up disk space.");
    cbFiducialess.setToolTipText("Use cross-correlation alignment only.");
    ltfRotation
        .setToolTipText("Rotation angle of tilt axis for generating aligned stack from "
            + "cross-correlation alignment only.");
    spinBinning
        .setToolTipText("Set the binning for the aligned image stack and tomogram.  With a "
            + "binned tomogram, all of the thickness, position, and size parameters"
            + " below are still entered in unbinned pixels.");
    cbUseZFactors
        .setToolTipText("Use the file containing factors for adjusting the backprojection position "
            + "in each image as a function of Z height in the output slice (.zfac file).  "
            + "These factors are necessary when input images have been transformed to "
            + "correct for an apparent specimen stretch.  "
            + "If this box is not checked, "
            + "Z factors in a local alignment file will not be applied.");
    ltfExtraExcludeList
        .setToolTipText("List of views to exclude from the reconstruction, in addition to the ones"
            + "excluded from fine alignment.");
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.NEWSTACK, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.ReadException e) {
      e.printStackTrace();
    }
    if (autodoc != null) {
      ltfSizeToOutputInXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          NewstParam.SIZE_TO_OUTPUT_IN_X_AND_Y));
    }
  }
}