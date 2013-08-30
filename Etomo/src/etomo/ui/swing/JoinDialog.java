package etomo.ui.swing;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.AutoAlignmentController;
import etomo.JoinManager;
import etomo.comscript.FinishjoinParam;
import etomo.comscript.MidasParam;
import etomo.comscript.XfalignParam;
import etomo.comscript.XfjointomoParam;
import etomo.logic.DatasetTool;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.storage.LogFile;
import etomo.storage.ModelFileFilter;
import etomo.storage.ParameterStore;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstJoinMetaData;
import etomo.type.DataFileType;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.FileType;
import etomo.type.JoinMetaData;
import etomo.type.JoinScreenState;
import etomo.type.JoinState;
import etomo.type.NullRequiredNumberException;
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.AutoAlignmentDisplay;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;
import etomo.util.DatasetFiles;
import etomo.util.Utilities;

/**
 * <p>Description: The dialog box for creating the fiducial model(s).</p>
 *
 * <p>Copyright: Copyright (c) 2004</p>
 *
 * <p>Organization: Boulder Laboratory for 3D Fine Structure,
 * University of Colorado</p>
 *
 * @author $Author$
 *
 * @version $Revision$
 *
 * <p> $Log$
 * <p> Revision 1.6  2011/07/19 20:01:14  sueh
 * <p> Bug# 1459 Wrapped checkboxes in a panel and used glue to left justify them.  Prevented spinners
 * <p> which have a value when they are first displayed from going all the way to the right.
 * <p>
 * <p> Revision 1.5  2011/06/21 18:07:49  sueh
 * <p> Bug# 1490 In CheckBoxSpinner, changed spinner to a Spinner type (from JSpinner), so it will work with
 * <p> the UITests.
 * <p>
 * <p> Revision 1.4  2011/05/05 01:29:29  sueh
 * <p> bug# 1396  Popping up an error message and failing when the dataset directory ends in a space.
 * <p>
 * <p> Revision 1.3  2011/04/22 02:16:45  sueh
 * <p> bug# 1474 Handling NullRequiredNumberException in action() when setSizeAndShift is called.
 * <p>
 * <p> Revision 1.2  2011/02/22 18:13:21  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.73  2010/04/28 16:41:55  sueh
 * <p> bug# 1344 In startRefine calling closeImod with FileType.
 * <p>
 * <p> Revision 1.72  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 1.71  2009/11/20 17:26:41  sueh
 * <p> bug# 1282 Naming all the file choosers by constructing a FileChooser
 * <p> instance instead of a JFileChooser instance.  Added isMenuSaveEnabled to
 * <p> allow a save function to have the same limits as the save menu option.
 * <p>
 * <p> Revision 1.70  2009/09/20 21:32:41  sueh
 * <p> bug# 1268 Added timestamp and dialog identification to log.
 * <p>
 * <p> Revision 1.69  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 1.68  2009/06/05 02:13:25  sueh
 * <p> bug# 1219 Reformatted.
 * <p>
 * <p> Revision 1.67  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 1.66  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 1.65  2009/01/20 20:11:51  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 1.64  2008/12/10 21:56:38  sueh
 * <p> bug# 1166 Stopped calling LabeledSpinner.setTextMaxmimumSize
 * <p>
 * <p> Revision 1.63  2008/11/20 01:46:09  sueh
 * <p> bug# 1147 Added AxisID to manager.imodOpen.
 * <p>
 * <p> Revision 1.62  2008/09/30 21:57:11  sueh
 * <p> bug# 1113 Added getFocusComponent.  Made components to be
 * <p> focused on final.
 * <p>
 * <p> Revision 1.61  2008/08/18 22:39:44  sueh
 * <p> bug# 1130 Added cbLocalFits to the Join tab.  Written to / retrieved from
 * <p> JoinMetaData.
 * <p>
 * <p> Revision 1.60  2008/06/24 20:09:42  sueh
 * <p> bug# 1102 Changed tabPane from JTabbedPane to TabbedPane, so it can
 * <p> name itself.
 * <p>
 * <p> Revision 1.59  2008/06/20 20:06:19  sueh
 * <p> Removed an old setDebug(true) call.
 * <p>
 * <p> Revision 1.58  2008/05/30 21:30:55  sueh
 * <p> bug# 1102 Changed the tabbed pane to TabbedPane so that it can self-
 * <p> name.
 * <p>
 * <p> Revision 1.57  2008/05/28 02:50:10  sueh
 * <p> bug# 1111 Add a dialogType parameter to the ProcessSeries
 * <p> constructor.  DialogType must be passed to any function that constructs
 * <p> a ProcessSeries instance.
 * <p>
 * <p> Revision 1.56  2008/05/13 23:02:23  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 1.55  2008/05/03 00:50:14  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 1.54  2008/01/31 20:26:10  sueh
 * <p> bug# 1055 throwing a FileException when LogFile.getInstance fails.
 * <p>
 * <p> Revision 1.53  2007/12/10 22:42:49  sueh
 * <p> bug# 1041 Standardized JoinMetaData.getRootName to getDatasetName.
 * <p>
 * <p> Revision 1.52  2007/07/30 18:54:06  sueh
 * <p> bug# 1002 ParameterStore.getInstance can return null - handle it.
 * <p>
 * <p> Revision 1.51  2007/06/08 23:58:28  sueh
 * <p> bug# 995 Opening the rejoin trial without the model unless it was created
 * <p> with all slices.
 * <p>
 * <p> Revision 1.50  2007/05/26 00:32:31  sueh
 * <p> bug# 994 Not automatically setting button size in SpacedPanel anymore.
 * <p> Setting button size in UI.
 * <p>
 * <p> Revision 1.49  2007/05/01 22:27:51  sueh
 * <p> bug# 964 Added yaxisType and yaxisContour.
 * <p>
 * <p> Revision 1.48  2007/03/30 23:50:50  sueh
 * <p> bug# 964 In FileTextField, changed setEnabled to setEditable and then added a new
 * <p> setEnabled.
 * <p>
 * <p> Revision 1.47  2007/03/21 19:45:54  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 1.46  2007/03/09 22:05:54  sueh
 * <p> bug# 964 Reduced visibility on changeTab().
 * <p>
 * <p> Revision 1.45  2007/03/07 21:11:28  sueh
 * <p> bug# 981 Turned RadioButton into a wrapper rather then a child of JRadioButton,
 * <p> because it is getting more complicated.
 * <p>
 * <p> Revision 1.44  2007/03/01 01:38:38  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 1.43  2007/02/22 20:37:54  sueh
 * <p> bug# 964 Moved FileTextField from JoinDialog to the etomo.ui package so that it
 * <p> can be shared.
 * <p>
 * <p> Revision 1.42  2007/02/09 00:50:04  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.  Added tooltips for model and rejoin tabs.
 * <p>
 * <p> Revision 1.41  2007/02/08 02:05:55  sueh
 * <p> bug# 962 Added trial rejoin and removed a bug which made enabling the rejoin
 * <p> button dependent on running the optional view-tranformed-model button.
 * <p>
 * <p> Revision 1.40  2007/02/05 23:39:03  sueh
 * <p> bug# 962 Added model and rejoin tabs.
 * <p>
 * <p> Revision 1.39  2006/11/15 21:06:34  sueh
 * <p> bug# 872 In action(), using ParameterStore to save meta data.
 * <p>
 * <p> Revision 1.38  2006/11/07 22:43:36  sueh
 * <p> bug# 954 Adding tooltip to the second label in the spinners.
 * <p>
 * <p> Revision 1.37  2006/10/25 15:51:29  sueh
 * <p> bug# 950 The section table setMetaData function calls the function that sets the
 * <p> default join size.  So load the join size from metaData after the section table
 * <p> setMetaData function has been called.
 * <p>
 * <p> Revision 1.36  2006/10/16 22:52:19  sueh
 * <p> bug# 919  Added setInverted().
 * <p>
 * <p> Revision 1.35  2006/07/21 19:03:07  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 1.34  2006/07/20 17:20:27  sueh
 * <p> bug# 848 Made UIParameters a singleton.
 * <p>
 * <p> Revision 1.33  2006/06/29 20:08:35  sueh
 * <p> bug# 880 Made working dir and root name editable/not editable instead of using
 * <p> setEnabled() so that they would be readable.
 * <p>
 * <p> Revision 1.32  2006/06/21 15:53:44  sueh
 * <p> bug# 581 Passing axis to ContextPopup, so that imodqtassist can be run.
 * <p>
 * <p> Revision 1.31  2006/04/06 20:16:45  sueh
 * <p> bug# 808 Added setRevertState(boolean) to do all work associated with
 * <p> enabling and disabling revert in Setup.
 * <p>
 * <p> Revision 1.30  2006/01/27 18:42:31  sueh
 * <p> bug# 801 Added validation for makejoin and finishjoin
 * <p>
 * <p> Revision 1.29  2006/01/03 23:38:19  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.  Converted JRadioButton's
 * <p> toRadioButton.
 * <p>
 * <p> Revision 1.28  2005/12/16 18:26:46  sueh
 * <p> bug# 785 Added getMode().
 * <p>
 * <p> Revision 1.27  2005/12/16 01:45:43  sueh
 * <p> bug# 784 Added tool tips.
 * <p>
 * <p> Revision 1.26  2005/12/14 01:31:59  sueh
 * <p> bug# 782 Added toString().  Bug# 783 Added defaultXYSize() and init().
 * <p> Calling default xy size when going to the join tab.  Function has no
 * <p> effect when the default size is not changed.  Added isSetupTab(), etc, so
 * <p> that there is only one instance of curTab.
 * <p>
 * <p> Revision 1.25  2005/12/06 23:02:46  sueh
 * <p> bug# 757 Changed synchronized(int).  It is called when changing tabs and
 * <p> should not call setNumSections.
 * <p>
 * <p> Revision 1.24  2005/11/30 21:17:28  sueh
 * <p> bug# 757 Not trying to control where XMax, YMax, and ZMax come from.
 * <p> The section table can decide where to get them, depending on which tab
 * <p> is displayed.
 * <p>
 * <p> Revision 1.23  2005/11/29 22:49:19  sueh
 * <p> bug# 757 Listeners where firing while things where being built so moved
 * <p> all add listener calls to addListeners and calling it after everything is built.
 * <p> Added synchronize(int prevTab) to move data to/from the setup and join
 * <p> tabs when changing tabs.  Added synchronize() to move data from the
 * <p> join tab to the setup tab before saving.
 * <p>
 * <p> Revision 1.22  2005/11/14 22:12:57  sueh
 * <p> bug# 762 Made action() protected.
 * <p>
 * <p> Revision 1.21  2005/11/02 23:59:53  sueh
 * <p> bug# 738 Added midas limit.
 * <p>
 * <p> Revision 1.20  2005/09/29 19:10:07  sueh
 * <p> bug# 532 Preventing Etomo from saving to the .edf or .ejf file over and
 * <p> over during exit.  Added BaseManager.exiting and
 * <p> saveIntermediateParamFile(), which will not save when exiting it true.
 * <p> Setting exiting to true in BaseManager.exitProgram().  Moved call to
 * <p> saveParamFile() to the child exitProgram functions so that the param file
 * <p> is saved after all the done functions are run.
 * <p>
 * <p> Revision 1.19  2005/08/11 23:56:49  sueh
 * <p> bug# 711  Change 3dmod buttons to Run3dmodButton.  Implement
 * <p> Run3dmodButtonContainer.  Change enum Run3dmodMenuOption to
 * <p> Run3dmodMenuOptions, which can turn on multiple options at once.
 * <p> This allows ImodState to combine input from the context menu and the
 * <p> pulldown menu.  Get rid of duplicate code by running the 3dmods from a
 * <p> private function called run3dmod(String, Run3dmodMenuOptions).  It can
 * <p> be called from run3dmod(Run3dmodButton, Run3dmodMenuOptions) and
 * <p> the action function.
 * <p>
 * <p> Revision 1.18  2005/08/09 20:24:07  sueh
 * <p> bug# 711 Moving button sizing from UIUtilities to the multi line button
 * <p> classes.  default setSize() sets the standard button dimension.
 * <p>
 * <p> Revision 1.17  2005/08/04 20:11:40  sueh
 * <p> bug# 532  Centralizing fit window functionality by placing fitting functions
 * <p> in UIHarness.  Removing packMainWindow from the manager.  Sending
 * <p> the manager to UIHarness.pack() so that packDialogs() can be called.
 * <p>
 * <p> Revision 1.16  2005/07/29 19:47:42  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 1.15  2005/07/29 00:54:10  sueh
 * <p> bug# 709 Going to EtomoDirector to get the current manager is unreliable
 * <p> because the current manager changes when the user changes the tab.
 * <p> Passing the manager where its needed.
 * <p>
 * <p> Revision 1.14  2005/07/06 23:36:29  sueh
 * <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
 * <p> their functionality in SpacedPanel.  Simplified the construction of
 * <p> SpacedPanel.
 * <p>
 * <p> Revision 1.13  2005/04/26 17:39:37  sueh
 * <p> bug# 615 Made MainFrame a package-level class.  All MainFrame
 * <p> functionality is handled through UIHarness to make Etomo more
 * <p> compatible with JUnit.
 * <p>
 * <p> Revision 1.12  2005/04/25 21:06:22  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 1.11  2005/04/21 20:34:48  sueh
 * <p> bug# 615 Pass axisID to packMainWindow so it can pack only the frame
 * <p> that requires it.  Moved fitWindow from MainPanel to EtomoFrame.
 * <p>
 * <p> Revision 1.10  2005/01/29 00:18:22  sueh
 * <p> Removed print statements
 * <p>
 * <p> Revision 1.9  2005/01/26 00:05:02  sueh
 * <p> Removing ConstEtomoNumber.displayDefault.  To get the default to
 * <p> display, set displayValue and default the same when creating the
 * <p> variable.  Removed script oriented functionality from EtomoNumber.
 * <p>
 * <p> Revision 1.8  2005/01/21 23:44:04  sueh
 * <p> bug# 509 bug# 591  Added isUpdateCommand() in place of
 * <p> isSetAndNotDefault() and isSet() as a standard why to decide if a
 * <p> parameter should be placed in a comscript.
 * <p>
 * <p> Revision 1.7  2005/01/10 23:54:56  sueh
 * <p> bug# 578 Changing calls to ConstEtomoNumber.isNull() to !isSet().
 * <p>
 * <p> Revision 1.6  2004/12/14 21:50:40  sueh
 * <p> bug# 572:  Removing state object from meta data and managing it with a
 * <p> manager class.  Bug# 565:  Save all of .ejf each time a save is done.
 * <p>
 * <p> Revision 1.5  2004/12/04 01:00:50  sueh
 * <p> bug# 569 Fixed the check to see if working directory is empty in isValid()
 * <p>
 * <p> Revision 1.4  2004/12/02 20:41:01  sueh
 * <p> bug# 566 Move mouse listener to the tab pane.  Filled in
 * <p> popUpContextMenu with separate menus for each tab.
 * <p>
 * <p> Revision 1.3  2004/11/23 22:34:04  sueh
 * <p> bug# 520 getMetaData() returning a success boolean.  On Change Setup:
 * <p> save screen to metaData and save metaData.
 * <p>
 * <p> Revision 1.2  2004/11/19 23:56:33  sueh
 * <p> bug# 520 merging Etomo_3-4-6_JOIN branch to head.
 * <p>
 * <p> Revision 1.1.2.34  2004/11/19 03:05:50  sueh
 * <p> bug# 520 Removed useDefault statics.  The is handled by
 * <p> ConstJoinMetaData, ConstEtomoNumber.displayDefault.
 * <p>
 * <p> Revision 1.1.2.33  2004/11/19 00:20:52  sueh
 * <p> bug# 520 Added equals function to check whether the screen fields have
 * <p> changed since meta data was updated.  Added equalsSample to check
 * <p> whether the fields used to create the sample have changed.  Placed
 * <p> useDefault values in static variables because they are used in
 * <p> setMetaData, equals, and equalsSample.
 * <p>
 * <p> Revision 1.1.2.32  2004/11/17 02:27:27  sueh
 * <p> bug# 520 Changed mode setting names to end in "_MODE".  In Revert to
 * <p> Last Setup, deleting sections before getting data from meta data.
 * <p>
 * <p> Revision 1.1.2.31  2004/11/16 02:28:50  sueh
 * <p> bug# 520 Replacing EtomoSimpleType, EtomoInteger, EtomoDouble,
 * <p> EtomoFloat, and EtomoLong with EtomoNumber.
 * <p>
 * <p> Revision 1.1.2.30  2004/11/15 22:25:32  sueh
 * <p> bug# 520 Added setMode().  Moved enabling and disabling to setMode().
 * <p> Implemented ChangeSetup.
 * <p>
 * <p> Revision 1.1.2.29  2004/11/13 02:39:10  sueh
 * <p> bug# 520 Added new buttons Change Setup and Revert to Last Setup.
 * <p>
 * <p> Revision 1.1.2.28  2004/11/12 23:00:36  sueh
 * <p> bug# 520 Added setSizeAndShift() to implement get subarea.
 * <p>
 * <p> Revision 1.1.2.27  2004/11/11 01:42:23  sueh
 * <p> bug# 520 Changed useEveryNSections to useEveryNSlices (sampling rate
 * <p> in Z).  Prevented null values from getting into useEveryNSlices and
 * <p> trialBinning.  Changed setNumSections to preserve user set values in
 * <p> spinners densityRefSection, AlignmentRefSection, and useEveryNSlices.
 * <p>
 * <p> Revision 1.1.2.26  2004/11/09 15:45:11  sueh
 * <p> bug# 520 Removed member variables that are unnecessary because they
 * <p> are only used in one function.  Added createOpen3dmodPanel() to create
 * <p> all open 3dmod with binning panels in the join dialogs.  Changed Every N
 * <p> Sections to a spinner.
 * <p>
 * <p> Revision 1.1.2.25  2004/11/08 22:26:15  sueh
 * <p> Bug# 520 On Join tab:  Moved finish join functionality to the left of the
 * <p> table by changing the orientation of pnlJoin.  Moved Finish Join button and
 * <p> open in 3dmod button into pnlFinishJoin.
 * <p>
 * <p> Revision 1.1.2.24  2004/10/30 02:36:30  sueh
 * <p> bug# 520 Added getRootName().
 * <p>
 * <p> Revision 1.1.2.23  2004/10/29 01:20:48  sueh
 * <p> bug# 520 Removed working directory from meta data.  Getting working
 * <p> directory from constructor.
 * <p>
 * <p> Revision 1.1.2.22  2004/10/28 22:15:15  sueh
 * <p> bug# 520 Keep the text associated with the Alignment ref section
 * <p> checkbox from getting grayed out.
 * <p>
 * <p> Revision 1.1.2.21  2004/10/28 17:09:12  sueh
 * <p> bug# 520 Adding revert to empty.  Putting revert buttons in a box to the
 * <p> right.  Making button text available for message boxes.
 * <p>
 * <p> Revision 1.1.2.20  2004/10/25 23:14:03  sueh
 * <p> bug# 520 Set default size in X, Y when changing to the join tab.  Fixed
 * <p> spinners not initializing in setMetaData by setting numSections before
 * <p> initializing.
 * <p>
 * <p> Revision 1.1.2.19  2004/10/22 21:08:07  sueh
 * <p> bug# 520 Changed offsetInX, Y to shiftInX, Y.
 * <p>
 * <p> Revision 1.1.2.18  2004/10/22 03:26:45  sueh
 * <p> bug# 520 Reducing the number of ConstJoinMetaData functions by
 * <p> passing EtomoInteger, EtomoFloat, etc and using their get() and
 * <p> getString() functions.
 * <p>
 * <p> Revision 1.1.2.17  2004/10/21 02:58:46  sueh
 * <p> bug# 520 Implemented buttons, added enableMidas to be used after
 * <p> xfalign is run.
 * <p>
 * <p> Revision 1.1.2.16  2004/10/18 19:11:15  sueh
 * <p> bug# 520 Added a call to JoinManager.midasSample().
 * <p>
 * <p> Revision 1.1.2.15  2004/10/18 18:11:10  sueh
 * <p> bug# 520 Passing fields to and from meta data.  Added call to xfalign().
 * <p> Moved validation of workingDir and rootName to ConstJoinMetaData.
 * <p>
 * <p> Revision 1.1.2.14  2004/10/15 00:46:31  sueh
 * <p> bug# 520 Added setMetaData()
 * <p>
 * <p> Revision 1.1.2.13  2004/10/14 17:23:11  sueh
 * <p> bug# 520 Open sample averages.
 * <p>
 * <p> Revision 1.1.2.12  2004/10/14 03:31:38  sueh
 * <p> bug# 520 Disabled Align and Join tabs until Make Samples is run.
 * <p> Otherwise ImodManager is not initialized with meta data.  Added
 * <p> functionality for Open Samples in 3dmod button.  Added invalidReason.
 * <p> Did a validation check when loading meta data.
 * <p>
 * <p> Revision 1.1.2.11  2004/10/14 02:28:58  sueh
 * <p> bug# 520 Fixed action().  Setting working directory in join manager when
 * <p> Make Samples is pressed.
 * <p>
 * <p> Revision 1.1.2.10  2004/10/13 23:12:27  sueh
 * <p> bug# 520 Added align and join ui components.
 * <p>
 * <p> Revision 1.1.2.9  2004/10/11 02:13:46  sueh
 * <p> bug# 520 Using a variable called propertyUserDir instead of the "user.dir"
 * <p> property.  This property would need a different value for each manager.
 * <p> This variable can be retrieved from the manager if the object knows its
 * <p> manager.  Otherwise it can retrieve it from the current manager using the
 * <p> EtomoDirector singleton.  If there is no current manager, EtomoDirector
 * <p> gets the value from the "user.dir" property.
 * <p>
 * <p> Revision 1.1.2.8  2004/10/08 16:30:53  sueh
 * <p> bug# 520 Changed the name of the function retrieveData(JoinMetaData)
 * <p> to getMetaData.  Retrieve now only refers to pulling data off the screen
 * <p> and into storage owned by the ui object.  Edded getInvalidReason().
 * <p>
 * <p> Revision 1.1.2.7  2004/10/06 02:23:20  sueh
 * <p> bug# 520 Changed Make Join button to Make Samples.  Removed Use
 * <p> Density Reference Section checkbox.  Added a function to get the
 * <p> working directory File.  Added addSection(File) to control adding a
 * <p> tomogram file to the table from the outside.  Added abortAddSection() to
 * <p> signal that adding the section had failed.  These are necessary when a
 * <p> tomogram must be flipped because the signal that the flip is finished
 * <p> comes from outside.
 * <p>
 * <p> Revision 1.1.2.6  2004/10/01 19:58:55  sueh
 * <p> bug# 520 Moved working dir and root name above section table.
 * <p>
 * <p> Revision 1.1.2.5  2004/09/29 19:34:05  sueh
 * <p> bug# 520 Added retrieveData() to retrieve data from the screen.
 * <p>
 * <p> Revision 1.1.2.4  2004/09/23 23:37:46  sueh
 * <p> bug# 520 Converted to DoubleSpacedPanel and SpacedPanel.  Added
 * <p> MakeJoin panel.
 * <p>
 * <p> Revision 1.1.2.3  2004/09/21 18:00:42  sueh
 * <p> bug# 520 Moved the buttons that affected section table rows to
 * <p> SectionTablePanel.  Placed a X axis panel called pnlSetupTab behind the
 * <p> Y axis Setup panel to create rigid areas on the left and right of the Setup
 * <p> border.
 * <p>
 * <p> Revision 1.1.2.2  2004/09/15 22:40:07  sueh
 * <p> bug# 520 added create panel functions
 * <p> </p>
 */
public final class JoinDialog implements ContextMenu, Run3dmodButtonContainer,
    AutoAlignmentDisplay {
  public static final String rcsid = "$Id$";

  public static final int SETUP_MODE = -1;
  public static final int SAMPLE_NOT_PRODUCED_MODE = -2;
  public static final int SAMPLE_PRODUCED_MODE = -3;
  public static final int CHANGING_SAMPLE_MODE = -4;
  public static final DialogType DIALOG_TYPE = DialogType.JOIN;
  public static final String FINISH_JOIN_TEXT = "Finish Join";
  public static final String WORKING_DIRECTORY_TEXT = "Working directory";
  public static final String GET_MAX_SIZE_TEXT = "Get Max Size and Shift";
  public static final String TRIAL_JOIN_TEXT = "Trial Join";
  public static final String REJOIN_TEXT = "Rejoin";
  public static final String TRIAL_REJOIN_TEXT = "Trial Rejoin";
  private static final String REFINE_JOIN_TEXT = "Refine Join";
  private static final String OPEN_IN_3DMOD = "Open in 3dmod";

  private static Dimension dimSpinner = UIParameters.INSTANCE.getSpinnerDimension();

  private JPanel rootPanel;
  private TabbedPane tabPane;
  private final SpacedPanel pnlSetupTab = SpacedPanel.getFocusableInstance();
  private SectionTablePanel pnlSectionTable;
  private final SpacedPanel pnlAlignTab = SpacedPanel.getFocusableInstance();
  private final SpacedPanel pnlJoinTab = SpacedPanel.getFocusableInstance();
  private SpacedPanel setupPanel2;
  private SpacedPanel alignPanel1;
  private SpacedPanel pnlFinishJoin;
  private SpacedPanel pnlMidasLimit = SpacedPanel.getInstance();
  private FileTextField ftfWorkingDir;
  private Run3dmodButton btnOpenSample;
  private MultiLineButton btnGetMaxSize;
  private MultiLineButton btnGetSubarea;
  private MultiLineButton btnChangeSetup;
  private MultiLineButton btnRevertToLastSetup;
  private LabeledTextField ltfRootName;
  private LabeledTextField ltfSizeInX;
  private LabeledTextField ltfSizeInY;
  private LabeledTextField ltfShiftInX;
  private LabeledTextField ltfShiftInY;
  private final CheckBox cbLocalFits = new CheckBox("Do local linear fits");
  private LabeledTextField ltfMidasLimit = new LabeledTextField(FieldType.INTEGER,
      "Squeeze samples to ");
  private JLabel lblMidasLimit = new JLabel("pixels if bigger.");
  private CheckBoxSpinner cbsAlignmentRefSection;
  private LabeledSpinner spinDensityRefSection;
  private LabeledSpinner spinTrialBinning;
  private LabeledSpinner spinRejoinTrialBinning;
  private LabeledSpinner spinUseEveryNSlices;
  private LabeledSpinner spinRejoinUseEveryNSlices;
  private final AutoAlignmentPanel autoAlignmentPanel;

  // state
  private int numSections = 0;
  private Tab curTab = Tab.SETUP;
  private String invalidReason = null;

  private JoinActionListener joinActionListener = new JoinActionListener(this);
  private WorkingDirActionListener workingDirActionListener = new WorkingDirActionListener(
      this);
  private final AxisID axisID;
  private final JoinManager manager;

  private int defaultXSize = 0;
  private int defaultYSize = 0;

  private final CheckBox cbRefineWithTrial = new CheckBox("Refine with trial");
  private final MultiLineButton btnRefineJoin = new MultiLineButton(REFINE_JOIN_TEXT);
  private final Run3dmodButton btnMakeRefiningModel = Run3dmodButton.get3dmodInstance(
      "Make Refining Model", this);
  private final MultiLineButton btnXfjointomo = new MultiLineButton(
      "Find Transformations");

  private final BoundaryTable boundaryTable;
  private boolean refiningJoin = false;
  private SpacedPanel pnlTransformations = null;
  private final TransformChooserPanel tcModel =  TransformChooserPanel.getJoinModelInstance();
  private final LabeledTextField ltfBoundariesToAnalyze = new LabeledTextField(
      FieldType.INTEGER_LIST, "Boundaries to analyze: ");
  private final LabeledTextField ltfObjectsToInclude = new LabeledTextField(
      FieldType.INTEGER_LIST, "Objects to include: ");
  private final LabeledTextField ltfGapStart = new LabeledTextField(
      FieldType.FLOATING_POINT, "Start: ");
  private final LabeledTextField ltfGapEnd = new LabeledTextField(
      FieldType.FLOATING_POINT, "End: ");
  private final LabeledTextField ltfGapInc = new LabeledTextField(
      FieldType.FLOATING_POINT, "Increment: ");
  private final LabeledTextField ltfPointsToFitMin = new LabeledTextField(
      FieldType.INTEGER, "Min: ");
  private final LabeledTextField ltfPointsToFitMax = new LabeledTextField(
      FieldType.INTEGER, "Max: ");
  private JPanel pnlTables = null;
  private JPanel pnlRejoin = null;

  private LabeledTextField ltfTransformedModel;
  private final BinnedXY3dmodButton b3bOpenRejoinWithModel = new BinnedXY3dmodButton(
      "Open Rejoin with Transformed Model", this);
  private final Run3dmodButton btnTransformModel = Run3dmodButton
      .getDeferred3dmodInstance("Transform Model", this);
  private final JoinState state;
  private final MultiLineButton btnTransformAndViewModel = new MultiLineButton(
      "Transform & View Model");
  private final Run3dmodButton btnOpenSampleAverages = Run3dmodButton.get3dmodInstance(
      "Open Sample Averages in 3dmod", this);
  private final Run3dmodButton btnMakeSamples = Run3dmodButton.getDeferred3dmodInstance(
      "Make Samples", this, "averages in 3dmod");
  private final SpacedPanel pnlModelTab = SpacedPanel.getFocusableInstance(true);
  private final EtomoPanel pnlRejoinTab = new EtomoPanel();
  private final BinnedXY3dmodButton b3bOpenTrialIn3dmod = new BinnedXY3dmodButton(
      "Open Trial in 3dmod", this);
  private final Run3dmodButton btnTrialJoin = Run3dmodButton.getDeferred3dmodInstance(
      TRIAL_JOIN_TEXT, this);
  private final FileTextField ftfModelFile = new FileTextField("Model file: ");
  private final CheckBox cbGap = new CheckBox("Try gaps: ");
  private final BinnedXY3dmodButton b3bOpenIn3dmod = new BinnedXY3dmodButton(
      OPEN_IN_3DMOD, this);
  private final Run3dmodButton btnFinishJoin = Run3dmodButton.getDeferred3dmodInstance(
      FINISH_JOIN_TEXT, this);
  private final BinnedXY3dmodButton b3bOpenRejoin = new BinnedXY3dmodButton(
      "Open Rejoin in 3dmod", this);
  private final Run3dmodButton btnRejoin = Run3dmodButton.getDeferred3dmodInstance(
      REJOIN_TEXT, this);
  private final BinnedXY3dmodButton b3bOpenTrialRejoin = new BinnedXY3dmodButton(
      "Open Trial Rejoin in 3dmod", this);
  private final Run3dmodButton btnTrialRejoin = Run3dmodButton.getDeferred3dmodInstance(
      TRIAL_REJOIN_TEXT, this);

  /**
   * Create JoinDialog with workingDirName equal to the location of the .ejf
   * file.
   * @param joinManager
   * @param workingDirName
   */
  private JoinDialog(JoinManager manager, String workingDirName,
      ConstJoinMetaData metaData, JoinState state) {
    System.err.println(Utilities.getDateTimeStamp() + "\nDialog: " + DialogType.JOIN);
    this.state = state;
    axisID = AxisID.ONLY;
    this.manager = manager;
    boundaryTable = new BoundaryTable(manager, this);
    autoAlignmentPanel = AutoAlignmentPanel.getJoinInstance(manager);
    setRefiningJoin();
    createRootPanel(workingDirName);
    UIHarness.INSTANCE.pack(manager);
    setMetaData(metaData);
    setScreenState(manager.getScreenState());
    init();
    setToolTipText();
  }

  public Component getFocusComponent() {
    if (isSetupTab()) {
      return pnlSetupTab.getContainer();
    }
    if (isAlignTab()) {
      return pnlAlignTab.getContainer();
    }
    if (isJoinTab()) {
      return pnlJoinTab.getContainer();
    }
    if (isModelTab()) {
      return pnlModelTab.getContainer();
    }
    if (isRejoinTab()) {
      return pnlRejoinTab;
    }
    return null;
  }

  public void setAutoAlignmentController(
      final AutoAlignmentController autoAlignmentController) {
    autoAlignmentPanel.setController(autoAlignmentController);
  }

  public AxisID getAxisID() {
    return axisID;
  }

  public DialogType getDialogType() {
    return DialogType.JOIN;
  }

  /**
   * Create JoinDialog without an .ejf file
   * @param joinManager
   */
  public static JoinDialog getInstance(JoinManager manager, ConstJoinMetaData metaData,
      JoinState state) {
    JoinDialog instance = new JoinDialog(manager, null, metaData, state);
    instance.addListeners();
    return instance;
  }

  /**
   * Create JoinDialog with workingDirName equal to the location of the .ejf
   * file.
   * @param joinManager
   * @param workingDirName
   */
  public static JoinDialog getInstance(JoinManager manager, String workingDirName,
      ConstJoinMetaData metaData, JoinState state) {
    JoinDialog instance = new JoinDialog(manager, workingDirName, metaData, state);
    instance.addListeners();
    return instance;
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  String paramString() {
    return "ltfRootName=" + ltfRootName + ",ltfSizeInX=" + ltfSizeInX + ",\nltfSizeInY="
        + ltfSizeInY + ",ltfShiftInX=" + ltfShiftInX + ",\nltfShiftInY=" + ltfShiftInY
        + ",ltfMidasLimit=" + ltfMidasLimit + ",\nspinDensityRefSection="
        + spinDensityRefSection + ",\nspinTrialBinning=" + spinTrialBinning
        + ",\nspinUseEveryNSlices=" + spinUseEveryNSlices + ",\nnumSections"
        + numSections + ",curTab=" + curTab + ",invalidReason" + invalidReason
        + ",\naxisID=" + axisID + "," + super.toString();
  }

  JComponent getModelTabJComponent() {
    return pnlModelTab.getJPanel();
  }

  JComponent getRejoinTabJComponent() {
    return pnlRejoinTab;
  }

  JComponent getSetupTabJComponent() {
    return pnlSetupTab.getJPanel();
  }

  JComponent getAlignTabJComponent() {
    return pnlAlignTab.getJPanel();
  }

  JComponent getJoinTabJComponent() {
    return pnlJoinTab.getJPanel();
  }

  /**
   * Create the root panel.
   * @param workingDirName
   */
  private void createRootPanel(String workingDirName) {
    rootPanel = new JPanel();
    createTabPane(workingDirName);
    rootPanel.add(tabPane);
  }

  private void addListeners() {
    tabPane.addMouseListener(new GenericMouseAdapter(this));
    TabChangeListener tabChangeListener = new TabChangeListener(this);
    tabPane.addChangeListener(tabChangeListener);
    ftfWorkingDir.addActionListener(workingDirActionListener);
    btnChangeSetup.addActionListener(joinActionListener);
    btnRevertToLastSetup.addActionListener(joinActionListener);
    btnMakeSamples.addActionListener(joinActionListener);
    btnOpenSample.addActionListener(joinActionListener);
    btnOpenSampleAverages.addActionListener(joinActionListener);

    btnGetMaxSize.addActionListener(joinActionListener);
    btnFinishJoin.addActionListener(joinActionListener);
    b3bOpenIn3dmod.addActionListener(joinActionListener);
    btnTrialJoin.addActionListener(joinActionListener);
    b3bOpenTrialIn3dmod.addActionListener(joinActionListener);
    btnGetSubarea.addActionListener(joinActionListener);
    btnRefineJoin.addActionListener(joinActionListener);
    btnMakeRefiningModel.addActionListener(joinActionListener);
    btnXfjointomo.addActionListener(joinActionListener);
    btnTransformAndViewModel.addActionListener(joinActionListener);
    btnRejoin.addActionListener(joinActionListener);
    btnTrialRejoin.addActionListener(joinActionListener);
    ftfModelFile.addActionListener(new ModelFileActionListener(this));
    cbGap.addActionListener(joinActionListener);
    b3bOpenRejoin.addActionListener(joinActionListener);
    b3bOpenTrialRejoin.addActionListener(joinActionListener);
    b3bOpenRejoinWithModel.addActionListener(joinActionListener);
    btnTransformModel.addActionListener(joinActionListener);
  }

  /**
   * Create the tabbed pane.
   * @param workingDirName
   */
  private void createTabPane(String workingDirName) {
    tabPane = new TabbedPane();
    // tabPane.addMouseListener(new GenericMouseAdapter(this));
    // TabChangeListener tabChangeListener = new TabChangeListener(this);
    // tabPane.addChangeListener(tabChangeListener);
    createSetupPanel(workingDirName);
    tabPane.addTab("Setup", pnlSetupTab);
    createAlignPanel();
    tabPane.addTab("Align", pnlAlignTab);
    createJoinPanel();
    tabPane.addTab("Join", pnlJoinTab);
    createModelPanel();
    tabPane.addTab("Model", pnlModelTab);
    createRejoinPanel();
    tabPane.addTab("Rejoin", pnlRejoinTab);
    addPanelComponents(Tab.SETUP);
    updateDisplay();
  }

  public void updateDisplay() {
    // must be refining join to have access to model and rejoin tabs
    tabPane.setEnabledAt(Tab.MODEL.getIndex(), refiningJoin);
    tabPane.setEnabledAt(Tab.REJOIN.getIndex(), refiningJoin);
    // control gap text fields with checkbox
    boolean enable = cbGap.isSelected();
    ltfGapStart.setEnabled(enable);
    ltfGapStart.setTextPreferredWidth(UIParameters.INSTANCE.getIntegerWidth());
    ltfGapEnd.setEnabled(enable);
    ltfGapEnd.setTextPreferredWidth(UIParameters.INSTANCE.getIntegerWidth());
    ltfGapInc.setEnabled(enable);
    ltfGapInc.setTextPreferredWidth(UIParameters.INSTANCE.getIntegerWidth());
    // .join file must exist before it can be opened
    enable = DatasetFiles.getJoinFile(false, manager).exists();
    b3bOpenRejoinWithModel.setEnabled(enable);
    b3bOpenRejoin.setEnabled(enable);
    // Need boundary list to transform model
    enable = !state.isRefineStartListEmpty();
    btnTransformModel.setEnabled(enable);
    // Can't only refine if a join exists
    // A trial join is only refinable if it was created with all slices
    boolean trialJoinRefinable = DatasetFiles.getJoinFile(true, manager).exists()
        && state.getJoinTrialUseEveryNSlices().equals(1);
    cbRefineWithTrial.setEnabled(trialJoinRefinable);
    if (!trialJoinRefinable) {
      cbRefineWithTrial.setSelected(false);
    }
    btnRefineJoin.setEnabled(DatasetFiles.getJoinFile(false, manager).exists()
        || trialJoinRefinable);
  }

  /**
   * Add components to the current tab
   * @param tab
   */
  private Component addPanelComponents(Tab tab) {
    if (tab == Tab.SETUP) {
      addSetupPanelComponents();
      return pnlSetupTab.getContainer();
    }
    if (tab == Tab.ALIGN) {
      addAlignPanelComponents();
      return pnlAlignTab.getContainer();
    }
    if (tab == Tab.JOIN) {
      addJoinPanelComponents();
      return pnlJoinTab.getContainer();
    }
    if (tab == Tab.MODEL) {
      addModelPanelComponents();
      return pnlModelTab.getContainer();
    }
    if (tab == Tab.REJOIN) {
      addRejoinPanelComponents();
      return pnlRejoinTab;
    }
    return null;
  }

  /**
   * Get rubberband coordinates and calculate a new size and shift based on
   * the last finishjoin trial.
   * @param coordinates
   */
  private void setSizeAndShift(Vector coordinates) throws NullRequiredNumberException {
    if (coordinates == null) {
      return;
    }
    int size = coordinates.size();
    if (size == 0) {
      return;
    }
    int index = 0;
    EtomoNumber estXMin = new EtomoNumber(EtomoNumber.Type.INTEGER);
    EtomoNumber estYMin = new EtomoNumber(EtomoNumber.Type.INTEGER);
    EtomoNumber estXMax = new EtomoNumber(EtomoNumber.Type.INTEGER);
    EtomoNumber estYMax = new EtomoNumber(EtomoNumber.Type.INTEGER);
    while (index < size) {
      if (ImodProcess.RUBBERBAND_RESULTS_STRING.equals((String) coordinates.get(index++))) {
        estXMin.set((String) coordinates.get(index++));
        if (index >= size) {
          break;
        }
        estYMin.set((String) coordinates.get(index++));
        if (index >= size) {
          break;
        }
        estXMax.set((String) coordinates.get(index++));
        if (index >= size) {
          break;
        }
        estYMax.set((String) coordinates.get(index++));
      }
    }
    int min;
    int max;
    ConstJoinMetaData metaData = manager.getConstMetaData();
    if (!estXMin.isNull() && !estXMax.isNull()) {
      min = metaData.getCoordinate(estXMin, state);
      max = metaData.getCoordinate(estXMax, state);
      ltfSizeInX.setText(JoinMetaData.getSize(min, max));
      ltfShiftInX.setText(state.getNewShiftInX(min, max));
    }
    if (!estYMin.isNull() && !estYMax.isNull()) {
      min = metaData.getCoordinate(estYMin, state);
      max = metaData.getCoordinate(estYMax, state);
      ltfSizeInY.setText(JoinMetaData.getSize(min, max));
      ltfShiftInY.setText(state.getNewShiftInY(min, max));
    }
  }

  private void removePanelComponents(Tab tab) {
    if (tab == Tab.SETUP) {
      pnlSetupTab.removeAll();
    }
    else if (tab == Tab.ALIGN) {
      pnlAlignTab.removeAll();
    }
    else if (tab == Tab.JOIN) {
      pnlJoinTab.removeAll();
    }
    else if (tab == Tab.MODEL) {
      pnlModelTab.removeAll();
    }
    else if (tab == Tab.REJOIN) {
      pnlRejoinTab.removeAll();
    }
  }

  final boolean isSetupTab() {
    return curTab == Tab.SETUP;
  }

  final boolean isAlignTab() {
    return curTab == Tab.ALIGN;
  }

  final boolean isJoinTab() {
    return curTab == Tab.JOIN;
  }

  final boolean isModelTab() {
    return curTab == Tab.MODEL;
  }

  final boolean isRejoinTab() {
    return curTab == Tab.REJOIN;
  }

  Tab getTab() {
    return curTab;
  }

  int getSectionTableSize() {
    return pnlSectionTable.size();
  }

  private final void changeTab(ChangeEvent event) {
    Tab prevTab = curTab;
    removePanelComponents(prevTab);
    curTab = Tab.getInstance(tabPane.getSelectedIndex());
    synchronize(prevTab);
    Component displayedComponent = addPanelComponents(curTab);
    UIHarness.INSTANCE.pack(manager);
  }

  public void setInverted() throws LogFile.LockException {
    pnlSectionTable.setInverted();
  }

  public final void setMode(int mode) {
    if (mode == SETUP_MODE) {
      ftfWorkingDir.setEditable(true);
      ltfRootName.setEditable(true);
    }
    else {
      ftfWorkingDir.setEditable(false);
      ltfRootName.setEditable(false);
    }
    switch (mode) {
    case SETUP_MODE:
    case SAMPLE_NOT_PRODUCED_MODE:
      tabPane.setEnabledAt(1, false);
      tabPane.setEnabledAt(2, false);
      ltfMidasLimit.setEnabled(true);
      lblMidasLimit.setEnabled(true);
      spinDensityRefSection.setEnabled(true);
      btnChangeSetup.setEnabled(false);
      setRevertState(false);
      btnMakeSamples.setEnabled(true);
      break;
    case SAMPLE_PRODUCED_MODE:
      tabPane.setEnabledAt(1, true);
      tabPane.setEnabledAt(2, true);
      ltfMidasLimit.setEnabled(false);
      lblMidasLimit.setEnabled(false);
      spinDensityRefSection.setEnabled(false);
      btnChangeSetup.setEnabled(true);
      setRevertState(false);
      btnMakeSamples.setEnabled(false);
      break;
    case CHANGING_SAMPLE_MODE:
      tabPane.setEnabledAt(1, false);
      tabPane.setEnabledAt(2, false);
      ltfMidasLimit.setEnabled(true);
      lblMidasLimit.setEnabled(true);
      spinDensityRefSection.setEnabled(true);
      btnChangeSetup.setEnabled(false);
      setRevertState(true);
      btnMakeSamples.setEnabled(true);
      break;
    default:
      throw new IllegalStateException("mode=" + mode);
    }
    pnlSectionTable.setMode(mode);
  }

  private void setRevertState(boolean enableRevert) {
    btnRevertToLastSetup.setEnabled(enableRevert);
    state.setRevertState(enableRevert);
  }

  private void createSetupPanel(String workingDirName) {
    pnlSetupTab.setBoxLayout(BoxLayout.Y_AXIS);
    // first component
    ftfWorkingDir = new FileTextField(WORKING_DIRECTORY_TEXT + ": ");
    ftfWorkingDir.setText(workingDirName);
    // second component
    ltfRootName = new LabeledTextField(FieldType.STRING, "Root name for output file: ");
    // third component
    pnlSectionTable = new SectionTablePanel(this, manager, state);
    // midas limit panel
    pnlMidasLimit.setBoxLayout(BoxLayout.X_AXIS);
    pnlMidasLimit.add(ltfMidasLimit.getContainer());
    pnlMidasLimit.add(lblMidasLimit);
    // fifth component
    spinDensityRefSection = LabeledSpinner.getInstance(
        "Reference section for density matching: ", 1, 1, numSections < 1 ? 1
            : numSections, 1);
    // sixth component
    setupPanel2 = SpacedPanel.getInstance();
    setupPanel2.setBoxLayout(BoxLayout.X_AXIS);
    btnChangeSetup = new MultiLineButton("Change Setup");
    // btnChangeSetup.addActionListener(joinActionListener);
    btnChangeSetup.setSize();
    setupPanel2.add(btnChangeSetup);
    btnRevertToLastSetup = new MultiLineButton("Revert to Last Setup");
    setRevertState(true);
    // btnRevertToLastSetup.addActionListener(joinActionListener);
    btnRevertToLastSetup.setSize();
    setupPanel2.add(btnRevertToLastSetup);
    // seventh component
    btnMakeSamples.setDeferred3dmodButton(btnOpenSampleAverages);
    btnMakeSamples.setSize();
    btnMakeSamples.setAlignmentX(Component.CENTER_ALIGNMENT);
  }

  private void addSetupPanelComponents() {
    pnlSetupTab.add(ftfWorkingDir.getContainer());
    pnlSetupTab.add(ltfRootName);
    pnlSetupTab.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.displayCurTab();
    pnlSetupTab.add(pnlMidasLimit);
    JPanel pnlDensityRefSection = new JPanel();
    pnlDensityRefSection.setLayout(new BoxLayout(pnlDensityRefSection, BoxLayout.X_AXIS));
    pnlDensityRefSection.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlDensityRefSection.add(spinDensityRefSection.getContainer());
    pnlDensityRefSection.add(Box.createHorizontalGlue());
    pnlSetupTab.add(pnlDensityRefSection);
    pnlSetupTab.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSetupTab.add(setupPanel2);
    btnMakeSamples.setSize();
    pnlSetupTab.add(btnMakeSamples);
  }

  private void createAlignPanel() {
    pnlAlignTab.setBoxLayout(BoxLayout.Y_AXIS);
    // second component
    alignPanel1 = SpacedPanel.getInstance();
    alignPanel1.setBoxLayout(BoxLayout.X_AXIS);
    btnOpenSample = Run3dmodButton.get3dmodInstance("Open Sample in 3dmod", this);
    btnOpenSample.setSize();
    alignPanel1.add(btnOpenSample);
    btnOpenSampleAverages.setSize();
    alignPanel1.add(btnOpenSampleAverages);
    // fourth component

  }

  private void addAlignPanelComponents() {
    // first component
    pnlAlignTab.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.displayCurTab();
    // second component
    pnlAlignTab.add(alignPanel1);
    // auto alignment
    pnlAlignTab.add(autoAlignmentPanel.getRootComponent());
  }

  private void createModelPanel() {
    // create model panel only once
    if (pnlTransformations != null) {
      return;
    }
    // construct panels
    pnlTransformations = SpacedPanel.getInstance();
    SpacedPanel pnlGapStartEndInc = SpacedPanel.getInstance();
    SpacedPanel pnlPointsToFit = SpacedPanel.getInstance();
    // initialize
    btnRefineJoin.setSize();
    // build panels
    pnlModelTab.setBoxLayout(BoxLayout.Y_AXIS);
    pnlModelTab.alignComponentsX(Component.CENTER_ALIGNMENT);
    // transformations panel
    pnlTransformations.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTransformations.setBorder(new EtchedBorder("Transformations").getBorder());
    pnlTransformations.add(tcModel.getComponent());
    pnlTransformations.add(ltfBoundariesToAnalyze.getContainer());
    pnlTransformations.add(ltfObjectsToInclude.getContainer());
    pnlTransformations.add(pnlGapStartEndInc);
    pnlTransformations.add(pnlPointsToFit);
    btnXfjointomo.setSize();
    JPanel pnlXfjointomo = new JPanel();
    pnlXfjointomo.setLayout(new BoxLayout(pnlXfjointomo, BoxLayout.X_AXIS));
    pnlXfjointomo.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlXfjointomo.add(Box.createHorizontalGlue());
    pnlXfjointomo.add(btnXfjointomo.getComponent());
    pnlXfjointomo.add(Box.createHorizontalGlue());
    pnlTransformations.add(pnlXfjointomo);
    pnlTransformations.add(Box.createRigidArea(FixedDim.x0_y5));
    btnTransformAndViewModel.setSize();
    JPanel pnlTransformAndViewModel = new JPanel();
    pnlTransformAndViewModel.setLayout(new BoxLayout(pnlTransformAndViewModel,
        BoxLayout.X_AXIS));
    pnlTransformAndViewModel.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlTransformAndViewModel.add(Box.createHorizontalGlue());
    pnlTransformAndViewModel.add(btnTransformAndViewModel.getComponent());
    pnlTransformAndViewModel.add(Box.createHorizontalGlue());
    pnlTransformations.add(pnlTransformAndViewModel);
    // gap panel
    pnlGapStartEndInc.setBoxLayout(BoxLayout.X_AXIS);
    pnlGapStartEndInc.add(cbGap);
    pnlGapStartEndInc.add(ltfGapStart);
    pnlGapStartEndInc.add(ltfGapEnd);
    pnlGapStartEndInc.add(ltfGapInc.getContainer());
    // points to fit panel
    pnlPointsToFit.setBoxLayout(BoxLayout.X_AXIS);
    pnlPointsToFit.add(new JLabel("Points to fit: "));
    pnlPointsToFit.add(ltfPointsToFitMin);
    pnlPointsToFit.add(ltfPointsToFitMax);
  }

  private void addModelPanelComponents() {
    createModelPanel();
    btnMakeRefiningModel.setSize();
    pnlModelTab.add(btnMakeRefiningModel);
    pnlModelTab.add(boundaryTable.getContainer());
    boundaryTable.display();
    pnlModelTab.add(pnlTransformations);
  }

  private void addRejoinPanelComponents() {
    createRejoinPanel();
    pnlRejoinTab.add(pnlTables);
    pnlTables.add(pnlSectionTable.getContainer());
    pnlSectionTable.displayCurTab();
    pnlTables.add(boundaryTable.getContainer());
    boundaryTable.display();
    pnlRejoinTab.add(pnlRejoin);
  }

  private void createRejoinPanel() {
    if (pnlTables != null) {
      return;
    }
    pnlRejoinTab.setLayout(new BoxLayout(pnlRejoinTab, BoxLayout.Y_AXIS));
    pnlTables = new JPanel();
    pnlTables.setLayout(new BoxLayout(pnlTables, BoxLayout.X_AXIS));
    pnlRejoin = new JPanel();
    pnlRejoin.setLayout(new BoxLayout(pnlRejoin, BoxLayout.Y_AXIS));
    // use every spinner
    SpacedPanel pnlUseEvery = SpacedPanel.getInstance();
    pnlUseEvery.setBoxLayout(BoxLayout.X_AXIS);
    int zMax = pnlSectionTable.getZMax();
    int currentValue = zMax < 1 ? 1 : zMax < 10 ? zMax : 10;
    spinRejoinUseEveryNSlices = LabeledSpinner.getInstance("Use every ", currentValue, 1,
        zMax < 1 ? 1 : zMax, 1);
    pnlUseEvery.add(spinRejoinUseEveryNSlices);
    pnlUseEvery.add(new JLabel("slices"));
    // trial rejoin button
    SpacedPanel pnlTrialRejoinButton = SpacedPanel.getInstance();
    pnlTrialRejoinButton.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTrialRejoinButton.setBorder(BorderFactory.createEtchedBorder());
    pnlTrialRejoinButton.add(pnlUseEvery.getContainer());
    spinRejoinTrialBinning = LabeledSpinner.getInstance("Binning in X and Y: ", 1, 1, 50,
        1);
    pnlTrialRejoinButton.add(spinRejoinTrialBinning.getContainer());
    btnTrialRejoin.setDeferred3dmodButton(b3bOpenTrialRejoin);
    btnTrialRejoin.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTrialRejoin.setSize();
    pnlTrialRejoinButton.add(btnTrialRejoin);
    // trial rejoin buttons
    EtomoPanel pnlTrialRejoinButtons = new EtomoPanel();
    pnlTrialRejoinButtons
        .setLayout(new BoxLayout(pnlTrialRejoinButtons, BoxLayout.X_AXIS));
    pnlTrialRejoinButtons.setBorder(new EtchedBorder("Trial Rejoin").getBorder());
    pnlTrialRejoinButtons.add(Box.createHorizontalGlue());
    pnlTrialRejoinButtons.add(pnlTrialRejoinButton.getContainer());
    pnlTrialRejoinButtons.add(b3bOpenTrialRejoin.getContainer());
    pnlRejoin.add(pnlTrialRejoinButtons);
    // rejoin buttons
    EtomoPanel pnlRejoinButtons = new EtomoPanel();
    pnlRejoinButtons.setLayout(new BoxLayout(pnlRejoinButtons, BoxLayout.X_AXIS));
    pnlRejoinButtons.setBorder(new EtchedBorder("Final Rejoin").getBorder());
    pnlRejoinButtons.add(Box.createHorizontalGlue());
    pnlRejoinButtons.add(btnRejoin.getComponent());
    btnRejoin.setDeferred3dmodButton(b3bOpenRejoin);
    btnRejoin.setSize();
    pnlRejoinButtons.add(b3bOpenRejoin.getContainer());
    pnlRejoin.add(pnlRejoinButtons);
    // transform model
    SpacedPanel pnlTransformModel = SpacedPanel.getInstance();
    pnlTransformModel.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTransformModel.setBorder(new EtchedBorder("Transform Model").getBorder());
    pnlTransformModel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlTransformModel.add(ftfModelFile.getContainer());
    ltfTransformedModel = new LabeledTextField(FieldType.STRING, "Output file: ");
    pnlTransformModel.add(ltfTransformedModel.getContainer());
    // transform model buttons
    JPanel pnlTransformModelButtons = new JPanel();
    pnlTransformModelButtons.setLayout(new BoxLayout(pnlTransformModelButtons,
        BoxLayout.X_AXIS));
    pnlTransformModelButtons.add(Box.createHorizontalGlue());
    btnTransformModel.setDeferred3dmodButton(b3bOpenRejoinWithModel);
    btnTransformModel.setSize();
    pnlTransformModelButtons.add(btnTransformModel.getComponent());
    pnlTransformModelButtons.add(b3bOpenRejoinWithModel.getContainer());
    pnlTransformModel.add(pnlTransformModelButtons);
    pnlRejoin.add(pnlTransformModel.getContainer());
  }

  private void createJoinPanel() {
    pnlJoinTab.setBoxLayout(BoxLayout.X_AXIS);
    // second component
    createFinishJoinPanel();
  }

  private void addJoinPanelComponents() {
    // first component
    pnlJoinTab.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.displayCurTab();
    // second component
    pnlJoinTab.add(pnlFinishJoin);
  }

  private void createFinishJoinPanel() {
    pnlFinishJoin = SpacedPanel.getInstance();
    pnlFinishJoin.setBoxLayout(BoxLayout.Y_AXIS);
    pnlFinishJoin.setBorder(BorderFactory.createEtchedBorder());
    pnlFinishJoin.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    // first component
    cbsAlignmentRefSection = CheckBoxSpinner
        .getInstance("Reference section for alignment: ");
    SpinnerNumberModel spinnerModel = new SpinnerNumberModel(1, 1, numSections < 1 ? 1
        : numSections, 1);
    cbsAlignmentRefSection.setModel(spinnerModel);
    cbsAlignmentRefSection.setMaximumSize(dimSpinner);
    pnlFinishJoin.add(cbsAlignmentRefSection.getContainer());
    // second component
    btnGetMaxSize = new MultiLineButton(GET_MAX_SIZE_TEXT);
    btnGetMaxSize.setSize();
    pnlFinishJoin.add(btnGetMaxSize);
    // third component
    SpacedPanel finishJoinPanel2 = SpacedPanel.getInstance();
    finishJoinPanel2.setBoxLayout(BoxLayout.X_AXIS);
    ltfSizeInX = new LabeledTextField(FieldType.INTEGER, "Size in X: ");
    finishJoinPanel2.add(ltfSizeInX);
    ltfSizeInY = new LabeledTextField(FieldType.INTEGER, "Y: ");
    finishJoinPanel2.add(ltfSizeInY);
    pnlFinishJoin.add(finishJoinPanel2);
    // fourth component
    SpacedPanel finishJoinPanel3 = SpacedPanel.getInstance();
    finishJoinPanel3.setBoxLayout(BoxLayout.X_AXIS);
    ltfShiftInX = new LabeledTextField(FieldType.INTEGER, "Shift in X: ");
    finishJoinPanel3.add(ltfShiftInX);
    ltfShiftInY = new LabeledTextField(FieldType.INTEGER, "Y: ");
    finishJoinPanel3.add(ltfShiftInY);
    pnlFinishJoin.add(finishJoinPanel3);
    JPanel pnlLocalFits = new JPanel();
    pnlLocalFits.setLayout(new BoxLayout(pnlLocalFits, BoxLayout.X_AXIS));
    pnlLocalFits.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlLocalFits.add(cbLocalFits);
    pnlLocalFits.add(Box.createHorizontalGlue());
    pnlFinishJoin.add(pnlLocalFits);
    // fifth component
    createTrialJoinPanel();
    // sixth component
    btnFinishJoin.setDeferred3dmodButton(b3bOpenIn3dmod);
    btnFinishJoin.setSize();
    pnlFinishJoin.add(btnFinishJoin);
    // seventh component
    b3bOpenIn3dmod
        .setSpinnerToolTipText("The binning to use when opening the joined tomogram in 3dmod.");
    pnlFinishJoin.add(b3bOpenIn3dmod.getContainer());
    // eight component
    JPanel pnlRefineWithTrial = new JPanel();
    pnlRefineWithTrial.setLayout(new BoxLayout(pnlRefineWithTrial, BoxLayout.X_AXIS));
    pnlRefineWithTrial.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRefineWithTrial.add(cbRefineWithTrial);
    pnlRefineWithTrial.add(Box.createHorizontalGlue());
    pnlFinishJoin.add(pnlRefineWithTrial);
    btnRefineJoin.setSize();
    pnlFinishJoin.add(btnRefineJoin);
  }

  private void createTrialJoinPanel() {
    SpacedPanel pnlTrialJoin = SpacedPanel.getInstance();
    pnlTrialJoin.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTrialJoin.setBorder(new EtchedBorder(TRIAL_JOIN_TEXT).getBorder());
    pnlTrialJoin.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    // first component
    SpacedPanel trialJoinPanel1 = SpacedPanel.getInstance();
    trialJoinPanel1.setBoxLayout(BoxLayout.X_AXIS);
    int zMax = pnlSectionTable.getZMax();
    int currentValue = zMax < 1 ? 1 : zMax < 10 ? zMax : 10;
    spinUseEveryNSlices = LabeledSpinner.getInstance("Use every ", currentValue, 1,
        zMax < 1 ? 1 : zMax, 1);
    trialJoinPanel1.add(spinUseEveryNSlices);
    trialJoinPanel1.add(new JLabel("slices"));
    pnlTrialJoin.add(trialJoinPanel1);
    // second component
    spinTrialBinning = LabeledSpinner.getInstance("Binning in X and Y: ", 1, 1, 50, 1);
    pnlTrialJoin.add(spinTrialBinning);
    // third component
    btnTrialJoin.setDeferred3dmodButton(b3bOpenTrialIn3dmod);
    btnTrialJoin.setSize();
    pnlTrialJoin.add(btnTrialJoin);
    // fourth component
    b3bOpenTrialIn3dmod
        .setSpinnerToolTipText("The binning to use when opening the trial joined tomogram in 3dmod.");
    pnlTrialJoin.add(b3bOpenTrialIn3dmod.getContainer());
    // fifth component
    btnGetSubarea = new MultiLineButton("Get Subarea Size And Shift");
    btnGetSubarea.setSize();
    pnlTrialJoin.add(btnGetSubarea);
    pnlFinishJoin.add(pnlTrialJoin);
  }

  void msgRowChange() {
    boundaryTable.msgRowChange();
  }

  /**
   * change the model for spinners when the number of sections changes, but
   * preserve any value set by the user.
   * @param numSections
   */
  void setNumSections(int numSections) {
    this.numSections = numSections;
    // density matching (setup)
    EtomoNumber spinnerValue = new EtomoNumber(EtomoNumber.Type.INTEGER);
    spinnerValue.set(spinDensityRefSection.getValue());
    spinnerValue.setDisplayValue(1);
    spinDensityRefSection.setModel(spinnerValue.getInt(), 1, numSections < 1 ? 1
        : numSections, 1);
    // alignment (join)
    spinnerValue.set((Integer) cbsAlignmentRefSection.getValue());
    SpinnerNumberModel spinnerModel = new SpinnerNumberModel(spinnerValue.getInt(), 1,
        numSections < 1 ? 1 : numSections, 1);
    cbsAlignmentRefSection.setModel(spinnerModel);
    // every n sections (join)
    int zMax = pnlSectionTable.getZMax();
    if (zMax == 0) {
      spinnerValue.set(1);
    }
    else {
      spinnerValue.setCeiling(zMax);
      spinnerValue.set((Integer) spinUseEveryNSlices.getValue());
    }
    spinnerValue.setDisplayValue(zMax < 1 ? 1 : zMax < 10 ? zMax : 10);
    spinUseEveryNSlices.setModel(spinnerValue.getInt(), 1, zMax < 1 ? 1 : zMax, 1);
    spinnerValue.set((Integer) spinRejoinUseEveryNSlices.getValue());
    int min = spinnerValue.getInt();
    spinRejoinUseEveryNSlices.setModel(min, 1, zMax < min ? min : zMax, 1);
    defaultSizeInXY();
  }

  private void init() {
    defaultXSize = pnlSectionTable.getXMax();
    defaultYSize = pnlSectionTable.getYMax();
  }

  /**
   * Call this when the number of sections has changed or a section rotation
   * has changed.
   * Will override the values on the screen if xMax and yMax have changed
   */
  void defaultSizeInXY() {
    // update size in X and Y defaults
    ConstJoinMetaData metaData = manager.getConstMetaData();
    int xMax = pnlSectionTable.getXMax();
    int yMax = pnlSectionTable.getYMax();
    if (xMax == defaultXSize && yMax == defaultYSize) {
      return;
    }
    defaultXSize = xMax;
    defaultYSize = yMax;
    ltfSizeInX.setText(defaultXSize);
    ltfSizeInY.setText(defaultYSize);
  }

  public void setSizeInX(ConstEtomoNumber sizeInX) {
    ltfSizeInX.setText(sizeInX.toString());
  }

  public void setSizeInY(ConstEtomoNumber sizeInY) {
    ltfSizeInY.setText(sizeInY.toString());
  }

  public void setShiftInX(int shiftInX) {
    ltfShiftInX.setText(shiftInX);
  }

  public void setShiftInY(int shiftInY) {
    ltfShiftInY.setText(shiftInY);
  }

  public String getInvalidReason() {
    if (invalidReason != null) {
      return invalidReason;
    }
    return pnlSectionTable.getInvalidReason();
  }

  public int getMode() {
    return pnlSectionTable.getMode();
  }

  public boolean getParameters(XfjointomoParam param, final boolean doValidation) {
    try {
      param.setTransform(tcModel.getTransform());
      param.setBoundariesToAnalyze(ltfBoundariesToAnalyze.getText(doValidation));
      param.setObjectsToInclude(ltfObjectsToInclude.getText(doValidation));
      param.setPointsToFit(ltfPointsToFitMin.getText(doValidation),
          ltfPointsToFitMax.getText(doValidation));
      if (cbGap.isSelected()) {
        param.setGapStartEndInc(ltfGapStart.getText(doValidation),
            ltfGapEnd.getText(doValidation), ltfGapInc.getText(doValidation));
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public void getAutoAlignmentParameters(final MidasParam param) {
    manager.getParameters(param, axisID);
  }

  public boolean getAutoAlignmentParameters(final XfalignParam param,
      final boolean doValidation) {
    manager.getParameters(param, axisID);
    return true;
  }

  public void setXfjointomoResult() throws LogFile.LockException, FileNotFoundException,
      IOException {
    boundaryTable.setXfjointomoResult();
  }

  public boolean getMetaData(JoinMetaData metaData, final boolean doValidation) {
    synchronize();
    try {
      metaData.setRootName(ltfRootName.getText(doValidation));
      metaData.setDensityRefSection(spinDensityRefSection.getValue());
      if (!autoAlignmentPanel.getParameters(metaData.getAutoAlignmentMetaData(),
          doValidation)) {
        return false;
      }
      metaData.setUseAlignmentRefSection(cbsAlignmentRefSection.isSelected());
      metaData.setAlignmentRefSection(cbsAlignmentRefSection.getValue());
      metaData.setSizeInX(ltfSizeInX.getText(doValidation));
      metaData.setSizeInY(ltfSizeInY.getText(doValidation));
      metaData.setShiftInX(ltfShiftInX.getText(doValidation));
      metaData.setShiftInY(ltfShiftInY.getText(doValidation));
      metaData.setLocalFits(cbLocalFits.isSelected());
      metaData.setUseEveryNSlices(spinUseEveryNSlices.getValue());
      metaData.setRejoinUseEveryNSlices(spinRejoinUseEveryNSlices.getValue());
      metaData.setTrialBinning(spinTrialBinning.getValue());
      metaData.setMidasLimit(ltfMidasLimit.getText(doValidation));
      metaData.setModelTransform(tcModel.getTransform());
      metaData.setBoundariesToAnalyze(ltfBoundariesToAnalyze.getText(doValidation));
      metaData.setObjectsToInclude(ltfObjectsToInclude.getText(doValidation));
      metaData.setGap(cbGap.isSelected());
      metaData.setGapStart(ltfGapStart.getText(doValidation));
      metaData.setGapEnd(ltfGapEnd.getText(doValidation));
      metaData.setGapInc(ltfGapInc.getText(doValidation));
      metaData.setPointsToFitMin(ltfPointsToFitMin.getText(doValidation));
      metaData.setPointsToFitMax(ltfPointsToFitMax.getText(doValidation));
      metaData.setRejoinTrialBinning(spinRejoinTrialBinning.getValue());
      boundaryTable.getMetaData();
      return pnlSectionTable.getMetaData(metaData);
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  SectionTablePanel getSectionTable() {
    return pnlSectionTable;
  }

  public void getScreenState(JoinScreenState screenState) {
    screenState.setRefineWithTrial(cbRefineWithTrial.isSelected());
    boundaryTable.getScreenState();
  }

  public void setMetaData(ConstJoinMetaData metaData) {
    ltfRootName.setText(metaData.getDatasetName());
    spinDensityRefSection.setValue(metaData.getDensityRefSection().getInt());
    autoAlignmentPanel.setParameters(metaData.getAutoAlignmentMetaData());
    cbsAlignmentRefSection.setSelected(metaData.isUseAlignmentRefSection());
    cbsAlignmentRefSection.setValue(metaData.getAlignmentRefSection().getInt());
    ltfShiftInX.setText(metaData.getShiftInX().toString());
    ltfShiftInY.setText(metaData.getShiftInY().toString());
    spinUseEveryNSlices.setValue(metaData.getUseEveryNSlices());
    spinRejoinUseEveryNSlices.setValue(metaData.getRejoinUseEveryNSlices());
    spinTrialBinning.setValue(metaData.getTrialBinning());
    ltfMidasLimit.setText(metaData.getMidasLimit().toString());
    pnlSectionTable.setMetaData(metaData);
    ltfSizeInX.setText(metaData.getSizeInX().toString());
    ltfSizeInY.setText(metaData.getSizeInY().toString());
    cbLocalFits.setSelected(metaData.isLocalFits());
    tcModel.setTransform(metaData.getModelTransform());
    ltfBoundariesToAnalyze.setText(metaData.getBoundariesToAnalyze());
    ltfObjectsToInclude.setText(metaData.getObjectsToInclude());
    cbGap.setSelected(metaData.getGap().is());
    updateDisplay();
    ltfGapStart.setText(metaData.getGapStart());
    ltfGapEnd.setText(metaData.getGapEnd());
    ltfGapInc.setText(metaData.getGapInc());
    ltfPointsToFitMin.setText(metaData.getPointsToFitMin());
    ltfPointsToFitMax.setText(metaData.getPointsToFitMax());
    spinRejoinTrialBinning.setValue(metaData.getRejoinTrialBinning());
  }

  public void setScreenState(JoinScreenState screenState) {
    if (cbRefineWithTrial.isEnabled()) {
      cbRefineWithTrial.setSelected(screenState.getRefineWithTrial().is());
    }
  }

  public Container getContainer() {
    return rootPanel;
  }

  public boolean validateMakejoincom() {
    return pnlSectionTable.validateMakejoincom();
  }

  public boolean validateFinishjoin() {
    return pnlSectionTable.validateFinishjoin();
  }

  public String getWorkingDirName() {
    return ftfWorkingDir.getText();
  }

  public File getWorkingDir() {
    String workingDirName = ftfWorkingDir.getText();
    if (workingDirName == null || workingDirName.length() == 0
        || workingDirName.matches("\\s+")) {
      return null;
    }
    if (workingDirName.endsWith(" ")) {
      UIHarness.INSTANCE.openMessageDialog(manager, "The directory, " + workingDirName
          + ", cannot be used because it ends with a space.", "Unusable Directory Name",
          AxisID.ONLY);
      return null;
    }
    return new File(ftfWorkingDir.getText());
  }

  public String getRootName() {
    return ltfRootName.getText();
  }

  public void abortAddSection() {
    pnlSectionTable.enableAddSection();
  }

  public void msgProcessEnded() {
    autoAlignmentPanel.msgProcessChange(true);
  }

  /**
   * checking if dialog is equal to meta data.  Set useDefault to match how 
   * useDefault is used in setMetaData()
   * @param metaData
   * @return
   */
  public boolean equals(ConstJoinMetaData metaData) {
    if (!ltfRootName.equals(metaData.getDatasetName())) {
      return false;
    }
    if (!metaData.getDensityRefSection()
        .equals((Number) spinDensityRefSection.getValue())) {
      return false;
    }
    if (!autoAlignmentPanel.equals(metaData)) {
      return false;
    }
    if (cbsAlignmentRefSection.isSelected() != metaData.isUseAlignmentRefSection()) {
      return false;
    }
    if (!metaData.getAlignmentRefSection().equals(
        (Number) cbsAlignmentRefSection.getValue())) {
      return false;
    }
    if (!metaData.getSizeInX().equals(ltfSizeInX.getText())) {
      return false;
    }
    if (!metaData.getSizeInY().equals(ltfSizeInY.getText())) {
      return false;
    }
    if (!metaData.getShiftInX().equals(ltfShiftInX.getText())) {
      return false;
    }
    if (!metaData.getShiftInY().equals(ltfShiftInY.getText())) {
      return false;
    }
    if (!metaData.getUseEveryNSlices().equals((Number) spinUseEveryNSlices.getValue())) {
      return false;
    }
    if (!metaData.getRejoinUseEveryNSlices().equals(
        (Number) spinRejoinUseEveryNSlices.getValue())) {
      return false;
    }
    if (!metaData.getTrialBinning().equals((Number) spinTrialBinning.getValue())) {
      return false;
    }
    if (!pnlSectionTable.equals(metaData)) {
      return false;
    }
    return true;
  }

  /**
   * checking if dialog fields used to make the sample are equal to the fields
   * in meta data.  Set useDefault to match how 
   * useDefault is used in setMetaData()
   * @param metaData
   * @return
   */
  public boolean equalsSample(ConstJoinMetaData metaData) {
    if (!ltfRootName.equals(metaData.getDatasetName())) {
      return false;
    }
    if (!metaData.getDensityRefSection()
        .equals((Number) spinDensityRefSection.getValue())) {
      return false;
    }
    if (!pnlSectionTable.equalsSample(metaData)) {
      return false;
    }
    return true;
  }

  public void addSection(File tomogram) {
    pnlSectionTable.addSection(tomogram);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel;
    String[] manPage;
    String[] logFileLabel;
    String[] logFile;
    ContextPopup contextPopup;
    if (curTab == Tab.SETUP) {
      manPagelabel = new String[] { "3dmod" };
      manPage = new String[] { "3dmod.html" };
      logFileLabel = new String[] { "startjoin" };
      logFile = new String[] { "startjoin.log" };
      contextPopup = new ContextPopup(rootPanel, mouseEvent, "Setup",
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
          axisID);
    }
    else if (curTab == Tab.ALIGN) {
      manPagelabel = new String[] { "Xfalign", "Midas", "3dmod" };
      manPage = new String[] { "xfalign.html", "midas.html", "3dmod.html" };
      contextPopup = new ContextPopup(rootPanel, mouseEvent, "Align",
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage, manager, axisID);
    }
    else if (curTab == Tab.JOIN) {
      manPagelabel = new String[] { "Finishjoin", "3dmod" };
      manPage = new String[] { "finishjoin.html", "3dmod.html" };
      contextPopup = new ContextPopup(rootPanel, mouseEvent, "Joining",
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage, manager, axisID);
    }
    else if (curTab == Tab.MODEL) {
      manPagelabel = new String[] { "Xfjointomo", "3dmod" };
      manPage = new String[] { "xfjointomo.html", "3dmod.html" };
      logFile = new String[1];
      logFileLabel = new String[] { "xfjointomo" };
      logFile = new String[] { "xfjointomo.log" };
      contextPopup = new ContextPopup(rootPanel, mouseEvent, "Joining",
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage, logFileLabel, logFile, manager,
          axisID);
    }
  }

  /**
   * Handle actions
   * @param event
   */
  private void action(final String command, Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    try {
      if (command.equals(btnMakeSamples.getActionCommand())) {
        if (ftfWorkingDir.isEditable()
            && !DatasetTool.validateDatasetName(manager, axisID, ftfWorkingDir.getFile(),
                ltfRootName.getText(), DataFileType.JOIN, null, true)) {
          return;
        }
        manager.makejoincom(null, deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
      }
      else if (command.equals(btnFinishJoin.getActionCommand())) {
        manager.finishjoin(FinishjoinParam.Mode.FINISH_JOIN, FINISH_JOIN_TEXT, null,
            deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
      }
      else if (command.equals(btnGetMaxSize.getActionCommand())) {
        manager.finishjoin(FinishjoinParam.Mode.MAX_SIZE, GET_MAX_SIZE_TEXT, null, null,
            null, DIALOG_TYPE);
      }
      else if (command.equals(btnTrialJoin.getActionCommand())) {
        manager.finishjoin(FinishjoinParam.Mode.TRIAL, TRIAL_JOIN_TEXT, null,
            deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
      }
      else if (command.equals(btnGetSubarea.getActionCommand())) {
        try {
          setSizeAndShift(manager.imodGetRubberbandCoordinates(
              ImodManager.TRIAL_JOIN_KEY, AxisID.ONLY));
        }
        catch (NullRequiredNumberException e) {
          UIHarness.INSTANCE.openMessageDialog(manager,
              "Unable to retrieve the trial join's binning, size and/or shift.  Please "
                  + "press " + btnGetMaxSize.getUnformattedLabel()
                  + " and then rebuild the trial join in order get a correct subarea "
                  + "size and " + "shift.\n" + e.getMessage(), "Rerun Trial Join");
        }
      }
      else if (command.equals(btnChangeSetup.getActionCommand())) {
        // Prepare for Revert: meta data file should match the screen
        JoinMetaData metaData = manager.getJoinMetaData();
        getMetaData(metaData, false);
        try {
          ParameterStore parameterStore = manager.getParameterStore();
          if (parameterStore != null) {
            parameterStore.save(metaData);
          }
        }
        catch (LogFile.LockException e) {
          e.printStackTrace();
          UIHarness.INSTANCE.openMessageDialog(manager,
              "Unable to save or write JoinMetaData.\n" + e.getMessage(), "Etomo Error");
        }
        catch (IOException e) {
          UIHarness.INSTANCE.openMessageDialog(manager,
              "Unable to save or write JoinMetaData.\n" + e.getMessage(), "Etomo Error");
        }
        setMode(JoinDialog.CHANGING_SAMPLE_MODE);
      }
      else if (command.equals(btnRevertToLastSetup.getActionCommand())) {
        ConstJoinMetaData metaData = manager.getConstMetaData();
        if (!state.isSampleProduced()) {
          throw new IllegalStateException(
              "sample produced is false but Revert to Last Setup is enabled");
        }
        pnlSectionTable.deleteSections();
        setMetaData(manager.getConstMetaData());
        state.revert();
        setMode(SAMPLE_PRODUCED_MODE);
      }
      else if (command.equals(btnRefineJoin.getActionCommand())) {
        startRefine();
      }
      else if (command.equals(btnXfjointomo.getActionCommand())) {
        manager.xfjointomo(null);
      }
      else if (command.equals(btnRejoin.getActionCommand())) {
        manager.finishjoin(FinishjoinParam.Mode.REJOIN, REJOIN_TEXT, null,
            deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
      }
      else if (command.equals(btnTrialRejoin.getActionCommand())) {
        manager.finishjoin(FinishjoinParam.Mode.TRIAL_REJOIN, TRIAL_REJOIN_TEXT, null,
            deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
      }
      else if (command.equals(cbGap.getActionCommand())) {
        updateDisplay();
      }
      else if (command.equals(btnTransformModel.getActionCommand())) {
        manager.xfmodel(ftfModelFile.getText(), ltfTransformedModel.getText(true), null,
            deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
      }
      else if (command.equals(btnTransformAndViewModel.getActionCommand())) {
        manager.finishjoin(FinishjoinParam.Mode.SUPPRESS_EXECUTION, REJOIN_TEXT, null,
            null, null, DIALOG_TYPE);
      }
      else if (command.equals(btnOpenSample.getActionCommand())) {
        manager.imodOpen(ImodManager.JOIN_SAMPLES_KEY, run3dmodMenuOptions);
      }
      else if (command.equals(btnOpenSampleAverages.getActionCommand())) {
        manager.imodOpen(ImodManager.JOIN_SAMPLE_AVERAGES_KEY, run3dmodMenuOptions);
      }
      else if (command.equals(b3bOpenIn3dmod.getActionCommand())) {
        manager.imodOpen(ImodManager.JOIN_KEY, b3bOpenIn3dmod.getBinningInXandY(),
            run3dmodMenuOptions);
      }
      else if (command.equals(b3bOpenTrialIn3dmod.getActionCommand())) {
        manager.imodOpen(ImodManager.TRIAL_JOIN_KEY,
            b3bOpenTrialIn3dmod.getBinningInXandY(), run3dmodMenuOptions);
      }
      else if (command.equals(btnMakeRefiningModel.getActionCommand())) {
        manager.imodOpen(AxisID.ONLY, ImodManager.MODELED_JOIN_KEY,
            DatasetFiles.getRefineModelFileName(manager), run3dmodMenuOptions, true);
      }
      else if (command.equals(b3bOpenRejoin.getActionCommand())) {
        manager.imodOpen(ImodManager.JOIN_KEY, b3bOpenRejoin.getBinningInXandY(),
            DatasetFiles.getRefineAlignedModelFileName(manager), run3dmodMenuOptions);
      }
      else if (command.equals(b3bOpenTrialRejoin.getActionCommand())) {
        ConstEtomoNumber useEveryNSlices = state.getRefineTrialUseEveryNSlices();
        if (useEveryNSlices.isNull() || useEveryNSlices.gt(1)) {
          // don't open the model if all the slices have not been included
          manager.imodOpen(ImodManager.TRIAL_JOIN_KEY,
              b3bOpenTrialRejoin.getBinningInXandY(), run3dmodMenuOptions);
        }
        else {
          manager.imodOpen(ImodManager.TRIAL_JOIN_KEY,
              b3bOpenTrialRejoin.getBinningInXandY(),
              DatasetFiles.getRefineAlignedModelFileName(manager), run3dmodMenuOptions);
        }
      }
      else if (command.equals(b3bOpenRejoinWithModel.getActionCommand())) {
        manager.imodOpen(ImodManager.JOIN_KEY,
            b3bOpenRejoinWithModel.getBinningInXandY(),
            ltfTransformedModel.getText(true), run3dmodMenuOptions);
      }
      else {
        throw new IllegalStateException("Unknown command " + command);
      }
    }
    catch (FieldValidationFailedException e) {
    }
  }

  private void setRefineDataHighlight(boolean highlight) {
    ltfSizeInX.setHighlight(highlight);
    ltfSizeInY.setHighlight(highlight);
    ltfShiftInX.setHighlight(highlight);
    ltfShiftInY.setHighlight(highlight);
    cbsAlignmentRefSection.setHighlight(highlight);
    pnlSectionTable.setJoinFinalStartHighlight(highlight);
    pnlSectionTable.setJoinFinalEndHighlight(highlight);
    btnGetSubarea.setHighlight(highlight);
    btnGetMaxSize.setHighlight(highlight);
    if (cbRefineWithTrial.isSelected()) {
      spinTrialBinning.setHighlight(highlight);
      spinUseEveryNSlices.setHighlight(highlight);
    }
  }

  /**
   * Copy the join or trial join file to _modeled.join.  Also try to make sure
   * that the data about the join or trial join file is accurate.
   */
  private void startRefine() {
    boolean useTrial = cbRefineWithTrial.isSelected();
    FileType joinFileType;
    if (useTrial) {
      joinFileType = FileType.TRIAL_JOIN;
    }
    else {
      joinFileType = FileType.JOIN;
    }
    String buttonName = useTrial ? TRIAL_JOIN_TEXT : FINISH_JOIN_TEXT;
    String joinFileName = joinFileType.getFileName(manager, axisID);
    // make sure the file to be moved exists
    if (!DatasetFiles.getJoinFile(useTrial, manager).exists()) {
      UIHarness.INSTANCE.openMessageDialog(manager, joinFileName
          + " does not exist.  Press " + buttonName + " to create it.",
          "Failed File Move");
      return;
    }
    // Make sure that there is data available about the .join file which will be
    // used in the refine. If there is not, ask for the user's assurance that
    // data on the screen corresponds to the .join file.
    boolean convertVersion = false;
    if (!state.isJoinVersionGe(useTrial, JoinState.MIN_REFINE_VERSION)) {
      setRefineDataHighlight(true);
      convertVersion = UIHarness.INSTANCE
          .openYesNoDialog(
              manager,
              "IMPORTANT!!  Information about the "
                  + joinFileName
                  + " file has not been saved.\nRefine cannot proceed without complete information on the join file.  If the highlighted data on the screen has NOT been changed since the "
                  + joinFileName
                  + " file was created, press Yes.  Otherwise press no and then press "
                  + buttonName + " to recreate the file.", AxisID.ONLY);
      setRefineDataHighlight(false);
      if (!convertVersion) {
        return;
      }
    }
    // The _modeled.join file already exists and there is a model that may be
    // associated with it. Warn the user before overwriting it.
    if (DatasetFiles.getModeledJoinFile(manager).exists()
        && DatasetFiles.getRefineModelFile(manager).exists()) {
      if (!UIHarness.INSTANCE.openYesNoDialog(manager,
          "The modeled join file and the refine model already exist.\n"
              + "Press Yes to overwrite the modeled join file.  " + "IMPORTANT:  "
              + "If the binning of the modeled join file will be different, "
              + "you must open the model with the new modeled join file and "
              + "save it at least once.", AxisID.ONLY)) {
        return;
      }
    }
    String imodKey;
    if (useTrial) {
      imodKey = ImodManager.TRIAL_JOIN_KEY;
    }
    else {
      imodKey = ImodManager.JOIN_KEY;
    }
    if (!manager.closeImod(imodKey, axisID, joinFileName + " file in 3dmod",
        "This file will be moved to " + DatasetFiles.getModeledJoinFileName(manager),
        false)) {
      /* UIHarness.INSTANCE.openMessageDialog(manager, "Please close the " + joinFileName
       * + " file in 3dmod.  This file will be moved to " +
       * DatasetFiles.getModeledJoinFileName(manager) + '.', "Close 3dmod"); */
      return;
    }
    try {
      // move the join file (or trial join) to the _modeled.join file
      System.out.println("backup " + FileType.MODELED_JOIN);
      manager.backupImageFile(FileType.MODELED_JOIN, axisID);
      System.out.println("rename " + joinFileType + "," + FileType.MODELED_JOIN);
      Utilities.renameFile(joinFileType.getFile(manager, axisID),
          FileType.MODELED_JOIN.getFile(manager, axisID));
      // LogFile.getInstance(manager.getPropertyUserDir(), joinFileName).move(
      // LogFile.getInstance(manager.getPropertyUserDir(), DatasetFiles
      // .getModeledJoinFileName(manager)));
    }
    catch (IOException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog(manager,
          "Unable to move join file.\n" + e.getMessage(), "Failed File Move");
      return;
    }
    // convertVersion is true when the .ejf file is an older version and the user
    // wishes to use screen values to convert it.
    if (convertVersion) {
      if (!pnlSectionTable.getMetaData(manager.getJoinMetaData())) {
        UIHarness.INSTANCE.openMessageDialog(manager,
            "Unable to proceed.  Screen data is invalid.", "Data Error");
        return;
      }
      state.setJoinVersion1_0(useTrial, manager.getJoinMetaData());
    }
    refiningJoin = true;
    state.setRefineTrial(useTrial);
    updateDisplay();
  }

  private void setRefiningJoin() {
    refiningJoin = DatasetFiles.getModeledJoinFile(manager).exists();
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    action(button.getActionCommand(), button.getDeferred3dmodButton(),
        run3dmodMenuOptions);
  }

  protected void workingDirAction() {
    // Open up the file chooser in the current working directory
    JFileChooser chooser = new FileChooser(new File(manager.getPropertyUserDir()));
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File workingDir = chooser.getSelectedFile();
      try {
        ftfWorkingDir.setText(workingDir.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  protected void modelFileAction() {
    // Open up the file chooser in the current working directory
    JFileChooser chooser = new FileChooser(new File(manager.getPropertyUserDir()));
    ModelFileFilter modelFilter = new ModelFileFilter();
    chooser.setFileFilter(modelFilter);
    chooser.setPreferredSize(UIParameters.INSTANCE.getFileChooserDimension());
    int returnVal = chooser.showOpenDialog(ftfModelFile.getContainer());
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File modelFile = chooser.getSelectedFile();
      try {
        ftfModelFile.setText(modelFile.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  /**
   * synchronize when changing tabs
   * @param prevTab
   */
  final void synchronize(Tab prevTab) {
    pnlSectionTable.synchronize(prevTab, curTab);
  }

  /**
   * synchronize when saving to the .ejf file
   */
  final void synchronize() {
    pnlSectionTable.synchronize(curTab, null);
  }

  private void setToolTipText() {
    String text = "Enter the directory where you wish to place the joined tomogram.";
    ftfWorkingDir.setToolTipText(text);
    ftfWorkingDir.setToolTipText(text);
    ltfRootName.setToolTipText("Enter the root name for the joined tomogram.");
    text = "The size to which samples will be squeezed if they are bigger (default 1024).";
    ltfMidasLimit.setToolTipText(text);
    lblMidasLimit.setToolTipText(TooltipFormatter.INSTANCE.format(text));
    spinDensityRefSection
        .setToolTipText("Select a section to use as a reference for density scaling.");
    btnChangeSetup.setToolTipText("Press to redo an existing sample.");
    btnRevertToLastSetup.setToolTipText("Press to go back to the existing sample.");
    btnMakeSamples.setToolTipText("Press to make a sample.");
    btnOpenSampleAverages.setToolTipText("Press to the sample averages file in 3dmod.");
    btnOpenSample.setToolTipText("Press to the sample file in 3dmod.");
    btnOpenSample.setToolTipText("Press to the sample file in 3dmod.");
    cbsAlignmentRefSection
        .setCheckBoxToolTipText("Make a section the reference for alignment.  This means that the chosen section will not be transformed, and the other sections will be transformed into alignment with it.");
    cbsAlignmentRefSection
        .setSpinnerToolTipText("Choose a section to be the reference for alignment.  This means that it will not be transformed, and the other sections will be transformed into alignment with it.");
    btnGetMaxSize
        .setToolTipText("Compute the maximum size and offsets needed to contain the transformed images from all of the sections, given the current transformations.");
    ltfSizeInX
        .setToolTipText("The size in X parameter for the trial and final joined tomograms.");
    ltfSizeInY
        .setToolTipText("The size in Y parameter for the trial and final joined tomograms.");
    ltfShiftInX
        .setToolTipText("The X offset parameter for the trial and final joined tomograms.");
    ltfShiftInY
        .setToolTipText("The Y offset parameter for the trial and final joined tomograms.");
    cbLocalFits.setToolTipText("When running Xftoxg(1) on the primary alignment "
        + "transforms, run the program in its default mode, which does local "
        + "fits to 7 adjacent sections.  This option may eliminate unwanted "
        + "trends in data sets with many sections.  When it is not entered, "
        + "Xftoxg(1) is run with \"-nfit 0\", which computes a global " + "alignment.");
    text = "Slices to use when creating the trial joined tomogram.";
    spinUseEveryNSlices.setToolTipText(text);
    spinRejoinUseEveryNSlices.setToolTipText(text);
    text = "The binning to use when creating the trial joined tomogram.";
    spinTrialBinning.setToolTipText(text);
    spinRejoinTrialBinning.setToolTipText(text);
    btnTrialJoin.setToolTipText("Press to make a trial version of the joined tomogram.");
    b3bOpenTrialIn3dmod.setButtonToolTipText("Press to open the trial joined tomogram.");
    btnGetSubarea
        .setToolTipText("Press to get maximum size and shift from the trail joined tomogram using the rubber band functionality in 3dmod.");
    btnFinishJoin.setToolTipText("Press to make the joined tomogram.");
    b3bOpenIn3dmod.setButtonToolTipText("Press to open the joined tomogram in 3dmod.");
    cbRefineWithTrial
        .setToolTipText("Check to make the refining model using the trial join.");
    btnRefineJoin
        .setToolTipText("Press to refine the serial section join using a refining model.");
    btnMakeRefiningModel.setToolTipText("Press to create the refining model.");
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.XFJOINTOMO,
          AxisID.ONLY);
      ltfBoundariesToAnalyze.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          XfjointomoParam.BOUNDARIES_TO_ANALYZE_KEY));
      ltfObjectsToInclude.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          XfjointomoParam.OBJECTS_TO_INCLUDE));
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
    cbGap
        .setToolTipText("Check to allow the the final start and end values to change when the join is recreated.  "
            + "Uncheck keep the existing final start and end values");
    if (autodoc != null) {
      text = EtomoAutodoc.getTooltip(autodoc, XfjointomoParam.GAP_START_END_INC);
      if (text != null) {
        ltfGapStart.setToolTipText(text);
        ltfGapEnd.setToolTipText(text);
        ltfGapInc.setToolTipText(text);
      }
      text = EtomoAutodoc.getTooltip(autodoc, XfjointomoParam.POINTS_TO_FIT);
      if (text != null) {
        ltfPointsToFitMin.setToolTipText(text);
        ltfPointsToFitMax.setToolTipText(text);
      }
    }
    btnXfjointomo
        .setToolTipText("Press to run xfjointomo, "
            + "which computes transforms for aligning tomograms of serial sections from features modeled on an initial joined tomogram.");
    btnTransformAndViewModel
        .setToolTipText("Press to apply tranformations to the refining model and view the result.");
    btnRejoin
        .setToolTipText("Press to make the joined tomogram using the adjusted end and start values.");
    btnTrialRejoin
        .setToolTipText("Press to make a trial version of the joined tomogram using the adjusted end and start values.");
    ftfModelFile.setToolTipText("The model to transform.");
    ltfTransformedModel.setToolTipText("The transformed model.");
    btnTransformModel.setToolTipText("Press to transform the model");
    b3bOpenRejoinWithModel
        .setButtonToolTipText("Press to open the joined tomogram with the transformed model.");
    b3bOpenTrialRejoin
        .setButtonToolTipText("Press to open the trial joined tomogram with the transformed refining model.");
    b3bOpenRejoin
        .setButtonToolTipText("Press to open the joined tomogram with the transformed refining model.");
  }

  //
  // Action listener adapters
  //
  private final class JoinActionListener implements ActionListener {

    private final JoinDialog adaptee;

    private JoinActionListener(final JoinDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.action(event.getActionCommand(), null, null);
    }
  }

  class WorkingDirActionListener implements ActionListener {

    JoinDialog adaptee;

    WorkingDirActionListener(JoinDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.workingDirAction();
    }
  }

  private class ModelFileActionListener implements ActionListener {

    private JoinDialog adaptee;

    private ModelFileActionListener(JoinDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.modelFileAction();
    }
  }

  /**
   * Connect tab state changes to the appropriate dialog method
   */
  class TabChangeListener implements ChangeListener {
    JoinDialog adaptee;

    public TabChangeListener(JoinDialog dialog) {
      adaptee = dialog;
    }

    public void stateChanged(ChangeEvent event) {
      adaptee.changeTab(event);
    }
  }

  static final class Tab {
    private static final int SETUP_INDEX = 0;
    private static final int ALIGN_INDEX = 1;
    private static final int JOIN_INDEX = 2;
    private static final int MODEL_INDEX = 3;
    private static final int REJOIN_INDEX = 4;

    static final Tab SETUP = new Tab(SETUP_INDEX);
    static final Tab ALIGN = new Tab(ALIGN_INDEX);
    static final Tab JOIN = new Tab(JOIN_INDEX);
    static final Tab MODEL = new Tab(MODEL_INDEX);
    static final Tab REJOIN = new Tab(REJOIN_INDEX);

    private final int index;

    private Tab(int index) {
      this.index = index;
    }

    int getIndex() {
      return index;
    }

    /**
     * Get the tab associated with the index.  Default: SETUP
     * @param index
     * @return
     */
    static Tab getInstance(int index) {
      switch (index) {
      case SETUP_INDEX:
        return SETUP;
      case ALIGN_INDEX:
        return ALIGN;
      case JOIN_INDEX:
        return JOIN;
      case MODEL_INDEX:
        return MODEL;
      case REJOIN_INDEX:
        return REJOIN;
      default:
        return SETUP;
      }
    }
  }

}