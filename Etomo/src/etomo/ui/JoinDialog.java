package etomo.ui;

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
import javax.swing.ButtonGroup;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.JoinManager;
import etomo.comscript.FinishjoinParam;
import etomo.comscript.XfjointomoParam;
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
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.JoinMetaData;
import etomo.type.JoinScreenState;
import etomo.type.JoinState;
import etomo.type.Run3dmodMenuOptions;
import etomo.type.Transform;
import etomo.util.DatasetFiles;

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
public final class JoinDialog implements ContextMenu, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  public static final int SETUP_MODE = -1;
  public static final int SAMPLE_NOT_PRODUCED_MODE = -2;
  public static final int SAMPLE_PRODUCED_MODE = -3;
  public static final int CHANGING_SAMPLE_MODE = -4;
  public static final DialogType DIALOG_TYPE = DialogType.JOIN;

  public static final String REFINE_AUTO_ALIGNMENT_TEXT = "Refine Auto Alignment";
  public static final String MIDAS_TEXT = "Midas";
  public static final String FINISH_JOIN_TEXT = "Finish Join";
  public static final String WORKING_DIRECTORY_TEXT = "Working directory";
  public static final String GET_MAX_SIZE_TEXT = "Get Max Size and Shift";
  public static final String TRIAL_JOIN_TEXT = "Trial Join";
  public static final String REJOIN_TEXT = "Rejoin";
  public static final String TRIAL_REJOIN_TEXT = "Trial Rejoin";

  private static final String REFINE_JOIN_TEXT = "Refine Join";

  private static final String OPEN_IN_3DMOD = "Open in 3dmod";

  private static Dimension dimSpinner = UIParameters.INSTANCE
      .getSpinnerDimension();

  private JPanel rootPanel;
  private JTabbedPane tabPane;
  private SpacedPanel pnlSetup;
  private SectionTablePanel pnlSectionTable;
  private SpacedPanel pnlAlign;
  private SpacedPanel pnlJoin;
  private SpacedPanel setupPanel2;
  private SpacedPanel alignPanel1;
  private SpacedPanel alignPanel2;
  private SpacedPanel pnlXfalign;
  private SpacedPanel pnlFinishJoin;
  private SpacedPanel pnlMidasLimit = new SpacedPanel();

  private FileTextField ftfWorkingDir;
  private Run3dmodButton btnOpenSample;
  private MultiLineButton btnInitialAutoAlignment;
  private MultiLineButton btnMidas;
  private MultiLineButton btnRefineAutoAlignment;
  private MultiLineButton btnRevertToMidas;
  private MultiLineButton btnRevertToEmpty;
  private MultiLineButton btnGetMaxSize;
  private MultiLineButton btnGetSubarea;
  private MultiLineButton btnChangeSetup;
  private MultiLineButton btnRevertToLastSetup;

  private LabeledTextField ltfRootName;
  private LabeledTextField ltfSigmaLowFrequency;
  private LabeledTextField ltfCutoffHighFrequency;
  private LabeledTextField ltfSigmaHighFrequency;
  private LabeledTextField ltfSizeInX;
  private LabeledTextField ltfSizeInY;
  private LabeledTextField ltfShiftInX;
  private LabeledTextField ltfShiftInY;
  private LabeledTextField ltfMidasLimit = new LabeledTextField(
      "Squeeze samples to ");
  private JLabel lblMidasLimit = new JLabel("pixels if bigger.");
  private final TransformChooser tcAlign = new TransformChooser();
  private CheckBoxSpinner cbsAlignmentRefSection;
  private LabeledSpinner spinDensityRefSection;
  private LabeledSpinner spinTrialBinning;
  private LabeledSpinner spinRejoinTrialBinning;
  private LabeledSpinner spinUseEveryNSlices;
  private LabeledSpinner spinRejoinUseEveryNSlices;

  //state
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
  private final MultiLineButton btnRefineJoin = new MultiLineButton(
      REFINE_JOIN_TEXT);
  private final Run3dmodButton btnMakeRefiningModel = Run3dmodButton
      .get3dmodInstance("Make Refining Model", this);
  private final MultiLineButton btnXfjointomo = new MultiLineButton(
      "Find Transformations");

  private final BoundaryTable boundaryTable;
  private boolean refiningJoin = false;
  private SpacedPanel pnlTransformations = null;
  private final TransformChooser tcModel = new TransformChooser();
  private final LabeledTextField ltfBoundariesToAnalyze = new LabeledTextField(
      "Boundaries to analyze: ");
  private final LabeledTextField ltfObjectsToInclude = new LabeledTextField(
      "Objects to include: ");
  private final LabeledTextField ltfGapStart = new LabeledTextField("Start: ");
  private final LabeledTextField ltfGapEnd = new LabeledTextField("End: ");
  private final LabeledTextField ltfGapInc = new LabeledTextField("Increment: ");
  private final LabeledTextField ltfPointsToFitMin = new LabeledTextField(
      "Min: ");
  private final LabeledTextField ltfPointsToFitMax = new LabeledTextField(
      "Max: ");
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
  private final Run3dmodButton btnOpenSampleAverages = Run3dmodButton
      .get3dmodInstance("Open Sample Averages in 3dmod", this);
  private final Run3dmodButton btnMakeSamples = Run3dmodButton
      .getDeferred3dmodInstance("Make Samples", this, "averages in 3dmod");
  private final SpacedPanel pnlModel = new SpacedPanel(true);
  private final JPanel pnlRejoinTab = new JPanel();
  private final BinnedXY3dmodButton b3bOpenTrialIn3dmod = new BinnedXY3dmodButton(
      "Open Trial in 3dmod", this);
  private final Run3dmodButton btnTrialJoin = Run3dmodButton
      .getDeferred3dmodInstance(TRIAL_JOIN_TEXT, this);
  private final FileTextField ftfModelFile = new FileTextField("Model file: ");
  private final CheckBox cbGap = new CheckBox("Try gaps: ");
  private final BinnedXY3dmodButton b3bOpenIn3dmod = new BinnedXY3dmodButton(
      OPEN_IN_3DMOD, this);
  private final Run3dmodButton btnFinishJoin = Run3dmodButton
      .getDeferred3dmodInstance(FINISH_JOIN_TEXT, this);
  private final BinnedXY3dmodButton b3bOpenRejoin = new BinnedXY3dmodButton(
      "Open Rejoin in 3dmod", this);
  private final Run3dmodButton btnRejoin = Run3dmodButton
      .getDeferred3dmodInstance(REJOIN_TEXT, this);
  private final BinnedXY3dmodButton b3bOpenTrialRejoin = new BinnedXY3dmodButton(
      "Open Trial Rejoin in 3dmod", this);
  private final Run3dmodButton btnTrialRejoin = Run3dmodButton
      .getDeferred3dmodInstance(TRIAL_REJOIN_TEXT, this);

  /**
   * Create JoinDialog without an .ejf file
   * @param joinManager
   */
  public JoinDialog(JoinManager manager, ConstJoinMetaData metaData,
      JoinState state) {
    this(manager, null, metaData, state);
  }

  /**
   * Create JoinDialog with workingDirName equal to the location of the .ejf
   * file.
   * @param joinManager
   * @param workingDirName
   */
  public JoinDialog(JoinManager manager, String workingDirName,
      ConstJoinMetaData metaData, JoinState state) {
    this.state = state;
    axisID = AxisID.ONLY;
    this.manager = manager;
    boundaryTable = new BoundaryTable(manager, this);
    setRefiningJoin();
    createRootPanel(workingDirName);
    UIHarness.INSTANCE.pack(axisID, manager);
    addListeners();
    setMetaData(metaData);
    setScreenState(manager.getScreenState());
    init();
    setToolTipText();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  String paramString() {
    return "ltfRootName=" + ltfRootName + ",\nltfSigmaLowFrequency="
        + ltfSigmaLowFrequency + ",\nltfCutoffHighFrequency="
        + ltfCutoffHighFrequency + ",\nltfSigmaHighFrequency="
        + ltfSigmaHighFrequency + ",ltfSizeInX=" + ltfSizeInX
        + ",\nltfSizeInY=" + ltfSizeInY + ",ltfShiftInX=" + ltfShiftInX
        + ",\nltfShiftInY=" + ltfShiftInY + ",ltfMidasLimit=" + ltfMidasLimit
        + ",\nspinDensityRefSection=" + spinDensityRefSection
        + ",\nspinTrialBinning=" + spinTrialBinning + ",\nspinUseEveryNSlices="
        + spinUseEveryNSlices + ",\nnumSections" + numSections + ",curTab="
        + curTab + ",invalidReason" + invalidReason + ",\naxisID=" + axisID
        + "," + super.toString();
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
    btnInitialAutoAlignment.addActionListener(joinActionListener);
    btnMidas.addActionListener(joinActionListener);
    btnRefineAutoAlignment.addActionListener(joinActionListener);
    btnRevertToMidas.addActionListener(joinActionListener);
    btnRevertToEmpty.addActionListener(joinActionListener);
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
    tabPane = new JTabbedPane();
    //tabPane.addMouseListener(new GenericMouseAdapter(this));
    //TabChangeListener tabChangeListener = new TabChangeListener(this);
    //tabPane.addChangeListener(tabChangeListener);
    tabPane.setBorder(new BeveledBorder("Join").getBorder());
    createSetupPanel(workingDirName);
    tabPane.addTab("Setup", pnlSetup.getContainer());
    createAlignPanel();
    tabPane.addTab("Align", pnlAlign.getContainer());
    createJoinPanel();
    tabPane.addTab("Join", pnlJoin.getContainer());
    createModelPanel();
    tabPane.addTab("Model", pnlModel.getContainer());
    createRejoinPanel();
    tabPane.addTab("Rejoin", pnlRejoinTab);
    addPanelComponents(Tab.SETUP);
    updateDisplay();
  }

  public void updateDisplay() {
    //must be refining join to have access to model and rejoin tabs
    tabPane.setEnabledAt(Tab.MODEL.getIndex(), refiningJoin);
    tabPane.setEnabledAt(Tab.REJOIN.getIndex(), refiningJoin);
    //control gap text fields with checkbox
    boolean enable = cbGap.isSelected();
    ltfGapStart.setEnabled(enable);
    ltfGapEnd.setEnabled(enable);
    ltfGapInc.setEnabled(enable);
    //.join file must exist before it can be opened
    enable = DatasetFiles.getJoinFile(false, manager).exists();
    b3bOpenRejoinWithModel.setEnabled(enable);
    b3bOpenRejoin.setEnabled(enable);
    //Need boundary list to transform model
    enable = !state.isRefineStartListEmpty();
    btnTransformModel.setEnabled(enable);
    //Can't only refine if a join exists
    //A trial join is only refinable if it was created with all slices
    boolean trialJoinRefinable = DatasetFiles.getJoinFile(true, manager)
        .exists()
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
  private void addPanelComponents(Tab tab) {
    if (tab == Tab.SETUP) {
      addSetupPanelComponents();
    }
    else if (tab == Tab.ALIGN) {
      addAlignPanelComponents();
    }
    else if (tab == Tab.JOIN) {
      addJoinPanelComponents();
    }
    else if (tab == Tab.MODEL) {
      addModelPanelComponents();
    }
    else if (tab == Tab.REJOIN) {
      addRejoinPanelComponents();
    }
  }

  /**
   * Get rubberband coordinates and calculate a new size and shift based on
   * the last finishjoin trial.
   * @param coordinates
   */
  private void setSizeAndShift(Vector coordinates) {
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
      if (ImodProcess.RUBBERBAND_RESULTS_STRING.equals((String) coordinates
          .get(index++))) {
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
      pnlSetup.removeAll();
    }
    else if (tab == Tab.ALIGN) {
      pnlAlign.removeAll();
    }
    else if (tab == Tab.JOIN) {
      pnlJoin.removeAll();
    }
    else if (tab == Tab.MODEL) {
      pnlModel.removeAll();
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

  final boolean isRejoinTab() {
    return curTab == Tab.REJOIN;
  }

  Tab getTab() {
    return curTab;
  }

  int getSectionTableSize() {
    return pnlSectionTable.getRowsSize();
  }

  private final void changeTab(ChangeEvent event) {
    Tab prevTab = curTab;
    removePanelComponents(prevTab);
    curTab = Tab.getInstance(tabPane.getSelectedIndex());
    synchronize(prevTab);
    addPanelComponents(curTab);
    UIHarness.INSTANCE.pack(manager);
  }

  public void setInverted() throws LogFile.FileException {
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
    pnlSetup = new SpacedPanel();
    pnlSetup.setBoxLayout(BoxLayout.Y_AXIS);
    //first component
    ftfWorkingDir = new FileTextField(WORKING_DIRECTORY_TEXT + ": ");
    ftfWorkingDir.setText(workingDirName);
    //second component
    ltfRootName = new LabeledTextField("Root name for output file: ");
    //third component    
    pnlSectionTable = new SectionTablePanel(this, manager, state);
    //midas limit panel
    pnlMidasLimit.setBoxLayout(BoxLayout.X_AXIS);
    pnlMidasLimit.add(ltfMidasLimit.getContainer());
    pnlMidasLimit.add(lblMidasLimit);
    //fifth component
    SpinnerNumberModel spinnerModel = new SpinnerNumberModel(1, 1,
        numSections < 1 ? 1 : numSections, 1);
    spinDensityRefSection = new LabeledSpinner(
        "Reference section for density matching: ", spinnerModel);
    spinDensityRefSection.setTextMaxmimumSize(dimSpinner);
    //sixth component
    setupPanel2 = new SpacedPanel();
    setupPanel2.setBoxLayout(BoxLayout.X_AXIS);
    setupPanel2.setBorder(BorderFactory.createEtchedBorder());
    btnChangeSetup = new MultiLineButton("Change Setup");
    //btnChangeSetup.addActionListener(joinActionListener);
    btnChangeSetup.setSize();
    setupPanel2.add(btnChangeSetup);
    btnRevertToLastSetup = new MultiLineButton("Revert to Last Setup");
    setRevertState(true);
    //btnRevertToLastSetup.addActionListener(joinActionListener);
    btnRevertToLastSetup.setSize();
    setupPanel2.add(btnRevertToLastSetup);
    //seventh component
    btnMakeSamples.setDeferred3dmodButton(btnOpenSampleAverages);
    btnMakeSamples.setSize();
    btnMakeSamples.setAlignmentX(Component.CENTER_ALIGNMENT);
  }

  private void addSetupPanelComponents() {
    pnlSetup.add(ftfWorkingDir.getContainer());
    pnlSetup.add(ltfRootName);
    pnlSetup.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.displayCurTab();
    pnlSetup.add(pnlMidasLimit);
    pnlSetup.add(spinDensityRefSection);
    pnlSetup.add(setupPanel2);
    btnMakeSamples.setSize();
    pnlSetup.add(btnMakeSamples);
  }

  private void createAlignPanel() {
    pnlAlign = new SpacedPanel();
    pnlAlign.setBoxLayout(BoxLayout.Y_AXIS);
    //second component
    alignPanel1 = new SpacedPanel();
    alignPanel1.setBoxLayout(BoxLayout.X_AXIS);
    btnOpenSample = Run3dmodButton.get3dmodInstance("Open Sample in 3dmod",
        this);
    btnOpenSample.setSize();
    alignPanel1.add(btnOpenSample);
    btnOpenSampleAverages.setSize();
    alignPanel1.add(btnOpenSampleAverages);
    //third componentadd
    pnlXfalign = new SpacedPanel();
    pnlXfalign.setBoxLayout(BoxLayout.Y_AXIS);
    pnlXfalign.setBorder(new EtchedBorder("Auto Alignment Parameters")
        .getBorder());
    ltfSigmaLowFrequency = new LabeledTextField(
        "Sigma for low-frequency filter: ");
    pnlXfalign.add(ltfSigmaLowFrequency);
    ltfCutoffHighFrequency = new LabeledTextField(
        "Cutoff for high-frequency filter: ");
    pnlXfalign.add(ltfCutoffHighFrequency);
    ltfSigmaHighFrequency = new LabeledTextField(
        "Sigma for high-frequency filter: ");
    pnlXfalign.add(ltfSigmaHighFrequency);
    pnlXfalign.add(tcAlign.getContainer());
    //fourth component
    alignPanel2 = new SpacedPanel();
    alignPanel2.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel alignPanel2A = new SpacedPanel();
    alignPanel2A.setBoxLayout(BoxLayout.Y_AXIS);
    btnInitialAutoAlignment = new MultiLineButton("Initial Auto Alignment");
    btnInitialAutoAlignment.setSize();
    alignPanel2A.add(btnInitialAutoAlignment);
    btnMidas = new MultiLineButton(MIDAS_TEXT);
    btnMidas.setSize();
    alignPanel2A.add(btnMidas);
    btnRefineAutoAlignment = new MultiLineButton(REFINE_AUTO_ALIGNMENT_TEXT);
    btnRefineAutoAlignment.setSize();
    alignPanel2A.add(btnRefineAutoAlignment);
    alignPanel2.add(alignPanel2A);
    SpacedPanel alignPanel2B = new SpacedPanel();
    alignPanel2B.setBoxLayout(BoxLayout.Y_AXIS);
    alignPanel2B.setBorder(BorderFactory.createEtchedBorder());
    btnRevertToMidas = new MultiLineButton("Revert Auto Alignment to Midas");
    //btnRevertToMidas.addActionListener(joinActionListener);
    btnRevertToMidas.setSize();
    alignPanel2B.add(btnRevertToMidas);
    btnRevertToEmpty = new MultiLineButton("Revert to No Transforms");
    //btnRevertToEmpty.addActionListener(joinActionListener);
    btnRevertToEmpty.setSize();
    alignPanel2B.add(btnRevertToEmpty);
    alignPanel2.add(alignPanel2B);
  }

  private void addAlignPanelComponents() {
    //first component
    pnlAlign.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.displayCurTab();
    //second component
    pnlAlign.add(alignPanel1);
    //third component
    pnlAlign.add(pnlXfalign);
    //fourth component
    pnlAlign.add(alignPanel2);
  }

  private void createModelPanel() {
    //create model panel only once
    if (pnlTransformations != null) {
      return;
    }
    //construct panels
    pnlTransformations = new SpacedPanel();
    SpacedPanel pnlGapStartEndInc = new SpacedPanel();
    SpacedPanel pnlPointsToFit = new SpacedPanel();
    //initialize
    btnRefineJoin.setSize();
    //build panels
    pnlModel.setBoxLayout(BoxLayout.Y_AXIS);
    pnlModel.alignComponentsX(Component.CENTER_ALIGNMENT);
    //transformations panel
    pnlTransformations.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTransformations.setBorder(new EtchedBorder("Transformations")
        .getBorder());
    tcModel.includeTranslation();
    pnlTransformations.add(tcModel.getContainer());
    pnlTransformations.add(ltfBoundariesToAnalyze.getContainer());
    pnlTransformations.add(ltfObjectsToInclude.getContainer());
    pnlTransformations.add(pnlGapStartEndInc);
    pnlTransformations.add(pnlPointsToFit);
    btnXfjointomo.setSize();
    pnlTransformations.add(btnXfjointomo);
    btnTransformAndViewModel.setSize();
    pnlTransformations.add(btnTransformAndViewModel);
    //gap panel
    pnlGapStartEndInc.setBoxLayout(BoxLayout.X_AXIS);
    pnlGapStartEndInc.add(cbGap);
    pnlGapStartEndInc.add(ltfGapStart);
    pnlGapStartEndInc.add(ltfGapEnd);
    pnlGapStartEndInc.add(ltfGapInc.getContainer());
    //points to fit panel
    pnlPointsToFit.setBoxLayout(BoxLayout.X_AXIS);
    pnlPointsToFit.add(new JLabel("Points to fit: "));
    pnlPointsToFit.add(ltfPointsToFitMin);
    pnlPointsToFit.add(ltfPointsToFitMax);
  }

  private void addModelPanelComponents() {
    createModelPanel();
    btnMakeRefiningModel.setSize();
    pnlModel.add(btnMakeRefiningModel);
    pnlModel.add(boundaryTable.getContainer());
    boundaryTable.display();
    pnlModel.add(pnlTransformations);
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
    //use every spinner
    SpacedPanel pnlUseEvery = new SpacedPanel();
    pnlUseEvery.setBoxLayout(BoxLayout.X_AXIS);
    int zMax = pnlSectionTable.getZMax();
    SpinnerNumberModel spinnerModel = new SpinnerNumberModel(zMax < 1 ? 1
        : zMax < 10 ? zMax : 10, 1, zMax < 1 ? 1 : zMax, 1);
    spinRejoinUseEveryNSlices = new LabeledSpinner("Use every ", spinnerModel);
    spinRejoinUseEveryNSlices.setTextMaxmimumSize(dimSpinner);
    pnlUseEvery.add(spinRejoinUseEveryNSlices);
    pnlUseEvery.add(new JLabel("slices"));
    //trial rejoin button
    SpacedPanel pnlTrialRejoinButton = new SpacedPanel();
    pnlTrialRejoinButton.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTrialRejoinButton.setBorder(BorderFactory.createEtchedBorder());
    pnlTrialRejoinButton.add(pnlUseEvery.getContainer());
    spinnerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinRejoinTrialBinning = new LabeledSpinner("Binning in X and Y: ",
        spinnerModel);
    spinRejoinTrialBinning.setTextMaxmimumSize(dimSpinner);
    pnlTrialRejoinButton.add(spinRejoinTrialBinning.getContainer());
    btnTrialRejoin.setDeferred3dmodButton(b3bOpenTrialRejoin);
    btnTrialRejoin.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTrialRejoin.setSize();
    pnlTrialRejoinButton.add(btnTrialRejoin);
    //trial rejoin buttons
    JPanel pnlTrialRejoinButtons = new JPanel();
    pnlTrialRejoinButtons.setLayout(new BoxLayout(pnlTrialRejoinButtons,
        BoxLayout.X_AXIS));
    pnlTrialRejoinButtons.setBorder(new EtchedBorder("Trial Rejoin")
        .getBorder());
    pnlTrialRejoinButtons.add(Box.createHorizontalGlue());
    pnlTrialRejoinButtons.add(pnlTrialRejoinButton.getContainer());
    pnlTrialRejoinButtons.add(b3bOpenTrialRejoin.getContainer());
    pnlRejoin.add(pnlTrialRejoinButtons);
    //rejoin buttons
    JPanel pnlRejoinButtons = new JPanel();
    pnlRejoinButtons
        .setLayout(new BoxLayout(pnlRejoinButtons, BoxLayout.X_AXIS));
    pnlRejoinButtons.setBorder(new EtchedBorder("Final Rejoin").getBorder());
    pnlRejoinButtons.add(Box.createHorizontalGlue());
    pnlRejoinButtons.add(btnRejoin.getComponent());
    btnRejoin.setDeferred3dmodButton(b3bOpenRejoin);
    btnRejoin.setSize();
    pnlRejoinButtons.add(b3bOpenRejoin.getContainer());
    pnlRejoin.add(pnlRejoinButtons);
    //transform model
    SpacedPanel pnlTransformModel = new SpacedPanel();
    pnlTransformModel.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTransformModel
        .setBorder(new EtchedBorder("Transform Model").getBorder());
    pnlTransformModel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    pnlTransformModel.add(ftfModelFile.getContainer());
    ltfTransformedModel = new LabeledTextField("Output file: ");
    pnlTransformModel.add(ltfTransformedModel.getContainer());
    //transform model buttons
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
    pnlJoin = new SpacedPanel();
    pnlJoin.setBoxLayout(BoxLayout.X_AXIS);
    //second component
    createFinishJoinPanel();
  }

  private void addJoinPanelComponents() {
    //first component
    pnlJoin.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.displayCurTab();
    //second component
    pnlJoin.add(pnlFinishJoin);
  }

  private void createFinishJoinPanel() {
    pnlFinishJoin = new SpacedPanel();
    pnlFinishJoin.setBoxLayout(BoxLayout.Y_AXIS);
    pnlFinishJoin.setBorder(BorderFactory.createEtchedBorder());
    pnlFinishJoin.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    //first component
    cbsAlignmentRefSection = new CheckBoxSpinner(
        "Reference section for alignment: ");
    SpinnerModel spinnerModel = new SpinnerNumberModel(1, 1,
        numSections < 1 ? 1 : numSections, 1);
    cbsAlignmentRefSection.setModel(spinnerModel);
    cbsAlignmentRefSection.setMaximumSize(dimSpinner);
    pnlFinishJoin.add(cbsAlignmentRefSection.getContainer());
    //second component
    btnGetMaxSize = new MultiLineButton(GET_MAX_SIZE_TEXT);
    btnGetMaxSize.setSize();
    pnlFinishJoin.add(btnGetMaxSize);
    //third component
    SpacedPanel finishJoinPanel2 = new SpacedPanel();
    finishJoinPanel2.setBoxLayout(BoxLayout.X_AXIS);
    ltfSizeInX = new LabeledTextField("Size in X: ");
    finishJoinPanel2.add(ltfSizeInX);
    ltfSizeInY = new LabeledTextField("Y: ");
    finishJoinPanel2.add(ltfSizeInY);
    pnlFinishJoin.add(finishJoinPanel2);
    //fourth component
    SpacedPanel finishJoinPanel3 = new SpacedPanel();
    finishJoinPanel3.setBoxLayout(BoxLayout.X_AXIS);
    ltfShiftInX = new LabeledTextField("Shift in X: ");
    finishJoinPanel3.add(ltfShiftInX);
    ltfShiftInY = new LabeledTextField("Y: ");
    finishJoinPanel3.add(ltfShiftInY);
    pnlFinishJoin.add(finishJoinPanel3);
    //fifth component
    createTrialJoinPanel();
    //sixth component
    btnFinishJoin.setDeferred3dmodButton(b3bOpenIn3dmod);
    btnFinishJoin.setSize();
    pnlFinishJoin.add(btnFinishJoin);
    //seventh component
    b3bOpenIn3dmod
        .setSpinnerToolTipText("The binning to use when opening the joined tomogram in 3dmod.");
    pnlFinishJoin.add(b3bOpenIn3dmod.getContainer());
    //eight component
    pnlFinishJoin.add(cbRefineWithTrial);
    btnRefineJoin.setSize();
    pnlFinishJoin.add(btnRefineJoin);
  }

  private void createTrialJoinPanel() {
    SpacedPanel pnlTrialJoin = new SpacedPanel();
    pnlTrialJoin.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTrialJoin.setBorder(new EtchedBorder(TRIAL_JOIN_TEXT).getBorder());
    pnlTrialJoin.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    //first component
    SpacedPanel trialJoinPanel1 = new SpacedPanel();
    trialJoinPanel1.setBoxLayout(BoxLayout.X_AXIS);
    int zMax = pnlSectionTable.getZMax();
    SpinnerNumberModel spinnerModel = new SpinnerNumberModel(zMax < 1 ? 1
        : zMax < 10 ? zMax : 10, 1, zMax < 1 ? 1 : zMax, 1);
    spinUseEveryNSlices = new LabeledSpinner("Use every ", spinnerModel);
    spinUseEveryNSlices.setTextMaxmimumSize(dimSpinner);
    trialJoinPanel1.add(spinUseEveryNSlices);
    trialJoinPanel1.add(new JLabel("slices"));
    pnlTrialJoin.add(trialJoinPanel1);
    //second component
    spinnerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinTrialBinning = new LabeledSpinner("Binning in X and Y: ", spinnerModel);
    spinTrialBinning.setTextMaxmimumSize(dimSpinner);
    pnlTrialJoin.add(spinTrialBinning);
    //third component
    btnTrialJoin.setDeferred3dmodButton(b3bOpenTrialIn3dmod);
    btnTrialJoin.setSize();
    pnlTrialJoin.add(btnTrialJoin);
    //fourth component
    b3bOpenTrialIn3dmod
        .setSpinnerToolTipText("The binning to use when opening the trial joined tomogram in 3dmod.");
    pnlTrialJoin.add(b3bOpenTrialIn3dmod.getContainer());
    //fifth component
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
    //density matching (setup)
    EtomoNumber spinnerValue = new EtomoNumber(EtomoNumber.Type.INTEGER);
    spinnerValue.set(spinDensityRefSection.getValue());
    spinnerValue.setDisplayValue(1);
    SpinnerNumberModel spinnerModel = new SpinnerNumberModel(spinnerValue
        .getInt(), 1, numSections < 1 ? 1 : numSections, 1);
    spinDensityRefSection.setModel(spinnerModel);
    //alignment (join)
    spinnerValue.set((Integer) cbsAlignmentRefSection.getValue());
    spinnerModel = new SpinnerNumberModel(spinnerValue.getInt(), 1,
        numSections < 1 ? 1 : numSections, 1);
    cbsAlignmentRefSection.setModel(spinnerModel);
    //every n sections (join)
    int zMax = pnlSectionTable.getZMax();
    if (zMax == 0) {
      spinnerValue.set(1);
    }
    else {
      spinnerValue.setCeiling(zMax);
      spinnerValue.set((Integer) spinUseEveryNSlices.getValue());
    }
    spinnerValue.setDisplayValue(zMax < 1 ? 1 : zMax < 10 ? zMax : 10);
    spinnerModel = new SpinnerNumberModel(spinnerValue.getInt(), 1,
        zMax < 1 ? 1 : zMax, 1);
    spinUseEveryNSlices.setModel(spinnerModel);
    spinnerValue.set((Integer) spinRejoinUseEveryNSlices.getValue());
    spinnerModel = new SpinnerNumberModel(spinnerValue.getInt(), 1,
        zMax < 1 ? 1 : zMax, 1);
    spinRejoinUseEveryNSlices.setModel(spinnerModel);
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
  final void defaultSizeInXY() {
    //update size in X and Y defaults
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

  public ConstEtomoNumber getSizeInY() {
    EtomoNumber sizeInY = new EtomoNumber(EtomoNumber.Type.INTEGER);
    sizeInY.set(ltfSizeInY.getText());
    return sizeInY;
  }

  public ConstEtomoNumber getShiftInX() {
    EtomoNumber shiftInX = new EtomoNumber(EtomoNumber.Type.INTEGER);
    shiftInX.set(ltfShiftInX.getText());
    return shiftInX;
  }

  public ConstEtomoNumber getShiftInY() {
    EtomoNumber shiftInY = new EtomoNumber(EtomoNumber.Type.INTEGER);
    shiftInY.set(ltfShiftInY.getText());
    return shiftInY;
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

  public final int getMode() {
    return pnlSectionTable.getMode();
  }

  public void getParameters(XfjointomoParam param) {
    param.setTransform(tcModel.get());
    param.setBoundariesToAnalyze(ltfBoundariesToAnalyze.getText());
    param.setObjectsToInclude(ltfObjectsToInclude.getText());
    param.setPointsToFit(ltfPointsToFitMin.getText(), ltfPointsToFitMax
        .getText());
    if (cbGap.isSelected()) {
      param.setGapStartEndInc(ltfGapStart.getText(), ltfGapEnd.getText(),
          ltfGapInc.getText());
    }
  }

  public void setXfjointomoResult() throws LogFile.ReadException,
      LogFile.FileException {
    boundaryTable.setXfjointomoResult();
  }

  public boolean getMetaData(JoinMetaData metaData) {
    synchronize();
    metaData.setRootName(ltfRootName.getText());
    metaData.setDensityRefSection(spinDensityRefSection.getValue());
    metaData.setSigmaLowFrequency(ltfSigmaLowFrequency.getText());
    metaData.setCutoffHighFrequency(ltfCutoffHighFrequency.getText());
    metaData.setSigmaHighFrequency(ltfSigmaHighFrequency.getText());
    metaData.setAlignTransform(tcAlign.get());
    metaData.setUseAlignmentRefSection(cbsAlignmentRefSection.isSelected());
    metaData.setAlignmentRefSection(cbsAlignmentRefSection.getValue());
    metaData.setSizeInX(ltfSizeInX.getText());
    metaData.setSizeInY(ltfSizeInY.getText());
    metaData.setShiftInX(ltfShiftInX.getText());
    metaData.setShiftInY(ltfShiftInY.getText());
    metaData.setUseEveryNSlices(spinUseEveryNSlices.getValue());
    metaData.setRejoinUseEveryNSlices(spinRejoinUseEveryNSlices.getValue());
    metaData.setTrialBinning(spinTrialBinning.getValue());
    metaData.setMidasLimit(ltfMidasLimit.getText());
    metaData.setModelTransform(tcModel.get());
    metaData.setBoundariesToAnalyze(ltfBoundariesToAnalyze.getText());
    metaData.setObjectsToInclude(ltfObjectsToInclude.getText());
    metaData.setGap(cbGap.isSelected());
    metaData.setGapStart(ltfGapStart.getText());
    metaData.setGapEnd(ltfGapEnd.getText());
    metaData.setGapInc(ltfGapInc.getText());
    metaData.setPointsToFitMin(ltfPointsToFitMin.getText());
    metaData.setPointsToFitMax(ltfPointsToFitMax.getText());
    metaData.setRejoinTrialBinning(spinRejoinTrialBinning.getValue());
    boundaryTable.getMetaData();
    return pnlSectionTable.getMetaData(metaData);
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
    ltfSigmaLowFrequency.setText(metaData.getSigmaLowFrequency().toString());
    ltfCutoffHighFrequency
        .setText(metaData.getCutoffHighFrequency().toString());
    ltfSigmaHighFrequency.setText(metaData.getSigmaHighFrequency().toString());
    tcAlign.set(metaData.getAlignTransform());
    cbsAlignmentRefSection.setSelected(metaData.isUseAlignmentRefSection());
    cbsAlignmentRefSection.setValue(metaData.getAlignmentRefSection()
        .getNumber());
    ltfShiftInX.setText(metaData.getShiftInX().toString());
    ltfShiftInY.setText(metaData.getShiftInY().toString());
    spinUseEveryNSlices.setValue(metaData.getUseEveryNSlices().getNumber());
    spinRejoinUseEveryNSlices.setValue(metaData.getRejoinUseEveryNSlices()
        .getNumber());
    spinTrialBinning.setValue(metaData.getTrialBinning().getNumber());
    ltfMidasLimit.setText(metaData.getMidasLimit().toString());
    pnlSectionTable.setMetaData(metaData);
    ltfSizeInX.setText(metaData.getSizeInX().toString());
    ltfSizeInY.setText(metaData.getSizeInY().toString());
    tcModel.set(metaData.getModelTransform());
    ltfBoundariesToAnalyze.setText(metaData.getBoundariesToAnalyze());
    ltfObjectsToInclude.setText(metaData.getObjectsToInclude());
    cbGap.setSelected(metaData.getGap().is());
    updateDisplay();
    ltfGapStart.setText(metaData.getGapStart());
    ltfGapEnd.setText(metaData.getGapEnd());
    ltfGapInc.setText(metaData.getGapInc());
    ltfPointsToFitMin.setText(metaData.getPointsToFitMin());
    ltfPointsToFitMax.setText(metaData.getPointsToFitMax());
    spinRejoinTrialBinning.setValue(metaData.getRejoinTrialBinning()
        .getNumber());
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
    return new File(ftfWorkingDir.getText());
  }

  public String getRootName() {
    return ltfRootName.getText();
  }

  public void abortAddSection() {
    pnlSectionTable.enableAddSection();
  }

  public void enableMidas() {
    btnMidas.setEnabled(true);
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
    if (!metaData.getDensityRefSection().equals(
        (Number) spinDensityRefSection.getValue())) {
      return false;
    }
    if (!metaData.getSigmaLowFrequency().equals(ltfSigmaLowFrequency.getText())) {
      return false;
    }
    if (!metaData.getCutoffHighFrequency().equals(
        ltfCutoffHighFrequency.getText())) {
      return false;
    }
    if (!metaData.getSigmaHighFrequency().equals(
        ltfSigmaHighFrequency.getText())) {
      return false;
    }
    if (tcAlign.get() != metaData.getAlignTransform()) {
      return false;
    }
    if (cbsAlignmentRefSection.isSelected() != metaData
        .isUseAlignmentRefSection()) {
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
    if (!metaData.getUseEveryNSlices().equals(
        (Number) spinUseEveryNSlices.getValue())) {
      return false;
    }
    if (!metaData.getRejoinUseEveryNSlices().equals(
        (Number) spinRejoinUseEveryNSlices.getValue())) {
      return false;
    }
    if (!metaData.getTrialBinning()
        .equals((Number) spinTrialBinning.getValue())) {
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
    if (!metaData.getDensityRefSection().equals(
        (Number) spinDensityRefSection.getValue())) {
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
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage, logFileLabel,
          logFile, manager, axisID);
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
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage, logFileLabel,
          logFile, manager, axisID);
    }
  }

  /**
   * Handle actions
   * @param event
   */
  private void action(final String command,
      Deferred3dmodButton deferred3dmodButton,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnMakeSamples.getActionCommand())) {
      manager.makejoincom(null, deferred3dmodButton, run3dmodMenuOptions,
          DIALOG_TYPE);
    }
    else if (command.equals(btnInitialAutoAlignment.getActionCommand())) {
      btnMidas.setEnabled(false);
      manager.xfalignInitial(null);
    }
    else if (command.equals(btnMidas.getActionCommand())) {
      manager.midasSample();
    }
    else if (command.equals(btnRefineAutoAlignment.getActionCommand())) {
      btnMidas.setEnabled(false);
      manager.xfalignRefine(null);
    }
    else if (command.equals(btnRevertToMidas.getActionCommand())) {
      manager.revertXfFileToMidas();
    }
    else if (command.equals(btnRevertToEmpty.getActionCommand())) {
      manager.revertXfFileToEmpty();
    }
    else if (command.equals(btnFinishJoin.getActionCommand())) {
      manager.finishjoin(FinishjoinParam.Mode.FINISH_JOIN, FINISH_JOIN_TEXT,
          null, deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
    }
    else if (command.equals(btnGetMaxSize.getActionCommand())) {
      manager.finishjoin(FinishjoinParam.Mode.MAX_SIZE, GET_MAX_SIZE_TEXT,
          null, null, null, DIALOG_TYPE);
    }
    else if (command.equals(btnTrialJoin.getActionCommand())) {
      manager.finishjoin(FinishjoinParam.Mode.TRIAL, TRIAL_JOIN_TEXT, null,
          deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
    }
    else if (command.equals(btnGetSubarea.getActionCommand())) {
      setSizeAndShift(manager.imodGetRubberbandCoordinates(
          ImodManager.TRIAL_JOIN_KEY, AxisID.ONLY));
    }
    else if (command.equals(btnChangeSetup.getActionCommand())) {
      //Prepare for Revert:  meta data file should match the screen
      JoinMetaData metaData = manager.getJoinMetaData();
      getMetaData(metaData);
      try {
        ParameterStore parameterStore = manager.getParameterStore();
        if (parameterStore != null) {
          parameterStore.save(metaData);
        }
      }
      catch (LogFile.FileException e) {
        UIHarness.INSTANCE.openMessageDialog("Unable to save JoinMetaData.\n"
            + e.getMessage(), "Etomo Error");
      }
      catch (LogFile.WriteException e) {
        UIHarness.INSTANCE.openMessageDialog("Unable to write JoinMetaData.\n"
            + e.getMessage(), "Etomo Error");
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
      manager.finishjoin(FinishjoinParam.Mode.TRIAL_REJOIN, TRIAL_REJOIN_TEXT,
          null, deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
    }
    else if (command.equals(cbGap.getActionCommand())) {
      updateDisplay();
    }
    else if (command.equals(btnTransformModel.getActionCommand())) {
      manager.xfmodel(ftfModelFile.getText(), ltfTransformedModel.getText(),
          null, deferred3dmodButton, run3dmodMenuOptions, DIALOG_TYPE);
    }
    else if (command.equals(btnTransformAndViewModel.getActionCommand())) {
      manager.finishjoin(FinishjoinParam.Mode.SUPPRESS_EXECUTION, REJOIN_TEXT,
          null, null, null, DIALOG_TYPE);
    }
    else if (command.equals(btnOpenSample.getActionCommand())) {
      manager.imodOpen(ImodManager.JOIN_SAMPLES_KEY, run3dmodMenuOptions);
    }
    else if (command.equals(btnOpenSampleAverages.getActionCommand())) {
      manager.imodOpen(ImodManager.JOIN_SAMPLE_AVERAGES_KEY,
          run3dmodMenuOptions);
    }
    else if (command.equals(b3bOpenIn3dmod.getActionCommand())) {
      manager.imodOpen(ImodManager.JOIN_KEY, b3bOpenIn3dmod.getInt(),
          run3dmodMenuOptions);
    }
    else if (command.equals(b3bOpenTrialIn3dmod.getActionCommand())) {
      manager.imodOpen(ImodManager.TRIAL_JOIN_KEY,
          b3bOpenTrialIn3dmod.getInt(), run3dmodMenuOptions);
    }
    else if (command.equals(btnMakeRefiningModel.getActionCommand())) {
      manager.imodOpen(ImodManager.MODELED_JOIN_KEY, DatasetFiles
          .getRefineModelFileName(manager), run3dmodMenuOptions);
    }
    else if (command.equals(b3bOpenRejoin.getActionCommand())) {
      manager.imodOpen(ImodManager.JOIN_KEY, b3bOpenRejoin.getInt(),
          DatasetFiles.getRefineAlignedModelFileName(manager),
          run3dmodMenuOptions);
    }
    else if (command.equals(b3bOpenTrialRejoin.getActionCommand())) {
      ConstEtomoNumber useEveryNSlices = state.getRefineTrialUseEveryNSlices();
      if (useEveryNSlices.isNull() || useEveryNSlices.gt(1)) {
        //don't open the model if all the slices have not been included
        manager.imodOpen(ImodManager.TRIAL_JOIN_KEY, b3bOpenTrialRejoin
            .getInt(), run3dmodMenuOptions);
      }
      else {
        manager.imodOpen(ImodManager.TRIAL_JOIN_KEY, b3bOpenTrialRejoin
            .getInt(), DatasetFiles.getRefineAlignedModelFileName(manager),
            run3dmodMenuOptions);
      }
    }
    else if (command.equals(b3bOpenRejoinWithModel.getActionCommand())) {
      manager.setDebug(true);
      manager.imodOpen(ImodManager.JOIN_KEY, b3bOpenRejoinWithModel.getInt(),
          ltfTransformedModel.getText(), run3dmodMenuOptions);
      manager.setDebug(false);
    }
    else {
      throw new IllegalStateException("Unknown command " + command);
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
    String joinFileName = DatasetFiles.getJoinFileName(useTrial, manager);
    String buttonName = useTrial ? TRIAL_JOIN_TEXT : FINISH_JOIN_TEXT;
    //make sure the file to be moved exists
    if (!DatasetFiles.getJoinFile(useTrial, manager).exists()) {
      UIHarness.INSTANCE.openMessageDialog(joinFileName
          + " does not exist.  Press " + buttonName + " to create it.",
          "Failed File Move");
      return;
    }
    //Make sure that there is data available about the .join file which will be
    //used in the refine.  If there is not, ask for the user's assurance that
    //data on the screen corresponds to the .join file.
    boolean convertVersion = false;
    if (!state.isJoinVersionGe(useTrial, JoinState.MIN_REFINE_VERSION)) {
      setRefineDataHighlight(true);
      convertVersion = UIHarness.INSTANCE
          .openYesNoDialog(
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
    //The _modeled.join file already exists and there is a model that may be
    //associated with it.  Warn the user before overwriting it.
    if (DatasetFiles.getModeledJoinFile(manager).exists()
        && DatasetFiles.getRefineModelFile(manager).exists()) {
      if (!UIHarness.INSTANCE.openYesNoDialog(
          "The modeled join file and the refine model already exist.\n"
              + "Press Yes to overwrite the modeled join file.  "
              + "IMPORTANT:  "
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
    if (manager.isImodOpen(imodKey)) {
      UIHarness.INSTANCE.openMessageDialog("Please close the " + joinFileName
          + " file in 3dmod.  This file will be moved to "
          + DatasetFiles.getModeledJoinFileName(manager) + '.', "Close 3dmod");
      return;
    }
    try {
      //move the join file (or trial join) to the _modeled.join file
      LogFile.getInstance(manager.getPropertyUserDir(), joinFileName).move(
          LogFile.getInstance(manager.getPropertyUserDir(), DatasetFiles
              .getModeledJoinFileName(manager)));
    }
    catch (LogFile.FileException e) {
      e.printStackTrace();
      UIHarness.INSTANCE.openMessageDialog("Unable to move join file.\n"
          + e.getMessage(), "Failed File Move");
      return;
    }
    //convertVersion is true when the .ejf file is an older version and the user
    //wishes to use screen values to convert it.
    if (convertVersion) {
      if (!pnlSectionTable.getMetaData(manager.getJoinMetaData())) {
        UIHarness.INSTANCE.openMessageDialog(
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
    //  Open up the file chooser in the current working directory
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
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
    //  Open up the file chooser in the current working directory
    JFileChooser chooser = new JFileChooser(new File(manager
        .getPropertyUserDir()));
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
    btnRevertToLastSetup
        .setToolTipText("Press to go back to the existing sample.");
    btnMakeSamples.setToolTipText("Press to make a sample.");
    btnOpenSampleAverages
        .setToolTipText("Press to the sample averages file in 3dmod.");
    btnOpenSample.setToolTipText("Press to the sample file in 3dmod.");
    btnOpenSample.setToolTipText("Press to the sample file in 3dmod.");
    ltfSigmaLowFrequency
        .setToolTipText("Sigma of an inverted gaussian for filtering out low frequencies before searching for transformation.");
    ltfCutoffHighFrequency
        .setToolTipText("Starting radius of a gaussian for filtering out high frequencies before searching for transformation.");
    ltfSigmaHighFrequency
        .setToolTipText("Sigma of gaussian for filtering out high frequencies before searching for transformation.");
    btnInitialAutoAlignment
        .setToolTipText("OPTIONAL:  Run xfalign.  Find preliminary translational alignments with tiltxcorr rather then using an existing .xf file.");
    btnMidas
        .setToolTipText("Open Midas to check the output of the auto alignment and to make transformations by hand.");
    btnRefineAutoAlignment
        .setToolTipText("OPTIONAL:  Run xfalign using preliminary alignments created by the most recent use of Midas or xfalign.");
    btnRevertToMidas
        .setToolTipText("Use to ignore xfalign changes.  Returns transformations to the state created by the most recent save done in Midas.");
    btnRevertToEmpty.setToolTipText("Use to remove all transformations.");
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
    text = "Slices to use when creating the trial joined tomogram.";
    spinUseEveryNSlices.setToolTipText(text);
    spinRejoinUseEveryNSlices.setToolTipText(text);
    text = "The binning to use when creating the trial joined tomogram.";
    spinTrialBinning.setToolTipText(text);
    spinRejoinTrialBinning.setToolTipText(text);
    btnTrialJoin
        .setToolTipText("Press to make a trial version of the joined tomogram.");
    b3bOpenTrialIn3dmod
        .setButtonToolTipText("Press to open the trial joined tomogram.");
    btnGetSubarea
        .setToolTipText("Press to get maximum size and shift from the trail joined tomogram using the rubber band functionality in 3dmod.");
    btnFinishJoin.setToolTipText("Press to make the joined tomogram.");
    b3bOpenIn3dmod
        .setButtonToolTipText("Press to open the joined tomogram in 3dmod.");
    cbRefineWithTrial
        .setToolTipText("Check to make the refining model using the trial join.");
    btnRefineJoin
        .setToolTipText("Press to refine the serial section join using a refining model.");
    btnMakeRefiningModel.setToolTipText("Press to create the refining model.");
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.XFJOINTOMO,
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
    catch (LogFile.ReadException except) {
      except.printStackTrace();
    }
    cbGap
        .setToolTipText("Check to allow the the final start and end values to change when the join is recreated.  "
            + "Uncheck keep the existing final start and end values");
    if (autodoc != null) {
      text = EtomoAutodoc
          .getTooltip(autodoc, XfjointomoParam.GAP_START_END_INC);
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
  //  Action listener adapters
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

  private static final class TransformChooser {
    private final RadioButton rbFullLinearTransformation = new RadioButton(
        "Full linear transformation");
    private final RadioButton rbRotationTranslationMagnification = new RadioButton(
        "Rotation/translation/magnification");
    private final RadioButton rbRotationTranslation = new RadioButton(
        "Rotation/translation");
    private final RadioButton rbTranslation = new RadioButton("Translation");

    private JPanel pnlTranslationChooser = null;
    private boolean includeTranslation = false;

    Container getContainer() {
      if (pnlTranslationChooser == null) {
        pnlTranslationChooser = new JPanel();
        pnlTranslationChooser.setLayout(new BoxLayout(pnlTranslationChooser,
            BoxLayout.Y_AXIS));
        pnlTranslationChooser.add(new JLabel("Search For:"));
        ButtonGroup group = new ButtonGroup();
        group.add(rbFullLinearTransformation.getAbstractButton());
        group.add(rbRotationTranslationMagnification.getAbstractButton());
        group.add(rbRotationTranslation.getAbstractButton());
        pnlTranslationChooser.add(rbFullLinearTransformation.getComponent());
        pnlTranslationChooser.add(rbRotationTranslationMagnification
            .getComponent());
        pnlTranslationChooser.add(rbRotationTranslation.getComponent());
        //set default
        set(null);
        rbFullLinearTransformation
            .setToolTipText("Use rotation, translation, magnification, and stretching to align images.");
        rbRotationTranslationMagnification
            .setToolTipText("Use translation, rotation, and magnification to align images.");
        rbRotationTranslation
            .setToolTipText("Use translation and rotation to align images.");
        if (includeTranslation) {
          group.add(rbTranslation.getAbstractButton());
          pnlTranslationChooser.add(rbTranslation.getComponent());
          rbTranslation.setToolTipText("Use translation to align images.");
        }
      }
      return pnlTranslationChooser;
    }

    Transform get() {
      if (rbFullLinearTransformation.isSelected()) {
        return Transform.FULL_LINEAR_TRANSFORMATION;
      }
      if (rbRotationTranslationMagnification.isSelected()) {
        return Transform.ROTATION_TRANSLATION_MAGNIFICATION;
      }
      if (rbRotationTranslation.isSelected()) {
        return Transform.ROTATION_TRANSLATION;
      }
      if (rbTranslation.isSelected()) {
        return Transform.TRANSLATION;
      }
      return JoinMetaData.TRANSFORM_DEFAULT;
    }

    void set(Transform transform) {
      if (transform == null) {
        transform = JoinMetaData.TRANSFORM_DEFAULT;
      }
      if (transform == Transform.FULL_LINEAR_TRANSFORMATION) {
        rbFullLinearTransformation.setSelected(true);
      }
      else if (transform == Transform.ROTATION_TRANSLATION_MAGNIFICATION) {
        rbRotationTranslationMagnification.setSelected(true);
      }
      else if (transform == Transform.ROTATION_TRANSLATION) {
        rbRotationTranslation.setSelected(true);
      }
      else if (transform == Transform.TRANSLATION) {
        rbTranslation.setSelected(true);
      }
    }

    void includeTranslation() {
      includeTranslation = true;
    }
  }
}