package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.*;
import java.io.File;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.JoinManager;
import etomo.comscript.FinishjoinParam;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstJoinMetaData;
import etomo.type.EtomoNumber;
import etomo.type.JoinMetaData;
import etomo.type.JoinState;
import etomo.type.Run3dmodMenuOptions;

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
public class JoinDialog implements ContextMenu, Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  public static final int SETUP_TAB = 0;
  public static final int ALIGN_TAB = 1;
  public static final int JOIN_TAB = 2;

  public static final int SETUP_MODE = -1;
  public static final int SAMPLE_NOT_PRODUCED_MODE = -2;
  public static final int SAMPLE_PRODUCED_MODE = -3;
  public static final int CHANGING_SAMPLE_MODE = -4;

  public static final String REFINE_AUTO_ALIGNMENT_TEXT = "Refine Auto Alignment";
  public static final String MIDAS_TEXT = "Midas";
  public static final String FINISH_JOIN_TEXT = "Finish Join";
  public static final String WORKING_DIRECTORY_TEXT = "Working directory";
  public static final String GET_MAX_SIZE_TEXT = "Get Max Size and Shift";
  public static final String TRIAL_JOIN_TEXT = "Trial Join";

  static final String OPEN_BINNED_BY = "Open binned by ";

  private static final String OPEN_IN_3DMOD = "Open in 3dmod";
  private static final String IN_X_AND_Y = "in X and Y";

  private static ImageIcon iconFolder = new ImageIcon(ClassLoader
      .getSystemResource("images/openFile.gif"));
  private static Dimension dimSpinner = UIParameters.getSpinnerDimension();

  private JPanel rootPanel;
  private JTabbedPane tabPane;
  private SpacedPanel pnlSetup;
  private SectionTablePanel pnlSectionTable;
  private SpacedPanel pnlAlign;
  private SpacedPanel pnlJoin;
  private SpacedPanel setupPanel1;
  private SpacedPanel setupPanel2;
  private SpacedPanel alignPanel1;
  private SpacedPanel alignPanel2;
  private SpacedPanel pnlXfalign;
  private SpacedPanel pnlFinishJoin;
  private SpacedPanel pnlMidasLimit = new SpacedPanel();

  private JButton btnWorkingDir;
  private MultiLineButton btnMakeSamples;
  private Run3dmodButton btnOpenSample;
  private Run3dmodButton btnOpenSampleAverages;
  private MultiLineButton btnInitialAutoAlignment;
  private MultiLineButton btnMidas;
  private MultiLineButton btnRefineAutoAlignment;
  private MultiLineButton btnRevertToMidas;
  private MultiLineButton btnRevertToEmpty;
  private MultiLineButton btnGetMaxSize;
  private MultiLineButton btnTrialJoin;
  private Run3dmodButton btnOpenTrialIn3dmod;
  private MultiLineButton btnGetSubarea;
  private MultiLineButton btnFinishJoin;
  private Run3dmodButton btnOpenIn3dmod;
  private MultiLineButton btnChangeSetup;
  private MultiLineButton btnRevertToLastSetup;

  private LabeledTextField ltfWorkingDir;
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
  private RadioButton rbFullLinearTransformation;
  private RadioButton rbRotationTranslationMagnification;
  private RadioButton rbRotationTranslation;
  private CheckBox cbUseAlignmentRefSection;
  private JSpinner spinAlignmentRefSection;
  private LabeledSpinner spinDensityRefSection;
  private LabeledSpinner spinTrialBinning;
  private LabeledSpinner spinOpenBinnedBy;
  private LabeledSpinner spinOpenTrialBinnedBy;
  private LabeledSpinner spinUseEveryNSlices;

  //state
  private int numSections = 0;
  private int curTab = SETUP_TAB;
  private String invalidReason = null;

  private JoinActionListener joinActionListener = new JoinActionListener(this);
  private WorkingDirActionListener workingDirActionListener = new WorkingDirActionListener(
      this);
  private UseAlignmentRefSectionActionListener useAlignmentRefSectionActionListener = new UseAlignmentRefSectionActionListener(
      this);

  private final AxisID axisID;
  private final JoinManager joinManager;

  private int defaultXSize = 0;
  private int defaultYSize = 0;

  /**
   * Create JoinDialog without an .ejf file
   * @param joinManager
   */
  public JoinDialog(JoinManager joinManager, ConstJoinMetaData metaData) {
    this(joinManager, null, metaData);
  }

  /**
   * Create JoinDialog with workingDirName equal to the location of the .ejf
   * file.
   * @param joinManager
   * @param workingDirName
   */
  public JoinDialog(JoinManager joinManager, String workingDirName,
      ConstJoinMetaData metaData) {
    axisID = AxisID.ONLY;
    this.joinManager = joinManager;
    createRootPanel(workingDirName);
    UIHarness.INSTANCE.pack(axisID, joinManager);
    addListeners();
    setMetaData(metaData);
    init();
    setToolTipText();
  }

  public String toString() {
    return getClass().getName() + "[" + paramString() + "]";
  }

  protected String paramString() {
    return "ltfWorkingDir=" + ltfWorkingDir + ",ltfRootName=" + ltfRootName
        + ",\nltfSigmaLowFrequency=" + ltfSigmaLowFrequency
        + ",\nltfCutoffHighFrequency=" + ltfCutoffHighFrequency
        + ",\nltfSigmaHighFrequency=" + ltfSigmaHighFrequency + ",ltfSizeInX="
        + ltfSizeInX + ",\nltfSizeInY=" + ltfSizeInY + ",ltfShiftInX="
        + ltfShiftInX + ",\nltfShiftInY=" + ltfShiftInY + ",ltfMidasLimit="
        + ltfMidasLimit + ",\nrbFullLinearTransformation="
        + rbFullLinearTransformation + ",\nrbRotationTranslationMagnification="
        + rbRotationTranslationMagnification + ",\nrbRotationTranslation="
        + rbRotationTranslation + ",\ncbUseAlignmentRefSection="
        + cbUseAlignmentRefSection + ",\nspinAlignmentRefSection="
        + spinAlignmentRefSection + ",\nspinDensityRefSection="
        + spinDensityRefSection + ",\nspinTrialBinning=" + spinTrialBinning
        + ",\nspinOpenBinnedBy=" + spinOpenBinnedBy + ",spinOpenTrialBinnedBy"
        + spinOpenTrialBinnedBy + ",\nspinUseEveryNSlices="
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
    btnWorkingDir.addActionListener(workingDirActionListener);
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
    cbUseAlignmentRefSection
        .addActionListener(useAlignmentRefSectionActionListener);
    btnGetMaxSize.addActionListener(joinActionListener);
    btnFinishJoin.addActionListener(joinActionListener);
    btnOpenIn3dmod.addActionListener(joinActionListener);
    btnTrialJoin.addActionListener(joinActionListener);
    btnOpenTrialIn3dmod.addActionListener(joinActionListener);
    btnGetSubarea.addActionListener(joinActionListener);
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
    addPanelComponents(SETUP_TAB);
  }

  /**
   * Add components to the current tab
   * @param tab
   */
  private void addPanelComponents(int tab) {
    if (tab == SETUP_TAB) {
      addSetupPanelComponents();
    }
    else if (tab == ALIGN_TAB) {
      addAlignPanelComponents();
    }
    else if (tab == JOIN_TAB) {
      addJoinPanelComponents();
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
    EtomoNumber estXMin = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    EtomoNumber estYMin = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    EtomoNumber estXMax = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    EtomoNumber estYMax = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
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
    ConstJoinMetaData metaData = joinManager.getConstMetaData();
    JoinState state = joinManager.getState();
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

  private void removePanelComponents(int tab) {
    if (tab == SETUP_TAB) {
      pnlSetup.removeAll();
    }
    else if (tab == ALIGN_TAB) {
      pnlAlign.removeAll();
    }
    else if (tab == JOIN_TAB) {
      pnlJoin.removeAll();
    }
  }

  final boolean isSetupTab() {
    return curTab == SETUP_TAB;
  }

  final boolean isAlignTab() {
    return curTab == ALIGN_TAB;
  }

  final boolean isJoinTab() {
    return curTab == JOIN_TAB;
  }

  final void changeTab(ChangeEvent event) {
    int prevTab = curTab;
    removePanelComponents(prevTab);
    curTab = tabPane.getSelectedIndex();
    synchronize(prevTab);
    addPanelComponents(curTab);
    UIHarness.INSTANCE.pack(joinManager);
  }

  public final void setMode(int mode) {
    if (mode == SETUP_MODE) {
      ltfWorkingDir.setEnabled(true);
      btnWorkingDir.setEnabled(true);
      ltfRootName.setEnabled(true);
    }
    else {
      ltfWorkingDir.setEnabled(false);
      btnWorkingDir.setEnabled(false);
      ltfRootName.setEnabled(false);
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
      btnRevertToLastSetup.setEnabled(false);
      btnMakeSamples.setEnabled(true);
      break;
    case SAMPLE_PRODUCED_MODE:
      tabPane.setEnabledAt(1, true);
      tabPane.setEnabledAt(2, true);
      ltfMidasLimit.setEnabled(false);
      lblMidasLimit.setEnabled(false);
      spinDensityRefSection.setEnabled(false);
      btnChangeSetup.setEnabled(true);
      btnRevertToLastSetup.setEnabled(false);
      btnMakeSamples.setEnabled(false);
      break;
    case CHANGING_SAMPLE_MODE:
      tabPane.setEnabledAt(1, false);
      tabPane.setEnabledAt(2, false);
      ltfMidasLimit.setEnabled(true);
      lblMidasLimit.setEnabled(true);
      spinDensityRefSection.setEnabled(true);
      btnChangeSetup.setEnabled(false);
      btnRevertToLastSetup.setEnabled(true);
      btnMakeSamples.setEnabled(true);
      break;
    default:
      throw new IllegalStateException("mode=" + mode);
    }
    pnlSectionTable.setMode(mode);
  }

  private void createSetupPanel(String workingDirName) {
    pnlSetup = new SpacedPanel();
    pnlSetup.setBoxLayout(BoxLayout.Y_AXIS);
    //first component
    setupPanel1 = new SpacedPanel();
    setupPanel1.setBoxLayout(BoxLayout.X_AXIS);
    ltfWorkingDir = new LabeledTextField(WORKING_DIRECTORY_TEXT + ": ");
    ltfWorkingDir.setText(workingDirName);
    setupPanel1.add(ltfWorkingDir);
    btnWorkingDir = new JButton(iconFolder);
    btnWorkingDir.setPreferredSize(FixedDim.folderButton);
    btnWorkingDir.setMaximumSize(FixedDim.folderButton);
    //btnWorkingDir.addActionListener(workingDirActionListener);
    setupPanel1.add(btnWorkingDir);
    //second component
    ltfRootName = new LabeledTextField("Root name for output file: ");
    //third component    
    pnlSectionTable = new SectionTablePanel(this, joinManager);
    //midas limit panel
    pnlMidasLimit.setBoxLayout(BoxLayout.X_AXIS);
    pnlMidasLimit.add(ltfMidasLimit.getContainer());
    pnlMidasLimit.add(lblMidasLimit);
    //fifth component
    SpinnerModel spinnerModel = new SpinnerNumberModel(1, 1,
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
    setupPanel2.add(btnChangeSetup);
    btnRevertToLastSetup = new MultiLineButton("Revert to Last Setup");
    //btnRevertToLastSetup.addActionListener(joinActionListener);
    setupPanel2.add(btnRevertToLastSetup);
    //seventh component
    btnMakeSamples = new MultiLineButton("Make Samples");
    //btnMakeSamples.addActionListener(joinActionListener);
    btnMakeSamples.setSize();
    btnMakeSamples.setAlignmentX(Component.CENTER_ALIGNMENT);
  }

  private void addSetupPanelComponents() {
    pnlSetup.add(setupPanel1);
    pnlSetup.add(ltfRootName);
    pnlSetup.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.displayCurTab();
    pnlSetup.add(pnlMidasLimit);
    pnlSetup.add(spinDensityRefSection);
    pnlSetup.add(setupPanel2);
    pnlSetup.add(btnMakeSamples);
  }

  private void createAlignPanel() {
    pnlAlign = new SpacedPanel();
    pnlAlign.setBoxLayout(BoxLayout.Y_AXIS);
    //second component
    alignPanel1 = new SpacedPanel();
    alignPanel1.setBoxLayout(BoxLayout.X_AXIS);
    btnOpenSample = new Run3dmodButton("Open Sample in 3dmod", this);
    //btnOpenSample.addActionListener(joinActionListener);
    btnOpenSampleAverages = new Run3dmodButton("Open Sample Averages in 3dmod",
        this);
    //btnOpenSampleAverages.addActionListener(joinActionListener);
    alignPanel1.add(btnOpenSample);
    alignPanel1.add(btnOpenSampleAverages);
    //third component
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
    ButtonGroup bgSearchFor = new ButtonGroup();
    rbFullLinearTransformation = new RadioButton("Full linear transformation");
    rbRotationTranslationMagnification = new RadioButton(
        "Rotation/translation/magnification");
    rbRotationTranslation = new RadioButton("Rotation/translation");
    bgSearchFor.add(rbFullLinearTransformation);
    bgSearchFor.add(rbRotationTranslationMagnification);
    bgSearchFor.add(rbRotationTranslation);
    pnlXfalign.add(new JLabel("Search For:"));
    pnlXfalign.add(rbFullLinearTransformation);
    pnlXfalign.add(rbRotationTranslationMagnification);
    pnlXfalign.add(rbRotationTranslation);
    //fourth component
    alignPanel2 = new SpacedPanel();
    alignPanel2.setBoxLayout(BoxLayout.X_AXIS);
    SpacedPanel alignPanel2A = new SpacedPanel();
    alignPanel2A.setBoxLayout(BoxLayout.Y_AXIS);
    btnInitialAutoAlignment = new MultiLineButton("Initial Auto Alignment");
    alignPanel2A.add(btnInitialAutoAlignment);
    btnMidas = new MultiLineButton(MIDAS_TEXT);
    alignPanel2A.add(btnMidas);
    btnRefineAutoAlignment = new MultiLineButton(REFINE_AUTO_ALIGNMENT_TEXT);
    alignPanel2A.add(btnRefineAutoAlignment);
    alignPanel2.add(alignPanel2A);
    SpacedPanel alignPanel2B = new SpacedPanel();
    alignPanel2B.setBoxLayout(BoxLayout.Y_AXIS);
    alignPanel2B.setBorder(BorderFactory.createEtchedBorder());
    btnRevertToMidas = new MultiLineButton("Revert Auto Alignment to Midas");
    //btnRevertToMidas.addActionListener(joinActionListener);
    alignPanel2B.add(btnRevertToMidas);
    btnRevertToEmpty = new MultiLineButton("Revert to No Transforms");
    //btnRevertToEmpty.addActionListener(joinActionListener);
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
    JPanel finishJoinPanel1 = new JPanel();
    finishJoinPanel1
        .setLayout(new BoxLayout(finishJoinPanel1, BoxLayout.X_AXIS));
    cbUseAlignmentRefSection = new CheckBox(
        "Reference section for alignment: ");
    //cbUseAlignmentRefSection.addActionListener(useAlignmentRefSectionActionListener);
    finishJoinPanel1.add(cbUseAlignmentRefSection);
    SpinnerModel spinnerModel = new SpinnerNumberModel(1, 1,
        numSections < 1 ? 1 : numSections, 1);
    spinAlignmentRefSection = new JSpinner();
    spinAlignmentRefSection.setModel(spinnerModel);
    spinAlignmentRefSection.setMaximumSize(dimSpinner);
    finishJoinPanel1.add(spinAlignmentRefSection);
    pnlFinishJoin.add(finishJoinPanel1);
    //second component
    btnGetMaxSize = new MultiLineButton(GET_MAX_SIZE_TEXT);
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
    btnFinishJoin = new MultiLineButton(FINISH_JOIN_TEXT);
    //btnFinishJoin.addActionListener(joinActionListener);
    pnlFinishJoin.add(btnFinishJoin);
    //seventh component
    spinnerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinOpenBinnedBy = new LabeledSpinner(OPEN_BINNED_BY, spinnerModel);
    btnOpenIn3dmod = new Run3dmodButton(OPEN_IN_3DMOD, this);
    //btnOpenIn3dmod.addActionListener(joinActionListener);
    pnlFinishJoin.add(createOpen3dmodPanel(spinOpenBinnedBy, btnOpenIn3dmod));
  }

  private void createTrialJoinPanel() {
    SpacedPanel pnlTrialJoin = new SpacedPanel();
    pnlTrialJoin.setBoxLayout(BoxLayout.Y_AXIS);
    pnlTrialJoin.setBorder(new EtchedBorder("Trial Join").getBorder());
    pnlTrialJoin.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    //first component
    SpacedPanel trialJoinPanel1 = new SpacedPanel();
    trialJoinPanel1.setBoxLayout(BoxLayout.X_AXIS);
    int zMax = pnlSectionTable.getZMax();
    SpinnerModel spinnerModel = new SpinnerNumberModel(zMax < 1 ? 1
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
    btnTrialJoin = new MultiLineButton(TRIAL_JOIN_TEXT);
    pnlTrialJoin.add(btnTrialJoin);
    //fourth component
    btnOpenTrialIn3dmod = new Run3dmodButton("Open Trial in 3dmod", this);
    spinnerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinOpenTrialBinnedBy = new LabeledSpinner(OPEN_BINNED_BY, spinnerModel);
    pnlTrialJoin.add(createOpen3dmodPanel(spinOpenTrialBinnedBy,
        btnOpenTrialIn3dmod));
    //fifth component
    btnGetSubarea = new MultiLineButton("Get Subarea Size And Shift");
    pnlTrialJoin.add(btnGetSubarea);
    pnlFinishJoin.add(pnlTrialJoin);
  }

  SpacedPanel createOpen3dmodPanel(LabeledSpinner spinner,
      MultiLineButton button) {
    SpacedPanel open3dmodPanel = new SpacedPanel();
    open3dmodPanel.setBoxLayout(BoxLayout.Y_AXIS);
    open3dmodPanel.setBorder(BorderFactory.createEtchedBorder());
    //spinner panel
    SpacedPanel spinnerPanel = new SpacedPanel();
    spinnerPanel.setBoxLayout(BoxLayout.X_AXIS);
    spinner.setTextMaxmimumSize(dimSpinner);
    spinnerPanel.add(spinner);
    spinnerPanel.add(new JLabel(IN_X_AND_Y));
    open3dmodPanel.add(spinnerPanel);
    //add button
    open3dmodPanel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    open3dmodPanel.add(button);
    return open3dmodPanel;
  }

  /**
   * change the model for spinners when the number of sections changes, but
   * preserve any value set by the user.
   * @param numSections
   */
  void setNumSections(int numSections) {
    this.numSections = numSections;
    //density matching (setup)
    EtomoNumber spinnerValue = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    spinnerValue.set(spinDensityRefSection.getValue());
    spinnerValue.setDisplayValue(1);
    SpinnerModel spinnerModel = new SpinnerNumberModel(spinnerValue.getInt(),
        1, numSections < 1 ? 1 : numSections, 1);
    spinDensityRefSection.setModel(spinnerModel);
    //alignment (join)
    spinnerValue.set((Integer) spinAlignmentRefSection.getValue());
    spinnerModel = new SpinnerNumberModel(spinnerValue.getInt(), 1,
        numSections < 1 ? 1 : numSections, 1);
    spinAlignmentRefSection.setModel(spinnerModel);
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
    ConstJoinMetaData metaData = joinManager.getConstMetaData();
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
    EtomoNumber sizeInY = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    sizeInY.set(ltfSizeInY.getText());
    return sizeInY;
  }

  public ConstEtomoNumber getShiftInX() {
    EtomoNumber shiftInX = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    shiftInX.set(ltfShiftInX.getText());
    return shiftInX;
  }

  public ConstEtomoNumber getShiftInY() {
    EtomoNumber shiftInY = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
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

  public boolean getMetaData(JoinMetaData metaData) {
    synchronize();
    metaData.setRootName(ltfRootName.getText());
    metaData.setDensityRefSection(spinDensityRefSection.getValue());
    metaData.setSigmaLowFrequency(ltfSigmaLowFrequency.getText());
    metaData.setCutoffHighFrequency(ltfCutoffHighFrequency.getText());
    metaData.setSigmaHighFrequency(ltfSigmaHighFrequency.getText());
    metaData.setFullLinearTransformation(rbFullLinearTransformation
        .isSelected());
    metaData
        .setRotationTranslationMagnification(rbRotationTranslationMagnification
            .isSelected());
    metaData.setRotationTranslation(rbRotationTranslation.isSelected());
    metaData.setUseAlignmentRefSection(cbUseAlignmentRefSection.isSelected());
    metaData.setAlignmentRefSection(spinAlignmentRefSection.getValue());
    metaData.setSizeInX(ltfSizeInX.getText());
    metaData.setSizeInY(ltfSizeInY.getText());
    metaData.setShiftInX(ltfShiftInX.getText());
    metaData.setShiftInY(ltfShiftInY.getText());
    metaData.setUseEveryNSlices(spinUseEveryNSlices.getValue());
    metaData.setTrialBinning(spinTrialBinning.getValue());
    metaData.setMidasLimit(ltfMidasLimit.getText());
    return pnlSectionTable.getMetaData(metaData);
  }

  public void setMetaData(ConstJoinMetaData metaData) {
    ltfRootName.setText(metaData.getRootName());
    spinDensityRefSection.setValue(metaData.getDensityRefSection().getInt());
    ltfSigmaLowFrequency.setText(metaData.getSigmaLowFrequency().toString());
    ltfCutoffHighFrequency
        .setText(metaData.getCutoffHighFrequency().toString());
    ltfSigmaHighFrequency.setText(metaData.getSigmaHighFrequency().toString());
    rbFullLinearTransformation.setSelected(metaData
        .isFullLinearTransformation());
    rbRotationTranslationMagnification.setSelected(metaData
        .isRotationTranslationMagnification());
    rbRotationTranslation.setSelected(metaData.isRotationTranslation());
    cbUseAlignmentRefSection.setSelected(metaData.isUseAlignmentRefSection());
    useAlignmentRefSectionAction();
    spinAlignmentRefSection.setValue(metaData.getAlignmentRefSection()
        .getNumber());
    ltfSizeInX.setText(metaData.getSizeInX().toString());
    ltfSizeInY.setText(metaData.getSizeInY().toString());
    ltfShiftInX.setText(metaData.getShiftInX().toString());
    ltfShiftInY.setText(metaData.getShiftInY().toString());
    spinUseEveryNSlices.setValue(metaData.getUseEveryNSlices().getNumber());
    spinTrialBinning.setValue(metaData.getTrialBinning().getNumber());
    ltfMidasLimit.setText(metaData.getMidasLimit().toString());
    pnlSectionTable.setMetaData(metaData);
  }

  public Container getContainer() {
    return rootPanel;
  }

  public String getWorkingDirName() {
    return ltfWorkingDir.getText();
  }

  public File getWorkingDir() {
    String workingDirName = ltfWorkingDir.getText();
    if (workingDirName == null || workingDirName.length() == 0
        || workingDirName.matches("\\s+")) {
      return null;
    }
    return new File(ltfWorkingDir.getText());
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
    if (!ltfRootName.equals(metaData.getRootName())) {
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
    if (rbFullLinearTransformation.isSelected() != metaData
        .isFullLinearTransformation()) {
      return false;
    }
    if (rbRotationTranslationMagnification.isSelected() != metaData
        .isRotationTranslationMagnification()) {
      return false;
    }
    if (rbRotationTranslation.isSelected() != metaData.isRotationTranslation()) {
      return false;
    }
    if (cbUseAlignmentRefSection.isSelected() != metaData
        .isUseAlignmentRefSection()) {
      return false;
    }
    if (!metaData.getAlignmentRefSection().equals(
        (Number) spinAlignmentRefSection.getValue())) {
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
    if (!ltfRootName.equals(metaData.getRootName())) {
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
    switch (curTab) {
    case SETUP_TAB:
      manPagelabel = new String[] { "3dmod" };
      manPage = new String[] { "3dmod.html" };
      logFileLabel = new String[] { "startjoin" };
      logFile = new String[] { "startjoin.log" };
      contextPopup = new ContextPopup(rootPanel, mouseEvent, "Setup",
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage, logFileLabel,
          logFile, joinManager);
      break;
    case ALIGN_TAB:
      manPagelabel = new String[] { "Xfalign", "Midas", "3dmod" };
      manPage = new String[] { "xfalign.html", "midas.html", "3dmod.html" };
      contextPopup = new ContextPopup(rootPanel, mouseEvent, "Align",
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage);
      break;
    case JOIN_TAB:
      manPagelabel = new String[] { "Finishjoin", "3dmod" };
      manPage = new String[] { "finishjoin.html", "3dmod.html" };
      contextPopup = new ContextPopup(rootPanel, mouseEvent, "Joining",
          ContextPopup.JOIN_GUIDE, manPagelabel, manPage);
      break;
    }
  }

  /**
   * Handle actions
   * @param event
   */
  protected void action(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnMakeSamples.getActionCommand())) {
      joinManager.makejoincom();
    }
    else if (command.equals(btnInitialAutoAlignment.getActionCommand())) {
      btnMidas.setEnabled(false);
      joinManager.xfalignInitial();
    }
    else if (command.equals(btnMidas.getActionCommand())) {
      joinManager.midasSample();
    }
    else if (command.equals(btnRefineAutoAlignment.getActionCommand())) {
      btnMidas.setEnabled(false);
      joinManager.xfalignRefine();
    }
    else if (command.equals(btnRevertToMidas.getActionCommand())) {
      joinManager.revertXfFileToMidas();
    }
    else if (command.equals(btnRevertToEmpty.getActionCommand())) {
      joinManager.revertXfFileToEmpty();
    }
    else if (command.equals(btnFinishJoin.getActionCommand())) {
      joinManager.runFinishjoin(FinishjoinParam.FINISH_JOIN_MODE,
          FINISH_JOIN_TEXT);
    }
    else if (command.equals(btnGetMaxSize.getActionCommand())) {
      joinManager.runFinishjoin(FinishjoinParam.MAX_SIZE_MODE,
          GET_MAX_SIZE_TEXT);
    }
    else if (command.equals(btnTrialJoin.getActionCommand())) {
      joinManager.runFinishjoin(FinishjoinParam.TRIAL_MODE, TRIAL_JOIN_TEXT);
    }
    else if (command.equals(btnGetSubarea.getActionCommand())) {
      setSizeAndShift(joinManager.imodGetRubberbandCoordinates(
          ImodManager.TRIAL_JOIN_KEY, AxisID.ONLY));
    }
    else if (command.equals(btnChangeSetup.getActionCommand())) {
      //Prepare for Revert:  meta data file should match the screen
      getMetaData(joinManager.getJoinMetaData());
      joinManager.saveIntermediateParamFile(AxisID.ONLY);
      setMode(JoinDialog.CHANGING_SAMPLE_MODE);
    }
    else if (command.equals(btnRevertToLastSetup.getActionCommand())) {
      ConstJoinMetaData metaData = joinManager.getConstMetaData();
      if (!joinManager.getState().isSampleProduced()) {
        throw new IllegalStateException(
            "sample produced is false but Revert to Last Setup is enabled");
      }
      pnlSectionTable.deleteSections();
      setMetaData(joinManager.getConstMetaData());
      setMode(SAMPLE_PRODUCED_MODE);
    }
    else if (!run3dmod(command, new Run3dmodMenuOptions())) {
      throw new IllegalStateException("Unknown command " + command);
    }
  }

  public void run3dmod(Run3dmodButton button, Run3dmodMenuOptions menuOptions) {
    run3dmod(button.getActionCommand(), menuOptions);
  }

  private boolean run3dmod(String command, Run3dmodMenuOptions menuOptions) {
    if (command.equals(btnOpenSample.getActionCommand())) {
      joinManager.imodOpen(ImodManager.JOIN_SAMPLES_KEY, menuOptions);
      return true;
    }
    if (command.equals(btnOpenSampleAverages.getActionCommand())) {
      joinManager.imodOpen(ImodManager.JOIN_SAMPLE_AVERAGES_KEY, menuOptions);
      return true;
    }
    if (command.equals(btnOpenIn3dmod.getActionCommand())) {
      joinManager.imodOpen(ImodManager.JOIN_KEY, ((Integer) spinOpenBinnedBy
          .getValue()).intValue(), menuOptions);
      return true;
    }
    if (command.equals(btnOpenTrialIn3dmod.getActionCommand())) {
      joinManager.imodOpen(ImodManager.TRIAL_JOIN_KEY,
          ((Integer) this.spinOpenTrialBinnedBy.getValue()).intValue(),
          menuOptions);
      return true;
    }
    return false;
  }

  protected void workingDirAction() {
    //  Open up the file chooser in the current working directory
    JFileChooser chooser = new JFileChooser(new File(joinManager
        .getPropertyUserDir()));
    chooser.setPreferredSize(FixedDim.fileChooser);
    chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    int returnVal = chooser.showOpenDialog(rootPanel);
    if (returnVal == JFileChooser.APPROVE_OPTION) {
      File workingDir = chooser.getSelectedFile();
      try {
        ltfWorkingDir.setText(workingDir.getAbsolutePath());
      }
      catch (Exception excep) {
        excep.printStackTrace();
      }
    }
  }

  protected void useAlignmentRefSectionAction() {
    spinAlignmentRefSection.setEnabled(cbUseAlignmentRefSection.isSelected());
  }

  /**
   * synchronize when changing tabs
   * @param prevTab
   */
  final void synchronize(int prevTab) {
    pnlSectionTable.synchronize(prevTab, curTab);
  }

  /**
   * synchronize when saving to the .ejf file
   */
  final void synchronize() {
    pnlSectionTable.synchronize(curTab, -1);
  }

  private void setToolTipText() {
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    String toolTip = tooltipFormatter.setText(
        "Enter the directory where you wish to place the joined tomogram.")
        .format();
    ltfWorkingDir.setToolTipText(toolTip);
    btnWorkingDir.setToolTipText(toolTip);
    ltfRootName.setToolTipText(tooltipFormatter.setText(
        "Enter the root name for the joined tomogram.").format());
    toolTip = tooltipFormatter
        .setText(
            "The size to which samples will be squeezed if they are bigger (default 1024).")
        .format();
    ltfMidasLimit.setToolTipText(toolTip);
    lblMidasLimit.setToolTipText(toolTip);
    spinDensityRefSection
        .setToolTipText(tooltipFormatter.setText(
            "Select a section to use as a reference for density scaling.")
            .format());
    btnChangeSetup.setToolTipText(tooltipFormatter.setText(
        "Press to redo an existing sample.").format());
    btnRevertToLastSetup.setToolTipText(tooltipFormatter.setText(
        "Press to go back to the existing sample.").format());
    btnMakeSamples.setToolTipText(tooltipFormatter.setText(
        "Press to make a sample.").format());
    btnOpenSampleAverages.setToolTipText(tooltipFormatter.setText(
        "Press to the sample averages file in 3dmod.").format());
    btnOpenSample.setToolTipText(tooltipFormatter.setText(
        "Press to the sample file in 3dmod.").format());
    btnOpenSample.setToolTipText(tooltipFormatter.setText(
        "Press to the sample file in 3dmod.").format());
    ltfSigmaLowFrequency
        .setToolTipText(tooltipFormatter
            .setText(
                "Sigma of an inverted gaussian for filtering out low frequencies before searching for transformation.")
            .format());
    ltfCutoffHighFrequency
        .setToolTipText(tooltipFormatter
            .setText(
                "Starting radius of a gaussian for filtering out high frequencies before searching for transformation.")
            .format());
    ltfSigmaHighFrequency
        .setToolTipText(tooltipFormatter
            .setText(
                "Sigma of gaussian for filtering out high frequencies before searching for transformation.")
            .format());
    rbFullLinearTransformation
        .setToolTipText(tooltipFormatter
            .setText(
                "Use rotation, translation, magnification, and stretching to align images.")
            .format());
    rbRotationTranslationMagnification.setToolTipText(tooltipFormatter.setText(
        "Use translation, rotation, and magnification  to align images.")
        .format());
    rbRotationTranslation.setToolTipText(tooltipFormatter.setText(
        "Use translation and rotation  to align images.").format());
    btnInitialAutoAlignment
        .setToolTipText(tooltipFormatter
            .setText(
                "OPTIONAL:  Run xfalign.  Find preliminary translational alignments with tiltxcorr rather then using an existing .xf file.")
            .format());
    btnMidas
        .setToolTipText(tooltipFormatter
            .setText(
                "Open Midas to check the output of the auto alignment and to make transformations by hand.")
            .format());
    btnRefineAutoAlignment
        .setToolTipText(tooltipFormatter
            .setText(
                "OPTIONAL:  Run xfalign using preliminary alignments created by the most recent use of Midas or xfalign.")
            .format());
    btnRevertToMidas
        .setToolTipText(tooltipFormatter
            .setText(
                "Use to ignore xfalign changes.  Returns transformations to the state created by the most recent save done in Midas.")
            .format());
    btnRevertToEmpty.setToolTipText(tooltipFormatter.setText(
        "Use to remove all transformations.").format());
    cbUseAlignmentRefSection
        .setToolTipText(tooltipFormatter
            .setText(
                "Make a section the reference for alignment.  This means that the chosen section will not be transformed, and the other sections will be transformed into alignment with it.")
            .format());
    spinAlignmentRefSection
        .setToolTipText(tooltipFormatter
            .setText(
                "Choose a section to be the reference for alignment.  This means that it will not be transformed, and the other sections will be transformed into alignment with it.")
            .format());
    btnGetMaxSize
        .setToolTipText(tooltipFormatter
            .setText(
                "Compute the maximum size and offsets needed to contain the transformed images from all of the sections, given the current transformations.")
            .format());
    ltfSizeInX.setToolTipText(tooltipFormatter.setText(
        "The size in X parameter for the trial and final joined tomograms.").format());
    ltfSizeInY.setToolTipText(tooltipFormatter.setText(
        "The size in Y parameter for the trial and final joined tomograms.").format());
    ltfShiftInX.setToolTipText(tooltipFormatter.setText(
        "The X offset parameter for the trial and final joined tomograms.").format());
    ltfShiftInY.setToolTipText(tooltipFormatter.setText(
        "The Y offset parameter for the trial and final joined tomograms.").format());
    spinUseEveryNSlices.setToolTipText(tooltipFormatter.setText(
        "Slices to use when creating the trial joined tomogram.").format());
    spinTrialBinning.setToolTipText(tooltipFormatter.setText(
        "The binning to use when creating the trial joined tomogram.").format());
    btnTrialJoin.setToolTipText(tooltipFormatter.setText(
        "Press to make a trial version of the joined tomogram.").format());
    spinOpenTrialBinnedBy.setToolTipText(tooltipFormatter.setText(
        "The binning to use when opening the trial joined tomogram in 3dmod.").format());
    btnOpenTrialIn3dmod.setToolTipText(tooltipFormatter.setText(
        "Press to open the trial joined tomogram.").format());
    btnGetSubarea
        .setToolTipText(tooltipFormatter
            .setText(
                "Press to get maximum size and shift from the trail joined tomogram using the rubber band functionality in 3dmod.")
            .format());
    btnFinishJoin.setToolTipText(tooltipFormatter.setText(
        "Press to make the joined tomogram.").format());
    spinOpenBinnedBy.setToolTipText(tooltipFormatter.setText(
        "The binning to use when opening the joined tomogram in 3dmod.").format());
    btnOpenIn3dmod.setToolTipText(tooltipFormatter.setText(
        "Press to open the joined tomogram in 3dmod.").format());
  }

  //
  //  Action listener adapters
  //
  class JoinActionListener implements ActionListener {

    JoinDialog adaptee;

    JoinActionListener(JoinDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.action(event);
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

  class UseAlignmentRefSectionActionListener implements ActionListener {

    JoinDialog adaptee;

    UseAlignmentRefSectionActionListener(JoinDialog adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.useAlignmentRefSectionAction();
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
}