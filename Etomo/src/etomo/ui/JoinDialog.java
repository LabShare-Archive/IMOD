package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.*;
import java.io.File;
import java.util.Vector;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.EtomoDirector;
import etomo.JoinManager;
import etomo.comscript.FinishjoinParam;
import etomo.process.ImodManager;
import etomo.process.ImodProcess;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.ConstJoinMetaData;
import etomo.type.EtomoNumber;
import etomo.type.JoinMetaData;

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
public class JoinDialog implements ContextMenu {
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
  private static Dimension dimButton = UIParameters.getButtonDimension();
  private static Dimension dimSpinner = UIParameters.getSpinnerDimension();

  private JPanel rootPanel;
  private JTabbedPane tabPane;
  private DoubleSpacedPanel pnlSetup;
  private SectionTablePanel pnlSectionTable;
  private DoubleSpacedPanel pnlAlign;
  private DoubleSpacedPanel pnlJoin;
  private SpacedPanel setupPanel1;
  private DoubleSpacedPanel setupPanel2;
  private SpacedPanel alignPanel1;
  private SpacedPanel alignPanel2;
  private DoubleSpacedPanel pnlXfalign;
  private DoubleSpacedPanel pnlFinishJoin;
  
  private JButton btnWorkingDir;
  private MultiLineButton btnMakeSamples;
  private MultiLineButton btnOpenSample;
  private MultiLineButton btnOpenSampleAverages;
  private MultiLineButton btnInitialAutoAlignment;
  private MultiLineButton btnMidas;
  private MultiLineButton btnRefineAutoAlignment;
  private MultiLineButton btnRevertToMidas;
  private MultiLineButton btnRevertToEmpty;
  private MultiLineButton btnGetMaxSize;
  private MultiLineButton btnTrialJoin;
  private MultiLineButton btnOpenTrialIn3dmod;
  private MultiLineButton btnGetSubarea;
  private MultiLineButton btnFinishJoin;
  private MultiLineButton btnOpenIn3dmod;
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
  private JRadioButton rbFullLinearTransformation;
  private JRadioButton rbRotationTranslationMagnification;
  private JRadioButton rbRotationTranslation;
  private JCheckBox cbUseAlignmentRefSection;
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

  public JoinDialog(JoinManager joinManager) {
    this(joinManager, null);
  }
  
  public JoinDialog(JoinManager joinManager, String workingDirName) {
    axisID = AxisID.ONLY;
    this.joinManager = joinManager;
    createRootPanel(workingDirName);
    joinManager.packMainWindow();
  }

  private void createRootPanel(String workingDirName) {
    rootPanel = new JPanel();
    createTabPane(workingDirName);
    rootPanel.add(tabPane);
  }

  private void createTabPane(String workingDirName) {
    tabPane = new JTabbedPane();
    TabChangeListener tabChangeListener = new TabChangeListener(this);
    tabPane.addMouseListener(new GenericMouseAdapter(this));
    tabPane.addChangeListener(tabChangeListener);
    tabPane.setBorder(new BeveledBorder("Join").getBorder());
    createSetupPanel(workingDirName);
    tabPane.addTab("Setup", pnlSetup.getContainer());
    createAlignPanel();
    tabPane.addTab("Align", pnlAlign.getContainer());
    createJoinPanel();
    tabPane.addTab("Join", pnlJoin.getContainer());
  }
  
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
      if (ImodProcess
        .RUBBERBAND_RESULTS_STRING
        .equals((String) coordinates.get(index++))) {
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
    ConstJoinMetaData metaData = joinManager.getMetaData();
    if (!estXMin.isNull() && !estXMax.isNull()) {
      min = metaData.getCoordinate(estXMin);
      max = metaData.getCoordinate(estXMax);
      ltfSizeInX.setText(JoinMetaData.getSize(min, max));
      ltfShiftInX.setText(metaData.getNewShiftInX(min, max));
    }
    if (!estYMin.isNull() && !estYMax.isNull()) {
      min = metaData.getCoordinate(estYMin);
      max = metaData.getCoordinate(estYMax);
      ltfSizeInY.setText(JoinMetaData.getSize(min, max));
      ltfShiftInY.setText(metaData.getNewShiftInY(min, max));
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
  
  void changeTab(ChangeEvent event){
    removePanelComponents(curTab);
    curTab = tabPane.getSelectedIndex();
    addPanelComponents(curTab);
    if (EtomoDirector.getInstance().getUserConfiguration().isAutoFit()) {
      joinManager.getMainPanel().fitWindow();
    }
  }
  
  public void setMode(int mode) {
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
      spinDensityRefSection.setEnabled(true);
      btnChangeSetup.setEnabled(false);
      btnRevertToLastSetup.setEnabled(false);
      btnMakeSamples.setEnabled(true);
      break;
    case SAMPLE_PRODUCED_MODE:
      tabPane.setEnabledAt(1, true);
      tabPane.setEnabledAt(2, true);
      spinDensityRefSection.setEnabled(false);
      btnChangeSetup.setEnabled(true);
      btnRevertToLastSetup.setEnabled(false);
      btnMakeSamples.setEnabled(false);
      break;
    case CHANGING_SAMPLE_MODE:
      tabPane.setEnabledAt(1, false);
      tabPane.setEnabledAt(2, false);
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
    pnlSetup = new DoubleSpacedPanel(false, FixedDim.x5_y0, FixedDim.x0_y5);
    //first component
    setupPanel1 = new SpacedPanel(FixedDim.x5_y0);
    setupPanel1
        .setLayout(new BoxLayout(setupPanel1.getContainer(), BoxLayout.X_AXIS));
    ltfWorkingDir = new LabeledTextField(WORKING_DIRECTORY_TEXT + ": ");
    ltfWorkingDir.setText(workingDirName);
    setupPanel1.add(ltfWorkingDir);
    btnWorkingDir = new JButton(iconFolder);
    btnWorkingDir.setPreferredSize(FixedDim.folderButton);
    btnWorkingDir.setMaximumSize(FixedDim.folderButton);
    btnWorkingDir.addActionListener(workingDirActionListener);
    setupPanel1.add(btnWorkingDir);
    //second component
    ltfRootName = new LabeledTextField("Root name for output file: ");
    //third component    
    pnlSectionTable = new SectionTablePanel(this, joinManager, curTab);
    //fourth component
    SpinnerModel spinnerModel = new SpinnerNumberModel(1, 1,
        numSections < 1 ? 1 : numSections, 1);
    spinDensityRefSection = new LabeledSpinner(
        "Reference section for density matching: ", spinnerModel);
    spinDensityRefSection.setTextMaxmimumSize(dimSpinner);
    //fifth component
    setupPanel2 = new DoubleSpacedPanel(true, FixedDim.x5_y0, FixedDim.x0_y5,
        BorderFactory.createEtchedBorder());
    btnChangeSetup = new MultiLineButton("Change Setup");
    btnChangeSetup.addActionListener(joinActionListener);
    setupPanel2.addMultiLineButton(btnChangeSetup);
    btnRevertToLastSetup = new MultiLineButton("Revert to Last Setup");
    btnRevertToLastSetup.addActionListener(joinActionListener);
    setupPanel2.addMultiLineButton(btnRevertToLastSetup);
    //sixth component
    btnMakeSamples = new MultiLineButton("Make Samples");
    btnMakeSamples.addActionListener(joinActionListener);
    UIUtilities.setButtonSize(btnMakeSamples, dimButton);
    btnMakeSamples.setAlignmentX(Component.CENTER_ALIGNMENT);
  }
  
  private void addSetupPanelComponents() {
    pnlSetup.add(setupPanel1);
    pnlSetup.add(ltfRootName);
    pnlSetup.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.setCurTab(SETUP_TAB);
    pnlSectionTable.displayCurTab();
    pnlSetup.add(spinDensityRefSection);
    pnlSetup.add(setupPanel2);
    pnlSetup.add(btnMakeSamples);
  }
  
  private void createAlignPanel() {
    pnlAlign = new DoubleSpacedPanel(false, FixedDim.x5_y0, FixedDim.x0_y5);
    //second component
    alignPanel1 = new SpacedPanel(FixedDim.x5_y0);
    alignPanel1.setLayout(new BoxLayout(alignPanel1.getContainer(), BoxLayout.X_AXIS));
    btnOpenSample = new MultiLineButton("Open Sample in 3dmod");
    btnOpenSample.addActionListener(joinActionListener);
    btnOpenSampleAverages = new MultiLineButton("Open Sample Averages in 3dmod");
    btnOpenSampleAverages.addActionListener(joinActionListener);
    alignPanel1.addMultiLineButton(btnOpenSample);
    alignPanel1.addMultiLineButton(btnOpenSampleAverages);
    //third component
    pnlXfalign = new DoubleSpacedPanel(false, FixedDim.x5_y0, FixedDim.x0_y5, new EtchedBorder("Auto Alignment Parameters").getBorder(), false);
    ltfSigmaLowFrequency = new LabeledTextField("Sigma for low-frequency filter: ");
    pnlXfalign.add(ltfSigmaLowFrequency);
    ltfCutoffHighFrequency = new LabeledTextField("Cutoff for high-frequency filter: ");
    pnlXfalign.add(ltfCutoffHighFrequency);
    ltfSigmaHighFrequency = new LabeledTextField("Sigma for high-frequency filter: ");
    pnlXfalign.add(ltfSigmaHighFrequency);
    ButtonGroup bgSearchFor = new ButtonGroup();
    rbFullLinearTransformation = new JRadioButton("Full linear transformation");
    rbRotationTranslationMagnification = new JRadioButton("Rotation/translation/magnification");
    rbRotationTranslation = new JRadioButton("Rotation/translation");
    bgSearchFor.add(rbFullLinearTransformation);
    bgSearchFor.add(rbRotationTranslationMagnification);
    bgSearchFor.add(rbRotationTranslation);
    pnlXfalign.add(new JLabel("Search For:"));
    pnlXfalign.add(rbFullLinearTransformation, false);
    pnlXfalign.add(rbRotationTranslationMagnification, false);
    pnlXfalign.add(rbRotationTranslation);
    //fourth component
    alignPanel2 = new SpacedPanel(FixedDim.x5_y0);
    alignPanel2.setLayout(new BoxLayout(alignPanel2
        .getContainer(), BoxLayout.X_AXIS));
    SpacedPanel alignPanel2A = new SpacedPanel(FixedDim.x0_y5);
    alignPanel2A.setLayout(new BoxLayout(alignPanel2A
        .getContainer(), BoxLayout.Y_AXIS));
    btnInitialAutoAlignment = new MultiLineButton("Initial Auto Alignment");
    btnInitialAutoAlignment.addActionListener(joinActionListener);
    alignPanel2A.addMultiLineButton(btnInitialAutoAlignment);
    btnMidas = new MultiLineButton(MIDAS_TEXT);
    btnMidas.addActionListener(joinActionListener);
    alignPanel2A.addMultiLineButton(btnMidas);
    btnRefineAutoAlignment = new MultiLineButton(REFINE_AUTO_ALIGNMENT_TEXT);
    btnRefineAutoAlignment.addActionListener(joinActionListener);
    alignPanel2A.addMultiLineButton(btnRefineAutoAlignment);
    alignPanel2.add(alignPanel2A);
    DoubleSpacedPanel alignPanel2B = new DoubleSpacedPanel(false, FixedDim.x5_y0,
        FixedDim.x0_y5, BorderFactory.createEtchedBorder());
    btnRevertToMidas = new MultiLineButton("Revert Auto Alignment to Midas");
    btnRevertToMidas.addActionListener(joinActionListener);
    alignPanel2B.addMultiLineButton(btnRevertToMidas);
    btnRevertToEmpty= new MultiLineButton("Revert to No Transforms");
    btnRevertToEmpty.addActionListener(joinActionListener);
    alignPanel2B.addMultiLineButton(btnRevertToEmpty);
    alignPanel2.add(alignPanel2B);
  }
  
  private void addAlignPanelComponents() {
    //first component
    pnlAlign.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.setCurTab(ALIGN_TAB);
    pnlSectionTable.displayCurTab();
    //second component
    pnlAlign.add(alignPanel1);
    //third component
    pnlAlign.add(pnlXfalign);
    //fourth component
    pnlAlign.add(alignPanel2);
  }

  private void createJoinPanel() {
    pnlJoin = new DoubleSpacedPanel(true, FixedDim.x5_y0, FixedDim.x0_y5);
    //second component
    createFinishJoinPanel();
  }
  
  private void addJoinPanelComponents() {
    //first component
    pnlJoin.add(pnlSectionTable.getRootPanel());
    pnlSectionTable.setCurTab(JOIN_TAB);
    pnlSectionTable.displayCurTab();
    //second component
    pnlJoin.add(pnlFinishJoin);
  }
  
  private void createFinishJoinPanel() {
    pnlFinishJoin = new DoubleSpacedPanel(false, FixedDim.x5_y0, FixedDim.x0_y5, BorderFactory.createEtchedBorder());
    pnlFinishJoin.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    //first component
    JPanel finishJoinPanel1 = new JPanel();
    finishJoinPanel1.setLayout(new BoxLayout(finishJoinPanel1, BoxLayout.X_AXIS));
    cbUseAlignmentRefSection = new JCheckBox("Reference section for alignment: ");
    cbUseAlignmentRefSection.addActionListener(useAlignmentRefSectionActionListener);
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
    btnGetMaxSize.addActionListener(joinActionListener);
    pnlFinishJoin.addMultiLineButton(btnGetMaxSize);
    //third component
    SpacedPanel finishJoinPanel2 = new SpacedPanel(FixedDim.x5_y0);
    finishJoinPanel2.setLayout(new BoxLayout(finishJoinPanel2.getContainer(), BoxLayout.X_AXIS));
    ltfSizeInX = new LabeledTextField("Size in X: ");
    finishJoinPanel2.add(ltfSizeInX);
    ltfSizeInY = new LabeledTextField("Y: ");
    finishJoinPanel2.add(ltfSizeInY);
    pnlFinishJoin.add(finishJoinPanel2);
    //fourth component
    SpacedPanel finishJoinPanel3 = new SpacedPanel(FixedDim.x5_y0);
    finishJoinPanel3.setLayout(new BoxLayout(finishJoinPanel3.getContainer(), BoxLayout.X_AXIS));
    ltfShiftInX = new LabeledTextField("Shift in X: ");
    finishJoinPanel3.add(ltfShiftInX);
    ltfShiftInY = new LabeledTextField("Y: ");
    finishJoinPanel3.add(ltfShiftInY);
    pnlFinishJoin.add(finishJoinPanel3);
    //fifth component
    createTrialJoinPanel();
    //sixth component
    btnFinishJoin = new MultiLineButton(FINISH_JOIN_TEXT);
    btnFinishJoin.addActionListener(joinActionListener);
    pnlFinishJoin.addMultiLineButton(btnFinishJoin);
    //seventh component
    spinnerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinOpenBinnedBy = new LabeledSpinner(OPEN_BINNED_BY, spinnerModel);
    btnOpenIn3dmod = new MultiLineButton(OPEN_IN_3DMOD);
    btnOpenIn3dmod.addActionListener(joinActionListener);
    pnlFinishJoin.add(createOpen3dmodPanel(spinOpenBinnedBy, btnOpenIn3dmod));
  }
  
  private void createTrialJoinPanel() {
    DoubleSpacedPanel pnlTrialJoin = new DoubleSpacedPanel(false, FixedDim.x5_y0, FixedDim.x0_y5, new EtchedBorder("Trial Join").getBorder(), false);
    pnlTrialJoin.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    //first component
    SpacedPanel trialJoinPanel1 = new SpacedPanel(FixedDim.x5_y0);
    trialJoinPanel1.setLayout(new BoxLayout(trialJoinPanel1.getContainer(), BoxLayout.X_AXIS));
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
    spinTrialBinning = new LabeledSpinner(
        "Binning in X and Y: ", spinnerModel);
    spinTrialBinning.setTextMaxmimumSize(dimSpinner);
    pnlTrialJoin.add(spinTrialBinning);
    //third component
    btnTrialJoin = new MultiLineButton(TRIAL_JOIN_TEXT);
    btnTrialJoin.addActionListener(joinActionListener);
    pnlTrialJoin.addMultiLineButton(btnTrialJoin);
    //fourth component
    btnOpenTrialIn3dmod = new MultiLineButton("Open Trial in 3dmod");
    btnOpenTrialIn3dmod.addActionListener(joinActionListener);
    spinnerModel = new SpinnerNumberModel(1, 1, 50, 1);
    spinOpenTrialBinnedBy = new LabeledSpinner(
        OPEN_BINNED_BY, spinnerModel);
    pnlTrialJoin.add(createOpen3dmodPanel(spinOpenTrialBinnedBy, btnOpenTrialIn3dmod));
    //fifth component
    btnGetSubarea = new MultiLineButton("Get Subarea Size And Shift");
    btnGetSubarea.addActionListener(joinActionListener);
    pnlTrialJoin.addMultiLineButton(btnGetSubarea);
    pnlFinishJoin.add(pnlTrialJoin);
  }
  
  SpacedPanel createOpen3dmodPanel(LabeledSpinner spinner, MultiLineButton button) {
    SpacedPanel open3dmodPanel = new SpacedPanel(FixedDim.x0_y5, true);
    open3dmodPanel.setLayout(new BoxLayout(open3dmodPanel.getContainer(),
        BoxLayout.Y_AXIS));
    open3dmodPanel.setBorder(BorderFactory.createEtchedBorder());
    //spinner panel
    SpacedPanel spinnerPanel = new SpacedPanel(FixedDim.x5_y0, true);
    spinnerPanel.setLayout(new BoxLayout(spinnerPanel.getContainer(),
        BoxLayout.X_AXIS));
    spinner.setTextMaxmimumSize(dimSpinner);
    spinnerPanel.add(spinner);
    spinnerPanel.add(new JLabel(IN_X_AND_Y));
    open3dmodPanel.add(spinnerPanel);
    //add button
    open3dmodPanel.setComponentAlignmentX(Component.CENTER_ALIGNMENT);
    open3dmodPanel.addMultiLineButton(button);
    return open3dmodPanel;
  }
  
  /**
   * change the model for spinners when the number of sections changes, but
   * preserve any value set by the user.
   * @param numSections
   */
  void setNumSections(int numSections) {
    this.numSections = numSections;
    //setup
    EtomoNumber spinnerValue = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    spinnerValue.set(spinDensityRefSection.getValue());
    spinnerValue.setDefault(1);
    SpinnerModel spinnerModel = new SpinnerNumberModel(spinnerValue.getInteger(true),
        1, numSections < 1 ? 1 : numSections, 1);
    spinDensityRefSection.setModel(spinnerModel);
    //align
    spinnerValue.set((Integer) spinAlignmentRefSection.getValue());
    spinnerModel = new SpinnerNumberModel(spinnerValue
        .getInteger(true), 1,
        numSections < 1 ? 1 : numSections, 1);
    spinAlignmentRefSection.setModel(spinnerModel);
    //every n sections
    int zMax = pnlSectionTable.getZMax();
    if (zMax == 0) {
      spinnerValue.set(1);
    }
    else {
      spinnerValue.setCeiling(zMax);
      spinnerValue.set((Integer) spinUseEveryNSlices.getValue());
    }
    spinnerValue.setDefault(zMax < 1 ? 1 : zMax < 10 ? zMax : 10);
    spinnerModel = new SpinnerNumberModel(spinnerValue.getInteger(true), 1,
        zMax < 1 ? 1 : zMax, 1);
    spinUseEveryNSlices.setModel(spinnerModel);
    //update size in X and Y defaults
    ConstJoinMetaData metaData = joinManager.getMetaData();
    ConstEtomoNumber size = metaData.getSizeInX();
    size.setDefault(pnlSectionTable.getXMax());
    ltfSizeInX.setText(size.getInteger(true));
    size = metaData.getSizeInY();
    size.setDefault(pnlSectionTable.getYMax());
    ltfSizeInY.setText(size.getInteger(true));
  }
  
  public ConstEtomoNumber getSizeInX() {
    EtomoNumber sizeInX = new EtomoNumber(EtomoNumber.INTEGER_TYPE);
    sizeInX.set(ltfSizeInX.getText());
    return sizeInX;
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
    ltfSizeInX.setText(sizeInX.toString(true));
  }
  
  public void setSizeInY(ConstEtomoNumber sizeInY) {
    ltfSizeInY.setText(sizeInY.toString(true));
  }
  
  public void setShiftInX(ConstEtomoNumber shiftInX) {
    ltfShiftInX.setText(shiftInX.toString(true));
  }
  
  public void setShiftInY(ConstEtomoNumber shiftInY) {
    ltfShiftInY.setText(shiftInY.toString(true));
  }
  
  public String getInvalidReason() {
    if (invalidReason != null) {
      return invalidReason;
    }
    return pnlSectionTable.getInvalidReason();
  }
  
  public boolean getMetaData(JoinMetaData metaData) { 
    metaData.setRootName(ltfRootName.getText());
    metaData.setDensityRefSection(spinDensityRefSection.getValue());
    metaData.setSigmaLowFrequency(ltfSigmaLowFrequency.getText());
    metaData.setCutoffHighFrequency(ltfCutoffHighFrequency.getText());
    metaData.setSigmaHighFrequency(ltfSigmaHighFrequency.getText());
    metaData.setFullLinearTransformation(rbFullLinearTransformation.isSelected());
    metaData.setRotationTranslationMagnification(rbRotationTranslationMagnification.isSelected());
    metaData.setRotationTranslation(rbRotationTranslation.isSelected());
    metaData.setUseAlignmentRefSection(cbUseAlignmentRefSection.isSelected());
    metaData.setAlignmentRefSection(spinAlignmentRefSection.getValue());
    metaData.setSizeInX(ltfSizeInX.getText());
    metaData.setSizeInY(ltfSizeInY.getText());
    metaData.setShiftInX(ltfShiftInX.getText());
    metaData.setShiftInY(ltfShiftInY.getText());
    metaData.setUseEveryNSlices(spinUseEveryNSlices.getValue());
    metaData.setTrialBinning(spinTrialBinning.getValue());
    return pnlSectionTable.getMetaData(metaData);
  }
  
  public void setMetaData(ConstJoinMetaData metaData) {
    ltfRootName.setText(metaData.getRootName());
    spinDensityRefSection.setValue(metaData.getDensityRefSection().getInteger());
    ltfSigmaLowFrequency.setText(metaData.getSigmaLowFrequency().toString());
    ltfCutoffHighFrequency.setText(metaData.getCutoffHighFrequency().toString());
    ltfSigmaHighFrequency.setText(metaData.getSigmaHighFrequency().toString());
    rbFullLinearTransformation.setSelected(metaData.isFullLinearTransformation());
    rbRotationTranslationMagnification.setSelected(metaData.isRotationTranslationMagnification());
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
  private void action(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(btnMakeSamples.getActionCommand())) {
      joinManager.makejoincom();
    }
    else if (command.equals(btnOpenSample.getActionCommand())) {
      joinManager.imodOpen(ImodManager.JOIN_SAMPLES_KEY);
    }
    else if (command.equals(btnOpenSampleAverages.getActionCommand())) {
      joinManager.imodOpen(ImodManager.JOIN_SAMPLE_AVERAGES_KEY);
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
      joinManager.runFinishjoin(FinishjoinParam.FINISH_JOIN_MODE, FINISH_JOIN_TEXT);
    }
    else if (command.equals(btnOpenIn3dmod.getActionCommand())) {
      joinManager.imodOpen(ImodManager.JOIN_KEY, ((Integer) spinOpenBinnedBy
          .getValue()).intValue());
    }
    else if (command.equals(btnGetMaxSize.getActionCommand())) {
      joinManager.runFinishjoin(FinishjoinParam.MAX_SIZE_MODE, GET_MAX_SIZE_TEXT);
    }
    else if (command.equals(btnTrialJoin.getActionCommand())) {
      joinManager.runFinishjoin(FinishjoinParam.TRIAL_MODE, TRIAL_JOIN_TEXT);
    }
    else if (command.equals(btnOpenTrialIn3dmod.getActionCommand())) {
      joinManager.imodOpen(ImodManager.TRIAL_JOIN_KEY,
          ((Integer) this.spinOpenTrialBinnedBy.getValue()).intValue());
    }
    else if (command.equals(btnGetSubarea.getActionCommand())) {
      setSizeAndShift(joinManager
          .imodGetRubberbandCoordinates(ImodManager.TRIAL_JOIN_KEY));
    }
    else if (command.equals(btnChangeSetup.getActionCommand())) {
      //Prepare for Revert:  meta data file should match the screen
      getMetaData(joinManager.getJoinMetaData());
      joinManager.saveMetaData();
      setMode(JoinDialog.CHANGING_SAMPLE_MODE);
    }
    else if (command.equals(btnRevertToLastSetup.getActionCommand())) {
      ConstJoinMetaData metaData = joinManager.getMetaData();
      if (!metaData.isSampleProduced()) {
        throw new IllegalStateException("sample produced is false but Revert to Last Setup is enabled");
      }
      pnlSectionTable.deleteSections();
      setMetaData(joinManager.getMetaData());
      setMode(SAMPLE_PRODUCED_MODE);
    }
    else {
      throw new IllegalStateException("Unknown command " + command);
    }
  }

  private void workingDirAction() {
    //  Open up the file chooser in the working directory
    JFileChooser chooser = new JFileChooser(new File(joinManager.getPropertyUserDir()));
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
    
  private void useAlignmentRefSectionAction() {
    spinAlignmentRefSection.setEnabled(cbUseAlignmentRefSection.isSelected());
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