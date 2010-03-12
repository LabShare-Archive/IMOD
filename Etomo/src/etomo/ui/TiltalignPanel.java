package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import etomo.ApplicationManager;
import etomo.comscript.ConstTiltalignParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.comscript.TiltalignParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.storage.autodoc.ReadOnlySection;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.MetaData;
import etomo.type.ReconScreenState;

/**
 * <p>Description: </p>
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
 */

final class TiltalignPanel implements Expandable {
  public static final String rcsid = "$Id$";

  private static final String MIN_LOCAL_PATCH_SIZE_LABEL = "Min. local patch size or overlap factor (x,y): ";
  private static final String MIN_LOCAL_PATCH_SIZE_OVERLAP_ONLY_LABEL = "Overlap factor (x,y): ";
  private final AxisID axisID;

  /*
   //  TODO need recomended default for all sub groups see (align.com)
   private final int defaultTiltAngleType = 5;
   private final int defaultTiltAngleGroupSize = 5;
   private final int defaultMagnificationType = 3;
   private final int defaultDistortionType = 2;
   private final int defaultXstretchType = 3;
   private final int defaultXstretchGroupSize = 7;
   private final int defaultSkewType = 3;
   private final int defaultSkewGroupSize = 11;

   private final int defaultLocalRotationType = 3;
   private final int defaultLocalRotationGroupSize = 6;
   private final int defaultLocalTiltAngleType = 5;
   private final int defaultLocalTiltAngleGroupSize = 6;
   private final int defaultLocalMagnificationType = 3;
   private final int defaultLocalMagnificationGroupSize = 7;
   private final int defaultLocalDistortionType = 2;
   private final int defaultLocalXstretchType = 3;
   private final int defaultLocalXstretchGroupSize = 7;
   private final int defaultLocalSkewType = 3;
   private final int defaultLocalSkewGroupSize = 11;*/

  private final TabbedPane tabPane = new TabbedPane();

  //  General pane
  private final EtomoPanel pnlGeneral = new EtomoPanel();
  private final JPanel pnlGeneralBody = new JPanel();

  private final LabeledTextField ltfResidualThreshold = new LabeledTextField(
      "Threshold for residual report: ");

  private final RadioButton rbResidAllViews = new RadioButton("All views");
  private final RadioButton rbResidNeighboring = new RadioButton(
      "Neighboring views");
  private final ButtonGroup bgResidualThreshold = new ButtonGroup();
  private final EtomoPanel pnlResidualThreshold = new EtomoPanel();

  private final RadioButton rbSingleFiducialSurface = new RadioButton(
      "Do not sort fiducials into 2 surfaces for analysis");
  private final RadioButton rbDualFiducialSurfaces = new RadioButton(
      "Assume fiducials on 2 surfaces for analysis");
  private final ButtonGroup bgFiducialSurfaces = new ButtonGroup();
  private final EtomoPanel pnlFiducialSurfaces = new EtomoPanel();

  private final LabeledTextField ltfExcludeList = new LabeledTextField(
      "List of views to exclude: ");
  private final LabeledTextField ltfSeparateViewGroups = new LabeledTextField(
      "Separate view groups: ");

  private final EtomoPanel pnlVolumeParameters = new EtomoPanel();
  private final LabeledTextField ltfTiltAngleOffset = new LabeledTextField(
      "Total tilt angle offset: ");
  private final LabeledTextField ltfTiltAxisZShift = new LabeledTextField(
      "Tilt axis z shift: ");

  private final EtomoPanel pnlMinimizationParams = new EtomoPanel();
  private final LabeledTextField ltfMetroFactor = new LabeledTextField(
      "Metro factor: ");
  private final LabeledTextField ltfCycleLimit = new LabeledTextField(
      "Cycle limit: ");

  private final EtomoPanel pnlLocalParameters = new EtomoPanel();
  private final EtomoPanel pnlLocalParametersBody = new EtomoPanel();
  private final SpacedPanel pnlLocalPatches = SpacedPanel.getInstance(true);
  private final CheckBox cbLocalAlignments = new CheckBox(
      "Enable local alignments");
  private final ButtonGroup bgLocalPatches = new ButtonGroup();
  private final RadioTextField rtfTargetPatchSizeXandY = RadioTextField
      .getInstance("Target patch size (x,y): ", bgLocalPatches);
  private final RadioTextField rtfNLocalPatches = RadioTextField.getInstance(
      "# of local patches (x,y): ", bgLocalPatches);
  private final LabeledTextField ltfMinLocalPatchSize = new LabeledTextField(
      MIN_LOCAL_PATCH_SIZE_OVERLAP_ONLY_LABEL);
  private final LabeledTextField ltfMinLocalFiducials = new LabeledTextField(
      "Min. # of fiducials (total, each surface): ");
  private final CheckBox cbFixXYZCoordinates = new CheckBox(
      "Use global X-Y-Z coordinates");

  //  Global variables pane
  private final EtomoPanel pnlGlobalVariable = new EtomoPanel();
  private final JPanel pnlGlobalVariableBody = new JPanel();

  //  Tilt angle pane
  private final RadioButton rbTiltAngleFixed = new RadioButton(
      "Fixed tilt angles");
  private final RadioButton rbTiltAngleAll = new RadioButton(
      "Solve for all except minimum tilt");
  private final RadioButton rbTiltAngleAutomap = new RadioButton(
      "Group tilt angles ");
  private final ButtonGroup bgTiltAngleSolution = new ButtonGroup();
  private final EtomoPanel pnlTiltAngleSolution = new EtomoPanel();

  private final LabeledTextField ltfTiltAngleGroupSize = new LabeledTextField(
      "Group size: ");
  private final LabeledTextField ltfTiltAngleNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  //  Magnfication pane
  private final RadioButton rbMagnificationFixed = new RadioButton(
      "Fixed magnification at 1.0");
  private final RadioButton rbMagnificationAll = new RadioButton(
      "Solve for all magnifications");
  private RadioButton rbMagnificationAutomap = new RadioButton(
      "Group magnifications");
  private final ButtonGroup bgMagnificationSolution = new ButtonGroup();
  private final EtomoPanel pnlMagnificationSolution = new EtomoPanel();

  private final LabeledTextField ltfMagnificationReferenceView = new LabeledTextField(
      "Reference view: ");
  private final LabeledTextField ltfMagnificationGroupSize = new LabeledTextField(
      "Group size: ");
  private final LabeledTextField ltfMagnificationNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  // GlobalDistortion pane
  private final EtomoPanel pnlDistortionSolution = new EtomoPanel();
  private final RadioButton rbDistortionDisabled = new RadioButton("Disabled");
  private final RadioButton rbDistortionFullSolution = new RadioButton(
      "Full solution");
  private final RadioButton rbDistortionSkew = new RadioButton("Skew only");
  private final ButtonGroup bgDistortionSolution = new ButtonGroup();

  private final LabeledTextField ltfXstretchGroupSize = new LabeledTextField(
      "X stretch group size: ");
  private final LabeledTextField ltfXstretchNonDefaultGroups = new LabeledTextField(
      "X stretch non-default grouping: ");

  private final LabeledTextField ltfSkewGroupSize = new LabeledTextField(
      "Skew group size: ");
  private final LabeledTextField ltfSkewNonDefaultGroups = new LabeledTextField(
      "Skew non-default grouping: ");

  //  Local variables pane
  private final EtomoPanel pnlLocalSolution = new EtomoPanel();
  private final JPanel pnlLocalSolutionBody = new JPanel();

  //  Local rotation pane
  private final EtomoPanel pnlLocalRotationSolution = new EtomoPanel();
  private final CheckBox cbLocalRotation = new CheckBox("Enable");

  private final LabeledTextField ltfLocalRotationGroupSize = new LabeledTextField(
      "Group size: ");
  private final LabeledTextField ltfLocalRotationNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  //  Local tilt angle pane
  private final EtomoPanel pnlLocalTiltAngleSolution = new EtomoPanel();
  private final CheckBox cbLocalTiltAngle = new CheckBox("Enable");

  private final LabeledTextField ltfLocalTiltAngleGroupSize = new LabeledTextField(
      "Group size: ");
  private final LabeledTextField ltfLocalTiltAngleNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  // Local magnfication pane
  private final EtomoPanel pnlLocalMagnificationSolution = new EtomoPanel();
  private final CheckBox cbLocalMagnification = new CheckBox("Enable");

  private final LabeledTextField ltfLocalMagnificationGroupSize = new LabeledTextField(
      "Group size: ");
  private final LabeledTextField ltfLocalMagnificationNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");

  //  Local distortion pane
  private final EtomoPanel pnlLocalDistortionSolution = new EtomoPanel();
  private final RadioButton rbLocalDistortionDisabled = new RadioButton(
      "Disabled");
  private final RadioButton rbLocalDistortionFullSolution = new RadioButton(
      "Full solution");
  private final RadioButton rbLocalDistortionSkew = new RadioButton("Skew only");
  private final ButtonGroup bgLocalDistortionSolution = new ButtonGroup();

  private final LabeledTextField ltfLocalXstretchGroupSize = new LabeledTextField(
      "X stretch group size: ");
  private final LabeledTextField ltfLocalXstretchNonDefaultGroups = new LabeledTextField(
      "X stretch non-default grouping: ");

  private final LabeledTextField ltfLocalSkewGroupSize = new LabeledTextField(
      "Skew group size: ");
  private final LabeledTextField ltfLocalSkewNonDefaultGroups = new LabeledTextField(
      "Skew non-default grouping: ");

  //  Rotation pane
  private final RadioButton rbRotationNone = new RadioButton("No rotation");
  private final RadioButton rbRotationAll = new RadioButton(
      "Solve for all rotations");
  private final RadioButton rbRotationAutomap = new RadioButton(
      "Group rotations");
  private final RadioButton rbRotationOne = new RadioButton("One rotation");
  private final ButtonGroup bgRotationSolution = new ButtonGroup();
  private final EtomoPanel pnlRotationSolution = new EtomoPanel();
  private final LabeledTextField ltfRotationAngle = new LabeledTextField(
      "Rotation angle: ");
  private final LabeledTextField ltfRotationGroupSize = new LabeledTextField(
      "Group size: ");
  private final LabeledTextField ltfRotationNonDefaultGroups = new LabeledTextField(
      "Non-default grouping: ");
  private final CheckBox cbProjectionStretch = new CheckBox(
      "Solve for single stretch during projection");
  private final ApplicationManager appMgr;
  private final ButtonGroup bgBeamTiltOption = new ButtonGroup();
  private final RadioButton rbNoBeamTilt = new RadioButton("No beam tilt");
  private final RadioTextField rtfFixedBeamTilt = RadioTextField.getInstance(
      "Fixed beam tilt (degrees): ", bgBeamTiltOption);
  private final RadioButton rbSolveForBeamTilt = new RadioButton(
      "Solve for beam tilt");
  private final EtomoPanel pnlBeamTilt = new EtomoPanel();
  private final JPanel pnlBeamTiltBody = new JPanel();
  private final PanelHeader phBeamTilt;

  private Tab currentTab = Tab.GENERAL;
  private boolean patchTracking = false;

  private TiltalignPanel(final AxisID axis, final ApplicationManager appMgr,
      GlobalExpandButton globalAdvancedButton) {
    this.appMgr = appMgr;
    axisID = axis;
    tabPane.setBorder(new EtchedBorder("Tiltalign Parameters").getBorder());
    phBeamTilt = PanelHeader.getAdvancedBasicOnlyInstance("Beam Tilt", this,
        DialogType.FINE_ALIGNMENT, globalAdvancedButton);
    globalAdvancedButton.register(this);
    //  Create the tabs
    createGeneralTab();
    createGlobalSolutionTab();
    createLocalSolutionTab();
    setToolTipText();
  }

  /**
   * Construct a local instance and add listeners
   * @param axis
   * @param appMgr
   * @return local instance of TiltalignPanel
   */
  static TiltalignPanel getInstance(final AxisID axis,
      final ApplicationManager appMgr, GlobalExpandButton globalAdvancedButton) {
    TiltalignPanel tiltalignPanel = new TiltalignPanel(axis, appMgr,
        globalAdvancedButton);
    tiltalignPanel.addListeners();
    return tiltalignPanel;
  }

  private void addListeners() {
    ResidualRadioListener residualRadioListener = new ResidualRadioListener(
        this);
    rbResidAllViews.addActionListener(residualRadioListener);
    rbResidNeighboring.addActionListener(residualRadioListener);
    FiducialRadioListener fiducialRadioListener = new FiducialRadioListener(
        this);
    rbSingleFiducialSurface.addActionListener(fiducialRadioListener);
    rbDualFiducialSurfaces.addActionListener(fiducialRadioListener);
    LocalAlignmentsListener localAlignmentsListener = new LocalAlignmentsListener(
        this);
    cbLocalAlignments.addActionListener(localAlignmentsListener);
    RotationRadioListener rotationRadioListener = new RotationRadioListener(
        this);
    rbRotationNone.addActionListener(rotationRadioListener);
    rbRotationAll.addActionListener(rotationRadioListener);
    rbRotationAutomap.addActionListener(rotationRadioListener);
    rbRotationOne.addActionListener(rotationRadioListener);
    TiltAngleRadioListener tiltAngleRadioListener = new TiltAngleRadioListener(
        this);
    rbTiltAngleFixed.addActionListener(tiltAngleRadioListener);
    rbTiltAngleAll.addActionListener(tiltAngleRadioListener);
    rbTiltAngleAutomap.addActionListener(tiltAngleRadioListener);
    MagnificationRadioListener magnificationRadioListener = new MagnificationRadioListener(
        this);
    rbMagnificationFixed.addActionListener(magnificationRadioListener);
    rbMagnificationAll.addActionListener(magnificationRadioListener);
    rbMagnificationAutomap.addActionListener(magnificationRadioListener);
    DistortionRadioListener distortionRadioListener = new DistortionRadioListener(
        this);
    rbDistortionDisabled.addActionListener(distortionRadioListener);
    rbDistortionFullSolution.addActionListener(distortionRadioListener);
    rbDistortionSkew.addActionListener(distortionRadioListener);
    LocalRotationCheckListener localRotationCheckListener = new LocalRotationCheckListener(
        this);
    cbLocalRotation.addActionListener(localRotationCheckListener);
    LocalTiltAngleCheckListener localTiltAngleCheckListener = new LocalTiltAngleCheckListener(
        this);
    cbLocalTiltAngle.addActionListener(localTiltAngleCheckListener);
    LocalMagnificationCheckListener localMagnificationCheckListener = new LocalMagnificationCheckListener(
        this);
    cbLocalMagnification.addActionListener(localMagnificationCheckListener);
    LocalDistortionRadioListener localDistortionRadioListener = new LocalDistortionRadioListener(
        this);
    rbLocalDistortionDisabled.addActionListener(localDistortionRadioListener);
    rbLocalDistortionFullSolution
        .addActionListener(localDistortionRadioListener);
    rbLocalDistortionSkew.addActionListener(localDistortionRadioListener);
    TPActionListener tpActionListener = new TPActionListener(this);
    rtfTargetPatchSizeXandY.addActionListener(tpActionListener);
    rtfNLocalPatches.addActionListener(tpActionListener);
    tabPane.addChangeListener(new TabChangeListener(this));
    rbNoBeamTilt.addActionListener(tpActionListener);
    rtfFixedBeamTilt.addActionListener(tpActionListener);
    rbSolveForBeamTilt.addActionListener(tpActionListener);
  }

  private void changeTab(ChangeEvent changeEvent) {
    updateTab(false);
    currentTab = Tab.getInstance(tabPane.getSelectedIndex());
    updateTab(true);
    UIHarness.INSTANCE.pack(axisID, appMgr);
  }

  private void updateTab(boolean visible) {
    if (currentTab == Tab.GENERAL) {
      pnlGeneralBody.setVisible(visible);
    }
    else if (currentTab == Tab.GLOBAL_VARIABLES) {
      pnlGlobalVariableBody.setVisible(visible);
    }
    else if (currentTab == Tab.LOCAL_VARIABLES) {
      pnlLocalSolutionBody.setVisible(visible);
    }
  }

  /**
   * More field enabling.  Enable/disable global radio buttons based on which
   * radio buttons are selected.
   */
  private void updateDisplay() {
    //Beam Tilt
    //Don't disable a field which is selected.  This shouldn't come up unless the
    //comscript was changed.  In this case the problem will be handled by a tiltalign
    //error message.
    //Rotation
    //Solve for all rotations:
    boolean keepEnabled = rbRotationAll.isSelected();
    //Group and solve rotations:  disable if full or skew AND solve beam tilt
    //selected.
    boolean disable = (rbDistortionFullSolution.isSelected() || rbDistortionSkew
        .isSelected())
        && rbSolveForBeamTilt.isSelected();
    rbRotationAll.setEnabled(keepEnabled || !disable);
    //Group Rotations:
    keepEnabled = rbRotationAutomap.isSelected();
    rbRotationAutomap.setEnabled(keepEnabled || !disable);
    //Distortion
    //Distortion:  Full solution:
    keepEnabled = rbDistortionFullSolution.isSelected();
    //Full and skew:  disabled if All rotations or Group rotations AND solve beam
    //tilt is on
    disable = (rbRotationAll.isSelected() || rbRotationAutomap.isSelected())
        && rbSolveForBeamTilt.isSelected();
    rbDistortionFullSolution.setEnabled(keepEnabled || !disable);
    //Distortion:  Skew only:
    keepEnabled = rbDistortionSkew.isSelected();
    rbDistortionSkew.setEnabled(keepEnabled || !disable);
    //Beam Tilt
    //Solve for beam tilt:
    keepEnabled = rbSolveForBeamTilt.isSelected();
    //Solve for beam tilt:  disable if All rotations or Group rotations AND Full or
    //Skew distortion selected.
    disable = (rbRotationAll.isSelected() || rbRotationAutomap.isSelected())
        && (rbDistortionFullSolution.isSelected() || rbDistortionSkew
            .isSelected());
    rbSolveForBeamTilt.setEnabled(keepEnabled || !disable);
  }

  private void action(ActionEvent actionEvent) {
    String actionCommand = actionEvent.getActionCommand();
    if (actionCommand.equals(rtfTargetPatchSizeXandY.getActionCommand())) {
      setMinLocalPatchSizeLabel();
    }
    else if (actionCommand.equals(rtfNLocalPatches.getActionCommand())) {
      setMinLocalPatchSizeLabel();
    }
    else if (actionCommand.equals(rbNoBeamTilt.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(rtfFixedBeamTilt.getActionCommand())) {
      updateDisplay();
    }
    else if (actionCommand.equals(rbSolveForBeamTilt.getActionCommand())) {
      updateDisplay();
    }
  }

  /**
   * The header only covers part of the advanced fields, so use the global
   * advanced button directly to expand.
   */
  public void expand(GlobalExpandButton button) {
    updateAdvanced(button.isExpanded());
    UIHarness.INSTANCE.pack(axisID, appMgr);
  }

  public void expand(ExpandButton button) {
    if (phBeamTilt.equalsAdvancedBasic(button)) {
      updateAdvancedBeamTilt(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, appMgr);
  }

  private void updateAdvancedBeamTilt(boolean advanced) {
    pnlBeamTiltBody.setVisible(advanced);
  }

  private void setMinLocalPatchSizeLabel() {
    if (rtfTargetPatchSizeXandY.isSelected()) {
      ltfMinLocalPatchSize.setLabel(MIN_LOCAL_PATCH_SIZE_OVERLAP_ONLY_LABEL);
    }
    else if (rtfNLocalPatches.isSelected()) {
      ltfMinLocalPatchSize.setLabel(MIN_LOCAL_PATCH_SIZE_LABEL);
    }
  }

  /**
   * Set the values of the panel using a constant tiltalign parameter
   * object
   */
  void setParameters(final ConstTiltalignParam params) {
    //  General panel parameters
    if (params.getSurfacesToAnalyze().getInt() == 2) {
      rbDualFiducialSurfaces.setSelected(true);
    }
    else {
      rbSingleFiducialSurface.setSelected(true);
    }

    ltfResidualThreshold.setText(Math.abs(params.getResidualReportCriterion()
        .getDouble()));
    if (params.getResidualReportCriterion().getDouble() < 0) {
      rbResidNeighboring.setSelected(true);
    }
    else {
      rbResidAllViews.setSelected(true);
    }

    if (params.isExcludeListAvailable()) {
      ltfExcludeList.setEnabled(true);
      ltfExcludeList.setText(params.getExcludeList());
    }
    else {
      ltfExcludeList.setEnabled(false);
    }

    ltfSeparateViewGroups.setText(params.getSeparateGroup());
    ltfTiltAngleOffset.setText(params.getAngleOffset().toString());
    ltfTiltAxisZShift.setText(params.getAxisZShift().toString());

    ltfMetroFactor.setText(params.getMetroFactor().toString());
    ltfCycleLimit.setText(params.getMaximumCycles().toString());

    cbLocalAlignments.setSelected(params.getLocalAlignments().is());
    if (!params.isTargetPatchSizeXandYEmpty()) {
      rtfTargetPatchSizeXandY.setSelected(true);
      rtfTargetPatchSizeXandY.setText(params.getTargetPatchSizeXandY());
    }
    else if (!params.isNumberOfLocalPatchesXandYEmpty()) {
      rtfNLocalPatches.setSelected(true);
      rtfNLocalPatches.setText(params.getNumberOfLocalPatchesXandY());
    }
    setMinLocalPatchSizeLabel();
    ltfMinLocalPatchSize.setText(params.getMinSizeOrOverlapXandY());
    ltfMinLocalFiducials.setText(params.getMinFidsTotalAndEachSurface());
    cbFixXYZCoordinates.setSelected(params.getFixXYZCoordinates().is());

    //  Tilt angle solution parameters
    int solutionType = params.getTiltOption().getInt();
    if (solutionType == 0) {
      rbTiltAngleFixed.setSelected(true);
    }
    if (solutionType == 2) {
      rbTiltAngleAll.setSelected(true);
    }
    if (solutionType == 5) {
      rbTiltAngleAutomap.setSelected(true);
    }
    ltfTiltAngleGroupSize.setText(params.getTiltDefaultGrouping().toString());
    ltfTiltAngleNonDefaultGroups.setText(params.getTiltNondefaultGroup());

    //  Magnification solution parameters
    //  TODO what to do if the magnification type is not one of the cases
    //  below
    ltfMagnificationReferenceView.setText(params.getMagReferenceView()
        .toString());
    solutionType = params.getMagOption().getInt();
    if (solutionType == 0) {
      rbMagnificationFixed.setSelected(true);
    }
    if (solutionType == 1) {
      rbMagnificationAll.setSelected(true);
    }
    if (solutionType == TiltalignParam.AUTOMAPPED_OPTION) {
      rbMagnificationAutomap.setSelected(true);
    }
    ltfMagnificationGroupSize
        .setText(params.getMagDefaultGrouping().toString());
    ltfMagnificationNonDefaultGroups.setText(params.getMagNondefaultGroup());

    //  Rotation solution parameters
    solutionType = params.getRotOption().getInt();
    if (solutionType == 0) {
      rbRotationNone.setSelected(true);
    }
    if (solutionType == 1) {
      rbRotationAll.setSelected(true);
    }
    if (solutionType == TiltalignParam.AUTOMAPPED_OPTION) {
      rbRotationAutomap.setSelected(true);
    }
    if (solutionType == TiltalignParam.SINGLE_OPTION) {
      rbRotationOne.setSelected(true);
    }
    ltfRotationAngle.setText(params.getRotationAngle().toString());
    ltfRotationGroupSize.setText(params.getRotDefaultGrouping().toString());
    ltfRotationNonDefaultGroups.setText(params.getRotNondefaultGroup());

    //  Compression solution parameters
    /*
     ltfCompressionReferenceView.setText(
     params.getCompressionSolutionReferenceView());
     solutionType = params.getCompressionSolutionType();
     if (solutionType == 1) {
     rbCompressionAll.setSelected(true);
     }
     if (solutionType == 3) {
     rbCompressionAutomapLinear.setSelected(true);
     }
     if (solutionType == 4) {
     rbCompressionAutomapFixed.setSelected(true);
     }
     
     if (solutionType > 2) {
     ltfCompressionGroupSize.setText(params.getCompressionSolutionGroupSize());
     ltfCompressionAdditionalGroups.setText(
     params.getCompressionSolutionAdditionalGroups());
     }
     */
    //  Global distortion solution type
    int xStretchSolutionType = params.getXStretchOption().getInt();
    int skewSolutionType = params.getSkewOption().getInt();
    if (xStretchSolutionType == 0 && skewSolutionType == 0) {
      rbDistortionDisabled.setSelected(true);
    }
    else if (xStretchSolutionType == 3 && skewSolutionType == 3) {
      rbDistortionFullSolution.setSelected(true);
    }
    else {
      this.rbDistortionSkew.setSelected(true);
    }
    ltfXstretchGroupSize
        .setText(params.getXStretchDefaultGrouping().toString());
    ltfXstretchNonDefaultGroups.setText(params.getXStretchNondefaultGroup());
    //   skew solution parameters
    ltfSkewGroupSize.setText(params.getSkewDefaultGrouping().toString());
    ltfSkewNonDefaultGroups.setText(params.getSkewNondefaultGroup());

    cbProjectionStretch.setSelected(params.getProjectionStretch().is());

    // Local rotation solution parameters
    // NOTE this is brittle since we are mapping a numeric value to a boolean
    // at David's request
    solutionType = params.getLocalRotOption().getInt();
    if (solutionType == 0) {
      cbLocalRotation.setSelected(false);
    }
    else {
      cbLocalRotation.setSelected(true);
    }
    ltfLocalRotationGroupSize.setText(params.getLocalRotDefaultGrouping()
        .toString());
    ltfLocalRotationNonDefaultGroups.setText(params
        .getLocalRotNondefaultGroup());

    // Local tilt angle solution parameters
    solutionType = params.getLocalTiltOption().getInt();
    if (solutionType == 0) {
      cbLocalTiltAngle.setSelected(false);
    }
    else {
      cbLocalTiltAngle.setSelected(true);
    }
    ltfLocalTiltAngleGroupSize.setText(params.getLocalTiltDefaultGrouping()
        .toString());
    ltfLocalTiltAngleNonDefaultGroups.setText(params
        .getLocalTiltNondefaultGroup());

    //  Local magnification solution parameters
    solutionType = params.getLocalMagOption().getInt();
    if (solutionType == 0) {
      cbLocalMagnification.setSelected(false);
    }
    else {
      cbLocalMagnification.setSelected(true);
    }
    ltfLocalMagnificationGroupSize.setText(params.getLocalMagDefaultGrouping()
        .toString());
    ltfLocalMagnificationNonDefaultGroups.setText(params
        .getLocalMagNondefaultGroup());

    //  Local distortion solution type
    xStretchSolutionType = params.getLocalXStretchOption().getInt();
    skewSolutionType = params.getLocalSkewOption().getInt();
    if (xStretchSolutionType == 0 && skewSolutionType == 0) {
      rbLocalDistortionDisabled.setSelected(true);
    }
    else if (xStretchSolutionType == 3 && skewSolutionType == 3) {
      rbLocalDistortionFullSolution.setSelected(true);
    }
    else {
      rbLocalDistortionSkew.setSelected(true);
    }
    ltfLocalXstretchGroupSize.setText(params.getLocalXStretchDefaultGrouping()
        .toString());
    ltfLocalXstretchNonDefaultGroups.setText(params
        .getLocalXStretchNondefaultGroup());
    //  Local skew solution parameters
    ltfLocalSkewGroupSize.setText(params.getLocalSkewDefaultGrouping()
        .toString());
    ltfLocalSkewNonDefaultGroups.setText(params.getLocalSkewNondefaultGroup());
    //rest of the radio buttons must be set from metadata because they both
    //correspond to BeamTiltOption == 0
    if (params.getBeamTiltOption().equals(TiltalignParam.BEAM_SEARCH_OPTION)) {
      rbSolveForBeamTilt.setSelected(true);
    }
    //Only using FixedOrInitialBeamTilt when BeamTiltOption==0
    if (rtfFixedBeamTilt.isSelected()) {
      rtfFixedBeamTilt.setText(params.getFixedOrInitialBeamTilt());
    }
    //  Set the UI to match the data
    enableFields();
    updateDisplay();
  }

  void getParameters(final MetaData metaData) {
    metaData.setTargetPatchSizeXandY(rtfTargetPatchSizeXandY.getText());
    metaData.setNumberOfLocalPatchesXandY(rtfNLocalPatches.getText());
    metaData.setNoBeamTiltSelected(axisID, rbNoBeamTilt.isSelected());
    metaData.setFixedBeamTiltSelected(axisID, rtfFixedBeamTilt.isSelected());
    metaData.setFixedBeamTilt(axisID, rtfFixedBeamTilt.getText());
  }

  /**
   * Backwards compatibility: setParameters(ConstMetaData) must be called before
   * setParameters(TiltalignParam).
   */
  void setParameters(final ConstMetaData metaData) {
    rtfTargetPatchSizeXandY.setText(metaData.getTargetPatchSizeXandY());
    rtfNLocalPatches.setText(metaData.getNumberOfLocalPatchesXandY());
    rbNoBeamTilt.setSelected(metaData.getNoBeamTiltSelected(axisID).is());
    rtfFixedBeamTilt
        .setSelected(metaData.getFixedBeamTiltSelected(axisID).is());
    rtfFixedBeamTilt.setText(metaData.getFixedBeamTilt(axisID));
    updateDisplay();
  }

  public void setParameters(ReconScreenState screenState) {
    phBeamTilt.setState(screenState.getFineAlignBeamTiltHeaderState());
  }

  public void setPatchTracking(boolean input) {
    patchTracking = input;
  }

  /**
   * Selects a fiducial surface radio button depending on surfacesToAnalyze.
   * Only the surfacesToAnalyze values 1 and 2 have an effect.
   * @param surfacesToAnalyze
   */
  void setSurfacesToAnalyze(int surfacesToAnalyze) {
    if (surfacesToAnalyze == 1) {
      rbSingleFiducialSurface.setSelected(true);
    }
    else if (surfacesToAnalyze == 2) {
      rbDualFiducialSurfaces.setSelected(true);
    }
  }

  public void getParameters(ReconScreenState screenState) {
    phBeamTilt.getState(screenState.getFineAlignBeamTiltHeaderState());
  }

  /**
   * Get the values from the panel by updating tiltalign parameter
   * object.  Currently this makes the assumption that the argument
   * contains valid parameters and that only the known parameters will
   * be changed.
   * getParameters(MetaData) must be called before getParameters(TiltalignParam).
   */
  void getParameters(final TiltalignParam params)
      throws FortranInputSyntaxException {
    String badParameter = "";
    params.setImagesAreBinned(UIExpertUtilities.INSTANCE.getStackBinning(
        appMgr, axisID, ".preali"));
    try {
      if (rbDualFiducialSurfaces.isSelected()) {
        params.setSurfacesToAnalyze(2);
      }
      else {
        params.setSurfacesToAnalyze(1);
      }

      badParameter = ltfResidualThreshold.getLabel();
      double resid = Double.parseDouble(ltfResidualThreshold.getText());
      if (rbResidNeighboring.isSelected()) {
        resid *= -1;
      }
      params.setResidualReportCriterion(resid);

      //  Currently only supports Exclude list or blank entries
      badParameter = ltfExcludeList.getLabel();
      if (ltfExcludeList.isEnabled()) {
        params.setExcludeList(ltfExcludeList.getText());
      }
      badParameter = ltfSeparateViewGroups.getLabel();
      params.setSeparateGroup(ltfSeparateViewGroups.getText());

      badParameter = ltfTiltAngleOffset.getLabel();
      params.setAngleOffset(ltfTiltAngleOffset.getText());

      badParameter = ltfTiltAxisZShift.getLabel();
      params.setAxisZShift(ltfTiltAxisZShift.getText());

      badParameter = ltfMetroFactor.getLabel();
      params.setMetroFactor(ltfMetroFactor.getText());

      badParameter = ltfCycleLimit.getLabel();
      params.setMaximumCycles(ltfCycleLimit.getText());

      badParameter = cbLocalAlignments.getText();
      params.setLocalAlignments(cbLocalAlignments.isSelected());

      badParameter = rtfTargetPatchSizeXandY.getLabel();
      params
          .setTargetPatchSizeXandYActive(rtfTargetPatchSizeXandY.isSelected());
      params.setTargetPatchSizeXandY(rtfTargetPatchSizeXandY.getText());

      badParameter = rtfNLocalPatches.getLabel();
      params.setNumberOfLocalPatchesXandYActive(rtfNLocalPatches.isSelected());
      params.setNumberOfLocalPatchesXandY(rtfNLocalPatches.getText());

      badParameter = ltfMinLocalPatchSize.getLabel();
      params.setMinSizeOrOverlapXandY(ltfMinLocalPatchSize.getText());

      badParameter = ltfMinLocalFiducials.getLabel();
      params.setMinFidsTotalAndEachSurface(ltfMinLocalFiducials.getText());

      badParameter = cbFixXYZCoordinates.getText();
      params.setFixXYZCoordinates(cbFixXYZCoordinates.isSelected());

      // Tilt angle pane
      int type = 0;
      if (rbTiltAngleFixed.isSelected())
        type = 0;
      if (rbTiltAngleAll.isSelected())
        type = 2;
      if (rbTiltAngleAutomap.isSelected())
        type = 5;
      params.setTiltOption(type);
      badParameter = ltfTiltAngleGroupSize.getLabel();
      params.setTiltDefaultGrouping(ltfTiltAngleGroupSize.getText());

      badParameter = ltfTiltAngleNonDefaultGroups.getLabel();
      params.setTiltNondefaultGroup(ltfTiltAngleNonDefaultGroups.getText());

      // Magnification pane
      badParameter = ltfMagnificationReferenceView.getLabel();
      params.setMagReferenceView(ltfMagnificationReferenceView.getText());

      if (rbMagnificationFixed.isSelected())
        type = 0;
      if (rbMagnificationAll.isSelected())
        type = 1;
      if (rbMagnificationAutomap.isSelected())
        type = TiltalignParam.AUTOMAPPED_OPTION;
      params.setMagOption(type);

      badParameter = ltfMagnificationGroupSize.getLabel();
      params.setMagDefaultGrouping(ltfMagnificationGroupSize.getText());

      badParameter = ltfMagnificationNonDefaultGroups.getLabel();
      params.setMagNondefaultGroup(ltfMagnificationNonDefaultGroups.getText());

      // Rotation pane
      if (rbRotationNone.isSelected())
        type = 0;
      if (rbRotationAll.isSelected())
        type = 1;
      if (rbRotationAutomap.isSelected())
        type = TiltalignParam.AUTOMAPPED_OPTION;
      if (rbRotationOne.isSelected()) {
        type = TiltalignParam.SINGLE_OPTION;
      }
      params.setRotOption(type);
      badParameter = ltfRotationAngle.getLabel();
      params.setRotationAngle(ltfRotationAngle.getText());
      badParameter = ltfRotationGroupSize.getLabel();
      params.setRotDefaultGrouping(ltfRotationGroupSize.getText());
      badParameter = ltfRotationNonDefaultGroups.getLabel();
      params.setRotNondefaultGroup(ltfRotationNonDefaultGroups.getText());

      // Distortion pane
      type = 0;
      //  Set the necessary types for distortion xstretch and skew
      if (rbDistortionDisabled.isSelected()) {
        params.setSkewOption(TiltalignParam.FIXED_OPTION);
        params.setXStretchOption(TiltalignParam.FIXED_OPTION);
      }
      else {
        params.setSkewOption(TiltalignParam.AUTOMAPPED_OPTION);
        if (rbDistortionFullSolution.isSelected()) {
          params.setXStretchOption(TiltalignParam.AUTOMAPPED_OPTION);
        }
        else {
          params.setXStretchOption(TiltalignParam.FIXED_OPTION);
        }
      }

      badParameter = ltfSkewGroupSize.getLabel();
      params.setSkewDefaultGrouping(ltfSkewGroupSize.getText());

      badParameter = ltfSkewNonDefaultGroups.getLabel();
      params.setSkewNondefaultGroup(ltfSkewNonDefaultGroups.getText());

      badParameter = ltfXstretchGroupSize.getLabel();
      params.setXStretchDefaultGrouping(ltfXstretchGroupSize.getText());

      badParameter = ltfXstretchNonDefaultGroups.getLabel();
      params.setXStretchNondefaultGroup(ltfXstretchNonDefaultGroups.getText());

      badParameter = cbProjectionStretch.getText();
      params.setProjectionStretch(cbProjectionStretch.isSelected());

      //  Get the local alignment parameters
      // Rotation pane
      // NOTE this only works if 0 and 5 are valid local tilt angle codes
      type = 0;
      if (cbLocalRotation.isSelected())
        type = params.getLocalRotOption().getDisplayInteger();
      params.setLocalRotOption(type);
      badParameter = ltfLocalRotationGroupSize.getLabel();
      params.setLocalRotDefaultGrouping(ltfLocalRotationGroupSize.getText());

      badParameter = ltfLocalRotationNonDefaultGroups.getLabel();
      params.setLocalRotNondefaultGroup(ltfLocalRotationNonDefaultGroups
          .getText());

      // Tilt angle pane
      type = 0;
      if (cbLocalTiltAngle.isSelected())
        type = params.getLocalTiltOption().getDisplayInteger();
      params.setLocalTiltOption(type);
      badParameter = ltfLocalTiltAngleGroupSize.getLabel();
      params.setLocalTiltDefaultGrouping(ltfLocalTiltAngleGroupSize.getText());

      badParameter = ltfLocalTiltAngleNonDefaultGroups.getLabel();
      params.setLocalTiltNondefaultGroup(ltfLocalTiltAngleNonDefaultGroups
          .getText());

      // Local magnification pane
      if (cbLocalMagnification.isSelected()) {
        params
            .setLocalMagOption(params.getLocalMagOption().getDisplayInteger());
      }
      else {
        params.setLocalMagOption(0);
      }

      badParameter = ltfLocalMagnificationGroupSize.getLabel();
      params.setLocalMagDefaultGrouping(ltfLocalMagnificationGroupSize
          .getText());

      badParameter = ltfLocalMagnificationNonDefaultGroups.getLabel();
      params.setLocalMagNondefaultGroup(ltfLocalMagnificationNonDefaultGroups
          .getText());

      // Distortion pane
      type = 0;
      if (rbLocalDistortionDisabled.isSelected()) {
        params.setLocalSkewOption(TiltalignParam.FIXED_OPTION);
        params.setLocalXStretchOption(TiltalignParam.FIXED_OPTION);
      }
      else {
        params.setLocalSkewOption(TiltalignParam.AUTOMAPPED_OPTION);
        if (rbLocalDistortionFullSolution.isSelected()) {
          params.setLocalXStretchOption(TiltalignParam.AUTOMAPPED_OPTION);
        }
        else {
          params.setLocalXStretchOption(TiltalignParam.FIXED_OPTION);
        }
      }
      badParameter = ltfLocalSkewGroupSize.getLabel();
      params.setLocalSkewDefaultGrouping(ltfLocalSkewGroupSize.getText());

      badParameter = ltfLocalSkewNonDefaultGroups.getLabel();
      params
          .setLocalSkewNondefaultGroup(ltfLocalSkewNonDefaultGroups.getText());

      badParameter = ltfLocalXstretchGroupSize.getLabel();
      params.setLocalXStretchDefaultGrouping(ltfLocalXstretchGroupSize
          .getText());

      badParameter = ltfLocalXstretchNonDefaultGroups.getLabel();
      params.setLocalXStretchNondefaultGroup(ltfLocalXstretchNonDefaultGroups
          .getText());
      //params needs to have other values set before it can set OutputZFactorFile
      params.setOutputZFactorFile();

      if (rbNoBeamTilt.isSelected()) {
        badParameter = rbNoBeamTilt.getText();
        params.setBeamTiltOption(TiltalignParam.FIXED_OPTION);
        params.resetFixedOrInitialBeamTilt();
      }
      else if (rtfFixedBeamTilt.isSelected()) {
        badParameter = rtfFixedBeamTilt.getLabel();
        params.setBeamTiltOption(TiltalignParam.FIXED_OPTION);
        params.setFixedOrInitialBeamTilt(rtfFixedBeamTilt.getText());
      }
      else if (rbSolveForBeamTilt.isSelected()) {
        badParameter = rbSolveForBeamTilt.getText();
        params.setBeamTiltOption(TiltalignParam.BEAM_SEARCH_OPTION);
      }
    }
    catch (FortranInputSyntaxException except) {
      String message = badParameter + " " + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  boolean isValid() {
    if (rtfFixedBeamTilt.isSelected() && rtfFixedBeamTilt.getText().equals("")) {
      UIHarness.INSTANCE.openMessageDialog(appMgr, rtfFixedBeamTilt.getLabel()
          + " can not be empty when it is selected.", "Entry Error");
      return false;
    }
    if (patchTracking && rbDualFiducialSurfaces.isSelected()) {
      UIHarness.INSTANCE.openMessageDialog(appMgr,
          "Patch tracking puts fiducials only on one side.  Select \""
              + rbSingleFiducialSurface.getText() + "\" for better results.",
          "Entry Warning", axisID);
      //This is just a warning so don't return false.
    }
    return true;
  }

  void setFirstTab() {
    tabPane.setSelectedComponent(pnlGeneral);
  }

  void updateAdvanced(final boolean state) {
    updateAdvancedBeamTilt(state);
    //    ltfMetroFactor.setVisible(state);
    //    ltfCycleLimit.setVisible(state);
    pnlMinimizationParams.setVisible(state);
    ltfMagnificationReferenceView.setVisible(state);
    ltfRotationNonDefaultGroups.setVisible(state);
    ltfTiltAngleNonDefaultGroups.setVisible(state);
    ltfMagnificationNonDefaultGroups.setVisible(state);
    ltfXstretchNonDefaultGroups.setVisible(state);
    ltfSkewNonDefaultGroups.setVisible(state);
    cbProjectionStretch.setVisible(state);
    ltfLocalRotationNonDefaultGroups.setVisible(state);
    ltfLocalTiltAngleNonDefaultGroups.setVisible(state);
    ltfLocalMagnificationNonDefaultGroups.setVisible(state);
    ltfLocalXstretchNonDefaultGroups.setVisible(state);
    ltfLocalSkewNonDefaultGroups.setVisible(state);
    ltfMinLocalPatchSize.setVisible(state);
    cbFixXYZCoordinates.setVisible(state);
  }

  //  Local alignment state
  private void enableLocalAlignmentFields() {
    boolean state = cbLocalAlignments.isSelected();
    rtfTargetPatchSizeXandY.setEnabled(state);
    rtfNLocalPatches.setEnabled(state);
    ltfMinLocalPatchSize.setEnabled(state);
    ltfMinLocalFiducials.setEnabled(state);
    cbFixXYZCoordinates.setEnabled(state);
    tabPane.setEnabledAt(tabPane.indexOfComponent(pnlLocalSolution), state);
  }

  /**
   * Signal each pane to update its enabled/disabled state.
   */
  private void enableFields() {
    //  update all of the enable/disable states
    enableLocalAlignmentFields();

    enableRotationSolutionFields();
    enableTiltAngleSolutionFields();
    enableMagnificationSolutionFields();
    enableDistortionSolutionFields();

    enableLocalRotationSolutionFields();
    enableLocalTiltAngleSolutionFields();
    enableLocalMagnificationSolutionFields();
    enableLocalDistortionSolutionFields();
  }

  /**
   * Update the enabled/disabled state of the specified solution panel.
   */
  private void enableTiltAngleSolutionFields() {
    boolean state = rbTiltAngleAutomap.isSelected();
    ltfTiltAngleGroupSize.setEnabled(state);
    ltfTiltAngleNonDefaultGroups.setEnabled(state);
  }

  private void enableMagnificationSolutionFields() {
    boolean state = rbMagnificationAutomap.isSelected();
    ltfMagnificationGroupSize.setEnabled(state);
    ltfMagnificationNonDefaultGroups.setEnabled(state);
  }

  private void enableRotationSolutionFields() {
    boolean state = rbRotationAutomap.isSelected();
    ltfRotationGroupSize.setEnabled(state);
    ltfRotationNonDefaultGroups.setEnabled(state);
    ltfRotationAngle.setEnabled(rbRotationNone.isSelected());
  }

  private void setDistortionSolutionState() {
    if (rbDistortionDisabled.isSelected()) {
      rbLocalDistortionDisabled.setSelected(true);
    }
    else {
      rbTiltAngleAutomap.setSelected(true);
      enableTiltAngleSolutionFields();
      if (rbDistortionFullSolution.isSelected()) {
        rbLocalDistortionFullSolution.setSelected(true);
      }
      else if (rbDistortionSkew.isSelected()) {
        rbLocalDistortionSkew.setSelected(true);
      }
    }
    enableLocalDistortionSolutionFields();
    enableDistortionSolutionFields();
  }

  private void enableDistortionSolutionFields() {
    boolean xStretchState = rbDistortionFullSolution.isSelected();
    ltfXstretchGroupSize.setEnabled(xStretchState);
    ltfXstretchNonDefaultGroups.setEnabled(xStretchState);
    boolean skewState = rbDistortionFullSolution.isSelected()
        || rbDistortionSkew.isSelected();
    ltfSkewGroupSize.setEnabled(skewState);
    ltfSkewNonDefaultGroups.setEnabled(skewState);
  }

  private void enableLocalRotationSolutionFields() {
    boolean state = cbLocalRotation.isSelected();
    ltfLocalRotationGroupSize.setEnabled(state);
    ltfLocalRotationNonDefaultGroups.setEnabled(state);
  }

  private void enableLocalTiltAngleSolutionFields() {
    boolean state = cbLocalTiltAngle.isSelected();
    ltfLocalTiltAngleGroupSize.setEnabled(state);
    ltfLocalTiltAngleNonDefaultGroups.setEnabled(state);
  }

  private void enableLocalMagnificationSolutionFields() {
    boolean state = cbLocalMagnification.isSelected();
    ltfLocalMagnificationGroupSize.setEnabled(state);
    ltfLocalMagnificationNonDefaultGroups.setEnabled(state);
  }

  private void enableLocalDistortionSolutionFields() {
    boolean xStretchState = rbLocalDistortionFullSolution.isSelected();
    ltfLocalXstretchGroupSize.setEnabled(xStretchState);
    ltfLocalXstretchNonDefaultGroups.setEnabled(xStretchState);
    boolean skewState = rbLocalDistortionSkew.isSelected()
        || rbLocalDistortionFullSolution.isSelected();
    ltfLocalSkewGroupSize.setEnabled(skewState);
    ltfLocalSkewNonDefaultGroups.setEnabled(skewState);
  }

  Container getContainer() {
    return tabPane;
  }

  /**
   * 
   * @param panel
   * @param group
   * @param items
   */
  private void createRadioBox(final JPanel panel, final ButtonGroup group,
      RadioButton[] items) {
    createRadioBox(panel, group, items, 245);
  }

  /**
   * 
   * @param panel
   * @param group
   * @param items
   * @param width
   */
  private void createRadioBox(final JPanel panel, final ButtonGroup group,
      RadioButton[] items, int width) {
    int radioButtonHeight = (int) (18 * UIParameters.INSTANCE
        .getFontSizeAdjustment());
    Dimension radioButtonItemSize = new Dimension(
        (int) (width * UIParameters.INSTANCE.getFontSizeAdjustment()),
        radioButtonHeight);

    // Add the items to the group and to the panel
    for (int i = 0; i < items.length; i++) {
      group.add(items[i].getAbstractButton());
      panel.add(items[i].getAbstractButton());
      items[i].setPreferredSize(radioButtonItemSize);
    }
  }

  /**
   * Layout the general parameters tab
   */
  private void createGeneralTab() {

    pnlGeneral.setLayout(new BoxLayout(pnlGeneral, BoxLayout.Y_AXIS));
    pnlGeneralBody.setLayout(new BoxLayout(pnlGeneralBody, BoxLayout.Y_AXIS));
    pnlGeneralBody.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlGeneralBody.add(ltfExcludeList.getContainer());
    pnlGeneralBody.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlGeneralBody.add(ltfSeparateViewGroups.getContainer());
    pnlGeneralBody.add(Box.createRigidArea(FixedDim.x0_y10));

    //Residual reporting
    pnlResidualThreshold.setLayout(new BoxLayout(pnlResidualThreshold,
        BoxLayout.Y_AXIS));
    pnlResidualThreshold.setBorder(new EtchedBorder("Residual Reporting")
        .getBorder());
    //top panel
    SpacedPanel topResidualPanel = SpacedPanel.getInstance();
    topResidualPanel.setBoxLayout(BoxLayout.X_AXIS);
    ltfResidualThreshold.setColumns(10);
    topResidualPanel.add(ltfResidualThreshold);
    topResidualPanel.add(new JLabel("s.d."));
    pnlResidualThreshold.add(topResidualPanel.getContainer());
    //bottom panel
    SpacedPanel bottomResidualPanel = SpacedPanel.getInstance();
    bottomResidualPanel.setBoxLayout(BoxLayout.X_AXIS);
    bottomResidualPanel.setComponentAlignmentX(Container.RIGHT_ALIGNMENT);
    bottomResidualPanel.add(new JLabel("Relative to"));
    //create radio button group
    RadioButton[] items = new RadioButton[2];
    items[0] = rbResidAllViews;
    items[1] = rbResidNeighboring;
    JPanel pnlRBResidual = new JPanel();
    pnlRBResidual.setLayout(new BoxLayout(pnlRBResidual, BoxLayout.Y_AXIS));
    createRadioBox(pnlRBResidual, bgResidualThreshold, items, 300);
    bottomResidualPanel.add(pnlRBResidual);
    pnlResidualThreshold.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlResidualThreshold.add(bottomResidualPanel.getContainer());

    pnlGeneralBody.add(pnlResidualThreshold);
    pnlGeneralBody.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlFiducialSurfaces.setLayout(new BoxLayout(pnlFiducialSurfaces,
        BoxLayout.X_AXIS));
    pnlFiducialSurfaces
        .setBorder(new EtchedBorder("Analysis of Surface Angles").getBorder());

    //  Need an extra panel to make border extend the appropriate width
    JPanel pnlRBFiducual = new JPanel();
    pnlRBFiducual.setLayout(new BoxLayout(pnlRBFiducual, BoxLayout.Y_AXIS));
    items = new RadioButton[2];
    items[0] = rbSingleFiducialSurface;
    items[1] = rbDualFiducialSurfaces;
    createRadioBox(pnlRBFiducual, bgFiducialSurfaces, items, 355);

    pnlFiducialSurfaces.add(pnlRBFiducual);
    pnlFiducialSurfaces.add(Box.createHorizontalGlue());
    pnlGeneralBody.add(pnlFiducialSurfaces);
    pnlGeneralBody.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlVolumeParameters.setLayout(new BoxLayout(pnlVolumeParameters,
        BoxLayout.Y_AXIS));
    pnlVolumeParameters
        .setBorder(new EtchedBorder("Volume Position Parameters").getBorder());
    pnlVolumeParameters.add(ltfTiltAngleOffset.getContainer());
    pnlVolumeParameters.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlVolumeParameters.add(ltfTiltAxisZShift.getContainer());
    pnlVolumeParameters.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlGeneralBody.add(pnlVolumeParameters);
    pnlGeneralBody.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlMinimizationParams.setLayout(new BoxLayout(pnlMinimizationParams,
        BoxLayout.Y_AXIS));
    pnlMinimizationParams.setBorder(new EtchedBorder("Minimization Parameters")
        .getBorder());
    pnlMinimizationParams.add(ltfMetroFactor.getContainer());
    pnlMinimizationParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMinimizationParams.add(ltfCycleLimit.getContainer());
    pnlMinimizationParams.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlGeneralBody.add(pnlMinimizationParams);
    pnlGeneralBody.add(Box.createRigidArea(FixedDim.x0_y10));

    //local alignment
    ltfMinLocalFiducials.setTextPreferredWidth(60 * UIParameters.INSTANCE
        .getFontSizeAdjustment());
    pnlLocalParameters.setLayout(new BoxLayout(pnlLocalParameters,
        BoxLayout.Y_AXIS));
    pnlLocalParameters.setBorder(new EtchedBorder("Local Alignment Parameters")
        .getBorder());
    cbLocalAlignments.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlLocalParameters.add(cbLocalAlignments);
    pnlLocalPatches.setBoxLayout(BoxLayout.Y_AXIS);
    pnlLocalPatches.setBorder(new EtchedBorder("Local Patch Layout:")
        .getBorder());
    pnlLocalPatches.add(rtfTargetPatchSizeXandY.getContainer());
    pnlLocalPatches.add(rtfNLocalPatches.getContainer());
    createVariablePanel(pnlLocalParametersBody, pnlLocalPatches.getContainer(),
        ltfMinLocalPatchSize, ltfMinLocalFiducials, null, null,
        cbFixXYZCoordinates, null, FixedDim.x10_y0);
    pnlLocalParameters.add(pnlLocalParametersBody);
    pnlGeneralBody.add(pnlLocalParameters);
    pnlGeneralBody.add(Box.createVerticalGlue());
    pnlGeneral.add(pnlGeneralBody);
    tabPane.addTab("General", pnlGeneral);
  }

  /**
   * Layout the global estimate tab
   */
  private void createGlobalSolutionTab() {
    pnlGlobalVariable.setLayout(new BoxLayout(pnlGlobalVariable,
        BoxLayout.Y_AXIS));
    pnlGlobalVariableBody.setLayout(new BoxLayout(pnlGlobalVariableBody,
        BoxLayout.Y_AXIS));

    //  Layout the global rotation variable parameters
    JPanel pnlRBRotation = new JPanel();
    pnlRBRotation.setLayout(new BoxLayout(pnlRBRotation, BoxLayout.Y_AXIS));
    RadioButton[] items = new RadioButton[4];
    items[0] = rbRotationNone;
    items[1] = rbRotationOne;
    items[2] = rbRotationAutomap;
    items[3] = rbRotationAll;
    createRadioBox(pnlRBRotation, bgRotationSolution, items);
    createVariablePanel(pnlRotationSolution, pnlRBRotation, ltfRotationAngle,
        ltfRotationGroupSize, ltfRotationNonDefaultGroups, null, null,
        "Rotation Solution Type", null);

    //  Layout the global tilt angle estimate pane
    JPanel pnlRBTiltAngle = new JPanel();
    pnlRBTiltAngle.setLayout(new BoxLayout(pnlRBTiltAngle, BoxLayout.Y_AXIS));
    items = new RadioButton[3];
    items[0] = rbTiltAngleFixed;
    items[1] = rbTiltAngleAutomap;
    items[2] = rbTiltAngleAll;
    createRadioBox(pnlRBTiltAngle, bgTiltAngleSolution, items);
    createVariablePanel(pnlTiltAngleSolution, pnlRBTiltAngle,
        ltfTiltAngleGroupSize, ltfTiltAngleNonDefaultGroups,
        "Tilt Angle Solution Type");

    //  Layout the global magnification variable parameters
    JPanel pnlRBMagnification = new JPanel();
    pnlRBMagnification.setLayout(new BoxLayout(pnlRBMagnification,
        BoxLayout.Y_AXIS));
    items = new RadioButton[3];
    items[0] = rbMagnificationFixed;
    items[1] = rbMagnificationAutomap;
    items[2] = rbMagnificationAll;
    createRadioBox(pnlRBMagnification, bgMagnificationSolution, items);
    createVariablePanel(pnlMagnificationSolution, pnlRBMagnification,
        ltfMagnificationReferenceView, ltfMagnificationGroupSize,
        ltfMagnificationNonDefaultGroups, null, null,
        "Magnification Solution Type", null);

    // Layout the global distortion pane

    //Create radio box
    items = new RadioButton[3];
    items[0] = rbDistortionDisabled;
    items[1] = rbDistortionFullSolution;
    items[2] = rbDistortionSkew;
    JPanel pnlRBDistortion = new JPanel();
    pnlRBDistortion.setLayout(new BoxLayout(pnlRBDistortion, BoxLayout.Y_AXIS));
    createRadioBox(pnlRBDistortion, bgDistortionSolution, items);
    ltfXstretchNonDefaultGroups.setTextPreferredWidth(UIParameters.INSTANCE
        .getIntegerTripletWidth());
    ltfXstretchGroupSize.setTextPreferredWidth(UIParameters.INSTANCE
        .getFourDigitWidth());
    createVariablePanel(pnlDistortionSolution, pnlRBDistortion,
        ltfXstretchGroupSize, ltfXstretchNonDefaultGroups, ltfSkewGroupSize,
        ltfSkewNonDefaultGroups, null, "Distortion Solution Type", null);

    //  Add the individual panes to the tab
    pnlGlobalVariableBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlGlobalVariableBody.add(pnlRotationSolution);
    pnlGlobalVariableBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlGlobalVariableBody.add(pnlMagnificationSolution);
    pnlGlobalVariableBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlGlobalVariableBody.add(Box.createVerticalGlue());
    pnlGlobalVariableBody.add(pnlTiltAngleSolution);
    pnlGlobalVariableBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlGlobalVariableBody.add(Box.createVerticalGlue());
    pnlGlobalVariableBody.add(pnlDistortionSolution);
    pnlGlobalVariableBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlGlobalVariableBody.add(Box.createVerticalGlue());
    //beam tilt
    pnlBeamTilt.setLayout(new BoxLayout(pnlBeamTilt, BoxLayout.Y_AXIS));
    pnlBeamTilt.setBorder(BorderFactory.createEtchedBorder());
    pnlBeamTilt.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlBeamTilt.add(phBeamTilt);
    //beam tilt body
    pnlBeamTiltBody.setLayout(new BoxLayout(pnlBeamTiltBody, BoxLayout.Y_AXIS));
    //no beam tilt
    bgBeamTiltOption.add(rbNoBeamTilt.getAbstractButton());
    pnlBeamTiltBody.add(rbNoBeamTilt.getComponent());
    //fixed beam tilt
    pnlBeamTiltBody.add(rtfFixedBeamTilt.getContainer());
    //solve for beam tilt
    bgBeamTiltOption.add(rbSolveForBeamTilt.getAbstractButton());
    pnlBeamTiltBody.add(rbSolveForBeamTilt.getComponent());
    UIUtilities.alignComponentsX(pnlBeamTiltBody, Component.LEFT_ALIGNMENT);
    pnlBeamTilt.add(pnlBeamTiltBody);
    pnlGlobalVariableBody.add(pnlBeamTilt);
    pnlGlobalVariableBody.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlGlobalVariableBody.add(Box.createVerticalGlue());
    cbProjectionStretch.setAlignmentX(Component.RIGHT_ALIGNMENT);
    pnlGlobalVariableBody.add(cbProjectionStretch);
    pnlGlobalVariable.add(pnlGlobalVariableBody);
    tabPane.addTab("Global Variables", pnlGlobalVariable);
    pnlGlobalVariableBody.setVisible(false);
  }

  private void createLocalSolutionTab() {
    //  Construct the local solution panel
    pnlLocalSolution
        .setLayout(new BoxLayout(pnlLocalSolution, BoxLayout.Y_AXIS));
    pnlLocalSolutionBody.setLayout(new BoxLayout(pnlLocalSolutionBody,
        BoxLayout.Y_AXIS));
    //pnlLocalSolution.setPreferredSize(new Dimension(400, 350));

    //  Construct the rotation solution objects
    createVariablePanel(pnlLocalRotationSolution, cbLocalRotation,
        ltfLocalRotationGroupSize, ltfLocalRotationNonDefaultGroups,
        "Local Rotation Solution Type");

    //  Construct the tilt angle solution objects
    createVariablePanel(pnlLocalTiltAngleSolution, cbLocalTiltAngle,
        ltfLocalTiltAngleGroupSize, ltfLocalTiltAngleNonDefaultGroups,
        "Local Tilt Angle Solution Type");

    //  Construct the local magnification pane
    createVariablePanel(pnlLocalMagnificationSolution, cbLocalMagnification,
        ltfLocalMagnificationGroupSize, ltfLocalMagnificationNonDefaultGroups,
        "Local Magnification Solution Type");

    //  Construction the local distortion pane

    //Create radio box
    RadioButton[] items = new RadioButton[3];
    items[0] = rbLocalDistortionDisabled;
    items[1] = rbLocalDistortionFullSolution;
    items[2] = rbLocalDistortionSkew;
    JPanel pnlRBLocalDistortion = new JPanel();
    pnlRBLocalDistortion.setLayout(new BoxLayout(pnlRBLocalDistortion,
        BoxLayout.Y_AXIS));
    createRadioBox(pnlRBLocalDistortion, bgLocalDistortionSolution, items);
    ltfLocalXstretchNonDefaultGroups
        .setTextPreferredWidth(UIParameters.INSTANCE.getIntegerTripletWidth());
    ltfLocalXstretchGroupSize.setTextPreferredWidth(UIParameters.INSTANCE
        .getFourDigitWidth());
    createVariablePanel(pnlLocalDistortionSolution, pnlRBLocalDistortion,
        ltfLocalXstretchGroupSize, ltfLocalXstretchNonDefaultGroups,
        ltfLocalSkewGroupSize, ltfLocalSkewNonDefaultGroups, null,
        "Local Distortion Solution Type", null);

    pnlLocalSolutionBody.add(Box.createVerticalGlue());
    pnlLocalSolutionBody.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlLocalSolutionBody.add(pnlLocalRotationSolution);

    pnlLocalSolutionBody.add(Box.createVerticalGlue());
    pnlLocalSolutionBody.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlLocalSolutionBody.add(pnlLocalMagnificationSolution);

    pnlLocalSolutionBody.add(Box.createVerticalGlue());
    pnlLocalSolutionBody.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlLocalSolutionBody.add(pnlLocalTiltAngleSolution);

    pnlLocalSolutionBody.add(Box.createVerticalGlue());
    pnlLocalSolutionBody.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlLocalSolutionBody.add(pnlLocalDistortionSolution);
    pnlLocalSolution.add(pnlLocalSolutionBody);
    tabPane.addTab("Local Variables", pnlLocalSolution);
    pnlLocalSolution.setVisible(false);
  }

  private void createVariablePanel(final EtomoPanel panel,
      final CheckBox checkBox, final LabeledTextField groupSize,
      final LabeledTextField additionalGroups, final String title) {
    createVariablePanel(panel, checkBox, groupSize, additionalGroups, null,
        title);
  }

  private void createVariablePanel(final EtomoPanel panel,
      final CheckBox checkBox, final LabeledTextField field1,
      final LabeledTextField field2, final LabeledTextField field3,
      final String title) {
    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
    buttonPanel.add(checkBox);
    createVariablePanel(panel, buttonPanel, field1, field2, field3, null, null,
        title, null);
  }

  private void createVariablePanel(final EtomoPanel panel,
      final CheckBox checkBox1, final LabeledTextField field1,
      final LabeledTextField field2, final LabeledTextField field3,
      final CheckBox checkBox2, final String title) {
    JPanel buttonPanel = new JPanel();
    buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
    buttonPanel.add(checkBox1);
    createVariablePanel(panel, buttonPanel, field1, field2, field3, null,
        checkBox2, title, null);
  }

  /*
   * create a variable panel with an internal panel (can contain a radio button
   * group
   */
  private void createVariablePanel(final EtomoPanel panel,
      final JPanel buttonPanel, final LabeledTextField groupSize,
      final LabeledTextField additionalGroups, final String title) {
    createVariablePanel(panel, buttonPanel, groupSize, additionalGroups, null,
        null, null, title, null);
  }

  private void createVariablePanel(final EtomoPanel panel,
      final Container buttonPanel, final LabeledTextField field1,
      final LabeledTextField field2, final LabeledTextField field3,
      final LabeledTextField field4, final CheckBox checkBox,
      final String title, Dimension spacing) {
    if (spacing == null) {
      spacing = FixedDim.x40_y0;
    }
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    //panel.add(Box.createRigidArea(FixedDim.x5_y0));
    panel.add(buttonPanel);
    panel.add(Box.createRigidArea(spacing));
    SpacedPanel fieldPanel = SpacedPanel.getInstance();
    fieldPanel.setBoxLayout(BoxLayout.Y_AXIS);
    fieldPanel.add(field1);
    if (field2 != null) {
      fieldPanel.add(field2);
    }
    if (field3 != null) {
      fieldPanel.add(field3);
    }
    if (field4 != null) {
      fieldPanel.add(field4);
    }
    if (checkBox != null) {
      checkBox.setAlignmentX(Component.RIGHT_ALIGNMENT);
      fieldPanel.add(checkBox);
    }
    panel.add(fieldPanel.getContainer());
    if (title != null) {
      panel.setBorder(new EtchedBorder(title).getBorder());
    }
  }

  private static final class ResidualRadioListener implements ActionListener {
    private final TiltalignPanel panel;

    private ResidualRadioListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
    }
  }

  private static final class FiducialRadioListener implements ActionListener {
    private final TiltalignPanel panel;

    private FiducialRadioListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
    }
  }

  private static final class TiltAngleRadioListener implements ActionListener {
    private final TiltalignPanel panel;

    private TiltAngleRadioListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.enableTiltAngleSolutionFields();
    }
  }

  private static final class MagnificationRadioListener implements
      ActionListener {
    private final TiltalignPanel panel;

    private MagnificationRadioListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.enableMagnificationSolutionFields();
    }
  }

  private static final class RotationRadioListener implements ActionListener {
    private final TiltalignPanel panel;

    private RotationRadioListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.enableRotationSolutionFields();
      panel.updateDisplay();
    }
  }

  private static final class DistortionRadioListener implements ActionListener {
    private final TiltalignPanel panel;

    private DistortionRadioListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.setDistortionSolutionState();
      panel.updateDisplay();
    }
  }

  private static final class LocalAlignmentsListener implements ActionListener {
    private final TiltalignPanel panel;

    private LocalAlignmentsListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.enableLocalAlignmentFields();
    }
  }

  private static final class LocalRotationCheckListener implements
      ActionListener {
    private final TiltalignPanel panel;

    private LocalRotationCheckListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.enableLocalRotationSolutionFields();
    }
  }

  private static final class LocalTiltAngleCheckListener implements
      ActionListener {
    private final TiltalignPanel panel;

    private LocalTiltAngleCheckListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.enableLocalTiltAngleSolutionFields();
    }
  }

  private static final class LocalMagnificationCheckListener implements
      ActionListener {
    private final TiltalignPanel panel;

    private LocalMagnificationCheckListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.enableLocalMagnificationSolutionFields();
    }
  }

  private static final class LocalDistortionRadioListener implements
      ActionListener {
    private final TiltalignPanel panel;

    private LocalDistortionRadioListener(final TiltalignPanel adaptee) {
      panel = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.enableLocalDistortionSolutionFields();
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    ReadOnlySection section;
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(appMgr, AutodocFactory.TILTALIGN,
          axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
    // General tab
    ltfExcludeList.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.EXCLUDE_LIST_KEY));
    ltfSeparateViewGroups.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.SEPARATE_GROUP_KEY));
    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        TiltalignParam.RESIDUAL_REPORT_CRITERION_KEY);
    if (section != null) {
      ltfResidualThreshold.setToolTipText(EtomoAutodoc.getTooltip(section));
      rbResidAllViews.setToolTipText(EtomoAutodoc.getTooltip(section, "all"));
      rbResidNeighboring.setToolTipText(EtomoAutodoc.getTooltip(section,
          "neighboring"));
    }
    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        TiltalignParam.SURFACES_TO_ANALYZE_KEY);
    if (section != null) {
      rbSingleFiducialSurface.setToolTipText(EtomoAutodoc.getTooltip(section,
          "1")
          + "  Use if fiducials are on one surface or distributed in Z.");
      rbDualFiducialSurfaces.setToolTipText(EtomoAutodoc.getTooltip(section,
          "2"));
    }
    ltfTiltAngleOffset.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.ANGLE_OFFSET_KEY));
    ltfTiltAxisZShift.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.AXIS_Z_SHIFT_KEY));
    ltfMetroFactor.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.METRO_FACTOR_KEY));
    ltfCycleLimit.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.MAXIMUM_CYCLES_KEY));
    cbLocalAlignments.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_ALIGNMENTS_KEY));
    rtfTargetPatchSizeXandY.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.TARGET_PATCH_SIZE_X_AND_Y_KEY));
    rtfNLocalPatches.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.NUMBER_OF_LOCAL_PATCHES_X_AND_Y_KEY));
    ltfMinLocalPatchSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.MIN_SIZE_OR_OVERLAP_X_AND_Y_KEY));
    ltfMinLocalFiducials.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.MIN_FIDS_TOTAL_AND_EACH_SURFACE_KEY));
    cbFixXYZCoordinates.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.FIX_XYZ_COORDINATES_KEY));
    //  Global variables
    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        TiltalignParam.TILT_OPTION_KEY);
    if (section != null) {
      rbTiltAngleFixed.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.FIXED_OPTION));
      rbTiltAngleAll.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.TILT_ALL_OPTION));
      rbTiltAngleAutomap.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.TILT_AUTOMAPPED_OPTION));
    }
    ltfTiltAngleGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.TILT_DEFAULT_GROUPING_KEY));
    ltfTiltAngleNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, TiltalignParam.TILT_NONDEFAULT_GROUP_KEY));

    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        TiltalignParam.MAG_OPTION_KEY);
    if (section != null) {
      rbMagnificationFixed.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.FIXED_OPTION));
      rbMagnificationAll.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.ALL_OPTION));
      rbMagnificationAutomap.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.AUTOMAPPED_OPTION));
    }
    ltfMagnificationReferenceView.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, TiltalignParam.MAG_REFERENCE_VIEW_KEY));
    ltfMagnificationGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.MAG_DEFAULT_GROUPING_KEY));
    ltfMagnificationNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, TiltalignParam.MAG_NONDEFAULT_GROUP_KEY));

    section = autodoc.getSection(EtomoAutodoc.FIELD_SECTION_NAME,
        TiltalignParam.ROT_OPTION_KEY);
    if (section != null) {
      rbRotationNone.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.NONE_OPTION));
      rbRotationAll.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.ALL_OPTION));
      rbRotationAutomap.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.AUTOMAPPED_OPTION));
      rbRotationOne.setToolTipText(EtomoAutodoc.getTooltip(section,
          TiltalignParam.SINGLE_OPTION));
    }
    ltfRotationAngle.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.ROT_ANGLE_KEY));
    ltfRotationGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.ROT_DEFAULT_GROUPING_KEY));
    ltfRotationNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.ROT_NONDEFAULT_GROUP_KEY));
    rbDistortionDisabled
        .setToolTipText("Do not solve for distortions in the plane of section.");
    rbDistortionFullSolution
        .setToolTipText("Solve for X-stretch and skew in the plane of section.");
    rbDistortionSkew.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.SKEW_OPTION_KEY));
    ltfXstretchGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.X_STRETCH_DEFAULT_GROUPING_KEY));
    ltfXstretchNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.X_STRETCH_NONDEFAULT_GROUP_KEY));
    ltfSkewGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.SKEW_DEFAULT_GROUPING_KEY));
    ltfSkewNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.SKEW_NONDEFAULT_GROUP_KEY));
    cbProjectionStretch.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.PROJECTION_STRETCH_KEY));
    //local variables
    cbLocalRotation.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_ROT_OPTION_KEY));
    ltfLocalRotationGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_ROT_DEFAULT_GROUPING_KEY));
    ltfLocalRotationNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, TiltalignParam.LOCAL_ROT_NONDEFAULT_GROUP_KEY));
    cbLocalTiltAngle.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_TILT_OPTION_KEY));
    ltfLocalTiltAngleGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_TILT_DEFAULT_GROUPING_KEY));
    ltfLocalTiltAngleNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, TiltalignParam.LOCAL_TILT_NONDEFAULT_GROUP_KEY));
    cbLocalMagnification.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_MAG_OPTION_KEY));
    ltfLocalMagnificationGroupSize.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, TiltalignParam.LOCAL_MAG_DEFAULT_GROUPING_KEY));
    ltfLocalMagnificationNonDefaultGroups.setToolTipText(EtomoAutodoc
        .getTooltip(autodoc, TiltalignParam.LOCAL_MAG_NONDEFAULT_GROUP_KEY));
    rbLocalDistortionDisabled
        .setToolTipText("Do not solve for local distortions in the plane of section.");
    rbLocalDistortionFullSolution
        .setToolTipText("Solve for local X-stretch and skew in the plane of section.");
    rbLocalDistortionSkew.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_SKEW_OPTION_KEY));
    ltfLocalXstretchGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_X_STRETCH_DEFAULT_GROUPING_KEY));
    ltfLocalXstretchNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, TiltalignParam.LOCAL_X_STRETCH_NONDEFAULT_GROUP_KEY));
    ltfLocalSkewGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.LOCAL_SKEW_DEFAULT_GROUPING_KEY));
    ltfLocalSkewNonDefaultGroups.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, TiltalignParam.LOCAL_SKEW_NONDEFAULT_GROUP_KEY));
    rbNoBeamTilt.setToolTipText("The beam axis is perpendicular to tilt axis");
    rtfFixedBeamTilt
        .setRadioButtonToolTipText("Set the non-perpendicularity between tilt axis and beam axis.");
    rtfFixedBeamTilt.setTextFieldToolTipText(EtomoAutodoc.getTooltip(autodoc,
        TiltalignParam.FIXED_OR_INITIAL_BEAM_TILT));
    rbSolveForBeamTilt
        .setToolTipText("Perform the minimization at a series of fixed beam tilt values and"
            + " search for the value that gives the smallest error.");
  }

  private static final class TPActionListener implements ActionListener {
    private final TiltalignPanel tiltalignPanel;

    private TPActionListener(final TiltalignPanel tiltalignPanel) {
      this.tiltalignPanel = tiltalignPanel;
    }

    public void actionPerformed(final ActionEvent actionEvent) {
      tiltalignPanel.action(actionEvent);
    }
  }

  private static final class TabChangeListener implements ChangeListener {
    TiltalignPanel tiltalignPanel;

    public TabChangeListener(TiltalignPanel tiltalignPanel) {
      this.tiltalignPanel = tiltalignPanel;
    }

    public void stateChanged(ChangeEvent changeEvent) {
      tiltalignPanel.changeTab(changeEvent);
    }
  }

  private static final class Tab {
    private static final int GENERAL_INDEX = 0;
    private static final int GLOBAL_VARIABLES_INDEX = 1;
    private static final int LOCAL_VARIABLES_INDEX = 2;

    private static final Tab GENERAL = new Tab(GENERAL_INDEX);
    private static final Tab GLOBAL_VARIABLES = new Tab(GLOBAL_VARIABLES_INDEX);
    private static final Tab LOCAL_VARIABLES = new Tab(LOCAL_VARIABLES_INDEX);

    private final int index;

    private Tab(int index) {
      this.index = index;
    }

    private int getIndex() {
      return index;
    }

    /**
     * Get the tab associated with the index.  Default: SETUP
     * @param index
     * @return
     */
    private static Tab getInstance(int index) {
      switch (index) {
      case GENERAL_INDEX:
        return GENERAL;
      case GLOBAL_VARIABLES_INDEX:
        return GLOBAL_VARIABLES;
      case LOCAL_VARIABLES_INDEX:
        return LOCAL_VARIABLES;
      default:
        return GENERAL;
      }
    }

  }
}

/**
 * <p> $Log$
 * <p> Revision 3.55  2010/03/03 05:09:10  sueh
 * <p> bug# 1311 Added patchTracking, setSurfacesToAnalyze.  Added a check
 * <p> to isValid.
 * <p>
 * <p> Revision 3.54  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.53  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.52  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.51  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
 * <p> Revision 3.50  2009/01/20 20:31:34  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.49  2008/11/21 17:11:44  sueh
 * <p> bug# 1123 Added get and setParameters(ReconScreenState).
 * <p>
 * <p> Revision 3.48  2008/11/20 01:47:56  sueh
 * <p> bug# 1147 Fixed metaData.setNoBeamTiltSelected,
 * <p> setFixedBeamTiltSelected, and setFixedBeamTilt.
 * <p>
 * <p> Revision 3.47  2008/09/30 22:45:08  sueh
 * <p> bug# 1113 Using a private constructor in SpacedPanel.
 * <p>
 * <p> Revision 3.46  2008/07/16 20:15:01  sueh
 * <p> bug# 1126 Added ltfRotationAngle.
 * <p>
 * <p> Revision 3.45  2007/07/30 16:30:15  sueh
 * <p> bug# 1001 Rearranged fields.
 * <p>
 * <p> Revision 3.44  2007/04/13 20:39:32  sueh
 * <p> bug# 964 Removed radioValue from radio buttons, and added EnumeratedType,
 * <p> which is the interface for enumeration types.
 * <p>
 * <p> Revision 3.43  2007/03/21 19:47:03  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.42  2007/03/09 22:09:21  sueh
 * <p> bug# 964 Put all of beam tilt under Advance/Basic panel header.  Letting tabs
 * <p> size themselves so that Advanced/Basic panel header won't grow too large.
 * <p> Enabling/disabling radio buttons in rotation, distortion solution, and beam tilt
 * <p> based on tiltalign.
 * <p>
 * <p> Revision 3.41  2007/03/08 22:14:27  sueh
 * <p> bug# 973 Changed the title of pnlLocalPatches.
 * <p>
 * <p> Revision 3.40  2007/03/07 21:15:16  sueh
 * <p> bug# 981 Added rbNoBeamTilt, rtfFixedBeamTilt, and rbSolveForBeamTilt.
 * <p>
 * <p> Revision 3.39  2007/03/03 01:08:03  sueh
 * <p> bug# 973 Rearranged the local alignments box on the general tab.  Replaced the
 * <p> number patches field with a radio button box which allows choosing between
 * <p> number patches and target size.
 * <p>
 * <p> Revision 3.38  2007/03/01 01:44:35  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 3.37  2007/02/09 00:53:43  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.36  2006/08/22 22:45:20  sueh
 * <p> bug# 913 Added single rot option
 * <p>
 * <p> Revision 3.35  2006/07/21 19:18:12  sueh
 * <p> bug# 848 Moved dimensions that have to be adjusted for font size from
 * <p> FixedDim to UIParameters.
 * <p>
 * <p> Revision 3.34  2006/07/20 17:23:08  sueh
 * <p> bug# 848 Made UIParameters a singleton.  Adjusting radio box widths by
 * <p> UIParameters.fontSizeAdjustment.
 * <p>
 * <p> Revision 3.33  2006/05/19 19:48:08  sueh
 * <p> bug# 866 Formatted.
 * <p>
 * <p> Revision 3.32  2006/04/25 19:23:53  sueh
 * <p> bug# 787 Changing the type of tabPane to TabbedPane.
 * <p>
 * <p> Revision 3.31  2006/01/12 17:38:13  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.30  2006/01/03 23:57:54  sueh
 * <p> bug# 675 Converted JCheckBox's to CheckBox.  Converted JRadioButton's
 * <p> toRadioButton.
 * <p>
 * <p> Revision 3.29  2005/08/27 22:42:33  sueh
 * <p> bug# 532 Changed Autodoc.get() to getInstance().
 * <p>
 * <p> Revision 3.28  2005/07/29 19:48:14  sueh
 * <p> bug# 692 Changed ConstEtomoNumber.getInteger() to getInt.
 * <p>
 * <p> Revision 3.27  2005/07/06 23:50:30  sueh
 * <p> bug# 619 Removed DoubleSpacedPanel and FormattedPanel.  Placed
 * <p> their functionality in SpacedPanel.  Simplified the construction of
 * <p> SpacedPanel.
 * <p>
 * <p> Revision 3.26  2005/06/14 23:07:19  sueh
 * <p> bug# 687 Fixed setParameters(ConstTiltalignParam), which was setting
 * <p> RotOptions with MagOptions.
 * <p>
 * <p> Revision 3.25  2005/06/14 22:06:44  sueh
 * <p> bug# 681 Added advanced checkbox fixXYZCoordinates to General tab
 * <p> below Min # fiducials.
 * <p>
 * <p> Revision 3.24  2005/06/11 02:50:46  sueh
 * <p> bug# 583, bug# 682, bug# 679  Moved binning calculation to
 * <p> ApplicationManager.  Upgraded align.com and tilt.com to have all
 * <p> unbinned parameters and a binning value.  Fixed potential divide by 0
 * <p> errors and incorrect binning calculation errors in Fine Align.  Removed
 * <p> member variables:  fidXyz, prealignedBinning, prealiHeader,
 * <p> rawstackHeader.
 * <p>
 * <p> Revision 3.23  2005/04/25 21:41:22  sueh
 * <p> bug# 615 Passing the axis where a command originates to the message
 * <p> functions so that the message will be popped up in the correct window.
 * <p> This requires adding AxisID to many objects.
 * <p>
 * <p> Revision 3.22  2005/02/24 00:52:35  sueh
 * <p> bug# 600 Fixed a bug that was saving a value to the wrong parameter.
 * <p>
 * <p> Revision 3.21  2005/02/21 23:05:04  sueh
 * <p> bug# 600 Making ConstTiltalignParam parameter name statics public.
 * <p>
 * <p> Revision 3.20  2005/02/18 01:29:32  sueh
 * <p> bug# 600 Using parameter names from TiltalignParam.
 * <p>
 * <p> Revision 3.19  2005/02/15 22:22:22  sueh
 * <p> bug# 600 Convert tooltips to autodoc.
 * <p>
 * <p> Revision 3.18  2005/02/11 16:46:12  sueh
 * <p> bug# 600 Getting tooltips from autodoc.
 * <p>
 * <p> Revision 3.17  2005/01/26 00:06:49  sueh
 * <p> Converted ConstEtomoNumber.resetValue to displayValue.
 * <p>
 * <p> Revision 3.16  2005/01/11 01:01:05  sueh
 * <p> bug# 567 Varying the widths of the radio button panels.
 * <p>
 * <p> Revision 3.15  2005/01/06 18:19:45  sueh
 * <p> bug# 567 In getParameters(), get z factor file.
 * <p>
 * <p> Revision 3.14  2005/01/05 20:12:17  sueh
 * <p> bug# 567 Made ProjectionStretch advanced only.
 * <p>
 * <p> Revision 3.13  2005/01/05 20:07:20  sueh
 * <p> bug# 567 Added ProjectionStretch checkbox to the global tab.
 * <p>
 * <p> Revision 3.12  2005/01/05 00:08:31  sueh
 * <p> bug# 567 Set the preferred width of fields that are too small
 * <p>
 * <p> Revision 3.11  2004/12/30 19:35:14  sueh
 * <p> bug# 567 in createGeneralTab(): capitalized radio box title.
 * <p>
 * <p> Revision 3.10  2004/12/30 19:33:53  sueh
 * <p> bug# 567 In createGeneralTab() Maved the items in the Resdual Reporting
 * <p> box to make two rows.
 * <p>
 * <p> Revision 3.9  2004/12/30 18:48:34  sueh
 * <p> bug# 567 In createVariablePanel(): Reduce the space between the
 * <p> checkbox and the text fields
 * <p>
 * <p> Revision 3.8  2004/12/30 18:01:54  sueh
 * <p> bug# 567 Fixed a bug in setParameters(ConstTiltalignParam):
 * <p> localRotationNonDefaultGroups was being set incorrectly.
 * <p>
 * <p> Revision 3.7  2004/12/29 23:50:23  sueh
 * <p> bug# 567 Put checkbox or radio buttons to the left on the fields in global and
 * <p> local tabs and in the local alignment panel on the general tab.
 * <p>
 * <p> Revision 3.6  2004/12/29 01:54:46  sueh
 * <p> bug# 567 Adding rotation solution panel.  Putting global and local rotation
 * <p> solution next to tilt angle solution panel.  Putting global and local
 * <p> xstretch and skew side-by-side.
 * <p>
 * <p> Revision 3.5  2004/12/29 00:16:18  sueh
 * <p> bug# 567 Converted distortion checkbox and local distortion checkbox to
 * <p> radio button groups.  Moved default values to ConstTiltalignParam.
 * <p> Changed update...Panel() functions to enable...Fields().  Changed
 * <p> selectGlobalDistortion to setDistortionSolutionState().  No longer testing
 * <p> type values before loading from the param.  Adapt to new tiltalignParam.
 * <p>
 * <p> Revision 3.4  2004/08/31 21:51:19  sueh
 * <p> bug# 545 SetParameters: handle the situation where fid.xyz exists but is empty.
 * <p> Substitute the pixel spacing from pre-aligned stack.
 * <p>
 * <p> Revision 3.3  2004/07/02 17:42:18  sueh
 * <p> bug#461 removing prints
 * <p>
 * <p> Revision 3.2  2004/07/02 00:40:07  sueh
 * <p> bug# 461 adding a connection to fid.xyz, preali header, and rawstack
 * <p> header to the constructor.  Info from these files is required
 * <p> to calculate z shift.  Correcting the calculation of binning
 * <p> when caculating z shift.
 * <p>
 * <p> Revision 3.1  2004/06/21 17:16:37  rickg
 * <p> Bug #461 z shift is scaled by the prealigned binning
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.10  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.9  2003/10/28 20:59:33  rickg
 * <p> Bug# 280 Tooltips
 * <p>
 * <p> Revision 2.8  2003/10/22 22:53:37  rickg
 * <p> Bug# 279 Removed "linear"
 * <p>
 * <p> Revision 2.7  2003/10/20 20:08:37  sueh
 * <p> Bus322 corrected labels
 * <p>
 * <p> Revision 2.6  2003/10/14 20:30:25  rickg
 * <p> Bug#279  Label layout and name changes
 * <p>
 * <p> Revision 2.5  2003/10/09 23:21:21  rickg
 * <p> Bug#279  Label layout and name changes
 * <p>
 * <p> Revision 2.4  2003/05/27 08:50:28  rickg
 * <p> Context menu handled by parent window
 * <p>
 * <p> Revision 2.3  2003/05/23 22:03:06  rickg
 * <p> Changed default tilt angle group size to 5
 * <p>
 * <p> Revision 2.2  2003/03/20 17:43:52  rickg
 * <p> Comment update
 * <p>
 * <p> Revision 2.1  2003/03/02 23:30:41  rickg
 * <p> Combine layout in progress
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.23  2003/01/15 00:11:42  rickg
 * <p> Fixed handling of xstretch and skew types (both global and
 * <p> local) when the original align.com does not have those
 * <p> parameters set and the user requests them.
 * <p>
 * <p> Revision 1.22.2.2  2003/01/24 19:04:54  rickg
 * <p> Merged changes from main branch
 * <p>
 * <p> Revision 1.22.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window   class TabChangeListener implements ChangeListener {
 JoinDialog adaptee;

 public TabChangeListener(JoinDialog dialog) {
 adaptee = dialog;
 }

 public void stateChanged(ChangeEvent event) {
 adaptee.changeTab(event);
 }
 }GUI layout initial revision
 * <p>
 * <p> Revision 1.22  2003/01/06 05:57:29  rickg
 * <p> Quick fix for residual threshold value text field size.  Needs to be more
 * <p> robust
 * <p>
 * <p> Revision 1.21  2003/01/04 00:24:29  rickg
 * <p> Wrap tab pane with a border to ID it as a tiltalign
 * <p> panel.
 * <p>
 * <p> Revision 1.20  2002/12/31 23:13:47  rickg
 * <p> Layout simplification
 * <p>
 * <p> Revision 1.19  2002/12/24 01:08:08  rickg
 * <p> Moved min local patch size to advanced
 * <p>
 * <p> Revision 1.18  2002/12/20 01:08:19  rickg
 * <p> Spelling correction
 * <p>
 * <p> Revision 1.17  2002/12/18 19:15:31  rickg
 * <p> Added advanced capability for metro factor and
 * <p> cycle limit.
 * <p> Ordered handling of panel updates
 * <p>
 * <p> Revision 1.16  2002/12/18 00:53:00  rickg
 * <p> Update in progress
 * <p>
 * <p> Revision 1.15  2002/12/17 01:02:08  rickg
 * <p> Additional groups are now advanced
 * <p>
 * <p> Revision 1.14  2002/12/10 21:33:10  rickg
 * <p> Add residual threshold control
 * <p>
 * <p> Revision 1.13  2002/12/06 01:02:58  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.12  2002/12/05 01:21:31  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.11  2002/12/04 04:42:13  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.10  2002/12/04 01:19:10  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.9  2002/12/03 05:41:13  rickg
 * <p> redesign in progress
 * <p>
 * <p> Revision 1.8  2002/12/03 05:22:29  rickg
 * <p> added getLocalRotationSolutionGroupSize
 * <p>
 * <p> Revision 1.7  2002/12/03 00:53:20  rickg
 * <p> Redesign in progress
 * <p>
 * <p> Revision 1.6  2002/11/25 15:59:37  rickg
 * <p> Removed local skew radio buttons
 * <p>
 * <p> Revision 1.5  2002/11/22 00:56:52  rickg
 * <p> removal of non-used fields in progress
 * <p>
 * <p> Revision 1.4  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.3  2002/10/17 23:40:32  rickg
 * <p> Added methods to set titl/mag and distortion defaults.
 * <p>
 * <p> Revision 1.2  2002/10/16 23:20:36  rickg
 * <p> Reformat
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
