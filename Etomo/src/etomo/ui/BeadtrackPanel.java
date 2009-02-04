package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.process.ImodProcess;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.BaseScreenState;
import etomo.type.ConstEtomoNumber;
import etomo.type.DialogType;
import etomo.type.EtomoAutodoc;
import etomo.type.InvalidEtomoNumberException;
import etomo.type.ProcessResultDisplay;
import etomo.type.Run3dmodMenuOptions;

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
 * <p> $Log$
 * <p> Revision 3.26  2009/01/20 19:44:48  sueh
 * <p> bug# 1102 Changed labeled panels to type EtomoPanel so that they can name themselves.
 * <p>
 * <p> Revision 3.25  2008/12/12 17:38:53  sueh
 * <p> bug# 1160 Added BeadDiameter to beadtrack panel.
 * <p>
 * <p> Revision 3.24  2008/12/09 21:33:15  sueh
 * <p> bug# 1160 Removed centroid radius.
 * <p>
 * <p> Revision 3.23  2008/12/05 00:55:05  sueh
 * <p> bug# 1156 In buttonAction, when ltfViewSkipList is set, validate it and
 * <p> send it to ApplicationManager.imodFixFiducials.
 * <p>
 * <p> Revision 3.22  2008/05/13 22:59:33  sueh
 * <p> bug# 847 Adding a right click menu for deferred 3dmods to some
 * <p> process buttons.
 * <p>
 * <p> Revision 3.21  2008/05/03 00:47:08  sueh
 * <p> bug# 847 Passing null for ProcessSeries to process funtions.
 * <p>
 * <p> Revision 3.20  2007/09/10 20:41:57  sueh
 * <p> bug# 925 Should only load button states once.  Changed
 * <p> ProcessResultDisplayFactory to load button states immediately, so removing
 * <p> button state load in the dialogs.
 * <p>
 * <p> Revision 3.19  2007/07/27 16:53:57  sueh
 * <p> bug# 979 Rearranged "Track Seed Model", "Fix Fiducial Model", and "Use fid as seed".  Changed "Use fid as seed" to "Track with Fiducial Model as Seed", which copies the fid to the seed and then tracks.
 * <p>
 * <p> Revision 3.18  2007/03/21 19:44:49  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.17  2007/03/01 01:27:17  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 3.16  2007/02/09 00:44:07  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level ui
 * <p> classes.
 * <p>
 * <p> Revision 3.15  2006/07/19 15:33:01  sueh
 * <p> bug# 903 Take the use fid as seed button out of advanced.
 * <p>
 * <p> Revision 3.14  2006/07/04 18:46:29  sueh
 * <p> bug# 893 Added updateAdvanced(boolean) to change headers when the
 * <p> advanced button is pressed.
 * <p>
 * <p> Revision 3.13  2006/06/16 15:24:59  sueh
 * <p> bug# 734 Moved track and use buttons from fiducial model dialog to beadtracker
 * <p> dialog.  Added open/close and adv/basic button.  Placed expert parameters in
 * <p> a separate box with an open/close button.
 * <p>
 * <p> Revision 3.12  2006/01/12 17:06:14  sueh
 * <p> bug# 798 Moved the autodoc classes to etomo.storage.autodoc.
 * <p>
 * <p> Revision 3.11  2006/01/03 23:29:43  sueh
 * <p> bug# 675 Converted JCheckBoxes to CheckBox
 * <p>
 * <p> Revision 3.10  2005/11/14 21:27:40  sueh
 * <p> bug# 762 Made buttonAction() protected.
 * <p>
 * <p> Revision 3.9  2005/08/27 22:35:17  sueh
 * <p> bug# 532 Changed Autodoc.get() to getInstance().
 * <p>
 * <p> Revision 3.8  2005/05/17 19:36:41  sueh
 * <p> bug# 658 Preventing null pointer exception by exiting if autodoc is null.
 * <p>
 * <p> Revision 3.7  2005/05/13 19:11:05  sueh
 * <p> bug# 658 Moved setEnabled() call to setParameters().
 * <p>
 * <p> Revision 3.6  2005/05/13 17:46:05  sueh
 * <p> bug# 658 Changed the position of the new fields on the screen.  They
 * <p> follow Max Gap.  Changed the labels of MinTiltRangeToFindAxis and
 * <p> Angels.
 * <p>
 * <p> Revision 3.5  2005/05/12 19:10:15  sueh
 * <p> bug# 658 Enabling/disabling fields based on cbTrackLocalArea in
 * <p> getParameters().
 * <p>
 * <p> Revision 3.3  2005/05/12 01:31:20  sueh
 * <p> bug# 658 Getting tooltips from the autodoc.  Split secondPassParams,
 * <p> tiltAngleMinRange, and fiducialParams into two fields on the screen.
 * <p>
 * <p> Revision 3.2  2005/05/10 03:25:31  sueh
 * <p> bug# 658 GetParameters(): Change to new BeadtrackParam.set functions
 * <p> where necessary.  Validate fields after they are set, throw exception and
 * <p> popup message.  SetParameters(): Change to new BeadtrackParam.get
 * <p> functions where necessary.
 * <p>
 * <p> Revision 3.1  2004/03/15 20:19:04  sueh
 * <p> bug# 276 placed to checkbox in a panel to get the alignments right
 * <p>
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 2.5  2003/11/06 22:44:33  sueh
 * <p> cleaning up tasks
 * <p>
 * <p> Revision 2.4  2003/10/30 01:43:44  rickg
 * <p> Bug# 338 Remapped context menu entries
 * <p>
 * <p> Revision 2.3  2003/10/15 01:33:59  sueh
 * <p> Bug277 added tooltips
 * <p>
 * <p> Revision 2.2  2003/10/15 00:26:37  sueh
 * <p> Bug274 move fields to Advanced, changed labels,
 * <p> removed Piece List File
 * <p>
 * <p> Revision 2.1  2003/05/07 17:52:39  rickg
 * <p> Removed context menu, parent panel handles it
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.3.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.3  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public final class BeadtrackPanel implements Expandable,
    Run3dmodButtonContainer {
  public static final String rcsid = "$Id$";

  public static final String TRACK_LABEL = "Track Seed Model";
  public static final String USE_MODEL_LABEL = "Track with Fiducial Model as Seed";
  private static final String VIEW_SKIP_LIST_LABEL = "View skip list";

  private final EtomoPanel panelBeadtrack = new EtomoPanel();
  private final JPanel panelBeadtrackBody = new JPanel();
  private final AxisID axisID;
  private final Run3dmodButton btnFixModel;

  private final LabeledTextField ltfViewSkipList = new LabeledTextField(
      VIEW_SKIP_LIST_LABEL + ": ");
  private final LabeledTextField ltfAdditionalViewSets = new LabeledTextField(
      "Separate view groups: ");
  private final LabeledTextField ltfTiltAngleGroupSize = new LabeledTextField(
      "Tilt angle group size: ");
  private final LabeledTextField ltfTiltAngleGroups = new LabeledTextField(
      "Non-default tilt angle groups: ");
  private final LabeledTextField ltfMagnificationGroupSize = new LabeledTextField(
      "Magnification group size: ");
  private final LabeledTextField ltfMagnificationGroups = new LabeledTextField(
      "Non-default magnification groups: ");
  private final LabeledTextField ltfNMinViews = new LabeledTextField(
      "Minimum # of views for tilt alignment: ");
  private final LabeledTextField ltfBeadDiameter = new LabeledTextField(
      "Unbinned bead diameter: ");
  private final CheckBox cbLightBeads = new CheckBox("Light fiducial markers");
  CheckBox cbFillGaps = new CheckBox("Fill seed model gaps");
  private final LabeledTextField ltfMaxGap = new LabeledTextField(
      "Maximum gap size: ");
  private final LabeledTextField ltfMinTiltRangeToFindAxis = new LabeledTextField(
      "Minimum tilt range for finding axis: ");
  private final LabeledTextField ltfMinTiltRangeToFindAngle = new LabeledTextField(
      "Minimum tilt range for finding angles: ");
  private final LabeledTextField ltfSearchBoxPixels = new LabeledTextField(
      "Search box size (pixels): ");
  private final LabeledTextField ltfMaxFiducialsAvg = new LabeledTextField(
      "Maximum # of views for fiducial avg.: ");
  private final LabeledTextField ltfFiducialExtrapolationParams = new LabeledTextField(
      "Fiducial extrapolation limits: ");
  private final LabeledTextField ltfRescueAttemptParams = new LabeledTextField(
      "Rescue attempt criteria: ");
  private final LabeledTextField ltfMinRescueDistance = new LabeledTextField(
      "Distance criterion for rescue (pixels): ");
  private final LabeledTextField ltfRescueRelaxtionParams = new LabeledTextField(
      "Rescue relaxation factors: ");
  private final LabeledTextField ltfResidualDistanceLimit = new LabeledTextField(
      "First pass residual limit for deletion: ");
  private final LabeledTextField ltfMeanResidChangeLimits = new LabeledTextField(
      "Residual change limits: ");
  private final LabeledTextField ltfDeletionParams = new LabeledTextField(
      "Deletion residual parameters: ");
  private final LabeledTextField ltfDensityRelaxationPostFit = new LabeledTextField(
      "Second pass density relaxation: ");
  private final LabeledTextField ltfMaxRescueDistance = new LabeledTextField(
      "Second pass maximum rescue distance: ");

  private final CheckBox cbLocalAreaTracking = new CheckBox("Local tracking");
  private final LabeledTextField ltfLocalAreaTargetSize = new LabeledTextField(
      "Local area size: ");
  private final LabeledTextField ltfMinBeadsInArea = new LabeledTextField(
      "Minimum beads in area: ");
  private final LabeledTextField ltfMinOverlapBeads = new LabeledTextField(
      "Minimum beads overlapping: ");
  private final LabeledTextField ltfMaxViewsInAlign = new LabeledTextField(
      "Max. # views to include in align: ");
  private final LabeledTextField ltfRoundsOfTracking = new LabeledTextField(
      "Rounds of tracking: ");

  private final JPanel pnlCheckbox = new JPanel();
  private final JPanel pnlLightBeads = new JPanel();
  private final JPanel pnlLocalAreaTracking = new JPanel();
  private final EtomoPanel pnlExpertParameters = new EtomoPanel();
  private final JPanel pnlExpertParametersBody = new JPanel();

  private final PanelHeader expertParametersHeader;
  private final PanelHeader header;
  private final ApplicationManager manager;
  private final MultiLineButton btnTrack;
  private final MultiLineButton btnUseModel = new MultiLineButton(
      USE_MODEL_LABEL);
  private final BeadtrackPanelActionListener actionListener = new BeadtrackPanelActionListener(
      this);

  /**
   * Construct a new beadtrack panel.
   * @param label specifies the suffix for the logfile
   */
  private BeadtrackPanel(ApplicationManager manager, AxisID id,
      DialogType dialogType) {
    this.manager = manager;
    axisID = id;
    btnTrack = (MultiLineButton) manager.getProcessResultDisplayFactory(axisID)
        .getTrackFiducials();
    btnFixModel = (Run3dmodButton) manager.getProcessResultDisplayFactory(
        axisID).getFixFiducialModel();
    btnFixModel.setContainer(this);
    expertParametersHeader = PanelHeader.getInstance("Expert Parameters", this,
        dialogType);
    header = PanelHeader.getAdvancedBasicInstance("Beadtracker", this,
        dialogType);

    panelBeadtrackBody.setLayout(new BoxLayout(panelBeadtrackBody,
        BoxLayout.Y_AXIS));
    panelBeadtrackBody.add(Box.createRigidArea(FixedDim.x0_y5));
    panelBeadtrackBody.add(ltfViewSkipList.getContainer());
    panelBeadtrackBody.add(ltfViewSkipList.getContainer());
    panelBeadtrackBody.add(ltfAdditionalViewSets.getContainer());
    panelBeadtrackBody.add(ltfTiltAngleGroupSize.getContainer());
    panelBeadtrackBody.add(ltfTiltAngleGroups.getContainer());
    panelBeadtrackBody.add(ltfMagnificationGroupSize.getContainer());
    panelBeadtrackBody.add(ltfMagnificationGroups.getContainer());
    panelBeadtrackBody.add(ltfNMinViews.getContainer());
    panelBeadtrackBody.add(ltfBeadDiameter.getContainer());

    pnlLightBeads.setLayout(new BoxLayout(pnlLightBeads, BoxLayout.Y_AXIS));
    pnlLightBeads.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlLightBeads.add(cbLightBeads);
    panelBeadtrackBody.add(pnlLightBeads);
    pnlCheckbox.setLayout(new BoxLayout(pnlCheckbox, BoxLayout.Y_AXIS));
    pnlCheckbox.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlCheckbox.add(cbFillGaps);

    panelBeadtrackBody.add(pnlCheckbox);
    panelBeadtrackBody.add(ltfMaxGap.getContainer());

    pnlLocalAreaTracking.setLayout(new BoxLayout(pnlLocalAreaTracking,
        BoxLayout.Y_AXIS));
    pnlLocalAreaTracking.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlLocalAreaTracking.add(cbLocalAreaTracking);
    panelBeadtrackBody.add(pnlLocalAreaTracking);

    panelBeadtrackBody.add(ltfLocalAreaTargetSize.getContainer());
    panelBeadtrackBody.add(ltfMinBeadsInArea.getContainer());
    panelBeadtrackBody.add(ltfMinOverlapBeads.getContainer());
    panelBeadtrackBody.add(ltfMaxViewsInAlign.getContainer());
    panelBeadtrackBody.add(ltfRoundsOfTracking.getContainer());

    panelBeadtrackBody.add(ltfMinTiltRangeToFindAxis.getContainer());
    panelBeadtrackBody.add(ltfMinTiltRangeToFindAngle.getContainer());
    panelBeadtrackBody.add(ltfSearchBoxPixels.getContainer());
    panelBeadtrackBody.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlExpertParametersBody.setLayout(new BoxLayout(pnlExpertParametersBody,
        BoxLayout.Y_AXIS));
    pnlExpertParametersBody.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlExpertParametersBody.add(ltfMaxFiducialsAvg.getContainer());
    pnlExpertParametersBody.add(ltfFiducialExtrapolationParams.getContainer());
    pnlExpertParametersBody.add(ltfRescueAttemptParams.getContainer());
    pnlExpertParametersBody.add(ltfMinRescueDistance.getContainer());
    pnlExpertParametersBody.add(ltfRescueRelaxtionParams.getContainer());
    pnlExpertParametersBody.add(ltfResidualDistanceLimit.getContainer());
    pnlExpertParametersBody.add(ltfDensityRelaxationPostFit.getContainer());
    pnlExpertParametersBody.add(ltfMaxRescueDistance.getContainer());
    pnlExpertParametersBody.add(ltfMeanResidChangeLimits.getContainer());
    pnlExpertParametersBody.add(ltfDeletionParams.getContainer());

    pnlExpertParameters.setLayout(new BoxLayout(pnlExpertParameters,
        BoxLayout.Y_AXIS));
    pnlExpertParameters.setBorder(BorderFactory.createEtchedBorder());
    pnlExpertParameters.add(expertParametersHeader);
    pnlExpertParameters.add(pnlExpertParametersBody);
    panelBeadtrackBody.add(pnlExpertParameters);

    btnTrack.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTrack.setSize();
    panelBeadtrackBody.add(btnTrack.getComponent());

    JPanel pnlTrack = new JPanel();
    pnlTrack.setLayout(new BoxLayout(pnlTrack, BoxLayout.X_AXIS));
    pnlTrack.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnFixModel.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnFixModel.setSize();
    pnlTrack.add(btnFixModel.getComponent());
    pnlTrack.add(Box.createRigidArea(FixedDim.x5_y0));
    btnUseModel.setSize();
    pnlTrack.add(btnUseModel.getComponent());
    panelBeadtrackBody.add(Box.createRigidArea(FixedDim.x0_y5));
    panelBeadtrackBody.add(pnlTrack);
    panelBeadtrackBody.add(Box.createRigidArea(FixedDim.x0_y5));

    panelBeadtrack.setLayout(new BoxLayout(panelBeadtrack, BoxLayout.Y_AXIS));
    panelBeadtrack.setBorder(BorderFactory.createEtchedBorder());
    panelBeadtrack.add(header);
    panelBeadtrack.add(panelBeadtrackBody);
    setToolTipText();
  }

  public static BeadtrackPanel getInstance(ApplicationManager manager,
      AxisID id, DialogType dialogType) {
    BeadtrackPanel instance = new BeadtrackPanel(manager, id, dialogType);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    cbLocalAreaTracking.addActionListener(actionListener);
    btnTrack.addActionListener(actionListener);
    btnUseModel.addActionListener(actionListener);
    btnFixModel.addActionListener(actionListener);
  }

  public static ProcessResultDisplay getTrackFiducialsDisplay(
      DialogType dialogType) {
    return MultiLineButton.getToggleButtonInstance(TRACK_LABEL, dialogType);
  }

  /**
   * Update the header with the current advanced state
   */
  void updateAdvanced(boolean isAdvanced) {
    header.setAdvanced(isAdvanced);
  }

  public void expand(ExpandButton button) {
    if (expertParametersHeader.equalsOpenClose(button)) {
      pnlExpertParametersBody.setVisible(button.isExpanded());
    }
    else if (header.equalsOpenClose(button)) {
      panelBeadtrackBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      setAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public void setParameters(BaseScreenState screenState) {
    expertParametersHeader.setButtonStates(screenState, false);
    header.setButtonStates(screenState);
    //btnFixModel.setButtonState(screenState.getButtonState(btnFixModel
    //    .getButtonStateKey()));
    //btnTrack.setButtonState(screenState.getButtonState(btnTrack
    //   .getButtonStateKey()));
  }

  public void getParameters(BaseScreenState screenState) {
    expertParametersHeader.getButtonStates(screenState);
    header.getButtonStates(screenState);
  }

  /**
   * Set the field values for the panel from the ConstBeadtrackParam object
   */
  public void setParameters(BeadtrackParam beadtrackParams) {
    ConstEtomoNumber field = null;
    ltfViewSkipList.setText(beadtrackParams.getSkipViews());
    ltfAdditionalViewSets.setText(beadtrackParams.getAdditionalViewGroups());
    ltfTiltAngleGroupSize.setText(beadtrackParams.getTiltDefaultGrouping()
        .toString());
    ltfTiltAngleGroups.setText(beadtrackParams.getTiltAngleGroups());
    ltfMagnificationGroupSize.setText(beadtrackParams
        .getMagnificationGroupSize());
    ltfMagnificationGroups.setText(beadtrackParams.getMagnificationGroups());
    ltfNMinViews.setText(beadtrackParams.getMinViewsForTiltalign().toString());
    ltfBeadDiameter.setText(beadtrackParams.getBeadDiameter().toString());
    cbLightBeads.setSelected(beadtrackParams.getLightBeads().is());
    cbFillGaps.setSelected(beadtrackParams.getFillGaps());
    ltfMaxGap.setText(beadtrackParams.getMaxGapSize().toString());
    ltfMinTiltRangeToFindAxis.setText(beadtrackParams
        .getMinTiltRangeToFindAxis().toString());
    ltfMinTiltRangeToFindAngle.setText(beadtrackParams
        .getMinTiltRangeToFindAngles().toString());
    ltfSearchBoxPixels.setText(beadtrackParams.getSearchBoxPixels());
    ltfMaxFiducialsAvg.setText(beadtrackParams.getMaxBeadsToAverage()
        .toString());
    ltfFiducialExtrapolationParams.setText(beadtrackParams
        .getFiducialExtrapolationParams());
    ltfRescueAttemptParams.setText(beadtrackParams.getRescueAttemptParams());
    ltfMinRescueDistance.setText(beadtrackParams.getDistanceRescueCriterion()
        .toString());
    ltfRescueRelaxtionParams.setText(beadtrackParams
        .getRescueRelaxationParams());
    ltfResidualDistanceLimit.setText(beadtrackParams.getPostFitRescueResidual()
        .toString());
    ltfDensityRelaxationPostFit.setText(beadtrackParams
        .getDensityRelaxationPostFit().toString());
    ltfMaxRescueDistance.setText(beadtrackParams.getMaxRescueDistance()
        .toString());
    ltfMeanResidChangeLimits
        .setText(beadtrackParams.getMeanResidChangeLimits());
    ltfDeletionParams.setText(beadtrackParams.getDeletionParams());

    cbLocalAreaTracking
        .setSelected(beadtrackParams.getLocalAreaTracking().is());
    ltfLocalAreaTargetSize.setText(beadtrackParams.getLocalAreaTargetSize()
        .toString());
    ltfMinBeadsInArea.setText(beadtrackParams.getMinBeadsInArea().toString());
    ltfMinOverlapBeads.setText(beadtrackParams.getMinOverlapBeads().toString());
    ltfMaxViewsInAlign.setText(beadtrackParams.getMaxViewsInAlign().toString());
    ltfRoundsOfTracking.setText(beadtrackParams.getRoundsOfTracking()
        .toString());

    setEnabled();
  }

  /**
   * Get the field values from the panel filling in the BeadtrackParam object
   */
  public void getParameters(BeadtrackParam beadtrackParams)
      throws FortranInputSyntaxException, InvalidEtomoNumberException {
    beadtrackParams.setFillGaps(cbFillGaps.isSelected());
    beadtrackParams.setImagesAreBinned(UIExpertUtilities.INSTANCE.getStackBinning(
        manager, axisID, ".preali"));
    String errorTitle = "Field Error";
    String badParameter = "";
    //handle field that throw FortranInputSyntaxException
    try {
      badParameter = ltfViewSkipList.getLabel();
      beadtrackParams.setSkipViews(ltfViewSkipList.getText());

      badParameter = ltfAdditionalViewSets.getLabel();
      beadtrackParams.setAdditionalViewGroups(ltfAdditionalViewSets.getText());

      badParameter = ltfTiltAngleGroups.getLabel();
      beadtrackParams.setTiltAngleGroups(ltfTiltAngleGroups.getText());

      badParameter = ltfMagnificationGroups.getLabel();
      beadtrackParams.setMagnificationGroups(ltfMagnificationGroups.getText());

      badParameter = ltfSearchBoxPixels.getLabel();
      beadtrackParams.setSearchBoxPixels(ltfSearchBoxPixels.getText());

      badParameter = ltfFiducialExtrapolationParams.getLabel();
      beadtrackParams
          .setFiducialExtrapolationParams(ltfFiducialExtrapolationParams
              .getText());

      badParameter = ltfRescueAttemptParams.getLabel();
      beadtrackParams.setRescueAttemptParams(ltfRescueAttemptParams.getText());

      badParameter = ltfRescueRelaxtionParams.getLabel();
      beadtrackParams.setRescueRelaxationParams(ltfRescueRelaxtionParams
          .getText());

      badParameter = ltfMeanResidChangeLimits.getLabel();
      beadtrackParams.setMeanResidChangeLimits(ltfMeanResidChangeLimits
          .getText());

      badParameter = ltfDeletionParams.getLabel();
      beadtrackParams.setDeletionParams(ltfDeletionParams.getText());

      //handle fields that display their own messages and throw
      //InvalidEtomoNumberException
      try {
        badParameter = ltfTiltAngleGroupSize.getLabel();
        beadtrackParams.setTiltDefaultGrouping(ltfTiltAngleGroupSize.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = ltfMagnificationGroupSize.getLabel();
        beadtrackParams.setMagDefaultGrouping(
            ltfMagnificationGroupSize.getText()).validate(errorTitle,
            badParameter, axisID);

        badParameter = ltfNMinViews.getLabel();
        beadtrackParams.setMinViewsForTiltalign(ltfNMinViews.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = ltfMaxGap.getLabel();
        beadtrackParams.setMaxGapSize(ltfMaxGap.getText()).validate(errorTitle,
            badParameter, axisID);

        badParameter = ltfMaxFiducialsAvg.getLabel();
        beadtrackParams.setMaxBeadsToAverage(ltfMaxFiducialsAvg.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = ltfMinRescueDistance.getLabel();
        beadtrackParams.setDistanceRescueCriterion(
            ltfMinRescueDistance.getText()).validate(errorTitle, badParameter,
            axisID);

        badParameter = ltfResidualDistanceLimit.getLabel();
        beadtrackParams.setPostFitRescueResidual(
            ltfResidualDistanceLimit.getText()).validate(errorTitle,
            badParameter, axisID);

        badParameter = ltfDensityRelaxationPostFit.getLabel();
        beadtrackParams.setDensityRelaxationPostFit(
            ltfDensityRelaxationPostFit.getText()).validate(errorTitle,
            badParameter, axisID);

        badParameter = ltfMaxRescueDistance.getLabel();
        beadtrackParams.setMaxRescueDistance(ltfMaxRescueDistance.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = ltfMinTiltRangeToFindAxis.getLabel();
        beadtrackParams.setMinTiltRangeToFindAxis(
            ltfMinTiltRangeToFindAxis.getText()).validate(errorTitle,
            badParameter, axisID);

        badParameter = ltfMinTiltRangeToFindAngle.getLabel();
        beadtrackParams.setMinTiltRangeToFindAngles(
            ltfMinTiltRangeToFindAngle.getText()).validate(errorTitle,
            badParameter, axisID);

        badParameter = ltfBeadDiameter.getLabel();
        beadtrackParams.setBeadDiameter(ltfBeadDiameter.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = cbLightBeads.getText();
        beadtrackParams.setLightBeads(cbLightBeads.isSelected()).validate(
            errorTitle, badParameter, axisID);

        badParameter = cbLocalAreaTracking.getText();
        beadtrackParams.setLocalAreaTracking(cbLocalAreaTracking.isSelected())
            .validate(errorTitle, badParameter, axisID);

        badParameter = ltfLocalAreaTargetSize.getText();
        beadtrackParams
            .setLocalAreaTargetSize(ltfLocalAreaTargetSize.getText()).validate(
                errorTitle, badParameter, axisID);

        badParameter = ltfMinBeadsInArea.getText();
        beadtrackParams.setMinBeadsInArea(ltfMinBeadsInArea.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = ltfMinOverlapBeads.getText();
        beadtrackParams.setMinOverlapBeads(ltfMinOverlapBeads.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = ltfMaxViewsInAlign.getText();
        beadtrackParams.setMaxViewsInAlign(ltfMaxViewsInAlign.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = ltfRoundsOfTracking.getText();
        beadtrackParams.setRoundsOfTracking(ltfRoundsOfTracking.getText())
            .validate(errorTitle, badParameter, axisID);
      }
      catch (InvalidEtomoNumberException e) {
        throw e;
      }
    }
    catch (FortranInputSyntaxException except) {
      String message = badParameter + " " + except.getMessage();
      throw new FortranInputSyntaxException(message);
    }
  }

  public JPanel getContainer() {
    return panelBeadtrack;
  }

  protected void buttonAction(String command,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    if (command.equals(btnTrack.getActionCommand())) {
      manager.fiducialModelTrack(axisID, btnTrack, null);
    }
    else if (command.equals(btnUseModel.getActionCommand())) {
      if (manager.makeFiducialModelSeedModel(axisID)) {
        manager.fiducialModelTrack(axisID, btnUseModel, null);
      }
    }
    else if (command.equals(cbLocalAreaTracking.getText())) {
      setEnabled();
    }
    else if (command.equals(btnFixModel.getActionCommand())) {
      //Validate skipList
      String skipList = ltfViewSkipList.getText().trim();
      if (skipList.length() > 0) {
        if (skipList.matches(".*\\s+.*")) {
          UIHarness.INSTANCE.openMessageDialog(VIEW_SKIP_LIST_LABEL
              + " cannot contain embedded spaces.", "Entry Error", axisID);
          return;
        }
      }
      else {
        skipList = null;
      }
      manager.imodFixFiducials(axisID, run3dmodMenuOptions, btnFixModel,
          ImodProcess.GAP_MODE, skipList);
    }
  }

  public void action(Run3dmodButton button,
      Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), run3dmodMenuOptions);
  }

  private void setEnabled() {
    ltfLocalAreaTargetSize.setEnabled(cbLocalAreaTracking.isSelected());
    ltfMinBeadsInArea.setEnabled(cbLocalAreaTracking.isSelected());
    ltfMinOverlapBeads.setEnabled(cbLocalAreaTracking.isSelected());
  }

  /**
   * Makes the advanced components visible or invisible
   */
  void setAdvanced(boolean state) {
    ltfTiltAngleGroupSize.setVisible(state);
    ltfTiltAngleGroups.setVisible(state);
    ltfMagnificationGroupSize.setVisible(state);
    ltfMagnificationGroups.setVisible(state);
    ltfNMinViews.setVisible(state);
    ltfBeadDiameter.setVisible(state);
    cbLightBeads.setVisible(state);
    ltfMaxGap.setVisible(state);
    ltfMinTiltRangeToFindAxis.setVisible(state);
    ltfMinTiltRangeToFindAngle.setVisible(state);
    ltfSearchBoxPixels.setVisible(state);
    pnlExpertParameters.setVisible(state);
    ltfMinBeadsInArea.setVisible(state);
    ltfMinOverlapBeads.setVisible(state);
    ltfRoundsOfTracking.setVisible(state);
  }

  public void done() {
    btnTrack.removeActionListener(actionListener);
    btnFixModel.removeActionListener(actionListener);
  }

  //  ToolTip string setup
  private void setToolTipText() {
    String text;
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(AutodocFactory.BEADTRACK, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
    if (autodoc == null) {
      return;
    }
    ltfViewSkipList.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.SKIP_VIEW_LIST_KEY));
    ltfAdditionalViewSets.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.ADDITIONAL_VIEW_GROUPS_KEY));
    ltfTiltAngleGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.TILT_ANGLE_GROUP_PARAMS_KEY));
    ltfTiltAngleGroups.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.TILT_ANGLE_GROUPS_KEY));
    ltfMagnificationGroupSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MAGNIFICATION_GROUP_PARAMS_KEY));
    ltfMagnificationGroups.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MAGNIFICATION_GROUPS_KEY));
    ltfNMinViews.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.N_MIN_VIEWS_KEY));
    ltfBeadDiameter.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.BEAD_DIAMETER_KEY));
    cbLightBeads.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.LIGHT_BEADS_KEY));
    cbFillGaps.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.FILL_GAPS_KEY));
    ltfMaxGap.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MAX_GAP_KEY));
    ltfMinTiltRangeToFindAxis.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MIN_TILT_RANGE_TO_FIND_AXIS_KEY));
    ltfMinTiltRangeToFindAngle.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MIN_TILT_RANGE_TO_FIND_ANGLES_KEY));
    ltfSearchBoxPixels.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.SEARCH_BOX_PIXELS_KEY));
    ltfMaxFiducialsAvg.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MAX_FIDUCIALS_AVG_KEY));
    ltfFiducialExtrapolationParams.setToolTipText(EtomoAutodoc.getTooltip(
        autodoc, BeadtrackParam.FIDUCIAL_EXTRAPOLATION_PARAMS_KEY));
    ltfRescueAttemptParams.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.RESCUE_ATTEMPT_PARAMS_KEY));
    ltfMinRescueDistance.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MIN_RESCUE_DISTANCE_KEY));
    ltfRescueRelaxtionParams.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.RESCUE_RELAXATION_PARAMS_KEY));
    ltfResidualDistanceLimit.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.RESIDUAL_DISTANCE_LIMIT_KEY));
    ltfDensityRelaxationPostFit.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.DENSITY_RELAXATION_POST_FIT_KEY));
    ltfMaxRescueDistance.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MAX_RESCUE_DISTANCE_KEY));
    ltfMeanResidChangeLimits.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MEAN_RESID_CHANGE_LIMITS_KEY));
    ltfDeletionParams.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.DELETION_PARAMS_KEY));

    cbLocalAreaTracking.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.LOCAL_AREA_TRACKING_KEY));
    ltfLocalAreaTargetSize.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.LOCAL_AREA_TARGET_SIZE_KEY));
    ltfMinBeadsInArea.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MIN_BEADS_IN_AREA_KEY));
    ltfMinOverlapBeads.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MIN_OVERLAP_BEADS_KEY));
    ltfMaxViewsInAlign.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MAX_VIEWS_IN_ALIGN_KEY));
    ltfRoundsOfTracking.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.ROUNDS_OF_TRACKING_KEY));
    btnTrack
        .setToolTipText("Run Beadtrack to produce fiducial model from seed model.");
    btnFixModel.setToolTipText("Load fiducial model into 3dmod.");
    btnUseModel
        .setToolTipText("Turn the output of Beadtrack (fiducial model) into a new seed model and then track.  "
            + "Your original seed model will be moved into an _orig.seed file.");
  }

  private class BeadtrackPanelActionListener implements ActionListener {

    BeadtrackPanel adaptee;

    BeadtrackPanelActionListener(BeadtrackPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null);
    }
  }

}
