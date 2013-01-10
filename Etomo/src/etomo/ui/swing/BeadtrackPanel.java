package etomo.ui.swing;

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
import etomo.type.Run3dmodMenuOptions;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

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
 * <p> Revision 1.4  2011/04/09 06:36:47  sueh
 * <p> bug# 1416 In getParameters(BeadtrackParam) eliminating dead code by assigning errorMessage correctly.
 * <p>
 * <p> Revision 1.3  2011/02/22 18:02:09  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.2  2010/12/05 04:54:57  sueh
 * <p> bug# 1420 Moved ProcessResultDisplayFactory to etomo.ui.swing package.  Removed static button construction functions.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 3.34  2010/03/11 06:03:19  sueh
 * <p> bug# 1311 Added ImodProcess.BeadFixerMode instead of using strings.
 * <p>
 * <p> Revision 3.33  2010/03/03 05:01:49  sueh
 * <p> bug# 1311 added setVisible.
 * <p>
 * <p> Revision 3.32  2010/02/17 05:03:12  sueh
 * <p> bug# 1301 Using manager instead of manager key for popping up messages.
 * <p>
 * <p> Revision 3.31  2009/09/01 03:18:25  sueh
 * <p> bug# 1222
 * <p>
 * <p> Revision 3.30  2009/06/15 20:16:26  sueh
 * <p> bug# 1221 Implemented BeadTrackDisplay.
 * <p>
 * <p> Revision 3.29  2009/05/02 01:13:01  sueh
 * <p> bug# 1216 Added pickSeed and pickRaptor.
 * <p>
 * <p> Revision 3.28  2009/03/17 00:46:24  sueh
 * <p> bug# 1186 Pass managerKey to everything that pops up a dialog.
 * <p>
 * <p> Revision 3.27  2009/02/04 23:36:48  sueh
 * <p> bug# 1158 Changed id and exception classes in LogFile.
 * <p>
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
 * <p> bug# 979 Rearranged "Track Seed Model", "Fix Fiducial Model", and "Use
 * <p> fid as seed".  Changed "Use fid as seed" to "Track with Fiducial Model as
 * <p> Seed", which copies the fid to the seed and then tracks.
 * <p>
 * <p> Revision 3.18  2007/03/21 19:44:49  sueh
 * <p> bug# 964 Limiting access to autodoc classes by using ReadOnly interfaces.
 * <p> Added AutodocFactory to create Autodoc instances.
 * <p>
 * <p> Revision 3.17  2007/03/01 01:27:17  sueh
 * <p> bug# 964 Added LogFile to Autodoc.
 * <p>
 * <p> Revision 3.16  2007/02/09 00:44:07  sueh
 * <p> bug# 962 Made TooltipFormatter a singleton and moved its use to low-level
 * <p> ui classes.
 * <p>
 * <p> Revision 3.15  2006/07/19 15:33:01  sueh
 * <p> bug# 903 Take the use fid as seed button out of advanced.
 * <p>
 * <p> Revision 3.14  2006/07/04 18:46:29  sueh
 * <p> bug# 893 Added updateAdvanced(boolean) to change headers when the
 * <p> advanced button is pressed.
 * <p>
 * <p> Revision 3.13  2006/06/16 15:24:59  sueh
 * <p> bug# 734 Moved track and use buttons from fiducial model dialog to
 * <p> beadtracker dialog.  Added open/close and adv/basic button.  Placed
 * <p> expert parameters in a separate box with an open/close button.
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
public final class BeadtrackPanel implements Expandable, Run3dmodButtonContainer,
    BeadTrackDisplay {
  public static final String rcsid = "$Id$";

  public static final String TRACK_LABEL = "Track Seed Model";
  public static final String USE_MODEL_LABEL = "Track with Fiducial Model as Seed";
  private static final String VIEW_SKIP_LIST_LABEL = "View skip list";
  static final String LIGHT_BEADS_LABEL = "Light fiducial markers";

  private final EtomoPanel panelBeadtrackX = new EtomoPanel();
  private final EtomoPanel panelBeadtrack = new EtomoPanel();
  private final JPanel panelBeadtrackBody = new JPanel();
  private final AxisID axisID;
  private final Run3dmodButton btnFixModel;

  private final LabeledTextField ltfViewSkipList = new LabeledTextField(
      FieldType.INTEGER_LIST, VIEW_SKIP_LIST_LABEL + ": ");
  private final LabeledTextField ltfAdditionalViewSets = new LabeledTextField(
      FieldType.INTEGER_LIST, "Separate view groups: ");
  private final LabeledTextField ltfTiltAngleGroupSize = new LabeledTextField(
      FieldType.INTEGER, "Tilt angle group size: ");
  private final LabeledTextField ltfTiltAngleGroups = new LabeledTextField(
      FieldType.INTEGER_TRIPLE, "Non-default tilt angle groups: ");
  private final LabeledTextField ltfMagnificationGroupSize = new LabeledTextField(
      FieldType.INTEGER, "Magnification group size: ");
  private final LabeledTextField ltfMagnificationGroups = new LabeledTextField(
      FieldType.INTEGER_TRIPLE, "Non-default magnification groups: ");
  private final LabeledTextField ltfNMinViews = new LabeledTextField(FieldType.INTEGER,
      "Minimum # of views for tilt alignment: ");
  private final LabeledTextField ltfBeadDiameter = new LabeledTextField(
      FieldType.FLOATING_POINT, "Unbinned bead diameter: ");
  private final CheckBox cbLightBeads = new CheckBox(LIGHT_BEADS_LABEL);
  CheckBox cbFillGaps = new CheckBox("Fill seed model gaps");
  private final LabeledTextField ltfMaxGap = new LabeledTextField(FieldType.INTEGER,
      "Maximum gap size: ");
  private final LabeledTextField ltfMinTiltRangeToFindAxis = new LabeledTextField(
      FieldType.FLOATING_POINT, "Minimum tilt range for finding axis: ");
  private final LabeledTextField ltfMinTiltRangeToFindAngle = new LabeledTextField(
      FieldType.FLOATING_POINT, "Minimum tilt range for finding angles: ");
  private final LabeledTextField ltfSearchBoxPixels = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Search box size (pixels): ");
  private final LabeledTextField ltfMaxFiducialsAvg = new LabeledTextField(
      FieldType.INTEGER, "Maximum # of views for fiducial avg.: ");
  private final LabeledTextField ltfFiducialExtrapolationParams = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Fiducial extrapolation limits: ");
  private final LabeledTextField ltfRescueAttemptParams = new LabeledTextField(
      FieldType.FLOATING_POINT_PAIR, "Rescue attempt criteria: ");
  private final LabeledTextField ltfMinRescueDistance = new LabeledTextField(
      FieldType.FLOATING_POINT, "Distance criterion for rescue (pixels): ");
  private final LabeledTextField ltfRescueRelaxtionParams = new LabeledTextField(
      FieldType.FLOATING_POINT_PAIR, "Rescue relaxation factors: ");
  private final LabeledTextField ltfResidualDistanceLimit = new LabeledTextField(
      FieldType.FLOATING_POINT, "First pass residual limit for deletion: ");
  private final LabeledTextField ltfMeanResidChangeLimits = new LabeledTextField(
      FieldType.INTEGER_PAIR, "Residual change limits: ");
  private final LabeledTextField ltfDeletionParams = new LabeledTextField(
      FieldType.FLOATING_POINT_PAIR, "Deletion residual parameters: ");
  private final LabeledTextField ltfDensityRelaxationPostFit = new LabeledTextField(
      FieldType.FLOATING_POINT, "Second pass density relaxation: ");
  private final LabeledTextField ltfMaxRescueDistance = new LabeledTextField(
      FieldType.FLOATING_POINT, "Second pass maximum rescue distance: ");

  private final CheckBox cbLocalAreaTracking = new CheckBox("Local tracking");
  private final LabeledTextField ltfLocalAreaTargetSize = new LabeledTextField(
      FieldType.INTEGER, "Local area size: ");
  private final LabeledTextField ltfMinBeadsInArea = new LabeledTextField(
      FieldType.INTEGER, "Minimum beads in area: ");
  private final LabeledTextField ltfMinOverlapBeads = new LabeledTextField(
      FieldType.INTEGER, "Minimum beads overlapping: ");
  private final LabeledTextField ltfMaxViewsInAlign = new LabeledTextField(
      FieldType.INTEGER, "Max. # views to include in align: ");
  private final LabeledTextField ltfRoundsOfTracking = new LabeledTextField(
      FieldType.INTEGER, "Rounds of tracking: ");
  private final CheckBox cbSobelFilterCentering = new CheckBox(
      "Refine center with Sobel filter");
  private final LabeledTextField ltfKernelSigmaForSobel = new LabeledTextField(
      FieldType.FLOATING_POINT, "Sigma for kernel filter: ");

  private final JPanel pnlCheckbox = new JPanel();
  private final JPanel pnlLightBeads = new JPanel();
  private final JPanel pnlLocalAreaTracking = new JPanel();
  private final EtomoPanel pnlExpertParameters = new EtomoPanel();
  private final JPanel pnlExpertParametersBody = new JPanel();

  private final PanelHeader expertParametersHeader;
  private final PanelHeader header;
  private final ApplicationManager manager;
  private final MultiLineButton btnTrack;
  private final MultiLineButton btnUseModel = new MultiLineButton(USE_MODEL_LABEL);
  private final BeadtrackPanelActionListener actionListener = new BeadtrackPanelActionListener(
      this);
  private final JPanel pnlFillGaps = new JPanel();
  private final JPanel pnlTrack = new JPanel();

  private final DialogType dialogType;

  private boolean autofidseedMode = false;

  /**
   * Construct a new beadtrack panel.
   * @param label specifies the suffix for the logfile
   */
  private BeadtrackPanel(final ApplicationManager manager, AxisID id,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    this.manager = manager;
    this.dialogType = dialogType;
    axisID = id;
    btnTrack = (MultiLineButton) manager.getProcessResultDisplayFactory(axisID)
        .getTrackFiducials();
    btnFixModel = (Run3dmodButton) manager.getProcessResultDisplayFactory(axisID)
        .getFixFiducialModel();
    btnFixModel.setContainer(this);
    expertParametersHeader = PanelHeader.getInstance("Expert Parameters", this,
        dialogType);
    header = PanelHeader.getAdvancedBasicInstance("Beadtracker", this, dialogType,
        globalAdvancedButton);

    JPanel pnlSobelFilterCentering = new JPanel();

    panelBeadtrackBody.setLayout(new BoxLayout(panelBeadtrackBody, BoxLayout.Y_AXIS));
    panelBeadtrackBody.add(Box.createRigidArea(FixedDim.x0_y5));
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
    pnlLightBeads.add(Box.createHorizontalGlue());
    panelBeadtrackBody.add(pnlLightBeads);
    panelBeadtrackBody.add(Box.createRigidArea(FixedDim.x0_y2));
    panelBeadtrackBody.add(pnlSobelFilterCentering);
    panelBeadtrackBody.add(ltfKernelSigmaForSobel.getContainer());
    panelBeadtrackBody.add(Box.createRigidArea(FixedDim.x0_y2));
    pnlCheckbox.setLayout(new BoxLayout(pnlCheckbox, BoxLayout.Y_AXIS));
    pnlCheckbox.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlFillGaps.setLayout(new BoxLayout(pnlFillGaps, BoxLayout.X_AXIS));
    pnlFillGaps.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlFillGaps.add(cbFillGaps);
    pnlFillGaps.add(Box.createHorizontalGlue());
    pnlCheckbox.add(pnlFillGaps);

    panelBeadtrackBody.add(pnlCheckbox);
    panelBeadtrackBody.add(ltfMaxGap.getContainer());

    pnlLocalAreaTracking.setLayout(new BoxLayout(pnlLocalAreaTracking, BoxLayout.Y_AXIS));
    pnlLocalAreaTracking.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlLocalAreaTracking.add(cbLocalAreaTracking);
    pnlLocalAreaTracking.add(Box.createHorizontalGlue());
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

    // SobelFilterCentering
    pnlSobelFilterCentering.setLayout(new BoxLayout(pnlSobelFilterCentering,
        BoxLayout.X_AXIS));
    pnlSobelFilterCentering.add(cbSobelFilterCentering);
    pnlSobelFilterCentering.add(Box.createHorizontalGlue());

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

    pnlExpertParameters.setLayout(new BoxLayout(pnlExpertParameters, BoxLayout.Y_AXIS));
    pnlExpertParameters.setBorder(BorderFactory.createEtchedBorder());
    pnlExpertParameters.add(expertParametersHeader);
    pnlExpertParameters.add(pnlExpertParametersBody);
    panelBeadtrackBody.add(pnlExpertParameters);

    btnTrack.setAlignmentX(Component.CENTER_ALIGNMENT);
    btnTrack.setSize();
    panelBeadtrackBody.add(btnTrack.getComponent());

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

    panelBeadtrackX.setLayout(new BoxLayout(panelBeadtrackX, BoxLayout.X_AXIS));
    panelBeadtrackX.add(panelBeadtrack);
    setToolTipText();
  }

  /**
   * Changes the display to/from autofidseedMode.  Does nothing if autofidseedMode would
   * be unchanged.
   * @param input
   */
  void updateAutofidseed(final boolean input) {
    if (input == autofidseedMode) {
      return;
    }
    autofidseedMode = input;
    // Change the padding
    panelBeadtrackX.removeAll();
    if (autofidseedMode) {
      panelBeadtrackX.add(Box.createRigidArea(FixedDim.x197_y0));
    }
    panelBeadtrackX.add(panelBeadtrack);
    if (autofidseedMode) {
      panelBeadtrackX.add(Box.createRigidArea(FixedDim.x197_y0));
    }
    // Change visibility of fields
    ltfTiltAngleGroupSize.setVisible(!autofidseedMode);
    ltfTiltAngleGroups.setVisible(!autofidseedMode);
    ltfMagnificationGroupSize.setVisible(!autofidseedMode);
    ltfMagnificationGroups.setVisible(!autofidseedMode);
    ltfNMinViews.setVisible(!autofidseedMode);
    ltfBeadDiameter.setVisible(!autofidseedMode);
    pnlCheckbox.setVisible(!autofidseedMode);
    ltfMaxGap.setVisible(!autofidseedMode);
    pnlFillGaps.setVisible(!autofidseedMode);
    pnlLocalAreaTracking.setVisible(!autofidseedMode);
    ltfLocalAreaTargetSize.setVisible(!autofidseedMode);
    ltfMinBeadsInArea.setVisible(!autofidseedMode);
    ltfMinOverlapBeads.setVisible(!autofidseedMode);
    ltfMaxViewsInAlign.setVisible(!autofidseedMode);
    ltfRoundsOfTracking.setVisible(!autofidseedMode);
    ltfMinTiltRangeToFindAxis.setVisible(!autofidseedMode);
    ltfMinTiltRangeToFindAngle.setVisible(!autofidseedMode);
    ltfSearchBoxPixels.setVisible(!autofidseedMode);
    pnlExpertParameters.setVisible(!autofidseedMode);
    btnTrack.setVisible(!autofidseedMode);
    pnlTrack.setVisible(!autofidseedMode);
    updateAdvanced(header.isAdvanced());
  }

  public static BeadtrackPanel getInstance(ApplicationManager manager, AxisID id,
      DialogType dialogType, GlobalExpandButton globalAdvancedButton) {
    BeadtrackPanel instance = new BeadtrackPanel(manager, id, dialogType,
        globalAdvancedButton);
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    cbLocalAreaTracking.addActionListener(actionListener);
    btnTrack.addActionListener(actionListener);
    btnUseModel.addActionListener(actionListener);
    btnFixModel.addActionListener(actionListener);
    cbSobelFilterCentering.addActionListener(actionListener);
  }

  void setVisible(final boolean visible) {
    panelBeadtrackX.setVisible(visible);
  }

  public void expand(GlobalExpandButton button) {
  }

  public void expand(ExpandButton button) {
    if (expertParametersHeader.equalsOpenClose(button)) {
      pnlExpertParametersBody.setVisible(button.isExpanded());
    }
    else if (header.equalsOpenClose(button)) {
      panelBeadtrackBody.setVisible(button.isExpanded());
    }
    else if (header.equalsAdvancedBasic(button)) {
      updateAdvanced(button.isExpanded());
    }
    UIHarness.INSTANCE.pack(axisID, manager);
  }

  public void setParameters(BaseScreenState screenState) {
    expertParametersHeader.setButtonStates(screenState, false);
    header.setButtonStates(screenState);
    // btnFixModel.setButtonState(screenState.getButtonState(btnFixModel
    // .getButtonStateKey()));
    // btnTrack.setButtonState(screenState.getButtonState(btnTrack
    // .getButtonStateKey()));
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
    ltfTiltAngleGroupSize.setText(beadtrackParams.getTiltDefaultGrouping().toString());
    ltfTiltAngleGroups.setText(beadtrackParams.getTiltAngleGroups());
    ltfMagnificationGroupSize.setText(beadtrackParams.getMagnificationGroupSize());
    ltfMagnificationGroups.setText(beadtrackParams.getMagnificationGroups());
    ltfNMinViews.setText(beadtrackParams.getMinViewsForTiltalign().toString());
    ltfBeadDiameter.setText(beadtrackParams.getBeadDiameter().toString());
    cbLightBeads.setSelected(beadtrackParams.getLightBeads().is());
    cbFillGaps.setSelected(beadtrackParams.getFillGaps());
    ltfMaxGap.setText(beadtrackParams.getMaxGapSize().toString());
    ltfMinTiltRangeToFindAxis.setText(beadtrackParams.getMinTiltRangeToFindAxis()
        .toString());
    ltfMinTiltRangeToFindAngle.setText(beadtrackParams.getMinTiltRangeToFindAngles()
        .toString());
    ltfSearchBoxPixels.setText(beadtrackParams.getSearchBoxPixels());
    ltfMaxFiducialsAvg.setText(beadtrackParams.getMaxBeadsToAverage().toString());
    ltfFiducialExtrapolationParams.setText(beadtrackParams
        .getFiducialExtrapolationParams());
    ltfRescueAttemptParams.setText(beadtrackParams.getRescueAttemptParams());
    ltfMinRescueDistance.setText(beadtrackParams.getDistanceRescueCriterion().toString());
    ltfRescueRelaxtionParams.setText(beadtrackParams.getRescueRelaxationParams());
    ltfResidualDistanceLimit.setText(beadtrackParams.getPostFitRescueResidual()
        .toString());
    ltfDensityRelaxationPostFit.setText(beadtrackParams.getDensityRelaxationPostFit()
        .toString());
    ltfMaxRescueDistance.setText(beadtrackParams.getMaxRescueDistance().toString());
    ltfMeanResidChangeLimits.setText(beadtrackParams.getMeanResidChangeLimits());
    ltfDeletionParams.setText(beadtrackParams.getDeletionParams());

    cbLocalAreaTracking.setSelected(beadtrackParams.getLocalAreaTracking().is());
    ltfLocalAreaTargetSize.setText(beadtrackParams.getLocalAreaTargetSize().toString());
    ltfMinBeadsInArea.setText(beadtrackParams.getMinBeadsInArea().toString());
    ltfMinOverlapBeads.setText(beadtrackParams.getMinOverlapBeads().toString());
    ltfMaxViewsInAlign.setText(beadtrackParams.getMaxViewsInAlign().toString());
    ltfRoundsOfTracking.setText(beadtrackParams.getRoundsOfTracking().toString());
    cbSobelFilterCentering.setSelected(beadtrackParams.isSobelFilterCentering());
    ltfKernelSigmaForSobel.setText(beadtrackParams.getKernelSigmaForSobel());

    setEnabled();
  }

  /**
   * Get the field values from the panel filling in the BeadtrackParam object
   */
  public boolean getParameters(BeadtrackParam beadtrackParams, final boolean doValidation)
      throws FortranInputSyntaxException, InvalidEtomoNumberException {
    try {
      beadtrackParams.setSkipViews(ltfViewSkipList.getText(doValidation));
      beadtrackParams
          .setAdditionalViewGroups(ltfAdditionalViewSets.getText(doValidation));
      beadtrackParams.setLightBeads(cbLightBeads.isSelected());
      beadtrackParams.setSobelFilterCentering(cbSobelFilterCentering.isSelected());
      beadtrackParams
          .setKernelSigmaForSobel(ltfKernelSigmaForSobel.getText(doValidation));
      beadtrackParams.setImagesAreBinned(UIExpertUtilities.INSTANCE.getStackBinning(
          manager, axisID, ".preali"));

      if (!autofidseedMode) {
        beadtrackParams.setFillGaps(cbFillGaps.isSelected());

        String errorTitle = "FieldInterface Error";
        String badParameter = "";
        // handle field that throw FortranInputSyntaxException
        try {
          badParameter = ltfTiltAngleGroups.getLabel();
          beadtrackParams.setTiltAngleGroups(ltfTiltAngleGroups.getText(doValidation));

          badParameter = ltfMagnificationGroups.getLabel();
          beadtrackParams.setMagnificationGroups(ltfMagnificationGroups
              .getText(doValidation));

          badParameter = ltfSearchBoxPixels.getLabel();
          beadtrackParams.setSearchBoxPixels(ltfSearchBoxPixels.getText(doValidation));

          badParameter = ltfFiducialExtrapolationParams.getLabel();
          beadtrackParams.setFiducialExtrapolationParams(ltfFiducialExtrapolationParams
              .getText(doValidation));

          badParameter = ltfRescueAttemptParams.getLabel();
          beadtrackParams.setRescueAttemptParams(ltfRescueAttemptParams
              .getText(doValidation));

          badParameter = ltfRescueRelaxtionParams.getLabel();
          beadtrackParams.setRescueRelaxationParams(ltfRescueRelaxtionParams
              .getText(doValidation));

          badParameter = ltfMeanResidChangeLimits.getLabel();
          beadtrackParams.setMeanResidChangeLimits(ltfMeanResidChangeLimits
              .getText(doValidation));

          badParameter = ltfDeletionParams.getLabel();
          beadtrackParams.setDeletionParams(ltfDeletionParams.getText(doValidation));

          // handle fields that display their own messages and throw
          // InvalidEtomoNumberException
          try {
            badParameter = ltfTiltAngleGroupSize.getLabel();
            String errorMessage = beadtrackParams.setTiltDefaultGrouping(
                ltfTiltAngleGroupSize.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMagnificationGroupSize.getLabel();
            errorMessage = beadtrackParams.setMagDefaultGrouping(
                ltfMagnificationGroupSize.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfNMinViews.getLabel();
            errorMessage = beadtrackParams.setMinViewsForTiltalign(
                ltfNMinViews.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMaxGap.getLabel();
            errorMessage = beadtrackParams.setMaxGapSize(ltfMaxGap.getText(doValidation))
                .validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMaxFiducialsAvg.getLabel();
            errorMessage = beadtrackParams.setMaxBeadsToAverage(
                ltfMaxFiducialsAvg.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMinRescueDistance.getLabel();
            errorMessage = beadtrackParams.setDistanceRescueCriterion(
                ltfMinRescueDistance.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfResidualDistanceLimit.getLabel();
            errorMessage = beadtrackParams.setPostFitRescueResidual(
                ltfResidualDistanceLimit.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfDensityRelaxationPostFit.getLabel();
            errorMessage = beadtrackParams.setDensityRelaxationPostFit(
                ltfDensityRelaxationPostFit.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMaxRescueDistance.getLabel();
            errorMessage = beadtrackParams.setMaxRescueDistance(
                ltfMaxRescueDistance.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMinTiltRangeToFindAxis.getLabel();
            errorMessage = beadtrackParams.setMinTiltRangeToFindAxis(
                ltfMinTiltRangeToFindAxis.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMinTiltRangeToFindAngle.getLabel();
            errorMessage = beadtrackParams.setMinTiltRangeToFindAngles(
                ltfMinTiltRangeToFindAngle.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfBeadDiameter.getLabel();
            errorMessage = beadtrackParams.setBeadDiameter(
                ltfBeadDiameter.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = cbLocalAreaTracking.getText();
            errorMessage = beadtrackParams.setLocalAreaTracking(
                cbLocalAreaTracking.isSelected()).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfLocalAreaTargetSize.getText(doValidation);
            errorMessage = beadtrackParams.setLocalAreaTargetSize(
                ltfLocalAreaTargetSize.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMinBeadsInArea.getText(doValidation);
            errorMessage = beadtrackParams.setMinBeadsInArea(
                ltfMinBeadsInArea.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMinOverlapBeads.getText(doValidation);
            errorMessage = beadtrackParams.setMinOverlapBeads(
                ltfMinOverlapBeads.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfMaxViewsInAlign.getText(doValidation);
            errorMessage = beadtrackParams.setMaxViewsInAlign(
                ltfMaxViewsInAlign.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }

            badParameter = ltfRoundsOfTracking.getText(doValidation);
            errorMessage = beadtrackParams.setRoundsOfTracking(
                ltfRoundsOfTracking.getText(doValidation)).validate(badParameter);
            if (errorMessage != null) {
              UIHarness.INSTANCE.openMessageDialog(manager, errorMessage, errorTitle,
                  axisID);
              throw new InvalidEtomoNumberException(errorMessage);
            }
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
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  public JPanel getContainer() {
    return panelBeadtrackX;
  }

  void buttonAction(String command, Run3dmodMenuOptions run3dmodMenuOptions) {
    try {
      if (command.equals(btnTrack.getActionCommand())) {
        manager.fiducialModelTrack(axisID, btnTrack, null, dialogType, this);
      }
      else if (command.equals(btnUseModel.getActionCommand())) {
        if (manager.makeFiducialModelSeedModel(axisID)) {
          manager.fiducialModelTrack(axisID, btnUseModel, null, dialogType, this);
        }
      }
      else if (command.equals(cbLocalAreaTracking.getText())
          || command.equals(cbSobelFilterCentering.getText())) {
        setEnabled();
      }
      else if (command.equals(btnFixModel.getActionCommand())) {
        // Validate skipList
        String skipList = ltfViewSkipList.getText(true).trim();
        if (skipList.length() > 0) {
          if (skipList.matches(".*\\s+.*")) {
            UIHarness.INSTANCE.openMessageDialog(manager, VIEW_SKIP_LIST_LABEL
                + " cannot contain embedded spaces.", "Entry Error", axisID);
            return;
          }
        }
        else {
          skipList = null;
        }
        manager.imodFixFiducials(axisID, run3dmodMenuOptions, btnFixModel,
            ImodProcess.BeadFixerMode.GAP_MODE, skipList);
      }
    }
    catch (FieldValidationFailedException e) {
      return;
    }
  }

  public void action(final Run3dmodButton button,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    buttonAction(button.getActionCommand(), run3dmodMenuOptions);
  }

  private void setEnabled() {
    ltfLocalAreaTargetSize.setEnabled(cbLocalAreaTracking.isSelected());
    ltfMinBeadsInArea.setEnabled(cbLocalAreaTracking.isSelected());
    ltfMinOverlapBeads.setEnabled(cbLocalAreaTracking.isSelected());
    ltfKernelSigmaForSobel.setEnabled(cbSobelFilterCentering.isSelected());
  }

  /**
   * Makes the advanced components visible or invisible
   */
  void updateAdvanced(boolean state) {
    cbLightBeads.setVisible(state);
    if (autofidseedMode) {
      return;
    }
    ltfTiltAngleGroupSize.setVisible(state);
    ltfTiltAngleGroups.setVisible(state);
    ltfMagnificationGroupSize.setVisible(state);
    ltfMagnificationGroups.setVisible(state);
    ltfNMinViews.setVisible(state);
    ltfBeadDiameter.setVisible(state);
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

  // ToolTip string setup
  private void setToolTipText() {
    String text;
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.BEADTRACK, axisID);
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
    ltfMaxGap
        .setToolTipText(EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.MAX_GAP_KEY));
    ltfMinTiltRangeToFindAxis.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MIN_TILT_RANGE_TO_FIND_AXIS_KEY));
    ltfMinTiltRangeToFindAngle.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MIN_TILT_RANGE_TO_FIND_ANGLES_KEY));
    ltfSearchBoxPixels.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.SEARCH_BOX_PIXELS_KEY));
    ltfMaxFiducialsAvg.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.MAX_FIDUCIALS_AVG_KEY));
    ltfFiducialExtrapolationParams.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.FIDUCIAL_EXTRAPOLATION_PARAMS_KEY));
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
    cbSobelFilterCentering.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.SOBEL_FILTER_CENTERING_KEY));
    ltfKernelSigmaForSobel.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
        BeadtrackParam.KERNEL_SIGMA_FOR_SOBEL_KEY));
    btnTrack.setToolTipText("Run Beadtrack to produce fiducial model from seed model.");
    btnFixModel.setToolTipText("Load fiducial model into 3dmod.");
    btnUseModel
        .setToolTipText("Turn the output of Beadtrack (fiducial model) into a new seed model and then track.  "
            + "Your original seed model will be moved into an _orig.seed file.");
  }

  private final class BeadtrackPanelActionListener implements ActionListener {
    private final BeadtrackPanel adaptee;

    private BeadtrackPanelActionListener(final BeadtrackPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(final ActionEvent event) {
      adaptee.buttonAction(event.getActionCommand(), null);
    }
  }

}
