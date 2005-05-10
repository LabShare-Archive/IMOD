package etomo.ui;

import javax.swing.*;
import java.awt.Component;

import etomo.comscript.BeadtrackParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.InvalidEtomoNumberException;

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
public class BeadtrackPanel {
  public static final String rcsid =
    "$Id$";

  private JPanel panelBeadtrack = new JPanel();
  private AxisID axisID;

  private LabeledTextField ltfInputImage =
    new LabeledTextField("Input image file: ");
  private LabeledTextField ltfSeedModelFile =
    new LabeledTextField("Seed model file: ");
  private LabeledTextField ltfOutputModelFile =
    new LabeledTextField("Output model file: ");
  private LabeledTextField ltfViewSkipList =
    new LabeledTextField("View skip list: ");
  private LabeledTextField ltfAdditionalViewSets =
    new LabeledTextField("Separate view groups: ");
  private LabeledTextField ltfTiltAngleGroupSize =
    new LabeledTextField("Tilt angle group size: ");
  private LabeledTextField ltfTiltAngleGroups =
    new LabeledTextField("Non-default tilt angle groups: ");
  private LabeledTextField ltfMagnificationGroupSize =
    new LabeledTextField("Magnification group size: ");
  private LabeledTextField ltfMagnificationGroups =
    new LabeledTextField("Non-default magnification groups: ");
  private LabeledTextField ltfNMinViews =
    new LabeledTextField("Minimum # of views for tilt alignment: ");
  private LabeledTextField ltfFiducialParams =
    new LabeledTextField("Fiducial marker radius & polarity: ");
  JCheckBox cbFillGaps = new JCheckBox("Fill seed model gaps");
  private LabeledTextField ltfMaxGap =
    new LabeledTextField("Maximum gap size: ");
  private LabeledTextField ltfTiltAngleMinRange =
    new LabeledTextField("Minimums: ");
  private LabeledTextField ltfSearchBoxPixels =
    new LabeledTextField("Search box size (pixels): ");
  private LabeledTextField ltfMaxFiducialsAvg =
    new LabeledTextField("Maximum # of views for fiducial avg.: ");
  private LabeledTextField ltfFiducialExtrapolationParams =
    new LabeledTextField("Fiducial extrapolation limits: ");
  private LabeledTextField ltfRescueAttemptParams =
    new LabeledTextField("Rescue attempt criteria: ");
  private LabeledTextField ltfMinRescueDistance =
    new LabeledTextField("Maximum rescue distance (pixels): ");
  private LabeledTextField ltfRescueRelaxtionParams =
    new LabeledTextField("Rescue relaxation factors: ");
  private LabeledTextField ltfResidualDistanceLimit =
    new LabeledTextField("First pass residual limit for deletion: ");
  private LabeledTextField ltfSecondPassParams =
    new LabeledTextField("Second pass relaxation parameters: ");
  private LabeledTextField ltfMeanResidChangeLimits =
    new LabeledTextField("Residual change limits: ");
  private LabeledTextField ltfDeletionParams =
    new LabeledTextField("Deletion residual parameters: ");
  private JPanel pnlCheckbox = new JPanel();

  /**
   * Construct a new beadtrack panel.
   * @param label specifies the suffix for the logfile
   */
  public BeadtrackPanel(AxisID id) {
    axisID = id;
    setToolTipText();

    panelBeadtrack.setLayout(new BoxLayout(panelBeadtrack, BoxLayout.Y_AXIS));

    panelBeadtrack.add(ltfInputImage.getContainer());
//    panelBeadtrack.add(ltfPiceListFile.getContainer());
    panelBeadtrack.add(ltfSeedModelFile.getContainer());
    panelBeadtrack.add(ltfOutputModelFile.getContainer());
    panelBeadtrack.add(ltfViewSkipList.getContainer());
    panelBeadtrack.add(ltfViewSkipList.getContainer());
    panelBeadtrack.add(ltfAdditionalViewSets.getContainer());
    panelBeadtrack.add(ltfTiltAngleGroupSize.getContainer());
    panelBeadtrack.add(ltfTiltAngleGroups.getContainer());
    panelBeadtrack.add(ltfMagnificationGroupSize.getContainer());
    panelBeadtrack.add(ltfMagnificationGroups.getContainer());
    panelBeadtrack.add(ltfNMinViews.getContainer());
    panelBeadtrack.add(ltfFiducialParams.getContainer());
    
    pnlCheckbox.setLayout(new BoxLayout(pnlCheckbox, BoxLayout.Y_AXIS));
    pnlCheckbox.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlCheckbox.add(cbFillGaps);
    
    panelBeadtrack.add(pnlCheckbox);
    panelBeadtrack.add(ltfMaxGap.getContainer());
    panelBeadtrack.add(ltfTiltAngleMinRange.getContainer());
    panelBeadtrack.add(ltfSearchBoxPixels.getContainer());
    panelBeadtrack.add(ltfMaxFiducialsAvg.getContainer());
    panelBeadtrack.add(ltfFiducialExtrapolationParams.getContainer());
    panelBeadtrack.add(ltfRescueAttemptParams.getContainer());
    panelBeadtrack.add(ltfMinRescueDistance.getContainer());
    panelBeadtrack.add(ltfRescueRelaxtionParams.getContainer());
    panelBeadtrack.add(ltfResidualDistanceLimit.getContainer());
    panelBeadtrack.add(ltfSecondPassParams.getContainer());
    panelBeadtrack.add(ltfMeanResidChangeLimits.getContainer());
    panelBeadtrack.add(ltfDeletionParams.getContainer());

  }

  /**
   * Set the field values for the panel from the ConstBeadtrackParam object
   */
  public void setParameters(/*Const*/BeadtrackParam beadtrackParams) {
    ConstEtomoNumber field = null;
    ltfInputImage.setText(beadtrackParams.getInputFile());
//    ltfPiceListFile.setText(beadtrackParams.getPieceListFile());
    ltfSeedModelFile.setText(beadtrackParams.getSeedModelFile());
    ltfOutputModelFile.setText(beadtrackParams.getOutputModelFile());
    ltfViewSkipList.setText(beadtrackParams.getSkipViews());
    ltfAdditionalViewSets.setText(beadtrackParams.getAdditionalViewGroups());
    ltfTiltAngleGroupSize.setText(beadtrackParams.getTiltDefaultGrouping().toString());
    ltfTiltAngleGroups.setText(beadtrackParams.getTiltAngleGroups());
    ltfMagnificationGroupSize.setText(
      beadtrackParams.getMagnificationGroupSize());
    ltfMagnificationGroups.setText(beadtrackParams.getMagnificationGroups());
    ltfNMinViews.setText(beadtrackParams.getMinViewsForTiltalign().toString());
    ltfFiducialParams.setText(beadtrackParams.getFiducialParams());
    cbFillGaps.setSelected(beadtrackParams.getFillGaps());
    ltfMaxGap.setText(beadtrackParams.getMaxGapSize().toString());
    ltfTiltAngleMinRange.setText(beadtrackParams.getTiltAngleMinRange());
    ltfSearchBoxPixels.setText(beadtrackParams.getSearchBoxPixels());
    ltfMaxFiducialsAvg.setText(beadtrackParams.getMaxBeadsToAverage().toString());
    ltfFiducialExtrapolationParams.setText(
      beadtrackParams.getFiducialExtrapolationParams());
    ltfRescueAttemptParams.setText(beadtrackParams.getRescueAttemptParams());
    ltfMinRescueDistance.setText(beadtrackParams.getDistanceRescueCriterion().toString());
    ltfRescueRelaxtionParams.setText(
      beadtrackParams.getRescueRelaxationParams());
    ltfResidualDistanceLimit.setText(
      beadtrackParams.getPostFitRescueResidual().toString());
    ltfSecondPassParams.setText(beadtrackParams.getSecondPassParams());
    ltfMeanResidChangeLimits.setText(
      beadtrackParams.getMeanResidChangeLimits());
    ltfDeletionParams.setText(beadtrackParams.getDeletionParams());
  }

  /**
   * Get the field values from the panel filling in the BeadtrackParam object
   */
  public void getParameters(BeadtrackParam beadtrackParams)
      throws FortranInputSyntaxException, InvalidEtomoNumberException {
    beadtrackParams.setInputFile(ltfInputImage.getText());
    //      beadtrackParams.setPieceListFile(ltfPiceListFile.getText());
    beadtrackParams.setSeedModelFile(ltfSeedModelFile.getText());
    beadtrackParams.setOutputModelFile(ltfOutputModelFile.getText());
    beadtrackParams.setFillGaps(cbFillGaps.isSelected());

    String badParameter = "";
    try {
      //handle fields that do not display their own messages
      try {
        badParameter = ltfViewSkipList.getLabel();
        beadtrackParams.setSkipViews(ltfViewSkipList.getText());

        badParameter = ltfAdditionalViewSets.getLabel();
        beadtrackParams
            .setAdditionalViewGroups(ltfAdditionalViewSets.getText());

        badParameter = ltfTiltAngleGroups.getLabel();
        beadtrackParams.setTiltAngleGroups(ltfTiltAngleGroups.getText());

        badParameter = ltfTiltAngleMinRange.getLabel();
        beadtrackParams.setTiltAngleMinRange(ltfTiltAngleMinRange.getText());

        badParameter = ltfMagnificationGroups.getLabel();
        beadtrackParams
            .setMagnificationGroups(ltfMagnificationGroups.getText());

        badParameter = ltfFiducialParams.getLabel();
        beadtrackParams.setFiducialParams(ltfFiducialParams.getText());

        badParameter = ltfSearchBoxPixels.getLabel();
        beadtrackParams.setSearchBoxPixels(ltfSearchBoxPixels.getText());

        badParameter = ltfFiducialExtrapolationParams.getLabel();
        beadtrackParams
            .setFiducialExtrapolationParams(ltfFiducialExtrapolationParams
                .getText());

        badParameter = ltfRescueAttemptParams.getLabel();
        beadtrackParams
            .setRescueAttemptParams(ltfRescueAttemptParams.getText());

        badParameter = ltfRescueRelaxtionParams.getLabel();
        beadtrackParams.setRescueRelaxationParams(ltfRescueRelaxtionParams
            .getText());

        badParameter = ltfSecondPassParams.getLabel();
        beadtrackParams.setSecondPassParams(ltfSecondPassParams.getText());

        badParameter = ltfMeanResidChangeLimits.getLabel();
        beadtrackParams.setMeanResidChangeLimits(ltfMeanResidChangeLimits
            .getText());

        badParameter = ltfDeletionParams.getLabel();
        beadtrackParams.setDeletionParams(ltfDeletionParams.getText());
      }
      catch (InvalidEtomoNumberException e) {
        UIHarness.INSTANCE.openMessageDialog(badParameter + " "
            + e.getMessage(), "Field Error", axisID);
        throw e;
      }
      //handle fields that do display their own messages
      try {
        badParameter = ltfTiltAngleGroupSize.getLabel();
        beadtrackParams.setTiltDefaultGrouping(ltfTiltAngleGroupSize.getText())
            .validate(badParameter, axisID);
        
        badParameter = ltfMagnificationGroupSize.getLabel();
        beadtrackParams.setMagDefaultGrouping(
            ltfMagnificationGroupSize.getText()).validate(badParameter, axisID);

        badParameter = ltfNMinViews.getLabel();
        beadtrackParams.setMinViewsForTiltalign(ltfNMinViews.getText())
            .validate(badParameter, axisID);

        badParameter = ltfMaxGap.getLabel();
        beadtrackParams.setMaxGapSize(ltfMaxGap.getText()).validate(
            badParameter, axisID);

        badParameter = ltfMaxFiducialsAvg.getLabel();
        beadtrackParams.setMaxBeadsToAverage(ltfMaxFiducialsAvg.getText())
            .validate(badParameter, axisID);

        badParameter = ltfMinRescueDistance.getLabel();
        beadtrackParams.setDistanceRescueCriterion(
            ltfMinRescueDistance.getText()).validate(badParameter, axisID);

        badParameter = ltfResidualDistanceLimit.getLabel();
        beadtrackParams.setPostFitRescueResidual(
            ltfResidualDistanceLimit.getText()).validate(badParameter, axisID);
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

  public void setVisible(boolean state) {
    panelBeadtrack.setVisible(state);
  }

  /**
   * Makes the advanced components visible or invisible
   */
  void setAdvanced(boolean state) {
    //  always visible components
    //ltfViewSkipList.setVisible(state);
    //ltfNAdditionalViewSets.setVisible(state);
    //ltfAdditionalViewSets.setVisible(state);
    //ltfTiltAngleGroupParams.setVisible(state);
    //ltfTiltAngleGroups.setVisible(state);
    //chkboxFillGaps.setVisible(state);
    ltfInputImage.setVisible(state);
//    ltfPiceListFile.setVisible(state);
    ltfSeedModelFile.setVisible(state);
    ltfOutputModelFile.setVisible(state);
    ltfTiltAngleGroupSize.setVisible(state);
    ltfTiltAngleGroups.setVisible(state);
    ltfMagnificationGroupSize.setVisible(state);
    ltfMagnificationGroups.setVisible(state);
    ltfNMinViews.setVisible(state);
    ltfFiducialParams.setVisible(state);

    ltfMaxGap.setVisible(state);
    ltfTiltAngleMinRange.setVisible(state);
    ltfSearchBoxPixels.setVisible(state);
    ltfMaxFiducialsAvg.setVisible(state);
    ltfFiducialExtrapolationParams.setVisible(state);
    ltfRescueAttemptParams.setVisible(state);
    ltfMinRescueDistance.setVisible(state);
    ltfRescueRelaxtionParams.setVisible(state);
    ltfResidualDistanceLimit.setVisible(state);
    ltfSecondPassParams.setVisible(state);
    ltfMeanResidChangeLimits.setVisible(state);
    ltfDeletionParams.setVisible(state);

  }

  //  ToolTip string setup
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text = "Input file with images to track.";
    ltfInputImage.setToolTipText(tooltipFormatter.setText(text).format());
    
    text = "Input model file of starting points to track.";
    ltfSeedModelFile.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Output file for tracked model.";
    ltfOutputModelFile.setToolTipText(tooltipFormatter.setText(text).format());

    text = "List of views to skip over (comma-separated ranges without spaces).";
    ltfViewSkipList.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Lists of views to group separately from other views.  Multiple lists can be "      + "entered; separate them by spaces.";
    ltfAdditionalViewSets.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Default number of views to group together in solving for tilt angles.";
    ltfTiltAngleGroupSize.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Sets of views with non-default grouping for tilt angle.  For each set, "
      + "enter starting and ending view number and group size, separated by commas; "
      + "separate multiple sets with spaces.";
    ltfTiltAngleGroups.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Default number of views to group together in solving for magnifications.";
    ltfMagnificationGroupSize.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Sets of views with non-default grouping for magnification.  For each set, "
      + "enter starting and ending view number and group size, separated by commas; "
      + "separate multiple sets with spaces.";
    ltfMagnificationGroups.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Minimum number of views with bead positions required to do a tilt "      + "alignment.";
    ltfNMinViews.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Radius for calculation of centroid, and 0 if beads are darker or 1 if "
      + "they are lighter than background.";
    ltfFiducialParams.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Fill in gaps in the seed model or leave them empty.";
    cbFillGaps.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum size of gap to create in the model before giving up on a fiducial.";
    ltfMaxGap.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Minimum range of tilt angles for which data must be available before "
      + "solving for tilt axis angle, and minimum range of angles required before "
      + "solving for tilt angles.";
    ltfTiltAngleMinRange.setToolTipText(tooltipFormatter.setText(text).format());
    
    text = "X and Y dimensions of the box used to search for a bead.";
    ltfSearchBoxPixels.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Maximum number of views over which to average a bead.";
    ltfMaxFiducialsAvg.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Number of positions to use for extrapolating a bead position to "
      + "the next view, and minimum required to use extrapolation rather "
      + "than just the mean of positions on the last few views.";
    ltfFiducialExtrapolationParams.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Fraction of mean bead integral, and number of SDs "
      + "below mean, to use as the criterion for when to attempt a rescue "
      + "based on bead density.";
    ltfRescueAttemptParams.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Distance away from expected position at which to attempt "
      + "a rescue based on excessive distance.";
    ltfMinRescueDistance.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Factors by which to relax the density criterion when "
      + "trying to rescue - a factor for density rescue and one for "
      + "distance rescue.";
    ltfRescueRelaxtionParams.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Criterion distance for deletion of a point on the first pass "
      + "after tilt alignment.";
    ltfResidualDistanceLimit.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Factor by which to relax the density criterion on the second pass, "
      + "and maximum distance to search from the expected position.";
    ltfSecondPassParams.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Maximum and minimum number of changes in mean residual to use in "
      + "finding the mean and SD of changes in the mean residual for a bead.";
    ltfMeanResidChangeLimits.setToolTipText(tooltipFormatter.setText(text).format());

    text = 
      "Minimum change in residual, and criterion number of SDs from the "
      + "mean residual change, to require for deletion of a point.";
    ltfDeletionParams.setToolTipText(tooltipFormatter.setText(text).format());
  }
}
