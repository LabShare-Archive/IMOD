package etomo.ui;

import javax.swing.*;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.FileNotFoundException;
import java.io.IOException;

import etomo.comscript.BeadtrackParam;
import etomo.comscript.FortranInputSyntaxException;
import etomo.type.AxisID;
import etomo.type.ConstEtomoNumber;
import etomo.type.EtomoAutodoc;
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
  private LabeledTextField ltfCentroidRadius =
    new LabeledTextField("Fiducial marker radius: ");
  private JCheckBox cbLightBeads = new JCheckBox("Light fiducial markers");
  JCheckBox cbFillGaps = new JCheckBox("Fill seed model gaps");
  private LabeledTextField ltfMaxGap =
    new LabeledTextField("Maximum gap size: ");
  private LabeledTextField ltfMinTiltRangeToFindAxis =
    new LabeledTextField("Minimum axis: ");
  private LabeledTextField ltfMinTiltRangeToFindAngle =
    new LabeledTextField("Minimum angle: ");
  private LabeledTextField ltfSearchBoxPixels =
    new LabeledTextField("Search box size (pixels): ");
  private LabeledTextField ltfMaxFiducialsAvg =
    new LabeledTextField("Maximum # of views for fiducial avg.: ");
  private LabeledTextField ltfFiducialExtrapolationParams =
    new LabeledTextField("Fiducial extrapolation limits: ");
  private LabeledTextField ltfRescueAttemptParams =
    new LabeledTextField("Rescue attempt criteria: ");
  private LabeledTextField ltfMinRescueDistance =
    new LabeledTextField("Distance criterion for rescue (pixels): ");
  private LabeledTextField ltfRescueRelaxtionParams =
    new LabeledTextField("Rescue relaxation factors: ");
  private LabeledTextField ltfResidualDistanceLimit =
    new LabeledTextField("First pass residual limit for deletion: ");
  private LabeledTextField ltfMeanResidChangeLimits =
    new LabeledTextField("Residual change limits: ");
  private LabeledTextField ltfDeletionParams =
    new LabeledTextField("Deletion residual parameters: ");
  private LabeledTextField ltfDensityRelaxationPostFit = new LabeledTextField(
      "Second pass density relaxation: ");
  private LabeledTextField ltfMaxRescueDistance = new LabeledTextField(
  "Second pass maximum rescue distance: ");
  
  private JCheckBox cbLocalAreaTracking = new JCheckBox("Local tracking");
  private LabeledTextField ltfLocalAreaTargetSize = new LabeledTextField(
      "Local area size: ");
  private LabeledTextField ltfMinBeadsInArea = new LabeledTextField(
      "Minimum beads in area: ");
  private LabeledTextField ltfMinOverlapBeads = new LabeledTextField(
  "Minimum beads overlapping: ");
  private LabeledTextField ltfMaxViewsInAlign = new LabeledTextField(
  "Max. # views to include in align: ");
  private LabeledTextField ltfRoundsOfTracking = new LabeledTextField(
  "Rounds of tracking: ");
  
  private JPanel pnlCheckbox = new JPanel();
  private JPanel pnlLightBeads = new JPanel();
  private JPanel pnlLocalAreaTracking = new JPanel();
  
  /**
   * Construct a new beadtrack panel.
   * @param label specifies the suffix for the logfile
   */
  public BeadtrackPanel(AxisID id) {
    axisID = id;
    setToolTipText();

    BeadtrackPanelActionListener listener = new BeadtrackPanelActionListener(this);
    cbLocalAreaTracking.addActionListener(listener);
    
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
    panelBeadtrack.add(ltfCentroidRadius.getContainer());
    
    pnlLightBeads.setLayout(new BoxLayout(pnlLightBeads, BoxLayout.Y_AXIS));
    pnlLightBeads.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlLightBeads.add(cbLightBeads);
    panelBeadtrack.add(pnlLightBeads);
    
    pnlCheckbox.setLayout(new BoxLayout(pnlCheckbox, BoxLayout.Y_AXIS));
    pnlCheckbox.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlCheckbox.add(cbFillGaps);
    
    panelBeadtrack.add(pnlCheckbox);
    panelBeadtrack.add(ltfMaxGap.getContainer());
    panelBeadtrack.add(ltfMinTiltRangeToFindAxis.getContainer());
    panelBeadtrack.add(ltfMinTiltRangeToFindAngle.getContainer());
    panelBeadtrack.add(ltfSearchBoxPixels.getContainer());
    panelBeadtrack.add(ltfMaxFiducialsAvg.getContainer());
    panelBeadtrack.add(ltfFiducialExtrapolationParams.getContainer());
    panelBeadtrack.add(ltfRescueAttemptParams.getContainer());
    panelBeadtrack.add(ltfMinRescueDistance.getContainer());
    panelBeadtrack.add(ltfRescueRelaxtionParams.getContainer());
    panelBeadtrack.add(ltfResidualDistanceLimit.getContainer());
    panelBeadtrack.add(ltfDensityRelaxationPostFit.getContainer());
    panelBeadtrack.add(ltfMaxRescueDistance.getContainer());
    panelBeadtrack.add(ltfMeanResidChangeLimits.getContainer());
    panelBeadtrack.add(ltfDeletionParams.getContainer());
    
    pnlLocalAreaTracking.setLayout(new BoxLayout(pnlLocalAreaTracking, BoxLayout.Y_AXIS));
    pnlLocalAreaTracking.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlLocalAreaTracking.add(cbLocalAreaTracking);
    panelBeadtrack.add(pnlLocalAreaTracking);
    
    panelBeadtrack.add(ltfLocalAreaTargetSize.getContainer());
    panelBeadtrack.add(ltfMinBeadsInArea.getContainer());
    panelBeadtrack.add(ltfMinOverlapBeads.getContainer());
    panelBeadtrack.add(ltfMaxViewsInAlign.getContainer());
    panelBeadtrack.add(ltfRoundsOfTracking.getContainer());
    
    setEnabled();
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
    ltfCentroidRadius.setText(beadtrackParams.getCentroidRadius().toString());
    cbLightBeads.setSelected(beadtrackParams.getLightBeads().is());
    cbFillGaps.setSelected(beadtrackParams.getFillGaps());
    ltfMaxGap.setText(beadtrackParams.getMaxGapSize().toString());
    ltfMinTiltRangeToFindAxis.setText(beadtrackParams.getMinTiltRangeToFindAxis().toString());
    ltfMinTiltRangeToFindAngle.setText(beadtrackParams.getMinTiltRangeToFindAngles().toString());
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
    ltfDensityRelaxationPostFit.setText(beadtrackParams
        .getDensityRelaxationPostFit().toString());
    ltfMaxRescueDistance.setText(beadtrackParams.getMaxRescueDistance()
        .toString());
    ltfMeanResidChangeLimits.setText(
      beadtrackParams.getMeanResidChangeLimits());
    ltfDeletionParams.setText(beadtrackParams.getDeletionParams());
    
    cbLocalAreaTracking.setSelected(beadtrackParams.getLocalAreaTracking().is());
    ltfLocalAreaTargetSize.setText(beadtrackParams.getLocalAreaTargetSize().toString());
    ltfMinBeadsInArea.setText(beadtrackParams.getMinBeadsInArea().toString());
    ltfMinOverlapBeads.setText(beadtrackParams.getMinOverlapBeads().toString());
    ltfMaxViewsInAlign.setText(beadtrackParams.getMaxViewsInAlign().toString());
    ltfRoundsOfTracking.setText(beadtrackParams.getRoundsOfTracking().toString());
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

        badParameter = ltfCentroidRadius.getLabel();
        beadtrackParams.setCentroidRadius(ltfCentroidRadius.getText())
            .validate(errorTitle, badParameter, axisID);

        badParameter = cbLightBeads.getText();
        beadtrackParams.setLightBeads(cbLightBeads.isSelected()).validate(
            errorTitle, badParameter, axisID);
        
        
        badParameter = cbLocalAreaTracking.getText();
        beadtrackParams.setLocalAreaTracking(cbLocalAreaTracking.isSelected())
            .validate(errorTitle, badParameter, axisID);
        
        badParameter = ltfLocalAreaTargetSize.getText();
        beadtrackParams.setLocalAreaTargetSize(ltfLocalAreaTargetSize.getText())
            .validate(errorTitle, badParameter, axisID);
        
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
  
  private void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(cbLocalAreaTracking.getText())) {
      setEnabled();
    }
  }
  
  private void setEnabled() {
    ltfLocalAreaTargetSize.setEnabled(cbLocalAreaTracking.isSelected());
    ltfMinBeadsInArea.setEnabled(cbLocalAreaTracking.isSelected());
    ltfMinOverlapBeads.setEnabled(cbLocalAreaTracking.isSelected());
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
    ltfCentroidRadius.setVisible(state);
    cbLightBeads.setVisible(state);
    
    ltfMaxGap.setVisible(state);
    ltfMinTiltRangeToFindAxis.setVisible(state);
    ltfMinTiltRangeToFindAngle.setVisible(state);
    ltfSearchBoxPixels.setVisible(state);
    ltfMaxFiducialsAvg.setVisible(state);
    ltfFiducialExtrapolationParams.setVisible(state);
    ltfRescueAttemptParams.setVisible(state);
    ltfMinRescueDistance.setVisible(state);
    ltfRescueRelaxtionParams.setVisible(state);
    ltfResidualDistanceLimit.setVisible(state);
    ltfDensityRelaxationPostFit.setVisible(state);
    ltfMaxRescueDistance.setVisible(state);
    ltfMeanResidChangeLimits.setVisible(state);
    ltfDeletionParams.setVisible(state);
    
    ltfMinBeadsInArea.setVisible(state);
    ltfMinOverlapBeads.setVisible(state);
    ltfRoundsOfTracking.setVisible(state);
  }

  //  ToolTip string setup
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    Autodoc autodoc = null;
    try {
      autodoc = Autodoc.get(Autodoc.BEADTRACK, axisID);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    ltfInputImage.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.INPUT_FILE_KEY))
        .format());
    ltfSeedModelFile.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.SEED_MODEL_FILE_KEY))
        .format());
    ltfOutputModelFile.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.OUTPUT_MODEL_FILE_KEY))
        .format());
    ltfViewSkipList.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.SKIP_VIEW_LIST_KEY))
        .format());
    ltfAdditionalViewSets.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.ADDITIONAL_VIEW_GROUPS_KEY)).format());
    ltfTiltAngleGroupSize.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.TILT_ANGLE_GROUP_PARAMS_KEY)).format());
    ltfTiltAngleGroups.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.TILT_ANGLE_GROUPS_KEY))
        .format());
    ltfMagnificationGroupSize.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.MAGNIFICATION_GROUP_PARAMS_KEY)).format());
    ltfMagnificationGroups.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.MAGNIFICATION_GROUPS_KEY)).format());
    ltfNMinViews.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.N_MIN_VIEWS_KEY))
        .format());
    ltfCentroidRadius.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.CENTROID_RADIUS_KEY))
        .format());
    cbLightBeads.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.LIGHT_BEADS_KEY))
        .format());
    cbFillGaps.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.FILL_GAPS_KEY))
        .format());
    ltfMaxGap.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.MAX_GAP_KEY)).format());
    ltfMinTiltRangeToFindAxis.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.MIN_TILT_RANGE_TO_FIND_AXIS_KEY)).format());
    ltfMinTiltRangeToFindAngle.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.MIN_TILT_RANGE_TO_FIND_ANGLES_KEY)).format());
    ltfSearchBoxPixels.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.SEARCH_BOX_PIXELS_KEY))
        .format());
    ltfMaxFiducialsAvg.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.MAX_FIDUCIALS_AVG_KEY))
        .format());
    ltfFiducialExtrapolationParams.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.FIDUCIAL_EXTRAPOLATION_PARAMS_KEY)).format());
    ltfRescueAttemptParams.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.RESCUE_ATTEMPT_PARAMS_KEY)).format());
    ltfMinRescueDistance.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc
            .getTooltip(autodoc, BeadtrackParam.MIN_RESCUE_DISTANCE_KEY))
        .format());
    ltfRescueRelaxtionParams.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.RESCUE_RELAXATION_PARAMS_KEY)).format());
    ltfResidualDistanceLimit.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.RESIDUAL_DISTANCE_LIMIT_KEY)).format());
    ltfDensityRelaxationPostFit.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.DENSITY_RELAXATION_POST_FIT_KEY)).format());
    ltfMaxRescueDistance.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc
            .getTooltip(autodoc, BeadtrackParam.MAX_RESCUE_DISTANCE_KEY))
        .format());
    ltfMeanResidChangeLimits.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.MEAN_RESID_CHANGE_LIMITS_KEY)).format());
    ltfDeletionParams.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.DELETION_PARAMS_KEY))
        .format());
    
    cbLocalAreaTracking.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc
            .getTooltip(autodoc, BeadtrackParam.LOCAL_AREA_TRACKING_KEY))
        .format());
    ltfLocalAreaTargetSize.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc,
            BeadtrackParam.LOCAL_AREA_TARGET_SIZE_KEY)).format());
    ltfMinBeadsInArea.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.MIN_BEADS_IN_AREA_KEY))
        .format());
    ltfMinOverlapBeads.setToolTipText(tooltipFormatter.setText(
        EtomoAutodoc.getTooltip(autodoc, BeadtrackParam.MIN_OVERLAP_BEADS_KEY))
        .format());
    ltfMaxViewsInAlign.setToolTipText(tooltipFormatter
        .setText(
            EtomoAutodoc.getTooltip(autodoc,
                BeadtrackParam.MAX_VIEWS_IN_ALIGN_KEY)).format());
    ltfRoundsOfTracking.setToolTipText(tooltipFormatter
        .setText(
            EtomoAutodoc.getTooltip(autodoc,
                BeadtrackParam.ROUNDS_OF_TRACKING_KEY)).format());
  }
  
  private class BeadtrackPanelActionListener implements ActionListener {

    BeadtrackPanel adaptee;

    BeadtrackPanelActionListener(BeadtrackPanel adaptee) {
      this.adaptee = adaptee;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }

}
