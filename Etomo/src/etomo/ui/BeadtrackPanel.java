package etomo.ui;


import java.awt.event.*;
import javax.swing.*;

import etomo.comscript.ConstBeadtrackParam;
import etomo.comscript.BeadtrackParam;
import etomo.comscript.FortranInputSyntaxException;

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
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class BeadtrackPanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel panelBeadtrack = new JPanel();
  private String logSuffix;

  private LabeledTextField ltfInputImage =
    new LabeledTextField("Input image file: ");
  private LabeledTextField ltfPiceListFile =
    new LabeledTextField("Piece list file: ");
  private LabeledTextField ltfSeedModelFile =
    new LabeledTextField("Seed model file: ");
  private LabeledTextField ltfOutputModelFile =
    new LabeledTextField("Output model file: ");
  private LabeledTextField ltfViewSkipList =
    new LabeledTextField("View skip list: ");
  private LabeledTextField ltfAdditionalViewSets =
    new LabeledTextField("Additional view groups: ");
  private LabeledTextField ltfTiltAngleGroupSize =
    new LabeledTextField("Tilt angle group size: ");
  private LabeledTextField ltfTiltAngleGroups =
    new LabeledTextField("Non-default tilt angle groups: ");
  private LabeledTextField ltfMagnificationGroupSize =
    new LabeledTextField("Magnification group size: ");
  private LabeledTextField ltfMagnificationGroups =
    new LabeledTextField("Non-default magnification groups: ");
  private LabeledTextField ltfNMinViews =
    new LabeledTextField("Minimum number of views for tilt alignment: ");
  private LabeledTextField ltfFiducialParams =
    new LabeledTextField("Fiducial marker parameters: ");
  JCheckBox chkboxFillGaps = new JCheckBox("Fill seed model gaps");
  private LabeledTextField ltfMaxGap =
    new LabeledTextField("Maximum gap size: ");
  private LabeledTextField ltfTiltAngleMinRange =
    new LabeledTextField("Tilt angle mimimums: ");
  private LabeledTextField ltfSearchBoxPixels =
    new LabeledTextField("Search box size (pixels): ");
  private LabeledTextField ltfMaxFiducialsAvg =
    new LabeledTextField("Maximum # of views for fiducial avg.: ");
  private LabeledTextField ltfFiducialExtrapolationParams =
    new LabeledTextField("Fiducial extrapolation parameters: ");
  private LabeledTextField ltfRescueAttemptParams =
    new LabeledTextField("Rescue attempt parameters: ");
  private LabeledTextField ltfMinRescueDistance =
    new LabeledTextField("Min rescue distance (pixels): ");
  private LabeledTextField ltfRescueRelaxtionParams =
    new LabeledTextField("Rescue relaxation parameters: ");
  private LabeledTextField ltfResidualDistanceLimit =
    new LabeledTextField("First pass residual limit for deletion: ");
  private LabeledTextField ltfSecondPassParams =
    new LabeledTextField("Second pass relaxation parameters: ");
  private LabeledTextField ltfMeanResidChangeLimits =
    new LabeledTextField("Residual change limits: ");
  private LabeledTextField ltfDeletionParams =
    new LabeledTextField("Deletion residual parameters: ");

  private JButton buttonTrack =
    new JButton("<html><b>Track fiducial<br>seed model</b>");

  /**
   * Construct a new beadtrack panel.
   * @param label specifies the suffix for the logfile
   */
  public BeadtrackPanel(String suffix) {
    logSuffix = suffix;
    setToolTipText();

    buttonTrack.setAlignmentX(0.5F);
    buttonTrack.setPreferredSize(FixedDim.button2Line);
    buttonTrack.setMaximumSize(FixedDim.button2Line);

    panelBeadtrack.setLayout(new BoxLayout(panelBeadtrack, BoxLayout.Y_AXIS));

    panelBeadtrack.add(ltfInputImage.getContainer());
    panelBeadtrack.add(ltfPiceListFile.getContainer());
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
    chkboxFillGaps.setAlignmentX(1.0f);
    panelBeadtrack.add(chkboxFillGaps);
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
    panelBeadtrack.add(buttonTrack);

    //  Mouse adapter for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    panelBeadtrack.addMouseListener(mouseAdapter);

  }

  public void setParameters(ConstBeadtrackParam beadtrackParams) {
    ltfInputImage.setText(beadtrackParams.getInputFile());
    ltfPiceListFile.setText(beadtrackParams.getPieceListFile());
    ltfSeedModelFile.setText(beadtrackParams.getSeedModelFile());
    ltfOutputModelFile.setText(beadtrackParams.getOutputModelFile());
    ltfViewSkipList.setText(beadtrackParams.getViewSkipList());
    ltfAdditionalViewSets.setText(beadtrackParams.getAdditionalViewGroups());
    ltfTiltAngleGroupSize.setText(beadtrackParams.getTiltAngleGroupSize());
    ltfTiltAngleGroups.setText(beadtrackParams.getTiltAngleGroups());
    ltfMagnificationGroupSize.setText(
      beadtrackParams.getMagnificationGroupSize());
    ltfMagnificationGroups.setText(beadtrackParams.getMagnificationGroups());
    ltfNMinViews.setText(beadtrackParams.getNMinViews());
    ltfFiducialParams.setText(beadtrackParams.getFiducialParams());
    chkboxFillGaps.setSelected(beadtrackParams.getFillGaps());
    ltfMaxGap.setText(beadtrackParams.getMaxGap());
    ltfTiltAngleMinRange.setText(beadtrackParams.getTiltAngleMinRange());
    ltfSearchBoxPixels.setText(beadtrackParams.getSearchBoxPixels());
    ltfMaxFiducialsAvg.setText(beadtrackParams.getMaxFiducialsAvg());
    ltfFiducialExtrapolationParams.setText(
      beadtrackParams.getFiducialExtrapolationParams());
    ltfRescueAttemptParams.setText(beadtrackParams.getRescueAttemptParams());
    ltfMinRescueDistance.setText(beadtrackParams.getMinRescueDistance());
    ltfRescueRelaxtionParams.setText(
      beadtrackParams.getRescueRelaxationParams());
    ltfResidualDistanceLimit.setText(
      beadtrackParams.getResidualDistanceLimit());
    ltfSecondPassParams.setText(beadtrackParams.getSecondPassParams());
    ltfMeanResidChangeLimits.setText(
      beadtrackParams.getMeanResidChangeLimits());
    ltfDeletionParams.setText(beadtrackParams.getDeletionParams());
  }

  public void getParameters(BeadtrackParam beadtrackParams)
    throws FortranInputSyntaxException {
    String badParameter = "";
    try {
      beadtrackParams.setInputFile(ltfInputImage.getText());
      beadtrackParams.setPieceListFile(ltfPiceListFile.getText());
      beadtrackParams.setSeedModelFile(ltfSeedModelFile.getText());
      beadtrackParams.setOutputModelFile(ltfOutputModelFile.getText());

      badParameter = ltfViewSkipList.getLabel();
      beadtrackParams.setViewSkipList(ltfViewSkipList.getText());

      badParameter = ltfAdditionalViewSets.getLabel();
      beadtrackParams.setAdditionalViewGroups(ltfAdditionalViewSets.getText());

      badParameter = ltfTiltAngleGroupSize.getLabel();
      beadtrackParams.setTiltAngleGroupSize(
        Integer.parseInt(ltfTiltAngleGroupSize.getText()));

      badParameter = ltfTiltAngleGroups.getLabel();
      beadtrackParams.setTiltAngleGroups(ltfTiltAngleGroups.getText());

      badParameter = ltfMagnificationGroupSize.getLabel();
      beadtrackParams.setMagnificationGroupSize(
        Integer.parseInt(ltfMagnificationGroupSize.getText()));

      badParameter = ltfMagnificationGroups.getLabel();
      beadtrackParams.setMagnificationGroups(ltfMagnificationGroups.getText());

      badParameter = ltfNMinViews.getLabel();
      beadtrackParams.setNMinViews(Integer.parseInt(ltfNMinViews.getText()));

      badParameter = ltfFiducialParams.getLabel();
      beadtrackParams.setFiducialParams(ltfFiducialParams.getText());

      beadtrackParams.setFillGaps(chkboxFillGaps.isSelected());

      badParameter = ltfMaxGap.getLabel();
      beadtrackParams.setMaxGap(Integer.parseInt(ltfMaxGap.getText()));

      badParameter = ltfTiltAngleMinRange.getLabel();
      beadtrackParams.setTiltAngleMinRange(ltfTiltAngleMinRange.getText());

      badParameter = ltfSearchBoxPixels.getLabel();
      beadtrackParams.setSearchBoxPixels(ltfSearchBoxPixels.getText());

      badParameter = ltfMaxFiducialsAvg.getLabel();
      beadtrackParams.setMaxFiducialsAvg(
        Integer.parseInt(ltfMaxFiducialsAvg.getText()));

      badParameter = ltfFiducialExtrapolationParams.getLabel();
      beadtrackParams.setFiducialExtrapolationParams(
        ltfFiducialExtrapolationParams.getText());

      badParameter = ltfRescueAttemptParams.getLabel();
      beadtrackParams.setRescueAttemptParams(ltfRescueAttemptParams.getText());

      badParameter = ltfMinRescueDistance.getLabel();
      beadtrackParams.setMinRescueDistance(
        Integer.parseInt(ltfMinRescueDistance.getText()));

      badParameter = ltfRescueRelaxtionParams.getLabel();
      beadtrackParams.setRescueRelaxationParams(
        ltfRescueRelaxtionParams.getText());

      badParameter = ltfResidualDistanceLimit.getLabel();
      beadtrackParams.setResidualDistanceLimit(
        Double.parseDouble(ltfResidualDistanceLimit.getText()));

      badParameter = ltfSecondPassParams.getLabel();
      beadtrackParams.setSecondPassParams(ltfSecondPassParams.getText());

      badParameter = ltfMeanResidChangeLimits.getLabel();
      beadtrackParams.setMeanResidChangeLimits(
        ltfMeanResidChangeLimits.getText());

      badParameter = ltfDeletionParams.getLabel();
      beadtrackParams.setDeletionParams(ltfDeletionParams.getText());
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
    ltfPiceListFile.setVisible(state);
    ltfSeedModelFile.setVisible(state);
    ltfOutputModelFile.setVisible(state);
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

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "beadtrack" };
    String[] manPage = { "beadtrack.html" };
    String[] logFileLabel = { "track" };
    String[] logFile = new String[1];
    logFile[0] = "track" + logSuffix + ".log";
//    ContextPopup contextPopup =
      new ContextPopup(
        panelBeadtrack,
        mouseEvent,
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  public void setButtonTrackActionListener(ActionListener actionAdapter) {
    buttonTrack.addActionListener(actionAdapter);
  }

  //
  //  ToolTip string setup
  //
  private void setToolTipText() {
    String line1, line2, line3, line4, line5, line6, line7;
    line1 = "<html>New text<br>";
    line2 = "New text<br>";
    line3 = "New text.";
    ltfInputImage.setToolTipText(line1 + line2 + line3);

  }
}
