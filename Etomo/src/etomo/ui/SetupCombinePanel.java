package etomo.ui;

import etomo.type.FiducialMatch;
import etomo.type.CombinePatchSize;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.CombineParams;

import java.awt.Checkbox;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.*;
import javax.swing.*;

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
 * <p> Revision 1.4  2002/11/14 04:41:11  rickg
 * <p> Fined matchorwarp man page entry
 * <p>
 * <p> Revision 1.3  2002/10/09 00:06:28  rickg
 * <p> Added getting and setting of patch boundary parameters
 * <p>
 * <p> Revision 1.2  2002/10/07 22:31:18  rickg
 * <p> removed unused imports
 * <p> reformat after emacs trashed it
 * <p>
 * <p> Revision 1.1  2002/09/09 22:57:02  rickg
 * <p> Initial CVS entry, basic functionality not including combining
 * <p> </p>
 */
public class SetupCombinePanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private JPanel contentPane = new JPanel();
  private BeveledBorder brdrContent =
    new BeveledBorder("Combination parameters");

  private JPanel panelToSelector = new JPanel();
  private ButtonGroup bgToSelector = new ButtonGroup();
  private BeveledBorder brdrToSelector =
    new BeveledBorder("Tomogram matching relationship");
  private JRadioButton rbBtoA = new JRadioButton("Match the B tomogram to A");
  private JRadioButton rbAtoB =
    new JRadioButton("Match the A tomogram to B                                               ");

  private JPanel panelFiducialParams = new JPanel();
  private BeveledBorder brdrFiducialParams =
    new BeveledBorder("Volume alignment method");
  private ButtonGroup bgFiducialParams = new ButtonGroup();
  private JRadioButton rbBothSides =
    new JRadioButton("Fiducials on both sides");
  private JRadioButton rbOneSide =
    new JRadioButton("Fiducials on one side, NOT inverted");
  private JRadioButton rbOneSideInverted =
    new JRadioButton("Fiducials on one side, inverted");
  private JRadioButton rbUseModel =
    new JRadioButton("Models of corresponding points");

  private LabeledTextField ltfFiducialMatchListA =
    new LabeledTextField("Corresponding fiducial list A: ");
  private LabeledTextField ltfFiducialMatchListB =
    new LabeledTextField("Corresponding fiducial list B: ");

  private JPanel panelPatchParams = new JPanel();
  private BeveledBorder brdrPatchParams = new BeveledBorder("Patch parameters");
  private JRadioButton rbSmallPatch = new JRadioButton("Small patches");
  private JRadioButton rbMediumPatch = new JRadioButton("Medium patches");
  private JRadioButton rbLargePatch = new JRadioButton("Large patches");
  private ButtonGroup bgPatchSize = new ButtonGroup();
  private LabeledTextField ltfPatchRegionModel =
    new LabeledTextField("Patch region model file: ");

  private JPanel panelRunIMOD = new JPanel();
  private JToggleButton buttonImodVolumeA = new JToggleButton("Imod volume A");
  private JToggleButton buttonImodVolumeB = new JToggleButton("Imod volume B");

  private JPanel panelPatchRegion = new JPanel();
  private LabeledTextField ltfXMin = new LabeledTextField("X axis min: ");
  private LabeledTextField ltfXMax = new LabeledTextField("X axis max: ");
  private LabeledTextField ltfYMin = new LabeledTextField("Y axis min: ");
  private LabeledTextField ltfYMax = new LabeledTextField("Y axis max: ");
  private LabeledTextField ltfZMin = new LabeledTextField("Z axis min: ");
  private LabeledTextField ltfZMax = new LabeledTextField("Z axis max: ");

  private JPanel panelTempDirectory = new JPanel();
  private BeveledBorder brdrIntermediateStorage =
    new BeveledBorder("Intermediate data storage");
  private LabeledTextField ltfTempDirectory =
    new LabeledTextField("Temporary directory: ");
  private Checkbox chkManualCleanup = new Checkbox("Manual cleanup");

  private JButton buttonCreate = new JButton("Create combine script");

  public SetupCombinePanel() {

    //  Create the matching direction selector panel
    rbAtoB.setAlignmentX(0.0f);
    rbBtoA.setAlignmentX(0.0f);
    bgToSelector.add(rbAtoB);
    bgToSelector.add(rbBtoA);
    panelToSelector.setBorder(brdrToSelector.getBorder());
    panelToSelector.setLayout(new BoxLayout(panelToSelector, BoxLayout.Y_AXIS));
    panelToSelector.add(rbBtoA);
    panelToSelector.add(rbAtoB);

    //  Create the fiducial relationship panel
    rbBothSides.setAlignmentX(0.0f);
    rbOneSide.setAlignmentX(0.0f);
    rbOneSideInverted.setAlignmentX(0.0f);
    rbUseModel.setAlignmentX(0.0f);
    bgFiducialParams.add(rbBothSides);
    bgFiducialParams.add(rbOneSide);
    bgFiducialParams.add(rbOneSideInverted);
    bgFiducialParams.add(rbUseModel);
    panelFiducialParams.setBorder(brdrFiducialParams.getBorder());
    panelFiducialParams.setLayout(
      new BoxLayout(panelFiducialParams, BoxLayout.Y_AXIS));
    panelFiducialParams.add(rbBothSides);
    panelFiducialParams.add(rbOneSide);
    panelFiducialParams.add(rbOneSideInverted);
    panelFiducialParams.add(rbUseModel);
    panelFiducialParams.add(Box.createRigidArea(FixedDim.x0_y10));
    panelFiducialParams.add(ltfFiducialMatchListA.getContainer());
    panelFiducialParams.add(ltfFiducialMatchListB.getContainer());

    //  Create the patch parmeters panel
    rbSmallPatch.setAlignmentX(0.0f);
    rbMediumPatch.setAlignmentX(0.0f);
    rbLargePatch.setAlignmentX(0.0f);
    bgPatchSize.add(rbSmallPatch);
    bgPatchSize.add(rbMediumPatch);
    bgPatchSize.add(rbLargePatch);
    panelPatchParams.setBorder(brdrPatchParams.getBorder());
    panelPatchParams.setLayout(
      new BoxLayout(panelPatchParams, BoxLayout.Y_AXIS));
    panelPatchParams.add(rbSmallPatch);
    panelPatchParams.add(rbMediumPatch);
    panelPatchParams.add(rbLargePatch);
    panelPatchParams.add(ltfPatchRegionModel.getContainer());

    panelRunIMOD.setLayout(new GridLayout(1, 2, 30, 10));
    panelRunIMOD.add(buttonImodVolumeA);
    panelRunIMOD.add(buttonImodVolumeB);
    panelPatchParams.add(panelRunIMOD);

    panelPatchRegion.setLayout(new GridLayout(2, 3, 10, 10));
    panelPatchRegion.add(ltfXMin.getContainer());
    panelPatchRegion.add(ltfYMin.getContainer());
    panelPatchRegion.add(ltfZMin.getContainer());
    panelPatchRegion.add(ltfXMax.getContainer());
    panelPatchRegion.add(ltfYMax.getContainer());
    panelPatchRegion.add(ltfZMax.getContainer());
    panelPatchParams.add(panelPatchRegion);

    //  Create the temporary storage panel
    panelTempDirectory.setBorder(brdrIntermediateStorage.getBorder());
    panelTempDirectory.setLayout(
      new BoxLayout(panelTempDirectory, BoxLayout.Y_AXIS));
    panelTempDirectory.add(ltfTempDirectory.getContainer());
    panelTempDirectory.add(chkManualCleanup);

    panelToSelector.setAlignmentX(0.0f);
    contentPane.setLayout(new BoxLayout(contentPane, BoxLayout.Y_AXIS));
    contentPane.setBorder(brdrContent.getBorder());
    contentPane.add(panelToSelector);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelFiducialParams);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelPatchParams);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelTempDirectory);
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(buttonCreate);
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    contentPane.addMouseListener(mouseAdapter);

  }

  public Container getContainer() {
    return contentPane;
  }

  public void setParameters(ConstCombineParams combineParams) {

    if (combineParams.getMatchBtoA()) {
      rbBtoA.setSelected(true);
    }
    else {
      rbAtoB.setSelected(true);
    }

    if (combineParams.getFiducialMatch() == FiducialMatch.BOTH_SIDES) {
      rbBothSides.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE) {
      rbOneSide.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.ONE_SIDE_INVERTED) {
      rbOneSideInverted.setSelected(true);
    }
    if (combineParams.getFiducialMatch() == FiducialMatch.USE_MODEL) {
      rbUseModel.setSelected(true);
    }
    ltfFiducialMatchListA.setText(combineParams.getFiducialMatchListA());
    ltfFiducialMatchListB.setText(combineParams.getFiducialMatchListB());

    if (combineParams.getPatchSize() == CombinePatchSize.SMALL) {
      rbSmallPatch.setSelected(true);
    }
    if (combineParams.getPatchSize() == CombinePatchSize.MEDIUM) {
      rbMediumPatch.setSelected(true);
    }
    if (combineParams.getPatchSize() == CombinePatchSize.LARGE) {
      rbLargePatch.setSelected(true);
    }

    ltfPatchRegionModel.setText(combineParams.getPatchRegionModel());

    ltfXMin.setText(combineParams.getPatchXMin());
    ltfXMax.setText(combineParams.getPatchXMax());
    ltfYMin.setText(combineParams.getPatchYMin());
    ltfYMax.setText(combineParams.getPatchYMax());
    ltfZMin.setText(combineParams.getPatchZMin());
    ltfZMax.setText(combineParams.getPatchZMax());

    ltfTempDirectory.setText(combineParams.getTempDirectory());

    chkManualCleanup.setState(combineParams.getManualCleanup());
  }

  public void getParameters(CombineParams combineParams)
    throws NumberFormatException {
    String badParameter = "uknown";
    try {

      combineParams.setMatchBtoA(rbBtoA.isSelected());

      if (rbBothSides.isSelected()) {
        combineParams.setFiducialMatch(FiducialMatch.BOTH_SIDES);
      }
      if (rbOneSide.isSelected()) {
        combineParams.setFiducialMatch(FiducialMatch.ONE_SIDE);
      }
      if (rbOneSideInverted.isSelected()) {
        combineParams.setFiducialMatch(FiducialMatch.ONE_SIDE_INVERTED);
      }
      if (rbUseModel.isSelected()) {
        combineParams.setFiducialMatch(FiducialMatch.USE_MODEL);
      }

      combineParams.setFiducialMatchListA(ltfFiducialMatchListA.getText());
      combineParams.setFiducialMatchListB(ltfFiducialMatchListB.getText());

      if (rbSmallPatch.isSelected()) {
        combineParams.setPatchSize(CombinePatchSize.SMALL);
      }
      if (rbMediumPatch.isSelected()) {
        combineParams.setPatchSize(CombinePatchSize.MEDIUM);
      }
      if (rbLargePatch.isSelected()) {
        combineParams.setPatchSize(CombinePatchSize.LARGE);
      }

      combineParams.setPatchRegionModel(ltfPatchRegionModel.getText());
      badParameter = ltfXMin.getLabel();
      combineParams.setPatchXMin(Integer.parseInt(ltfXMin.getText()));
      badParameter = ltfXMax.getLabel();
      combineParams.setPatchXMax(Integer.parseInt(ltfXMax.getText()));
      badParameter = ltfYMin.getLabel();
      combineParams.setPatchYMin(Integer.parseInt(ltfYMin.getText()));
      badParameter = ltfYMax.getLabel();
      combineParams.setPatchYMax(Integer.parseInt(ltfYMax.getText()));
      badParameter = ltfZMin.getLabel();
      combineParams.setPatchZMin(Integer.parseInt(ltfZMin.getText()));
      badParameter = ltfZMax.getLabel();
      combineParams.setPatchZMax(Integer.parseInt(ltfZMax.getText()));
      badParameter = "unknown";

      combineParams.setTempDirectory(ltfTempDirectory.getText());

      combineParams.setManualCleanup(chkManualCleanup.getState());
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }

  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "matchorwarp" };
    String[] manPage = { "matchorwarp.html" };
    ContextPopup contextPopup =
      new ContextPopup(
        contentPane,
        mouseEvent,
        "COMBINING TWO TOMOGRAMS",
        manPagelabel,
        manPage);
  }

  /**
   * Add an action listener for the Imod Volume A button
   */
  void addImodAActionListener(ActionListener actionListener) {
    buttonImodVolumeA.addActionListener(actionListener);
  }

  /**
   * Add an action listener for the Imod Volume B button
   */
  void addImodBActionListener(ActionListener actionListener) {
    buttonImodVolumeB.addActionListener(actionListener);
  }

  /**
   * Add an action listener to the create button
   */
  void addCreateActionListener(ActionListener actionListener) {
    buttonCreate.addActionListener(actionListener);
  }

  public void sizePanels() {
    System.out.println(panelToSelector.getSize());
    System.out.println(panelFiducialParams.getSize());
    System.out.println(panelPatchParams.getSize());
    // Find the current maximum panel width and resize the relationship panel
    // to that width
    Dimension maxSize = panelFiducialParams.getSize();
    Dimension toSelectorSize = panelToSelector.getSize();
    toSelectorSize.width = (int) maxSize.width;
    panelToSelector.setPreferredSize(toSelectorSize);
    panelToSelector.setSize(toSelectorSize);
    System.out.println(panelToSelector.getSize());
  }
}
