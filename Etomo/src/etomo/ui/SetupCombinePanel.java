package etomo.ui;

import etomo.type.FiducialMatch;
import etomo.type.CombinePatchSize;
import etomo.ApplicationManager;
import etomo.comscript.ConstCombineParams;
import etomo.comscript.CombineParams;
import etomo.type.AxisID;

import java.awt.Component;
import java.awt.Container;
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
 * <p> Revision 2.1  2003/01/29 20:42:55  rickg
 * <p> Swtiched checkbox to jcheckbox
 * <p>
 * <p> Revision 2.0  2003/01/24 20:30:31  rickg
 * <p> Single window merge to main branch
 * <p>
 * <p> Revision 1.6.2.1  2003/01/24 18:43:37  rickg
 * <p> Single window GUI layout initial revision
 * <p>
 * <p> Revision 1.6  2003/01/08 21:40:46  rickg
 * <p> Added transferfid.log to the context menu
 * <p>
 * <p> Revision 1.5  2002/11/14 21:18:37  rickg
 * <p> Added anchors into the tomoguide
 * <p>
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

  private ApplicationManager applicationManager;

  private JPanel contentPane = new JPanel();
  private BeveledBorder brdrContent =
    new BeveledBorder("Combination parameters");

  private JPanel panelToSelector = new JPanel();
  private ButtonGroup bgToSelector = new ButtonGroup();
  private JRadioButton rbBtoA = new JRadioButton("Match the B tomogram to A");
  private JRadioButton rbAtoB =
    new JRadioButton("Match the A tomogram to B                                               ");

  private JPanel panelFiducialParams = new JPanel();
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
  private JRadioButton rbSmallPatch = new JRadioButton("Small patches");
  private JRadioButton rbMediumPatch = new JRadioButton("Medium patches");
  private JRadioButton rbLargePatch = new JRadioButton("Large patches");
  private ButtonGroup bgPatchSize = new ButtonGroup();
  private JCheckBox cbPatchRegionModel =
    new JCheckBox("Use patch region model file");

  private JPanel panelPatchRegion = new JPanel();
  private LabeledTextField ltfXMin = new LabeledTextField("X axis min: ");
  private LabeledTextField ltfXMax = new LabeledTextField("X axis max: ");
  private LabeledTextField ltfYMin = new LabeledTextField("Y axis min: ");
  private LabeledTextField ltfYMax = new LabeledTextField("Y axis max: ");
  private LabeledTextField ltfZMin = new LabeledTextField("Z axis min: ");
  private LabeledTextField ltfZMax = new LabeledTextField("Z axis max: ");

  private JPanel panelTempDirectory = new JPanel();
  private LabeledTextField ltfTempDirectory =
    new LabeledTextField("Temporary directory: ");
  private JCheckBox chkManualCleanup = new JCheckBox("Manual cleanup");

  private JPanel panelButton = new JPanel();
  private JToggleButton buttonImodVolumeA = new JToggleButton("Imod volume A");
  private JToggleButton buttonImodVolumeB = new JToggleButton("Imod volume B");
  private JButton buttonCreate = new JButton("Create combine script");

  /**
   * Default constructor
   */
  public SetupCombinePanel(ApplicationManager appMgr) {

    applicationManager = appMgr;

    //  Create the matching direction selector panel
    rbAtoB.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbBtoA.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgToSelector.add(rbAtoB);
    bgToSelector.add(rbBtoA);
    panelToSelector.setBorder(
      new EtchedBorder("Tomogram matching relationship").getBorder());
    panelToSelector.setLayout(new BoxLayout(panelToSelector, BoxLayout.Y_AXIS));
    panelToSelector.add(rbBtoA);
    panelToSelector.add(rbAtoB);

    //  Create the fiducial relationship panel
    rbBothSides.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbOneSide.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbOneSideInverted.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbUseModel.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgFiducialParams.add(rbBothSides);
    bgFiducialParams.add(rbOneSide);
    bgFiducialParams.add(rbOneSideInverted);
    bgFiducialParams.add(rbUseModel);
    panelFiducialParams.setBorder(
      new EtchedBorder("Volume alignment method").getBorder());
    panelFiducialParams.setLayout(
      new BoxLayout(panelFiducialParams, BoxLayout.Y_AXIS));
    panelFiducialParams.add(rbBothSides);
    panelFiducialParams.add(rbOneSide);
    panelFiducialParams.add(rbOneSideInverted);
    panelFiducialParams.add(rbUseModel);
    panelFiducialParams.add(Box.createRigidArea(FixedDim.x0_y10));
    panelFiducialParams.add(ltfFiducialMatchListA.getContainer());
    panelFiducialParams.add(Box.createRigidArea(FixedDim.x0_y5));
    panelFiducialParams.add(ltfFiducialMatchListB.getContainer());

    //  Create the patch parmeters panel
    rbSmallPatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbMediumPatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbLargePatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgPatchSize.add(rbSmallPatch);
    bgPatchSize.add(rbMediumPatch);
    bgPatchSize.add(rbLargePatch);
    panelPatchParams.setBorder(
      new EtchedBorder("Patch Parameters").getBorder());
    panelPatchParams.setLayout(
      new BoxLayout(panelPatchParams, BoxLayout.Y_AXIS));
    panelPatchParams.add(rbSmallPatch);
    panelPatchParams.add(rbMediumPatch);
    panelPatchParams.add(rbLargePatch);
    panelPatchParams.add(cbPatchRegionModel);

    panelPatchRegion.setLayout(new GridLayout(2, 3, 10, 10));
    panelPatchRegion.add(ltfXMin.getContainer());
    panelPatchRegion.add(ltfYMin.getContainer());
    panelPatchRegion.add(ltfZMin.getContainer());
    panelPatchRegion.add(ltfXMax.getContainer());
    panelPatchRegion.add(ltfYMax.getContainer());
    panelPatchRegion.add(ltfZMax.getContainer());
    panelPatchParams.add(Box.createRigidArea(FixedDim.x0_y10));
    panelPatchParams.add(panelPatchRegion);

    //  Create the temporary storage panel
    panelTempDirectory.setBorder(
      new EtchedBorder("Intermediate Data Storate").getBorder());
    panelTempDirectory.setLayout(
      new BoxLayout(panelTempDirectory, BoxLayout.Y_AXIS));
    panelTempDirectory.add(ltfTempDirectory.getContainer());
    panelTempDirectory.add(chkManualCleanup);

    //  Bind the buttons to the action listener
    SetupCombineActionListener actionListener =
      new SetupCombineActionListener(this);
    buttonImodVolumeA.addActionListener(actionListener);
    buttonImodVolumeB.addActionListener(actionListener);
    buttonCreate.addActionListener(actionListener);

    //  Button panel
    panelButton.setLayout(new GridLayout(1, 2, 30, 10));
    panelButton.add(buttonImodVolumeA);
    panelButton.add(buttonImodVolumeB);
    panelButton.add(buttonCreate);

    panelToSelector.setAlignmentX(Component.CENTER_ALIGNMENT);
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
    contentPane.add(Box.createVerticalGlue());
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(Box.createRigidArea(FixedDim.x0_y10));
    contentPane.add(panelButton);

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

    cbPatchRegionModel.setSelected(combineParams.usePatchRegionModel());

    ltfXMin.setText(combineParams.getPatchXMin());
    ltfXMax.setText(combineParams.getPatchXMax());
    ltfYMin.setText(combineParams.getPatchYMin());
    ltfYMax.setText(combineParams.getPatchYMax());
    ltfZMin.setText(combineParams.getPatchZMin());
    ltfZMax.setText(combineParams.getPatchZMax());

    ltfTempDirectory.setText(combineParams.getTempDirectory());

    chkManualCleanup.setSelected(combineParams.getManualCleanup());
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

      if (cbPatchRegionModel.isSelected()) {
        combineParams.setDefaultPatchRegionModel();
      }

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

      combineParams.setManualCleanup(chkManualCleanup.isSelected());
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }

  }

  //  Action functions for setup panel buttons
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command.equals(buttonImodVolumeA.getActionCommand())) {
      applicationManager.imodTomogram(AxisID.FIRST);
    }
    if (command.equals(buttonImodVolumeB.getActionCommand())) {
      applicationManager.imodTomogram(AxisID.SECOND);
    }
    if (command.equals(buttonCreate.getActionCommand())) {
      applicationManager.createCombineScripts();
    }
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "matchorwarp" };
    String[] manPage = { "matchorwarp.html" };
    String[] logFileLabel = { "transferfid" };
    String[] logFile = { "transferfid.log" };
    ContextPopup contextPopup =
      new ContextPopup(
        contentPane,
        mouseEvent,
        "COMBINING TWO TOMOGRAMS",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }
}

//  Button action listener
class SetupCombineActionListener implements ActionListener {

  SetupCombinePanel adaptee;
  public SetupCombineActionListener(SetupCombinePanel adaptee) {
    this.adaptee = adaptee;
  }

  public void actionPerformed(ActionEvent event) {
    adaptee.buttonAction(event);
  }
}
