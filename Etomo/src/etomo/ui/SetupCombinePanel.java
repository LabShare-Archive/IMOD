package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JToggleButton;

import etomo.ApplicationManager;
import etomo.comscript.CombineParams;
import etomo.comscript.ConstCombineParams;
import etomo.type.AxisID;
import etomo.type.CombinePatchSize;
import etomo.type.FiducialMatch;

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
 * <p> Revision 2.8  2003/09/25 23:29:28  rickg
 * <p> Bug #246 fixed to the appropriate combine method in the app manager
 * <p>
 * <p> Revision 2.7  2003/09/08 05:49:07  rickg
 * <p> Method name change for opening the complete volume
 * <p>
 * <p> Revision 2.6  2003/06/05 04:43:20  rickg
 * <p> Added create patch region model button
 * <p>
 * <p> Revision 2.5  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 2.4  2003/03/20 17:46:21  rickg
 * <p> Added right button context menu
 * <p>
 * <p> Revision 2.3  2003/03/18 00:32:33  rickg
 * <p> combine development in progress
 * <p>
 * <p> Revision 2.2  2003/02/24 23:49:36  rickg
 * <p> Panel layout for combination dialog
 * <p> Changed borders
 * <p>
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

  private JPanel pnlRoot = new JPanel();
  private BeveledBorder brdrContent =
    new BeveledBorder("Combination Parameters");

  private JPanel pnlToSelector = new JPanel();
  private ButtonGroup bgToSelector = new ButtonGroup();
  private JRadioButton rbBtoA = new JRadioButton("Match the B tomogram to A");
  private JRadioButton rbAtoB =
    new JRadioButton("Match the A tomogram to B                                               ");

  private JPanel pnlFiducialParams = new JPanel();
  private ButtonGroup bgFiducialParams = new ButtonGroup();
  private JRadioButton rbBothSides =
    new JRadioButton("Fiducials on both sides");
  private JRadioButton rbOneSide =
    new JRadioButton("Fiducials on one side, NOT inverted");
  private JRadioButton rbOneSideInverted =
    new JRadioButton("Fiducials on one side, inverted");
  private JRadioButton rbUseModel =
    new JRadioButton("Use models of corresponding points, not cross-correlation");

  private LabeledTextField ltfFiducialMatchListA =
    new LabeledTextField("Corresponding fiducial list A: ");
  private LabeledTextField ltfFiducialMatchListB =
    new LabeledTextField("Corresponding fiducial list B: ");

  private JPanel pnlPatchParams = new JPanel();
  private JRadioButton rbSmallPatch = new JRadioButton("Small patches");
  private JRadioButton rbMediumPatch = new JRadioButton("Medium patches");
  private JRadioButton rbLargePatch = new JRadioButton("Large patches");
  private ButtonGroup bgPatchSize = new ButtonGroup();
  private JPanel pnlPatchRegionModel = new JPanel();
  private JCheckBox cbPatchRegionModel =
    new JCheckBox("Use patch region model");
  private JButton btnPatchRegionModel =
    new JButton("<html><b>Create/Edit Patch Region Model</b>");

  private JPanel pnlPatchRegion = new JPanel();
  private LabeledTextField ltfXMin = new LabeledTextField("X axis min: ");
  private LabeledTextField ltfXMax = new LabeledTextField("X axis max: ");
  private LabeledTextField ltfYMin = new LabeledTextField("Y axis min: ");
  private LabeledTextField ltfYMax = new LabeledTextField("Y axis max: ");
  private LabeledTextField ltfZMin = new LabeledTextField("Z axis min: ");
  private LabeledTextField ltfZMax = new LabeledTextField("Z axis max: ");

  private JPanel pnlTemdDirectory = new JPanel();
  private LabeledTextField ltfTempDirectory =
    new LabeledTextField("Temporary directory: ");
  private JCheckBox chkManualCleanup = new JCheckBox("Manual cleanup");

  private JPanel pnlButton = new JPanel();
  private JButton btnImodVolumeA = new JButton("<html><b>3dmod Volume A</b>");
  private JButton btnImodVolumeB = new JButton("<html><b>3dmod Volume B</b>");
  private JToggleButton btnCreate =
    new JToggleButton("<html><b>Create Combine Scripts</b>");
  private JToggleButton btnCombine =
    new JToggleButton("<html><b>Start Combine</b>");

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
    pnlToSelector.setBorder(
      new EtchedBorder("Tomogram Matching Relationship").getBorder());
    pnlToSelector.setLayout(new BoxLayout(pnlToSelector, BoxLayout.Y_AXIS));
    pnlToSelector.add(rbBtoA);
    pnlToSelector.add(rbAtoB);

    //  Create the fiducial relationship panel
    rbBothSides.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbOneSide.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbOneSideInverted.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbUseModel.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgFiducialParams.add(rbBothSides);
    bgFiducialParams.add(rbOneSide);
    bgFiducialParams.add(rbOneSideInverted);
    bgFiducialParams.add(rbUseModel);
    pnlFiducialParams.setBorder(
      new EtchedBorder("Initial Volume Alignment Method").getBorder());
    pnlFiducialParams.setLayout(
      new BoxLayout(pnlFiducialParams, BoxLayout.Y_AXIS));
    pnlFiducialParams.add(rbBothSides);
    pnlFiducialParams.add(rbOneSide);
    pnlFiducialParams.add(rbOneSideInverted);
    pnlFiducialParams.add(rbUseModel);
    pnlFiducialParams.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlFiducialParams.add(ltfFiducialMatchListA.getContainer());
    pnlFiducialParams.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlFiducialParams.add(ltfFiducialMatchListB.getContainer());

    //  Create the patch parmeters panel
    rbSmallPatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbMediumPatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    rbLargePatch.setAlignmentX(Component.LEFT_ALIGNMENT);
    bgPatchSize.add(rbSmallPatch);
    bgPatchSize.add(rbMediumPatch);
    bgPatchSize.add(rbLargePatch);
    pnlPatchParams.setBorder(
      new EtchedBorder("Patch Parameters for Refining Alignment").getBorder());
    pnlPatchParams.setLayout(new BoxLayout(pnlPatchParams, BoxLayout.Y_AXIS));
    pnlPatchParams.add(rbSmallPatch);
    pnlPatchParams.add(rbMediumPatch);
    pnlPatchParams.add(rbLargePatch);

    pnlPatchRegionModel.setLayout(
      new BoxLayout(pnlPatchRegionModel, BoxLayout.X_AXIS));
    pnlPatchRegionModel.add(cbPatchRegionModel);
    pnlPatchRegionModel.add(btnPatchRegionModel);
    pnlPatchParams.add(pnlPatchRegionModel);

    pnlPatchRegion.setLayout(new GridLayout(2, 3, 10, 10));
    pnlPatchRegion.add(ltfXMin.getContainer());
    pnlPatchRegion.add(ltfYMin.getContainer());
    pnlPatchRegion.add(ltfZMin.getContainer());
    pnlPatchRegion.add(ltfXMax.getContainer());
    pnlPatchRegion.add(ltfYMax.getContainer());
    pnlPatchRegion.add(ltfZMax.getContainer());
    pnlPatchParams.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlPatchParams.add(pnlPatchRegion);

    //  Create the temporary storage panel
    pnlTemdDirectory.setBorder(
      new EtchedBorder("Intermediate Data Storage").getBorder());
    pnlTemdDirectory.setLayout(
      new BoxLayout(pnlTemdDirectory, BoxLayout.Y_AXIS));
    pnlTemdDirectory.add(ltfTempDirectory.getContainer());
    pnlTemdDirectory.add(chkManualCleanup);

    //  Bind the btns to the action listener
    SetupCombineActionListener actionListener =
      new SetupCombineActionListener(this);
    btnPatchRegionModel.addActionListener(actionListener);
    btnImodVolumeA.addActionListener(actionListener);
    btnImodVolumeB.addActionListener(actionListener);
    btnCreate.addActionListener(actionListener);
    btnCombine.addActionListener(actionListener);

    //  Get the current text height from one of the 
    double height = cbPatchRegionModel.getPreferredSize().getHeight();

    //  Set the button sizes
    Dimension dimButton = new Dimension();
    dimButton.setSize(8 * height, 2 * height);
    btnPatchRegionModel.setPreferredSize(dimButton);
    btnPatchRegionModel.setMaximumSize(dimButton);
    btnImodVolumeA.setSize(dimButton);
    btnImodVolumeA.setMaximumSize(dimButton);
    btnImodVolumeB.setSize(dimButton);
    btnImodVolumeB.setMaximumSize(dimButton);
    btnCreate.setSize(dimButton);
    btnCreate.setMaximumSize(dimButton);
    btnCombine.setSize(dimButton);
    btnCombine.setMaximumSize(dimButton);

    //  Button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodVolumeA);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodVolumeB);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnCreate);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnCombine);
    pnlButton.add(Box.createHorizontalGlue());

    pnlToSelector.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    pnlRoot.setBorder(brdrContent.getBorder());
    pnlRoot.add(pnlToSelector);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlFiducialParams);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlPatchParams);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlTemdDirectory);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(Box.createVerticalGlue());
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlButton);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);

  }

  public Container getContainer() {
    return pnlRoot;
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

  //  Action functions for setup panel btns
  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (event
      .getActionCommand()
      .equals(btnPatchRegionModel.getActionCommand())) {
      applicationManager.imodPatchRegionModel();
    }
    if (command.equals(btnImodVolumeA.getActionCommand())) {
      applicationManager.imodFullVolume(AxisID.FIRST);
    }
    if (command.equals(btnImodVolumeB.getActionCommand())) {
      applicationManager.imodFullVolume(AxisID.SECOND);
    }
    if (command.equals(btnCreate.getActionCommand())) {
      applicationManager.createCombineScripts();
    }
    if (command.equals(btnCombine.getActionCommand())) {
      if (rbUseModel.isSelected()) {
        applicationManager.modelCombine();
      }
      else {
        applicationManager.combine();
      }
    }

  }

  /**
   * Right mouse btn context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel =
      { "solvematch", "matchshifts", "patchcrawl3d", "matchorwarp" };
    String[] manPage =
      {
        "solvematch.html",
        "matchshifts.html",
        "patchcrawl3d.html",
        "matchorwarp.html" };
    String[] logFileLabel =
      {
        "transferfid",
        "solvematchshift",
        "solvematchmod",
        "patchcorr",
        "matchorwarp",
        "volcombine" };
    String[] logFile =
      {
        "transferfid.log",
        "solvematchshift.log",
        "solvematchmod.log",
        "patchcorr.log",
        "matchorwarp.log",
        "volcombine.log" };

    ContextPopup contextPopup =
      new ContextPopup(
        pnlRoot,
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
