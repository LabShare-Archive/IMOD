package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstSolvematchmodParam;
import etomo.comscript.ConstSolvematchshiftParam;
import etomo.comscript.SolvematchmodParam;
import etomo.comscript.SolvematchshiftParam;

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
 * <p> Revision 3.0  2003/11/07 23:19:01  rickg
 * <p> Version 1.0.0
 * <p>
 * <p> Revision 1.16  2003/11/05 19:56:58  rickg
 * <p> Bug# 300 Selecting matching models on setup patch now
 * <p> selects matching models on initial page
 * <p>
 * <p> Revision 1.15  2003/10/29 17:23:02  rickg
 * <p> Bug# 301 Tooltips
 * <p>
 * <p> Revision 1.14  2003/10/21 23:43:42  rickg
 * <p> Changed imod buttons to non multiline
 * <p>
 * <p> Revision 1.13  2003/10/20 20:25:59  rickg
 * <p> Bug# 228 added Restart at matchvol1 button
 * <p>
 * <p> Revision 1.12  2003/10/15 22:46:41  rickg
 * <p> Button size change
 * <p> Label changes
 * <p>
 * <p> Revision 1.11  2003/10/15 19:15:52  rickg
 * <p> Bug# 299 Moved buttons up
 * <p>
 * <p> Revision 1.10  2003/06/05 04:42:37  rickg
 * <p> Label change for view match shift results
 * <p>
 * <p> Revision 1.9  2003/04/28 23:25:25  rickg
 * <p> Changed visible imod references to 3dmod
 * <p>
 * <p> Revision 1.8  2003/03/20 21:19:08  rickg
 * <p> Added matchshift results button/access
 * <p>
 * <p> Revision 1.7  2003/03/20 17:47:21  rickg
 * <p> Initial implementation of panel
 * <p>
 * <p> Revision 1.6  2003/03/18 23:41:07  rickg
 * <p> Restructured for both model and non model based combines
 * <p>
 * <p> </p>
 */
public class InitialCombinePanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;

  private JPanel pnlRoot = new JPanel();

  private JPanel pnlSolvematch = new JPanel();

  private LabeledTextField ltfFiducialMatchListA =
    new LabeledTextField("Corresponding fiducial list A: ");
  private LabeledTextField ltfFiducialMatchListB =
    new LabeledTextField("Corresponding fiducial list B: ");

  private LabeledTextField ltfResidulThreshold =
    new LabeledTextField("Residual Threshold: ");

  private JPanel pnlModelSelect = new JPanel();
  private JCheckBox cbUseModel =
    new JCheckBox("Use models of corresponding points, not cross-correlation");
  private MultiLineButton btnImodMatchModels =
    new MultiLineButton("<html><b>Create Matching Models in 3dmod</b>");

  private JPanel pnlButton = new JPanel();
  private MultiLineButton btnMatchcheck =
    new MultiLineButton("<html><b>View Match Check Volume</b>");
  private MultiLineButton btnRestart = new MultiLineButton("<html><b>Restart Combine</b>");
  private MultiLineToggleButton btnMatchvolRestart =
    new MultiLineToggleButton("<html><b>Restart at Matchvol1</b>");

  /**
   * Default constructor
   * @param appMgr
   */
  public InitialCombinePanel(ApplicationManager appMgr) {

    applicationManager = appMgr;

    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));

    //  Set the button sizes
    Dimension dimButton = UIParameters.getButtonDimension();
    btnImodMatchModels.setPreferredSize(dimButton);
    btnImodMatchModels.setMaximumSize(dimButton);
    btnMatchcheck.setPreferredSize(dimButton);
    btnMatchcheck.setMaximumSize(dimButton);
    btnRestart.setPreferredSize(dimButton);
    btnRestart.setMaximumSize(dimButton);
    btnMatchvolRestart.setPreferredSize(dimButton);
    btnMatchvolRestart.setMaximumSize(dimButton);

    pnlSolvematch.setLayout(new BoxLayout(pnlSolvematch, BoxLayout.Y_AXIS));
    pnlSolvematch.setBorder(
      new EtchedBorder("Solvematch Parameters").getBorder());

    pnlModelSelect.setLayout(new BoxLayout(pnlModelSelect, BoxLayout.X_AXIS));
    pnlModelSelect.add(cbUseModel);
    pnlModelSelect.add(btnImodMatchModels);
    pnlSolvematch.add(pnlModelSelect);
    pnlSolvematch.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSolvematch.add(ltfFiducialMatchListA.getContainer());
    pnlSolvematch.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSolvematch.add(ltfFiducialMatchListB.getContainer());
    pnlSolvematch.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSolvematch.add(ltfResidulThreshold.getContainer());
    pnlSolvematch.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlSolvematch.add(ltfResidulThreshold.getContainer());

    //  Layout the button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnMatchcheck);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnRestart);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnMatchvolRestart);
    pnlButton.add(Box.createHorizontalGlue());

    pnlRoot.add(pnlSolvematch);
    pnlRoot.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlRoot.add(pnlButton);
    pnlRoot.add(Box.createVerticalGlue());

    //  Bind the UI objects to their ActionListeners
    ButtonActionListener buttonAction = new ButtonActionListener(this);
    btnImodMatchModels.addActionListener(buttonAction);
    btnRestart.addActionListener(buttonAction);
    btnMatchvolRestart.addActionListener(buttonAction);
    btnMatchcheck.addActionListener(buttonAction);
    CheckBoxActionListener checkboxAction = new CheckBoxActionListener(this);
    cbUseModel.addActionListener(checkboxAction);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);
    setToolTipText();
  }

  public Container getContainer() {
    return pnlRoot;
  }

  public void setAdvanced(boolean state) {

  }

  /**
   * Set the solvematchshift parameters of the UI 
   * @param solvematchshiftParam
   */
  public void setSolvematchshiftParams(ConstSolvematchshiftParam solvematchshiftParam) {
    ltfFiducialMatchListA.setText(
      solvematchshiftParam.getFiducialMatchListA().toString());
    ltfFiducialMatchListB.setText(
      solvematchshiftParam.getFiducialMatchListB().toString());
    ltfResidulThreshold.setText(solvematchshiftParam.getResidualThreshold());
    cbUseModel.setSelected(false);
  }

  /**
   * Get the solvematchshift parameters from the UI
   * @param solvematchshiftParam
   */
  public void getSolvematchshiftParams(SolvematchshiftParam solvematchshiftParam) {
    solvematchshiftParam.setFiducialMatchListA(ltfFiducialMatchListA.getText());
    solvematchshiftParam.setFiducialMatchListB(ltfFiducialMatchListB.getText());
    solvematchshiftParam.setResidualThreshold(
      Double.parseDouble(ltfResidulThreshold.getText()));
  }

  /**
   * Set the solvematchmod parameters of the UI 
   * @param solvematchmodParam
   */
  public void setSolvematchmodParams(ConstSolvematchmodParam solvematchmodParam) {
    ltfFiducialMatchListA.setText(
      solvematchmodParam.getFiducialMatchListA().toString());
    ltfFiducialMatchListB.setText(
      solvematchmodParam.getFiducialMatchListB().toString());
    ltfResidulThreshold.setText(solvematchmodParam.getResidualThreshold());
    cbUseModel.setSelected(true);
  }

  /**
   * Get the solvematchmod parameters from the UI
   * @param solvematchmodParam
   */
  public void getSolvematchmodParams(SolvematchmodParam solvematchmodParam) {
    solvematchmodParam.setFiducialMatchListA(ltfFiducialMatchListA.getText());
    solvematchmodParam.setFiducialMatchListB(ltfFiducialMatchListB.getText());
    solvematchmodParam.setResidualThreshold(
      Double.parseDouble(ltfResidulThreshold.getText()));
  }

  /**
   * Set the state of the matching model checkbox.
   * @param state
   */
  void setMatchingModels(boolean state) {
    cbUseModel.setSelected(state);
  }

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "Solvematch", "Matchshifts" };
    String[] manPage = { "solvematch.html", "matchshifts.html" };
    String[] logFileLabel =
      { "Transferfid", "Solvematchshift", "Solvematchmod" };
    String[] logFile =
      { "transferfid.log", "solvematchshift.log", "solvematchmod.log" };

    ContextPopup contextPopup =
      new ContextPopup(
        pnlRoot,
        mouseEvent,
        "Initial Problems in Combining",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  /**
   * Respond to button actions
   * @param event
   */
  private void buttonAction(ActionEvent event) {
    if (event
      .getActionCommand()
      .equals(btnImodMatchModels.getActionCommand())) {
      applicationManager.imodMatchingModel();
    }

    if (event.getActionCommand().equals(btnMatchcheck.getActionCommand())) {
      applicationManager.imodMatchCheck();
    }

    if (event.getActionCommand().equals(btnRestart.getActionCommand())) {
      if (cbUseModel.isSelected()) {
        applicationManager.modelCombine();
      }
      else {
        applicationManager.combine();
      }
    }
    if (event
      .getActionCommand()
      .equals(btnMatchvolRestart.getActionCommand())) {
      applicationManager.matchvol1();
    }
  }

  /**
   * Respond to checkbox actions
   * @param event
   */
  private void checkboxAction(ActionEvent event) {
    if (event.getActionCommand().equals(cbUseModel.getActionCommand())) {
      if (cbUseModel.isSelected()) {
        applicationManager.loadSolvematchMod();
      }
      else {
        applicationManager.loadSolvematchShift();
      }
    }
  }

  /**
   * Button action listener inner class
   */
  class ButtonActionListener implements ActionListener {
    InitialCombinePanel listenee;

    ButtonActionListener(InitialCombinePanel initialCombinePanel) {
      listenee = initialCombinePanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.buttonAction(event);
    }
  }

  /**
   * Checkbox action listener inner class
   */
  class CheckBoxActionListener implements ActionListener {
    InitialCombinePanel listenee;

    CheckBoxActionListener(InitialCombinePanel initialCombinePanel) {
      listenee = initialCombinePanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.checkboxAction(event);
    }
  }

  /**
   * Initialize the tooltip text for the axis panel objects
   */
  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();

    text =
      "Use models of corresponding points instead of cross-correlation to find"
        + " the shifts between volumes.";
    cbUseModel.setToolTipText(tooltipFormatter.setText(text).format());

    text = "Open both volumes in 3dmod to make models of corresponding points.";
    btnImodMatchModels.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Enter the list of fiducials in A for which you know the corresponding "
        + "fiducial in B.  Use the point number in *fid.xyz, not the contour "
        + "number.";
    ltfFiducialMatchListA.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "Enter the list of fiducials in B that correspond to the ones in the "
        + "list entered for A.  Use the point number in *fid.xyz, not the "
        + "contour number.";
    ltfFiducialMatchListB.setToolTipText(
      tooltipFormatter.setText(text).format());

    text =
      "The highest allowed value for the maximum residual when fitting the "
        + "pairs of corresponding fiducial points to a linear transformation.";
    ltfResidulThreshold.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "View the two volumes that are used for assessing whether Matchshifts "
        + "found the correct shifts between the volumes.";
    btnMatchcheck.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Restart the combine operation from the beginning with the parameters "
        + "specified here.";
    btnRestart.setToolTipText(tooltipFormatter.setText(text).format());

    text =
      "Resume and make first matching volume, despite a small displacement "
        + "between the match check volumes";
    btnMatchvolRestart.setToolTipText(tooltipFormatter.setText(text).format());
  }
}
