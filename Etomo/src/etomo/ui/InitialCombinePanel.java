package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
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
  private JButton btnImodMatchModels =
    new JButton("<html><b>Create matching models in 3dmod</b>");
  private JCheckBox cbUseModel = new JCheckBox("Model based initial match");

  private JPanel pnlButton = new JPanel();
  private JButton btnMatchcheck =
    new JButton("<html><b>View matchshift results</b>");
  private JButton btnRestart = new JButton("<html><b>Restart combine</b>");

  /**
   * Default constructor
   * @param appMgr
   */
  public InitialCombinePanel(ApplicationManager appMgr) {

    applicationManager = appMgr;

    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));
    //  Get the current text height from one of the 
    double height = ltfFiducialMatchListA.getLabelPreferredSize().getHeight();

    //  Set the button sizes
    Dimension dimButton = new Dimension();
    dimButton.setSize(10 * height, 3 * height);
    btnImodMatchModels.setPreferredSize(dimButton);
    btnImodMatchModels.setMaximumSize(dimButton);
    btnMatchcheck.setPreferredSize(dimButton);
    btnMatchcheck.setMaximumSize(dimButton);
    btnRestart.setPreferredSize(dimButton);
    btnRestart.setMaximumSize(dimButton);

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

    //  Layout the button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnMatchcheck);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnRestart);
    pnlButton.add(Box.createHorizontalGlue());

    pnlRoot.add(pnlSolvematch);
    pnlRoot.add(Box.createVerticalGlue());
    pnlRoot.add(pnlButton);

    //  Bind the UI objects to their ActionListeners
    ButtonActionListener buttonAction = new ButtonActionListener(this);
    btnImodMatchModels.addActionListener(buttonAction);
    btnRestart.addActionListener(buttonAction);
    btnMatchcheck.addActionListener(buttonAction);
    CheckBoxActionListener checkboxAction = new CheckBoxActionListener(this);
    cbUseModel.addActionListener(checkboxAction);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);
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
   * Right mouse btn context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "solvematch", "matchshifts" };
    String[] manPage = { "solvematch.html", "matchshifts.html" };
    String[] logFileLabel =
      { "transferfid", "solvematchshift", "solvematchmod" };
    String[] logFile =
      { "transferfid.log", "solvematchshift.log", "solvematchmod.log" };

    ContextPopup contextPopup =
      new ContextPopup(
        pnlRoot,
        mouseEvent,
        "Initial Problems in Combinging",
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
}
