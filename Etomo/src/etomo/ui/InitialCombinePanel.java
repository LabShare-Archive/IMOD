package etomo.ui;

import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;

import etomo.ApplicationManager;
import etomo.comscript.ConstSolvematchshiftParam;
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
 * <p> $Log:
 * <p> </p>
 */
public class InitialCombinePanel {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;

  private JPanel rootPanel = new JPanel();

  private JPanel panelSolvematchshift = new JPanel();

  private LabeledTextField ltfFiducialMatchListA =
    new LabeledTextField("Corresponding fiducial list A: ");
  private LabeledTextField ltfFiducialMatchListB =
    new LabeledTextField("Corresponding fiducial list B: ");

  private LabeledTextField ltfResidulThreshold =
    new LabeledTextField("Residual Threshold: ");

  private JPanel pnlModelSelect = new JPanel();
  private JButton btnImodMatchModels =
    new JButton("<html><b>Create matching models in imod</b>");
  private JCheckBox cbUseModel = new JCheckBox("Model based initial match");

  private JPanel panelButton = new JPanel();
  private JButton buttonCombineRestart =
    new JButton("<html><b>Restart combine</b>");
  private JButton buttonModelCombine =
    new JButton("<html><b>Model based combine</b>");

  /**
   * Default constructor
   * @param appMgr
   */
  public InitialCombinePanel(ApplicationManager appMgr) {

    applicationManager = appMgr;

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));
    //  Get the current text height from one of the 
    double height = ltfFiducialMatchListA.getLabelPreferredSize().getHeight();

    //  Set the button sizes
    Dimension dimButton = new Dimension();
    dimButton.setSize(10 * height, 3 * height);
    btnImodMatchModels.setPreferredSize(dimButton);
    btnImodMatchModels.setMaximumSize(dimButton);
    buttonCombineRestart.setPreferredSize(dimButton);
    buttonCombineRestart.setMaximumSize(dimButton);
    buttonModelCombine.setPreferredSize(dimButton);
    buttonModelCombine.setMaximumSize(dimButton);

    panelSolvematchshift.setLayout(
      new BoxLayout(panelSolvematchshift, BoxLayout.Y_AXIS));
    panelSolvematchshift.setBorder(
      new EtchedBorder("Solvematch Parameters").getBorder());

    pnlModelSelect.setLayout(new BoxLayout(pnlModelSelect, BoxLayout.X_AXIS));
    pnlModelSelect.add(cbUseModel);
    pnlModelSelect.add(btnImodMatchModels);
    panelSolvematchshift.add(pnlModelSelect);
    panelSolvematchshift.add(Box.createRigidArea(FixedDim.x0_y5));    
    panelSolvematchshift.add(ltfFiducialMatchListA.getContainer());
    panelSolvematchshift.add(Box.createRigidArea(FixedDim.x0_y5));
    panelSolvematchshift.add(ltfFiducialMatchListB.getContainer());
    panelSolvematchshift.add(Box.createRigidArea(FixedDim.x0_y5));
    panelSolvematchshift.add(ltfResidulThreshold.getContainer());

    //  Layout the button panel
    panelButton.setLayout(new BoxLayout(panelButton, BoxLayout.X_AXIS));
    panelButton.add(Box.createHorizontalGlue());
    panelButton.add(buttonCombineRestart);
    panelButton.add(Box.createHorizontalGlue());
    panelButton.add(buttonModelCombine);
    panelButton.add(Box.createHorizontalGlue());

    rootPanel.add(panelSolvematchshift);
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(panelButton);

    //  Bind the buttons to the ActionListener
    ButtonActionListener buttonAction = new ButtonActionListener(this);
    btnImodMatchModels.addActionListener(buttonAction);
    buttonCombineRestart.addActionListener(buttonAction);
    buttonModelCombine.addActionListener(buttonAction);
  }

  public Container getContainer() {
    return rootPanel;
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
   * Respond to button actions
   * @param event
   */
  private void buttonAction(ActionEvent event) {
    if (event
      .getActionCommand()
      .equals(btnImodMatchModels.getActionCommand())) {
      applicationManager.imodMatchingModel();
    }

    if (event
      .getActionCommand()
      .equals(buttonCombineRestart.getActionCommand())) {
      applicationManager.combine();
    }

    if (event
      .getActionCommand()
      .equals(buttonModelCombine.getActionCommand())) {
      applicationManager.modelCombine();
    }

  }

  /**
   * Respond to checkbox actions
   * @param event
   */
  private void checkboxAction(ActionEvent event) {
    if (event.getActionCommand().equals(cbUseModel.getActionCommand())) {
      //  FIXME!! wrong method
      if (cbUseModel.isSelected()) {
        applicationManager.updateSolvematchmodCom();
      }
      else {
        applicationManager.updateSolvematchshiftCom();
      }

    }
  }
  
  
  /**
   * Button action listener class
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
   * Checkbox action listener class
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
