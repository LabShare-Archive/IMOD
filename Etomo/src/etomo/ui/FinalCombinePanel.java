package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import etomo.ApplicationManager;
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;

/**
 * <p>Description: 
 * 
 * The Y and Z parameters are presented to the user in swapped format, all other
 * representations of those parameters are as they appear in the commands.
 * Specifically Y contains the depth dimension. 
 * </p>
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
public class FinalCombinePanel {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;

  private JPanel rootPanel = new JPanel();

  private JPanel panelPatchcorr = new JPanel();

  private JPanel panelPatchsize = new JPanel();
  private JPanel panelPatchsizeEdit = new JPanel();
  private LabeledTextField ltfXPatchSize =
    new LabeledTextField("X patch size :");
  private LabeledTextField ltfYPatchSize =
    new LabeledTextField("Z patch size :");
  private LabeledTextField ltfZPatchSize =
    new LabeledTextField("Y patch size :");
  private JPanel panelPatchsizeButtons = new JPanel();
  private JButton buttonPatchsizeIncrease =
    new JButton("<html><b>Patch size +20%</b>");
  private JButton buttonPatchsizeDecrease =
    new JButton("<html><b>Patch size -20%</b>");

  private LabeledTextField ltfXNPatches =
    new LabeledTextField("# of X patches :");
  private LabeledTextField ltfYNPatches =
    new LabeledTextField("# of Z patches :");
  private LabeledTextField ltfZNPatches =
    new LabeledTextField("# of Y patches :");

  private JPanel panelBoundary = new JPanel();
  private LabeledTextField ltfXLow = new LabeledTextField("X Low :");
  private LabeledTextField ltfXHigh = new LabeledTextField("X high :");
  private LabeledTextField ltfYLow = new LabeledTextField("Z Low :");
  private LabeledTextField ltfYHigh = new LabeledTextField("Z high :");
  private LabeledTextField ltfZLow = new LabeledTextField("Y Low :");
  private LabeledTextField ltfZHigh = new LabeledTextField("Y high :");

  private JButton buttonPatchcorrRestart =
    new JButton("<html><b>Restart at patchcorr</b>");

  private JPanel panelMatchorwarp = new JPanel();
  private JPanel panelPatchRegionModel = new JPanel();
  private JCheckBox cbUsePatchRegionModel =
    new JCheckBox("Use patch region model");
  private JButton buttonPatchRegionModel =
    new JButton("<html><b>Create/edit patch region model</b>");
  private LabeledTextField ltfWarpLimit = new LabeledTextField("Warp limit: ");
  private LabeledTextField ltfRefineLimit =
    new LabeledTextField("Refine limit: ");

  private CheckBoxTextField cbtfXLowerExclude =
    new CheckBoxTextField("columns to exclude on left (xlower): ");
  private CheckBoxTextField cbtfXUpperExclude =
    new CheckBoxTextField("columns to exclude on right (xupper): ");
  private CheckBoxTextField cbtfZLowerExclude =
    new CheckBoxTextField("rows to exclude on bottom (zlower): ");
  private CheckBoxTextField cbtfZUpperExclude =
    new CheckBoxTextField("rows to exclude on top (zupper): ");
  private JPanel panelMatchorwarpButtons = new JPanel();
  private JButton buttonMatchorwarpRestart =
    new JButton("<html><b>Restart at matchorwarp</b>");
  private JButton buttonMatchorwarpTrial =
    new JButton("<html><b>Matchorwarp trial run</b>");

  private JPanel panelButton = new JPanel();

  private JButton buttonPatchVectorModel =
    new JButton("<html><b>Examine patch vector model</b>");
  private JButton buttonImodMatchedTo =
    new JButton("<html><b>Open volume being matched to</b>");

  /**
   * Default constructor
   * @param appMgr
   */
  public FinalCombinePanel(ApplicationManager appMgr) {

    applicationManager = appMgr;

    rootPanel.setLayout(new BoxLayout(rootPanel, BoxLayout.Y_AXIS));

    //  Get the current text height from one of the 
    double height = cbUsePatchRegionModel.getPreferredSize().getHeight();

    //  Set the button sizes
    Dimension dimButton = new Dimension();
    dimButton.setSize(8 * height, 2 * height);
    buttonPatchRegionModel.setPreferredSize(dimButton);
    buttonPatchRegionModel.setMaximumSize(dimButton);
    buttonPatchVectorModel.setPreferredSize(dimButton);
    buttonPatchVectorModel.setMaximumSize(dimButton);
    buttonImodMatchedTo.setPreferredSize(dimButton);
    buttonImodMatchedTo.setMaximumSize(dimButton);
    buttonPatchcorrRestart.setPreferredSize(dimButton);
    buttonPatchcorrRestart.setMaximumSize(dimButton);
    buttonPatchsizeIncrease.setPreferredSize(dimButton);
    buttonPatchsizeIncrease.setMaximumSize(dimButton);
    buttonPatchsizeDecrease.setPreferredSize(dimButton);
    buttonPatchsizeDecrease.setMaximumSize(dimButton);

    buttonMatchorwarpRestart.setPreferredSize(dimButton);
    buttonMatchorwarpRestart.setMaximumSize(dimButton);
    buttonMatchorwarpTrial.setPreferredSize(dimButton);
    buttonMatchorwarpTrial.setMaximumSize(dimButton);

    // Layout the Patchcorr panel
    panelPatchcorr.setLayout(new BoxLayout(panelPatchcorr, BoxLayout.Y_AXIS));
    panelPatchcorr.setBorder(
      new EtchedBorder("Patchcorr Parameters").getBorder());

    panelPatchsizeButtons.setLayout(
      new BoxLayout(panelPatchsizeButtons, BoxLayout.Y_AXIS));
    panelPatchsizeButtons.add(buttonPatchsizeIncrease);
    panelPatchsizeButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    panelPatchsizeButtons.add(buttonPatchsizeDecrease);

    panelPatchsizeEdit.setLayout(
      new BoxLayout(panelPatchsizeEdit, BoxLayout.Y_AXIS));

    panelPatchsizeEdit.add(ltfXPatchSize.getContainer());
    panelPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    panelPatchsizeEdit.add(ltfZPatchSize.getContainer());
    panelPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    panelPatchsizeEdit.add(ltfYPatchSize.getContainer());
    panelPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));

    panelPatchsize.setLayout(new BoxLayout(panelPatchsize, BoxLayout.X_AXIS));
    panelPatchsize.add(panelPatchsizeEdit);
    panelPatchsize.add(Box.createRigidArea(FixedDim.x10_y0));
    panelPatchsize.add(panelPatchsizeButtons);
    panelPatchcorr.add(panelPatchsize);

    panelPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));

    panelBoundary.setLayout(new GridLayout(3, 3, 5, 5));
    panelBoundary.add(ltfXNPatches.getContainer());
    panelBoundary.add(ltfXLow.getContainer());
    panelBoundary.add(ltfXHigh.getContainer());
    panelBoundary.add(ltfZNPatches.getContainer());
    panelBoundary.add(ltfZLow.getContainer());
    panelBoundary.add(ltfZHigh.getContainer());
    panelBoundary.add(ltfYNPatches.getContainer());
    panelBoundary.add(ltfYLow.getContainer());
    panelBoundary.add(ltfYHigh.getContainer());
    panelPatchcorr.add(panelBoundary);
    panelPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    buttonPatchcorrRestart.setAlignmentX(Component.CENTER_ALIGNMENT);

    panelPatchcorr.add(buttonPatchcorrRestart);
    panelPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    rootPanel.add(panelPatchcorr);

    //  Layout the Matchorwarp panel
    panelMatchorwarp.setLayout(
      new BoxLayout(panelMatchorwarp, BoxLayout.Y_AXIS));
    panelMatchorwarp.setBorder(
      new EtchedBorder("Matchorwarp Parameters").getBorder());

    panelPatchRegionModel.setLayout(
      new BoxLayout(panelPatchRegionModel, BoxLayout.X_AXIS));
    panelPatchRegionModel.add(cbUsePatchRegionModel);
    panelPatchRegionModel.add(buttonPatchRegionModel);
    panelMatchorwarp.add(panelPatchRegionModel);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));
    panelMatchorwarp.add(ltfWarpLimit.getContainer());
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));
    panelMatchorwarp.add(ltfRefineLimit.getContainer());
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));

    panelMatchorwarp.add(cbtfXLowerExclude);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    panelMatchorwarp.add(cbtfXUpperExclude);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    panelMatchorwarp.add(cbtfZLowerExclude);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    panelMatchorwarp.add(cbtfZUpperExclude);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));

    panelMatchorwarpButtons.setLayout(
      new BoxLayout(panelMatchorwarpButtons, BoxLayout.X_AXIS));
    panelMatchorwarpButtons.add(Box.createHorizontalGlue());
    panelMatchorwarpButtons.add(buttonMatchorwarpRestart);
    panelMatchorwarpButtons.add(Box.createHorizontalGlue());
    panelMatchorwarpButtons.add(buttonMatchorwarpTrial);
    panelMatchorwarpButtons.add(Box.createHorizontalGlue());
    panelMatchorwarp.add(panelMatchorwarpButtons);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    rootPanel.add(panelMatchorwarp);

    //  Create the button panel
    panelButton.setLayout(new BoxLayout(panelButton, BoxLayout.X_AXIS));
    panelButton.add(Box.createHorizontalGlue());
    panelButton.add(buttonPatchVectorModel);
    panelButton.add(Box.createHorizontalGlue());
    panelButton.add(buttonImodMatchedTo);
    panelButton.add(Box.createHorizontalGlue());
    rootPanel.add(Box.createVerticalGlue());
    rootPanel.add(panelButton);

    // Bind the buttons to action listener
    ButtonActionListener actionListener = new ButtonActionListener(this);
    buttonPatchcorrRestart.addActionListener(actionListener);
    buttonPatchsizeIncrease.addActionListener(actionListener);
    buttonPatchsizeDecrease.addActionListener(actionListener);
    buttonPatchRegionModel.addActionListener(actionListener);
    buttonMatchorwarpRestart.addActionListener(actionListener);
    buttonMatchorwarpTrial.addActionListener(actionListener);
    buttonPatchVectorModel.addActionListener(actionListener);
    buttonImodMatchedTo.addActionListener(actionListener);
  }

  void setAdvanced(boolean state) {
    panelBoundary.setVisible(state);
    ltfRefineLimit.setVisible(state);
  }

  /**
   * Return the rootPanel reference
   * @return Container
   */
  public Container getContainer() {
    return rootPanel;
  }

  /**
   * Set the values of the patchcrawl3D UI objects from the 
   * ConstPatchcrawl3DParam object.
   * @param patchrawlParam
   */
  public void setPatchcrawl3DParams(ConstPatchcrawl3DParam patchrawlParam) {
    ltfXPatchSize.setText(patchrawlParam.getXPatchSize());
    ltfYPatchSize.setText(patchrawlParam.getYPatchSize());
    ltfZPatchSize.setText(patchrawlParam.getZPatchSize());
    ltfXNPatches.setText(patchrawlParam.getNX());
    ltfYNPatches.setText(patchrawlParam.getNY());
    ltfZNPatches.setText(patchrawlParam.getNZ());
    ltfXLow.setText(patchrawlParam.getXLow());
    ltfXHigh.setText(patchrawlParam.getXHigh());
    ltfYLow.setText(patchrawlParam.getYLow());
    ltfYHigh.setText(patchrawlParam.getYHigh());
    ltfZLow.setText(patchrawlParam.getZLow());
    ltfZHigh.setText(patchrawlParam.getZHigh());
  }

  /**
   * Set the Patchcrawl3DParam object values from the UI values.
   * @param patchrawlParam
   * @throws NumberFormatException
   */
  public void getPatchcrawl3DParams(Patchcrawl3DParam patchcrawl3DParam)
    throws NumberFormatException {
    String badParameter = "";

    try {
      badParameter = ltfXPatchSize.getLabel();
      patchcrawl3DParam.setXPatchSize(
        Integer.parseInt(ltfXPatchSize.getText()));
      badParameter = ltfYPatchSize.getLabel();
      patchcrawl3DParam.setYPatchSize(
        Integer.parseInt(ltfYPatchSize.getText()));
      badParameter = ltfZPatchSize.getLabel();
      patchcrawl3DParam.setZPatchSize(
        Integer.parseInt(ltfZPatchSize.getText()));
      badParameter = ltfXNPatches.getLabel();
      patchcrawl3DParam.setNX(Integer.parseInt(ltfXNPatches.getText()));
      badParameter = ltfYNPatches.getLabel();
      patchcrawl3DParam.setNY(Integer.parseInt(ltfYNPatches.getText()));
      badParameter = ltfZNPatches.getLabel();
      patchcrawl3DParam.setNZ(Integer.parseInt(ltfZNPatches.getText()));
      badParameter = ltfXLow.getLabel();
      patchcrawl3DParam.setXLow(Integer.parseInt(ltfXLow.getText()));
      badParameter = ltfXHigh.getLabel();
      patchcrawl3DParam.setXHigh(Integer.parseInt(ltfXHigh.getText()));
      badParameter = ltfYLow.getLabel();
      patchcrawl3DParam.setYLow(Integer.parseInt(ltfYLow.getText()));
      badParameter = ltfYHigh.getLabel();
      patchcrawl3DParam.setYHigh(Integer.parseInt(ltfYHigh.getText()));
      badParameter = ltfZLow.getLabel();
      patchcrawl3DParam.setZLow(Integer.parseInt(ltfZLow.getText()));
      badParameter = ltfZHigh.getLabel();
      patchcrawl3DParam.setZHigh(Integer.parseInt(ltfZHigh.getText()));

    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  /**
   * Set the values of the matchorwarp UI objects from the 
   * ConstMatchorwarpParam object.
   * @param matchorwarpParam
   */
  public void setMatchorwarpParams(ConstMatchorwarpParam matchorwarpParam) {
    cbUsePatchRegionModel.setSelected(
      matchorwarpParam.getModelFile().equals(""));
    ltfWarpLimit.setText(matchorwarpParam.getWarpLimit());
    ltfRefineLimit.setText(matchorwarpParam.getRefineLimit());

    if (matchorwarpParam.getXLowerExclude() > 0) {
      cbtfXLowerExclude.setCheckBoxSelected(true);
      cbtfXLowerExclude.setTextField(
        String.valueOf(matchorwarpParam.getXLowerExclude()));
    }
    if (matchorwarpParam.getXUpperExclude() > 0) {
      cbtfXUpperExclude.setCheckBoxSelected(true);
      cbtfXUpperExclude.setTextField(
        String.valueOf(matchorwarpParam.getXUpperExclude()));
    }

    if (matchorwarpParam.getZLowerExclude() > 0) {
      cbtfZLowerExclude.setCheckBoxSelected(true);
      cbtfZLowerExclude.setTextField(
        String.valueOf(matchorwarpParam.getZLowerExclude()));
    }

    if (matchorwarpParam.getZUpperExclude() > 0) {
      cbtfZUpperExclude.setCheckBoxSelected(true);
      cbtfZUpperExclude.setTextField(
        String.valueOf(matchorwarpParam.getZUpperExclude()));
    }
  }

  /**
   * Set the MatchorwarpParam object values from the UI values.
   * @param matchorwarpParam
   * @throws NumberFormatException
   */
  public void getMatchorwarpParams(MatchorwarpParam matchorwarpParam)
    throws NumberFormatException {
    String badParameter = "";

    try {
      badParameter = cbUsePatchRegionModel.getText();
      if (cbUsePatchRegionModel.isSelected()) {
        matchorwarpParam.setDefaultModelFile();
      }
      else {
        matchorwarpParam.setModelFile("");
      }

      badParameter = ltfWarpLimit.getLabel();
      matchorwarpParam.setWarpLimit(ltfWarpLimit.getText());

      badParameter = ltfRefineLimit.getLabel();
      matchorwarpParam.setRefineLimit(
        Double.parseDouble(ltfRefineLimit.getText()));

      badParameter = cbtfXLowerExclude.getCheckBoxLabel();
      if (cbtfXLowerExclude.isCheckBoxSelected()) {
        matchorwarpParam.setXLowerExclude(
          Integer.parseInt(cbtfXLowerExclude.getTextField()));
      }
      else {
        matchorwarpParam.setXLowerExclude(0);
      }

      badParameter = cbtfXUpperExclude.getCheckBoxLabel();
      if (cbtfXUpperExclude.isCheckBoxSelected()) {
        matchorwarpParam.setXUpperExclude(
          Integer.parseInt(cbtfXUpperExclude.getTextField()));
      }
      else {
        matchorwarpParam.setXUpperExclude(0);
      }

      badParameter = cbtfZLowerExclude.getCheckBoxLabel();
      if (cbtfZLowerExclude.isCheckBoxSelected()) {
        matchorwarpParam.setZLowerExclude(
          Integer.parseInt(cbtfZLowerExclude.getTextField()));
      }
      else {
        matchorwarpParam.setZLowerExclude(0);
      }

      badParameter = cbtfZUpperExclude.getCheckBoxLabel();
      if (cbtfZUpperExclude.isCheckBoxSelected()) {
        matchorwarpParam.setZUpperExclude(
          Integer.parseInt(cbtfZUpperExclude.getTextField()));
      }
      else {
        matchorwarpParam.setZUpperExclude(0);
      }

    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  private void buttonAction(ActionEvent event) {

    if (event
      .getActionCommand()
      .equals(buttonPatchsizeDecrease.getActionCommand())) {
      ltfXPatchSize.setText(
        Math.round(Integer.parseInt(ltfXPatchSize.getText()) / 1.2f));
      ltfYPatchSize.setText(
        Math.round(Integer.parseInt(ltfYPatchSize.getText()) / 1.2f));
      ltfZPatchSize.setText(
        Math.round(Integer.parseInt(ltfZPatchSize.getText()) / 1.2f));
    }

    if (event
      .getActionCommand()
      .equals(buttonPatchsizeIncrease.getActionCommand())) {
      ltfXPatchSize.setText(
        Math.round(Integer.parseInt(ltfXPatchSize.getText()) * 1.2f));
      ltfYPatchSize.setText(
        Math.round(Integer.parseInt(ltfYPatchSize.getText()) * 1.2f));
      ltfZPatchSize.setText(
        Math.round(Integer.parseInt(ltfZPatchSize.getText()) * 1.2f));

    }

    if (event
      .getActionCommand()
      .equals(buttonPatchcorrRestart.getActionCommand())) {
      applicationManager.patchcorrCombine();
    }

    if (event
      .getActionCommand()
      .equals(buttonPatchRegionModel.getActionCommand())) {
      //TODO call imod on the patch region model
    }

    if (event
      .getActionCommand()
      .equals(buttonMatchorwarpRestart.getActionCommand())) {
      applicationManager.matchorwarpCombine();
    }

    if (event
      .getActionCommand()
      .equals(buttonMatchorwarpTrial.getActionCommand())) {
      //TODO add the trial flag and run matchorwarp

    }
    if (event
      .getActionCommand()
      .equals(buttonPatchVectorModel.getActionCommand())) {
      //TODO open imod on the patch vector model
    }
    if (event
      .getActionCommand()
      .equals(buttonImodMatchedTo.getActionCommand())) {
      //TODO open imod on the tomogram being matched to
    }

  }

  class ButtonActionListener implements ActionListener {
    FinalCombinePanel listenee;

    ButtonActionListener(FinalCombinePanel finalCombinePanel) {
      listenee = finalCombinePanel;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.buttonAction(event);
    }
  }
}

/**
 * CheckBoxTextField combines a JCheckBox with it's label and an editable text
 * field.  The included action listener enables/disables the text field to match
 * the state of the check box.
 * @author rickg
 */
class CheckBoxTextField extends JPanel {
  private JCheckBox checkBox;
  private JTextField textField;

  /**
   * Default constructor
   * @param label
   */
  public CheckBoxTextField(String label) {

    checkBox = new JCheckBox(label);
    textField = new JTextField();
    this.setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
    this.add(checkBox);
    this.add(textField);

    //  set the size of the text field
    double height = checkBox.getPreferredSize().getHeight();
    Dimension dim = textField.getPreferredSize();
    dim.setSize(50 * height, height);
    textField.setMaximumSize(dim);
    textField.setEnabled(checkBox.isSelected());
    checkBox.addActionListener(new CheckBoxActionListener(this));

  }

  String getCheckBoxLabel() {
    return checkBox.getText();
  }

  void setCheckBoxSelected(boolean state) {
    checkBox.setSelected(state);
    textField.setEnabled(state);
  }

  boolean isCheckBoxSelected() {
    return checkBox.isSelected();
  }

  void setTextField(String string) {
    textField.setText(string);
  }

  String getTextField() {
    return textField.getText();
  }

  //  Action event handler for the check box
  void manageCheckBoxState(ActionEvent event) {
    textField.setEnabled(checkBox.isSelected());
  }

  //  ActionListener class for the check box
  class CheckBoxActionListener implements ActionListener {
    CheckBoxTextField listenee;

    CheckBoxActionListener(CheckBoxTextField checkBoxTextField) {
      listenee = checkBoxTextField;
    }

    public void actionPerformed(ActionEvent event) {
      listenee.manageCheckBoxState(event);
    }
  }

}