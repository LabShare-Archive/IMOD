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
 * Note:
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
 * <p> $Log$
 * <p> </p>
 */
public class FinalCombinePanel implements ContextMenu {
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;

  private JPanel pnlRoot = new JPanel();

  private JPanel pnlPatchcorr = new JPanel();

  private JPanel pnlPatchsize = new JPanel();
  private JPanel pnlPatchsizeEdit = new JPanel();
  private LabeledTextField ltfXPatchSize =
    new LabeledTextField("X patch size :");
  private LabeledTextField ltfYPatchSize =
    new LabeledTextField("Z patch size :");
  private LabeledTextField ltfZPatchSize =
    new LabeledTextField("Y patch size :");
  private JPanel pnlPatchsizeButtons = new JPanel();
  private JButton btnPatchsizeIncrease =
    new JButton("<html><b>Patch size +20%</b>");
  private JButton btnPatchsizeDecrease =
    new JButton("<html><b>Patch size -20%</b>");

  private LabeledTextField ltfXNPatches =
    new LabeledTextField("# of X patches :");
  private LabeledTextField ltfYNPatches =
    new LabeledTextField("# of Z patches :");
  private LabeledTextField ltfZNPatches =
    new LabeledTextField("# of Y patches :");

  private JPanel pnlBoundary = new JPanel();
  private LabeledTextField ltfXLow = new LabeledTextField("X Low :");
  private LabeledTextField ltfXHigh = new LabeledTextField("X high :");
  private LabeledTextField ltfYLow = new LabeledTextField("Z Low :");
  private LabeledTextField ltfYHigh = new LabeledTextField("Z high :");
  private LabeledTextField ltfZLow = new LabeledTextField("Y Low :");
  private LabeledTextField ltfZHigh = new LabeledTextField("Y high :");

  private JButton btnPatchcorrRestart =
    new JButton("<html><b>Restart at patchcorr</b>");

  private JPanel pnlMatchorwarp = new JPanel();
  private JPanel pnlPatchRegionModel = new JPanel();
  private JCheckBox cbUsePatchRegionModel =
    new JCheckBox("Use patch region model");
  private JButton btnPatchRegionModel =
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
  private JPanel pnlMatchorwarpButtons = new JPanel();
  private JButton btnMatchorwarpRestart =
    new JButton("<html><b>Restart at matchorwarp</b>");
  private JButton btnMatchorwarpTrial =
    new JButton("<html><b>Matchorwarp trial run</b>");

  private JPanel pnlButton = new JPanel();

  private JButton btnPatchVectorModel =
    new JButton("<html><b>Examine patch vector model</b>");
  private JButton btnImodMatchedTo =
    new JButton("<html><b>Open volume being matched to</b>");
  private JButton btnImodCombined =
    new JButton("<html><b>Open combined volume</b>");

  /**
   * Default constructor
   * @param appMgr
   */
  public FinalCombinePanel(ApplicationManager appMgr) {

    applicationManager = appMgr;

    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.Y_AXIS));

    //  Get the current text height from one of the 
    double height = cbUsePatchRegionModel.getPreferredSize().getHeight();

    //  Set the button sizes
    Dimension dimButton = new Dimension();
    dimButton.setSize(8 * height, 2 * height);
    btnPatchRegionModel.setPreferredSize(dimButton);
    btnPatchRegionModel.setMaximumSize(dimButton);
    btnPatchVectorModel.setPreferredSize(dimButton);
    btnPatchVectorModel.setMaximumSize(dimButton);
    btnImodMatchedTo.setPreferredSize(dimButton);
    btnImodMatchedTo.setMaximumSize(dimButton);
    btnImodCombined.setPreferredSize(dimButton);
    btnImodCombined.setMaximumSize(dimButton);

    btnPatchcorrRestart.setPreferredSize(dimButton);
    btnPatchcorrRestart.setMaximumSize(dimButton);
    btnPatchsizeIncrease.setPreferredSize(dimButton);
    btnPatchsizeIncrease.setMaximumSize(dimButton);
    btnPatchsizeDecrease.setPreferredSize(dimButton);
    btnPatchsizeDecrease.setMaximumSize(dimButton);

    btnMatchorwarpRestart.setPreferredSize(dimButton);
    btnMatchorwarpRestart.setMaximumSize(dimButton);
    btnMatchorwarpTrial.setPreferredSize(dimButton);
    btnMatchorwarpTrial.setMaximumSize(dimButton);

    // Layout the Patchcorr panel
    pnlPatchcorr.setLayout(new BoxLayout(pnlPatchcorr, BoxLayout.Y_AXIS));
    pnlPatchcorr.setBorder(
      new EtchedBorder("Patchcorr Parameters").getBorder());

    pnlPatchsizeButtons.setLayout(
      new BoxLayout(pnlPatchsizeButtons, BoxLayout.Y_AXIS));
    pnlPatchsizeButtons.add(btnPatchsizeIncrease);
    pnlPatchsizeButtons.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeButtons.add(btnPatchsizeDecrease);

    pnlPatchsizeEdit.setLayout(
      new BoxLayout(pnlPatchsizeEdit, BoxLayout.Y_AXIS));

    pnlPatchsizeEdit.add(ltfXPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeEdit.add(ltfZPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlPatchsizeEdit.add(ltfYPatchSize.getContainer());
    pnlPatchsizeEdit.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlPatchsize.setLayout(new BoxLayout(pnlPatchsize, BoxLayout.X_AXIS));
    pnlPatchsize.add(pnlPatchsizeEdit);
    pnlPatchsize.add(Box.createRigidArea(FixedDim.x10_y0));
    pnlPatchsize.add(pnlPatchsizeButtons);
    pnlPatchcorr.add(pnlPatchsize);

    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlBoundary.setLayout(new GridLayout(3, 3, 5, 5));
    pnlBoundary.add(ltfXNPatches.getContainer());
    pnlBoundary.add(ltfXLow.getContainer());
    pnlBoundary.add(ltfXHigh.getContainer());
    pnlBoundary.add(ltfZNPatches.getContainer());
    pnlBoundary.add(ltfZLow.getContainer());
    pnlBoundary.add(ltfZHigh.getContainer());
    pnlBoundary.add(ltfYNPatches.getContainer());
    pnlBoundary.add(ltfYLow.getContainer());
    pnlBoundary.add(ltfYHigh.getContainer());
    pnlPatchcorr.add(pnlBoundary);
    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    btnPatchcorrRestart.setAlignmentX(Component.CENTER_ALIGNMENT);

    pnlPatchcorr.add(btnPatchcorrRestart);
    pnlPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(pnlPatchcorr);

    //  Layout the Matchorwarp panel
    pnlMatchorwarp.setLayout(new BoxLayout(pnlMatchorwarp, BoxLayout.Y_AXIS));
    pnlMatchorwarp.setBorder(
      new EtchedBorder("Matchorwarp Parameters").getBorder());

    pnlPatchRegionModel.setLayout(
      new BoxLayout(pnlPatchRegionModel, BoxLayout.X_AXIS));
    pnlPatchRegionModel.add(cbUsePatchRegionModel);
    pnlPatchRegionModel.add(btnPatchRegionModel);
    pnlMatchorwarp.add(pnlPatchRegionModel);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlMatchorwarp.add(ltfWarpLimit.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));
    pnlMatchorwarp.add(ltfRefineLimit.getContainer());
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y10));

    pnlMatchorwarp.add(cbtfXLowerExclude);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(cbtfXUpperExclude);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(cbtfZLowerExclude);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlMatchorwarp.add(cbtfZUpperExclude);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));

    pnlMatchorwarpButtons.setLayout(
      new BoxLayout(pnlMatchorwarpButtons, BoxLayout.X_AXIS));
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarpButtons.add(btnMatchorwarpRestart);
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarpButtons.add(btnMatchorwarpTrial);
    pnlMatchorwarpButtons.add(Box.createHorizontalGlue());
    pnlMatchorwarp.add(pnlMatchorwarpButtons);
    pnlMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    pnlRoot.add(pnlMatchorwarp);

    //  Create the button panel
    pnlButton.setLayout(new BoxLayout(pnlButton, BoxLayout.X_AXIS));
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnPatchVectorModel);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodMatchedTo);
    pnlButton.add(Box.createHorizontalGlue());
    pnlButton.add(btnImodCombined);
    pnlButton.add(Box.createHorizontalGlue());
    pnlRoot.add(Box.createVerticalGlue());
    pnlRoot.add(pnlButton);

    // Bind the buttons to action listener
    ButtonActionListener actionListener = new ButtonActionListener(this);
    btnPatchcorrRestart.addActionListener(actionListener);
    btnPatchsizeIncrease.addActionListener(actionListener);
    btnPatchsizeDecrease.addActionListener(actionListener);
    btnPatchRegionModel.addActionListener(actionListener);
    btnMatchorwarpRestart.addActionListener(actionListener);
    btnMatchorwarpTrial.addActionListener(actionListener);
    btnPatchVectorModel.addActionListener(actionListener);
    btnImodMatchedTo.addActionListener(actionListener);
    btnImodCombined.addActionListener(actionListener);

    // Mouse listener for context menu
    GenericMouseAdapter mouseAdapter = new GenericMouseAdapter(this);
    pnlRoot.addMouseListener(mouseAdapter);
  }

  void setAdvanced(boolean state) {
    pnlBoundary.setVisible(state);
    ltfRefineLimit.setVisible(state);
  }

  /**
   * Return the pnlRoot reference
   * @return Container
   */
  public Container getContainer() {
    return pnlRoot;
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
      !matchorwarpParam.getModelFile().equals(""));
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

  /**
   * Right mouse button context menu
   */
  public void popUpContextMenu(MouseEvent mouseEvent) {
    String[] manPagelabel = { "patchcrawl3d", "matchorwarp" };
    String[] manPage = { "patchcrawl3d.html", "matchorwarp.html" };
    String[] logFileLabel = { "patchcorr", "matchorwarp", "volcombine" };
    String[] logFile = { "patchcorr.log", "matchorwarp.log", "volcombine.log" };

    ContextPopup contextPopup =
      new ContextPopup(
        pnlRoot,
        mouseEvent,
        "Patch Problems in Combining",
        manPagelabel,
        manPage,
        logFileLabel,
        logFile);
  }

  private void buttonAction(ActionEvent event) {

    // Decrease patch sizes by 20% and then round to ints since they are in
    // pixels
    if (event
      .getActionCommand()
      .equals(btnPatchsizeDecrease.getActionCommand())) {
      ltfXPatchSize.setText(
        Math.round(Integer.parseInt(ltfXPatchSize.getText()) / 1.2f));
      ltfYPatchSize.setText(
        Math.round(Integer.parseInt(ltfYPatchSize.getText()) / 1.2f));
      ltfZPatchSize.setText(
        Math.round(Integer.parseInt(ltfZPatchSize.getText()) / 1.2f));
    }

    //  Increase patch sizes by 20% and then round to ints since they are in
    // pixels
    if (event
      .getActionCommand()
      .equals(btnPatchsizeIncrease.getActionCommand())) {
      ltfXPatchSize.setText(
        Math.round(Integer.parseInt(ltfXPatchSize.getText()) * 1.2f));
      ltfYPatchSize.setText(
        Math.round(Integer.parseInt(ltfYPatchSize.getText()) * 1.2f));
      ltfZPatchSize.setText(
        Math.round(Integer.parseInt(ltfZPatchSize.getText()) * 1.2f));

    }

    if (event
      .getActionCommand()
      .equals(btnPatchcorrRestart.getActionCommand())) {
      applicationManager.patchcorrCombine();
    }

    if (event
      .getActionCommand()
      .equals(btnPatchRegionModel.getActionCommand())) {
      applicationManager.imodPatchRegionModel();
    }

    if (event
      .getActionCommand()
      .equals(btnMatchorwarpRestart.getActionCommand())) {
      applicationManager.matchorwarpCombine();
    }

    if (event
      .getActionCommand()
      .equals(btnMatchorwarpTrial.getActionCommand())) {
      applicationManager.matchorwarpTrial();
    }

    if (event
      .getActionCommand()
      .equals(btnPatchVectorModel.getActionCommand())) {
      applicationManager.imodPatchVectorModel();
    }

    if (event.getActionCommand().equals(btnImodMatchedTo.getActionCommand())) {
      applicationManager.imodMatchedToTomogram();
    }

    if (event.getActionCommand().equals(btnImodCombined.getActionCommand())) {
      applicationManager.imodCombinedTomogram();
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