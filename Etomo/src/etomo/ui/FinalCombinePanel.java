package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JTextField;

import etomo.ApplicationManager;

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
public class FinalCombinePanel {
  public static final String rcsid = "$Id$";

  private ApplicationManager applicationManager;

  private JPanel rootPanel = new JPanel();

  private JPanel panelPatchcorr = new JPanel();
  private LabeledTextField ltfPatchSizeX =
    new LabeledTextField("X patch size :");
  private LabeledTextField ltfPatchSizeY =
    new LabeledTextField("Y patch size :");
  private LabeledTextField ltfPatchSizeZ =
    new LabeledTextField("Z patch size :");
  private JButton buttonPatchcorrRestart =
    new JButton("<html><b>Restart at patchcorr</b>");

  private JPanel panelMatchorwarp = new JPanel();
  private JPanel panelPatchRegionModel = new JPanel();
  private JCheckBox cbUsePatchRegionModel =
    new JCheckBox("Use patch region model");
  private JButton buttonPatchRegionModel =
    new JButton("<html><b>Create/edit patch region model</b>");
  private LabeledTextField ltfWarpLimit = new LabeledTextField("Warp limit: ");
  private CheckBoxTextField cbtfExcludeXLeft =
    new CheckBoxTextField("Small X (left) exclude:");
  private CheckBoxTextField cbtfExcludeXRight =
    new CheckBoxTextField("Large X (right) exclude:");
  private CheckBoxTextField cbtfExcludeZBottom =
    new CheckBoxTextField("Small Z (bottom) exclude:");
  private CheckBoxTextField cbtfExcludeZTop =
    new CheckBoxTextField("Large Z (top) exclude:");
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
    buttonMatchorwarpRestart.setPreferredSize(dimButton);
    buttonMatchorwarpRestart.setMaximumSize(dimButton);
    buttonMatchorwarpTrial.setPreferredSize(dimButton);
    buttonMatchorwarpTrial.setMaximumSize(dimButton);

    // Layout the Patchcorr panel
    panelPatchcorr.setLayout(new BoxLayout(panelPatchcorr, BoxLayout.Y_AXIS));
    panelPatchcorr.setBorder(
      new EtchedBorder("Patchcorr Parameters").getBorder());
    panelPatchcorr.add(ltfPatchSizeX.getContainer());
    panelPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    panelPatchcorr.add(ltfPatchSizeY.getContainer());
    panelPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    panelPatchcorr.add(ltfPatchSizeZ.getContainer());
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

    panelMatchorwarp.add(cbtfExcludeXLeft);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    panelMatchorwarp.add(cbtfExcludeXRight);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    panelMatchorwarp.add(cbtfExcludeZBottom);
    panelMatchorwarp.add(Box.createRigidArea(FixedDim.x0_y5));
    panelMatchorwarp.add(cbtfExcludeZTop);
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
  }

  public Container getContainer() {
    return rootPanel;
  }

  public void setPatchcorrParams() {

  }

  public void getPatchcorrParams() {

  }

  public void setMatchorwarpParams() {
  }

  public void getMatchorwarpParams() {
  }

}

/**
 * CheckBoxTextField
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

  void setCheckBoxSelected(boolean state) {
    checkBox.setSelected(state);
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