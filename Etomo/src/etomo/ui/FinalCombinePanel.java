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
import etomo.comscript.ConstMatchorwarpParam;
import etomo.comscript.ConstPatchcrawl3DParam;
import etomo.comscript.MatchorwarpParam;
import etomo.comscript.Patchcrawl3DParam;

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
  public static final String rcsid =
    "$Id$";

  private ApplicationManager applicationManager;

  private JPanel rootPanel = new JPanel();

  private JPanel panelPatchcorr = new JPanel();
  private LabeledTextField ltfXPatchSize =
    new LabeledTextField("X patch size :");
  private LabeledTextField ltfYPatchSize =
    new LabeledTextField("Y patch size :");
  private LabeledTextField ltfZPatchSize =
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
  private CheckBoxTextField cbtfXLowerExclude =
    new CheckBoxTextField("X lower (left) exclude:");
  private CheckBoxTextField cbtfXUpperExclude =
    new CheckBoxTextField("X upper (right) exclude:");
  private CheckBoxTextField cbtfZLowerExclude =
    new CheckBoxTextField("Z lower (bottom) exclude:");
  private CheckBoxTextField cbtfZUpperExclude =
    new CheckBoxTextField("Z upper (top) exclude:");
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
    panelPatchcorr.add(ltfXPatchSize.getContainer());
    panelPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    panelPatchcorr.add(ltfYPatchSize.getContainer());
    panelPatchcorr.add(Box.createRigidArea(FixedDim.x0_y5));
    panelPatchcorr.add(ltfZPatchSize.getContainer());
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
  }

  /**
   * Set the Patchcrawl3DParam object values from the UI values.
   * @param patchrawlParam
   * @throws NumberFormatException
   */
  public void getPatchcrawl3DParams(Patchcrawl3DParam patchrawlParam)
    throws NumberFormatException {
    String badParameter = "";

    try {
      badParameter = "X patch size";
      patchrawlParam.setXPatchSize(Integer.parseInt(ltfXPatchSize.getText()));
      badParameter = "Y patch size";
      patchrawlParam.setYPatchSize(Integer.parseInt(ltfYPatchSize.getText()));
      badParameter = "Z patch size";
      patchrawlParam.setZPatchSize(Integer.parseInt(ltfZPatchSize.getText()));
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
    ltfWarpLimit.setText(matchorwarpParam.getWarpLimit());
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
      badParameter = ltfWarpLimit.getLabel();
      matchorwarpParam.setWarpLimit(ltfWarpLimit.getText());

      badParameter = cbtfXLowerExclude.getCheckBoxLabel();
      if (cbtfXLowerExclude.isCheckBoxSelected()) {
        matchorwarpParam.setXLowerExclude(
          Integer.parseInt(cbtfXLowerExclude.getTextField()));
      }

      badParameter = cbtfXUpperExclude.getCheckBoxLabel();
      if (cbtfXUpperExclude.isCheckBoxSelected()) {
        matchorwarpParam.setXUpperExclude(
          Integer.parseInt(cbtfXUpperExclude.getTextField()));
      }

      badParameter = cbtfZLowerExclude.getCheckBoxLabel();
      if (cbtfZLowerExclude.isCheckBoxSelected()) {
        matchorwarpParam.setZLowerExclude(
          Integer.parseInt(cbtfZLowerExclude.getTextField()));
      }

      badParameter = cbtfZUpperExclude.getCheckBoxLabel();
      if (cbtfZUpperExclude.isCheckBoxSelected()) {
        matchorwarpParam.setZUpperExclude(
          Integer.parseInt(cbtfZUpperExclude.getTextField()));
      }

    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
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