package etomo.ui;

import java.awt.Component;
import java.awt.Container;
import java.awt.Color;
import javax.swing.*;
import javax.swing.border.TitledBorder;

import etomo.comscript.TransferfidParam;

/**
 * @author rickg
 *
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates.
 * To enable and disable the creation of type comments go to
 * Window>Preferences>Java>Code Generation.
 */
public class TransferfidPanel {
  public static final String rcsid =
    "$Id$";

  // FIXME: these should be gotten from the app some how
  private final Color highlight = new Color(248, 254, 255);
  private final Color shadow = new Color(121, 124, 136);

  private JPanel panelTransferfid = new JPanel();

  private JCheckBox chkRunMidas = new JCheckBox("Run midas");
  private LabeledTextField ltfCenterViewA =
    new LabeledTextField("Center view A: ");
  private LabeledTextField ltfCenterViewB =
    new LabeledTextField("Center view B: ");

  private JPanel panelSearchDirection = new JPanel();
  private ButtonGroup bgSearchDirection = new ButtonGroup();
  private JRadioButton rbSearchBoth = new JRadioButton("Both directions");
  private JRadioButton rbSearchPlus90 = new JRadioButton("+90 (CCW) only");
  private JRadioButton rbSearchMinus90 = new JRadioButton("-90 (CW) only");

  public TransferfidPanel() {

    panelTransferfid.setLayout(
      new BoxLayout(panelTransferfid, BoxLayout.Y_AXIS));
    panelTransferfid.setBorder(
      new TitledBorder(
        BorderFactory.createEtchedBorder(highlight, shadow),
        "Transferfid Parameters"));
    chkRunMidas.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfid.add(chkRunMidas);

    //  Add a horizontal strut to keep the panel a minimum size    
    panelTransferfid.add(Box.createHorizontalStrut(300));
    panelTransferfid.add(ltfCenterViewA.getContainer());
    panelTransferfid.add(ltfCenterViewB.getContainer());

    bgSearchDirection.add(rbSearchBoth);
    bgSearchDirection.add(rbSearchPlus90);
    bgSearchDirection.add(rbSearchMinus90);
    panelSearchDirection.setLayout(
      new BoxLayout(panelSearchDirection, BoxLayout.Y_AXIS));
    panelSearchDirection.setBorder(
      new TitledBorder(
        BorderFactory.createEtchedBorder(highlight, shadow),
        "Search Direction"));
    panelSearchDirection.add(rbSearchBoth);
    panelSearchDirection.add(rbSearchPlus90);
    panelSearchDirection.add(rbSearchMinus90);
    panelSearchDirection.setAlignmentX(Component.RIGHT_ALIGNMENT);
    panelTransferfid.add(panelSearchDirection);
  }

  /**
   * Set the values of the panel using a TransferfidParam parameter
   * object
   */
  public void setParameters(TransferfidParam params) {
    chkRunMidas.setSelected(params.isRunMidas());
    if (params.getCenterViewA() > 0) {
      ltfCenterViewA.setText(params.getCenterViewA());
    }

    if (params.getCenterViewB() > 0) {
      ltfCenterViewB.setText(params.getCenterViewB());
    }

    if (params.getSearchDirection() == 0) {
      rbSearchBoth.setSelected(true);
    }
    if (params.getSearchDirection() < 0) {
      rbSearchMinus90.setSelected(true);
    }
    if (params.getSearchDirection() > 0) {
      rbSearchPlus90.setSelected(true);
    }

  }

  /**
   * Get the values from the panel filling in the TransferfidParam object
   */
  public void getParameters(TransferfidParam params) {
    params.setRunMidas(chkRunMidas.isSelected());
    if (ltfCenterViewA.getText().matches("^\\s*$")) {
      params.setCenterViewA(0);
    }
    else {
      params.setCenterViewA(Integer.parseInt(ltfCenterViewA.getText()));
    }
    if (ltfCenterViewB.getText().matches("^\\s*$")) {
      params.setCenterViewB(0);
    }
    else {
      params.setCenterViewB(Integer.parseInt(ltfCenterViewA.getText()));
    }
    if (rbSearchBoth.isSelected()) {
      params.setSearchDirection(0);
    }
    if (rbSearchPlus90.isSelected()) {
      params.setSearchDirection(1);
    }
    if (rbSearchMinus90.isSelected()) {
      params.setSearchDirection(-1);
    }

  }

  public Container getContainer() {
    return panelTransferfid;
  }

  public void setAdvanced(boolean isAdvanced) {
    ltfCenterViewA.setVisible(isAdvanced);
    ltfCenterViewB.setVisible(isAdvanced);
    panelSearchDirection.setVisible(isAdvanced);
  }
}
