package etomo.ui;

import java.awt.Container;
import java.awt.Color;
import javax.swing.*;
import javax.swing.JCheckBox;
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
  private LabeledTextField ltfCenterView = new LabeledTextField("Center view: ");

  private JPanel panelSearchDirection = new JPanel();
  private ButtonGroup bgSearchDirection = new ButtonGroup();
  private JRadioButton rbSearchBoth = new JRadioButton("Both directions");
  private JRadioButton rbSearchPlus90 = new JRadioButton("+90 (CCW) only");
  private JRadioButton rbSearchMinus90 = new JRadioButton("-90 (CW) only");

  private String logSuffix;

  public TransferfidPanel(String suffix) {

    logSuffix = suffix;

    panelTransferfid.setLayout(
      new BoxLayout(panelTransferfid, BoxLayout.Y_AXIS));
    panelTransferfid.setBorder(new TitledBorder(
        BorderFactory.createEtchedBorder(highlight, shadow),
        "Transferfid Parameters"));
    panelTransferfid.add(chkRunMidas);
    panelTransferfid.add(ltfCenterView.getContainer());

    bgSearchDirection.add(rbSearchBoth);
    bgSearchDirection.add(rbSearchPlus90);
    bgSearchDirection.add(rbSearchMinus90);
    panelSearchDirection.setLayout(
      new BoxLayout(panelSearchDirection, BoxLayout.Y_AXIS));
    panelSearchDirection.setBorder(new TitledBorder(
        BorderFactory.createEtchedBorder(highlight, shadow),
        "Search Direction"));
    panelSearchDirection.add(rbSearchBoth);
    panelSearchDirection.add(rbSearchPlus90);
    panelSearchDirection.add(rbSearchMinus90);
    panelTransferfid.add(panelSearchDirection);
  }

  /**
   * Set the values of the panel using a TransferfidParam parameter
   * object
   */
  public void setParameters(TransferfidParam params) {
    chkRunMidas.setSelected(params.isRunMidas());
  }

  /**
   * Get the values from the panel filling in the TransferfidParam object
   */
  public void getParameters(TransferfidParam params) {
    params.setRunMidas(chkRunMidas.isSelected());

  }

  public Container getContainer() {
    return panelTransferfid;
  }

}
