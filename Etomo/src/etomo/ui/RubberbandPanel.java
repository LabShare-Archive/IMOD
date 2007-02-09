package etomo.ui;

import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.comscript.XYParam;
import etomo.process.ImodProcess;
import etomo.type.AxisID;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 */
public final class RubberbandPanel {
  public static final String rcsid = "$Id$";

  private final BaseManager manager;

  private final JPanel pnlRubberband = new JPanel();
  private final JPanel pnlRange = new JPanel();
  private final LabeledTextField ltfXMin;
  private final LabeledTextField ltfXMax;
  private final LabeledTextField ltfYMin;
  private final LabeledTextField ltfYMax;
  private final MultiLineButton btnRubberband;
  private final Strings strings;

  RubberbandPanel(BaseManager manager, Strings panelStrings) {
    strings = panelStrings;
    this.manager = manager;
    pnlRubberband.setBorder(new EtchedBorder(strings.borderLabel).getBorder());
    btnRubberband = new MultiLineButton(strings.buttonLabel);
    ltfXMin = new LabeledTextField(strings.xMinLabel);
    ltfXMax = new LabeledTextField(strings.xMaxLabel);
    ltfYMin = new LabeledTextField(strings.yMinLabel);
    ltfYMax = new LabeledTextField(strings.yMaxLabel);
    pnlRubberband.setLayout(new BoxLayout(pnlRubberband, BoxLayout.Y_AXIS));
    pnlRange.setLayout(new GridLayout(2, 2));
    pnlRange.add(ltfXMin.getContainer());
    pnlRange.add(ltfXMax.getContainer());
    pnlRange.add(ltfYMin.getContainer());
    pnlRange.add(ltfYMax.getContainer());
    pnlRubberband.add(pnlRange);
    btnRubberband.setSize();
    pnlRubberband.add(Box.createRigidArea(FixedDim.x0_y5));
    btnRubberband.setAlignmentX(Component.CENTER_ALIGNMENT);
    pnlRubberband.add(btnRubberband.getComponent());
    setToolTipText();
    RubberbandActionListener actionListener = new RubberbandActionListener(this);
    btnRubberband.addActionListener(actionListener);
  }

  Component getComponent() {
    return pnlRubberband;
  }

  void buttonAction(ActionEvent event) {
    String command = event.getActionCommand();
    if (command == btnRubberband.getActionCommand()) {
      setXYMinAndMax(manager.imodGetRubberbandCoordinates(strings.imodKey,
          AxisID.ONLY));
    }
  }

  public void setXYMinAndMax(Vector coordinates) {
    if (coordinates == null) {
      return;
    }
    int size = coordinates.size();
    if (size == 0) {
      return;
    }
    int index = 0;
    while (index < size) {
      if (ImodProcess.RUBBERBAND_RESULTS_STRING.equals((String) coordinates
          .get(index++))) {
        ltfXMin.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
        ltfYMin.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
        ltfXMax.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
        ltfYMax.setText((String) coordinates.get(index++));
        return;
      }
    }
  }

  void setEnabled(boolean enable) {
    ltfXMin.setEnabled(enable);
    ltfXMax.setEnabled(enable);
    ltfYMin.setEnabled(enable);
    ltfYMax.setEnabled(enable);
    btnRubberband.setEnabled(enable);
  }

  void setVisible(boolean visible) {
    pnlRubberband.setVisible(visible);
  }

  public void getParameters(XYParam xyParam) {
    xyParam.setXMin(ltfXMin.getText());
    xyParam.setXMax(ltfXMax.getText());
    xyParam.setYMin(ltfYMin.getText());
    xyParam.setYMax(ltfYMax.getText());
  }

  public void setParameters(XYParam xyParam) {
    ltfXMin.setText(xyParam.getXMin());
    ltfXMax.setText(xyParam.getXMax());
    ltfYMin.setText(xyParam.getYMin());
    ltfYMax.setText(xyParam.getYMax());
  }

  private void setToolTipText() {
    String text;
    ltfXMin.setToolTipText(strings.xMinTooltip);
    ltfXMax.setToolTipText(strings.xMaxTooltip);
    ltfYMin.setToolTipText(strings.yMinTooltip);
    ltfYMax.setToolTipText(strings.yMaxTooltip);
    btnRubberband.setToolTipText("After pressing the " + strings.buttonLabel
        + " button, " + "press shift-B in the ZaP window.  "
        + "Create a rubberband around the contrast range.  "
        + "Then press this button to retrieve X and Y coordinates.");
  }

  private static final class RubberbandActionListener implements ActionListener {
    RubberbandPanel adaptee;

    RubberbandActionListener(RubberbandPanel panel) {
      adaptee = panel;
    }

    public void actionPerformed(ActionEvent event) {
      adaptee.buttonAction(event);
    }
  }

  static class Strings {
    final String imodKey;
    final String borderLabel;
    final String buttonLabel;
    final String xMinLabel;
    final String xMaxLabel;
    final String yMinLabel;
    final String yMaxLabel;
    final String xMinTooltip;
    final String xMaxTooltip;
    final String yMinTooltip;
    final String yMaxTooltip;

    Strings(String imodKey, String borderLabel, String buttonLabel,
        String xMinLabel, String xMaxLabel, String yMinLabel, String yMaxLabel,
        String xMinTooltip, String xMaxTooltip, String yMinTooltip,
        String yMaxTooltip) {
      this.imodKey = imodKey;
      this.borderLabel = borderLabel;
      this.buttonLabel = buttonLabel;
      this.xMinLabel = xMinLabel;
      this.xMaxLabel = xMaxLabel;
      this.yMinLabel = yMinLabel;
      this.yMaxLabel = yMaxLabel;
      this.xMinTooltip = xMinTooltip;
      this.xMaxTooltip = xMaxTooltip;
      this.yMinTooltip = yMinTooltip;
      this.yMaxTooltip = yMaxTooltip;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.3  2006/08/16 22:41:46  sueh
 * <p> bug# 912 Make panel less flexible
 * <p>
 * <p> Revision 1.2  2006/08/16 18:51:56  sueh
 * <p> bug# 912 Making panel generic so it can be used for all places where a
 * <p> rubberband is used.
 * <p>
 * <p> Revision 1.1  2006/06/28 23:29:36  sueh
 * <p> bug# 881 Panel to get X and Y scaling range using a 3dmod rubberband.
 * <p> </p>
 */
