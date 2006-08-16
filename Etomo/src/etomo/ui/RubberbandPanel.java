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
    btnRubberband = new MultiLineButton(strings.buttonLabel);
    ltfXMin = new LabeledTextField(strings.xMinLabel);
    ltfXMax = new LabeledTextField(strings.xMaxLabel);
    ltfYMin = new LabeledTextField(strings.yMinLabel);
    ltfYMax = new LabeledTextField(strings.yMaxLabel);
    String borderLabel = panelStrings.getBorderLabel();
    if (borderLabel != null) {
      pnlRubberband.setBorder(new EtchedBorder(borderLabel).getBorder());
    }
    pnlRubberband.setLayout(new BoxLayout(pnlRubberband, BoxLayout.Y_AXIS));
    pnlRange.setLayout(new GridLayout(2, 2));
    pnlRange.add(ltfXMin.getContainer());
    pnlRange.add(ltfXMax.getContainer());
    pnlRange.add(ltfYMin.getContainer());
    pnlRange.add(ltfYMax.getContainer());
    pnlRubberband.add(pnlRange);
    if (borderLabel != null) {
      pnlRubberband.add(Box.createRigidArea(FixedDim.x0_y5));
      btnRubberband.setSize();
      btnRubberband.setAlignmentX(Component.CENTER_ALIGNMENT);
      pnlRubberband.add(btnRubberband.getComponent());
    }
    setToolTipText();
    RubberbandActionListener actionListener = new RubberbandActionListener(this);
    btnRubberband.addActionListener(actionListener);
  }

  MultiLineButton getButton() {
    return btnRubberband;
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
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    ltfXMin.setToolTipText(tooltipFormatter.setText(strings.xMinTooltip)
        .format());
    ltfXMax.setToolTipText(tooltipFormatter.setText(strings.xMaxTooltip)
        .format());
    ltfYMin.setToolTipText(tooltipFormatter.setText(strings.yMinTooltip)
        .format());
    ltfYMax.setToolTipText(tooltipFormatter.setText(strings.yMaxTooltip)
        .format());
    btnRubberband.setToolTipText(tooltipFormatter.setText(
        "After pressing the " + strings.buttonLabel + " button, "
            + "press shift-B in the ZaP window.  "
            + "Create a rubberband around the contrast range.  "
            + "Then press this button to retrieve X and Y coordinates.")
        .format());
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
    final String buttonLabel;
    final String xMinLabel;
    final String xMaxLabel;
    final String yMinLabel;
    final String yMaxLabel;
    final String xMinTooltip;
    final String xMaxTooltip;
    final String yMinTooltip;
    final String yMaxTooltip;

    private String borderLabel = null;

    Strings(String imodKey, String buttonLabel, String xMinLabel,
        String xMaxLabel, String yMinLabel, String yMaxLabel,
        String xMinTooltip, String xMaxTooltip, String yMinTooltip,
        String yMaxTooltip) {
      this.imodKey = imodKey;
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

    void setBorderLabel(String borderLabel) {
      this.borderLabel = borderLabel;
    }

    String getBorderLabel() {
      return borderLabel;
    }
  }
}
/**
 * <p> $Log$
 * <p> Revision 1.1  2006/06/28 23:29:36  sueh
 * <p> bug# 881 Panel to get X and Y scaling range using a 3dmod rubberband.
 * <p> </p>
 */
