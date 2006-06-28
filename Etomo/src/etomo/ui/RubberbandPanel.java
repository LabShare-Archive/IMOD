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
import etomo.comscript.TrimvolParam;
import etomo.process.ImodManager;
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

  private static final String RUBBERBAND_BUTTON_LABEL = "Get XY Sub-Area From 3dmod";

  private final BaseManager manager;

  private final JPanel pnlRubberband = new JPanel();
  private final JPanel pnlRange = new JPanel();
  private final LabeledTextField ltfXMin = new LabeledTextField("X Min: ");
  private final LabeledTextField ltfXMax = new LabeledTextField(" X Max: ");
  private final LabeledTextField ltfYMin = new LabeledTextField("Y Min: ");
  private final LabeledTextField ltfYMax = new LabeledTextField(" Y Max: ");
  private final MultiLineButton btnRubberband = new MultiLineButton(
      RUBBERBAND_BUTTON_LABEL);

  RubberbandPanel(BaseManager manager) {
    this.manager = manager;
    pnlRubberband.setLayout(new BoxLayout(pnlRubberband, BoxLayout.Y_AXIS));
    pnlRubberband.setBorder(new EtchedBorder("Scaling from sub-area:")
        .getBorder());
    pnlRange.setLayout(new GridLayout(2, 2));
    pnlRange.add(ltfXMin.getContainer());
    pnlRange.add(ltfXMax.getContainer());
    pnlRange.add(ltfYMin.getContainer());
    pnlRange.add(ltfYMax.getContainer());
    pnlRubberband.add(pnlRange);
    pnlRubberband.add(Box.createRigidArea(FixedDim.x0_y5));
    btnRubberband.setSize();
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
      setXYMinAndMax(manager.imodGetRubberbandCoordinates(
          ImodManager.COMBINED_TOMOGRAM_KEY, AxisID.ONLY));
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

  public void getParameters(TrimvolParam trimvolParam) {
    trimvolParam.setScaleXMin(ltfXMin.getText());
    trimvolParam.setScaleXMax(ltfXMax.getText());
    trimvolParam.setScaleYMin(ltfYMin.getText());
    trimvolParam.setScaleYMax(ltfYMax.getText());
  }

  public void setParameters(TrimvolParam trimvolParam) {
    ltfXMin.setText(trimvolParam.getScaleXMin());
    ltfXMax.setText(trimvolParam.getScaleXMax());
    ltfYMin.setText(trimvolParam.getScaleYMin());
    ltfYMax.setText(trimvolParam.getScaleYMax());
  }

  private void setToolTipText() {
    String text;
    TooltipFormatter tooltipFormatter = new TooltipFormatter();
    ltfXMin.setToolTipText(tooltipFormatter.setText(
        "Minimum X coordinate on the left side to analyze for contrast range.")
        .format());
    ltfXMax
        .setToolTipText(tooltipFormatter
            .setText(
                "Maximum X coordinate on the right side to analyze for contrast range.")
            .format());
    ltfYMin.setToolTipText(tooltipFormatter.setText(
        "The lower Y coordinate to analyze for contrast range.").format());
    ltfYMax.setToolTipText(tooltipFormatter.setText(
        "The upper Y coordinate to analyze for contrast range.").format());
    btnRubberband.setToolTipText(tooltipFormatter.setText(
        "After pressing the " + RUBBERBAND_BUTTON_LABEL + " button, "
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
}
/**
 * <p> $Log$ </p>
 */
