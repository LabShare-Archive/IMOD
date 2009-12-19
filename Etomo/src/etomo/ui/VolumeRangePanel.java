package etomo.ui;

import java.awt.Component;
import java.awt.GridLayout;
import java.util.Vector;

import etomo.comscript.TrimvolParam;
import etomo.process.ImodProcess;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2009</p>
 *
 * <p>Organization:
 * Boulder Laboratory for 3-Dimensional Electron Microscopy of Cells (BL3DEMC),
 * University of Colorado</p>
 * 
 * @author $Author$
 * 
 * @version $Revision$
 * 
 * <p> $Log$ </p>
 */
final class VolumeRangePanel {
  public static final String rcsid = "$Id$";

  private final EtomoPanel pnlRoot = new EtomoPanel();
  private final LabeledTextField ltfXMin = new LabeledTextField("X min: ");
  private final LabeledTextField ltfXMax = new LabeledTextField("X max: ");
  private final LabeledTextField ltfYMin = new LabeledTextField("Y min: ");
  private final LabeledTextField ltfYMax = new LabeledTextField("Y max: ");
  private final LabeledTextField ltfZMin = new LabeledTextField("Z min: ");
  private final LabeledTextField ltfZMax = new LabeledTextField("Z max: ");

  private VolumeRangePanel() {
  }

  static VolumeRangePanel getInstance() {
    VolumeRangePanel instance = new VolumeRangePanel();
    instance.createPanel();
    instance.setTooltips();
    return instance;
  }

  private void createPanel() {
    //Root panel
    pnlRoot.setLayout(new GridLayout(3, 2, 5, 5));
    pnlRoot.setBorder(new EtchedBorder("Volume Range").getBorder());
    pnlRoot.add(ltfXMin.getContainer());
    pnlRoot.add(ltfXMax.getContainer());
    pnlRoot.add(ltfYMin.getContainer());
    pnlRoot.add(ltfYMax.getContainer());
    pnlRoot.add(ltfZMin.getContainer());
    pnlRoot.add(ltfZMax.getContainer());
  }

  Component getComponent() {
    return pnlRoot;
  }

  /**
   * Set the panel values with the specified parameters
   * @param trimvolParam
   */
  void setParameters(TrimvolParam trimvolParam) {
    ltfXMin.setText(trimvolParam.getXMin());
    ltfXMax.setText(trimvolParam.getXMax());
    //  Y and Z  are swapped to present the user with Z as the depth domain
    ltfYMin.setText(trimvolParam.getZMin());
    ltfYMax.setText(trimvolParam.getZMax());
    ltfZMin.setText(trimvolParam.getYMin());
    ltfZMax.setText(trimvolParam.getYMax());
  }

  /**
   * Get the parameter values from the panel 
   * @param trimvolParam
   */
  void getParameters(TrimvolParam trimvolParam) {
    trimvolParam.setXMin(ltfXMin.getText());
    trimvolParam.setXMax(ltfXMax.getText());
    //  Y and Z  are swapped to present the user with Z as the depth domain
    trimvolParam.setYMin(ltfZMin.getText());
    trimvolParam.setYMax(ltfZMax.getText());
    trimvolParam.setZMin(ltfYMin.getText());
    trimvolParam.setZMax(ltfYMax.getText());
  }

  void setXYMinAndMax(Vector coordinates) {
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
        if (index >= size) {
          return;
        }
        ltfZMin.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
        ltfZMax.setText((String) coordinates.get(index++));
        if (index >= size) {
          return;
        }
      }
    }
  }

  private void setTooltips() {
    ltfXMin
        .setToolTipText("The X coordinate on the left side to retain in the volume.");
    ltfXMax
        .setToolTipText("The X coordinate on the right side to retain in the volume.");
    ltfYMin.setToolTipText("The lower Y coordinate to retain in the volume.");
    ltfYMax.setToolTipText("The upper Y coordinate to retain in the volume.");
    ltfZMin.setToolTipText("The bottom Z slice to retain in the volume.");
    ltfZMax.setToolTipText("The top Z slice to retain in the volume.");
  }
}
