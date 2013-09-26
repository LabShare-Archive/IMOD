package etomo.ui.swing;

import java.awt.Component;
import java.awt.GridLayout;
import java.util.Vector;

import etomo.comscript.TrimvolParam;
import etomo.process.ImodProcess;
import etomo.type.ConstMetaData;
import etomo.type.MetaData;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

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
 * <p> $Log$
 * <p> Revision 1.2  2011/02/22 21:47:44  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.1  2009/12/19 01:21:03  sueh
 * <p> bug# 1294 Panel factored out of TrimVolPanel.
 * <p> </p>
 */
final class VolumeRangePanel {
  public static final String rcsid = "$Id$";

  private final EtomoPanel pnlRoot = new EtomoPanel();
  private final LabeledTextField ltfXMin = new LabeledTextField(FieldType.INTEGER,
      "X min: ");
  private final LabeledTextField ltfXMax = new LabeledTextField(FieldType.INTEGER,
      "X max: ");
  private final LabeledTextField ltfYMin = new LabeledTextField(FieldType.INTEGER,
      "Y min: ");
  private final LabeledTextField ltfYMax = new LabeledTextField(FieldType.INTEGER,
      "Y max: ");
  private final LabeledTextField ltfZMin = new LabeledTextField(FieldType.INTEGER,
      "Z min: ");
  private final LabeledTextField ltfZMax = new LabeledTextField(FieldType.INTEGER,
      "Z max: ");

  /**
   * When lockPanel is true, do not load default, metaData, or comscript values, and do
   * not save to metaData or comscripts.  LockPanel is set when the data to create this
   * panel correctly is not available.  Saving data at this point may prevent the panel
   * from being created correctly when the data is available.
   */
  private final boolean lockPanel;

  private VolumeRangePanel(final boolean lockPanel) {
    this.lockPanel = lockPanel;
  }

  static VolumeRangePanel getInstance(final boolean lockPanel) {
    VolumeRangePanel instance = new VolumeRangePanel(lockPanel);
    instance.createPanel();
    instance.setTooltips();
    return instance;
  }

  private void createPanel() {
    // Root panel
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
  void setParameters(final TrimvolParam param) {
    if (lockPanel) {
      return;
    }
    ltfXMin.setText(param.getXMin());
    ltfXMax.setText(param.getXMax());
    ltfYMin.setText(param.getYMin());
    ltfYMax.setText(param.getYMax());
    ltfZMin.setText(param.getZMin());
    ltfZMax.setText(param.getZMax());
  }

  /**
   * Set the panel values with the specified parameters
   * @param trimvolParam
   */
  void setParameters(final ConstMetaData metaData) {
    if (lockPanel) {
      return;
    }
    ltfXMin.setText(metaData.getPostTrimvolXMin());
    ltfXMax.setText(metaData.getPostTrimvolXMax());
    ltfYMin.setText(metaData.getPostTrimvolYMin());
    ltfYMax.setText(metaData.getPostTrimvolYMax());
    ltfZMin.setText(metaData.getPostTrimvolZMin());
    ltfZMax.setText(metaData.getPostTrimvolZMax());
  }

  void getParameters(MetaData metaData) {
    if (lockPanel) {
      return;
    }
    metaData.setPostTrimvolXMin(ltfXMin.getText());
    metaData.setPostTrimvolXMax(ltfXMax.getText());
    metaData.setPostTrimvolYMin(ltfYMin.getText());
    metaData.setPostTrimvolYMax(ltfYMax.getText());
    metaData.setPostTrimvolZMin(ltfZMin.getText());
    metaData.setPostTrimvolZMax(ltfZMax.getText());
  }

  void getParametersForTrimvol(final MetaData metaData) {
    if (lockPanel) {
      return;
    }
    metaData.setPostTrimvolNewStyleZ(ltfZMin.getText(), ltfZMax.getText());
  }

  /**
   * Get the parameter values from the panel 
   * @param trimvolParam
   */
  boolean getParameters(TrimvolParam trimvolParam, final boolean doValidation) {
    if (lockPanel) {
      return true;
    }
    try {
      trimvolParam.setXMin(ltfXMin.getText(doValidation));
      trimvolParam.setXMax(ltfXMax.getText(doValidation));
      trimvolParam.setYMin(ltfYMin.getText(doValidation));
      trimvolParam.setYMax(ltfYMax.getText(doValidation));
      trimvolParam.setZMin(ltfZMin.getText(doValidation));
      trimvolParam.setZMax(ltfZMax.getText(doValidation));
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
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
      if (ImodProcess.RUBBERBAND_RESULTS_STRING.equals((String) coordinates.get(index++))) {
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
    ltfXMin.setToolTipText("The X coordinate on the left side to retain in the volume.");
    ltfXMax.setToolTipText("The X coordinate on the right side to retain in the volume.");
    ltfYMin.setToolTipText("The lower Y coordinate to retain in the volume.");
    ltfYMax.setToolTipText("The upper Y coordinate to retain in the volume.");
    ltfZMin.setToolTipText("The bottom Z slice to retain in the volume.");
    ltfZMax.setToolTipText("The top Z slice to retain in the volume.");
  }
}
