package etomo.ui.swing;

import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BoxLayout;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstPeetMetaData;
import etomo.type.EtomoAutodoc;
import etomo.type.EtomoNumber;
import etomo.type.PeetMetaData;

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
 * <p> Revision 1.2  2011/02/22 18:07:17  sueh
 * <p> bug# 1437 Reformatting.
 * <p>
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.3  2010/01/13 21:55:30  sueh
 * <p> bug# 1298 Passing parametersOnly to setParameters functions.
 * <p>
 * <p> Revision 1.2  2009/12/23 02:21:41  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.
 * <p>
 * <p> Revision 1.1  2009/12/08 02:46:12  sueh
 * <p> Factored CylinderOrientation out of PeetDialog.
 * <p> </p>
 */
final class CylinderOrientationPanel {
  public static final String rcsid = "$Id$";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final LabeledTextField ltfZRotation = new LabeledTextField("Z Rotation: ");
  private final LabeledTextField ltfYRotation = new LabeledTextField("Y Rotation: ");

  private final BaseManager manager;
  private final CylinderOrientationParent parent;

  private CylinderOrientationPanel(final BaseManager manager,
      CylinderOrientationParent parent) {
    this.manager = manager;
    this.parent = parent;
  }

  static CylinderOrientationPanel getInstance(final BaseManager manager,
      final CylinderOrientationParent parent) {
    CylinderOrientationPanel instance = new CylinderOrientationPanel(manager, parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
  }

  private void createPanel() {
    // root panel
    pnlRoot.setBoxLayout(BoxLayout.X_AXIS);
    pnlRoot.setBorder(new EtchedBorder("Manual Cylinder Orientation").getBorder());
    pnlRoot.add(ltfZRotation);
    pnlRoot.add(ltfYRotation);
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  void getParameters(final PeetMetaData metaData) {
    metaData.setMaskModelPtsZRotation(ltfZRotation.getText());
    metaData.setMaskModelPtsYRotation(ltfYRotation.getText());
  }

  /**
   * Set parameters from metaData.
   * @param metaData
   */
  void setParameters(final ConstPeetMetaData metaData) {
    ltfZRotation.setText(metaData.getMaskModelPtsZRotation());
    ltfYRotation.setText(metaData.getMaskModelPtsYRotation());
  }

  /**
   * Load data from MatlabParamFile.
   * @param matlabParamFile
   */
  void setParameters(final MatlabParam matlabParam) {
    if (!matlabParam.isMaskModelPtsEmpty()) {
      ltfZRotation.setText(matlabParam.getMaskModelPtsZRotation());
      ltfYRotation.setText(matlabParam.getMaskModelPtsYRotation());
    }
  }

  void getParameters(final MatlabParam matlabParam) {
    if (parent.isMaskTypeCylinderSelected()) {
      matlabParam.setMaskModelPtsZRotation(ltfZRotation.getText());
      matlabParam.setMaskModelPtsYRotation(ltfYRotation.getText());
    }
    else {
      matlabParam.clearMaskModelPts();
    }
  }

  String validateRun() {
    if (!parent.isMaskTypeCylinderSelected()) {
      return null;
    }
    // validate Z Rotation
    EtomoNumber rotation = new EtomoNumber(EtomoNumber.Type.FLOAT);
    rotation.set(ltfZRotation.getText());
    String description = ltfZRotation.getLabel() + " field in " + pnlRoot.getName();
    if (!rotation.isValid()) {
      return description + " must be numeric - " + rotation.getInvalidReason() + ".";

    }
    if (!rotation.isNull()) {
      float fZRotation = Math.abs(rotation.getFloat());
      if (fZRotation < 0 || fZRotation > 90) {
        return "Valid values for " + description + " are 0 to 90.";
      }
    }
    // validate Y Rotation
    rotation.set(ltfZRotation.getText());
    if (!rotation.isValid()) {
      return ltfYRotation.getLabel() + " field in " + pnlRoot.getName()
          + " must be numeric - " + rotation.getInvalidReason() + ".";
    }
    return null;
  }

  /**
   * Enabled/disables fields.
   */
  public void updateDisplay() {
    boolean cylinder = parent.isMaskTypeCylinderSelected();
    ltfZRotation.setEnabled(cylinder);
    ltfYRotation.setEnabled(cylinder);
  }

  private void setTooltips() {
    ReadOnlyAutodoc autodoc = null;
    try {
      autodoc = AutodocFactory.getInstance(manager, AutodocFactory.PEET_PRM, AxisID.ONLY);
    }
    catch (FileNotFoundException except) {
      except.printStackTrace();
    }
    catch (IOException except) {
      except.printStackTrace();
    }
    catch (LogFile.LockException except) {
      except.printStackTrace();
    }
    String tooltip = EtomoAutodoc.getTooltip(autodoc, "maskModelPts");
    ltfZRotation.setToolTipText(tooltip);
    ltfYRotation.setToolTipText(tooltip);
  }
}
