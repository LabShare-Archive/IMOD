package etomo.ui;

import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BoxLayout;

import etomo.BaseManager;
import etomo.storage.LogFile;
import etomo.storage.MatlabParam;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.ConstPeetMetaData;
import etomo.type.EtomoAutodoc;
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
 * <p> $Log$ </p>
 */
final class CylinderOrientationPanel {
  public static final String rcsid = "$Id$";

  private static final String MASK_CYLINDER_LABEL = "Cylinder Orientation";
  private static final String PARTICLE_LABEL = "Particle #";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final Spinner sMaskModelPtsModelNumber = Spinner
      .getLabeledInstance("Model #: ");
  private final LabeledTextField ltfMaskModelPtsParticle = new LabeledTextField(
      PARTICLE_LABEL + ": ");

  private final BaseManager manager;
  private final CylinderOrientationParent parent;

  private CylinderOrientationPanel(BaseManager manager,
      CylinderOrientationParent parent) {
    this.manager = manager;
    this.parent = parent;
  }

  static CylinderOrientationPanel getInstance(BaseManager manager,
      CylinderOrientationParent parent) {
    CylinderOrientationPanel instance = new CylinderOrientationPanel(manager,
        parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
  }

  private void createPanel() {
    //root panel
    pnlRoot.setBoxLayout(BoxLayout.X_AXIS);
    pnlRoot.setBorder(new EtchedBorder(MASK_CYLINDER_LABEL).getBorder());
    pnlRoot.add(sMaskModelPtsModelNumber.getContainer());
    pnlRoot.add(ltfMaskModelPtsParticle.getContainer());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  void getParameters(final PeetMetaData metaData) {
    metaData.setMaskUseReferenceParticle(sMaskModelPtsModelNumber.isEnabled());
    metaData.setMaskModelPtsModelNumber(sMaskModelPtsModelNumber.getValue());
    metaData.setMaskModelPtsParticle(ltfMaskModelPtsParticle.getText());
  }

  /**
   * Set parameters from metaData.
   * @param metaData
   */
  void setParameters(final ConstPeetMetaData metaData) {
    sMaskModelPtsModelNumber.setValue(metaData.getMaskModelPtsModelNumber());
    ltfMaskModelPtsParticle.setText(metaData.getMaskModelPtsParticle());
  }

  /**
   * Load data from MatlabParamFile.
   * @param matlabParamFile
   */
  void setParameters(final MatlabParam matlabParam) {
    if (!matlabParam.isMaskModelPtsEmpty()) {
      sMaskModelPtsModelNumber.setValue(matlabParam.getMaskModelPtsVolume());
      ltfMaskModelPtsParticle.setText(matlabParam.getMaskModelPtsParticle());
    }
  }

  void getParameters(final MatlabParam matlabParam) {
    if (parent.isMaskTypeCylinderSelected() && parent.isReferenceFileSelected()) {
      matlabParam.setMaskModelPtsVolume(sMaskModelPtsModelNumber.getValue());
      matlabParam.setMaskModelPtsParticle(ltfMaskModelPtsParticle.getText());
    }
    else {
      matlabParam.clearMaskModelPts();
    }
  }

  String validateRun() {
    //if Cylinder is selected and reference file is in use, then Particle # is
    //required.
    if (parent.isMaskTypeCylinderSelected() && parent.isReferenceFileSelected()
        && ltfMaskModelPtsParticle.isEnabled()
        && ltfMaskModelPtsParticle.isEmpty()) {
      return "In " + MASK_CYLINDER_LABEL + ", " + PARTICLE_LABEL
          + " is required when " + MaskingPanel.MASK_TYPE_CYLINDER_LABEL + " "
          + MaskingPanel.MASK_TYPE_LABEL + " is selected and "
          + ReferencePanel.REFERENCE_FILE_LABEL + " is selected. ";
    }
    return null;
  }

  /**
   * Enabled/disables fields.
   */
  public void updateDisplay() {
    boolean cylinder = parent.isMaskTypeCylinderSelected();
    sMaskModelPtsModelNumber.setEnabled(cylinder
        && parent.isReferenceFileSelected());
    sMaskModelPtsModelNumber.setMax(parent.getVolumeTableSize());
    ltfMaskModelPtsParticle.setEnabled(cylinder
        && parent.isReferenceFileSelected());
  }

  private void setTooltips() {
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
          AutodocFactory.PEET_PRM, manager.getManagerKey());
      String tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.MASK_MODEL_PTS_KEY);
      sMaskModelPtsModelNumber.setToolTipText(tooltip);
      ltfMaskModelPtsParticle.setToolTipText(tooltip);
    }
    catch (FileNotFoundException e) {
      e.printStackTrace();
    }
    catch (IOException e) {
      e.printStackTrace();
    }
    catch (LogFile.LockException e) {
      e.printStackTrace();
    }
  }
}
