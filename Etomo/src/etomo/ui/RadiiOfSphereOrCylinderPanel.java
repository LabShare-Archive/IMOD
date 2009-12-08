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
import etomo.type.EtomoAutodoc;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2008</p>
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
 * <p> Revision 1.1  2009/12/02 04:42:12  sueh
 * <p> Factored RadiiOfSphereOrCylinder panel out of PeetDialog.
 * <p> </p>
 */
final class RadiiOfSphereOrCylinderPanel {
  public static final String rcsid = "$Id$";

  private static final String MASK_RADII_LABEL = "Radii of Sphere or Cylinder";
  private static final String INSIDE_MASK_RADIUS_LABEL = "Inner";
  private static final String OUTSIDE_MASK_RADIUS_LABEL = "Outer";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final LabeledTextField ltfInsideMaskRadius = new LabeledTextField(
      INSIDE_MASK_RADIUS_LABEL + ": ");
  private final LabeledTextField ltfOutsideMaskRadius = new LabeledTextField(
      OUTSIDE_MASK_RADIUS_LABEL + ": ");

  private final BaseManager manager;
  private final RadiiOfSphereOrCylinderParent parent;

  private RadiiOfSphereOrCylinderPanel(BaseManager manager,
      RadiiOfSphereOrCylinderParent parent) {
    this.manager = manager;
    this.parent = parent;
  }

  static RadiiOfSphereOrCylinderPanel getInstance(BaseManager manager,
      RadiiOfSphereOrCylinderParent parent) {
    RadiiOfSphereOrCylinderPanel instance = new RadiiOfSphereOrCylinderPanel(
        manager, parent);
    instance.createPanel();
    instance.setTooltips();
    return instance;
  }

  private void createPanel() {
    //root panel
    pnlRoot.setBoxLayout(BoxLayout.X_AXIS);
    pnlRoot.setBorder(new EtchedBorder(MASK_RADII_LABEL).getBorder());
    pnlRoot.add(ltfInsideMaskRadius.getContainer());
    pnlRoot.add(ltfOutsideMaskRadius.getContainer());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  /**
   * Load data from MatlabParamFile.
   * @param matlabParamFile
   */
  void setParameters(final MatlabParam matlabParam) {
    ltfInsideMaskRadius.setText(matlabParam.getInsideMaskRadius());
    ltfOutsideMaskRadius.setText(matlabParam.getOutsideMaskRadius());
  }

  void getParameters(final MatlabParam matlabParam) {
    matlabParam.setInsideMaskRadius(ltfInsideMaskRadius.getText());
    matlabParam.setOutsideMaskRadius(ltfOutsideMaskRadius.getText());
  }

  String validateRun() {
    //if sphere or cylinder is selected, require inner or outer or both.  
    if (((parent.isMaskTypeSphereSelected() | parent
        .isMaskTypeCylinderSelected()))
        && ltfInsideMaskRadius.isEnabled()
        && ltfInsideMaskRadius.isEmpty()
        && ltfOutsideMaskRadius.isEnabled() && ltfOutsideMaskRadius.isEmpty()) {
      return "In " + MaskingPanel.MASK_TYPE_LABEL + ", "
          + INSIDE_MASK_RADIUS_LABEL + " and/or " + OUTSIDE_MASK_RADIUS_LABEL
          + " " + MASK_RADII_LABEL + " are required when either "
          + MaskingPanel.MASK_TYPE_SPHERE_LABEL + " or "
          + MaskingPanel.MASK_TYPE_CYLINDER_LABEL + " is selected.";
    }
    return null;
  }

  /**
   * Enabled/disables fields.
   */
  void updateDisplay() {
    boolean sphere = parent.isMaskTypeSphereSelected();
    boolean cylinder = parent.isMaskTypeCylinderSelected();
    ltfInsideMaskRadius.setEnabled(sphere || cylinder);
    ltfOutsideMaskRadius.setEnabled(sphere || cylinder);
  }

  private void setTooltips() {
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
          AutodocFactory.PEET_PRM, manager.getManagerKey());
      ltfInsideMaskRadius.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.INSIDE_MASK_RADIUS_KEY));
      ltfOutsideMaskRadius.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.OUTSIDE_MASK_RADIUS_KEY));
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
