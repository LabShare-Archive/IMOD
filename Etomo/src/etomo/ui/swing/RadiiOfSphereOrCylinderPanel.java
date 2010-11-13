package etomo.ui.swing;

import java.awt.Component;

import javax.swing.BoxLayout;

import etomo.storage.MatlabParam;

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
 * <p> Revision 1.3  2009/12/23 02:26:41  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.
 * <p>
 * <p> Revision 1.2  2009/12/08 02:48:05  sueh
 * <p> bug# 1286 Made class function package private.
 * <p>
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

  private final RadiiOfSphereOrCylinderParent parent;

  private RadiiOfSphereOrCylinderPanel(RadiiOfSphereOrCylinderParent parent) {
    this.parent = parent;
  }

  static RadiiOfSphereOrCylinderPanel getInstance(
      RadiiOfSphereOrCylinderParent parent) {
    RadiiOfSphereOrCylinderPanel instance = new RadiiOfSphereOrCylinderPanel(
        parent);
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
    ltfInsideMaskRadius
        .setToolTipText("Inner radius of the mask region in pixels.");
    ltfOutsideMaskRadius
        .setToolTipText("Inner and outer radii of the mask region in pixels.");
  }
}
