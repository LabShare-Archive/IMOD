package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;

import etomo.storage.MatlabParam;
import etomo.type.ConstPeetMetaData;
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
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.2  2009/12/23 02:25:54  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.
 * <p>
 * <p> Revision 1.1  2009/12/01 00:25:00  sueh
 * <p> bug# 1285 Factored out MissingWedgeCompensation.
 * <p> </p>
 */
final class MissingWedgeCompensationPanel {
  public static final String rcsid = "$Id$";

  private static final String VOLUME_SIZE_LABEL = "Volume Size";
  private static final String MISSING_WEDGE_COMPENSATION_LABEL = "Missing Wedge Compensation";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final LabeledTextField ltfVolumeSizeX = new LabeledTextField("X: ");
  private final LabeledTextField ltfVolumeSizeY = new LabeledTextField("Y: ");
  private final LabeledTextField ltfVolumeSizeZ = new LabeledTextField("Z: ");
  private final CheckBox cbMissingWedgeCompensation = new CheckBox("Enabled");
  private final Spinner sEdgeShift = Spinner.getInstance("Edge shift: ",
      MatlabParam.EDGE_SHIFT_DEFAULT, MatlabParam.EDGE_SHIFT_MIN,
      MatlabParam.EDGE_SHIFT_MAX);
  private final Spinner sNWeightGroup = Spinner.getInstance("Weight groups: ",
      MatlabParam.N_WEIGHT_GROUP_DEFAULT, MatlabParam.N_WEIGHT_GROUP_MIN,
      MatlabParam.N_WEIGHT_GROUP_MAX);
  /**
   * @deprecated
   * Replaced by cbMissingWedgeCompensation
   */
  private final CheckBox cbTiltRange = new CheckBox("Use tilt range in averaging");
  /**
   * @deprecated
   * Replaced by cbMissingWedgeCompensation
   */
  private final CheckBox cbFlgWedgeWeight = new CheckBox("Use tilt range in alignment");

  private final MissingWedgeCompensationParent parent;

  private MissingWedgeCompensationPanel(MissingWedgeCompensationParent parent) {
    this.parent = parent;
  }

  static MissingWedgeCompensationPanel getInstance(MissingWedgeCompensationParent parent) {
    MissingWedgeCompensationPanel instance = new MissingWedgeCompensationPanel(parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    ActionListener actionListener = new MissingWedgeCompensationActionListener(this);
    cbMissingWedgeCompensation.addActionListener(actionListener);
    cbTiltRange.addActionListener(actionListener);
    cbFlgWedgeWeight.addActionListener(actionListener);
  }

  private void createPanel() {
    // init
    cbTiltRange.setVisible(false);
    cbFlgWedgeWeight.setVisible(false);
    // local panels
    SpacedPanel pnlVolumeSize = SpacedPanel.getInstance();
    SpacedPanel pnlMissingWedgeCompensation = SpacedPanel.getInstance();
    SpacedPanel pnlEnabled = SpacedPanel.getInstance();
    // Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlRoot.add(pnlVolumeSize);
    pnlRoot.add(pnlMissingWedgeCompensation);
    // pnlVolumeSize
    pnlVolumeSize.setBoxLayout(BoxLayout.X_AXIS);
    pnlVolumeSize
        .setBorder(new EtchedBorder(VOLUME_SIZE_LABEL + " (Voxels)").getBorder());
    pnlVolumeSize.add(ltfVolumeSizeX.getContainer());
    pnlVolumeSize.add(ltfVolumeSizeY.getContainer());
    pnlVolumeSize.add(ltfVolumeSizeZ.getContainer());
    // pnlMissingWedgeCompensation
    pnlMissingWedgeCompensation.setBoxLayout(BoxLayout.Y_AXIS);
    pnlMissingWedgeCompensation.setBorder(new EtchedBorder(
        MISSING_WEDGE_COMPENSATION_LABEL).getBorder());
    pnlMissingWedgeCompensation.setComponentAlignmentX(Component.RIGHT_ALIGNMENT);
    pnlMissingWedgeCompensation.add(pnlEnabled);
    pnlMissingWedgeCompensation.add(cbTiltRange);
    pnlMissingWedgeCompensation.add(cbFlgWedgeWeight);
    // enabled
    pnlEnabled.setBoxLayout(BoxLayout.X_AXIS);
    pnlEnabled.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlEnabled.add(cbMissingWedgeCompensation);
    pnlEnabled.addRigidArea(FixedDim.x3_y0);
    pnlEnabled.add(sEdgeShift.getContainer());
    pnlEnabled.addRigidArea(FixedDim.x3_y0);
    pnlEnabled.add(sNWeightGroup.getContainer());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  public void getParameters(final PeetMetaData metaData) {
    metaData.setEdgeShift(sEdgeShift.getValue());
    int nWeightGroup = metaData.getNWeightGroup().getInt();
    metaData.setNWeightGroup(nWeightGroup);
    if (cbTiltRange.isVisible()) {
      metaData.setTiltRange(cbTiltRange.isSelected());
      metaData.setFlgWedgeWeight(cbFlgWedgeWeight.isSelected());
    }
    else {
      metaData.setTiltRange(cbMissingWedgeCompensation.isSelected());
      metaData.setFlgWedgeWeight(cbMissingWedgeCompensation.isSelected());
    }
  }

  /**
   * Set parameters from metaData and then overwrite them with parameters from
   * MatlabParamFile.  This allows inactive data to appear on the screen but
   * allows MatlabParamFile's active data to override active metaData.  So if
   * the user changes the .prm file, the active data on the screen will be
   * correct.
   * @param metaData
   */
  public void setParameters(final ConstPeetMetaData metaData, boolean parametersOnly) {
    cbTiltRange.setSelected(metaData.isTiltRange());
    cbFlgWedgeWeight.setSelected(metaData.isFlgWedgeWeight());
    sEdgeShift.setValue(metaData.getEdgeShift());
    sNWeightGroup.setValue(metaData.getNWeightGroup());
  }

  /**
   * Load data from MatlabParamFile.
   * @param matlabParamFile
   */
  public void setParameters(final MatlabParam matlabParam) {
    ltfVolumeSizeX.setText(matlabParam.getSzVolX());
    ltfVolumeSizeY.setText(matlabParam.getSzVolY());
    ltfVolumeSizeZ.setText(matlabParam.getSzVolZ());
    // the new checkbox is selected when when tiltRange and flgWedgeWeight are both
    // selected. Expose the tiltRange and flgWedgeWeight checkboxes if one of them is
    // selected.
    if (!matlabParam.isTiltRangeEmpty()) {
      cbTiltRange.setSelected(true);
      cbFlgWedgeWeight.setSelected(matlabParam.isFlgWedgeWeight());
    }
    cbMissingWedgeCompensation.setSelected(cbTiltRange.isSelected()
        && cbFlgWedgeWeight.isSelected());
    if (!cbMissingWedgeCompensation.isSelected()) {
      if (cbTiltRange.isSelected() || cbFlgWedgeWeight.isSelected()) {
        cbTiltRange.setVisible(true);
        cbFlgWedgeWeight.setVisible(true);
      }
    }
    if (cbMissingWedgeCompensation.isSelected()
        || (cbTiltRange.isVisible() && cbTiltRange.isSelected())) {
      sEdgeShift.setValue(matlabParam.getEdgeShift());
    }
    if (cbMissingWedgeCompensation.isSelected()
        || (cbTiltRange.isVisible() && cbFlgWedgeWeight.isSelected())) {
      sNWeightGroup.setValue(matlabParam.getNWeightGroup());
    }
    updateDisplay();
  }

  boolean isTiltRangeRequired() {
    return cbMissingWedgeCompensation.isSelected()
        || (cbTiltRange.isVisible() && cbTiltRange.isSelected());
  }

  public void getParameters(final MatlabParam matlabParam) {
    matlabParam.setSzVolX(ltfVolumeSizeX.getText());
    matlabParam.setSzVolY(ltfVolumeSizeY.getText());
    matlabParam.setSzVolZ(ltfVolumeSizeZ.getText());
    // If cbTiltRange is off, this overrides what was set in the volumeTable.
    if (!cbMissingWedgeCompensation.isSelected()
        && (!cbTiltRange.isVisible() || !cbTiltRange.isSelected())) {
      matlabParam.setTiltRangeEmpty();
    }
    matlabParam.setFlgWedgeWeight(cbMissingWedgeCompensation.isSelected()
        || (cbTiltRange.isVisible() && cbTiltRange.isSelected() && cbFlgWedgeWeight
            .isSelected()));
    if (sEdgeShift.isEnabled()) {
      matlabParam.setEdgeShift(sEdgeShift.getValue());
    }
    if (sNWeightGroup.isEnabled()) {
      matlabParam.setNWeightGroup(sNWeightGroup.getValue());
    }
  }

  public void updateDisplay() {
    sEdgeShift.setEnabled(cbMissingWedgeCompensation.isSelected()
        || (cbTiltRange.isVisible() && cbTiltRange.isSelected()));
    sNWeightGroup.setEnabled(cbMissingWedgeCompensation.isSelected()
        || (cbTiltRange.isVisible() && !parent.isVolumeTableEmpty()
            && cbTiltRange.isSelected() && cbFlgWedgeWeight.isSelected()
            && parent.isReferenceParticleSelected() && cbFlgWedgeWeight.isSelected()));
    //Get the new checkbox and the old backward compatibility checkboxes to sync to each other.
    if (cbMissingWedgeCompensation.isSelected()) {
      cbTiltRange.setSelected(true);
      cbFlgWedgeWeight.setSelected(true);
    }
    else if (cbTiltRange.isSelected() && cbFlgWedgeWeight.isSelected()) {
      cbTiltRange.setSelected(false);
      cbFlgWedgeWeight.setSelected(false);
    }
    cbFlgWedgeWeight.setEnabled(cbTiltRange.isSelected());
  }

  /**
   * Called when the old checkboxes are changed.
   */
  public void updateDisplayBackwardCompatibility() {
    cbMissingWedgeCompensation.setSelected(cbTiltRange.isVisible()
        && cbTiltRange.isSelected() && cbFlgWedgeWeight.isSelected());
    parent.updateDisplay();
  }

  String validateRun() {
    // particle volume
    if (ltfVolumeSizeX.isEmpty()) {
      return "In " + VOLUME_SIZE_LABEL + ", " + ltfVolumeSizeX.getLabel()
          + " is required.";
    }
    if (ltfVolumeSizeY.isEmpty()) {
      return "In " + VOLUME_SIZE_LABEL + ", " + ltfVolumeSizeY.getLabel()
          + " is required.";
    }
    if (ltfVolumeSizeZ.isEmpty()) {
      return "In " + VOLUME_SIZE_LABEL + ", " + ltfVolumeSizeZ.getLabel()
          + " is required.";
    }
    return null;
  }

  private void action(final String actionCommand) {
    if (actionCommand.equals(cbMissingWedgeCompensation.getActionCommand())) {
      parent.updateDisplay();
    }
    if (actionCommand.equals(cbTiltRange.getActionCommand())
        || actionCommand.equals(cbFlgWedgeWeight.getActionCommand())) {
      updateDisplayBackwardCompatibility();
    }
  }

  /**
   * Reset values and set defaults.
   */
  public void reset() {
    ltfVolumeSizeX.clear();
    ltfVolumeSizeY.clear();
    ltfVolumeSizeZ.clear();
    cbMissingWedgeCompensation.setSelected(false);
    cbTiltRange.setSelected(false);
    sEdgeShift.reset();
    cbFlgWedgeWeight.setSelected(false);
    sNWeightGroup.reset();
  }

  void setDefaults() {
    sEdgeShift.setValue(MatlabParam.EDGE_SHIFT_DEFAULT);
    sNWeightGroup.setValue(MatlabParam.N_WEIGHT_GROUP_DEFAULT);
  }

  private void setTooltips() {
    String tooltip = "The size of the volume around each particle to excise and "
        + "average.";
    ltfVolumeSizeX.setToolTipText(tooltip);
    ltfVolumeSizeY.setToolTipText(tooltip);
    ltfVolumeSizeZ.setToolTipText(tooltip);
    cbTiltRange.setToolTipText("Use the tilt range(s) specified in the volume table for "
        + "missing wedge compensation during averaging.");
    cbFlgWedgeWeight
        .setToolTipText("Use the tilt range(s) specified in the volume table for "
            + "missing wedge compensation during alignment.");
    cbMissingWedgeCompensation
        .setToolTipText("Use the tilt range(s) specified in the volume table for "
            + "missing wedge compensation during averaging.  Use the tilt range(s) "
            + "specified in the volume table for missing wedge compensation during "
            + "alignment.");
    sNWeightGroup.setToolTipText("Number of groups to use for equalizing median cross-"
        + "correlation coefficient between groups.  Set to 0 or 1 to turn off.");
    sEdgeShift.setToolTipText("Number of pixels to shift the edge of the wedge mask to "
        + "include frequency information just inside the missing wedge.");
  }

  private static final class MissingWedgeCompensationActionListener implements
      ActionListener {
    private final MissingWedgeCompensationPanel panel;

    private MissingWedgeCompensationActionListener(
        final MissingWedgeCompensationPanel panel) {
      this.panel = panel;
    }

    public void actionPerformed(final ActionEvent event) {
      panel.action(event.getActionCommand());
    }
  }
}
