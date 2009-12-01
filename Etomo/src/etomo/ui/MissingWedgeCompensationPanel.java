package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
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
final class MissingWedgeCompensationPanel {
  public static final String rcsid = "$Id$";

  private static final String MISSING_WEDGE_COMPENSATION_LABEL = "Missing Wedge Compensation";
  static final String TILT_RANGE_LABEL = "Use tilt range in averaging";
  private static final String EDGE_SHIFT_LABEL = "Edge shift";
  static final String FLG_WEDGE_WEIGHT_LABEL = "Use tilt range in alignment";
  private static final String N_WEIGHT_GROUP_LABEL = "# of weight groups for equalizing CCCs: ";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final CheckBox cbTiltRange = new CheckBox(TILT_RANGE_LABEL);
  private final LabeledTextField ltfEdgeShift = new LabeledTextField(
      EDGE_SHIFT_LABEL + ": ");
  private final CheckBox cbFlgWedgeWeight = new CheckBox(FLG_WEDGE_WEIGHT_LABEL);
  private final CheckBox cbNWeightGroup = new CheckBox(N_WEIGHT_GROUP_LABEL);
  private final Spinner sNWeightGroup = Spinner.getInstance(
      N_WEIGHT_GROUP_LABEL, MatlabParam.N_WEIGHT_GROUP_DEFAULT,
      MatlabParam.N_WEIGHT_GROUP_MIN, 20);

  private final MissingWedgeCompensationParent parent;
  private final BaseManager manager;

  private MissingWedgeCompensationPanel(MissingWedgeCompensationParent parent,
      BaseManager manager) {
    this.manager = manager;
    this.parent = parent;
  }

  static MissingWedgeCompensationPanel getInstance(
      MissingWedgeCompensationParent parent, BaseManager manager) {
    MissingWedgeCompensationPanel instance = new MissingWedgeCompensationPanel(
        parent, manager);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    ActionListener actionListener = new MissingWedgeCompensationActionListener(
        this);
    cbTiltRange.addActionListener(actionListener);
    cbFlgWedgeWeight.addActionListener(actionListener);
    cbNWeightGroup.addActionListener(actionListener);
  }

  private void createPanel() {
    //local panels
    SpacedPanel pnlTiltRange = SpacedPanel.getInstance();
    SpacedPanel pnlFlgWedgeWeight = SpacedPanel.getInstance();
    SpacedPanel pnlNWeightGroup = SpacedPanel.getInstance();
    //Root panel
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new EtchedBorder(MISSING_WEDGE_COMPENSATION_LABEL)
        .getBorder());
    pnlRoot.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlRoot.add(pnlTiltRange);
    pnlRoot.add(pnlFlgWedgeWeight);
    pnlRoot.add(pnlNWeightGroup);
    //tiltRange and edgeShift
    pnlTiltRange.setBoxLayout(BoxLayout.X_AXIS);
    pnlTiltRange.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlTiltRange.add(cbTiltRange);
    pnlTiltRange.addRigidArea(FixedDim.x20_y0);
    ltfEdgeShift.setTextPreferredWidth(UIParameters.INSTANCE.getIntegerWidth());
    pnlTiltRange.add(ltfEdgeShift.getContainer());
    //flgWedgeWeight
    pnlFlgWedgeWeight.setBoxLayout(BoxLayout.X_AXIS);
    pnlFlgWedgeWeight.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlFlgWedgeWeight.add(cbFlgWedgeWeight);
    pnlFlgWedgeWeight.addHorizontalGlue();
    //nWeightGroup panel
    pnlNWeightGroup.setBoxLayout(BoxLayout.X_AXIS);
    pnlNWeightGroup.add(cbNWeightGroup);
    pnlNWeightGroup.add(sNWeightGroup.getContainer());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  public void getParameters(final PeetMetaData metaData) {
    metaData.setEdgeShift(ltfEdgeShift.getText());
    metaData.setFlgWedgeWeight(cbFlgWedgeWeight.isSelected());
    metaData.setUseNWeightGroup(cbNWeightGroup.isSelected());
    metaData.setNWeightGroup(sNWeightGroup.getValue());
    metaData.setTiltRange(cbTiltRange.isSelected());
  }

  /**
   * Set parameters from metaData and then overwrite them with parameters from
   * MatlabParamFile.  This allows inactive data to appear on the screen but
   * allows MatlabParamFile's active data to override active metaData.  So if
   * the user changes the .prm file, the active data on the screen will be
   * correct.
   * @param metaData
   */
  public void setParameters(final ConstPeetMetaData metaData,
      boolean parametersOnly) {
    if (!parametersOnly) {
      cbFlgWedgeWeight.setSelected(metaData.isFlgWedgeWeight());
    }
    ltfEdgeShift.setText(metaData.getEdgeShift());
    cbNWeightGroup.setSelected(metaData.isUseNWeightGroup());
    //backwards compatibility - raised nWeightGroup minimum from 0 to 2
    int nWeightGroup = metaData.getNWeightGroup().getInt();
    if (nWeightGroup < MatlabParam.N_WEIGHT_GROUP_MIN) {
      nWeightGroup = MatlabParam.N_WEIGHT_GROUP_MIN;
    }
    sNWeightGroup.setValue(nWeightGroup);
    //Distinguish between what is set in the tilt range numbers and the check
    //box by saving them separately.  This works better then trying to tell the
    //difference between [] and {} in the .prm file.
    cbTiltRange.setSelected(metaData.isTiltRange());
  }

  /**
   * Load data from MatlabParamFile.
   * @param matlabParamFile
   */
  public void setParameters(final MatlabParam matlabParam) {
    //Backwards compatibility:  if the tilt range has numbers in it, then check
    //cbTiltRange.  If not, rely on the new variable in MetaData.
    if (!matlabParam.isTiltRangeEmpty()) {
      cbTiltRange.setSelected(true);
    }
    if (cbTiltRange.isSelected()) {
      ltfEdgeShift.setText(matlabParam.getEdgeShift());
      cbFlgWedgeWeight.setSelected(matlabParam.isFlgWedgeWeight());
    }
    if (isFlgWedgeWeightEnabled()) {
      cbNWeightGroup.setSelected(!matlabParam.isNWeightGroupEmpty());
    }
    if (isFlgWedgeWeightEnabled() && cbNWeightGroup.isSelected()) {
      sNWeightGroup.setValue(matlabParam.getNWeightGroup());
    }
  }

  private boolean isFlgWedgeWeightEnabled() {
    return !parent.isVolumeTableEmpty() && cbTiltRange.isSelected()
        && cbFlgWedgeWeight.isSelected()
        && parent.isReferenceParticleSelected();
  }

  boolean isTiltRangeSelected() {
    return cbTiltRange.isSelected();
  }

  public void getParameters(final MatlabParam matlabParam) {
    //If cbTiltRange is off, this overrides what was set in the volumeTable.
    if (!cbTiltRange.isSelected()) {
      matlabParam.setTiltRangeEmpty();
    }
    if (ltfEdgeShift.isEnabled()) {
      matlabParam.setEdgeShift(ltfEdgeShift.getText());
    }
    if (cbFlgWedgeWeight.isEnabled()) {
      matlabParam.setFlgWedgeWeight(cbFlgWedgeWeight.isSelected());
    }
    if (sNWeightGroup.isEnabled()) {
      matlabParam.setUseNWeightGroup(true);
      matlabParam.setNWeightGroup(sNWeightGroup.getValue());
    }
    else {
      matlabParam.setUseNWeightGroup(false);
    }
  }

  public void updateDisplay() {
    //tilt range
    ltfEdgeShift.setEnabled(cbTiltRange.isSelected());
    cbFlgWedgeWeight.setEnabled(cbTiltRange.isSelected());
    cbNWeightGroup.setEnabled(isFlgWedgeWeightEnabled());
    sNWeightGroup.setEnabled(isFlgWedgeWeightEnabled()
        && cbNWeightGroup.isSelected());
  }

  boolean isFlgWedgeWeightSelected() {
    return cbFlgWedgeWeight.isSelected();
  }

  String validateRun() {
    //Edge shift cannot be empty if use tilt range in averaging is checked.
    if (cbTiltRange.isSelected() && ltfEdgeShift.getText().matches("\\s*")) {
      return "In " + MISSING_WEDGE_COMPENSATION_LABEL + ", " + EDGE_SHIFT_LABEL
          + " is required when " + TILT_RANGE_LABEL + " is selected.";
    }
    return null;
  }

  private void action(final String actionCommand) {
    if (actionCommand.equals(cbTiltRange.getActionCommand())
        || actionCommand.equals(cbFlgWedgeWeight.getActionCommand())
        || actionCommand.equals(cbNWeightGroup.getActionCommand())) {
      parent.updateDisplay();
    }
  }

  /**
   * Reset values and set defaults.
   */
  public void reset() {
    cbTiltRange.setSelected(false);
    ltfEdgeShift.clear();
    cbFlgWedgeWeight.setSelected(false);
    cbNWeightGroup.setSelected(false);
    sNWeightGroup.reset();
  }

  void setDefaults() {
    ltfEdgeShift.setText(MatlabParam.EDGE_SHIFT_DEFAULT);
    sNWeightGroup.setValue(MatlabParam.N_WEIGHT_GROUP_DEFAULT);
  }

  private void setTooltips() {
    try {
      ReadOnlyAutodoc autodoc = AutodocFactory.getInstance(
          AutodocFactory.PEET_PRM, manager.getManagerKey());
      cbTiltRange.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.TILT_RANGE_KEY));
      ltfEdgeShift.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.EDGE_SHIFT_KEY));
      cbFlgWedgeWeight.setToolTipText(EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.FLG_WEDGE_WEIGHT_KEY));
      String tooltip = EtomoAutodoc.getTooltip(autodoc,
          MatlabParam.N_WEIGHT_GROUP_KEY);
      cbNWeightGroup.setToolTipText(tooltip);
      sNWeightGroup.setToolTipText(tooltip);
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
