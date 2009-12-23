package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JPanel;

import etomo.storage.MatlabParam;
import etomo.type.EnumeratedType;
import etomo.type.Run3dmodMenuOptions;

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
 * <p> Revision 1.2  2009/12/08 16:07:43  sueh
 * <p> bug# 1287 Added getYAxisType.
 * <p>
 * <p> Revision 1.1  2009/12/08 02:50:59  sueh
 * <p> bug# 1286 Factored out of PeetDialog.
 * <p> </p>
 */

final class YAxisTypePanel {
  public static final String rcsid = "$Id$";

  private static final String Y_AXIS_TYPE_LABEL = "Y Axis Type";
  private static final String Y_AXIS_CONTOUR_LABEL = "End points of contour";
  private static final String YAXIS_OBJECT_NUM_LABEL = "Object #";
  private static final String YAXIS_CONTOUR_NUM_LABEL = "Contour #";

  private final SpacedPanel pnlRoot = SpacedPanel.getInstance();
  private final ButtonGroup bgYAxisType = new ButtonGroup();
  private final RadioButton rbYAxisTypeYAxis = new RadioButton(
      "Original Y axis", MatlabParam.YAxisType.Y_AXIS, bgYAxisType);
  private final RadioButton rbYAxisTypeParticleModel = new RadioButton(
      "Particle model points", MatlabParam.YAxisType.PARTICLE_MODEL,
      bgYAxisType);
  private final RadioButton rbYAxisTypeContour = new RadioButton(
      Y_AXIS_CONTOUR_LABEL + ":  ", MatlabParam.YAxisType.CONTOUR, bgYAxisType);
  private final LabeledTextField ltfYaxisObjectNum = new LabeledTextField(" "
      + YAXIS_OBJECT_NUM_LABEL + ": ");
  private final LabeledTextField ltfYaxisContourNum = new LabeledTextField(" "
      + YAXIS_CONTOUR_NUM_LABEL + ": ");

  private final YAxisTypeParent parent;

  private YAxisTypePanel(YAxisTypeParent parent) {
    this.parent = parent;
  }

  static YAxisTypePanel getInstance(YAxisTypeParent parent) {
    YAxisTypePanel instance = new YAxisTypePanel(parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    YAxisTypeActionListener actionListener = new YAxisTypeActionListener(this);
    rbYAxisTypeYAxis.addActionListener(actionListener);
    rbYAxisTypeParticleModel.addActionListener(actionListener);
    rbYAxisTypeContour.addActionListener(actionListener);
  }

  private void createPanel() {
    //local panels
    JPanel pnlYAxisContour = new JPanel();
    //YaxisType
    pnlRoot.setBoxLayout(BoxLayout.Y_AXIS);
    pnlRoot.setBorder(new EtchedBorder(Y_AXIS_TYPE_LABEL).getBorder());
    pnlRoot.setComponentAlignmentX(Component.LEFT_ALIGNMENT);
    pnlRoot.add(rbYAxisTypeYAxis);
    pnlRoot.add(rbYAxisTypeParticleModel);
    pnlRoot.add(pnlYAxisContour);
    //YaxisContour
    pnlYAxisContour.setLayout(new BoxLayout(pnlYAxisContour, BoxLayout.X_AXIS));
    pnlYAxisContour.add(rbYAxisTypeContour.getComponent());
    ltfYaxisObjectNum.setTextPreferredWidth(UIParameters.INSTANCE
        .getIntegerWidth());
    pnlYAxisContour.add(ltfYaxisObjectNum.getContainer());
    ltfYaxisContourNum.setTextPreferredWidth(UIParameters.INSTANCE
        .getIntegerWidth());
    pnlYAxisContour.add(ltfYaxisContourNum.getContainer());
  }

  Component getComponent() {
    return pnlRoot.getContainer();
  }

  /**
   * Called from parent.updateDisplay(). Enabled/disables fields.
   */
  void updateDisplay() {
    boolean volumeRows = !parent.isVolumeTableEmpty();
    rbYAxisTypeContour.setEnabled(volumeRows);
    ltfYaxisObjectNum.setEnabled(volumeRows && rbYAxisTypeContour.isSelected());
    ltfYaxisContourNum
        .setEnabled(volumeRows && rbYAxisTypeContour.isSelected());
  }

  /**
   * Load data from MatlabParamFile.  Load only active data after the meta data
   * has been loaded.
   * Do not rely on whether fields are enabled to make decisions in this
   * function; updateDisplay may not have been run with data in the screen.
   * When looking at the settings of dialog fields, make sure that they have
   * already been loaded in THIS function.  This is sometimes the first data-
   * loading function to be run.
   * @param matlabParamFile
   * @param paramatersOnly 
   */
  void setParameters(final MatlabParam matlabParam, boolean parametersOnly) {
    MatlabParam.YAxisType yaxisType = matlabParam.getYAxisType();
    if (yaxisType == MatlabParam.YAxisType.Y_AXIS) {
      rbYAxisTypeYAxis.setSelected(true);
    }
    else if (yaxisType == MatlabParam.YAxisType.PARTICLE_MODEL) {
      rbYAxisTypeParticleModel.setSelected(true);
    }
    else if (yaxisType == MatlabParam.YAxisType.CONTOUR) {
      rbYAxisTypeContour.setSelected(true);
    }
    if (!parametersOnly) {
      ltfYaxisObjectNum.setText(matlabParam.getYaxisObjectNum());
      ltfYaxisContourNum.setText(matlabParam.getYaxisContourNum());
    }
  }

  void getParameters(final MatlabParam matlabParam) {
    matlabParam.setYaxisType(((RadioButton.RadioButtonModel) bgYAxisType
        .getSelection()).getEnumeratedType());
    matlabParam.setYaxisObjectNum(ltfYaxisObjectNum.getText());
    matlabParam.setYaxisContourNum(ltfYaxisContourNum.getText());
  }

  EnumeratedType getYAxisType() {
    return ((RadioButton.RadioButtonModel) bgYAxisType.getSelection())
        .getEnumeratedType();
  }

  void reset() {
    ltfYaxisObjectNum.clear();
    ltfYaxisContourNum.clear();
    rbYAxisTypeYAxis.setSelected(false);
    rbYAxisTypeParticleModel.setSelected(false);
    rbYAxisTypeContour.setSelected(false);
  }

  /**
   * Validation for run.
   * @return null if valid, error message if invalid
   */
  String validateRun() {
    //If end points of contour is checked, must have object # and contour#
    if (rbYAxisTypeContour.isSelected()
        && ((ltfYaxisObjectNum.isEnabled() && ltfYaxisObjectNum.getText()
            .matches("\\s*")) || (ltfYaxisContourNum.isEnabled() && ltfYaxisContourNum
            .getText().matches("\\s*")))) {
      return "In " + Y_AXIS_TYPE_LABEL + ", " + YAXIS_OBJECT_NUM_LABEL
          + " and " + YAXIS_CONTOUR_NUM_LABEL + " are required when "
          + Y_AXIS_CONTOUR_LABEL + " is selected.";
    }
    return null;
  }

  private void action(final String actionCommand,
      final Run3dmodMenuOptions run3dmodMenuOptions) {
    if (actionCommand.equals(rbYAxisTypeYAxis.getActionCommand())
        || actionCommand.equals(rbYAxisTypeParticleModel.getActionCommand())
        || actionCommand.equals(rbYAxisTypeContour.getActionCommand())) {
      parent.updateDisplay();
    }
  }

  private void setTooltips() {
    rbYAxisTypeYAxis
        .setToolTipText("Use volume's Y axis as particle's Y axis (the phi axis of the search).");
    rbYAxisTypeParticleModel
        .setToolTipText("Use the vector between neighboring model points as the "
            + "particle's Y axis (the phi axis of the search).");
    rbYAxisTypeContour
        .setToolTipText("Use the end points of the specified contour in each "
            + "volume as the particle's Y axis (the phi axis of the search).");
    ltfYaxisObjectNum
        .setToolTipText("The number of the object used to determine the "
            + "particle's Y axis in each volume.");
    ltfYaxisContourNum
        .setToolTipText("The number of the contour used to determine the "
            + "particle's Y axis in each volume.");
  }

  private static final class YAxisTypeActionListener implements ActionListener {
    private final YAxisTypePanel yAxisTypePanel;

    private YAxisTypeActionListener(final YAxisTypePanel yAxisTypePanel) {
      this.yAxisTypePanel = yAxisTypePanel;
    }

    public void actionPerformed(final ActionEvent event) {
      yAxisTypePanel.action(event.getActionCommand(), null);
    }
  }
}
