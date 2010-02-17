package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;

import etomo.BaseManager;
import etomo.storage.MatlabParam;

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
 * <p> Revision 1.2  2009/12/23 02:27:28  sueh
 * <p> bug# 1296 Stop taking tooltips from peetprm.adoc.
 * <p>
 * <p> Revision 1.1  2009/12/08 02:49:27  sueh
 * <p> bug# 1286 Factored out of PeetDialog.
 * <p> </p>
 */
final class SphericalSamplingForThetaAndPsiPanel {
  public static final String rcsid = "$Id$";

  private static final String SPHERICAL_SAMPLING_LABEL = "Spherical Sampling for Theta and Psi";
  private static final String SAMPLE_SPHERE_FULL_LABEL = "Full sphere";
  private static final String SAMPLE_SPHERE_HALF_LABEL = "Half sphere";
  private static final String SAMPLE_INTERVAL_LABEL = "Sample interval";

  private final EtomoPanel pnlRoot = new EtomoPanel();
  private final ButtonGroup bgSampleSphere = new ButtonGroup();
  private final RadioButton rbSampleSphereNone = new RadioButton("None",
      MatlabParam.SampleSphere.NONE, bgSampleSphere);
  private final RadioButton rbSampleSphereFull = new RadioButton(
      SAMPLE_SPHERE_FULL_LABEL, MatlabParam.SampleSphere.FULL, bgSampleSphere);
  private final RadioButton rbSampleSphereHalf = new RadioButton(
      SAMPLE_SPHERE_HALF_LABEL, MatlabParam.SampleSphere.HALF, bgSampleSphere);
  private final LabeledTextField ltfSampleInterval = new LabeledTextField(
      SAMPLE_INTERVAL_LABEL + ": ");

  private final BaseManager manager;
  private final SphericalSamplingForThetaAndPsiParent parent;

  private SphericalSamplingForThetaAndPsiPanel(BaseManager manager,
      SphericalSamplingForThetaAndPsiParent parent) {
    this.manager = manager;
    this.parent = parent;
  }

  static SphericalSamplingForThetaAndPsiPanel getInstance(BaseManager manager,
      SphericalSamplingForThetaAndPsiParent parent) {
    SphericalSamplingForThetaAndPsiPanel instance = new SphericalSamplingForThetaAndPsiPanel(
        manager, parent);
    instance.createPanel();
    instance.setTooltips();
    instance.addListeners();
    return instance;
  }

  private void addListeners() {
    SphericalSamplingForThetaAndPsiActionListener actionListener = new SphericalSamplingForThetaAndPsiActionListener(
        this);
    rbSampleSphereNone.addActionListener(actionListener);
    rbSampleSphereFull.addActionListener(actionListener);
    rbSampleSphereHalf.addActionListener(actionListener);
  }

  private void createPanel() {
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    pnlRoot.setBorder(new EtchedBorder(SPHERICAL_SAMPLING_LABEL).getBorder());
    pnlRoot.add(rbSampleSphereNone.getComponent());
    pnlRoot.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlRoot.add(rbSampleSphereFull.getComponent());
    pnlRoot.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlRoot.add(rbSampleSphereHalf.getComponent());
    pnlRoot.add(Box.createRigidArea(FixedDim.x5_y0));
    pnlRoot.add(ltfSampleInterval.getContainer());
    pnlRoot.add(Box.createHorizontalGlue());
  }

  Component getComponent() {
    return pnlRoot;
  }

  /**
   * Called from parent updateDisplay(). Enabled/disables fields.
   */
  void updateDisplay() {
    ltfSampleInterval.setEnabled(!rbSampleSphereNone.isSelected());
  }

  boolean isSampleSphereNoneSelected() {
    return rbSampleSphereNone.isSelected();
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
   */
  void setParameters(final MatlabParam matlabParam) {
    MatlabParam.SampleSphere sampleSphere = matlabParam.getSampleSphere();
    if (sampleSphere == MatlabParam.SampleSphere.NONE) {
      rbSampleSphereNone.setSelected(true);
    }
    else if (sampleSphere == MatlabParam.SampleSphere.FULL) {
      rbSampleSphereFull.setSelected(true);
    }
    else if (sampleSphere == MatlabParam.SampleSphere.HALF) {
      rbSampleSphereHalf.setSelected(true);
    }
    ltfSampleInterval.setText(matlabParam.getSampleInterval());
  }

  void getParameters(final MatlabParam matlabParam) {
    matlabParam.setSampleSphere(((RadioButton.RadioButtonModel) bgSampleSphere
        .getSelection()).getEnumeratedType());
    matlabParam.setSampleInterval(ltfSampleInterval.getText());
  }

  /**
   * Reset values and set defaults.
   */
  void reset() {
    ltfSampleInterval.clear();
  }

  void setDefaults() {
    rbSampleSphereNone.setSelected(true);
  }

  /**
   * Validate for run.  Pops up error message if invalid.
   * @return true if valid, false if invalid
   */
  boolean validateRun() {
    //spherical sampling for theta and psi:
    //If full sphere or half sphere is selected, sample interval is required.
    if ((rbSampleSphereFull.isSelected() || rbSampleSphereHalf.isSelected())
        && ltfSampleInterval.isEnabled() && ltfSampleInterval.isEmpty()) {
      UIHarness.INSTANCE.openMessageDialog(manager, "In "
          + SPHERICAL_SAMPLING_LABEL + ", " + SAMPLE_INTERVAL_LABEL
          + " is required when either " + SAMPLE_SPHERE_FULL_LABEL + " or "
          + SAMPLE_SPHERE_HALF_LABEL + " is selected.", "Entry Error");
      return false;
    }
    return true;
  }

  private void action(final String actionCommand) {
    if (actionCommand.equals(rbSampleSphereNone.getActionCommand())
        || actionCommand.equals(rbSampleSphereFull.getActionCommand())
        || actionCommand.equals(rbSampleSphereHalf.getActionCommand())) {
      parent.updateDisplay();
    }
  }

  private void setTooltips() {
    rbSampleSphereNone
        .setToolTipText("Use the angular search parameters specified in the "
            + "Iteration Table for the first alignment search.");
    rbSampleSphereFull
        .setToolTipText("During the first alignment search, perform an optimized "
            + "180 degree search in Theta and 360 degree search for Psi, "
            + "ignoring the values specified in the Iteration Table for these "
            + "parameters.  Optimization prevents over-sampling near the poles "
            + "(on the X axis).  Phi Max should be set to 180 degrees for "
            + "spherical sampling.");
    rbSampleSphereHalf
        .setToolTipText("During the first alignment search, perform a optimized "
            + "search spanning 180 degrees Theta and 180 degrees in Psi, "
            + "ignoring the values specified in the Iteration Table for these "
            + "parameters.  Optimization prevents over-sampling near the poles "
            + "Phi Max should be set to 180 degrees for spherical sampling.");
    ltfSampleInterval
        .setToolTipText("The interval, in degrees, at which theta will be "
            + "sampled when using spherical sampling.  Psi will also be sampled "
            + "at this interval at the equator, and with decreasing frequency "
            + "near the poles (on the X axis).");
  }

  private static final class SphericalSamplingForThetaAndPsiActionListener
      implements ActionListener {
    private final SphericalSamplingForThetaAndPsiPanel sphericalSamplingForThetaAndPsiPanel;

    private SphericalSamplingForThetaAndPsiActionListener(
        final SphericalSamplingForThetaAndPsiPanel sphericalSamplingForThetaAndPsiPanel) {
      this.sphericalSamplingForThetaAndPsiPanel = sphericalSamplingForThetaAndPsiPanel;
    }

    public void actionPerformed(final ActionEvent event) {
      sphericalSamplingForThetaAndPsiPanel.action(event.getActionCommand());
    }
  }
}
