package etomo.ui.swing;

import java.awt.Component;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.comscript.ConstTiltParam;
import etomo.comscript.TiltParam;

/**
* <p>Description: </p>
* 
* <p>Copyright: Copyright 2010</p>
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
* <p> Revision 1.1  2010/12/05 05:16:07  sueh
* <p> bug# 1416 Moved radial filter fields to RadialPanel so they can be reused
* <p> in SirtPanel.
* <p> </p>
*/
final class RadialPanel {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final LabeledTextField ltfRadialMax = new LabeledTextField(
      "Radial filter cutoff: ");
  private final LabeledTextField ltfRadialFallOff = new LabeledTextField(" Falloff: ");

  private RadialPanel() {
  }

  static RadialPanel getInstance() {
    RadialPanel instance = new RadialPanel();
    instance.createPanel();
    instance.setTooltips();
    return instance;
  }

  private void createPanel() {
    pnlRoot.setLayout(new BoxLayout(pnlRoot, BoxLayout.X_AXIS));
    pnlRoot.add(ltfRadialMax.getContainer());
    pnlRoot.add(ltfRadialFallOff.getContainer());
    UIUtilities.alignComponentsX(pnlRoot, Component.LEFT_ALIGNMENT);
  }

  Component getRoot() {
    return pnlRoot;
  }

  void setEnabled(boolean enable) {
    updateDisplay(enable);
  }

  private void updateDisplay(boolean enable) {
    ltfRadialMax.setEnabled(enable);
    ltfRadialFallOff.setEnabled(enable);
  }

  void setParameters(final ConstTiltParam tiltParam) {
    if (tiltParam.hasRadialWeightingFunction()) {
      ltfRadialMax.setText(tiltParam.getRadialBandwidth());
      ltfRadialFallOff.setText(tiltParam.getRadialFalloff());
    }
  }

  public void getParameters(final TiltParam tiltParam) throws NumberFormatException {
    String badParameter = "";
    try {
      if (ltfRadialMax.getText().matches("\\S+")
          || ltfRadialFallOff.getText().matches("\\S+")) {
        badParameter = ltfRadialMax.getLabel();
        tiltParam.setRadialBandwidth(Float.parseFloat(ltfRadialMax.getText()));
        badParameter = ltfRadialFallOff.getLabel();
        tiltParam.setRadialFalloff(Float.parseFloat(ltfRadialFallOff.getText()));
      }
      else {
        tiltParam.resetRadialFilter();
      }
    }
    catch (NumberFormatException except) {
      String message = badParameter + " " + except.getMessage();
      throw new NumberFormatException(message);
    }
  }

  private void setTooltips() {
    ltfRadialMax
        .setToolTipText("The spatial frequency at which to switch from the R-weighted radial "
            + "filter to a Gaussian falloff.  Frequency is in cycles/pixel and "
            + "ranges from 0-0.5.  Both a cutoff and a falloff must be entered.");
    ltfRadialFallOff
        .setToolTipText("The sigma value of a Gaussian which determines how fast the radial "
            + "filter falls off at spatial frequencies above the cutoff frequency."
            + "  Frequency is in cycles/pixel and ranges from 0-0.5.  Both a "
            + "cutoff and a falloff must be entered ");
  }
}
