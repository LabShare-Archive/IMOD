package etomo.ui.swing;

import java.awt.Component;
import java.io.FileNotFoundException;
import java.io.IOException;

import javax.swing.BoxLayout;
import javax.swing.JPanel;

import etomo.BaseManager;
import etomo.comscript.ConstTiltParam;
import etomo.comscript.SirtsetupParam;
import etomo.comscript.TiltParam;
import etomo.storage.LogFile;
import etomo.storage.autodoc.AutodocFactory;
import etomo.storage.autodoc.ReadOnlyAutodoc;
import etomo.type.AxisID;
import etomo.type.ConstMetaData;
import etomo.type.EtomoAutodoc;
import etomo.type.PanelId;
import etomo.ui.FieldType;
import etomo.ui.FieldValidationFailedException;

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
* <p> Revision 1.2  2011/02/12 00:24:53  sueh
* <p> bug# 1437 Reformatting.
* <p>
* <p> Revision 1.1  2010/12/05 05:16:07  sueh
* <p> bug# 1416 Moved radial filter fields to RadialPanel so they can be reused
* <p> in SirtPanel.
* <p> </p>
*/
final class RadialPanel {
  public static final String rcsid = "$Id$";

  private final JPanel pnlRoot = new JPanel();
  private final LabeledTextField ltfRadialMax = new LabeledTextField(
      FieldType.FLOATING_POINT, "Radial filter cutoff: ");
  private final LabeledTextField ltfRadialFallOff = new LabeledTextField(
      FieldType.FLOATING_POINT, " Falloff: ");

  final PanelId panelId;
  final BaseManager manager;
  final AxisID axisID;

  private RadialPanel(final BaseManager manager, final AxisID axisID,
      final PanelId panelId) {
    this.manager = manager;
    this.axisID = axisID;
    this.panelId = panelId;
  }

  static RadialPanel getInstance(final BaseManager manager, final AxisID axisID,
      final PanelId panelId) {
    RadialPanel instance = new RadialPanel(manager, axisID, panelId);
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

  void setVisible(final boolean visible) {
    pnlRoot.setVisible(visible);
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

  void setParameters(final ConstMetaData metaData) {
    ltfRadialMax.setText(metaData.getGenRadialRadius(axisID));
    ltfRadialFallOff.setText(metaData.getGenRadialSigma(axisID));
  }

  void setParameters(final ConstTiltParam tiltParam) {
    if (tiltParam.hasRadialWeightingFunction()) {
      if (ltfRadialMax.isEnabled()) {
        ltfRadialMax.setText(tiltParam.getRadialBandwidth());
      }
      if (ltfRadialFallOff.isEnabled()) {
        ltfRadialFallOff.setText(tiltParam.getRadialFalloff());
      }
    }
  }

  boolean getParameters(final SirtsetupParam param, final boolean doValidation) {
    try {
      if (ltfRadialMax.isEnabled()) {
        param.setRadiusAndSigma(0, ltfRadialMax.getText(doValidation));
      }
      if (ltfRadialFallOff.isEnabled()) {
        param.setRadiusAndSigma(1, ltfRadialFallOff.getText(doValidation));
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  void setParameters(final SirtsetupParam param) {
    if (ltfRadialMax.isEnabled()) {
      ltfRadialMax.setText(param.getRadiusAndSigma(0));
    }
    if (ltfRadialFallOff.isEnabled()) {
      ltfRadialFallOff.setText(param.getRadiusAndSigma(1));
    }
  }

  public boolean getParameters(final TiltParam tiltParam, final boolean doValidation)
      throws NumberFormatException {
    try {
      String badParameter = "";
      try {
        if (ltfRadialMax.getText(doValidation).matches("\\S+")
            || ltfRadialFallOff.getText(doValidation).matches("\\S+")) {
          badParameter = ltfRadialMax.getLabel();
          tiltParam.setRadialBandwidth(Double.parseDouble(ltfRadialMax
              .getText(doValidation)));
          badParameter = ltfRadialFallOff.getLabel();
          tiltParam.setRadialFalloff(Double.parseDouble(ltfRadialFallOff
              .getText(doValidation)));
        }
        else {
          tiltParam.resetRadialFilter();
        }
      }
      catch (NumberFormatException except) {
        String message = badParameter + " " + except.getMessage();
        throw new NumberFormatException(message);
      }
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  private void setTooltips() {
    if (panelId == PanelId.SIRTSETUP) {
      ReadOnlyAutodoc autodoc = null;
      try {
        autodoc = AutodocFactory.getInstance(manager, AutodocFactory.SIRTSETUP, axisID);
      }
      catch (FileNotFoundException except) {
        except.printStackTrace();
      }
      catch (IOException except) {
        except.printStackTrace();
      }
      catch (LogFile.LockException e) {
        e.printStackTrace();
      }
      String tooltip = EtomoAutodoc.getTooltip(autodoc,
          SirtsetupParam.RADIUS_AND_SIGMA_KEY);
      ltfRadialMax.setToolTipText(tooltip);
      ltfRadialFallOff.setToolTipText(tooltip);
    }
    else {
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
}