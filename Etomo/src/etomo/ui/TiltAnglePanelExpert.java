package etomo.ui;

import java.awt.Component;
import java.awt.event.ActionEvent;

import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.type.UserConfiguration;

/**
 * <p>Description: </p>
 * 
 * <p>Copyright: Copyright 2006</p>
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
final class TiltAnglePanelExpert {
  public static final String rcsid = "$Id$";

  private final TiltAnglePanel panel;

  TiltAnglePanelExpert() {
    panel = new TiltAnglePanel(this);
  }
  
  Component getComponent() {
    return panel.getComponent();
  }

  void setFields(final TiltAngleSpec tiltAngleSpec, final UserConfiguration userConfig) {
    if (tiltAngleSpec.getType() == TiltAngleType.FILE
        || userConfig.getTiltAnglesRawtltFile()) {
      panel.setFile(true);
      enableAngleFields(false);
    }
    else if (tiltAngleSpec.getType() == TiltAngleType.EXTRACT) {
      panel.setExtract(true);
      enableAngleFields(false);
    }
    else if (tiltAngleSpec.getType() == TiltAngleType.RANGE) {
      panel.setSpecify(true);
      enableAngleFields(true);
    }

    panel.setMin(tiltAngleSpec.getRangeMin());
    panel.setStep(tiltAngleSpec.getRangeStep());
  }

  void enableAngleFields(final boolean enable) {
    panel.setMinEnabled(enable);
    panel.setStepEnabled(enable);
  }

  void getFields(final TiltAngleSpec tiltAngleSpec) {
    if (panel.isExtractSelected()) {
      tiltAngleSpec.setType(TiltAngleType.EXTRACT);
    }
    if (panel.isSpecifySelected()) {
      tiltAngleSpec.setType(TiltAngleType.RANGE);
    }
    if (panel.isFileSelected()) {
      tiltAngleSpec.setType(TiltAngleType.FILE);
    }
    tiltAngleSpec.setRangeMin(panel.getMin());
    tiltAngleSpec.setRangeStep(panel.getStep());
  }

  /**
   * Validates and return an error messaage.  This panel does not use
   * isValid() because it does not have enough information to display a
   * complete error message.
   * @return partial error message
   */
  String getErrorMessage() {
    if (panel.isSpecifySelected()) {
      if (panel.isMinEmpty()) {
        return new String("Starting angle cannot be empty");
      }
      if (panel.isStepEmpty()) {
        return new String("Increment cannot be empty");
      }
    }
    return null;
  }

  /**
   * Walk through all of the objects applying the appropriate state
   * @param enable
   */
  void setEnabled(final boolean enable) {
    panel.setSourceEnabled(enable);
    panel.setAngleEnabled(enable);
    panel.setExtractEnabled(enable);
    panel.setFileEnabled(enable);
    panel.setSpecifyEnabled(enable);
    enableAngleFields(panel.isSpecifySelected() & enable);
  }

  void setTooltips() {
    panel
        .setSourceTooltip("Specify the source of the view tilt angles");
    panel
        .setExtractTooltip("Select the Extract option if the extended header of the "
            + "raw data stack contains the angles");
    panel
        .setSpecifyTooltip("Select the Specify option if you wish to manually "
            + "specify the tilt angles in the edit boxes below");
    panel.setMinTooltip("Starting tilt angle of the series");
    panel.setStepTooltip("Tilt increment between views");
    panel
        .setFileTooltip("Select the File option if the tilt angles already exist "
            + "in a *.rawtlt file");
  }
  
  /**
   *  Set the state of the text fields depending upon the radio button state
   */ 
  void setRadioButtonState(final ActionEvent event) {
    enableAngleFields(event.getActionCommand().equals(
        panel.getSpecify()));
  }
}
