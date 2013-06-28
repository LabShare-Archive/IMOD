package etomo.ui.swing;

import java.awt.Component;
import java.awt.event.ActionEvent;

import etomo.BaseManager;
import etomo.logic.DatasetTool;
import etomo.storage.DirectiveFileCollection;
import etomo.type.AxisID;
import etomo.type.TiltAngleSpec;
import etomo.type.TiltAngleType;
import etomo.type.UserConfiguration;
import etomo.ui.FieldValidationFailedException;

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
 * <p> $Log$
 * <p> Revision 1.1  2010/11/13 16:07:34  sueh
 * <p> bug# 1417 Renamed etomo.ui to etomo.ui.swing.
 * <p>
 * <p> Revision 1.1  2007/12/26 22:36:34  sueh
 * <p> bug# 1052 Turned TiltAnglePanel into an extremely thin GUI.  Moved decisions
 * <p> and knowledge to TiltAnglePanelExpert.
 * <p> </p>
 */
final class TiltAnglePanelExpert {
  public static final String rcsid = "$Id$";

  final BaseManager manager;
  final AxisID axisID;

  private final TiltAnglePanel panel;

  TiltAnglePanelExpert(final BaseManager manager, final AxisID axisID) {
    this.manager = manager;
    this.axisID = axisID;
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

  boolean getFields(final TiltAngleSpec tiltAngleSpec, final boolean doValidation) {
    try {
      if (panel.isExtractSelected()) {
        tiltAngleSpec.setType(TiltAngleType.EXTRACT);
      }
      if (panel.isSpecifySelected()) {
        tiltAngleSpec.setType(TiltAngleType.RANGE);
      }
      if (panel.isFileSelected()) {
        tiltAngleSpec.setType(TiltAngleType.FILE);
      }
      tiltAngleSpec.setRangeMin(panel.getMin(doValidation));
      tiltAngleSpec.setRangeStep(panel.getStep(doValidation));
      return true;
    }
    catch (FieldValidationFailedException e) {
      return false;
    }
  }

  /**
   * Validates and return an error messaage.  This panel does not use
   * isValid() because it does not have enough information to display a
   * complete error message.
   * @return partial error message
   */
  boolean validate(final String errorTitle) {
    return DatasetTool.validateTiltAngle(manager, AxisID.ONLY, errorTitle, axisID,
        panel.isSpecifySelected(), panel.getMin(), panel.getStep());
  }
  
  void checkpoint() {
    panel.checkpoint();
  }

  void updateTemplateValues(final DirectiveFileCollection directiveFileCollection) {
    panel.updateTemplateValues(directiveFileCollection, axisID);
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
    panel.setSourceTooltip("Specify the source of the view tilt angles");
    panel.setExtractTooltip("Select the Extract option if the extended header of the "
        + "raw data stack contains the angles");
    panel.setSpecifyTooltip("Select the Specify option if you wish to manually "
        + "specify the tilt angles in the edit boxes below");
    panel.setMinTooltip("Starting tilt angle of the series");
    panel.setStepTooltip("Tilt increment between views");
    panel.setFileTooltip("Select the File option if the tilt angles already exist "
        + "in a *.rawtlt file");
  }

  /**
   *  Set the state of the text fields depending upon the radio button state
   */
  void setRadioButtonState(final ActionEvent event) {
    enableAngleFields(event.getActionCommand().equals(panel.getSpecify()));
  }
}
