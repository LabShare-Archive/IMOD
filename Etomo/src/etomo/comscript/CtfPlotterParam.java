package etomo.comscript;

import java.io.File;

import etomo.type.ConstEtomoNumber;
import etomo.type.ConstStringParameter;
import etomo.type.EtomoBoolean2;
import etomo.type.EtomoNumber;
import etomo.type.ScriptParameter;
import etomo.type.StringParameter;

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
 * <p> Revision 1.2  2009/02/25 00:14:32  sueh
 * <p> bug# 1182 Made sphericalAberration a double.
 * <p>
 * <p> Revision 1.1  2008/10/27 17:48:52  sueh
 * <p> bug# 1141 Class to update a ctfplotter call.
 * <p> </p>
 */
public final class CtfPlotterParam implements ConstCtfPlotterParam,
    CommandParam {
  public static final String rcsid = "$Id$";

  public static final String COMMAND = "ctfplotter";
  public static final String CONFIG_FILE_OPTION = "ConfigFile";
  public static final String EXPECTED_DEFOCUS_OPTION = "ExpectedDefocus";

  private final ScriptParameter voltage = new ScriptParameter(
      CtfPhaseFlipParam.VOLTAGE_OPTION);
  private final ScriptParameter sphericalAberration = new ScriptParameter(
      EtomoNumber.Type.DOUBLE, CtfPhaseFlipParam.SPHERICAL_ABERRATION_OPTION);
  private final EtomoBoolean2 invertTiltAngles = new EtomoBoolean2(
      CtfPhaseFlipParam.INVERT_TILT_ANGLES_OPTION);
  private final ScriptParameter amplitudeContrast = new ScriptParameter(
      EtomoNumber.Type.FLOAT, CtfPhaseFlipParam.AMPLITUDE_CONTRAST_OPTION);
  private final StringParameter configFile = new StringParameter(
      CONFIG_FILE_OPTION);
  private final ScriptParameter expectedDefocus = new ScriptParameter(
      EtomoNumber.Type.FLOAT, EXPECTED_DEFOCUS_OPTION);

  public void parseComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException, InvalidParameterException,
      FortranInputSyntaxException {
    reset();
    voltage.parse(scriptCommand);
    sphericalAberration.parse(scriptCommand);
    invertTiltAngles.parse(scriptCommand);
    amplitudeContrast.parse(scriptCommand);
    configFile.parse(scriptCommand);
    expectedDefocus.parse(scriptCommand);
  }

  public void updateComScriptCommand(ComScriptCommand scriptCommand)
      throws BadComScriptException {
    voltage.updateComScript(scriptCommand);
    sphericalAberration.updateComScript(scriptCommand);
    invertTiltAngles.updateComScript(scriptCommand);
    amplitudeContrast.updateComScript(scriptCommand);
    configFile.updateComScript(scriptCommand);
    expectedDefocus.updateComScript(scriptCommand);
  }

  public void initializeDefaults() {
  }

  private void reset() {
    voltage.reset();
    sphericalAberration.reset();
    invertTiltAngles.reset();
    amplitudeContrast.reset();
    expectedDefocus.reset();
  }

  public void setVoltage(String input) {
    voltage.set(input);
  }

  public void setSphericalAberration(String input) {
    sphericalAberration.set(input);
  }

  public void setInvertTiltAngles(boolean input) {
    invertTiltAngles.set(input);
  }

  public void setAmplitudeContrast(String input) {
    amplitudeContrast.set(input);
  }

  public void setConfigFile(File input) {
    configFile.set(input);
  }

  public ConstStringParameter getConfigFile() {
    return configFile;
  }

  public void setExpectedDefocus(String input) {
    expectedDefocus.set(input);
  }

  public ConstEtomoNumber getExpectedDefocus() {
    return expectedDefocus;
  }
}
